library(httr)
library(purrr)
library(tidyverse)

# Sys.setenv(EASYNVEST_AUTH_TOKEN = "")

notas_de_negociacao <- function(
  end_date = lubridate::today() - lubridate::days(1),
  start_date = as.Date(end_date) - lubridate::days(89)
  ) {

  if(as.Date(end_date) - start_date > 90) rlang::abort("Período de seleção deve ser de até 90 dias após a data inicial", class = "value")

  url <- glue::glue("https://www.easynvest.com.br/api/gringott/invoices/1/RV?startDate={start_date}&endDate={end_date}")
  authorization <- Sys.getenv("EASYNVEST_AUTH_TOKEN")

  invoices <- httr::GET(url, httr::add_headers(authorization = authorization)) %>%
    httr::content() %>%
    purrr::pluck("value") %>%
    purrr::pluck("invoices") %>%
    tibble::enframe() %>%
    dplyr::mutate(value = purrr::map(value, tibble::as_tibble)) %>%
    tidyr::unnest(value)

  if(nrow(invoices) == 0)
    invoices <- tibble::tribble(~"name", ~"invoiceNumber", ~"date", ~"description")

  invoices
}

baixar_pdf_da_nota_de_negociacao <- function(date, invoice_number, path = "", overwrite = TRUE) {
  url <- glue::glue("https://www.easynvest.com.br/api/gringott/invoices/report/1/RV?date={date}&invoiceNumber={invoice_number}")
  authorization <- Sys.getenv("EASYNVEST_AUTH_TOKEN")
  pdf_filename <- glue::glue("invoice_{date}_{invoice_number}.pdf")
  pdf_filename <- file.path(path, pdf_filename)

  if(!file.exists(pdf_filename)) {
    response <- httr::GET(
      url,
      httr::add_headers(authorization = authorization),
      httr::write_disk(pdf_filename, overwrite = overwrite)
    )
  }
  pdf_filename
}


datas <- seq.Date(lubridate::today(), as.Date("2013-12-01"), by = -89 )
invoices <- map_dfr(datas, notas_de_negociacao)

invoices_baixados <- invoices %>%
  mutate(
    date = as.Date(date),
    pdf_path = map2_chr(date, invoiceNumber, baixar_pdf_da_nota_de_negociacao, path = "C:/Users/ap_da/OneDrive/Desktop/invoices")
  )
readr::write_rds(invoices_baixados, "invoices_baixados.rds")
invoices_baixados <- read_rds("invoices_baixados.rds")

invoices_baixados$pdf_path[1]

extrai_tabela_de_negociacoes <- function(pdf_path) {
  tibble::tibble(
    texto = pdftools::pdf_text(pdf_path) %>% str_split("\\n") %>% unlist() %>% str_subset("BOVESPA     ") %>% str_replace(" {45,}", "   -   ")
  ) %>%
    separate(
      texto,
      into = c("Mercado", "C/V", "Tipo de Mercado", "Especificação do Título", "Observação", "Quantidade", "Preço/Ajuste", "Valor/Ajuste", "D/C"),
      sep = "[[:blank:]]{3,}"
    ) %>%
    mutate(
      sigla_acao = stringr::str_extract(`Especificação do Título`, "[A-Z]{4}[0-9]"),
      `Valor/Ajuste` = readr::parse_number(`Valor/Ajuste`, locale = readr::locale(decimal_mark = ",")),
      `Preço/Ajuste` = readr::parse_number(`Preço/Ajuste`, locale = readr::locale(decimal_mark = ",")),
      Quantidade = readr::parse_number(Quantidade, locale = readr::locale(decimal_mark = ","))
    )
}

extrai_tabela_de_saldo_liquido_do_dia <- function(pdf_path) {
  valor_liquido_do_dia = pdftools::pdf_text(pdf_path) %>%
    str_extract("Líquido para .+-?[0-9\\.,]+") %>%
    str_extract("-?[0-9\\.,]+$") %>%
    readr::parse_number(locale = readr::locale(decimal_mark = ","))

  valor_liquido_do_dia
}

# nivel invoice
invoices <- invoices_baixados %>%
  distinct(invoiceNumber, .keep_all = TRUE) %>%
  mutate(
    tabela = map(pdf_path, extrai_tabela_de_negociacoes),
    valor_liquido_do_invoice = map_dbl(pdf_path, extrai_tabela_de_saldo_liquido_do_dia)
  )

# nivel acao-invoice
invoices_acoes <- invoices %>%
  unnest(tabela) %>%
  arrange(invoiceNumber, `Especificação do Título`, date) %>%
  group_by(invoiceNumber, date, `C/V`, sigla_acao) %>%
  summarise(
    quantidade = sum(Quantidade),
    valor_bruto = first(ifelse(`C/V` == "C", -1, 1))*sum(`Valor/Ajuste`),
    valor_liquido_do_invoice = first(valor_liquido_do_invoice)
  ) %>%
  group_by(invoiceNumber, date) %>%
  mutate(
    quantidade = case_when(
      str_detect(sigla_acao, "MGLU3") & date < as.Date("2020-10-07") ~ quantidade*4,
      str_detect(sigla_acao, "UGPA3") & date < as.Date("2019-04-18") ~ quantidade*2,
      TRUE ~ quantidade
    ),
    quantidade_do_invoice = sum(quantidade),
    valor_bruto_do_invoice = sum(valor_bruto),
    custos_proporcionais_do_invoice = (valor_bruto_do_invoice-valor_liquido_do_invoice)/quantidade_do_invoice,
    valor_liquido = -(valor_bruto - quantidade*custos_proporcionais_do_invoice),
    mes = lubridate::floor_date(date, "month"),
    preco_de_negociacao = valor_liquido/quantidade
  ) %>%
  arrange(invoiceNumber)

lucros_e_despezas_por_mes_por_acao <- invoices_acoes %>%
  arrange(sigla_acao, date) %>%
  group_by(sigla_acao) %>%
  mutate(
    realizacao_id = lag(cumsum(`C/V` == "V"), default = 0),
    quantidade = ifelse(`C/V` == "C", 1, -1)*quantidade,
    preco = (valor_liquido)/quantidade,
    preco_atual = purrr::accumulate2(
      quantidade,
      valor_liquido,
      function(p, q, v) {
        estoque <- q + p$estoque
        preco_medio <- case_when(
          q > 0 & estoque > 0 ~ (v + p$preco_medio * p$estoque)/estoque,
          TRUE ~ p$preco_medio
        )

        return(list(estoque = estoque, preco_medio = preco_medio))
      }, .init = list(estoque = 0, preco_medio = 0)) %>%
      discard(~.x$preco_medio == 0)
  ) %>%
  unnest_wider(preco_atual) %>%
  select(date, mes, sigla_acao, `C/V`, realizacao_id, quantidade, valor_bruto, valor_liquido, preco, estoque, preco_medio) %>%
  mutate(
    lucro = (abs((preco - preco_medio) * quantidade) * (`C/V` == "V"))
  )

lucros_e_despezas_por_mes_por_acao

lucros_e_despezas_por_mes_por_acao %>%
  arrange(mes) %>%
  group_by(mes, date) %>%
  summarise(
    valor_de_vendas = sum(abs(valor_liquido)[`C/V` == "V"]),
    lucro = sum(lucro)
  ) %>%
  mutate(
    valor_de_vendas_no_mes = sum(valor_de_vendas),
    vendas_maior_que_20mil_no_mes = valor_de_vendas_no_mes >= 20000,
    imposto_sobre_lucro = lucro * vendas_maior_que_20mil_no_mes * 0.15
  ) %>%
  ungroup() %>%
  summarise(
    sum(imposto_sobre_lucro)
  )

