googlesheets4::gs4_deauth()

gscholar_stats <- function(url) {
  cites <- get_cites(url)
  paste(
    "Citations:", cites$citations, "•",
    "h-index:", cites$hindex, "•",
    "i10-index:", cites$i10index
  )
}

get_cites <- function(url) {
  html <- xml2::read_html(url)
  node <- rvest::html_nodes(html, xpath = '//*[@id="gsc_rsb_st"]')
  cites_df <- rvest::html_table(node)[[1]]
  cites <- data.frame(t(as.data.frame(cites_df)[, 2]))
  names(cites) <- c("citations", "hindex", "i10index")
  cites
}

get_cv_sheet <- function(sheet, audience = params$audience) {
  df <- googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1GmjuFlFnyhJNCXqtyO6mrwYf84ptDxyKy4O6O_A7zKY/edit?usp=sharing",
    sheet = sheet
  )

  include_col <- dplyr::case_when(
    audience == "academia" ~ "include_academia",
    audience == "industry" ~ "include_industry",
    TRUE ~ NA_character_
  )

  if (!is.na(include_col) && include_col %in% names(df)) {
    df <- df %>%
      dplyr::filter(
        !is.na(.data[[include_col]]) &
          tolower(trimws(.data[[include_col]])) == "y"
      )
  }

  df
}

make_ordered_list <- function(x) {
  pander::pandoc.list(x, style = "ordered")
}

make_bullet_list <- function(x) {
  pander::pandoc.list(x, style = "bullet")
}

make_ordered_list_filtered <- function(df, cat) {
  df %>%
    dplyr::filter(category == {{ cat }}) %>%
    dplyr::mutate(
      citation = stringr::str_replace_all(
        citation,
        "\\\\\\*(\\w+),",
        "\\\\*\\\\underline{\\1},"
      )
    ) %>%
    dplyr::pull(citation) %>%
    make_ordered_list()
}

na_to_space <- function(x) {
  ifelse(is.na(x), "", x)
}

enquote <- function(x) {
  paste0('"', x, '"')
}

markdown_url <- function(url) {
  paste0("[", url, "](", url, ")")
}