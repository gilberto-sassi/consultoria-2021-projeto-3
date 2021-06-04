#' Contingency table
#' 
#' @param df a tibble 
#' @param variavel_x a character (or factor) variable 
#' @param variavel_y a character (or factor) variable
#' @param rotulo label of x axis
#' 
#' @return a \code{knitr::kable} object
#' 
#' @usage contingency(df, variavel_x, variavel_y, rotulo_x, rotulo_y)
#' 
#' @examples
#' df <- tibble(x = sample(letters[1:5], 100, replace = T),
#'              y = sample(letters[6:10], 100, replace = T))
#' contingency(df, x, y, "x", "y")
contingency <- function(df, variavel_x, variavel_y, rotulo, base = "") {

  tab <- df |>
    group_by(.data[[variavel_x]], .data[[variavel_y]]) |>
    summarise(frequencia = n(), .groups = "keep") |>
    pivot_wider(names_from = .data[[variavel_y]], values_from = frequencia) 
  nomes <- colnames(tab)
  nomes[1] <- rotulo
  colnames(tab) <- nomes
  
  readr::write_csv2(tab,
                    stringr::str_interp("${base}tabela_contingencia/tab_contingency_${variavel_x}_${variavel_y}.csv"))
  openxlsx::write.xlsx(tab,
                       stringr::str_interp("${base}tabela_contingencia/tab_contingency_${variavel_x}_${variavel_y}.xlsx"))
  
  tab |>
    knitr::kable(format = "pipe", digits = 2,
                 align = "c",
                 format.args = list(decimal.mark = ","))
  
}
