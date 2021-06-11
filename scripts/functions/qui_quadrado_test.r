#' Chi-square test table in pt-br
#' 
#' @param df a \code{tibble} object 
#' @param variavel_x a character vector (or factor) 
#' @param variavel_y a character vector (or factor)
#' @param rotulo a caption of the table
#' @param base directory where to save the results
#' 
#' @return a \code{knitr::kable} object with statistic, degree freedom and p-value
#' 
#' @usage qui_test(x, y)
#' 
#' @example 
#' df <- tibble(x = sample(letters[1:5], 100, replace = T),
#'              y = sample(letters[6:10], 100, replace = T))
#' qui_test(df, x, y, "x", "y")
qui_test <- function(df, variavel_x, variavel_y, caption, base = "") {

  if(missing(caption)) caption <- stringr::str_interp("Teste qui-quadrado entre ${variavel_x} e ${variavel_y}.")
  base::suppressWarnings({
    teste <- chisq.test(x = df[, variavel_x][[1]], y = df[, variavel_y][[1]])
  })
  
  
  
  tab <- tibble(`Estatística` = teste$statistic,
         `Graus de liberdade` = teste$parameter,
         `Valor-p` = teste$p.value)

  readr::write_csv2(
    tab, 
    stringr::str_interp("${base}teste_qui_quadrado/qui_test_${variavel_x}_${variavel_y}.csv"))
  openxlsx::write.xlsx(
    tab,
    stringr::str_interp("${base}teste_qui_quadrado/qui_test_${variavel_x}_${variavel_y}.xlsx"))
  
  tab |> 
    knitr::kable(format = 'pipe', digits = 2, align = "c",
                 caption = caption,
                 format.args = list(decimal.mark = ","))
}
