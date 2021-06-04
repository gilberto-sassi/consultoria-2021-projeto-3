#' Resume measures for a Likert variavel
#' 
#' @param df a \code{tibble} object 
#' @param variavel a character vector (or factor) 
#' @param rotulo a caption of the table
#' 
#' @return a \code{knitr::kable} object with resume measures: mean, median, 
#' standard deviation, first quartile, third quartile
#' 
#' @usage qui_test(x, y)
#' 
#' @example 
#' df <- tibble(x = sample(1:4, 100, replace = T))
#' resume_measures(df, x, "Legend.")
resume_measures <- function(df, variavel, rotulo, base = "") {
  if(missing(rotulo)) rotulo <- stringr::str_interp("Resumos para variável ${variavel}.")
  
  tab <- dados |>
    summarise(`Média` = mean(.data[[variavel]]),
              `Desvio Padrão` = sd(.data[[variavel]]),
              Mediana = median(.data[[variavel]]),
              `1Qua` = quantile(.data[[variavel]], probs = 0.25),
              `3Qua` = quantile(.data[[variavel]], probs = 0.75))
  
  readr::write_csv2(tab,
                    stringr::str_interp("${base}medidas_resumos_unidimensional/medias_resumo_${variavel}.csv"))
  openxlsx::write.xlsx(tab,
                       stringr::str_interp("${base}medidas_resumos_unidimensional/medias_resumo_${variavel}.xlsx"))
  
  tab |> 
    knitr::kable(format = 'pipe', digits = 2, align = "c",
                 caption = rotulo,
                 format.args = list(decimal.mark = ","))
  
}