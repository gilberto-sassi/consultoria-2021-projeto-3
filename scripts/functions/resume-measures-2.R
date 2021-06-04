#' Resume measures grouping by a character variable
#' 
#' @param df a tibble 
#' @param by a character (or factor) variable 
#' @param variavel numeric vector with a scale Likert
#' @param rotulo caption of table
#' @param base output directory to save results
#' 
#' @return a \code{knitr::kable} object with mean, standar deviation, 
#' median, first quartile and third quartile
#' 
#' @usage resume_measures_2(df, by, variavel, rotulo)
#' 
#' @examples
#' df <- tibble(x = sample(letters[1:5], 100, replace = T),
#'              y = sample(0:3, 100, replace = T))
#' resume_measures_2(df, x, y, "x", "y")
resume_measures_2 <- function(df, by, variavel, rotulo, base = "") {
  
  if(missing(rotulo)) rotulo <- variavel
  
  tab <- df |>
    group_by(.data[[by]]) |> 
    summarise(`Média` = mean(.data[[variavel]]),
              `Desvio Padrão` = sd(.data[[variavel]]),
              Mediana = median(.data[[variavel]]),
              `1 Quartil` = quantile(.data[[variavel]], probs = 0.25),
              `3 Quartil` = quantile(.data[[variavel]], probs = 0.75))
  nomes <- colnames(tab)
  nomes[1] <- rotulo
  colnames(tab) <- nomes
  
  readr::write_csv2(
    df,
    stringr::str_interp("${base}medidas_resumos_bidimensional/resumes_measures_2_${by}_${variavel}.csv")
  )
  openxlsx::write.xlsx(
    df,
    stringr::str_interp("${base}medidas_resumos_bidimensional/resumes_measures_2_${by}_${variavel}.xlsx")
  )
  
  tab |> 
    knitr::kable(format = "pipe", digits = 2, align = "c",
                 caption = stringr::str_interp("Medidas de resumo por ${rotulo}."),
                 format.args = list(decimal.mark = ","))
}
