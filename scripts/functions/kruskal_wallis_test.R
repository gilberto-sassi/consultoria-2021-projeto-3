#' Kruskal Wallis
#' 
#' @param df a tibble 
#' @param variavel a numeric vector
#' @param g a character (or factor) variable
#' @param caption caption of table
#' @param base directory to save the graphs
#' 
#' @return a \code{knitr::kable} object 
#' 
#' @usage kruskal_wallis_test(df, variavel, g, caption, base)
#' 
#' @examples
#' df <- tibble(x = sample(0:3, 100, replace = T),
#'              y = sample(letters[6:10], 100, replace = T))
#' kruskal_wallis_test(df, x, y)
kruskal_wallis_test <- function(df, variavel, g, caption = "", base = "") {
  
    teste <- kruskal.test(unlist(df[, variavel]), unlist(df[, g]))
    
    tab <- tibble::tibble(`Estatística` = teste$statistic,
                          `Parâmetro` = teste$parameter,
                          `valor p` = teste$p.value)
    
    readr::write_csv2(
      tab,
      file = glue::glue("{base}kruskal_wallis_test/anova_{variavel}_{g}.csv")
    )
    openxlsx::write.xlsx(
      tab,
      file = glue::glue("{base}kruskal_wallis_test/anova_{variavel}_{g}.xlsx")
    )
    
    tab |> 
      knitr::kable(
        format = 'pipe',
        digits = 2,
        align = 'c',
        caption = caption,
        format.args = list(decimal.mark = ',')
      )
}
