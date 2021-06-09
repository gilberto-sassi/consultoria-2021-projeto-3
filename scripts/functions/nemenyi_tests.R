#' Pairwise Test for Multiple Comparisons of Mean Rank Sums 
#' 
#' Calculate pairwise multiple comparisons between group levels. These
#' tests are sometimes referred to as Nemenyi-tests for multiple comparisons
#' of (mean) rank sums of independent samples.
#' 
#' @param df a tibble 
#' @param variavel a character (or factor) variable 
#' @param group a character (or factor) variable
#' @param caption caption of the table
#' @param base directory to save the graphs
#' 
#' @details This function automatically save the graph in the directory
#' \eqn{nemenyi\_tests}.
#' 
#' @return a \code{knitr::kable} object 
#' 
#' @usage nemenyi_tests(df, variavel, group, caption, base)
#' 
#' @examples
#' df <- tibble(x = sample(0:3, 100, replace = T),
#'              y = sample(letters[6:10], 100, replace = T))
#' nemenyi_tests(df, x, y)
nemenyi_tests <- function(df, variavel, group, caption = "", base = "") {
  
  v1 <- unlist(df[, variavel])
  v2 <- unlist(df[, group]) |> as.factor()
  base::suppressWarnings({
    teste <- posthoc.kruskal.nemenyi.test(v1, v2,
                                          p.adjust.method	= 'bonferroni')
  })
  
  
  m <- teste$p.value
  
  readr::write_csv2(
    m |> as_tibble(),
    file = glue::glue("{base}nemenyi_tests/teste_pares_{variavel}_{group}.csv")
  )
  openxlsx::write.xlsx(
    m |> as_tibble(),
    file = glue::glue("{base}nemenyi_tests/teste_pares_{variavel}_{group}.xlsx"),
    rowNames = TRUE
  )
  
  m |>
    knitr::kable(
      format = "pipe",
      digits = 2,
      row.names = TRUE,
      align = 'c',
      caption = caption, 
      format.args = list(decimal.mark = ",")
    )
}
