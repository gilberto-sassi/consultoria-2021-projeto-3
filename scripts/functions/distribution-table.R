#' Distribution table for one variable
#'
#' \code{tabela} computes the distribution table for a \code{tibble} object,
#' and save it in a \code{output} directory
#' 
#' @param df a \code{tibble} object, i.e., a \code{data.frame}
#' @param variavel a character (or factor) variable
#' @param legenda caption of the distribution table
#' @param rotulo a label at distribution table
#' @param base directory to save results
#' @param na_rm remove rows with \code{NA} values
#' 
#' @return a \code{tibble} that is distribution table
#' 
#' @usage tabela(df, variavel, legenda)
#' 
#' @examples 
#' n <- 1000
#' df <- tibble(nome = sample(c('A', 'B', 'C'), size = n, replace = T,
#'                            prob = c(0.6, 0.2, 0.2)),
#'              numero = rnorm(n))
#' tabela(df, 'nome', 'Nome')
tabela <- function(df, variavel, rotulo, legenda,
                   base = "",
                   na_rm = FALSE) {
  
  if(na_rm) {
    df_clean <- df %>% filter(.[[variavel]] != "")
  } else {
    df_clean <- df 
  }
  
  tab <- df_clean |> 
    dplyr::group_by(.data[[variavel]]) |> 
    dplyr::summarise(`Frequência` = dplyr::n()) |> 
    dplyr::mutate(`Frequência Relativa` = `Frequência` / sum(`Frequência`),
                  Porcentagem = `Frequência Relativa` * 100) |> 
    dplyr::arrange(desc(`Frequência`)) 
  colnames(tab) <- c(rotulo, "Frequência", "Frequência relativa", "Porcentagem")
  
  readr::write_csv2(
    tab,
    file = stringr::str_interp("${base}tabela_distribuicao/tab_distribuicao_${variavel}.csv")
    )
  openxlsx::write.xlsx(
    tab,
    file = stringr::str_interp("${base}tabela_distribuicao/tab_distribuicao_${variavel}.xlsx")
    )
  
  tab |> 
    knitr::kable(digits = 2, format.args = list(decimal.mark = ","),
                 format = "pipe",
                 caption = legenda,
                 align = "c")
}
