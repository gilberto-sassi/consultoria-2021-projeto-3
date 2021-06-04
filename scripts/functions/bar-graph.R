#' Bar graph
#' 
#' \code{grafico} builds the bar graph for a character variable.
#' 
#' @param df a tibble 
#' @param variavel a character (or factor) variable 
#' @param rotulo label of y axis
#' 
#' @return a \code{tibble} that is distribution table
#' 
#' @usage grafico(df, variable, rotulo)
#' 
#' @examples 
#' n <- 1000
#' df <- tibble(nome = sample(c('A', 'B', 'C'), size = n, replace = T,
#'                           prob = c(0.6, 0.2, 0.2)),
#'            numero = rnorm(n))
#' grafico(df, 'nome', 'Nome')
grafico <- function(df, variavel, rotulo, base = "") {

  tab <- df |> 
    dplyr::group_by(.data[[variavel]]) |> 
    dplyr::summarise(fa = n(), .groups = 'drop') |> 
    dplyr::mutate(p = fa * 100 / sum(fa)) |> 
    dplyr::arrange(fa)
  colnames(tab) <- c("nome", 'fa', 'p')
  tab <- tab |> 
    mutate(nome = factor(nome, levels = nome))
  
  g <- tab |>
    ggplot() +
    geom_bar(aes(x = nome, y = p, group = 1), fill = "blue", stat = 'identity') +
    labs(x = rotulo, y = "Porcentagem") +
    coord_flip()
  
  ggsave(
    stringr::str_interp("${base}grafico_barra_unidimensional/grafico_barra_${rotulo}.png"),
    plot = g
  )
  ggsave(
    stringr::str_interp("${base}grafico_barra_unidimensional/grafico_barra_${rotulo}.pdf"),
    plot = g
  )
  
  g
  
}
