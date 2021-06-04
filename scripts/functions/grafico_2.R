#' Bar graph for association
#' 
#' @param df a tibble 
#' @param variavel_x a character (or factor) variable 
#' @param variavel_y a character (or factor) variable
#' @param rotulo_x label of x axis
#' @param rotulo_fill label of legend
#' @param base directory to save the graphs
#' 
#' @return a \code{ggplot2} object 
#' 
#' @usage grafico_2(df, variavel_x, variavel_y, rotulo_x, rotulo_y)
#' 
#' @examples
#' df <- tibble(x = sample(letters[1:5], 100, replace = T),
#'              y = sample(letters[6:10], 100, replace = T))
#' grafico_2(df, x, y, "x", "y")
grafico_2 <- function(df, variavel_x, variavel_y, rotulo_x, rotulo_y = "", base = "") {

  g <- ggplot2::ggplot(df) +
    ggplot2::geom_bar(aes_string(x = variavel_x, fill = variavel_y),
                      position = 'fill') +
    ggplot2::labs(x = rotulo_x, y = "Porcentabem") +
    ggplot2::scale_fill_discrete(rotulo_y) +
    ggplot2::scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.25),
                                labels = seq(from = 0, to = 1, by = 0.25) * 100) +
    ggplot2::coord_flip()
  
  ggsave(
    stringr::str_interp("${base}grafico_barra_bidimensional/grafico_barra_${variavel_x}_${variavel_y}.png"),
    plot = g
  )
  ggsave(
    stringr::str_interp("${base}grafico_barra_bidimensional/grafico_barra_${variavel_x}_${variavel_y}.pdf"),
    plot = g
  )
  
  g
}
