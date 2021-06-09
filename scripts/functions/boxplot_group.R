#' Boxplot to compare means and distribution
#' 
#' @param df a tibble 
#' @param variavel a character (or factor) variable 
#' @param group a character (or factor) variable
#' @param rotulo_variavel label of x axis
#' @param rotulo_group label of legend
#' @param base directory to save the graphs
#' 
#' @details This function automatically save the graph in the directory
#' \eqn{boxplot\_dimensional}.
#' 
#' @return a \code{ggplot2} object 
#' 
#' @usage boxplot_group(df, variavel_x, variavel_fill, rotulo_x, rotulo_y)
#' 
#' @examples
#' df <- tibble(x = sample(0:3, 100, replace = T),
#'              y = sample(letters[6:10], 100, replace = T))
#' boxplot_group(df, x, y, "x", "y")
boxplot_group <- function(df, variavel, group, rotulo_variavel, rotulo_group,
                          base = "") {
  
  g <- ggplot(df) +
    geom_boxplot(aes_string(x = group, y = variavel, group = group)) +
    labs(x = group, y = rotulo_variavel) +
    scale_y_continuous(breaks = -1:4, limits = c(-1,4))
  
  ggsave(
    glue::glue("{base}boxplot_bidimensional/boxplot_{variavel}_{group}.png"),
    plot = g
  )
  ggsave(
    glue::glue("{base}boxplot_bidimensional/boxplot_{variavel}_{group}.pdf"),
    plot = g
  )
  
  g
}
