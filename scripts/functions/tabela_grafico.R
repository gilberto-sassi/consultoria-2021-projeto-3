#' Distribution table
#'
#' \code{tabela} computes the distribution table for a \code{tibble} object,
#' and save it in a \code{output} directory
#' 
#' @param df a \code{tibble} object, i.e., a \code{data.frame}
#' @param variavel a character (or factor) variable
#' @param legenda caption of the distribution table
#' @param rotulo a label at distribution table
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
                   base = "output",
                   na_rm = FALSE) {
  
  if(na_rm) {
    df_clean <- df %>% filter(.[[variavel]] != "")
  } else {
    df_clean <- df 
  }
  
  tab <- df |> 
    dplyr::group_by(.data[[variavel]]) |> 
    dplyr::summarise(`Frequência` = dplyr::n()) |> 
    dplyr::mutate(`Frequência Relativa` = `Frequência` / sum(`Frequência`),
                  Porcentagem = `Frequência Relativa` * 100) |> 
    dplyr::arrange(desc(`Frequência`)) 
  colnames(tab) <- c(rotulo, "Frequência", "Frequência relativa", "Porcentagem")
  
  readr::write_csv2(tab, file = stringr::str_interp("${base}tab_distribuicao_${variavel}.csv"))
  openxlsx::write.xlsx(tab, file = stringr::str_interp("${base}tab_distribuicao_${variavel}.xlsx"))
  
  tab |> 
    knitr::kable(digits = 2, format.args = list(decimal.mark = ","),
                 format = "pipe",
                 caption = legenda,
                 align = rep("c", 4))
}

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
grafico <- function(df, variavel, rotulo) {

  tab <- df |> 
    dplyr::group_by(.data[[variavel]]) |> 
    dplyr::summarise(fa = n(), .groups = 'drop') |> 
    dplyr::mutate(p = fa * 100 / sum(fa)) |> 
    dplyr::arrange(fa)
  colnames(tab) <- c("nome", 'fa', 'p')
  tab <- tab |> 
    mutate(nome = factor(nome, levels = nome))
  
  tab |> 
    ggplot() +
    geom_bar(aes(x = nome, y = p, group = 1), fill = "blue", stat = 'identity') +
    labs(x = rotulo, y = "Porcentagem") +
    coord_flip()
  
}

#' Bar graph for association
#' 
#' @param df a tibble 
#' @param variavel_x a character (or factor) variable 
#' @param variavel_y a character (or factor) variable
#' @param rotulo_x label of x axis
#' @param rotulo_y label of y axis
#' 
#' @return a \code{ggplot2} object 
#' 
#' @usage grafico_2(df, variavel_x, variavel_y, rotulo_x, rotulo_y)
#' 
#' @examples
#' df <- tibble(x = sample(letters[1:5], 100, replace = T),
#'              y = sample(letters[6:10], 100, replace = T))
#' grafico_2(df, x, y, "x", "y")
grafico_2 <- function(df, variavel_x, variavel_y, rotulo_x, rotulo_y = "") {

  ggplot2::ggplot(df) +
  ggplot2::geom_bar(aes_string(x = variavel_x, fill = variavel_y),
                    position = 'fill') +
  ggplot2::labs(x = rotulo_x, y = "Porcentabem") +
  ggplot2::scale_fill_discrete(rotulo_y) +
  ggplot2::scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.25),
                              labels = seq(from = 0, to = 1, by = 0.25) * 100) +
  ggplot2::coord_flip()
}

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
                    stringr::str_interp("${base}tab_contingency_${variavel_x}_${variavel_y}.csv"))
  openxlsx::write.xlsx(tab,
                       stringr::str_interp("${base}tab_contingency_${variavel_x}_${variavel_y}.xlsx"))
  
  tab |>
    knitr::kable(format = "pipe", digits = 2,
                 align = "c",
                 format.args = list(decimal.mark = ","))
  
}

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
  teste <- chisq.test(x = df[, variavel_x][[1]], y = df[, variavel_y][[1]])
  
  
  tab <- tibble(statistic = teste$statistic,
         df = teste$parameter,
         p_value = teste$p.value)
  
  readr::write_csv2(tab, "${base}qui_test_${variavel_x}_${variavel_y}.csv")
  openxlsx::write.xlsx(tab, "${base}qui_test_${variavel_x}_${variavel_y}.xlsx")
  
  tab |> 
    knitr::kable(format = 'pipe', digits = 2, align = "c",
                 caption = caption,
                 format.args = list(decimal.mark = ","))
}

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
                    stringr::str_interp("${base}medias_resumo_${variavel}.csv"))
  openxlsx::write.xlsx(tab,
                       stringr::str_interp("${base}medias_resumo_${variavel}.xlsx"))
  
  tab |> 
    knitr::kable(format = 'pipe', digits = 2, align = "c",
                 caption = rotulo,
                 format.args = list(decimal.mark = ","))
  
}