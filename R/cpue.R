#' Calculate Mean Catch-per-unit Effort (CPUE)
#'
#' @param data `data.frame` or `tibble` containing samples as rows, and species counts as columns along with an effort columnt
#' @param species name of column (unquoted) for species for which CPUE is desired
#' @param effort name of column (unquoted) specifying the sample effort value (typically minutes)
#'
#' @return named vector containing "Mean CPUE" and "SE" estimates
#' @export
#'
#' @examples
#' fish_data <- tibble(BLUE = c(5.5, 2.3, 6.8, 4.5, 7.9,
#'                              8.5, 6.9, 7.1, 2.3, 9.1),
#'                     minutes = rep(10, 10))
#' fish_data %>% 
#'   cpue("BLUE", "minutes")
#' 
#' 
cpue <- function(data, species, effort) {
 # sample_CPUE <- data[, c(species)] / data[, c(effort)]
  
  data %>% 
    mutate(sample_CPUE = !!rlang::ensym(species)/!!rlang::ensym(effort))  %>%
    summarise("Mean CPUE" = mean(sample_CPUE),
              "SE" = sd(sample_CPUE) / sqrt(n()))
}