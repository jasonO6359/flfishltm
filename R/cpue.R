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
#' fish_data <- tibble(
#'   site = c(1:5,3:7),
#'   species_name = c(rep("BLUE", 5), rep("RESU", 5)),
#'   ct = c(5, 2, 6, 4, 7,
#'          8, 6, 7, 2, 9),
#'   minutes = rep(10, 10))
#' 
#' fish_data %>% 
#'   cpue(ct, "minutes") # can specify column name as either a symbol or character
#'   
#' fish_data %>% 
#'   group_by(species_name) %>% # use dplyr::group_by to specify grouping variables.
#'   cpue(ct, "minutes")
#'   
#' # fish_data %>% 
#' #   add_zero_count(c(site,minutes), species_name, ct) %>% #use [add_zero_count()]to account for missing absence data
#' #   group_by(species_name) %>% 
#' #   cpue(ct, "minutes")
cpue <- function(data, count, effort) {
  data %>% 
    dplyr::mutate(sample_CPUE = !!rlang::ensym(count)/!!rlang::ensym(effort))  %>%
    dplyr::summarise("mean_CPUE" = mean(sample_CPUE),
              "SE" = sd(sample_CPUE) / sqrt(n()),
              "N" = dplyr::n(),
              .groups = "keep")
}

