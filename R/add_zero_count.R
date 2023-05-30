#' Add zero count data for species missing from sample
#'
#' @param data dataset as data.frame or tibble
#' @param sample_ids sample id variables
#' @param species_id species ID field
#' @param count field containing species counts
#' @param format defaults to "long" - data returned with 1 row per sample_id x species_id, else if not "long" returns data in "wide" format, with one row per sample_id and 1 column per unique species in the species_id. 
#'
#' @return tibble
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
#'   add_zero_count(sample_ids = c(site,minutes), species_id = species_name, count = ct)
add_zero_count <- function(data, 
                           sample_ids,
                           species_id, 
                           count, 
                           format = "long") {
  
  
  ct_data <-
    data %>%
    dplyr::select(c({{ sample_ids }}, {{ species_id }}, {{ count }})) %>%
    dplyr::group_by(pick({{ sample_ids }}, {{ species_id }})) %>%
    dplyr::summarise(total = sum({{ count }}, na.rm = TRUE), .groups = "keep") %>%
    tidyr::pivot_wider(names_from = {{ species_id }}, values_from = total, values_fill = 0)

  if(format == "long") {
    ct_data <- ct_data %>%
      ungroup() %>%
      tidyr::pivot_longer(!c({{ sample_ids }}), 
                          names_to = rlang::as_string(rlang::ensym(species_id)), 
                          values_to =  rlang::as_string(rlang::ensym(count)))
  }

  ct_data
}