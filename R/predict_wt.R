# len_wt_reg <- readr::read_csv("data/len_wt_reg.csv") |>
#                 tibble::tibble()
# save(len_wt_reg, file = "data/len_wt_reg.rda")
# load("data/len_wt_reg.rda")

#' Predict Weigth at Length
#'
#' @param species_code FWC LTM species code
#' @param tl Total length (mm)
#' @param error numeric error scale expressed as a % of estimate, defaults to 0.5
#'
#' @return tibble containing tl, prediction, upper and lower bounds 
#' @export
#'
#' @examples
#' predict_wt("BLUE", 150)
#' predict_wt("BLUE", 150)

predict_wt <- function(species_code, tl, error = 0.5) {
  data(len_wt_reg)
  params <- 
    len_wt_reg |> 
    dplyr::filter(FISHCODE == species_code)
  
  est = 10^((params[[1,"SLOPE1"]]*log10(tl)+params[[1,"INTER1"]]))
  
  tibble::tibble(
    tl = tl, 
    min = est - (est * error),
    est = est,
    max = est + (est * error))
}



# rewrite to tidyize this function 
