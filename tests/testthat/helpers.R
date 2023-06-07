new_fish_data <- function() {
  tibble(
   site = c(1:5,3:7, 1, 3, 4, 6, 7),
   species_name = c(rep("BLUE", 5), rep("RESU", 5), rep("BLCR", 5)),
   ct = c(5, 2, 6, 4, 7,
          8, 6, 7, 2, 9,
          1, 2, 1, 3, 2),
   minutes = rep(10, 15))
}