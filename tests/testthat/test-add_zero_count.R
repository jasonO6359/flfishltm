test_that("each species has same number of observations", {
  fish_data <-
    new_fish_data() %>% 
    add_zero_count(sample_ids = c(site,minutes), species_id = species_name, count = ct)

  species_cts <- fish_data %>% group_by(species_name) %>% count() %>% pull(n)
  
  expect_equal(max(species_cts), min(species_cts))
})

test_that("nrow in output is equal to nSites * nSpecies", {
  fish_data <-
    new_fish_data() %>% 
    add_zero_count(sample_ids = c(site,minutes),
                   species_id = species_name, 
                   count = ct,
                   format = "long")
  
  site_ct <- 
    new_fish_data() %>% 
    group_by(site, minutes) %>%
    count() %>%
    ungroup() %>% 
    count() %>% 
    pull(n)
  
  species_ct <- 
    new_fish_data() %>% 
    distinct(species_name) %>% 
    count() %>% 
    pull(n)
  
  expect_equal(nrow(fish_data), species_ct *site_ct)
})
