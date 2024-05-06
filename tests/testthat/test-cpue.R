load(test_path('fixtures', 'newnans.rds'))
newn <- ltm.data.summary("newnans-test", newnans)

test_that("CPUE Test 1", {
  
  fish_cpue <-
    new_fish_data() %>% 
    group_by(species_name) %>% 
    cpue(ct, minutes)
  
  expect_equal(fish_cpue[[which(fish_cpue$species_name == "BLUE"), "mean_CPUE"]], 0.48)
  expect_equal(fish_cpue[[which(fish_cpue$species_name == "BLCR"), "mean_CPUE"]], 0.18)
})

test_that("Bluegill CPUE estimates for Newnans calculated correctly", {
  
  newn_cpue <- 
  newnans %>% 
  mutate(year = lubridate::year(lubridate::mdy(as.character(Date))),
         Effort = Effort/60) %>% 
  add_zero_count(sample_ids = c(ID:TransType, year), species_id = SpeciesCode, count = Count) %>% 
  group_by(WaterBody, year, Season, SamplingType, Target, SpeciesCode) %>% 
  cpue(count = Count, effort = Effort)

expect_equal(newn_cpue %>% 
               filter(year == 2006, SpeciesCode == "BLUE") %>% 
               pull(mean_CPUE), 17.826666667)

expect_equal(newn_cpue %>% 
               filter(year == 2018, SpeciesCode == "BLUE") %>% 
               pull(mean_CPUE), 1.428)
})

test_that("cpue_plot(): works", {
  
  expect_no_message({ 
    cpue_plot(newn, speciesList = "BLUE") 
    })

})