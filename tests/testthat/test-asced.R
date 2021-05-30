library(testthat)
library(abscorr)
library(dplyr)


test_that("asced_foe is as-expected", {
  # Check object
  expect_is(asced_foe, "tbl_df")
  # Check non-title labels
  expect_equal(asced_foe[[2]][200], "Health")
  # Check title labels
  expect_equal(asced_foe[[2]][150], "Architecture and Building")
  # Check digit conversion
  expect_identical(asced_foe$aced_foe_broad_code,
                   substr(asced_foe$aced_foe_detailed_code, 1, 2))
  expect_identical(asced_foe$aced_foe_narrow_code,
                   substr(asced_foe$aced_foe_detailed_code, 1, 4))
  # Spotcheck occupations and skill levels
  expect_equal(asced_foe %>%
                 filter(aced_foe_detailed_code == "091901") %>%
                 pull(aced_foe_detailed),
               "Economics")

  expect_equal(asced_foe %>%
                 filter(aced_foe_detailed_code == "129999") %>%
                 pull(aced_foe_detailed),
               "Mixed Field Programmes, n.e.c.")

  expect_equal(asced_foe %>%
                 filter(aced_foe_detailed_code == "040101") %>%
                 pull(aced_foe_narrow),
               "Architecture and Urban Environment")
})

