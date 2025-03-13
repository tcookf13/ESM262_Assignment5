


# Testing oil() function
# ESM 262 Assignment 5
# Taylor Cook and Kelsey Warren
# 13 March 2025

# Test to ensure that oil() is working as expected






# oil(volume, wtemp, windspeed, viscosity, ratecoeff)
# example:
# volume_data = c(100000, 200000, 300000)
# oil(volume=volume_data, wtemp=20, windspeed=6, viscosity = .0001, ratecoeff = 0.16) ### we expect area_km2 = 470.5285, 746.9175, and 978.7388, respectively

test_that("oil_works", {
  volume_data = c(100000, 200000, 300000)
  expect_that(oil(volume_data, wtemp=20, windspeed=6, viscosity=0.0001, ratecoeff=0.16)$area_km2, equals(c(470.529,746.917,978.739)))
  expect_that(oil(volume=0, wtemp=20, windspeed=6, viscosity=0.0001, ratecoeff=0.16)$area, equals(0))
  expect_that(oil(volume=0, wtemp=20, windspeed=6, viscosity=0.0001, ratecoeff=0.16)$thickness, equals(NaN))
})


