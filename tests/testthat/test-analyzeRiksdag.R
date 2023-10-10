context("analyzeRiksdag")



test_that("Fetching riksdag members works", {
  expect_error(fun_fetch_data("seaturtle", "AU10", "4"))  
  expect_error(fun_fetch_data("2099", "AU10", "4"))
  expect_equal(fun_fetch_data("2022%2F23","AU10", "2")[1,"voteringlista.votering.fornamn"], "Kenneth G")  
})

test_that("Translating county names works.", {
  expect_equal(fun_get_county_options()[1,2],"Blekinge l%c3%A4n")
})

test_that("Translating year format works.",{
  expect_equal(fun_get_assembly_year_options()["2098/99"], NULL)
  expect_equal(fun_get_assembly_year_options()["2003/04"], "2003%2F04")
})

