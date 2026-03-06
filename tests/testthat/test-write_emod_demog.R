# ── build_emod_demog ----------------------------------------------------------

test_that("build_emod_demog returns correct structure", {
  result <- build_emod_demog(
    lat = -3.08, lon = 29.37, pop = 100000,
    name = "BUBANZA",
    crude_death_rate = 8.5,
    crude_birth_rate = 38.0
  )

  expect_type(result, "list")
  expect_named(result, c("Defaults", "Metadata", "Nodes"))

  # metadata
  expect_equal(result$Metadata$IdReference, "Legacy")
  expect_equal(result$Metadata$NodeCount, 1)
  expect_equal(result$Metadata$Tool, "emod-api")

  # node
  node <- result$Nodes[[1]]
  expect_equal(node$NodeID, 1L)
  expect_equal(node$NodeAttributes$FacilityName, "BUBANZA")
  expect_equal(node$NodeAttributes$InitialPopulation, 100000L)
  expect_equal(node$NodeAttributes$Latitude, -3.08)
  expect_equal(node$NodeAttributes$Longitude, 29.37)

  # birth rate: per person per day
  expected_br <- 38.0 / 1000 / 365
  expect_equal(
    result$Defaults$NodeAttributes$BirthRate,
    expected_br
  )

  # mortality
  mort <- result$Defaults$IndividualAttributes$MortalityDistribution
  expect_equal(mort$ResultValues[[1]], 8.5)
  expect_equal(mort$ResultValues[[2]], 8.5)
})

test_that("build_emod_demog uses custom node_id and id_reference", {
  result <- build_emod_demog(
    lat = 0, lon = 0, pop = 1000, name = "TEST",
    crude_death_rate = 10, crude_birth_rate = 30,
    node_id = 42L, id_reference = "Custom_Ref"
  )

  expect_equal(result$Metadata$IdReference, "Custom_Ref")
  expect_equal(result$Nodes[[1]]$NodeID, 42L)
})

test_that("build_emod_demog uses default age distribution", {
  result <- build_emod_demog(
    lat = 0, lon = 0, pop = 1000, name = "TEST",
    crude_death_rate = 10, crude_birth_rate = 30
  )

  age_dist <- result$Defaults$IndividualAttributes$AgeDistribution
  expect_type(age_dist, "list")
  expect_equal(age_dist$ResultScaleFactor, 1)
  expect_equal(length(age_dist$DistributionValues), 50)
  expect_equal(length(age_dist$ResultValues), 50)
  expect_equal(age_dist$DistributionValues[1], 0)
  expect_equal(age_dist$ResultValues[1], 0)
})

test_that("build_emod_demog uses default individual properties", {
  result <- build_emod_demog(
    lat = 0, lon = 0, pop = 1000, name = "TEST",
    crude_death_rate = 10, crude_birth_rate = 30
  )

  ips <- result$Defaults$IndividualProperties
  expect_length(ips, 4)

  props <- vapply(ips, function(x) x$Property, character(1))
  expect_equal(
    props,
    c("AgeGroup", "DrugStatus", "SMCAccess", "VaccineStatus")
  )
})

test_that("build_emod_demog accepts custom age_distribution", {
  custom_age <- list(
    DistributionValues = c(0, 0.5, 1),
    ResultScaleFactor = 1,
    ResultValues = c(0, 5000, 10000)
  )

  result <- build_emod_demog(
    lat = 0, lon = 0, pop = 1000, name = "TEST",
    crude_death_rate = 10, crude_birth_rate = 30,
    age_distribution = custom_age
  )

  expect_identical(
    result$Defaults$IndividualAttributes$AgeDistribution,
    custom_age
  )
})

test_that("build_emod_demog accepts custom prevalence params", {
  result <- build_emod_demog(
    lat = 0, lon = 0, pop = 1000, name = "TEST",
    crude_death_rate = 10, crude_birth_rate = 30,
    initial_prevalence = 0.05,
    prevalence_distribution = c(0.1, 0.3)
  )

  attrs <- result$Defaults$IndividualAttributes
  expect_equal(attrs$InitialPrevalence, 0.05)
  expect_equal(attrs$PrevalenceDistribution1, 0.1)
  expect_equal(attrs$PrevalenceDistribution2, 0.3)
})

# ── write_emod_demog_by_adm2 ------------------------------------------------

test_that("write_emod_demog_by_adm2 writes one JSON per row", {
  skip_if_not_installed("jsonlite")

  tmp <- withr::local_tempdir()

  df <- data.frame(
    adm2 = c("Bubanza", "Cibitoke"),
    lat = c(-3.08, -2.89),
    lon = c(29.37, 29.18),
    pop = c(100000L, 120000L),
    crude_death_rate = c(8.5, 8.5),
    crude_birth_rate = c(38.0, 38.0),
    stringsAsFactors = FALSE
  )

  write_emod_demog_by_adm2(df, tmp)

  # default folder_case is "title"
  expect_true(dir.exists(file.path(tmp, "Bubanza")))
  expect_true(dir.exists(file.path(tmp, "Cibitoke")))

  f1 <- file.path(tmp, "Bubanza", "demographics.json")
  f2 <- file.path(tmp, "Cibitoke", "demographics.json")
  expect_true(file.exists(f1))
  expect_true(file.exists(f2))

  # parse and validate structure
  demog <- jsonlite::fromJSON(f1, simplifyVector = FALSE)
  expect_equal(demog$Metadata$IdReference, "Legacy")
  expect_equal(demog$Metadata$NodeCount, 1)
  expect_equal(demog$Nodes[[1]]$NodeID, 1)
  expect_equal(
    demog$Nodes[[1]]$NodeAttributes$FacilityName,
    "Bubanza"
  )
  expect_equal(
    demog$Nodes[[1]]$NodeAttributes$InitialPopulation,
    100000
  )
})

test_that("write_emod_demog_by_adm2 respects folder_case", {
  skip_if_not_installed("jsonlite")

  tmp <- withr::local_tempdir()

  df <- data.frame(
    adm2 = "Bubanza", lat = -3.08, lon = 29.37,
    pop = 100000L, crude_death_rate = 8.5,
    crude_birth_rate = 38.0,
    stringsAsFactors = FALSE
  )

  write_emod_demog_by_adm2(df, tmp, folder_case = "upper")
  expect_true(dir.exists(file.path(tmp, "BUBANZA")))

  tmp2 <- withr::local_tempdir()
  write_emod_demog_by_adm2(df, tmp2, folder_case = "lower")
  expect_true(dir.exists(file.path(tmp2, "bubanza")))
})

test_that("write_emod_demog_by_adm2 cleans special chars", {
  skip_if_not_installed("jsonlite")

  tmp <- withr::local_tempdir()

  df <- data.frame(
    adm2 = "My-District (North)", lat = 0, lon = 0,
    pop = 1000L, crude_death_rate = 10,
    crude_birth_rate = 30,
    stringsAsFactors = FALSE
  )

  write_emod_demog_by_adm2(df, tmp, folder_case = "title")
  # gsub("[^A-Za-z0-9]+", "_", ...) -> "My_District_North_"
  # str_to_title -> "My_District_North_"
  dirs <- list.dirs(tmp, recursive = FALSE, full.names = FALSE)
  expect_length(dirs, 1)
  expect_false(grepl("[^A-Za-z0-9_]", dirs[1]))
})

test_that("write_emod_demog_by_adm2 uses custom id_reference", {
  skip_if_not_installed("jsonlite")

  tmp <- withr::local_tempdir()

  df <- data.frame(
    adm2 = "Test", lat = 0, lon = 0, pop = 1000L,
    crude_death_rate = 10, crude_birth_rate = 30,
    stringsAsFactors = FALSE
  )

  write_emod_demog_by_adm2(
    df, tmp, id_reference = "Custom_Ref"
  )

  f <- file.path(tmp, "Test", "demographics.json")
  demog <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  expect_equal(demog$Metadata$IdReference, "Custom_Ref")
})

test_that("write_emod_demog_by_adm2 applies prefix and suffix", {
  skip_if_not_installed("jsonlite")

  tmp <- withr::local_tempdir()

  df <- data.frame(
    adm2 = "Bubanza", lat = -3.08, lon = 29.37,
    pop = 100000L, crude_death_rate = 8.5,
    crude_birth_rate = 38.0,
    stringsAsFactors = FALSE
  )

  write_emod_demog_by_adm2(
    df, tmp,
    demog_prefix = "BDI_",
    demog_suffix = "_wSMC"
  )

  f <- file.path(
    tmp, "Bubanza", "BDI_demographics_wSMC.json"
  )
  expect_true(file.exists(f))
})

test_that("write_emod_demog_by_adm2 auto-prefix with NULL", {
  skip_if_not_installed("jsonlite")

  tmp <- withr::local_tempdir()

  df <- data.frame(
    adm2 = "Bubanza", lat = -3.08, lon = 29.37,
    pop = 100000L, crude_death_rate = 8.5,
    crude_birth_rate = 38.0,
    stringsAsFactors = FALSE
  )

  write_emod_demog_by_adm2(df, tmp, demog_prefix = NULL)

  f <- file.path(
    tmp, "Bubanza", "Bubanza_demographics.json"
  )
  expect_true(file.exists(f))
})

test_that("write_emod_demog_by_adm2 errors on missing columns", {
  tmp <- withr::local_tempdir()

  bad_df <- data.frame(adm2 = "X", lat = 0, lon = 0)

  expect_error(
    write_emod_demog_by_adm2(bad_df, tmp),
    "Missing"
  )
})

# ── climate/demog alignment ---------------------------------------------------

test_that("demographics JSON matches climate JSON contract", {
  skip_if_not_installed("jsonlite")

  tmp <- withr::local_tempdir()

  df <- data.frame(
    adm2 = "Bubanza", lat = -3.08, lon = 29.37,
    pop = 100000L, crude_death_rate = 8.5,
    crude_birth_rate = 38.0,
    stringsAsFactors = FALSE
  )

  write_emod_demog_by_adm2(
    df, tmp, id_reference = "Legacy"
  )

  f <- file.path(tmp, "Bubanza", "demographics.json")
  demog <- jsonlite::fromJSON(f, simplifyVector = FALSE)

  # IdReference must be "Legacy" (matching climate default)
  expect_equal(demog$Metadata$IdReference, "Legacy")

  # NodeCount must be 1 (one node per adm2 folder)
  expect_equal(demog$Metadata$NodeCount, 1)

  # NodeID must be 1 (matching weather's remap to 1L)
  expect_equal(demog$Nodes[[1]]$NodeID, 1)

  # lat/lon present
  expect_equal(demog$Nodes[[1]]$NodeAttributes$Latitude, -3.08)
  expect_equal(demog$Nodes[[1]]$NodeAttributes$Longitude, 29.37)
})

# ── build_emod_demog_from_wpp -------------------------------------------------

test_that("build_emod_demog_from_wpp errors without wpp2024", {
  skip_if(
    requireNamespace("wpp2024", quietly = TRUE),
    "wpp2024 is installed, skipping missing-package test"
  )

  expect_error(
    build_emod_demog_from_wpp("Burundi", 2022),
    "wpp2024"
  )
})

test_that("build_emod_demog_from_wpp returns correct structure", {
  skip_if_not_installed("wpp2024")
  skip_if_not_installed("data.table")

  result <- build_emod_demog_from_wpp("Burundi", 2022)

  expect_type(result, "list")
  expect_named(
    result,
    c(
      "age_distribution", "crude_birth_rate",
      "crude_death_rate", "birth_rate_daily",
      "country", "year"
    )
  )

  expect_true(is.numeric(result$crude_birth_rate))
  expect_true(is.numeric(result$crude_death_rate))
  expect_true(result$crude_birth_rate > 0)
  expect_true(result$crude_death_rate > 0)

  # birth_rate_daily = cbr / 365
  expect_equal(
    result$birth_rate_daily,
    result$crude_birth_rate / 365
  )

  # age distribution structure
  age <- result$age_distribution
  expect_type(age, "list")
  expect_true("DistributionValues" %in% names(age))
  expect_true("ResultValues" %in% names(age))
  expect_equal(age$ResultScaleFactor, 1)

  # CDF should end at ~1
  cdf <- age$DistributionValues[[1]]
  expect_true(abs(cdf[length(cdf)] - 1) < 0.01)
})

test_that("build_emod_demog_from_wpp handles case-insensitive match", {
  skip_if_not_installed("wpp2024")
  skip_if_not_installed("data.table")

  result <- build_emod_demog_from_wpp("burundi", 2022)
  expect_equal(result$country, "Burundi")
})
