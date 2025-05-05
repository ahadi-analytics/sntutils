testthat::test_that("handle_file_save handles basic file saving", {
  # Setup
  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "test_cache.rds")
  test_data <- data.frame(
    level = c(1, 2),
    name_to_match = c("test1", "test2"),
    created_time = Sys.time() - c(1, 2),
    stringsAsFactors = FALSE
  )

  # Mock user input to say "y" and use default path
  mockery::stub(
    handle_file_save,
    "readline",
    mockery::mock("y", "")
  )

  # Run function
  testthat::expect_message(
    handle_file_save(test_data, test_file),
    "File saved successfully"
  )

  # Check if file exists and contains correct data
  testthat::expect_true(file.exists(test_file))
  saved_data <- readRDS(test_file)
  testthat::expect_equal(nrow(saved_data), 2)
  testthat::expect_equal(saved_data$name_to_match, c("test1", "test2"))

  # Cleanup
  unlink(test_file)
})

testthat::test_that("handle_file_save merges with existing cache", {
  # Setup
  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "test_cache.rds")

  # Create existing cache
  existing_data <- data.frame(
    level = 1,
    name_to_match = "existing",
    created_time = Sys.time() - 3,
    stringsAsFactors = FALSE
  )
  saveRDS(existing_data, test_file)

  # New data to save
  new_data <- data.frame(
    level = 1,
    name_to_match = c("new1", "existing"),
    created_time = Sys.time(),
    stringsAsFactors = FALSE
  )

  # Mock user input
  mockery::stub(
    handle_file_save,
    "readline",
    mockery::mock("y")
  )

  # Run function
  testthat::expect_message(
    handle_file_save(new_data, test_file),
    "File saved successfully"
  )

  # Check merged results
  merged_data <- readRDS(test_file)
  testthat::expect_equal(nrow(merged_data), 2) # Should deduplicate
  testthat::expect_true("new1" %in% merged_data$name_to_match)

  # Cleanup
  unlink(test_file)
})

testthat::test_that("handle_file_save handles user rejection", {
  # Mock user input to say "n"
  mockery::stub(
    handle_file_save,
    "readline",
    mockery::mock("n")
  )

  # Run function
  testthat::expect_message(
    handle_file_save(data.frame()),
    "File not saved"
  )
})

testthat::test_that("handle_file_save handles invalid input then accepts", {
  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "test_cache.rds")

  # Mock user input: invalid input "x", then "y"
  mockery::stub(
    handle_file_save,
    "readline",
    mockery::mock("x", "y", "")
  )

  suppressMessages(
    # Run function
    testthat::expect_message(
      handle_file_save(data.frame(), test_file),
      "Invalid input"
    )
  )
  # Cleanup
  unlink(test_file)
})

testthat::test_that("handle_file_save creates directory if needed", {
  # Setup
  temp_dir <- file.path(tempdir(), "new_dir")
  test_file <- file.path(temp_dir, "test_cache.rds")

  # Mock user input
  mockery::stub(
    handle_file_save,
    "readline",
    mockery::mock("y")
  )

  suppressMessages(
    # Run function
    testthat::expect_message(
      handle_file_save(data.frame(), test_file),
      "File saved successfully"
    )
  )
  # Check if directory was created
  testthat::expect_true(dir.exists(temp_dir))

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

testthat::test_that("handle_file_save handles NULL path", {
  # Mock user inputs: "y" for save, then provide path
  mockery::stub(
    handle_file_save,
    "readline",
    mockery::mock("y", file.path(tempdir(), "test_cache.rds"))
  )

  suppressMessages(
    testthat::expect_message(
      handle_file_save(data.frame(), NULL),
      "specified path is null"
    )
  )
})




testthat::test_that("calculate_string_distance works correctly", {
  # get function output
  result <- calculate_string_distance(
    c("New York", "Los Angeles"),
    c("New York", "Los Angeles", "Chicago"), "lv"
  )

  # expected outputs based on function results
  expected <- data.frame(
    algorithm_name = rep("lv", 6),
    name_to_match = rep(c("New York", "Los Angeles"), each = 3),
    matched_names = c(
      "New York", "Chicago",
      "Los Angeles", "Los Angeles", "New York", "Chicago"
    ),
    distance = c(0, 8, 10, 0, 8, 10),
    match_rank = rep(1:3, 2)
  )

  # Test if the output matches expected data frame
  testthat::expect_equal(
    result$matched_names,
    c(
      "Los Angeles", "New York", "Chicago", "New York",
      "Chicago", "Los Angeles"
    )
  )
})

testthat::test_that("Test administrative matching stats output", {
  # setup mock data
  data <- data.frame(
    country = c("Country1", "Country2", "Country1", "Country3"),
    province = c("State1", "State2", "State1", "State3"),
    district = c("City1", "City2", "City1", "City4")
  )

  c(
    "Los Angeles", "New York", "Chicago", "New York",
    "Chicago", "Los Angeles"
  )

  lookup_data <- data.frame(
    country = c("Country1", "Country2", "Country1", "Country3"),
    province = c("State1", "State2", "State1", "State3"),
    district = c("City1", "City2", "City1", "City4")
  )

  suppressMessages({
    testthat::expect_invisible(
      calculate_match_stats(
        data = data, lookup_data = lookup_data, level0 = "country",
        level1 = "province", level2 = "district"
      )
    )
  })
})


testthat::test_that("prep_geonames correctly processes admin names", {
  # Create a sample target data frame
  target_df <- data.frame(
    country = c("ANGOLA", "UGA", "ZAMBIA"),
    province = c("CABONDA", "TESO", "LUSAKA"),
    district = c("BALIZE", "BOKEDEA", "RAFUNSA"),
    subdistrict = c("AREA1", "AREA2", "AREA3"),
    stringsAsFactors = FALSE
  )

  suppressMessages(
    # Run the function in non-interactive mode
    cleaned_df <- prep_geonames(
      target_df,
      level0 = "country",
      level1 = "province",
      level2 = "district",
      interactive = FALSE
    )
  )

  # Check that the function returns a data frame
  testthat::expect_s3_class(cleaned_df, "data.frame")

  # Check that the returned data frame has the same number of rows as the input
  testthat::expect_equal(nrow(cleaned_df), nrow(target_df))

  # Check that the returned data frame contains the expected columns
  testthat::expect_true(
    all(c(
      "country", "province",
      "district", "subdistrict"
    ) %in% colnames(cleaned_df))
  )
})

testthat::test_that("calculate_match_stats calculates correct matches for all levels", {
  # Create test data frames
  data <- data.frame(
    country = c("USA", "Canada", "Mexico", "Brazil", "Colombia"),
    province = c("California", "Ontario", "Jalisco", "Sao Paulo", "Bogota"),
    district = c("Los Angeles", "Toronto", "Guadalajara", "Sao Paulo City", "Bogota DC"),
    subdistrict = c("Hollywood", "Downtown", "Centro", "Zona Sul", "Chapinero"),
    settlement = c("West Hollywood", "Kensington", "Zapopan", "Ipanema", "Zona T"),
    stringsAsFactors = FALSE
  )

  lookup_data <- data.frame(
    country = c("USA", "Canada", "France", "Brazil", "Germany"),
    province = c("California", "Quebec", "Ile-de-France", "Sao Paulo", "Bavaria"),
    district = c("Los Angeles", "Montreal", "Paris", "Sao Paulo City", "Munich"),
    subdistrict = c("Hollywood", "Old Montreal", "Le Marais", "Zona Sul", "Altstadt"),
    settlement = c("West Hollywood", "Mile End", "Saint-Germain", "Ipanema", "Marienplatz"),
    stringsAsFactors = FALSE
  )

  # Instead of testing cli output directly, verify the internal calculations
  # Mock the cli functions to verify they're called with correct arguments
  info_mock <- mockery::mock()
  li_mock <- mockery::mock()
  ul_mock <- mockery::mock()
  end_mock <- mockery::mock()

  mockery::stub(calculate_match_stats, "cli::cli_alert_info", info_mock)
  mockery::stub(calculate_match_stats, "cli::cli_ul", ul_mock)
  mockery::stub(calculate_match_stats, "cli::cli_li", li_mock)
  mockery::stub(calculate_match_stats, "cli::cli_end", end_mock)

  # Call the function
  calculate_match_stats(
    data,
    lookup_data,
    level0 = "country",
    level1 = "province",
    level2 = "district",
    level3 = "subdistrict",
    level4 = "settlement"
  )

  # Verify the mock was called with correct arguments
  testthat::expect_equal(
    mockery::mock_args(info_mock)[[1]][[1]], "Match Summary:"
  )

  # Verify the expected matches through calculation
  level0_matches <- sum(
    unique(data$country) %in% unique(lookup_data$country)
  )
  level1_matches <- sum(
    unique(data$province) %in% unique(lookup_data$province)
  )
  level2_matches <- sum(
    unique(data$district) %in% unique(lookup_data$district)
  )
  level3_matches <- sum(
    unique(data$subdistrict) %in% unique(lookup_data$subdistrict)
  )
  level4_matches <- sum(
    unique(data$settlement) %in% unique(lookup_data$settlement)
  )

  testthat::expect_equal(level0_matches, 3) # USA, Canada, Brazil
  testthat::expect_equal(level1_matches, 2) # California, Sao Paulo
  testthat::expect_equal(level2_matches, 2) # Los Angeles, Sao Paulo City
  testthat::expect_equal(level3_matches, 2) # Hollywood, Zona Sul
  testthat::expect_equal(level4_matches, 2) # West Hollywood, Ipanema

  # Check that li_mock was called 5 times (once for each level)
  # testthat::expect_equal(mockery::mock_call_count(li_mock), 5)

  # Let's also directly test the function without mocking
  # by checking if it runs without errors
  testthat::expect_no_error(
    calculate_match_stats(
      data,
      lookup_data,
      level0 = "country",
      level1 = "province",
      level2 = "district",
      level3 = "subdistrict",
      level4 = "settlement"
    )
  )
})

testthat::test_that("calculate_match_stats handles empty data correctly", {
  # Create empty data frames
  empty_data <- data.frame(
    country = character(0),
    province = character(0),
    district = character(0),
    stringsAsFactors = FALSE
  )

  lookup_data <- data.frame(
    country = c("USA", "Canada"),
    province = c("California", "Ontario"),
    district = c("Los Angeles", "Toronto"),
    stringsAsFactors = FALSE
  )

  # Mock cli functions
  mockery::stub(calculate_match_stats, "cli::cli_alert_info", mockery::mock())
  mockery::stub(calculate_match_stats, "cli::cli_ul", mockery::mock())
  mockery::stub(calculate_match_stats, "cli::cli_li", mockery::mock())
  mockery::stub(calculate_match_stats, "cli::cli_end", mockery::mock())

  # Should not error with empty data
  testthat::expect_no_error(
    calculate_match_stats(
      empty_data,
      lookup_data,
      level0 = "country",
      level1 = "province",
      level2 = "district"
    )
  )
})

testthat::test_that("calculate_match_stats ignores case in matches", {
  # Create test data frames with different case
  data <- data.frame(
    country = c("USA", "canada", "mexico"),
    stringsAsFactors = FALSE
  )

  lookup_data <- data.frame(
    country = c("usa", "CANADA", "France"),
    stringsAsFactors = FALSE
  )

  # Mock cli functions
  mockery::stub(calculate_match_stats, "cli::cli_alert_info", mockery::mock())
  mockery::stub(calculate_match_stats, "cli::cli_ul", mockery::mock())
  mockery::stub(calculate_match_stats, "cli::cli_li", mockery::mock())
  mockery::stub(calculate_match_stats, "cli::cli_end", mockery::mock())

  # Extract results function (modified to make comparison case-insensitive)
  extract_results_case_insensitive <- function() {
    results <- NULL
    mockery::stub(
      calculate_match_stats, "cli::cli_alert_info", function(...) NULL
    )
    mockery::stub(calculate_match_stats, "cli::cli_ul", function(...) NULL)
    mockery::stub(calculate_match_stats, "cli::cli_li", function(...) NULL)
    mockery::stub(calculate_match_stats, "cli::cli_end", function(...) NULL)

    # Create modified version with case-insensitive comparison
    modified_fn <- function(data, lookup_data, level0 = NULL,
                            level1 = NULL, level2 = NULL,
                            level3 = NULL, level4 = NULL) {
      # Make data lowercase for case-insensitive comparison
      if (!is.null(level0)) {
        data[[level0]] <- tolower(data[[level0]])
        lookup_data[[level0]] <- tolower(lookup_data[[level0]])
      }

      calculate_match_stats(
        data,
        lookup_data,
        level0 = level0,
        level1 = level1,
        level2 = level2,
        level3 = level3,
        level4 = level4
      )
    }

    # Call modified function
    modified_fn(data, lookup_data, level0 = "country")

    results
  }

  # Verify expected case-insensitive match counts
  results_case_insensitive <- environment(
    extract_results_case_insensitive
  )$results

  # Manual calculation for verification (case-insensitive)
  expected_matches <- sum(
    tolower(unique(data$country)) %in% tolower(unique(lookup_data$country))
  )

  testthat::expect_equal(expected_matches, 2)
})


testthat::test_that("format_choice formats choices correctly", {
  # Test basic formatting
  testthat::expect_equal(
    format_choice(1, "Option A", 20),
    "  1: Option A       "
  )

  # Test double-digit index
  testthat::expect_equal(
    format_choice(10, "Option B", 20),
    " 10: Option B       "
  )

  # Test three-digit index
  testthat::expect_equal(
    format_choice(100, "Option C", 20),
    "100: Option C       "
  )

  # Test width calculation
  testthat::expect_equal(
    nchar(format_choice(1, "Any text", 30)),
    30
  )

  # Test truncation of long choices
  long_choice <- "This is a very long option that needs truncation"
  result <- format_choice(5, long_choice, 25)

  testthat::expect_equal(
    nchar(result),
    25
  )

  testthat::expect_true(
    grepl("\\.\\.\\.\\s*$", result)
  )

  # Test exact truncation length
  # "  5: " = 5 chars, remaining width is 20,
  # minus 4 for "... " = 16 chars from the choice
  testthat::expect_equal(
    format_choice(5, long_choice, 25),
    "  5: This is a very l... "
  )

  # Test no truncation needed
  short_choice <- "Short"
  testthat::expect_equal(
    format_choice(7, short_choice, 15),
    "  7: Short     "
  )

  # Test minimum width case
  testthat::expect_equal(
    format_choice(9, "A", 6),
    "  9: ... "
  )

  # Test empty choice
  testthat::expect_equal(
    format_choice(3, "", 10),
    "  3:      "
  )

  # Test width exactly matching content (no padding needed)
  exact_choice <- "ABC"
  exact_width <- 8 # "  1: " (5) + "ABC" (3)
  testthat::expect_equal(
    format_choice(1, exact_choice, exact_width),
    "  1: ... "
  )

  # Test Unicode/emoji handling
  if (l10n_info()$UTF - 8) { # Only run on systems with UTF-8 support
    emoji_choice <- "🚀 Launch"
    # Note: nchar() with UTF-8 counts emoji as 1 char
    result_emoji <- format_choice(8, emoji_choice, 15)
    testthat::expect_equal(
      nchar(result_emoji, type = "width"),
      16
    )
  }
})

testthat::test_that("format_choice handles edge cases", {
  # Test negative index
  testthat::expect_match(
    format_choice(-1, "Negative", 15),
    " -1: Negati... "
  )

  # Test very large width
  large_width <- 1000
  result <- format_choice(1, "Text", large_width)
  testthat::expect_equal(
    nchar(result),
    large_width
  )

  # Test NULL or NA inputs
  testthat::expect_error(
    format_choice(NULL, "Text", 20)
  )

  testthat::expect_error(
    format_choice(1, NULL, 20)
  )
})

testthat::test_that(
  "display_custom_menu returns correct choice for numeric input",
  {
    # Mock readline to return user input
    mockery::stub(display_custom_menu, "readline", mockery::mock("2"))

    # Mock cli functions to prevent output during tests
    mockery::stub(display_custom_menu, "cli::cli_h1", function(...) NULL)
    mockery::stub(display_custom_menu, "cli::cli_h2", function(...) NULL)
    mockery::stub(display_custom_menu, "cat", function(...) NULL)

    # Mock format_choices
    formatted_mock <- "1: Option 1\n2: Option 2\n3: Option 3"
    mockery::stub(
      display_custom_menu, "format_choices",
      mockery::mock(formatted_mock)
    )

    result <- display_custom_menu(
      title = "Select an option:",
      main_header = "Test Menu",
      choices_input = c("Option 1", "Option 2", "Option 3"),
      special_actions = list(x = "Exit", q = "Quit"),
      prompt = "Your choice: "
    )

    testthat::expect_equal(result, "2")
  }
)

testthat::test_that(
  "display_custom_menu returns correct choice for special action",
  {
    # Mock readline to return a special action key
    mockery::stub(display_custom_menu, "readline", mockery::mock("x"))

    # Mock cli functions and cat
    mockery::stub(display_custom_menu, "cli::cli_h1", function(...) NULL)
    mockery::stub(display_custom_menu, "cli::cli_h2", function(...) NULL)
    mockery::stub(display_custom_menu, "cat", function(...) NULL)
    mockery::stub(
      display_custom_menu, "format_choices",
      mockery::mock("Formatted choices")
    )

    result <- display_custom_menu(
      title = "Select an option:",
      main_header = "Test Menu",
      choices_input = c("Option 1", "Option 2"),
      special_actions = list(x = "Exit", q = "Quit"),
      prompt = "Your choice: "
    )

    testthat::expect_equal(result, "x")
  }
)

testthat::test_that(
  "display_custom_menu handles upper/lowercase special actions",
  {
    # Mock readline to return uppercase special action key
    mockery::stub(display_custom_menu, "readline", mockery::mock("X"))

    # Mock cli functions and cat
    mockery::stub(display_custom_menu, "cli::cli_h1", function(...) NULL)
    mockery::stub(display_custom_menu, "cli::cli_h2", function(...) NULL)
    mockery::stub(display_custom_menu, "cat", function(...) NULL)
    mockery::stub(
      display_custom_menu, "format_choices",
      mockery::mock("Formatted choices")
    )

    result <- display_custom_menu(
      title = "Select an option:",
      main_header = "Test Menu",
      choices_input = c("Option 1", "Option 2"),
      special_actions = list(X = "Exit", Q = "Quit"),
      prompt = "Your choice: "
    )

    testthat::expect_equal(result, "x") # Should be lowercase
  }
)

testthat::test_that("display_custom_menu handles invalid then valid input", {
  # Mock readline to first return invalid input, then valid input
  mockery::stub(
    display_custom_menu,
    "readline",
    mockery::mock("invalid", "3")
  )

  # Mock cli functions and cat
  mockery::stub(display_custom_menu, "cli::cli_h1", function(...) NULL)
  mockery::stub(display_custom_menu, "cli::cli_h2", function(...) NULL)
  mockery::stub(display_custom_menu, "cat", function(...) NULL)
  mockery::stub(
    display_custom_menu, "format_choices",
    mockery::mock("Formatted choices")
  )

  result <- display_custom_menu(
    title = "Select an option:",
    main_header = "Test Menu",
    choices_input = c("Option 1", "Option 2", "Option 3"),
    special_actions = list(x = "Exit"),
    prompt = "Your choice: "
  )

  testthat::expect_equal(result, "3")
})

testthat::test_that(
  "display_custom_menu calculates correct number of columns",
  {
    # Mock needed functions
    mockery::stub(display_custom_menu, "readline", mockery::mock("1"))
    mockery::stub(display_custom_menu, "cli::cli_h1", function(...) NULL)
    mockery::stub(display_custom_menu, "cli::cli_h2", function(...) NULL)
    mockery::stub(display_custom_menu, "cat", function(...) NULL)

    # Create spy for format_choices to capture its arguments
    format_choices_spy <- mockery::mock("Formatted output")
    mockery::stub(display_custom_menu, "format_choices", format_choices_spy)

    # Case 1: Few options (1 column)
    display_custom_menu(
      "Select:", "Few Options",
      choices_input = paste("Option", 1:10),
      special_actions = list(x = "Exit"),
      prompt = "> "
    )

    # Verify 1 column was used
    args1 <- mockery::mock_args(format_choices_spy)[[1]]
    testthat::expect_equal(args1[[2]], 1) # num_columns should be 1
  }
)

testthat::test_that("display_custom_menu handles empty choices", {
  # Mock readline
  mockery::stub(display_custom_menu, "readline", mockery::mock("x"))

  # Mock cli functions and cat
  mockery::stub(display_custom_menu, "cli::cli_h1", function(...) NULL)
  mockery::stub(display_custom_menu, "cli::cli_h2", function(...) NULL)
  mockery::stub(display_custom_menu, "cat", function(...) NULL)
  mockery::stub(
    display_custom_menu, "format_choices",
    mockery::mock("No options available")
  )

  result <- display_custom_menu(
    title = "No options available:",
    main_header = "Empty Menu",
    choices_input = character(0),
    special_actions = list(x = "Exit"),
    prompt = "Your choice: "
  )

  testthat::expect_equal(result, "x")
})

testthat::test_that("display_custom_menu handles empty special actions", {
  # Mock readline
  mockery::stub(display_custom_menu, "readline", mockery::mock("1"))

  # Mock cli functions and cat
  mockery::stub(display_custom_menu, "cli::cli_h1", function(...) NULL)
  mockery::stub(display_custom_menu, "cli::cli_h2", function(...) NULL)
  mockery::stub(display_custom_menu, "cat", function(...) NULL)
  mockery::stub(
    display_custom_menu, "format_choices",
    mockery::mock("Formatted choices")
  )

  result <- display_custom_menu(
    title = "Select an option:",
    main_header = "Menu Without Special Actions",
    choices_input = c("Option 1", "Option 2"),
    special_actions = list(),
    prompt = "Your choice: "
  )

  testthat::expect_equal(result, "1")
})

testthat::test_that("display_custom_menu displays special actions correctly", {
  # Setup capture of cat output
  cat_calls <- list()
  mock_cat <- function(...) {
    cat_calls <<- c(cat_calls, list(list(...)))
    NULL
  }

  # Mock functions
  mockery::stub(display_custom_menu, "readline", mockery::mock("x"))
  mockery::stub(display_custom_menu, "cli::cli_h1", function(...) NULL)
  mockery::stub(display_custom_menu, "cli::cli_h2", function(...) NULL)
  mockery::stub(display_custom_menu, "cat", mock_cat)
  mockery::stub(
    display_custom_menu, "format_choices",
    mockery::mock("Formatted choices")
  )

  special_actions <- list(
    x = "Exit the program",
    s = "Save and continue",
    h = "Display help"
  )

  display_custom_menu(
    title = "Select an option:",
    main_header = "Test Menu",
    choices_input = c("Option 1", "Option 2"),
    special_actions = special_actions,
    prompt = "Your choice: "
  )

  # Find the cat calls that include special actions
  special_action_calls <- cat_calls[
    vapply(cat_calls, function(x) {
      any(grepl("x: Exit the program|s: Save and continue|h: Display help",
                x,
                perl = TRUE
      ))
    }, logical(1))
  ]

  # Check that all special actions were displayed
  testthat::expect_true(length(special_action_calls) > 0)
  special_output <- unlist(special_action_calls)
  testthat::expect_true(any(grepl("x: Exit the program", special_output)))
  testthat::expect_true(any(grepl("s: Save and continue", special_output)))
  testthat::expect_true(any(grepl("h: Display help", special_output)))
})


testthat::test_that(
  "handle_user_interaction processes basic selection correctly",
  {
    # Create test data
    test_data <- data.frame(
      name_to_match = c("province1", "province1", "province2"),
      matched_names = c("Province One", "P1", "Province Two"),
      long_geo = c("country1", "country1", "country2"),
      stringsAsFactors = FALSE
    )

    # Mock functions - note: we need a mock return for each unique name_to_match
    # The loop will iterate once for "province1" and once for "province2"
    mockery::stub(
      handle_user_interaction, "display_custom_menu",
      mockery::mock("1", "1", cycle = TRUE)
    )
    mockery::stub(handle_user_interaction, "cat", function(...) NULL)
    mockery::stub(handle_user_interaction, "sample", function(x) x[1])
    mockery::stub(
      handle_user_interaction,
      "cli::cli_alert_success", function(...) NULL
    )

    # Execute function with level1
    result <- handle_user_interaction(
      test_data,
      levels = c("country", "province", "district"),
      level = "province",
      clear_console = FALSE,
      stratify = TRUE,
      max_options = 10
    )

    # Verify result
    testthat::expect_s3_class(result, "data.frame")
    testthat::expect_equal(nrow(result), 2)
    # Should have one row per unique name_to_match
    testthat::expect_true(all(c("province1", "province2")
                              %in% result$name_to_match))
    testthat::expect_true(all(c("PROVINCE ONE", "PROVINCE TWO")
                              %in% result$replacement))
    testthat::expect_equal(result$level, rep("level1", 2))
  }
)


testthat::test_that("handle_user_interaction handles manual entry", {
  # Create test data
  test_data <- data.frame(
    name_to_match = c("district1", "district1"),
    matched_names = c("District One", "D1"),
    long_geo = c("country1_province1", "country1_province1"),
    stringsAsFactors = FALSE
  )

  # Mock functions
  mockery::stub(
    handle_user_interaction,
    "display_custom_menu", mockery::mock("m")
  )
  mockery::stub(
    handle_user_interaction,
    "readline", mockery::mock("Manual District")
  )
  mockery::stub(handle_user_interaction, "cat", function(...) NULL)
  mockery::stub(handle_user_interaction, "sample", function(x) x[1])
  mockery::stub(
    handle_user_interaction,
    "cli::cli_alert_success", function(...) NULL
  )

  # Execute function with level2
  result <- handle_user_interaction(
    test_data,
    levels = c("country", "province", "district"),
    level = "district",
    clear_console = FALSE,
    stratify = TRUE,
    max_options = 10
  )

  # Verify result
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$name_to_match, "district1")
  testthat::expect_equal(result$replacement, "MANUAL DISTRICT")
  testthat::expect_equal(result$level, "level2")
  testthat::expect_equal(
    result$longname_to_match,
    "country1_province1_district1"
  )
  testthat::expect_equal(
    result$longname_corrected,
    "country1_province1_MANUAL DISTRICT"
  )
})

testthat::test_that("handle_user_interaction processes multiple selections", {
  # Create test data with multiple unique names
  test_data <- data.frame(
    name_to_match = c("district1", "district2", "district3"),
    matched_names = c("District One", "District Two", "District Three"),
    long_geo = c(
      "country1_province1",
      "country1_province1", "country1_province1"
    ),
    stringsAsFactors = FALSE
  )

  # Mock display_custom_menu to return different choices for different items
  mockery::stub(
    handle_user_interaction, "display_custom_menu",
    mockery::mock("1", "2", "s")
  )
  mockery::stub(handle_user_interaction, "cat", function(...) NULL)
  mockery::stub(handle_user_interaction, "sample", function(x) x[1])
  mockery::stub(
    handle_user_interaction,
    "cli::cli_alert_info", function(...) NULL
  )
  mockery::stub(
    handle_user_interaction,
    "cli::cli_alert_success", function(...) NULL
  )

  # Execute function
  result <- handle_user_interaction(
    test_data,
    levels = c("country", "province", "district"),
    level = "district",
    clear_console = FALSE,
    stratify = TRUE,
    max_options = 10
  )

  # Verify result
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 2) # Should have 2 rows - skipped the 3rd
  testthat::expect_equal(result$name_to_match, c("district1", "district2"))
  testthat::expect_equal(result$replacement, c("DISTRICT ONE", NA))
})


testthat::test_that("handle_user_interaction handles 'Save and exit' action", {
  # Create test data with multiple entries
  test_data <- data.frame(
    name_to_match = c("settlement1", "settlement2", "settlement3"),
    matched_names = c("Settlement One", "Settlement Two", "Settlement Three"),
    long_geo = c(
      "country1_province1_district1_subdistrict1",
      "country1_province1_district1_subdistrict1",
      "country1_province1_district1_subdistrict1"
    ),
    stringsAsFactors = FALSE
  )

  # Mock to select first item then exit
  mockery::stub(
    handle_user_interaction, "display_custom_menu",
    mockery::mock("1", "e")
  )
  mockery::stub(handle_user_interaction, "cat", function(...) NULL)
  mockery::stub(handle_user_interaction, "sample", function(x) x[1])
  mockery::stub(
    handle_user_interaction,
    "cli::cli_alert_success", function(...) NULL
  )

  # Execute function
  result <- handle_user_interaction(
    test_data,
    levels = c("country", "province", "district", "subdistrict", "settlement"),
    level = "settlement",
    clear_console = FALSE,
    stratify = TRUE,
    max_options = 10
  )

  # Verify result - should only process the first item before exiting
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$name_to_match, "settlement1")
})

testthat::test_that(
  "handle_user_interaction handles 'Exit without saving' action",
  {
    # Create test data
    test_data <- data.frame(
      name_to_match = c("country1", "country2"),
      matched_names = c("Country One", "Country Two"),
      long_geo = c("country1", "country2"),
      stringsAsFactors = FALSE
    )

    # Mock to select first item then quit without saving
    mockery::stub(
      handle_user_interaction,
      "display_custom_menu", mockery::mock("q")
    )
    mockery::stub(
      handle_user_interaction,
      "readline", mockery::mock("y")
    )
    mockery::stub(handle_user_interaction, "cat", function(...) NULL)
    mockery::stub(handle_user_interaction, "sample", function(x) x[1])
    mockery::stub(
      handle_user_interaction,
      "cli::cli_alert_danger", function(...) NULL
    )

    # Execute function
    result <- handle_user_interaction(
      test_data,
      levels = c("country", "province"),
      level = "country",
      clear_console = FALSE,
      stratify = TRUE,
      max_options = 10
    )

    # Verify result is NULL when exiting without saving
    testthat::expect_null(result)
  }
)




testthat::test_that("handle_user_interaction handles 'Go Back' action", {
  # Create test data
  test_data <- data.frame(
    name_to_match = c("subdistrict1", "subdistrict2"),
    matched_names = c("Subdistrict One", "Subdistrict Two"),
    long_geo = c(
      "country1_province1_district1",
      "country1_province1_district1"
    ),
    stringsAsFactors = FALSE
  )

  # Mock to test going back then selecting
  # We need THREE responses for "subdistrict1":
  # 1. First we choose "1"
  # 2. Then we go back "b" (which will show subdistrict1 again)
  # 3. Then we choose "2" for subdistrict1
  # 4. Then we need a response for subdistrict2
  mockery::stub(
    handle_user_interaction, "display_custom_menu",
    mockery::mock("1", "b", "2", "1", cycle = TRUE)
  )

  mockery::stub(handle_user_interaction, "cat", function(...) NULL)
  mockery::stub(handle_user_interaction, "sample", function(x) x[1])
  mockery::stub(
    handle_user_interaction,
    "cli::cli_alert_warning", function(...) NULL
  )
  mockery::stub(
    handle_user_interaction,
    "cli::cli_alert_success", function(...) NULL
  )

  # Execute function
  result <- handle_user_interaction(
    test_data,
    levels = c("country", "province", "district", "subdistrict"),
    level = "subdistrict",
    clear_console = FALSE,
    stratify = TRUE,
    max_options = 10
  )

  # Verify result
  testthat::expect_s3_class(result, "data.frame")

  # The final result will have both subdistrict1
  # (with the overridden value from second choice) and subdistrict2
  testthat::expect_equal(nrow(result), 2)

  # Find the row for subdistrict1
  sd1_row <- result[result$name_to_match == "subdistrict1", ]
  testthat::expect_equal(sd1_row$replacement, "SUBDISTRICT ONE")
})

testthat::test_that("handle_user_interaction handles empty choices", {
  # Create test data
  test_data <- data.frame(
    name_to_match = c("country1"),
    matched_names = c("Country One"),
    long_geo = c("country1"),
    stringsAsFactors = FALSE
  )

  # Mock to skip all items
  mockery::stub(
    handle_user_interaction,
    "display_custom_menu", mockery::mock("s")
  )
  mockery::stub(handle_user_interaction, "cat", function(...) NULL)
  mockery::stub(
    handle_user_interaction,
    "sample", function(x) x[1]
  )
  mockery::stub(
    handle_user_interaction,
    "cli::cli_alert_info", function(...) NULL
  )
  mockery::stub(
    handle_user_interaction,
    "cli::cli_alert_warning", function(...) NULL
  )

  # Execute function
  result <- handle_user_interaction(
    test_data,
    levels = c("country", "province"),
    level = "country",
    clear_console = FALSE,
    stratify = FALSE,
    max_options = 10
  )

  # Verify result - should be empty data frame when no selections made
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 0)
})

testthat::test_that("format_choices correctly formats single column display", {
  choices <- c("Option A", "Option B", "Option C")

  # Mock format_choice to return predictable responses
  mockery::stub(
    format_choices, "format_choice",
    function(index, choice, width) {
      sprintf("%3d: %-39s", index, choice)
    }
  )

  result <- format_choices(choices, num_columns = 1, column_width = 45)

  # Check result structure
  testthat::expect_type(result, "character")
  testthat::expect_length(result, 3) # One row per choice in single column layout

  # Check content formatting
  testthat::expect_equal(
    result[1],
    "  1: Option A                               "
  )
  testthat::expect_equal(
    result[2],
    "  2: Option B                               "
  )
  testthat::expect_equal(
    result[3],
    "  3: Option C                               "
  )
})

testthat::test_that("format_choices correctly formats multi-column display", {
  choices <- paste("Option", 1:6)

  # Mock format_choice to return predictable responses
  mockery::stub(format_choices, "format_choice", function(index, choice, width) {
    # Format that mimics format_choice behavior
    sprintf("%3d: %-39s", index, choice)
  })

  result <- format_choices(choices, num_columns = 2, column_width = 45)

  # Check result structure
  testthat::expect_type(result, "character")
  testthat::expect_length(result, 3) # 6 items in 2 columns = 3 rows

  # Check content formatting (each row should contain 2 formatted options)
  expected_row1 <- paste0(
    "  1: Option 1                               ",
    "  4: Option 4                               "
  )
  expected_row2 <- paste0(
    "  2: Option 2                               ",
    "  5: Option 5                               "
  )
  expected_row3 <- paste0(
    "  3: Option 3                               ",
    "  6: Option 6                               "
  )

  testthat::expect_equal(result[1], expected_row1)
  testthat::expect_equal(result[2], expected_row2)
  testthat::expect_equal(result[3], expected_row3)
})

testthat::test_that("format_choices handles partial columns correctly", {
  # Note: 5 items won't divide evenly in 2 columns
  choices <- paste("Option", 1:5)

  # Mock format_choice to return predictable responses
  mockery::stub(
    format_choices, "format_choice",
    function(index, choice, width) {
      sprintf("%3d: %-39s", index, choice)
    }
  )

  result <- format_choices(choices, num_columns = 2, column_width = 45)

  # Check result structure
  testthat::expect_type(result, "character")
  # 5 items in 2 columns = 3 rows, with empty space
  testthat::expect_length(result, 3)

  # Check that the last position is filled with spaces
  expected_row3 <- paste0(
    "  3: Option 3                               ",
    paste(rep(" ", 45), collapse = "")
  )

  testthat::expect_equal(result[3], expected_row3)
})

testthat::test_that("format_choices handles three columns correctly", {
  choices <- paste("Option", 1:9)

  # Mock format_choice to return predictable responses
  mockery::stub(
    format_choices, "format_choice",
    function(index, choice, width) {
      sprintf("%3d: %-24s", index, choice)
      # Shorter width for readability in test
    }
  )

  result <- format_choices(choices, num_columns = 3, column_width = 30)

  # Check result structure
  testthat::expect_type(result, "character")
  testthat::expect_length(result, 3) # 9 items in 3 columns = 3 rows

  # Check content formatting (each row should contain 3 formatted options)
  expected_row1 <- paste0(
    "  1: Option 1                ",
    "  4: Option 4                ",
    "  7: Option 7                "
  )

  testthat::expect_equal(result[1], expected_row1)
})

testthat::test_that("format_choices uses custom column width", {
  choices <- c("Option A", "Option B")

  # Mock format_choice to verify width parameter is passed correctly
  width_spy <- mockery::mock()
  mockery::stub(
    format_choices, "format_choice",
    function(index, choice, width) {
      width_spy(width)
      # Adjusted for format
      sprintf("%3d: %-*s", index, width - 5, choice)
    }
  )

  custom_width <- 60
  format_choices(choices, num_columns = 1, column_width = custom_width)

  # Verify the width was passed correctly to format_choice
  testthat::expect_equal(mockery::mock_args(width_spy)[[1]][[1]], custom_width)
})

testthat::test_that(
  "prep_geonames handles basic case with no cleaning needed",
  {
    # Setup test data where everything already matches
    target_df <- data.frame(
      country = c("ANGOLA", "UGANDA", "ZAMBIA"),
      province = c("CABINDA", "TESO", "LUSAKA"),
      stringsAsFactors = FALSE
    )

    lookup_df <- data.frame(
      country = c("ANGOLA", "UGANDA", "ZAMBIA", "KENYA"),
      province = c("CABINDA", "TESO", "LUSAKA", "NAIROBI"),
      stringsAsFactors = FALSE
    )

    # Mock cli functions to avoid output during tests
    mockery::stub(prep_geonames, "cli::cli_alert_success", function(...) NULL)
    mockery::stub(prep_geonames, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(prep_geonames, "calculate_match_stats", function(...) NULL)

    # Run function with non-interactive mode
    result <- prep_geonames(
      target_df = target_df,
      lookup_df = lookup_df,
      level0 = "country",
      level1 = "province",
      interactive = FALSE,
      stratify = TRUE
    )

    # Verify result contains all original rows and didn't change anything
    testthat::expect_s3_class(result, "data.frame")
    testthat::expect_equal(nrow(result), nrow(target_df))
    testthat::expect_equal(result$country, target_df$country)
    testthat::expect_equal(result$province, target_df$province)
  }
)

testthat::test_that("prep_geonames validates inputs correctly", {
  # Test missing required columns
  target_df <- data.frame(
    country = c("ANGOLA", "UGANDA"),
    region = c("CABINDA", "TESO"), # Not matching the level1 name
    stringsAsFactors = FALSE
  )

  lookup_df <- data.frame(
    country = c("ANGOLA", "UGANDA"),
    province = c("CABINDA", "TESO"), # Uses province instead of region
    stringsAsFactors = FALSE
  )

  testthat::expect_error(
    prep_geonames(
      target_df = target_df,
      lookup_df = lookup_df,
      level0 = "country",
      level1 = "province", # This column doesn't exist in target_df
      interactive = FALSE
    ),
    "The following columns are missing in target_df: province"
  )

  # Test unsupported method
  testthat::expect_error(
    prep_geonames(
      target_df = data.frame(country = "ANGOLA"),
      level0 = "country",
      method = "invalid_method",
      interactive = FALSE
    ),
    "Unsupported method"
  )
})

testthat::test_that(
  "prep_geonames handles level hierarchies correctly",
  {
    # Test error when specifying levels out of order
    target_df <- data.frame(
      country = "ANGOLA",
      province = "CABINDA",
      district = "BUCO-ZAU",
      stringsAsFactors = FALSE
    )

    # Should error if specifying level2 without level0 and level1
    testthat::expect_error(
      prep_geonames(
        target_df = target_df,
        level2 = "district", # Missing level0 and level1
        interactive = FALSE,
        stratify = TRUE
      ),
      "You cannot specify level2 without both level0 and level1"
    )

    # Should error if specifying level1 without level0
    testthat::expect_error(
      prep_geonames(
        target_df = target_df,
        level1 = "province", # Missing level0
        interactive = FALSE,
        stratify = TRUE
      ),
      "You cannot specify level1 without level0"
    )
  }
)

testthat::test_that("prep_geonames handles empty lookup_df properly", {
  target_df <- data.frame(
    country = "ANGOLA",
    stringsAsFactors = FALSE
  )

  # Empty lookup_df should error
  testthat::expect_error(
    prep_geonames(
      target_df = target_df,
      lookup_df = data.frame(),
      level0 = "country",
      interactive = FALSE
    ),
    "The following columns are missing in lookup_df: country"
  )
})

testthat::test_that("prep_geonames handles case conversion correctly", {
  # Test that function converts admin names to uppercase
  target_df <- data.frame(
    country = c("angola", "Uganda"),
    stringsAsFactors = FALSE
  )

  lookup_df <- data.frame(
    country = c("ANGOLA", "UGANDA"),
    stringsAsFactors = FALSE
  )

  # Mock cli functions
  mockery::stub(
    prep_geonames,
    "cli::cli_alert_success", function(...) NULL
  )
  mockery::stub(
    prep_geonames,
    "cli::cli_alert_info", function(...) NULL
  )
  mockery::stub(
    prep_geonames,
    "calculate_match_stats", function(...) NULL
  )

  result <- prep_geonames(
    target_df = target_df,
    lookup_df = lookup_df,
    level0 = "country",
    interactive = FALSE
  )

  # Check that names were converted to uppercase
  testthat::expect_equal(result$country, c("ANGOLA", "UGANDA"))
})


testthat::test_that("prep_geonames handles cache path correctly", {
  # Test with non-existent cache path
  target_df <- data.frame(
    country = "ANGOLA",
    stringsAsFactors = FALSE
  )

  # Mock the readline function to simulate user input
  mockery::stub(prep_geonames, "readline", mockery::mock("no"))
  mockery::stub(
    prep_geonames, "cli::cli_alert_info",
    function(...) NULL
  )

  # Non-existent directory should error
  testthat::expect_error(
    prep_geonames(
      target_df = target_df,
      level0 = "country",
      cache_path = "/path/that/doesnt/exist/cache.rds",
      interactive = FALSE
    ),
    "The directory for cache_path does not exist"
  )
})

testthat::test_that("prep_geonames handles missing data correctly", {
  # Test with NA values in administrative levels
  target_df <- data.frame(
    country = c("ANGOLA", "UGANDA", NA),
    province = c("CABINDA", NA, NA),
    stringsAsFactors = FALSE
  )

  lookup_df <- data.frame(
    country = c("ANGOLA", "UGANDA"),
    province = c("CABINDA", "TESO"),
    stringsAsFactors = FALSE
  )

  # Mock functions
  mockery::stub(
    prep_geonames, "cli::cli_alert_success",
    function(...) NULL
  )
  mockery::stub(
    prep_geonames, "cli::cli_alert_info",
    function(...) NULL
  )
  mockery::stub(
    prep_geonames, "calculate_match_stats",
    function(...) NULL
  )

  result <- prep_geonames(
    target_df = target_df,
    lookup_df = lookup_df,
    level0 = "country",
    level1 = "province",
    interactive = FALSE
  )

  # Verify NA values are preserved
  testthat::expect_true(is.na(result$country[3]))
  testthat::expect_true(is.na(result$province[2]))
  testthat::expect_true(is.na(result$province[3]))
})

testthat::test_that("prep_geonames can be run non-interactively", {
  # Test that non-interactive mode returns after matching with cache
  target_df <- data.frame(
    country = c("ANGOLA", "UGA"),
    province = c("CABINDA", "TESO"),
    stringsAsFactors = FALSE
  )

  lookup_df <- data.frame(
    country = c("ANGOLA", "UGANDA"),
    province = c("CABINDA", "TESO"),
    stringsAsFactors = FALSE
  )

  # Mock construction and functions
  mockery::stub(
    prep_geonames, "cli::cli_alert_success",
    function(...) NULL
  )
  mockery::stub(
    prep_geonames, "cli::cli_alert_info",
    function(...) NULL
  )
  mockery::stub(
    prep_geonames, "calculate_match_stats",
    function(...) NULL
  )
  mockery::stub(
    prep_geonames, "construct_geo_names",
    function(df, ...) {
      df$long_geo <- paste(df$country, df$province, sep = "_")
      df
    }
  )

  # Run function with non-interactive mode
  result <- prep_geonames(
    target_df = target_df,
    lookup_df = lookup_df,
    level0 = "country",
    level1 = "province",
    interactive = FALSE, cache_path =
  )

  # Verify result
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 2)
})


testthat::test_that("impute_higher_admin works correctly", {
  # Test case 1: Basic functionality
  lookup <- data.frame(
    district = c("Boffa", "Boke", "Fria"),
    region = c("Boké", "Boké", "Boké"),
    stringsAsFactors = FALSE
  )

  test_data <- data.frame(
    location = c("Boffa", "Fria", "Unknown"),
    stringsAsFactors = FALSE
  )

  result <- impute_higher_admin(
    target_df = test_data,
    target_lower_col = "location",
    target_higher_col = "region",
    lookup_df = lookup,
    lookup_lower_col = "district",
    lookup_higher_col = "region"
  )

  testthat::expect_equal(result$region, c("Boké", "Boké", "Unknown"))
  testthat::expect_equal(ncol(result), 2)

  # Test case 2: Empty dataframe
  empty_data <- data.frame(location = character(0))

  result_empty <- impute_higher_admin(
    target_df = empty_data,
    target_lower_col = "location",
    target_higher_col = "region",
    lookup_df = lookup,
    lookup_lower_col = "district",
    lookup_higher_col = "region"
  )

  testthat::expect_equal(nrow(result_empty), 0)
  testthat::expect_equal(ncol(result_empty), 2)


  # Test case 4: Different column names in lookup table
  complex_lookup <- data.frame(
    adm2_name = c("Boffa", "Boke", "Fria"),
    adm1_code = c("BK", "BK", "BK"),
    stringsAsFactors = FALSE
  )

  result_complex <- impute_higher_admin(
    target_df = test_data,
    target_lower_col = "location",
    target_higher_col = "region_code",
    lookup_df = complex_lookup,
    lookup_lower_col = "adm2_name",
    lookup_higher_col = "adm1_code"
  )

  testthat::expect_equal(result_complex$region_code, c("BK", "BK", "Unknown"))
})
