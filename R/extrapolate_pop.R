#' Extrapolate Population Estimates for Future Years
#'
#' Fills missing population values for specified future years by applying
#' a multiplier to the previous year's population estimate, grouped by location.
#' Supports applying the same multiplier to all years or custom multipliers per
#' year. Automatically expands data to a full year range.
#'
#' @param data A data frame containing population data with year, population,
#'   and location columns.
#' @param year_col The name of the year column (unquoted or character).
#' @param pop_col The name of the population column (unquoted or character).
#' @param group_cols A character vector of grouping column names defining
#'   location.
#' @param years_to_extrap A vector of target years to extrapolate. Can be
#'   unnamed (e.g., c(2021, 2022)) or named with specific multipliers
#'   (e.g., c(`2021` = 1.5, `2022` = 1.3)).
#' @param multiplier A single numeric multiplier to apply to all years when
#'   `years_to_extrap` is unnamed (e.g., 1.5). Ignored if `years_to_extrap`
#'   provides named multipliers.
#'
#' @return A data frame with updated population estimates for the specified
#'   years.
#'
#' @examples
#'
#' # Dummy data for 3 districts over 4 years
#' dummy_data <- expand.grid(
#'  adm0 = "COUNTRYX",
#'  adm1 = c("RegionA", "RegionB"),
#'  adm2 = c("District1", "District2"),
#'  year = 2018:2020
#' ) |>
#'  dplyr::mutate(
#'    adm3 = paste0(adm2, "_Subarea"),
#'    total_pop = sample(1000:5000, size = dplyr::n(), replace = TRUE)
#'  ) |>
#'  dplyr::arrange(adm0, adm1, adm2, year)
#'
#' # Example with the same multiplier for all years
#' extrapolate_pop(
#'   data = dummy_data,
#'   year_col = "year",
#'   pop_col = "total_pop",
#'   group_cols = c("adm0", "adm1", "adm2", "adm3"),
#'   years_to_extrap = c(2021, 2022),
#'   multiplier = 1.5
#' )
#'
#' # Example with different multipliers for each year
#' extrapolate_pop(
#'   data = dummy_data,
#'   year_col = "year",
#'   pop_col = "total_pop",
#'   group_cols = c("adm0", "adm1", "adm2", "adm3"),
#'   years_to_extrap = c(`2021` = 1.5, `2022` = 1.3)
#' )
#'
#' @export
extrapolate_pop <- function(
    data,
    year_col,
    pop_col,
    group_cols,
    years_to_extrap,
    multiplier = NULL
) {

  year_sym <- rlang::sym(year_col)
  pop_sym <- rlang::sym(pop_col)
  group_syms <- rlang::syms(group_cols)
  min_year <- min(dplyr::pull(data, !!year_sym), na.rm = TRUE)

  # get the max year for named and unamed inputs
  if (!is.null(names(years_to_extrap)) && all(names(years_to_extrap) != "")) {
    # Named vector: extract from names
    max_year <- max(as.numeric(names(years_to_extrap)), na.rm = TRUE)
  } else {
    # Unnamed vector: extract from values
    max_year <- max(as.numeric(years_to_extrap), na.rm = TRUE)
  }

  year_range <- min_year:max_year

  data <- data |>
    dplyr::group_by(!!!group_syms) |>
    tidyr::complete(!!year_sym := year_range) |>
    dplyr::arrange(!!year_sym)

  # Determine multipliers per year
  if (is.null(names(years_to_extrap)) && !is.null(multiplier)) {
    # Apply the same multiplier to all years if unnamed
    multipliers <- setNames(
      rep(multiplier, length(years_to_extrap)),
      as.character(years_to_extrap)
    )
  } else {
    # Use provided named multipliers
    multipliers <- years_to_extrap
  }

  # Process each target year
  for (target_year in as.numeric(names(multipliers))) {
    mult <- multipliers[[as.character(target_year)]]

    data <- data |>
      dplyr::mutate(
        !!pop_sym := dplyr::case_when(
          !!year_sym == target_year ~ dplyr::lag(!!pop_sym) * mult,
          TRUE ~ !!pop_sym
        ),
        !!pop_sym := round(!!pop_sym)
      )
  }

  data <- data |> dplyr::ungroup()
  return(data)
}
