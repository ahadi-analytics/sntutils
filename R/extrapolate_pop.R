#' Extrapolate Population Estimates for Target Years
#'
#' Fills missing population values for specified years by applying a multiplier
#' to the nearest available year within each location group.
#' Supports extending forward beyond the latest data or backward before the
#' earliest data. Automatically expands data to a full year range.
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
#' # Example with backward extrapolation from a single year
#' backward_data <- tibble::tibble(
#'   adm0 = "COUNTRYX",
#'   year = 2024,
#'   total_pop = 5000
#' )
#'
#' extrapolate_pop(
#'   data = backward_data,
#'   year_col = "year",
#'   pop_col = "total_pop",
#'   group_cols = "adm0",
#'   years_to_extrap = 2020:2023,
#'   multiplier = 1.2
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

  # determine target years from input
  if (!is.null(names(years_to_extrap)) && all(names(years_to_extrap) != "")) {
    target_years <- as.numeric(names(years_to_extrap))
  } else {
    target_years <- as.numeric(years_to_extrap)
  }

  if (base::anyNA(target_years)) {
    cli::cli_abort("`years_to_extrap` must only contain numeric years.")
  }

  target_years <- base::sort(base::unique(target_years))

  data_years <- dplyr::pull(data, !!year_sym)
  min_year <- base::min(c(data_years, target_years), na.rm = TRUE)
  max_year <- base::max(c(data_years, target_years), na.rm = TRUE)

  year_range <- base::seq(min_year, max_year)

  data <- data |>
    dplyr::group_by(!!!group_syms) |>
    tidyr::complete(!!year_sym := year_range) |>
    dplyr::arrange(!!year_sym)

  has_named <- !base::is.null(names(years_to_extrap)) &&
    base::all(names(years_to_extrap) != "")

  default_multiplier <- NA_real_

  if (!has_named) {
    if (base::is.null(multiplier)) {
      cli::cli_abort(
        "`multiplier` must be provided when `years_to_extrap` is unnamed."
      )
    }
    multipliers <- stats::setNames(
      rep(multiplier, length(target_years)),
      base::as.character(target_years)
    )
    default_multiplier <- multiplier
  } else {
    multipliers <- stats::setNames(
      base::as.numeric(years_to_extrap),
      base::as.character(target_years)
    )
    default_multiplier <- NA_real_
  }

  year_col_chr <- rlang::as_name(year_sym)
  pop_col_chr <- rlang::as_name(pop_sym)

  get_multiplier <- function(year) {
    name <- base::as.character(year)
    value <- multipliers[name]
    if (base::length(value) && !base::is.na(value)) {
      return(value[[1]])
    }
    if (!base::is.na(default_multiplier)) {
      return(default_multiplier)
    }
    NA_real_
  }

  fill_group <- function(group_df) {
    group_df <- group_df[base::order(group_df[[year_col_chr]]), , drop = FALSE]

    non_missing <- !base::is.na(group_df[[pop_col_chr]])
    if (!base::any(non_missing)) {
      return(group_df)
    }

    years_vec <- group_df[[year_col_chr]]
    pops_vec <- group_df[[pop_col_chr]]
    target_mask <- years_vec %in% target_years

    step_multiplier <- function(start_year, end_year) {
      if (start_year == end_year) {
        return(1)
      }
      sequence_years <- base::seq(start_year + 1, end_year)
      mults <- base::vapply(
        sequence_years,
        get_multiplier,
        numeric(1),
        USE.NAMES = FALSE
      )
      if (base::any(base::is.na(mults))) {
        missing_years <- sequence_years[base::is.na(mults)]
        cli::cli_abort(
          cli::format_inline(
            paste0(
              "Need multipliers for years {missing_years} between ",
              "{start_year} and {end_year}."
            )
          )
        )
      }
      base::prod(mults)
    }

    unresolved <- target_mask & base::is.na(pops_vec)
    changed <- TRUE

    while (base::any(unresolved) && changed) {
      changed <- FALSE

      for (idx in base::which(unresolved)) {
        current_year <- years_vec[idx]

        prev_idx <- utils::tail(
          base::which(!base::is.na(pops_vec) & years_vec < current_year),
          n = 1
        )
        next_idx <- utils::head(
          base::which(!base::is.na(pops_vec) & years_vec > current_year),
          n = 1
        )

        prev_gap <- if (base::length(prev_idx)) {
          current_year - years_vec[prev_idx]
        } else {
          Inf
        }

        next_gap <- if (base::length(next_idx)) {
          years_vec[next_idx] - current_year
        } else {
          Inf
        }

        use_prev <- prev_gap <= next_gap

        if (use_prev && base::is.finite(prev_gap)) {
          prev_year <- years_vec[prev_idx]
          prev_value <- pops_vec[prev_idx]
          multiplier_chain <- step_multiplier(prev_year, current_year)
          new_value <- base::round(prev_value * multiplier_chain)
          pops_vec[idx] <- new_value
          changed <- TRUE
          next
        }

        if (base::length(next_idx)) {
          next_year <- years_vec[next_idx]
          next_value <- pops_vec[next_idx]
          multiplier_chain <- step_multiplier(current_year, next_year)
          if (multiplier_chain == 0) {
            cli::cli_abort(
              cli::format_inline(
                paste0(
                  "Need non-zero multipliers between {current_year} and ",
                  "{next_year}."
                )
              )
            )
          }
          new_value <- base::round(next_value / multiplier_chain)
          pops_vec[idx] <- new_value
          changed <- TRUE
          next
        }
      }

      unresolved <- target_mask & base::is.na(pops_vec)
    }

    group_df[[pop_col_chr]] <- pops_vec
    group_df
  }

  data <- data |>
    dplyr::group_split(.keep = TRUE) |>
    base::lapply(fill_group) |>
    dplyr::bind_rows()

  return(data)
}
