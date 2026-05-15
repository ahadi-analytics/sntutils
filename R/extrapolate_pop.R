#' Extrapolate Population Estimates for Target Years
#'
#' Fills missing population values for specified years by applying multipliers
#' to the nearest available year within each location group. Can handle multiple
#' population columns simultaneously. Supports extending forward beyond the latest
#' data or backward before the earliest data. Automatically calculates growth
#' rates from existing data when multipliers are not provided.
#'
#' @param data A data frame containing population data with year, multiple
#'   population columns, and location columns.
#' @param year_col The name of the year column (unquoted or character).
#' @param pop_cols A character vector of population column names to extrapolate.
#' @param group_cols A character vector of grouping column names defining
#'   location.
#' @param years_to_extrap A vector of target years to extrapolate. Can be
#'   unnamed (e.g., c(2021, 2022)) or named with specific multipliers
#'   (e.g., c(`2021` = 1.5, `2022` = 1.3)).
#' @param multiplier A single numeric multiplier to apply to all years when
#'   `years_to_extrap` is unnamed (e.g., 1.5). Can also be a named list/vector
#'   with multipliers for each population column. For year-specific multipliers,
#'   use a nested list structure like `list(pop_total = c('2021' = 1.03, '2022' = 1.025))`.
#'   If NULL and sufficient data exists, growth rates will be calculated automatically.
#'
#' @return A data frame with updated population estimates for all specified
#'   population columns and years.
#'
#' @examples
#'
#' # Dummy data for 3 districts over 3 years with multiple population columns
#' dummy_data <- expand.grid(
#'  adm0 = "COUNTRYX",
#'  adm1 = c("RegionA", "RegionB"),
#'  adm2 = c("District1", "District2"),
#'  year = 2018:2020
#' ) |>
#'  dplyr::mutate(
#'    adm3 = paste0(adm2, "_Subarea"),
#'    pop_total = sample(1000:5000, size = dplyr::n(), replace = TRUE),
#'    pop_0_11m = pop_total * 0.08,
#'    pop_0_4y = pop_total * 0.15,
#'    pop_u15 = pop_total * 0.45
#'  ) |>
#'  dplyr::arrange(adm0, adm1, adm2, year)
#'
#' # Example with automatic growth rate calculation (no multiplier provided)
#' extrapolate_pop(
#'   data = dummy_data,
#'   year_col = "year",
#'   pop_cols = c("pop_total", "pop_0_11m", "pop_0_4y", "pop_u15"),
#'   group_cols = c("adm0", "adm1", "adm2", "adm3"),
#'   years_to_extrap = c(2021, 2022)
#' )
#'
#' # Example with same multiplier for all columns
#' extrapolate_pop(
#'   data = dummy_data,
#'   year_col = "year",
#'   pop_cols = c("pop_total", "pop_0_11m", "pop_0_4y", "pop_u15"),
#'   group_cols = c("adm0", "adm1", "adm2", "adm3"),
#'   years_to_extrap = c(2021, 2022),
#'   multiplier = 1.03
#' )
#'
#' # Example with different multipliers for each column
#' extrapolate_pop(
#'   data = dummy_data,
#'   year_col = "year",
#'   pop_cols = c("pop_total", "pop_0_11m", "pop_0_4y", "pop_u15"),
#'   group_cols = c("adm0", "adm1", "adm2", "adm3"),
#'   years_to_extrap = c(2021, 2022),
#'   multiplier = list(
#'     pop_total = 1.025,
#'     pop_0_11m = 1.030,
#'     pop_0_4y = 1.028,
#'     pop_u15 = 1.020
#'   )
#' )
#'
#' # Example with year-specific multipliers for each column
#' extrapolate_pop(
#'   data = dummy_data,
#'   year_col = "year",
#'   pop_cols = c("pop_total", "pop_0_11m"),
#'   group_cols = c("adm0", "adm1", "adm2", "adm3"),
#'   years_to_extrap = c(2021, 2022),
#'   multiplier = list(
#'     pop_total = c(`2021` = 1.03, `2022` = 1.025),
#'     pop_0_11m = c(`2021` = 1.035, `2022` = 1.030)
#'   )
#' )
#'
#' @export
extrapolate_pop <- function(
    data,
    year_col,
    pop_cols,
    group_cols,
    years_to_extrap,
    multiplier = NULL
) {

  year_sym <- rlang::sym(year_col)
  group_syms <- rlang::syms(group_cols)
  
  # validate pop_cols exist in data
  missing_cols <- setdiff(pop_cols, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Missing population columns: {missing_cols}")
  }

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

  observed_data <- data

  data <- data |>
    dplyr::group_by(!!!group_syms) |>
    tidyr::complete(!!year_sym := year_range) |>
    dplyr::arrange(!!year_sym)

  # helper function to calculate growth rates automatically
  calculate_growth_rates <- function(data, year_col, pop_cols, group_cols) {
    year_col_chr <- year_col
    growth_rates <- list()
    
    for (pop_col in pop_cols) {
      col_clean <- data |>
        dplyr::filter(!base::is.na(.data[[year_col_chr]])) |>
        dplyr::filter(!base::is.na(.data[[pop_col]]))

      if (base::nrow(col_clean) == 0) {
        growth_rates[[pop_col]] <- 1.02
        cli::cli_warn("No growth data for {pop_col}, using default 2% growth")
        next
      }

      if (base::length(group_cols) > 0) {
        growth_values <- col_clean |>
          dplyr::group_by(
            dplyr::across(dplyr::all_of(group_cols))
          ) |>
          dplyr::arrange(.data[[year_col_chr]], .by_group = TRUE) |>
          dplyr::mutate(
            prev_pop = dplyr::lag(.data[[pop_col]]),
            growth_ratio = dplyr::if_else(
              prev_pop > 0,
              .data[[pop_col]] / prev_pop,
              NA_real_
            )
          ) |>
          dplyr::pull(growth_ratio)
      } else {
        growth_values <- col_clean |>
          dplyr::arrange(.data[[year_col_chr]]) |>
          dplyr::mutate(
            prev_pop = dplyr::lag(.data[[pop_col]]),
            growth_ratio = dplyr::if_else(
              prev_pop > 0,
              .data[[pop_col]] / prev_pop,
              NA_real_
            )
          ) |>
          dplyr::pull(growth_ratio)
      }

      valid_growth <- growth_values[
        !base::is.na(growth_values) & base::is.finite(growth_values)
      ]

      if (base::length(valid_growth) == 0) {
        growth_rates[[pop_col]] <- 1.02
        cli::cli_warn("No growth data for {pop_col}, using default 2% growth")
      } else {
        growth_rates[[pop_col]] <- base::mean(valid_growth)
      }
    }
    
    return(growth_rates)
  }

  # calculate automatic growth rates if multiplier is NULL
  if (base::is.null(multiplier)) {
    growth_rates <- calculate_growth_rates(
      observed_data,
      year_col,
      pop_cols,
      group_cols
    )
    multiplier <- growth_rates
  }

  has_named <- !base::is.null(names(years_to_extrap)) &&
    base::all(names(years_to_extrap) != "")

  # handle named years_to_extrap (override multiplier if present)
  if (has_named) {
    # years_to_extrap has multipliers by year, apply to all columns
    year_multipliers <- stats::setNames(
      base::as.numeric(years_to_extrap),
      base::as.character(target_years)
    )
    multipliers_by_col <- stats::setNames(
      rep(list(year_multipliers), length(pop_cols)),
      pop_cols
    )
  } else {
    # handle multipliers for multiple columns
    if (base::is.list(multiplier) || base::length(names(multiplier)) > 0) {
      # named multipliers for different columns
      multipliers_by_col <- multiplier
    } else {
      # single multiplier for all columns
      multipliers_by_col <- stats::setNames(
        rep(list(multiplier), length(pop_cols)),
        pop_cols
      )
    }
  }

  year_col_chr <- rlang::as_name(year_sym)

  get_multiplier <- function(year, pop_col) {
    col_multipliers <- multipliers_by_col[[pop_col]]
    if (base::is.null(col_multipliers)) {
      return(1.02)  # default fallback
    }
    
    # check if we have year-specific multipliers (named vector)
    if (base::is.numeric(col_multipliers) && !base::is.null(names(col_multipliers))) {
      year_name <- base::as.character(year)
      if (year_name %in% names(col_multipliers)) {
        return(col_multipliers[[year_name]])
      }
      # if year not found in named vector, use first available
      if (base::length(col_multipliers) > 0) {
        return(col_multipliers[[1]])
      }
    }
    
    # single numeric value for this column
    if (base::is.numeric(col_multipliers) && base::length(col_multipliers) == 1) {
      return(col_multipliers)
    }
    
    # if it's a list (shouldn't happen with our structure but safety check)
    if (base::is.list(col_multipliers)) {
      year_name <- base::as.character(year)
      if (year_name %in% names(col_multipliers)) {
        return(col_multipliers[[year_name]])
      }
    }
    
    return(1.02)  # fallback
  }

  fill_group <- function(group_df) {
    group_df <- group_df[base::order(group_df[[year_col_chr]]), , drop = FALSE]
    years_vec <- group_df[[year_col_chr]]
    target_mask <- years_vec %in% target_years
    
    # process each population column
    for (pop_col_chr in pop_cols) {
      pops_vec <- group_df[[pop_col_chr]]
      non_missing <- !base::is.na(pops_vec)
      
      if (!base::any(non_missing)) {
        next  # skip this column if no data
      }

      step_multiplier <- function(start_year, end_year, pop_col) {
        if (start_year == end_year) {
          return(1)
        }
        sequence_years <- base::seq(start_year + 1, end_year)
        mults <- base::vapply(
          sequence_years,
          function(yr) get_multiplier(yr, pop_col),
          numeric(1),
          USE.NAMES = FALSE
        )
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
            multiplier_chain <- step_multiplier(prev_year, current_year, pop_col_chr)
            new_value <- base::round(prev_value * multiplier_chain)
            pops_vec[idx] <- new_value
            changed <- TRUE
            next
          }

          if (base::length(next_idx)) {
            next_year <- years_vec[next_idx]
            next_value <- pops_vec[next_idx]
            multiplier_chain <- step_multiplier(current_year, next_year, pop_col_chr)
            if (multiplier_chain == 0) {
              multiplier_chain <- 1  # fallback to no change
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
    }
    
    group_df
  }

  result <- data |>
    dplyr::group_split(.keep = TRUE) |>
    base::lapply(fill_group) |>
    dplyr::bind_rows()

  return(result)
}
