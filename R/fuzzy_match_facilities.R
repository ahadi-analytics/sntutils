#' Facility name matching across datasets (DHIS2 vs MFL)
#'
#' @description
#' Orchestrates a multi-step matching pipeline between a *target* dataset
#' (e.g., DHIS2 facilities) and a *lookup* dataset (e.g., MFL).
#' Steps include exact match, interactive stratified match
#' (using `sntutils::prep_geonames()`), standardized-name match, and
#' fuzzy match.
#' Returns consolidated results and QA summaries.
#'
#' @param target_df Tibble/data.frame with facility names and admin columns.
#' @param lookup_df Tibble/data.frame with facility names and admin columns.
#' @param admin_cols Character vector of admin columns ordered high to low.
#'   Default c("adm0", "adm1", "adm2").
#' @param hf_col_name Character. Facility name column used in both data
#'   frames. Default "hf".
#' @param uid_col Character. Column in `target_df` uniquely identifying each
#'   facility. Default "hf_uid".
#' @param steps Named logical vector toggling steps:
#'   c(exact = TRUE, interactive = FALSE, standardization = TRUE, fuzzy = TRUE).
#' @param lookup_cols Character vector of `lookup_df` columns to append to the
#'   match results and returned target table. Default `character()`.
#' @param match_interactivity Logical flag forwarded to the interactive
#'   matching helper. Default `TRUE`.
#' @param fuzzy_methods Character vector of string distance methods to
#'   combine. Supported: c("jw", "lv"). Default c("jw", "lv").
#' @param fuzzy_threshold Integer in 0-100 for acceptance. Default 80.
#' @param score_exact Integer score for exact matches. Default 100.
#' @param score_standardization Integer score for standardized matches.
#'   Default 100.
#' @param score_interactive Integer score for interactive matches.
#'   Default 95.
#' @param status_cuts Named numeric thresholds for status classification:
#'   c(high = 95, medium = 80, low = 70).
#' @param include_missing_name_rows Logical. If TRUE, adds rows with missing
#'   name as `match_method = "missing_name"`. Default TRUE.
#' @param save_path Character. Directory path root for outputs. Default NULL;
#'   when NULL, results are not written to disk.
#' @param matching_cache_path Character. Directory for interactive cache and
#'   unmatched files. Default here::here(paths$cache).
#' @param save_stem Character. Base filename stem for saved outputs.
#'   Default "facility_matching".
#' @param summary_language Character vector choosing summary language columns.
#'   Supported codes: "en", "fr", "pt". Default "en".
#' @param verbose Logical. If TRUE, prints a CLI summary box preview of
#'   matching results. Default TRUE.
#'
#' @return A list with results, by_method, dhis2_only, mfl_only,
#'   target_augmented, summary_table, coverage_summary, and params. One-to-one
#'   matching is always enforced, ensuring each MFL facility is matched by at
#'   most one DHIS2 facility.
#' @export
fuzzy_match_facilities <- function(
  target_df,
  lookup_df,
  admin_cols = c("adm0", "adm1", "adm2"),
  hf_col_name = "hf",
  uid_col = "hf_uid",
  steps = c(
    exact = TRUE,
    interactive = TRUE,
    standardization = TRUE,
    fuzzy = TRUE
  ),
  lookup_cols = character(),
  match_interactivity = TRUE,
  fuzzy_methods = c("jw", "lv"),
  fuzzy_threshold = 80L,
  score_exact = 100L,
  score_standardization = 100L,
  score_interactive = 95L,
  status_cuts = c(high = 95, medium = 80, low = 70),
  include_missing_name_rows = TRUE,
  save_path = NULL,
  matching_cache_path = NULL,
  save_stem = "facility_matching",
  summary_language = "en",
  verbose = TRUE
) {
  allowed_languages <- c("en", "fr", "pt")
  if (!base::all(summary_language %in% allowed_languages)) {
    cli::cli_abort(
      "`summary_language` must be any of {allowed_languages}."
    )
  }
  summary_language <- unique(summary_language)

  # ---------------------------- input validation ------------------------------
  if (!(base::is.logical(steps) && !base::is.null(base::names(steps)))) {
    cli::cli_abort("`steps` must be a named logical vector.")
  }
  if (!base::is.character(lookup_cols)) {
    cli::cli_abort("`lookup_cols` must be a character vector.")
  }
  if (
    !(base::is.logical(match_interactivity) &&
      base::length(match_interactivity) == 1L)
  ) {
    cli::cli_abort("`match_interactivity` must be a single logical value.")
  }
  if (base::length(lookup_cols) > 0L) {
    missing_lookup_cols <- base::setdiff(lookup_cols, base::names(lookup_df))
    if (base::length(missing_lookup_cols) > 0L) {
      cli::cli_abort(
        "`lookup_cols` columns missing from lookup_df: {missing_lookup_cols}."
      )
    }
    overlapping_cols <- base::intersect(lookup_cols, base::names(target_df))
    if (base::length(overlapping_cols) > 0L) {
      cli::cli_abort(
        "`lookup_cols` overlaps with target_df columns: {overlapping_cols}."
      )
    }
    lookup_cols <- unique(lookup_cols)
  }
  if (!base::all(admin_cols %in% base::names(target_df))) {
    missing_target_cols <- base::setdiff(admin_cols, base::names(target_df))
    cli::cli_abort(
      "Missing admin cols in target_df: {missing_target_cols}."
    )
  }
  if (!base::all(admin_cols %in% base::names(lookup_df))) {
    missing_lookup_cols <- base::setdiff(admin_cols, base::names(lookup_df))
    cli::cli_abort(
      "Missing admin cols in lookup_df: {missing_lookup_cols}."
    )
  }
  if (!hf_col_name %in% base::names(target_df)) {
    cli::cli_abort("`hf_col_name` ({hf_col_name}) not found in target_df.")
  }
  if (!hf_col_name %in% base::names(lookup_df)) {
    cli::cli_abort("`hf_col_name` ({hf_col_name}) not found in lookup_df.")
  }
  if (!uid_col %in% base::names(target_df)) {
    cli::cli_abort("`uid_col` ({uid_col}) not found in target_df.")
  }
  if (!base::all(fuzzy_methods %in% c("jw", "lv"))) {
    cli::cli_abort("Supported `fuzzy_methods` are 'jw' and/or 'lv'.")
  }
  if (!(base::is.logical(verbose) && base::length(verbose) == 1L)) {
    cli::cli_abort("`verbose` must be a single logical value.")
  }

  if (base::is.null(matching_cache_path)) {
    paths <- base::getOption("snt.paths")
    if (base::is.null(paths) || base::is.null(paths$cache)) {
      cli::cli_abort(
        "`matching_cache_path` is NULL and `paths$cache` is unavailable."
      )
    }
    matching_cache_path <- here::here(paths$cache)
  }

  # ---------------------------- normalize inputs ------------------------------
  target_df <- target_df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(admin_cols, hf_col_name, uid_col)),
        base::as.character
      )
    )

  lookup_df <- lookup_df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(admin_cols, hf_col_name)),
        base::as.character
      )
    )

  # ------------------- split missing vs valid names ---------------------------
  target_missing <- target_df |>
    dplyr::filter(base::is.na(.data[[hf_col_name]])) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(uid_col)), .keep_all = TRUE)

  target_valid <- target_df |>
    dplyr::filter(!base::is.na(.data[[hf_col_name]])) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(
      admin_cols,
      hf_col_name,
      uid_col
    ))))

  # ------------- precompute standardized names (for later steps) --------------
  target_valid <- target_valid |>
    dplyr::mutate(
      hf_target_raw = .data[[hf_col_name]],
      hf_target_std = sntutils::standardize_names(.data[[hf_col_name]])
    )

  lookup_df <- lookup_df |>
    dplyr::mutate(
      hf_lookup_raw = .data[[hf_col_name]],
      hf_lookup_std = sntutils::standardize_names(.data[[hf_col_name]])
    )

  lookup_missing_count <- base::sum(base::is.na(lookup_df$hf_lookup_raw))

  # ------------------------------ containers ----------------------------------
  matches_exact <- tibble::tibble()
  matches_interactive <- tibble::tibble()
  matches_std <- tibble::tibble()
  matches_fuzzy <- tibble::tibble()

  # --------------------------- step 1: exact ----------------------------------
  if (base::isTRUE(steps[["exact"]]) && base::nrow(target_valid) > 0) {
    matches_exact <- .match_step_exact(
      target_valid = target_valid,
      lookup_df = lookup_df,
      admin_cols = admin_cols,
      score_exact = score_exact,
      uid_col = uid_col
    )
  }

  # ---------------------- remove matched from pool ----------------------------
  pool <- .anti_by_uid(target_valid, matches_exact, uid_col = uid_col)

  # ------ step 2: interactive (via sntutils::prep_geonames + unmatched) -------
  if (base::isTRUE(steps[["interactive"]]) && base::nrow(pool) > 0) {
    matches_interactive <- .match_step_interactive(
      pool = pool,
      lookup_df = lookup_df,
      admin_cols = admin_cols,
      hf_col_name = hf_col_name,
      interactivity = match_interactivity,
      score_interactive = score_interactive,
      uid_col = uid_col,
      matching_cache_path = matching_cache_path
    )
    pool <- .anti_by_uid(pool, matches_interactive, uid_col = uid_col)
  }

  # ---------------- step 3: standardized-name + admin -------------------------
  if (base::isTRUE(steps[["standardization"]]) && base::nrow(pool) > 0) {
    matches_std <- .match_step_standardized(
      pool = pool,
      lookup_df = lookup_df,
      admin_cols = admin_cols,
      score_standardization = score_standardization,
      uid_col = uid_col
    )
    pool <- .anti_by_uid(pool, matches_std, uid_col = uid_col)
  }

  # ------------------ step 4: fuzzy within admin groups -----------------------
  if (base::isTRUE(steps[["fuzzy"]]) && base::nrow(pool) > 0) {
    matches_fuzzy <- .match_step_fuzzy(
      pool = pool,
      lookup_df = lookup_df,
      admin_cols = admin_cols,
      fuzzy_methods = fuzzy_methods,
      fuzzy_threshold = fuzzy_threshold,
      uid_col = uid_col
    )
    pool <- .anti_by_uid(pool, matches_fuzzy, uid_col = uid_col)
  }

  # ------------- step 5: unmatched + missing-name rows ------------------------
  unmatched <- pool |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(admin_cols)),
      !!uid_col := .data[[uid_col]],
      hf_target_raw = .data[[hf_col_name]],
      hf_target_std = hf_target_std,
      match_method = "unmatched",
      match_pct = 0L,
      hf_mfl = NA_character_
    )

  missing_rows <- if (
    base::isTRUE(include_missing_name_rows) && base::nrow(target_missing) > 0
  ) {
    target_missing |>
      dplyr::transmute(
        dplyr::across(dplyr::all_of(admin_cols)),
        !!uid_col := .data[[uid_col]],
        hf_target_raw = NA_character_,
        hf_target_std = NA_character_,
        match_method = "missing_name",
        match_pct = 0L,
        hf_mfl = NA_character_
      )
  } else {
    tibble::tibble()
  }

  # -------------------------- step 6: consolidate -----------------------------
  all_matches <- dplyr::bind_rows(
    matches_exact,
    matches_interactive,
    matches_std,
    matches_fuzzy
  )

  uids_before_dedup <- all_matches[[uid_col]]
  all_matches <- .enforce_one_to_one_matching(
    matches = all_matches,
    admin_cols = admin_cols,
    uid_col = uid_col
  )
  uids_after_dedup <- all_matches[[uid_col]]
  removed_uids <- base::setdiff(uids_before_dedup, uids_after_dedup)

  if (base::length(removed_uids) > 0L) {
    removed_facilities <- target_valid |>
      dplyr::filter(.data[[uid_col]] %in% removed_uids) |>
      dplyr::transmute(
        dplyr::across(dplyr::all_of(admin_cols)),
        !!uid_col := .data[[uid_col]],
        hf_target_raw = .data[[hf_col_name]],
        hf_target_std = hf_target_std,
        match_method = "unmatched",
        match_pct = 0L,
        hf_mfl = NA_character_
      )
    unmatched <- dplyr::bind_rows(unmatched, removed_facilities)
  }

  results <- dplyr::bind_rows(
    all_matches,
    unmatched,
    missing_rows
  ) |>
    .add_match_status(status_cuts = status_cuts) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(admin_cols)), hf_target_raw)

  if (base::length(lookup_cols) > 0L) {
    lookup_cols_tbl <- lookup_df |>
      dplyr::select(
        dplyr::all_of(admin_cols),
        hf_lookup_raw,
        dplyr::all_of(lookup_cols)
      ) |>
      dplyr::arrange(
        dplyr::across(dplyr::all_of(c(admin_cols, "hf_lookup_raw")))
      ) |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(c(admin_cols, "hf_lookup_raw")))
      ) |>
      dplyr::slice_head(n = 1L) |>
      dplyr::ungroup()
    join_keys <- c(
      stats::setNames(admin_cols, admin_cols),
      hf_mfl = "hf_lookup_raw"
    )
    results <- results |>
      dplyr::left_join(
        lookup_cols_tbl,
        by = join_keys,
        relationship = "many-to-one"
      )
  }

  result_cols_for_target <- results |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(uid_col)),
      .keep_all = TRUE
    ) |>
    dplyr::select(
      dplyr::all_of(unique(c(uid_col, "hf_mfl", lookup_cols)))
    )

  target_augmented <- target_df |>
    dplyr::left_join(
      result_cols_for_target,
      by = stats::setNames(uid_col, uid_col),
      relationship = "many-to-one"
    )

  # ------------------------------ summaries -----------------------------------
  by_method <- results |>
    dplyr::count(match_method, name = "count") |>
    dplyr::arrange(match_method)

  unique_target <- dplyr::n_distinct(target_df[[uid_col]])

  # ---------------------- coverage cross-checks ---------------------------
  cross <- .coverage_crosscheck(
    results = results,
    lookup_df = lookup_df,
    admin_cols = admin_cols,
    uid_col = uid_col
  )

  lookup_total_unique <- lookup_df |>
    dplyr::distinct(dplyr::across(dplyr::all_of(admin_cols)), hf_lookup_raw) |>
    base::nrow()

  n_unique_mfl_matched <- results |>
    dplyr::filter(!base::is.na(hf_mfl)) |>
    dplyr::distinct(hf_mfl) |>
    base::nrow()

  # calculate mfl_only as simple subtraction: total - matched
  mfl_only_count <- lookup_total_unique - n_unique_mfl_matched

  counts <- list(
    n_exact = base::sum(results$match_method == "exact_admin", na.rm = TRUE),
    n_interactive = base::sum(
      results$match_method == "interactive",
      na.rm = TRUE
    ),
    n_standardization = base::sum(
      results$match_method == "standardization",
      na.rm = TRUE
    ),
    n_fuzzy = base::sum(results$match_method == "fuzzy", na.rm = TRUE),
    n_unmatched = base::sum(results$match_method == "unmatched", na.rm = TRUE),
    n_missing = base::sum(
      results$match_method == "missing_name",
      na.rm = TRUE
    ),
    n_lookup_missing = lookup_missing_count,
    unique_target = unique_target,
    n_mfl_matched = n_unique_mfl_matched,
    n_mfl_only = mfl_only_count,
    lookup_total = lookup_total_unique
  )

  summary_table <- .build_summary_table(
    lookup_n = lookup_total_unique,
    counts = counts,
    languages = summary_language,
    steps_enabled = steps
  )
  attr(summary_table, "counts") <- counts
  attr(summary_table, "lookup_total") <- lookup_total_unique
  attr(summary_table, "steps") <- steps

  if (base::isTRUE(verbose)) {
    .display_match_summary_box(summary_table)
  }

  # ------------------------------ optional save ---------------------------
  params_list <- list(
    admin_cols = admin_cols,
    hf_col_name = hf_col_name,
    uid_col = uid_col,
    steps = steps,
    lookup_cols = lookup_cols,
    match_interactivity = match_interactivity,
    fuzzy_methods = fuzzy_methods,
    fuzzy_threshold = fuzzy_threshold,
    scores = list(
      exact = score_exact,
      standardization = score_standardization,
      interactive = score_interactive
    ),
    status_cuts = status_cuts,
    include_missing_name_rows = include_missing_name_rows,
    matching_cache_path = matching_cache_path,
    save_path = save_path,
    save_stem = save_stem,
    summary_language = summary_language,
    verbose = verbose
  )

  save_bundle <- list(
    facility_matching_results = results,
    facility_matching_summary_table = summary_table,
    crosswalk = list(
      mfl_only = cross$mfl_only,
      target_only = cross$target_only
    ),
    coverage_summary = list(
      mfl_only = list(
        count = mfl_only_count,
        pct_of_lookup = if (lookup_total_unique > 0L) {
          mfl_only_count / lookup_total_unique
        } else {
          NA_real_
        }
      )
    ),
    params = params_list,
    metadata = list(
      timestamp = base::Sys.time()
    )
  )

  if (!base::is.null(save_path)) {
    base::dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
    base::saveRDS(
      save_bundle,
      base::file.path(save_path, base::paste0(save_stem, "_bundle.rds"))
    )
    sntutils::write_snt_data(
      obj = save_bundle,
      data_name = save_stem,
      path = save_path,
      file_formats = c("qs2", "xlsx")
    )
  }

  # return -----------------------------------------------------------------
  list(
    results = results,
    by_method = by_method,
    dhis2_only = cross$target_only,
    mfl_only = cross$mfl_only,
    target_augmented = target_augmented,
    summary_table = summary_table,
    coverage_summary = list(
      mfl_only = list(
        count = mfl_only_count,
        pct_of_lookup = if (lookup_total_unique > 0L) {
          mfl_only_count / lookup_total_unique
        } else {
          NA_real_
        }
      )
    ),
    params = params_list
  )
}

# ---------- helpers (uid_col-aware) ---------------------------------------

#' classify match confidence bands (internal)
#'
#' @description
#' label each row by comparing `match_pct` against the provided thresholds.
#'
#' @param results Tibble with aggregated match results.
#' @param status_cuts Named numeric vector defining high, medium, and low cuts.
#'
#' @return Tibble with `match_status` column added.
#'
#' @keywords internal
#' @noRd
.add_match_status <- function(results, status_cuts) {
  results |>
    dplyr::mutate(
      match_status = dplyr::case_when(
        match_method == "missing_name" ~ "missing",
        match_method == "unmatched" ~ "unmatched",
        match_pct >= status_cuts[["high"]] ~ "high_confidence",
        match_pct >= status_cuts[["medium"]] ~ "medium_confidence",
        match_pct >= status_cuts[["low"]] ~ "low_confidence",
        TRUE ~ "review_needed"
      )
    )
}

#' remove already matched identifiers (internal)
#'
#' @description
#' drop rows from the working pool once their unique identifiers appear in
#' prior match outputs.
#'
#' @param pool Tibble of candidate rows still under consideration.
#' @param matches Tibble containing identifiers already matched.
#' @param uid_col Character name of the unique identifier column.
#'
#' @return Filtered tibble preserving only unmatched identifiers.
#'
#' @keywords internal
#' @noRd
.anti_by_uid <- function(pool, matches, uid_col) {
  if (base::nrow(matches) == 0L) {
    return(pool)
  }
  pool |>
    dplyr::anti_join(
      matches |> dplyr::distinct(dplyr::across(dplyr::all_of(uid_col))),
      by = stats::setNames(uid_col, uid_col)
    )
}

#' enforce one-to-one matching (internal)
#'
#' @description
#' when multiple target facilities match the same lookup facility, keep only
#' the best match. disambiguation rules: highest match_pct wins; if tied,
#' earliest method wins (exact > interactive > standardization > fuzzy).
#'
#' @param matches Tibble of all matches (before adding unmatched rows).
#' @param admin_cols Character vector of admin columns.
#' @param uid_col Character name of the unique identifier column.
#'
#' @return Tibble with duplicate matches removed.
#'
#' @keywords internal
#' @noRd
.enforce_one_to_one_matching <- function(matches, admin_cols, uid_col) {
  if (base::nrow(matches) == 0L) {
    return(matches)
  }

  method_priority <- c(
    "exact_admin" = 1L,
    "interactive" = 2L,
    "standardization" = 3L,
    "fuzzy" = 4L
  )

  matches |>
    dplyr::mutate(
      method_rank = dplyr::case_when(
        match_method == "exact_admin" ~ 1L,
        match_method == "interactive" ~ 2L,
        match_method == "standardization" ~ 3L,
        match_method == "fuzzy" ~ 4L,
        TRUE ~ 99L
      )
    ) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(admin_cols, "hf_mfl")))
    ) |>
    dplyr::arrange(
      dplyr::desc(match_pct),
      method_rank,
      .by_group = TRUE
    ) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup() |>
    dplyr::select(-method_rank)
}

#' exact name matching within admin levels (internal)
#'
#' @description
#' compare upper-cased facility names within shared admin groups to confirm
#' deterministic matches.
#'
#' @param target_valid Tibble of target facilities with valid names.
#' @param lookup_df Tibble of lookup facilities.
#' @param admin_cols Character vector of administrative levels in join order.
#' @param score_exact Integer score to assign to exact matches.
#' @param uid_col Character identifier column present in `target_valid`.
#'
#' @return Tibble of matches with lookup names and scores.
#'
#' @keywords internal
#' @noRd
.match_step_exact <- function(
  target_valid,
  lookup_df,
  admin_cols,
  score_exact,
  uid_col
) {
  target_up <- target_valid |>
    dplyr::mutate(hf_target_raw_upper = base::toupper(hf_target_raw))
  lookup_up <- lookup_df |>
    dplyr::mutate(hf_lookup_raw_upper = base::toupper(hf_lookup_raw))

  target_up |>
    dplyr::inner_join(
      lookup_up,
      by = stats::setNames(admin_cols, admin_cols),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(hf_target_raw_upper == hf_lookup_raw_upper) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(uid_col)), .keep_all = TRUE) |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(admin_cols)),
      !!uid_col := .data[[uid_col]],
      hf_target_raw,
      hf_target_std,
      match_method = "exact_admin",
      match_pct = score_exact,
      hf_mfl = hf_lookup_raw
    )
}

# ---- INTERACTIVE (prep_geonames + unmatched file) ------------------------

#' interactive review matches (internal)
#'
#' @description
#' call `sntutils::prep_geonames()` to support manual review and retain rows
#' confirmed during that pass.
#'
#' @param pool Tibble of remaining target facilities.
#' @param lookup_df Tibble of lookup facilities.
#' @param admin_cols Character vector of admin columns.
#' @param hf_col_name Character name of the facility column.
#' @param interactivity Logical flag passed to the interactive helper.
#' @param score_interactive Integer score awarded to reviewed matches.
#' @param uid_col Character unique identifier column.
#' @param matching_cache_path Path where cache and unmatched files live.
#'
#' @return Tibble containing interactive matches.
#'
#' @keywords internal
#' @noRd
.match_step_interactive <- function(
  pool,
  lookup_df,
  admin_cols,
  hf_col_name,
  interactivity,
  score_interactive,
  uid_col,
  matching_cache_path
) {
  # 1) Minimal inputs for prep_geonames (rename facility col to 'hf')
  target_min <- pool |>
    dplyr::select(
      dplyr::all_of(c(admin_cols, hf_col_name, uid_col)),
      hf_target_std
    ) |>
    dplyr::rename_with(
      ~ "hf",
      dplyr::all_of(hf_col_name)
    )

  lookup_min <- lookup_df |>
    dplyr::select(dplyr::all_of(c(admin_cols, hf_col_name))) |>
    dplyr::rename_with(
      ~ "hf",
      dplyr::all_of(hf_col_name)
    )

  # helper to silence messaging of prep_geoname
  # if not interactive
  quiet_prep_geonames <- function(...) {
    withCallingHandlers(
      suppressWarnings(
        suppressMessages(
          {
            # swallow stdout from cat()/print()
            utils::capture.output(
              res <- sntutils::prep_geonames(...),
              type = "output"
            )
            res
          }
        )
      ),
      # swallow cli::cli_* output
      cliMessage = function(c) invokeRestart("muffleMessage")
    )
  }

  # 2) Run interactive stratified tool (writes unmatched to RDS)
  if (isTRUE(interactivity)) {
    geo_out <- sntutils::prep_geonames(
      target_df = target_min,
      lookup_df = lookup_min,
      level0 = admin_cols[1],
      level1 = admin_cols[2],
      level2 = admin_cols[3],
      level3 = "hf",
      cache_path = here::here(matching_cache_path, "geoname_cache_hf.rds"),
      unmatched_export_path = here::here(
        matching_cache_path,
        "unmatched_dhis2_hf.rds"
      ),
      interactive = interactivity
    ) |>
      tibble::as_tibble()
  } else {
    geo_out <- quiet_prep_geonames(
      target_df = target_min,
      lookup_df = lookup_min,
      level0 = admin_cols[1],
      level1 = admin_cols[2],
      level2 = admin_cols[3],
      level3 = "hf",
      cache_path = here::here(matching_cache_path, "geoname_cache_hf.rds"),
      unmatched_export_path = here::here(
        matching_cache_path,
        "unmatched_dhis2_hf.rds"
      ),
      interactive = FALSE
    ) |>
      tibble::as_tibble()
  }

  # 3) Load unmatched (if missing, treat as none unmatched)
  unmatched_path <- here::here(matching_cache_path, "unmatched_dhis2_hf.rds")
  unmatched_after <- base::tryCatch(
    base::readRDS(unmatched_path),
    error = function(e) tibble::tibble(hf = character())
  )

  unmatched_names_uc <- unmatched_after$hf |>
    base::toupper() |>
    base::unique() |>
    stats::na.omit()

  # 4) interactive matches skip unmatched; restore lookup name when present
  #    geo_out may contain both sides.
  #    when present, use its 'hf' as lookup name.
  matches_interactive <- target_min |>
    dplyr::left_join(
      geo_out |>
        dplyr::select(
          !!uid_col := .data[[uid_col]],
          hf_mfl = .data[["hf"]] # lookup-side name from geo_out
        ),
      by = uid_col
    ) |>
    dplyr::filter(!base::toupper(.data[["hf"]]) %in% unmatched_names_uc) |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(admin_cols)),
      !!uid_col := .data[[uid_col]],
      hf_target_raw = .data[["hf"]],
      hf_target_std,
      match_method = "interactive",
      match_pct = score_interactive,
      hf_mfl
    ) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(uid_col)), .keep_all = TRUE)

  matches_interactive
}

#' standardized name matches (internal)
#'
#' @description
#' match facilities whose standardized names align within the same admin
#' grouping.
#'
#' @param pool Tibble of remaining target rows.
#' @param lookup_df Tibble of lookup rows with standardized names.
#' @param admin_cols Character vector of admin columns.
#' @param score_standardization Integer score assigned to these matches.
#' @param uid_col Character name of the identifier column.
#'
#' @return Tibble of standardized matches.
#'
#' @keywords internal
#' @noRd
.match_step_standardized <- function(
  pool,
  lookup_df,
  admin_cols,
  score_standardization,
  uid_col
) {
  pool |>
    dplyr::inner_join(
      lookup_df,
      by = stats::setNames(admin_cols, admin_cols),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(hf_target_std == hf_lookup_std) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(uid_col)), .keep_all = TRUE) |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(admin_cols)),
      !!uid_col := .data[[uid_col]],
      hf_target_raw,
      hf_target_std,
      match_method = "standardization",
      match_pct = score_standardization,
      hf_mfl = hf_lookup_raw
    )
}

#' fuzzy string matches within admin groups (internal)
#'
#' @description
#' evaluate target and lookup names with multiple string distance methods and
#' keep the highest scoring matches above a threshold.
#'
#' @param pool Tibble of remaining target rows.
#' @param lookup_df Tibble of lookup rows.
#' @param admin_cols Character admin columns to group on.
#' @param fuzzy_methods Character vector of string distance methods.
#' @param fuzzy_threshold Integer acceptance threshold in percent.
#' @param uid_col Character identifier column.
#'
#' @return Tibble of fuzzy matches.
#'
#' @keywords internal
#' @noRd
.match_step_fuzzy <- function(
  pool,
  lookup_df,
  admin_cols,
  fuzzy_methods,
  fuzzy_threshold,
  uid_col
) {
  joined <- pool |>
    dplyr::inner_join(
      lookup_df,
      by = stats::setNames(admin_cols, admin_cols),
      relationship = "many-to-many"
    ) |>
    dplyr::select(
      dplyr::all_of(admin_cols),
      dplyr::all_of(uid_col),
      hf_target_raw,
      hf_target_std,
      hf_lookup_raw,
      hf_lookup_std
    )

  if (base::nrow(joined) == 0L) {
    return(tibble::tibble())
  }

  groups <- joined |>
    dplyr::group_by(dplyr::across(dplyr::all_of(admin_cols))) |>
    dplyr::group_split(.keep = TRUE)

  purrr::map_dfr(
    groups,
    \(g) {
      .fuzzy_group_best(g, admin_cols, fuzzy_methods, fuzzy_threshold, uid_col)
    }
  )
}

#' select best fuzzy matches inside a group (internal)
#'
#' @description
#' compute similarity matrices for the requested methods and keep the top
#' candidate per target when it clears the threshold.
#'
#' @param g Tibble for a single admin grouping.
#' @param admin_cols Character vector of admin columns.
#' @param fuzzy_methods Character vector of methods to blend.
#' @param fuzzy_threshold Integer acceptance threshold in percent.
#' @param uid_col Character identifier column.
#'
#' @return Tibble of matches from one admin group.
#'
#' @keywords internal
#' @noRd
.fuzzy_group_best <- function(
  g,
  admin_cols,
  fuzzy_methods,
  fuzzy_threshold,
  uid_col
) {
  targets <- g |>
    dplyr::distinct(dplyr::across(dplyr::all_of(uid_col)), hf_target_raw, hf_target_std) |>
    dplyr::rename(uid = !!uid_col)
  cands <- g |> dplyr::distinct(hf_lookup_raw, hf_lookup_std)

  if (base::nrow(cands) == 0L || base::nrow(targets) == 0L) {
    return(tibble::tibble())
  }

  jw_sim <- if ("jw" %in% fuzzy_methods) {
    100 *
      (1 -
        stringdist::stringdistmatrix(
          targets$hf_target_std,
          cands$hf_lookup_std,
          method = "jw"
        ))
  } else {
    NULL
  }

  lv_sim <- if ("lv" %in% fuzzy_methods) {
    lv <- stringdist::stringdistmatrix(
      targets$hf_target_std,
      cands$hf_lookup_std,
      method = "lv"
    )
    la <- base::nchar(targets$hf_target_std)
    lb <- base::nchar(cands$hf_lookup_std)
    maxlen <- base::pmax(
      base::matrix(
        la,
        nrow = base::length(la),
        ncol = base::length(lb)
      ),
      base::matrix(
        lb,
        nrow = base::length(la),
        ncol = base::length(lb),
        byrow = TRUE
      )
    )
    100 * base::pmax(0, 1 - lv / base::pmax(1, maxlen))
  } else {
    NULL
  }

  sims <- purrr::compact(list(jw_sim, lv_sim))
  avg <- if (base::length(sims) == 1L) {
    sims[[1L]]
  } else {
    base::Reduce("+", sims) / base::length(sims)
  }

  best_idx <- base::apply(avg, 1L, base::which.max)
  best_score <- avg[cbind(base::seq_len(base::nrow(avg)), best_idx)]
  keep <- base::which(best_score >= fuzzy_threshold)
  if (base::length(keep) == 0L) {
    return(tibble::tibble())
  }

  admin_stub <- g[1, admin_cols, drop = FALSE]
  admin_stub[base::rep(1, base::length(keep)), , drop = FALSE] |>
    tibble::as_tibble() |>
    dplyr::bind_cols(
      tibble::tibble(!!uid_col := targets$uid[keep]),
      targets[keep, c("hf_target_raw", "hf_target_std")],
      tibble::tibble(
        match_method = "fuzzy",
        match_pct = base::as.integer(base::round(best_score[keep], 0)),
        hf_mfl = cands$hf_lookup_raw[best_idx[keep]]
      )
    )
}

#' compare coverage between results and lookup (internal)
#'
#' @description
#' identify unmatched lookup facilities and target rows still missing
#' matches.
#'
#' @param results Tibble of consolidated matching outputs.
#' @param lookup_df Tibble of lookup facilities with raw names.
#' @param admin_cols Character vector of admin columns.
#' @param uid_col Character unique identifier column.
#'
#' @return List with `mfl_only` and `target_only` tibbles.
#'
#' @keywords internal
#' @noRd
.coverage_crosscheck <- function(results, lookup_df, admin_cols, uid_col) {
  lookup_keys <- lookup_df |>
    dplyr::distinct(dplyr::across(dplyr::all_of(admin_cols)), hf_lookup_raw)

  matched_mfl_names <- results |>
    dplyr::filter(!base::is.na(hf_mfl)) |>
    dplyr::distinct(hf_mfl) |>
    dplyr::pull(hf_mfl)

  matched_keys <- lookup_df |>
    dplyr::filter(hf_lookup_raw %in% matched_mfl_names) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(admin_cols)), hf_lookup_raw)

  mfl_only <- lookup_keys |>
    dplyr::anti_join(matched_keys, by = c(admin_cols, "hf_lookup_raw"))

  target_only <- results |>
    dplyr::filter(match_method %in% c("unmatched", "missing_name")) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(uid_col)), .keep_all = TRUE)

  list(
    mfl_only = mfl_only,
    target_only = target_only,
    matched_keys = matched_keys
  )
}

#' display facility matching summary in CLI box
#'
#' @description
#' Render a formatted CLI box summarizing facility matching results.
#' Metrics and values are aligned for readability for a quick visual QA
#' of matching performance.
#'
#' @param summary_table Tibble produced by `.build_summary_table()` with
#'   `metric` and `value` columns.
#'
#' @return Invisibly returns `NULL` after printing the box to the console.
#'
#' @keywords internal
#' @noRd
.display_match_summary_box <- function(summary_table) {
  if (base::nrow(summary_table) == 0L) {
    return(invisible(NULL))
  }

  counts <- attr(summary_table, "counts")
  lookup_total <- attr(summary_table, "lookup_total")

  if (base::is.null(counts)) {
    cli::cli_alert_warning("Summary counts unavailable; skipping CLI box.")
    return(invisible(NULL))
  }

  fmt_count <- function(x, width = NULL) {
    if (base::is.null(x) || base::is.na(x)) {
      return("NA")
    }
    out <- formatC(x, format = "d", big.mark = ",")
    if (!base::is.null(width) && width > 0L) {
      out <- sprintf(paste0("%", width, "s"), out)
    }
    out
  }

  fmt_pct <- function(count, denom) {
    if (
      base::is.null(count) || base::is.na(count) ||
        base::is.null(denom) || base::is.na(denom) ||
        denom <= 0
    ) {
      return(NA_real_)
    }
    100 * count / denom
  }

  fmt_with_pct <- function(count, denom, width = 5L) {
    if (base::is.null(count) || base::is.na(count)) {
      return("NA")
    }
    if (base::is.null(denom) || base::is.na(denom) || denom <= 0) {
      return(fmt_count(count, width))
    }
    pct <- fmt_pct(count, denom)
    sprintf("%s (%.1f%%)", fmt_count(count, width), pct)
  }

  dhis2_total <- counts$unique_target
  matched_total <- counts$n_exact + counts$n_interactive +
    counts$n_standardization + counts$n_fuzzy
  matched_pct <- fmt_pct(matched_total, dhis2_total)

  mfl_total <- if (base::is.null(lookup_total)) counts$lookup_total else lookup_total
  mfl_total <- if (base::is.null(mfl_total)) NA_real_ else mfl_total

  mfl_missing <- counts$n_lookup_missing
  mfl_only <- counts$n_mfl_only

  has_metric <- function(metric) metric %in% summary_table$metric

  format_primary <- function(label, value) {
    sprintf("  \u2022 %-38s : %s", label, value)
  }

  format_secondary <- function(label, value) {
    sprintf("    \u2013 %-36s : %s", label, value)
  }

  matched_value <- sprintf(
    "%s of %s",
    fmt_count(matched_total),
    fmt_count(dhis2_total)
  )

  matched_pct_line <- if (!base::is.na(matched_pct)) {
    sprintf("      (%.1f%% of all DHIS2 facilities)", matched_pct)
  } else {
    "      (insufficient data to compute match rate)"
  }

  mfl_matched <- counts$n_mfl_matched

  lines <- c(
    format_primary("Matched DHIS2 facilities", matched_value),
    matched_pct_line,
    "",
    format_primary("MFL facilities (total)", fmt_count(mfl_total)),
    format_secondary("Missing names in MFL", fmt_count(mfl_missing, 5)),
    format_secondary(
      "MFL facilities matched to DHIS2",
      fmt_with_pct(mfl_matched, mfl_total)
    ),
    format_secondary(
      "MFL facilities not matched to DHIS2",
      fmt_with_pct(mfl_only, mfl_total)
    ),
    "",
    format_primary(
      "DHIS2 facilities (total)",
      paste0(fmt_count(dhis2_total), " (100%)")
    )
  )

  if (has_metric("Exact matches (admin)")) {
    lines <- c(
      lines,
      format_secondary(
        "Exact matches (admin)",
        fmt_with_pct(counts$n_exact, dhis2_total)
      )
    )
  }
  if (has_metric("Interactive matches")) {
    lines <- c(
      lines,
      format_secondary(
        "Interactive matches",
        fmt_with_pct(counts$n_interactive, dhis2_total)
      )
    )
  }
  if (has_metric("Standardization matches")) {
    lines <- c(
      lines,
      format_secondary(
        "Standardization match",
        fmt_with_pct(counts$n_standardization, dhis2_total)
      )
    )
  }
  if (has_metric("Fuzzy matches")) {
    lines <- c(
      lines,
      format_secondary(
        "Fuzzy matches",
        fmt_with_pct(counts$n_fuzzy, dhis2_total)
      )
    )
  }

  lines <- c(
    lines,
    format_secondary(
      "Unmatched",
      fmt_with_pct(counts$n_unmatched, dhis2_total)
    )
  )

  style_line <- function(text) {
    if (identical(text, "")) {
      return(text)
    }
    if (startsWith(text, "      (")) {
      return(cli::col_silver(text))
    }
    if (startsWith(text, "    \u2013")) {
      return(cli::col_silver(text))
    }
    if (startsWith(text, "  \u2022")) {
      return(cli::style_bold(text))
    }
    text
  }

  styled_lines <- vapply(lines, style_line, character(1))

  box_width <- max(cli::ansi_nchar(styled_lines)) + 8L
  box_width <- min(box_width, as.integer(cli::console_width()))

  cli::cat_line("")
  cli::cat_line(
    cli::boxx(
      styled_lines,
      header = cli::style_bold("Health Facility Matching Summary"),
      border_style = "double",
      padding = 1L,
      width = box_width
    )
  )
  cli::cat_line("")

  invisible(NULL)
}

# -------- summary table with FR option ----------------------------------------

#' assemble summarised matching counts (internal)
#'
#' @description
#' create summary tables of matching outcomes in english, french, and
#' portuguese. compute coverage rates for quick qa checks.
#'
#' @param lookup_n Integer count of lookup facilities.
#' @param counts Named list of component match counts.
#' @param languages Character vector choosing output languages.
#' @param steps_enabled Named logical vector indicating which matching
#'   steps ran.
#'
#' @return Tibble with metrics, values, and optional descriptions.
#'
#' @keywords internal
#' @noRd
.build_summary_table <- function(
  lookup_n,
  counts,
  languages = "en",
  steps_enabled = NULL
) {
  allowed_languages <- c("en", "fr", "pt")
  if (!base::all(languages %in% allowed_languages)) {
    cli::cli_abort(
      "`languages` must be any of {allowed_languages}."
    )
  }
  languages <- unique(languages)
  if (base::is.null(steps_enabled)) {
    steps_enabled <- stats::setNames(logical(0), character(0))
  }

  total_matched <- counts$n_exact +
    counts$n_interactive +
    counts$n_standardization +
    counts$n_fuzzy
  total_valid <- total_matched + counts$n_unmatched
  pct <- function(x, d) {
    base::paste0(x, " (", base::round(100 * x / base::max(1, d), 1), "%)")
  }

  table <- tibble::tibble(
    metric = c(
      "MFL facilities",
      "Missing names in MFL",
      "DHIS2 facilities (total)",
      "Missing names in DHIS2",
      "Valid DHIS2 facilities",
      "Exact matches (admin)",
      "Interactive matches",
      "Standardization matches",
      "Fuzzy matches",
      "Total matched",
      "Unmatched",
      "MFL-only facilities"
    ),
    value = c(
      lookup_n,
      counts$n_lookup_missing,
      counts$unique_target,
      counts$n_missing,
      total_valid,
      pct(counts$n_exact, counts$unique_target),
      pct(counts$n_interactive, counts$unique_target),
      pct(counts$n_standardization, counts$unique_target),
      pct(counts$n_fuzzy, counts$unique_target),
      pct(total_matched, counts$unique_target),
      pct(counts$n_unmatched, counts$unique_target),
      pct(counts$n_mfl_only, lookup_n)
    ),
    denominator = c(
      NA_character_,
      "MFL total",
      NA_character_,
      "DHIS2 total",
      "DHIS2 total",
      "DHIS2 total",
      "DHIS2 total",
      "DHIS2 total",
      "DHIS2 total",
      "DHIS2 total",
      "DHIS2 total",
      "MFL total"
    ),
    description_en = c(
      "Total number of facilities in Master Facility List (MFL)",
      "Unique MFL facilities with missing names",
      "Total number of facilities in DHIS2 dataset",
      "Unique DHIS2 facilities with missing names",
      "Unique DHIS2 facilities with valid names",
      "Facilities matched on exact name and admin unit",
      "Facilities matched through manual interactive review",
      "Facilities matched after name standardization",
      "Facilities matched using fuzzy string similarity",
      "Total facilities successfully matched",
      "Facilities still unmatched after all methods",
      "MFL facilities never matched to any DHIS2 facility"
    ),
    description_fr = c(
      "Nombre total de formations sanitaires dans la liste ma\u00eetresse (FOSA)",
      "FS sans nom dans la liste ma\u00eetresse",
      "Nombre total de formations sanitaires dans DHIS2",
      "FS DHIS2 avec noms manquants",
      "FS DHIS2 avec noms valides",
      "FS appari\u00e9es par nom exact et niveau administratif",
      "FS appari\u00e9es par revue interactive manuelle",
      "FS appari\u00e9es apr\u00e8s standardisation des noms",
      "FS appari\u00e9es par similarit\u00e9 de texte floue",
      "Total des FS appari\u00e9es avec succ\u00e8s",
      "FS restant non appari\u00e9es apr\u00e8s toutes les m\u00e9thodes",
      "FS MFL jamais appari\u00e9es \u00e0 DHIS2"
    ),
    description_pt = c(
      "N\u00famero total de unidades na Lista Mestra de Estabelecimentos (MFL)",
      "Unidades na MFL sem nome",
      "N\u00famero total de unidades no conjunto DHIS2",
      "Unidades DHIS2 sem nome",
      "Unidades DHIS2 com nomes v\u00e1lidos",
      "Unidades correspondidas por nome exato e n\u00edvel administrativo",
      "Unidades correspondidas por revis\u00e3o interativa manual",
      "Unidades correspondidas ap\u00f3s padroniza\u00e7\u00e3o de nomes",
      "Unidades correspondidas por similaridade de texto aproximada",
      "Total de unidades correspondidas com sucesso",
      "Unidades ainda sem correspond\u00eancia ap\u00f3s todos os m\u00e9todos",
      "Unidades MFL nunca correspondidas a DHIS2"
    )
  )

  inactive_metrics <- character()
  if (!base::isTRUE(steps_enabled[["exact"]])) {
    inactive_metrics <- c(inactive_metrics, "Exact matches (admin)")
  }
  if (!base::isTRUE(steps_enabled[["interactive"]])) {
    inactive_metrics <- c(inactive_metrics, "Interactive matches")
  }
  if (!base::isTRUE(steps_enabled[["standardization"]])) {
    inactive_metrics <- c(inactive_metrics, "Standardization matches")
  }
  if (!base::isTRUE(steps_enabled[["fuzzy"]])) {
    inactive_metrics <- c(inactive_metrics, "Fuzzy matches")
  }
  if (base::length(inactive_metrics) > 0L) {
    table <- table |>
      dplyr::filter(!metric %in% inactive_metrics)
  }

  language_cols <- paste0("description_", languages)
  table |>
    dplyr::select(
      metric,
      value,
      denominator,
      dplyr::all_of(language_cols)
    )
}
