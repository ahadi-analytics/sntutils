#' Get Upstream and Downstream Variables for Malaria Pathway Indicators
#'
#' Determines upstream and downstream variables for a given malaria indicator
#' based on the clinical care pathway. Structural imputation is only allowed
#' for indicators that follow predictable relationships in the pathway.
#'
#' Assumes variable names match standard: 'susp', 'test', 'conf',
#' 'maltreat', 'maladm', 'maldth'.
#'
#' @param var_to_impute Character. Variable name to analyze (e.g., "conf").
#'   Do not include suffix here; suffix is appended separately if provided.
#' @param malaria_pathway Character vector. Ordered indicator sequence.
#' @param facility_type Character or NULL. Facility type. Used to restrict
#'  imputation for admissions ('maladm'), which only apply to inpatient-
#'  capable facilities. Examples include: "hospital", "referral", "inpatient",
#'   "health_facility" or "tertiary". All other facility types
#'  (e.g., "health_post", "clinic") are treated as outpatient-only.
#' @param suffix Character or NULL. Optional suffix to append to variable names
#'   in output (e.g., "_hf" to return "test_hf", "conf_hf").
#'
#' @return A list with:
#' \describe{
#'   \item{var_to_impute}{Indicator with suffix applied.}
#'   \item{upstream_vars}{Indicators that logically precede it.}
#'   \item{downstream_vars}{Indicators that logically follow it.}
#'   \item{use_structural}{Logical. TRUE if structural imputation is allowed.}
#' }
#'
#' @details
#' Indicator usage in imputation logic depends on facility type.
#' Admissions ('maladm') are only included at inpatient-capable facilities.
#'
#' +------------+-----------------+--------------------+-----------------------+
#' | Indicator  | Facility Type   | Upstream Used      | Downstream Used       |
#' +------------+-----------------+--------------------+-----------------------+
#' | susp       | All             | —                  | test, conf, maladm*,  |
#' |            |                 |                    | maldth                |
#' | test       | All             | susp               | conf, maladm*, maldth |
#' | conf       | All             | susp, test         | maladm*, maldth       |
#' | maltreat   | All             | susp, test, conf   | maladm*, maldth       |
#' | maladm     | Inpatient only  | susp, test, conf   | maldth                |
#' | maladm     | Outpatient only | —                  | —                     |
#' | maldth     | All             | susp, test, conf,  |                       |
#' |            |                 | maladm*            | —                     |
#' +------------+-----------------+--------------------+-----------------------+
#' * Only used if facility is inpatient-capable
#'
#' @examples
#' get_pathway_vars("test")
#' get_pathway_vars("conf", suffix = "_hf")
#' get_pathway_vars("maltreat")
#' get_pathway_vars("maladm", facility_type = "hospital")
#' get_pathway_vars("maladm", facility_type = "health_post")
#'
#' @export
get_pathway_vars <- function(
  var_to_impute,
  malaria_pathway = c("susp", "test", "conf", "maltreat", "maladm", "maldth"),
  facility_type = NULL,
  suffix = NULL
) {
  append_suffix <- function(x) {
    if (length(x) == 0) {
      character()
    } else if (is.null(suffix)) {
      x
    } else {
      paste0(x, suffix)
    }
  }

  inpatient_types <- c("hospital", "referral", "inpatient", "tertiary")

  if (var_to_impute == "maladm") {
    is_inpatient <- !is.null(facility_type) &&
      tolower(facility_type) %in% tolower(inpatient_types)

    if (!is_inpatient) {
      cli::cli_h1("Malaria Pathway Summary")
      cli::cli_alert_info(
        "Imputing for {.field {append_suffix(var_to_impute)}}"
      )
      cli::cli_alert_danger(
        "Admissions not applicable at outpatient facilities (e.g., health post)."
      )
      cli::cli_alert_warning("Structural imputation excluded")
      cli::cli_alert_success("Upstream: None")
      cli::cli_alert_success("Downstream: None")

      return(list(
        var_to_impute = append_suffix(var_to_impute),
        upstream_vars = character(),
        downstream_vars = character(),
        use_structural = FALSE
      ))
    }
  }

  is_inpatient <- !is.null(facility_type) &&
    tolower(facility_type) %in% tolower(inpatient_types)

  non_linear <- character()
  if (var_to_impute != "maltreat") {
    non_linear <- c(non_linear, "maltreat")
  }
  if (!is_inpatient) {
    non_linear <- c(non_linear, "maladm")
  }

  linear_path <- malaria_pathway[!malaria_pathway %in% non_linear]
  pos <- match(var_to_impute, linear_path)

  upstream_vars <- if (is.na(pos) || pos <= 1) {
    character()
  } else {
    linear_path[1:(pos - 1)]
  }

  downstream_vars <- if (is.na(pos) || pos >= length(linear_path)) {
    character()
  } else {
    linear_path[(pos + 1):length(linear_path)]
  }

  upstream_str <- if (length(upstream_vars) == 0) {
    "None"
  } else {
    paste(append_suffix(upstream_vars), collapse = ", ")
  }
  downstream_str <- if (length(downstream_vars) == 0) {
    "None"
  } else {
    paste(append_suffix(downstream_vars), collapse = ", ")
  }

  cli::cli_h1("Malaria Pathway Summary")
  cli::cli_alert_info("Imputing for {.field {append_suffix(var_to_impute)}}")
  cli::cli_alert_success("Upstream: {.field {upstream_str}}")
  cli::cli_alert_success("Downstream: {.field {downstream_str}}")

  if (
    var_to_impute != "maladm" &&
      "maladm" %in% malaria_pathway &&
      !is_inpatient
  ) {
    cli::cli_alert_info(
      paste0(
        "Note: {.val ",
        append_suffix("maladm"),
        "} excluded from dependency logic due to outpatient facility."
      )
    )
  }

  if (
    var_to_impute != "maltreat" &&
      "maltreat" %in% malaria_pathway
  ) {
    cli::cli_alert_info(
      paste0(
        "Note: {.val ",
        append_suffix("maltreat"),
        "} excluded from dependency logic due to presumptive treatment."
      )
    )
  }

  cli::cat_line()

  return(list(
    var_to_impute = append_suffix(var_to_impute),
    upstream_vars = append_suffix(upstream_vars),
    downstream_vars = append_suffix(downstream_vars),
    use_structural = TRUE
  ))
}
