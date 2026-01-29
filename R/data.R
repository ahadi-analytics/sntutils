
#' SNT Variable Tree
#'
#' A hierarchical data structure containing standardized variable names,
#' multilingual labels, and metadata for Subnational Tailoring (SNT) of
#' malaria interventions. This dataset provides the foundation for variable
#' validation, labeling, and data dictionary generation across the SNT workflow.
#'
#' @format A named list with the following top-level elements:
#' \describe{
#'   \item{_meta}{Metadata about the variable tree structure}
#'   \item{metadata}{Variables related to dataset metadata and identifiers}
#'   \item{population}{Population-related variables and demographics}
#'   \item{routine_data}{Health facility routine data variables}
#'   \item{interventions}{Malaria intervention coverage variables}
#'   \item{stock_management}{Stock and supply chain management variables}
#'   \item{anemia}{Anemia-related health indicators}
#'   \item{schema}{Schema definitions for variable components including:
#'     \itemize{
#'       \item test_types: Diagnostic test type codes and labels
#'       \item age_groups: Age group classifications
#'       \item population_groups: Population subgroup definitions
#'       \item sectors: Health sector classifications
#'       \item service_levels: Service delivery level definitions
#'     }
#'   }
#' }
#'
#' @details
#' Each variable entry contains:
#' \itemize{
#'   \item \code{snt_var_name}: Standardized variable name
#'   \item \code{label_en}: English label
#'   \item \code{label_fr}: French label
#'   \item \code{label_pt}: Portuguese label (where available)
#'   \item Additional metadata such as disaggregation options
#' }
#'
#' The schema section provides the building blocks for constructing and
#' parsing SNT variable names, which follow the pattern:
#' \code{domain_testtype_agegroup_populationgroup_sector_servicelevel}
#'
#' @examples
#' # Load the dataset
#' data(snt_var_tree)
#'
#' # View top-level structure
#' names(snt_var_tree)
#'
#' # Access schema information
#' names(snt_var_tree$schema)
#'
#' # View age groups
#' snt_var_tree$schema$age_groups
#'
#' @seealso
#' \code{\link{snt_data_dict}} for flattening the tree into a tidy format,
#' \code{\link{check_snt_var}} for parsing variable names,
#' \code{\link{build_dictionary}} for generating data dictionaries
#'
#' @source Subnational Tailoring Initiative for Malaria Interventions
#' @keywords datasets
"snt_var_tree"

#' Validation utility terms dictionary
#'
#' @description
#' A dictionary of common column names used in validation output from
#' `validate_routine_hf_data()`, with translations in English, French, and Portuguese.
#'
#' @format A named list where each element is a variable name containing:
#' \describe{
#'   \item{en}{English label}
#'   \item{fr}{French label}
#'   \item{pt}{Portuguese label}
#' }
#'
#' @details
#' This dictionary covers:
#' - Summary tab terms (check, issues_found, total_records, percent)
#' - Missing values terms (variable, n_missing, column_type)
#' - Consistency terms (input_indicator, output_indicator, difference, difference_prop)
#' - Facility activeness terms (activity_status, reporting_rate)
#' - Outlier terms (outlier_flag_iqr, outlier_flag_median)
#' - Common ID columns (record_id, hf_uid, date, yearmon)
#' - Admin columns (adm0, adm1, adm2, adm3)
#' - Rate and trend analysis terms (indic_spec, rate, rate_cat, first_n0-last_n4, rate_n1-rate_n4, mk_p, sens_slope, plotting_sens_slope, slope)
#'
#' For malaria-specific indicator names (conf, test, maldth, etc.), use
#' the `snt_var_tree` dataset instead.
#'
#' @source Built from data-raw/validation_terms.yml
#'
#' @examples
#' data(validation_terms)
#'
#' # Get French label for input_indicator
#' validation_terms$input_indicator$fr
#'
#' # Get all English labels
#' sapply(validation_terms, function(x) x$en)
#' @keywords datasets
"validation_terms"
