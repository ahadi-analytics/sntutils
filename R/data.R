
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
#'   \item{validation_utility}{Validation and utility terms for data dictionaries, validation reports, and analysis outputs (rate/trend analysis, outliers, consistency checks)}
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

#' Validation Terms Dictionary
#'
#' Multilingual labels for validation-related column names used in
#' `validate_routine_hf_data()` output.
#'
#' @format A named list where each element is a variable name containing
#'   a list of language codes (en, fr, pt) mapping to translated labels:
#'   \describe{
#'     \item{check}{Label for validation check type}
#'     \item{issues_found}{Label for number of issues found}
#'     \item{total_records}{Label for total record count}
#'     \item{percent}{Label for percentage}
#'   }
#'
#' @examples
#' data(validation_terms)
#' validation_terms$check$en
#' validation_terms$check$fr
#'
#' @seealso \code{\link{validate_routine_hf_data}}
#' @keywords datasets
"validation_terms"
