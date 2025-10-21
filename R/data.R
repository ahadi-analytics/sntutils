#' Administrative Names
#'
#' This dataset contains detailed administrative boundary information
#' categorized by the World Health Organization (WHO) regions. The data covers
#' various administrative levels, including countries, provinces, and districts,
#' along with relevant geographic and administrative identifiers. The is sourced
#' from: https://hub.arcgis.com/datasets/WHO::polio-administrative-boundaries.
#' The shapefile aspect of the data has been removed in order to reduce size and
#' include in the package.
#'
#' @format An \code{sf} object with 48,745 rows and 39 columns:
#' \describe{
#'   \item{\code{OBJECTID}}{Integer, unique object identifier}
#'   \item{\code{WHO_REGION}}{Character, WHO region of the admin area}
#'   \item{\code{ISO_2_CODE}}{Character, two-letter country code}
#'   \item{\code{ISO_3_CODE}}{Character, three-letter country code}
#'   \item{\code{WHO_CODE}}{Character, WHO-specific country code}
#'   \item{\code{ADM2_NAME}}{Character, name of the admin level 2 division}
#'   \item{\code{ADM1_NAME}}{Character, name of the admin level 1 division}
#'   \item{\code{ADM0_NAME}}{Character, name of the country}
#'   \item{\code{ADM2_CODE}}{Character, admin level 2 code}
#'   \item{\code{ADM1_CODE}}{Character, admin level 1 code}
#'   \item{\code{ADM0_CODE}}{Character, country code}
#'   \item{\code{STARTDATE}}{POSIXct, start date of the data validity}
#'   \item{\code{ENDDATE}}{POSIXct, end date of the data validity}
#'   \item{\code{GUID}}{Character, globally unique identifier}
#'   \item{\code{ADM2_ALTNAME}}{Character, alternative name for level 2 div}
#'   \item{\code{ADM2_ALTCODE}}{Character, alternative code for level 2 div}
#'   \item{\code{LVL}}{Integer, admin level}
#'   \item{\code{WHO_STATUS}}{Character, membership status in WHO}
#'   \item{\code{UN_CODE}}{Integer, United Nations code}
#'   \item{\code{CENTER_LON}}{Numeric, longitude of the center point}
#'   \item{\code{CENTER_LAT}}{Numeric, latitude of the center point}
#'   \item{\code{GlobalID}}{Character, another form of unique identifier}
#'   \item{\code{ADM1_VIZ_NAME}}{Character, visual name for level 1 division}
#'   \item{\code{ADM2_VIZ_NAME}}{Character, visual name for level 2 division}
#'   \item{\code{ADM1_GUID}}{Character, GUID for admin level 1}
#'   \item{\code{ADM0_GUID}}{Character, GUID for admin level 0 (country)}
#'   \item{\code{NOTES}}{Character, additional notes}
#'   \item{\code{ADM0_VIZ_NAME}}{Character, visual name for the country}
#'   \item{\code{WHO_SUBREGION}}{Character, WHO subregion classification}
#' }
#' @source https://hub.arcgis.com/datasets/WHO::polio-administrative-boundaries
#' @keywords datasets
"shp_global"

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
