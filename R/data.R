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
