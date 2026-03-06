# ── WPP 2024 demographic inputs ──────────────────────────────────────────────

#' Build EMOD Demographic Inputs from UN WPP 2024 Data
#'
#' Pulls age structure and vital rates from the UN World Population
#' Prospects 2024 and returns EMOD-ready AgeDistribution, birth rate,
#' and death rate fields.
#'
#' @param country Character. Country name as used in wpp2024
#'   (e.g. "Burundi", "Kenya", "Nigeria").
#' @param year Integer. Reference year for demographic data.
#' @return A named list with:
#'   \describe{
#'     \item{age_distribution}{List with DistributionValues (CDF),
#'       ResultScaleFactor, ResultValues (age in days).}
#'     \item{crude_birth_rate}{Numeric. Annual births per 1000.}
#'     \item{crude_death_rate}{Numeric. Annual deaths per 1000.}
#'     \item{birth_rate_daily}{Numeric. BirthRate for EMOD
#'       NodeAttributes.}
#'     \item{country}{Character. Matched country name.}
#'     \item{year}{Integer. Reference year used.}
#'   }
#' @examples
#' \dontrun{
#' demog <- build_emod_demog_from_wpp("Burundi", 2022)
#' demog$crude_birth_rate
#' }
#' @export
build_emod_demog_from_wpp <- function(country, year) {

  rlang::check_installed(
    "wpp2024",
    reason = paste0(
      "to pull UN WPP 2024 data. ",
      "Install with: devtools::install_github('PPgp/wpp2024')"
    )
  )

  # load wpp2024 datasets into a local env
  env <- new.env(parent = baseenv())
  utils::data("popAge1dt", package = "wpp2024", envir = env)
  utils::data("misc1dt", package = "wpp2024", envir = env)
  pop_age <- env[["popAge1dt"]]
  misc <- env[["misc1dt"]]

  # validate country
  available <- unique(pop_age$name)
  matched <- grep(
    paste0("^", country, "$"), available,
    value = TRUE, ignore.case = TRUE
  )

  if (length(matched) == 0) {
    matched <- grep(
      country, available,
      value = TRUE, ignore.case = TRUE
    )
    if (length(matched) == 0) {
      cli::cli_abort(
        "Country {.val {country}} not found in wpp2024."
      )
    }
    cli::cli_alert_info(
      "Partial match: using {.val {matched[1]}} for {.val {country}}"
    )
  }
  country_name <- matched[1]

  # validate year
  available_years <- unique(
    pop_age[pop_age$name == country_name, ]$year
  )
  if (!year %in% available_years) {
    closest <- available_years[
      which.min(abs(available_years - year))
    ]
    cli::cli_alert_warning(
      "Year {year} not available. Using closest: {closest}"
    )
    year <- closest
  }

  # build age distribution (CDF)
  cli::cli_alert_info(
    "Building age distribution for {country_name} ({year})"
  )

  age_data <- pop_age[
    pop_age$name == country_name & pop_age$year == year,
  ]
  age_data <- age_data[order(age_data$age), ]

  # pop is in thousands
  age_data$pop_actual <- age_data$pop * 1000
  total_pop <- sum(age_data$pop_actual, na.rm = TRUE)
  age_data$prop <- age_data$pop_actual / total_pop
  age_data$cdf <- cumsum(age_data$prop)
  age_data$age_days <- age_data$age * 365

  age_distribution <- list(
    DistributionValues = list(age_data$cdf),
    ResultScaleFactor = 1,
    ResultValues = list(age_data$age_days)
  )

  # get vital rates
  vital <- misc[
    misc$name == country_name & misc$year == year,
  ]

  if (nrow(vital) == 0) {
    vital_years <- unique(
      misc[misc$name == country_name, ]$year
    )
    closest_v <- vital_years[
      which.min(abs(vital_years - year))
    ]
    cli::cli_alert_warning(
      "Vital rates not available for {year}. Using {closest_v}"
    )
    vital <- misc[
      misc$name == country_name & misc$year == closest_v,
    ]
  }

  crude_birth_rate <- vital$cbr[1]
  crude_death_rate <- vital$cdr[1]
  birth_rate_daily <- crude_birth_rate / 365.0

  cli::cli_alert_success(
    "CBR: {round(crude_birth_rate, 2)}, ",
    "CDR: {round(crude_death_rate, 2)} (per 1000/year)"
  )

  list(
    age_distribution = age_distribution,
    crude_birth_rate = crude_birth_rate,
    crude_death_rate = crude_death_rate,
    birth_rate_daily = birth_rate_daily,
    country          = country_name,
    year             = year
  )
}

# ── Single-node demographics builder ────────────────────────────────────────

#' Build EMOD Demographics JSON for a Single Node
#'
#' Creates the demographics list structure expected by EMOD for one
#' admin unit. Output matches the emod-api Python
#' \code{from_template_node()} format.
#'
#' @param lat Numeric. Latitude of the node centroid.
#' @param lon Numeric. Longitude of the node centroid.
#' @param pop Integer. Initial population for the node.
#' @param name Character. Name for the node (FacilityName).
#' @param crude_death_rate Numeric. Annual deaths per 1000.
#' @param crude_birth_rate Numeric. Annual births per 1000.
#' @param node_id Integer. Node ID. Default 1L. Must match the
#'   NodeID used in climate .bin.json files.
#' @param id_reference Character. IdReference string. Default
#'   "Legacy". Must match the IdReference in climate .bin.json.
#' @param age_distribution List with DistributionValues,
#'   ResultScaleFactor, ResultValues. NULL uses SSAfrica default.
#' @param individual_properties List of IP definitions. NULL uses
#'   default AgeGroup, DrugStatus, SMCAccess, VaccineStatus.
#' @param initial_prevalence Numeric. Default 0.
#' @param prevalence_distribution Numeric vector of length 2.
#'   Default c(0.2, 0.2).
#' @return A named list representing the demographics JSON.
#' @export
build_emod_demog <- function(
    lat,
    lon,
    pop,
    name,
    crude_death_rate,
    crude_birth_rate,
    node_id = 1L,
    id_reference = "Legacy",
    age_distribution = NULL,
    individual_properties = NULL,
    initial_prevalence = 0,
    prevalence_distribution = c(0.2, 0.2)
) {

  # default SSAfrica age distribution (from emod-api Python)
  if (is.null(age_distribution)) {
    age_distribution <- .default_age_distribution()
  }

  # default individual properties
  if (is.null(individual_properties)) {
    individual_properties <- .default_individual_properties()
  }

  # birth rate: per person per day
  birth_rate_daily <- crude_birth_rate / 1000 / 365

  list(
    Defaults = list(
      IndividualAttributes = list(
        AgeDistribution = age_distribution,
        AgeDistribution1 = 0,
        AgeDistribution2 = 18250,
        AgeDistributionFlag = 1,
        ImmunityDistribution1 = 1,
        ImmunityDistribution2 = 0,
        ImmunityDistributionFlag = 0,
        InitialPrevalence = initial_prevalence,
        MortalityDistribution = list(
          AxisNames = c("gender", "age"),
          AxisScaleFactors = c(1, 365),
          AxisUnits = c("male=0,female=1", "years"),
          NumDistributionAxes = 2,
          NumPopulationGroups = c(2, 1),
          PopulationGroups = list(c(0, 1), I(c(0))),
          ResultScaleFactor = 2.74e-06,
          ResultUnits = "annual deaths per 1000 individuals",
          ResultValues = list(
            I(c(crude_death_rate)),
            I(c(crude_death_rate))
          )
        ),
        PrevalenceDist_Description = paste0(
          "Uniform draw from ",
          prevalence_distribution[1],
          " to ",
          prevalence_distribution[2]
        ),
        PrevalenceDistribution1 = prevalence_distribution[1],
        PrevalenceDistribution2 = prevalence_distribution[2],
        PrevalenceDistributionFlag = 1,
        PrevalenceDistribution_Description = paste0(
          "Constant Initial Prevalence (",
          prevalence_distribution[1], ")"
        ),
        RiskDist_Description = "LogNormal distributed risk",
        RiskDistribution1 = 1,
        RiskDistribution2 = 0,
        RiskDistributionFlag = 3
      ),
      IndividualProperties = individual_properties,
      NodeAttributes = list(
        Airport = 1,
        Altitude = 0,
        BirthRate = birth_rate_daily,
        Region = 1,
        Seaport = 1
      )
    ),
    Metadata = list(
      Author = Sys.info()[["user"]],
      DateCreated = format(Sys.Date(), "%m/%d/%Y"),
      IdReference = id_reference,
      NodeCount = 1,
      Tool = "emod-api"
    ),
    Nodes = list(
      list(
        IndividualAttributes = structure(
          list(), names = character(0)
        ),
        NodeAttributes = list(
          FacilityName = name,
          InitialPopulation = as.integer(pop),
          Latitude = lat,
          Longitude = lon
        ),
        NodeID = as.integer(node_id)
      )
    )
  )
}

# ── Write demographics per ADM2 ────────────────────────────────────────────

#' Write EMOD Demographics JSON Files by ADM2
#'
#' Writes one demographics JSON per admin-2 unit, mirroring the
#' folder layout used by [write_emod_weather_by_adm2()]. Each
#' folder gets `demographics.json` with matching `IdReference`,
#' `NodeID = 1`, and `NodeCount = 1`.
#'
#' @param df Data frame with columns: `adm2`, `lat`, `lon`, `pop`,
#'   `crude_death_rate`, `crude_birth_rate`.
#' @param output_dir Character. Root directory for output folders.
#' @param folder_case Case style for subfolder names: `"title"`
#'   (default), `"upper"`, or `"lower"`. Must match the case used
#'   by [write_emod_weather_by_adm2()].
#' @param demog_prefix Character. Prefix before "demographics" in
#'   the filename. Default `""` gives `"demographics.json"`. Set
#'   to NULL to auto-prefix with the folder name.
#' @param demog_suffix Character. Optional suffix before .json
#'   (e.g. "_wSMC_risk").
#' @param id_reference Character. IdReference string. Default
#'   "Legacy". Must match the climate files.
#' @param individual_properties Optional list of IP definitions.
#' @param age_distribution Optional age distribution list.
#' @return Invisible NULL. Called for side effect of writing files.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   adm2 = c("Bubanza", "Cibitoke"),
#'   lat = c(-3.08, -2.89),
#'   lon = c(29.37, 29.18),
#'   pop = c(100000L, 120000L),
#'   crude_death_rate = c(8.5, 8.5),
#'   crude_birth_rate = c(38.0, 38.0)
#' )
#' write_emod_demog_by_adm2(df, tempdir())
#' }
#' @export
write_emod_demog_by_adm2 <- function(
    df,
    output_dir,
    folder_case = "title",
    demog_prefix = "",
    demog_suffix = "",
    id_reference = "Legacy",
    individual_properties = NULL,
    age_distribution = NULL
) {

  rlang::check_installed(
    "jsonlite", reason = "to write demographics JSON"
  )

  folder_case <- match.arg(
    folder_case, c("title", "upper", "lower")
  )

  case_fn <- switch(
    folder_case,
    title = stringr::str_to_title,
    upper = toupper,
    lower = tolower
  )

  # validate required columns
  required <- c(
    "adm2", "lat", "lon", "pop",
    "crude_death_rate", "crude_birth_rate"
  )
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Missing column{?s} in df: {.val {missing_cols}}"
    )
  }

  cli::cli_progress_bar(
    "Writing demographics", total = nrow(df)
  )

  for (i in seq_len(nrow(df))) {
    row <- df[i, ]

    # clean + case folder name (same logic as weather)
    folder_name <- gsub("[^A-Za-z0-9]+", "_", row$adm2) |>
      case_fn()

    demog <- build_emod_demog(
      lat = row$lat,
      lon = row$lon,
      pop = row$pop,
      name = folder_name,
      crude_death_rate = row$crude_death_rate,
      crude_birth_rate = row$crude_birth_rate,
      node_id = 1L,
      id_reference = id_reference,
      age_distribution = age_distribution,
      individual_properties = individual_properties
    )

    dir_path <- file.path(output_dir, folder_name)
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)

    prefix <- if (is.null(demog_prefix)) {
      paste0(folder_name, "_")
    } else {
      demog_prefix
    }

    file_path <- file.path(
      dir_path,
      paste0(prefix, "demographics", demog_suffix, ".json")
    )

    jsonlite::write_json(
      demog, file_path,
      auto_unbox = TRUE, pretty = TRUE, digits = NA
    )

    cli::cli_progress_update()
  }

  cli::cli_progress_done()
  cli::cli_alert_success(
    "Wrote {nrow(df)} demographics file{?s} to {.path {output_dir}}"
  )

  invisible(NULL)
}

# ── Internal helpers ────────────────────────────────────────────────────────

#' Default SSAfrica age distribution from emod-api
#' @noRd
.default_age_distribution <- function() {
  list(
    DistributionValues = c(
      0, 0.0874071047257126, 0.16737703029028825,
      0.24054255502934432, 0.3074826208089575,
      0.3686613109892203, 0.4246989953576121,
      0.47596861687986874, 0.522875857439861,
      0.5657918836833704, 0.6050561581209166,
      0.6409789502300984, 0.673845189729804,
      0.7038827889943505, 0.7313967340232171,
      0.7565694182395838, 0.7795999267083209,
      0.800670843446059, 0.8199488968312877,
      0.8375866300551988, 0.8537235049479931,
      0.8684713425858462, 0.8819801447631247,
      0.8943395212968485, 0.9056472694733465,
      0.915992766949792, 0.9254579031519203,
      0.9341176809019833, 0.9420406227370048,
      0.9492816775343581, 0.9559142701362873,
      0.9619824759265774, 0.9675343569769829,
      0.9726138439176826, 0.9772611297725079,
      0.981512933676782, 0.9854029390818465,
      0.9889581498026758, 0.9922146658619191,
      0.9951940927775798, 0.9979199629370703,
      rep(1.0000000000000013, 9)
    ),
    ResultScaleFactor = 1,
    ResultValues = c(
      0, 894, 1788, 2682, 3576, 4469, 5363, 6257,
      7151, 8045, 8939, 9833, 10727, 11620, 12514,
      13408, 14302, 15196, 16090, 16984, 17878,
      18771, 19665, 20559, 21453, 22347, 23241,
      24135, 25029, 25922, 26816, 27710, 28604,
      29498, 30392, 31286, 32180, 33073, 33967,
      34861, 35755, 36649, 37543, 38437, 39331,
      40224, 41118, 42012, 42906, 43800
    )
  )
}

#' Default EMOD individual properties
#' @noRd
.default_individual_properties <- function() {
  list(
    list(
      Initial_Distribution = c(1, 0, 0, 0),
      Property = "AgeGroup",
      Values = c("Under15", "15to30", "30to50", "50plus")
    ),
    list(
      Initial_Distribution = c(1, 0),
      Property = "DrugStatus",
      Values = c("None", "RecentDrug")
    ),
    list(
      Initial_Distribution = c(0.5, 0.5),
      Property = "SMCAccess",
      Values = c("Low", "High")
    ),
    list(
      Initial_Distribution = c(1, 0, 0, 0),
      Property = "VaccineStatus",
      Values = c(
        "None", "GotVaccine",
        "GotBooster1", "GotBooster2"
      )
    )
  )
}
