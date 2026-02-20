###############################################################################
#                TOGO — Telecharger et traiter les donnees climatiques
#
#  Ce script montre 4 approches pour obtenir des donnees climatiques
#  sous forme de tableaux par unite administrative :
#
#    1. CHIRPS  — Pluviometrie       (public, pas de compte requis)
#    2. MODIS   — Vegetation / LST   (compte NASA Earthdata requis)
#    3. ERA5    — Temperature, pluie (cle API Copernicus CDS requise)
#    4. POWER   — Donnees ponctuelles (public, pas de compte requis)
#
#  Les 4 pipelines suivent le meme schema :
#    telecharger -> traiter les rasters -> tableau tidy -> sauvegarder
#
#  (POWER saute l'etape raster — il retourne directement un tableau tidy.)
#
#  NOTE : pour la demonstration, on telecharge uniquement l'annee 2024.
#  En production, il suffit d'elargir les dates (ex. start = "2015-01",
#  end = "2025-12") pour couvrir plusieurs annees.
#
#  ---- Identifiants requis ----
#
#  Deux sources exigent des identifiants (gratuits) :
#
#  MODIS (NASA Earthdata) :
#    1. Creer un compte sur https://urs.earthdata.nasa.gov/
#    2. Ajouter ces lignes dans votre fichier .Renviron
#       (ouvrir avec : usethis::edit_r_environ()) :
#
#         EARTHDATA_USERNAME=votre_utilisateur
#         EARTHDATA_PASSWORD=votre_mot_de_passe
#
#    3. Redemarrer R pour que les variables soient chargees.
#    4. Dans le code, les recuperer avec :
#         Sys.getenv("EARTHDATA_USERNAME")
#         Sys.getenv("EARTHDATA_PASSWORD")
#
#  ERA5 (Copernicus CDS) :
#    1. Creer un compte sur https://cds.climate.copernicus.eu/
#    2. Copier votre cle API depuis votre profil :
#       https://cds.climate.copernicus.eu/profile
#    3. Ajouter dans .Renviron :
#
#         ERA5_API_KEY=votre_cle_api
#
#    4. Redemarrer R. Recuperer avec :
#         Sys.getenv("ERA5_API_KEY")
#
#  CHIRPS et POWER sont publics — aucun identifiant necessaire.
#
###############################################################################

cli::cli_h1("Togo — Telecharger et traiter les donnees climatiques")

## ---------------------------------------------------------------------------
## 0. Configuration, donnees et parametres
## ---------------------------------------------------------------------------

cli::cli_process_start(
  "Setting up environment and loading data",
  msg_done = "Environment set up and data loaded"
)

# identifiants du pays
iso3 <- "tgo"
adm0_name <- "Togo"

# chemins du projet
paths <- sntutils::setup_project_paths(
  base_path = Sys.getenv("AHADI_ONEDRIVE_PROJECT"),
  quiet = TRUE
)

clim_path <- here::here(paths$climate)

# charger le shapefile admin-2 (districts)
shp_adm2 <- sntutils::read_snt_data(
  path = here::here(paths$admin_shp, "processed"),
  data_name = glue::glue("{iso3}_shp_list"),
  file_formats = c("qs2")
)$final_spat_vec$adm2 |>
  dplyr::group_by(adm0, adm1, adm2) |>
  dplyr::summarise(
    geometry = sf::st_union(geometry),
    .groups = "drop"
  )

cli::cli_process_done()


###############################################################################
#
#  1. CHIRPS — Pluviometrie
#
#  Source : UCSB Climate Hazards Center
#  Identifiants : AUCUN (donnees publiques)
#  Resolution : ~5 km, mensuelle
#  Resultat : un GeoTIFF par mois
#
#  Fonctions utiles :
#    sntutils::chirps_options()            — lister les jeux de donnees
#    sntutils::check_chirps_available()    — verifier ce qui est en ligne
#    sntutils::download_chirps()           — telecharger
#    sntutils::process_raster_collection() — extraire stats par admin
#
###############################################################################

cli::cli_h1("1. CHIRPS — Pluviometrie")

## -- telecharger ------------------------------------------------------------

# on telecharge 2024 uniquement pour la demo ;
# changer start/end pour couvrir plus d'annees
sntutils::download_chirps(
  dataset = "africa_monthly",
  start = "2024-01",
  end = "2024-12",
  out_dir = here::here(clim_path, "chirps", "raw")
)

## -- traiter ----------------------------------------------------------------

rain_data <- sntutils::process_raster_collection(
  directory = here::here(clim_path, "chirps", "raw"),
  shapefile = shp_adm2,
  aggregations = c("sum", "mean", "median")
) |>
  dplyr::mutate(
    location = paste(adm1, "~", adm2),
    date = paste(year, month, "01", sep = "-") |> as.Date(),
    yearmon = factor(
      sntutils::translate_yearmon(date),
      levels = sntutils::translate_yearmon(sort(unique(date)))
    )
  ) |>
  dplyr::distinct(
    adm0, adm1, adm2, location,
    year, month, yearmon,
    total_rainfall_mm = sum,
    mean_rainfall_mm = mean,
    median_rainfall_mm = median
  )

## -- sauvegarder ------------------------------------------------------------

sntutils::write_snt_data(
  obj = list(
    data = rain_data,
    dict = sntutils::build_dictionary(rain_data, language = "fr")
  ),
  data_name = glue::glue("{iso3}_chirps_rainfall"),
  path = here::here(clim_path, "chirps", "processed"),
  file_formats = c("qs2", "xlsx")
)


###############################################################################
#
#  2. NASA MODIS — Vegetation (EVI)
#
#  Source : NASA CMR / LP DAAC
#  Identifiants : nom d'utilisateur + mot de passe NASA Earthdata
#     S'inscrire ici -> https://urs.earthdata.nasa.gov/
#
#  Resolution : 1 km, mensuelle
#  Resultat : un GeoTIFF par mois (mosaique, decoupe, mise a l'echelle)
#
#  Fonctions utiles :
#    sntutils::modis_options()             — parcourir les produits
#    sntutils::modis_options("vegetation") — filtrer par mot-cle
#    sntutils::download_modis()            — telecharger
#    sntutils::process_raster_collection() — extraire stats par admin
#
#  Astuce : definir les identifiants une fois dans .Renviron pour ne
#           pas les coller dans chaque script :
#
#    EARTHDATA_USERNAME=votre_utilisateur
#    EARTHDATA_PASSWORD=votre_mot_de_passe
#
###############################################################################

cli::cli_h1("2. MODIS — EVI (indice de vegetation)")

## -- telecharger ------------------------------------------------------------

# on telecharge 2024 uniquement pour la demo
sntutils::download_modis(
  shapefile = shp_adm2,
  start = "2024-01-01",
  end = "2024-12-31",
  product = "MOD13A3",
  band = "1 km monthly EVI",
  out_dir = here::here(clim_path, "evi", "raw"),
  username = Sys.getenv("EARTHDATA_USERNAME"),
  password = Sys.getenv("EARTHDATA_PASSWORD")
)

## -- traiter ----------------------------------------------------------------

evi_data <- sntutils::process_raster_collection(
  directory = here::here(clim_path, "evi", "raw"),
  shapefile = shp_adm2,
  aggregations = c("mean", "median")
) |>
  dplyr::mutate(
    location = paste(adm1, "~", adm2),
    date = paste(year, month, "01", sep = "-") |> as.Date(),
    yearmon = factor(
      sntutils::translate_yearmon(date),
      levels = sntutils::translate_yearmon(sort(unique(date)))
    )
  ) |>
  dplyr::distinct(
    adm0, adm1, adm2, location,
    year, month, yearmon,
    mean_evi = mean,
    median_evi = median
  )

## -- sauvegarder ------------------------------------------------------------

sntutils::write_snt_data(
  obj = list(
    data = evi_data,
    dict = sntutils::build_dictionary(evi_data, language = "fr")
  ),
  data_name = glue::glue("{iso3}_modis_evi"),
  path = here::here(clim_path, "evi", "processed"),
  file_formats = c("qs2", "xlsx")
)


###############################################################################
#
#  3. ERA5 — Temperature, pluviometrie et humidite du sol
#
#  Source : Copernicus Climate Data Store (CDS)
#  Identifiants : cle API CDS
#     L'obtenir ici -> https://cds.climate.copernicus.eu/profile
#
#  Resolution : ~31 km, mensuelle
#  Resultat : fichiers NetCDF -> convertis en tableau tidy via read_era5()
#
#  Contrairement a CHIRPS/MODIS, ERA5 fournit des fichiers NetCDF qu'on
#  lit avec sntutils::read_era5(). Cette fonction convertit
#  automatiquement les Kelvin en Celsius et les metres en mm
#  si convert_units = TRUE.
#
#  Fonctions utiles :
#    sntutils::era5_options()       — lister les jeux de donnees
#    sntutils::download_era5()      — telecharger
#    sntutils::read_era5()          — NetCDF -> tableau tidy
#
#  Astuce : definir la cle dans .Renviron :
#    ERA5_API_KEY=votre_cle_api_cds
#
###############################################################################

cli::cli_h1("3. ERA5 — Temperature, Pluviometrie et Humidite du sol")

## -- telecharger ------------------------------------------------------------

# boite englobante du Togo : xmin, ymin, xmax, ymax
tgo_bbox <- c(-0.2, 6.0, 1.9, 11.2)

# on telecharge 2024 uniquement pour la demo
sntutils::download_era5(
  dataset = "monthly_single_levels",
  years = 2024,
  months = 1:12,
  variables = c(
    "2m_temperature",
    "total_precipitation",
    "volumetric_soil_water_layer_1"
  ),
  bbox = tgo_bbox,
  out_dir = here::here(clim_path, "era5", "raw"),
  cds_key = Sys.getenv("ERA5_API_KEY")
)

## -- lire et mettre en forme ------------------------------------------------

era5_files <- list.files(
  here::here(clim_path, "era5", "raw"),
  pattern = "\\.nc$",
  full.names = TRUE
)

era5_tidy <- sntutils::read_era5(
  nc_files = era5_files,
  country = "TGO",
  convert_units = TRUE
)

# agreger en moyennes mensuelles par admin-2
# les donnees era5 arrivent sous forme de grille (lon/lat) ;
# on les convertit en sf, on fait une jointure spatiale avec les
# limites administratives, puis on regroupe
era5_sf <- sf::st_as_sf(
  era5_tidy,
  coords = c("lon", "lat"),
  crs = 4326
)

era5_joined <- sf::st_join(era5_sf, shp_adm2, join = sf::st_within)

era5_data <- era5_joined |>
  sf::st_drop_geometry() |>
  dplyr::filter(!is.na(adm2)) |>
  dplyr::group_by(adm0, adm1, adm2, year, month, variable) |>
  dplyr::summarise(
    value = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = variable,
    values_from = value
  ) |>
  dplyr::mutate(
    location = paste(adm1, "~", adm2),
    date = paste(year, month, "01", sep = "-") |> as.Date(),
    yearmon = factor(
      sntutils::translate_yearmon(date),
      levels = sntutils::translate_yearmon(sort(unique(date)))
    )
  )

## -- sauvegarder ------------------------------------------------------------

sntutils::write_snt_data(
  obj = list(
    data = era5_data,
    dict = sntutils::build_dictionary(era5_data, language = "fr")
  ),
  data_name = glue::glue("{iso3}_era5_climate"),
  path = here::here(clim_path, "era5", "processed"),
  file_formats = c("qs2", "xlsx")
)


###############################################################################
#
#  4. NASA POWER — Donnees climatiques journalieres ponctuelles
#
#  Source : API NASA POWER
#  Identifiants : AUCUN (donnees publiques)
#  Resolution : ~50 km, points echantillonnes dans les polygones admin
#  Resultat : tableaux tidy journaliers + mensuels — pas d'etape raster
#
#  POWER echantillonne des points aleatoires dans chaque polygone admin,
#  interroge l'API NASA POWER pour chaque point, puis retourne la
#  mediane. Variables par defaut : pluviometrie, temperature
#  (min/moy/max), temperature de surface, humidite relative.
#
#  C'est l'approche la plus simple — un seul appel de fonction
#  retourne tout.
#
#  Fonctions utiles :
#    sntutils::download_process_nasapower() — telecharger + traiter
#
###############################################################################

cli::cli_h1("4. NASA POWER — Donnees climatiques journalieres")

## -- telecharger et traiter (une seule etape) -------------------------------

power_result <- sntutils::download_process_nasapower(
  adm_sf = shp_adm2,
  admin_cols = c("adm0", "adm1", "adm2"),
  start_date = "2024-01-01",
  end_date = "2024-12-31"
)

# le resultat est une liste avec donnees journalieres + mensuelles
# et les dictionnaires correspondants
power_daily <- power_result$daily
power_monthly <- power_result$monthly

## -- sauvegarder ------------------------------------------------------------

sntutils::write_snt_data(
  obj = list(
    daily = power_daily,
    monthly = power_monthly,
    dict_daily = power_result$dict_daily,
    dict_monthly = power_result$dict_monthly
  ),
  data_name = glue::glue("{iso3}_nasapower_climate"),
  path = here::here(clim_path, "nasapower", "processed"),
  file_formats = c("qs2", "xlsx")
)


###############################################################################
## Termine
###############################################################################

invisible(gc())

cli::cli_rule(
  left = "Tous les telechargements climatiques sont termines",
  right = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)
