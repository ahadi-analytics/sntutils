# Rebuild validation_terms data object from YAML
# This script regenerates the validation_terms.rda file from validation_terms.yml

library(yaml)

validation_terms <- yaml::read_yaml(
  here::here("data-raw", "validation_terms.yml")
)

usethis::use_data(validation_terms, overwrite = TRUE)

cat("validation_terms data object has been rebuilt from validation_terms.yml\n")
