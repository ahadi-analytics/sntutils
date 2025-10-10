# Example: Using prep_geonames with various file formats
# The function now uses sntutils read() and write() functions
# supporting all their file formats

library(sntutils)

# Create sample data with administrative names
sample_data <- data.frame(
  country = c("KENYA", "UGANDA", "TANZANIA"),
  province = c("NYANZA", "CENTRAL", "ARUSHA"),
  district = c("KISUMU", "KAMPALA", "MOSHI"),
  stringsAsFactors = FALSE
)

# Example 1: Using RDS cache (default)
cache_rds <- prep_geonames(
  sample_data,
  level0 = "country",
  level1 = "province",
  level2 = "district",
  cache_path = "geoname_cache.rds",
  interactive = FALSE
)

# Example 2: Using CSV cache
cache_csv <- prep_geonames(
  sample_data,
  level0 = "country", 
  level1 = "province",
  level2 = "district",
  cache_path = "geoname_cache.csv",
  interactive = FALSE
)

# Example 3: Using Excel cache
cache_xlsx <- prep_geonames(
  sample_data,
  level0 = "country",
  level1 = "province", 
  level2 = "district",
  cache_path = "geoname_cache.xlsx",
  interactive = FALSE
)

# Example 4: Using Stata DTA cache
cache_dta <- prep_geonames(
  sample_data,
  level0 = "country",
  level1 = "province",
  level2 = "district", 
  cache_path = "geoname_cache.dta",
  interactive = FALSE
)

# Example 5: Export unmatched records in different formats
# Excel format
result_xlsx <- prep_geonames(
  sample_data,
  level0 = "country",
  level1 = "province",
  level2 = "district",
  cache_path = "cache.rds",
  unmatched_export_path = "unmatched_records.xlsx",
  interactive = FALSE
)

# CSV format  
result_csv <- prep_geonames(
  sample_data,
  level0 = "country",
  level1 = "province",
  level2 = "district",
  cache_path = "cache.rds", 
  unmatched_export_path = "unmatched_records.csv",
  interactive = FALSE
)

# TSV format
result_tsv <- prep_geonames(
  sample_data,
  level0 = "country",
  level1 = "province",
  level2 = "district",
  cache_path = "cache.rds",
  unmatched_export_path = "unmatched_records.tsv", 
  interactive = FALSE
)

# The function now supports all file formats that sntutils read() and write() support:
# - CSV (.csv)
# - TSV (.tsv) 
# - Excel (.xlsx, .xls)
# - Stata (.dta)
# - SPSS (.sav)
# - RDS (.rds)
# - RData (.RData)
# - and more...