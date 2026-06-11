# SNT Variable Tree

A hierarchical data structure containing standardized variable names,
multilingual labels, and metadata for Subnational Tailoring (SNT) of
malaria interventions. This dataset provides the foundation for variable
validation, labeling, and data dictionary generation across the SNT
workflow.

## Usage

``` r
snt_var_tree
```

## Format

A named list with the following top-level elements:

- \_meta:

  Metadata about the variable tree structure

- metadata:

  Variables related to dataset metadata and identifiers

- population:

  Population-related variables and demographics

- routine_data:

  Health facility routine data variables

- interventions:

  Malaria intervention coverage variables

- stock_management:

  Stock and supply chain management variables

- anemia:

  Anemia-related health indicators

- validation_utility:

  Validation and utility terms for data dictionaries, validation
  reports, and analysis outputs (rate/trend analysis, outliers,
  consistency checks)

- schema:

  Schema definitions for variable components including:

  - test_types: Diagnostic test type codes and labels

  - age_groups: Age group classifications

  - population_groups: Population subgroup definitions

  - sectors: Health sector classifications

  - service_levels: Service delivery level definitions

## Source

Subnational Tailoring Initiative for Malaria Interventions

## Details

Each variable entry contains:

- `snt_var_name`: Standardized variable name

- `label_en`: English label

- `label_fr`: French label

- `label_pt`: Portuguese label (where available)

- Additional metadata such as disaggregation options

The schema section provides the building blocks for constructing and
parsing SNT variable names, which follow the pattern:
`domain_testtype_agegroup_populationgroup_sector_servicelevel`

## See also

[`snt_data_dict`](https://ahadi-analytics.github.io/sntutils/reference/snt_data_dict.md)
for flattening the tree into a tidy format,
[`check_snt_var`](https://ahadi-analytics.github.io/sntutils/reference/check_snt_var.md)
for parsing variable names,
[`build_dictionary`](https://ahadi-analytics.github.io/sntutils/reference/build_dictionary.md)
for generating data dictionaries

## Examples

``` r
# Load the dataset
data(snt_var_tree)

# View top-level structure
names(snt_var_tree)
#>  [1] "_meta"              "metadata"           "population"        
#>  [4] "routine_data"       "interventions"      "stock_management"  
#>  [7] "anemia"             "dhs_u5mr"           "dhs_csb"           
#> [10] "dhs_itn"            "dhs_wealth"         "dhs_pfpr"          
#> [13] "dhs_iptp"           "dhs_anemia"         "mbg_indicators"    
#> [16] "schema"             "validation_utility" "survey_indicators" 

# Access schema information
names(snt_var_tree$schema)
#>   [1] "_description"                  "label_en"                     
#>   [3] "label_fr"                      "label_pt"                     
#>   [5] "age_groups"                    "population_groups"            
#>   [7] "sectors"                       "test_types"                   
#>   [9] "service_levels"                "sex"                          
#>  [11] "rainfall_mm"                   "total_rainfall_mm"            
#>  [13] "mean_temperature_c"            "min_temperature_c"            
#>  [15] "max_temperature_c"             "mean_humidity_pct"            
#>  [17] "median_humidity_pct"           "min_humidity_pct"             
#>  [19] "max_humidity_pct"              "humidity_pct"                 
#>  [21] "mean_air_temperature_c"        "min_air_temperature_c"        
#>  [23] "max_air_temperature_c"         "mean_land_temperature_c"      
#>  [25] "min_land_temperature_c"        "max_land_temperature_c"       
#>  [27] "mean_rainfall_mm"              "median_rainfall_mm"           
#>  [29] "min_rainfall_mm"               "max_rainfall_mm"              
#>  [31] "median_air_temperature_c"      "mean_max_air_temperature_c"   
#>  [33] "median_max_air_temperature_c"  "min_max_air_temperature_c"    
#>  [35] "max_max_air_temperature_c"     "mean_min_air_temperature_c"   
#>  [37] "median_min_air_temperature_c"  "min_min_air_temperature_c"    
#>  [39] "max_min_air_temperature_c"     "median_land_temperature_c"    
#>  [41] "mean_max_land_temperature_c"   "median_max_land_temperature_c"
#>  [43] "min_max_land_temperature_c"    "max_max_land_temperature_c"   
#>  [45] "mean_min_land_temperature_c"   "median_min_land_temperature_c"
#>  [47] "min_min_land_temperature_c"    "max_min_land_temperature_c"   
#>  [49] "pfpr_rdt_u5_mean"              "pfpr_rdt_u5_lower"            
#>  [51] "pfpr_rdt_u5_upper"             "pfpr_mic_u5_mean"             
#>  [53] "pfpr_mic_u5_lower"             "pfpr_mic_u5_upper"            
#>  [55] "pfpr_rdt_5_10_mean"            "pfpr_rdt_5_10_lower"          
#>  [57] "pfpr_rdt_5_10_upper"           "pfpr_mic_5_10_mean"           
#>  [59] "pfpr_mic_5_10_lower"           "pfpr_mic_5_10_upper"          
#>  [61] "pfpr_rdt_u10_mean"             "pfpr_rdt_u10_lower"           
#>  [63] "pfpr_rdt_u10_upper"            "pfpr_mic_u10_mean"            
#>  [65] "pfpr_mic_u10_lower"            "pfpr_mic_u10_upper"           
#>  [67] "pfpr_rdt_2_10_mean"            "pfpr_rdt_2_10_lower"          
#>  [69] "pfpr_rdt_2_10_upper"           "pfpr_mic_2_10_mean"           
#>  [71] "pfpr_mic_2_10_lower"           "pfpr_mic_2_10_upper"          
#>  [73] "itn_ownership_mean"            "itn_ownership_lower"          
#>  [75] "itn_ownership_upper"           "itn_access_mean"              
#>  [77] "itn_access_lower"              "itn_access_upper"             
#>  [79] "itn_use_all_mean"              "itn_use_all_lower"            
#>  [81] "itn_use_all_upper"             "itn_use_u5_mean"              
#>  [83] "itn_use_u5_lower"              "itn_use_u5_upper"             
#>  [85] "itn_use_5_10_mean"             "itn_use_5_10_lower"           
#>  [87] "itn_use_5_10_upper"            "itn_use_10_20_mean"           
#>  [89] "itn_use_10_20_lower"           "itn_use_10_20_upper"          
#>  [91] "itn_use_20plus_mean"           "itn_use_20plus_lower"         
#>  [93] "itn_use_20plus_upper"          "itn_use_pregnant_mean"        
#>  [95] "itn_use_pregnant_lower"        "itn_use_pregnant_upper"       
#>  [97] "itn_use_if_access_mean"        "itn_use_if_access_lower"      
#>  [99] "itn_use_if_access_upper"       "irs_coverage_mean"            
#> [101] "irs_coverage_lower"            "irs_coverage_upper"           
#> [103] "anc_anc1_mean"                 "anc_anc1_lower"               
#> [105] "anc_anc1_upper"                "anc_anc4_mean"                
#> [107] "anc_anc4_lower"                "anc_anc4_upper"               
#> [109] "anc_anc8_mean"                 "anc_anc8_lower"               
#> [111] "anc_anc8_upper"                "csb_any_mean"                 
#> [113] "csb_any_lower"                 "csb_any_upper"                
#> [115] "csb_public_mean"               "csb_public_lower"             
#> [117] "csb_public_upper"              "csb_private_mean"             
#> [119] "csb_private_lower"             "csb_private_upper"            
#> [121] "csb_trained_mean"              "csb_trained_lower"            
#> [123] "csb_trained_upper"             "csb_none_mean"                
#> [125] "csb_none_lower"                "csb_none_upper"               
#> [127] "csb_act_mean"                  "csb_act_lower"                
#> [129] "csb_act_upper"                 "anemia_any_mean"              
#> [131] "anemia_any_lower"              "anemia_any_upper"             
#> [133] "anemia_moderate_plus_mean"     "anemia_moderate_plus_lower"   
#> [135] "anemia_moderate_plus_upper"    "anemia_severe_mean"           
#> [137] "anemia_severe_lower"           "anemia_severe_upper"          
#> [139] "anemia_mild_only_mean"         "anemia_mild_only_lower"       
#> [141] "anemia_mild_only_upper"        "anemia_moderate_only_mean"    
#> [143] "anemia_moderate_only_lower"    "anemia_moderate_only_upper"   
#> [145] "anemia_severe_only_mean"       "anemia_severe_only_lower"     
#> [147] "anemia_severe_only_upper"      "iptp_1plus_mean"              
#> [149] "iptp_1plus_lower"              "iptp_1plus_upper"             
#> [151] "iptp_2plus_mean"               "iptp_2plus_lower"             
#> [153] "iptp_2plus_upper"              "iptp_3plus_mean"              
#> [155] "iptp_3plus_lower"              "iptp_3plus_upper"             
#> [157] "iptp_4plus_mean"               "iptp_4plus_lower"             
#> [159] "iptp_4plus_upper"              "iptp_1only_mean"              
#> [161] "iptp_1only_lower"              "iptp_1only_upper"             
#> [163] "iptp_2only_mean"               "iptp_2only_lower"             
#> [165] "iptp_2only_upper"              "iptp_3only_mean"              
#> [167] "iptp_3only_lower"              "iptp_3only_upper"             
#> [169] "epi_bcg_mean"                  "epi_bcg_lower"                
#> [171] "epi_bcg_upper"                 "epi_dpt1_mean"                
#> [173] "epi_dpt1_lower"                "epi_dpt1_upper"               
#> [175] "epi_dpt2_mean"                 "epi_dpt2_lower"               
#> [177] "epi_dpt2_upper"                "epi_dpt3_mean"                
#> [179] "epi_dpt3_lower"                "epi_dpt3_upper"               
#> [181] "epi_polio3_mean"               "epi_polio3_lower"             
#> [183] "epi_polio3_upper"              "epi_measles1_mean"            
#> [185] "epi_measles1_lower"            "epi_measles1_upper"           
#> [187] "epi_measles2_mean"             "epi_measles2_lower"           
#> [189] "epi_measles2_upper"            "epi_full_mean"                
#> [191] "epi_full_lower"                "epi_full_upper"               
#> [193] "epi_malaria_mean"              "epi_malaria_lower"            
#> [195] "epi_malaria_upper"             "u5mr_q_under1_mean"           
#> [197] "u5mr_q_under1_lower"           "u5mr_q_under1_upper"          
#> [199] "u5mr_q_1yr_mean"               "u5mr_q_1yr_lower"             
#> [201] "u5mr_q_1yr_upper"              "u5mr_q_2yr_mean"              
#> [203] "u5mr_q_2yr_lower"              "u5mr_q_2yr_upper"             
#> [205] "u5mr_q_3yr_mean"               "u5mr_q_3yr_lower"             
#> [207] "u5mr_q_3yr_upper"              "u5mr_q_4yr_mean"              
#> [209] "u5mr_q_4yr_lower"              "u5mr_q_4yr_upper"             
#> [211] "u5mr_combined_mean"            "u5mr_combined_lower"          
#> [213] "u5mr_combined_upper"           "smc_receipt_mean"             
#> [215] "smc_receipt_lower"             "smc_receipt_upper"            

# View age groups
snt_var_tree$schema$age_groups
#> $u1
#> $u1$label_en
#> [1] "Under 1 year"
#> 
#> $u1$label_fr
#> [1] "Moins de 1 an"
#> 
#> $u1$label_pt
#> [1] "Menos de 1 ano"
#> 
#> 
#> $u5
#> $u5$label_en
#> [1] "Under 5 years"
#> 
#> $u5$label_fr
#> [1] "Moins de 5 ans"
#> 
#> $u5$label_pt
#> [1] "Menos de 5 anos"
#> 
#> 
#> $ov5
#> $ov5$label_en
#> [1] "5 years and above"
#> 
#> $ov5$label_fr
#> [1] "5 ans et plus"
#> 
#> $ov5$label_pt
#> [1] "5 anos e mais"
#> 
#> 
#> $ov15
#> $ov15$label_en
#> [1] "15 years and above"
#> 
#> $ov15$label_fr
#> [1] "15 ans et plus"
#> 
#> $ov15$label_pt
#> [1] "15 anos e mais"
#> 
#> 
```
