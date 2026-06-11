# French malaria acronyms mapping

returns a tibble of English\<-\>French acronyms and their full phrases
used in malaria program contexts. the first French acronym listed is
treated as the preferred one when enforcing acronyms.

## Usage

``` r
french_malaria_acronyms()
```

## Value

tibble with columns: english_acronyms, english_full, french_acronyms,
french_full, preferred_french_acronym

## Examples

``` r
tbl <- french_malaria_acronyms()
tbl$preferred_french_acronym
#>  [1] "ASBC"    "PEC"     "PECADOM" "iCCM"    "CTA"     "CPS"     "TPIg"   
#>  [8] "TPIn"    "PID"     "MILDA"   "PEV"     "OMS"     "FM"      "EDS"    
#> [15] "EPI"     "DS"      "CSP"     "M5"      "M1"      "CPP"     "TDR"    
#> [22] "PSN"     "DF"      "FOSA"   
```
