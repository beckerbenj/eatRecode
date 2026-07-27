# Get manual recodes

Get manual recodes

## Usage

``` r
extractManualRecode(recodedDf, varName)
```

## Arguments

- recodedDf:

  A data frame with the automatically recoded values.

- varName:

  Character string for the column containing the automatically recoded
  values.

## Value

Returns a data frame which only includes the values that have to be
recoded manually. It can be saved to Excel for manual recoding.

## Examples

``` r
# example data frame
df <- data.frame(id = 1:4,
             country = c("Berlin", "Skotland", "Sweden" , "Cairo"),
             country_recoded = c("Germany", NA, "Sweden", NA))
# extract values to recode manually
manual_recodes <- extractManualRecode(recodedDf = df, varName = "country_recoded")
manual_recodes
#>   id  country newValues
#> 2  2 Skotland      <NA>
#> 4  4    Cairo      <NA>
# export to Excel, edit, import
filePath_temp <- tempfile(fileext = ".xlsx")
writexl::write_xlsx(manual_recodes, path = filePath_temp)
readxl::read_xlsx(filePath_temp)
#> # A tibble: 2 × 3
#>      id country  newValues
#>   <dbl> <chr>    <lgl>    
#> 1     2 Skotland NA       
#> 2     4 Cairo    NA       
```
