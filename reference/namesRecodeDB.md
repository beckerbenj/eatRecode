# Get Names of Recode Data Base

Get the names of the individual recode lists within a recode data base.

## Usage

``` r
namesRecodeDB(directory, DBname, fileType = "csv2")
```

## Arguments

- directory:

  Path to the directory where the data base is stored.

- DBname:

  Name of the database.

- fileType:

  `csv2` (default), `csv`, `xlsx`

## Examples

``` r
# Create recode list data base using `createRecodeDB`
Countries <- list( Europe = data.frame(
                                  id = 1:4,
                                  oldValues = c("Berlin", "Copenhagen", "Rome", "Madrid"),
                                  newValues = c("Germany", "Denmark", "Italy" , "Spain")),
                   Asia = data.frame(
                                  id = 1:4,
                                  oldValues = c("Baku", "Tokyo", "Kathmandu", "Singapore"),
                                  newValues = c("Azerbaijan", "Japan", "Nepal" , "Singapore")))
Countries
#> $Europe
#>   id  oldValues newValues
#> 1  1     Berlin   Germany
#> 2  2 Copenhagen   Denmark
#> 3  3       Rome     Italy
#> 4  4     Madrid     Spain
#> 
#> $Asia
#>   id oldValues  newValues
#> 1  1      Baku Azerbaijan
#> 2  2     Tokyo      Japan
#> 3  3 Kathmandu      Nepal
#> 4  4 Singapore  Singapore
#> 
directory <- tempdir()
createRecodeDB(recodeListList = Countries,
               directory = directory,
               DBname = "Countries",
               fileType = "csv2",
               overwrite = TRUE)
#> [1] "Successfully created Countries.csv"
createRecodeDB(recodeListList = Countries,
               directory = directory,
               DBname = "Countries",
               fileType = "xlsx",
               overwrite = TRUE)
#> [1] "Successfully created Countries.xlsx"
# Get names of the recode lists in the data base
namesRecodeDB(directory = directory, DBname = "Countries", fileType = "csv2")
#> [1] "Asia"   "Europe"
namesRecodeDB(directory = directory, DBname = "Countries", fileType = "xlsx")
#> [1] "Europe" "Asia"  
```
