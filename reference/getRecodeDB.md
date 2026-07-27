# Get Recode Data Base

Import a complete recode data base containing multiple `recodeLists`.

## Usage

``` r
getRecodeDB(directory, DBname, fileType = "csv2")
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
# Import csv data base
getRecodeDB(directory = directory, DBname = "Countries", fileType = "csv2")
#> $Asia
#>   id oldValues  newValues
#> 1  1      Baku Azerbaijan
#> 2  2     Tokyo      Japan
#> 3  3 Kathmandu      Nepal
#> 4  4 Singapore  Singapore
#> 
#> $Europe
#>   id  oldValues newValues
#> 1  1     Berlin   Germany
#> 2  2 Copenhagen   Denmark
#> 3  3       Rome     Italy
#> 4  4     Madrid     Spain
#> 
# Import xlsx data base
getRecodeDB(directory = directory, DBname = "Countries", fileType = "xlsx")
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
```
