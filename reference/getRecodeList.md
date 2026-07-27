# Get Recode List

Import a single recode list from an Excel file containing a data base of
different `recodeLists`.

## Usage

``` r
getRecodeList(directory, DBname, ListName, fileType = "csv2")
```

## Arguments

- directory:

  Path to the directory where the data base is stored.

- DBname:

  Name of the database.

- ListName:

  Name of the specific recode list to be imported.

- fileType:

  `csv2` (default), `csv`, `xlsx`

## Value

A recode list.

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
# Import lists from csv data base
getRecodeList(directory = directory, DBname = "Countries", ListName = "Europe", fileType = "csv2")
#>   id  oldValues newValues
#> 1  1     Berlin   Germany
#> 2  2 Copenhagen   Denmark
#> 3  3       Rome     Italy
#> 4  4     Madrid     Spain
getRecodeList(directory = directory, DBname = "Countries", ListName = "Asia", fileType = "csv2")
#>   id oldValues  newValues
#> 1  1      Baku Azerbaijan
#> 2  2     Tokyo      Japan
#> 3  3 Kathmandu      Nepal
#> 4  4 Singapore  Singapore
# Import lists from xlsx data base
getRecodeList(directory = directory, DBname = "Countries", ListName = "Europe", fileType = "xlsx")
#>   id  oldValues newValues
#> 1  1     Berlin   Germany
#> 2  2 Copenhagen   Denmark
#> 3  3       Rome     Italy
#> 4  4     Madrid     Spain
getRecodeList(directory = directory, DBname = "Countries", ListName = "Asia", fileType = "xlsx")
#>   id oldValues  newValues
#> 1  1      Baku Azerbaijan
#> 2  2     Tokyo      Japan
#> 3  3 Kathmandu      Nepal
#> 4  4 Singapore  Singapore
```
