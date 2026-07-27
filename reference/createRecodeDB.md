# Create Recode Data Base

Create and store a recode data base in an Excel (`.csv` or `.xlsx`)
file. For each recodeList, a separate `.csv` file will be generated. The
`.xlsx` file will have sheets corresponding to each recodeList name.

## Usage

``` r
createRecodeDB(
  recodeListList,
  directory,
  DBname,
  fileType = "csv2",
  overwrite = FALSE
)
```

## Arguments

- recodeListList:

  A named list of `recodeLists`.

- directory:

  Path of the directory where the data base will be saved.

- DBname:

  Name of the database (will be used as filename).

- fileType:

  `csv2` (default), `csv`, `xlsx`.

- overwrite:

  If there already is a database on the specified path, should it be
  overwritten?

## Examples

``` r
# create a named list of data frames
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
# create .csv file with comma as decimal point (default)
createRecodeDB(recodeListList = Countries,
               directory = directory,
               DBname = "Countries",
               fileType = "csv2",
               overwrite = TRUE)
#> [1] "Successfully created Countries.csv"
# create .csv file
createRecodeDB(recodeListList = Countries,
               directory = directory,
               DBname = "Countries",
               fileType = "csv",
               overwrite = TRUE)
#> [1] "Successfully created Countries.csv"
# create .xlsx file
createRecodeDB(recodeListList = Countries,
               directory = directory,
               DBname = "Countries",
               fileType = "xlsx",
               overwrite = TRUE)
#> [1] "Successfully created Countries.xlsx"
```
