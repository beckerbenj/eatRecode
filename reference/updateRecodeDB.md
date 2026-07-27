# Update Recode Data Base

Update a recode data base stored in an Excel file. Takes new recode
information, compares it to the existing recode database, and updates
the database accordingly.

## Usage

``` r
updateRecodeDB(
  newRecodes,
  oldValues = "oldValues",
  newValues = "newValues",
  directory,
  newDirectory = directory,
  DBname,
  newDBname = DBname,
  ListName,
  fileType = "csv2",
  replace = FALSE
)
```

## Arguments

- newRecodes:

  A `data.frame` containing new recode information.

- oldValues:

  A character string of the column name containing the old values in the
  `newRecodes` data.frame.

- newValues:

  A character string of the column name containing the newly recoded
  values in the `newRecodes data.frame`.

- directory:

  Path to the directory where the data base is stored.

- newDirectory:

  If the updated data base should be stored in a different directory,
  specify its path here.

- DBname:

  Name of the database.

- newDBname:

  If the updated data base should be stored under a different name,
  specify it here. are present in the `newRecodes`?

- ListName:

  Name of the specific recode list to be imported.

- fileType:

  `csv2` (default), `csv`, `xlsx`

- replace:

  Logical of length 1. Should existing recode pairs be overwritten when
  conflicting newer recode pairs.

## Examples

``` r
# example data base
oldDatabase <- list( Europe = data.frame(oldValues = c("Berlin", "Copenhagen", "Rome", "Madrid"),
                                         newValues = c("France", "Denmark", "Italy" , "Spain")),
                     Asia = data.frame(oldValues = c("Baku", "Tokyo", "Kathmandu", "Singapore"),
                                       newValues = c("Azerbaijan", "Japan", "Nepal" , "Singapore")))
oldDatabase
#> $Europe
#>    oldValues newValues
#> 1     Berlin    France
#> 2 Copenhagen   Denmark
#> 3       Rome     Italy
#> 4     Madrid     Spain
#> 
#> $Asia
#>   oldValues  newValues
#> 1      Baku Azerbaijan
#> 2     Tokyo      Japan
#> 3 Kathmandu      Nepal
#> 4 Singapore  Singapore
#> 
directory <- tempdir()
createRecodeDB(recodeListList = oldDatabase,
               directory = directory,
               DBname = "countries",
               overwrite = TRUE)
#> [1] "Successfully created countries.csv"
newRecodes <- data.frame( city = c("Berlin", "Paris", "Athens"),
                          country = c("Germany", "France", "Greece"))
# update the data base without overwriting old information
# (the row containing "Berlin - France" keeps it's old value)
updateRecodeDB(newRecodes = newRecodes,
               oldValues = "city",
               newValues = "country",
               directory = directory,
               DBname = "countries",
               ListName = "Europe",
               replace = FALSE)
#> [1] "Successfully updated countries.csv"
getRecodeDB(directory, "countries")
#> $Asia
#>   oldValues  newValues
#> 1      Baku Azerbaijan
#> 2     Tokyo      Japan
#> 3 Kathmandu      Nepal
#> 4 Singapore  Singapore
#> 
#> $Europe
#>    oldValues newValues
#> 1     Athens    Greece
#> 2     Berlin    France
#> 3 Copenhagen   Denmark
#> 4     Madrid     Spain
#> 5      Paris    France
#> 6       Rome     Italy
#> 
# update the data base, overwriting old information
# (the row containing "Berlin - France" get's updated)
updateRecodeDB(newRecodes = newRecodes,
               oldValues = "city",
               newValues = "country",
               directory = directory,
               DBname = "countries",
               ListName = "Europe",
               replace = TRUE)
#> The following recode pairs in the existing data base in sheet 'Europe' will be overwritten:
#> Berlin -> France; now: Germany
#> [1] "Successfully updated countries.csv"
getRecodeDB(directory, "countries")
#> $Asia
#>   oldValues  newValues
#> 1      Baku Azerbaijan
#> 2     Tokyo      Japan
#> 3 Kathmandu      Nepal
#> 4 Singapore  Singapore
#> 
#> $Europe
#>    oldValues newValues
#> 1     Athens    Greece
#> 2     Berlin   Germany
#> 3 Copenhagen   Denmark
#> 4     Madrid     Spain
#> 5      Paris    France
#> 6       Rome     Italy
#> 
```
