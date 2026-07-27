# Use Recode List

Add a new column to a data frame containing recoded values from a chosen
column in that data frame.

## Usage

``` r
useRecodeList(df, varName, new_varName, recodeList)
```

## Arguments

- df:

  A data.frame.

- varName:

  Column in `df` which should be recoded.

- new_varName:

  Column in which the recoded values will be stored.

- recodeList:

  A recode list: a data frame with the columns "oldValues" and
  "newValues".

## Value

The `df` with a recoded `newValues`.

## Examples

``` r
# example data frame
df <- data.frame(id = 1:4, country = c("Berlin", "Kairo", "Englant", "Schottland"))
# example recode list
recodeList <- data.frame(oldValues = c("Berlin", "Kairo", "Englant"),
                         newValues = c("Deutschland", "Ägypten", "England"))

useRecodeList(df, varName = "country", new_varName = "r_country", recodeList)
#>   id    country   r_country
#> 1  1     Berlin Deutschland
#> 2  2      Kairo     Ägypten
#> 3  3    Englant     England
#> 4  4 Schottland        <NA>
```
