# Check Recode List

Checks whether a data frame called 'recodeList' has columns 'oldValues',
containing the values to be recoded, and 'newValues', containing the new
values which recode the old ones.

## Usage

``` r
checkRecodeList(recodeList)
```

## Arguments

- recodeList:

  A recode list: a data frame with the columns "oldValues" and
  "newValues".

## Examples

``` r
recodeList <- data.frame(oldValues = c("Berlin", "Kairo", "Englant", "Schottland"),
                         newValues = c("Deutschland", "Ägypten", "England", "Schottland"))
checkRecodeList(recodeList)
```
