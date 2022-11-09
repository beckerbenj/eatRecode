
## helper files for tests

rl <- data.frame(oldValues = c("Bavaria", "Berlin", "England", "Wales"),
                 newValues = c("Germany", "Germany", "UK", "UK"),
                 stringsAsFactors = FALSE)

dat1 <- data.frame(var1 = c("Germany", "UK", "Bavaria", "Wales", "Italy", "Scotland"),
                   var2 = NA,
                   stringsAsFactors = FALSE)

rl2 <- data.frame(oldValues = c("Garman", "Germam", "Bavarian"),
                 newValues = c("German", "German", "German"),
                 stringsAsFactors = FALSE)




saveRDS(rl, "tests/testthat/helper_recodeList.RDS")
saveRDS(rl2, "tests/testthat/helper_recodeList2.RDS")
saveRDS(dat1, "tests/testthat/helper_dat.RDS")

input <- list(country = rl, language = rl2)
createRecodeDB(input, filePath = "tests/testthat/helper_recodeDB.xlsx")


#usethis::use_data(helper_files, overwrite = TRUE)
