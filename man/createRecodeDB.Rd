% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createRecodeDB.R
\name{createRecodeDB}
\alias{createRecodeDB}
\title{Create Recode Data Base}
\usage{
createRecodeDB(
  recodeListList,
  directory,
  DBname,
  fileType = "csv2",
  overwrite = FALSE
)
}
\arguments{
\item{recodeListList}{A named list of \code{recodeLists}.}

\item{directory}{Path of the directory where the data base will be saved.}

\item{DBname}{Name of the database (will be used as filename).}

\item{fileType}{\code{csv2} (default), \code{csv}, \code{xlsx}.}

\item{overwrite}{If there already is a database on the specified path, should it be overwritten?}
}
\description{
Create and store a recode data base in an Excel (\code{.csv} or \code{.xlsx}) file.
For each recodeList, a separate \code{.csv} file will be generated.
The \code{.xlsx} file will have sheets corresponding to each recodeList name.
}
\examples{
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
directory <- tempdir()
# create .csv file with comma as decimal point (default)
createRecodeDB(recodeListList = Countries,
               directory = directory,
               DBname = "Countries",
               fileType = "csv2",
               overwrite = TRUE)
# create .csv file
createRecodeDB(recodeListList = Countries,
               directory = directory,
               DBname = "Countries",
               fileType = "csv",
               overwrite = TRUE)
# create .xlsx file
createRecodeDB(recodeListList = Countries,
               directory = directory,
               DBname = "Countries",
               fileType = "xlsx",
               overwrite = TRUE)

}
