#' A list of all available assay endpoints saved as an rda file
#'
#' This dataset is used so that we don't have to call a
#' large api endpoint multiple times everytime someone loads a plot
#'
#' @source https://ccte-api-ccd.epa.gov/ccdapp2/assay-endpoint-detail/
"full_assay_list"

#' A list of all available assays that have an associated pdf description
#'
#' This is a list of aeids that have an associated descriptive pdf stored in clowder
#'
#' @format full_assay_list
#' vector of integer values of aeids that have associated data
#'
#' @source invitrodb
"available_assay_descriptions"

#' Example data that can be used in place of an api call
#'
#' This is a dataframe of summary level data that can be used in place
#' of an upstream api call
#'
#' @format tcplSummaryPlotExampleData
#' dataframe 223 obs. of 23 variables
#'
#' @source upstream api call
"tcplSummaryPlotExampleData"
