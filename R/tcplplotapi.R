#* @apiTitle Visualization tool for Bioactivity Data
#* @apiDescription The goal of this API is to productionize the plotting of scientific endpoints.
#*  This should allow development to rapidly adapt to changes in the science
#*  without the need to replicate processes in the UI.
#*


#' Plot out data from httr/htpp
#' @name plotfit2
#' @param cellType String of the cell type for the chemical
#' @param signature String of the signature name
#' @param sampleId String of the sample Id that should be plotted
#' @param dataset String of the dataset that the sample is included in
#' @param url_env String of environment that api should be called in
#* @get /plotfit2
#* @serializer htmlwidget
function(cellType,signature,sampleId,dataset,url_env = "https://ccte-api-ccd-dev.epa.gov/", res) {

  url_env <- Sys.getenv("URL_ENV", url_env)
  dat <- tcplPlot::tcplHTTrConcResp(cellType = cellType,
                                    signature = signature,
                                    sampleId = sampleId,
                                    dataset = dataset,
                                    url_env = url_env)
  if (is.integer(dat)) {
    res$status <- dat
    res$body <- jsonlite::toJSON(auto_unbox = TRUE, list(
      status = dat,
      message = paste0("Upstream services returning status code: ",dat)
    ))
    return(res)
  } else {
  tcplPlot::tcplfit2Plot(dat)
  }
}



#' Plot Summary data for dtxsid
#' @name plotSummary
#' @param dsstox_id String - the dsstox substance id
#' @param url_env String of environment that api should be called in
#* @get /plotSummary
#* @serializer htmlwidget
function(dsstox_id,url_env = "https://ccte-api-ccd-dev.epa.gov/",res) {
  url_env <- Sys.getenv("URL_ENV", url_env)
  dat <- tcplPlot::tcplSummaryLoad(dsstox_sid = dsstox_id, url_env = url_env)
  if (is.integer(dat)) {
    res$status <- dat
    res$body <- jsonlite::toJSON(auto_unbox = TRUE, list(
      status = dat,
      message = "Upstream services returning error."
    ))
    return(res)
  } else {
  tcplPlot::tcplPlotSummary(dat)
  }
}

#' Plot Summary for HTTr Summary data
#' @name plotSummaryHTTr
#' @param dsstox_id String - the dsstox substance id
#' @param url_env String of environment that api should be called in
#* @get /plotSummaryHTTr
#* @serializer htmlwidget
function(dsstox_id,url_env = "https://ccte-api-ccd-dev.epa.gov/",res) {
  url_env <- Sys.getenv("URL_ENV", url_env)
  dat <- tcplPlot::tcplSummaryHTTrLoad(dsstox_sid = dsstox_id, url_env = url_env)
  if (is.integer(dat)) {
    res$status <- dat
    res$body <- jsonlite::toJSON(auto_unbox = TRUE, list(
      status = dat,
      message = "Upstream services returning error."
    ))
    return(res)
  } else {
    tcplPlot::tcplPlotSummaryHTTr(dat)
  }
}

#' Plot Summary for HTPP Summary data
#' @name plotSummaryHTPP
#' @param dsstox_id String - the dsstox substance id
#' @param approach String - either "category" or "feature" determines which subset to plot
#' @param url_env String - environment that api should be called in
#* @get /plotSummaryHTPP
#* @serializer htmlwidget
function(dsstox_id,approach = "category",url_env = "https://ccte-api-ccd-dev.epa.gov/",res) {
  url_env <- Sys.getenv("URL_ENV", url_env)
  dat <- tcplPlot::tcplSummaryHTPPLoad(dsstox_sid = dsstox_id, url_env = url_env)
  if (is.integer(dat)) {
    res$status <- dat
    res$body <- jsonlite::toJSON(auto_unbox = TRUE, list(
      status = dat,
      message = "Upstream services returning error."
    ))
    return(res)
  } else {
    tcplPlot::tcplPlotSummaryHTPP(dat,approach = approach)
  }
}


#' Plot out data from invitrodb
#' @name plot
#' @param dsstox_id String of the dsstox substance id
#' @param assay_endpoint_nm String of the assay endpoint name
#' @param representative_sample TRUE/FALSE should only the representative sample be displayed
#' @param url_env String of environment that api should be called in
#* @get /plot
#* @serializer htmlwidget
function(dsstox_id, assay_endpoint_nm, representative_sample = TRUE,url_env = "https://ccte-api-ccd-dev.epa.gov/", res) {
  # tcplPlot::use_local_proxy()
  # httr::set_config(httr::use_proxy("socks5://localhost:1030"))
  url_env <- Sys.getenv("URL_ENV", url_env)
  dat <- tcplPlot::tcplApiQuery(dsstox_sid = dsstox_id, aenm = assay_endpoint_nm, rs = representative_sample, url_env = url_env)
  if (is.integer(dat)) {
    res$status <- dat
    res$body <- jsonlite::toJSON(auto_unbox = TRUE, list(
      status = dat,
      message = "Upstream services returning error."
    ))
    return(res)
  } else {
    tcplPlot::tcplPlotlyMultiPlot(dat)
  }
}


#' Plot out data from htpp conc resp
#' @name plotfit2_HTPP
#' @param sampleId String of the sample Id that should be plotted
#' @param categoryName String of the categoryName that the sample is included in
#' @param url_env String of environment that api should be called in
#* @get /plotfit2_HTPP
#* @serializer htmlwidget
function(sampleId,categoryName,url_env = "https://ccte-api-ccd-dev.epa.gov/", res) {
  url_env <- Sys.getenv("URL_ENV", url_env)
  dat <- tcplPlot::tcplHTPPConcResp(sampleId = sampleId,
                                    categoryName = categoryName,
                                    url_env = url_env)
  if (is.integer(dat)) {
    res$status <- dat
    res$body <- jsonlite::toJSON(auto_unbox = TRUE, list(
      status = dat,
      message = paste0("Upstream services returning status code: ",dat)
    ))
    return(res)
  } else {
    tcplPlot::tcplfit2PlotHTPP(dat)
  }
}


#' Plot out assay annotations in matrix format
#' @name plotAssays
#' @param xaxis Character, the x-axis field name from dat, default "intendedTargetFamily"
#' @param yaxis Character, the y-axis field name from dat, default "organism"
#' @param api_key String - the api key for the CTX API
#' @param url_env String of environment that api should be called in
#* @get /plotAssays
#* @serializer htmlwidget
function(xaxis = "intendedTargetFamily", yaxis = "organism", api_key = "api_key_goes_here", url_env = "https://api-ccte.epa.gov/bioactivity", res) {
  url_env <- Sys.getenv("CTX_URL_ENV", url_env)
  api_key <- Sys.getenv("CTX_API_KEY", api_key)
  assays <- tcplPlot::tcplAssayAnnotationsLoad(api_key = api_key, url_env = url_env)
  if (is.integer(assays)) {
    res$status <- assays
    res$body <- jsonlite::toJSON(auto_unbox = TRUE, list(
      status = assays,
      message = "Upstream services returning error."
    ))
    return(res)
  } else {
    invalid_annotations <- "aeid|acid|aid|asid|assayName|assayComponentName|assayComponentEndpointName|Desc|description|dataUsability|organismId|entrez|FullName|citation"
    valid_cols <- colnames(assays)[!grepl(invalid_annotations, colnames(assays))]
    if (!all(c(xaxis, yaxis) %in% valid_cols)) {
      res$status <- 400
      res$body <- jsonlite::toJSON(auto_unbox = TRUE, list(
        status = 400,
        message = "x- or y-axis are not valid."
      ))
      return(res)
    }
    tcplPlot::tcplPlotAssays(assays, xaxis, yaxis)
  }
}


#' Get a list of fields available for x- and y-axis of assay annotation plot
#' @name assayAnnotations
#' @param api_key String - the api key for the CTX API
#' @param url_env String of environment that api should be called in
#* @get /assayAnnotations
#* @serializer json
function(api_key = "api_key_goes_here", url_env = "https://api-ccte.epa.gov/bioactivity", res) {
  url_env <- Sys.getenv("CTX_URL_ENV", url_env)
  api_key <- Sys.getenv("CTX_API_KEY", api_key)
  assays <- tcplPlot::tcplAssayAnnotationsLoad(api_key = api_key, url_env = url_env)
  if (is.integer(assays)) {
    res$status <- assays
    res$body <- jsonlite::toJSON(auto_unbox = TRUE, list(
      status = assays,
      message = "Upstream services returning error."
    ))
    return(res)
  } else {
    invalid_annotations <- "aeid|acid|aid|asid|assayName|assayComponentName|assayComponentEndpointName|Desc|description|dataUsability|organismId|entrez|FullName|citation"
    colnames(assays)[!grepl(invalid_annotations, colnames(assays))]
  }
}


#' Plot Summary data for AEID
#' @name plotSummaryAEID
#' @param AEID String - the assay endpoint ID
#* @get /plotSummaryAEID
#* @serializer htmlwidget
function(AEID,api_key = "api_key_goes_here",res) {
  api_key <- Sys.getenv("CTX_API_KEY", api_key)
  dat <- tcplPlot::tcplSummaryLoadAEID(AEID = AEID,api_key = api_key)
  if (is.integer(dat)) {
    res$status <- dat
    res$body <- jsonlite::toJSON(auto_unbox = TRUE, list(
      status = dat,
      message = "Upstream services returning error."
    ))
    return(res)
  } else {
    tcplPlot::tcplPlotSummaryAEID(dat)
  }
}





# http://localhost:8000/plot?m4id=30058318

# docker commands:
# docker build --tag plotapi:1.0.5 .
# docker run --rm -p 8000:8000 plotapi:1.0.5 tcplplotapi.R

#
# docker pull brownjason/tcplapi
# docker run --rm -p 8000:8000 brownjason/tcplapi tcplplotapi.R

# to kill running docker
# docker container ls
# docker container stop containerid
