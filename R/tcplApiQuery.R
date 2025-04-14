#' tcplApiQuery used to get upstream API data for bioactivity (invitrodb) data
#'
#' @param dsstox_sid String - dsstox substance id of the form DTXSIDXXXXXX
#' @param aenm String - the name of the assay tested. Must match name in data hub exactly
#' @param rs Bool - default TRUE do we want to return only the representative sample.
#' @param url_env String - the environment in which the api should hit.
#'
#' @return json
#' @export
#' @import dplyr
#' @importFrom data.table rbindlist
#' @importFrom httr GET content
#' @importFrom purrr map
#' @importFrom utils URLencode
tcplApiQuery <- function(dsstox_sid, aenm, rs = TRUE, url_env = "https://ccte-api-ccd-dev.epa.gov/") {
  #variable binding
  m4Id <- logcAgg <- respAgg <- assayComponentEndpNm <- hitcallContinuous <- NULL
  mc5ChidRep <- chnm <- casn <- dtxsid <- sampleId <- mc5Coff <- modelType <- NULL
  modl <- mc5Fitc <- flag <- normalizedDataTypCd <- conc <- resp <- NULL

  #URL encode aenm as it may include special characters
  aenm <- URLencode(aenm, reserved = TRUE)

  # boolean signalling if we want to return only the rep sample or all samples
  if (rs) {
    rs_url <- "-with-rs"
  }
  else {
    rs_url <- ""
  }
  #construct url to get bioactivity information
  url <- paste0(url_env, "ccdapp1/bioactivity-data/search/by-dtxsid", rs_url, "?id=", dsstox_sid, "&assay=", aenm)
  #construct url to get chemical info
  url2 <- paste0(url_env,"ccdapp1/search/chemical/equal-with-detail/",dsstox_sid)
  # get bioactivity via api call
  bioactivity <- GET(URLencode(url),repeated = TRUE)
  if(!bioactivity$status_code == 200){
    return(bioactivity$status_code)
  }

  # pluck all bioactivity information from the results
  res <- content(bioactivity) %>% purrr::pluck("_embedded", "Bioactivity")
  # remove unneeded information
  res <- lapply(res, function(x) within(x, rm("_links")))
  # remove gene info as the array causes duplicates when rbindlist is used.
  res <- lapply(res, function(x) within(x, rm("geneInfo")))
  # transform into datatable
  df <- rbindlist(res, fill = TRUE)

  # add chemical details to the dataframe
  chemdetails <- GET(url2)
  if(!chemdetails$status_code == 200){
    return(chemdetails$status_code)
  }
  chemdetails <- content(chemdetails)


  # map casnumber
  df$casn <- purrr::map(chemdetails, "casrn")[[1]]
  # map chemical name
  df$chnm <- purrr::map(chemdetails, "preferredName")[[1]]


  # select all information that will be the same no matter what modeltype
  translated <- df %>% select(
    m4id = m4Id,
    conc = logcAgg,
    resp = respAgg,
    aenm = assayComponentEndpNm,
    hitc = hitcallContinuous,
    chid_rep = mc5ChidRep,
    chnm,
    casn,
    dsstox_id = dtxsid,
    spid = sampleId,
    coff = mc5Coff,
    model_type = modelType,
    modl = modl,
    fitc = mc5Fitc,
    #modl_acc = mc5ModlAcc, # this might need to be moved into its own function specific to loec
    flag,
    normalized_data_type = normalizedDataTypCd,
    contains("Param")
  )


  # create numeric list of concs and responses instead of character string
  translated <- translated %>%
    rowwise() %>%
    mutate(
      conc = list(as.numeric(unlist(stringr::str_split(conc, "\\|\\|")))),
      resp = list(as.numeric(unlist(stringr::str_split(resp, "\\|\\|"))))
    ) %>%
    ungroup()

  # for now we limit it to 1 response.  Once we build functionality for multiple plots we can do more.
  #test_case <- translated %>% filter(chid_rep == 1)
  # test_case
  translated
}





#' API query to load summary level bioactivity (invitrodb) data
#'
#' @param dsstox_sid String dsstox substance id
#' @param url_env String url environment where the upstream api should be queried
#'
#' @return m5_res a dataframe with all of the summary level bioactivity data
#' @export
#' @import dplyr
#' @importFrom data.table rbindlist
#' @importFrom httr GET content
#' @importFrom purrr pluck
#' @importFrom utils URLencode
tcplSummaryLoad <- function(dsstox_sid,url_env = "https://ccte-api-ccd-dev.epa.gov/"){
  #variable binding
  hitc <- aeid <- endpointName <- NULL

  #construct url to get bioactivity information
  url <- paste0(url_env, "ccdapp2/assay-list-details/search/by-dtxsid?id=", dsstox_sid,"&projection=assay-detail-short")
  #construct url to get cytotox info
  url2 <- paste0(url_env,"ccdapp1/bioactivity-cytotox/search/by-dtxsid?id=",dsstox_sid)
  # get bioactivity via api call
  bioactivity <- httr::GET(url)
  if(!bioactivity$status_code == 200){
    return(bioactivity$status_code)
  }

  # pluck all bioactivity information from the results
  res <- httr::content(bioactivity) %>% purrr::pluck("_embedded", "assayListDetails")
  # remove unneeded information
  res <- lapply(res, function(x) within(x, rm("_links")))
  # transform into datatable
  m5 <- data.table::rbindlist(res, fill = TRUE)

  cytotox_url <- httr::GET(url2)
  if(!cytotox_url$status_code == 200){
    return(cytotox_url$status_code)
  }
  # pluck all bioactivity information from the results
  cytotox <- httr::content(cytotox_url) %>%
    purrr::pluck("_embedded", "Bioactivity") %>% # get Bioactivity (cytotox) data from results
    lapply(function(x) within(x, rm("_links"))) %>% # remove links information
    data.table::rbindlist(fill = TRUE) %>% # transform into datatable
    unique()

  assay_endpoint_dat <- tcplPlot::full_assay_list

  #join saved endpoint information
  m5_aeid <- m5 %>% filter(hitc==1) %>% left_join(assay_endpoint_dat, by = c("endpointName" = "assayComponentEndpointName"))

  #if there is missing assay info we need to retrieve
  if((m5_aeid %>% filter(hitc==1,is.na(aeid)) %>% nrow())>0){
    #httr does not allow body in get so must do each individually
    body <- m5_aeid %>% filter(hitc==1,is.na(aeid)) %>% pull(endpointName) %>% unique()
    url3 <- paste0(url_env,"ccdapp2/assay-endpoint-detail/search/by-endpointname?name=",body)

    #add missing assays to the assay_endpoint_dat
    assay_details <- lapply(url3,function(x) content(httr::GET(URLencode(x))))
    assay_details_flat <- assay_details %>% lapply(function(x) purrr::pluck(x,"annotation"))
    assay_details_flat <- unlist(assay_details_flat, recursive = FALSE) %>% data.table::rbindlist()
    assay_endpoint_dat <- rbind(assay_endpoint_dat,assay_details_flat)

    #rejoin m5 with additional assay information
    m5_aeid <- m5 %>% filter(hitc==1) %>% left_join(assay_endpoint_dat, by = c("endpointName" = "assayComponentEndpointName"))
  }

  m5_res <- m5_aeid %>% cbind(cytotox)
  m5_res$scaledTop <- as.numeric(m5_res$scaledTop)
  m5_res$geneSymbol <- sapply(m5_res$geneSymbol,function(x) paste0(x,collapse = ","))
  m5_res

}

#' tcplAssayAnnotationsLoad used to load assay annotations from CTX API
#'
#' @param api_key String - the api key for the CTX API
#' @param url_env String - the environment in which the api should hit.
#'
#' @return assays a data frame containing all assays and valid annotations for matrix
#' @export
#' @import dplyr
#' @importFrom tidyr unnest_wider
#' @importFrom ctxR get_all_assays
tcplAssayAnnotationsLoad <- function(api_key, url_env = "https://api-ccte.epa.gov/bioactivity") {
  # load API data
  assays <- ctxR::get_all_assays(API_key = api_key, Server = url_env) |>
    tidyr::unnest_wider(c("gene", "assayList", "citations"), names_sep = "_")
  colnames(assays)[colnames(assays) == "citations_citation"] <- "citation"
  assays <- assays |>
    select(-internalReady, -exportReady,-gene_geneId, -gene_geneName,
           -gene_description, -gene_geneSymbol, -gene_organismId,
           -gene_trackStatus, -gene_uniprotAccessionNumber,
           -contains("citations_")) |>
    mutate(across(everything(), as.character))

  # remove single quotes from attributes
  assays$keyPositiveControl<-gsub("'","",assays$keyPositiveControl)
  assays$keyAssayReagent<-gsub("'","",assays$keyAssayReagent)

  assays
}

#' API call to GET HTTr Summary Data
#'
#' @param dsstox_sid string dsstox substance id
#' @param url_env string url of environment that the data should be pulled from
#'
#' @return res_dt a datatable with all of the httr summary data collapsed
#' @export
#' @import dplyr
#' @importFrom data.table rbindlist
#' @importFrom httr GET content
#' @importFrom purrr pluck
tcplSummaryHTTrLoad <- function(dsstox_sid,url_env = "https://ccte-api-ccd-dev.epa.gov/"){

  #construct url to get summary httr info
  url <- paste0(url_env,"ccdapp2/httr/search/by-dtxsid?id=",dsstox_sid)
  # get bioactivity via api call
  bioactivity <- httr::GET(url)
  if(!bioactivity$status_code == 200){
    return(bioactivity$status_code)
  }

  # pluck all bioactivity information from the results
  res <- httr::content(bioactivity) %>% purrr::pluck("_embedded", "Httr")
  # remove unneeded information
  res <- lapply(res, function(x) within(x, rm("_links")))
  # transform into datatable
  res_dt <- data.table::rbindlist(res, fill = TRUE)


  #construct url to get cytotox info
  url2 <- paste0(url_env,"ccdapp1/bioactivity-cytotox/search/by-dtxsid?id=",dsstox_sid)
  cytotox_url <- httr::GET(url2)
  if(!cytotox_url$status_code == 200){
    return(cytotox_url$status_code)
  }
  # pluck all bioactivity information from the results
  cytotox <- httr::content(cytotox_url) %>%
    purrr::pluck("_embedded", "Bioactivity") %>% # get Bioactivity (cytotox) data from results
    lapply(function(x) within(x, rm("_links"))) %>% # remove links information
    data.table::rbindlist(fill = TRUE) %>% # transform into datatable
    unique()

  if(nrow(cytotox)>0){
    res_dt <- res_dt %>% left_join(cytotox, by = "dtxsid")
  }

  return(res_dt)
}


#' API call to GET HTPP Summary Data
#'
#' @param dsstox_sid dsstox substance id
#' @param url_env url environment that the api should call
#'
#' @return res_dt a datatable with all of the httr summary data collapsed
#' @export
#' @import dplyr
#' @importFrom data.table rbindlist
#' @importFrom httr GET content
#' @importFrom purrr pluck
tcplSummaryHTPPLoad <- function(dsstox_sid,url_env = "https://ccte-api-ccd-dev.epa.gov/"){

  #construct url to get summary httr info
  url <- paste0(url_env,"ccdapp2/htpp/search/by-dtxsid?id=",dsstox_sid)
  # get bioactivity via api call
  bioactivity <- httr::GET(url)
  if(!bioactivity$status_code == 200){
    return(bioactivity$status_code)
  }

  # pluck all bioactivity information from the results
  res <- httr::content(bioactivity) %>% purrr::pluck("_embedded", "Htpp")
  # remove unneeded information
  res <- lapply(res, function(x) within(x, rm("_links")))
  # transform into datatable
  res_dt <- data.table::rbindlist(res, fill = TRUE)


  #construct url to get cytotox info
  url2 <- paste0(url_env,"ccdapp1/bioactivity-cytotox/search/by-dtxsid?id=",dsstox_sid)
  cytotox_url <- httr::GET(url2)
  if(!cytotox_url$status_code == 200){
    return(cytotox_url$status_code)
  }
  # pluck all bioactivity information from the results
  cytotox <- httr::content(cytotox_url) %>%
    purrr::pluck("_embedded", "Bioactivity") %>% # get Bioactivity (cytotox) data from results
    lapply(function(x) within(x, rm("_links"))) %>% # remove links information
    data.table::rbindlist(fill = TRUE) %>% # transform into datatable
    unique()

  res_dt <- res_dt %>% left_join(cytotox, by = "dtxsid")

  return(res_dt)
}


#' API call to GET HTTr Conc/Resp Data
#'
#' @param cellType String the celltype
#' @param signature String signature for the associated data
#' @param sampleId String sample id otherwise known as spid
#' @param dataset String required dataset indicator for httr
#' @param url_env String url where the api should be called
#'
#' @return res_dt a datatable with all of the httr conc/resp data collapsed
#' @export
#' @import dplyr
#' @importFrom data.table rbindlist
#' @importFrom httr GET content
#' @importFrom purrr pluck
#' @importFrom utils URLencode
tcplHTTrConcResp <- function(cellType,signature,sampleId,dataset,url_env = "https://ccte-api-ccd-dev.epa.gov/"){

  #construct url to get summary httr info
  url <- paste0(url_env,"ccdapp2/httr-conc-response?cellType=",cellType,
                "&signature=",signature,
                "&sampleId=",sampleId,
                "&dataset=",dataset)
  # get bioactivity via api call
  bioactivity <- httr::GET(URLencode(url))
  if(!bioactivity$status_code == 200){
    return(bioactivity$status_code)
  }

  # pluck all bioactivity information from the results
  res <- httr::content(bioactivity) #%>% purrr::pluck(1)

  # transform into datatable
  res_dt <- data.table::rbindlist(res, fill = TRUE)

  dsstox_sid <- unique(res_dt$dtxsid)
  #construct url to get cytotox info
  url2 <- paste0(url_env,"ccdapp2/chemical-detail/search/by-dsstoxsid?id=",dsstox_sid)
  chem_detail <- httr::GET(url2)
  if(!chem_detail$status_code == 200){
    return(chem_detail$status_code)
  }
  # pluck casrn information from the results
  casrn <- httr::content(chem_detail) %>%
    purrr::pluck("casrn")

  # pluck preferred name information from the results
  chnm <- httr::content(chem_detail) %>%
    purrr::pluck("preferredName")

  res_dt$chnm <- chnm
  res_dt$casrn <- casrn

  return(dplyr::as_tibble(res_dt))
}


#' API call to GET HTPP Conc/Resp Data
#'
#' @param sampleId String Sample ID
#' @param categoryName String associated category name for htpp data
#' @param url_env String URL base where the api should call the data from
#'
#' @return res_dt a datatable with all of the httr conc/resp data collapsed
#' @export
#' @import dplyr
#' @importFrom data.table rbindlist
#' @importFrom httr GET content
#' @importFrom purrr pluck
#' @importFrom utils URLencode
tcplHTPPConcResp <- function(sampleId,categoryName,url_env = "https://ccte-api-ccd-dev.epa.gov/"){

  #construct url to get summary httr info
  url <- paste0(url_env,"ccdapp2/htpp-conc-response?sampleId=",sampleId,
                "&categoryName=",categoryName)
  # get bioactivity via api call
  bioactivity <- httr::GET(URLencode(url))
  if(!bioactivity$status_code == 200){
    return(bioactivity$status_code)
  }

  # pluck all bioactivity information from the results
  res <- httr::content(bioactivity) #%>% purrr::pluck(1)

  # transform into datatable
  res_dt <- data.table::rbindlist(res, fill = TRUE)

  dsstox_sid <- unique(res_dt$dtxsid)
  #construct url to get cytotox info
  url2 <- paste0(url_env,"ccdapp2/chemical-detail/search/by-dsstoxsid?id=",dsstox_sid)
  chem_detail <- httr::GET(url2)
  if(!chem_detail$status_code == 200){
    return(chem_detail$status_code)
  }
  # pluck casrn information from the results
  casrn <- httr::content(chem_detail) %>%
    purrr::pluck("casrn")

  # pluck preferred name information from the results
  chnm <- httr::content(chem_detail) %>%
    purrr::pluck("preferredName")

  res_dt$chnm <- chnm
  res_dt$casrn <- casrn

  return(dplyr::as_tibble(res_dt))
}



#' API query to load summary level bioactivity (invitrodb) data
#'
#' @param AEID String assay endpoint ID
#' @param api_key String - the api key for the CTX API
#' @param url_env String url environment where the upstream api should be queried
#'
#' @return m5_res a dataframe with all of the summary level bioactivity data
#' @export
#' @import dplyr
#' @importFrom ctxR get_bioactivity_details get_annotation_by_aeid
tcplSummaryLoadAEID <- function(AEID,api_key = NULL, url_env = "https://api-ccte.epa.gov/bioactivity"){
  #load data by AEID
  aeid_data <- get_bioactivity_details(AEID = AEID, API_key = api_key, Server = url_env)
  assays <- get_annotation_by_aeid(AEID = AEID,API_key = api_key, Server = url_env)

  aeid_data <- aeid_data |> dplyr::left_join(assays, by = dplyr::join_by(aeid))
  aeid_data

}
