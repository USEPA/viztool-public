## code to prepare `DATASET` dataset goes here
available_assay_descriptions <- c(115L, 711L, 726L, 739L, 740L, 741L, 761L, 762L, 764L, 765L,
                                  2L, 117L, 1367L, 75L, 118L, 1355L, 119L, 708L, 714L, 750L, 751L,
                                  748L, 749L, 752L, 742L, 743L, 744L, 745L, 746L, 747L, 785L, 786L,
                                  788L, 789L, 143L, 1369L, 1094L, 1095L, 724L, 803L, 804L, 725L,
                                  132L, 133L, 134L, 102L, 718L, 719L, 757L, 758L, 1124L, 1125L,
                                  802L, 1127L)
usethis::use_data(available_assay_descriptions)


# load all assay information for bioactivity summary plot -----------------
library(dplyr)
library(purrr)
url <- "https://ccte-api-ccd-dev.epa.gov/ccdapp2/assay-endpoint-detail"
assay_info <- bioactivity <- httr::GET(url)
res <- httr::content(assay_info)
total_elements <- res$page$totalElements
assay_total <- NULL
for(u in 1:length(c(seq(1000,total_elements,1000),total_elements))-1){
  url <- paste0("https://ccte-api-ccd-dev.epa.gov/ccdapp2/assay-endpoint-detail?page=",u,"&size=1000")
  # pluck all bioactivity information from the results
  assay_info <- bioactivity <- httr::GET(url)
  res <- httr::content(assay_info)
  assay_list <- res$`_embedded` %>%
    map_depth(2, "annotation") %>%
    flatten() %>% flatten() %>%
    data.table::rbindlist()
  assay_total <- bind_rows(assay_total,assay_list)
}
gene_total <- NULL
for(u in 1:length(c(seq(1000,total_elements,1000),total_elements))-1){
  url <- paste0("https://ccte-api-ccd-dev.epa.gov/ccdapp2/assay-endpoint-detail?page=",u,"&size=1000")
  assay_info <- bioactivity <- httr::GET(url)
  res <- httr::content(assay_info, as = "text")
  res <- jsonlite::fromJSON(res,flatten = TRUE)
  res <- res$`_embedded`$assayEndpointDetail
  res <- res |> select(assayComponentEndpointName,gene) |> tidyr::unnest_longer(gene, keep_empty = TRUE)

  gene <- res |> mutate(geneName = gene$geneName, geneSymbol = gene$geneSymbol, entrezGeneId = gene$entrezGeneId) |> select(-gene)

  gene_total <- bind_rows(gene_total,gene)
}
assay_total <- assay_total |> left_join(gene_total, by = "assayComponentEndpointName")
assay_total <- assay_total %>% filter(!is.na(aeid)) %>% group_by(aeid,assayComponentEndpointName,assayComponentEndpointDesc,assayFunctionType,normalizedDataType,analysisDirection,burstAssay,keyPositiveControl,signalDirection,intendedTargetType,intendedTargetTypeSub,intendedTargetFamily,intendedTargetFamilySub,assayComponentName,assayComponentDesc,assayComponentTargetDesc,parameterReadoutType,assayDesignType,assayDesignTypeSub,biologicalProcessTarget,detectionTechnologyType,detectionTechnologyTypeSub,detectionTechnology,signalDirectionType,keyAssayReagentType,keyAssayReagent,technologicalTargetType,technologicalTargetTypeSub,assayName,assayDesc,timepointHr,organism,tissue,cellFormat,cellFreeComponentSource,cellShortName,cellGrowthMode,assayFootprint,assayFormatType,assayFormatTypeSub,contentReadoutType,dilutionSolvent,dilutionSolventPercentMax,assaySourceName,assaySourceDesc)
assay_total_sum <- assay_total %>% summarise(entrezGeneId = list(entrezGeneId),geneName = list(geneName),geneSymbol = list(geneSymbol)) %>% ungroup()
full_assay_list <- assay_total_sum
usethis::use_data(full_assay_list, overwrite = TRUE)

# Example summary plot data -----------------------------------------------

library(tcpl)
library(dplyr)
tcplConf(user = "_dataminer", pass = "pass", db = "prod_internal_invitrodb_v3_3", drvr = "MySQL", host = "localhost")

#load chemical
chem <- tcplLoadChem(field = "dsstox_substance_id", val = "DTXSID7020182")

mc5_chid <- tcplQuery("Select * from mc5_chid;")

mc5_dat <- tcplLoadData(lvl = 5, fld = "spid", val = chem$spid)

m5 <- mc5_dat %>% select(m5id,aeid,hitc,modl_tp,coff,modl_ga) %>% left_join(mc5_chid, by = "m5id") %>% filter(chid_rep == 1)

#load assay dat
assay_endpoint_dat <- tcplLoadAeid(fld = "aeid", val = m5$aeid,
                                   add.fld = c("intended_target_family", "acid","analysis_direction"))

assay_component_dat <- tcplLoadAcid(fld = "acid", val = assay_endpoint_dat$acid,
                                    add.fld = c("biological_process_target","detection_technology","aid"))

assay_dat <- tcplLoadAid(fld = "aid", val = assay_component_dat$aid,
                         add.fld = c("organism","tissue","cell_format","assay_desc"))

#load cytopt dat
cytopt <- tcplQuery("select * from cytotox")
chem <- chem %>% left_join(cytopt, by = "chid")
#get cytotox limit
cytotox <- unique(chem$cytotox_lower_bound_um)

m5 <- m5 %>% left_join(assay_endpoint_dat, by = "aeid") %>% left_join(assay_component_dat, by = "acid") %>% left_join(assay_dat, by = "aid")

# add in the scaled top
m5 <- m5 %>% mutate(scaled_top = modl_tp/coff)

#filter to only hitc 1
m5 <- m5 %>% filter(hitc == 1)

# add in unlogged ac50
m5 <- m5 %>% mutate(ac50 = 10^modl_ga, cytotox = cytotox)

#save dataset for use in package
tcplSummaryPlotExampleData <- m5

usethis::use_data(tcplSummaryPlotExampleData)
