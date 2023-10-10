library(jsonlite)
library(tibble)
library(ggplot2)
library(rvest)
library(stringr)
library(dplyr)
library(xml2)




importRiksdag <- function(){
  
  currYear <- as.numeric(strsplit(x= as.character(Sys.Date()), split = "-")[[1]][1])
  
  # Default settings
  url <- "https://data.riksdagen.se/voteringlista/"
  # All parties
  list_parties <- ""
  # All years in url-compatible format
  assembly_year <- paste0(2002:(currYear-1),"%2F",substr(x = 2003:currYear, start = 3, stop =4))
  # All counties
  county <- ""
  # All vote types
  vote_type <- ""
  beteckning <- ""
  iid <- ""
  rost <- ""
  agenda_item <- ""
  svar <- "10000"
  format <- "json"
  gruppering <- "votering_id"
  
  url <- paste0(url, 
                "?rm=", c(assembly_year ,assembly_year),
                "&bet=", beteckning,
                "&punkt=", agenda_item,
                paste0("&parti=", list_parties, collapse=""),
                "&valkrets=", county,
                "&rost=", rost,
                "&iid=", iid,
                "&sz=", svar,
                "&utformat=", format,
                "&gruppering=", gruppering
  )


  #Downloads all datasets
  df <- lapply(url, function(x) as.data.frame(fromJSON(x))[-c(1:9)])
  
  # Creates one dataset for each grouping type
  data_id <- do.call(rbind, lapply(1:length(url), function(x) df[[x]]))
  
  # Count initialized to track progress through print()
  count <- 0
  for(i in unique(data_id$voteringlista.votering.votering_id)){
    count <- count + 1
    print(count/length(unique(data_id$voteringlista.votering.votering_id)))
    url <- paste0("https://data.riksdagen.se/votering/",i)
    imported <- as_list(xml_child(read_xml(url)))
    data_id$dok_id[data_id$voteringlista.votering.votering_id == i] <- imported[["dok_id"]][[1]]
    data_id$rm[data_id$voteringlista.votering.votering_id == i] <- imported[["rm"]][[1]]
    data_id$bet[data_id$voteringlista.votering.votering_id == i] <- imported[["bet"]][[1]]
    data_id$typrubrik[data_id$voteringlista.votering.votering_id == i] <- imported[["typrubrik"]][[1]]
    data_id$dokumentnamn[data_id$voteringlista.votering.votering_id == i] <- imported[["dokumentnamn"]][[1]]
    data_id$organ[data_id$voteringlista.votering.votering_id == i] <- imported[["organ"]][[1]]
    data_id$nummer[data_id$voteringlista.votering.votering_id == i] <- imported[["nummer"]][[1]]
    data_id$titel[data_id$voteringlista.votering.votering_id == i] <- imported[["titel"]][[1]]
  }
  
  data_id$organ <- as.factor(data_id$organ)
  levels(data_id$organ)[levels(data_id$organ) %in% c("BOU", "FIU", "FÖU", "JUU", "KRU", "SFU", "SKU", "SOU", "UBU", "UFÖU")] <- 
    c("BoU", "FiU", "FöU", "JuU", "KrU", "SfU", "SkU", "SoU", "UbU", "UFöU")

  saveRDS(titleframe, "~/analyzeRiksdag/analyzeRiksdag/inst/titleframe.rds")
 
}
  





get_Riksdag <- function(){
  
  currYear <- as.numeric(strsplit(x= as.character(Sys.Date()), split = "-")[[1]][1])
  
  
  # Default settings
  url <- "https://data.riksdagen.se/voteringlista/"
  # All parties
  list_parties <- ""
  # All years in url-compatible format
  assembly_year <- paste0(2002:(currYear-1),"%2F",substr(x = 2003:currYear, start = 3, stop =4))
  # All counties
  county <- ""
  # All vote types
  vote_type <- ""
  beteckning <- ""
  iid <- ""
  rost <- ""
  agenda_item <- ""
  svar <- "10000"
  format <- "json"
  gruppering <- "votering_id"
  
  url <- paste0(url, 
                "?rm=", c(assembly_year ,assembly_year),
                "&bet=", beteckning,
                "&punkt=", agenda_item,
                paste0("&parti=", list_parties, collapse=""),
                "&valkrets=", county,
                "&rost=", rost,
                "&iid=", iid,
                "&sz=", svar,
                "&utformat=", format,
                "&gruppering=", gruppering
  )
  
  
  #Downloads all datasets
  df <- lapply(url, function(x) as.data.frame(fromJSON(x))[-c(1:9)])
  
  # Creates one dataset for each grouping type
  data_id <- do.call(rbind, lapply(1:length(url), function(x) df[[x]]))
  
  loaded_ids <- readRDS("~/analyzeRiksdag/analyzeRiksdag/data/titleframe.rds")$voteringlista.votering.votering_id
  
  
  missing_ids <- df$voteringlista.votering.votering_id[!(df$voteringlista.votering.votering_id %in% loaded_ids)]
  
  for(i in missing_ids){
    url <- paste0("https://data.riksdagen.se/votering/",i)
    imported <- as_list(xml_child(read_xml(url)))
    data_id$dok_id[data_id$voteringlista.votering.votering_id == i] <- imported[["dok_id"]][[1]]
    data_id$rm[data_id$voteringlista.votering.votering_id == i] <- imported[["rm"]][[1]]
    data_id$bet[data_id$voteringlista.votering.votering_id == i] <- imported[["bet"]][[1]]
    data_id$typrubrik[data_id$voteringlista.votering.votering_id == i] <- imported[["typrubrik"]][[1]]
    data_id$dokumentnamn[data_id$voteringlista.votering.votering_id == i] <- imported[["dokumentnamn"]][[1]]
    data_id$organ[data_id$voteringlista.votering.votering_id == i] <- imported[["organ"]][[1]]
    data_id$nummer[data_id$voteringlista.votering.votering_id == i] <- imported[["nummer"]][[1]]
    data_id$titel[data_id$voteringlista.votering.votering_id == i] <- imported[["titel"]][[1]]
  }
  
  data_id$organ <- as.factor(data_id$organ)
  levels(data_id$organ)[levels(data_id$organ) %in% c("BOU", "FIU", "FÖU", "JUU", "KRU", "SFU", "SKU", "SOU", "UBU", "UFÖU")] <- 
    c("BoU", "FiU", "FöU", "JuU", "KrU", "SfU", "SkU", "SoU", "UbU", "UFöU")
  
  
  saveRDS(rbind(data_id, 
                readRDS("~/analyzeRiksdag/analyzeRiksdag/inst/titleframe.rds")),
          "~/analyzeRiksdag/analyzeRiksdag/inst/titleframe.rds")
 
}
