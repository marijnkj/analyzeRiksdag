library(jsonlite)
library(tibble)
library(ggplot2)
library(rvest)
library(stringr)
library(dplyr)
library(xml2)




importRiksdag <- function(){
  
  # Default settings
  url <- "https://data.riksdagen.se/voteringlista/"
  # All parties
  list_parties <- ""
  # All years available twice, as we want to generate urls for two data sets per year
  assembly_year <- paste0(2002:2022,"%2F",substr(x = 2003:2023, start = 3, stop =4))
  # All counties
  county <- ""
  # All vote types
  vote_type <- ""
  iid <- ""
  rost <- ""
  agenda_item <- ""
  svar <- "10000"
  format <- "json"
  # Download data set twice for every year, with different groupings
  gruppering <- c("bet", "votering_id")
  
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

  # Order url in order of grouping type
  # Urls ends with different letters depending on grouping type
  # Reverses url-strings to they can be ordered by last letter
  url <- url[order(sapply(url, function(x) intToUtf8(rev(utf8ToInt(x)))))]
  #Downloads all datasets
  df <- lapply(url, function(x) as.data.frame(fromJSON(x))[-c(1:9)])
  
  # Creates one dataset for each grouping type
  data_id <- do.call(rbind, lapply(1:21, function(x) df[[x]]))
  data_bet <- do.call(rbind, lapply(22:42, function(x) df[[x]]))
  
  # Because our two data sets has to be merged without containing a common idenitifer variable
  # We create our own ID variable by making a string of each observations common variable
  createID <- function(df){
    df$ID <- paste0("v",df$voteringlista..villkor,
                    "j", df$voteringlista.votering.Ja,
                    "n", df$voteringlista.votering.Nej,
                    "f", df$voteringlista.votering.Frånvarande,
                    "a", df$voteringlista.votering.Avstår)
    df
  }
  
  
  data_bet<- createID(data_bet)
  data_id <- createID(data_id)
  # Merges our two datasets
  merged <- full_join(data_bet[,c(3:6,11)],data_id[,c(3,8)], by = "ID")
  
  count <- 0
  titles <- sapply(unique(merged$voteringlista.votering.votering_id), function(x){
    count <<- count + 1
    print(count/35825)
    url <- paste0("https://data.riksdagen.se/votering/",x)
    imported <- as_list(xml_child(read_xml(url)))
    return(imported$titel[[1]])
  })
  
  merged <- inner_join(x = merged, y = data.frame(voteringlista.votering.votering_id = names(titles), title = titles))

  }
