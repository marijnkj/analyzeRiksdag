library(jsonlite)
library(tibble)
library(ggplot2)
library(rvest)
library(stringr)
library(dplyr)

# User choice
assembly_year <- ""
beteckning <- "" # Match beteckning and punkt in one choice, scrape?
agenda_item <- "2" # Match to title at later stage

# Default settings
url <- "https://data.riksdagen.se/voteringlista/"
list_parties <- ""
county <- ""
vote_type <- ""
iid <- ""
rost <- ""
svar <- "10000"
format <- "json"
gruppering <- ""

url <- paste0(url, 
              "?rm=", assembly_year,
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
url1 <- "https://data.riksdagen.se/voteringlista/?rm=2022%2F23&bet=AU1&punkt=2&parti=C&parti=FP&valkrets=%C3%96sterg%C3%B6tlands+l%C3%A4n&rost=Fr%C3%A5nvarande&iid=0538982776628&sz=500&utformat=json&gruppering="
df <- as.data.frame(fromJSON(url))
df |>
  group_by("voteringlista.votering.namn") |>
  summarize(count=n())

titelregister <- list()
sapply(unique(df$voteringlista.votering.votering_url_xml), function(x){
  x <- paste0(x, ".html")
  imported <- as_list(xml_child(read_xml(x)))
  
  titelregister[[x]] <<- imported$titel[[1]]
})



importRiksdag <- function(){
  
  # Default settings
  url <- "https://data.riksdagen.se/voteringlista/"
  list_parties <- ""
  assembly_year <- paste0(2002:2022,"%2F",substr(x = 2003:2023, start = 3, stop =4))
  county <- ""
  vote_type <- ""
  iid <- ""
  rost <- ""
  agenda_item <- ""
  svar <- "10000"
  format <- "json"
  gruppering <- c("bet", "votering_id")
  
  url <- paste0(url, 
                "?rm=", c("2002%2F03" ,assembly_year),
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

  
  url <- url[order(sapply(url, function(x) intToUtf8(rev(utf8ToInt(x)))))]
  #url1 <- "https://data.riksdagen.se/voteringlista/?rm=2022%2F23&bet=AU1&punkt=2&parti=C&parti=FP&valkrets=%C3%96sterg%C3%B6tlands+l%C3%A4n&rost=Fr%C3%A5nvarande&iid=0538982776628&sz=500&utformat=json&gruppering="
  df <- as.data.frame(fromJSON(url))
  df <- lapply(url, function(x) as.data.frame(fromJSON(x))[-c(1:9)])
  
  data_id <- do.call(rbind, lapply(1:11, function(x) df[[x]]))
  data_bet <- do.call(rbind, lapply(12:22, function(x) df[[x]]))
  
  
  createID <- function(df){
    df$ID <- paste0("j", df$voteringlista.votering.Ja,
                    "n", df$voteringlista.votering.Nej,
                    "f", df$voteringlista.votering.Frånvarande,
                    "aa", df$voteringlista.votering.Avstår)
    df
  }
  
  data_bet<- createID(data_bet)
  data_id <- createID(data_id)
  merged <- inner_join(data_bet[,c(3:6,11)],data_id[,c(3,8)], by = "ID")

  }
