library(jsonlite)
library(tibble)
library(ggplot2)
library(rvest)
library(stringr)
library(dplyr)

# User choice
assembly_year <- "2022%2F23"
beteckning <- "" # Match beteckning and punkt in one choice, scrape?
agenda_item <- "2" # Match to title at later stage

# Default settings
url <- "https://data.riksdagen.se/voteringlista/"
list_parties <- ""
county <- ""
vote_type <- ""
rost <- ""
iid <- ""
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

df <- as.data.frame(fromJSON(url))

### Scrape county, party, and year options ###
# https://rvest.tidyverse.org/articles/rvest.html
# https://www.utf8-chartable.de/
# https://sparkbyexamples.com/r-programming/replace-character-in-a-string-of-r-dataframe/
county_options <- read_html("https://data.riksdagen.se/voteringlista/") |> html_elements("#valkrets") |> html_elements("option") |> html_text2()
county_options <- county_options[-1] # Remove "[Välj valkrets]"
country_options_enc <- county_options |> str_replace_all("Ö", "%C3%96") |> str_replace_all("ö", "%C3%B6") |> str_replace_all("Ä", "%C3%84") |> str_replace_all("ä", "%c3%A4") |> str_replace_all("Å", "%C3%85") |> str_replace_all("å", "%C3%A5")
df_county_options <- data.frame(option_name=options, option_enc=options_enc)

assembly_and_party_options <- read_html("https://data.riksdagen.se/voteringlista/") |> html_elements("fieldset") |> html_elements("label") |> html_text2()
assembly_year_options <- assembly_and_party_options[grepl("^[0-9]{4}/[0-9]{2}$", assembly_and_party_options)]
assembly_year_options_enc <- assembly_year_options |> str_replace_all("/", "%2F")
df_assembly_year_options <- data.frame(option_name=assembly_year_options, option_enc=assembly_year_options_enc)

party_options <- assembly_and_party_options[!grepl("^[0-9]{4}/[0-9]{2}$", assembly_and_party_options)]
df_party_options <- data.frame(option_name=party_options) |> 
  mutate(option_short=str_extract(option_name, regex(r"{\(([A-Z]+)\)}"))) |>
  mutate(option_short=sapply(option_short, function(x) substr(x, start=2, stop=nchar(x) - 1)))

### Scrape beteckning categories ###
# SOME USE ONLY FIRST LETTER
utskott_options <- read_html("https://www.riksdagen.se/sv/sa-fungerar-riksdagen/utskotten-och-eu-namnden/") |> html_elements(".sc-620257bf-2.djABWg") |> html_text2()
utskott_options_short <- c("AU", "CiU", "FiU", "FöU", "JuU", "KU", "KrU", "MJU", "NU", "SkU", "SfU", "SoU", "TU", "UbU", "UU", "EU")
