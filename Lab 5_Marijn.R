library(jsonlite)
library(tibble)
library(ggplot2)
library(plotly)
library(rvest) # Scraping
library(stringr)
library(dplyr)
library(ggplot2)
library(geojsonio)
library(shiny)

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

df <- as.data.frame(jsonlite::fromJSON(url))
### Scrape county, party, and year options ###
# https://rvest.tidyverse.org/articles/rvest.html
# https://www.utf8-chartable.de/
# https://sparkbyexamples.com/r-programming/replace-character-in-a-string-of-r-dataframe/
fun_get_county_options <- function() {
  county_options <- rvest::read_html("https://data.riksdagen.se/voteringlista/") |> rvest::html_elements("#valkrets") |> rvest::html_elements("option") |> rvest::html_text2()
  county_options <- county_options[-1] # Remove "[Välj valkrets]"
  country_options_enc <- county_options |> stringr::str_replace_all("Ö", "%C3%96") |> stringr::str_replace_all("ö", "%C3%B6") |> stringr::str_replace_all("Ä", "%C3%84") |> stringr::str_replace_all("ä", "%c3%A4") |> stringr::str_replace_all("Å", "%C3%85") |> stringr::str_replace_all("å", "%C3%A5")
  df_county_options <- data.frame(option_name=options, option_enc=options_enc)
  return(df_county_options)
}

fun_get_assembly_year_options <- function() {
  assembly_and_party_options <- rvest::read_html("https://data.riksdagen.se/voteringlista/") |> rvest::html_elements("fieldset") |> rvest::html_elements("label") |> rvest::html_text2()
  assembly_year_options <- assembly_and_party_options[grepl("^[0-9]{4}/[0-9]{2}$", assembly_and_party_options)]
  assembly_year_options_enc <- assembly_year_options |> stringr::str_replace_all("/", "%2F")
  df_assembly_year_options <- data.frame(option_name=assembly_year_options, option_enc=assembly_year_options_enc)
  return(df_assembly_year_options)
}

fun_get_party_options <- function() {
  party_options <- assembly_and_party_options[!grepl("^[0-9]{4}/[0-9]{2}$", assembly_and_party_options)]
  df_party_options <- data.frame(option_name=party_options) |> 
    mutate(option_short=stringr::str_extract(option_name, r"{\(([A-Z]+)\)}")) |>
    mutate(option_short=sapply(option_short, function(x) substr(x, start=2, stop=nchar(x) - 1)))
  return(df_party_options)
}

### Scrape beteckning categories ###
fun_initialize_utskott_csv <- function() {
  utskott_options <- rvest::read_html("https://www.riksdagen.se/sv/sa-fungerar-riksdagen/utskotten-och-eu-namnden/") |> rvest:html_elements(".sc-620257bf-2.djABWg") |> rvest::html_text2()
  # As of 05-10-2023
  utskott_options_short <- c("AU", "CiU", "FiU", "FöU", "JuU", "KU", "KrU", "MJU", "NU", "SkU", "SfU", "SoU", "TU", "UbU", "UU", "EU")
  df_utskott_options <- data.frame(option_name=utskott_options, option_short=utskott_options_short)
  write.csv(df_utskott_options, "utskott.csv")
}

fun_check_utskott_file <- function() {
  df_utskott_options <- read.csv("utskott.csv")
  utskott_from_website <- rvest::read_html("https://www.riksdagen.se/sv/sa-fungerar-riksdagen/utskotten-och-eu-namnden/") |> rvest::html_elements(".sc-620257bf-2.djABWg") |> rvest::html_text2()
  if (!all(df_utskott_options$option_name == utskott_from_website)) {
    warning("utskott.csv is not updated with the current listing on the website.")
  }
}

### Some plot ideas ###
swedish_counties <- geojsonio::geojson_read("https://github.com/okfse/sweden-geojson/blob/master/swedish_regions.geojson")

fun_bar_chart <- function(filter) {
  if (filter == "Gender") {
    filter_var <- "voteringlista.votering.kon"
  } else if (filter == "Region") {
    filter_var <- "voteringlista.votering.valkrets"
  } else if (filter == "Party") {
    filter_var <- "voteringlista.votering.parti"
  } else {
    stop("filter must be either 'Gender', 'Region', or 'Party'!")
  }
  
  df_counts <- as.data.frame.matrix(table(df[[filter_var]], df$voteringlista.votering.rost)) # https://www.geeksforgeeks.org/how-to-convert-table-to-dataframe-in-r/
  df_counts$filter <- rownames(df_counts)

  plot <- df_counts |> plot_ly(type="bar") |> layout(xaxis=list(title=""), yaxis=list(title="Count"))
  for (col in colnames(df_counts)) {
    if (col != "filter") {
      plot <- plot |> add_trace(x=~filter, y=as.formula(paste0("~", col)), name=col) # https://stackoverflow.com/questions/51628311/using-variables-to-select-axis-in-r-plotly-like-aes-string
    }
  }
  
  return(plot)
}

### SHINY APP ###
# https://plotly.com/r/bar-charts/
df_counts |>
  plot_ly(x=~filter, y=~Ja, type="bar", name="Yes") |>
  add_trace(y=~Nej, name="No") |>
  add_trace(y=~Avstår, name="Did not vote") |>
  add_trace(y=~Frånvarande, name="Absent") |>
  layout(xaxis=list(title=""),
         yaxis=list(title="Count"))

ui <- fluidPage(
  titlePanel("Swedish voting"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("filter", h3("Choose filter"),
                             choices=list("Gender"="Gender", "Region"="Region", "Party"="Party"), selected="Gender")
    ),
    mainPanel(
      plotlyOutput("bar_plot")
    )
  )
)

server <- function(input, output) {
  output$bar_plot <- renderPlotly(fun_bar_chart(input$filter))
}

shinyApp(ui=ui, server=server)

### Map ###
df_counts1 <- df_counts |>
  mutate(perc_yes=Ja / (Avstår + Frånvarande + Ja + Nej) * 100)

sweden_counties <- jsonlite::fromJSON("https://raw.githubusercontent.com/okfse/sweden-geojson/master/swedish_regions.geojson")
plot_ly() |>
  add_trace(type="choroplethmapbox",
            # geojson=sweden_counties,
            z=df_counts1$perc_yes)

row_vastra_gotaland <- as.data.frame(as.list(colSums(df_counts1[c("Västra Götalands läns norra", "Västra Götalands läns östra", "Västra Götalands läns södra", "Västra Götalands läns västra"), c("Avstår" ,"Frånvarande", "Ja", "Nej")])), row.names="Västra Götaland") # https://stackoverflow.com/questions/3991905/sum-rows-in-data-frame-or-matrix
row_vastra_gotaland$filter <- "Västra Götaland"

df_counts1 <- rbind(df_counts1, row_vastra_gotaland)
