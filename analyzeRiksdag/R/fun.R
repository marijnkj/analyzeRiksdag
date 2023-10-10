#' 
#' @importFrom jsonlite fromJSON
#' @importFrom plotly plot_ly layout add_trace
#' @importFrom rvest read_html html_elements html_text2
#' @importFrom stringr str_replace_all str_extract
#' @importFrom rgdal
#' 
#' @export fun_fetch_data
#' @export fun_get_assembly_year_options
#' @export fun_bar_chart

# library(geojsonio)
# library(rgdal)
# library(shiny)
# library(sf)

fun_fetch_data <- function(assembly_year, beteckning, agenda_item) {
  # List of defaults
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
  return(df)
}

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
  
  list_assembly_year_options <- as.list(assembly_year_options_enc)
  names(list_assembly_year_options) <- assembly_year_options
  return(list_assembly_year_options)
}

fun_get_party_options <- function() {
  party_options <- assembly_and_party_options[!grepl("^[0-9]{4}/[0-9]{2}$", assembly_and_party_options)]
  df_party_options <- data.frame(option_name=party_options) |> 
    mutate(option_short=stringr::str_extract(option_name, r"{\(([A-Z]+)\)}")) |>
    mutate(option_short=sapply(option_short, function(x) substr(x, start=2, stop=nchar(x) - 1)))
  return(df_party_options)
}

# Plotting functions
fun_get_counts <- function(df, filter) {
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
  
  return(df_counts)
}

fun_bar_chart <- function(df, filter) {
  df_counts <- fun_get_counts(df, filter)
  
  plot <- df_counts |> plot_ly(type="bar") |> layout(xaxis=list(title=""), yaxis=list(title="Count"))
  for (col in colnames(df_counts)) {
    if (col != "filter") {
      plot <- plot |> add_trace(x=~filter, y=as.formula(paste0("~", col)), name=col) # https://stackoverflow.com/questions/51628311/using-variables-to-select-axis-in-r-plotly-like-aes-string
    }
  }
  
  return(plot)
}

### For checking back-end ###
fun_initialize_utskott_csv <- function() {
  utskott_options <- rvest::read_html("https://www.riksdagen.se/sv/sa-fungerar-riksdagen/utskotten-och-eu-namnden/") |> rvest::html_elements(".sc-620257bf-2.djABWg") |> rvest::html_text2()
  # As of 05-10-2023
  utskott_options_short <- c("AU", "CiU", "FiU", "FöU", "JuU", "KU", "KrU", "MJU", "NU", "SkU", "SfU", "SoU", "TU", "UbU", "UU", "EU")
  df_utskott_options <- data.frame(option_name=utskott_options, option_short=utskott_options_short)
  write.csv(df_utskott_options, paste0(system.file("extdata", package="analyzeRiksdag"), "utskott.csv"))
}

fun_check_utskott_file <- function() {
  df_utskott_options <- read.csv(paste0(system.file("extdata", package="analyzeRiksdag"), "utskott.csv"))
  utskott_from_website <- rvest::read_html("https://www.riksdagen.se/sv/sa-fungerar-riksdagen/utskotten-och-eu-namnden/") |> rvest::html_elements(".sc-620257bf-2.djABWg") |> rvest::html_text2()
  if (!all(df_utskott_options$option_name == utskott_from_website)) {
    warning("utskott.csv is not updated with the current listing on the website.")
  } else {
    print("All good!")
  }
}

test_fun <- function(year) {
  if (year == "2022%2F23") {
    return(list("A"=1, "B"=2))
  } else {
    return(list("C"=3, "D"=4))
  }
}

ui <- fluidPage(
  titlePanel("Swedish voting"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("assembly_year", h3("Choose assembly year"),
                  choices=fun_get_assembly_year_options(), selected=fun_get_assembly_year_options()[1]),
      textInput("beteckning", h3("Choose beteckning"), value="AU10"),
      textInput("agenda_item", h3("Choose agenda point"), value="2"),
      selectInput("filter", h3("Choose filter"),
                  choices=list("Gender"="Gender", "Region"="Region", "Party"="Party"), selected="Gender"),
      uiOutput("utskott_choice")
    ),
    mainPanel(
      plotlyOutput("bar_plot")
    )
  )
)

# Takes assembly_year with %2F (can use fun_get_assembly_year_options), return list of utskott
# Takes list of utskott

server <- function(input, output) {
  output$utskott_choice <- renderUI({
    selectInput("test", h3("Test"),
                choices=test_fun(input$assembly_year))
  })
  
  output$bar_plot <- renderPlotly({
    df <- fun_fetch_data(input$assembly_year, input$beteckning, input$agenda_item)
    fun_bar_chart(df, input$filter)})
}

shinyApp(ui=ui, server=server)

### Map ###
# df_counts1 <- df_counts |>
#   mutate(perc_yes=Ja / (Avstår + Frånvarande + Ja + Nej) * 100)
# 
# sweden_counties <- jsonlite::fromJSON("https://raw.githubusercontent.com/okfse/sweden-geojson/master/swedish_regions.geojson")
# plot_ly() |>
#   add_trace(type="choroplethmapbox",
#             # geojson=sweden_counties,
#             z=df_counts1$perc_yes)
# 
# row_vastra_gotaland <- as.data.frame(as.list(colSums(df_counts1[c("Västra Götalands läns norra", "Västra Götalands läns östra", "Västra Götalands läns södra", "Västra Götalands läns västra"), c("Avstår" ,"Frånvarande", "Ja", "Nej")])), row.names="Västra Götaland") # https://stackoverflow.com/questions/3991905/sum-rows-in-data-frame-or-matrix
# row_vastra_gotaland$filter <- "Västra Götaland"
# 
# df_counts1 <- rbind(df_counts1, row_vastra_gotaland)
# 
# valkrets_shapes <- readOGR(dsn="2018_valgeografi_riksdagsvalkretsar/", layer="alla_riksdagsvalkretsar", verbose=FALSE)
# valkrets_geojson <- as.list(geojson_json(valkrets_shapes))
# 
# df_counts1 |>
#   plot_ly(locations=~filter, z=~perc_yes, type="choroplethmapbox", 
#           geojson=valkrets_geojson, 
#           marker=list(line=list(width=0), opacity=0.5)) |>
#   layout(mapbox=list(style="carto-positron"))




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
                readRDS("~/analyzeRiksdag/analyzeRiksdag/inst/extdata/titleframe.rds")),
          "~/analyzeRiksdag/analyzeRiksdag/inst/extdata/titleframe.rds")
  
}

get_utskott <- function(year){
  #titleframe <- readRDS(paste0(system.file("extdata", package = "analyzeRiksdag"), "titleframe.rds"))
  titleframe <- readRDS("~/analyzeRiksdag/analyzeRiksdag/inst/extdata/titleframe.rds")
  yearindex <- which(unlist(fun_get_assembly_year_options()) == year)
  year <- names(fun_get_assembly_year_options())[[yearindex]]
  titleframe <- titleframe[titleframe$rm == year, c("organ")]
  output <- as.list(unique(as.character(titleframe)))
  names(output) <- unlist(output)
  return(output)
}

get_titlar <- function(year, utskott){
  #titleframe <- readRDS(paste0(system.file("extdata", package = "analyzeRiksdag"), "titleframe.rds"))
  titleframe <- readRDS("~/analyzeRiksdag/analyzeRiksdag/inst/extdata/titleframe.rds")
  yearindex <- which(unlist(fun_get_assembly_year_options()) == year)
  year <- names(fun_get_assembly_year_options())[[yearindex]]
  titleframe <- titleframe[titleframe$rm == year, c("titel","nummer")]
  titleframe <- titleframe[order(titleframe$titel),]
  titleframe <- titleframe[!duplicated(titleframe$titel),]
  output <- as.list(sapply(1:nrow(titleframe), function(x) paste0(utskott,titleframe$nummer[x])))
  names(output) <- titleframe$titel
  return(output)
}
