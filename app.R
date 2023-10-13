# Requires the following packages
# install.packages(c(
#   "dplyr", "geofi", "ggplot2", "ggiraph", "sf", "shiny"
# ))

suppressPackageStartupMessages({
  library(dplyr)
  library(geofi)
  library(ggplot2)
  library(leaflet)
  #library(leaflet.extras)
  library(rlang)
  library(sf)
  library(shiny)
  library(tidyr)
})

options(shiny.fullstacktrace = TRUE)

source("utilities.R", local = TRUE)

leave <- read.csv("data/leave_by_municipYear_OCT23.csv") |>
  rename(
    id = kunta,
    year = vuosi,
  ) |>
  mutate(id = as.integer(id))

mp <- get_municipalities(year = 2022) |>
  select(!year) |>
  rename(id = kunta) |>
  st_transform("+proj=longlat +datum=WGS84") |>
  left_join(leave, by = "id")

theme_set(
  theme_minimal(
    base_family = "Arial",
    base_size = 20
  )
)

wsc <- mp |>
  select(!id) |>
  rename(id = hyvinvointialue_code) |>
  group_by(id) |>
  summarise(
    geom_wsc = st_union(geom),
    wsc = unique(hyvinvointialue_name_fi),
    leave_total = sum(leave_total, na.rm = TRUE)
  ) |>
  ungroup()


#zip <- get_zipcodes(year = 2022)
#zip$leave <- sample(1000, nrow(zip), replace = TRUE)

ui <- fluidPage(
  titlePanel("Parental leave"),
  tabsetPanel(
    type = "tabs",
    id = "nav_tabs",
    map_tab(
      "Municipalities",
      "mp",
      min(leave$year),
      max(leave$year)
    ),
    map_tab(
      "Wellbeing service counties",
      "wsc",
      min(leave$year),
      max(leave$year)
    )
  )
)

server <- function(input, output) {
  rv <- reactiveValues(
    mp_selected = integer(0L),
    wsc_selected = integer(0L)
  )
  lapply(c("mp", "wsc"), function(x) {
    eval(observe_selection)
  })
  output$mp_map <- renderLeaflet({
    render_map(mp, lab = "municipality_name_fi", var = "shareOver18")
  })
  observeEvent(input$mp_year, {
    tmp <- mp |> filter(year == input$mp_year)
    vals <- sort(unique(tmp$leave_total))
    pal <- colorQuantile(palette = "YlGnBu", domain = vals)
    leafletProxy("mp_map") |>
      addPolygons(
        data = tmp,
        layerId = ~id,
        fillColor = ~pal(leave_total),
        color = "black",
        weight = 1,
        opacity = .4,
        dashArray = "3",
        fillOpacity = 0.75,
        label = tmp$municipality_name_fi,
        highlight = highlightOptions(
          weight = 1,
          color = "#789",
          dashArray = "",
          fillOpacity = 0.4
        )
      )
  })
  output$wsc_map <- renderLeaflet({
    render_map(wsc, lab = "wsc", var = "shareOver18")
  })
  output$mp_ts <- renderPlot({
    render_timeseries(
      mp,
      sel = rv$mp_selected,
      name = "municipality_name_fi",
      var = "shareOver18"
    )
  })
  output$wsc_ts <- renderPlot({
    render_timeseries(
      wsc,
      sel = rv$wsc_selected,
      name = "wsc",
      var = "shareOver18"
    )
  })
}

shinyApp(ui = ui, server = server)
