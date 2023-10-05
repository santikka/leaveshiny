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

theme_set(
  theme_minimal(
    base_family = "Arial",
    base_size = 20
  )
)

mp <- get_municipalities(year = 2022) |>
  rename(id = kunta) |>
  sf::st_transform("+proj=longlat +datum=WGS84")
mp$leave1 <- sample(1000, nrow(mp), replace = TRUE)
mp$leave2 <- sample(1000, nrow(mp), replace = TRUE)
mp$leave3 <- sample(1000, nrow(mp), replace = TRUE)
mp$leave4 <- sample(1000, nrow(mp), replace = TRUE)
mp$leave5 <- sample(1000, nrow(mp), replace = TRUE)

wsc <- mp |>
  select(!id) |>
  rename(id = hyvinvointialue_code) |>
  group_by(id) |>
  summarise(
    geom_hyv = st_union(geom),
    wsc = unique(hyvinvointialue_name_fi),
    leave1 = sum(leave1),
    leave2 = sum(leave2),
    leave3 = sum(leave3),
    leave4 = sum(leave4),
    leave5 = sum(leave5)
  ) |>
  ungroup()

# p1 <- ggplot(mp, aes(fill = kunta)) +
#  geom_sf(colour = alpha("white", 1/3)) +
#  labs(subtitle = "municipalities") +
#  theme(
#    legend.position = "none",
#    axis.text = element_blank(),
#    axis.title = element_blank(),
#    panel.grid = element_blank()
#  )
#
# p2 <- ggplot(hyv, aes(fill = id)) +
#  geom_sf(colour = alpha("white", 1/3)) +
#  labs(subtitle = "wellbeing districts") +
#  theme(
#    legend.position = "none",
#    axis.text = element_blank(),
#    axis.title = element_blank(),
#    panel.grid = element_blank()
#  )


#zip <- get_zipcodes(year = 2022)
#zip$leave <- sample(1000, nrow(zip), replace = TRUE)

ui <- fluidPage(
  titlePanel("Parental leave"),
  tabsetPanel(
    type = "tabs",
    id = "nav_tabs",
    tabPanel(
      "Municipalities",
      fluidRow(
        column(
          6,
          leafletOutput("mp_map", width = "100%", height = 800)
        ),
        column(
          6,
          plotOutput(
            "mp_ts"
          )
        )
      )
    ),
    tabPanel(
      "Wellbeing service counties",
      fluidRow(
        column(
          6,
          leafletOutput("wsc_map", width = "100%", height = 800)
        ),
        column(
          6,
          plotOutput(
            "wsc_ts"
          )
        )
      )
    )
    #tabPanel(
    #  "ZIP codes",
    #  fluid_row(
    #    column(
    #      8,
    #      girafeOutput(
    #        "map_zipcodes",
    #        height = "800px"
    #      )
    #    )
    #  )
    #)
  )
)

server <- function(input, output) {
  rv <- reactiveValues(
    mp_selected = integer(0L),
    wsc_selected = integer(0L)
  )
  lapply(c("mp", "wsc"), function(x) {
    observe({
      sel_id <- input[[paste0(x, "_map_shape_click")]]$id
      rv_id <- paste0(x, "_selected")
      selected <- isolate(rv[[rv_id]])
      if (!is.null(sel_id)) {
        if (sel_id %in% selected) {
          rv[[rv_id]] <- setdiff(selected, sel_id)
          leafletProxy(mapId = paste0(x, "_map")) |>
            clearGroup(group = paste0(x, "_selected_", sel_id))
        } else {
          rv[[rv_id]] <- c(selected, sel_id)
          tmp <- get(x) |>
            filter(.data$id == sel_id)
          leafletProxy(mapId = paste0(x, "_map")) |>
            addPolylines(
              data = tmp,
              layerId = ~id,
              group = paste0(x, "_selected_", sel_id),
              color = "white",
              weight = 3.0,
              opacity = 1.0
            )
        }
      }
    })
  })
  output$mp_map <- renderLeaflet({
    render_map(mp, lab = "municipality_name_fi")
  })
  output$wsc_map <- renderLeaflet({
    render_map(wsc, lab = "wsc")
  })
  output$mp_ts <- renderPlot({
    render_timeseries(mp, sel = rv$mp_selected, name = "municipality_name_fi")
  })
  output$wsc_ts <- renderPlot({
    render_timeseries(wsc, sel = rv$wsc_selected, name = "wsc")
  })
}

shinyApp(ui = ui, server = server)
