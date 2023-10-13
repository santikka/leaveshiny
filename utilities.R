map_tab <- function(title, name, low, high) {
  tabPanel(
    title,
    fluidRow(
      column(
        6,
        sliderInput(
          inputId = paste0(name, "_year"),
          label = "Year",
          min = low,
          max = high,
          value = low,
          step = 1L,
          sep = "",
          ticks = FALSE,
          width = "100%",
          animate = animationOptions(
            loop = TRUE
          )
        ),
        leafletOutput(paste0(name, "_map"), width = "100%", height = 800)
      ),
      column(
        6,
        plotOutput(
          paste0(name, "_ts")
        )
      )
    )
  )
}

observe_selection <- quote({
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
        leaflet::leafletProxy(mapId = paste0(x, "_map")) |>
          leaflet::addPolylines(
            data = tmp,
            layerId = ~id,
            group = paste0(x, "_selected_", sel_id),
            color = "white",
            weight = 1.5,
            opacity = 1.0
          )
      }
    }
  })
})

render_map <- function(x, lab, var) {
  vals <- sort(unique(x[[var]]))
  pal <- colorQuantile(palette = "YlGnBu", domain = vals)
  x |>
    leaflet() |>
    addTiles(
      urlTemplate = "https://tiles.kartat.kapsi.fi/taustakartta/{z}/{x}/{y}.jpg",
      options = tileOptions(
        opacity = 0.4,
        continuousWorld = TRUE,
        minZoom = 5
      )
    ) |>
    addPolygons(
      layerId = ~id,
      fillColor = as.formula(paste0("~pal(", var, ")")),
      color = "black",
      weight = 1,
      opacity = .4,
      dashArray = "3",
      fillOpacity = 0.75,
      label = x[[lab]],
      highlight = highlightOptions(
        weight = 1,
        color = "#789",
        dashArray = "",
        fillOpacity = 0.4
      )
    ) |>
    addLegend(
      pal = pal,
      values = x[[var]][!is.na(x[[var]])],
      title = '<small>Total parental leave</small>',
      position = 'bottomleft'
    )
}

render_timeseries <- function(x, sel, name, var) {
  x_str <- deparse1(substitute(x))
  name_sym <- sym(name)
  var_sym <- sym(var)
  if (length(sel) > 0L) {
    as.data.frame(x) |>
      filter(id %in% sel) |>
      select(!!name_sym | !!var_sym | year) |>
      distinct() |>
      ggplot(
        aes(
          x = year,
          y = !!var_sym,
          group = !!name_sym,
          colour = !!name_sym
        )
      ) +
      scale_color_discrete(name = "Kunta") +
      geom_line(linewidth = 1.0) +
      geom_point()
  }
}
