render_map <- function(x, lab) {
  pal <- leaflet::colorNumeric(palette = "YlGnBu", domain = x$leave_total)
  x |>
    leaflet::leaflet() |>
    leaflet::addTiles(
      urlTemplate = "https://tiles.kartat.kapsi.fi/taustakartta/{z}/{x}/{y}.jpg",
      options = leaflet::tileOptions(opacity = 0.4, continuousWorld = TRUE)
    ) |>
    leaflet::addPolygons(
      layerId = ~id,
      fillColor = ~pal(leave_total),
      color = "black",
      weight = 2,
      opacity = .4,
      dashArray = "3",
      fillOpacity = 0.75,
      label = x[[lab]],
      highlight = highlightOptions(
        weight = 2,
        color = "#789",
        dashArray = "",
        fillOpacity = 0.4
      )
    ) |>
    leaflet::addLegend(
      pal = pal,
      values = x$leave_total[!is.na(x$leave_total)],
      title = '<small>Total parental leave</small>',
      position = 'bottomleft'
    )
}

render_timeseries <- function(x, sel, name) {
  x_str <- deparse1(substitute(x))
  name_sym <- sym(name)
  if (length(sel) > 0L) {
    as.data.frame(x) |>
      filter(id %in% sel) |>
      select(!!name_sym | leave_total) |>
      #select(!!name_sym | starts_with("leave")) |>
      # pivot_longer(
      #   cols = starts_with("leave"),
      #   names_to = "year",
      #   values_to = "leave"
      # ) |>
      ggplot(
        aes(
          x = !!name_sym,
          y = leave_total,
          group = !!name_sym,
          fill = !!name_sym
        )
      ) +
      geom_bar(stat = "identity") +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
      #scale_x_discrete(labels = c("2018", "2019", "2020", "2021", "2022")) +
      #scale_color_discrete(name = "Kunta") +
      #geom_line(linewidth = 1.0) +
      #geom_point()
  }
}
