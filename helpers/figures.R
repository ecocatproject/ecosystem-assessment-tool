# modules/figures.R

# =============================================================================
# Figure functions — ggplot2 (static) + plotly (interactive)
#
# All functions accept evaluated data (not Shiny reactives) and return
# a ggplot2 or plotly object.
#
# Called by:
#   • description_eu_working.R  - leaflet map distribution map
#   • criterion_a.R      — renderPlotly (plotly version)
#   • criterion_b.R      — renderPlotly (plotly version)
#   • report Rmd files   — HTML branch (plotly), Word branch (ggplot)
#   • report_wthumb.R    — thumbnail preview + PNG download (ggplot versions)
#   • script_export.R    — copied verbatim into R/ecocat_functions.R of the export
# =============================================================================

# Shared colours / linetypes — consistent throughout app
fig_cols     <- c("PRC" = "#D55E00", "ARC" = "#56B4E9")
fig_linetype <- c("median" = "dotted", "recent" = "dashed")
scenario_label <- c("recent" = "Near-term", "median" = "Long-term")

# plotly ribbon fill colours (method × scenario)
fig_ribbon_fill <- list(
  PRC.recent = "rgba(255,99,71,0.10)",  PRC.median = "rgba(255,99,71,0.18)",
  ARC.recent = "rgba(70,130,180,0.10)", ARC.median = "rgba(70,130,180,0.18)"
)


# =============================================================================
# 1.  DISTRIBUTION CHANGE MAP
# =============================================================================

# Static distribution map -------------------------------------------------
#   ggplot2 
#   r_map  — latest sampled SpatRaster already in EPSG:4326 (output of rasters())
#   r_loss — loss sampled SpatRaster in EPSG:4326, value 1 = loss, NA elsewhere
#             (output of rasters_change()). NULL when only one year uploaded.
#   years  — integer vector of all assessment years
#' Title
#'
#' @param r_map 
#' @param r_loss 
#' @param years 
#'
#' @return
#' @export
#'
#' @examples
make_fig_loss <- function(r_map, r_loss = NULL, years) {
  
  df_latest <- as.data.frame(r_map, xy = TRUE) |> stats::na.omit()
  
  # buffer derived from map aspect ratio
  x_range         <- range(df_latest$x, na.rm = TRUE)
  y_range         <- range(df_latest$y, na.rm = TRUE)
  aspect_ratio    <- diff(x_range) / diff(y_range)
  scalebar_buffer <- min(0.15, max(0.05, 0.08 / sqrt(aspect_ratio)))
  y_buffer        <- diff(y_range) * scalebar_buffer
  
  # the plot
  p <- ggplot2::ggplot() +
    # ggspatial::annotation_map_tile(zoomin=0) +
    ggplot2::geom_raster(data = df_latest,
                         ggplot2::aes(x = x, y = y), fill = "black") +
    # ggplot2::coord_sf(crs = 4326) +
    # Scale bar — positioned in the buffer zone below the map data
    ggspatial::annotation_scale(
      location   = "bl",
      width_hint = 0.25,
      pad_x      = ggplot2::unit(0.3, "cm"),
      pad_y      = ggplot2::unit(0.15, "cm"),
      text_cex   = 0.8
    ) +
    # ggplot2::coord_sf(
    #   ylim = c(y_range[1] - y_buffer, y_range[2])  # extra space at bottom
    # ) +
    ggplot2::coord_sf(
      # xlim   = range(df_latest$x),
      ylim   = c(y_range[1] - y_buffer, y_range[2]),
      crs    = sf::st_crs(4326)
    )
  ggplot2::labs(x = NULL, y = NULL)+#,
               # caption = "Generated with EcoCAT Assessment tool") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.margin = grid::unit(c(0.2, 0, 0, 0), "mm"))
  
  if (!is.null(r_loss)) {
    # add in loss part of the map too
    df_loss <- as.data.frame(r_loss, xy = TRUE) |> stats::na.omit()
    if (nrow(df_loss) > 0) {
      p <- p +
        ggplot2::geom_raster(data = df_loss,
                             ggplot2::aes(x = x, y = y), fill = "#d73027") +
        ggplot2::labs(x = NULL, y = NULL,
          title    = "Ecosystem distribution and loss",
          subtitle = paste0(min(years), "\u2013", max(years),
                            "  |  Black = present  \u2022  Red = lost")
        )
    }
  } else {
    p <- p + ggplot2::labs(
      title    = "Ecosystem distribution",
      subtitle = paste0("Year: ", max(years, na.rm = TRUE)),
      alt = "A map of the ecosystem showing the most recent distribution"
    )
  }
  p
}



# make distribution map ---------------------------------------------------
#' Create a static distribution map of the ecosystem with loss overlay if
#'  > 1 year of inout data provided
#'
#' @param r_map `SpatRaster`. The ecosystem distribution for the most
#'   recent mapped year. Non-NA cells are treated as present. Any CRS;
#'   reprojected internally to EPSG:4326.
#' @param r_loss `SpatRaster` or `NULL`. Cells lost between the earliest
#'   and latest mapped years. `NULL` (the default) draws the distribution
#'   alone and omits the loss entry from the legend.
#' @param years Numeric vector of mapped years. Used for the loss legend
#'   label; only the minimum and maximum are used.
#' @param world `sf` object of country polygons, e.g. from
#'   [rnaturalearth::ne_countries()]. Cropped internally to the map frame.
#' @param basemap `SpatRaster` or `NULL`. Single-band shaded relief
#'   (e.g. Natural Earth `SR_HR`), rendered as pale greys behind the
#'   data. `NULL` (the default) draws a flat land fill instead.
#' @param rivers `sf` object of river centrelines (from Natural Earth), or `NULL` to omit.
#' @param lakes `sf` object of lake polygons (from Natural Earth), or `NULL` to omit.
#' @param pad Numeric. Margin of context around the mapped data, as a
#'   proportion of the larger of its width and height. Defaults to 0.15.
#'
#' @return a `ggplot` object
#' @export
#'
#' @examples
make_fig_distribution <- function(r_map, r_loss = NULL, years,
                                  world, basemap = NULL, rivers = NULL,
                                  lakes = NULL, pad = 0.15) {
  
  # ---------------------------------------------------------------------------  
  # Rasters must be in lon/lat before becoming data frames: a data frame
  # carries no CRS. "near" keeps the classes intact.
  # to_df <- function(r) {
  #   if (is.null(r)) return(NULL)
  #   dat <- stats::na.omit(as.data.frame(
  #     terra::project(r, "EPSG:4326", method = "near"), xy = TRUE))
  #   if (nrow(dat) == 0) NULL else dat
  # }
  # df_latest  <- to_df(r_map)
  # df_loss <- to_df(r_loss)
  
  df_latest <- as.data.frame(r_map, xy = TRUE) |> stats::na.omit()
  if (!is.null(r_loss)) {
    # add in loss part of the map too
    df_loss <- as.data.frame(r_loss, xy = TRUE) |> stats::na.omit()
  } else {
    df_loss <- NULL
  }
  res_map   <- terra::res(r_map)
  # r_map4326 <- terra::project(r_map, "EPSG:4326", method = "near")
  # if (terra::ncell(r_map4326) > 5e5) {
  #   f <- ceiling(sqrt(terra::ncell(r_map4326) / 5e5))
  #   r_map4326 <- terra::aggregate(r_map4326, fact = f, fun = "modal", na.rm = TRUE)
  # }
  # df_latest <- stats::na.omit(as.data.frame(r_map4326, xy = TRUE))
  # res_map   <- terra::res(r_map4326)
  # 
  # if (!is.null(r_loss)) {
  #   r_loss4326 <- terra::project(r_loss, "EPSG:4326", method = "near")
  #   df_loss    <- stats::na.omit(as.data.frame(r_loss4326, xy = TRUE))
  #   res_loss   <- terra::res(r_loss4326)
  # } else {
  #   df_loss <- NULL
  # }

# Frame / extent ----------------------------------------------------------
#     the mapped ecosystem + a margin of context + bboudning box for cropping
  
  xs <- c(df_latest$x, df_loss$x)
  ys <- c(df_latest$y, df_loss$y)
  m  <- max(diff(range(xs)), diff(range(ys))) * pad
  xlim <- range(xs) + c(-m, m)
  ylim <- range(ys) + c(-m, m)
  
  bb <- sf::st_bbox(c(xmin = xlim[1], xmax = xlim[2],
                      ymin = ylim[1], ymax = ylim[2]), crs = 4326)
  
# labels ------------------------------------------------------------------
  lab_ecosystem  <- "Mapped ecosystem"
  lab_loss <- paste0("Lost ", min(years), "\u2013", max(years))
  
# start the plot ----------------------------------------------------------
  p <- ggplot2::ggplot()

# relief background -------------------------------------------------------
  land <- sf::st_crop(world, bb)
  if (!is.null(basemap)) {
    bg   <- terra::mask(
      terra::crop(basemap, terra::ext(xlim[1], xlim[2], ylim[1], ylim[2])),
      terra::vect(land))
    sea <- grDevices::col2rgb("#cfe2ec")[, 1]
    g   <- round(150 + 95 * bg / 255)
    bg  <- c(terra::ifel(is.na(g), sea[1], g),
             terra::ifel(is.na(g), sea[2], g),
             terra::ifel(is.na(g), sea[3], g))
    names(bg) <- c("red", "green", "blue")
    
    p <- p + ggspatial::layer_spatial(bg) +
      ggplot2::geom_sf(data = land, fill = NA, colour = "grey45", linewidth = 0.3)
  } else {
    p <- p + ggplot2::geom_sf(data = land, fill = "#e8e4dc",
                              colour = "#b9b2a6", linewidth = 0.3)
  }
  

# add lakes ---------------------------------------------------------------
  if (!is.null(lakes)) {
    lakes.bb <- sf::st_crop(lakes, bb)
    p <- p + ggplot2::geom_sf(data = lakes.bb, fill = "#cfe2ec",
                              colour = "grey55", linewidth = 0.2)
  }

# add rivers  -------------------------------------------------------------
  if (!is.null(rivers)) {
    rivers.bb <- sf::st_crop(rivers, bb)
    if (nrow(rivers.bb) > 0) {
      p <- p + ggplot2::geom_sf(data = rivers.bb, colour = "#8fb8cc",
                                linewidth = 0.35)
    }
  }

# add ecosystem map -------------------------------------------------------
  p <- p + ggplot2::geom_tile(data = df_latest,
                              ggplot2::aes(x, y, fill = lab_ecosystem),
                              width = res_map[1], height = res_map[2])

# add ecosystem loss map --------------------------------------------------
#   (optional)
  if (!is.null(df_loss)) {
    p <- p + ggplot2::geom_raster(data = df_loss,
                                  ggplot2::aes(x, y, fill = lab_loss))
  }
  
# add legend / annotations / theme ----------------------------------------
  p + ggplot2::scale_fill_manual(
    name   = NULL,
    breaks = if (is.null(df_loss)) lab_ecosystem else c(lab_ecosystem, lab_loss),
    values = stats::setNames(c("#1a1a1a", "#d73027"), c(lab_ecosystem, lab_loss))) +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.25,
                                text_cex = 0.8) +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, crs = sf::st_crs(4326),
                      expand = FALSE) +
    scale_x_continuous(
      guide = guide_axis(check.overlap = TRUE)
    ) +
    ggplot2::labs(
      title = if (is.null(df_loss)) "Ecosystem distribution"
      else "Ecosystem distribution and loss",
      x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "#cfe2ec", colour = NA),
      panel.border     = ggplot2::element_rect(fill = NA, colour = "grey60"),
      panel.grid       = ggplot2::element_line(colour = "grey90", linewidth = 0.2),
      legend.position  = "bottom")
}


# -- Interactive (leaflet) -----------------------------------------------------
#   r_map  — latest sampled SpatRaster in EPSG:4326
#   r_loss — loss SpatRaster in EPSG:4326, value 1 = loss. NULL if single year.
#   years  — integer vector
make_fig_distribution_leaflet <- function(r_map, r_loss = NULL, years,
                                          base_provider = "OpenStreetMap",
                                          base_label    = "OSM",
                                          draw_tools    = FALSE) {

  
  # Guard against NULL during upload reset
  if (is.null(r_map)) {
    return(
      leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet.extras::addDrawToolbar(
          targetGroup         = "drawn",
          polylineOptions     = FALSE, circleOptions = FALSE,
          markerOptions       = FALSE, circleMarkerOptions = FALSE,
          rectangleOptions    = leaflet.extras::drawRectangleOptions(repeatMode = TRUE),
          polygonOptions      = leaflet.extras::drawPolygonOptions(repeatMode  = TRUE),
          editOptions         = leaflet.extras::editToolbarOptions()
        )
    )
  }
  
  
  latest_year <- max(years, na.rm = TRUE)
  class_label <- paste0(latest_year, " classification")
  
  # map_stars  <- stars::st_as_stars(r_map)
  map_stars <- stars::st_as_stars(terra::project(r_map, "EPSG:3857", method = "near"))
  ext_r      <- as.vector(terra::ext(r_map))
  
    m <- leaflet::leaflet() |>
      leaflet::addProviderTiles(base_provider, group = base_label) |>
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery,
                                group = "ESRI Satellite") |>
    
      leafem::addStarsImage(map_stars, opacity = 0.7, colors = "#000000",
                          group = class_label, layerId = class_label,
                          project = FALSE) |>
    leaflet::fitBounds(unname(ext_r["xmin"]), unname(ext_r["ymin"]),
                       unname(ext_r["xmax"]), unname(ext_r["ymax"]))
  
  overlay_groups <- class_label
  
  if (!is.null(r_loss)) {
    loss_label <- paste0(min(years, na.rm = TRUE), "\u2013",
                         max(years, na.rm = TRUE), " loss")
    # loss_stars <- stars::st_as_stars(r_loss)
    loss_stars <- stars::st_as_stars(terra::project(r_loss, "EPSG:3857", method = "near"))
    loss_pal   <- leaflet::colorFactor(palette = "#d73027", domain = 1,
                                       na.color = "transparent")
    
    m <- m |>
      leafem::addStarsImage(loss_stars, opacity = 0.85, colors = loss_pal,
                            group = loss_label, layerId = "loss_map",
                            project = FALSE) |>
      leaflet::addLegend(position = "bottomright",
                         colors   = "#d73027", labels = "Loss",
                         title    = loss_label, opacity = 0.85,
                         group    = loss_label, layerId = "loss_legend")
    
    overlay_groups <- c(class_label, loss_label)
  }
  
  m |>
    leaflet::addLayersControl(
      # baseGroups    = c("OSM", "ESRI Satellite"),
      baseGroups = c(base_label, "ESRI Satellite"),
      overlayGroups = overlay_groups,
      options       = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    leaflet.extras::addDrawToolbar(
      targetGroup         = "drawn",
      polylineOptions     = FALSE,
      circleOptions       = FALSE,
      markerOptions       = FALSE,
      circleMarkerOptions = FALSE,
      rectangleOptions    = leaflet.extras::drawRectangleOptions(repeatMode = FALSE),
      polygonOptions      = leaflet.extras::drawPolygonOptions(repeatMode  = FALSE),
      editOptions         = leaflet.extras::editToolbarOptions()
    )
  #leaflet::hideGroup(overlay_groups[overlay_groups != class_label])
}



# =============================================================================
# 2.  CRITERION A — area change trend
# =============================================================================

# Internal: gap-filter projected rows that fall between observed years
.filter_gap <- function(data, obs_years) {
  dplyr::filter(data,
                !(type == "projected" &
                    year > min(obs_years) & year < max(obs_years))
  )
}

# # Static (ggplot2) ----------------------------------------------------------
make_fig_a <- function(area_prediction_data, is_bound = FALSE) {
  
  # 1. Check for data completeness  -----------------------------------------
  #    (i.e., deal with single file upload 
  required_cols <- c("year", "area", "type", "method", "segment", "scenario")
  if (is.null(area_prediction_data) ||
      nrow(area_prediction_data) == 0 ||
      !all(required_cols %in% names(area_prediction_data))) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }
  
  
  # 2. Past projection identical for 'recent' and 'median' scenario ---------
  #    both use median rate - remove "recent" past rows to avoid duplicate overlapping lines
  area_prediction_data <- dplyr::filter(
    area_prediction_data, !(segment == "past" & scenario == "recent")
  )
  
  # 3. legend entries ----------------------------------------------------------
  #    observed data needs own legend entryadd it to the colour scale
  #    override.aes keeps the point on the Observed key only
  colour_scale <- ggplot2::scale_colour_manual(
    values = c(Observed = "grey20", fig_cols),
    breaks = c("Observed", "ARC", "PRC"),
    guide  = ggplot2::guide_legend(
      order = 1,
      override.aes = list(shape     = c(16, NA, NA),
                          linewidth = c(1.2, 1, 1),
                          linetype  = "solid")))
  
  linetype_scale <- ggplot2::scale_linetype_manual(
    values = fig_linetype,
    labels = c("recent" = "Near-term", "median" = "Long-term"),
    guide  = ggplot2::guide_legend(order = 2))

  # 4. plot labels ----------------------------------------------------------
  plot_labs <- ggplot2::labs(x = "Year", y = expression("Area (km"^2*")"),
                             colour = "Method", linetype = "Scenario",
                             title    = paste0("Criterion A \u2014 Change in geographic distribution"),
                             alt = "A line graph showing the temporal change in ecosystem distribution. There are lines for the observed period, and for 2 calculated rates - PRC
                             (proportional rate of change) and ARC (absolute rate of change). For each rate there are projected scenarios too - 'long-term' (using mean rate of observed period)
                             and for any future projections there are 'most-recent' scenarios"
  )
  
  # 5. plot ribbon for upper and lower bounds (optional) --------------------
  #    if data has upper ("max") and lower ("min") bounds add a ribbon
  if (is_bound) {
    obs_years <- dplyr::filter(area_prediction_data,
                               Bound == "opt", type == "observed")$year
    plot_df   <- .filter_gap(
      dplyr::filter(area_prediction_data, Bound == "opt"), obs_years)
    
    obs_ribbon <- dplyr::inner_join(
      dplyr::filter(area_prediction_data, Bound == "min", type == "observed") |>
        dplyr::select(year, ymin = area),
      dplyr::filter(area_prediction_data, Bound == "max", type == "observed") |>
        dplyr::select(year, ymax = area),
      by = "year"
    )
    
    proj_ribbon <- purrr::map_dfr(c("PRC", "ARC"), function(m) {
      upper <- dplyr::filter(area_prediction_data, Bound == "max",
                             type == "projected", method == m) |>
        .filter_gap(obs_years) |>
        dplyr::group_by(year) |>
        dplyr::summarise(ymax = max(area), .groups = "drop")
      lower <- dplyr::filter(area_prediction_data, Bound == "min",
                             type == "projected", method == m) |>
        .filter_gap(obs_years) |>
        dplyr::group_by(year) |>
        dplyr::summarise(ymin = min(area), .groups = "drop")
      dplyr::inner_join(upper, lower, by = "year") |>
        dplyr::mutate(method = m)
    })
    
    p <- ggplot2::ggplot() +
      ggplot2::geom_ribbon(data = obs_ribbon,
                           ggplot2::aes(x = year, ymin = ymin, ymax = ymax),
                           fill = "grey70", alpha = 0.3) +
      ggplot2::geom_ribbon(data = proj_ribbon,
                           ggplot2::aes(x = year, ymin = ymin, ymax = ymax, fill = method),
                           alpha = 0.12) +
      ggplot2::scale_fill_manual(values = fig_cols, guide = "none")
    
  } else {
    obs_years <- area_prediction_data$year[area_prediction_data$type == "observed"]
    plot_df   <- .filter_gap(area_prediction_data, obs_years)
    p <- ggplot2::ggplot()
  }
  
  # 6. add all lines and points ---------------------------------------------
  p <- p +
    ggplot2::geom_line(
      data = dplyr::filter(plot_df, type == "observed"),
      ggplot2::aes(x = year, y = area, colour = "Observed"),
      linewidth = 1.2) +
    ggplot2::geom_point(
      data = dplyr::filter(plot_df, type == "observed"),
      ggplot2::aes(x = year, y = area, colour = "Observed"),
      size = 3) +
    ggplot2::geom_line(
      data = dplyr::filter(plot_df, type == "projected"),
      ggplot2::aes(x = year, y = area, colour = method, linetype = scenario,
                   group = interaction(method, scenario, segment)),
      linewidth = 1)
  
  # 7. add legends, labels and theme ----------------------------------------
  p <- p +
    colour_scale +
    linetype_scale +
    plot_labs +
    ggplot2::theme_bw(base_size = 13)
  
  p
}


# Interactive (plotly) ----------------------------------------------------------
#   show_ribbon — only relevant when is_bound = TRUE
make_fig_a_plotly <- function(area_prediction_data,
                              is_bound    = FALSE,
                              show_ribbon = TRUE) {
  
  # Guard: empty or missing required columns (e.g. single file uploaded)
  required_cols <- c("year", "area", "type", "method", "segment", "scenario")
  if (is.null(area_prediction_data) ||
      nrow(area_prediction_data) == 0 ||
      !all(required_cols %in% names(area_prediction_data))) {
    return(plotly::plot_ly() |>
             plotly::layout(
               xaxis = list(title = "Year"),
               yaxis = list(title = "Area (km\u00b2)"),
               annotations = list(list(
                 text = "Upload at least 2 years of data to view trend",
                 x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                 showarrow = FALSE, font = list(size = 14, color = "#888")
               ))
             ))
  }
  
  # Past projection is identical for recent and median — remove "recent" past
  area_prediction_data <- dplyr::filter(
    area_prediction_data, !(segment == "past" & scenario == "recent")
  )
  
  # # Check if near-term and long-term futures actually differ (>2 data points)
  # # With 2 data points both scenarios produce identical areas — show one trace
  # multi_scenario <- {
  #   fut <- dplyr::filter(area_prediction_data, type == "projected", segment == "future")
  #   scens <- unique(fut$scenario)
  #   if (length(scens) <= 1) FALSE
  #   else {
  #     r <- dplyr::filter(fut, scenario == scens[1]) |> dplyr::arrange(year, method)
  #     m <- dplyr::filter(fut, scenario == scens[2]) |> dplyr::arrange(year, method)
  #     !(nrow(r) == nrow(m) && all(abs(r$area - m$area) < 1e-6))
  #   }
  # }
  # 
  # active_scenarios <- if (multi_scenario) {
  #   unique(dplyr::filter(area_prediction_data, type == "projected")$scenario)
  # } else {
  #   unique(dplyr::filter(area_prediction_data, type == "projected")$scenario)[1]
  # }
  # 
  # trace_label <- function(scen, meth) {
  #   if (multi_scenario) paste(scenario_label[scen], meth) else meth
  # }
  
  dash_map   <- c(observed = "solid", recent = "dash", median = "dot")
  colour_map <- c(observed = "black", ARC = fig_cols[["ARC"]],
                  PRC = fig_cols[["PRC"]])
  
  p <- plotly::plot_ly()
  
  if (is_bound) {
    obs_years <- dplyr::filter(area_prediction_data,
                               Bound == "opt", type == "observed")$year
    obs_max   <- dplyr::filter(area_prediction_data,
                               Bound == "max", type == "observed") |>
      dplyr::arrange(year)
    obs_min   <- dplyr::filter(area_prediction_data,
                               Bound == "min", type == "observed") |>
      dplyr::arrange(year)
    
    if (show_ribbon) {
      # Observed grey ribbon
      p <- p |>
        plotly::add_trace(data = obs_max, x = ~year, y = ~area,
                          type = "scatter", mode = "lines",
                          line = list(color = "rgba(0,0,0,0)"),
                          showlegend = FALSE, hoverinfo = "none", name = "_obs_upper") |>
        plotly::add_trace(data = obs_min, x = ~year, y = ~area,
                          type = "scatter", mode = "lines",
                          fill = "tonexty", fillcolor = "rgba(100,100,100,0.15)",
                          line = list(color = "rgba(0,0,0,0)"),
                          name = "Min-Max range (observed)", hoverinfo = "none")
      
      # Projected ribbons — one per method × scenario (4 total)
      last_obs_max  <- dplyr::slice(obs_max, dplyr::n())
      last_obs_min  <- dplyr::slice(obs_min, dplyr::n())
      first_obs_max <- dplyr::slice(obs_max, 1)
      first_obs_min <- dplyr::slice(obs_min, 1)
      
      for (m in c("PRC", "ARC")) {
        for (s in c("recent", "median")) {
          fill_col  <- fig_ribbon_fill[[paste0(m, ".", s)]]
          ribbon_nm <- paste0(scenario_label[s], " ", m, " range")
          grp_nm    <- paste0("proj_ribbon_", m, "_", s)
          
          fut_upper <- dplyr::filter(area_prediction_data,
                                     Bound == "max", type == "projected",
                                     method == m, scenario == s, segment == "future") |>
            dplyr::bind_rows(last_obs_max |> dplyr::select(year, area)) |>
            dplyr::arrange(year)
          fut_lower <- dplyr::filter(area_prediction_data,
                                     Bound == "min", type == "projected",
                                     method == m, scenario == s, segment == "future") |>
            dplyr::bind_rows(last_obs_min |> dplyr::select(year, area)) |>
            dplyr::arrange(year)
          
          if (nrow(fut_upper) > 1 && nrow(fut_lower) > 1) {
            p <- p |>
              plotly::add_trace(data = fut_upper, x = ~year, y = ~area,
                                type = "scatter", mode = "lines",
                                line = list(color = "rgba(0,0,0,0)"),
                                showlegend = FALSE, hoverinfo = "none",
                                name = paste0("_fut_upper_", m, "_", s)) |>
              plotly::add_trace(data = fut_lower, x = ~year, y = ~area,
                                type = "scatter", mode = "lines",
                                fill = "tonexty", fillcolor = fill_col,
                                line = list(color = "rgba(0,0,0,0)"),
                                name = ribbon_nm, legendgroup = grp_nm,
                                showlegend = TRUE, hoverinfo = "none")
          }
          
          past_upper <- dplyr::filter(area_prediction_data,
                                      Bound == "max", type == "projected",
                                      method == m, scenario == s, segment == "past") |>
            dplyr::bind_rows(first_obs_max |> dplyr::select(year, area)) |>
            dplyr::arrange(year)
          past_lower <- dplyr::filter(area_prediction_data,
                                      Bound == "min", type == "projected",
                                      method == m, scenario == s, segment == "past") |>
            dplyr::bind_rows(first_obs_min |> dplyr::select(year, area)) |>
            dplyr::arrange(year)
          
          if (nrow(past_upper) > 1 && nrow(past_lower) > 1) {
            p <- p |>
              plotly::add_trace(data = past_upper, x = ~year, y = ~area,
                                type = "scatter", mode = "lines",
                                line = list(color = "rgba(0,0,0,0)"),
                                showlegend = FALSE, hoverinfo = "none",
                                name = paste0("_past_upper_", m, "_", s)) |>
              plotly::add_trace(data = past_lower, x = ~year, y = ~area,
                                type = "scatter", mode = "lines",
                                fill = "tonexty", fillcolor = fill_col,
                                line = list(color = "rgba(0,0,0,0)"),
                                name = paste0(ribbon_nm, " (past)"),
                                legendgroup = grp_nm, showlegend = FALSE, hoverinfo = "none")
          }
        }
      }
    } # end show_ribbon
    
    # Opt lines (observed + projected, gap-filtered)
    plot_opt <- dplyr::filter(area_prediction_data, Bound == "opt") |>
      .filter_gap(obs_years) |>
      dplyr::mutate(group = interaction(method, scenario, segment))
    
    for (grp in unique(plot_opt$group)) {
      d <- dplyr::filter(plot_opt, group == grp)
      is_obs <- unique(d$type) == "observed"
      p <- p |>
        plotly::add_trace(data = d, x = ~year, y = ~area,
                          type = "scatter",
                          mode = if (is_obs) "lines+markers" else "lines",
                          line = list(
                            color = if (is_obs) "black" else colour_map[unique(d$method)],
                            dash  = dash_map[unique(d$scenario)],
                            width = if (is_obs) 2 else 1.5),
                          name        = if (is_obs) "Observed (opt)"
                          else paste(scenario_label[unique(d$scenario)], unique(d$method)),
                          legendgroup = if (is_obs) "observed"
                          else paste(scenario_label[unique(d$scenario)], unique(d$method)),
                          showlegend  = unique(d$segment) != "past")
    }
    
  } else {
    #  Standard single-layer path -----------------------------------------
    obs_years <- area_prediction_data$year[area_prediction_data$type == "observed"]
    plot_data <- .filter_gap(area_prediction_data, obs_years) |>
      dplyr::mutate(group = interaction(method, scenario, segment))
    
    for (grp in unique(plot_data$group)) {
      d <- dplyr::filter(plot_data, group == grp)
      is_obs <- unique(d$type) == "observed"
      p <- p |>
        plotly::add_trace(data = d, x = ~year, y = ~area,
                          type = "scatter",
                          mode = if (is_obs) "lines+markers" else "lines",
                          line = list(
                            color = if (is_obs) "black" else colour_map[unique(d$method)],
                            dash  = dash_map[unique(d$scenario)],
                            width = if (is_obs) 2 else 1.5),
                          name        = if (is_obs) "Observed"
                          else paste(scenario_label[unique(d$scenario)], unique(d$method)),
                          legendgroup = if (is_obs) "observed"
                          else paste(scenario_label[unique(d$scenario)], unique(d$method)),
                          showlegend  = unique(d$segment) != "past")
    }
  }
  
  p |>
    plotly::layout(
      xaxis  = list(title = "Year"),
      yaxis  = list(title = "Area (km\u00b2)"),
      legend = list(title = list(text = "<b>Trend</b>")),
      font   = list(family = "Roboto", size = 14, color = "#333333")
    )
}


# =============================================================================
# 3.  CRITERION B1 — EOO map
# =============================================================================

# Static (ggplot2) ----------------------------------------------------------
make_fig_b1 <- function(mcp_poly_list, rasters_list, years,
                        context       = "country",  # "tight" | "country"
                        world,
                        pad_frac      = 0.12,
                        lakes         = NULL,
                        rivers         = NULL,
                        max_expand    = 25,         # cap on framing to host country
                        basemap       = NULL,   # "none" | "relief"
                        sea_col       = "#cfe2ec",
                        max_cells     = 2e6) {
  
  # Polygon for the latest year (list is in chronological order)
  latest_year   <- max(years, na.rm = TRUE)
  latest_yr_str <- as.character(latest_year)
  latest_mcp <- if (latest_yr_str %in% names(mcp_poly_list)) {
    mcp_poly_list[[latest_yr_str]]
  } else {
    mcp_poly_list[[length(mcp_poly_list)]]
  }
  
  eoo_sf   <- sf::st_as_sf(terra::project(latest_mcp, "EPSG:4326"))
  eoo_area <- round(as.data.frame(latest_mcp)$area_km2, 1)
  
  eoo_assessment <- rle_status_eoo(eoo_area)   # helpers/rle_assessment.R
  
  r_4326 <- terra::project(rasters_list[[length(rasters_list)]],
                           "EPSG:4326", method = "near")
  if (terra::ncell(r_4326) > max_cells) {
    r_4326 <- terra::spatSample(r_4326, max_cells, method = "regular",
                                as.raster = TRUE)
  }
  raster_df <- as.data.frame(r_4326, xy = TRUE) |> stats::na.omit()
  
  # world <- countries.sf
  # ----------------------------------------------------------------------------
  #  Map extent
  # "country" frames on whatever country the EOO sits in, so the land mass is
  # recognisable. Falls back to a padded EOO if that country is so large the
  # ecosystem would vanish (max_expand = ratio of framed area to EOO area).
  
  bb_eoo <- sf::st_bbox(eoo_sf)
  pad    <- max(as.numeric(bb_eoo["xmax"] - bb_eoo["xmin"]),
                as.numeric(bb_eoo["ymax"] - bb_eoo["ymin"])) * pad_frac
  bb_use <- bb_eoo + c(-pad, -pad, pad, pad)
  
  host <- world[lengths(sf::st_intersects(world, eoo_sf)) > 0, ]
  
  if (context == "country" && nrow(host) > 0) {
    bb_host <- sf::st_bbox(host)
    area_of <- function(b) as.numeric(b["xmax"] - b["xmin"]) *
      as.numeric(b["ymax"] - b["ymin"])
    if (area_of(bb_host) / area_of(bb_eoo) <= max_expand) {
      hp     <- max(as.numeric(bb_host["xmax"] - bb_host["xmin"]),
                    as.numeric(bb_host["ymax"] - bb_host["ymin"])) * 0.05
      bb_use <- bb_host + c(-hp, -hp, hp, hp)
    }
  }
  
  xlim <- c(bb_use[["xmin"]], bb_use[["xmax"]])
  ylim <- c(bb_use[["ymin"]], bb_use[["ymax"]])
  crop_box <- sf::st_bbox(c(xmin = xlim[1], xmax = xlim[2],
                            ymin = ylim[1], ymax = ylim[2]), crs = 4326)
  
  land <- suppressWarnings(sf::st_crop(world, crop_box))

  # ----------------------------------------------------------------------------
  # Main panel
  p <- ggplot2::ggplot()
  
  # if relief?
  if (!is.null(basemap)) {
    bg  <- terra::mask(
      terra::crop(basemap, terra::ext(xlim[1], xlim[2], ylim[1], ylim[2])),
      terra::vect(land))
    sea <- grDevices::col2rgb(sea_col)[, 1]
    g   <- round(150 + 95 * bg / 255)
    bg  <- c(terra::ifel(is.na(g), sea[1], g),
             terra::ifel(is.na(g), sea[2], g),
             terra::ifel(is.na(g), sea[3], g))
    names(bg) <- c("red", "green", "blue")
    
    p <- p + ggspatial::layer_spatial(bg) +
      ggplot2::geom_sf(data = land, fill = NA, colour = "grey45", linewidth = 0.3)
  } else {
    p <- p + ggplot2::geom_sf(data = land, fill = "#e8e4dc",
                              colour = "#b9b2a6", linewidth = 0.3)
  }
  
  # add lakes layer
  if (!is.null(lakes)) {
    lk <- sf::st_crop(lakes, crop_box)
    if (nrow(lk) > 0) {
      p <- p + ggplot2::geom_sf(data = lk, fill = sea_col,
                                colour = "grey55", linewidth = 0.2)
    }
  }
  
  # add rivers layer
  if (!is.null(rivers)) {
    rv <- sf::st_crop(rivers, crop_box)
    if (nrow(rv) > 0) {
      p <- p + ggplot2::geom_sf(data = rv, colour = "#8fb8cc", linewidth = 0.35)
    }
  }


  
 
  # ----------------------------------------------------------------------------
  # Main panel
  p +
    ggplot2::geom_raster(data = raster_df,
                         ggplot2::aes(x = x, y = y, fill = "Mapped ecosystem")) +
    
    ggplot2::geom_sf(data = eoo_sf,
                     ggplot2::aes(fill = "Extent of Occurrence (EOO)"),
                     alpha = 0.15, colour = "#8B2323", linewidth = 0.8) +
    ggspatial::annotation_scale(
      location = "bl", width_hint = 0.25,
      pad_x = ggplot2::unit(0.3, "cm"), pad_y = ggplot2::unit(0.3, "cm"),
      text_cex = 0.8) +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, crs = sf::st_crs(4326),
                      expand = FALSE) +
    ggplot2::scale_x_continuous(n.breaks = 3) +
    ggplot2::scale_y_continuous(n.breaks = 5) +
    ggplot2::scale_fill_manual(
      name   = NULL,
      breaks = c("Mapped ecosystem", "Extent of Occurrence (EOO)"),
      values = c("Mapped ecosystem"           = "#1a1a1a",
                 "Extent of Occurrence (EOO)" = "#8B2323"),
      guide  = ggplot2::guide_legend(
        override.aes = list(alpha  = c(1, 0.3),
                            colour = c(NA, "#8B2323")),
        position = "bottom", direction = "horizontal")) +
    ggplot2::labs(
      # title    = paste0("Criterion B1 \u2014 Extent of Occurrence (", latest_year, ")"),
      # subtitle = paste0("EOO area: ", format(eoo_area, big.mark = ","),
      #                   " km\u00b2  |  Assessment: ", eoo_assessment),
      title    = paste0("Subcriterion B1 \u2014 Extent of Occurrence"),
      subtitle = paste0("EOO area: ", format(round(eoo_area), big.mark = ","), " km\u00b2"),
      x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      # panel.background = ggplot2::element_rect(fill = "#f2f7fa", colour = NA),
      panel.background = ggplot2::element_rect(fill = sea_col, colour = NA),
      panel.border     = ggplot2::element_rect(fill = NA, colour = "grey60",
                                               linewidth = 0.4),
      panel.grid       = ggplot2::element_line(colour = "grey90", linewidth = 0.2),
      axis.text        = ggplot2::element_text(size = 8, colour = "grey30"),
      legend.position  = "bottom",
      plot.title       = ggplot2::element_text(face = "bold"),
      plot.subtitle    = ggplot2::element_text(colour = "grey30"))
  
}



# Interactive (leaflet) -----------------------------------------------------
#   mcp_all     — single combined SpatVector (terra::vect(mcp_poly_list))
#   r_recent    — most-recent SpatRaster (any CRS; projected internally)
#   latest_year — integer scalar
#' Title
#'
#' @param mcp_all 
#' @param r_recent 
#' @param latest_year 
#' @param base_provider 
#' @param base_label 
#' @param draw_tools 
#'
#' @return
#' @export
#'
#' @examples
make_fig_b1_leaflet <- function(mcp_all, r_recent, latest_year,
                                base_provider = "OpenStreetMap",
                                base_label    = "OSM",
                                draw_tools    = FALSE) {
  
  # raster_4326 <- terra::project(r_recent, "EPSG:4326")
  # eoo_sf      <- sf::st_as_sf(
  #   terra::project(mcp_all[mcp_all$year == latest_year, ], "EPSG:4326"))
  # eoo_sf      <- sf::st_as_sf(mcp_all[mcp_all$year == latest_year, ])
  # ext_r <- as.vector(terra::ext(r_recent))
  eoo_sf <- sf::st_as_sf(terra::project(mcp_all[mcp_all$year == latest_year, ],
                                        "EPSG:4326"))
  ext_r  <- as.vector(terra::ext(
    if (terra::is.lonlat(r_recent)) r_recent
    else terra::project(r_recent, "EPSG:4326")))
  
  leaflet::leaflet() |>
    leaflet::addProviderTiles(base_provider, group = base_label) |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery,
                              group = "ESRI Satellite") |>
    leaflet::fitBounds(unname(ext_r["xmin"]), unname(ext_r["ymin"]),
                       unname(ext_r["xmax"]), unname(ext_r["ymax"])) |>
    leaflet::addRasterImage(
      r_recent, opacity = 0.7, colors = "#000000", method= "ngb", project=TRUE,
      group = paste0(latest_year, " classification")
    ) |>
    leaflet::addPolygons(
      data = eoo_sf, fillOpacity = 0.3, color = "#8B2323", weight = 2,
      group = paste0(latest_year, " EOO (MCP)"),
      # label = ~paste0("Year: ", year,
      #                 " | Area: ", round(area_km2, 2), " km\u00b2"),
      highlightOptions = leaflet::highlightOptions(
        bringToFront = TRUE, color = "white", weight = 3)
    ) |>
    leaflet::addLayersControl(
      baseGroups    = c(base_label, "ESRI Satellite"),
      overlayGroups = c(paste0(latest_year, " EOO (MCP)"),
                        paste0(latest_year, " classification")),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
}


# =============================================================================
# 4.  CRITERION B2 — AOO map
# =============================================================================

# Static (ggplot2) ----------------------------------------------------------
#   b2_poly      — SpatVector with $type ("all" / "assessment")
#   rasters_list — list of SpatRasters
#   years        — integer vector
# make_fig_b2 <- function(b2_poly, rasters_list, years) {
#   raster_4326 <- terra::project(rasters_list[[length(rasters_list)]], "EPSG:4326", method= "near")
#   raster_df   <- as.data.frame(raster_4326, xy = TRUE) |> stats::na.omit()
#   cells_sf    <- sf::st_as_sf(b2_poly)
#   dat_all     <- dplyr::filter(cells_sf, type == "all")
#   dat_sel     <- dplyr::filter(cells_sf, type == "assessment")
#   aoo_assessment <- dplyr::case_when(
#     nrow(dat_sel) < 2  ~ "CR",
#     nrow(dat_sel) < 20 ~ "EN",
#     nrow(dat_sel) < 50 ~ "VU",
#     nrow(dat_sel) < 55 ~ "NT", # hard coded currently, could be related to a user input easily.
#     TRUE                 ~ "LC"
#   )
#   
#   # buffer derived from map aspect ratio
#   x_range         <- range(raster_df$x, na.rm = TRUE)
#   y_range         <- range(raster_df$y, na.rm = TRUE)
#   aspect_ratio    <- diff(x_range) / diff(y_range)
#   scalebar_buffer <- min(0.15, max(0.05, 0.08 / sqrt(aspect_ratio)))
#   y_buffer        <- diff(y_range) * scalebar_buffer
#   
#   ggplot2::ggplot() +
#     # ggspatial::annotation_map_tile(zoomin=0) +
#     ggplot2::geom_sf(data = dat_all,
#                      ggplot2::aes(fill = "10 x 10 km grid\n(<1% occupancy)"),
#                      alpha = 0.2, color = "black", linewidth = 0.3) +
#     ggplot2::geom_sf(data = dat_sel,
#                      ggplot2::aes(fill = "AOO\n(>1% occupancy)"),
#                      alpha = 0.7, color = "black", linewidth = 0.3) +
#     ggplot2::geom_raster(data = raster_df,
#                          ggplot2::aes(x = x, y = y)) +
#     # Scale bar — positioned in the buffer zone below the map data
#     ggspatial::annotation_scale(
#       location   = "bl",
#       width_hint = 0.25,
#       pad_x      = ggplot2::unit(0.3, "cm"),
#       pad_y      = ggplot2::unit(0.15, "cm"),
#       text_cex   = 0.8
#     ) +
#     # ggplot2::coord_sf(
#     #   ylim = c(y_range[1] - y_buffer, y_range[2])  # extra space at bottom
#     # ) +
#     ggplot2::coord_sf(
#       xlim   = range(raster_df$x),
#       ylim   = c(y_range[1] - y_buffer, y_range[2]),
#       crs    = sf::st_crs(4326)
#     ) +
#     ggplot2::scale_fill_manual(
#       name   = NULL,
#       values = c("10 x 10 km grid\n(<1% occupancy)" = "black",
#                  "AOO\n(>1% occupancy)"              = "orange"),
#       guide  = ggplot2::guide_legend(
#         override.aes = list(alpha = c(0.2, 0.7), color = "black"),
#         position = "bottom", direction = "horizontal")) +
#     # ggplot2::coord_sf() +
#     ggplot2::labs(
#       title    = paste0("Criterion B2 \u2014 Area of Occupancy (", max(years, na.rm = TRUE), ")"),
#       subtitle = paste0("Total occupied cells: ", nrow(dat_all),
#                         ", cells >1% occupancy (AOO): ", nrow(dat_sel), "  |  Assessment: ", aoo_assessment),
#      # caption  = "Generated with EcoCAT Assessment tool",
#       x = NULL, y = NULL) +
#     ggplot2::theme_minimal(base_size = 12) +
#     ggplot2::theme(plot.margin = grid::unit(c(0.2, 0, 0, 0), "mm")) +
#     theme(
#       panel.background = element_blank(),  # Remove panel background
#       plot.background = element_blank(),   # Remove plot background
#       panel.grid = element_blank()          # Remove grid lines (optional)
#     )
# }


make_fig_b2 <- function(b2_poly, rasters_list, years,
                        context       = "country",  # "tight" | "country"
                        world,
                        pad_frac      = 0.12,
                        lakes         = NULL,
                        rivers         = NULL,
                        max_expand    = 25,         # cap on framing to host country
                        basemap       = NULL,   # "none" | "relief"
                        sea_col       = "#cfe2ec",
                        max_cells     = 2e6) {
  
  latest_year <- max(years, na.rm = TRUE)
  
  # --- AOO grid cells ---------------------------------------------------------
  # Transform explicitly: the grid may be built in an equal-area CRS, and the
  # framing below needs lon/lat coordinates.
  cells_sf <- sf::st_transform(sf::st_as_sf(b2_poly), 4326)
  dat_all  <- dplyr::filter(cells_sf, type == "all")
  dat_sel  <- dplyr::filter(cells_sf, type == "assessment")
  n_aoo    <- nrow(dat_sel)
  
  aoo_assessment <- rle_status_aoo(n_aoo)       # helpers/rle_assessment.R
  
  # --- Ecosystem raster -------------------------------------------------------
  r_4326 <- terra::project(rasters_list[[length(rasters_list)]],
                           "EPSG:4326", method = "near")
  if (terra::ncell(r_4326) > max_cells) {
    r_4326 <- terra::spatSample(r_4326, max_cells, method = "regular",
                                as.raster = TRUE)
  }
  raster_df <- as.data.frame(r_4326, xy = TRUE) |> stats::na.omit()
  
  
  # ----------------------------------------------------------------------------
  # Map extent
  # Frames on the country the grid sits in, falling back to a padded grid
  # extent if that country is so large the ecosystem would vanish.
  
  bb_dat <- sf::st_bbox(dat_all)
  pad    <- max(as.numeric(bb_dat["xmax"] - bb_dat["xmin"]),
                as.numeric(bb_dat["ymax"] - bb_dat["ymin"])) * pad_frac
  bb_use <- bb_dat + c(-pad, -pad, pad, pad)
  
  host <- world[lengths(sf::st_intersects(world, dat_all)) > 0, ]
  
  if (context == "country" && nrow(host) > 0) {
    bb_host <- sf::st_bbox(host)
    area_of <- function(b) as.numeric(b["xmax"] - b["xmin"]) *
      as.numeric(b["ymax"] - b["ymin"])
    if (area_of(bb_host) / area_of(bb_dat) <= max_expand) {
      hp     <- max(as.numeric(bb_host["xmax"] - bb_host["xmin"]),
                    as.numeric(bb_host["ymax"] - bb_host["ymin"])) * 0.05
      bb_use <- bb_host + c(-hp, -hp, hp, hp)
    }
  }
  
  xlim <- c(bb_use[["xmin"]], bb_use[["xmax"]])
  ylim <- c(bb_use[["ymin"]], bb_use[["ymax"]])
  crop_box <- sf::st_bbox(c(xmin = xlim[1], xmax = xlim[2],
                            ymin = ylim[1], ymax = ylim[2]), crs = 4326)
  
  land <- suppressWarnings(sf::st_crop(world, crop_box))
  
  
  # ----------------------------------------------------------------------------
  # Main panel
  p <- ggplot2::ggplot()
  
  # if relief?
  if (!is.null(basemap)) {
    bg  <- terra::mask(
      terra::crop(basemap, terra::ext(xlim[1], xlim[2], ylim[1], ylim[2])),
      terra::vect(land))
    sea <- grDevices::col2rgb(sea_col)[, 1]
    g   <- round(150 + 95 * bg / 255)
    bg  <- c(terra::ifel(is.na(g), sea[1], g),
             terra::ifel(is.na(g), sea[2], g),
             terra::ifel(is.na(g), sea[3], g))
    names(bg) <- c("red", "green", "blue")
    
    p <- p + ggspatial::layer_spatial(bg) +
      ggplot2::geom_sf(data = land, fill = NA, colour = "grey45", linewidth = 0.3)
  } else {
    p <- p + ggplot2::geom_sf(data = land, fill = "#e8e4dc",
                              colour = "#b9b2a6", linewidth = 0.3)
  }
  
  # add lakes layer
  if (!is.null(lakes)) {
    lk <- sf::st_crop(lakes, crop_box)
    if (nrow(lk) > 0) {
      p <- p + ggplot2::geom_sf(data = lk, fill = sea_col,
                                colour = "grey55", linewidth = 0.2)
    }
  }
  
  # add rivers layer
  if (!is.null(rivers)) {
    rv <- sf::st_crop(rivers, crop_box)
    if (nrow(rv) > 0) {
      p <- p + ggplot2::geom_sf(data = rv, colour = "#8fb8cc", linewidth = 0.35)
    }
  }
  
  # ----------------------------------------------------------------------------
  # Legend labels, defined once so breaks, values and aes() stay in step
  lab_eco <- "Mapped ecosystem"
  lab_all <- "Occupied cells\n(<1% occupancy)"
  lab_aoo <- "AOO cells\n(>1% occupancy)"
  
  col_all <- "grey40"
  col_aoo <- "orange"#  "#e6862e"
  
  # ----------------------------------------
  p +
    # cell fills under the ecosystem...
    ggplot2::geom_sf(data = dat_all, ggplot2::aes(fill = lab_all),
                     alpha = 0.2, colour = NA) +
    ggplot2::geom_sf(data = dat_sel, ggplot2::aes(fill = lab_aoo),
                     alpha = 0.6, colour = NA) +
    ggplot2::geom_raster(data = raster_df,
                         ggplot2::aes(x = x, y = y, fill = lab_eco)) +
    # ...and cell outlines over it, so the grid stays visible
    ggplot2::geom_sf(data = dat_all, fill = NA, colour = "grey25",
                     linewidth = 0.15, show.legend = FALSE) +
    ggplot2::geom_sf(data = dat_sel, fill = NA, colour = "grey25",# "#a85a14",
                     linewidth = 0.25, show.legend = FALSE) +
    ggspatial::annotation_scale(
      location = "bl", width_hint = 0.25,
      pad_x = ggplot2::unit(0.3, "cm"), pad_y = ggplot2::unit(0.3, "cm"),
      text_cex = 0.8) +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, crs = sf::st_crs(4326),
                      expand = FALSE) +
    ggplot2::scale_x_continuous(n.breaks = 3) +
    ggplot2::scale_y_continuous(n.breaks = 5) +
    ggplot2::scale_fill_manual(
      name   = NULL,
      breaks = c(lab_eco, lab_all, lab_aoo),
      values = stats::setNames(c("#1a1a1a", col_all, col_aoo),
                               c(lab_eco, lab_all, lab_aoo)),
      guide  = ggplot2::guide_legend(
        override.aes = list(alpha  = c(1, 0.2, 0.6),
                            colour = c(NA, "grey25", "grey25")), #"#a85a14")),
        position = "bottom", direction = "horizontal")) +
    ggplot2::labs(
      # title    = paste0("Subcriterion B2 \u2014 Area of Occupancy (", latest_year, ")"),
      # subtitle = paste0("Occupied cells: ", nrow(dat_all),
      #                   "  |  AOO (>1% occupancy): ", n_aoo,
      #                   "  |  Assessment: ", aoo_assessment),
      title    = paste0("Subcriterion B2 \u2014 Area of Occupancy"),
      subtitle = paste0("Occupied cells: ", nrow(dat_all),
                        "  |  AOO (>1% occupancy): ", n_aoo),
      x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = sea_col, colour = NA),
      panel.border     = ggplot2::element_rect(fill = NA, colour = "grey60",
                                               linewidth = 0.4),
      panel.grid       = ggplot2::element_line(colour = "grey90", linewidth = 0.2),
      axis.text        = ggplot2::element_text(size = 8, colour = "grey30"),
      legend.position  = "bottom",
      plot.title       = ggplot2::element_text(face = "bold"),
      plot.subtitle    = ggplot2::element_text(colour = "grey30"))
  
}


# Interactive (leaflet) -----------------------------------------------------
#   dat_all     — sf with all occupied grid cells (type == "all")
#   dat_sel     — sf with AOO cells >1% occupancy (type == "assessment")
#   r_recent    — most-recent SpatRaster (any CRS; projected internally)
#   latest_year — integer scalar
make_fig_b2_leaflet <- function(dat_all, dat_sel, r_recent, latest_year,
                                base_provider = "OpenStreetMap",
                                base_label    = "OSM",
                                draw_tools    = FALSE) {

 # raster_4326 <- terra::project(r_recent, "EPSG:4326")
  # don't need this part above
  ext_r       <- as.vector(terra::ext(r_recent)) # was raster_4326
  classification_group <- paste0(latest_year, " classification")
  
  # White→black palette with transparent NAs, matching the in-app rendering
  pal <- leaflet::colorNumeric(
    palette  = c("white", "black"),
    domain   = c(0, 1),
    na.color = "transparent"
  )
  
  
  leaflet::leaflet() |>
    # leaflet::addTiles() |>
    leaflet::addProviderTiles(base_provider, group = base_label) |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery,
                              group = "ESRI Satellite") |>
    leaflet::fitBounds(unname(ext_r["xmin"]), unname(ext_r["ymin"]),
                       unname(ext_r["xmax"]), unname(ext_r["ymax"])) |>
    leaflet::addRasterImage(
      r_recent,
      colors  = pal,
      method  = "ngb",
      project = TRUE,           
      opacity = 0.7,
      group   = classification_group
    ) |>
    make_fig_b2_layers(
      dat_all     = dat_all,
      dat_sel     = dat_sel,
      r_recent    = r_recent,
      latest_year = latest_year
    ) |>
    leaflet::addLayersControl(
      baseGroups    = c(base_label, "ESRI Satellite"),
      overlayGroups = c(classification_group,
                        "10x10 km grid",
                        "AOO (>1% occupancy)"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      colors   = c("black", "orange"),
      labels   = c("10x10 km grid", "AOO (>1% occupancy)"),
      opacity  = 0.7,
      title    = "AOO layers"
    )
}


make_fig_b2_layers <- function(map, dat_all, dat_sel, r_recent, latest_year) {
  
  # raster_4326 <- terra::project(r_recent, "EPSG:4326")
  map |>
    leaflet::addPolygons(
      data = dat_all,
      group = "10x10 km grid",
      fillColor = "black",
      fillOpacity = 0.2,
      color = "black",
      weight = 0.5
    ) |>
    leaflet::addPolygons(
      data = dat_sel,
      group = "AOO (>1% occupancy)",
      fillColor = "orange",
      fillOpacity = 0.5,
      color = "black",
      weight = 0.5
    )
}

