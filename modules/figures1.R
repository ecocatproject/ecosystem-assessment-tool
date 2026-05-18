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
#   • script_export.R    — inlined verbatim into the standalone R script
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

# Static (ggplot2) ----------------------------------------------------------
#   r_map  — latest sampled SpatRaster already in EPSG:4326 (output of rasters())
#   r_loss — loss sampled SpatRaster in EPSG:4326, value 1 = loss, NA elsewhere
#             (output of rasters_change()). NULL when only one year uploaded.
#   years  — integer vector of all assessment years
make_fig_loss <- function(r_map, r_loss = NULL, years) {
  df_latest <- as.data.frame(r_map, xy = TRUE) |> stats::na.omit()
  
  # buffer derived from map aspect ratio
  x_range         <- range(df_latest$x, na.rm = TRUE)
  y_range         <- range(df_latest$y, na.rm = TRUE)
  aspect_ratio    <- diff(x_range) / diff(y_range)
  scalebar_buffer <- min(0.15, max(0.05, 0.08 / sqrt(aspect_ratio)))
  y_buffer        <- diff(y_range) * scalebar_buffer
  
  
  
  p <- ggplot2::ggplot() +
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
    ggplot2::coord_sf(
      ylim = c(y_range[1] - y_buffer, y_range[2])  # extra space at bottom
    ) +
    ggplot2::labs(x = NULL, y = NULL,
                  caption = "Generated with EcoCAT Assessment tool") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.margin = grid::unit(c(0.2, 0, 0, 0), "mm"))
  
  if (!is.null(r_loss)) {
    # add in loss part of the map too
    df_loss <- as.data.frame(r_loss, xy = TRUE) |> stats::na.omit()
    if (nrow(df_loss) > 0) {
      p <- p +
        ggplot2::geom_raster(data = df_loss,
                             ggplot2::aes(x = x, y = y), fill = "#d73027") +
        ggplot2::labs(
          title    = "Ecosystem distribution and loss",
          subtitle = paste0(min(years), "\u2013", max(years),
                            "  |  Black = present  \u2022  Red = lost")
        )
    }
  } else {
    p <- p + ggplot2::labs(
      title    = "Ecosystem distribution",
      subtitle = paste0("Year: ", max(years, na.rm = TRUE))
    )
  }
  p
}

# -- Interactive (leaflet) -----------------------------------------------------
#   r_map  — latest sampled SpatRaster in EPSG:4326
#   r_loss — loss SpatRaster in EPSG:4326, value 1 = loss. NULL if single year.
#   years  — integer vector
make_fig_distribution_leaflet <- function(r_map, r_loss = NULL, years) {
  
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
  
  map_stars  <- stars::st_as_stars(r_map)
  ext_r      <- as.vector(terra::ext(r_map))
  
  m <- leaflet::leaflet() |>
    leaflet::addTiles(group = "OSM") |>
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
    loss_stars <- stars::st_as_stars(r_loss)
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
      baseGroups    = c("OSM", "ESRI Satellite"),
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

# Static (ggplot2) ----------------------------------------------------------
#   area_prediction_data — output of predict_area_change()
#   is_bound             — TRUE when data has a Bound column (min/opt/max)
make_fig_a <- function(area_prediction_data, is_bound = FALSE) {
  
  if (is_bound) {
    obs_years  <- dplyr::filter(area_prediction_data,
                                Bound == "opt", type == "observed")$year
    plot_opt   <- .filter_gap(
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
    
    ggplot2::ggplot() +
      ggplot2::geom_ribbon(data = obs_ribbon,
                           ggplot2::aes(x = year, ymin = ymin, ymax = ymax),
                           fill = "grey70", alpha = 0.3) +
      ggplot2::geom_ribbon(data = proj_ribbon,
                           ggplot2::aes(x = year, ymin = ymin, ymax = ymax, fill = method),
                           alpha = 0.12) +
      ggplot2::geom_line(
        data = dplyr::filter(plot_opt, type == "observed"),
        ggplot2::aes(x = year, y = area),
        colour = "grey20", linewidth = 1.2) +
      ggplot2::geom_point(
        data = dplyr::filter(plot_opt, type == "observed"),
        ggplot2::aes(x = year, y = area),
        colour = "grey10", size = 3) +
      ggplot2::geom_line(
        data = dplyr::filter(plot_data, type == "projected"),
        ggplot2::aes(x = year, y = area, colour = method, linetype = scenario,
                     group = interaction(method, scenario, segment)),
        linewidth = 1) +
      ggplot2::scale_colour_manual(values = fig_cols) +
      ggplot2::scale_fill_manual(values = fig_cols, guide = "none") +
      ggplot2::scale_linetype_manual(
        values = fig_linetype,
        labels = c("recent" = "Near-term", "median" = "Long-term")) +
      ggplot2::labs(x = "Year", y = expression("Area (km"^2*")"),
                    colour = "Method", linetype = "Scenario",
                    caption = "Generated with EcoCAT Assessment tool") +
      ggplot2::theme_bw(base_size = 13)
    
  } else {
    obs_years <- area_prediction_data$year[area_prediction_data$type == "observed"]
    plot_data <- .filter_gap(area_prediction_data, obs_years)
    
    ggplot2::ggplot() +
      ggplot2::geom_line(
        data = dplyr::filter(plot_data, type == "observed"),
        ggplot2::aes(x = year, y = area),
        colour = "grey20", linewidth = 1.2) +
      ggplot2::geom_point(
        data = dplyr::filter(plot_data, type == "observed"),
        ggplot2::aes(x = year, y = area),
        colour = "grey10", size = 3) +
      ggplot2::geom_line(
        data = dplyr::filter(plot_data, type == "projected"),
        ggplot2::aes(x = year, y = area, colour = method, linetype = scenario,
                     group = interaction(method, scenario, segment)),
        linewidth = 1) +
      ggplot2::scale_colour_manual(values = fig_cols) +
      ggplot2::scale_linetype_manual(
        values = fig_linetype,
        labels = c("recent" = "Near-term", "median" = "Long-term")) +
      ggplot2::labs(x = "Year", y = expression("Area (km"^2*")"),
                    colour = "Method", linetype = "Scenario",
                    caption = "Generated with EcoCAT Assessment tool") +
      ggplot2::theme_bw(base_size = 13)
  }
}


# Interactive (plotly) ----------------------------------------------------------
#   show_ribbon — only relevant when is_bound = TRUE
make_fig_a_plotly <- function(area_prediction_data,
                              is_bound    = FALSE,
                              show_ribbon = TRUE) {
  
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
#   mcp_poly_list — list of SpatVectors (output of mcp_poly())
#   rasters_list  — list of SpatRasters  (output of rasters())
#   years         — integer vector
# make_fig_b1 <- function(mcp_poly_list, rasters_list, years) {
#   eoo_sf      <- sf::st_as_sf(terra::project(mcp_poly_list[[1]], "EPSG:4326"))
#   raster_4326 <- terra::project(rasters_list[[length(rasters_list)]], "EPSG:4326")
#   raster_df   <- as.data.frame(raster_4326, xy = TRUE) |> stats::na.omit()
#   latest_year <- max(years, na.rm = TRUE)
#   eoo_area    <- round(as.data.frame(mcp_poly_list[[1]])$area_km2, 1)
#   eoo_assessment <- dplyr::case_when(
#     eoo_area < 2000  ~ "CR",
#     eoo_area < 20000 ~ "EN",
#     eoo_area < 50000 ~ "VU",
#     eoo_area < 55000 ~ "NT",
#     TRUE ~ "LC"
#   )
#   
#   ggplot2::ggplot() +
#     ggplot2::geom_raster(data = raster_df,
#                          ggplot2::aes(x = x, y = y), fill = "black") +
#     ggplot2::geom_sf(data = eoo_sf,
#                      ggplot2::aes(fill = "Extent of Occurrence (EOO)"),
#                      alpha = 0.1, colour = "#8B2323", linewidth = 0.8) +
#     ggplot2::scale_fill_manual(
#       name   = NULL,
#       values = c("Extent of Occurrence (EOO)" = "#8B2323"),
#       guide  = ggplot2::guide_legend(
#         override.aes = list(alpha = 0.3, color = "#8B2323"),
#         position = "bottom", direction = "horizontal")) +
#     ggplot2::coord_sf() +
#     ggplot2::labs(
#       title    = paste0("Criterion B1 \u2014 Extent of Occurrence (", latest_year, ")"),
#       # subtitle = paste0("EOO area: ", eoo_area, " km\u00b2"),
#       subtitle = paste0("EOO area: ", round(eoo_area, 1), " km²  |  Assessment: ", eoo_assessment),
#       caption = "Generated with EcoCAT Assessment tool",
#       x = NULL, y = NULL
#     ) +
#     ggplot2::theme_minimal(base_size = 12) +
#     ggplot2::theme(plot.margin = grid::unit(c(0.2, 0, 0, 0), "mm"))
# }

make_fig_b1 <- function(mcp_poly_list, rasters_list, years) {
  eoo_sf      <- sf::st_as_sf(terra::project(mcp_poly_list[[1]], "EPSG:4326"))
  raster_4326 <- terra::project(rasters_list[[length(rasters_list)]], "EPSG:4326")
  raster_df   <- as.data.frame(raster_4326, xy = TRUE) |> stats::na.omit()
  latest_year <- max(years, na.rm = TRUE)
  eoo_area    <- round(as.data.frame(mcp_poly_list[[1]])$area_km2, 1)
  eoo_assessment <- dplyr::case_when(
    eoo_area < 2000  ~ "CR", eoo_area < 20000 ~ "EN",
    eoo_area < 50000 ~ "VU", eoo_area < 55000 ~ "NT", TRUE ~ "LC"
  )
  
  # Buffer below map extent — scale bar sits here, above the legend
  # y_range  <- range(raster_df$y, na.rm = TRUE)
  # y_buffer <- diff(y_range) * 0.08
  
  # buffer derived from map aspect ratio
  x_range         <- range(raster_df$x, na.rm = TRUE)
  y_range         <- range(raster_df$y, na.rm = TRUE)
  aspect_ratio    <- diff(x_range) / diff(y_range)
  scalebar_buffer <- min(0.15, max(0.05, 0.08 / sqrt(aspect_ratio)))
  y_buffer        <- diff(y_range) * scalebar_buffer
  
  
  ggplot2::ggplot() +
    ggplot2::geom_raster(data = raster_df,
                         ggplot2::aes(x = x, y = y), fill = "black") +
    ggplot2::geom_sf(data = eoo_sf,
                     ggplot2::aes(fill = "Extent of Occurrence (EOO)"),
                     alpha = 0.1, colour = "#8B2323", linewidth = 0.8) +
    # # North arrow — top-right, inside panel but away from data
    # ggspatial::annotation_north_arrow(
    #   location    = "br",
    #   which_north = "true",
    #   height      = ggplot2::unit(1.2, "cm"),
    #   width       = ggplot2::unit(1.0, "cm"),
    #   style       = ggspatial::north_arrow_orienteering(text_size = 8)
    # ) +
    # Scale bar — positioned in the buffer zone below the map data
    ggspatial::annotation_scale(
      location   = "bl",
      width_hint = 0.25,
      pad_x      = ggplot2::unit(0.3, "cm"),
      pad_y      = ggplot2::unit(0.15, "cm"),
      text_cex   = 0.8
    ) +
    ggplot2::coord_sf(
      ylim = c(y_range[1] - y_buffer, y_range[2])  # extra space at bottom
    ) +
    ggplot2::scale_fill_manual(
      name   = NULL,
      values = c("Extent of Occurrence (EOO)" = "#8B2323"),
      guide  = ggplot2::guide_legend(
        override.aes = list(alpha = 0.3, color = "#8B2323"),
        position = "bottom", direction = "horizontal")
    ) +
    ggplot2::labs(
      title    = paste0("Criterion B1 \u2014 Extent of Occurrence (", latest_year, ")"),
      subtitle = paste0("EOO area: ", eoo_area, " km²  |  Assessment: ", eoo_assessment),
      caption  = "Generated with EcoCAT Assessment tool",
      x = NULL, y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.margin = grid::unit(c(0.2, 0, 0, 0), "mm"))
}


# Interactive (leaflet) -----------------------------------------------------
#   mcp_all     — single combined SpatVector (terra::vect(mcp_poly_list))
#   r_recent    — most-recent SpatRaster (any CRS; projected internally)
#   latest_year — integer scalar
make_fig_b1_leaflet <- function(mcp_all, r_recent, latest_year) {
  raster_4326 <- terra::project(r_recent, "EPSG:4326")
  eoo_sf      <- sf::st_as_sf(
    terra::project(mcp_all[mcp_all$year == latest_year, ], "EPSG:4326"))
  ext_r <- as.vector(terra::ext(raster_4326))
  
  leaflet::leaflet() |>
    leaflet::addTiles() |>
    leaflet::fitBounds(ext_r["xmin"], ext_r["ymin"],
                       ext_r["xmax"], ext_r["ymax"]) |>
    leaflet::addRasterImage(
      raster_4326, opacity = 0.7, colors = "#000000",
      group = paste0(latest_year, " classification")
    ) |>
    leaflet::addPolygons(
      data = eoo_sf, fillOpacity = 0.3, color = "#8B2323", weight = 2,
      group = paste0(latest_year, " EOO (MCP)"),
      label = ~paste0("Year: ", year,
                      " | Area: ", round(area_km2, 2), " km\u00b2"),
      highlightOptions = leaflet::highlightOptions(
        bringToFront = TRUE, color = "white", weight = 3)
    ) |>
    leaflet::addProviderTiles(
      leaflet::providers$Esri.WorldImagery, group = "ESRI Satellite") |>
    leaflet::addLayersControl(
      baseGroups    = c("OSM", "ESRI Satellite"),
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
make_fig_b2 <- function(b2_poly, rasters_list, years) {
  raster_4326 <- terra::project(rasters_list[[length(rasters_list)]], "EPSG:4326")
  raster_df   <- as.data.frame(raster_4326, xy = TRUE) |> stats::na.omit()
  cells_sf    <- sf::st_as_sf(b2_poly)
  dat_all     <- dplyr::filter(cells_sf, type == "all")
  dat_sel     <- dplyr::filter(cells_sf, type == "assessment")
  aoo_assessment <- dplyr::case_when(
    nrow(dat_sel) < 2  ~ "CR",
    nrow(dat_sel) < 20 ~ "EN",
    nrow(dat_sel) < 50 ~ "VU",
    nrow(dat_sel) < 55 ~ "NT", # hard coded currently, could be related to a user input easily.
    TRUE                 ~ "LC"
  )
  
  # buffer derived from map aspect ratio
  x_range         <- range(raster_df$x, na.rm = TRUE)
  y_range         <- range(raster_df$y, na.rm = TRUE)
  aspect_ratio    <- diff(x_range) / diff(y_range)
  scalebar_buffer <- min(0.15, max(0.05, 0.08 / sqrt(aspect_ratio)))
  y_buffer        <- diff(y_range) * scalebar_buffer
  
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = dat_all,
                     ggplot2::aes(fill = "10 x 10 km grid\n(<1% occupancy)"),
                     alpha = 0.2, color = "black", linewidth = 0.3) +
    ggplot2::geom_sf(data = dat_sel,
                     ggplot2::aes(fill = "AOO\n(>1% occupancy)"),
                     alpha = 0.7, color = "black", linewidth = 0.3) +
    ggplot2::geom_raster(data = raster_df,
                         ggplot2::aes(x = x, y = y)) +
    # Scale bar — positioned in the buffer zone below the map data
    ggspatial::annotation_scale(
      location   = "bl",
      width_hint = 0.25,
      pad_x      = ggplot2::unit(0.3, "cm"),
      pad_y      = ggplot2::unit(0.15, "cm"),
      text_cex   = 0.8
    ) +
    ggplot2::coord_sf(
      ylim = c(y_range[1] - y_buffer, y_range[2])  # extra space at bottom
    ) +
    ggplot2::scale_fill_manual(
      name   = NULL,
      values = c("10 x 10 km grid\n(<1% occupancy)" = "black",
                 "AOO\n(>1% occupancy)"              = "orange"),
      guide  = ggplot2::guide_legend(
        override.aes = list(alpha = c(0.2, 0.7), color = "black"),
        position = "bottom", direction = "horizontal")) +
    ggplot2::coord_sf() +
    ggplot2::labs(
      title    = paste0("Criterion B2 \u2014 Area of Occupancy (", max(years, na.rm = TRUE), ")"),
      subtitle = paste0("Total occupied cells: ", nrow(dat_all),
                        ", cells >1% occupancy (AOO): ", nrow(dat_sel), "  |  Assessment: ", aoo_assessment),
      caption  = "Generated with EcoCAT Assessment tool",
      x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.margin = grid::unit(c(0.2, 0, 0, 0), "mm"))
}


# Interactive (leaflet) -----------------------------------------------------
#   dat_all     — sf with all occupied grid cells (type == "all")
#   dat_sel     — sf with AOO cells >1% occupancy (type == "assessment")
#   r_recent    — most-recent SpatRaster (any CRS; projected internally)
#   latest_year — integer scalar
make_fig_b2_leaflet <- function(dat_all, dat_sel, r_recent, latest_year) {
  
  leaflet::leaflet() |>
    leaflet::addTiles() |>
    make_fig_b2_layers(
      dat_all = dat_all,
      dat_sel = dat_sel,
      r_recent = r_recent,
      latest_year = latest_year
    ) |>
    leaflet::addLayersControl(
      baseGroups = c("OSM", "ESRI Satellite"),
      overlayGroups = c("classification", "10x10 km grid", "AOO (>1% occupancy)"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      colors   = c("black", "orange"),
      labels   = c("10x10 km grid", "AOO (>1% occupancy)"),
      opacity  = 0.7,
      title = "AOO layers"
    )
}


make_fig_b2_layers <- function(map, dat_all, dat_sel, r_recent, latest_year) {
  
  raster_4326 <- terra::project(r_recent, "EPSG:4326")
  
  map |>
    leaflet::addRasterImage(
      raster_4326,
      opacity = 0.7,
      colors = "#000000",
      group = "classification"
    ) |>
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