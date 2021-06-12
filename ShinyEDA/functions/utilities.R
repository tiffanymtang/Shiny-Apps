# script with utility functions for shiny apps

library(tidyverse)


getPlotTypes <- function(data, vars) {
  ##### Function Description #####
  # getPlotTypes: function to return possible plot types given variable inputs
  #   and data (based on # of input variables and # of factor variables)
  # 
  # inputs:
  # - data = data.frame; usual data matrix
  # - vars = character vector of variable names of interest (in data) or "None"
  #     or ""
  # 
  # outputs: named vector of possible graph icons (e.g., boxplot, barplot, 
  #   histogram, density, scatterplot, line)
  #######################
  
  # number of inputted variables
  vars <- setdiff(unique(vars), c("", "None"))
  nvars <- length(vars)
    
  if (nvars == 0) {
    plot_types <- "Please select variable(s) first"
    
  } else if (nvars == 1) {  # 1-dimensional plot
    
    x <- data[, vars]
    
    if (is.numeric(x)) {  # if continuous
      plot_types <- c(`<i class='fa fa-bar-chart'></i>` = "histogram",
                      `<img src="chart-density-white.png" width=15px height=13px><div class='jhr'></div></img>` = "density",
                      `<img src="chart-boxplot-white.png" width=15px height=13px><div class='jhr'></div></img>` = "boxplot")
    } else {  # if categorical
      plot_types <- c(`<i class='fa fa-bar-chart'></i>` = "bar")
    }
    
  } else if (nvars == 2) {  # 2-dimensional plot
    
    x <- data[, vars]
    
    if (is.numeric(x[, 1]) & is.numeric(x[, 1])) {  # if both continuous
      plot_types <- c(`<img src="chart-scatter-white.png" width=15px height=13px><div class='jhr'></div></img>` = "scatterplot",
                      `<i class='fa fa-line-chart'></i>` = "line")
    } else if (!is.numeric(x[, 1]) & !is.numeric(x[, 2])) {  # if both categorical
      plot_types <- c(`<i class='fa fa-bar-chart'></i>` = "bar")
    } else {  # only one is categorial
      plot_types <- c(`<img src="chart-boxplot-white.png" width=15px height=13px><div class='jhr'></div></img>` = "boxplot")
    }
    
  } else if (nvars == 3) {  # 3-dimensional scatter plot
    
    plot_types <- c(`<img src="chart-scatter-white.png" width=15px height=13px><div class='jhr'></div></img>` = "scatterplot")
    
  }
  
  return(plot_types)
}

getShowTypes <- function(types = c("GGplot", "Plotly", "Table")) {
  ##### Function Description #####
  # getShowTypes: function to return icons corresponding to "GGplot", "Plotly",
  # or "Table"
  # 
  # inputs:
  # - show = vector with one or more of the following: "GGplot", "Plotly", or
  #     "Table"
  # 
  # outputs: named vector of icons
  #######################
  
  out <- c()
  if ("GGplot" %in% types) {
    out <- c(out,
             `<img src="ggplot.png" width=18px height=18px><div class='jhr'></div></img>` = "GGplot")
  }
  if ("Plotly" %in% types) {
    out <- c(out,
             `<img src="plotly.png" width=18px height=18px><div class='jhr'></div></img>` = "Plotly")
  }
  if ("Table" %in% types) {
    out <- c(out,
             `<i class='fa fa-table'></i>` = "Table")
  }
  
  return(out)
}

addPlotOptions <- function(plt, input, id, plotly = FALSE, heatmap = FALSE,
                           font = "Helvetica", 
                           strip_background_color = "#2c3e50", 
                           show_ticks = T,
                           show_axis = c("x", "y")) {
  
  x_axis_text_size <- input[[paste0("xtext_", id)]]
  y_axis_text_size <- input[[paste0("ytext_", id)]]
  legend_text_size <- input[[paste0("ltext_", id)]]
  strip_text_size <- input[[paste0("stext_", id)]]
  x_axis_title_size <- input[[paste0("xtitle_", id)]]
  y_axis_title_size <- input[[paste0("ytitle_", id)]]
  legend_title_size <- input[[paste0("ltitle_", id)]]
  title_size <- input[[paste0("title_", id)]]
  axis_line_width <- input[[paste0("awidth_", id)]]
  x_text_angle <- input[[paste0("xangle_", id)]]
  show_axis <- input[[paste0("atext_", id)]]
  
  if (!heatmap) {
    background_color <- input[[paste0("bg_", id)]]
    grid_color <-ifelse(input[[paste0("grid_", id)]], 
                        "grey90", input[[paste0("bg_", id)]])
  } else {
    background_color <- "grey98"
    grid_color <- "grey90"
  }
  
  if (plotly) {
    x_axis_text_size <- x_axis_text_size + 2
    y_axis_text_size <- y_axis_text_size + 2
    legend_text_size <- legend_text_size + 2
    x_axis_title_size <- x_axis_title_size + 2
    y_axis_title_size <- y_axis_title_size + 2
    legend_title_size <- legend_title_size + 2
    title_size <- title_size + 2
    axis_line_width <- axis_line_width + 1.5
  }
  
  add_theme <- theme(
    axis.line = element_line(size = axis_line_width, color = "black"),
    axis.text.x = element_text(family = font, size = x_axis_text_size,
                               angle = ifelse(x_text_angle, 45, 0),
                               hjust = ifelse(x_text_angle, 1, 0.5)),
    axis.text.y = element_text(family = font, size = y_axis_text_size),
    axis.title.x = element_text(family = font, size = x_axis_title_size,
                                face = "bold"),
    axis.title.y = element_text(family = font, size = y_axis_title_size,
                                face = "bold"),
    axis.ticks = element_line(size = ifelse(show_ticks, rel(1), 0),
                              colour = "black"),
    panel.grid.major = element_line(colour = grid_color, size = rel(0.5)),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = background_color),
    strip.background = element_rect(fill = strip_background_color,
                                    color = strip_background_color),
    strip.text = element_text(color = "white", size = strip_text_size,
                              face = "bold"),
    legend.key = element_rect(fill = "grey98"),
    legend.text = element_text(family = font, size = legend_text_size),
    legend.title = element_text(family = font, size = legend_title_size,
                                face = "bold"),
    plot.title = element_text(family = font, size = title_size, face = "bold")
  )
  
  if (!("x" %in% show_axis)) {
    add_theme <- add_theme +
      theme(axis.text.x = element_blank(), 
            axis.ticks.x = element_blank())
  }
  if (!("y" %in% show_axis)) {
    add_theme <- add_theme +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
  }
  
  if ("patchwork" %in% class(plt)) {
    plt <- plt & add_theme
  } else {
    plt <- plt + add_theme
  }
  
  return(plt)
}


getHeatmapArgs <- function(input, id) {
  
  if (missing(id)) {
    id <- ""
  } else {
    id <- paste0("_", id)
  }
  
  # color scheme options
  if (input[[paste0("color_theme_heatmap", id)]] == "Temperature") {
    manual.fill <- "temperature"
    viridis_option <- "C"
  } else {
    manual.fill <- NULL
    if (str_detect(input[[paste0("color_theme_heatmap", id)]], "cool")) {
      viridis_option <- "D"
    } else {
      viridis_option <- "C"
    }
  }
  if (input[[paste0("color_scale_heatmap", id)]] == "By quantile") {
    col_quantile <- TRUE
  } else {
    col_quantile <- FALSE
  }
  
  args_out <- list(
    clust.x = input[[paste0("heatmap_cluster_x", id)]] == 
      "Hierarchical Clustering",
    clust.y = input[[paste0("heatmap_cluster_y", id)]] == 
      "Hierarchical Clustering",
    clust.x.wi.group = input[[paste0("heatmap_cluster_x_wi", id)]],
    clust.y.wi.group = input[[paste0("heatmap_cluster_y_wi", id)]],
    linkage.x = input[[paste0("hclust_linkage_heatmap", id)]],
    linkage.y = input[[paste0("hclust_linkage_heatmap", id)]],
    option = viridis_option,
    manual.fill = manual.fill, 
    col_quantile = col_quantile, 
    n_quantiles = input[[paste0("color_n_quantiles_heatmap", id)]]
  )
  return(args_out)
}


getCorHeatmapArgs <- function(input, id) {
  
  if (missing(id)) {
    id <- ""
  } else {
    id <- paste0("_", id)
  }
  
  # color scheme options
  if (input[[paste0("color_theme_cor", id)]] == "Temperature") {
    manual.fill <- "temperature"
    viridis_option <- "C"
  } else {
    manual.fill <- NULL
    if (str_detect(input[[paste0("color_theme_cor", id)]], "cool")) {
      viridis_option <- "D"
    } else {
      viridis_option <- "C"
    }
  }
  if (input[[paste0("color_scale_cor", id)]] == "By quantile") {
    col_quantile <- TRUE
  } else {
    col_quantile <- FALSE
  }
  
  args_out <- list(
    cor.type = tolower(input[[paste0("cor_type", id)]]),
    clust = input[[paste0("cor_cluster", id)]] == "Hierarchical Clustering",
    linkage = input[[paste0("cor_hclust_linkage", id)]],
    text.size = input[[paste0("size_cor", id)]],
    option = viridis_option,
    manual.fill = manual.fill, 
    col_quantile = col_quantile, 
    n_quantiles = input[[paste0("color_n_quantiles_cor", id)]]
  )
  return(args_out)
}



