# script with utility functions for observational study toolbox app

library(tidyverse)


getPlotTypes <- function(data, vars) {
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