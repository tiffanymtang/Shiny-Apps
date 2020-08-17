# script with utility functions for observational study toolbox app

library(tidyverse)


getPlotTypes <- function(data, var1 = "", var2 = "") {
  # getPlotTypes: function to return possible plot types given variable inputs
  #   and data (based on # of input variables and # of factor variables)
  # 
  # inputs:
  # - data = data.frame; usual data matrix
  # - var1 = string; must be name of variable of intrest (in data) or "None" or
  #     ""
  # - var2 = string; must be name of variable of intrest (in data) or "None" or
  #     ""
  # 
  # outputs: named vector of possible graph icons (e.g., boxplot, barplot, 
  #   histogram, density, scatterplot, line)
  
  # number of inputted variables
  nvars <- (var1 != "" & var1 != "None") + (var2 != "" & var2 != "None") -
    (var1 == var2 & var1 != "" & var1 != "None")
  
  if (nvars == 0) {
    plot_types <- "Please select variable(s) first"
    
  } else if (nvars == 1) {  # 1-dimensional plot
    
    if (var1 != "" & var1 != "None") {
      x <- data[, var1]
    } else {
      x <- data[, var2]
    }
    
    if (is.numeric(x)) {  # if continuous
      plot_types <- c(`<i class='fa fa-bar-chart'></i>` = "histogram",
                      `<img src="chart-density-white.png" width=15px height=13px><div class='jhr'></div></img>` = "density",
                      `<img src="chart-boxplot-white.png" width=15px height=13px><div class='jhr'></div></img>` = "boxplot")
    } else {  # if categorical
      plot_types <- c(`<i class='fa fa-bar-chart'></i>` = "bar")
    }
    
  } else if (nvars == 2) {  # 2-dimensional plot
    
    if (is.numeric(data[, var1]) & 
        is.numeric(data[, var2])) {  # if both continuous
      plot_types <- c(`<img src="chart-scatter-white.png" width=15px height=13px><div class='jhr'></div></img>` = "scatterplot",
                      `<i class='fa fa-line-chart'></i>` = "line")
    } else if (!is.numeric(data[, var1]) &
               !is.numeric(data[, var2])) {  # if both are categorical
      plot_types <- c(`<i class='fa fa-bar-chart'></i>` = "bar")
    } else {  # only one is categorial
      plot_types <- c(`<img src="chart-boxplot-white.png" width=15px height=13px><div class='jhr'></div></img>` = "boxplot")
    }
    
  }
  return(plot_types)
}