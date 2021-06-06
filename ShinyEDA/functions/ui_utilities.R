# script with utility functions for ui in shiny apps

library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinyBS)

isnull <- function(id, input = TRUE) {
  if (input) {
    out <- paste0("(typeof input.", id, " !== 'undefined' && input.", id, 
                  ".length > 0)")
  } else {
    out <- paste0("(typeof ", id, " !== 'undefined' && ", id, ".length > 0)")
  }
  return(out)
}

addTooltipBtn <- function(label, tooltip_id, icon_btn = icon("question"),
                          style = "default", size = "extra-small", margin = 6,
                          ...) {
  if (missing(label)) {
    out <- bsButton(
      inputId = paste0("tooltip_", tooltip_id), 
      label = "", 
      icon = icon_btn, 
      style = style,
      size = size,
      ...
    )
  } else {
    out <- HTML(
      paste(label,
            bsButton(
              inputId = paste0("tooltip_", tooltip_id), 
              label = "", 
              icon = icon_btn, 
              style = style,
              size = size,
              ...
            ) %>%
              tagAppendAttributes(style = paste0("margin-left: ", margin, "px")))
    )
  }
  return(out)
}

addTooltipPopover <- function(tooltip_id, title, content, 
                              placement = "right", trigger = "focus",
                              options = list(container = "body"), ...) {
  bsPopover(
    id = paste0("tooltip_", tooltip_id),
    title = title,
    content = content,
    placement = placement, 
    trigger = trigger, 
    options = options,
    ...
  )
}

fileUpload <- function(id, label, tooltip = FALSE,
                       tooltip_title = "", tooltip_content = "",
                       tooltip_placement = "right", tooltip_trigger = "focus",
                       tooltip_options = list(container = "body"),
                       tooltip_icon_btn = icon("question"),
                       tooltip_style = "default", tooltip_size = "extra-small",
                       tooltip_margin = 6) {
  ##### Function Description ######
  # function to upload files (.csv, .txt, .rds)
  # 
  # inputs:
  # - id = filename or root of id
  # - label = label argument in fileInput()
  ####################
  if (missing(id)) {
    id <- "file"
  } else {
    id <- paste0("file_", id)
  }
  
  label <- paste(label, "(.csv, .rds, .txt)")
  if (tooltip) {
    label <- addTooltipBtn(tooltip_id = id, label = label, 
                           icon_btn = tooltip_icon_btn, 
                           style = tooltip_style, 
                           size = tooltip_size,
                           margin = tooltip_margin)
  }
  
  out <- list(
    fileInput(
      inputId = id, 
      label = label, 
      multiple = FALSE, accept = c(".rds", ".csv", ".txt")
    ),
    conditionalPanel(
      condition = paste0("output.fileType_", id, " == '.txt'"),
      prettyRadioButtons(
        inputId = paste0("sep_", id), 
        label = "Separator",
        choices = c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"),
        icon = icon("check"),
        bigger = TRUE,
        inline = TRUE,
        status = "info",
        animation = "smooth"
      )
    ),
    conditionalPanel(
      condition = paste0("output.fileType_", id, " == '.csv' | ",
                         "output.fileType_", id, " == '.txt'"),
      materialSwitch(
        paste0("header_", id), HTML("<b>Header</b>"),
        value = TRUE, status = "info"
      )
    )
  )
  
  if (tooltip) {
    out <- c(out,
             list(
               addTooltipPopover(tooltip_id = id, 
                                 title = tooltip_title, 
                                 content = tooltip_content, 
                                 placement = tooltip_content, 
                                 trigger = tooltip_trigger, 
                                 options = tooltip_options)
             ))
  }
  return(out)
}

varInput <- function(id, label, choices, 
                     selected = NULL, options = NULL, ...) {
  ##### Function Description ######
  # wrapper to pickerInput() to select one variable
  # 
  # inputs:
  # - id = id argument in pickerInput()
  # - label = label argument in pickerInput()
  # - choices = choices argument in pickerInput()
  # - selected = selected argument in pickerInput()
  # - options = options argument in pickerInput()
  # - ... = other arguments to pass to pickerInput()
  ####################
  
  if (is.null(options)) {
    options <- list(`live-search` = TRUE,
                    size = 5,
                    title = "Nothing selected")
  }
  pickerInput(
    inputId = id, 
    label = label,
    choices = choices,
    selected = selected,
    options = options,
    ...
  )
}

varInputMultiple <- function(id, label, choices, 
                             selected = NULL, 
                             actionsBox = TRUE,
                             maxOptions = NULL,
                             options = NULL, ...) {
  ##### Function Description ######
  # wrapper to pickerInput() to select multiple variables
  # 
  # inputs:
  # - id = id argument in pickerInput()
  # - label = label argument in pickerInput()
  # - choices = choices argument in pickerInput()
  # - selected = selected argument in pickerInput()
  # - options = options argument in pickerInput()
  # - ... = other arguments to pass to pickerInput()
  ####################
  
  if (is.null(options)) {
    options <- list(`live-search` = TRUE,
                    size = 5,
                    `selected-text-format` = "count > 3",
                    title = "Nothing selected",
                    multipleSeparator = ", ",
                    `actions-box` = actionsBox,
                    `max-options` = maxOptions)
  }
  
  pickerInput(
    inputId = id, 
    label = label,
    choices = choices,
    selected = selected,
    multiple = TRUE,
    options = options,
    ...
  )
}

submitBtn <- function(id, label = "Submit", br = TRUE, 
                      style = "pill", color = "danger",
                      div_style = "display:inline-block", ...) {
  ##### Function Description ######
  # wrapper to actionBttn() 
  # 
  # inputs:
  # - id = id argument in actionBttn()
  # - label = label argument in actionBttn()
  # - style = style argument in actionBttn()
  # - color = color argument in actionBttn()
  # - ... = other arguments to pass to actionBttn()
  ###################
  
  if (br) {
    out <- list(
      br(),
      actionButton(
        inputId = id,
        label = label,
        style = style,
        color = color,
        ...
      ) %>%
        div(style = div_style)
    )
  } else {
    out <- actionButton(
      inputId = id,
      label = label,
      style = style,
      color = color,
      ...
    ) %>%
      div(style = div_style)
  }
}

refreshBtn <- function(id, style = "material-circle", color = "default",
                       size = "sm", right_align = TRUE, ...) {
  ##### Function Description ######
  # wrapper to actionBttn() with refresh icon
  # 
  # inputs:
  # - id = id argument in actionBttn()
  # - style = style argument in actionBttn()
  # - color = color argument in actionBttn()
  # - small = small argument in actionBttn()
  # - right_align = logical; whether or not to right align button
  # - ... = other arguments to pass to actionBttn()
  ###################
  actionBttn(
    inputId = paste0("refresh_", id),
    label = NULL,
    style = style,
    color = color, 
    size = size,
    icon = icon("refresh"),
    ...
  ) %>%
    tagAppendAttributes(style = "float: right")
}

radioBtns <- function(id, label, choices, selected = NULL,
                      status = "info", animation = "jelly",
                      icon_btn = icon("check"), bigger = TRUE, ...) {
  ##### Function Description ######
  # wrapper to prettyRadioButtons() 
  # 
  # inputs:
  # - id = id argument in prettyRadioButtons()
  # - label = label argument in prettyRadioButtons()
  # - choices = choices argument in prettyRadioButtons()
  # - selected = selected argument in prettyRadioButtons()
  # - ... = other arguments to pass to prettyRadioButtons()
  ###################
  
  prettyRadioButtons(
    inputId = id, 
    label = label,
    choices = choices,
    selected = selected,
    status = status, 
    animation = animation, 
    icon = icon_btn, 
    bigger = bigger,
    ...
  )
}

checkbox <- function(id, label, value = FALSE, 
                      status = "info", animation = "jelly",
                      icon_btn = icon("check"), bigger = TRUE, ...) {
  ##### Function Description ######
  # wrapper to prettyCheckbox() 
  # 
  # inputs:
  # - id = id argument in prettyCheckbox()
  # - label = label argument in prettyCheckbox()
  # - value = value argument in prettyCheckbox()
  # - ... = other arguments to pass to prettyCheckbox()
  ###################
  
  prettyCheckbox(
    inputId = id, 
    label = label,
    value = value,
    status = status,
    animation = animation,
    icon = icon_btn,
    ...
  )
}

checkboxGroup <- function(id, label, choices, selected = NULL,
                          status = "info", animation = "jelly",
                          icon_btn = icon("check"), bigger = TRUE, ...) {
  ##### Function Description ######
  # wrapper to prettyCheckboxGroup() 
  # 
  # inputs:
  # - id = id argument in prettyCheckboxGroup()
  # - label = label argument in prettyCheckboxGroup()
  # - choices = value argument in prettyCheckboxGroup()
  # - selected = selected argument in prettyCheckboxGroup()
  # - ... = other arguments to pass to prettyCheckboxGroup()
  ###################
  
  prettyCheckboxGroup(
    inputId = id, 
    label = label,
    choices = choices,
    selected = selected,
    status = status,
    animation = animation,
    icon = icon_btn,
    bigger = bigger,
    ...
  )
}

dataSplitToggle <- function(id, choices = c("Training", "Test"), 
                            selected = NULL, individual = FALSE,
                            size = "sm", justified = FALSE) {
  ##### Function Description ######
  # toggle for selecting data split
  ###################
  
  radioGroupButtons(
    inputId = paste0("datasplit_", id), 
    choices = choices,
    individual = individual,
    size = size,
    justified = justified
  ) %>%
    tagAppendAttributes(class = "btn-margin",
                        style = "display: inline-block;")
}

showToggle <- function(id, choices = c("GGplot", "Plotly", "Table"),
                       selected = NULL, individual = FALSE, size = "sm",
                       justified = FALSE) {
  ##### Function Description ######
  # toggle for selecting show type: ggplot, plotly, table
  ###################
  
  radioGroupButtons(
    inputId = paste0("plottype_", id),
    choices = getShowTypes(choices),
    selected = selected,
    individual = individual,
    size = size,
    justified = justified
  ) %>% 
    tagAppendAttributes(class = "btn-circle btn-margin",
                        style = "display: inline-block")
}

showSquareToggle <- function(id, choices = c("Heatmap", "P-values", "Both"),
                             selected = "Both", individual = FALSE, size = "sm",
                             justified = FALSE, full_id = FALSE) {
  ##### Function Description ######
  # toggle for selecting show type in local stability plots
  ###################
  
  if (full_id) {
    input_id <- id
  } else {
    input_id <- paste0("show_", id)
  }
  radioGroupButtons(
    inputId = input_id,
    choices = choices,
    selected = selected,
    individual = individual,
    size = size,
    justified = justified
  ) %>% 
    tagAppendAttributes(class = "btn-margin",
                        style = "display: inline-block")
}

plotTypeToggle <- function(id, choices, selected = NULL, 
                           individual = TRUE, size = "sm", justified = FALSE) {
  ##### Function Description ######
  # toggle for selecting plot type
  ###################
  
  radioGroupButtons(
    inputId = paste0("plotType", id), 
    choices = choices,
    individual = individual,
    size = size,
    justified = justified
  ) %>%
    tagAppendAttributes(class = "btn-circle btn-margin",
                        style = "display: inline-block")
}

plotPairsOptions <- function(id) {
  ##### Function Description ######
  # basic shiny input arguments for pairPlot()
  ###################
  
  list(
    sliderInput(
      paste0("subsample_", id), "Subsample Points", min = 0, max = 1, value = 1
    ),
    sliderInput(
      paste0("alpha_", id), "Transparency", min = 0, max = 1, value = 1
    ),
    numericInput(
      paste0("size_", id), "Point Size", value = 1, min = 0, max = 10
    ),
    numericInput(
      paste0("corsize_", id), "Correlation Text Size", value = 3.5, min = 0
    )
  )
}

plotHclustHeatmapOptions <- function(id, multicol = FALSE, 
                                     column_widths = NULL, total_width = 12) {
  ##### Function Description ######
  # basic shiny input arguments for plotHclustHeatmap()
  ###################
  
  if (missing(id)) {
    id <- ""
  } else {
    id <- paste0("_", id)
  }
  
  if (!multicol) {
    opts <- list(
      radioBtns(
        id = paste0("heatmap_cluster_x", id), 
        label = "Cluster x by",
        choices = c("Hierarchical Clustering", "None")
      ),
      radioBtns(
        id = paste0("heatmap_cluster_y", id), 
        label = "Cluster y by",
        choices = c("Hierarchical Clustering", "None")
      ),
      conditionalPanel(
        paste0("input.heatmap_cluster_x", id,
               " == 'Hierarchical Clustering' | input.heatmap_cluster_y", id, 
               " == 'Hierarchical Clustering'"),
        varInput(
          id = paste0("hclust_linkage_heatmap", id), 
          label = "Linkage",
          choices = c("ward.D", "ward.D2", "single", "complete", 
                      "average", "mcquitty", "median", "centroid"),
          selected = "ward.D"
        )
      ),
      radioBtns(
        id = paste0("color_theme_heatmap", id),
        label = "Color Theme",
        choices = c("Viridis - cool", "Viridis - warm", "Temperature")
      ),
      radioBtns(
        id = paste0("color_scale_heatmap", id), 
        label = "Color Scale",
        choices = c("By magnitude", "By quantile")
      ),
      conditionalPanel(
        paste0("input.color_scale_heatmap", id, 
               " == 'By quantile' & input.color_theme_heatmap", id,
               " != 'Temperature'"),
        numericInput(
          paste0("color_n_quantiles_heatmap", id), "Number of quantiles",
          value = 5, min = NA, max = NA, step = 1
        )
      ),
      checkbox(
        id = paste0("coord_flip_heatmap", id),
        label = "Flip x and y axes"
      )
    )
  } else {
    opts1 <- list(
      radioBtns(
        id = paste0("heatmap_cluster_x", id), 
        label = "Cluster x by",
        choices = c("Hierarchical Clustering", "None")
      ),
      radioBtns(
        id = paste0("heatmap_cluster_y", id), 
        label = "Cluster y by",
        choices = c("Hierarchical Clustering", "None")
      ),
      conditionalPanel(
        paste0("input.heatmap_cluster_x", id,
               " == 'Hierarchical Clustering' | input.heatmap_cluster_y", id, 
               " == 'Hierarchical Clustering'"),
        varInput(
          id = paste0("hclust_linkage_heatmap", id), 
          label = "Linkage",
          choices = c("ward.D", "ward.D2", "single", "complete", 
                      "average", "mcquitty", "median", "centroid"),
          selected = "ward.D"
        )
      )
    )
    
    opts2 <- list(
      radioBtns(
        id = paste0("color_theme_heatmap", id),
        label = "Color Theme",
        choices = c("Viridis - cool", "Viridis - warm", "Temperature")
      ),
      radioBtns(
        id = paste0("color_scale_heatmap", id), 
        label = "Color Scale",
        choices = c("By magnitude", "By quantile")
      ),
      conditionalPanel(
        paste0("input.color_scale_heatmap", id, 
               " == 'By quantile' & input.color_theme_heatmap", id,
               " != 'Temperature'"),
        numericInput(
          paste0("color_n_quantiles_heatmap", id), "Number of quantiles",
          value = 5, min = NA, max = NA, step = 1
        )
      ),
      checkbox(
        id = paste0("coord_flip_heatmap", id),
        label = "Flip x and y axes"
      )
    )
    
    if (is.null(column_widths)) {
      column_widths <- rep(total_width / 2, 2)
    } 
    opts <- list(
      column(column_widths[1], opts1),
      column(column_widths[2], opts2)
    )
  }
  return(opts)
}

plotCorHeatmapOptions <- function(id) {
  ##### Function Description ######
  # basic shiny input arguments for plotCorHeatmap()
  ###################
  
  if (missing(id)) {
    id <- ""
  } else {
    id <- paste0("_", id)
  }
  
  opts <- list(
    radioBtns(
      id = paste0("cor_cluster", id), 
      label = "Cluster by",
      choices = c("Hierarchical Clustering", "None")
    ),
    conditionalPanel(
      paste0("input.cor_cluster", id,
             " == 'Hierarchical Clustering' | input.cor_cluster", id, 
             " == 'Hierarchical Clustering'"),
      varInput(
        id = paste0("cor_hclust_linkage", id), 
        label = "Linkage",
        choices = c("ward.D", "ward.D2", "single", "complete", 
                    "average", "mcquitty", "median", "centroid"),
        selected = "ward.D"
      )
    ),
    numericInput(
      inputId = paste0("size_cor", id), 
      label = "Text Size", value = 0, min = 0, max = NA
    ),
    radioBtns(
      id = paste0("color_theme_cor", id),
      label = "Color Theme",
      choices = c("Viridis - cool", "Viridis - warm", "Temperature")
    ),
    radioBtns(
      id = paste0("color_scale_cor", id), 
      label = "Color Scale",
      choices = c("By magnitude", "By quantile")
    ),
    conditionalPanel(
      paste0("input.color_scale_cor", id, 
             " == 'By quantile' & input.color_theme_cor", id,
             " != 'Temperature'"),
      numericInput(
        paste0("color_n_quantiles_cor", id), "Number of quantiles",
        value = 5, min = NA, max = NA, step = 1
      )
    )
  )
  
  return(opts)
  
  
}

plotLocalStabilityRFOptions <- function(id) {
  ##### Function Description ######
  # basic shiny input arguments for plotLocalStabilityRF()
  ###################
  list(
    fileUpload(
      id = paste0("vargroups_", id), 
      label = "Variable Groups",
      tooltip = TRUE, 
      tooltip_title = "Variable Groups",
      tooltip_content = "Please upload a data frame with two columns named 'feature' and 'group' that maps each feature to a larger super-feature group."
    ),
    materialSwitch(
      paste0("first_", id), HTML("<b>Count First Split Only</b>"), 
      value = TRUE, status = "info"
    ),
    conditionalPanel(
      paste0("input.show_", id, " !== 'Heatmap'"),
      numericInput(
        paste0("nperm_", id), "Number of Permutations", 
        value = 1e4, step = 1e4, min = 0
      ),
    ),
    conditionalPanel(
      paste0("input.show_", id, " == 'Both'"),
      numericInput(
        paste0("size_", id), "Point Size", value = 1, step = 1, min = 0
      ),
      textInput(
        paste0("heights_", id), "Height Proportions (comma delimited)", 
        value = "1,4"
      )
    )
  )
}

plotTypeOptions <- function(id, plot_type_id) {
  ##### Function Description ######
  # basic options for common plot types
  ###################
  
  list(
    conditionalPanel(
      paste0("input.plotType", plot_type_id, " == 'histogram'"),
      sliderInput(
        inputId = paste0("bins_", id), 
        label = "Number of Bins", 
        value = 15, min = 1, max = 50
      )
    ),
    conditionalPanel(
      paste0("input.plotType", plot_type_id, " == 'density' | input.plotType", 
             plot_type_id, " == 'histogram' | input.plotType", plot_type_id, 
             " == 'scatterplot'"),
      sliderInput(
        inputId = paste0("alpha_", id), 
        label = "Transparency", 
        min = 0, max = 1, value = .7
      )
    ),
    conditionalPanel(
      paste0("input.plotType", plot_type_id, 
             " == 'scatterplot' | input.plotType", plot_type_id, " == 'line'"),
      numericInput(
        inputId = paste0("size_", id),
        label = "Point Size", 
        value = 1, min = 0, max = 10
      )
    )
  )
}

plotOptions <- function(id, heatmap = FALSE, multicol = FALSE, total_width = 12,
                        height = 500,
                        x_axis_text_size = 12, y_axis_text_size = 12,
                        legend_text_size = 12, strip_text_size = 14,
                        x_axis_title_size = 14, y_axis_title_size = 14,
                        legend_title_size = 14, title_size = 16, 
                        axis_line_width = 1, x_text_angle = FALSE) {
  ##### Function Description ######
  # basic options for all plots
  ###################
  
  if (!multicol) {
    opts <- list(
      checkboxGroup(id = paste0("atext_", id), label = "Show Axis Text",
                    choices = c("x", "y"), selected = c("x", "y")),
      checkbox(id = paste0("xangle_", id), label = "Angle X-Axis Text", 
               value = x_text_angle),
      numericInput(paste0("xtext_", id), "X-Axis Text Size", 
                   value = x_axis_text_size),
      numericInput(paste0("ytext_", id), "Y-Axis Text Size", 
                   value = y_axis_text_size),
      numericInput(paste0("ltext_", id), "Legend Text Size",
                   value = legend_text_size),
      numericInput(paste0("stext_", id), "Strip Text Size",
                   value = strip_text_size),
      numericInput(paste0("xtitle_", id), "X-Axis Title Size", 
                   value = x_axis_title_size),
      numericInput(paste0("ytitle_", id), "Y-Axis Title Size", 
                   value = y_axis_title_size),
      numericInput(paste0("ltitle_", id), "Legend Title Size",
                   value = legend_title_size),
      numericInput(paste0("title_", id), "Title Size",
                   value = title_size),
      numericInput(paste0("awidth_", id), "Axis Line Width",
                   value = axis_line_width),
      numericInput(paste0("height_", id), "Plot Height (px)",
                   value = height)
    )
    
    if (!heatmap) {
      opts <- c(
        list(
          textInput(paste0("bg_", id), "Background color", value = "grey98"),
          checkbox(id = paste0("grid_", id), label = "Show grid lines", 
                   value = TRUE),
          opts
        )
      )
    } 
  } else {
    opts_labs1 <- list(
      numericInput(paste0("title_", id), "Title Size",
                   value = title_size),
      numericInput(paste0("xtitle_", id), "X-Axis Title Size", 
                   value = x_axis_title_size),
      numericInput(paste0("ytitle_", id), "Y-Axis Title Size", 
                   value = y_axis_title_size),
      numericInput(paste0("ltitle_", id), "Legend Title Size",
                   value = legend_title_size)
    )
    opts_labs2 <- list(
      numericInput(paste0("xtext_", id), "X-Axis Text Size", 
                   value = x_axis_text_size),
      numericInput(paste0("ytext_", id), "Y-Axis Text Size", 
                   value = y_axis_text_size),
      numericInput(paste0("ltext_", id), "Legend Text Size",
                   value = legend_text_size),
      numericInput(paste0("stext_", id), "Strip Text Size",
                   value = strip_text_size)
    )
    opts_other <- list(
      checkboxGroup(id = paste0("atext_", id), label = "Show Axis Text",
                    choices = c("x", "y"), selected = c("x", "y"), inline = T),
      materialSwitch(inputId = paste0("xangle_", id), 
                     label = HTML("<b>Angle X-Axis Text</b>"),
                     value = x_text_angle,
                     status = "info"),
      numericInput(paste0("awidth_", id), "Axis Line Width",
                   value = axis_line_width),
      numericInput(paste0("height_", id), "Plot Height (px)",
                   value = height)
    )
    
    if (!heatmap) {
      opts_other <- c(
        list(
          opts_other,
          checkbox(id = paste0("grid_", id), label = "Show grid lines", 
                   value = TRUE),
          textInput(paste0("bg_", id), "Background color", value = "grey98")
        )
      )
    }
    
    opts <- list(
      column(total_width / 3, opts_labs1),
      column(total_width / 3, opts_labs2),
      column(total_width / 3, opts_other)
    )
  }
  
  return(opts)
}

tableOptions <- function(id, digits = NA, sigfig = FALSE) {
  ##### Function Description ######
  # basic table options: digits and sigfigs inputs
  # 
  # inputs:
  # - id = id argument
  # - digits = default digits to show in table
  # - sigfig = logical; whether or not to use sigfigs as default
  ###################
  
  if (missing(id)) {
    digits_id <- "digits"
    sigfig_id <- "sigfig"
  } else {
    digits_id <- paste0("digits_", id)
    sigfig_id <- paste0("sigfig_", id)
  }
  
  list(
    numericInput(
      inputId = digits_id, label = "Digits",
      value = digits, min = 0, max = NA, step = 1
    ),
    checkbox(
      id = sigfig_id, label = "Use Significant Digits",
      value = sigfig
    )
  )
}


