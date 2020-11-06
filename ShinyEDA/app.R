# shiny packages
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
# useful R packages
library(R.utils)
library(tidyverse)
library(broom)
# plotting packages
library(GGally)
library(ggpubr)
# table packages
library(knitr)
library(kableExtra)
library(DT)
library(plotly)

sourceDirectory("./functions/", modifiedOnly = F, recursive = F) 

data(iris)
data <- iris

options(shiny.maxRequestSize = 100 * 1024^2)  # increase max file size to 30Mb

css <- HTML(" 
.dropdown-toggle {
  color: #777;
  background-color: #FFFFFF;
  border: 1px solid #CCCCCC;
}

.dropdown-toggle:hover {
  color: #777;
  background-color: #FFFFFF;
  border: 1px solid #566566;
}

.open>.dropdown-toggle.btn-default, .open>.dropdown-toggle.btn-default:hover {
  color: #777;
  background-color: #FFFFFF;
  border: 2px solid #566566;
}

.bs-searchbox .form-control {
  border: 1px solid #DCE4EC;
}

.btn-default:focus {
  color: #777;
  background-color: #FFFFFF;
}
")

###################################### UI ######################################
# Define UI for app
ui <- fluidPage(
  tags$head(tags$style(css)),
  theme = shinytheme("flatly"),
  # theme = shinytheme("cerulean"),
  # shinythemes::themeSelector(),
  titlePanel("Exploratory Data Analysis"),
  
  sidebarLayout(
    # sidebar panel -----------------------------------------------------------
    sidebarPanel(id = "side-panel",
                 
      # file upload -----------------------------------------------------------
      fileInput(
        inputId = "file", label = "File Upload (.csv, .rds, .txt)", 
        multiple = FALSE, accept = c(".rds", ".csv", ".txt")
      ),
      conditionalPanel(
        condition = "output.fileType == '.txt'",
        prettyRadioButtons("sep", "Separator",
                           choices = c("Comma" = ",",
                                       "Semicolon" = ";",
                                       "Tab" = "\t"),
                           icon = icon("check"),
                           bigger = TRUE,
                           inline = TRUE,
                           status = "info",
                           animation = "smooth")
      ),
      conditionalPanel(
        condition = "output.fileType == '.csv' | output.fileType == '.txt'",
        materialSwitch("header", "Header", value = TRUE, status = "info")
      ),
      
      hr(),
      
      # data summary: variable inputs ----------------------------------------
      conditionalPanel(
        "input.tab == 'summary'",
        # variable inputs
        pickerInput(
          "vars_summary", "Variables:",
          choices = colnames(data)[sapply(data, class) == "numeric"],
          multiple = TRUE,
          options = list(`live-search` = TRUE,
                         size = 5,
                         `selected-text-format` = "count > 3",
                         title = "Nothing selected",
                         multipleSeparator = ", ",
                         `actions-box` = TRUE)
        )
      ),
    
      # basic plot: variable inputs -------------------------------------------
      conditionalPanel(
        "input.tab == 'basic'",
        pickerInput(
          "var1", "Variable 1:",
          choices = c("None", colnames(data)),
          options = list(`live-search` = TRUE,
                         size = 5,
                         title = "Nothing selected")
        ),
        pickerInput(
          "var2", "Variable 2:",
          choices = c("None", colnames(data)),
          options = list(`live-search` = TRUE,
                         size = 5,
                         title = "Nothing selected")
        ),
        pickerInput(
          "color1", "Color by:",
          choices = c("None", colnames(data)),
          options = list(`live-search` = TRUE,
                         size = 5)
        )
      ),
      
      # pair plot: variable inputs -------------------------------------------
      conditionalPanel(
        "input.tab == 'pair'",
        # variable inputs
        pickerInput(
          "vars_pairs", "Variables:",
          choices = colnames(data),
          multiple = TRUE,
          options = list(`live-search` = TRUE,
                         size = 5,
                         `selected-text-format` = "count > 3",
                         title = "Nothing selected",
                         multipleSeparator = ", ",
                         `actions-box` = TRUE)
        ),
        pickerInput(
          "color_pairs", "Color by: (max 2)",
          choices = colnames(data),
          multiple = TRUE,
          options = list(`live-search` = TRUE,
                         size = 5,
                         multipleSeparator = ", ",
                         `max-options` = 2)
        )
      ),
      
      # pca plot: variable inputs -------------------------------------------
      conditionalPanel(
        "input.tab == 'pca'",
        # variable inputs
        pickerInput(
          "pcs", "PCs:",
          choices = 1:10,
          selected = 1:2,
          multiple = TRUE,
          options = list(size = 5,
                         `selected-text-format` = "count > 3",
                         multipleSeparator = ", ")
        ),
        pickerInput(
          "color_pca", "Color by: (max 2)",
          choices = colnames(data),
          multiple = TRUE,
          options = list(`live-search` = TRUE,
                         size = 5,
                         multipleSeparator = ", ",
                         `max-options` = 2)
        ),
        prettyCheckboxGroup(
          "pca_options", "PCA Options:",
          choices = c("Center", "Scale"),
          selected = "Center", 
          status = "info", animation = "jelly", 
          icon = icon("check"), bigger = T
        )
      ),
      
      # heatmaps: variable inputs ----------------------------------
      conditionalPanel(
        "input.tab == 'heatmaps'",
        
        # how to select variables
        prettyRadioButtons(
          "vars_select_heatmap", "Select Features",
          choices = c("Manually", "Randomly"),
          status = "info", animation = "jelly", 
          icon = icon("check"), bigger = TRUE
        ),
        
        # how to select rows
        prettyRadioButtons(
          "sample_select_heatmap", "Select Samples",
          choices = c("Manually", "Randomly"),
          status = "info", animation = "jelly", 
          icon = icon("check"), bigger = TRUE
        ),
        
        # manual variable inputs
        conditionalPanel(
          "input.vars_select_heatmap == 'Manually'",
          pickerInput(
            "vars_heatmap", "Features:",
            choices = colnames(data)[sapply(data, class) == "numeric"],
            multiple = TRUE,
            options = list(`live-search` = TRUE,
                           size = 5,
                           `selected-text-format` = "count > 3",
                           title = "Nothing selected",
                           multipleSeparator = ", ",
                           `actions-box` = TRUE)
          )
        ),
        conditionalPanel(
          "input.sample_select_heatmap == 'Manually'",
          pickerInput(
            "sample_heatmap", "Samples:",
            choices = rownames(data),
            multiple = TRUE,
            options = list(`live-search` = TRUE,
                           size = 5,
                           `selected-text-format` = "count > 3",
                           title = "Nothing selected",
                           multipleSeparator = ", ",
                           `actions-box` = TRUE)
          )
        ),
        
        # random variable inputs
        conditionalPanel(
          "input.vars_select_heatmap == 'Randomly'",
          sliderInput(
            "p_heatmap", "Number of Features", value = 0, min = 0, max = 4
          )
        ),
        conditionalPanel(
          "input.sample_select_heatmap == 'Randomly'",
          sliderInput(
            "p_heatmap_rows", "Number of Rows", value = 0, min = 0, max = 150
          )
        )
      ),
    
      # correlation heatmap: variable inputs ----------------------------------
      conditionalPanel(
        "input.tab == 'correlation'",
        
        # what to plot correlation heatmap of
        prettyRadioButtons(
          "dim_select", "Select Dimension",
          choices = c("Columns", "Rows"),
          status = "info", animation = "jelly", 
          icon = icon("check"), bigger = TRUE
        ),
        
        # how to select variables
        conditionalPanel(
          "input.dim_select == 'Columns'",
          prettyRadioButtons(
            "vars_select", "Select Features",
            choices = c("Manually", "Randomly"),
            status = "info", animation = "jelly", 
            icon = icon("check"), bigger = TRUE
          ),
          
          # variable inputs
          conditionalPanel(
            "input.vars_select == 'Manually'",
            pickerInput(
              "vars_cor", "Features:",
              choices = colnames(data)[sapply(data, class) == "numeric"],
              multiple = TRUE,
              options = list(`live-search` = TRUE,
                             size = 5,
                             `selected-text-format` = "count > 3",
                             title = "Nothing selected",
                             multipleSeparator = ", ",
                             `actions-box` = TRUE)
            )
          ),
          
          conditionalPanel(
            "input.vars_select == 'Randomly'",
            sliderInput(
              "p_cor", "Number of Features", value = 0, min = 0, max = 4
            )
          ),
        ),
        
        conditionalPanel(
          "input.dim_select == 'Rows'",
          sliderInput(
            "p_cor_rows", "Number of Rows", value = 0, min = 0, max = 150
          )
        ),
        
        # correlation settings
        prettyRadioButtons(
          "cor_type", "Correlation Type",
          choices = c("Pearson", "Kendall", "Spearman"),
          status = "info", animation = "jelly",
          icon = icon("check"), bigger = TRUE
        )
      )
    ),
    
    # main panel -----------------------------------------------------------
    mainPanel(
      tabsetPanel(
        type = "tabs",
        id = "tab",
        # tab: Data Summary -------------------------------------------
        tabPanel(
          "Data Summary", value = "summary",
          
          # add small white space ---------------------------------------------
          textOutput("placeholder7"),
          tags$head(
            tags$style("#placeholder7{font-size: 3px; color: white}")
          ),
          
          # data summary text -----------------------------------------------
          br(),
          h4(textOutput("n_output")),
          h4(textOutput("p_output")),
          h4(textOutput("na_output")),
          h4(textOutput("const_output")),
          
          # data summary: dropdown panel --------------------------------------
          dropdown(
            # type of plot
            radioGroupButtons(
              "plotTypeSummary", "Choose a graph type:",
              choices = c(`<i class='fa fa-bar-chart'></i>` = "histogram",
                          `<img src="chart-density-white.png" width=15px height=13px><div class='jhr'></div></img>` = "density",
                          `<img src="chart-boxplot-white.png" width=15px height=13px><div class='jhr'></div></img>` = "boxplot"),
              justified = TRUE
            ),
            
            # graph settings
            conditionalPanel(
              "input.plotTypeSummary == 'histogram'",
              sliderInput(
                "bins_summary", "Number of Bins", value = 15, min = 1, max = 50
              )
            ),
            conditionalPanel(
              "input.plotTypeSummary == 'density' | input.plotTypeSummary == 'histogram'",
              sliderInput(
                "alpha_summary", "Transparency", min = 0, max = 1, value = .7
              )
            ),
            
            numericInput(
              "height_summary", "Plot Height (px)", value = 400
            ),
            
            # button settings
            circle = TRUE, status = "default", size = "sm",
            icon = icon("gear"), width = "300px", style = "material-circle",
            tooltip = tooltipOptions(title = "Click to see inputs")
          ),
          
          # data summary plots ----------------------------------------------
          textOutput("placeholder10"),
          tags$head(
            tags$style("#placeholder10{font-size: 3px; color: white}")
          ),
          plotlyOutput("dist", height = "100%"),
          textOutput("placeholder11"),
          tags$head(
            tags$style("#placeholder11{font-size: 3px; color: white}")
          ),
          h4(textOutput("mean_output")),
          h4(textOutput("var_output")),
          plotlyOutput("means", height = "100%"),
          textOutput("placeholder12"),
          tags$head(
            tags$style("#placeholder12{font-size: 3px; color: white}")
          ),
          plotlyOutput("variances", height = "100%")
        ),
        
        # tab: Data Table ---------------------------------------------------
        tabPanel(
          "Data Table", value = "data_tab",
          
          # add small white space ---------------------------------------------
          textOutput("placeholder6"),
          tags$head(
            tags$style("#placeholder6{font-size: 3px; color: white}")
          ),
          
          # data table: dropdown panel------------------------------------------
          dropdown(
            # table options
            numericInput(
              "digits", "Digits",
              value = 0, min = 0, max = NA, step = 1
            ),
            
            # button settings
            circle = TRUE, status = "default", size = "sm",
            icon = icon("gear"), width = "300px", style = "material-circle",
            tooltip = tooltipOptions(title = "Click to see inputs")
          ),
          
          # data table --------------------------------------------------------
          br(),
          DTOutput("table")
        ),
        # tab: Basic Plots ---------------------------------------------------
        tabPanel(
          "Basic Plots", value = "basic",
          
          # add small white space ---------------------------------------------
          textOutput("placeholder0"),
          tags$head(
            tags$style("#placeholder0{font-size: 3px; color: white}")
          ),
          
          # basic plot: dropdown panel -----------------------------------------
          dropdown(
            # type of plot
            radioGroupButtons(
              "plotTypeBasic", "Choose a graph type:",
              choices = "Please select variable(s) first",
              justified = TRUE
            ),
            
            # graph settings
            conditionalPanel(
              "input.plotTypeBasic != 'Please select variable(s) first'",
              sliderInput(
                "subsample_basic", "Subsample Points", 
                min = 0, max = 1, value = 1
              )
            ),
            conditionalPanel(
              "input.plotTypeBasic == 'histogram'",
              sliderInput(
                "bins_basic", "Number of Bins", value = 15, min = 1, max = 50
              )
            ),
            conditionalPanel(
              "input.plotTypeBasic == 'density' | input.plotTypeBasic == 'histogram' | input.plotTypeBasic == 'scatterplot'",
              sliderInput(
                "alpha_basic", "Transparency", min = 0, max = 1, value = .7
              )
            ),
            conditionalPanel(
              "input.plotTypeBasic == 'scatterplot' | input.plotTypeBasic == 'line'",
              numericInput(
                "size_basic", "Point Size", value = 1, min = 0, max = 10
              )
            ),
            
            numericInput(
              "height_basic", "Plot Height (px)", value = 400
            ),
            
            # button settings
            circle = TRUE, status = "default", size = "sm",
            icon = icon("gear"), width = "300px", style = "material-circle",
            tooltip = tooltipOptions(title = "Click to see inputs")
          ),
          
          # add small white space ---------------------------------------------
          textOutput("placeholder1"),
          tags$head(
            tags$style("#placeholder1{font-size: 4px; color: white}")
          ),
          
          # basic plot ---------------------------------------------------------
          plotlyOutput("basicPlot", height = "100%"),
          
          br(), br()
        ),
        
        # tab: Pair Plots ---------------------------------------------------
        tabPanel(
          "Pair Plots", value = "pair",
          
          # add small white space ---------------------------------------------
          textOutput("placeholder2"),
          tags$head(
            tags$style("#placeholder2{font-size: 3px; color: white}")
          ),
          
          # pair plot: dropdown panel-------------------------------------------
          dropdown(
            # graph settings
            conditionalPanel(
              "(typeof input.vars_pairs !== 'undefined' && input.vars_pairs.length > 0)",
              sliderInput(
                "subsample_pairs", "Subsample Points", 
                min = 0, max = 1, value = 1
              ),
              sliderInput(
                "alpha_pairs", "Transparency", min = 0, max = 1, value = 1
              ),
              numericInput(
                "size_pairs", "Point Size", value = 1, min = 0, max = 10
              )
            ),
            
            numericInput(
              "height_pairs", "Plot Height (px)", value = 500
            ),
            
            # button settings
            circle = TRUE, status = "default", size = "sm",
            icon = icon("gear"), width = "300px", style = "material-circle",
            tooltip = tooltipOptions(title = "Click to see inputs")
          ),
          
          # add small white space ---------------------------------------------
          textOutput("placeholder3"),
          tags$head(
            tags$style("#placeholder3{font-size: 4px; color: white}")
          ),
          
          # pair plot ----------------------------------------------------------
          plotOutput("pairPlot", height = "auto"), # %>% withSpinner(),
          
          br(), br()
        ),
        
        # tab: PCA Plots ---------------------------------------------------
        tabPanel(
          "PCA Plots", value = "pca",
          # add small white space ---------------------------------------------
          textOutput("placeholder4"),
          tags$head(
            tags$style("#placeholder4{font-size: 3px; color: white}")
          ),
          # pca plot: dropdown panel-------------------------------------------
          dropdown(
            # graph settings
            conditionalPanel(
              "(typeof input.pcs !== 'undefined' && input.pcs.length > 0)",
              sliderInput(
                "subsample_pca", "Subsample Points", min = 0, max = 1, value = 1
              ),
              sliderInput(
                "alpha_pca", "Transparency", min = 0, max = 1, value = 1
              ),
              numericInput(
                "size_pca", "Point Size", value = 1, min = 0, max = 10
              )
            ),
            
            numericInput(
              "height_pca", "Plot Height (px)", value = 500
            ),
            
            # button settings
            circle = TRUE, status = "default", size = "sm",
            icon = icon("gear"), width = "300px", style = "material-circle",
            tooltip = tooltipOptions(title = "Click to see inputs")
          ),
          
          # add small white space ---------------------------------------------
          textOutput("placeholder5"),
          tags$head(
            tags$style("#placeholder5{font-size: 4px; color: white}")
          ),
          
          # pca plot ----------------------------------------------------------
          plotOutput("PCAPlot", height = "auto"), # %>% withSpinner(),
          
          br(), br()
        ),
        
        # tab: Heatmaps -------------------------------------------
        tabPanel(
          "Heatmaps", value = "heatmaps",
          # add small white space ---------------------------------------------
          textOutput("placeholder13"),
          tags$head(
            tags$style("#placeholder13{font-size: 3px; color: white}")
          ),
          # heatmaps: dropdown panel-------------------------------
          dropdown(
            prettyRadioButtons(
              "heatmap_cluster_x", "Cluster x by",
              choices = c("Hierarchical Clustering", "None"),
              status = "info", animation = "jelly", icon = icon("check")
            ),
            prettyRadioButtons(
              "heatmap_cluster_y", "Cluster y by",
              choices = c("Hierarchical Clustering", "None"),
              status = "info", animation = "jelly", icon = icon("check")
            ),
            conditionalPanel(
              "input.heatmap_cluster == 'Hierarchical Clustering'",
              pickerInput(
                "hclust_linkage_heatmap", "Linkage",
                choices = c("ward.D", "ward.D2", "single", "complete", 
                            "average", "mcquitty", "median", "centroid"),
                selected = "ward.D",
                options = list(size = 5)
              )
            ),
            prettyCheckboxGroup(
              "labels_heatmap", "Show Axis Labels",
              choices = c("x", "y"), selected = c("x", "y"), 
              status = "info", animation = "jelly", icon = icon("check")
            ),
            prettyRadioButtons(
              "color_theme_heatmap", "Color Theme",
              choices = c("Viridis - cool", "Viridis - warm", "Temperature"), 
              status = "info", animation = "jelly", icon = icon("check")
            ),
            prettyRadioButtons(
              "color_scale_heatmap", "Color Scale",
              choices = c("By magnitude", "By quantile"), 
              status = "info", animation = "jelly", icon = icon("check")
            ),
            conditionalPanel(
              "input.color_scale_heatmap == 'By quantile' & input.color_theme_heatmap != 'Temperature'",
              numericInput(
                "color_n_quantiles_heatmap", "Number of quantiles",
                value = 5, min = NA, max = NA, step = 1
              )
            ),
            prettyCheckbox(
              "coord_flip_heatmap", "Flip x and y axes",
              status = "info", animation = "jelly", icon = icon("check")
            ),
            
            numericInput(
              "height_heatmap", "Plot Height (px)", value = 500
            ),
            
            # button settings
            circle = TRUE, status = "default", size = "sm",
            icon = icon("gear"), width = "300px", style = "material-circle",
            tooltip = tooltipOptions(title = "Click to see inputs")
          ),
          
          # add small white space ---------------------------------------------
          textOutput("placeholder14"),
          tags$head(
            tags$style("#placeholder14{font-size: 4px; color: white}")
          ),
          
          # heatmaps -----------------------------------------------
          plotOutput("Heatmap", height = "auto"), #%>% withSpinner(),
          
          br(), br()
        ),
        
        # tab: Correlation Heatmaps -------------------------------------------
        tabPanel(
          "Correlation Heatmaps", value = "correlation",
          # add small white space ---------------------------------------------
          textOutput("placeholder8"),
          tags$head(
            tags$style("#placeholder8{font-size: 3px; color: white}")
          ),
          # correlation heatmaps: dropdown panel-------------------------------
          dropdown(
            prettyRadioButtons(
              "cor_cluster", "Cluster by",
              choices = c("Hierarchical Clustering", "None"),
              status = "info", animation = "jelly", icon = icon("check")
            ),
            conditionalPanel(
              "input.cor_cluster == 'Hierarchical Clustering'",
              pickerInput(
                "hclust_linkage", "Linkage",
                choices = c("ward.D", "ward.D2", "single", "complete", 
                            "average", "mcquitty", "median", "centroid"),
                selected = "ward.D",
                options = list(size = 5)
              )
            ),
            prettyCheckboxGroup(
              "labels_cor", "Show Axis Labels",
              choices = c("x", "y"), selected = c("x", "y"), 
              status = "info", animation = "jelly", icon = icon("check")
            ),
            prettyRadioButtons(
              "color_theme_cor", "Color Theme",
              choices = c("Viridis - cool", "Viridis - warm", "Temperature"), 
              status = "info", animation = "jelly", icon = icon("check")
            ),
            prettyRadioButtons(
              "color_scale_cor", "Color Scale",
              choices = c("By magnitude", "By quantile"), 
              status = "info", animation = "jelly", icon = icon("check")
            ),
            conditionalPanel(
              "input.color_scale_cor == 'By quantile' & input.color_theme_cor != 'Temperature'",
              numericInput(
                "color_n_quantiles_cor", "Number of quantiles",
                value = 5, min = NA, max = NA, step = 1
              )
            ),
            numericInput(
              "cor_text_size", "Correlation Text Size", value = 0, min = 0
            ),
            
            numericInput(
              "height_cor", "Plot Height (px)", value = 500
            ),
            
            # button settings
            circle = TRUE, status = "default", size = "sm",
            icon = icon("gear"), width = "300px", style = "material-circle",
            tooltip = tooltipOptions(title = "Click to see inputs")
          ),
          
          # add small white space ---------------------------------------------
          textOutput("placeholder9"),
          tags$head(
            tags$style("#placeholder9{font-size: 4px; color: white}")
          ),
          
          # correlation heatmap -----------------------------------------------
          plotOutput("CorrelationHeatmap", height = "auto"), #%>% withSpinner(),
          
          br(), br()
        )
      )
    )
  )
)

#################################### Server ####################################

# Define server logic
server <- function(input, output, session) {
  
  ### update inputs -----------------------------------------------------------
  
  # update data file type -----------------------------------------------------
  output$fileType <- reactive({
    substr(input$file$datapath,
           start = nchar(input$file$datapath) - 3,
           stop = nchar(input$file$datapath))
  })
  
  # update data file ------------------------------------------------------
  dataInput <- reactive({
    fileType <- substr(input$file$datapath, 
                       start = nchar(input$file$datapath) - 3,
                       stop = nchar(input$file$datapath))
    if (identical(fileType, character(0))) {
      fileType <- "iris"
    }
    switch(
      fileType,
      "iris" = iris,
      ".rds" = readRDS(input$file$datapath),
      ".csv" = read.csv(input$file$datapath, header = input$header),
      ".txt" = read.table(input$file$datapath, header = input$header, 
                          sep = input$sep)
    )
  })
  
  # update feature selection if dataInput() changes --------------------------
  observe({
    data <- dataInput()
    vars <- colnames(data)
    num_vars <- vars[sapply(data, class) == "numeric"]
    updatePickerInput(session, "var1", choices = c("None", vars))
    updatePickerInput(session, "var2", choices = c("None", vars))
    updatePickerInput(session, "vars_pairs", choices = vars)
    updatePickerInput(session, "vars_summary", choices = num_vars)
    updatePickerInput(session, "vars_cor", choices = num_vars)
    updatePickerInput(session, "vars_heatmap", choices = vars)
    updatePickerInput(session, "sample_heatmap", choices = rownames(data))
    updateSliderInput(session, "p_cor",
                      value = 0, min = 0, max = length(num_vars))
    updateSliderInput(session, "p_cor",
                      value = 0, min = 0, max = nrow(data))
    updatePickerInput(session, "color1", choices = c("None", vars))
    updatePickerInput(session, "color_pairs", choices = vars)
    updatePickerInput(session, "color_pca", choices = vars)
  })
  
  # update plot types if inputs change ---------------------------------------
  plotTypeReactive <- reactive({
    data <- dataInput()
    plot_types <- getPlotTypes(data, input$var1, input$var2)
  })
  observe({
    updateRadioGroupButtons(
      session, "plotTypeBasic",
      choices = plotTypeReactive())
  })
  
  ### data summary: text outputs --------------------------------------------
  output$n_output <- renderText({
    data <- dataInput()
    paste0("Number of samples: ", nrow(data))
  })
  output$p_output <- renderText({
    data <- dataInput()
    paste0("Number of features: ", ncol(data))
  })
  output$na_output <- renderText({
    data <- dataInput()
    paste0("Number of NAs: ", sum(is.na(data)))
  })
  output$const_output <- renderText({
    data <- dataInput()
    paste0("Number of constant columns: ",
           sum(apply(as.data.frame(data[, sapply(data, class) == "numeric"]), 
                     2, var) == 0, na.rm = T))
  })
  output$mean_output <- renderText({
    req(length(input$vars_summary) == 1)
    data <- dataInput() %>%
      select(input$vars_summary) 
    req(is.numeric(data[, 1]))
    paste0("Mean of ", input$vars_summary, ": ", 
           formatC(colMeans(data, na.rm = T), digits = 3, format = "g"))
  })
  output$var_output <- renderText({
    req(length(input$vars_summary) == 1)
    data <- dataInput() %>%
      select(input$vars_summary) 
    req(is.numeric(data[, 1]))
    paste0("Variance of ", input$vars_summary, ": ",
           formatC(apply(data, 2, var, na.rm = T), digits = 3, format = "g"))
  })
  
  ### data summary: plot outputs --------------------------------------------
  output$dist <- renderPlotly({
    req(input$vars_summary)
    req(input$height_summary)
    req(input$height_summary > 0)
    data <- dataInput() %>%
      select(input$vars_summary)
    
    if (length(input$vars_summary) == 1) {
      axis_label <- input$vars_summary
    } else {
      axis_label <- "Value"
    }
    
    # initialize empty plot
    plt <- ggplot(data) +
      labs(x = "", y = "") +
      myGGplotTheme()
    
    # make plot
    if (input$plotTypeSummary == "histogram") {
      plt <- plotHistogram(data = data, position = "identity", 
                           bins = input$bins_summary, 
                           alpha = input$alpha_summary) +
        labs(x = axis_label, title = "Overall Distribution")
    } else if (input$plotTypeSummary == "density") {
      plt <- plotDensity(data = data, alpha = input$alpha_summary) +
        labs(x = axis_label, title = "Overall Distribution")
    } else if (input$plotTypeSummary == "boxplot") {
      plt <- plotBoxplot(data = data, horizontal = T) +
        labs(y = axis_label, title = "Overall Distribution")
    }
    plt <- plt +
      myGGplotTheme(axis_title_size = 16, axis_text_size = 12,
                    legend_title_size = 14, legend_text_size = 12,
                    title_size = 18)
    ggplotly(plt, height = input$height_summary)
  })
  
  output$variances <- renderPlotly({
    req(input$vars_summary)
    req(length(input$vars_summary) > 1)
    req(input$height_summary)
    req(input$height_summary > 0)
    data <- dataInput() %>%
      select(input$vars_summary) %>%
      apply(., 2, var, na.rm = T) %>%
      as.data.frame()
    
    if (length(input$vars_summary) == 1) {
      axis_label <- paste(input$vars_summary, "Variance")
    } else {
      axis_label <- "Distribution of Variances"
    }
    
    # make plot
    if (input$plotTypeSummary == "histogram") {
      plt <- plotHistogram(data = data,
                           bins = input$bins_summary, position = "identity", 
                           alpha = input$alpha_summary) +
        labs(x = "Variance", title = axis_label)
    } else if (input$plotTypeSummary == "density") {
      plt <- plotDensity(data = data, alpha = input$alpha_summary) +
        labs(x = "Variance", title = axis_label)
    } else if (input$plotTypeSummary == "boxplot") {
      plt <- plotBoxplot(data = data, horizontal = T) +
        labs(y = "Variance", title = axis_label)
    }
    plt <- plt + 
      myGGplotTheme(axis_title_size = 16, axis_text_size = 12,
                    legend_title_size = 14, legend_text_size = 12,
                    title_size = 18)
    ggplotly(plt, height = input$height_summary)
  })
  
  output$means <- renderPlotly({
    req(input$vars_summary)
    req(length(input$vars_summary) > 1)
    req(input$height_summary)
    req(input$height_summary > 0)
    data <- dataInput() %>%
      select(input$vars_summary) %>%
      colMeans(., na.rm = T) %>%
      as.data.frame()
    
    if (length(input$vars_summary) == 1) {
      axis_label <- paste(input$vars_summary, "Mean")
    } else {
      axis_label <- "Distribution of Means"
    }
    
    # make plot
    if (input$plotTypeSummary == "histogram") {
      plt <- plotHistogram(data = data,
                           bins = input$bins_summary, position = "identity", 
                           alpha = input$alpha_summary) +
        labs(x = "Mean", title = axis_label)
    } else if (input$plotTypeSummary == "density") {
      plt <- plotDensity(data = data, alpha = input$alpha_summary) +
        labs(x = "Mean", title = axis_label)
    } else if (input$plotTypeSummary == "boxplot") {
      plt <- plotBoxplot(data = data, horizontal = T) +
        labs(y = "Mean", title = axis_label)
    }
    plt <- plt +
      myGGplotTheme(axis_title_size = 16, axis_text_size = 12,
                    legend_title_size = 14, legend_text_size = 12,
                    title_size = 18)
    ggplotly(plt, height = input$height_summary)
  })
  
  ### data table: plot outputs ----------------------------------------------
  output$table <- renderDT({
    data <- dataInput()
    num_cols <- which(sapply(data, function(x) class(x) == "numeric"))
    names(num_cols) <- NULL
    dt <- datatable(data,
                    options = list(pageLength = 25,
                                   scrollX = T, scrollCollapse = T),
                    filter = "top")
    if (input$digits != 0) {
      dt <- dt %>% formatSignif(columns = num_cols, digits = input$digits)
    }
    dt
  })
  
  ### basic plot: plot outputs -----------------------------------------------
  output$basicPlot <- renderPlotly({
    req(input$height_basic)
    req(input$height_basic > 0)
    
    # read in data
    data <- dataInput()
    
    # number of inputted vars
    num_vars <- (input$var1 != "" & input$var1 != "None") +
      (input$var2 != "" & input$var2 != "None") -
      (input$var1 == input$var2 & input$var1 != "" & input$var1 != "None")
    
    # retrive color variable
    color_str <- NULL
    color_label <- ""
    if (!is.null(input$color1)) {
      if (as.character(input$color1) != "" & 
          as.character(input$color1) != "None") {
        color_label <- as.character(input$color1)
        if (length(unique(data[, color_label])) == 2) {
          data$color <- as.factor(data[, color_label])
        } else {
          data$color <- data[, color_label]
        }
        color_str <- "color"
      }
    }
    
    # initialize empty plot
    plt <- ggplot(data) +
      labs(x = "", y = "") +
      myGGplotTheme()
    
    if (num_vars == 1) {  # make 1-d plot
      plt_df <- data %>%
        sample_frac(size = input$subsample_basic, replace = F) %>%
        rename(x = ifelse(input$var1 != "" & input$var1 != "None",
                          as.character(input$var1),
                          as.character(input$var2))) %>%
        mutate_if(is.character, as.factor)
      
      # x axis title
      xlab_title <- ifelse(input$var1 != "" & input$var1 != "None",
                           as.character(input$var1),
                           as.character(input$var2))
      
      # make plot
      if (input$plotTypeBasic == "histogram") {
        plt <- plotHistogram(data = plt_df, x.str = "x", fill.str = color_str,
                             bins = input$bins_basic, position = "identity", 
                             alpha = input$alpha_basic) +
          labs(x = xlab_title, fill = color_label)
      } else if (input$plotTypeBasic == "density") {
        plt <- plotDensity(data = plt_df, x.str = "x", fill.str = color_str, 
                           alpha = input$alpha_basic) +
          labs(x = xlab_title, fill = color_label)
      } else if (input$plotTypeBasic == "boxplot") {
        plt <- plotBoxplot(data = plt_df, x.str = "x", fill.str = color_str) +
          labs(y = xlab_title, fill = color_label)
      } else if (input$plotTypeBasic == "bar") {
        plt <- plotBarplot(data = plt_df, x.str = "x", fill.str = color_str) +
          labs(x = xlab_title, fill = color_label)
      }
    } else if (num_vars == 2) {  # make 2d plot
      plt_df <- data %>%
        sample_frac(size = input$subsample_basic, replace = F) %>%
        rename(x = as.character(input$var1), y = as.character(input$var2)) %>%
        mutate_if(is.character, as.factor)
      
      # number of factors
      num_factors <- is.factor(plt_df$x) + is.factor(plt_df$y)  
      
      if (num_factors == 2) {
        plt_df <- plt_df %>%
          rename(x1 = x, y1 = y)
        plt1 <- plotBarplot(data = plt_df, x.str = "x1", fill.str = color_str) +
          labs(x = as.character(input$var1), fill = color_label) +
          myGGplotTheme(axis_title_size = 14, axis_text_size = 12, 
                        legend_title_size = 14, legend_text_size = 12)
        plt2 <- plotBarplot(data = plt_df, x.str = "y1", fill.str = color_str) +
          labs(x = as.character(input$var2), fill = color_label) +
          myGGplotTheme(axis_title_size = 14, axis_text_size = 12,
                        legend_title_size = 14, legend_text_size = 12)
        plt <- ggarrange(plotlist = list(plt1, plt2),
                         nrow = 1, ncol = 2, common.legend = T)
      } else if (num_factors == 1) {
        plt <- plotBoxplot(data = plt_df, 
                           x.str = ifelse(is.factor(plt_df$x), "y", "x"),
                           y.str = ifelse(is.factor(plt_df$x), "x", "y"),
                           fill.str = color_str) +
          labs(x = ifelse(!is.factor(plt_df$x), 
                          as.character(input$var2), 
                          as.character(input$var1)), 
               y = ifelse(!is.factor(plt_df$x), 
                          as.character(input$var1), 
                          as.character(input$var2)),
               fill = color_label)
      } else {
        if (input$plotTypeBasic == "scatterplot") {
          plt <- plotScatter(data = plt_df, x.str = "x", y.str = "y", 
                             color.str = color_str, 
                             alpha = input$alpha_basic, 
                             size = input$size_basic) +
            labs(x = as.character(input$var1), y = as.character(input$var2),
                 color = color_label)
        } else if (input$plotTypeBasic == "line") {
          plt <- plotLine(data = plt_df, x.str = "x", y.str = "y", 
                          color.str = color_str,
                          alpha = input$alpha_basic, size = input$size_basic) +
            labs(x = as.character(input$var1), y = as.character(input$var2),
                 color = color_label)
        }
      }
    }
    
    if (num_vars == 2) {
      if (num_factors != 2) {
        plt <- plt + 
          myGGplotTheme(axis_title_size = 16, axis_text_size = 12,
                        legend_title_size = 14, legend_text_size = 12)
      }
    } else {
      plt <- plt + 
        myGGplotTheme(axis_title_size = 16, axis_text_size = 12,
                      legend_title_size = 14, legend_text_size = 12)
    }
    ggplotly(plt, height = input$height_basic)
  })
  
  ### pair plot: plot outputs -----------------------------------------------
  output$pairPlot <- renderPlot({
    req(input$height_pairs)
    req(input$height_pairs > 0)
    
    # read in data
    data <- dataInput()
    
    # retrieve number of colors
    num_colors <- 0
    if (!is.null(input$color_pairs)) {
      num_colors <- length(input$color_pairs)
    }
    
    # initialize empty plot
    plt <- ggplot(data) +
      labs(x = "", y = "") +
      myGGplotTheme()
    
    if (!is.null(input$vars_pairs)) {
      # variables to plot
      plt_idx <- which(colnames(data) %in% input$vars_pairs)  
      
      # convert binary variables to factors for plotting
      plt_df <- data %>%
        mutate_if(
          function(col) {return(length(unique(col)) == 2)},
          as.factor
        )
      
      # set variables for plotting
      color <- NULL
      color.label <- ""
      color2 <- NULL
      color2.label <- ""
      if (num_colors == 1) {
        color <- plt_df[, input$color_pairs]
        color.label <- input$color_pairs
      } else if (num_colors == 2) {
        color <- plt_df[, input$color_pairs[1]]
        color.label <- input$color_pairs[1]
        color2 <- plt_df[, input$color_pairs[2]]
        color2.label <- input$color_pairs[2]
      }
      
      # make pair plot
      plt <- plotPairs(data = plt_df, columns = plt_idx, 
                       color = color, color.label = color.label,
                       color2 = color2, color2.label = color2.label,
                       size = input$size_pairs, alpha = input$alpha_pairs, 
                       subsample = input$subsample_pairs,
                       axis_title_size = 14, axis_text_size = 10,
                       legend_title_size = 14, legend_text_size = 10,
                       strip_text_size = 14)
    }
    plt
  },
  height = function() input$height_pairs)
  
  ### pca plot: plot outputs -----------------------------------------------
  # only perform pca when data file changes
  pca_out <- reactive({
    req(input$pcs)
    
    data <- dataInput()
    
    # only perform PCA on numeric features
    X <- data %>% select_if(is.numeric)
    
    plotPCA(X = X, pcs = as.numeric(input$pcs),
            size = 1, alpha = 1, subsample = 1,
            center = "Center" %in% input$pca_options, 
            scale = "Scale" %in% input$pca_options)
  })
  
  output$PCAPlot <- renderPlot({
    req(input$height_pca)
    req(input$height_pca > 0)
    
    data <- dataInput()
    pca_data <- pca_out()
    
    # retrieve number of colors
    num_colors <- 0
    if (!is.null(input$color_pca)) {
      num_colors <- length(input$color_pca)
    }
    
    # set variables for plotting
    color <- NULL
    color.label <- ""
    color2 <- NULL
    color2.label <- ""
    if (num_colors == 1) {
      color <- data[, input$color_pca]
      color.label <- input$color_pca
    } else if (num_colors == 2) {
      color <- data[, input$color_pca[1]]
      color.label <- input$color_pca[1]
      color2 <- data[, input$color_pca[2]]
      color2.label <- input$color_pca[2]
    }
    
    # make pca plot
    plt <- plotPCA(pca.out = pca_data, pcs = as.numeric(input$pcs),
                   color = color, color.label = color.label,
                   color2 = color2, color2.label = color2.label,
                   size = input$size_pca, alpha = input$alpha_pca,
                   subsample = input$subsample_pca,
                   axis_title_size = 14, axis_text_size = 10,
                   legend_title_size = 14, legend_text_size = 10,
                   strip_text_size = 14)$plot
    plt
  },
  height = function() input$height_pca)
  
  ### heatmap: plot outputs ---------------------------------------
  # filter data for heatmap
  heatmapData <- reactive({
    data <- dataInput()
    
    if (input$vars_select_heatmap == "Manually") {
      req(input$vars_heatmap)
      data <- data %>% select(input$vars_heatmap)
    } else if (input$vars_select_heatmap == "Randomly") {
      req(input$p_heatmap > 0)
      num_vars <- which(sapply(data, class) == "numeric")
      if (length(num_vars) != input$p_heatmap) {
        data <- data[, sample(num_vars, input$p_heatmap, replace = F)]
      } else {
        data <- data[, num_vars]
      }
    }
    
    if (input$sample_select_heatmap == "Manually") {
      req(input$sample_heatmap)
      data <- data[input$sample_heatmap, ]
    } else if (input$sample_select_heatmap == "Randomly") {
      req(input$p_heatmap_rows > 0)
      if (nrow(data) != input$p_heatmap_rows) {
        keep_rows <- sort(sample(1:nrow(data), input$p_heatmap_rows, 
                                 replace = F))
        data <- data[keep_rows, ]
      }
    }
    data
  })
  
  # reorder the data matrix via hierarchical clustering
  clusterHeatmap <- reactive({
    data <- heatmapData()
    hclust_out_x <- hclust(d = dist(data), 
                           method = input$hclust_linkage_heatmap)
    hclust_out_y <- hclust(d = dist(t(data)), 
                           method = input$hclust_linkage_heatmap)
    list(x = hclust_out_x$order, y = hclust_out_y$order)
  })
  
  # plot correlation heatmap
  output$Heatmap <- renderPlot({
    req(input$height_heatmap)
    req(input$height_heatmap > 0)
    
    heatmap_data <- heatmapData()
    
    # reorder features by hierarchical clustering
    if (input$heatmap_cluster_x == "Hierarchical Clustering") {
      cluster_order <- clusterHeatmap()
      heatmap_data <- heatmap_data[cluster_order$x, ]
    }
    if (input$heatmap_cluster_y == "Hierarchical Clustering") {
      if (!exists("cluster_order")) {
        cluster_order <- clusterHeatmap()
      }
      heatmap_data <- heatmap_data[, cluster_order$y]
    }
    
    # color scheme options
    if (input$color_theme_heatmap == "Temperature") {
      manual.fill <- "temperature"
    } else {
      manual.fill <- NULL
      if (str_detect(input$color_theme_heatmap, "cool")) {
        viridis_option <- "D"
      } else {
        viridis_option <- "C"
      }
    }
    if (input$color_scale_heatmap == "By quantile") {
      col_quantile <- TRUE
    } else {
      col_quantile <- FALSE
    }
    
    plt <- plotHeatmap(X = heatmap_data, 
                       y.labels = rownames(heatmap_data), 
                       x.labels = colnames(heatmap_data), 
                       position = "identity", 
                       manual.fill = manual.fill, option = viridis_option,
                       col_quantile = col_quantile, 
                       n_quantiles = input$color_n_quantiles_heatmap,
                       x_text_angle = T, 
                       axis_text_size = 12, axis_title_size = 16,
                       legend_title_size = 14, legend_text_size = 12) +
      labs(x = "Features", y = "Samples", fill = "Value") 
    
    # additional plotting options
    if (!("x" %in% input$labels_heatmap)) {
      plt <- plt + theme(axis.text.x = element_blank(),
                         axis.ticks.x = element_blank())
    }
    if (!("y" %in% input$labels_heatmap)) {
      plt <- plt + theme(axis.text.y = element_blank(),
                         axis.ticks.y = element_blank())
    }
    if (input$coord_flip_heatmap) {
      plt <- plt + coord_flip()
    }
    plt
  },
  height = function() input$height_heatmap)
  
  ### correlation heatmap: plot outputs ---------------------------------------
  # compute correlation matrix
  corMat <- reactive({
    data <- dataInput()
    
    if (input$dim_select == "Rows") {
      req(input$p_cor_rows > 0)
      num_vars <- which(sapply(data, class) == "numeric")
      keep_rows <- sort(sample(1:nrow(data), input$p_cor_rows, replace = F))
      data <- data[keep_rows, num_vars] %>%
        t() %>%
        as.data.frame()
    } else {
      if (input$vars_select == "Manually") {
        req(input$vars_cor)
        data <- data %>% select(input$vars_cor)
      } else if (input$vars_select == "Randomly") {
        req(input$p_cor > 0)
        num_vars <- which(sapply(data, class) == "numeric")
        data <- data[, sample(num_vars, input$p_cor, replace = F)] %>%
          as.data.frame()
      }
    }
    
    cor(data, method = tolower(input$cor_type), use = "pairwise.complete.obs")
  })
  
  # reorder the correlation matrix via hierarchical clustering
  clusterCor <- reactive({
    cor_mat <- corMat()
    cor_dist <- as.dist(1 - abs(cor_mat))
    hclust_out <- hclust(d = cor_dist, method = input$hclust_linkage)
    hclust_out$order
  })
  
  # plot correlation heatmap
  output$CorrelationHeatmap <- renderPlot({
    req(input$height_cor)
    req(input$height_cor > 0)
    
    cor_mat <- corMat()
    if (input$dim_select == "Rows") {
      axis_label <- "Samples"
    } else if (input$dim_select == "Columns") {
      axis_label <- "Features"
    }
    
    if (input$cor_cluster == "Hierarchical Clustering") {
      # reorder features by hierarchical clustering
      cluster_order <- clusterCor()
      cor_mat <- cor_mat[cluster_order, cluster_order]
    }
    
    # round correlations for viz
    cor_mat <- round(cor_mat, 2)
    
    # color scheme options
    if (input$color_theme_cor == "Temperature") {
      manual.fill <- "temperature"
    } else {
      manual.fill <- NULL
      if (str_detect(input$color_theme_cor, "cool")) {
        viridis_option <- "D"
      } else {
        viridis_option <- "C"
      }
    }
    if (input$color_scale_cor == "By quantile") {
      col_quantile <- TRUE
    } else {
      col_quantile <- FALSE
    }
    
    plt <- plotHeatmap(X = cor_mat, 
                       y.labels = colnames(cor_mat), 
                       x.labels = colnames(cor_mat), 
                       text.size = input$cor_text_size,
                       position = "identity",
                       manual.fill = manual.fill, option = viridis_option,
                       col_quantile = col_quantile,
                       n_quantiles = input$color_n_quantiles_cor,
                       x_text_angle = T, 
                       axis_text_size = 12, axis_title_size = 16,
                       legend_title_size = 14, legend_text_size = 12) +
      labs(x = axis_label, y = axis_label, fill = "Cor.") 
    
    # additional plotting options
    if (!("x" %in% input$labels_cor)) {
      plt <- plt + theme(axis.text.x = element_blank(),
                         axis.ticks.x = element_blank())
    }
    if (!("y" %in% input$labels_cor)) {
      plt <- plt + theme(axis.text.y = element_blank(),
                         axis.ticks.y = element_blank())
    }
    if ((input$color_theme_cor == "Temperature") & (!col_quantile)) {
      plt <- plt + 
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(-1, 1))
    }
    
    plt
  },
  height = function() input$height_cor)
  
  ### miscellaneous buttons/widgets -------------------------------------------
  
  # placeholder for additional spacing ----------------------------------------
  output$placeholder0 <- renderText({return("placeholder")})
  output$placeholder1 <- renderText({return("placeholder")})
  output$placeholder2 <- renderText({return("placeholder")})
  output$placeholder3 <- renderText({return("placeholder")})
  output$placeholder4 <- renderText({return("placeholder")})
  output$placeholder5 <- renderText({return("placeholder")})
  output$placeholder6 <- renderText({return("placeholder")})
  output$placeholder7 <- renderText({return("placeholder")})
  output$placeholder8 <- renderText({return("placeholder")})
  output$placeholder9 <- renderText({return("placeholder")})
  output$placeholder10 <- renderText({return("placeholder")})
  output$placeholder11<- renderText({return("placeholder")})
  output$placeholder12 <- renderText({return("placeholder")})
  output$placeholder13 <- renderText({return("placeholder")})
  output$placeholder14 <- renderText({return("placeholder")})
  
  ### output options ---------------------------------------------------------
  outputOptions(output, "fileType", suspendWhenHidden = FALSE)  
}


############################## Shiny App Function ##############################
shinyApp(ui = ui, server = server)

