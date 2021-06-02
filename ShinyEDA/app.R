# shiny/html packages
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(htmltools)
# useful R packages
library(R.utils)
library(tidyverse)
library(rlang)
library(NMF)
library(pheatmap)
library(Rtsne)
library(umap)
# plotting packages
library(GGally)
library(ggpubr)
library(plotly)
# table packages
library(knitr)
library(kableExtra)
library(DT)
library(skimr)

for (fname in list.files("functions", pattern = ".R$")) {
  if (fname != "test.R") {
    source(paste0("functions/", fname), chdir = T)
  }
}

# starter data set
data(iris)
data <- iris

# increase max file size to 30Mb
options(shiny.maxRequestSize = 100 * 1024^2)  

###################################### UI ######################################
# Define UI for app
ui <- fluidPage(
  includeCSS("www/custom_style.css"),
  theme = shinytheme("flatly"),
  useShinyjs(),
  # setBackgroundColor("#ecf0f1"),
  # theme = shinytheme("cerulean"),
  # shinythemes::themeSelector(),
  titlePanel(
    tagList(
      div(HTML('<i class="fas fa-search"></i> &nbsp; <b> Shiny Exploratory Data Analysis </b>'),
          id = "title")
    ),
    windowTitle = "ShinyEDA"
  ),
  tags$div(class = "smallwhitespace", tags$p("whitespace")),
  
  sidebarLayout(
    # sidebar panel -----------------------------------------------------------
    sidebarPanel(id = "side-panel", width = 3,
                 tags$style(HTML(paste0(".well {min-height: 700px;}"))),
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
      
      # data table: variable inputs -------------------------------------------
      conditionalPanel(
        "input.tab == 'data_tab'",
        # variable inputs
        pickerInput(
          "vars_table", "Variables:",
          choices = colnames(data),
          selected = colnames(data),
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
          "var3", "Variable 3:",
          choices = c("None", colnames(data)),
          options = list(`live-search` = TRUE,
                         size = 5,
                         title = "Nothing selected")
        ),
        pickerInput(
          "color_basic", "Color by:",
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
      
      # dimension reduction: variable inputs -----------------------------------
      conditionalPanel(
        "input.tab == 'dimred'",
        
        # dimension reduction technique
        prettyRadioButtons(
          "dimred_type", "Dimension Reduction Method",
          choices = c("PCA", "NMF", "tSNE", "UMAP"),
          status = "info", animation = "jelly", 
          icon = icon("check"), bigger = TRUE, #inline = TRUE
        ),
        
        # variable inputs
        pickerInput(
          "vars_dimred", "Variables included:",
          choices = colnames(data)[sapply(data, is.numeric)],
          multiple = TRUE,
          selected = colnames(data)[sapply(data, is.numeric)],
          options = list(`live-search` = TRUE,
                         size = 5,
                         `selected-text-format` = "count > 3",
                         title = "Nothing selected",
                         multipleSeparator = ", ",
                         `actions-box` = TRUE)
        ),
        
        # omit NA and constant columns
        disabled(
          prettyCheckboxGroup(
            "vars_options_dimred", NULL,
            choices = c("Remove columns with NAs", 
                        "Remove constant columns"),
            selected = c("Remove columns with NAs", 
                         "Remove constant columns"), 
            status = "info", animation = "jelly", 
            icon = icon("check"), bigger = T
          )
        ),
        
        # pca options ---------------------------------------------------------
        conditionalPanel(
          "input.dimred_type == 'PCA'",
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
        
        # non-pca options ----------------------------------------------------
        conditionalPanel(
          "input.dimred_type !== 'PCA'",
          pickerInput(
            "color_dimred", "Color by:",
            choices = colnames(data),
            selected = NULL,
            options = list(`live-search` = TRUE,
                           size = 5,
                           title = "Nothing selected")
          )
        ),
        
        # tsne options -------------------------------------------------------
        conditionalPanel(
          "input.dimred_type == 'tSNE'",
          numericInput("perplexity_tsne", "Perplexity", value = 30,
                       min = 1, step = 5),
          numericInput("maxiter_tsne", "Max. # Iterations", value = 1000,
                       min = 1, step = 100),
          materialSwitch("pca_tsne", "Perform PCA before tSNE", value = TRUE,
                         status = "info"),
          conditionalPanel(
            "input.pca_tsne",
            prettyCheckboxGroup(
              "pca_tsne_options", "PCA Options:",
              choices = c("Center", "Scale"),
              selected = "Center", 
              status = "info", animation = "jelly", 
              icon = icon("check"), bigger = T
            )
          )
        ),
        
        # umap options -------------------------------------------------------
        conditionalPanel(
          "input.dimred_type == 'UMAP'",
          numericInput("perplexity_umap", "# Neighbors", value = 15,
                       min = 1, step = 1),
          numericInput("maxiter_umap", "Max. # Iterations", value = 200,
                       min = 1, step = 100),
        ),
        
        # nmf options -------------------------------------------------------
        conditionalPanel(
          "input.dimred_type == 'NMF'",
          numericInput("rank_nmf", "NMF Rank", value = 3, min = 1, step = 1)
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
            choices = colnames(data)[sapply(data, is.numeric)],
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
        ), 
        
        # omit NA and constant columns
        disabled(
          prettyCheckboxGroup(
            "vars_options_heatmap", NULL,
            choices = "Remove constant columns",
            selected = "Remove constant columns", 
            status = "info", animation = "jelly", 
            icon = icon("check"), bigger = T
          )
        ),
        
        # centering and scaling options
        prettyCheckboxGroup(
          "heatmap_options", "Additional Options:",
          choices = c("Center", "Scale"),
          selected = NULL, 
          status = "info", animation = "jelly", 
          icon = icon("check"), bigger = T
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
              choices = colnames(data)[sapply(data, is.numeric)],
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
        
        # omit NA and constant columns
        disabled(
          prettyCheckboxGroup(
            "vars_options_cor", NULL,
            choices = "Remove constant columns",
            selected = "Remove constant columns", 
            status = "info", animation = "jelly", 
            icon = icon("check"), bigger = T
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
    mainPanel(id = "main-panel", width = 9,
              div(HTML('<button class="bttn-collapse action-button bttn bttn-simple bttn-sm bttn-default bttn-no-outline" id="collapse_sidebar" type="button"><i class="fa fa-bars"></i></button>')),
      tabsetPanel(
        type = "tabs",
        id = "tab",
        # tab: Data Summary -------------------------------------------
        tabPanel(
          "Data Summary", value = "summary",
          
          # data summary text -----------------------------------------------
          br(),
          fluidRow(column(6, 
                          h4(htmlOutput("text_summary")) %>%
                            tagAppendAttributes(class = "box-border"),
                          tags$head(tags$style("#text_summary{line-height: 1.4em; min-height: 135px; display: flex; align-items: center;}"))),
                   column(6, 
                          uiOutput("dtypes") %>%
                            tagAppendAttributes(class = "box-border") %>%
                            withSpinner(color = "#18bc9c"))),
          
          hr(style = "border-top: 1px solid #23313E;"),
          
          # data summary tables  ----------------------------------------------
          dropdown(
            # table options
            numericInput(
              "digits_summary", "Digits",
              value = 1, min = 0, max = NA, step = 1
            ),
            prettyRadioButtons(
              "sigfig_summary", "Use Significant Digits",
              choices = c("Yes", "No"), selected = "No",
              status = "info", animation = "jelly", icon = icon("check")
            ),
            
            # button settings
            circle = TRUE, status = "default", size = "sm",
            icon = icon("gear"), width = "300px", style = "material-circle",
            tooltip = tooltipOptions(title = "Click to see inputs")
          ),
          
          uiOutput("summary_tables") %>% 
            tagAppendAttributes(class = "box-border") %>%
            withSpinner(color = "#18bc9c"),
          
          # data summary plot: dropdown panel ---------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          dropdown(
            # type of plot
            radioGroupButtons(
              "plotTypeSummary", "Choose a graph type:",
              choices = c(`<i class='fa fa-bar-chart'></i>` = "histogram",
                          `<img src="chart-density-white.png" width=15px height=13px><div class='jhr'></div></img>` = "density",
                          `<img src="chart-boxplot-white.png" width=15px height=13px><div class='jhr'></div></img>` = "boxplot"),
              justified = TRUE
            ),
            
            # variable inputs
            pickerInput(
              "vars_summary", "Variables:",
              choices = colnames(data)[sapply(data, is.numeric)],
              selected = colnames(data)[sapply(data, is.numeric)],
              multiple = TRUE,
              options = list(`live-search` = TRUE,
                             size = 5,
                             `selected-text-format` = "count > 3",
                             title = "Nothing selected",
                             multipleSeparator = ", ",
                             `actions-box` = TRUE)
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
            
            textInput("bg_summary", "Background color", value = "grey98"),
            prettyCheckbox(
              "grid_summary", "Show grid lines", value = TRUE,
              status = "info", animation = "jelly", icon = icon("check")
            ),
            numericInput("height_summary", "Plot Height (px)", value = 400),
            
            # button settings
            circle = TRUE, status = "default", size = "sm",
            icon = icon("gear"), width = "300px", style = "material-circle",
            tooltip = tooltipOptions(title = "Click to see inputs")
          ),
          
          # data summary plots ----------------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          uiOutput("dist") %>%
            tagAppendAttributes(class = "box-border") %>%
            withSpinner(color = "#18bc9c"),
          uiOutput("means") %>%
            tagAppendAttributes(class = "box-border") %>%
            withSpinner(color = "#18bc9c"),
          uiOutput("variances") %>%
            tagAppendAttributes(class = "box-border") %>%
            withSpinner(color = "#18bc9c"),
          br()
        ),
        
        # tab: Data Table ---------------------------------------------------
        tabPanel(
          "Data Table", value = "data_tab",
          
          # data table: dropdown panel------------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
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
          DTOutput("table")  %>% 
            withSpinner(color = "#18bc9c")
        ),
        # tab: Basic Plots ---------------------------------------------------
        tabPanel(
          "Basic Plots", value = "basic",
          
          # basic plot: dropdown panel -----------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
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
            
            textInput("bg_basic", "Background color", value = "grey98"),
            prettyCheckbox(
              "grid_basic", "Show grid lines", value = TRUE,
              status = "info", animation = "jelly", icon = icon("check")
            ),
            numericInput(
              "xtext_basic", "X-Axis Text Size", value = 12
            ),
            numericInput(
              "ytext_basic", "Y-Axis Text Size", value = 12
            ),
            # numericInput(
            #   "ztext_basic", "Z-Axis Text Size", value = 12
            # ),
            numericInput(
              "height_basic", "Plot Height (px)", value = 500
            ),
            
            # button settings
            circle = TRUE, status = "default", size = "sm",
            icon = icon("gear"), width = "300px", style = "material-circle",
            tooltip = tooltipOptions(title = "Click to see inputs")
          ),
          
          # basic plot ---------------------------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          uiOutput("basicPlot") %>%
            tagAppendAttributes(class = "box-border"),
          br(), br()
        ),
        
        # tab: Pair Plots ---------------------------------------------------
        tabPanel(
          "Pair Plots", value = "pair",
          
          # pair plot: dropdown panel-------------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
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
              ),
              numericInput(
                "corsize_pairs", "Correlation Text Size",
                value = 3.5, min = 0
              )
            ),
            
            textInput("bg_pairs", "Background color", value = "grey98"),
            prettyCheckbox(
              "grid_pairs", "Show grid lines", value = TRUE,
              status = "info", animation = "jelly", icon = icon("check")
            ),
            numericInput(
              "xytext_pairs", "XY-Axis Text Size", value = 10
            ),
            numericInput(
              "height_pairs", "Plot Height (px)", value = 500
            ),
            
            # button settings
            circle = TRUE, status = "default", size = "sm",
            icon = icon("gear"), width = "300px", style = "material-circle",
            tooltip = tooltipOptions(title = "Click to see inputs")
          ),
          
          # pair plot ----------------------------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          plotOutput("pairPlot", height = "auto")  %>% 
            tagAppendAttributes(class = "box-border") %>%
            withSpinner(color = "#18bc9c"),
          
          br(), br()
        ),
        
        # tab: Dimension Reduction Plots --------------------------------------
        tabPanel(
          "Dim. Reduction", value = "dimred",
          
          # dim red plot: dropdown panel---------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          dropdown(
            # plot settings
            sliderInput(
              "subsample_dimred", "Subsample Points", 
              min = 0, max = 1, value = 1
            ),
            
            # non-nmf settings
            conditionalPanel(
              "(input.dimred_type == 'PCA' && (typeof input.pcs !== 'undefined' && input.pcs.length > 0))| input.dimred_type == 'UMAP' | input.dimred_type == 'tSNE'",
              sliderInput(
                "alpha_dimred", "Transparency", min = 0, max = 1, value = 1
              ),
              numericInput(
                "size_dimred", "Point Size", value = 1, min = 0, max = 10
              )
            ),
            
            # nmf settings
            conditionalPanel(
              "input.dimred_type == 'NMF'",
              prettyRadioButtons(
                "color_nmf", "Color Theme",
                choices = c("Viridis - cool", "Viridis - warm", "YlOrRd"), 
                selected = "YlOrRd",
                status = "info", animation = "jelly", icon = icon("check")
              ),
              prettyRadioButtons(
                "nmf_cluster_x", "Cluster samples by",
                choices = c("Hierarchical Clustering", "None"),
                status = "info", animation = "jelly", icon = icon("check")
              ),
              prettyRadioButtons(
                "nmf_cluster_y", "Cluster features by",
                choices = c("Hierarchical Clustering", "None"),
                status = "info", animation = "jelly", icon = icon("check")
              ),
              conditionalPanel(
                "input.nmf_cluster_x == 'Hierarchical Clustering' | input.nmf_cluster_y == 'Hierarchical Clustering'",
                pickerInput(
                  "hclust_linkage_nmf", "Linkage",
                  choices = c("ward.D", "ward.D2", "single", "complete", 
                              "average", "mcquitty", "median", "centroid",
                              "correlation"),
                  selected = "ward.D",
                  options = list(size = 5)
                )
              ),
              prettyCheckboxGroup(
                "labels_nmf", "Show Labels",
                choices = c("Samples", "Features"), selected = "Features", 
                status = "info", animation = "jelly", icon = icon("check")
              )
            ),
            
            # ggplot background settings
            conditionalPanel(
              "(input.dimred_type == 'PCA' && (typeof input.pcs !== 'undefined' && input.pcs.length > 0))| input.dimred_type == 'UMAP' | input.dimred_type == 'tSNE'",
              textInput("bg_dimred", "Background color", value = "grey98"),
              prettyCheckbox(
                "grid_dimred", "Show grid lines", value = TRUE,
                status = "info", animation = "jelly", icon = icon("check")
              ),
            ),
            numericInput(
              "xytext_dimred", "XY-Axis Text Size", value = 12
            ),
            numericInput(
              "height_dimred", "Plot Height (px)", value = 500
            ),
            
            # button settings
            circle = TRUE, status = "default", size = "sm",
            icon = icon("gear"), width = "300px", style = "material-circle",
            tooltip = tooltipOptions(title = "Click to see inputs")
          ),
          
          # dim red plot ------------------------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          uiOutput("dimRedPlot") %>%
            tagAppendAttributes(class = "box-border") %>%
            withSpinner(color = "#18bc9c"),
          
          # additional pc plot outputs --------------------------------------
          conditionalPanel(
            "input.dimred_type == 'PCA'",
            # pc var plot dropdown panel -----------------------------------
            dropdown(
              materialSwitch("show_cum_var", tags$b("Show cumulative variance"), 
                             value = FALSE, status = "info"),
              numericInput("max_pc_show_var", "Max PC", value = 4),
              
              conditionalPanel(
                "input.show_cum_var",
                numericInput(
                  "point_size_pca", "Point Size", value = 1, min = 0,
                ),
                numericInput(
                  "line_width_pca", "Line Width", value = 1, min = 0,
                )
              ),
              
              textInput("bg_pca_var", "Background color", value = "grey98"),
              prettyCheckbox(
                "grid_pca_var", "Show grid lines", value = TRUE,
                status = "info", animation = "jelly", icon = icon("check")
              ),
              numericInput(
                "xtext_pca_var", "X-Axis Text Size", value = 12
              ),
              numericInput(
                "ytext_pca_var", "Y-Axis Text Size", value = 12
              ),
              numericInput(
                "height_pca_var", "Variance Plot Height (px)", value = 500
              ),
              
              # button settings
              circle = TRUE, status = "default", size = "sm",
              icon = icon("gear"), width = "300px", style = "material-circle",
              tooltip = tooltipOptions(title = "Click to see inputs")
            ),
            
            # pc variance plot -----------------------------------------------
            tags$div(class = "whitespace", tags$p("whitespace")),
            uiOutput("PCAVar") %>%
              tagAppendAttributes(class = "box-border") %>%
              withSpinner(color = "#18bc9c"),
            
            # pc heatmap loadings/scores plot dropdown panel ------------------
            tags$div(class = "whitespace", tags$p("whitespace")),
            dropdown(
              numericInput("max_pc_heatmap", "Max PC", value = 4),
              prettyRadioButtons(
                "pc_heatmap_fill", "Plot",
                choices = c("Loadings", "Scores"),
                selected = "Loadings",
                status = "info", animation = "jelly", icon = icon("check")
              ),
              
              prettyRadioButtons(
                "pc_heatmap_cluster", "Cluster by",
                choices = c("Hierarchical Clustering", "None"),
                status = "info", animation = "jelly", icon = icon("check")
              ),
              conditionalPanel(
                "input.pc_heatmap_cluster == 'Hierarchical Clustering'",
                pickerInput(
                  "hclust_linkage_pc_heatmap", "Linkage",
                  choices = c("ward.D", "ward.D2", "single", "complete", 
                              "average", "mcquitty", "median", "centroid"),
                  selected = "ward.D",
                  options = list(size = 5)
                )
              ),
              prettyRadioButtons(
                "color_theme_pc_heatmap", "Color Theme",
                choices = c("Viridis - cool", "Viridis - warm", "Temperature"), 
                status = "info", animation = "jelly", icon = icon("check")
              ),
              prettyCheckbox(
                "coord_flip_pc_heatmap", "Flip x and y axes",
                status = "info", animation = "jelly", icon = icon("check")
              ),
              
              numericInput(
                "xtext_pc_heatmap", "X-Axis Text Size", value = 12
              ),
              numericInput(
                "ytext_pc_heatmap", "Y-Axis Text Size", value = 12
              ),
              numericInput(
                "height_pca_heatmap", "Loadings Plot Height (px)", value = 400
              ),
              
              # button settings
              circle = TRUE, status = "default", size = "sm",
              icon = icon("gear"), width = "300px", style = "material-circle",
              tooltip = tooltipOptions(title = "Click to see inputs")
            ),
            
            # pc heatmap loadings/scores plot --------------------------------
            tags$div(class = "whitespace", tags$p("whitespace")),
            uiOutput("PCAHeatmap") %>%
              tagAppendAttributes(class = "box-border") %>%
              withSpinner(color = "#18bc9c"),
            br(), br()
          ),
        ),
        
        # tab: Heatmaps -------------------------------------------
        tabPanel(
          "Heatmaps", value = "heatmaps",
          # heatmaps: dropdown panel-------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
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
              "input.heatmap_cluster_x == 'Hierarchical Clustering' | input.heatmap_cluster_y == 'Hierarchical Clustering'",
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
              "xtext_heatmap", "X-Axis Text Size", value = 12
            ),
            numericInput(
              "ytext_heatmap", "Y-Axis Text Size", value = 12
            ),
            numericInput(
              "height_heatmap", "Plot Height (px)", value = 500
            ),
            
            # button settings
            circle = TRUE, status = "default", size = "sm",
            icon = icon("gear"), width = "300px", style = "material-circle",
            tooltip = tooltipOptions(title = "Click to see inputs")
          ),
          
          # heatmaps -----------------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          uiOutput("Heatmap") %>%
            tagAppendAttributes(class = "box-border"),
          br(), br()
        ),
        
        # tab: Correlation Heatmaps -------------------------------------------
        tabPanel(
          "Correlation Heatmaps", value = "correlation",
          # correlation heatmaps: dropdown panel-------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
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
              "xtext_cor", "X-Axis Text Size", value = 12
            ),
            numericInput(
              "ytext_cor", "Y-Axis Text Size", value = 12
            ),
            numericInput(
              "height_cor", "Plot Height (px)", value = 500
            ),
            
            # button settings
            circle = TRUE, status = "default", size = "sm",
            icon = icon("gear"), width = "300px", style = "material-circle",
            tooltip = tooltipOptions(title = "Click to see inputs")
          ),
          
          # correlation heatmap -----------------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          uiOutput("CorrelationHeatmap") %>%
            tagAppendAttributes(class = "box-border"),
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
      "iris" = data,
      ".rds" = readRDS(input$file$datapath),
      ".csv" = read.csv(input$file$datapath, header = input$header, 
                        check.names = F),
      ".txt" = read.table(input$file$datapath, header = input$header, 
                          sep = input$sep, check.names = F)
    )
    
  })
  
  # update feature selection if dataInput() changes --------------------------
  observe({
    data <- dataInput()
    
    # all variable names
    vars <- colnames(data)
    # numeric variable names
    num_vars <- vars[sapply(data, is.numeric)]
    # NA column variable names
    na_vars <- vars[apply(data, 2, FUN = function(x) any(is.na(x)))]
    # constant column variable names
    const_vars <- vars[apply(data, 2, FUN = function(x) {
      if (!all(is.na(x))) {
        return(all(x == x[!is.na(x)][1], na.rm = T))
      } else {
        return(FALSE)
      }
    })]
    
    # update variables
    updatePickerInput(session, "vars_table", choices = vars, selected = vars)
    updatePickerInput(session, "var1", choices = c("None", vars))
    updatePickerInput(session, "var2", choices = c("None", vars))
    updatePickerInput(session, "var3", choices = c("None", vars))
    updatePickerInput(session, "vars_pairs", choices = vars)
    updatePickerInput(session, "vars_summary", choices = num_vars,
                      selected = num_vars)
    updatePickerInput(session, "vars_dimred", 
                      choices = setdiff(num_vars, c(const_vars, na_vars)), 
                      selected = setdiff(num_vars, c(const_vars, na_vars)))
    updatePickerInput(session, "vars_cor", 
                      choices = setdiff(num_vars, const_vars))
    updatePickerInput(session, "vars_heatmap", 
                      choices = setdiff(num_vars, const_vars))
    updatePickerInput(session, "sample_heatmap", choices = rownames(data))
    updateSliderInput(session, "p_cor", value = 0, 
                      min = 0, max = length(setdiff(num_vars, const_vars)))
    updateSliderInput(session, "p_cor_rows",
                      value = 0, min = 0, max = nrow(data))
    updateSliderInput(session, "p_heatmap", value = 0, 
                      min = 0, max = length(setdiff(num_vars, const_vars)))
    updateSliderInput(session, "p_heatmap_rows",
                      value = 0, min = 0, max = nrow(data))
    updatePickerInput(session, "color_basic", choices = c("None", vars))
    updatePickerInput(session, "color_pairs", choices = vars)
    updatePickerInput(session, "color_pca", choices = vars)
    updatePickerInput(session, "color_dimred", choices = vars)
  })
  
  # update plot types if inputs change ---------------------------------------
  plotTypeReactive <- reactive({
    data <- dataInput()
    plot_types <- getPlotTypes(data, c(input$var1, input$var2, input$var3))
  })
  observe({
    updateRadioGroupButtons(
      session, "plotTypeBasic",
      choices = plotTypeReactive())
  })
  
  ### helper functions ------------------------------------------------------
  addTheme <- function(plt, plotly = F, ...) {
    if (plotly) {
      plt <- plt + myGGplotTheme(axis_title_size = 16, axis_text_size = 12,
                                 legend_title_size = 14, legend_text_size = 12,
                                 title_size = 18, axis_line_width = 2.5, ...)
    } else {
      plt <- plt + myGGplotTheme(axis_title_size = 14, axis_text_size = 10,
                                 legend_title_size = 14, legend_text_size = 10,
                                 strip_text_size = 14, ...)
    }
    return(plt)
  }
  ### data summary: text outputs --------------------------------------------
  output$text_summary <- renderText({
    data <- dataInput()
    
    text_out <- paste0(paste0("Number of samples: ", nrow(data), "<br/>"),
                       paste0("Number of features: ", ncol(data), "<br/>"),
                       paste0("Number of NAs: ", sum(is.na(data)), "<br/>"),
                       paste0("Number of columns with NAs: ", 
                              sum(apply(data, 2, 
                                        FUN = function(x) any(is.na(x)))), 
                              "<br/>"),
                       paste0("Number of constant columns: ",
                              sum(apply(data, 2,
                                        FUN = function(x) {
                                          all(x == x[!is.na(x)][1], na.rm = T)
                                        }), na.rm = T)))
    text_out
  })
  
  ### data summary: table outputs --------------------------------------------
  skimData <- reactive({
    data <- dataInput()
    skim(data)
  })
  
  output$dtypes_table <- renderDT({
    skim_out <- skimData()
    dtypes_df <- data.frame(table(skim_out$skim_type)) %>%
      mutate(Var1 = capitalize(Var1)) %>%
      column_to_rownames("Var1") %>%
      t()
    DT::datatable(dtypes_df, rownames = F,
                  caption = tags$caption(
                    style = "color: black; font-weight: bold; font-size: 125%",
                    "Frequency of Column Types"
                  ), 
                  options = list(
                    columnDefs = list(list(className = "dt-center",
                                           targets = "_all")),
                    dom = "t"
                  ))
  })
  output$dtypes <- renderUI({
    fluidPage(DTOutput("dtypes_table"),
              tags$div(class = "whitespace", tags$p("whitespace")))
  })
  
  skimWrapper <- function(skim_out, dtype, digits, sigfig) {
    if (sigfig  == "Yes") {
      sigfig <- "g"
    } else {
      sigfig <- "f"
    }
    
    skim_df <- skim_out %>%
      filter(skim_type == dtype) %>%
      mutate(complete_rate = formatC((1 - complete_rate) * 100, digits = digits,
                                     format = sigfig, flag = "#"))
    
    keep_cols <- c("Variable Name" = "skim_variable",
                   "# Missing" = "n_missing",
                   "% Missing" = "complete_rate")
    
    if (dtype == "factor") {
      skim_df <- skim_df %>%
        mutate(factor.ordered = capitalize(tolower(factor.ordered)))
      keep_cols <- c(keep_cols,
                     "Ordered Factor" = "factor.ordered",
                     "# Unique Factors" = "factor.n_unique",
                     "Top Factor Counts" = "factor.top_counts")
    } else if (dtype == "numeric") {
      skim_df <- skim_df %>%
        mutate_at(vars(c("numeric.mean", "numeric.sd",
                         "numeric.p0", "numeric.p25", "numeric.p50",
                         "numeric.p75", "numeric.p100")),
                  formatC, digits = digits, format = sigfig, flag = "#")
      keep_cols <- c(keep_cols,
                     "Mean" = "numeric.mean",
                     "SD" = "numeric.sd",
                     "Minimum" = "numeric.p0",
                     "Q1" = "numeric.p25",
                     "Median" = "numeric.p50",
                     "Q3" = "numeric.p75",
                     "Maximum" = "numeric.p100",
                     "Histogram" = "numeric.hist")
    } else if (dtype == "character") {
      keep_cols <- c(keep_cols,
                     "# Empty" = "character.empty",
                     "Min Length" = "character.min",
                     "Max Length" = "character.max",
                     "# Unique" = "character.n_unique",
                     "Whitespace" = "character.whitespace")
    } else if (dtype == "logical") {
      skim_df <- skim_df %>%
        mutate(logical.count = str_replace(str_replace(logical.count, 
                                                       "FAL", "False"),
                                           "TRU", "True"))
      keep_cols <- c(keep_cols,
                     "Mean" = "logical.mean",
                     "Count" = "logical.count")
    } else if (dtype == "complex") {
      keep_cols <- c(keep_cols, "Mean" = "complex.mean")
    } else if ((dtype == "Date") | (dtype == "POSIXct")) {
      keep_cols <- c(keep_cols,
                     "Minimum" = paste0(dtype, ".min"),
                     "Maximum" = paste0(dtype, ".max"),
                     "Median" = paste0(dtype, ".median"),
                     "# Unique" = paste0(dtype, ".n_unique"))
    }
    
    grouped <- !identical(attr(skim_out, "groups"), list())
    if (grouped) {
      keep_cols <- c(keep_cols[1],
                     map_chr(attr(skim_out, "groups"), ~as_string(.x)),
                     keep_cols[2:length(keep_cols)])
    }
    
    if (nrow(skim_df) > 10) {
      dt_dom <- "tip"
    } else {
      dt_dom <- "t"
    }
    dt_out <- DT::datatable(skim_df %>% select(keep_cols),
                            caption = tags$caption(
                              style = "color: black; font-weight: bold; font-size: 125%",
                              paste("Summary of", capitalize(dtype),
                                    "Variables")
                            ), 
                            rownames = F,
                            options = list(
                              columnDefs = list(list(className = "dt-center",
                                                     targets = "_all")),
                              dom = dt_dom,
                              scrollX = TRUE
                            ))
    return(dt_out)
  }
  
  output$factor_table <- renderDT({
    skim_out <- skimData()
    req(sum(skim_out$skim_type == "factor") > 0)
    skimWrapper(skim_out, "factor", 
                input$digits_summary, input$sigfig_summary)
  })
  
  output$numeric_table <- renderDT({
    skim_out <- skimData()
    req(sum(skim_out$skim_type == "numeric") > 0)
    skimWrapper(skim_out, "numeric",
                input$digits_summary, input$sigfig_summary)
  })
  
  output$character_table <- renderDT({
    skim_out <- skimData()
    req(sum(skim_out$skim_type == "character") > 0)
    skimWrapper(skim_out, "character",
                input$digits_summary, input$sigfig_summary)
  })
  
  output$logical_table <- renderDT({
    skim_out <- skimData()
    req(sum(skim_out$skim_type == "logical") > 0)
    skimWrapper(skim_out, "logical",
                input$digits_summary, input$sigfig_summary)
  })
  
  output$complex_table <- renderDT({
    skim_out <- skimData()
    req(sum(skim_out$skim_type == "complex") > 0)
    skimWrapper(skim_out, "complex",
                input$digits_summary, input$sigfig_summary)
  })
  
  output$Date <- renderDT({
    skim_out <- skimData()
    req(sum(skim_out$skim_type == "Date") > 0)
    skimWrapper(skim_out, "Date",
                input$digits_summary, input$sigfig_summary)
  })
  
  output$POSIXct <- renderDT({
    skim_out <- skimData()
    req(sum(skim_out$skim_type == "POSIXct") > 0)
    skimWrapper(skim_out, "POSIXct",
                input$digits_summary, input$sigfig_summary)
  })
  
  output$summary_tables <- renderUI({
    skim_out <- skimData()
    out <- NULL
    if (sum(skim_out$skim_type == "numeric") > 0) {
      out <- c(out, DTOutput("numeric_table"), list(br()))
    }
    if (sum(skim_out$skim_type == "factor") > 0) {
      out <- c(out, DTOutput("factor_table"), list(br()))
    }
    if (sum(skim_out$skim_type == "character") > 0) {
      out <- c(out, DTOutput("character_table"), list(br()))
    }
    if (sum(skim_out$skim_type == "logical") > 0) {
      out <- c(out, DTOutput("logical_table"), list(br()))
    }
    if (sum(skim_out$skim_type == "complex") > 0) {
      out <- c(out, DTOutput("complex_table"), list(br()))
    }
    if (sum(skim_out$skim_type == "complex") > 0) {
      out <- c(out, DTOutput("Date_table"), list(br()))
    }
    if (sum(skim_out$skim_type == "POSIXct") > 0) {
      out <- c(out, DTOutput("POSIXct_table"), list(br()))
    }
    fluidPage(out)
  })
  
  ### data summary: plot outputs --------------------------------------------
  makeDistPlot <- reactive({
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
    plt
  })
  output$dist_plot <- renderPlotly({
    plt <- makeDistPlot()
    plt <- addTheme(plt, plotly = T, background_color = input$bg_summary, 
                    grid_color = ifelse(input$grid_summary, 
                                        "grey90", input$bg_summary))
    ggplotly(plt, height = input$height_summary, dynamicTicks = T)
  })
  output$dist <- renderUI({
    fluidPage(plotlyOutput("dist_plot", height = "100%"))
  })
  
  makeVarPlot <- reactive({
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
    plt
  })
  output$variances_plot <- renderPlotly({
    plt <- makeVarPlot()
    plt <- addTheme(plt, plotly = T, background_color = input$bg_summary, 
                    grid_color = ifelse(input$grid_summary, 
                                        "grey90", input$bg_summary))
    ggplotly(plt, height = input$height_summary, dynamicTicks = T)
  })
  output$variances <- renderUI({
    fluidPage(plotlyOutput("variances_plot", height = "100%"))
  })
  
  makeMeanPlot <- reactive({
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
    plt
  })
  output$means_plot <- renderPlotly({
    plt <- makeMeanPlot()
    plt <- addTheme(plt, plotly = T, background_color = input$bg_summary, 
                    grid_color = ifelse(input$grid_summary, 
                                        "grey90", input$bg_summary))
    ggplotly(plt, height = input$height_summary, dynamicTicks = T)
  })
  output$means <- renderUI({
    fluidPage(plotlyOutput("means_plot", height = "100%"))
  })
  
  ### data table: plot outputs ----------------------------------------------
  output$table <- renderDT({
    req(input$vars_table)
    data <- dataInput()
    
    data <- data %>%
      select(input$vars_table)
    
    num_cols <- which(sapply(data, 
                             FUN = function(x) is.numeric(x) & !is.integer(x)))
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
  makeBasicPlot <- reactive({
    req(input$height_basic)
    req(input$height_basic > 0)
    
    # read in data
    data <- dataInput()
    
    # number of inputted vars
    vars <- setdiff(c(input$var1, input$var2, input$var3), c("", "None"))
    num_vars <- length(vars)
    
    # retrive color variable
    color_str <- NULL
    color_label <- ""
    if (!is.null(input$color_basic)) {
      if (!(as.character(input$color_basic) %in% c("", "None"))) {
        color_label <- as.character(input$color_basic)
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
        rename(x = vars) %>%
        mutate_if(is.character, as.factor)
      
      # x axis title
      xlab_title <- vars
      
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
        rename(x = vars[1], y = vars[2]) %>%
        mutate_if(is.character, as.factor)
      
      # number of factors
      num_factors <- is.factor(plt_df$x) + is.factor(plt_df$y)  
      
      if (num_factors == 2) {
        plt_df <- plt_df %>%
          rename(x1 = x, y1 = y)
        plt1 <- plotBarplot(data = plt_df, x.str = "x1", fill.str = color_str) +
          labs(x = as.character(input$var1), fill = color_label) +
          myGGplotTheme(axis_title_size = 14, axis_text_size = 12, 
                        legend_title_size = 14, legend_text_size = 12,
                        axis_line_width = 2)
        plt2 <- plotBarplot(data = plt_df, x.str = "y1", fill.str = color_str) +
          labs(x = as.character(input$var2), fill = color_label) +
          myGGplotTheme(axis_title_size = 14, axis_text_size = 12,
                        legend_title_size = 14, legend_text_size = 12,
                        axis_line_width = 2)
        plt <- list(plt1, plt2)
      } else if (num_factors == 1) {
        plt <- plotBoxplot(data = plt_df, 
                           x.str = ifelse(is.factor(plt_df$x), "y", "x"),
                           y.str = ifelse(is.factor(plt_df$x), "x", "y"),
                           fill.str = color_str) +
          labs(x = ifelse(!is.factor(plt_df$x), vars[2], vars[1]), 
               y = ifelse(!is.factor(plt_df$x), vars[1], vars[2]),
               fill = color_label)
      } else {
        if (input$plotTypeBasic == "scatterplot") {
          plt <- plotScatter(data = plt_df, x.str = "x", y.str = "y", 
                             color.str = color_str, 
                             alpha = input$alpha_basic, 
                             size = input$size_basic) +
            labs(x = vars[1], y = vars[2], color = color_label)
        } else if (input$plotTypeBasic == "line") {
          plt <- plotLine(data = plt_df, x.str = "x", y.str = "y", 
                          color.str = color_str,
                          alpha = input$alpha_basic, size = input$size_basic) +
            labs(x = vars[1], y = vars[2], color = color_label)
        }
      }
    } else if (num_vars == 3) {  # make 3d plot
      plt_df <- data %>%
        sample_frac(size = input$subsample_basic, replace = F) %>%
        rename(x = vars[1], y = vars[2], z = vars[3], color = color_str) %>%
        mutate_if(is.character, as.factor)
      
      if (!is.null(color_str)) {
        plt <- plot_ly(plt_df, x = ~x, y = ~y, z = ~z, color = ~color,
                       marker = list(size = input$size_basic,
                                     opacity = input$alpha_basic)) %>%
          add_markers() %>%
          colorbar(title = input$color_basic) %>%
          layout(scene = list(xaxis = list(title = vars[1]),
                              yaxis = list(title = vars[2]),
                              zaxis = list(title = vars[3])))
      } else {
        plt <- plot_ly(plt_df, x = ~x, y = ~y, z = ~z,
                       marker = list(size = input$size_basic,
                                     opacity = input$alpha_basic)) %>%
          add_markers() %>%
          layout(scene = list(xaxis = list(title = vars[1]),
                              yaxis = list(title = vars[2]),
                              zaxis = list(title = vars[3])))
      }
    }
    
    if (num_vars == 2) {
      if (num_factors != 2) {
        plt <- plt + 
          myGGplotTheme(axis_title_size = 16, axis_text_size = 12,
                        legend_title_size = 14, legend_text_size = 12,
                        axis_line_width = 2.5)
      }
    } else if (num_vars < 3) {
      plt <- plt + 
        myGGplotTheme(axis_title_size = 16, axis_text_size = 12,
                      legend_title_size = 14, legend_text_size = 12,
                      axis_line_width = 2.5)
    }
    plt
  })
  output$basicPlot1 <- renderPlotly({
    plt <- makeBasicPlot()
    if (length(plt) == 2) {
      plt <- plt[[1]]
    } 
    if ("ggplot" %in% class(plt)) {
      plt <- plt + theme(
        panel.background = element_rect(fill = input$bg_basic),
        panel.grid.major = element_line(colour = ifelse(input$grid_basic, 
                                                        "grey90", 
                                                        input$bg_basic),
                                        size = rel(0.5)),
        axis.text.x = element_text(size = input$xtext_basic),
        axis.text.y = element_text(size = input$ytext_basic)
      )
      return(ggplotly(plt, height = input$height_basic, dynamicTicks = T))
    } else {
      return(plt %>% layout(height = input$height_basic))
    }
  })
  output$basicPlot2 <- renderPlotly({
    plt <- makeBasicPlot()
    if (length(plt) == 2) {
      plt <- plt[[2]]
    } 
    plt <- plt + theme(
      panel.background = element_rect(fill = input$bg_basic),
      panel.grid.major = element_line(colour = ifelse(input$grid_basic, 
                                                      "grey90", input$bg_basic),
                                      size = rel(0.5)),
      axis.text.x = element_text(size = input$xtext_basic),
      axis.text.y = element_text(size = input$ytext_basic)
    )
    ggplotly(plt, height = input$height_basic, dynamicTicks = T)
  })
  output$basicPlot <- renderUI({
    
    # read in data
    data <- dataInput()
    
    # number of inputted vars
    vars <- setdiff(c(input$var1, input$var2, input$var3), c("", "None"))
    num_vars <- length(vars)
    
    # number of factors
    num_factors <- data %>%
      select(vars) %>%
      select_if(is.factor) %>%
      ncol()
    
    if ((num_vars == 2) & (num_factors == 2)) {
      fluidRow(column(6, plotlyOutput("basicPlot1", height = "100%")),
               column(6, plotlyOutput("basicPlot2", height = "100%")))
    } else if ((num_vars == 3) & (num_factors != 0)) {
      h4("3D scatter plot only accepts numeric variables. Please change variable selection.")
    } else {
      fluidPage(plotlyOutput("basicPlot1", height = "100%"))
    }
  })  
  
  ### pair plot: plot outputs -----------------------------------------------
  makePairPlot <- reactive({
    req(input$height_pairs)
    req(input$height_pairs > 0)
    req(input$vars_pairs)
    
    # read in data
    data <- dataInput()
    
    # retrieve number of colors
    num_colors <- 0
    if (!is.null(input$color_pairs)) {
      num_colors <- length(input$color_pairs)
    }
    
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
                     cor.text.size = input$corsize_pairs,
                     subsample = input$subsample_pairs,
                     axis_title_size = 14, axis_text_size = 10,
                     legend_title_size = 14, legend_text_size = 10,
                     strip_text_size = 14, 
                     background_color = input$bg_pairs,
                     grid_color = ifelse(input$grid_pairs, 
                                         "grey90", input$bg_pairs))
    plt
  })
  output$pairPlot <- renderPlot({
    if (!is.null(input$vars_pairs)) {
      plt <- makePairPlot() +
        theme(axis.text = element_text(size = input$xytext_pairs))
    } else {
      # initialize empty plot
      plt <- ggplot(data) +
        labs(x = "", y = "") +
        myGGplotTheme()
    }
    plt
  },
  height = function() input$height_pairs)
  
  ### dim red plot: plot outputs ---------------------------------------------
  ## pca: plot outputs --------------------------------------------------------
  # only perform pca if selected and if data file changes
  get_max_pc <- reactive({
    max(input$max_pc_show_var, input$max_pc_heatmap)
  })
  pca_plot_out <- reactive({
    req(input$pcs)
    
    data <- dataInput()
    
    # only perform PCA on numeric features
    X <- data %>% select(input$vars_dimred)
    
    plotPCA(X = X, pcs = as.numeric(input$pcs),
            size = 1, alpha = 1, subsample = 1,
            center = "Center" %in% input$pca_options, 
            scale = "Scale" %in% input$pca_options)
  })
  pca_out <- reactive({
    req(input$vars_dimred)
    req(input$max_pc_show_var > 0)
    
    data <- dataInput()
    
    # only perform PCA on numeric features
    X <- data %>% 
      select(input$vars_dimred) %>%
      scale(center = "Center" %in% input$pca_options,
            scale = "Scale" %in% input$pca_options)
    
    max_pc <- get_max_pc()
    
    if (max_pc / min(nrow(X), ncol(X)) > .25) {  # do full svd
      X_svd <- svd(X)
    } else {
      X_svd <- irlba(X, nu = max_pc, nv = max_pc)
    }
    
    total_var <- norm(as.matrix(X), "F")^2
    var_explained <- X_svd$d^2 / total_var
    var_explained <- var_explained[1:min(max_pc, length(var_explained))]
    
    scores <- X_svd$u
    loadings <- X_svd$v
    rownames(scores) <- rownames(X)
    rownames(loadings) <- colnames(X)
    
    list(var_explained = var_explained, scores = scores, loadings = loadings)
  })
  
  # make dimension reduction plot for pca
  makePCAPlot <- reactive({
    req(input$height_dimred)
    req(input$height_dimred > 0)
    req(input$dimred_type == "PCA")
    
    data <- dataInput()
    pca_data <- pca_plot_out()
    
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
      color <- data %>% pull(input$color_pca)
      color.label <- input$color_pca
    } else if (num_colors == 2) {
      color <- data %>% pull(input$color_pca[1])
      color.label <- input$color_pca[1]
      color2 <- data %>% pull(input$color_pca[2])
      color2.label <- input$color_pca[2]
    }
    
    # make pca plot
    plt <- plotPCA(pca.out = pca_data, pcs = as.numeric(input$pcs),
                   color = color, color.label = color.label,
                   color2 = color2, color2.label = color2.label,
                   size = input$size_dimred, alpha = input$alpha_dimred,
                   subsample = input$subsample_dimred,
                   axis_title_size = 16, axis_text_size = 12,
                   legend_title_size = 14, legend_text_size = 12,
                   strip_text_size = 16)$plot
    plt
  })
  output$PCAPlot <- renderPlot({
    plt <- makePCAPlot()
    plt <- plt + theme(
      panel.background = element_rect(fill = input$bg_dimred),
      panel.grid.major = element_line(colour = ifelse(input$grid_dimred,
                                                      "grey90", 
                                                      input$bg_dimred),
                                      size = rel(0.5)),
      axis.text = element_text(size = input$xytext_dimred)
    )
    plt
  },
  height = function() input$height_dimred)
  
  # pca variance explained plot
  makePCAVarPlot <- reactive({
    req(input$height_pca_var)
    req(input$height_pca_var > 0)
    req(input$dimred_type == "PCA")
    
    var_explained <- pca_out()$var_explained
    var_explained <- var_explained[1:min(input$max_pc_show_var, 
                                         length(var_explained))]
    
    if (input$show_cum_var) {  # plot cumulative variance
      plt_df <- data.frame(PC = 1:length(var_explained),
                           Var = cumsum(var_explained))
      plt <- ggplot(plt_df) +
        aes(x = PC, y = Var) +
        geom_point(size = input$point_size_pca, color = "blue") +
        geom_line(size = input$line_width_pca, color = "blue") +
        labs(x = "PC", y = "Cumulative Variance Explained")
    } else {  # plot marginal variance
      plt_df <- data.frame(PC = 1:length(var_explained),
                           Var = var_explained)
      plt <- ggplot(plt_df) +
        aes(x = PC, y = Var) +
        geom_bar(stat = "identity", fill = "#6FBBE3") +
        labs(x = "PC", y = "Prop. of Variance Explained")
    }
    
    plt <- plt + 
      myGGplotTheme(axis_title_size = 14, axis_text_size = 10,
                    axis_line_width = 2.5)
    
  })
  output$PCAVarPlot <- renderPlotly({
    plt <- makePCAVarPlot()
    plt <- plt + theme(
      panel.background = element_rect(fill = input$bg_pca_var),
      panel.grid.major = element_line(colour = ifelse(input$grid_pca_var,
                                                      "grey90", 
                                                      input$bg_pca_var),
                                      size = rel(0.5)),
      axis.text.x = element_text(size = input$xtext_pca_var),
      axis.text.y = element_text(size = input$ytext_pca_var)
    )
    ggplotly(plt, height = input$height_pca_var)
  })
  output$PCAVar <- renderUI({
    fluidPage(plotlyOutput("PCAVarPlot", height = "100%"))
  })
  
  # pca loadings plot
  makePCAHeatmapPlot <- reactive({
    req(input$height_pca_heatmap)
    req(input$height_pca_heatmap > 0)
    req(input$dimred_type == "PCA")
    
    data <- dataInput()
    pca_data <- pca_out()
    
    if (input$pc_heatmap_fill == "Loadings") {
      pc_df <- pca_data$loadings
      pc_df <- pc_df[, 1:min(ncol(pc_df), input$max_pc_heatmap)] %>%
        t() %>%
        as.data.frame()
      xlab <- "Features"
    } else if (input$pc_heatmap_fill == "Scores") {
      pc_df <- pca_data$scores
      pc_df <- pc_df[, 1:min(ncol(pc_df), input$max_pc_heatmap)] %>%
        t() %>%
        as.data.frame()
      xlab <- "Samples"
    }
    
    # do hierarchical clustering on features, if desired
    if (input$pc_heatmap_cluster == "Hierarchical Clustering") {
      hclust_out <- hclust(d = dist(t(pc_df)),
                           method = input$hclust_linkage_pc_heatmap)
      pc_df <- pc_df[, hclust_out$order]
    }
    
    # color scheme options
    if (input$color_theme_pc_heatmap == "Temperature") {
      manual.fill <- "temperature"
    } else {
      manual.fill <- NULL
      if (str_detect(input$color_theme_pc_heatmap, "cool")) {
        viridis_option <- "D"
      } else {
        viridis_option <- "C"
      }
    }
    
    plt <- plotHeatmap(X = pc_df,
                       x.labels = colnames(pc_df),
                       y.labels = paste0("PC", 1:nrow(pc_df)),
                       position = "identity",
                       manual.fill = manual.fill, option = viridis_option,
                       x_text_angle = TRUE,
                       axis_text_size = 12, axis_title_size = 16,
                       legend_title_size = 14, legend_text_size = 12,
                       axis_line_width = 2.5) +
      labs(x = xlab, y = "PC", fill = input$pc_heatmap_fill)
    if (input$coord_flip_pc_heatmap) {
      plt <- plt + coord_flip()
    }
    plt
  })
  output$PCAHeatmapPlot <- renderPlotly({
    plt <- makePCAHeatmapPlot() +
      theme(axis.text.x = element_text(size = input$xtext_pc_heatmap),
            axis.text.y = element_text(size = input$ytext_pc_heatmap))
    ggplotly(plt, height = input$height_pca_heatmap)
  })
  output$PCAHeatmap <- renderUI({
    fluidPage(plotlyOutput("PCAHeatmapPlot", height = "100%"))
  })
  
  ## tsne/umap: plot outputs -------------------------------------------------
  # only perform tsne if selected and if data changed
  tsne_out <- reactive({
    req(input$dimred_type == "tSNE")
    
    # only perform on selected numeric features
    data <- dataInput()
    X <- data %>% select(input$vars_dimred)
    
    Rtsne(X = X, dims = 2, check_duplicates = FALSE,
          perplexity = input$perplexity_tsne, pca = input$pca_tsne, 
          pca_center = "Center" %in% input$pca_tsne_options,
          pca_scale = "Scale" %in% input$pca_tsne_options,
          max_iter = input$maxiter_tsne)$Y
  })
  
  # only perform umap if selected and if data changed
  umap_out <- reactive({
    req(input$dimred_type == "UMAP")
    
    # only perform on selected numeric features
    data <- dataInput()
    X <- data %>% select(input$vars_dimred)
    
    # set umap parameters
    umap_params <- umap.defaults
    umap_params$n_neighbors <- input$perplexity_umap
    umap_params$n_epochs <- input$maxiter_umap
    
    umap(X, config = umap_params)$layout
  })
  
  # make dimension reduction plot for tsne and umap
  makeTsneUmapPlot <- reactive({
    req(input$height_dimred)
    req(input$height_dimred > 0)
    req((input$dimred_type == "tSNE") | (input$dimred_type == "UMAP"))
    
    data <- dataInput()
    if (input$dimred_type == "tSNE") {
      dr_out <- tsne_out()
    } else if (input$dimred_type == "UMAP") {
      dr_out <- umap_out()
    }
    dr_out <- as.data.frame(dr_out)
    
    if (input$color_dimred != "") {  # color points
      dr_out <- cbind(dr_out, col = data[, input$color_dimred])
      if (input$subsample_dimred != 1) {
        dr_out <- sample_frac(dr_out, size = input$subsample_dimred)
      }
      plt <- ggplot(dr_out) +
        aes(x = V1, y = V2, color = col) +
        geom_point(size = input$size_dimred, alpha = input$alpha_dimred) +
        labs(x = paste(input$dimred_type, "Component 1"), 
             y = paste(input$dimred_type, "Component 2"),
             color = input$color_dimred) +
        myGGplotColor(color = dr_out$col)
    } else {
      if (input$subsample_dimred != 1) {
        dr_out <- sample_frac(dr_out, size = input$subsample_dimred)
      }
      plt <- ggplot(dr_out) +
        aes(x = V1, y = V2) +
        geom_point(size = input$size_dimred, alpha = input$alpha_dimred) +
        labs(x = paste(input$dimred_type, "Component 1"), 
             y = paste(input$dimred_type, "Component 2"))
    }
    plt
  })
  output$tsneUmapPlot <- renderPlotly({
    plt <- makeTsneUmapPlot()
    plt <- addTheme(plt, plotly = T, background_color = input$bg_dimred, 
                    grid_color = ifelse(input$grid_dimred, 
                                        "grey90", input$bg_dimred)) +
      theme(axis.text = element_text(size = input$xytext_dimred))
    ggplotly(plt, height = input$height_dimred, dynamicTicks = T)
  })
  
  ## nmf: plot outputs -----------------------------------------------------
  nmf_out <- reactive({
    req(input$dimred_type == "NMF")
    req(input$vars_dimred)
    
    # only perform on selected numeric features
    data <- dataInput()
    X <- data %>% select(input$vars_dimred)
    
    # remove rows with all 0s
    zero_rows <- apply(X, 1, function(x) all(x == 0))
    X <- X[!zero_rows, ]

    nmf_out <- nmf(x = X, rank = input$rank_nmf)@fit
  })
  
  makeNmfWPlot <- reactive({

    nmf_data <- nmf_out()
    W <- nmf_data@W  # n x k matrix

    if (input$subsample_dimred != 1) {
      sample_idx <- sample(1:nrow(W), size = input$subsample_dimred * nrow(W),
                           replace = F)
      W <- W[sample_idx, ]
    }

    rownames(W) <- 1:nrow(W)
    colnames(W) <- paste("NMF", 1:ncol(W), sep = "")

    # color scheme
    if (input$color_nmf == "YlOrRd") {
      col_pal <- colorRampPalette(rev(brewer.pal(n = 7, name = "YlOrRd")))(50)
    } else if (input$color_nmf == "Viridis - warm") {
      col_pal <- viridis_pal(option = "C")(50)
    } else if (input$color_nmf == "Viridis - cool") {
      col_pal <- viridis_pal(option = "D")(50)
    }

    if (input$color_dimred != "") {  # color points
      data <- dataInput()
      if (input$subsample_dimred != 1) {
        data <- data[sample_idx, ]
      }
      anno_col <- data.frame(data %>% pull(input$color_dimred))
      colnames(anno_col) <- input$color_dimred
      rownames(anno_col) <- 1:nrow(W)
    } else {
      anno_col <- NA
    }
    
    shiny_dev <- dev.cur()
    ht <- pheatmap(t(W), color = col_pal, cluster_rows = F, border_color = NA,
                   cluster_cols = input$nmf_cluster_x != "None", 
                   clustering_method = input$hclust_linkage_nmf,
                   annotation_col = anno_col,
                   show_colnames = "Samples" %in% input$labels_nmf,
                   main = "Sample NMF Matrix", fontsize = input$xytext_dimred)
    list(plt = ht, shiny_dev = shiny_dev)
  })
  output$nmfWPlot <- renderPlot({
    out <- makeNmfWPlot()
    dev.set(out$shiny_dev)
    out$plt
  },
  height = function() input$height_dimred)
  
  makeNmfHPlot <- reactive({
    
    nmf_data <- nmf_out()
    H <- nmf_data@H  # k x p matrix

    rownames(H) <- paste("NMF", 1:nrow(H), sep = "")

    # color scheme
    if (input$color_nmf == "YlOrRd") {
      col_pal <- colorRampPalette(rev(brewer.pal(n = 7, name = "YlOrRd")))(50)
    } else if (input$color_nmf == "Viridis - warm") {
      col_pal <- viridis_pal(option = "C")(50)
    } else if (input$color_nmf == "Viridis - cool") {
      col_pal <- viridis_pal(option = "D")(50)
    }

    shiny_dev <- dev.cur()
    ht <- pheatmap(H, color = col_pal, cluster_rows = FALSE, border_color = NA,
                   cluster_cols = input$nmf_cluster_y != "None", 
                   clustering_method = input$hclust_linkage_nmf,
                   show_colnames = "Features" %in% input$labels_nmf,
                   main = "Feature NMF Matrix", fontsize = input$xytext_dimred)
    list(plt = ht, shiny_dev = shiny_dev)
  })
  output$nmfHPlot <- renderPlot({
    out <- makeNmfHPlot()
    dev.set(out$shiny_dev)
    out$plt
  },
  height = function() input$height_dimred)
  
  ## dim red: ui output  ------------------------------------------------------
  output$dimRedPlot <- renderUI({
    if (input$dimred_type == "PCA") {
      fluidPage(plotOutput("PCAPlot", height = "auto") %>%
                  withSpinner(color = "#18bc9c"))
    } else if ((input$dimred_type == "tSNE") | (input$dimred_type == "UMAP")) {
      fluidPage(plotlyOutput("tsneUmapPlot", height = "100%") %>%
                  withSpinner(color = "#18bc9c"))
    } else if (input$dimred_type == "NMF") {
      fluidPage(plotOutput("nmfWPlot", height = "auto") %>%
                  withSpinner(color = "#18bc9c"), br(), br(),
                plotOutput("nmfHPlot", height = "auto") %>%
                  withSpinner(color = "#18bc9c"))
    }
  })
  
  ### heatmap: plot outputs ---------------------------------------
  # filter data for heatmap
  heatmapData <- reactive({
    data <- dataInput()
    
    if (input$vars_select_heatmap == "Manually") {
      req(input$vars_heatmap)
      data <- data %>% select(input$vars_heatmap)
    } else if (input$vars_select_heatmap == "Randomly") {
      req(input$p_heatmap > 0)
      num_vars <- which(sapply(data, is.numeric))
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
    
    xcenter <- FALSE
    xscale <- FALSE
    if ("Center" %in% input$heatmap_options) {
      xcenter <- TRUE
    }
    if ("Scale" %in% input$heatmap_options) {
      xscale <- TRUE
    }
    if (xcenter | xscale) {
      numeric_cols <- sapply(data, is.numeric)
      if (any(numeric_cols)) {
        data[, numeric_cols] <- scale(data[, numeric_cols], 
                                      center = xcenter, scale = xscale)
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
  
  # plot heatmap
  makeHeatmapPlot <- reactive({
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
                       legend_title_size = 14, legend_text_size = 12,
                       axis_line_width = 2.5) +
      labs(x = "Features", y = "Samples", fill = "Value") 
    plt
  })
  output$heatmapPlot <- renderPlotly({
    plt <- makeHeatmapPlot() + 
      theme(axis.text.x = element_text(size = input$xtext_heatmap),
            axis.text.y = element_text(size = input$ytext_heatmap))
    
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
    
    ggplotly(plt, height = input$height_heatmap)
  })
  
  # ui heatmap
  output$Heatmap <- renderUI({
    if (((input$vars_select_heatmap == "Manually") & 
         (is.null(input$vars_heatmap))) |
        ((input$vars_select_heatmap == "Randomly") &
         (input$p_heatmap == 0))) {
      flag <- TRUE
    } else if (((input$sample_select_heatmap == "Manually") & 
         (is.null(input$sample_heatmap))) |
        ((input$sample_select_heatmap == "Randomly") &
         (input$p_heatmap_rows == 0))) {
      flag <- TRUE
    } else {
      flag <- FALSE
    }
    
    if (flag) {
      out <- h4("Some inputs are missing. Please provide required inputs using the left sidebar...")
    } else {
      out <- plotlyOutput("heatmapPlot", height = "auto")  %>%
        withSpinner(color = "#18bc9c")
    }
    out
  })
  
  ### correlation heatmap: plot outputs ---------------------------------------
  # compute correlation matrix
  corMat <- reactive({
    data <- dataInput()
    
    if (input$dim_select == "Rows") {
      req(input$p_cor_rows > 0)
      num_vars <- which(sapply(data, is.numeric) & 
                          !apply(data, 2, FUN = function(x) {
                            if (!all(is.na(x))) {  # constant columns
                              return(all(x == x[!is.na(x)][1], na.rm = T))
                            } else {  # all NA column
                              return(TRUE)
                            }
                          }))
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
        num_vars <- which(sapply(data, is.numeric))
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
  makeCorrelationHeatmapPlot <- reactive({
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
                       legend_title_size = 14, legend_text_size = 12,
                       axis_line_width = 2.5) +
      labs(x = axis_label, y = axis_label, fill = "Cor.") 
    plt
  })
  output$correlationHeatmapPlot <- renderPlotly({
    plt <- makeCorrelationHeatmapPlot()
    
    # additional plotting options
    if (!("x" %in% input$labels_cor)) {
      plt <- plt + theme(axis.text.x = element_blank(),
                         axis.ticks.x = element_blank())
    }
    if (!("y" %in% input$labels_cor)) {
      plt <- plt + theme(axis.text.y = element_blank(),
                         axis.ticks.y = element_blank())
    }
    if ((input$color_theme_cor == "Temperature") & 
        (input$color_scale_cor != "By quantile")) {
      plt <- plt + 
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(-1, 1))
    }
    
    plt <- plt +
      theme(axis.text.x = element_text(size = input$xtext_cor),
            axis.text.y = element_text(size = input$ytext_cor))
    
    ggplotly(plt, height = input$height_cor)
  })
  
  output$CorrelationHeatmap <- renderUI({
    
    if (((input$dim_select == "Rows") & (input$p_cor_rows == 0)) |
        ((input$dim_select == "Columns") & 
         (input$vars_select == "Manually") &
         (length(input$vars_cor) == 0)) |
        ((input$dim_select == "Columns") & 
         (input$vars_select == "Randomly") &
         (input$p_cor == 0)) ) {
      flag <- TRUE
    } else {
      flag <- FALSE
    }
    
    if (flag) {
      out <- h4("Some inputs are missing. Please provide required inputs using the left sidebar...")
    } else {
      out <- plotlyOutput("correlationHeatmapPlot", height = "auto")  %>%
        withSpinner(color = "#18bc9c")
    }
    out
  })
  
  ### miscellaneous buttons/widgets -------------------------------------------
  # collapse sidebar and expand main panel
  observeEvent(input$collapse_sidebar, {
    shinyjs::toggle(id = "side-panel")
    toggleCssClass("main-panel", "col-sm-9")
    toggleCssClass("main-panel", "col-sm-12")
  })
  
  ### output options ---------------------------------------------------------
  outputOptions(output, "fileType", suspendWhenHidden = FALSE)  
}


############################## Shiny App Function ##############################
shinyApp(ui = ui, server = server)

