# shiny/html packages
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(shinyBS)
library(htmltools)
# useful R packages
library(R.utils)
library(tidyverse)
library(rlang)
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
      div(HTML('<i class="fas fa-tree"></i> &nbsp; <b> RFViz </b>'),
          id = "title")
    ),
    windowTitle = "RFViz"
  ),
  tags$div(class = "smallwhitespace", tags$p("whitespace")),
  
  sidebarLayout(
    # sidebar panel -----------------------------------------------------------
    sidebarPanel(id = "side-panel", width = 3,
                 tags$style(HTML(paste0(".well {min-height: 700px;}"))),
      # file upload -----------------------------------------------------------
      bsCollapse(
        id = "file_upload", open = "File Upload",
        bsCollapsePanel(
          "File Upload", 
          # data upload ----------------------------------------------------
          fileUpload(id = "xtrain", label = "Training X"),
          fileUpload(id = "ytrain", label = "Training y"),
          fileUpload(id = "xtest", label = "Test X"),
          fileUpload(id = "ytest", label = "Test y"),
          
          # rf/irf upload ----------------------------------------------------
          fileInput(
            inputId = "file_rf", label = "RF/iRF Output (.rds)", 
            multiple = FALSE, accept = c(".rds")
          ),
          conditionalPanel(
            condition = "output.rf_type == 'irf'",
            numericInput("irf_iteration", "iRF Iteration", value = 1, min = 1)
          ),
          
          # epitree upload ---------------------------------------------------
          fileInput(
            inputId = "file_epitree", label = "Epitree Output (.rds)", 
            multiple = FALSE, accept = c(".rds")
          ),
          
          # style ------------------------------------------------------------
          style = "default"
        )         
      ),
      
      # other variable inputs -----------------------------------------------
      hr(),
      
      # basic plot: variable inputs -------------------------------------------
      conditionalPanel(
        "input.tab == 'basic'",
        varInput(id = "var1", label = "Variable 1:", choices = NULL),
        varInput(id = "var2", label = "Variable 2:", choices = NULL),
        varInput(id = "var3", label = "Variable 3:", choices = NULL),
        varInput(id = "color_basic", label = "Color by:", choices = NULL)
      ),
      
      # pair plot: variable inputs -------------------------------------------
      conditionalPanel(
        "input.tab == 'pair'",
        # variable inputs
        varInputMultiple(id = "vars_pairs", label = "Variables:", 
                         choices = NULL),
        varInputMultiple(id = "color_pairs", label = "Color by: (max 2)",
                         choices = NULL, maxOptions = 2, actionsBox = FALSE)
      ),
      
      # rf evaluation: variable inputs ----------------------------------
      conditionalPanel(
        "input.tab == 'eval'",
        
        radioBtns(id = "type_eval", label = "Evaluation Type", 
                  choices = c("OOB", "Test"), selected = "Test"),
        radioBtns(id = "sample_select_heatmap_eval", label = "Select Samples",
                  choices = c("Manually", "Randomly")),
        
        # manual variable inputs
        conditionalPanel(
          "input.sample_select_heatmap_eval == 'Manually'",
          varInputMultiple(id = "sample_heatmap_eval", label = "Samples:",
                           choices = NULL)
        ),
        
        # random variable inputs
        conditionalPanel(
          "input.sample_select_heatmap_eval == 'Randomly'",
          sliderInput(
            "p_heatmap_eval_rows", "Number of Rows", 
            value = 0, min = 0, max = 150
          )
        )
      ),
      
      # rf tree: variable inputs ----------------------------------
      conditionalPanel(
        "input.tab == 'tree'",
        numericInput("tree_id", "Tree Index", value = 1, min = 1, max = 500)
      ),
      
      # rf vimp: variable inputs ----------------------------------
      conditionalPanel(
        "input.tab == 'vimp'",
        varInputMultiple(id = "vars_vimp", label = "Variables:", choices = NULL)
      ),
      
      # rf interaction: variable inputs ----------------------------------
      conditionalPanel(
        "input.tab == 'ints'",
        varInputMultiple(id = "vars_int", label = "Interactions:",
                         choices = NULL)
      ),
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
          h3("Training Data"),
          fluidRow(
            column(6,
                   h4(htmlOutput("text_summary_train")) %>%
                     tagAppendAttributes(
                       class = "box-border",
                       style = "line-height: 1.4em; display: flex; align-items: center"
                     )
            ),
            column(6,
                   conditionalPanel(
                     "output.trainDataUploaded",
                     uiOutput("dtypes_train") %>%
                       tagAppendAttributes(class = "box-border") %>%
                       withSpinner(color = "#18bc9c")
                   )
            )
          ),
          
          h3("Test Data"),
          fluidRow(
            column(6,
                   h4(htmlOutput("text_summary_test")) %>%
                     tagAppendAttributes(
                       class = "box-border",
                       style = "line-height: 1.4em; display: flex; align-items: center"
                     )
            ),
            column(6,
                   conditionalPanel(
                     "output.testDataUploaded",
                     uiOutput("dtypes_test") %>%
                       tagAppendAttributes(class = "box-border") %>%
                       withSpinner(color = "#18bc9c")
                   )
            )
          ),
          
          hr(style = "border-top: 1px solid #23313E;"),
          
          # data summary table inputs  -----------------------------------------
          fluidRow(
            # data summary table dropdown ----------------------------------
            column(1,
              dropdown(
                # table options
                tableOptions(id = "summary", digits = 2),
                
                # button settings
                circle = TRUE, status = "default", size = "sm",
                icon = icon("gear"), width = "300px", style = "material-circle",
                tooltip = tooltipOptions(title = "Click to see inputs")
              ),
            ),
            # data summary table toggles ----------------------------------
            column(11, dataSplitToggle(id = "summary"))
          ),
          
          # data summary tables --------------------------------------------
          uiOutput("summary_tables") %>% 
            tagAppendAttributes(class = "box-border") %>%
            withSpinner(color = "#18bc9c"),
          
          # data summary X plot inputs --------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          fluidRow(
            # data summary X plot dropdown ----------------------------------
            column(1,
              dropdown(
                # graph settings
                column(3,
                  plotTypeOptions(id = "summary", plot_type_id = "Summary"),
                ),
                plotOptions(id = "summary", height = 400, multicol = TRUE,
                            total_width = 9),
                
                # button settings
                circle = TRUE, status = "default", size = "sm",
                icon = icon("gear"), width = "1000px", style = "material-circle",
                tooltip = tooltipOptions(title = "Click to see inputs")
              )
            ),
            # data summary X plot toggles ----------------------------------
            column(11,
              # type of plot
              plotTypeToggle(id = "Summary", 
                             choices = c(`<i class='fa fa-bar-chart fa-lg'></i>` = "histogram",
                                         `<img src="chart-density-white.png" width=16px height=12px><div class='jhr'></div></img>` = "density",
                                         `<img src="chart-boxplot-white.png" width=16px height=12px><div class='jhr'></div></img>` = "boxplot")),
              
              # variable inputs
              tags$div("Variables:",
                       style = "margin-right: 5px; display: inline-block; font-weight: bold"),
              varInputMultiple(id = "vars_summary", label = NULL, 
                               choices = NULL) %>%
                tagAppendAttributes(style = "display: inline-block")
            )
          ),
          
          # data summary X plot ---------------------------------------------
          uiOutput("xdist") %>%
            tagAppendAttributes(class = "box-border") %>%
            withSpinner(color = "#18bc9c"),
          
          # data summary y plot inputs ---------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          fluidRow(
            # data summary y plot dropdown ------------------------------------
            column(1,
              dropdown(
                # graph settings
                column(3,
                  plotTypeOptions(id = "summary_y", plot_type_id = "SummaryY")
                ),
                plotOptions(id = "summary_y", height = 400, multicol = TRUE,
                            total_width = 9),
                
                # button settings
                circle = TRUE, status = "default", size = "sm",
                icon = icon("gear"), width = "1000px", style = "material-circle",
                tooltip = tooltipOptions(title = "Click to see inputs")
              ),
            ),
            # data summary y plot toggles ------------------------------------
            column(11,
              # type of plot
              conditionalPanel(
                "output.yDataUploaded",
                plotTypeToggle(id = "SummaryY", 
                               choices = "Please select variable(s) first")
              )
            )
          ),
          
          # data summary y plot ----------------------------------------------
          uiOutput("ydist") %>%
            tagAppendAttributes(class = "box-border") %>%
            withSpinner(color = "#18bc9c"),
          br(),
        ),
        # tab: Basic Plots ---------------------------------------------------
        tabPanel(
          "Basic Plots", value = "basic",
          
          # basic plot inputs ----------------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          fluidRow(
            # basic plot dropdown --------------------------------------------
            column(1,
              dropdown(
                # graph settings
                conditionalPanel(
                  "input.plotTypeBasic != 'Please select variable(s) first'",
                  sliderInput(
                    "subsample_basic", "Subsample Points", 
                    min = 0, max = 1, value = 1
                  )
                ),
                column(3,
                  plotTypeOptions(id = "basic", plot_type_id = "Basic")
                ),
                plotOptions(id = "basic", multicol = T, total_width = 9),
                
                # button settings
                circle = TRUE, status = "default", size = "sm",
                icon = icon("gear"), width = "1000px", style = "material-circle",
                tooltip = tooltipOptions(title = "Click to see inputs")
              )
            ),
            # basic plot toggles --------------------------------------------
            column(11,
              dataSplitToggle(id = "basic"),
              conditionalPanel(
                "output.basicVarsInput",
                plotTypeToggle(id = "Basic", 
                               choices = "Please select variable(s) first")
              ) %>%
                tagAppendAttributes(style = "display: inline-block")
            )
          ),
          
          # basic plot ---------------------------------------------------------
          uiOutput("basicPlot") %>%
            tagAppendAttributes(class = "box-border"),
          br(), br()
        ),
        
        # tab: Pair Plots ---------------------------------------------------
        tabPanel(
          "Pair Plots", value = "pair",
          
          # pair plot inputs -------------------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          fluidRow(
            # pair plot dropdown -------------------------------------------
            column(1,
              dropdown(
                # graph settings
                column(3,
                  conditionalPanel(
                    isnull(id = "vars_pairs"),
                    plotPairsOptions(id = "pairs"),
                  )
                ),
                plotOptions(id = "pairs", multicol = T, total_width = 9),
                
                # button settings
                circle = TRUE, status = "default", size = "sm",
                icon = icon("gear"), width = "1000px", style = "material-circle",
                tooltip = tooltipOptions(title = "Click to see inputs")
              )
            ),
            # pair plot toggles -------------------------------------------
            column(11, dataSplitToggle(id = "pairs"))
          ),
          
          # pair plot ----------------------------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          uiOutput("pairs") %>% 
            tagAppendAttributes(class = "box-border"),
          
          br(), br()
        ),
        
        # tab: RF Evaluation Plots --------------------------------------------
        tabPanel(
          "Fit Evaluation", value = "eval",
          
          # rf prediction accuracy table: dropdown panel ---------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          dropdown(
            varInput(id = "group_var_eval", label = "Grouping Variable:",
                     choices = NULL),
            # table options
            tableOptions(id = "eval"),
            
            # button settings
            circle = TRUE, status = "default", size = "sm", 
            icon = icon("gear"), width = "300px", style = "material-circle",
            tooltip = tooltipOptions(title = "Click to see inputs")
          ),
          
          # rf prediction accuracy table -------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          uiOutput("rfAcc")  %>% 
            tagAppendAttributes(class = "box-border") %>%
            withSpinner(color = "#18bc9c"),
          
          # rf evaluation prediction dist plot inputs ------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          fluidRow(
            # rf evaluation prediction dist plot dropdown --------------------
            column(1,
              dropdown(
                # graph settings
                column(3,
                  conditionalPanel(
                    "input.plotTypeEval == 'scatterplot'",
                    varInput(id = "color_eval", label = "Color by:",
                             choices = NULL)
                  ),
                  plotTypeOptions(id = "eval", plot_type_id = "Eval")
                ),
                plotOptions(id = "eval", multicol = T, total_width = 9),
                
                # button settings
                circle = TRUE, status = "default", size = "sm",
                icon = icon("gear"), width = "1000px", style = "material-circle",
                tooltip = tooltipOptions(title = "Click to see inputs")
              )
            ),
            # rf evaluation prediction dist plot toggles --------------------
            column(11,
              showToggle(id = "eval", choices = c("GGplot", "Plotly"),
                         selected = "GGplot"),
              conditionalPanel(
                "output.rfDataUploaded",
                plotTypeToggle(id = "Eval", 
                               choices = "Please select variable(s) first")
              ) %>%
                tagAppendAttributes(style = "display: inline-block")
            )
          ),
          
          # rf evaluation prediction dist plot ------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          uiOutput("rfPred") %>%
            tagAppendAttributes(class = "box-border"),
          
          # rf evaluation heatmap plot inputs -------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          fluidRow(
            # rf evaluation heatmap plot dropdown -----------------------------
            column(1,
              dropdown(
                column(5,
                  plotHclustHeatmapOptions(id = "eval", multicol = T, 
                                           column_widths = c(7, 5))
                ),
                column(7,
                  plotOptions(id = "heatmap_eval", heatmap = TRUE, multicol = T)
                ),
                
                # button settings
                circle = TRUE, status = "default", size = "sm",
                icon = icon("gear"), width = "1000px", style = "material-circle",
                tooltip = tooltipOptions(title = "Click to see inputs")
              )
            ),
            
            # rf evaluation heatmap plot toggles -----------------------------
            column(11,
              showToggle(id = "heatmap_eval", choices = c("GGplot", "Plotly"),
                         selected = "GGplot")
            )
          ),
          
          # rf evaluation heatmap plot --------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          uiOutput("rfPredHeatmap") %>%
            tagAppendAttributes(class = "box-border"),
          br(), br()
        ),
        
        # tab: Tree Plots ---------------------------------------------------
        tabPanel(
          "Tree Plots", value = "tree",
          
          # tree plot: dropdown panel-------------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          dropdown(
            numericInput(
              "height_tree", "Plot Height (px)", value = 500
            ),
            
            # button settings
            circle = TRUE, status = "default", size = "sm",
            icon = icon("gear"), width = "300px", style = "material-circle",
            tooltip = tooltipOptions(title = "Click to see inputs")
          ),
          
          # tree plot ----------------------------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          uiOutput("treeUI")  %>% 
            tagAppendAttributes(class = "box-border"),
          
          br(), br()
        ),
        
        # tab: Variable Importance Plots --------------------------------------
        tabPanel(
          "Feature Importance", value = "vimp",
          # feature importance plot/table inputs ---------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          fluidRow(
            # feature importance plot/table dropdown --------------------------
            column(1,
              conditionalPanel(
                "input.plottype_vimp == 'Table'",
                dropdown(
                  tableOptions(id = "vimp"),
                  # button settings
                  circle = TRUE, status = "default", size = "sm",
                  icon = icon("gear"), width = "300px", style = "material-circle",
                  tooltip = tooltipOptions(title = "Click to see inputs")
                )
              ),
              conditionalPanel(
                "input.plottype_vimp == 'Plotly'",
                dropdown(
                  column(3,
                    numericInput(
                      "min_imp_thr_vimp",
                      "Minimum Importance Threshold", 
                      value = 0
                    )
                  ),
                  plotOptions(id = "vimp", multicol = T, total_width = 9),
                  
                  # button settings
                  circle = TRUE, status = "default", size = "sm",
                  icon = icon("gear"), width = "1000px", style = "material-circle",
                  tooltip = tooltipOptions(title = "Click to see inputs")
                )
              )
            ),
            # feature importance plot/table toggles --------------------------
            column(11,
              showToggle(id = "vimp", choices = c("Plotly", "Table"),
                         selected = "Plotly")
            )
          ),
          
          # feature importance plot/table -----------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          uiOutput("rfVimp") %>%
            tagAppendAttributes(class = "box-border"),
          br(),
          
          # feature splits plot inputs -------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          fluidRow(
            # feature splits plot dropdown ------------------------------------
            column(1,
              dropdown(
                plotOptions(id = "splits", multicol = T),
                
                # button settings
                circle = TRUE, status = "default", size = "sm",
                icon = icon("gear"), width = "700px", style = "material-circle",
                tooltip = tooltipOptions(title = "Click to see inputs")
              )
            ),
            # feature splits plot toggle ------------------------------------
            column(11,
              showToggle(id = "splits", choices = c("GGplot", "Plotly"),
                         selected = "GGplot"),
              dataSplitToggle(id = "splits")
            )
          ),
          
          # feature splits plot -----------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          uiOutput("rfSplits") %>%
            tagAppendAttributes(class = "box-border"),
          br(),
          
          # local stability plot inputs -----------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          fluidRow(
            # local stability plot dropdown -----------------------------------
            column(1,
              conditionalPanel(
                "input.show_lstab !== 'P-values'",
                dropdown(
                  column(2,
                         plotLocalStabilityRFOptions(id = "lstab")
                  ),
                  column(4,
                         plotHclustHeatmapOptions(id = "lstab", multicol = T, 
                                                  column_widths = c(7, 5))
                  ),
                  plotOptions(id = "lstab", multicol = T, total_width = 6),
                  
                  # button settings
                  circle = TRUE, status = "default", size = "sm",
                  icon = icon("gear"), width = "1200px", style = "material-circle",
                  tooltip = tooltipOptions(title = "Click to see inputs")
                )
              ),
              conditionalPanel(
                "input.show_lstab == 'P-values'",
                dropdown(
                  column(3,
                         plotLocalStabilityRFOptions(id = "lstab")
                  ),
                  plotOptions(id = "lstab", multicol = T, total_width = 9),
                  
                  # button settings
                  circle = TRUE, status = "default", size = "sm",
                  icon = icon("gear"), width = "1000px", style = "material-circle",
                  tooltip = tooltipOptions(title = "Click to see inputs")
                )
              )
            ),
            # local stability plot toggle -----------------------------------
            column(11,
              showToggle(id = "lstab", choices = c("GGplot", "Plotly"),
                         selected = "GGplot"),
              dataSplitToggle(id = "lstab", choices = c("OOB", "Test"),
                              selected = "OOB"),
              showSquareToggle(id = "lstab")
            )
          ),
          
          # local stability plot ---------------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          uiOutput("localStability") %>%
            tagAppendAttributes(class = "box-border"),
          br(),
          
          # local stability score distribution plot inputs -------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          fluidRow(
            # local stability score distribution plot dropdown ----------------
            column(1,
              dropdown(
                # graph settings
                column(3,
                  varInputMultiple(id = "vars_lstab_dist", label = "Variables:",
                                   choices = NULL),
                  conditionalPanel(
                    "input.plotTypeLstab == 'scatterplot'",
                    varInput(id = "color_lstab_dist", label = "Color by:",
                             choices = NULL)
                  ),
                  plotTypeOptions(id = "lstab_dist", plot_type_id = "Lstab")
                ),
                plotOptions(id = "lstab_dist", multicol = T, total_width = 9),
                
                # button settings
                circle = TRUE, status = "default", size = "sm",
                icon = icon("gear"), width = "1000px", style = "material-circle",
                tooltip = tooltipOptions(title = "Click to see inputs")
              )
            ),
            # local stability score distribution plot toggles ----------------
            column(11,
              showToggle(id = "lstab_dist", choices = c("GGplot", "Plotly"),
                         selected = "GGplot"),
              conditionalPanel(
                isnull("vars_vimp"),
                plotTypeToggle(id = "Lstab", 
                               choices = "Please select variable(s) first")
              ) %>%
                tagAppendAttributes(style = "display: inline-block")
            )
          ),
          
          # local stability score distribution plot ---------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          uiOutput("localStabilityDist") %>%
            tagAppendAttributes(class = "box-border"),
          br(),
          
        ),
        
        # tab: Interaction Importance Plots -----------------------------------
        tabPanel(
          "Interaction Importance", value = "ints",
          # irf results plot/table inputs -------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          fluidRow(
            # irf results plot/table dropdown --------------------------------
            column(1,
              conditionalPanel(
                "input.plottype_irf_res == 'Table'",
                dropdown(
                  tableOptions(id = "irf_res"),
                  
                  # button settings
                  circle = TRUE, status = "default", size = "sm",
                  icon = icon("gear"), width = "300px", style = "material-circle",
                  tooltip = tooltipOptions(title = "Click to see inputs")
                )
              ),
              conditionalPanel(
                "input.plottype_irf_res !== 'Table'",
                dropdown(
                  column(3,
                    varInputMultiple(id = "metrics_irf_res", label = "Metrics:",
                                     choices = c("prevalence", "precision", "cpe",
                                                 "sta.cpe", "fsd", "sta.fsd", 
                                                 "mip", "sta.mip", "stability"),
                                     selected = c("prevalence", "precision", 
                                                  "stability")),
                    varInput(id = "rank_by_irf_res", label = "Rank by:",
                             choices = c("None", "prevalence", "precision", "cpe",
                                         "sta.cpe", "fsd", "sta.fsd", "mip", 
                                         "sta.mip", "stability"),
                             selected = "prevalence"),
                    numericInput("top_p_irf_res", "Top p", value = NA),
                    numericInput("bar_width_irf_res", "Bar Width", value = 0.8),
                    numericInput("text_size_irf_res", "Text Size", value = 5),
                    numericInput("hjust_irf_res", "Text Position", value = 1.5),
                    numericInput("pltdigits_irf_res", "Digits", value = 2)
                  ),
                  plotOptions(id = "irf_res", multicol = T, total_width = 9),
                  
                  # button settings
                  circle = TRUE, status = "default", size = "sm",
                  icon = icon("gear"), width = "1000px", style = "material-circle",
                  tooltip = tooltipOptions(title = "Click to see inputs")
                )
              )
            ),
            # irf results plot/table toggles --------------------------------
            column(11, showToggle(id = "irf_res", selected = "GGplot"))
          ),
          
          # irf results plot/table --------------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          uiOutput("iRFResults") %>%
            tagAppendAttributes(class = "box-border"),
          br(),
          
          # iRF interaction output plot/table with choice of plot or table
          # epitree results plot inputs -----------------------------------
          fluidRow(
            # epitree results plot dropdown -----------------------------------
            column(1,
              dropdown(
                column(3,
                  numericInput("bar_width_epitree_res", "Bar Width", value = 0.8),
                  numericInput("text_size_epitree_res", "Text Size", value = 5),
                  numericInput("hjust_epitree_res", "Text Position", value = 1.5),
                  numericInput("pltdigits_epitree_res", "Digits", value = 2)
                ),
                plotOptions(id = "epitree_res", multicol = T, total_width = 9),
                
                # button settings
                circle = TRUE, status = "default", size = "sm",
                icon = icon("gear"), width = "1000px", style = "material-circle",
                tooltip = tooltipOptions(title = "Click to see inputs")
              )
            ),
            # epitree results plot toggles -----------------------------------
            column(11,
              showToggle(id = "epitree_res", choices = c("GGplot", "Plotly"),
                         selected = "GGplot"),
              showSquareToggle(id = "epitree_res",
                               choices = c("P-values", "Stability", 
                                           "Prediction Accuracy", "All"),
                               selected = "All")
            )
          ),
          
          # epiTree results plot ----------------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          uiOutput("epitreeResults") %>%
            tagAppendAttributes(class = "box-border"),
          br(),
          
          # local stability plot inputs ----------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          fluidRow(
            # local stability plot dropdown -------------------------------
            column(1,
              conditionalPanel(
                "input.show_lstab_int !== 'P-values'",
                dropdown(
                  column(2,
                         plotLocalStabilityRFOptions(id = "lstab_int")
                  ),
                  column(4,
                         plotHclustHeatmapOptions(id = "lstab_int", multicol = T, 
                                                  column_widths = c(7, 5))
                  ),
                  plotOptions(id = "lstab_int", multicol = T, total_width = 6),
                  
                  # button settings
                  circle = TRUE, status = "default", size = "sm",
                  icon = icon("gear"), width = "1200px", style = "material-circle",
                  tooltip = tooltipOptions(title = "Click to see inputs")
                )
              ),
              conditionalPanel(
                "input.show_lstab_int == 'P-values'",
                dropdown(
                  column(3,
                         plotLocalStabilityRFOptions(id = "lstab_int")
                  ),
                  plotOptions(id = "lstab_int", multicol = T, total_width = 9),
                  
                  # button settings
                  circle = TRUE, status = "default", size = "sm",
                  icon = icon("gear"), width = "1000px", style = "material-circle",
                  tooltip = tooltipOptions(title = "Click to see inputs")
                )
              )
            ),
            # local stability plot toggles -------------------------------
            column(11,
              showToggle(id = "lstab_int", choices = c("GGplot", "Plotly"),
                         selected = "GGplot"),
              dataSplitToggle(id = "lstab_int", choices = c("OOB", "Test"),
                              selected = "OOB"),
              showSquareToggle(id = "lstab_int")
            )
          ),
          
          # local stability plot ---------------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          uiOutput("intLocalStability") %>%
            tagAppendAttributes(class = "box-border"),
          br(),
          
          # local stability score distribution plot inputs -----------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          fluidRow(
            # local stability score distribution plot dropdown ----------------
            column(1, 
              dropdown(
                # graph settings
                column(3,
                  varInputMultiple(id = "vars_lstab_dist_int", 
                                   label = "Interactions:",
                                   choices = NULL),
                  conditionalPanel(
                    "input.plotTypeLstab_int == 'scatterplot'",
                    varInput(id = "color_lstab_dist_int", label = "Color by:",
                             choices = NULL)
                  ),
                  plotTypeOptions(id = "lstab_dist_int", 
                                  plot_type_id = "Lstab_int")
                ),
                plotOptions(id = "lstab_dist_int",
                            multicol = T, total_width = 9),
                
                # button settings
                circle = TRUE, status = "default", size = "sm",
                icon = icon("gear"), width = "1000px", style = "material-circle",
                tooltip = tooltipOptions(title = "Click to see inputs")
              )
            ),
            # local stability score distribution plot toggles ----------------
            column(11,
              showToggle(id = "lstab_dist_int", choices = c("GGplot", "Plotly"),
                         selected = "GGplot"),
              conditionalPanel(
                isnull("vars_int"),
                plotTypeToggle(id = "Lstab_int",
                               choices = "Please select variable(s) first")
              ) %>%
                tagAppendAttributes(style = "display: inline-block")
            )
          ),
          
          # local stability score distribution plot ---------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          uiOutput("intLocalStabilityDist") %>%
            tagAppendAttributes(class = "box-border"),
          br(),
          
          # individual interaction plot inputs -----------------------------
          fluidRow(
            # individual interaction plot dropdown ---------------------------
            column(1,
              dropdown(
                column(3,
                  radioBtns(id = "show_int", label = "Show",
                            choices = c("iRF 3D Surface", "iRF 2D Surface",
                                        "epiTree Tree", "epiTree Heatmaps",
                                        "Local Stability Heatmap"),
                            selected = "iRF 3D Surface"),
                  conditionalPanel(
                    "input.show_int == 'Local Stability Heatmap'",
                    plotLocalStabilityRFOptions(id = "lstab_ft_int")
                  )
                ),
                plotOptions(id = "int", multicol = T, total_width = 9),
                
                # button settings
                circle = TRUE, status = "default", size = "sm",
                icon = icon("gear"), width = "1000px", style = "material-circle",
                tooltip = tooltipOptions(title = "Click to see inputs")
              )
            ),
            # individual interaction plot toggles -----------------------------
            column(11,
              conditionalPanel(
                "(input.show_int == 'iRF 2D Surface') | (input.show_int == 'Local Stability Heatmap')",
                showToggle(id = "int", choices = c("GGplot", "Plotly"),
                           selected = "GGplot")
              ) %>%
                tagAppendAttributes(style = "display: inline-block;"),
              
              conditionalPanel(
                "(input.show_int == 'iRF 2D Surface') | (input.show_int == 'iRF 3D Surface') | (input.show_int == 'Local Stability Heatmap')",
                dataSplitToggle(id = "int")
              ) %>%
                tagAppendAttributes(style = "display: inline-block;"),
              
              conditionalPanel(
                "input.show_int == 'Local Stability Heatmap'",
                showSquareToggle(id = "lstab_ft_int")
              ) %>%
                tagAppendAttributes(style = "display: inline-block;"),
              
              tags$div("Interaction:",
                       style = "display: inline-block; margin-right: 5px; font-weight: bold"),
              varInput(id = "var_int", label = NULL, choices = NULL) %>%
                tagAppendAttributes(style = "display: inline-block"),
            )
          ),
          
          # individual interaction plot ---------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          uiOutput("intIndivPlot") %>%
            tagAppendAttributes(class = "box-border"),
          br(),
          
          # interaction heatmap plot inputs -------------------------------
          fluidRow(
            # interaction heatmap plot dropdown ------------------------------
            column(1,
              dropdown(
                column(3,
                  radioBtns(id = "show_heatmap_int", label = "Show",
                            choices = c("Active Interactions", 
                                        "Prediction Heatmap",
                                        "Prediction Error Heatmap")),
                  radioBtns(id = "cluster_x_heatmap_int", label = "Cluster x by",
                            choices = c("Hierarchical Clustering", "None")),
                  radioBtns(id = "cluster_y_heatmap_int", label = "Cluster y by",
                            choices = c("Hierarchical Clustering", "None"))
                ),
                plotOptions(id = "heatmap_int", multicol = T, total_width = 9),
                
                # button settings
                circle = TRUE, status = "default", size = "sm",
                icon = icon("gear"), width = "1000px", style = "material-circle",
                tooltip = tooltipOptions(title = "Click to see inputs")
              )
            ),
            # interaction heatmap plot toggles ------------------------------
            column(11,
              showToggle(id = "heatmap_int", choices = c("GGplot", "Plotly"),
                         selected = "GGplot"),
              dataSplitToggle(id = "heatmap_int"),
              conditionalPanel(
                "input.show_heatmap_int == 'Prediction Error Heatmap'",
                showSquareToggle(id = "err_heatmap_int",
                                 choices = c("MAE", "Class"),
                                 selected = "MAE", 
                                 full_id = TRUE)
              ) %>%
                tagAppendAttributes(style = "display: inline-block;")
            )
          ),
          # interaction heatmap plot -----------------------------------------
          tags$div(class = "whitespace", tags$p("whitespace")),
          uiOutput("intHeatmap") %>%
            tagAppendAttributes(class = "box-border"),
          br()
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
  output$fileType_xtrain <- reactive({
    substr(input$file_xtrain$datapath,
           start = nchar(input$file_xtrain$datapath) - 3,
           stop = nchar(input$file_xtrain$datapath))
  })
  output$fileType_xtest <- reactive({
    substr(input$file_xtest$datapath,
           start = nchar(input$file_xtest$datapath) - 3,
           stop = nchar(input$file_xtest$datapath))
  })
  output$fileType_ytrain <- reactive({
    substr(input$file_ytrain$datapath,
           start = nchar(input$file_ytrain$datapath) - 3,
           stop = nchar(input$file_ytrain$datapath))
  })
  output$fileType_ytest <- reactive({
    substr(input$file_ytest$datapath,
           start = nchar(input$file_ytest$datapath) - 3,
           stop = nchar(input$file_ytest$datapath))
  })
  
  # update data file ------------------------------------------------------
  xtrainInput <- reactive({
    req(input$file_xtrain$datapath)
    fileType <- substr(input$file_xtrain$datapath, 
                       start = nchar(input$file_xtrain$datapath) - 3,
                       stop = nchar(input$file_xtrain$datapath))
    switch(
      fileType,
      ".rds" = readRDS(input$file_xtrain$datapath),
      ".csv" = read.csv(input$file_xtrain$datapath, 
                        header = input$header_xtrain, 
                        check.names = F),
      ".txt" = read.table(input$file_xtrain$datapath,
                          header = input$header_xtrain, 
                          sep = input$sep_xtrain, 
                          check.names = F)
    )
  })
  xtestInput <- reactive({
    req(input$file_xtest$datapath)
    fileType <- substr(input$file_xtest$datapath, 
                       start = nchar(input$file_xtest$datapath) - 3,
                       stop = nchar(input$file_xtest$datapath))
    switch(
      fileType,
      ".rds" = readRDS(input$file_xtest$datapath),
      ".csv" = read.csv(input$file_xtest$datapath, 
                        header = input$header_xtest, 
                        check.names = F),
      ".txt" = read.table(input$file_xtest$datapath,
                          header = input$header_xtest, 
                          sep = input$sep_xtest, 
                          check.names = F)
    )
  })
  ytrainInput <- reactive({
    req(input$file_ytrain$datapath)
    fileType <- substr(input$file_ytrain$datapath, 
                       start = nchar(input$file_ytrain$datapath) - 3,
                       stop = nchar(input$file_ytrain$datapath))
    switch(
      fileType,
      ".rds" = readRDS(input$file_ytrain$datapath),
      ".csv" = read.csv(input$file_ytrain$datapath, 
                        header = input$header_ytrain, 
                        check.names = F)[, 1],
      ".txt" = read.table(input$file_ytrain$datapath,
                          header = input$header_ytrain, 
                          sep = input$sep_ytrain, 
                          check.names = F)[, 1]
    )
  })
  ytestInput <- reactive({
    req(input$file_ytest$datapath)
    fileType <- substr(input$file_ytest$datapath, 
                       start = nchar(input$file_ytest$datapath) - 3,
                       stop = nchar(input$file_ytest$datapath))
    switch(
      fileType,
      ".rds" = readRDS(input$file_ytest$datapath),
      ".csv" = read.csv(input$file_ytest$datapath, 
                        header = input$header_ytest, 
                        check.names = F)[, 1],
      ".txt" = read.table(input$file_ytest$datapath,
                          header = input$header_ytest, 
                          sep = input$sep_ytest, 
                          check.names = F)[, 1]
    )
  })
  dataInputBasic <- reactive({
    req(input$datasplit_basic)
    if (input$datasplit_basic == "Training") {
      X <- xtrainInput()
      y <- ytrainInput()
    } else if (input$datasplit_basic == "Test") {
      X <- xtestInput()
      y <- ytestInput()
    }
    cbind(.y = y, X)
  })
  dataInputPairs <- reactive({
    req(input$datasplit_pairs)
    if (input$datasplit_pairs == "Training") {
      X <- xtrainInput()
      y <- ytrainInput()
    } else if (input$datasplit_pairs == "Test") {
      X <- xtestInput()
      y <- ytestInput()
    }
    cbind(.y = y, X)
  })
  dataInput <- reactive({
    req(input$datasplit_basic)
    if (input$datasplit_basic == "Training") {
      X <- xtrainInput()
      y <- ytrainInput()
    } else if (input$datasplit_basic == "Test") {
      X <- xtestInput()
      y <- ytestInput()
    }
    cbind(.y = y, X)
  })

  rfInput <- reactive({
    req(input$file_rf$datapath)
    readRDS(input$file_rf$datapath)
  })
  rfFit <- reactive({
    rf_fit <- rfInput()
    if ("rf.list" %in% names(rf_fit)) {
      rf_fit <- rf_fit$rf.list[[input$irf_iteration]]
    }
    rf_fit
  })
  irfFit <- reactive({
    rf_fit <- rfInput()
    if ("rf.list" %in% names(rf_fit)) {
      return(rf_fit)
    } else {
      return(NULL)
    }
  })
  output$rf_type <- reactive({
    rf_fit <- rfInput()
    if ("rf.list" %in% names(rf_fit)) {
      type <- "irf"
    } else {
      type <- "rf"
    }
    type
  })
  epitreeInput <- reactive({
    req(input$file_epitree$datapath)
    readRDS(input$file_epitree$datapath)
  })
  
  # missing input checks ---------------------------------------------------
  trainDataUploaded <- reactive({
    !(is.null(input$file_xtrain$datapath) | is.null(input$file_ytrain$datapath))
  })
  output$trainDataUploaded <- reactive({
    return(trainDataUploaded())
  })
  
  testDataUploaded <- reactive({
    !(is.null(input$file_xtest$datapath) | is.null(input$file_ytest$datapath))
  })
  output$testDataUploaded <- reactive({
    return(testDataUploaded())
  })
  
  output$yDataUploaded <- reactive({
    !(is.null(input$file_ytrain$datapath) | is.null(input$file_ytest$datapath))
  })
  output$basicVarsInput <- reactive({
    vars <- setdiff(c(input$var1, input$var2, input$var3), c("", "None"))
    return(length(vars) > 0)
  })
  
  rfDataUploaded <- reactive({
    !(is.null(input$file_rf$datapath) | is.null(input$file_xtrain$datapath) |
        is.null(input$file_xtest$datapath) | is.null(input$file_ytrain$datapath) |
        is.null(input$file_ytest$datapath))
  })
  output$rfDataUploaded <- reactive({
    return(rfDataUploaded())
  })
  
  # update feature selection if dataInput() changes --------------------------
  observe({
    if (trainDataUploaded()) {
      data <- cbind(.y = ytrainInput(), xtrainInput())
    } else {
      data <- cbind(.y = ytestInput(), xtestInput())
    }
    
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
    updatePickerInput(session, "vars_summary", 
                      choices = setdiff(num_vars, ".y"),
                      selected = setdiff(num_vars, ".y"))
    updatePickerInput(session, "var1", choices = c("None", vars))
    updatePickerInput(session, "var2", choices = c("None", vars))
    updatePickerInput(session, "var3", choices = c("None", vars))
    updatePickerInput(session, "vars_pairs", choices = vars)
    updatePickerInput(session, "group_var_eval", 
                      choices = setdiff(c("None", vars), ".y"))
    
    updatePickerInput(session, "color_basic", choices = c("None", vars))
    updatePickerInput(session, "color_pairs", choices = vars)
    updatePickerInput(session, "color_eval", 
                      choices = setdiff(c("None", vars), ".y"))
    updatePickerInput(session, "color_lstab_dist", 
                      choices = setdiff(c("None", vars), ".y"))
  })
  
  observe({
    if (!is.null(input$file_xtrain$datapath)) {
      X <- xtrainInput()
      vars <- colnames(X)
    } else if (!is.null(input$file_xtest$datapath)) {
      X <- xtestInput()
      vars <- colnames(X)
    } else {
      rf_fit <- rfFit()
      vars <- rf_fit$forest$independent.variable.names
    }
    updatePickerInput(session, "vars_vimp", choices = vars)
  })
  observe({
    signed_vars <- c(paste0(input$vars_vimp, "+"), paste0(input$vars_vimp, "-"))
    updatePickerInput(session, "vars_lstab_dist",
                      choices = signed_vars,
                      selected = signed_vars)
  })
  
  observe({
    irf_fit <- irfFit()
    req(irf_fit)
    irf_int <- irf_fit$interaction
    updatePickerInput(session, "vars_int", choices = irf_int$int)
    updatePickerInput(session, "var_int", choices = irf_int$int)
  })
  observe({
    irf_fit <- irfFit()
    req(irf_fit)
    irf_int <- irf_fit$interaction
    updatePickerInput(session, "vars_lstab_dist_int", 
                      choices = irf_int$int,
                      selected = input$vars_int)
  })
  
  observe({
    ytrain <- ytrainInput()
    if (all(ytrain %in% 0:1)) {
      eval.metrics <- c("MAE", "Class")
    } else {
      eval.metrics <- c("RMSE", "MAE")
    }
    updatePrettyRadioButtons(session, "err_heatmap_int", 
                             choices = eval.metrics, selected = eval.metrics[1])
  })
  
  # update plot types if inputs change ---------------------------------------
  plotTypeYReactive <- reactive({
    ytrain <- ytrainInput()
    plot_types <- getPlotTypes(data.frame(y = ytrain), "y")
  })
  observe({
    updateRadioGroupButtons(
      session, "plotTypeSummaryY",
      choices = plotTypeYReactive(),
      size = "sm")
  })
  
  plotTypeReactive <- reactive({
    data <- cbind(.y = ytrainInput(), xtrainInput())
    plot_types <- getPlotTypes(data, c(input$var1, input$var2, input$var3))
  })
  observe({
    updateRadioGroupButtons(
      session, "plotTypeBasic",
      size = "sm",
      choices = plotTypeReactive())
  })
  
  plotTypeEvalReactive <- reactive({
    ytrain <- ytrainInput()
    if (is.factor(ytrain)) {
      # histogram, density, or boxplot
      plot_types <- getPlotTypes(data.frame(y = 1:3), "y")
    } else {
      plot_types <- c(`<img src="chart-scatter-white.png" width=15px height=13px><div class='jhr'></div></img>` = "scatterplot")
    }
  })
  observe({
    updateRadioGroupButtons(
      session, "plotTypeEval",
      choices = plotTypeEvalReactive(),
      size = "sm"
    )
    updateRadioGroupButtons(
      session, "plotTypeLstab",
      choices = plotTypeEvalReactive(),
      selected = "density",
      size = "sm"
    )
    updateRadioGroupButtons(
      session, "plotTypeLstab_int",
      choices = plotTypeEvalReactive(),
      selected = "density",
      size = "sm"
    )
  })
  
  ### data summary: text outputs --------------------------------------------
  output$text_summary_train <- renderText({
    if (!trainDataUploaded()) {
      return("Missing inputs. Please upload training X and y.")
    }
    
    Xtrain <- xtrainInput()
    ytrain <- ytrainInput()
    
    if (length(ytrain) != nrow(Xtrain)) {
      text_out <- paste0(
        "Warning: Number of samples in Xtrain (n = ", nrow(Xtrain), ") ",
        "not equal to number of samples in ytrain (n = ", length(ytrain), ")."
      )
    } else {
      text_out <- paste0(
        paste0("Number of samples: ", nrow(Xtrain), "<br/>"),
        paste0("Number of features: ", ncol(Xtrain), "<br/>"),
        paste0("Number of NAs in y: ", sum(is.na(ytrain)), "<br/>"),
        paste0("Number of NAs in X: ", sum(is.na(Xtrain)), "<br/>"),
        paste0("Number of columns in X with NAs: ", 
               sum(apply(Xtrain, 2, FUN = function(x) any(is.na(x)))), 
               "<br/>"),
        paste0("Number of constant columns in X: ",
               sum(apply(Xtrain, 2,
                         FUN = function(x) {
                           all(x == x[!is.na(x)][1], na.rm = T)
                         }), na.rm = T)))
    }
    text_out
  })
  
  output$text_summary_test <- renderText({
    if (!testDataUploaded()) {
      return("Missing inputs. Please upload test X and y.")
    }
    
    Xtest <- xtestInput()
    ytest <- ytestInput()
    
    if (length(ytest) != nrow(Xtest)) {
      text_out <- paste0(
        "Warning: Number of samples in Xtest (n = ", nrow(Xtest), ") ",
        "not equal to number of samples in ytest (n = ", length(ytest), ")."
      )
    } else {
      text_out <- paste0(
        paste0("Number of samples: ", nrow(Xtest), "<br/>"),
        paste0("Number of features: ", ncol(Xtest), "<br/>"),
        paste0("Number of NAs in y: ", sum(is.na(ytest)), "<br/>"),
        paste0("Number of NAs in X: ", sum(is.na(Xtest)), "<br/>"),
        paste0("Number of columns in X with NAs: ", 
               sum(apply(Xtest, 2, FUN = function(x) any(is.na(x)))), 
               "<br/>"),
        paste0("Number of constant columns in X: ",
               sum(apply(Xtest, 2,
                         FUN = function(x) {
                           all(x == x[!is.na(x)][1], na.rm = T)
                         }), na.rm = T)))
    }
    text_out
  })
  
  ### data summary: table outputs --------------------------------------------
  skimTrain <- reactive({
    if (!trainDataUploaded()) return(NULL)
    Xtrain <- xtrainInput()
    ytrain <- ytrainInput()
    skim(cbind(`.y` = ytrain, Xtrain))
  })
  skimTest <- reactive({
    if (!testDataUploaded()) return(NULL)
    Xtest <- xtestInput()
    ytest <- ytestInput()
    skim(cbind(`.y` = ytest, Xtest))
  })
  skimData <- reactive({
    if (input$datasplit_summary == "Training") {
      skim_out <- skimTrain()
    } else if (input$datasplit_summary == "Test") {
      skim_out <- skimTest()
    }
    skim_out
  })
  
  output$dtypes_train_table <- renderDT({
    skim_out <- skimTrain()
    dtypes_df <- skim_out %>%
      mutate(`.group` = ifelse(skim_variable == ".y", "y", "X"),
             skim_type = as.factor(skim_type)) %>%
      group_by(`.group`) %>%
      summarise(Freq = c(table(skim_type))) %>%
      ungroup() %>%
      mutate(Class = capitalize(names(Freq))) %>%
      spread(key = "Class", value = "Freq") %>%
      column_to_rownames(".group")
    DT::datatable(dtypes_df, rownames = T,
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
  output$dtypes_train <- renderUI({
    fluidPage(DTOutput("dtypes_train_table"),
              tags$div(class = "whitespace", tags$p("whitespace")))
  })
  
  output$dtypes_test_table <- renderDT({
    skim_out <- skimTest()
    dtypes_df <- skim_out %>%
      mutate(`.group` = ifelse(skim_variable == ".y", "y", "X"),
             skim_type = as.factor(skim_type)) %>%
      group_by(`.group`) %>%
      summarise(Freq = c(table(skim_type))) %>%
      ungroup() %>%
      mutate(Class = capitalize(names(Freq))) %>%
      spread(key = "Class", value = "Freq") %>%
      column_to_rownames(".group")
    DT::datatable(dtypes_df, rownames = T,
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
  output$dtypes_test <- renderUI({
    fluidPage(DTOutput("dtypes_test_table"),
              tags$div(class = "whitespace", tags$p("whitespace")))
  })
  
  skimWrapper <- function(skim_out, dtype, digits, sigfig) {
    if (sigfig) {
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
  makeXDistPlot <- reactive({
    req(input$vars_summary)
    req(input$height_summary)
    req(input$height_summary > 0)
    
    Xtrain <- xtrainInput() %>%
      select(input$vars_summary) %>%
      mutate(`.split` = "Train")
    Xtest <- xtestInput() %>%
      select(input$vars_summary) %>%
      mutate(`.split` = "Test")
    data <- bind_rows(Xtrain, Xtest) %>%
      mutate(`.split` = as.factor(`.split`))
    
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
                           fill.str = ".split",
                           bins = input$bins_summary, 
                           alpha = input$alpha_summary) +
        labs(x = axis_label, title = "Overall X Distribution", fill = "Split")
    } else if (input$plotTypeSummary == "density") {
      plt <- plotDensity(data = data, fill.str = ".split",
                         alpha = input$alpha_summary) +
        labs(x = axis_label, title = "Overall X Distribution", fill = "Split")
    } else if (input$plotTypeSummary == "boxplot") {
      plt <- plotBoxplot(data = data, fill.str = ".split", horizontal = T) +
        labs(y = axis_label, title = "Overall X Distribution", fill = "Split")
    }
    print(plt)
    plt
  })
  output$xdist_plot <- renderPlotly({
    plt <- makeXDistPlot() %>%
      addPlotOptions(input = input, id = "summary", plotly = T)
    ggplotly(plt, height = input$height_summary)
  })
  output$xdist <- renderUI({
    fluidPage(plotlyOutput("xdist_plot", height = "100%"))
  })
  
  makeYDistPlot <- reactive({
    req(input$height_summary_y)
    req(input$height_summary_y > 0)
    
    ytrain <- data.frame(y = ytrainInput()) %>%
      mutate(`.split` = "Train")
    ytest <- data.frame(y = ytestInput()) %>%
      mutate(`.split` = "Test")
    data <- bind_rows(ytrain, ytest) %>%
      mutate(`.split` = as.factor(`.split`))
    
    # initialize empty plot
    plt <- ggplot(data) +
      labs(x = "", y = "") +
      myGGplotTheme()
    
    # make plot
    if (input$plotTypeSummaryY == "histogram") {
      plt <- plotHistogram(data = data, position = "identity", 
                           x.str = "y", fill.str = ".split",
                           bins = input$bins_summary_y, 
                           alpha = input$alpha_summary_y) +
        labs(x = "y", title = "Overall Y Distribution", fill = "Split")
    } else if (input$plotTypeSummaryY == "density") {
      plt <- plotDensity(data = data, x.str = "y", fill.str = ".split", 
                         alpha = input$alpha_summary_y) +
        labs(x = "y", title = "Overall Y Distribution", fill = "Split")
    } else if (input$plotTypeSummaryY == "boxplot") {
      plt <- plotBoxplot(data = data, x.str = "y", fill.str = ".split",
                         horizontal = T) +
        labs(y = "y", title = "Overall Y Distribution", fill = "Split")
    } else if (input$plotTypeSummaryY == "bar") {
      plt <- plotBarplot(data = data, x.str = "y", fill.str = ".split",
                         position = "fill", stat = "count") +
        labs(y = "Proportion", title = "Overall Y Distribution", fill = "Split")
    }
    plt
  })
  output$ydist_plot <- renderPlotly({
    plt <- makeYDistPlot() %>%
      addPlotOptions(input = input, id = "summary_y", plotly = T)
    ggplotly(plt, height = input$height_summary_y)
  })
  output$ydist <- renderUI({
    fluidPage(plotlyOutput("ydist_plot", height = "100%"))
  })
  
  
  ### basic plot: plot outputs -----------------------------------------------
  makeBasicPlot <- reactive({
    req(input$height_basic)
    req(input$height_basic > 0)
    
    # read in data
    data <- dataInputBasic()
    
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
          labs(x = as.character(input$var1), fill = color_label)
        plt2 <- plotBarplot(data = plt_df, x.str = "y1", fill.str = color_str) +
          labs(x = as.character(input$var2), fill = color_label)
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
                                     opacity = input$alpha_basic),
                       height = input$height_basic) %>%
          add_markers() %>%
          colorbar(title = input$color_basic) %>%
          layout(scene = list(xaxis = list(title = vars[1]),
                              yaxis = list(title = vars[2]),
                              zaxis = list(title = vars[3])))
      } else {
        plt <- plot_ly(plt_df, x = ~x, y = ~y, z = ~z,
                       marker = list(size = input$size_basic,
                                     opacity = input$alpha_basic),
                       height = input$height_basic) %>%
          add_markers() %>%
          layout(scene = list(xaxis = list(title = vars[1]),
                              yaxis = list(title = vars[2]),
                              zaxis = list(title = vars[3])))
      }
    }
    
    plt
  })
  output$basicPlot1 <- renderPlotly({
    plt <- makeBasicPlot()
    if (length(plt) == 2) {
      plt <- plt[[1]]
    } 
    
    if ("ggplot" %in% class(plt)) {
      plt <- plt %>%
        addPlotOptions(input = input, id = "basic", plotly = TRUE)
      return(ggplotly(plt, height = input$height_basic))
    } else {
      return(plt)
    }
  })
  output$basicPlot2 <- renderPlotly({
    plt <- makeBasicPlot()
    if (length(plt) == 2) {
      plt <- plt[[2]]
    } 
    plt <- plt %>%
      addPlotOptions(input = input, id = "basic", plotly = TRUE)
    ggplotly(plt, height = input$height_basic)
  })
  output$basicPlot <- renderUI({
    
    # number of inputted vars
    vars <- setdiff(c(input$var1, input$var2, input$var3), c("", "None"))
    num_vars <- length(vars)
    
    if (num_vars == 0) {
      if (input$datasplit_basic == "Training") {
        if (!trainDataUploaded()) {
          return(h4("Missing inputs. Please upload training X and y."))
        }
      } else if (input$datasplit_basic == "Test") {
        if (!testDataUploaded()) {
          return(h4("Missing inputs. Please upload test X and y."))
        }
      }
    }
    
    # read in data
    data <- dataInputBasic()
    
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
    } else if (num_vars == 0) {
      fluidPage(h4("Missing inputs. Please select variables to plot in left sidebar."),
                plotlyOutput("basicPlot1", height = "100%"))
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
    data <- dataInputPairs()
    
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
                     theme_function = addPlotOptions,
                     input = input, id = "pairs")
                     # axis_title_size = 14, axis_text_size = 10,
                     # legend_title_size = 14, legend_text_size = 10,
                     # strip_text_size = 14, 
                     # background_color = input$bg_pairs,
                     # grid_color = ifelse(input$grid_pairs, 
                     #                     "grey90", input$bg_pairs))
    plt
  })
  output$pairPlot <- renderPlot({
    if (!is.null(input$vars_pairs)) {
      plt <- makePairPlot()
    } else {
      # initialize empty plot
      plt <- ggplot(data.frame(x = 1:2, y = 1:2)) +
        labs(x = "", y = "") +
        myGGplotTheme()
    }
    plt
  },
  height = function() input$height_pairs)
  output$pairs <- renderUI({
    if (input$datasplit_pairs == "Training") {
      if (!trainDataUploaded()) {
        return(h4("Missing inputs. Please upload training X and y."))
      }
    } else if (input$datasplit_pairs == "Test") {
      if (!testDataUploaded()) {
        return(h4("Missing inputs. Please upload test X and y."))
      }
    }
    
    if (is.null(input$vars_pairs)) {
      fluidPage(h4("Missing inputs. Please select variables to plot in left sidebar."),
                plotOutput("pairPlot", height = "auto"))
    } else {
      fluidPage(plotOutput("pairPlot", height = "auto") %>%
                  withSpinner(color = "#18bc9c"))
    }
  })
  
  ### rf evaluation: table outputs --------------------------------------------
  observe({
    if (input$type_eval == "OOB") {
      X <- xtrainInput()
    } else if (input$type_eval == "Test") {
      X <- xtestInput()
    }
    updatePickerInput(session, "sample_heatmap_eval", choices = rownames(X))
    updateSliderInput(session, "p_heatmap_eval_rows",
                      value = 0, min = 0, max = nrow(X))
  })
  predYhat <- reactive({
    rf_fit <- rfFit()
    
    if (input$type_eval == "OOB") {
      X <- xtrainInput()
      y <- ytrainInput()
      oob_idx <- do.call(cbind, rf_fit$inbag.counts) == 0  # oob index
    } else if (input$type_eval == "Test") {
      X <- xtestInput()
      y <- ytestInput()
    }
    
    if (is.factor(y) & (nlevels(y)) == 2) {
      yhat <- predict(rf_fit, X, predict.all = TRUE,
                      num.threads = 1)$predictions
      if (!((min(yhat) >= 0) & (max(yhat) <= 1))) {
        yhat <- yhat - 1
      }
      if (input$type_eval == "OOB") {
        yhat <- rowSums(oob_idx * yhat) / rowSums(oob_idx)
      } else {
        yhat <- rowMeans(yhat)
      }
    } else if (is.factor(y)) {
      if (input$type_eval == "OOB") {
        yhat <- predict(rf_fit, data = X, predict.all = TRUE,
                        num.threads = 1)$predictions
        yhat[!oob_idx] <- NA
        yhat <- apply(yhat, 1, function(x) names(which.max(table(x)))) %>%
          as.numeric()
        yhat <- factor(levels(y)[yhat], levels = levels(y))
      } else {
        yhat <- predict(rf_fit, data = X, num.threads = 1)$predictions
      }
    } else {
      if (input$type_eval == "OOB") {
        yhat <- predict(rf_fit, data = X, predict.all = TRUE,
                        num.threads = 1)$predictions
        yhat[!oob_idx] <- NA
        yhat <- rowMeans(yhat, na.rm = TRUE)
      } else {
        yhat <- predict(rf_fit, data = X, num.threads = 1)$predictions
      }
    }
  })
  output$rfAccTable <- renderDT({
    
    yhat <- predYhat()
    if (input$type_eval == "OOB") {
      y <- ytrainInput()
    } else if (input$type_eval == "Test") {
      y <- ytestInput()
    }
    
    if (input$group_var_eval %in% c("None", "")) {
      group_var <- NULL
    } else {
      group_var <- input$group_var
    }
    
    if (is.factor(y) & (nlevels(y)) == 2) {
      eval_out <- bind_rows(evalPreds(y = y, yhat = (yhat >= 0.5) * 1,
                                      metric = c("Class", "BalancedClass"), 
                                      group = group_var),
                            evalPreds(y = y, yhat = yhat, 
                                      metric = c("AUC", "PR"), 
                                      group = group_var))
    } else if (is.factor(y)) {
      eval_out <- evalPreds(y = y, yhat = yhat,
                            metric = c("Class", "BalancedClass"),
                            group = group_var)
    } else {
      eval_out <- evalPreds(y = y, yhat = yhat,
                            metric = c("RMSE", "MSE", "R2", 
                                       "MAE", "Correlation"),
                            group = group_var)
    }
    
    if (is.na(input$digits_eval)) {
      digits <- NULL
    } else {
      digits <- input$digits_eval
    }
    
    dt <- myDT(
      eval_out, 
      digits = digits,
      sigfig = input$sigfig_eval,
      rownames = F,
      caption = tags$caption(
        style = "color: black; font-weight: bold; font-size: 125%",
        paste(input$type_eval, "Prediction Accuracy")
      ), 
      options = list(
        columnDefs = list(list(className = "dt-center",
                               targets = "_all")),
        dom = "t"
      )
    )
    dt
  })
  output$rfConfTable <- renderDT({
    yhat <- predYhat()
    if (input$type_eval == "OOB") {
      y <- ytrainInput()
    } else if (input$type_eval == "Test") {
      y <- ytestInput()
    }
    
    if (is.factor(y) & (nlevels(y)) == 2) {
      conf_mat <- as.data.frame.matrix(table(y, 1 * (yhat >= 0.5)))
    } else {
      conf_mat <- as.data.frame.matrix(table(y, yhat))
    }
    rownames(conf_mat) <- paste("Observed", rownames(conf_mat))
    colnames(conf_mat) <- paste("Predicted", colnames(conf_mat))
    conf_mat
    
    dt <- DT::datatable(conf_mat, rownames = T,
                        caption = tags$caption(
                          style = "color: black; font-weight: bold; font-size: 125%",
                          paste(input$type_eval, "Confusion Matrix")
                        ), 
                        options = list(
                          columnDefs = list(list(className = "dt-center",
                                                 targets = "_all")),
                          dom = "t"
                        ))
    dt
  })
  output$rfAcc <- renderUI({
    if (!rfDataUploaded()) {
      return(h4("Missing inputs. Please upload RF fit and training/test X and y."))
    }
    y <- ytrainInput()
    if (is.factor(y)) {
      out <- c(DTOutput("rfAccTable"), list(br()),
               DTOutput("rfConfTable"), list(br()))
    } else {
      out <- c(DTOutput("rfAccTable"), list(br()))
    }
    fluidPage(out)
  })
  
  ### rf evaluation: plot outputs --------------------------------------------
  predYhatTrees <- reactive({
    rf_fit <- rfFit()
    
    if (input$type_eval == "OOB") {
      X <- xtrainInput()
      y <- ytrainInput()
      oob_idx <- do.call(cbind, rf_fit$inbag.counts) == 0  # oob index
    } else if (input$type_eval == "Test") {
      X <- xtestInput()
      y <- ytestInput()
    }
    
    yhat <- predict(rf_fit, data = X, predict.all = TRUE,
                    num.threads = 1)$predictions
    
    if (is.factor(y) & (nlevels(y) == 2)) {
      if (!((min(yhat) >= 0) & (max(yhat) <= 1))) {
        yhat <- yhat - 1
      }
    }  
    if (input$type_eval == "OOB") {
      yhat[!oob_idx] <- NA
    } 
    rownames(yhat) <- rownames(X)
    yhat
  })
  makeRFPred <- reactive({
    yhat_trees <- predYhatTrees()
    if (input$type_eval == "OOB") {
      y <- ytrainInput()
      X <- xtrainInput()
    } else if (input$type_eval == "Test") {
      y <- ytestInput()
      X <- xtestInput()
    }
    
    if (is.factor(y)) {
      ylevels <- setdiff(unique(yhat_trees), NA)
      yhat <- map_dfr(1:nrow(yhat_trees),
                      function(i) {
                        x <- yhat_trees[i, ]
                        c(table(factor(x, levels = ylevels)) / sum(!is.na(x)))
                      }) %>%
        setNames(levels(y))
      if (ncol(yhat) == 2) {
        yhat <- yhat %>% select(2)
      }
      plt_df <- cbind(y = y, yhat) %>%
        gather(key = "Prediction", value = "Probability", -y) %>%
        mutate_if(is.character, as.factor)
      
    } else {
      yhat <- data.frame(yhat = rowMeans(yhat_trees, na.rm = T))
      plt_df <- cbind(y = y, yhat)
    }
    
    # initialize empty plot
    plt <- ggplot(plt_df) +
      labs(x = "", y = "") +
      myGGplotTheme()
    
    # make plot
    if (input$plotTypeEval == "histogram") {
      plt <- plotHistogram(data = plt_df, position = "identity",
                           fill.str = "y", x.str = "Probability",
                           bins = input$bins_eval, 
                           alpha = input$alpha_eval) +
        facet_wrap(~ Prediction, scales = "free_y") +
        labs(x = "Probability", fill = "y",
             title = paste(input$type_eval, "Predicted Probabilities"))
    } else if (input$plotTypeEval == "density") {
      plt <- plotDensity(data = plt_df, 
                         fill.str = "y", x.str = "Probability",
                         alpha = input$alpha_eval) +
        facet_wrap(~ Prediction, scales = "free_y") +
        labs(x = "Probability", fill = "y",
             title = paste(input$type_eval, "Predicted Probabilities"))
    } else if (input$plotTypeEval == "boxplot") {
      plt <- plotBoxplot(data = plt_df, fill.str = "y", x.str = "Probability",
                         horizontal = T) +
        facet_wrap(~ Prediction) +
        labs(x = "Probability", fill = "y",
             title = paste(input$type_eval, "Predicted Probabilities"))
    } else if (input$plotTypeEval == "scatterplot") {
      if (!(input$color_eval %in% c("", "None"))) {
        color.str <- input$color_eval
      } else {
        color.str <- NULL
      }
      plt <- plotScatter(data = cbind(plt_df, color = X[, color.str]), 
                         x.str = "y", y.str = "yhat", color.str = "color",
                         alpha = input$alpha_eval, size = input$size_eval) +
        labs(x = "Observed y", y = "Predicted y", color = color.str)
    }
    plt
  })
  output$rfPredPlot <- renderPlot({
    plt <- makeRFPred() %>%
      addPlotOptions(input = input, id = "eval", plotly = FALSE)
    plt
  },
  height = function() input$height_eval)
  output$rfPredPlotly <- renderPlotly({
    plt <- makeRFPred() %>%
      addPlotOptions(input = input, id = "eval", plotly = TRUE)
    ggplotly(plt, height = input$height_eval)
  })
  output$rfPred <- renderUI({
    if (input$plottype_eval == "GGplot") {
      out <- plotOutput("rfPredPlot", height = "auto") %>%
        withSpinner(color = "#18bc9c")
    } else if (input$plottype_eval == "Plotly") {
      out <- plotlyOutput("rfPredPlotly", height = "100%") %>%
        withSpinner(color = "#18bc9c")
    }
    out
  })  
  
  makeRFPredHeatmap <- reactive({
    req(input$height_heatmap_eval)
    req(input$height_heatmap_eval > 0)
    
    yhat_trees <- predYhatTrees()
    if (is.factor(ytrainInput())) {
      if (input$type_eval == "OOB") {
        y.groups <- ytrainInput()
      } else {
        y.groups <- ytestInput()
      }
      names(y.groups) <- rownames(yhat_trees)
    } else {
      y.groups <- NULL
    }
    
    if (input$sample_select_heatmap_eval == "Manually") {
      req(input$sample_select_heatmap_eval)
      yhat_trees <- yhat_trees[input$sample_heatmap_eval, ]
      y.groups <- y.groups[input$sample_heatmap_eval]
    } else if (input$sample_select_heatmap_eval == "Randomly") {
      req(input$p_heatmap_eval_rows > 0)
      if (nrow(yhat_trees) != input$p_heatmap_eval_rows) {
        keep_rows <- sort(sample(1:nrow(yhat_trees), input$p_heatmap_eval_rows, 
                                 replace = F))
        yhat_trees <- yhat_trees[keep_rows, ]
        y.groups <- y.groups[keep_rows]
      }
    }
    
    args_out <- getHeatmapArgs(input = input, id = "eval")
    
    plt <- plotHclustHeatmap(
      X = as.data.frame(yhat_trees), 
      x.labels = 1:ncol(yhat_trees),
      y.groups = y.groups,
      clust.x = args_out$clust.x,
      clust.y = args_out$clust.y,
      linkage.x = args_out$linkage.x,
      linkage.y = args_out$linkage.y,
      option = args_out$option,
      manual.fill = args_out$manual.fill, 
      col_quantile = args_out$col_quantile, 
      n_quantiles = args_out$n_quantiles
    ) +
      labs(x = "Tree", y = "Samples", fill = "Prediction", 
           title = "Prediciton Heatmap")
  })
  output$rfPredHeatmapPlot <- renderPlot({
    plt <- makeRFPredHeatmap() %>%
      addPlotOptions(input = input, id = "heatmap_eval", plotly = FALSE,
                     heatmap = TRUE)
    if (input$coord_flip_heatmap_eval) {
      plt <- plt + coord_flip()
    }
    plt
  },
  height = function() input$height_heatmap_eval)
  output$rfPredHeatmapPlotly <- renderPlotly({
    plt <- makeRFPredHeatmap() %>%
      addPlotOptions(input = input, id = "heatmap_eval", plotly = TRUE,
                     heatmap = TRUE)
    if (input$coord_flip_heatmap_eval) {
      plt <- plt + coord_flip()
    }
    ggplotly(plt, height = input$height_heatmap_eval)
  })
  output$rfPredHeatmap <- renderUI({
    if (((input$sample_select_heatmap_eval == "Manually") & 
         (is.null(input$sample_heatmap_eval))) |
        ((input$sample_select_heatmap_eval == "Randomly") &
         (input$p_heatmap_eval_rows == 0))) {
      flag <- TRUE
    } else {
      flag <- FALSE
    }
    
    if (flag) {
      out <- h4("Missing inputs. Please select samples to plot and other required inputs in left sidebar.")
    } else {
      if (input$plottype_heatmap_eval == "GGplot") {
        out <- plotOutput("rfPredHeatmapPlot", height = "auto")  %>%
          withSpinner(color = "#18bc9c")
      } else if (input$plottype_heatmap_eval == "Plotly") {
        out <- plotlyOutput("rfPredHeatmapPlotly", height = "100%")  %>%
          withSpinner(color = "#18bc9c")
      }
    }
    out
  })
  
  ### tree plot: plot outputs --------------------------------------------
  makeTreePlot <- reactive({
    req(input$height_tree)
    req(input$height_tree > 0)
    
    rf_fit <- rfFit()
    plotTree(rf.fit = rf_fit, tree.id = input$tree_id)
  })
  output$treePlot <- renderPlot({
    plt <- makeTreePlot()
    plt
  }, 
  height = function() input$height_tree)
  output$treeUI <- renderUI({
    if (is.null(input$file_rf$datapath)) {
      return(h4("Missing input. Please upload RF fit."))
    } else {
      plotOutput("treePlot", height = "auto")  %>% 
        withSpinner(color = "#18bc9c")
    }
  })
  
  ### vimp: vimp plot/table output -----------------------------------------
  makeRFVimp <- reactive({
    req(input$height_vimp)
    req(input$height_vimp > 0)
    
    rf_fit <- rfFit()
    vimp_out <- plotFeatureImp(rf.fit = rf_fit, 
                               min.imp.thr = input$min_imp_thr_vimp)
    vimp_out
  })
  output$rfVimpPlotly <- renderPlotly({
    vimp_out <- makeRFVimp()
    plt <- vimp_out$plot %>%
      addPlotOptions(input = input, id = "vimp", plotly = TRUE)
    ggplotly(plt, height = input$height_vimp)
  })
  output$rfVimpTable <- renderDT({
    vimp_out <- makeRFVimp()
    vimp_df <- vimp_out$vimp_df
    
    if (is.na(input$digits_vimp)) {
      digits <- NULL
    } else {
      digits <- input$digits_vimp
    }
    
    dt <- myDT(
      vimp_df,
      digits = digits,
      sigfig = input$sigfig_vimp,
      rownames = F,
      caption = tags$caption(
        style = "color: black; font-weight: bold; font-size: 125%",
        "Feature Importances"
      ), 
      options = list(
        columnDefs = list(list(className = "dt-center",
                               targets = "_all"))
      )
    )
    dt
  })
  output$rfVimp <- renderUI({
    if (is.null(input$file_rf$datapath)) {
      return(h4("Missing input. Please upload RF fit."))
    } else if (input$plottype_vimp == "Plotly") {
      out <- plotlyOutput("rfVimpPlotly", height = "100%")  %>%
        withSpinner(color = "#18bc9c")
    } else if (input$plottype_vimp == "Table") {
      out <- c(DTOutput("rfVimpTable"), list(br()))
    }
    fluidPage(out)
  })
  
  ### vimp: feature splits plot ouptuts ------------------------------------
  makeRFSplits <- reactive({
    req(input$height_splits)
    req(input$height_splits > 0)
    req(input$vars_vimp)
    
    rf_fit <- rfFit()
    X <- NULL
    y <- NULL
    if (input$datasplit_splits == "Training") {
      if (trainDataUploaded()) {
        X <- xtrainInput()
        y <- ytrainInput()
      }
    } else if (input$datasplit_splits == "Test") {
      if (testDataUploaded()) {
        X <- xtestInput()
        y <- ytestInput()
      }
    }
    
    splits_out <- plotFeatureSplits(rf.fit = rf_fit, X = X, y = y,
                                    features = input$vars_vimp) +
      labs(title = "Distribution of RF Split Thresholds")
    splits_out
  })
  output$rfSplitsPlot <- renderPlot({
    splits_out <- makeRFSplits()
    plt <- makeRFSplits() %>%
      addPlotOptions(input = input, id = "splits", plotly = FALSE)
    plt
  }, 
  height = function() input$height_splits)
  output$rfSplitsPlotly <- renderPlotly({
    plt <- makeRFSplits() %>%
      addPlotOptions(input = input, id = "splits", plotly = TRUE)
    ggplotly(plt, height = input$height_splits)
  })
  output$rfSplits <- renderUI({
    if (is.null(input$vars_vimp)) {
      return(h4("Missing inputs. Please select variables to plot in left sidebar."))
    }
    if (input$plottype_splits == "GGplot") {
      out <- plotOutput("rfSplitsPlot", height = "auto") %>%
        withSpinner(color = "#18bc9c")
    } else if (input$plottype_splits == "Plotly") {
      out <- plotlyOutput("rfSplitsPlotly", height = "100%") %>%
        withSpinner(color = "#18bc9c")
    }
    out
  })
  
  ### vimp: local stability plot ouptuts ------------------------------------
  makeLocalStability <- reactive({
    req(input$height_lstab)
    req(input$height_lstab > 0)
    req(input$vars_vimp)
    
    rf_fit <- rfFit()
    if (input$datasplit_lstab == "OOB") {
      X <- xtrainInput()
      y <- ytrainInput()
    } else if (input$datasplit_lstab == "Test") {
      X <- xtestInput()
      y <- ytestInput()
    }
    
    args_out <- getHeatmapArgs(input = input, id = "lstab")
    if (!is.null(args_out$manual.fill)) {
      z.range <- c(0, 1)
    } else {
      z.range <- NULL
    }
    
    lstab_out <- plotLocalStabilityRF(
      rf.fit = rf_fit, X = X, y = y, 
      features = c(paste0(input$vars_vimp, "+"), paste0(input$vars_vimp, "-")), 
      first.only = input$first_lstab, 
      oob = input$datasplit_lstab == "OOB", 
      return.pval = TRUE,
      nperm = input$nperm_lstab, 
      point.size = input$size_lstab, 
      heights = as.numeric(unlist(strsplit(input$heights_lstab, ","))),
      clust.x = args_out$clust.x,
      clust.y = args_out$clust.y,
      linkage.x = args_out$linkage.x,
      linkage.y = args_out$linkage.y,
      option = args_out$option,
      manual.fill = args_out$manual.fill,
      col_quantile = args_out$col_quantile, 
      n_quantiles = args_out$n_quantiles,
      z.range = z.range
    )
    lstab_out
  })
  output$localStabilityPlot <- renderPlot({
    lstab_out <- makeLocalStability()
    if (input$show_lstab == "Heatmap") {
      plt <- lstab_out$stab_plt
      hmap <- TRUE
    } else if (input$show_lstab == "P-values") {
      plt <- lstab_out$pval_plt
      hmap <- FALSE
    } else if (input$show_lstab == "Both") {
      plt <- lstab_out$plot
      hmap <- FALSE
    }
    plt <- plt %>%
      addPlotOptions(input = input, id = "lstab", heatmap = hmap, plotly = F)
    
    if (input$show_lstab == "Both") {
      plt[[1]] <- plt[[1]] +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.x = element_blank(),
              axis.text.y = element_text(family = "Helvetica", 
                                         size = input$ytext_lstab))
    } else if (input$show_lstab == "Heatmap") {
      if (input$coord_flip_heatmap_lstab) {
        plt <- plt + coord_flip()
      }
    }
    
    plt
  }, 
  height = function() input$height_lstab)
  output$localStabilityPlotly <- renderPlotly({
    lstab_out <- makeLocalStability()
    if (input$show_lstab == "Heatmap") {
      plt <- lstab_out$stab_plt
      hmap <- TRUE
    } else if (input$show_lstab == "P-values") {
      plt <- lstab_out$pval_plt
      hmap <- FALSE
    } else if (input$show_lstab == "Both") {
      plt <- lstab_out$plot
      hmap <- FALSE
    }
    plt <- plt %>%
      addPlotOptions(input = input, id = "lstab", heatmap = hmap, plotly = T)
    
    if (input$show_lstab == "Heatmap") {
      if (input$coord_flip_heatmap_lstab) {
        plt <- plt + coord_flip()
      }
    }
    
    ggplotly(plt, height = input$height_lstab)
  })
  output$localStability <- renderUI({
    if (input$datasplit_lstab == "OOB") {
      if (!trainDataUploaded()) {
        return(h4("No local stability results. Please upload training X and y."))
      }
    } else if (input$datasplit_lstab == "Test") {
      if (!testDataUploaded()) {
        return(h4("No local stability results. Please upload test X and y."))
      }
    }
    
    if ((input$plottype_lstab == "Plotly") & (input$show_lstab != "Both")) {
      out <- plotlyOutput("localStabilityPlotly", height = "100%") %>%
        withSpinner(color = "#18bc9c")
    } else {
      out <- plotOutput("localStabilityPlot", height = "auto") %>%
        withSpinner(color = "#18bc9c")
    }
    out
  })
  
  ### vimp: local stability distribution plot ouptuts -------------------------
  makeLocalStabilityDistPlot <- reactive({
    lstab_out <- makeLocalStability()
    stab_df <- lstab_out$stab_df
    
    if (input$datasplit_lstab == "OOB") {
      X <- xtrainInput()
      y <- ytrainInput()
    } else if (input$datasplit_lstab == "Test") {
      X <- xtestInput()
      y <- ytestInput()
    }
    
    plt_df <- stab_df %>%
      select(all_of(input$vars_lstab_dist)) %>%
      cbind(., y = y) %>%
      gather(key = "Feature", value = "Stability", -y)
    
    # initialize empty plot
    plt <- ggplot(plt_df) +
      labs(x = "", y = "") +
      myGGplotTheme()
    
    # make plot
    if (input$plotTypeLstab == "histogram") {
      plt <- plotHistogram(data = plt_df, position = "identity",
                           fill.str = "y", x.str = "Stability",
                           bins = input$bins_lstab_dist, 
                           alpha = input$alpha_lstab_dist) +
        facet_wrap(~ Feature, scales = "free_y") +
        labs(x = "Local Stability Score")
    } else if (input$plotTypeLstab == "density") {
      plt <- plotDensity(data = plt_df, 
                         fill.str = "y", x.str = "Stability",
                         alpha = input$alpha_lstab_dist) +
        facet_wrap(~ Feature, scales = "free_y") +
        labs(x = "Local Stability Score")
    } else if (input$plotTypeLstab == "boxplot") {
      plt <- plotBoxplot(data = plt_df, fill.str = "y", x.str = "Stability",
                         horizontal = T) +
        facet_wrap(~ Feature) +
        labs(x = "Local Stability Score")
    } else if (input$plotTypeLstab == "scatterplot") {
      if (!(input$color_lstab_dist %in% c("", "None"))) {
        color.str <- input$color_lstab_dist
      } else {
        color.str <- NULL
      }
      plt <- plotScatter(data = cbind(plt_df, color = X[, color.str]), 
                         x.str = "Stability", y.str = "y", color.str = "color", 
                         alpha = input$alpha_lstab_dist,
                         size = input$size_lstab_dist) +
        facet_wrap(~ Feature) +
        labs(x = "Local Stability Score", y = "Observed y", color = color.str)
    }
    plt +
      labs(title = "Distribution of Local Stability Scores")
  })
  output$localStabilityDistPlot <- renderPlot({
    plt <- makeLocalStabilityDistPlot() %>%
      addPlotOptions(input = input, id = "lstab_dist", plotly = FALSE)
    plt
  },
  height = function() input$height_lstab_dist)
  output$localStabilityDistPlotly <- renderPlotly({
    plt <- makeLocalStabilityDistPlot() %>%
      addPlotOptions(input = input, id = "lstab_dist", plotly = TRUE)
    ggplotly(plt, height = input$height_lstab_dist)
  })
  output$localStabilityDist <- renderUI({
    if (input$plottype_lstab_dist == "GGplot") {
      out <- plotOutput("localStabilityDistPlot", height = "auto") %>%
        withSpinner(color = "#18bc9c")
    } else if (input$plottype_lstab_dist == "Plotly") {
      out <- plotlyOutput("localStabilityDistPlotly", height = "100%") %>%
        withSpinner(color = "#18bc9c")
    }
    out
  })  
  
  ### int: irf results plot outputs -------------------------------------------
  makeiRFResults <- reactive({
    req(input$height_irf_res)
    req(input$height_irf_res > 0)
    
    irf_fit <- irfFit()
    req(irf_fit)
    
    if (is.na(input$top_p_irf_res)) {
      top_p <- NULL
    } else {
      top_p <- input$top_p_irf_res
    }
    if (input$rank_by_irf_res == "None") {
      rank_by <- NULL
    } else {
      rank_by <- input$rank_by_irf_res
    }
    
    irf_out <- plotiRFResults(irf.out = irf_fit, 
                              ints = input$vars_int,
                              bar_width = input$bar_width_irf_res, 
                              text_size = input$text_size_irf_res,
                              hjust = input$hjust_irf_res,
                              digits = input$pltdigits_irf_res,
                              metrics = input$metrics_irf_res, 
                              top_p = top_p,
                              rank_by = rank_by) +
      labs(title = "iRF Results Summary", x = "", y = "")
    irf_out
  })
  output$iRFResultsPlot <- renderPlot({
    plt <- makeiRFResults() %>%
      addPlotOptions(input = input, id = "irf_res", plotly = FALSE)
    plt
  },
  height = function() input$height_irf_res)
  output$iRFResultsPlotly <- renderPlotly({
    plt <- makeiRFResults() %>%
      addPlotOptions(input = input, id = "irf_res", plotly = TRUE)
    ggplotly(plt, height = input$height_irf_res)
  })
  output$iRFResultsTable <- renderDT({
    irf_fit <- irfFit()
    req(irf_fit)
    irf_int <- irf_fit$interaction
    
    if (is.na(input$digits_irf_res)) {
      digits <- NULL
    } else {
      digits <- input$digits_irf_res
    }
    
    dt <- myDT(
      irf_int,
      digits = digits,
      sigfig = input$sigfig_irf_res,
      rownames = F,
      caption = tags$caption(
        style = "color: black; font-weight: bold; font-size: 125%",
        "iRF Interaction Results Summary"
      ), 
      options = list(
        columnDefs = list(list(className = "dt-center",
                               targets = "_all")),
        scrollX = TRUE
      )
    )
  })
  output$iRFResults <- renderUI({
    if (is.null(input$file_rf$datapath)) {
      return(h4("No iRF results. Please upload iRF fit."))
    } else {
      rf_fit <- rfInput()
      if (!("rf.list" %in% names(rf_fit))) {
        return(h4("No iRF results. Uploaded RF fit is not of class iRF."))
      }
    }
    
    if (input$plottype_irf_res == "GGplot") {
      out <- plotOutput("iRFResultsPlot", height = "auto")  %>%
        withSpinner(color = "#18bc9c")
    } else if (input$plottype_irf_res == "Plotly") {
      out <- plotlyOutput("iRFResultsPlotly", height = "100%")  %>%
        withSpinner(color = "#18bc9c")
    } else if (input$plottype_irf_res == "Table") {
      out <- c(DTOutput("iRFResultsTable"), list(br()))
    }
    fluidPage(out)
  })
  
  ### int: epitree results plot outputs ---------------------------------------
  makeEpitreeResults <- reactive({
    epitree_out <- epitreeInput()
    y <- epitree_out$data$ytrain
    
    if (is.factor(y) & (nlevels(y) == 2) & !is.null(input$file_rf$datapath)) {
      rf_fit <- rfFit()
      Xtest <- xtestInput()
      yhat <- predict(rf_fit, Xtest, predict.all = TRUE,
                      num.threads = 1)$predictions - 1
      yhat <- rowMeans(yhat)
    } else {
      yhat <- NULL
    }
    
    epitree_res <- plotEpitreeResults(epitree.out = epitree_out, 
                                      ints = input$vars_int, 
                                      ypreds = yhat, 
                                      bar_width = input$bar_width_epitree_res,
                                      text_size = input$text_size_epitree_res,
                                      hjust = input$hjust_epitree_res,
                                      digits = input$pltdigits_epitree_res)
    epitree_res
  })
  output$epitreeResultsPlot <- renderPlot({
    epitree_res <- makeEpitreeResults()
    if (input$show_epitree_res == "P-values") {
      plt <- epitree_res$plotPV
    } else if (input$show_epitree_res == "Stability") {
      plt <- epitree_res$plotStab
    } else if (input$show_epitree_res == "Prediction Accuracy") {
      plt <- epitree_res$plotPA
    } else if (input$show_epitree_res == "All") {
      plt <- epitree_res$plot +
        plot_annotation(
          title = "Epitree Results Summary",
          theme = theme(
            plot.title = element_text(family = "Helvetica", face = "bold",
                                      size = input$title_epitree_res)
          )
        )
    }
    
    plt <- plt %>%
      addPlotOptions(input = input, id = "epitree_res", plotly = FALSE)
    
    if (input$show_epitree_res != "All") {
      plt <- plt +
        labs(title = paste("Epitree", input$show_epitree_res))
    } else {
      plt[[2]] <- plt[[2]] +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank())
      plt[[3]] <- plt[[3]] +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank())
    }
    plt
  },
  height = function() input$height_epitree_res)
  output$epitreeResultsPlotly <- renderPlotly({
    epitree_res <- makeEpitreeResults()
    if (input$show_epitree_res == "P-values") {
      plt <- epitree_res$plotPV
    } else if (input$show_epitree_res == "Stability") {
      plt <- epitree_res$plotStab
    } else if (input$show_epitree_res == "Prediction Accuracy") {
      plt <- epitree_res$plotPA
    } else if (input$show_epitree_res == "All") {
      plt <- epitree_res$plot +
        plot_annotation(
          title = "Epitree Results Summary",
          theme = theme(
            plot.title = element_text(family = "Helvetica", face = "bold",
                                      size = input$title_epitree_res)
          )
        )
    }
    
    plt <- plt %>%
      addPlotOptions(input = input, id = "epitree_res", plotly = TRUE) 
    
    if (input$show_epitree_res != "All") {
      plt <- plt +
        labs(title = paste("Epitree", input$show_epitree_res))
    }
    ggplotly(plt, height = input$height_epitree_res)
  })
  output$epitreeResults <- renderUI({
    if (is.null(input$file_epitree$datapath)) {
      return(h4("No epiTree results. Please upload epiTree fit."))
    }
    
    if ((input$plottype_epitree_res == "GGplot") | 
        (input$show_epitree_res == "All")) {
      out <- plotOutput("epitreeResultsPlot", height = "auto") %>%
        withSpinner(color = "#18bc9c")
    } else if (input$plottype_epitree_res == "Plotly") {
      out <- plotlyOutput("epitreeResultsPlotly", height = "100%") %>%
        withSpinner(color = "#18bc9c")
    }
    out
  })  
  
  ### int: local stability plot ouptuts ------------------------------------
  makeIntLocalStability <- reactive({
    req(input$height_lstab_int)
    req(input$height_lstab_int > 0)
    req(input$vars_int)
    
    rf_fit <- rfFit()
    if (input$datasplit_lstab_int == "OOB") {
      X <- xtrainInput()
      y <- ytrainInput()
    } else if (input$datasplit_lstab_int == "Test") {
      X <- xtestInput()
      y <- ytestInput()
    }
    
    args_out <- getHeatmapArgs(input = input, id = "lstab_int")
    if (!is.null(args_out$manual.fill)) {
      z.range <- c(0, 1)
    } else {
      z.range <- NULL
    }
    
    lstab_out <- plotLocalStabilityRF(
      rf.fit = rf_fit, X = X, y = y, 
      features = "None",
      ints = input$vars_int, 
      first.only = input$first_lstab_int, 
      oob = input$datasplit_lstab_int == "OOB", 
      return.pval = TRUE,
      nperm = input$nperm_lstab_int, 
      point.size = input$size_lstab_int, 
      heights = as.numeric(unlist(strsplit(input$heights_lstab_int, ","))),
      clust.x = args_out$clust.x,
      clust.y = args_out$clust.y,
      linkage.x = args_out$linkage.x,
      linkage.y = args_out$linkage.y,
      option = args_out$option,
      manual.fill = args_out$manual.fill,
      col_quantile = args_out$col_quantile, 
      n_quantiles = args_out$n_quantiles,
      z.range = z.range
    )
    
    lstab_out
  })
  output$intLocalStabilityPlot <- renderPlot({
    lstab_out <- makeIntLocalStability()
    if (input$show_lstab_int == "Heatmap") {
      plt <- lstab_out$stab_plt
      hmap <- TRUE
    } else if (input$show_lstab_int == "P-values") {
      plt <- lstab_out$pval_plt
      hmap <- FALSE
    } else if (input$show_lstab_int == "Both") {
      plt <- lstab_out$plot
      hmap <- FALSE
    }
    
    plt <- plt %>%
      addPlotOptions(input = input, id = "lstab_int", plotly = FALSE, 
                     heatmap = hmap)
    
    if (input$show_lstab_int == "Both") {
      plt[[1]] <- plt[[1]] +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.x = element_blank(),
              axis.text.y = element_text(family = "Helvetica", 
                                         size = input$ytext_lstab_int))
    } else if (input$show_lstab_int == "Heatmap") {
      if (input$coord_flip_heatmap_lstab_int) {
        plt <- plt + coord_flip()
      }
    }
    plt
  }, 
  height = function() input$height_lstab_int)
  output$intLocalStabilityPlotly <- renderPlotly({
    lstab_out <- makeIntLocalStability()
    if (input$show_lstab_int == "Heatmap") {
      plt <- lstab_out$stab_plt
      hmap <- TRUE
    } else if (input$show_lstab_int == "P-values") {
      plt <- lstab_out$pval_plt
      hmap <- FALSE
    } else if (input$show_lstab_int == "Both") {
      plt <- lstab_out$plot
      hmap <- FALSE
    }
    plt <- plt %>%
      addPlotOptions(input = input, id = "lstab_int", plotly = TRUE, 
                     heatmap = hmap)
    
    if (input$show_lstab_int == "Heatmap") {
      if (input$coord_flip_heatmap_lstab_int) {
        plt <- plt + coord_flip()
      }
    }
    
    ggplotly(plt, height = input$height_lstab_int)
  })
  output$intLocalStability <- renderUI({
    if (is.null(input$file_rf$datapath)) {
      return(NULL)
    } else if (!("rf.list" %in% names(rfInput()))) {
      return(NULL)
    } else if (input$datasplit_lstab_int == "OOB") {
      if (!trainDataUploaded()) {
        return(h4("No local interaction stability results. Please upload training X and y."))
      } 
    } else if (input$datasplit_lstab_int == "Test") {
      if (!testDataUploaded()) {
        return(h4("No local interaction stability results. Please upload test X and y."))
      }
    } 
    if (is.null(input$vars_int)) {
      return(h4("No local interaction stability results. Please select interactions of interest in left sidebar."))
    }
    
    if ((input$plottype_lstab_int == "Plotly") & (input$show_lstab_int != "Both")) {
      out <- plotlyOutput("intLocalStabilityPlotly", height = "100%") %>%
        withSpinner(color = "#18bc9c")
    } else {
      out <- plotOutput("intLocalStabilityPlot", height = "auto") %>%
        withSpinner(color = "#18bc9c")
    }
    out
  })
  
  ### int: local stability distribution plot ouptuts -------------------------
  makeIntLocalStabilityDistPlot <- reactive({
    lstab_out <- makeIntLocalStability()
    stab_df <- lstab_out$stab_df
    
    if (input$datasplit_lstab_int == "OOB") {
      X <- xtrainInput()
      y <- ytrainInput()
    } else if (input$datasplit_lstab_int == "Test") {
      X <- xtestInput()
      y <- ytestInput()
    }
    
    plt_df <- stab_df %>%
      select(all_of(input$vars_lstab_dist_int)) %>%
      cbind(., y = y) %>%
      gather(key = "Feature", value = "Stability", -y)
    
    # initialize empty plot
    plt <- ggplot(plt_df) +
      labs(x = "", y = "") +
      myGGplotTheme()
    
    # make plot
    if (input$plotTypeLstab_int == "histogram") {
      plt <- plotHistogram(data = plt_df, position = "identity",
                           fill.str = "y", x.str = "Stability",
                           bins = input$bins_lstab_dist_int, 
                           alpha = input$alpha_lstab_dist_int) +
        facet_wrap(~ Feature, scales = "free_y") +
        labs(x = "Local Stability Score")
    } else if (input$plotTypeLstab_int == "density") {
      plt <- plotDensity(data = plt_df, 
                         fill.str = "y", x.str = "Stability",
                         alpha = input$alpha_lstab_dist_int) +
        facet_wrap(~ Feature, scales = "free_y") +
        labs(x = "Local Stability Score")
    } else if (input$plotTypeLstab_int == "boxplot") {
      plt <- plotBoxplot(data = plt_df, fill.str = "y", x.str = "Stability",
                         horizontal = T) +
        facet_wrap(~ Feature) +
        labs(x = "Local Stability Score")
    } else if (input$plotTypeLstab_int == "scatterplot") {
      if (!(input$color_lstab_dist_int %in% c("", "None"))) {
        color.str <- input$color_lstab_dist_int
      } else {
        color.str <- NULL
      }
      plt <- plotScatter(data = cbind(plt_df, color = X[, color.str]), 
                         x.str = "Stability", y.str = "y", color.str = "color", 
                         alpha = input$alpha_lstab_dist_int,
                         size = input$size_lstab_dist_int) +
        facet_wrap(~ Feature) +
        labs(x = "Local Stability Score", y = "Observed y", color = color.str)
    }
    plt +
      labs(title = "Distribution of Local Stability Scores")
  })
  output$intLocalStabilityDistPlot <- renderPlot({
    plt <- makeIntLocalStabilityDistPlot() %>%
      addPlotOptions(input = input, id = "lstab_dist_int", plotly = FALSE)
    plt
  },
  height = function() input$height_lstab_dist_int)
  output$intLocalStabilityDistPlotly <- renderPlotly({
    plt <- makeIntLocalStabilityDistPlot() %>%
      addPlotOptions(input = input, id = "lstab_dist_int", plotly = TRUE)
    ggplotly(plt, height = input$height_lstab_dist_int)
  })
  output$intLocalStabilityDist <- renderUI({
    if (input$plottype_lstab_dist_int == "GGplot") {
      out <- plotOutput("intLocalStabilityDistPlot", height = "auto") %>%
        withSpinner(color = "#18bc9c")
    } else if (input$plottype_lstab_dist_int == "Plotly") {
      out <- plotlyOutput("intLocalStabilityDistPlotly", height = "100%") %>%
        withSpinner(color = "#18bc9c")
    }
    out
  })  
  
  ### int: individual interaction plot outputs --------------------------------
  makeiRFIntPlot <- reactive({
    req(input$height_int)
    req(input$height_int > 0)
    req(input$var_int)
    
    rf_fit <- rfFit()
    if (input$datasplit_int == "Training") {
      X <- xtrainInput()
      y <- ytrainInput()
    } else if (input$datasplit_int == "Test") {
      X <- xtestInput()
      y <- ytestInput()
    }
    
    if (input$show_int == "iRF 3D Surface") {
      plt <- plotIntSurface(X = X, int = input$var_int, y = y, fit = rf_fit,
                            type = "plotly")
    } else if (input$show_int == "iRF 2D Surface") {
      plt <- plotIntSurface(X = X, int = input$var_int, y = y, fit = rf_fit,
                            type = "ggplot")
    }
    plt
  })
  output$iRFIntPlot <- renderPlot({
    plt <- makeiRFIntPlot() %>%
      addPlotOptions(input = input, id = "int", plotly = FALSE)
    plt
  },
  height = function() input$height_int)
  output$iRFIntPlotly <- renderPlotly({
    plt <- makeiRFIntPlot() %>%
      addPlotOptions(input = input, id = "int", plotly = TRUE)
    ggplotly(plt, height = input$height_int)
  })
  output$iRFInt3DPlotly <- renderPlotly({
    plt <- makeiRFIntPlot()
    plt
  })
  
  makeEpitreeIntPlot <- reactive({
    req(input$height_int)
    req(input$height_int > 0)
    req(input$var_int)
    
    epitree_out <- epitreeInput()
    out <- plotEpitreeInt(epitree.out = epitree_out, int = input$var_int)
  })
  output$epitreeIntPlot <- renderPlot({
    out <- makeEpitreeIntPlot()
    
    plt_names <- c("noepi.heatmap", "epi.heatmap", 
                   "tr.data.heatmap", "data.heatmap")
    plt_ls <- list()
    for (plt_name in plt_names) {
      plt_ls[[plt_name]] <- out[[plt_name]] %>%
        addPlotOptions(input = input, id = "int", plotly = FALSE)
    }
    ggarrange(plotlist = plt_ls, ncol = 4, nrow = 1, 
              common.legend = T, legend = "bottom")
  }, 
  height = function() input$height_int)
  output$epitreeIntTrees <- renderPlot({
    out <- makeEpitreeIntPlot()
    tree.ls <- out$tree.ls
    varnames <- names(tree.ls)[1:2]
    
    ytrain <- ytrainInput()
    if (is.factor(ytrain) & (nlevels(ytrain) == 2)) {
      ytrain <- as.numeric(ytrain) - 1
    }
    tree.comb.plt <- map(tree.ls[1:2],
                         function(x) {
                           x$frame$yval <- x$frame$yval + mean(ytrain)
                           return(x)
                         })

    par(mfrow = c(1, 3))
    rpart.plot(tree.comb.plt[[1]], box.palette = "RdBu", 
               main = paste0("CART(", varnames[1], ")"))
    rpart.plot(tree.comb.plt[[2]], box.palette = "RdBu",
               main = paste0("CART(", varnames[2], ")"))
    rpart.plot(tree.ls[[3]], box.palette = "RdBu", 
               main = paste0("CART(", paste(varnames, collapse = ", "), ")"))
  }, 
  height = function() input$height_int)
  
  makeLocalStabilityIntPlot <- reactive({
    req(input$height_int)
    req(input$height_int > 0)
    req(input$var_int)
    
    rf_fit <- rfFit()
    if (input$datasplit_int == "Training") {
      X <- xtrainInput()
      y <- ytrainInput()
    } else if (input$datasplit_int == "Test") {
      X <- xtestInput()
      y <- ytestInput()
    }
    
    lstab_out <- plotLocalStabilityRF(
      rf.fit = rf_fit, X = X, y = y, 
      features = str_split(input$var_int, "_")[[1]],
      ints = input$var_int, 
      first.only = input$first_lstab_ft_int, 
      oob = input$datasplit_int == "Training", 
      return.pval = TRUE,
      nperm = input$nperm_lstab_ft_int, 
      point.size = input$size_lstab_ft_int, 
      heights = as.numeric(unlist(strsplit(input$heights_lstab_ft_int, ",")))
    )
    
    lstab_out
  })
  output$localStabilityIntPlot <- renderPlot({
    lstab_out <- makeLocalStabilityIntPlot()
    if (input$show_lstab_ft_int == "Heatmap") {
      plt <- lstab_out$stab_plt
      hmap <- TRUE
    } else if (input$show_lstab_ft_int == "P-values") {
      plt <- lstab_out$pval_plt
      hmap <- FALSE
    } else if (input$show_lstab_ft_int == "Both") {
      plt <- lstab_out$plot
      hmap <- FALSE
    }
    plt <- plt %>%
      addPlotOptions(input = input, id = "int", plotly = FALSE, heatmap = hmap)
    
    if (input$show_lstab_ft_int == "Both") {
      plt[[1]] <- plt[[1]] +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.x = element_blank(),
              axis.text.y = element_text(family = "Helvetica", 
                                         size = input$ytext_int))
    }
    plt
  }, 
  height = function() input$height_int)
  output$localStabilityIntPlotly <- renderPlotly({
    lstab_out <- makeLocalStabilityIntPlot()
    if (input$show_lstab_ft_int == "Heatmap") {
      plt <- lstab_out$stab_plt
      hmap <- TRUE
    } else if (input$show_lstab_ft_int == "P-values") {
      plt <- lstab_out$pval_plt
      hmap <- FALSE
    } else if (input$show_lstab_ft_int == "Both") {
      plt <- lstab_out$plot
      hmap <- FALSE
    }
    plt <- plt %>%
      addPlotOptions(input = input, id = "int", plotly = TRUE, heatmap = hmap)
    ggplotly(plt, height = input$height_int)
  })
  
  output$intIndivPlot <- renderUI({
    if (is.null(input$file_rf$datapath)) {
      return(NULL)
    } else if (!("rf.list" %in% names(rfInput()))) {
      return(NULL)
    } else if (input$datasplit_int == "Training") {
      if (!trainDataUploaded()) {
        return(NULL)
      } 
    } else if (input$datasplit_int == "Test") {
      if (!testDataUploaded()) {
        return(NULL)
      }
    } 
    if (input$var_int == "") {
      return(h4("No individual interaction results. Please select interaction of interest above."))
    }
    
    if (input$show_int == "iRF 3D Surface") {
      out <- plotlyOutput("iRFInt3DPlotly", height = "auto") %>%
        withSpinner(color = "#18bc9c")
    } else if (input$show_int == "iRF 2D Surface") {
      if (input$plottype_int == "GGplot") {
        out <- plotOutput("iRFIntPlot", height = "100%") %>%
          withSpinner(color = "#18bc9c")
      } else if (input$plottype_int == "Plotly") {
        out <- plotlyOutput("iRFIntPlotly", height = "auto") %>%
          withSpinner(color = "#18bc9c")
      }
    } else if (input$show_int == "epiTree Tree") {
      if (is.null(input$file_epitree$datapath)) {
        return(h4("No epitree results. Please upload epiTree fit."))
      }
      out <- plotOutput("epitreeIntTrees", height = "100%") %>%
        withSpinner(color = "#18bc9c")
    } else if (input$show_int == "epiTree Heatmaps") {
      if (is.null(input$file_epitree$datapath)) {
        return(h4("No epitree results. Please upload epiTree fit."))
      }
      out <- plotOutput("epitreeIntPlot", height = "100%") %>%
        withSpinner(color = "#18bc9c")
    } else if (input$show_int == "Local Stability Heatmap") {
      if (input$plottype_int == "GGplot") {
        out <- plotOutput("localStabilityIntPlot", height = "100%") %>%
          withSpinner(color = "#18bc9c")
      } else if (input$plottype_int == "Plotly") {
        out <- plotlyOutput("localStabilityIntPlotly", height = "auto") %>%
          withSpinner(color = "#18bc9c")
      }
    }
    out
  }) 
  
  ### int: interaction heatmap plot outputs -----------------------------------
  makeIntHeatmap <- reactive({
    
    irf_fit <- irfFit()
    rf_fit <- rfFit()
    if (input$datasplit_heatmap_int == "Training") {
      X <- xtrainInput()
      y <- ytrainInput()
    } else if (input$datasplit_heatmap_int == "Test") {
      X <- xtestInput()
      y <- ytestInput()
    }
    
    read_forest <- readForest(rf_fit, x = X, varnames.grp = colnames(X),
                              oob.importance = FALSE)
    out <- plotIntHeatmap(irf.fit = irf_fit, X = X, y = y, 
                          read.forest = read_forest,
                          ints = input$vars_int, 
                          clust.x = input$cluster_x_heatmap_int == "Hierarchical Clustering", 
                          clust.y = input$cluster_y_heatmap_int == "Hierarchical Clustering")
    out
  })
  output$intHeatmapPlot <- renderPlot({
    out <- makeIntHeatmap()
    if (input$show_heatmap_int == "Active Interactions") {
      plt <- out$active.heatmap
    } else if (input$show_heatmap_int == "Prediction Heatmap") {
      plt <- out$pred.heatmap
    } else if (input$show_heatmap_int == "Prediction Error Heatmap") {
      plt <- out$err.heatmaps[[input$err_heatmap_int]]
    }
    
    plt <- plt %>%
      addPlotOptions(input = input, id = "int", plotly = FALSE, heatmap = TRUE)
    plt
  },
  height = function() input$height_heatmap_int)
  output$intHeatmapPlotly <- renderPlotly({
    out <- makeIntHeatmap()
    if (input$show_heatmap_int == "Active Interactions") {
      plt <- out$active.heatmap
    } else if (input$show_heatmap_int == "Prediction Heatmap") {
      plt <- out$pred.heatmap
    } else if (input$show_heatmap_int == "Prediction Error Heatmap") {
      plt <- out$err.heatmaps[[input$err_heatmap_int]]
    }
    
    plt <- plt %>%
      addPlotOptions(input = input, id = "int", plotly = TRUE, heatmap = hmap)
    ggplotly(plt, height = input$height_heatmap_int)
  })
  output$intHeatmap <- renderUI({
    if (input$plottype_heatmap_int == "GGplot") {
      out <- plotOutput("intHeatmapPlot", height = "auto") %>%
        withSpinner(color = "#18bc9c")
    } else if (input$plottype_heatmap_int == "Plotly") {
      out <- plotlyOutput("intHeatmapPlotly", height = "100%") %>%
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
  outputOptions(output, "fileType_xtrain", suspendWhenHidden = FALSE)  
  outputOptions(output, "fileType_xtest", suspendWhenHidden = FALSE)  
  outputOptions(output, "fileType_ytrain", suspendWhenHidden = FALSE)  
  outputOptions(output, "fileType_ytest", suspendWhenHidden = FALSE)  
  outputOptions(output, "rf_type", suspendWhenHidden = FALSE)  
  outputOptions(output, 'trainDataUploaded', suspendWhenHidden = FALSE)
  outputOptions(output, 'testDataUploaded', suspendWhenHidden=FALSE)
  outputOptions(output, 'yDataUploaded', suspendWhenHidden=FALSE)
  outputOptions(output, 'basicVarsInput', suspendWhenHidden=FALSE)
  outputOptions(output, 'rfDataUploaded', suspendWhenHidden=FALSE)
}


############################## Shiny App Function ##############################
shinyApp(ui = ui, server = server)

