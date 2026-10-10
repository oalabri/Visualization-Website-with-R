
library(dplyr)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(glue)
library(tidyverse)

## The UI

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "Analytic Dashboard"
  ),
  dashboardSidebar(
    sidebarMenu(
      
      menuItem(
        "Home",
        tabName = "Home",
        icon = icon("house")
      ),
      
      menuItem(
        "Upload and see data info",
        tabName = "upload",
        icon = icon("upload")
      ),
      
      menuItem(
        "Visualizing",
        tabName = "Visualizing",
        icon = icon("table")
      ),

      menuItem(
        "Manual Plottig",
        tabName = "manual_plotting",
        icon = icon("play")
      ),

      menuItem(
        "Best Predictor",
        tabName = "Best_Predictor",
        icon = icon("play")
      ),

      menuItem(
        "Explore variances",
        tabName = "Explore_variences",
        icon = icon("play")
      ),
      
      menuItem(
        "Contact",
        tabName = "Contact",
        icon = icon("phone")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),

    tabItems(
      tabItem(
        tabName = "Home", #The home tab.
        fluidRow(
          box(
            width = 12,
            title = "Welcome",
            h2("Data analysis helper Website with visualization"),
            p("Upload your data and explore it as you want"),
            
            
            h3("What you can do here: "),
            
            tags$ul(
              tags$li("upload your csv data"),
              tags$li("Look how your data look"),
              tags$li("Create models and compare between them"),
              tags$li("Compare two columns variance"),
              tags$li("Plot each columns variance")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "upload",
        fluidRow(
          
          box(
            width = 4,
            title = "Upload data",
            status = "primary",
            solidHeader = TRUE,
            
            fileInput(
              "file",
              "Choose your csv file",
              accept = ".csv"
            )
          ),
          
          box(
            width = 8,
            title = "Dataset info",
            status = "info",
            solidHeader = TRUE,
            verbatimTextOutput("dataset_info")
          )
        ),
        
        fluidRow(
          
          box(
            width = 12,
            title = "Dataset Preview",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("table")
          )
        )
      ),

      tabItem(
        tabName = "Visualizing",
        fluidRow(
          box(
            width = 8,
            div(
              style = "display: flex; flex-direction: column; align-items: center; font-size: 25px;",
              p("Would you like to allow generating choatic plots? (Unlocks all plotting choices)"),
              checkboxInput("allow_choas", "Yes")
            ),
          ),
          box(
            width = 8,
            title = "Plotting choices",
            status = "primary",
            solidHeader = TRUE,

            selectInput(
              "x_var",
              "X variable:",
              choices = NULL
            ),
            selectInput(
              "y_var",
              "Y variable:",
              choices = NULL
            ),
            selectInput(
              "plot_type",
              "Select Plot type",
              choices = NULL
            ),
            hr(),
            actionButton(
              "go_plot",
              "Generate Plot",
              icon = icon("play")
            )
          ),

          box(
            width = 12,
            title = "Visualization",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("v_plot", height = "500px")
          )
        )
      ),

      tabItem(
        tabName = "manual_plotting",
        fluidRow(
          div(
            style = "font-size: 30px;",
            p("Still working on this")

          )
        )

      ),

      tabItem(
        tabName = "Best_Predictor",
        fluidRow(
          p("You must choose columns to perform regression",
          style= "text-align: center; font-size: 25px;"),
          p("STILL DOESNT SUPPORT Y BEING MORE THAN 2 CATAGORIES",
          style= "text-align: center; font-size: 15px;"),
          box(
            width = 4,
            title = "Parameters selection",
            status = "success",
            solidHeader = TRUE,
            selectInput(
              "y_predict",
              "Select the target Y",
              choices = NULL,
            ),
            hr(),
            selectInput(
              "x_predict",
              "Select the predictors",
              choices = NULL,
              multiple = TRUE
            )
          ),
          sliderInput(
            "threshold",
            "Enter the desired threshold: ",
            max = 0.1,
            min = 0,
            value = 0.05
          ),
          p("0.05 is the initail threshold"),
          hr(),
          actionButton(
            "run_model",
            "Generate regression summaries",
            icon = icon("play")
          ),
          hr(),
          box(
            width = 12,
            title = "Regression summaries",
            status = "primary",
            solidHeader = TRUE,
            verbatimTextOutput("model_results")
          )
        )

      ),

      tabItem(
        tabName = "Explore_variences",
        fluidRow(
          div(
            style= "text-align: center; font-size: 25px;",
            p("You can compare two columns variances and plot each column's variance")

          ),
          box(
            width = 12,
            title = "Choose columns to compare",
            status = "primary",
            solidHeader = TRUE,
            selectInput(
              "x_variance_compare",
              "X variable:",
              choices = NULL
            ),
            selectInput(
              "y_variance_compare",
              "Y variable:",
              choices = NULL
            ),
            hr(),
            actionButton(
              "go_variance_compare",
              "Explore variences",
              icon = icon("play")
            )
          ),

          box(
            width = 12,
            title = "Comparison information",
            status = "primary",
            solidHeader = TRUE,
            tableOutput("info_table"),
            verbatimTextOutput("info_compare"),


            plotOutput("variance_plot"),

            
            actionButton(
              "show_stats",
              "Understand the Statistics",
              icon = icon("circle-question")
            ),

            
            conditionalPanel(
              condition = "input.show_stats > 0",

              wellPanel(
                h3("Understanding Your Results"),

                h4("Levene's Test p-value"),
                p("Checks whether the groups have different variances.
                  A p-value below 0.05 suggests that their variances differ."),

                h4("ANOVA p-value"),
                p("Checks whether the group averages are different.
                  It does not directly compare variances."),

                h4("Correlation Coefficient (r)"),
                p("Measures how strongly two numerical variables are
                  related. Values near -1 or +1 indicate a strong
                  linear relationship; values near 0 indicate a weak
                  linear relationship."),

                h4("F-statistic"),
                p("For a direct comparison of two variances, it is
                  the ratio of the sample variances. A value near 1
                  means the sample variances are similar."),

                h4("Comparison p-value"),
                p("A value below 0.05 usually indicates a statistically
                  significant difference between the variances."),

                h4("Degrees of Freedom"),
                p("Describe how much independent information is used
                  in the statistical test. They help determine the
                  p-value and confidence interval."),

                h4("95% Confidence Interval for the Ratio of Variances"),
                p("Gives a range of plausible values for the ratio
                  of the population variances. If the interval excludes
                  1, it suggests that the variances differ significantly
                  at the 5% level."),

                h4("Chi-square Statistic"),
                p("Measures how different the observed category counts are
                  from the counts we would expect if the two categorical
                  variables were unrelated. A larger value generally means
                  a bigger difference between observed and expected counts."),

                h4("Chi-square p-value"),
                p("Tells us whether there is evidence of a relationship
                  between the two categorical variables. A p-value below
                  0.05 suggests a statistically significant association.
                  A p-value of 0.05 or higher means there is not enough
                  evidence to conclude that an association exists."),

                h4("Cramer's V"),
                p("Measures the strength of the relationship between
                  two categorical variables. It ranges from 0 to 1.
                  A value near 0 means a weak or no association, while
                  a value near 1 means a strong association. The meaning
                  of intermediate values depends on the data and table size."),

                h4("Entropy X"),
                p("Measures how spread out the categories of variable X are.
                  An entropy near 0 means most or all observations belong
                  to one category. Higher entropy means the observations
                  are more evenly distributed across categories."),

                h4("Entropy Y"),
                p("Measures how spread out the categories of variable Y are.
                  An entropy near 0 means most or all observations belong
                  to one category. Higher entropy means the observations
                  are more evenly distributed across categories.")
              )
            ),

          ),

          
          


          box(
            width = 12,
            title = "Choose columns to plot",
            status = "primary",
            solidHeader = TRUE,
            selectInput(
              "variance_plot_var",
              "Choose what columns to plot variance",
              choices = NULL,
              multiple = TRUE
            ),
            selectInput(
              "variance_plot_type",
              "What type of plots do you want for numerical columns?",
              choices = c("Boxplot" = "Boxplot", "Violen" = "Violen")
            ),
            p("Catagorical columns are always plotted in columns"),
            hr(),
            actionButton(
              "go_variance_plot",
              "Plot variances",
              icon = icon("play")
            )
          ),
          box(
            width = 12,
            title = "Columns plots: ",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("plot_variance_out")

          )
          )
        ),

        tabItem(
        tabName = "Contact",
        fluidRow(
          box(
            width = 8,
            title = "Contact us",
            status = "success",
            solidHeader = FALSE,
            p("If you notice any bugs or have any sugestions contact us:"),
            tags$ul(
              tags$li("Email: oalabri09@gmail.com"),
              tags$li("Phone: ---"),
              tags$li("Discord: os.abri")
            ),
            p("My GitHub: https://github.com/oalabri")
          )
        )
      )
      )


    )
  )