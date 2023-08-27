# Load Packages ----

library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(shinyAce)
library(shinycssloaders)
library(sf)
library(lwgeom)
library(plotly)
library(boastUtils)
library(DT)
library(dplyr)
library(mosaic)
library(plot3D)
library(datasets)
library(learnr)
library(knitr)
library(rmarkdown)

source("helpers.R")
source("aceText.R")
bank <- read.csv("questionbank.csv")
bank <- data.frame(
  lapply(bank, as.character), 
  stringsAsFactors = FALSE)

# Define UI for App ----
ui <- list(
  dashboardPage(
    skin = "green",
    header = dashboardHeader(
      title = 'Data Visualization',
      tags$li(
        class = "dropdown", actionLink("info", icon("info"), class = "myClass")
        ),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Data_Visualization")
      ), 
      tags$li(
        class = "dropdown",
        tags$a(href = "https://shinyapps.science.psu.edu/", icon("home"))
      )
    ),
    
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = 'pages',
        menuItem('Overview', tabName = 'overview', icon = icon("tachometer-alt")),
        menuItem('Prerequisites', tabName = 'prerequisite', icon = icon("book")),
        menuItem('Simple Visualization', tabName = 'VisualOne', 
                 icon = icon('wpexplorer')),
        menuItem('Advanced Visualization', tabName = 'exp4', 
                 icon = icon('wpexplorer')),
        menuItem('References', tabName = "References", icon = icon("leanpub"))
      ),
      tags$div(class = "sidebar-logo",
               boastUtils::sidebarFooter())
    ),
    ### Create the body ----
    dashboardBody(
      tags$head(
        tags$link(
          rel = "stylesheet", type = "text/css", 
          href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
      ),
      useShinyjs(),
      tabItems(
        #### Overview Page ----
        tabItem(
          tabName = 'overview',
          h1('Data Visualization'),
          p('This app illustrates R code for data visualization.'),
          br(),
          h2('Instructions'),
          tags$ol(
            tags$li("Familiarize yourself with data visualization and the different types 
                    of plots by reviewing the prerequisites page."),
            tags$li("Engage in practical exercises involving simple data visualization, where 
                    you will manipulate different datasets using various plot types. This section introduces 
                    the creation of commonly used plots using ggplot and Rplot, concluding with exercise questions."),
            tags$li("In the Advanced Data Visualization section, you will gain knowledge about coding features of 3D plots, 
                    line plots, contour plots, and heat maps.")
          ),
          br(),
          div(
            style = 'text-align: center',
            bsButton(
              inputId = 'go2p',
              label = 'Go to Prerquisite page',
              icon = icon('bolt'),
              size = 'large', 
              class='circle grow')
          ),
          ##### Create two lines of space
          br(),
          br(),
          h2('Acknowledgements'),
          p(
          'This application was coded and developed by Anna (Yinqi) Zhang in 2018 
           and Yiyun Gong in 2019.
           Special Thanks to Grace (Yubaihe) Zhou for being incredibly helpful 
           with programming issues.
           It was updated for formatting by Ethan Wright 2020 
          and Yijun Yao in 2022. This app was updated in 2023 by Aisiri C.Narendra'
          ),
          div(
            class = "updated", "Last Update: 7/18/2023 by ACN.")
        ),
        #### Prerequisites Page ----
        tabItem(
          tabName = "prerequisite",
          withMathJax(),
          h2("Introduction to Data Visualization"),
          p("Data visualization is the graphical representation of data to gain insights and communicate information 
            effectively. It involves creating visual representations, such as charts, graphs, and maps, to explore 
            patterns, trends, and relationships in data. Data visualization aids in understanding complex datasets, 
            identifying outliers, and presenting findings to a broader audience."),
          p("Some commonly used libraries for data visualization in R include ggplot and Rplot."),
          p("Access the", 
            a(href = 'https://rstudio.github.io/cheatsheets/html/data-visualization.html?_gl=1*hpq94h*
              _ga*MTE1NTY3OTU3MS4xNjg5Njc2MDcx*_ga_2C0WZ1JHG0*MTY4OTY3NjA3MS4xLjAuMTY4OTY3NjA3MS4wLjAuMA', 
              'Data Visualization cheatsheet', target="_blank"),
            "for more information on data visualization with ggplot2."),
       
          
          h2("Examples of plots"),
          tags$ul(
            box(
              title = strong("Density Plot"),
              status = "primary",
              collapsible = TRUE,
              collapsed = TRUE,
              width = "100%", 
              tags$li("Shows the distribution of a continuous variable by estimating its probability density function.")),
            
            box(
              title = strong("Histogram Plot"),
              status = "primary",
              collapsible = TRUE,
              collapsed = TRUE,
              width = "100%", 
              tags$li("Displays the distribution of a continuous variable by dividing the data into bins and showing the 
                        frequency or density of observations in each bin.")),
            
            box(
              title = strong("Bar Plot"),
              status = "primary",
              collapsible = TRUE,
              collapsed = TRUE,
              width = "100%", 
              tags$li("Represents categorical data using rectangular bars, where the height or length of each bar corresponds 
                        to the frequency or value of the category.")),
            
            box(
              title = strong("Normal Q-Q Plot"),
              status = "primary",
              collapsible = TRUE,
              collapsed = TRUE,
              width = "100%", 
              tags$li("Compares the quantiles of a sample to the quantiles of a specified theoretical distribution 
                        (usually the normal distribution) to assess if the sample follows a particular distribution.")),
            box(
              title = strong("Scatter Plot"),
              status = "primary",
              collapsible = TRUE,
              collapsed = TRUE,
              width = "100%", 
              tags$li("Displays the relationship between two continuous variables by plotting individual data points as dots 
                        on a two-dimensional coordinate system.")),
            box(
              title = strong("Log transformation Plot"),
              status = "primary",
              collapsible = TRUE,
              collapsed = TRUE,
              width = "100%", 
              tags$li("Applies a logarithmic transformation to one or both axes of a plot, which can help visualize 
                        data with exponential or highly skewed distributions.")),
            box(
              title = strong("Box Plot"),
              status = "primary",
              collapsible = TRUE,
              collapsed = TRUE,
              width = "100%", 
              tags$li("Summarizes the distribution of a continuous variable using a box and whisker plot, showing the median, 
                        quartiles, and potential outliers.")),
            box(
              title = strong("3D Plot"),
              status = "primary",
              collapsible = TRUE,
              collapsed = TRUE,
              width = "100%", 
              tags$li("Display data in a three-dimensional space, allowing visualization of relationships among three variables.")),
            box(
              title = strong("Line Plot"),
              status = "primary",
              collapsible = TRUE,
              collapsed = TRUE,
              width = "100%", 
              tags$li("Represent data points connected by lines, commonly used for time series or continuous data analysis.")),
            box(
              title = strong("Contour Plot"),
              status = "primary",
              collapsible = TRUE,
              collapsed = TRUE,
              width = "100%", 
              tags$li("Contour plots: Visualize 3D data on a 2D plane, using contour lines to represent levels of a continuous variable.")),
            box(
              title = strong("Heat Maps"),
              status = "primary",
              collapsible = TRUE,
              collapsed = TRUE,
              width = "100%", 
              tags$li("Use colors to represent values in a matrix or grid, providing a visual summary of patterns or intensities."))
            
          )
        ),
        #### simple visualization Page ----
        tabItem(
          tabName = 'VisualOne',
          tabsetPanel(
            type = 'tabs',
            ###### Single Variable ----
            tabPanel(
              'Single Variable',
              h2('One Variable Visualization'),
              p('This section illustrates R code for data
                 visualization includes plot() and ggplot() with one Variable'),
              br(),
              sidebarLayout(
                sidebarPanel(
                  id="sidebar",
                  checkboxInput("previewData", "Preview of Datasets"),
                  ##### select between plot and ggplot
                  selectInput(
                    inputId="plotType", label="Select Plot Method",
                    choices = c('plot', 'ggplot'),
                    selected = 'plot'
                  ),
                  ##### select datasets
                  selectInput(
                    inputId="dataset", label="Select Dataset:",
                    choices= c('cars', 'trees'),
                    selected = 'cars'
                  ),
                  ##### variable options for 'car' dataset
                  conditionalPanel(
                    condition = "input.dataset == 'cars'",
                    selectInput(
                      inputId="carsVariable", 
                      label="Select Variables",
                      choices = c("speed", "dist"),
                      selected = 'speed')
                  ),
                  ##### variable option for 'trees' dataset
                  conditionalPanel(
                    condition = "input.dataset == 'trees'",
                    selectInput(
                      inputId="treesVariable", label="Select Variables",
                      choices = c("Girth", "Height", "Volume"),
                      selected = 'Girth')
                  ),
                  conditionalPanel(
                    condition="input.previewData==1",
                    fluidRow(
                      column(
                        width = 1,
                        p(strong("Dataset"))
                      )
                    ),
                    fluidRow(
                      column(
                        width = 1,
                        DT::dataTableOutput("dataTable")
                      )
                    )
                  ),
                  
                ),
                mainPanel(
                  fluidRow(
                    column(6,
                           plotOutput(
                             outputId="oneDensity", 
                             width="150%",
                             height="350px")%>% 
                             withSpinner(color="#FFFFFF")),
                  ),
                  tags$strong('R Code: '),
                  fluidRow(
                    column(width = 10, uiOutput(outputId="DensityoneCode")),
                  ),
                  
                  br(),
                  br(),
                  
                  fluidRow(
                    column(6,
                           plotOutput(
                             outputId="onehist", 
                             width="150%",
                             height="350px")%>% 
                             withSpinner(color="#FFFFFF")),
                  ),
                  tags$strong('R Code: '),
                  fluidRow(
                    column(width = 10, uiOutput(outputId="HistogramoneCode")),
                  ),
               
                
                  
                    
                  
                  br(),
                  br(),
                  fluidRow(
                    column(6,
                           plotOutput(
                             outputId="onebar", 
                             width="180%",
                             height="350px")%>% 
                             withSpinner(color="#FFFFFF")),
                  ),
                  tags$strong('R Code: '),
                  fluidRow(
                    column(width = 10, uiOutput(outputId="BarCode")),
                  ),
                  
                  
                  br(),
                  br(),
                  fluidRow(
                    column(6,
                           plotOutput(
                             outputId="oneqq", 
                             width="180%",
                             height="350px")%>% 
                             withSpinner(color="#1E7B14")
                    ),
                    column(
                      width = 8,
                      tags$strong('R code: '),
                      uiOutput('qqCode'),
                      
                    
                    )
                  ),
                    
                  
                  
                )
              )
            ),
            ##### Two Variables ----
            tabPanel(title='Two Variables', value='panel2',
                     h2('Two Variables Visualization'),
                     p('This section illustrates R code for data
                        visualization uses ggplot() with Two Variables'),
                     br(),
                     sidebarLayout(
                       sidebarPanel(
                         id="sidebar",
                         ###### select continuous variable 1
                         checkboxInput("previewDataTwo", "Preview of Datasets"),
                         selectInput(
                           inputId="continuous1",
                           label="Select First Continuous Variable as X:",
                           choices= c('Sepal.Length',
                                      'Sepal.Width'),
                           selected = 'Sepal.Length'),
                         selectInput(
                           inputId="continuous2",
                           label="Select Second Continuous Variable as Y:",
                           choices= c('Petal.Length',
                                      'Petal.Width'),
                           selected = 'Petal.Length'),
                         selectInput(inputId="CategoryVar",
                                     label="Select Categorical Variable:",
                                     choices= 'Species',
                                     selected = 'Species')
                       ),
                       mainPanel(
                         conditionalPanel(
                           condition="input.previewDataTwo==1",
                           fluidRow(
                             column(1, p(strong("Dataset iris"))),
                             column(6, dataTableOutput("Previewiris"))
                           )
                         ),
                         fluidRow(
                           column(6,
                                  plotOutput(
                                    outputId="twoscatter", 
                                    width="200%",
                                    height="350px")%>% 
                                    withSpinner(color="#FFFFFF")),
                         ),
                         tags$strong('R Code: '),
                         fluidRow(
                           column(width = 10, uiOutput(outputId="twoscattercode")),
                         ),
                         br(),
                         br(),
                         fluidRow(
                           column(
                             6,plotOutput(
                               outputId="logTransformation", 
                               width="200%",
                               height="350px")%>% 
                               withSpinner(color="#FFFFFF"))
                         ),
                         tags$strong('R Code: '),
                         fluidRow(
                           column(width = 10, 
                                  uiOutput(outputId="logTransformationCode"))
                         ),
                         br(),
                         br(),
                         fluidRow(
                           column(6,
                                  plotOutput(
                                    outputId="twobar", 
                                    width="200%",
                                    height="350px")%>% 
                                    withSpinner(color="#FFFFFF")),
                         ),
                         tags$strong('R Code: '),
                         fluidRow(
                           column(width = 10, uiOutput(outputId="twobarcode")),
                         ),
                         br(),
                         br(),
                         fluidRow(
                           column(6,
                                  plotOutput(
                                    outputId="twobox",
                                    width="200%",
                                    height="350px")%>% 
                                    withSpinner(color="#FFFFFF"))
                         ),
                         tags$strong('R Code: '),
                         fluidRow(
                           column(width = 10, uiOutput(outputId="twoboxcode"))
                         )
                       )
                     )
            ),
            ##### exercises page ----
            tabPanel(
              title='Exercises', 
              value='panel2',
              fluidRow(
                column(6,
                       verticalLayout(
                         h2("Instructions"),
                         wellPanel(
                           style = "background-color: #FFFFFF",
                           tags$div(
                             tags$ul(
                               tags$li("You can try the following questions"),
                               tags$li(
                                 "Test your code with the following R script
                                  box with the RMarkDown output under 
                                 the 'Knitted Output' header"
                               ),
                               tags$li(
                                 "Try questions randomly drawn from the question bank."
                               ),
                               tags$li(
                                 "Uncomment the sample code to start to explore what the code produces."
                               )
                             ),
                             style = "background-color: #FFFFFF")),
                         h2("Exercises"),
                         uiOutput('progress'),
                         wellPanel(
                           style = "background-color: #FFFFFF",
                           uiOutput("question")%>% 
                             withSpinner(color="#FFFFFF"),
                           uiOutput("options"),
                           br(),
                           selectInput(
                             "answer", 
                             "Select your answer", 
                             c("","A", "B", "C")
                           ),
                           uiOutput("mark"),
                           tags$style(type='text/css', 
                                      '#question{font-size: 15px;
                                      background-color: #FFFFFF;color: black;}',
                                      '.well { padding: 10px; margin-bottom: 15px; 
                                      max-width: 1000px; }')
                         ),
                         fluidPage(
                           fluidRow(
                             column(12, align="center",
                                    div(
                                      style="display: inline-block",
                                      actionButton(
                                        inputId = 'submit', 
                                        label = 'Submit', disabled = TRUE, 
                                        style="success")
                                    ),
                                    div(
                                      style="display: 
                                      inline-block;
                                      vertical-align:top; width: 30px;",
                                      HTML("<br>")
                                    ),
                                    div(
                                      style="display: inline-block", 
                                      bsButton(inputId = "nextq",
                                               label = "Next", disabled = TRUE)
                                    ),
                                    div(
                                      style="display: 
                                      inline-block;vertical-align:top; 
                                      width: 30px;",
                                      HTML("<br>")
                                    ),
                                    div(
                                      style="display: inline-block", 
                                      bsButton(
                                        inputId = "reset",
                                        label = "Restart", 
                                        style="danger", disabled = TRUE))
                             )
                           )
                         ),
                         
                         h2("Try Your Code"),
                         aceEditor(
                           outputId = "rmd",
                           mode = "markdown",
                           value = initialAceText,
                           theme = "xcode",
                           wordWrap = TRUE
                         ),
                         
                                column(6,
                                       withBusyIndicatorUI(
                                         actionButton("eval", "Run")))
                              )),
                       br(),
                       column(6,
                              h2("Knitted Output"),
                              htmlOutput("knitDoc")
                       )
                     )
            )
          )
        ),
####Advanced ----
tabItem(
  tabName = 'exp4',
  tabsetPanel(
    type = 'tabs',
    ##### Maps ----
    tabPanel(
      'Maps',
      br(),
      box(
        title = NULL, 
        style = 'background-color: #FFFFFF', 
        width = NULL, 
        height = NULL,
        ###### plotly option
        selectInput(
          inputId = 'mapsOp', 
          label = 'Make a US/World Map with ggplot2',
          choices = c('US Map - ggplot2', 'US Map - plotly'), 
          selected = 'US Map'
        ),
        bsPopover(
          id = 'mapsOp', 
          title = " ", 
          content = 'mUSMap takes in one dataframe that includes 
                    information about different US states and 
                    returns this data or a ggplot object constructed with the data. 
                    mWorldMap does the same but it takes in one dataframe that 
                    includes information about different countries.', 
                    trigger = 'click'
        ),
        ###### plot option
        conditionalPanel(
          'input.mapsOp == "US Map - ggplot2"',
          selectInput(
            inputId = 'usMap1', 
            label = 'Plot Option', 
            choices = c('borders', 'frame')
          ),
          selectInput(
            inputId = 'usMap2', 
            label = 'Style Option', 
            choices = c('compact', 'real'))
        )
      ),
      conditionalPanel(
        'input.mapsOp == "US Map - ggplot2"',
        div(style = "background-color: #FFFFFF",
            tags$strong('R code: '),
            uiOutput('usMapOut2'),
            br(),
            fluidRow(
              column(12, align="center",
                     plotOutput('usMapOut1', width="80%")
              )
            ),
            br()
        )
      ),
      conditionalPanel(
        'input.mapsOp == "US Map - plotly"',
        div(style = "background-color: #FFFFFF",
            tags$strong('R code: '),
            uiOutput('plotlyUScode'),
            br(),
            fluidRow(
              column(12, align="center",
                     plotlyOutput('plotlyUSMap', width='80%'))
            ),
            br()
        )
      )
    ),
    ##### 3D Plots ----
    tabPanel(
      '3D Plots',
      br(),
      fluidRow(
        column(
          width = 12,
          box(
            title = NULL, 
            style = 'background-color: #FFFFFF', 
            width = NULL, 
            height = NULL,
            selectInput(
              inputId = 'Exsel', 
              label = '3D Plot Type',
              choices = c('Normal Simulation via Plotly', 
                          '3D Basic Scatter Plot', 
                          '3D Texts Plot'),
              selected = 'Normal Simulation via Plotly',
              multiple = FALSE),
            #b. normal simulation via plotly
            conditionalPanel(
              'input.Exsel == "Normal Simulation via Plotly"',
              sliderInput(
                inputId = 'Exsel1', 
                label = 'Please Select Your Sample Size', 
                min = 0, 
                max = 100,
                value = 30, 
                step = 1, 
                ticks = TRUE
              )
            ),
            #b. Basic Scatter Plot
            conditionalPanel(
              'input.Exsel == "3D Basic Scatter Plot"',
              selectInput(
                inputId = 'basicX', 
                label = 'Variable for X-Axis',
                choices = c('Sepal.Length', 'Sepal.Width', 
                            'Petal.Length', 'Petal.Width'),
                selected = 'Sepal.Length', multiple = FALSE
              ),
              selectInput(
                inputId = 'basicY', 
                label = 'Variable for Y-Axis', 
                choices = c('Sepal.Length', 'Sepal.Width', 
                            'Petal.Length', 'Petal.Width'),
                selected = 'Sepal.Width', multiple = FALSE
              ),
              selectInput(
                inputId = 'basicZ', 
                label = 'Variable for Z-Axis', 
                choices = c('Sepal.Length', 'Sepal.Width', 
                            'Petal.Length', 'Petal.Width'),
                selected = 'Petal.Length', multiple = FALSE
              )
            ),
            #c. 3D Plots with Confidence Intervals
            conditionalPanel(
              'input.Exsel == "3D Plots with Confidence Intervals"',
              selectInput(
                inputId = 'CIX',
                label = 'Variable for X-Axis', 
                choices = c('Sepal.Length', 'Sepal.Width', 
                            'Petal.Length', 'Petal.Width'),
                selected = 'Sepal.Length', multiple = FALSE
              ),
              selectInput(
                inputId = 'CIY', 
                label = 'Variable for Y-Axis', 
                choices = c('Sepal.Length', 'Sepal.Width', 
                            'Petal.Length', 'Petal.Width'),
                selected = 'Sepal.Width', multiple = FALSE
              ),
              selectInput(
                inputId = 'CIZ', 
                label = 'Variable for Z-Axis', 
                choices = c('Sepal.Length', 'Sepal.Width', 
                            'Petal.Length', 'Petal.Width'),
                selected = 'Petal.Length', multiple = FALSE
              )
            )
          )
        )
      ),
      fluidRow(
        column(width = 12,
               box(title = NULL, 
                   style = 'background-color: #FFFFFF', 
                   width = NULL, 
                   height = NULL,
                   #a. Normal Simulation via Plotly
                   conditionalPanel(
                     'input.Exsel == "Normal Simulation via Plotly"',
                     tags$strong('R code: '),
                     uiOutput('ExCode'),
                     br(),
                     plotlyOutput('plotly1'),
                     br(),
                     verbatimTextOutput("hover"),
                     verbatimTextOutput("click")
                   ),
                   #b. Basic Scatter Plot
                   conditionalPanel(
                     'input.Exsel == "3D Basic Scatter Plot"',
                     tags$strong('R code: '),
                     uiOutput('basicRcode'),
                     br(),
                     tableOutput('bspTable'),
                     plotOutput('bspOut1')
                   ),                
                   #e. 3D Texts Plot
                   conditionalPanel(
                     'input.Exsel == "3D Texts Plot"',
                     tags$strong('R code: '),
                     uiOutput('textRcode'),
                     br(),
                     tableOutput('textTable'),
                     plotOutput('textOut')
                   ) # conditional panel
               )#box
        )# column
      )#fluidrow
    ),#tabpanel
    ##### 2D Line Plots ----
    tabPanel(
      '2D Line Plots',
      br(),
      fluidRow(
        column(width = 12,
               box(title = NULL, 
                   style = 'background-color: #FFFFFF', 
                   width = NULL, 
                   height = NULL,
                   sliderInput(
                     inputId = 'LPsel1', 
                     label = 'Please Set the Maximum of X-Axis', 
                     min = 0, 
                     max = 200,
                     value = 80, 
                     step = 1, 
                     ticks = TRUE
                   ),
                   numericInput(
                     inputId = 'LPnum1',
                     label = 'Theoretical Mean of trace 0',
                     value = 10, 
                     step = 1
                   ),
                   selectInput(
                     inputId = 'LPSEL1', 
                     label = 'Please Select Your First Graph Mode',
                     choices = c('Lines', 'Markers')),
                   numericInput(
                     inputId = 'LPnum2', 
                     label = 'Theoretical Mean of trace 1',
                     value = -10, 
                     step = 1
                   ),
                   selectInput(
                     inputId = 'LPSEL2', 
                     label = 'Please Select Your Second Graph Mode',
                     choices = c('Lines', 'Markers'))
               )
        )
      ),
      fluidRow(
        column(width = 12,
               box(title = NULL, 
                   style = 'background-color: #FFFFFF',
                   width = NULL, height = NULL,
                   tags$strong('R code: '),
                   uiOutput('LPCode'),
                   br(),
                   plotlyOutput('plotly2')
               )
        )
      )
    ),
    ##### Contour Plots & Heat Maps ----
    tabPanel(
      'Contour Plots & Heatmaps',
      br(),
      sidebarLayout(
        sidebarPanel(
          
          id="sidebarmap",
          div('Heat maps and contour plots are visualization techniques to show
                   data density on a map. They are particularly helpful when you have
                   a lot of data points on the map and are mainly interested in their
                   overall distribution.'),
          br(),
          selectInput(
            inputId = 'chSel', 
            label = 'Please Select Your Display Option', 
            choices = c('', 'Contour Plots', 'Heatmaps'), 
            selected = 'Contour Plots'),
          #contour plots
          conditionalPanel(
            'input.chSel == "Contour Plots"',
            selectInput(
              inputId = 'chSel2', 
              label = 'View An Example', 
              choices = c('Volcano', 'Protein-Protein Interaction')
            ),
            conditionalPanel(
              'input.chSel2 == "Volcano"',
              checkboxInput(
                inputId = 'contourLabel', 
                label = 'Add Contour Labels', 
                value = FALSE)
            )
          ),
          #heat maps
          conditionalPanel(
            'input.chSel == "Heatmaps"',
            selectInput(
              inputId = 'heat1', 
              label = 'View An Example', 
              choices = c('Volcano', 'Cars'
              )
            ),
            conditionalPanel(
              'input.heat1 == "Volcano"',
              sliderTextInput(
                inputId = 'heatmapCol', 
                label = 'Please Select Your Colorscale',
                choices = c('purple+green', 'yellow+red', 'pink+purple', 
                            'white+black'), 
                grid = TRUE
              )
            )
          )
        ),
        mainPanel(
          box(title = NULL, 
              style = 'background-color: #FFFFFF', 
              width = NULL, 
              height = NULL,
              conditionalPanel(
                'input.chSel == "Contour Plots"',
                conditionalPanel(
                  'input.chSel2 == "Volcano"',
                  tags$b(
                  'In this section, we will use an embedded dataset named Volcano. 
                  It is a matrix containing 87 rows and 61 columns.'),
                  br(),
                  br(),
                  tags$strong('R Code: '),
                  uiOutput('CPCode1'), #volcano code
                  br(),
                  plotlyOutput('plotly3') #volcano plot
                ),
                conditionalPanel(
                  'input.chSel2 == "Protein-Protein Interaction"',
                  plotOutput('proteinInt')
                )
              ),
              conditionalPanel(
                'input.chSel == "Heatmaps"',
                conditionalPanel(
                  'input.heat1 == "Volcano"',
                  tags$strong('R Code: '),
                  uiOutput('CPCode2'),
                  br(),
                  plotlyOutput('plotly4')
                ),
                conditionalPanel('input.heat1 == "Cars"',
                                 plotlyOutput('cars1')
                )
              )
          ) # box
        ) #mainpanel
      ) # sidebarlayout
    )# tabpanel
  )
),

#### Set up the References Page ----
tabItem(
  tabName = "References",
  withMathJax(),
  h2("References"),
  p(class = "hangingindent",
    "Attali, D. (2020). 
  shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds, R package. 
  Available from https://CRAN.R-project.org/package=shinyjs"),
  p(class = "hangingindent",
    "Bailey, E. (2015). 
  shinyBS: Twitter Bootstrap Components for Shiny, R package.
  Available from https://CRAN.R-project.org/package=shinyBS"),
  p(class = "hangingindent",
    "Carey R. (2019). 
  rlocker: Learning Locker for Shiny, R package. 
  Available from https://github.com/rpc5102/rlocker"),
  p(class = "hangingindent",
    "Carey, R. and Hatfield, N. (2020). boastUtils: BOAST Utilities. R
  package version 0.1.4.
  https://github.com/EducationShinyAppTeam/boastUtils"),
  p(class = "hangingindent",
    "Chang, W. and Borges Ribeiro, B. (2018),
  shinydashboard: Create Dashboards with 'Shiny', R package. 
  Available from https://CRAN.R-project.org/package=shinydashboard"),
  p(class = "hangingindent",
    "Chang, W., Cheng, J., Allaire, J., Xie, Y., and MchPherson, J. (2020),
  shiny: Web Application Framework for R, R package.
  Available from https://CRAN.R-project.org/package=shiny"),
  p(class = "hangingindent",
    "Lu,H.,Lu,L.,and Skolnick, J.(2003). Protein-Protein interaction dataset.[Text file].
  Available from https://pubmed.ncbi.nlm.nih.gov/12609891/"),
  p(class = "hangingindent",
    "Molecular Organisation and Assembly in Cells(2007). Contour Plots of Matrix Data.
  University of Warwick: Warwick, UK. 
  Available from 
  https://warwick.ac.uk/fac/sci/moac/people/students/peter_cock/r/matrix_contour/#references"),
  p(class = "hangingindent",
    "Nijs, V., Fang, F., Trestle Technology, LLC and Allen, J. (2019). 
  shinyAce: Ace Editor Bindings for Shiny, R package.
  Available from https://CRAN.R-project.org/package=shinyAce"),
  p(class = "hangingindent",
    "Perrier, V., Meyer, F., and Granjon, D. (2020). 
  shinyWidgets: Custom Inputs Widgets for Shiny, R package. 
  Available from https://CRAN.R-project.org/package=shinyWidgets"),
  p(class = "hangingindent",
    "Sali, A. and Attali D. (2020). shinycssloaders: Add CSS Loading
  Animations to 'shiny' Outputs, R package.
  https://CRAN.R-project.org/package=shinycssloaders"),
  p(class = "hangingindent",
    "Sievert, C. (2020).
  plotly: Interactive Web-Based Data Visualization with R, plotly, and
  shiny. Chapman and Hall/CRC Florida, 2020."),
  p(class = "hangingindent",
    "Wickham, H., ggplot2: Elegant Graphics for Data Analysis, R package.
  Springer-Verlag New York, 2016."
  ), 
  br(), 
  br(), 
  br(), 
  boastUtils::copyrightInfo() 
)
      )
    )
  )
)

# Define server logic ----

server <- function(input, output, session) {
  output$dataTable <- DT::renderDataTable({
    if(input$dataset == "cars")
      datatable(cars[1:10, ], options = list(pageLength = 10))
    else
      datatable(trees[1:10, ], options = list(pageLength = 10))
  })
  output$Previewiris <-
    DT::renderDataTable({
      datatable(iris[1:10, ], options = list(pageLength = 10))
    }) 
  ###### KNITR
  observeEvent(input$eval, {
    withBusyIndicatorServer("eval", {
      output$knitDoc <- renderUI({
        isolate(
          HTML(
          knit2html(
            text = input$rmd,
            template = FALSE,
            quiet = TRUE
          )
        ))
        
      })
      
      output$output <- renderPrint({
        return(isolate(eval(parse(text = input$code))))
      })
    })
  })
  
  
  
  output$output <- renderPrint({
    input$eval
    return(isolate(eval(parse(text = input$code))))
  })
  
  observeEvent(input$info0, {
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Move the sliders or select from the dropdown menus and view the R 
      code that produces the results.",
      type = NULL
    )
  })
  observeEvent(input$info, {
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Select the variables or the dataset to be used. Select answers 
      for the exercise.",
      type = NULL
    )
  })
  observeEvent(input$go2p, {
    updateTabItems(session, 'pages', 'prerequisite')
  })

  observeEvent(input$next2, {
    updateTabsetPanel(session, 'VisualOne', selected = 'panel2')
  })
  ### simple visualization Page ----
  #### single variable ----
  output$oneDensity <-
    renderCachedPlot({
      if (input$dataset == 'cars') {
        if (input$carsVariable == 'speed') {
          if (input$plotType == 'plot') {
            plot(density(cars$speed),
                 main = "Density Plot",
                 xlab = input$carsVariable)
          }
          else if (input$plotType == 'ggplot') {
            ggplot(aes(speed), data = cars) +
              geom_density(color = "darkblue",
                           fill = "lightblue",
                           alpha = 0.4) +
              ggtitle('Density Plot')
          }
        }
        else if (input$carsVariable == 'dist') {
          if (input$plotType == 'plot') {
            plot(density(cars$dist),
                 main = "Density Plot",
                 xlab = input$carsVariable)
          }
          else if (input$plotType == 'ggplot') {
            ggplot(aes(dist), data = cars) +
              geom_density(color = "darkblue",
                           fill = "lightblue",
                           alpha = 0.4) +
              ggtitle('Density Plot')
          }
        }
      }
      else if (input$dataset == 'trees') {
        if (input$treesVariable == 'Girth') {
          if (input$plotType == 'plot') {
            plot(density(trees$Girth),
                 main = "Density Plot",
                 xlab = input$carsVariable)
          }
          else if (input$plotType == 'ggplot') {
            ggplot(aes(Girth), data = trees) +
              geom_density(color = "darkblue",
                           fill = "lightblue",
                           alpha = 0.4) +
              ggtitle('Density Plot')
          }
        }
        else if (input$treesVariable == 'Height') {
          if (input$plotType == 'plot') {
            plot(density(trees$Height),
                 main = "Density Plot",
                 xlab = input$carsVariable)
          }
          else if (input$plotType == 'ggplot') {
            ggplot(aes(Height), data = trees) +
              geom_density(color = "darkblue",
                           fill = "lightblue",
                           alpha = 0.4) +
              ggtitle('Density Plot')
          }

        }
        else if (input$treesVariable == 'Volume') {
          if (input$plotType == 'plot') {
            plot(density(trees$Volume),
                 main = "Density Plot",
                 xlab = input$carsVariable)
          }
          else if (input$plotType == 'ggplot') {
            ggplot(aes(Volume), data = trees) +
              geom_density(color = "darkblue",
                           fill = "lightblue",
                           alpha = 0.4) +
              ggtitle('Density Plot')
          }

        }
      }
    },
    cacheKeyExpr = {
      list(input$dataset,
           input$carsVariable,
           input$plotType,
           input$treesVariable)
    }
    )

  output$onehist <- renderCachedPlot({
    if (input$dataset == 'cars') {
      if (input$carsVariable == 'speed') {
        if (input$plotType == 'plot') {
          hist(cars$speed,
               main = "Histogram",
               xlab = input$carsVariable)
        }
        else if (input$plotType == 'ggplot') {
          ggplot(aes(speed), data = cars) +
            geom_histogram(color = "darkblue",
                           fill = "lightblue",
                           alpha = 0.4) +
            ggtitle("Histogram")
        }
      }
      else if (input$carsVariable == 'dist') {
        if (input$plotType == 'plot') {
          hist(cars$dist,
               main = "Histogram",
               xlab = input$carsVariable)
        }
        else if (input$plotType == 'ggplot') {
          ggplot(aes(dist), data = cars) +
            geom_histogram(color = "darkblue",
                           fill = "lightblue",
                           alpha = 0.4) +
            ggtitle("Histogram")
        }

      }
    }
    else if (input$dataset == 'trees') {
      if (input$treesVariable == 'Girth') {
        if (input$plotType == 'plot') {
          hist(trees$Girth,
               main = "Histogram",
               xlab = input$carsVariable)
        }
        else if (input$plotType == 'ggplot') {
          ggplot(aes(Girth), data = trees) +
            geom_histogram(color = "darkblue",
                           fill = "lightblue",
                           alpha = 0.4) +
            ggtitle("Histogram")
        }
      }
      else if (input$treesVariable == 'Height') {
        if (input$plotType == 'plot') {
          hist(trees$Height,
               main = "Histogram",
               xlab = input$carsVariable)
        }
        else if (input$plotType == 'ggplot') {
          ggplot(aes(Height), data = trees) +
            geom_histogram(color = "darkblue",
                           fill = "lightblue",
                           alpha = 0.4) +
            ggtitle("Histogram")
        }
      }
      else if (input$treesVariable == 'Volume') {
        if (input$plotType == 'plot') {
          hist(trees$Volume,
               main = "Histogram",
               xlab = input$carsVariable)
        }
        else if (input$plotType == 'ggplot') {
          ggplot(aes(Volume), data = trees) +
            geom_histogram(color = "darkblue",
                           fill = "lightblue",
                           alpha = 0.4) +
            ggtitle("Histogram")
        }

      }
    }
  },
  cacheKeyExpr = {
    list(input$dataset,
         input$plotType,
         input$carsVariable,
         input$treesVariable)
  })

  output$onebar <-
    renderCachedPlot({
      if (input$dataset == 'cars') {
        if (input$carsVariable == 'speed') {
          if (input$plotType == 'plot') {
            barplot(cars$speed,
                    main = "Bar Plot",
                    xlab = input$carsVariable)
          }
          else if (input$plotType == 'ggplot') {
            ggplot(aes(speed), data = cars) +
              geom_freqpoly(bins = 30) +
              geom_area(
                stat = "bin",
                bins = 30,
                color = "darkblue",
                fill = "lightblue",
                alpha = 0.4
              ) +
              ggtitle('Frequency polygon')
          }
        }
        else if (input$carsVariable == 'dist') {
          if (input$plotType == 'plot') {
            barplot(cars$dist,
                    main = "Bar Plot",
                    xlab = input$carsVariable)
          }
          else if (input$plotType == 'ggplot') {
            ggplot(aes(dist), data = cars) +
              geom_freqpoly(bins = 30) +
              geom_area(
                stat = "bin",
                bins = 30,
                color = "darkblue",
                fill = "lightblue",
                alpha = 0.4
              ) +
              ggtitle('Frequency polygon')
          }

        }
      }
      else if (input$dataset == 'trees') {
        if (input$treesVariable == 'Girth') {
          if (input$plotType == 'plot') {
            barplot(trees$Girth,
                    main = "Bar Plot",
                    xlab = input$carsVariable)
          }
          else if (input$plotType == 'ggplot') {
            ggplot(aes(Girth), data = trees) +
              geom_freqpoly(bins = 30) +
              geom_area(
                stat = "bin",
                bins = 30,
                color = "darkblue",
                fill = "lightblue",
                alpha = 0.4
              ) +
              ggtitle('Frequency polygon')
          }
        }
        else if (input$treesVariable == 'Height') {
          if (input$plotType == 'plot') {
            barplot(trees$Height,
                    main = "Bar Plot",
                    xlab = input$carsVariable)
          }
          else if (input$plotType == 'ggplot') {
            ggplot(aes(Height), data = trees) +
              geom_freqpoly(bins = 30) +
              geom_area(
                stat = "bin",
                bins = 30,
                color = "darkblue",
                fill = "lightblue",
                alpha = 0.4
              ) +
              ggtitle('Frequency polygon')
          }

        }
        else if (input$treesVariable == 'Volume') {
          if (input$plotType == 'plot') {
            barplot(trees$Volume,
                    main = "Bar Plot",
                    xlab = input$carsVariable)
          }
          else if (input$plotType == 'ggplot') {
            ggplot(aes(Volume), data = trees) +
              geom_freqpoly(bins = 30) +
              geom_area(
                stat = "bin",
                bins = 30,
                color = "darkblue",
                fill = "lightblue",
                alpha = 0.4
              ) +
              ggtitle('Frequency polygon')
          }
        }
      }
    }, cacheKeyExpr = {
      list(input$dataset,
           input$plotType,
           input$carsVariable,
           input$treesVariable)
    })

  output$oneqq <-
    renderCachedPlot({
      if (input$dataset == 'cars') {
        if (input$carsVariable == 'speed') {
          if (input$plotType == 'plot') {
            qqnorm(cars$speed)
            qqline(cars$speed, col = 'red')
          }
          else if (input$plotType == 'ggplot') {
            ggplot(aes(sample = speed), data = cars) +
              stat_qq(color = "darkblue",
                      fill = "lightblue",
                      alpha = 0.4) +
              stat_qq_line(color = 'red')
          }
        }
        else if (input$carsVariable == 'dist') {
          if (input$plotType == 'plot') {
            qqnorm(cars$dist)
            qqline(cars$dist, col = 'red')
          }
          else if (input$plotType == 'ggplot') {
            ggplot(aes(sample = dist), data = cars) + 
              stat_qq(color = "darkblue",
                      fill = "lightblue",
                      alpha = 0.4) +
              stat_qq_line(color = 'red')
          }
        }
      }
      else if (input$dataset == 'trees') {
        if (input$treesVariable == 'Girth') {
          if (input$plotType == 'plot') {
            qqnorm(trees$Girth)
            qqline(trees$Girth, col = 'red')
          }
          else if (input$plotType == 'ggplot') {
            ggplot(aes(sample = Girth), data = trees) +
              stat_qq(color = "darkblue",
                      fill = "lightblue",
                      alpha = 0.4) +
              stat_qq_line(color = 'red')
          }
        }
        else if (input$treesVariable == 'Height') {
          if (input$plotType == 'plot') {
            qqnorm(trees$Height)
            qqline(trees$Height, col = 'red')
          }
          else if (input$plotType == 'ggplot') {
            ggplot(aes(sample = Height), data = trees) +
              stat_qq(color = "darkblue",
                      fill = "lightblue",
                      alpha = 0.4) +
              stat_qq_line(color = 'red')
          }
        }
        else if (input$treesVariable == 'Volume') {
          if (input$plotType == 'plot') {
            qqnorm(trees$Volume)
            qqline(trees$Volume, col = 'red')
          }
          else if (input$plotType == 'ggplot') {
            ggplot(aes(sample = Volume), data = trees) +
              stat_qq(color = "darkblue",
                      fill = "lightblue",
                      alpha = 0.4) +
              stat_qq_line(color = 'red')
          }
        }
      }
    }, cacheKeyExpr = {
      list(input$dataset,
           input$plotType,
           input$carsVariable,
           input$treesVariable)
    })

  output$DensityoneCode <- renderText({
    if (input$dataset == 'cars') {
      if (input$plotType == 'plot') {
        code <- paste0(
          'plot(density(',
          input$carsVariable,
          ', data = ',
          input$dataset,
          '))'
        )
      } else if (input$plotType == 'ggplot') {
        code <- paste0(
          "ggplot(aes(",
          input$carsVariable,
          "), data = cars) + ",
          "geom_density(color = 'darkblue', fill = 'lightblue', alpha = 0.4) + ",
          "ggtitle('Density Plot')"
        )
      }
    } else {
      if (input$plotType == 'plot') {
        code <- paste0(
          'plot(density(',
          input$dataset,
          '$',
          input$treesVariable,
          '))'
        )
      } else if (input$plotType == 'ggplot') {
        code <- paste0(
          "ggplot(aes(",
          input$treesVariable,
          "), data = trees) + ",
          "geom_density(color = 'darkblue', fill = 'lightblue', alpha = 0.4) + ",
          "ggtitle('Density Plot')"
        )
      }
    }
    
    code
  })
  
  

  output$HistogramoneCode <- renderText({
    if (input$dataset == 'cars') {
      if (input$plotType == 'plot') {
        code <- paste0(
          'hist(',
          input$carsVariable,
          ', data = ',
          input$dataset,
          ')'
        )
      } else {
        code <- paste0(
          "ggplot(aes(",
          input$carsVariable,
          "), data = cars) + ",
          "geom_histogram(color = 'darkblue', fill = 'lightblue', alpha = 0.4) + ",
          "ggtitle('Histogram')"
        )
      }
    } else {
      if (input$plotType == 'plot') {
        code <- paste0(
          'hist(',
          input$treesVariable,
          ', data = ',
          input$dataset,
          ')'
        )
      } else {
        code <- paste0(
          "ggplot(aes(",
          input$treesVariable,
          "), data = trees) + ",
          "geom_histogram(color = 'darkblue', fill = 'lightblue', alpha = 0.4) + ",
          "ggtitle('Histogram')"
        )
      }
    }
    
    code
  })
  
  output$BarCode <- renderText({
    if (input$dataset == 'cars') {
      if (input$plotType == 'plot') {
        code <- paste0(
          'barplot(',
          input$carsVariable,
          ', data = ',
          input$dataset,
          ')'
        )
      } else {
        code <- paste0(
          "ggplot(aes(",
          input$carsVariable,
          "), data = cars) + ",
          "geom_freqpoly(bins = 30) + ",
          "geom_area(stat = 'bin', bins = 30, color = 'darkblue', fill = 'lightblue', alpha = 0.4) + ",
          "ggtitle('Frequency polygon')"
        )
      }
    } else {
      if (input$plotType == 'plot') {
        code <- paste0(
          'barplot(',
          input$treesVariable,
          ', data = ',
          input$dataset,
          ')'
        )
      } else {
        code <- paste0(
          "ggplot(aes(",
          input$treesVariable,
          "), data = trees) + ",
          "geom_freqpoly(bins = 30) + ",
          "geom_area(stat = 'bin', bins = 30, color = 'darkblue', fill = 'lightblue', alpha = 0.4) + ",
          "ggtitle('Frequency polygon')"
        )
      }
    }
    
    code
  })
  
  output$qqCode <- renderText({
    if (input$dataset == 'cars') {
      if (input$plotType == 'plot') {
        code <- paste0(
          'qqnorm(',
          input$carsVariable,
          ', data = ',
          input$dataset,
          ')\n',
          'qqline(',
          input$carsVariable,
          ', data = ',
          input$dataset,
          ')'
        )
      } else {
        code <- paste0(
          "ggplot(aes(sample = ",
          input$carsVariable,
          "), data = cars) + ",
          "stat_qq(color = 'darkblue', fill = 'lightblue', alpha = 0.4) + ",
          "stat_qq_line(color = 'red')"
        )
      }
    } else {
      if (input$plotType == 'plot') {
        code <- paste0(
          'qqnorm(',
          input$treesVariable,
          ', data = ',
          input$dataset,
          ')\n',
          'qqline(',
          input$treesVariable,
          ', data = ',
          input$dataset,
          ')'
        )
      } else {
        code <- paste0(
          "ggplot(aes(sample = ",
          input$treesVariable,
          "), data = trees) + ",
          "stat_qq(color = 'darkblue', fill = 'lightblue', alpha = 0.4) + ",
          "stat_qq_line(color = 'red')"
        )
      }
    }
    
    code
  })
  
  #### Two Variables ----
  output$twoscatter <- renderCachedPlot({
    if (input$continuous1 == 'Sepal.Length') {
      if (input$continuous2 == 'Petal.Length') {
        ggplot(aes(Sepal.Length, Petal.Length), data = iris) +
          geom_point(aes(colour = factor(Species))) +
          geom_smooth(aes(colour = factor(Species)),
                      linetype = 'twodash',
                      size = 0.8) +
          ggtitle("Scatter Plot")
      }
      else if (input$continuous2 == 'Petal.Width') {
        ggplot(aes(Sepal.Length, Petal.Width), data = iris) +
          geom_point(aes(colour = factor(Species))) +
          geom_smooth(aes(colour = factor(Species)),
                      linetype = 'twodash',
                      size = 0.8) +
          ggtitle("Scatter Plot")
      }
    }
    else if (input$continuous1 == 'Sepal.Width') {
      if (input$continuous2 == 'Petal.Length') {
        ggplot(aes(Sepal.Width, Petal.Length), data = iris) +
          geom_point(aes(colour = factor(Species))) +
          geom_smooth(aes(colour = factor(Species)),
                      linetype = 'twodash',
                      size = 0.8) +
          ggtitle("Scatter Plot")
      }
      else if (input$continuous2 == 'Petal.Width') {
        ggplot(aes(Sepal.Width, Petal.Width), data = iris) +
          geom_point(aes(colour = factor(Species))) +
          geom_smooth(aes(colour = factor(Species)),
                      linetype = 'twodash',
                      size = 0.8) +
          ggtitle("Scatter Plot")
      }
    }
  }, cacheKeyExpr = {
    list(input$continuous1, input$continuous2)
  })


  output$logTransformation <- renderCachedPlot({
    if (input$continuous1 == 'Sepal.Length') {
      if (input$continuous2 == 'Petal.Length') {
        ggplot(aes(Sepal.Length, Petal.Length), data = iris) +
          geom_point(aes(colour = factor(Species))) +
          coord_trans(x = "log2", y = "log2") +
          ggtitle("Log Transformation")
      }
      else if (input$continuous2 == 'Petal.Width') {
        ggplot(aes(Sepal.Length, Petal.Width), data = iris) +
          geom_point(aes(colour = factor(Species))) +
          coord_trans(x = "log2", y = "log2") +
          ggtitle("Log Transformation")
        #sunflowerplot(Sepal.Length~Petal.Width, data=iris, main="SunflowerPlot")
      }
    }
    else if (input$continuous1 == 'Sepal.Width') {
      if (input$continuous2 == 'Petal.Length') {
        ggplot(aes(Sepal.Width, Petal.Length), data = iris) +
          geom_point(aes(colour = factor(Species))) +
          coord_trans(x = "log2", y = "log2") +
          ggtitle("Log Transformation")
      }
      else if (input$continuous2 == 'Petal.Width') {
        ggplot(aes(Sepal.Width, Petal.Width), data = iris) +
          geom_point(aes(colour = factor(Species))) +
          coord_trans(x = "log2", y = "log2") +
          ggtitle("Log Transformation")
        #sunflowerplot(Sepal.Width~Petal.Width, data=iris, main="SunflowerPlot")
      }
    }
  }, cacheKeyExpr = {
    list(input$continuous1, input$continuous2)
  })

  output$twobar <- renderCachedPlot({
    if (input$continuous1 == 'Sepal.Length') {
      if (input$continuous2 == 'Petal.Length') {
        ggplot(data = iris,
               aes(
                 x = Sepal.Length,
                 y = Petal.Length,
                 fill = factor(Species)
               )) +
          geom_bar(stat = "identity") +
          ggtitle('Bar Plot')
      }
      else if (input$continuous2 == 'Petal.Width') {
        ggplot(data = iris,
               aes(
                 x = Sepal.Length,
                 y = Petal.Width,
                 fill = factor(Species)
               )) +
          geom_bar(stat = "identity") +
          ggtitle('Bar Plot')
      }
    }
    else if (input$continuous1 == 'Sepal.Width') {
      if (input$continuous2 == 'Petal.Length') {
        ggplot(data = iris,
               aes(
                 x = Sepal.Width,
                 y = Petal.Length,
                 fill = factor(Species)
               )) +
          geom_bar(stat = "identity") +
          ggtitle('Bar Plot')
      }
      else if (input$continuous2 == 'Petal.Width') {
        ggplot(data = iris,
               aes(
                 x = Sepal.Width,
                 y = Petal.Width,
                 fill = factor(Species)
               )) +
          geom_bar(stat = "identity") +
          ggtitle('Bar Plot')
      }
    }
  }, cacheKeyExpr = {
    list(input$continuous1, input$continuous2)
  })


  output$twobox <- renderCachedPlot({
    if (input$continuous1 == 'Sepal.Length') {
      if (input$continuous2 == 'Petal.Length') {
        ggplot(data = iris,
               aes(
                 x = Sepal.Length,
                 y = Petal.Length,
                 color = Species
               )) +
          geom_boxplot() +
          ggtitle('Boxplot')
      }
      else if (input$continuous2 == 'Petal.Width') {
        ggplot(data = iris,
               aes(
                 x = Sepal.Length,
                 y = Petal.Width,
                 color = Species
               )) +
          geom_boxplot() +
          ggtitle('Boxplot')
      }
    }
    else if (input$continuous1 == 'Sepal.Width') {
      if (input$continuous2 == 'Petal.Length') {
        ggplot(data = iris,
               aes(
                 x = Sepal.Width,
                 y = Petal.Length,
                 color = Species
               )) +
          geom_boxplot() +
          ggtitle('Boxplot')
      }
      else if (input$continuous2 == 'Petal.Width') {
        ggplot(data = iris,
               aes(
                 x = Sepal.Width,
                 y = Petal.Width,
                 color = Species
               )) +
          geom_boxplot() +
          ggtitle('Boxplot')
      }
    }
  }, cacheKeyExpr = {
    list(input$continuous1, input$continuous2)
  })

  output$twoscattercode <- renderUI({
    tags$code(
      "ggplot(aes(",
      input$continuous1,
      ',',
      input$continuous2,
      "), data=iris)+
      geom_point(aes(colour = factor(Species)))+
      geom_smooth(aes(colour = factor(Species)), linetype='twodash', size=0.8)+
      ggtitle('Scatter Plot')",
      seq = ''
    )
  })

  output$logTransformationCode <- renderUI({
    tags$code(
      "ggplot(aes(",
      input$continuous1,
      ',',
      input$continuous2,
      "), data=iris)+
      geom_point(aes(colour = factor(Species)))+
      coord_trans(x='log2', y='log2')+
      ggtitle('Log Transformation')",
      seq = ''
    )
  })

  output$twobarcode <- renderUI({
    tags$code(
      "ggplot(data=iris, aes(",
      input$continuous1,
      ',',
      input$continuous2,
      "fill=factor(Species)))+
                    geom_bar(stat='identity')+
                    ggtitle('Bar Plot')",
      seq = ''
    )
  })

  output$twoboxcode <- renderUI({
    tags$code(
      "ggplot(data=iris, aes(",
      input$continuous1,
      ',',
      input$continuous2,
      "color=Species)) +
      geom_boxplot()+
      ggtitle('Boxplot')",
      seq = ''
    )
  })


  #### Exercises Part ----

  observeEvent(input$nextq, {
    updateSelectInput(session,
                      "answer",
                      "pick an answer from below",
                      c("", "A", "B", "C"))
    
    output$mark <- renderUI({
      img(src = NULL, width = 30)
    })
    disable("submit")
  })


  ###### question bank 
  value <- reactiveValues(index =  1,
                          mistake = 0,
                          correct = 0)
  ans <- as.matrix(bank[1:14, 6])
  index_list <- reactiveValues(list = sample(2:14, 10, replace = FALSE))

  observeEvent(input$nextq, {
    value$answerbox <- value$index
    index_list$list = index_list$list[-1]
    value$index <- index_list$list[1]
    value$answerbox <- value$index

    updateButton(session, "nextq", disabled = TRUE)
    updateButton(session, "submit", disabled = FALSE)
  })

  output$question <- renderUI({
    h4(bank[value$index, 2])
  })

  output$options <- renderUI({
    str1 <- paste("A.", bank[value$index, 3])
    str2 <- paste("B.", bank[value$index, 4])
    str3 <- paste("C.", bank[value$index, 5])
    HTML(paste(str1, str2, str3, sep = '<br/>'))
  })


  observeEvent(input$answer, {
     req(input$answer, input$answer != '')
     answer <- isolate(input$answer)
     enable("submit") 
   })


  getResponseText <- function(index, answer) {
    if (answer == 'A') {
      key = 3
    } else if (answer == 'B') {
      key = 4
    } else {
      key = 5
    }
    return(bank[index, key])
  }

  observeEvent(input$submit, {
    if (length(index_list$list) == 1) {
      updateButton(session, "nextq", disabled = TRUE)
      updateButton(session, "submit", disabled = TRUE)
      updateButton(session, "reset", disabled = FALSE)
    }
    else{
      updateButton(session, "nextq", disabled = FALSE)
      updateButton(session, "submit", disabled = TRUE)
      updateButton(session, "reset", disabled = FALSE)
    }
    answer <- isolate(input$answer)

    output$mark <- boastUtils::renderIcon(
        icon = ifelse(any(answer == ans[value$index, 1]),
                      "correct",
                      "incorrect"),
        width = 50)
  })

  observeEvent(input$reset, {
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session, "reset", disable = TRUE)
    updateSelectInput(session,
                      "answer",
                      "pick an answer from below",
                      c("", "A", "B", "C"))
    index_list$list <-
      c(index_list$list, sample(2:14, 10, replace = FALSE))
    value$index <- 1
    value$answerbox = value$index
    ans <- as.matrix(bank[1:14, 6])
    output$mark <- renderUI({
      img(src = NULL, width = 30)
    })
    
  })
  ### advanced visualization ----
  #### Maps ----
  #a. usMap
  output$usMapOut1 <- renderCachedPlot({
    USArrests2 <- USArrests %>% st_as_sf()
    if (input$usMap1 == 'borders' & input$usMap2 == 'compact') {
      mUSMap(
        USArrests2,
        key = "state",
        fill = "UrbanPop",
        plot = 'borders',
        style = 'compact'
      )
    }
    else if (input$usMap1 == 'borders' & input$usMap2 == 'real') {
      mUSMap(
        USArrests2,
        key = "state",
        fill = "UrbanPop",
        plot = 'borders',
        style = 'real'
      )
    }
    else if (input$usMap1 == 'frame' & input$usMap2 == 'compact') {
      mUSMap(
        USArrests2,
        key = "state",
        fill = "UrbanPop",
        plot = 'frame',
        style = 'compact'
      )
    }
    else {
      mUSMap(
        USArrests2,
        key = "state",
        fill = "UrbanPop",
        plot = 'frame',
        style = 'real'
      )
    }
  }, cacheKeyExpr = {
    list(input$usMap1, input$usMap2)
  })
  
  output$usMapOut2 <- renderUI ({
    tags$code(
      'mUSMap(USArrests2, key = "state", fill = "UrbanPop", plot = "',
      input$usMap1,
      '", style = "',
      input$usMap2,
      '")'
    )
  })

  #plotly US Map - code
  output$plotlyUScode <- renderUI ({
    tags$code('p <- plot_geo(df, locationmode = "USA-states", sizes = c(1, 250))')
  })

  #plotly US Map
  output$plotlyUSMap <- renderPlotly({
    df <-
      read.csv(
        'https://raw.githubusercontent.com/plotly/datasets/master/2014_us_cities.csv'
      )
    df$q <- with(df, cut(pop, quantile(pop)))
    levels(df$q) <-
      paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
    df$q <- as.ordered(df$q)

    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showland = TRUE,
      landcolor = toRGB("gray85"),
      subunitwidth = 1,
      countrywidth = 1,
      subunitcolor = toRGB("white"),
      countrycolor = toRGB("white")
    )

    p <-
      plot_geo(df, locationmode = 'USA-states', sizes = c(1, 250)) %>%
      add_markers(
        x = ~ lon,
        y = ~ lat,
        size = ~ pop,
        color = ~ q,
        hoverinfo = "text",
        text = ~ paste(df$name, "<br />", df$pop / 1e6, " million")
      ) %>%
      layout(
        title = '2014 US city populations<br>(Click legend to toggle)', geo = g
        )
    g
    p
  })


  #### 3D Plots ----
  #a. Normal Simulation via Plotly
  output$plotly1 <- renderPlotly ({
    plot_ly(
      x = rnorm(input$Exsel1),
      y = rnorm(input$Exsel1),
      z = rnorm(input$Exsel1),
      type = 'scatter3d',
      mode = 'markers'
    )
  })

  output$ExCode <- renderUI ({
    tags$code(
      'plot_ly(x = rnorm(',
      input$Exsel1,
      '), y = rnorm(',
      input$Exsel1,
      '), z = rnorm(',
      input$Exsel1,
      '), type = "scatter3d", mode = "markers")'
    )
  })


  output$hover <- renderPrint({
    dataHover <- event_data("plotly_hover")
    if (is.null(dataHover)) {
      "Hover events appear here (unhover to clear)"
    }
    else {
      dataHover
    }
  })

  output$click <- renderPrint({
    dataClick <- event_data("plotly_click")
    if (is.null(dataClick)) {
      "Click events appear here (double-click to clear)"
    }
    else {
      dataClick
    }
  })

  #b. Basic Scatter Plot
  output$basicRcode <- renderUI ({
    tags$code(
      'scatter3D(x, y, z, clab = c("Sepal", "Width (cm)"), 
      xlab = input$basicX, ylab = input$basicY, zlab = input$basicZ)'
    )
  })

  output$bspTable <- renderTable ({
    head(iris)
  })

  output$bspOut1 <- renderPlot({
    x <- iris[, input$basicX]
    y <- iris[, input$basicY]
    z <- iris[, input$basicZ]
    scatter3D(
      x,
      y,
      z,
      clab = c("Sepal", "Width (cm)"),
      xlab = input$basicX,
      ylab = input$basicY,
      zlab = input$basicZ
    )
  })

  #e. 3D Texts Plot
  output$textRcode <- renderUI ({
    tags$code(
      'with(USArrests, text3D(Murder, Assault, Rape,
              labels = rownames(USArrests), colvar = UrbanPop,
              col = gg.col(100), theta = 60, phi = 20,
              xlab = "Murder", ylab = "Assault", zlab = "Rape",
              main = "USA arrests", cex = 0.6,
              bty = "g", ticktype = "detailed", d = 2,
              clab = c("Urban","Pop"), adj = 0.5, font = 2))'
    )
  })

  output$textTable <- renderTable ({
    head(USArrests)
  })

  output$textOut <- renderPlot ({
    data(USArrests)
    with(
      USArrests,
      text3D(
        Murder,
        Assault,
        Rape,
        labels = rownames(USArrests),
        colvar = UrbanPop,
        col = gg.col(100),
        theta = 60,
        phi = 20,
        xlab = "Murder",
        ylab = "Assault",
        zlab = "Rape",
        main = "USA arrests",
        cex = 0.6,
        bty = "g",
        ticktype = "detailed",
        d = 2,
        clab = c("Urban", "Pop"),
        adj = 0.5,
        font = 2
      )
    )
  })

  #### 2D Line Plots ----
  output$plotly2 <- renderPlotly ({
    trace_0 <-
      rnorm(as.numeric(input$LPsel1), mean = as.numeric(input$LPnum1))
    trace_1 <-
      rnorm(as.numeric(input$LPsel1), mean = as.numeric(input$LPnum2))
    x = c(1:as.numeric(input$LPsel1))
    data <- data.frame(x, trace_0, trace_1)
    if (input$LPSEL1 == 'Lines' & input$LPSEL2 == 'Lines') {
      plot_ly(
        data,
        x = ~ x,
        y = ~ trace_0,
        name = 'trace 0',
        type = 'scatter',
        mode = 'lines'
      ) %>%
        add_trace(y = ~ trace_1,
                  name = 'trace 1',
                  mode = 'lines')
    }
    else if (input$LPSEL1 == 'Markers' &
             input$LPSEL2 == 'Markers') {
      plot_ly(
        data,
        x = ~ x,
        y = ~ trace_0,
        name = 'trace 0',
        type = 'scatter',
        mode = 'markers'
      ) %>%
        add_trace(y = ~ trace_1,
                  name = 'trace 1',
                  mode = 'markers')
    }
    else if (input$LPSEL1 == 'Lines' & input$LPSEL2 == 'Markers') {
      plot_ly(
        data,
        x = ~ x,
        y = ~ trace_0,
        name = 'trace 0',
        type = 'scatter',
        mode = 'lines'
      ) %>%
        add_trace(y = ~ trace_1,
                  name = 'trace 1',
                  mode = 'markers')
    }
    else {
      plot_ly(
        data,
        x = ~ x,
        y = ~ trace_0,
        name = 'trace 0',
        type = 'scatter',
        mode = 'markers'
      ) %>%
        add_trace(y = ~ trace_1,
                  name = 'trace 1',
                  mode = 'lines')
    }
  })

  output$LPCode <- renderUI ({
    tags$code(
      'plot_ly(data, x = ~x, y = ~trace_0, name = "trace 0", 
               type = "scatter", mode = "lines") %>%
       add_trace(y = ~trace_1, name = "trace 1", mode = "markers + lines")'
    )
  })

  #### Contour Plots and Heatmaps ----
  #contour plot
  output$proteinInt <- renderPlot ({
    potentials <-
      as.matrix(read.table(
        "MULTIPOT_lu.txt",
        row.names = 1,
        header = TRUE
      ))
    matrix.axes <- function(data) {
      # Do the rows, las=2 for text perpendicular to the axis
      x <- (1:dim(data)[1] - 1) / (dim(data)[1] - 1)

      axis(
        side = 1,
        at = x,
        labels = rownames(data),
        las = 2
      )

      # Do the columns
      x <- (1:dim(data)[2] - 1) / (dim(data)[2] - 1)

      axis(
        side = 2,
        at = x,
        labels = colnames(data),
        las = 2
      )

    }
    filled.contour(potentials,
                   plot.axes = matrix.axes(potentials),
                   main = "Protein-Protein Interaction Potential")
  })

  output$plotly3 <- renderPlotly({
    if (input$contourLabel == FALSE) {
      plot_ly(z = volcano,
              type = "contour",
              colors = colorRamp(c("purple", "green")))
    }
    else {
      plot_ly(
        z = volcano,
        type = "contour",
        colors = colorRamp(c("purple", "green")),
        contours = list(showlabels = TRUE)
      )
    }
  })

  #contour plot r code
  output$CPCode1 <- renderUI ({
    if (input$contourLabel == FALSE) {
      tags$code(
        'plot_ly(z = volcano, type = "contour", 
        colors = colorRamp(c("purple", "green")))'
      )
    }
    else {
      tags$code(
        'plot_ly(z = volcano, type = "contour", 
        colors = colorRamp(c("purple", "green")), 
        contours = list(showlabels = TRUE))'
      )
    }
  })

  #heatmap
  output$plotly4 <- renderPlotly({
    if (input$heatmapCol == 'purple+green') {
      plot_ly(z = volcano,
              type = "heatmap",
              colors = colorRamp(c("purple", "green")))
    }
    else if (input$heatmapCol == 'yellow+red') {
      plot_ly(z = volcano,
              type = "heatmap",
              colors = colorRamp(c("yellow", "red")))
    }
    else if (input$heatmapCol == 'pink+purple') {
      plot_ly(z = volcano,
              type = "heatmap",
              colors = colorRamp(c("pink", "purple")))
    }
    else {
      plot_ly(z = volcano,
              type = "heatmap",
              colors = colorRamp(c("white", "black")))
    }
  })

  #heatmaps r code
  output$CPCode2 <- renderUI ({
    if (input$heatmapCol == 'purple+green') {
      tags$code(
        'plot_ly(z = volcano, type = "heatmap", 
        colors = colorRamp(c("purple", "green")))'
      )
    }
    else if (input$heatmapCol == 'yellow+red') {
      tags$code('plot_ly(z = volcano, type = "heatmap", 
                colors = colorRamp(c("yellow", "red")))')
    }
    else if (input$heatmapCol == 'pink+purple') {
      tags$code('plot_ly(z = volcano, type = "heatmap",
                colors = colorRamp(c("pink", "purple")))')
    }
    else {
      tags$code('plot_ly(z = volcano, type = "heatmap", 
                colors = colorRamp(c("white", "black")))')
    }
  })

  output$cars1 <- renderPlotly ({
    head(mtcars)
    data = as.matrix(mtcars)
    data = apply(data, 2, function(x) {
      x / mean(x)
    })
    plot_ly(
      x = colnames(data),
      y = rownames(data),
      z = data,
      type = "heatmap"
    )
  })

}
# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
