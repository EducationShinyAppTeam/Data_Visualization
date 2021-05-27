#once included datasets, mosaic and dyplr but beleived unnecessary. 
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(shinyAce)
library(shinycssloaders)
library(rlocker)
library(ggmap)
library(boastUtils)
library(DT)

source("helpers.R")
ui <- list(
  dashboardPage(skin = "green",
    header = dashboardHeader(title = 'Data Visualization',
                         tags$li(class = "dropdown", actionLink("info", icon("info"), class = "myClass")),
                         tags$li(class = "dropdown",tags$a(href = "https://shinyapps.science.psu.edu/",
                                        icon("home")))
                         ),

sidebar = dashboardSidebar(
  width = 250,
  sidebarMenu(id = 'tabs',
              menuItem('Overview', tabName = 'overview', icon = icon("dashboard")),
              menuItem('Simple Data Visualization', tabName = 'VisualOne', 
                       icon = icon('wpexplorer')),
              menuItem('Advanced Data Visualization', tabName = 'exp4', icon = icon('wpexplorer')),
              menuItem('References', tabName = "References", icon = icon("leanpub"))
  ),
  #PSU logo
  tags$div(class = "sidebar-logo",
           boastUtils::psu_eberly_logo("reversed"))
),

body = dashboardBody(
  tags$head( 
    tags$link(rel = "stylesheet", type = "text/css", href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
  ),
  tags$style(type = "text/css", ".content-wrapper,.right-side {background-color: white;}"),
  
  useShinyjs(),
  tabItems(
    tabItem(tabName = 'overview',
            h1('Data Visualization App'),
            br(),
            h2(strong('About:')),
            p('This app illustrates R code for data visualization.'),
            br(),
            
            h2(strong('Instructions:')),
            p(tags$li("Simple data visualization section introduces
                       how to create some commonly used plots with ggplot and Rplot with exercises at the end.")),
            p(tags$li("Advanced Data Visualization section
                      introduces 3D plots, line plots, contour plots, and heat maps.")),
            br(),
            div(style = 'text-align: center', 
                bsButton(inputId = 'go2', label = 'Explore', 
                         icon = icon('bolt'), size = 'large', class='circle grow')),
            br(),
            h2(strong('Acknowledgements:')),
            p('This application was coded and developed by Anna (Yinqi) Zhang in 2018 and Yiyun Gong in 2019. 
               Special Thanks to Grace (Yubaihe) Zhou for being incredibly helpful with programming issues.
              It was updated for formatting by Ethan Wright 2020'),
            div(class = "updated", "Last Update: 9/4/2020 by EJW.")
        ),
    
    ############ Data Visualization Introduction #######
    ######Characterizing one single Variable######
    
    tabItem(tabName = 'VisualOne',
            # div(style="display: inline-block;vertical-align:top;",
            #     tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
            # ),
            # div(style="display: inline-block;vertical-align:top;",
            #     circleButton("info0",icon = icon("info"), status = "myClass",size = "xs")
            # ),
            
            tabsetPanel(type = 'tabs',
                        ###### One Variable ######
                        tabPanel('Single Variable',
                                 h1(strong('One Variable Visualization')),
                                 # br(),
                                 p('This section illustrates R code for data 
                                    visulization includes plot() and ggplot() with one Variable'),
                                 
                                 br(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     id="sidebar",
                                     tags$head(tags$style(
                                       HTML('#sidebar{
                                                background-color: #FFFFFF;
                                            }')
                                     )),
                                     
                                     checkboxInput("previewData", "Preview of Datasets"),
                                     
                                     ####select between plot and ggplot
                                     selectInput(inputId="plotType", label="Select Plot Package",
                                                 choices = c('plot', 'ggplot'),
                                                 selected = 'plot'),
                                     
                                     ####select datasets
                                     selectInput(inputId="dataset", label="Select Dataset:", 
                                                 choices= c('cars', 'trees'), 
                                                 selected = 'cars'),
                                     
                                     ####variable options for 'car' dataset
                                     conditionalPanel(
                                       condition = "input.dataset == 'cars'",
                                       selectInput(inputId="carsVariable", label="Select Variables",
                                                   choices = c("speed", "dist"),
                                                   selected = 'speed')
                                     ),
                                     
                                     ####variable option for 'trees' dataset
                                     conditionalPanel(
                                       condition = "input.dataset == 'trees'",
                                       selectInput(inputId="treesVariable", label="Select Variables",
                                                   choices = c("Girth", "Height", "Volume"),
                                                   selected = 'Girth')
                                     )
                                     #tags$img(src="DataView.pdf")
                                     #includeHTML("ViewData.nb.html")
                                     #tags$a(tags$img(src="pdficon.png"), href="DataView.pdf", download="Viewdata.pdf")
                                     # br(),
                                     #downloadLink("downloadData", "Preview of Data"),
                                     
                                   ),
                                   
                                   mainPanel(
                                     conditionalPanel(
                                       condition="input.previewData==1", 
                                      
                                       fluidRow(
                                         column(2, p(strong("Dataset"))),
                                         column(4, DT::dataTableOutput("dataTable")),
                                       
                                       )
                                     ),
                                     fluidRow(
                                       column(6,plotOutput(outputId="oneDensity", width="100%",height="300px")%>% withSpinner(color="#FFFFFF")),  
                                       column(6,plotOutput(outputId="onehist", width="100%",height="300px")%>% withSpinner(color="#FFFFFF"))
                                     ),
                                     fluidRow(
                                       column(width = 6, textOutput(outputId="DensityoneCode")),
                                       column(width = 6, textOutput(outputId="HistogramoneCode"))
                                     ),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     tags$head(tags$style("#qqCode, #BarCode, #DensityoneCode, #HistogramoneCode,
                                                          #twoscattercode, #logTransformationCode, #twobarcode, #twoboxcode
                                                          {color: #FFFFFF}"
                                     )),
                                     
                                     
                                     fluidRow(
                                       column(6,plotOutput(outputId="onebar", width="100%",height="300px")%>% withSpinner(color="#1E7B14")),  
                                       column(6,plotOutput(outputId="oneqq", width="100%",height="300px")%>% withSpinner(color="#1E7B14"))
                                     ),
                                     fluidRow(
                                       column(width = 6, textOutput(outputId="BarCode")),
                                       column(width = 6, textOutput(outputId="qqCode"))
                                     ),
                                     br(),
                                     br()
                                     # div(style = 'text-align: center', 
                                     #     bsButton(inputId = 'next2', label = 'Next Section', 
                                     #              icon = icon('angle-double-right'), size = 'small', 
                                     #              class='circle grow'))
                                   )
                                 )
                                 ),
                        
                        ###### Two Variable ######
                        tabPanel(title='Two Variables', value='panel2',
                                 h3(strong('Two Variables Visualization')),
                                 # br(),
                                 h4('This section illustrates R code for data 
                                    visulization uses ggplot() with Two Variables'),
                                 
                                 br(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     id="sidebar",
                                     tags$head(tags$style(
                                       HTML('#sidebar{
                                                background-color: #FFFFFF;
                                            }')
                                     )),
                                     ####select continuous variable 1
                                     checkboxInput("previewDataTwo", "Preview of Datasets"),
                                     selectInput(inputId="continuous1",
                                                 label="Select First Continuous Variable as X:",
                                                 choices= c('Sepal.Length', 
                                                            'Sepal.Width'),
                                                 selected = 'Sepal.Length'),
                                     
                                     selectInput(inputId="continuous2", 
                                                 label="Select Second Continuous Variable as Y:",
                                                 choices= c('Petal.Length', 
                                                            'Petal.Width'),
                                                 selected = 'Petal.Length'),
                                     
                                     selectInput(inputId="CategoryVar", 
                                                 label="Select Categorical Variable:",
                                                 choices= 'Species',
                                                 selected = 'Species'),
                                     
                                   ),
                                   
                                   mainPanel(
                                     conditionalPanel(
                                       condition="input.previewDataTwo==1",
                                       fluidRow(
                                         column(2, p(strong("Dataset iris"))),
                                         column(5, dataTableOutput("Previewiris"))
                                       )
                                       #tableOutput("Previewiris")
                                       #p("First four rows of dataset iris")
                                     ),
                                     fluidRow(
                                       column(6,plotOutput(outputId="twoscatter")%>% withSpinner(color="#FFFFFF")),
                                       column(6,plotOutput(outputId="logTransformation")%>% withSpinner(color="#FFFFFF"))
                                     ),
                                     br(),
                                     fluidRow(
                                       column(6,textOutput(outputId="twoscattercode")),
                                       column(6,textOutput(outputId="logTransformationCode"))
                                     ),
                                     br(),
                                     fluidRow(
                                       column(6,plotOutput(outputId="twobar")%>% withSpinner(color="#FFFFFF")),
                                       column(6,plotOutput(outputId="twobox")%>% withSpinner(color="#FFFFFF"))
                                     ),
                                     br(),
                                     fluidRow(
                                       column(6,textOutput(outputId="twobarcode")),
                                       column(6,textOutput(outputId="twoboxcode"))
                                     )
                                   )
                                 )
                                 ),
                        
                        tabPanel(title='Exercises', value='panel2',
                                 #uiOutput("urltest"))
                                 #includeMarkdown("test.Rmd")
                                 #system.file("knitr", package="shinyAce")
                                 
                                 fluidRow(
                                   column(6,
                                          verticalLayout(
                                            h2("Instructions"),
                                            wellPanel(
                                              style = "background-color: #FFFFFF",
                                            tags$div(tags$ul(
                                              tags$li("You can try the following questions"),
                                              tags$li("Test your code with the following R script
                                             box with the RMarkDown output under the 'Knitted Output' header"),
                                              tags$li("In each turn, 10 questions will be randomly draw from the question bank."),
                                              tags$li("Uncomment the sample code to start to explore.")
                                              ),
                                              style = "background-color: #FFFFFF")),
                                            h2("Exercises"),
                                            uiOutput('progress'),
                                            wellPanel(style = "background-color: #FFFFFF",
                                                      uiOutput("question")%>% withSpinner(color="#FFFFFF"),
                                                      uiOutput("options"),
                                                      br(),
                                                      selectInput("answer", "Select your answer from below", c("","A", "B", "C")),
                                                      uiOutput("mark"),
                                                      tags$style(type='text/css', '#question{font-size: 15px;
                                                                 background-color: #FFFFFF;color: black;}',
                                                                 '.well { padding: 10px; margin-bottom: 15px; max-width: 1000px; }')
                                                      
                                            ),
                                          fluidPage(
                                            tags$head(
                                              #tags$style(HTML('#submit{background-color:#FFFFFF; color:white}')),
                                              #tags$style(HTML('#eval{background-color:#FFFFFF; color:white}')),
                                              #tags$style(HTML('#nextq{background-color:#FFFFFF; color:white}'))
                                            ),
                                            fluidRow(
                                              column(12, align="center",
                                                     div(style="display: inline-block", actionButton(inputId = 'submit', label = 'Submit', disabled = TRUE, style="success")),
                                                     div(style="display: inline-block;vertical-align:top; width: 30px;",HTML("<br>")),
                                                     div(style="display: inline-block", bsButton(inputId = "nextq",label = "Next", disabled = TRUE)),
                                                     div(style="display: inline-block;vertical-align:top; width: 30px;",HTML("<br>")),
                                                     div(style="display: inline-block", bsButton(inputId = "reset",label = "Restart", style="danger", disabled = TRUE)))
                                            )),
                                          
                                          
                                          # column(3,
                                          #        actionButton(inputId = 'submit', label = 'Submit', style="success")
                                          # ),
                                          # column(3,
                                          #        bsButton(inputId = "nextq",label = "Next", style='warning', disabled = TRUE)
                                          # ),
                                          # column(3,
                                          #        bsButton(inputId = "reset",label = "Restart", style="danger", )
                                          # )),
                                          br(),
                                          
                                          ##########try rlocker statement#########
                                          # tags$samp(
                                          #   htmlOutput("statements")
                                          # ),
                                          ##########end#############
                                          
                                          h2("Try Your Code"),  
                                          aceEditor("rmd", mode="markdown", value='This is some markdown text. It may also have embedded R code
which will be executed. Please also read the output 
message for more hints.

you can add a new code chuck with following two lines
```{r}
```
```{r}
#structure on datasets we used in previous cases
str(cars)
str(trees)
str(iris)
```
It can even include graphical elements.
```{r}
#ggplot with one variable
#ggplot(aes(x=dist), data=cars)+geom_histogram()
```
```{r}
#ggplot with two variable
#ggplot(aes(x=Sepal.Length, y=Petal.Length), data=iris)+
#geom_line()
```
```{r}
#Rplot with one variable
plot(cars$speed)
```
'),
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
    ######Advanced
    tabItem(tabName = 'exp4',
            tabsetPanel(type = 'tabs',
                        
                        ###### Maps ######
                        tabPanel('Maps',
                                 br(),
                                 box(title = NULL, style = 'background-color: #FFFFFF', width = NULL, height = NULL,
                                     
                                     selectInput(inputId = 'mapsOp', label = 'Make a US/World Map with ggplot2',
                                                 choices = c('US Map - ggplot2', 'US Map - plotly'), selected = 'US Map'),
                                     bsPopover(id = 'mapsOp', title = " ", content = 'mUSMap takes in one dataframe that includes information about different
                                               US states and returns this data or a ggplot object constructed with the data. mWorldMap does the same but it
                                               takes in one dataframe that includes information about different countries.', trigger = 'click'),
                                     
                                     # conditionalPanel('input.mapsOp == "World Map"',
                                     #                  sliderInput(inputId = 'worldMap1', label = 'The Number of Color Scales', min = 1, max = 10,
                                     #                              value = 5, step = 1, ticks = TRUE)
                                     #                  ),
                                     
                                     conditionalPanel('input.mapsOp == "US Map - ggplot2"',
                                                      selectInput(inputId = 'usMap1', label = 'Plot Option', choices = c('borders', 'frame')),
                                                      selectInput(inputId = 'usMap2', label = 'Style Option', choices = c('compact', 'real'))
                                     )
                                     
                                 ),
                                 
                                 # box(title = NULL, style = 'background-color: #f0f4c3', width=NULL, height = NULL,
                                
                                     conditionalPanel('input.mapsOp == "US Map - ggplot2"',
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
                                                          
                                                      )),
                                     
                                     conditionalPanel('input.mapsOp == "US Map - plotly"',
                                                      div(style = "background-color: #FFFFFF",
                                                      tags$strong('R code: '),
                                                      uiOutput('plotlyUScode'),
                                                      br(),
                                                      fluidRow(
                                                        column(12, align="center",
                                                               plotlyOutput('plotlyUSMap', width='80%'))
                                                      ),
                                                      br()
                                                      
                                     ))
                                 # )
                        ),
                        
                        ###### 3D Plots ######
                        tabPanel('3D Plots',
                                 br(),
                                 fluidRow(
                                   column(width = 12,
                                          box(title = NULL, style = 'background-color: #FFFFFF', width = NULL, height = NULL,
                                              selectInput(inputId = 'Exsel', label = '3D Plot Type', 
                                                          choices = c('Normal Simulation via Plotly', '3D Basic Scatter Plot', '3D Texts Plot'),
                                                          # choices = c('Normal Simulation via Plotly', 'Basic Scatter Plot', 'Basic Scatter Plot Colored by Groups', 
                                                          #             '3D Plots with Confidence Intervals', '3D Texts Plot'),
                                                          selected = 'Normal Simulation via Plotly', multiple = FALSE),
                                              
                                              # bsPopover(id = 'Exsel', title = 'Understand the Graph Type', 
                                              #           content = 'Mesh plot generates a wireframe plot while the scatter plot generates a dotted plot.', 
                                              #           placement = 'bottom', trigger = 'hover'),
                                              
                                              #a. Normal Simulation via Plotly
                                              conditionalPanel('input.Exsel == "Normal Simulation via Plotly"',
                                                               sliderInput(inputId = 'Exsel1', label = 'Please Select Your Sample Size', min = 0, max = 100,
                                                                           value = 30, step = 1, ticks = TRUE)
                                              ),
                                              
                                              #b. Basic Scatter Plot
                                              conditionalPanel('input.Exsel == "3D Basic Scatter Plot"',
                                                               selectInput(inputId = 'basicX', label = 'Variable for X-Axis', choices = c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'),
                                                                           selected = 'Sepal.Length', multiple = FALSE),
                                                               selectInput(inputId = 'basicY', label = 'Variable for Y-Axis', choices = c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'),
                                                                           selected = 'Sepal.Width', multiple = FALSE),
                                                               selectInput(inputId = 'basicZ', label = 'Variable for Z-Axis', choices = c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'),
                                                                           selected = 'Petal.Length', multiple = FALSE)
                                              ),
                                              #c.
                                              #d. 3D Plots with Confidence Intervals
                                              conditionalPanel('input.Exsel == "3D Plots with Confidence Intervals"',
                                                               selectInput(inputId = 'CIX', label = 'Variable for X-Axis', choices = c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'),
                                                                           selected = 'Sepal.Length', multiple = FALSE),
                                                               selectInput(inputId = 'CIY', label = 'Variable for Y-Axis', choices = c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'),
                                                                           selected = 'Sepal.Width', multiple = FALSE),
                                                               selectInput(inputId = 'CIZ', label = 'Variable for Z-Axis', choices = c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'),
                                                                           selected = 'Petal.Length', multiple = FALSE)
                                              )
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(width = 12,
                                          box(title = NULL, style = 'background-color: #FFFFFF', width = NULL, height = NULL,
                                              
                                              #a. Normal Simulation via Plotly
                                              conditionalPanel('input.Exsel == "Normal Simulation via Plotly"',
                                                               tags$strong('R code: '),
                                                               uiOutput('ExCode'),
                                                               br(),
                                                               plotlyOutput('plotly1'),
                                                               br(),
                                                               verbatimTextOutput("hover"),
                                                               verbatimTextOutput("click")
                                              ),
                                              
                                              #b. Basic Scatter Plot
                                              conditionalPanel('input.Exsel == "3D Basic Scatter Plot"',
                                                               tags$strong('R code: '),
                                                               uiOutput('basicRcode'),
                                                               br(),
                                                               tableOutput('bspTable'),
                                                               plotOutput('bspOut1')
                                              ),
                                              
                                              # #c. Basic Scatter Plot Colored by Groups
                                              # conditionalPanel('input.Exsel == "Basic Scatter Plot Colored by Groups"',
                                              #                  tableOutput('bspTableCopy'),
                                              #                  plotOutput('bspOut2')
                                              # ),
                                              
                                              # #d. 3D Plots with Confidence Intervals
                                              # conditionalPanel('input.Exsel == "3D Plots with Confidence Intervals"',
                                              #                  plotOutput('CIOut')
                                              #                  ),
                                              
                                              #e. 3D Texts Plot
                                              conditionalPanel('input.Exsel == "3D Texts Plot"',
                                                               tags$strong('R code: '),
                                                               uiOutput('textRcode'),
                                                               br(),
                                                               tableOutput('textTable'),
                                                               plotOutput('textOut')
                                              )
                                              
                                          )
                                   )
                                 )
                        ),
                        
                        ###### 2D Line Plots ######
                        tabPanel('2D Line Plots',
                                 br(),
                                 fluidRow(
                                   column(width = 12,
                                          box(title = NULL, style = 'background-color: #FFFFFF', width = NULL, height = NULL,
                                              
                                              sliderInput(inputId = 'LPsel1', label = 'Please Set the Maximum of X-Axis', min = 0, max = 200,
                                                          value = 80, step = 1, ticks = TRUE),
                                              
                                              numericInput(inputId = 'LPnum1', label = 'Theoretical Mean of trace 0',
                                                           value = 10, step = 1),
                                              selectInput(inputId = 'LPSEL1', label = 'Please Select Your First Graph Mode',
                                                          choices = c('Lines', 'Markers')),
                                              numericInput(inputId = 'LPnum2', label = 'Theoretical Mean of trace 1',
                                                           value = -10, step = 1),
                                              selectInput(inputId = 'LPSEL2', label = 'Please Select Your Second Graph Mode',
                                                          choices = c('Lines', 'Markers'))
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(width = 12,
                                          box(title = NULL, style = 'background-color: #FFFFFF', width = NULL, height = NULL,
                                              tags$strong('R code: '),
                                              uiOutput('LPCode'),
                                              br(),
                                              plotlyOutput('plotly2')
                                          )
                                   )
                                 )
                        ),
                        
                        ###### Contour Plots & Heat Maps ######
                        tabPanel('Contour Plots & Heatmaps',
                                 br(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     tags$head(tags$style(
                                       HTML('#sidebarmap{
                                                background-color: #FFFFFF;
                                            }')
                                     )),
                                     #box(title = NULL, style = 'background-color: #FFFFFF', width = NULL, height = NULL,
                                         id="sidebarmap",
                                        div('Heat maps and contour plots are visualization techniques to show 
                                            data density on a map. They are particularly helpful when you have 
                                            a lot of data points on the map and are mainly interested in their 
                                            overall distribution.'),
                                         br(),
                                         selectInput(inputId = 'chSel', label = 'Please Select Your Display Option', choices = c('', 'Contour Plots', 'Heatmaps'), selected = 'Contour Plots'),
                                         
                                         #contour plots
                                         conditionalPanel('input.chSel == "Contour Plots"',
                                                          selectInput(inputId = 'chSel2', label = 'View An Example', choices = c('Volcano', 'Protein-Protein Interaction')),
                                                          conditionalPanel('input.chSel2 == "Volcano"',
                                                                           checkboxInput(inputId = 'contourLabel', label = 'Add Contour Labels', value = FALSE)
                                                          )
                                         ),
                                         
                                         #heat maps
                                         conditionalPanel('input.chSel == "Heatmaps"',
                                                          selectInput(inputId = 'heat1', label = 'View An Example', choices = c('Volcano', 'Cars')),
                                                          conditionalPanel('input.heat1 == "Volcano"',
                                                                           sliderTextInput(inputId = 'heatmapCol', label = 'Please Select Your Colorscale', 
                                                                                           choices = c('purple+green', 'yellow+red', 'pink+purple', 'white+black'), grid = TRUE)
                                                          )
                                         )
                                     #)
                                   ),
                                   
                                   mainPanel(
                                     box(title = NULL, style = 'background-color: #FFFFFF', width = NULL, height = NULL,
                                         conditionalPanel('input.chSel == "Contour Plots"',
                                                          conditionalPanel('input.chSel2 == "Volcano"',
                                                                           tags$b('In this section, we will use an embedded dataset named Volcano. It is a matrix containing 87 rows and 61 columns.'),
                                                                           br(),
                                                                           br(),
                                                                           tags$strong('R Code: '),
                                                                           uiOutput('CPCode1'), #volcano code
                                                                           br(),
                                                                           plotlyOutput('plotly3') #volcano plot
                                                          ),
                                                          conditionalPanel('input.chSel2 == "Protein-Protein Interaction"',
                                                                           plotOutput('proteinInt')
                                                          )
                                                          
                                         ),
                                         
                                         conditionalPanel('input.chSel == "Heatmaps"',
                                                          conditionalPanel('input.heat1 == "Volcano"',
                                                                           tags$strong('R Code: '),
                                                                           uiOutput('CPCode2'),
                                                                           br(),
                                                                           plotlyOutput('plotly4')
                                                          ),
                                                          conditionalPanel('input.heat1 == "Cars"',
                                                                           plotlyOutput('cars1')
                                                          )
                                         )
                                     )
                                   )
                                 )
                          )
                  )
              ),
    #######Referencees Tab ###############
    tabItem(
      tabName = "References",
      withMathJax(),
      h2("References"),
      
      p('The Protein-Protein Interaction Dataset is from the Warwick University - Molecular Organisation and Assembly in Cells.'),
      
      p(class = "hangingindent",
        "Attali, D. (2020). 
  shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds, R package. 
  Available from https://CRAN.R-project.org/package=shinyjs"),
      p(class = "hangingindent",
        "Chang, W. and Borges Ribeiro, B. (2018),
  shinydashboard: Create Dashboards with 'Shiny', R package. 
  Available from https://CRAN.R-project.org/package=shinydashboard"),
      p(class = "hangingindent",
        "Chang, W., Cheng, J., Allaire, J., Xie, Y., and MchPherson, J. (2020),
  shiny: Web Application Framework for R, R package.
  Available from https://CRAN.R-project.org/package=shiny"),
      p(class = "hangingindent",
        "Bailey, E. (2015). 
  shinyBS: Twitter Bootstrap Components for Shiny, R package.
  Available from https://CRAN.R-project.org/package=shinyBS"),
      p(class = "hangingindent",
        "Perrier, V., Meyer, F., and Granjon, D. (2020). 
  shinyWidgets: Custom Inputs Widgets for Shiny, R package. 
  Available from https://CRAN.R-project.org/package=shinyWidgets"),
      p(class = "hangingindent",
        "Sievert, C. (2020).
  plotly: Interactive Web-Based Data Visualization with R, plotly, and
  shiny. Chapman and Hall/CRC Florida, 2020."
      ),
      p(class = "hangingindent",
        "Wickham, H., ggplot2: Elegant Graphics for Data Analysis, R package.
  Springer-Verlag New York, 2016."
      ),
      p(class = "hangingindent",
        "Nijs, V., Fang, F., Trestle Technology, LLC and Allen, J. (2019). 
  shinyAce: Ace Editor Bindings for Shiny, R package.
  Available from https://CRAN.R-project.org/package=shinyAce"
      ),
      p(class = "hangingindent",
        "Sali, A. and Attali D. (2020). shinycssloaders: Add CSS Loading
  Animations to 'shiny' Outputs, R package.
  https://CRAN.R-project.org/package=shinycssloaders"
      ),
      p(class = "hangingindent",
        "Carey R. (2019). 
  rlocker: Learning Locker for Shiny, R package. 
  Available from https://github.com/rpc5102/rlocker"),
      p(class = "hangingindent",
        "Carey, R. and Hatfield, N. (2020). boastUtils: BOAST Utilities. R
  package version 0.1.4.
  https://github.com/EducationShinyAppTeam/boastUtils"
      )
      
      
    )
    
  )
)))