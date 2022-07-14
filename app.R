library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)
library(tidyverse)


SS <- read_csv("000001.SS.csv")
DIA <- read_csv("DIA.csv")
ISFL <- read_csv("ISF.L.csv")
QQQ <- read_csv("QQQ.csv")
SPY <- read_csv("SPY.csv")

companyname <- list("A","AAL","AAP","AAPL","ABBV","ABC","ABMD")


ui <- dashboardPage(
  dashboardHeader(title = "Stock Market"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "page1", icon = icon("home")),
      menuItem("Global Market", tabName = "page2", icon = icon("globe")),
      menuItem("Domestic Market", tabName = "page3", icon = icon("dollar-sign")),
      menuItem("Company Performance", tabName = "page4", icon = icon("chart-line")),
      menuItem("Best Companies", tabName = "page5", icon = icon("building")),
      menuItem("Data", tabName = "page6", icon = icon("database"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("page1",
              tags$img(src="stock.jpeg", height=200, width=1200, 
                       style="display: block; margin-left: auto; margin-right: auto;"),
              tags$br(),
              fluidRow(
                box(
                  title = "Introduction", solidHeader = TRUE,
                  status = "success", width = 12, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           tags$span(
                             p("All datasets contain historical data on stock prices/indices at monthly intervals from December 31, 2009 to December 31, 2021."), 
                             p("something to be added")
                           )
                         )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "About the Dataset", solidHeader = TRUE,
                  status = "primary", width = 12, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           tags$span("From Yahoo Finance"),
                           br(),
                           tags$li(tags$strong("Source: "),tags$a(href = "https://finance.yahoo.com/quote/DIA/history?p=DIA", "Yahoo Finance Historical Data")),
                           tags$li("to be added"),
                           br(),
                           tags$span(
                             "The datasets include the following columns"),
                           br(),
                           fluidRow(column(6, tags$li("Date"), tags$li("Open"), tags$li("High"), tags$li("Low")), 
                                    column(6, tags$li("Close"), tags$li("Adj Close"), tags$li("Volume")))
                         )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Teams", solidHeader = TRUE, 
                  status = "info", width = 12, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           fluidRow(
                             column(3, tags$img(src="carey_logo.png", height=101, width=246, align ="center")),
                             column(9, tags$div("The team members are MSIS student at Johns Hopkins Carey Business School"),
                                    tags$li(tags$strong("abc: "), "abc123@jh.edu"),
                                    tags$li(tags$strong("abc: "), "abc123@jh.edu"),
                                    tags$li(tags$strong("abc: "), "abc123@jh.edu"),
                                    tags$li(tags$strong("abc: "), "abc123@jh.edu"))
                             )
                           )
                         )
                  )
                )
              ), 
      tabItem(tabName = "page2",
              fluidRow(
                box(
                  title = "Compare global stock market indices", solidHeader = TRUE,
                  status = "primary", width = 12, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           tags$span(
                             p("Move the slider to see a comparison of the performance of the three major indices for the year")
                           )
                         )
                  )
                )
              ),
              sliderInput("year", "Year:", min = 2010, max = 2020, value = 2010, 
                          step = 1, animate = animationOptions(interval = 2000, loop = FALSE)),
              #plotOutput("plot1") to be added
      ),
      tabItem(tabName = "page3",
              h2("Country Ranking"),
              fluidRow(
                box(
                  title = "Guide", solidHeader = TRUE,
                  status = "primary", width = 12, collapsible = TRUE, collapsed = FALSE,
                  h4("Panel Intro")
                ),
                box(
                  title = "Findings", solidHeader = TRUE,
                  status = "info", width = 12, collapsible = TRUE, collapsed = FALSE,
                  column(12, 
                         tags$li(" Information"),
                         tags$li("Information")        
                  ),
                  column(3,numericInput("num", "Number of Stocks to show:",
                                        value = 5, min = 1, max = 20, step = 1)),
                  column(4,sliderInput("Year", "Year:", min = 2010, max = 2021, 
                                       value = 2021, step = 1, 
                                       animate = animationOptions(interval = 3000, loop = TRUE))),br(),
                  column(5,box(title = "Company News/Intro",status = "success",
                               solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = TRUE,
                               HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/NzOLOdsTlLQ" 
                   frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))),
                  column(6,plotlyOutput("plot1", width  = 800)),
                  column(6,plotlyOutput("plot2", width = 800))),br(),
                fluidRow( 
                  box(
                    title = "There will be charts for stock prices",solidHeader = TRUE,
                    status = "primary",width = 12,collapsible = TRUE,collapsed = FALSE,
                    dataTableOutput("d1")
                    )
                  )
                ),
      ),
      tabItem(tabName = "page4",
              h1("Company Performance"),
              selectInput("Input", "Select a Company:",
                          list(`Name` = companyname)
                          ),
              # plotlyOutput("plot1")
      ),
      tabItem(tabName = "page5",
              # basic css style, I think we can build a ".css" file to decorate the whole projects
              fluidPage(
                h1(icon("globe"), "Best Companies", style = "text-align: center;"),
                br(),
                h2("Interactive Heat Map", style = "text-align: center;"),
                h5("(zoom in and check the locations", style = "text-align: center;"),
                leafletOutput("myMap")
              )
      ),
      tabItem(tabName = "page6",
              fluidPage(
                title = "Datasets",
                  tabsetPanel(
                    id = 'dataset',
                    tabPanel("SS", dataTableOutput("mytable1")),
                    tabPanel("DIA", dataTableOutput("mytable2")),
                    tabPanel("ISFL", dataTableOutput("mytable3")),
                    tabPanel("QQQ", dataTableOutput("mytable4")),
                    tabPanel("SPY", dataTableOutput("mytable5"))
                  )
              )
      )
    )
  )
)


server <- function(input, output, session) {
  
  output$mytable1 <- renderDataTable({
    datatable(SS)
  })
  
  output$mytable2 <- renderDataTable({
    datatable(DIA)
  })
  
  output$mytable3 <- renderDataTable({
    datatable(ISFL)
  })
  
  output$mytable4 <- renderDataTable({
    datatable(QQQ)
  })
  
  output$mytable5 <- renderDataTable({
    datatable(SPY)
  })
  
}

shinyApp(ui = ui, server = server)

