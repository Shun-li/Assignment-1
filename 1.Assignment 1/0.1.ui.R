#1.Ui-----------------------------------------------------------------------------------------------------------
library(shiny)
library(DT)


ui <- fluidPage(
  titlePanel("120 Years History Olympic Games"),
  sidebarLayout(position = "right",
                sidebarPanel("SHUN LI      65322005"),
                mainPanel(tabsetPanel(type = "tabs", 
                                      tabPanel("Database",fluidRow(column
                                                                   (4,selectInput("Season", "Season:",c("All",unique(as.character(m$Season))))
                                                                   ),
                                                                   column(4,
                                                                          selectInput("Sex",
                                                                                      "Sex:",
                                                                                      c("All",
                                                                                        unique(as.character(m$Sex))))
                                                                   ),
                                                                   column(4,
                                                                          selectInput("Sport",
                                                                                      "Sport:",
                                                                                      c("All",
                                                                                        unique(as.character(m$Sport))))
                                                                   )
                                      ),fluidRow(
                                        DT::dataTableOutput("table1"))),
                                      
                                      
                                      tabPanel(("Number"),verbatimTextOutput("Number"), verbatimTextOutput("Plot")),
                                      
                                      
                                      tabPanel(
                                        ("Text"),
                                        
                                        plotOutput("wordcloud"))
                                      ,
                                      
                                      tabPanel(("Spatial"),tabsetPanel(type = "tabs",tabPanel("Amsterdam--1928",plotOutput("Amsterdam")),tabPanel("Melbourne--1956",plotOutput("Melbourne")),tabPanel("Munich--1972",plotOutput("Munich")),tabPanel("Rio--2016",plotOutput("Rio")))),
                                      
                                      tabPanel(("Time series"),plotOutput("weight"),plotOutput("Height"),plotOutput("correlation"),plotOutput("Ratios")),
                                      
                                      tabPanel(("Medalist data"),tabsetPanel(type = "tabs",tabPanel(("summer olympic medalist's average of age"),plotOutput("summer")),tabPanel(("Winter olympic medalist's average of age"),plotOutput("winter"))))
                                      
                )
                )
  )
)







