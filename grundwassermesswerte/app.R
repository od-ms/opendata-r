
library(shiny)
library(rlog)
library(DT)
library(ggplot2)
library(dplyr)

Sys.setenv("LOG_LEVEL" = "DEBUG")
rlog::log_info("#### script startup ####")

# Source: https://opendata.stadt-muenster.de/sites/default/files/grundwasser-messwerte-muenster.csv
groundwater <- read.csv(file = 'data/grundwasser-messwerte-muenster.csv', sep=';', 
                        dec=",", stringsAsFactors=FALSE, 
                        colClasses=c("numeric","character","character", "numeric", "character", rep("numeric",5)))

groundwater$checkVal <- groundwater$Geländeoberkante..m.über.NHN2016. - groundwater$Wasserstand..m.über.NHN2016. 
groundwater$FixedDate <- as.Date(groundwater$Datum,format="%Y-%m-%d")

spaltennamen = colnames(groundwater)
rlog::log_info("Spaltennamen der CSV-Datei:")
rlog::log_info(spaltennamen)

rlog::log_info("Liste der Messpunkte:")
messpunkte <- unique(groundwater[2]) 
rlog::log_info(messpunkte)

rlog::log_info("Final Data Frame:")
rlog::log_info(str(groundwater))
#rlog::log_info("End Filtered Water")

ui <- fluidPage(
  fluidRow(
    column(5,
           h4("Grundwassermesswerte Münster"),
           br(),
           selectInput("dataset", label = "Messpunkt wählen", messpunkte),
    ),
    column(6, offset = 1,
           verbatimTextOutput("summary"),
    )
  ),
  plotOutput("plot", click = "plot_click"),
  DT::dataTableOutput("table2")
)

server <- function(input, output, session) {
  
  filtered_water = reactive(
    groundwater 
    |> filter(Name == input$dataset)
    |> select(3,6:8,11,12)
    )
  
  output$summary <- renderPrint({
    summary( filtered_water())
  })
  
  output$plot <- renderPlot({
    ggplot(filtered_water(),aes(x=FixedDate,y=checkVal))+geom_point(colour='red')
  }, res = 96)
  
  output$table2 <- renderDT({
    head(filtered_water(), 100)
  })
  #output$table <- renderTable({
  #  head(filtered_water())
  #})
}

shinyApp(ui, server)
