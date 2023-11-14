#Librerías----
library(shiny)
library(magrittr)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
#Datos----
vdem <- read.csv(file.path(getwd(), "vdem1.csv"), header = TRUE, sep = ";")
countries <- sort(unique(vdem$country_name))
avg <- aggregate(v2x_libdem ~ year, vdem, mean)
#UI----
ui <- fluidPage(
  titlePanel("Índice Liberal"),
  sidebarLayout(
    sidebarPanel(
      helpText(
        "la línea gris hace referencia al promedio entre todos los países"
      ),
      selectInput("Pais", "Selecciona un país:", choices = countries, selected = NULL) 
    ),
    mainPanel(
      plotOutput("grafico")
    )
  )
)
#Server----
server <- function(input, output) {
  vdem_xp <- reactive({
    vdem %>% filter(country_name == input$Pais)
  })
  output$grafico <- renderPlot({
    ggplot(vdem_xp(), aes(x = year)) +
      geom_line(aes(y = v2x_libdem)) +
      geom_line(data = avg, aes(y = v2x_libdem, group = 1), color = "gray", size = 1) +
      theme_ipsum() + 
      labs(x = "Año", y = "Índice Liberal")
  })
}
#App----
shinyApp(ui = ui, server = server)