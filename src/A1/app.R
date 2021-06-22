library(shiny)
library(tidyverse)

if (!require(plotly))
    install.packages("plotly")
library(plotly)

ui <- fluidPage(
    # Change yourname to your first name and last name
    h1("Programming Assignment 1: Minghao Du"),
    selectInput(
        inputId = "DeathConf",
        label = "Death/Confirmed",
        choices = c("Deaths", "Confirmed")
    ),
    plotlyOutput("plot1")
)

server <- function(input, output) {
    # Put the code for Part a-c below
    data <- read_csv("data_PA1.csv")
    data <- data %>% pivot_longer(-c(1, 2))
    data$name <- as.Date(data$name, format = "%m/%d/%y")
    data <- data %>% rename(Date = name, Count = value)
    
    output$plot1 <- renderPlotly({
        #Put the code for Part d below
        data1 <- data %>% filter(Type == input$DeathConf)
        
        # Put the code for Part e below
        p <- data1 %>%
            ggplot(mapping = aes(
                x = Date,
                y = Count,
                colour = Region
            )) +
            geom_line() +
            labs(title = paste(
                "COVID-19",
                input$DeathConf,
                "Case Count by Region",
                sep = " "
            ))
        
        p %>%
            ggplotly(x = ~Date) %>%
            layout(hovermode = "x unified") %>%
            layout(xaxis = list(autorange = TRUE),
                   yaxis = list(autorange = TRUE)) %>%
            layout(legend = list(
                orientation = "h",
                x = 0,
                y = -0.2
            ))
        
    })
}
shinyApp(ui, server)
