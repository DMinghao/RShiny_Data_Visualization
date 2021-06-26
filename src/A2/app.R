library(shiny)
# Copy and paste part a below
if (!require(tidyverse))
    install.packages("tidyverse")
if (!require(ggthemes))
    install.packages("ggthemes")
if (!require(scales))
    install.packages("scales")
if (!require(plotly))
    install.packages("plotly")
if (!require(ggrepel))
    install.packages("ggrepel")

library(tidyverse)
library(ggthemes)
library(scales)
library(plotly)
library(ggrepel)


ui <- fluidPage(titlePanel("Programming Assignment 2"),
                
                sidebarLayout(
                    sidebarPanel(
                        sliderInput(
                            "year",
                            "Year:",
                            min = 1990,
                            max = 2013,
                            value = 1970,
                            step = 1,
                            animate = animationOptions(interval = 1000, loop = FALSE)
                        ),
                        
                        numericInput(
                            "num",
                            "Number of countries to show:",
                            value = 5,
                            min = 0,
                            max = 15,
                            step = 1
                        )
                    ),
                    
                    mainPanel(plotOutput("plot1"))
                ))


server <- function(input, output) {
    # Copy and paste parts b-c below
    avgYearEdu <- read_csv("avgYearEdu.csv")
    education <- read_csv("education.csv")
    gdp <- read_csv("GDP.csv")
    
    education <- education %>% select(-c(2, 4, 5, 6, 7))
    colnames(education) <- c("country", "year", "female", "male")
    
    avgYearEdu <- avgYearEdu %>% select(-c(2))
    colnames(avgYearEdu) <- c("country", "year", "avgYearEdu")
    
    gdp <- gdp %>% select(-c(2))
    colnames(gdp) <-
        c("country", "year", "GDPperCapita", "population")
    
    data <-
        education %>%
        inner_join(avgYearEdu, by = c("country", "year")) %>%
        inner_join(gdp, by = c("country", "year")) %>%
        na.omit()
    
    data <- data %>%
        mutate(population = population / 1000000) %>%
        mutate(gender = case_when(female > male ~ "Female", female <= male ~ "Male"))
    
    output$plot1 <- renderPlot({
        # Copy and paste part d below
        thisYear <- input$year
        filteredData <- data %>% filter(year == thisYear)
        
        cnum <- input$num
        countriesToShow <- filteredData %>%
            arrange(-population) %>%
            pull(country) %>%
            head(cnum)
        
        p <- filteredData %>%
            ggplot(mapping = aes(x = avgYearEdu,
                                 y = GDPperCapita)) +
            geom_point(aes(size = population,
                           color = gender)) +
            geom_text_repel(
                filteredData %>% filter(country %in% countriesToShow),
                mapping = aes(label = country)
            )
        
        
        p <- p +
            annotate(
                "text",
                size = 20,
                x = 7.5,
                y = 35000,
                color = "grey80",
                label = paste(thisYear)
            ) +
            labs(
                title = "Average number of years of education Vs. GDP per capita",
                x = "Average number of years of education",
                y = "GDP per capita",
                size = "Population in millions:",
                col = "Number of out-of-school childern is higher among:"
            ) +
            scale_size_continuous(limits = c(0, 1000)) +
            scale_y_continuous(labels = dollar, limits = c(0, 70000)) +
            scale_x_continuous(limits = c(0, 15))
        
        
        p <- p +
            theme_wsj() +
            theme(
                plot.title = element_text(size = rel(0.6), hjust = 0.5),
                legend.title = element_text(size = rel(0.5)),
                axis.title = element_text(face = "bold", size = rel(0.6))
            )
        
        p
    })
}

shinyApp(ui = ui, server = server)
