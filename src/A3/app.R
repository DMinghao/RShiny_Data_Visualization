if(!require(shiny)) {
    install.packages("shiny")
}
if (!require(shinydashboard)) {
    install.packages("shinydashboard")
}
if (!require(leaflet)) {
    install.packages("leaflet")
}
if (!require(DT)) {
    install.packages("DT")
}
if (!require(plotly)) {
    install.packages("plotly")
}
if (!require(tidyverse)) {
    install.packages("tidyverse")
}
if (!require(plyr)) {
    install.packages("plyr")
}
if (!require(mgcv)) {
    install.packages("mgcv")
}
if (!require(mapdeck)) {
    install.packages("mapdeck")
}

library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(plotly)
library(tidyverse)
library(plyr)
library(mgcv)
library(mapdeck)


ui <- dashboardPage(
    dashboardHeader(title = "Shiny Title"),
    dashboardSidebar(sidebarMenu(
        menuItem("Plotly", tabName = "page1", icon = icon("line-chart")),
        menuItem("Density", tabName = "page2", icon = icon("area-chart")),
        menuItem("Map", tabName = "page3", icon = icon("map-o"))
    )),
    dashboardBody(tabItems(
        tabItem(
            tabName = "page1",
            checkboxInput("holiday", label = "Show holidays", value = FALSE),
            plotlyOutput("plot2", height = 500)
        ),
        tabItem(
            tabName = "page2",
            sliderInput(
                "year",
                "Year:",
                min = 2014,
                max = 2020,
                value = 1,
                step = 1,
                animate = animationOptions(interval = 2000, loop = FALSE)
            ),
            plotlyOutput("plot1")
        ),
        tabItem(tabName = "page3",
                leafletOutput("myMap", width = "100%"), 
                mapdeckOutput(outputId = "mapdeck", width = "100%")
                )
    ))
)


server <- function(input, output, session) {
    data <- read_csv("data.csv")
    data <- data %>%
        mutate(ArrestDate = ArrestDate %>% as.Date("%m/%d/%Y")) %>%
        mutate(Sex = Sex %>% as.factor())
    
    
    holidays <- read_csv("usholidays.csv")
    
    Abb <- c(
        "NYD",
        "MLKB",
        "WaB",
        "MeD",
        "InD",
        "LaD",
        "CoD",
        "VeD",
        "ThD",
        "ChD",
        "NYD",
        "MLKB",
        "WaB"
    )
    
    holidays <- holidays %>%
        mutate(Abb = Holiday %>%
                   mapvalues(Holiday %>% unique(), Abb) %>%
                   as.factor())
    
    data_hol <- data %>%
        group_by(Date = ArrestDate) %>%
        tally() %>%
        full_join(holidays) %>%
        drop_na(n)
    
    dataAbb <- data_hol %>% select(Date, n, Abb) %>% drop_na()
    
    
    sp.base <- smooth.spline(data_hol$Date, data_hol$n)
    sp.gam <-
        gam(data_hol$n ~ s(data_hol$Date %>% as.numeric()))
    sp.pred <- predict(sp.gam, type = "response", se.fit = TRUE)
    sp.df <- data.frame(
        x = sp.gam$model[, 2],
        y = sp.pred$fit,
        lb = as.numeric(sp.pred$fit - (1.96 * sp.pred$se.fit)),
        ub = as.numeric(sp.pred$fit + (1.96 * sp.pred$se.fit))
    )
    sp.df <- sp.df[order(sp.df$x), ]
    
    ll.smooth <-
        loess(data_hol$n ~ data_hol$Date %>% as.numeric(), span = 0.75)
    ll.pred <- predict(ll.smooth, se = TRUE)
    ll.df <- data.frame(
        x = ll.smooth$x,
        fit = ll.pred$fit,
        lb = ll.pred$fit - (1.96 * ll.pred$se),
        ub = ll.pred$fit + (1.96 * ll.pred$se)
    )
    ll.df <- ll.df[order(ll.df$data_hol.Date.....as.numeric..),]
    
    resolution <- 3
    resLim <- 5e-4
    
    loc_data <- data %>%
        group_by(lng = round(Longitude, resolution),
                 lat = round(Latitude, resolution)) %>%
        tally() %>%
        mutate(latL = lat - resLim) %>%
        mutate(latH = lat + resLim) %>%
        mutate(lngL = lng - resLim) %>%
        mutate(lngH = lng + resLim)
    
    l <- reactiveValues(loadFlag = FALSE)
    
    output$plot1 = renderPlotly({
        year <- input$year
        # year <- 2014
        
        subData <- data %>%
            filter(ArrestDate %>% format('%Y') %>% as.numeric() == year)
        
        mData <- subData %>%
            filter(Sex == "M") %>%
            pull(Age) %>%
            density()
        
        fData <- subData %>%
            filter(Sex == "F") %>%
            pull(Age) %>%
            density()
        
        
        plot_ly(
            x = ~ mData$x,
            y = ~ mData$y,
            type = 'scatter',
            mode = 'lines',
            name = 'Male',
            hovertemplate = ~ paste("Age: ", mData$x, "<br>Density: ", mData$y),
            line = list(width = 4)
        ) %>%
            add_trace(
                x = ~ fData$x,
                y = ~ fData$y,
                name = 'Female',
                hovertemplate = ~ paste("Age: ", fData$x, "<br>Density: ", fData$y),
                line = list(width = 4)
            ) %>%
            layout(
                # hovermode = "x",
                margin = list(
                    l = 50,
                    r = 50,
                    b = 100,
                    t = 100,
                    pad = 20
                ),
                annotations = list(
                    text = year,
                    x = 60,
                    y = 0.03,
                    showarrow = FALSE,
                    font = list(
                        color = '#cccccc',
                        family = 'montserrat',
                        size = 80
                    )
                    
                ),
                title = list(
                    text = 'Distribution of crimes reported over four main gender',
                    font = list(family = 'Montserrat',
                                size = 20)
                    # xanchor = "right"
                ),
                xaxis = list(title = 'Age', showgrid = FALSE),
                yaxis = list(
                    title = 'Density',
                    showline = FALSE,
                    showticklabels = FALSE,
                    showgrid = FALSE
                ),
                legend = list(title = list(text = '<b>Gender</b>'))
            )
        
        
    })
    
    output$plot2 = renderPlotly({
        f <- data_hol %>%
            plot_ly(
                x = ~ Date,
                y = ~ n,
                type = 'scatter',
                mode = 'lines',
                name = "Data",
                line = list(color = "#cccccc")
            ) %>%
            add_lines(
                x = ~ Date,
                y = ll.pred$fit,
                name = "Mean",
                line = list(color = "green", width = 2)
            ) %>%
            add_ribbons(
                x = ~ Date,
                ymin = ll.df$lb,
                ymax = ll.df$ub,
                name = "Mean 95% CI",
                line = list(
                    opacity = 0.4,
                    width = 0,
                    color = "green"
                )
            ) %>%
            add_lines(
                x = ~ Date,
                y = sp.pred$fit,
                name = "GAM",
                line = list(color = "blue", width = 2)
            ) %>%
            add_ribbons(
                x = ~ Date,
                ymin = sp.df$lb,
                ymax = sp.df$ub,
                name = "GAM 95% CI",
                fillcolor = list(color = "blue",
                                 opacity = 0.5),
                line = list(color = "blue",
                            width = 0)
            ) %>%
            add_lines(
                x = ~ Date,
                y = predict(sp.base)$y,
                name = "smooth.spline",
                line = list(
                    color = "red",
                    opacity = 0.4,
                    width = 2
                )
            ) %>%
            layout(
                hovermode = "x unified",
                margin = list(
                    l = 50,
                    r = 50,
                    b = 100,
                    t = 100,
                    pad = 20
                ),
                title = list(
                    text = 'Arrests in Baltimore',
                    font = list(family = 'Montserrat',
                                size = 20)
                    # xanchor = "right"
                ),
                xaxis = list(title = 'Date'),
                yaxis = list(title = 'Number of arrests')
            )
        if (input$holiday) {
            f <- f %>% add_markers(
                x = dataAbb$Date,
                y = dataAbb$n,
                text = paste("Holiday: ", dataAbb$Abb),
                line = list(color = "red", width = 0),
                name = "Holidays"
            ) %>%
                layout(
                    annotations = list(
                        x = dataAbb$Date,
                        y = dataAbb$n,
                        text = dataAbb$Abb,
                        showarrow = FALSE,
                        yshift = 10
                    )
                )
        }
        
        f
        
    })
    
    output$myMap = renderLeaflet({
        m <- loc_data %>% leaflet() %>% addTiles() %>%
            setView(-76.6, 39.31, zoom = 12) %>%
            addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
            addLayersControl(
                baseGroups = c("Toner", "OSM"),
                options = layersControlOptions(collapsed = FALSE)
            )
        m <- m %>%
            addRectangles(
                lng1 = ~ lngL,
                lng2 = ~ lngH,
                lat1 = ~ latL,
                lat2 = ~ latH, 
                fillOpacity = ~ n / 150,
                opacity = 0, 
                fillColor = "red",
                label = ~ n
            )
        m
    })
    
    output$mapdeck = renderMapdeck({
        m <- mapdeck(style = mapdeck_style('dark'),
                location = c(-76.590001, 39.290001),
                zoom = 0,
                pitch = 0,
                bearing = 0)
        # flag <- !l$loadFlag
        l$loadFlag = TRUE
        m
    })
    
    observeEvent(l$loadFlag, {
        req(l$loadFlag)
        mapdeck_update(map_id = "mapdeck")%>% 
            mapdeck_view(
                location = c(-76.590001, 39.290001), 
                zoom = 11.3, 
                pitch = 45, 
                bearing = -45, 
                duration = 5000,
                transition = "fly"
            )%>% 
            add_hexagon(
                data = data, 
                layer_id = "hex_layer", 
                lon = "Longitude", 
                lat = "Latitude", 
                radius = 40,
                auto_highlight = TRUE, 
                elevation_scale = 2, 
                legend = TRUE, 
                update_view = FALSE,
                transitions = list(elevation = 3000, colour = 3000)
            ) 
        l$loadFlag = FALSE
    })
    
    
}

shinyApp(ui = ui, server = server)
