#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")


ui <- fluidPage(
    titlePanel("State trends in COVID19 Cases"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "states",
                        label = "State",
                        choices = sort(covid19$state),
                        selected = c("California", "Minnesota", "Iowa"),
                        multiple = TRUE),
            submitButton(text = "Create my plot!")
        ),
        mainPanel(
            plotOutput("covidPlot")
        )
    )
)


server <- function(input, output) {
    
    options(scipen = 10)
    output$covidPlot <- renderPlot(
        covid19 %>% 
            group_by(state) %>% 
            filter(cases > 20,
                   state %in% input$states) %>% 
            mutate(days_since_20 = date - min(date)) %>% 
            ggplot(aes(x = days_since_20, y = cases, linetype = state, color = state)) +
            geom_line(size = 1.2) +
            scale_y_log10(labels = scales::comma) +
            scale_x_continuous() +
            labs(x = "Days since 20 cases",
                 y = "Total Cases (Log 10)",
                 linetype = "",
                 color = "")
    )
}

shinyApp(ui = ui, server = server)
