
# Load packages ----------------------------------------------------------------

library(shiny)
library(tidyverse)
library(tools)

# Load data --------------------------------------------------------------------

# load MVP data
mvp <- as_tibble(read.csv2(here::here("Baseball/MVPClean1960_2020")))
#max(mvp$WAR) - min(mvp$WAR)
# Load Cy Young Data
cy <- read.csv2(here::here("Baseball/CYClean1960_2020"))
# Load Rookie Data
rook <- read.csv2(here::here("Baseball/RookieClean1960_2020"))

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            
            # Input selector for Y-axis
            selectInput(
                inputId = "y",
                label = "Y-axis:",
                choices = c(
                    "Wins Above Replacement" = "WAR",
                    "Batting Average" = "BA",
                    "On Base Percentage" = "OBP",
                    "Slugging" = "SLG",
                    "Home Runs" = "HR",
                    "Runs Batted In" = "RBI",
                    "Stolen Bases" = "SB",
                    "Wins" = "W",
                    "Loses" = "L",
                    "Saves" = "SV",
                    "Earn Run Average" = "ERA",
                    "Innings Pitched" = "IP",
                    "Strike Outs" = "SO"
                ),
                selected = "WAR"
            ),
            
            
            selectInput(
                inputId = "x",
                label = "Season:",
                choices = c(
                    "Season" = "Year"
                ),
                selected = "Year"
            ),
            
            selectInput(
                inputId = "z",
                label = "Distribution Selector",
                choices = c(
                    "Wins Above Replacement" = "WAR",
                    "Batting Average" = "BA",
                    "On Base Percentage" = "OBP",
                    "Slugging" = "SLG"
                ),
                selected = "BA"
            ),
            
            sliderInput(
                inputId = "alpha",
                label = "Alpha:",
                min = 0, max = 1,
                value = 0.5
            ),
            
            sliderInput(
                inputId = "size",
                label = "Size:",
                min = 0, max = 5,
                value = 2
            ),
            
            sliderInput(
                inputId = "binwidth",
                label = "Bin Width:",
                min = 0, max = 0.50,
                value = 0.005,
                step = 0.005
            ),
            
            # textInput(
            #     inputId = "plot_title",
            #     label = "Player Stats",
            #     #        placeholder = "Enter text to be used as plot title"
            # ),
            
            #      actionButton(
            #        inputId = "update_plot_title",
            #        label = "Update plot title"
            #      )
        ),

        mainPanel(
            plotOutput(outputId = "scatterplot"),
            plotOutput(outputId = "histogram")
        )
    )
)


# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
    # new_plot_title <- eventReactive(
    #     eventExpr = input$update_plot_title,
    #     valueExpr = {
    #         toTitleCase(input$plot_title)
    #     }
    # )
    
    # Generate scatterplot for awardee Statistics# Generate scatterplot for awardee Statistics
    output$scatterplot <- renderPlot({
        ggplot(data = mvp, aes_string(x = input$x, y = input$y)) +
            geom_point()
    })
    
    # Histogram of Batting Average density across MVP Hitters
    output$histogram <- renderPlot({
        mvp %>%
            # Removing pitchers from displayed data
            filter(is.na(ERA)) %>%
            ggplot(mapping = aes_string(x = input$z)) + 
            geom_histogram(
                binwidth = input$binwidth, 
                color = "black", 
                fill = "blue"
                ) + 
            geom_density(color = "red") + 
            labs(
                title = "Batter Stats of MVP Winners",
                subtitle = "1960 to 2020",
                caption = "*Excludes Pitchers"
            ) +
            xlab(input$z) + 
            ylab("Count")
        
    })
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
