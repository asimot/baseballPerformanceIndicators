
# Load packages ----------------------------------------------------------------

library(rsconnect)
library(shiny)
library(tidyverse)
library(tools)


# Load data --------------------------------------------------------------------

# load MVP data
mvp <- as_tibble(read.csv2(here::here("Baseball/CopyOfMVPClean1960_2020")))

# Load Cy Young Data
cy <- as_tibble(read.csv2(here::here("Baseball/CopyOfCYClean1960_2020")))

# Load Rookie Data
rook <- as_tibble(read.csv2(here::here("Baseball/CopyOfRookieClean1960_2020")))

# Loading in the teams data for pitchers and batters
teamsBat <- read_csv2(here::here("Baseball/cleanTeamsBat1900_2020"))
teamsPitch <- read_csv2(here::here("Baseball/cleanTeamsPitch1900_2020"))

# Loading individual player's data for batters and pitchers
playerBat <- read_csv2(here::here("Baseball/cleanPlayerBat1960_2020"))
playerPitch <- read_csv2(here::here("Baseball/cleanPlayerPitch1960_2020"))


# Save related datasets to lists
awardSets <- list(mvp, rook, cy)

teamSets <- list(teamsBat, teamsPitch)

playerSets <- list(playerBat, playerPitch)

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            
            # Input selector for Y-axis
            selectInput(
                inputId = "y",
                label = "Y-axis:",
                choices = c(
                    "Average" = "AVG",
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
                selected = "AVG"
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
                inputId = "binwidth",
                label = "Bin Width:",
                min = 0, max = 0.50,
                value = 0.005,
                step = 0.005
            ),
            
            radioButtons(
                "award", "Select Award Category:",
                c("Most Valuable Player" = "mvp",
                  "Rookie of the Year" = "rook")

            ),
            
        ),

        mainPanel(
            plotOutput(outputId = "scatterplot"),
            plotOutput(outputId = "histogram")
        )
    )
)


# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
    
    # Generate scatterplot for player Statistics
    output$scatterplot <- renderPlot({
        ggplot(data = playerBat, aes_string(x = playerBat$Season, y = input$y)) +
            geom_point()
    })


    Selected_var <- reactive(mvp[[input$z]])
    Bw <- reactive((max(Selected_var())-min(Selected_var()))/mean(Selected_var()))
    # Scatter of Batting Average density across MVP Hitters

    
    # Histogram of Batting Average density across MVP Hitters

    output$histogram <- renderPlot({
        awardSets[[awardType()]] %>%
            # Removing pitchers from displayed data
            filter(is.na(ERA)) %>%
            ggplot(mapping = aes_string(x = input$z)) + 


            geom_histogram(binwidth = Bw(), color = "black", fill = "blue") + 


            geom_histogram(
                binwidth = input$binwidth, 
                color = "black", 
                fill = "blue"
                ) + 



            geom_density(color = "red") + 
            labs(
                title = paste0("Stats for ", awardName() , " Winners"),
                subtitle = "1960 to 2020",
                caption = "*Excludes Pitchers"
            ) +
            xlab(input$z) + 
            ylab("Count")
        
    })
    
    awardType <- reactive({
        a <- switch(input$award,
                    mvp = 1,
                    rook = 2,
                    1)
    })
    
    awardName <- reactive({
        a <- switch(input$award,
                    mvp = "Most Valuable Player",
                    rook = "Rookie of the Year",
                    cy = "Cy Young",
                    1)
    })
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
