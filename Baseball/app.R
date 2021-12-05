
# Load packages ----------------------------------------------------------------

library(rsconnect)
library(shiny)
library(tidyverse)
library(tools)
library(hash)
library(ggplot2)
library(rvest)
library(dplyr)

# Load data --------------------------------------------------------------------

# load MVP data
#mvp <- as_tibble(read.csv2(here::here("Baseball/MVPClean1960_2020")))

# Load Cy Young Data
#cy <- as_tibble(read.csv2(here::here("Baseball/CYClean1960_2020")))

# Load Rookie Data
#rook <- as_tibble(read.csv2(here::here("Baseball/RookieClean1960_2020")))

# Loading in the teams data for pitchers and batters
#teamsBat <- read_csv2(here::here("Baseball/cleanTeamsBat1900_2020"))
#teamsPitch <- read_csv2(here::here("Baseball/cleanTeamsPitch1900_2020"))

# Loading individual player's data for batters and pitchers
#playerBat <- read_csv2(here::here("Baseball/cleanPlayerBat1960_2020"))
#playerPitch <- read_csv2(here::here("Baseball/cleanPlayerPitch1960_2020"))


# Save related datasets to lists
#awardSets <- list(mvp, rook, cy)

#teamSets <- list(teamsBat, teamsPitch)

#playerSets <- list(playerBat, playerPitch)

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            # Input selector for Y-axis
            selectInput(
                inputId = "baty",
                label = "Batter Statistic:",
                choices = c(
                    "Games Played" = "G",
                    "Plate Appearances" = "PA",
                    "At Bats" = "AB",
                    "Runs Scored" = "R",
                    "Hits" = "H",
                    "Singles Hit" = "`1B`",
                    "Doubles Hit" = "`2B`",
                    "Triples Hit" = "`3B`",
                    "Home Runs Hit" = "HR",
                    "Runs Batted In" = "RBI",
                    "Stolen Bases" = "SB",
                    "Caught Stealing" = "CS",
                    "Bases on Balls" = "BB",
                    "Strikeouts" = "SO",
                    "Double Plays Grounded Into" = "GDP",
                    "Times Hit by Pitch" = "HBP",
                    "Sacrifice Hits" = "SH",
                    "Sacrifice Flies" = "SF",
                    "Intentional Bases on Balls" = "IBB",
                    "Battting Average" = "AVG"
                ),
                selected = "AVG"
            ),
            
            # selectInput(
            #     inputId = "x",
            #     label = "Season:",
            #     choices = c(
            #         "Season" = "Year"
            #     ),
            #     selected = "Year"
            # ),
            
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
    # load MVP data
    mvp <- as_tibble(read.csv2("MVPClean1960_2020"))
    
    # Load Cy Young Data
    cy <- as_tibble(read.csv2("CYClean1960_2020"))
    
    # Load Rookie Data
    rook <- as_tibble(read.csv2("RookieClean1960_2020"))
    
    # Loading in the teams data for pitchers and batters
    teamsBat <- read_csv2("cleanTeamsBat1900_2020")
    teamsPitch <- read_csv2("cleanTeamsPitch1900_2020")
    
    # Loading individual player's data for batters and pitchers
    playerBat <- read_csv2("cleanPlayerBat1960_2020")
    playerPitch <- read_csv2("cleanPlayerPitch1960_2020")
    
    
    # Save related datasets to lists
    awardSets <- list(mvp, rook, cy)
    
    teamSets <- list(teamsBat, teamsPitch)
    
    playerSets <- list(playerBat, playerPitch)
    
    # Generate scatterplot for player Statistics
    output$scatterplot <- renderPlot({
        ggplot(data = playerBat, 
               aes_string(x = playerBat$Season, y = input$baty)) +
            geom_point() + 
            labs(
                title = paste0("Batter Stats For ", str_replace_all(input$baty, "`", "")),
                subtitle = "1960 to 2020"
            ) + 
            xlab("Season") +
            ylab(str_replace_all(input$baty, "`", ""))
    })


    # Histogram of Batting Average density across MVP Hitters

    output$histogram <- renderPlot({
        awardSets[[awardType()]] %>%
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
