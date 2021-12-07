
# Load packages ----------------------------------------------------------------

library(rsconnect)
library(shiny)
library(tidyverse)
library(tools)
library(hash)
library(reactable)

# Load data --------------------------------------------------------------------
# This section is now moved to the server definition.
# By using loads here we only loaded data locally causing issues in Shiny

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
            # This will be play stats scatter plot
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
            
            # Input selector for Distribution of Awardees
            # Relates to MVP and Rookie of the Year
            # Cy Young being for pitchers has different stats
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
            
            # Input selector for Y-axis
            # This will be play stats scatter plot
            selectInput(
                inputId = "Pit",
                label = "Cy Young Statistic:",
                choices = c(
                    "Wins Above Replacement" = "WAR",
                    "Wins" = "W",
                    "Loses" = "L",
                    "Saves" = "SV",
                    "Earn Run Average" = "ERA",
                    "Innings Ptched" = "IP",
                    "Strikeouts" = "SO"
                ),
                selected = "W"
            ),       
            
            
            # Used to define the histogram's binwidth for distribution
            sliderInput(
                inputId = "binwidth",
                label = "Bin Width:",
                min = 0.005, max = 10,
                value = 0.005,
                step = 0.005
            ),
            
            # Used to select which award category to observe distribution of
            radioButtons(
                "award", "Select Award Category:",
                c("Most Valuable Player" = "mvp",
                  "Rookie of the Year" = "rook",
                  "Cy Young" = "cy")
            ),
        ),

        mainPanel(
            # Scatter for player stats
            plotOutput(outputId = "scatterplot"),
            
            # Distribution of MVP and Rookie stats
            plotOutput(outputId = "histogram"),
            
<<<<<<< HEAD
            # Distribution of Cy Young
            plotOutput(outputId = "Pitcher")
=======
            # Searchable player stats
            reactableOutput(outputId = "playerTable")
>>>>>>> e839a2068838091c5fc99fc53270907685fff595
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
    
    # Generate scatter plot for player Statistics
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
            if(awardType() == 3) {
                awardSets[[awardType()]] %>%
                ggplot(mapping = aes_string(x = input$Pit)) + 
                    geom_histogram(
                        binwidth = input$binwidth, 
                        color = "black", 
                        fill = "blue"
                    ) + 
                    labs(
                        title = paste0("Stats for ", awardName() , " Winners"),
                        subtitle = "1960 to 2020",
                    ) +
                    xlab(input$Pit) + 
                    ylab("Count")
                
            }
            else{
                awardSets[[awardType()]] %>%
            # Removing pitchers from displayed data (extreme outliers)
                filter(is.na(ERA)) %>%
                ggplot(mapping = aes_string(x = input$z)) + 
                geom_histogram(
                    binwidth = input$binwidth, 
                    color = "black", 
                    fill = "blue"
                 ) + 
                labs(
                  title = paste0("Stats for ", awardName() , " Winners"),
                 subtitle = "1960 to 2020",
                    caption = "*Excludes Pitchers"
             ) +
                xlab(input$z) + 
                ylab("Count")
            }
    })

    
    # Reactive to select data set from list based on award type button
    awardType <- reactive({
        a <- switch(input$award,
                    mvp = 1,
                    rook = 2,
                    cy = 3,
                    1)
    })
    
    # Used for titles to give a plot name based on selected award
    awardName <- reactive({
        a <- switch(input$award,
                    mvp = "Most Valuable Player",
                    rook = "Rookie of the Year",
                    cy = "Cy Young",
                    1)
    })
    
    output$playerTable <- renderReactable(
        reactable(playerBat,
                  defaultColDef = colDef(align = "center"),
                  sortable = TRUE,
                  resizable = TRUE,
                  filterable = TRUE,
                  searchable = TRUE,
                  pagination = TRUE,
                  defaultSorted = c("Season", "Name")
        )
    )
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
