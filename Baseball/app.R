
# Load packages ----------------------------------------------------------------

library(rsconnect)
library(shiny)
library(tidyverse)
library(tools)
library(reactable)
library(reactablefmtr)
library(shinythemes)

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
    # Shiny app title
    titlePanel("Baseball Performance Statistics 1960-2020"),
    theme = shinytheme("cyborg"),
    
    sidebarLayout(
        sidebarPanel(
            # MLB logo Image
            # image credit : https://en.wikipedia.org/wiki/Major_League_Baseball_logo
            img(src = "MLB_logo.png", height = 100, width = 200, align = "center"),
            
            
            # Input selector for Y-axis
            # This will be play stats box plot
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
            
            # Numeric input for year 
            numericInput(
                inputId = "yr",
                label = "Year:",
                min = 1960, max = 2020,
                value = 2020,
                step = 1
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
            
            # Submit button for user designated refreshing versus live refresh
            submitButton(text = "Refresh Graphics", icon("refresh"))
        ),

        mainPanel(
          
            # Distribution of player stats by year
            plotOutput(outputId = "playerDistrib"),
            
            # Distribution of MVP and Rookie stats
            plotOutput(outputId = "awardees"),
            
            strong("Searchable Individual Batter Statistics", align = "center"),
            # Searchable player batting stats
            reactableOutput(outputId = "playerBatTable"),
            
            strong("Searchable Individual Pitcher Statistics", align = "center"),
            # Searchable player pitching stats
            reactableOutput(outputId = "playerPitchTable")
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
    
    # Histogram of Batting Average density across MVP Hitters

    output$awardees <- renderPlot({
            if(awardType() == 3) {
                awardSets[[awardType()]] %>%
                ggplot(mapping = aes_string(x = input$Pit)) + 
                    geom_histogram(
                        binwidth = input$binwidth, 
                        color = "black", 
                        fill = "red"
                    ) + 
                    labs(
                        title = paste0("Stats for ", awardName() , " Winners"),
                        subtitle = "1960 to 2020",
                    ) +
                    xlab(input$Pit) + 
                    ylab("Count") +
                    # Styling the plot
                    theme_dark() + 
                    theme(plot.background = element_rect(fill = "black"), 
                          plot.title = element_text(color = "white"),
                          plot.subtitle = element_text(color = "white"),
                          axis.title = element_text(color = "white"),
                          axis.text = element_text(color = "white"))
                
            }
            else{
                awardSets[[awardType()]] %>%
            # Removing pitchers from displayed data (extreme outliers)
                filter(is.na(ERA)) %>%
                ggplot(mapping = aes_string(x = input$z)) + 
                geom_histogram(
                    binwidth = input$binwidth, 
                    color = "black", 
                    fill = "red"
                 ) + 
                labs(
                  title = paste0("Stats for ", awardName() , " Winners"),
                 subtitle = "1960 to 2020",
                    caption = "*Excludes Pitchers"
                ) +
                xlab(input$z) + 
                ylab("Count") +
                # Styling the plot
                theme_dark() + 
                theme(plot.background = element_rect(fill = "black"), 
                      plot.title = element_text(color = "white"),
                      plot.subtitle = element_text(color = "white"),
                      plot.caption = element_text(color = "white"),
                      axis.title = element_text(color = "white"),
                      axis.text = element_text(color = "white"))
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
    
    # Only use integer inputs for year
    num_yr <- reactive({
        floor(input$yr)
    })
    
    # Searchable table for Batter Statistics
    output$playerBatTable <- renderReactable(
        reactable(playerBat,
                  defaultColDef = colDef(align = "center"),
                  sortable = TRUE,
                  resizable = TRUE,
                  filterable = TRUE,
                  searchable = TRUE,
                  pagination = TRUE,
                  defaultSorted = c("Season", "Name"),
                  bordered = TRUE,
                  striped = TRUE,
                  theme = cyborg()
        )
    )
    
    # Searchable table for Pitchers Statistics
    output$playerPitchTable <- renderReactable(
        reactable(playerPitch,
                  defaultColDef = colDef(align = "center"),
                  sortable = TRUE,
                  resizable = TRUE,
                  filterable = TRUE,
                  searchable = TRUE,
                  pagination = TRUE,
                  defaultSorted = c("Season", "Name"),
                  bordered = TRUE,
                  striped = TRUE,
                  theme = cyborg()
        )
    )
    
    # Box plot for descriptive statistics on batters
    output$playerDistrib <- renderPlot(
        playerBat %>%
            filter(Season == num_yr()) %>%
            ggplot(mapping = aes_string(x = num_yr(), y = input$baty)) + 
            geom_boxplot(outlier.alpha = 0, color = "red", fill = "orange", alpha = 0.4) +
            geom_jitter(color = "yellow") +
            labs(
                title = paste0("Distribution of ", input$baty , " Among Batters")
            ) +
            xlab(paste0("Year : ", num_yr())) + 
            ylab(input$baty) +
            # Styling the plot
            theme_dark() + 
            theme(plot.background = element_rect(fill = "black"), 
                  plot.title = element_text(color = "white"),
                  plot.subtitle = element_text(color = "white"),
                  axis.title = element_text(color = "white"),
                  axis.text = element_text(color = "white"),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank())
    )
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
