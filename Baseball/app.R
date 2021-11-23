
# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(tools)

# Load data --------------------------------------------------------------------

# load MVP data
mvp <- read_csv2(here::here("Baseball/MVPClean1960_2020"))
# Load Cy Young Data
cy <- read_csv2(here::here("Baseball/CYClean1960_2020"))
# Load Rookie Data
rook <- read_csv2(here::here("Baseball/RookieClean1960_2020"))

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
                    "On Base Percentage" = "OPS",
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
            
            #      selectInput(
            #        inputId = "z",
            #        label = "Color by:",
            #        choices = c(
            #          "Title Type" = "title_type",
            #          "Genre" = "genre",
            #          "MPAA Rating" = "mpaa_rating",
            #          "Critics Rating" = "critics_rating",
            #          "Audience Rating" = "audience_rating"
            #        ),
            #        selected = "mpaa_rating"
            #      ),
            
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
            plotOutput(outputId = "scatterplot")
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
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
