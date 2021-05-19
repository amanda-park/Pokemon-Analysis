library(shinydashboard)
library(shiny)
library(shinyWidgets)

require(pacman)

p_load(tidyverse,
       tidytext,
       ggplot2,
       textdata,
       tokenizers,
       purrr,
       plotly)

source("shinyAppFunctions.R")
data <- read.csv("PokedexEntries.csv")
data <- tibble(data[,-1])

#Load stopwords
data(stop_words)

#Create tidy data for analysis
tidyData <- data %>%
    unnest_tokens(word, PokedexEntry) %>%
    anti_join(stop_words)

statsData <- read.csv("pokemonStats.csv")

statsData <- statsData %>%
    select(name, generation, type1, type2, attack, defense, speed,
           sp_attack, sp_defense, hp, is_legendary) %>%
    mutate(
        grassType = as.factor(if_else(type1 == "grass", 1, if_else(type2 == "grass", 1, 0))),
        normalType = as.factor(if_else(type1 == "normal", 1, if_else(type2 == "normal", 1, 0))),
        fireType = as.factor(if_else(type1 == "fire", 1, if_else(type2 == "fire", 1, 0))),
        waterType = as.factor(if_else(type1 == "water", 1, if_else(type2 == "water", 1, 0))),
        groundType = as.factor(if_else(type1 == "ground", 1, if_else(type2 == "ground", 1, 0))),
        rockType = as.factor(if_else(type1 == "rock", 1, if_else(type2 == "rock", 1, 0))),
        iceType = as.factor(if_else(type1 == "ice", 1, if_else(type2 == "ice", 1, 0))),
        electricType = as.factor(if_else(type1 == "electric", 1, if_else(type2 == "electric", 1, 0))),
        psychicType = as.factor(if_else(type1 == "psychic", 1, if_else(type2 == "psychic", 1, 0))),
        ghostType = as.factor(if_else(type1 == "ghost", 1, if_else(type2 == "ghost", 1, 0))),
        darkType = as.factor(if_else(type1 == "dark", 1, if_else(type2 == "dark", 1, 0))),
        fightingType = as.factor(if_else(type1 == "fighting", 1, if_else(type2 == "fighting", 1, 0))),
        dragonType = as.factor(if_else(type1 == "dragon", 1, if_else(type2 == "dragon", 1, 0))),
        fairyType = as.factor(if_else(type1 == "fairy", 1, if_else(type2 == "fairy", 1, 0))),
        bugType = as.factor(if_else(type1 == "bug", 1, if_else(type2 == "bug", 1, 0))),
        poisonType = as.factor(if_else(type1 == "poison", 1, if_else(type2 == "poison", 1, 0))),
        flyingType = as.factor(if_else(type1 == "flying", 1, if_else(type2 == "flying", 1, 0))),
        steelType = as.factor(if_else(type1 == "steel", 1, if_else(type2 == "steel", 1, 0)))
    )


ui <- dashboardPage(
    skin = "red",
    dashboardHeader(title = "Pokemon Shiny App"),

    dashboardSidebar(
        sidebarMenu(
            menuItem("Individual Pokemon",
                tabName = "indiv_pkmn_tab"),
            menuItem("Filter by Category",
                     tabName = "generation_pkmn")
        )
    ),

    dashboardBody(
        tabItems(
        tabItem(
            tabName = "indiv_pkmn_tab",
                box(
                    uiOutput("name"),
                    width = 12
                    ),
            
                tabBox(
                    tabPanel("Combat Statistics", plotlyOutput("pkmn_stats")),
                    tabPanel("Word Count", plotOutput("pkmn_word_count")),
                    width = 12
                )
            ),

        #Filter on Pokemon tab content
        tabItem(
            tabName = "generation_pkmn",
            box(
                uiOutput("generation"),
                width = 4
                ),
            box(
                uiOutput("type1"), 
                width = 4
                ),
            box(
                uiOutput("legendary"), 
                width = 4
            ),
            box(
                plotOutput("generation_word_count"),
                width = 12
            )
        )
        )

    )
)


server <- function(input, output) {

    output$name <- renderUI({
        selectInput(
            inputId = "name",
            label = "Pokemon",
            choices = statsData %>% select(name) %>% distinct()
                   )

    })

    output$generation <- renderUI({
        pickerInput(
            inputId = "generation",
            label = "Generation",
            choices = statsData %>% select(generation) %>% unique(),
            selected = 1,
            options = list(
                `actions-box` = TRUE
            ), 
            multiple = TRUE
        )

    })

    output$type1 <- renderUI({
        pickerInput(
            inputId = "type1",
            label = "Type 1",
            choices = statsData %>% select(type1) %>% distinct(),
            selected = 'fire',
            options = list(
                `actions-box` = TRUE
            ), 
            multiple = TRUE
        )
        
    })
    
    output$legendary <- renderUI({
        pickerInput(
            inputId = "legendary",
            label = "Legendary",
            choices = statsData %>% select(is_legendary) %>% distinct(),
            selected = 0,
            options = list(
                `actions-box` = TRUE
            ), 
            multiple = TRUE
        )
        
    })

    output$pkmn_word_count <- renderPlot({

        prepData <- tidyData %>%
            filter(Pokemon == input$name) %>%
            count(word, sort = TRUE) %>%
            filter(n > 1) %>%
            mutate(word = reorder(word, n)) %>%
            head(10)

        ggplot(prepData, aes(n, word)) +
            geom_col() +
            labs(y = NULL) +
            ggtitle(paste0("Most Frequent Words in Pokedex for ", input$name)) 

    })

    output$generation_word_count <- renderPlot({

        filt <- statsData %>%
            select(name, generation, type1, is_legendary)

        prepData <- tidyData %>%
            left_join(filt, by = c("Pokemon" = "name"))

        prepData <- prepData %>%
            filter(generation %in% input$generation,
                   type1 %in% input$type1,
                   is_legendary %in% input$legendary) %>%
            count(word, sort = TRUE) %>%
            filter(n > 1) %>%
            mutate(word = reorder(word, n)) %>%
            head(25)

        ggplot(prepData, aes(n, word)) +
            geom_col() +
            labs(y = NULL) +
            ggtitle(paste0("Most Frequent Words in Pokedex"))

    })

    output$pkmn_stats <- renderPlotly({
        pkmnIndivData <- statsData %>%
            filter(name == input$name)

        plot_ly(
            type = "scatterpolar",
            r = c(pkmnIndivData$attack,
                  pkmnIndivData$defense,
                  pkmnIndivData$sp_attack,
                  pkmnIndivData$sp_defense,
                  pkmnIndivData$speed,
                  pkmnIndivData$hp,
                  pkmnIndivData$attack),
            theta = c("Attack",
                      "Defense",
                      "Sp. Attack",
                      "Sp. Defense",
                      "Speed",
                      "HP",
                      "Attack"),
            fill = 'toself'
        )
    })




}

# Run the application
shinyApp(ui = ui, server = server)
