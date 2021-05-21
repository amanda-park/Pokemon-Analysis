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

stop_words <- stop_words %>%
    add_row(word = "it’s", lexicon = "SMART") %>%
    add_row(word = "it’ll", lexicon = "SMART") %>%
    add_row(word = "pokémon", lexicon = "SMART") %>%
    add_row(word = "pokémon’s", lexicon = "SMART")

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
    dashboardHeader(title = "Pokémon Shiny App"),

    dashboardSidebar(
        sidebarMenu(
            menuItem("Individual Pokémon",
                tabName = "indiv_pkmn_tab"),
            menuItem("Pokédex Common Words",
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
            tabBox(
                tabPanel("Words", plotOutput("word_count")),
                tabPanel("Bigrams", plotOutput("bigram_count")),
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

        type <- statsData %>%
            filter(name == input$name) %>%
            select(type1)
        
        fill_color <- case_when(
            type == 'grass' ~ '#7AC74C',
            type == 'fire' ~ '#EE8130',
            type == 'water' ~ '#6390F0',
            type == 'fighting' ~ '#C22E28',
            type == 'poison' ~ '#A33EA1',
            type == 'normal' ~ '#A8A77A',
            type == 'ice' ~ '#96D9D6',
            type == 'dark' ~ '#705746',
            type == 'steel' ~ '#B7B7CE',
            type == 'fairy' ~ '#D685AD',
            type == 'psychic' ~ '#F95587',
            type == 'rock' ~ '#B6A136',
            type == 'ghost' ~ '#735797', 
            type == 'bug' ~ '#A6B91A',
            type == 'dragon' ~ '#6F35FC',
            type == 'flying' ~ '#A98FF3',
            type == 'ground' ~ '#E2BF65',
            type == 'electric' ~ '#F7D02C',
            TRUE ~ 'black'
        )
        
        ggplot(prepData, aes(n, word)) +
            geom_col(fill = fill_color) +
            labs(y = NULL, x = "Frequency") +
            ggtitle(paste0("Most Frequent Words in Pokedex for ", input$name)) 

    })

    output$bigram_count <- renderPlot({
        filt <- statsData %>%
            select(name, generation, type1, is_legendary)
        
        bigrams <- data %>%
            unnest_tokens(ngram, PokedexEntry, token = "ngrams", n = 2) 
        
        bigrams_sep <- bigrams %>%
            separate(ngram, c("word1", "word2"), sep = " ")
        
        bigrams_filt <- bigrams_sep %>%
            filter(!word1 %in% stop_words$word) %>%
            filter(!word2 %in% stop_words$word) %>%
            unite(ngram, word1, word2, sep = " ")
        
        prepData <- bigrams_filt %>%
            left_join(filt, by = c("Pokemon" = "name"))
        
        prepData <- prepData %>%
            filter(generation %in% input$generation,
                   type1 %in% input$type1,
                   is_legendary %in% input$legendary) %>%
            count(ngram, sort = TRUE) %>%
            filter(n > 1) %>%
            mutate(ngram = reorder(ngram, n)) %>%
            head(25)
        
        ggplot(prepData, aes(n, ngram)) +
            geom_col() +
            labs(y = NULL, x = "Frequency",
                 title = "Most Frequent Bigrams in Pokedex",
                 subtitle = "Stop Words Removed")
        
    })
    
    output$word_count <- renderPlot({

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
            labs(y = NULL, x = "Frequency",
                 title = "Most Frequent Words in Pokedex",
                 subtitle = "Stop Words Removed")

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
