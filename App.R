library(shinydashboard)
library(text2vec)
library(dplyr)
library(plotly)
library(stringr)

#################################  UI PART ###########################################################

ui <- dashboardPage(
  dashboardHeader(title = "Nederlandse Word-Embeddings", titleWidth = 600),
  dashboardSidebar(
    width=300,
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("dashboard")),
      menuItem("Word Embeddings Similarity", tabName = "WordEmbeddings", icon = icon("dashboard")),
     # menuItem("Word Embeddings Associated", tabName = "WordEmbeddings2", icon = icon("dashboard")),
      textInput("word", "Word to search for", value="aanslagen"),
      #textInput("word1", "Word association 1", value="man"),
      #textInput("word2", "Word association 2", value="vrouw"),
      numericInput("nw", "Number of similar words", value= 10)
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "introduction",
        h3("Introduction"),
        list(
          h4("Using the text2vec package, I have used 500K RTL Nieuws articles to generate 45K word embeddings."),
          p(" "),
          h4("Cheers, Longhow")
          )
      ),
      tabItem(
        tabName = "WordEmbeddings",
        h4("Word Embeddings: Similar words"),
        fluidRow(
          dataTableOutput('we')
        )
      ),
      tabItem(
        tabName = "WordEmbeddings2",
        h4("Word Embeddings: Associated"),
        fluidRow(
          dataTableOutput('we2')
        )
      )
    )
    )
  )


################################  SERVER PART ########################################################


word_vectors = readRDS("data/NL_word_vectors.RDS")
woorden = rownames(word_vectors)

server <- function(input, output, session) {
  
  ######## reactive function #################
  

  ######## TABLE with closest words #############################
  
  output$we = renderDataTable({
    
    if(input$word %in% woorden)
    {
      WV <- word_vectors[str_to_lower(input$word), , drop = FALSE]
      cos_sim = sim2(x = word_vectors, y = WV, method = "cosine", norm = "l2")
      tmp = data.frame(
        head(
          sort(cos_sim[,1], decreasing = TRUE), 
          input$nw
        )
      )
    
      tibble(
        Words = row.names(tmp),
        similarity = tmp[,1]
      )
    }
  })
  
  
  output$we2 = renderDataTable({
    
    if(input$word %in% woorden & input$word1 %in% woorden & input$word2 %in% woorden )
    {
      
      WV <- word_vectors[input$word, , drop = FALSE] - 
        word_vectors[input$word1, , drop = FALSE]  + 
        word_vectors[input$word2, , drop = FALSE] 
      cos_sim = sim2(x = word_vectors, y = WV, method = "cosine", norm = "l2")
      tmp = data.frame(
        head(
          sort(cos_sim[,1], decreasing = TRUE), 
          input$nw
        )
      )
      
      tibble(
        Words = row.names(tmp),
        similarity = tmp[,1]
      )
    }
  })
  
  
  ######## plotly graph of extracted tags #########################
  output$tagoverview = renderPlotly({
    tmp = extractedImages()
    if(!is.null(tmp)){
      tmp %>% 
      group_by(class_description) %>% 
      summarise(n=mean(score)) %>%
      mutate(
        class_description = forcats::fct_reorder(class_description, n, .desc=TRUE)
      ) %>%
      plot_ly(
        x = ~class_description, 
        y = ~n, 
        type="bar"
      )
    }
  })
}

shinyApp(ui, server)