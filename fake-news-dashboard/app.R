# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(wordcloud2)
library(RColorBrewer)
library(plotly)

# Load dataset
news_data <- read.csv("data/combined_news_dataset.csv", stringsAsFactors = FALSE)

# UI
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Ctrl+Alt+Deceit"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Live Feed", tabName = "news", icon = icon("newspaper")),
      menuItem("Predictions", tabName = "predict", icon = icon("chart-bar")),
      menuItem("Dataset Viewer", tabName = "dataset", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # Overview Tab
      tabItem(tabName = "overview",
              h2("Overview Loaded Successfully"),
              fluidRow(
                valueBox(
                  value = sum(news_data$label == "FAKE", na.rm = TRUE),
                  subtitle = "Total Fake Articles",
                  icon = icon("exclamation-triangle"),
                  color = "red"
                ),
                valueBox(
                  value = sum(news_data$label == "REAL", na.rm = TRUE),
                  subtitle = "Total Real Articles",
                  icon = icon("check-circle"),
                  color = "green"
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  box(
                    title = "Word Cloud: Frequent Terms",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    wordcloud2Output("word_cloud", height = "300px")
                  )
                ),
                column(
                  width = 6,
                  box(
                    title = "Subject Distribution by Label",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    plotlyOutput("subject_distribution", height = "300px")
                  )
                )
              )
      ),
      
      # Live Feed Tab
      tabItem(tabName = "news",
              h2("Live Feed Loaded"),
              fluidRow(
                box(
                  title = "Filters",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  selectInput("subject_filter", "Filter by Subject:",
                              choices = c("All", sort(unique(news_data$subject))),
                              selected = "All"),
                  textInput("title_search", "Search by Title Keyword:", value = "")
                )
              ),
              fluidRow(
                box(
                  title = "Live News Feed",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("live_table")
                )
              )
      ),
      
      # Predictions Tab
      tabItem(tabName = "predict",
              h2("Predictions Loaded"),
              fluidRow(
                box(
                  title = "Fake vs Real News Count (Interactive Chart)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("prediction_chart")
                )
              ),
              fluidRow(
                box(
                  title = "Model Information",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  verbatimTextOutput("model_info")
                )
              )
      ),
      
      # Dataset Viewer Tab
      tabItem(tabName = "dataset",
              h2("Full Dataset Viewer"),
              fluidRow(
                box(
                  title = "Combined News Dataset",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("full_dataset_table")
                )
              )
      ),
      
      # About Tab
      tabItem(tabName = "about",
              h3("Project Description"),
              p("Ctrl+Alt+Deceit is a Shiny Dashboard designed to monitor and analyze the spread of fake news on Facebook in the Philippines."),
              h4("Key Features:"),
              tags$ul(
                tags$li("Interactive data visualizations (Plotly & ggplot2)"),
                tags$li("Real-time filtering of news articles"),
                tags$li("Machine Learning model summary for fake news classification"),
                tags$li("Text mining and word cloud visualizations")
              ),
              h4("Team Members:"),
              tags$ul(
                tags$li("TRABALLO, ALLEN MATHEW A."),
                tags$li("MINA, JOHN EMERICK S."),
                tags$li("PAYNITA, LEE MIKHAEL S."),
                tags$li("JOCSON, VINCE CHRISTIAN E."),
                tags$li("GABRIEL, CHARLES")
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Live Feed Table
  output$live_table <- renderDT({
    filtered_data <- news_data
    
    if (input$subject_filter != "All") {
      filtered_data <- subset(filtered_data, subject == input$subject_filter)
    }
    
    if (input$title_search != "") {
      filtered_data <- subset(filtered_data, grepl(input$title_search, title, ignore.case = TRUE))
    }
    
    datatable(filtered_data[, c("title", "subject", "date", "label")],
              options = list(pageLength = 10),
              colnames = c("Title", "Subject", "Date", "Label"))
  })
  
  # Interactive Word Cloud
  output$word_cloud <- renderWordcloud2({
    text_data <- paste(news_data$text, collapse = " ")
    words <- unlist(strsplit(text_data, "\\s+"))
    words <- gsub("[^a-zA-Z]", "", words)
    words <- tolower(words)
    words <- words[nchar(words) > 3]
    words <- words[nchar(words) < 15]
    
    stopwords <- c("https", "www", "com", "said", "listen", "report", "will", "that")
    words <- words[!words %in% stopwords]
    
    freq <- sort(table(words), decreasing = TRUE)
    df <- data.frame(word = names(freq), freq = as.numeric(freq))
    df <- head(df, 100)
    
    wordcloud2(df, size = 0.7, color = "random-light", backgroundColor = "white")
  })
  
  # Subject-wise Distribution
  output$subject_distribution <- renderPlotly({
    subject_data <- as.data.frame(table(news_data$subject, news_data$label))
    colnames(subject_data) <- c("Subject", "Label", "Count")
    
    p <- ggplot(subject_data, aes(x = Subject, y = Count, fill = Label)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "News Subject Distribution by Label", x = "Subject", y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Fake vs Real News Count
  output$prediction_chart <- renderPlotly({
    p <- ggplot(news_data, aes(x = label, fill = label)) +
      geom_bar() +
      labs(title = "Fake vs Real News Count", x = "News Type", y = "Count") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Model Info
  output$model_info <- renderPrint({
    cat("Model Used: XGBoost\n")
    cat("Accuracy: ~94.2%\n")
    cat("Features Used:\n")
    cat("- TF-IDF (from text)\n")
    cat("- Article Length\n")
    cat("- Subject Category\n")
    cat("\nEvaluation Metrics:\n")
    cat("- Accuracy, Precision, Recall, F1 Score, ROC-AUC\n")
    cat("\nTraining Dataset: Combined News Dataset (Kaggle)\n")
  })
  
  # Dataset Viewer
  output$full_dataset_table <- renderDT({
    datatable(news_data,
              options = list(pageLength = 15, scrollX = TRUE),
              filter = "top",
              rownames = FALSE)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
