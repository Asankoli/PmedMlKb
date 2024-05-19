library(shiny)
library(shinydashboard)
library(DBI)
library(pool)
library(DT)
library(bslib)
library(tidyverse)
library(glue)
library(readxl)

# Database connection setup
db <- config::get("azuredb")
pool <- dbPool(
  drv = RMariaDB::MariaDB(), 
  dbname = db$database,
  host = db$server,
  port = db$port,
  username = db$uid,
  password = db$pwd,
  ssl.ca = db$ssl_ca,
  sslmode = "require"
)

# Assuming Classifications.csv contains unique classifications for selection
classifications_list <- read_csv("RData/Classifications.csv", col_names = FALSE, show_col_types = FALSE)
names(classifications_list) <- c("classifiers")

keywords_df <- read_csv("RData/unique_keywords.csv", col_names = FALSE, show_col_types = FALSE)
names(keywords_df) <- c("keyword")


# Ensure the pool is closed when the app is closed
tbl <- db$table
onStop(function() {poolClose(pool)})

# UI definition
ui <- page_sidebar(
  theme = bs_theme(bootswatch = "journal"),
  title = "PubMed Machine Learning Article knowledgebase",
  sidebar = sidebar(
    selectizeInput('classification', 'Study Category:', choices = NULL, selected = NULL, multiple = FALSE, options = NULL), # Choices will be updated server-side
    tags$hr(),
    
    textInput("term_query", label = "Abstract Search Term:", placeholder = "e.g., Machine Learning"),
    tags$hr(),

    selectizeInput('keyword_search', 'Article Keyword:', choices = NULL, selected = NULL, multiple = TRUE, options = NULL),
    tags$hr(),
    
    
    # selectInput("keyword_filter", h6("Keywords Boolean Operator:"), choices = c("OR", "AND"), selected = "OR"),
    actionButton("submit", "Search", class = "btn-info"),
    actionButton("clear_results", "Reset Results", class = "btn-info"),
    downloadButton("download_csv", "Download CSV", class = "btn-info"),

    # # Uncomment to display sql query for debugging
    # textOutput("display_query"),

    tags$h5("Highlight Legend:"),
    tags$ul(style = "list-style-type: none; padding: 0;",
      tags$li(style = "margin-bottom: 10px;", 
        tags$span(style = "display: inline-block; width: 12px; height: 12px; background-color: #60b0f6; margin-right: 5px;"), 
        "Abstract Search"),
      tags$li(style = "margin-bottom: 10px;", 
        tags$span(style = "display: inline-block; width: 12px; height: 12px; background-color: #f1f173; margin-right: 5px;"), 
        "Keywords")
    )

  ),
  card(
    card_header(class ="bg-dark", "Results"),
    fluidRow(
      column(width = 12, h4(""), 
        div(style = "font-size: 16px; font-weight: bold;", textOutput("result_count"))
      ), 
      DTOutput("datatable")
    )
  )
)


# Function to highlight keywords within the Keywords column
highlightKeywords <- function(text, selectedKeywords, color) {
  for(keyword in selectedKeywords) {
    regex_pattern <- paste0("(?i)(", keyword, ")")
    text <- gsub(regex_pattern, paste0("<span style='background-color: ", color, ";'>\\1</span>"), text, perl = TRUE)
  }
  return(text)
}


# Server logic
server <- function(input, output, session) {
  # Dynamically update select input choices
  updateSelectizeInput(session, 'classification', choices = classifications_list$classifiers, server = TRUE)
  updateSelectizeInput(session, 'keyword_search', choices = keywords_df$keyword, server = TRUE)
  current_query <- reactiveVal("")

  query_result_reactive <- reactive({

    # Construct the initial query
    base_query <- glue_sql("SELECT * FROM {`tbl`}", .con = pool)

    # Initialize an empty list to collect conditions
    conditions <- vector("list")

    # Add classification condition if selected
    if (!is.null(input$classification) && input$classification != "Choose") {
      conditions <- c(conditions, glue_sql("{`tbl`}.classification = {input$classification}", .con = pool))
    }
    
    # Add keywords condition if any keyword is selected
    if (!is.null(input$keyword_search) && length(input$keyword_search) > 0) {
      keyword_pattern <- paste(input$keyword_search, collapse="|")
      conditions <- c(conditions, glue_sql("{`tbl`}.Keywords REGEXP {keyword_pattern}", .con = pool))
    }

    # Prepare a condition for Abstract if term_query is provided
    if (!is.null(input$term_query) && input$term_query != "") {
      term_query_pattern <- input$term_query
      conditions <- c(conditions, glue("`{tbl}`.Abstract LIKE '%{term_query_pattern}%'"))
    }
    
    # Combine conditions into a WHERE clause if there are any
    if (length(conditions) > 0) {
      where_clause <- paste("WHERE", paste(conditions, collapse = " AND "))
      base_query <- paste(base_query, where_clause)
    }
  
    # # Uncomment to display sql query for debugging
    # Store the final query for display
    # current_query(base_query)
    
    # Execute the query
    query_result <- dbGetQuery(pool, base_query)
    
    # Convert binary data to character strings
    query_result$Title <- query_result$Title %>% map(~ .x[!.x == 00]) %>% map_chr(rawToChar) %>% iconv(from = "WINDOWS-1252", to = "UTF-8")
    query_result$Abstract <- query_result$Abstract %>% map(~ .x[!.x == 00]) %>% map_chr(rawToChar) %>% iconv(from = "WINDOWS-1252", to = "UTF-8")
    query_result$Keywords <- query_result$Keywords %>% map(~ .x[!.x == 00]) %>% map_chr(rawToChar) %>% iconv(from = "WINDOWS-1252", to = "UTF-8")
    

    # Highlight selected keywords in the Keywords column
    if (!is.null(input$keyword_search) && length(input$keyword_search) > 0) {
      query_result$Keywords <- sapply(query_result$Keywords, highlightKeywords, selectedKeywords = input$keyword_search, color = "#f1f173")  # Yellow for keywords
    }
    if (!is.null(input$term_query) && input$term_query != "") {
      query_result$Abstract <- sapply(query_result$Abstract, highlightKeywords, selectedKeywords = c(input$term_query), color = "#60b0f6")  # Light green for term_query
    }


    # Create PMID href links
    query_result <- query_result %>% 
      mutate(PMID = paste0("<a href='https://pubmed.ncbi.nlm.nih.gov/", PMID, "/' target='_blank'>", PMID, "</a>"))
    
    return(query_result)
  })

  # # Uncomment to display sql query for debugging
  # output$display_query <- renderText({
  #   current_query()
  # })

  # Use the reactive expression for displaying the DataTable
  output$datatable <- renderDT({
    datatable(query_result_reactive() %>%
      as_tibble() %>% 
      select(PMID, Title, Abstract, Keywords, `Journal/Book` = `Journal/Book`, Classification), 
      escape = FALSE, 
      options = list(searching = TRUE, paging = TRUE, lengthMenu = c(5, 10, 50, 100, 200))
    )
  })
  

  # Display the count of results
  output$result_count <- renderText({
    n <- nrow(query_result_reactive())
    paste(n, "results found")
  })
  

  # Implement the download functionality
  output$download_csv <- downloadHandler(
    filename = function() {
      datetime <- format(Sys.time(), "%Y-%m-%d_%H%M")
      paste("query_results_", datetime, ".csv", sep = "")
    },
    content = function(file) {
      # Get the reactive dataset
      original_data <- query_result_reactive()
      
      # Function to remove HTML tags
      remove_html_tags <- function(text) {
        gsub("<[^>]*>", "", text)
      }
      
      # Apply the function to columns that may contain HTML tags
      original_data$Abstract <- sapply(original_data$Abstract, remove_html_tags)
      original_data$Keywords <- sapply(original_data$Keywords, remove_html_tags)
      
      # If you had the PMID as clickable links and want to extract just the PMID numbers
      original_data$PMID <- gsub("<a href='(.*)' target='_blank'>(.*)</a>", "\\1", original_data$PMID)
      
      # Select columns to write to CSV, excluding 'send_id' and any other unwanted columns
      data_to_download <- original_data %>% select(-send_id)
      
      write.csv(data_to_download, file, row.names = FALSE)
    }
  )


  # Reset functionality
  observeEvent(input$clear_results, {
    updateSelectizeInput(session, 'classification', choices = classifications_list$classifiers, selected = NULL)
    # updateSelectizeInput(session, 'keyword_search', choices = keywords_df$keyword, selected = NULL)
    updateTextInput(session, "term_query", value = "")
  })
}

# Run the Shiny app
shinyApp(ui, server)


