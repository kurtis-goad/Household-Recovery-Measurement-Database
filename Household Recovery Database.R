
# Install necessary packages
install.packages(c("shiny", "dplyr", "readr", "DT", "stringr"))

# Load required libraries
library(shiny)
library(dplyr)
library(readr)
library(DT)
library(stringr)

# 1. Load the Data
# Ensure "Household Recovery Data Set.csv" is in your working directory
df <- read_csv("Household Recovery Data Set.csv", show_col_types = FALSE) %>%
  select(-starts_with("...")) %>%
  mutate(
    # Extract the first number found in the "Sample Size" column
    numeric_size = as.numeric(str_extract(`Sample Size`, "\\d+")),
    
    # Create categorical ranges for Sample Size
    `Sample Size Range` = case_when(
      is.na(numeric_size) ~ "No Sample",
      numeric_size >= 1 & numeric_size <= 100 ~ "1 - 100",
      numeric_size >= 101 & numeric_size <= 500 ~ "101 - 500",
      numeric_size >= 501 & numeric_size <= 1000 ~ "501 - 1,000",
      numeric_size > 1000 ~ "1,000+",
      TRUE ~ "Other"
    )
  ) %>%
  select(-numeric_size) # Remove the temporary numeric column

# HELPER FUNCTION: Splits comma-separated cells into individual dropdown options
get_split_choices <- function(column_data) {
  # Remove NAs and convert to character
  char_data <- as.character(na.omit(column_data))
  # Split strings by comma
  split_list <- unlist(strsplit(char_data, ","))
  # Remove leading/trailing white spaces
  trimmed_data <- trimws(split_list)
  # Return "All" plus the sorted unique values
  return(c("All", sort(unique(trimmed_data))))
}

# 2. Define the User Interface (UI)
ui <- fluidPage(
  
  # App Title
  titlePanel("Household Recovery Database"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filter Data"),
      p("Use the dropdown menus below to filter the database."),
      
      # Dropdown for Sample Size Range
      selectInput("size_filter", 
                  "Sample Size:", 
                  choices = c("All", "1 - 100", "101 - 500", "501 - 1,000", "1,000+", "No Sample"), 
                  selected = "All"),
      
      # Dropdown for Sampling Characteristics
      selectInput("sampling_characteristics_filter", 
                  "Sampling Characteristics:", 
                  choices = get_split_choices(df$`Sampling Characteristics`), 
                  selected = "All"),
      
      # Dropdown for Disaster
      selectInput("disaster_filter", 
                  "Disaster:", 
                  choices = get_split_choices(df$`Disaster`), 
                  selected = "All"),
      
      # Dropdown for Built Environment
      selectInput("env_filter", 
                  "Built Environment:", 
                  choices = get_split_choices(df$`Built Environment`), 
                  selected = "All"),
      
      # Dropdown for Timeframe
      selectInput("time_filter", 
                  "Timeframe:", 
                  choices = get_split_choices(df$Timeframe), 
                  selected = "All"),
     
       # Dropdown for Recovery Focus
      selectInput("recovery_focus_filter", 
                  "Recovery Focus:", 
                  choices = get_split_choices(df$`Recovery Focus`), 
                  selected = "All"),
      
      # Dropdown for Data Method
      selectInput("data_method_filter", 
                  "Data Method:", 
                  choices = get_split_choices(df$`Data Method`), 
                  selected = "All"),
      
      # Dropdown for Instrument Included
      selectInput("ins_filter", 
                  "Instrument Included:", 
                  choices = get_split_choices(df$`Instrument Included`), 
                  selected = "All"),
      
      # Dropdown for Validation
      selectInput("validation_filter", 
                  "Validation:", 
                  choices = get_split_choices(df$Validation), 
                  selected = "All"),
      
      hr(),
      helpText("Note: Use the search box in the table to search by keywords in the data set.")
    ),
    
    # Main panel to display the datatable
    mainPanel(
      width = 9,
      DTOutput("recovery_table")
    )
  )
)

# 3. Define the Server Logic
server <- function(input, output, session) {
  
  # Reactive expression to filter the data based on dropdown inputs
  filtered_data <- reactive({
    
    data <- df
    
    # Apply Sample Size (using fixed text matching)
    if (input$size_filter != "All") {
      data <- data %>% filter(`Sample Size Range` == input$size_filter)
    }
    
    # Apply Sampling Characteristics (using fixed text matching)
    if (input$sampling_characteristics_filter != "All") {
      data <- data %>% 
        filter(grepl(tolower(input$sampling_characteristics_filter), tolower(`Sampling Characteristics`), fixed = TRUE))
    }
    
    # Apply Disaster filter (using fixed text matching)
    if (input$disaster_filter != "All") {
      data <- data %>% 
        filter(grepl(tolower(input$disaster_filter), tolower(Disaster), fixed = TRUE))
    }
    
    # Apply Data Method filter (using fixed text matching) (NEW)
    if (input$data_method_filter != "All") {
      data <- data %>% 
        filter(grepl(tolower(input$data_method_filter), tolower(`Data Method`), fixed = TRUE))
    }
    
    # Apply Built Environment filter
    if (input$env_filter != "All") {
      data <- data %>% filter(`Built Environment` == input$env_filter)
    }
    
    # Apply Timeframe filter
    if (input$time_filter != "All") {
      data <- data %>% filter(Timeframe == input$time_filter)
    }
    
    # Apply Recovery Focus filter
    if (input$recovery_focus_filter != "All") {
      data <- data %>% filter(Timeframe == input$recovery_focus_filter)
    }
    
    # Apply Validation filter (using fixed text matching)
    if (input$validation_filter != "All") {
      data <- data %>% 
        filter(grepl(tolower(input$validation_filter), tolower(Validation), fixed = TRUE))
    }
    
    return(data)
  })
  
  # Render the filtered DataTable
  output$recovery_table <- renderDT({
    
    display_data <- filtered_data() %>%
      select(-`Sample Size Range`)
    
    datatable(filtered_data(),
              options = list(
                pageLength = 50,       # Number of rows per page
                autoWidth = TRUE,      # Auto-adjust column widths
                scrollX = TRUE,        # Allow horizontal scrolling for wide tables
                searchHighlight = TRUE # Highlight search terms
              ),
              rownames = FALSE,        # Hide row names/numbers
              class = "display"        # Makes table look clean
    ) %>%
      formatStyle(
        columns = names(filtered_data()), # Apply to all columns
        `white-space` = "normal"          # Force text wrapping
      )
  })
}

# 4. Run the Application 
shinyApp(ui = ui, server = server)
