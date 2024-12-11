# Install necessary package if not already installed
# install.packages("shiny")

library(shiny)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(widyr)



fiscal_sponsor_directory <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-03-12/fiscal_sponsor_directory.csv')


tokenized_words <- fiscal_sponsor_directory %>% 
  select(services, project_types) %>% 
  mutate(combined_text = str_c(project_types,services, sep = " " ), 
         doc = row_number()) %>% 
  unnest_tokens(word_s, services) %>% 
  unnest_tokens(word_p, project_types) %>% 
  filter(!word_s %in% stop_words$word, !word_p %in% stop_words$word) %>% 
  count(word_p, word_s, sort = TRUE) 

main_services <- c("tax","accounting", "auditing" ,"marketing", "filmmakers", "education", "insurance", "bookkeeping", "resources", "nonprofit", "clinics", "stock", "fundraising")

# Define UI
ui <- fluidPage(
  titlePanel("Please select a service type:"),
  
  sidebarLayout(
    sidebarPanel(
      # Text box for user input
      textInput("text_input", "Enter text:", value = ""),
      
      # Dropdown menu that will be updated dynamically
      selectInput("dropdown", "Choose an option:", choices = NULL)
    ),
    
    mainPanel(
      textOutput("selected_option"),
      tableOutput("rec_key_words")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  
  # Update the dropdown choices dynamically based on user input
  observe({
    # Filter options based on the text entered
    filtered_options <- main_services[grep(input$text_input, main_services, ignore.case = TRUE)]
    
    # Update selectInput choices based on filtered options
    updateSelectInput(session, "dropdown", choices = filtered_options)
  })
  
  # Display selected option from the dropdown
  output$selected_option <- renderText({
    paste("If you want to increase your chance of getting sponsored from a company of service ", input$dropdown , ", please propose a project related to:")
  })
  
  output$rec_key_words <- renderTable({
    
    tokenized_words %>% 
      filter(word_s == input$dropdown) %>% 
      slice_max(n, n = 20) %>% 
      select(-n)
  
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
