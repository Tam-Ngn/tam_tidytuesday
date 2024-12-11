#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(quarto)
library(readr)

# Render the QMD file and make all variables accessible
data <- read_csv('so_survey_data.csv')

country_names <- data %>% distinct(country) %>%  pull()

ed_level_labels <- data %>% distinct(ed_level_label) %>%  pull()

age_labels <- data %>% distinct(age_label) %>%  pull()

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("StackOverflow Survey Data Analysis"),
  tabsetPanel(
    tabPanel("Demographics",
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 selectInput("country", "Choose a country:", country_names)
               ),
               mainPanel(
            
                  plotOutput("scatterPlot1"),
                  plotOutput("scatterPlot2"),
                  plotOutput("scatterPlot3")

               )
             )
    ),
    tabPanel("AI Prompts",
             sidebarLayout(
               sidebarPanel(
                 selectInput("country", "Choose a country:", country_names),
                 selectInput("age", "Choose age group:", age_labels),
                 selectInput("education", "Choose education level:", ed_level_labels)
               ),
               mainPanel(
                 
                 plotOutput("aiPlot1"),
                 plotOutput("aiPlot2"),
                 plotOutput("aiPlot3"),
                 plotOutput("aiPlot4"),
                 plotOutput("aiPlot5")
                 
               )
             )),
    tabPanel("About")
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$scatterPlot1 <- renderPlot({
      data %>% 
        filter(country == input$country) %>% 
        count(main_branch_label) %>% 
        ggplot(aes(x = fct_rev(fct_reorder(main_branch_label, n)), y = n ))+
        geom_col()+
        theme_minimal()+
        theme(
          axis.text.x = element_text(angle = 20, hjust = 1, size = 5),
          panel.grid.major.x = element_blank()
        )+
        labs( y = "",
              x= "",
              title = "Distribution of Respondants by Occupation "
              )
        
    })
    output$scatterPlot2 <- renderPlot({
      data %>% 
        filter(country == input$country) %>% 
        count(age_label) %>% 
        ggplot(aes(x = fct_rev(fct_reorder(age_label, n)), y = n ))+
        geom_col()+
        theme_minimal()+
        theme(
          axis.text.x = element_text(angle = 20, hjust = 1, size = 5),
          panel.grid.major.x = element_blank()
        )+
        labs( y = "",
              x= "",
              title = "Distribution of Respondants by Age "
        )
      
    })
    output$scatterPlot3 <- renderPlot({
      data %>% 
        filter(country == input$country) %>% 
        count(ed_level_label) %>% 
        ggplot(aes(x = fct_rev(fct_reorder(ed_level_label, n)), y = n ))+
        geom_col()+
        theme_minimal()+
        theme(
          axis.text.x = element_text(angle = 20, hjust = 1, size = 5),
          panel.grid.major.x = element_blank()
        )+
        labs( y = "",
              x= "",
              title = "Distribution of Respondants by Education Level "
        )
      
    })
    
    output$aiPlot1 <- renderPlot({
      data %>% 
        filter(country == input$country, age_label == input$age, ed_level_label == input$education) %>% 
        count(ai_select_label) %>% 
        ggplot(aes(x = fct_rev(fct_reorder(ai_select_label, n)), y = n ))+
        geom_col()+
        theme_minimal()+
        theme(
          axis.text.x = element_text(angle = 20, hjust = 1, size = 10),
          panel.grid.major.x = element_blank()
        )+
        labs( y = "",
              x= "",
              title = "Distribution of Respondants Using AI in Development Process"
        )
      
    })
    
    output$aiPlot2 <- renderPlot({
      data %>% 
        filter(country == input$country, age_label == input$age, ed_level_label == input$education) %>% 
        count(ai_sent_label) %>% 
        ggplot(aes(x = fct_rev(fct_reorder(ai_sent_label, n)), y = n ))+
        geom_col()+
        theme_minimal()+
        theme(
          axis.text.x = element_text(angle = 20, hjust = 1, size = 10),
          panel.grid.major.x = element_blank()
        )+
        labs( y = "",
              x= "",
              title = "Distribution of Respondants Who Approve AI Usage in Development Process"
        )
      
    })
    output$aiPlot3 <- renderPlot({
      data %>% 
        filter(country == input$country, age_label == input$age, ed_level_label == input$education) %>% 
        count(ai_acc_label) %>% 
        ggplot(aes(x = fct_rev(fct_reorder(ai_acc_label, n)), y = n ))+
        geom_col()+
        theme_minimal()+
        theme(
          axis.text.x = element_text(angle = 20, hjust = 1, size = 10),
          panel.grid.major.x = element_blank()
        )+
        labs( y = "",
              x= "",
              title = "Distribution of Respondants Who Trust AI in Development Process"
        )
      
    })
    output$aiPlot4 <- renderPlot({
      data %>% 
        filter(country == input$country, age_label == input$age, ed_level_label == input$education) %>% 
        count(ai_complex_label) %>% 
        ggplot(aes(x = fct_rev(fct_reorder(ai_complex_label, n)), y = n ))+
        geom_col()+
        theme_minimal()+
        theme(
          axis.text.x = element_text(angle = 20, hjust = 1, size = 10),
          panel.grid.major.x = element_blank()
        )+
        labs( y = "",
              x= "",
              title = "Distribution of Respondants Rating AI Performance on Complex Tasks "
        )
      
    })

    output$aiPlot5 <- renderPlot({
      data %>% 
        filter(country == input$country, age_label == input$age, ed_level_label == input$education) %>% 
        count(ai_threat_label) %>% 
        ggplot(aes(x = fct_rev(fct_reorder(ai_threat_label, n)), y = n ))+
        geom_col()+
        theme_minimal()+
        theme(
          axis.text.x = element_text(angle = 20, hjust = 1, size = 10),
          panel.grid.major.x = element_blank()
        )+
        labs( y = "",
              x= "",
              title = "Distribution of Respondants Who Believe AI is a Threat to Current Job "
        )
      
    })
}

# Run the application
shinyApp(ui = ui, server = server)
