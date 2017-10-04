library(shiny)
library(shinythemes)
library(markdown)
library(DT)
#setwd("C:/Users/snowf/Desktop/MGMT using R/epicurious-recipes-with-rating-and-nutrition")

# Define UI for app ----
# Add navigation bar
ui <- navbarPage (theme= shinytheme('united'),"Meal Mate",
  
  #tab1 layout
  tabPanel ( "Protein Optimizer",
    tags$head(
  
      #Set color for warning message: 
      tags$style(HTML("
        .shiny-output-error-validation {
        color: red;
        }
        "))
    ),
             
    
    sidebarLayout(
      sidebarPanel(
        # Input: number of meals 
        numericInput(inputId = "num_meal", label = "How many meals are you preparing?", 3, min = 2, max = 7),
        
        # Input: calories limit for each meal
        sliderInput(inputId = "cal_input", label = "Calories Limit per Meal", min = 0, max = 1500, value = 800),
        
        # Input: protein options
        checkboxGroupInput(inputId = "myProtein", label = "Select 2 Proteins", c('beef', 'chicken', 'shrimp', 'turkey', 'salmon', 'tofu')),
        
        # Input: vegetable options
        checkboxGroupInput(inputId = "myVeggie", label = "Select 3 Vegetables", c('potato', 'tomato', 'onion', 'carrot', 'broccoli', 'bell.pepper')),
        
        #actionbutton to execute 
        tags$head(
          tags$style(HTML('#button{background-color:orange}'))
        ),
        actionButton(inputId = 'button', label = 'GO')
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        
        #Add gif for background
        tags$div(img(src = "smallplates-hero-foreverloop.gif", width = "950px", height = "300px")),
        
        #Relevent info for functionality and nutrients fact
        htmlOutput('text'),
        
        #Optimization result
        DT::dataTableOutput('table')
        
      )
    )
  ),
  #tab2 layout
  tabPanel("Short on Time?",
    sidebarLayout(
      sidebarPanel(
        
        # input number of meal 
        numericInput(inputId = "num_meal2", label = "How many meals are you preparing?", 3, min = 2, max = 7),
        
        # input for calories
        sliderInput(inputId = "cal_input2", label = "Calories Limit per Meal", min = 0, max = 1500, value = 800),
        
        # input for protein option
        checkboxGroupInput(inputId = "myProtein2", label = "Select 2 Proteins", c('beef', 'chicken', 'shrimp','pork')),
        
        #set action button format
        tags$head(
          tags$style(HTML('#button2{background-color:orange}'))
        ),
        actionButton(inputId = 'button2', label = 'GO')
      ),
      
      mainPanel(
        #Background picture
        tags$div(img(src = "quick.jpg", width = "900px", height = "300px")),
        
        #Text message--function intro
        htmlOutput('text_0'),
        
        # Optimization result
        DT::dataTableOutput('table_2')
        
      )
    )           
  )
)

# Define server logic for slider examples ----
server <- function(input, output){
  #tab 1 text 
  output$text <- renderUI ({
    str0 <- paste("____________________________________________________________________________________________________________________________________________________ ")
    str1 <- paste("According to the 2010 FDA Dietary Guidelines:")
    str2 <- paste("The recommended calories for a 21-25 year-old moderately active adult is 1400 calories per lunch for Male and 1100 calories per lunch for female.")
    str3 <- paste("- The suggested fat intake is around 35% of total calories")
    str4 <- paste("- The Protein intake should be in the range of 10-35% of total calories")
    str5 <- paste("- The healthy range of sodium intake is 1100-1400 milligram per lunch.")
    str6 <- paste("____________________________________________________________________________________________________________________________________________________ ")
    HTML(paste(str0,str1,str2,str3,str4,str5,str6, sep='<br/>'))
  })  
  
  #tab 2 text
  output$text_0 <- renderUI ({
    str11 <- paste("____________________________________________________________________________________________________________________________________________________ ")
    str7 <- paste("The usual meal preparer in a household spent an average of 51 minutes in meal preparation on an average day.")
    str8 <- paste("STOP EATING FROZEN PIZZA")
    str9 <- paste("These are REAL FOOD that can be prepared in 22 min!!")
    str10 <- paste("____________________________________________________________________________________________________________________________________________________ ")
    HTML(paste(str11, str7, str8, str9, str10, sep = '<br/>'))
  })
  
  # source functions  
  source('receipeOptimizer.R')
  source('receipeOptimizer_2.R')
  source('epilink.R')
  
  #tab1 result render      
  df_result <- eventReactive(
    input$button,{
      validate(
        need(length(input$myProtein) == 2, 'Please select 2 proteins!!'),
        need(length(input$myVeggie) == 3, 'STOP! Vegetables are important, you need to choose 3!!')
        )
      receipeOptimizer(input$myProtein, input$myVeggie, input$num_meal, input$cal_input)
  })
  
  output$table <- DT::renderDataTable({
    
    df_result()
  },escape=FALSE,options = list(lengthChange = FALSE)
  ) 
  
  #tab2 result render
  df_result_2 <- eventReactive(
    input$button2,{
      validate(
        need(length(input$myProtein2)== 2, 'Please select 2 proteins!!')
      )
    receipeOptimizer_2(input$myProtein2, input$num_meal2, input$cal_input2)
  })
  
  
  output$table_2 <- DT::renderDataTable({
      df_result_2()
    },escape=FALSE,options = list(lengthChange = FALSE)
    ) 
  }

# Create Shiny app ----
shinyApp(ui = ui, server = server)





