# Road-Accident-Mortality-Dashboard
Our project aims to visualize and analyze the data related to road accident mortality in various Indian states using R programming language. 
####Library
 
library(shiny)
library(tidyverse)
library(shinydashboard)
library(plotly)
library(DT)

# Set working directory
work_dir_path <- 'D:/IIFT ACADEMIC/3rd trimester/RTSM'
setwd(work_dir_path)

# Load and prepare data (replace "Data.csv" with your actual file name)
data_file <- file.path(work_dir_path, "Data.csv")
data <- read.csv(data_file)

######
 
######

data <- data %>%
  pivot_longer(cols = c("x2018", "x2019", "x2020", "x2021"),
               names_to = "Year",
               values_to = "Deaths") %>%
  mutate(Year = as.integer(str_sub(Year, start = 2))) # Update to extract year from column names

ui <- dashboardPage(
  skin = "yellow",  # Set the skin to yellow
  dashboardHeader(
    title = "Analysis of Deaths in Indian States/UTs (2018-2021) Due to Road Accident", 
    titleWidth = 750, 
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://www.iift.ac.in/iift/index.php",
        tags$img(src = "https://upload.wikimedia.org/wikipedia/en/a/a5/Indian_Institute_of_Foreign_Trade_logo.png", height = "30px")
      )
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://www.linkedin.com/in/onlyshailendrasinghverma/",
        tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/a/aa/LinkedIn_2021.svg/1920px-LinkedIn_2021.svg.png", height = "24px")
      )
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://github.com/Shailendra-IIFTDELHI", 
        tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c2/GitHub_Invertocat_Logo.svg/1024px-GitHub_Invertocat_Logo.svg.png", height = "24px")
      )
    )
  ),
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("Dataset", tabName = "data", icon = icon("database"))
                
    ),
    selectInput("select_year", "Select Year", choices = unique(data$Year), selected = 2021),
    
    selectInput("select_state", "Select State/UTs", choices = unique(data$States.UTs))
    
  ),
  dashboardBody(
    tabItems(
      ## First tab item
      tabItem(tabName = "data", 
              tabBox(id="t1", width = 12,
                     tabPanel("About", 
                              fluidRow(
                                column(width = 8,
                                       img(src = "https://i.ibb.co/5MMy3br/This-collage-encapsulates-the-grim-reality-of-road-accidents-through-a-collection-of-images-From-hea.png", height = "600px")
                                ),
                                column(width = 4,
                                       uiOutput("description_paragraphs")
                                )
                              )
                     ) ,
                     tabPanel("Data", 
                              fluidRow(
                                column(width = 4,
                                       # Display total deaths in a value box
                                       valueBoxOutput("total_deaths_box", width = "50%")
                                ),
                                column(width = 4,
                                       # Display most affected state in a value box
                                       valueBoxOutput("most_affected_state_box", width = "50%")
                                ),
                                column(width = 4,
                                       # Display least affected state in a value box
                                       valueBoxOutput("least_affected_state_box", width = "50%")
                                )
                              ),
                              fluidRow(
                                column(width = 10,
                                       # Display the bar graph
                                       plotlyOutput("top_states_bar_plot", width = "110%")
                                ),
                                column(width = 10,
                                       # Display the bar graph
                                       plotlyOutput("state_deaths_bar_plot", width = "75%")
                                )
                              )
                     ),
                     tabPanel("Resource", 
                              dataTableOutput("resource_table")
                     ),
                     tabPanel("Test",
                              # Display test content here
                              h3("ANOVA and Pairwise t-test Results"),
                              print("-------------ANOVA-----------------------------------"),
                              htmlOutput("anova_results"),
                              print("-------------------------------------------------"),
                              htmlOutput("p_value"),
                              print("-------------------------------------------------"),
                              htmlOutput("t_test_results")
                              
                     )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values
  selected_year <- reactiveVal(2021)
  selected_state <- reactiveVal(NULL)
  
  # Total deaths reactive value
  total_deaths <- reactive({
    req(data) # Wait until data is available
    sum(data[data$Year == selected_year(), "Deaths"])
  })
  
  # Most affected state reactive value
  most_affected_state <- reactive({
    req(data) # Wait until data is available
    most_affected <- data %>%
      filter(Year == selected_year()) %>%
      top_n(1, Deaths) %>%
      pull(States.UTs)
    return(most_affected)
  })
  
  # Least affected state reactive value
  least_affected_state <- reactive({
    req(data) # Wait until data is available
    least_affected <- data %>%
      filter(Year == selected_year()) %>%
      top_n(-1, Deaths) %>%
      pull(States.UTs)
    return(least_affected)
  })
  
  # Data for the bar graph
  bar_data <- reactive({
    req(data) # Wait until data is available
    data %>%
      filter(Year == selected_year()) %>%
      group_by(States.UTs) %>%
      summarize(TotalDeaths = sum(Deaths)) %>%
      arrange(desc(TotalDeaths)) %>%
      head(10)
  })
  
  # Data for the bar graph
  state_deaths_data <- reactive({
    req(data, selected_state()) # Wait until data and selected state are available
    data %>%
      filter(States.UTs == selected_state() & Year %in% c(2018, 2019, 2020, 2021))
  })
  
  # Render resource table
  output$resource_table <- renderDataTable({
    datatable(data)
  })
  
  # Total deaths value box rendering
  output$total_deaths_box <- renderValueBox({
    valueBox(
      total_deaths(),
      paste("Total Deaths in", selected_year()),
      icon = icon("heart"),
      color = "purple"
    )
  })
  
  # Most affected state value box rendering
  output$most_affected_state_box <- renderValueBox({
    most_affected_state_val <- reactive({
      req(data) # Wait until data is available
      most_affected <- data %>%
        filter(Year == input$select_year) %>%
        top_n(1, Deaths) %>%
        pull(States.UTs)
      return(most_affected)
    })
    
    valueBox(
      most_affected_state_val(),
      paste("Most Affected State/UTs in", selected_year()),
      icon = icon("chart-line"),
      color = "red"
    )
  })
  
  # In the server part:
  
  output$description_paragraphs <- renderText({
    "Our project aims to visualize and analyze the data related to road accident mortality in various Indian states using R programming language. Road accidents are a significant cause of concern in India, leading to numerous fatalities and injuries each year. Through this dashboard, we provide insights into the trends, patterns, and contributing factors associated with road accident deaths across different states.

Features:\n 
Interactive Dashboard: The dashboard offers an interactive interface that allows users to explore the data dynamically. Users can select specific states, time periods, and other relevant parameters to tailor the visualization according to their preferences.

State-wise Analysis: We provide comprehensive analysis and visualization of road accident mortality data for each Indian state. Users can compare the number of fatalities, injury severity, and other relevant metrics across different states.

Temporal Trends: The dashboard showcases temporal trends in road accident deaths, enabling users to identify seasonal variations, trends over the years, and potential factors influencing mortality rates."
  })
  
  
  # Least affected state value box rendering
  output$least_affected_state_box <- renderValueBox({
    least_affected_state_val <- req({
      data %>%
        filter(Year == input$select_year) %>%
        top_n(-1
              
              , Deaths) %>%
        pull(States.UTs)
    })
    
    valueBox(
      least_affected_state_val[1],  # Selecting the first state as the least affected
      paste("Least Affected State/UTs in", selected_year()),
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  # Bar graph rendering
  output$state_deaths_bar_plot <- renderPlotly({
    req(state_deaths_data()) # Wait until data is available
    
    ggplot(state_deaths_data(), aes(x = Year, y = Deaths)) +
      geom_bar(stat = "identity", fill = "blue") +
      labs(title = paste("Deaths in", selected_state(), "for Years 2018-2021"),
           x = "Year" , y = "Number of Deaths") +
      theme_minimal()
    
  })
  
  # Bar graph rendering
  output$top_states_bar_plot <- renderPlotly({
    req(bar_data()) # Wait until data is available
    
    ggplot(bar_data(), aes(x = reorder(States.UTs, -TotalDeaths), y = TotalDeaths)) +
      geom_bar(stat = "identity", fill = "violet") +
      labs(title = paste("Top 10 States with Most Deaths in", selected_year()),
           x = "State/UT", y = "Number of Deaths") +
      theme_minimal() +
      theme(axis.text.x = element_text(hjust = 10))
  })
  #
  # Perform ANOVA from year 2018 to 2021
  output$anova_results <- renderPrint({
    req(data)
    anova_result <- aov(Deaths ~ Year, data=data %>% filter(Year %in% c(2018, 2019,2020,2021)))
     summary(anova_result)
  })
  
  # Calculate p-value
  p_value <- reactive({
    summary(anova_output())$`Pr(>F)`[1]
     
  })
  
  
  # Perform t-test for 2018-2019 and 2020-2021
  output$t_test_results <- renderPrint({
    req(data)
    t_test_result1 <- t.test(Deaths ~ Year, data = data %>% filter(Year %in% c(2018, 2019)))
    t_test_result2 <- t.test(Deaths ~ Year, data = data %>% filter(Year %in% c(2020, 2021)))
     
    cat("T-test results for 2018-2019:\n")
    print(t_test_result1)
    print("---------------")
    cat("\nT-test results for 2020-2021:\n")
    print(t_test_result2)
  })
  
  # Update reactive values based on user input
  observe({
    selected_year(input$select_year)
  })
  
  # Corrected observeEvent block
  observeEvent(input$select_state, {
    selected_state(input$select_state)
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
 



