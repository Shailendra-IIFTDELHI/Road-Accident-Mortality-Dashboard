library(shiny)
library(tidyverse)
library(shinydashboard)
library(plotly)

# Set working directory
work_dir_path <- 'Data.csv'
setwd(work_dir_path)

# Load and prepare data (replace "Data.csv" with your actual file name)
data_file <- file.path(work_dir_path, "Data.csv")
data <- read.csv(data_file)

data <- data %>%
  pivot_longer(cols = c("x2018", "x2019", "x2020", "x2021"),
               names_to = "Year",
               values_to = "Deaths") %>%
  mutate(Year = as.integer(str_sub(Year, start = 2))) # Update to extract year from column names



# Define UI with Shinydashboard

ui <- dashboardPage(
  skin = "yellow",  # Set the skin to yellow
  dashboardHeader(
    title = div(
      "48A - Shailendra",
      style = "text-align: center;"
    ),
    # Add logo
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://www.iift.ac.in/iift/index.php", # Link to your website o
        tags$img(src = "https://upload.wikimedia.org/wikipedia/en/a/a5/Indian_Institute_of_Foreign_Trade_logo.png", height = "40px")  # Adjust the path and height as needed
      )
    )
  ),
  
  dashboardSidebar(
    # Add a select input for choosing the year
    selectInput("select_year", "Select Year", choices = unique(data$Year), selected = 2021),
    
    selectInput("select_state", "Select State/UTs", choices = unique(data$States.UTs))
  ),
  
  dashboardBody(
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
      column(width = 6,
             # Display the bar graph
             plotlyOutput("top_states_bar_plot", width = "100%")
      ),
      column(width = 6,
             # Display the bar graph
             plotlyOutput("state_deaths_bar_plot", width = "100%")
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
  
  # Least affected state value box rendering
  output$least_affected_state_box <- renderValueBox({
    least_affected_state_val <- req({
      data %>%
        filter(Year == input$select_year) %>%
        top_n(-1, Deaths) %>%
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
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
