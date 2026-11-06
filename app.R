
# ONLINE LEARNING PLATFORMS AND STUDENT LEARNING EFFECTIVENESS
# STEP 6: SHINY DASHBOARD

library(shiny)
library(ggplot2)

# Load dataset
students <- read.csv("online_learning_effectiveness.csv")

# USER INTERFACE
ui <- fluidPage(
  
  titlePanel(
    "Online Learning Platforms and Student Learning Effectiveness"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Dashboard Filters"),
      
      selectInput(
        "year",
        "Select Year Level:",
        choices = c("All", sort(unique(students$Year_Level))),
        selected = "All"
      ),
      
      selectInput(
        "platform",
        "Select Online Learning Platform:",
        choices = c("All", sort(unique(students$Online_Platform))),
        selected = "All"
      ),
      
      selectInput(
        "connection",
        "Select Internet Connection:",
        choices = c("All", sort(unique(students$Internet_Connection))),
        selected = "All"
      )
    ),
    
    mainPanel(
      
      h3("Student Learning Effectiveness Dashboard"),
      
      fluidRow(
        
        column(4, wellPanel(
          h4("Total Students"),
          textOutput("total_students")
        )),
        
        column(4, wellPanel(
          h4("Average Learning Hours"),
          textOutput("average_hours")
        )),
        
        column(4, wellPanel(
          h4("Average Effectiveness"),
          textOutput("average_effectiveness")
        ))
      ),
      
      h4("Graph 1: Daily Online Learning Hours"),
      plotOutput("hours_plot"),
      
      h4("Graph 2: Learning Hours and Effectiveness"),
      plotOutput("effectiveness_plot"),
      
      h4("Graph 3: Effectiveness by Platform"),
      plotOutput("platform_plot"),
      
      h4("Student Data Table"),
      tableOutput("student_table")
    )
  )
)

# SERVER
server <- function(input, output) {
  
  # Filter dataset
  filtered_data <- reactive({
    
    data <- students
    
    if (input$year != "All") {
      data <- data[data$Year_Level == input$year, ]
    }
    
    if (input$platform != "All") {
      data <- data[
        data$Online_Platform == input$platform, ]
    }
    
    if (input$connection != "All") {
      data <- data[
        data$Internet_Connection == input$connection, ]
    }
    
    data
  })
  
  # Summary cards
  output$total_students <- renderText({
    nrow(filtered_data())
  })
  
  output$average_hours <- renderText({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return("No matching records")
    }
    
    paste(
      round(mean(data$Daily_Online_Learning_Hours), 2),
      "hours"
    )
  })
  
  output$average_effectiveness <- renderText({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return("No matching records")
    }
    
    round(
      mean(data$Learning_Effectiveness_Score),
      2
    )
  })
  
  # GRAPH 1
  output$hours_plot <- renderPlot({
    
    data <- filtered_data()
    req(nrow(data) > 0)
    
    ggplot(
      data,
      aes(x = Daily_Online_Learning_Hours)
    ) +
      geom_histogram(
        bins = 10,
        fill = "steelblue",
        color = "white"
      ) +
      labs(
        title = "Daily Online Learning Hours",
        x = "Hours Per Day",
        y = "Number of Students"
      ) +
      theme_minimal()
  })
  
  # GRAPH 2
  output$effectiveness_plot <- renderPlot({
    
    data <- filtered_data()
    req(nrow(data) > 0)
    
    ggplot(
      data,
      aes(
        x = Daily_Online_Learning_Hours,
        y = Learning_Effectiveness_Score
      )
    ) +
      geom_point(
        color = "darkgreen",
        size = 3
      ) +
      geom_smooth(
        method = "lm",
        se = FALSE
      ) +
      labs(
        title = "Learning Hours vs Effectiveness",
        x = "Daily Learning Hours",
        y = "Effectiveness Score"
      ) +
      theme_minimal()
  })
  
  # GRAPH 3
  output$platform_plot <- renderPlot({
    
    data <- filtered_data()
    req(nrow(data) > 0)
    
    ggplot(
      data,
      aes(
        x = Online_Platform,
        y = Learning_Effectiveness_Score,
        fill = Online_Platform
      )
    ) +
      geom_boxplot() +
      labs(
        title = "Learning Effectiveness by Platform",
        x = "Online Learning Platform",
        y = "Effectiveness Score"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(
          angle = 20,
          hjust = 1
        )
      )
  })
  
  # STUDENT DATA TABLE
  output$student_table <- renderTable({
    
    filtered_data()
    
  })
  
}

# RUN APPLICATION
shinyApp(ui = ui, server = server)
