# ==========================================
# 1. LOADING PACKAGES
# ==========================================
library(shiny)
library(shinydashboard)
library(tidyverse)
library(janitor)
library(plotly)
library(DT)

# ==========================================
# 2. IMPORTING DATA
# ==========================================
# Assuming 'Questionaire.csv' is in the same directory as this script
raw_data <- read.csv("Questionaire.csv", stringsAsFactors = FALSE)

# ==========================================
# 3. CLEANING & PRE-PROCESSING
# ==========================================
clean_data <- raw_data %>%
  # Clean column names (handles "Age     " and "Type  of Collision" )
  janitor::clean_names() %>%
  # 4. REMOVING UNNECESSARY/ETHICAL COLUMNS
  # Removes names, contact numbers, and addresses for privacy 
  select(-any_of(c("name", "contact_number", "address", "timestamp", "hospital_registration_number"))) %>%
  # Standardization: Fix Excel-corrupted GCS scores (e.g., 15-Oct to 10-15) 
  mutate(gcs_score = case_when(
    gcs_score == "15-Oct" ~ "10-15",
    gcs_score == "5-Jan" ~ "3-8",
    gcs_score == "10-May" ~ "9-12",
    TRUE ~ gcs_score
  ))

# ==========================================
# 4. SHINY UI
# ==========================================
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Maxillofacial Trauma Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analysis Dashboard", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table"))
    ),
    hr(),
    h4(" Interactive Filters", style = "padding-left: 15px;"),
    # 5. FILTERING INPUTS
    selectInput("gender_filter", "Gender Selection:", choices = c("All", "Male", "Female")),
    selectInput("area_filter", "Accident Area:", choices = c("All", unique(clean_data$area_of_accident))),
    checkboxGroupInput("age_filter", "Age Groups:", 
                       choices = unique(clean_data$age), 
                       selected = unique(clean_data$age))
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              # Top Row: Comparative Value Boxes
              fluidRow(
                valueBoxOutput("total_cases", width = 4),
                valueBoxOutput("helmet_usage", width = 4),
                valueBoxOutput("avg_collision", width = 4)
              ),
              
              # Second Row: Visualizations
              fluidRow(
                box(title = "Injuries by Facial Compartment", status = "primary", solidHeader = TRUE,
                    plotlyOutput("face_plot"), width = 7),
                box(title = "Type of Collision Share", status = "warning", solidHeader = TRUE,
                    plotlyOutput("collision_pie"), width = 5)
              ),
              
              # Third Row: Detailed Analysis
              fluidRow(
                box(title = "Comparative Analysis: Helmet Use vs. Tissue Injury", status = "info", solidHeader = TRUE,
                    plotlyOutput("helmet_vs_tissue"), width = 12)
              )
      ),
      
      tabItem(tabName = "data",
              DTOutput("raw_data_table")
      )
    )
  )
)

# ==========================================
# 5. SHINY SERVER
# ==========================================
server <- function(input, output) {
  
  # Reactive data: Changes based on user interaction
  filtered_df <- reactive({
    df <- clean_data
    
    if (input$gender_filter != "All") {
      df <- df %>% filter(gender == input$gender_filter)
    }
    
    if (input$area_filter != "All") {
      df <- df %>% filter(area_of_accident == input$area_filter)
    }
    
    df <- df %>% filter(age %in% input$age_filter)
    return(df)
  })
  
  # 6. GROUPING & SUMMARISING (Value Boxes)
  output$total_cases <- renderValueBox({
    valueBox(nrow(filtered_df()), "Total Cases Filtered", icon = icon("user-md"), color = "purple")
  })
  
  output$helmet_usage <- renderValueBox({
    pct <- round(mean(filtered_df()$use_of_helmet == "Yes", na.rm = TRUE) * 100, 1)
    valueBox(paste0(pct, "%"), "Helmet Usage Rate", icon = icon("helmet-safety"), color = "green")
  })
  
  output$avg_collision <- renderValueBox({
    top_col <- filtered_df() %>% count(type_of_collision) %>% slice_max(n, n = 1) %>% pull(type_of_collision)
    valueBox(top_col[1], "Primary Collision Type", icon = icon("car-burst"), color = "red")
  })
  
  # 7. PIVOTING (LONG FORMAT) & VISUALISATIONS
  output$face_plot <- renderPlotly({
    # Pivoting multi-value columns (Area of Face) to long format for accurate counting 
    long_face <- filtered_df() %>%
      separate_rows(area_of_face_involved, sep = ";") %>%
      count(area_of_face_involved) %>%
      arrange(desc(n))
    
    p <- ggplot(long_face, aes(x = reorder(area_of_face_involved, n), y = n, fill = area_of_face_involved)) +
      geom_col() + coord_flip() + theme_minimal() + 
      labs(x = "Face Area", y = "Case Count") + guides(fill = "none")
    
    ggplotly(p)
  })
  
  output$collision_pie <- renderPlotly({
    data <- filtered_df() %>% count(type_of_collision)
    plot_ly(data, labels = ~type_of_collision, values = ~n, type = 'pie')
  })
  
  output$helmet_vs_tissue <- renderPlotly({
    p <- ggplot(filtered_df(), aes(x = use_of_helmet, fill = type_of_tissue_injury)) +
      geom_bar(position = "dodge") +
      theme_minimal() +
      labs(title = "Helmet Use Impact on Tissue Injury Type", x = "Helmet Used", y = "Number of Patients")
    
    ggplotly(p)
  })
  
  output$raw_data_table <- renderDT({
    datatable(filtered_df(), options = list(scrollX = TRUE))
  })
}

# Run the App
shinyApp(ui = ui, server = server)