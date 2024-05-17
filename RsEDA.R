library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("slate"),
  shinythemes::themeSelector(),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Bir Dosya Seçiniz : ",
                multiple = TRUE,
                accept = c("text/csv", "text/comma-separated-values, text/plain, .csv")),
      checkboxInput("header", "Header", value = TRUE),
      radioButtons("sep", "Separator", 
                   choices = list("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"), 
                   selected = ","),
      selectInput("file2", "Datasets", 
                  choices = c("pressure", "mtcars", "iris")),
      selectInput("color", "Color", 
                  choices = c("Red", "Blue", "Yellow", "Orange")),
      radioButtons("quote", "Quote", 
                   choices = list("None" = "", "Double Quote" = "\"", "Single Quote" = "'"), 
                   selected = "\""),
      uiOutput("select_out"),
      uiOutput("select_out_2"),
      uiOutput("select_numeric_to_cat"),
      uiOutput("select_dummy_func")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data", icon = icon("table"),
                 tabsetPanel(
                   tabPanel("Datasets", tableOutput("data_out")),
                   tabPanel("Structure", verbatimTextOutput("str_out")),
                   tabPanel("Summary", icon = icon("list-alt"), verbatimTextOutput("sum_out"))
                 )),
        tabPanel("Univariate Analysis",
                 tabsetPanel(
                   tabPanel("Histogram", plotOutput("hist_out")),
                   tabPanel("Boxplot", plotOutput("box_out")),
                   tabPanel("Barplot", plotOutput("bar_out"))
                 )),
        tabPanel("Data Preprocessing",
                 tabsetPanel(
                   tabPanel("Determining Variable Type", tableOutput("check_col_type")),
                   tabPanel("Missing Value Analysis", 
                            selectInput("na_method", "Method to Correct Missing Data",
                                        choices = list("Delete" = "remove", 
                                                       "Fill with Mean" = "fill_mean"), 
                                        selected = "remove"),
                            actionButton("fix_na", "Fix Missing Data"),
                            tableOutput("missing_values")),
                   tabPanel("Outlier Analysis", 
                            selectInput("outlier_method", "Method of Correcting Contrary Observations",
                                        choices = list("Suppress" = "suppress", 
                                                       "Fill with Mean" = "fill_mean", 
                                                       "Fill with Median" = "fill_median", 
                                                       "Fill with Predict" = "fill_predict"), 
                                        selected = "suppress"),
                            actionButton("fix_outliers", "Correct Outlier Observations"),
                            tableOutput("outliers")),
                   tabPanel("Encoder"),
                   tabPanel("Conversions", 
                            actionButton("convert_numeric_to_cat", "Convert Numeric to Categorical"),
                            actionButton("detect_cat_cols", "Detect Categorical Variables"),
                            actionButton("detect_num_but_cat", "Detect Numeric But Categorical Variables"),
                            actionButton("convert_dummy_func", "Convert to Dummy Variables"),
                            actionButton("detect_cat_but_car", "Identify Categorical But Cardinal Variables"),
                            tableOutput("transformations_output"))
                 )),
        tabPanel("Bivariate Analysis",
                 tabsetPanel(
                   tabPanel("Scatterplot", 
                            plotOutput("scatter_out")),
                   tabPanel("Correlation", 
                            verbatimTextOutput("cor_out"))
                 ))
      )
    )
  )
)

server <- function(input, output, session) {
  # Helper functions for preprocessing
  numeric_to_cat <- function(data, col) {
    data[[col]] <- as.factor(data[[col]])
    return(data)
  }
  
  cat_cols <- function(data) {
    cat_cols <- sapply(data, function(col) {
      is.factor(col) || is.character(col) || is.logical(col)
    })
    names(data)[cat_cols]
  }
  
  num_but_cat <- function(data) {
    num_but_cat <- sapply(data, function(col) {
      is.numeric(col) && length(unique(col)) < 10
    })
    names(data)[num_but_cat]
  }
  
  dummy_func <- function(data, col) {
    dummy_vars <- model.matrix(~ data[[col]] - 1)
    colnames(dummy_vars) <- gsub("data\\[\\[col\\]\\]", col, colnames(dummy_vars))
    data <- cbind(data, dummy_vars)
    data[[col]] <- NULL
    return(data)
  }
  
  cat_but_car <- function(data) {
    cat_but_car <- sapply(data, function(col) {
      is.factor(col) && length(unique(col)) > 20
    })
    names(data)[cat_but_car]
  }
  
  detect_outliers <- function(data, method = "detect", threshold = 1.5) {
    cleaned_data <- data
    outlier_table <- data.frame()
    
    for (col in names(data)) {
      if (is.numeric(data[[col]])) {
        q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
        q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
        iqr <- q3 - q1
        lower_bound <- q1 - threshold * iqr
        upper_bound <- q3 + threshold * iqr
        
        outliers <- data[[col]] < lower_bound | data[[col]] > upper_bound
        
        if (method == "suppress") {
          cleaned_data[[col]][outliers] <- NA
        } else if (method == "fill_mean") {
          cleaned_data[[col]][outliers] <- mean(data[[col]], na.rm = TRUE)
        } else if (method == "fill_median") {
          cleaned_data[[col]][outliers] <- median(data[[col]], na.rm = TRUE)
        } else if (method == "fill_predict") {
          model <- lm(data[[col]] ~ ., data = data)
          predict_data <- predict(model, newdata = data)
          cleaned_data[[col]][outliers] <- predict_data[outliers]
        }
        
        # Aykýrý gözlemleri tabloya ekleme
        outliers_data <- data.frame(Column = rep(col, sum(outliers)), Value = data[[col]][outliers])
        outlier_table <- rbind(outlier_table, outliers_data)
      }
    }
    
    if (method == "detect") {
      return(outlier_table)
    } else {
      return(cleaned_data)
    }
  }
  
  find_missing_values <- function(data) {
    missing_values <- sapply(data, function(col) sum(is.na(col)))
    missing_values <- missing_values[missing_values > 0]
    data.frame(Sütun = names(missing_values), Eksik_Sayý = missing_values)
  }
  
  handle_missing_values <- function(data, method = "remove") {
    cleaned_data <- data
    
    if (method == "remove") {
      cleaned_data <- na.omit(data)
    } else if (method == "fill_mean") {
      for (col in names(data)) {
        if (is.numeric(data[[col]])) {
          cleaned_data[[col]][is.na(cleaned_data[[col]])] <- mean(data[[col]], na.rm = TRUE)
        }
      }
    }
    
    return(cleaned_data)
  }
  
  rv <- reactiveValues(data = NULL, cleaned_data = NULL, cat_cols = NULL, num_but_cat = NULL, cat_but_car = NULL)
  
  data <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(get(input$file2))
    }
    read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote, encoding = "UTF-8")
  })
  
  observe({
    rv$data <- data()
    rv$cleaned_data <- rv$data
  })
  
  output$select_out <- renderUI({
    selectInput('variable', "Select a Variable", choices = names(rv$cleaned_data))
  })
  
  output$select_out_2 <- renderUI({
    selectInput('variable2', "Select a Second Variable", choices = names(rv$cleaned_data))
  })
  
  output$select_numeric_to_cat <- renderUI({
    selectInput('numeric_to_cat_var', "Select Numeric Variable to Convert to Categorical", choices = names(rv$cleaned_data))
  })
  
  output$select_dummy_func <- renderUI({
    selectInput('dummy_var', "Select Variable to Create Dummy Variables", choices = names(rv$cleaned_data))
  })
  
  output$data_out <- renderTable({
    rv$cleaned_data
  })
  
  output$str_out <- renderPrint({
    str(rv$cleaned_data)
  })
  
  output$sum_out <- renderPrint({
    summary(rv$cleaned_data)
  })
  
  output$hist_out <- renderPlot({
    req(input$variable)
    hist(rv$cleaned_data[[input$variable]], main = paste("Histogram of", input$variable), xlab = input$variable, col = input$color)
  })
  
  output$box_out <- renderPlot({
    req(input$variable)
    boxplot(rv$cleaned_data[[input$variable]], main = paste("Boxplot of", input$variable), ylab = input$variable, col = input$color)
  })
  
  output$bar_out <- renderPlot({
    req(input$variable)
    barplot(table(rv$cleaned_data[[input$variable]]), main = paste("Barplot of", input$variable), xlab = input$variable, ylab = "Frequency", col = input$color)
  })
  
  output$check_col_type <- renderTable({
    if (!is.null(rv$cleaned_data)) {
      df <- rv$cleaned_data
      column_types <- sapply(df, function(col) {
        if (is.factor(col) || is.character(col)) {
          "Categorical"
        } else if (is.numeric(col)) {
          "Numeric"
        } else {
          "Unknown"
        }
      })
      data.frame(Sütun = names(df), Tür = column_types)
    }
  })
  
  observeEvent(input$fix_outliers, {
    if (!is.null(rv$cleaned_data)) {
      rv$cleaned_data <- detect_outliers(rv$cleaned_data, method = input$outlier_method)
    }
  })
  
  output$outliers <- renderTable({
    if (!is.null(rv$cleaned_data)) {
      detect_outliers(rv$cleaned_data, method = "detect")
    }
  })
  
  observeEvent(input$fix_na, {
    if (!is.null(rv$cleaned_data)) {
      rv$cleaned_data <- handle_missing_values(rv$cleaned_data, method = input$na_method)
    }
  })
  
  output$missing_values <- renderTable({
    if (!is.null(rv$cleaned_data)) {
      find_missing_values(rv$cleaned_data)
    }
  })
  
  output$scatter_out <- renderPlot({
    req(input$variable, input$variable2)
    plot(rv$cleaned_data[[input$variable]], rv$cleaned_data[[input$variable2]], 
         main = paste("Scatterplot of", input$variable, "and", input$variable2),
         xlab = input$variable, ylab = input$variable2, col = input$color)
  })
  
  output$cor_out <- renderPrint({
    req(input$variable, input$variable2)
    cor_value <- cor(rv$cleaned_data[[input$variable]], rv$cleaned_data[[input$variable2]], use = "complete.obs")
    paste("Correlation between", input$variable, "and", input$variable2, "is:", cor_value)
  })
  
  observeEvent(input$convert_numeric_to_cat, {
    req(input$numeric_to_cat_var)
    rv$cleaned_data <- numeric_to_cat(rv$cleaned_data, input$numeric_to_cat_var)
  })
  
  observeEvent(input$detect_cat_cols, {
    rv$cat_cols <- cat_cols(rv$cleaned_data)
  })
  
  observeEvent(input$detect_num_but_cat, {
    rv$num_but_cat <- num_but_cat(rv$cleaned_data)
  })
  
  observeEvent(input$convert_dummy_func, {
    req(input$dummy_var)
    rv$cleaned_data <- dummy_func(rv$cleaned_data, input$dummy_var)
  })
  
  observeEvent(input$detect_cat_but_car, {
    rv$cat_but_car <- cat_but_car(rv$cleaned_data)
  })
  
  output$transformations_output <- renderTable({
    list(
      "Categorical Columns" = rv$cat_cols,
      "Numeric but Categorical Columns" = rv$num_but_cat,
      "Categorical but Cardinal Columns" = rv$cat_but_car
    )
  })
}

shinyApp(ui = ui, server = server)