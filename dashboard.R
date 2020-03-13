#library(modelgrid)
# Setting the required packages ------------------------------------------------
pkgs <- c("shiny", "shinydashboard", "shinyWidgets",
          "shinycssloaders", "RColorBrewer",
          "plotly", "caret",
          "tidyverse", "RMySQL", "arules", "arulesViz",
          "DT", "knitr", "kableExtra", "doParallel"
)

for(pkg in pkgs){
  if(!(pkg %in% rownames(installed.packages()))){
    install.packages(pkg, dependencies = TRUE)
  }
  lapply(pkg, FUN = function(X) {
    do.call("require", list(X))
  })
}

source("treat_data2.R")

# setup ------------------------------------------------------------------------
options(digits = 2, scipen=999, warn=-1)
colors <- brewer.pal(12, "Paired")

# Elago vs. Belkin data ---------------------------------------------------------
data_base1 <- read.csv2("test_data.csv", na.strings = c("", " "))

data_base1$elevel <- as.factor(data_base1$elevel)
levels(data_base1$elevel) <- c("High School Degree", "Professional Diploma", "College Degree", "Master's Degree")
data_base1$elevel <- reorder(data_base1$elevel, data_base1$elevel, FUN = function(x) -length(x))

data_base1$car <- as.factor(data_base1$car)
levels(data_base1$car) <- c("BMW", "Tesla", "Volkswagen", "Fiat", "Chrysler", "Citroen", "Ford", "Honda", "Opel", 
                            "Citroen2", "Porsche", "Peugeot", "Audi", "Mercedes-Benz", "Kia", "Nissan", "Hyundai", 
                            "Renault", "Toyota", "Dacia")
data_base1$car <- reorder(data_base1$car, data_base1$car, FUN = function(x) -length(x))

data_base1$zipcode <- as.factor(data_base1$zipcode)
levels(data_base1$zipcode) <- c("Center", "East-Center", "East", "Northeast", "North", "Northwest", "Southeast", 
                                "Southwest", "West")
data_base1$zipcode <- reorder(data_base1$zipcode, data_base1$zipcode, FUN = function(x) -length(x))

# convert NA to 'No Response' in the brand variable
levels(data_base1$brand) <- c(levels(data_base1$brand), "No Response")
data_base1$brand[is.na(data_base1$brand)] <- "No Response" 
data_base1$brand <- factor(data_base1$brand, levels = c("No Response", "Belkin", "Elago"))

# Recommendation system data ----------------------------------------------------
db_password <- 'Danielly1964'
### Read data from mySQL
mydb <-  dbConnect(MySQL(), user = 'root', password = db_password,
                   dbname = 'imarket_sql', host = '127.0.0.1', port = 3306)
rm(db_password)

#### import line_item table
rs <- dbSendQuery(mydb, "select * from line_item")
line_item <-  fetch(rs, n = -1)
dbClearResult(rs)

#### import orders table
rs <- dbSendQuery(mydb, "select * from orders")
orders <-  fetch(rs, n = -1)
dbClearResult(rs)

#### import products table
rs <- dbSendQuery(mydb, "select * from products")
products <-  fetch(rs, n = -1)
dbClearResult(rs)
on.exit(dbDisconnect(mydb))

#### cleaning data
line_item <- line_item %>% mutate(date = ymd_hms(date), unit_price = as.numeric(gsub(",", ".", unit_price)))
orders <- orders %>% mutate(created_date = ymd_hms(created_date), total_paid = as.numeric(gsub(",", ".", total_paid)))
products <- products %>% mutate(price = as.numeric(as.character(price)))
line_item_clean <- line_item %>% inner_join(orders, by = "id_order")

line_item_clean <- line_item_clean %>% filter(state == "Completed") %>% 
  select(-state, -product_id)

line_item_clean <- line_item_clean %>% filter(sku %in% products$sku)

values_paid <- line_item_clean %>% group_by(id_order) %>% 
  summarise(total_estimated = sum(unit_price*product_quantity), total_paid = mean(total_paid))
values_paid <- values_paid %>% filter(abs(total_estimated - total_paid) < total_paid*0.50)

line_item_clean <- line_item_clean %>% filter(id_order %in% values_paid$id_order)
rm(values_paid)

line_item_clean <- line_item_clean %>% inner_join(products, by = "sku")

data_base2 <- treat_data2(line_item_clean)
rm(line_item, orders, products)

# UI sidebar --------------------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Overview", icon = icon("globe"), tabName = "overview"),
    menuItem("Belkin vs. Elago", icon = icon("burn"), startExpanded = TRUE,
      menuSubItem("Data", icon = icon("database"), tabName = "data1"),
      menuSubItem("EDA", icon = icon("chart-bar"), tabName = "eda1"),
      menuSubItem("Supervised Learning", icon = icon("search"), tabName = "ml1")
    ),
    menuItem("Recommendation System", icon = icon("fire"), startExpanded = TRUE,
      menuSubItem("Data", icon = icon("database"), tabName = "data2"),
      menuSubItem("EDA", icon = icon("chart-bar"), tabName = "eda2"),
      menuSubItem("Apriori", icon = icon("shopping-basket"), tabName = "ml2")
    )
  )
)

body <- dashboardBody(
  tabItems(
    # Overview content ----------------------------------------------------------------
    tabItem(
      tabName = "overview",
      fluidRow(
        box(width = 12, status="success", solidHeader = TRUE,
        title = "Belkin vs. Elago", "Our strategy team has decided that we should pursue an 
        exclusive partnership with one of the biggest accessory makers in the market. 
        We’re in talks with two of them: Belkin and Elago.", br(), br(), 
        "The sales team engaged a ", strong("market research"), " firm to conduct a survey 
        of our existing customers. One of the objectives of the survey was to find out 
        which of two brands of accessories our customers prefer. This information will 
        help us decide the manufacturer to partner with. Unfortunately, the answer 
        to the brand preference question was not properly captured for all of the 
        respondents. For that, we investigated if customer responses to some survey 
        questions (e.g. income, age, etc.) enable us to predict the answer to the 
        brand preference question. If we can do this with confidence, I would like 
        you to make those predictions and provide the sales team with a complete view 
        of what brand our customers prefer.", br(), br(),
        "1. ", strong("Exploratory Data Analysis (EDA)"), " from a survey to iMarket customers.", br(),
        "2. Run and optimize two different ", strong("decision tree classification methods"), " - C5.0 and RandomForest - and compare which one works better for this data set."
        )
      ),
      fluidRow(
        box(width = 12, status="success", solidHeader = TRUE,
        title = "Recommendation System", "We want to build a ", strong("recommendation system"), " 
        to increase cross-selling in our website. Our goal is to show customers who have 
        added a product to their basket another product they’re likely to want. Because 
        of the nature of our data, the best way to do that is by using a method called ", 
        strong("Market Basket Analysis"), ".", br(), 
        
        "The first step is the ", strong("data quality"), " assessment: we need to make 
        sure that we can trust the data that we’re using. Make sure the information across 
        our datasets (", em("line_item"), ", ", em("orders"), " and ", em("products"), ") 
        is consistent, and get rid of anything too strange or to believe.", br(),
        
        "Then, we will have to build a ", strong("transactional dataset:"), " a table with 
        one row per order and one column per product bought -meaning that the table has 
        as many columns as the biggest order, with lots of NAs.", br(),
        
        "Finally, we will use the ", strong("apriori algorithm"), " to create if-then rule 
        with our products: “if a customer buys product A, then it’s likely that they’ll 
        buy product B”. Focus on optimizing the model: it will be deployed into production 
        by our engineering team. "
        )
      )
    ),
    
    # Belkin vs. Elago -------------------------------------------------
    tabItem(
      ## Belkin vs. Elago - Data ------------------------------------------------------------
      tabName = "data1",
      fluidRow(
        column(width = 4,
               box(width = NULL,
                   title = "Input", status = "success", solidHeader = TRUE,
                   sliderInput("salary", "Salary input:", min = floor(min(data_base1$salary, na.rm = TRUE)), 
                               max = ceiling(max(data_base1$salary, na.rm = TRUE)), 
                               value = c(quantile(data_base1$salary, probs = .25, na.rm = TRUE), 
                                         quantile(data_base1$salary, probs = .75, na.rm = TRUE)), 
                               round = 1, sep = ""),
                   sliderInput("credit", "Credit input:", min = floor(min(data_base1$credit, na.rm = TRUE)), 
                               max = ceiling(max(data_base1$credit, na.rm = TRUE)), 
                               value = c(quantile(data_base1$credit, probs = .25, na.rm = TRUE),
                                         quantile(data_base1$credit, probs = .75, na.rm = TRUE)), 
                               round = 1, sep = ""),
                   sliderInput("age", "Age input:", min = floor(min(data_base1$age, na.rm = TRUE)), 
                               max = ceiling(max(data_base1$age, na.rm = TRUE)), 
                               value = c(quantile(data_base1$age, probs = .25, na.rm = TRUE), 
                                         quantile(data_base1$age, probs = .75, na.rm = TRUE)), 
                               round = 1, sep = ""),
                   selectInput("elevel", "Educational level input:", choices = unique(data_base1$elevel), multiple = TRUE, selected = c("Professional Diploma", "High School Degree")),
                   selectInput("car", "Car model input:", choices = unique(data_base1$car), multiple = TRUE, selected = c("Tesla", "BMW", "Fiat", "Opel", "Ford", "Citroen", "Honda", "Chrysler", "Volkswagen", "Citroen2")),
                   selectInput("zipcode", "Location input:", choices = unique(data_base1$zipcode), multiple = TRUE, selected = c("Center", "East-Center", "Northeast", "Northwest", "North", "Southeast", "East")),
                   checkboxGroupInput("brand", "Company input:", c("Belkin", "Elago", "No Response"), selected = c("Belkin", "Elago"), inline = TRUE)
               )        
          
        ),
        column(width = 8,
               fluidRow(
                 column(width = 5,
                   valueBox(
                     width = NULL,
                     value = textOutput("companies"), subtitle = "Belkin vs. Elago",
                     icon = icon("archway"), color = "green"
                   )
                 ),
                 column(width = 7,
                   valueBox(
                     width = NULL,
                     value = textOutput("average_salary"), subtitle = "Average salary per brand",
                     icon = icon("dollar-sign"), color = "green"
                   )
                 )
               ), 
               fluidRow(
                 box(width = NULL, 
                     status = "success", solidHeader = TRUE,
                     title = "Salary distribution by brand preference",
                     collapsible = TRUE,
                     plotlyOutput("data1_plot1")
                 ),
                 box(width = NULL, 
                     status = "success", solidHeader = TRUE,
                     title = "Age distribution by brand preference",
                     collapsible = TRUE, collapsed = TRUE,
                     plotlyOutput("data1_plot2")
                 )
               )
        )
      )
    ),
    
    # Belkin vs. Elago - EDA -------------------------------------------------
    tabItem(
      tabName = "eda1",
      fluidRow(
        column(width = 4,
               box(width = 12, status="success", solidHeader = TRUE,
                   selectInput('eda1_x', 'Select the variable on x-axis', names(data_base1), 'salary'),
                   selectInput('eda1_y', 'Select the variable on y-axis', names(data_base1), 'age'),
                   selectInput('eda1_color', 'Select the variable on color', names(data_base1), 'brand')
               ),
               box(status="success", solidHeader = TRUE,
                   selectInput('eda1_geom', "Select the graph type:", c("point", "bar"), selected = "point")
               )
        ), 
        column(width = 8,
               box(width = 12, status="success", solidHeader = TRUE,
                   plotlyOutput("eda1_plot")
               )
        )
      )
    ), 
    
    # Belkin vs. Elago - Supervised Learning -------------------------------------------------
    tabItem(
      tabName = "ml1",
      fluidRow(
        column(width = 4,
               box(width = NULL, 
                   title = "Parameters", status="success", solidHeader = TRUE,
                   selectInput("ml1_preprocess", "Preprocess methods:", c("Center" = "center", "Scale" = "scale", "Zero-variance" = "zv", "Correlation" = "corr"), selected = "center", multiple = TRUE),
                   sliderInput("ml1_proportion", "Proportion train/test set", min = 0, max = 1, value = 0.75),
                   selectInput("ml1_parameter", "Resampling method:", c("Cross-validation" = "repeatedcv", "Bootstrap" = "boot"), selected = "repeatedcv"),
                   sliderInput("ml1_number", "Number of folds/resampling iterations:", min = 1, max = 10, value = 2),
                   sliderInput("ml1_repeat", "k-fold cross-validation:", min = 1, max = 10, value = 1),
                   selectInput("ml1_model1", "Model 1:", c("Decision Tree" = "C5.0", "Random Forest" = "rf"), selected = "C5.0"),
                   selectInput("ml1_model2", "Model 2:", c("Decision Tree" = "C5.0", "Random Forest" = "rf"), selected = "rf")
               )
        ),
        column(width = 8,
               box(width = NULL, 
                   title = "Model 1", status="success", solidHeader = TRUE,
                   collapsible = TRUE,
                   verbatimTextOutput("confModel1")
               ),
               box(width = NULL, 
                   title = "Model 2", status="success", solidHeader = TRUE,
                   collapsible = TRUE, collapsed = TRUE,
                   verbatimTextOutput("confModel2")
               )
        )
      )
    ),
    
    # Recommendation system - Data -------------------------------------------------
    tabItem(
      tabName = "data2",
      fluidRow(
        column(width = 4,
               box(width = NULL,
                   title = "Input", status = "success", solidHeader = TRUE,
                   sliderInput("quantity", "Quatity input:", min = floor(min(data_base2$product_quantity, na.rm = TRUE)), 
                               max = ceiling(max(data_base2$product_quantity, na.rm = TRUE)), 
                               value = c(min(data_base2$product_quantity, na.rm = TRUE), 
                                         max(data_base2$product_quantity, na.rm = TRUE)), 
                               round = 1, sep = ""),
                   sliderInput("total_paid", "Total paid input:", min = floor(min(data_base2$total_paid, na.rm = TRUE)), 
                               max = ceiling(max(data_base2$total_paid, na.rm = TRUE)), 
                               value = c(floor(min(data_base2$total_paid, na.rm = TRUE)),
                                         ceiling(max(data_base2$total_paid, na.rm = TRUE))), 
                               round = 1, sep = ""),
                   selectInput("brand2", "Brand input:", choices = unique(data_base2$brand), multiple = TRUE, selected = c("Apple", "Toshiba", "Philips", "Samsung", "LG", "Dell", "Microsoft")),
                   selectInput("category", "Category input:", choices = unique(data_base2$manual_categories), multiple = TRUE, selected = c("accessories", "smartphone", "pc", "tablet", "laptop")),
                   dateRangeInput("dates", "Date range input:", start = min(data_base2$date), end = max(data_base2$date))
               )        
        ),
        column(width = 8,
               fluidRow(
                 column(width = 6,
                        valueBox(
                          width = NULL,
                          value = textOutput("number_products"), subtitle = "Number of Products",
                          icon = icon("shopping-cart"), color = "green"
                        )
                 ),
                 column(width = 6,
                        valueBox(
                          width = NULL,
                          value = textOutput("average_total_paid"), subtitle = "Average total paid",
                          icon = icon("dollar-sign"), color = "green"
                        )
                 )
               ), 
               fluidRow(
                 box(width = NULL, 
                     status = "success", solidHeader = TRUE,
                     title = "Total paid sum per Brand",
                     collapsible = TRUE,
                     plotlyOutput("data2_plot1")
                 ),
                 box(width = NULL, 
                     status = "success", solidHeader = TRUE,
                     title = "Total paid sum through month",
                     collapsible = TRUE, collapsed = TRUE,
                     plotlyOutput("data2_plot2")
                 )
               )
        )
      )
    ),
    
    # Recommendation system - EDA -------------------------------------------------
    tabItem(
      tabName = "eda2",
      fluidRow(
        column(width = 4,
               box(width = 12, status="success", solidHeader = TRUE,
                   selectInput('eda2_x', 'Select the variable on x-axis', names(data_base2), 'total_paid'),
                   selectInput('eda2_y', 'Select the variable on y-axis', names(data_base2), 'brand'),
                   selectInput('eda2_color', 'Select the variable on color', names(data_base2), 'manual_categories')
               )
        ), 
        column(width = 8,
               box(width = 12, status="success", solidHeader = TRUE,
                   plotlyOutput("eda2_plot")
               )
        )
      )
    ), 
    
    # Reccommendation system - Apriori -------------------------------------------------
    tabItem(
      tabName = "ml2",
      fluidRow(
        column(width = 4,
               box(width = NULL, 
                   title = "Parameters", status="success", solidHeader = TRUE,
                   sliderInput("ml2_support", "Support input:", min = 0, max = 0.003, value = 0.002),
                   sliderInput("ml2_confidence", "Confidence input:", min = 0, max = 0.8, value = 0.25),
                   sliderInput("ml2_minlen", "Minimum length for rules:", min = 1, max = 10, value = 2),
                   selectInput("ml2_rhs", "Right-hand-side input:", choices = unique(data_base2$sku), selected = "C5.0"),
                   selectInput("ml1_lhr", "Left-hand-side input:", choices = unique(data_base2$sku), selected = "rf")
               )
        ),
        column(width = 8,
               tabBox(width = NULL,
                   tabPanel("Transectional Data", 
                            verbatimTextOutput("ml2_summary")),
                   tabPanel("Apriori Summary", 
                            verbatimTextOutput("ml2_apriori")),
                   tabPanel("Rules", 
                            DT::dataTableOutput("apriori_rules")),
                   tabPanel("Grouped",
                            plotOutput("apriori_plot_grouped")),
                   tabPanel("Graph",
                            plotOutput("apriori_plot_graph"))
               )
        )
      )
    )
  )
)


# Dasboard Page ---------------------------
ui <- dashboardPage(
  skin = "green",
  header = dashboardHeader(title = "iMarket"),
  sidebar = sidebar,
  body = body
)


# Server ----------------------------------
server <- function(input, output, session){
  # reactive_db1 <- reactiveFileReader(
  #   intervalMillis = 1000,
  #   session = session,
  #   filePath = "test_data.csv",
  #   readFunc = function(filePath){
  #     read.csv2(filePath, na.strings = c("", " "))
  #   }
  # )
  
  # filter the database 1 ----------------------------------------------------
  data_base1_filter <- reactive({
    data_base1 %>% filter(brand %in% input$brand) %>% 
      filter(elevel %in% input$elevel) %>%
      filter(car %in% input$car) %>%  filter(zipcode %in% input$zipcode) %>% 
      filter(salary >= input$salary[1], salary <= input$salary[2]) %>% 
      filter(credit >= input$credit[1], credit <= input$credit[2]) %>% 
      filter(age >= input$age[1], age <= input$age[2]) %>% 
      droplevels()
  })
  
  # show the database 1 ------------------------------------------------------
  output$table1 <- DT::renderDataTable({
    head(data_base1_filter(), 25)
  })
  
  # plot the database 1 (Salary distribution by brand preference) -------
  output$data1_plot1 <- renderPlotly({
    if(length(input$brand) == 3){
      values_colors <- c(colors[8], colors[6], colors[10])
    }else if(length(input$brand) == 2){
      values_colors <- c(colors[6], colors[10])
    }else{
      values_colors <- c(colors[6])
    }
    
    data_base1_filter() %>% 
      ggplot(aes(salary, fill = brand)) + geom_histogram(bins = 100) +
      scale_fill_manual(values = values_colors) +
      geom_vline(aes(xintercept = median(salary)), linetype = "dashed") +
      labs(
        x = "",
        y = "Frequency",
        fill = ""
      )
  })
  
  # plot the database 1 (Age distribution by brand preference) -------
  output$data1_plot2 <- renderPlotly({
    if(length(input$brand) == 3){
      values_colors <- c(colors[8], colors[6], colors[10])
    }else if(length(input$brand) == 2){
      values_colors <- c(colors[6], colors[10])
    }else{
      values_colors <- c(colors[6])
    }
    
    data_base1_filter() %>% 
      ggplot(aes(age, fill = brand)) + geom_histogram(bins = 31) +
      scale_fill_manual(values = values_colors) +
      geom_vline(aes(xintercept = median(age)), linetype = "dashed") +
      labs(
        x = "",
        y = "Frequency",
        fill = ""
      )
  })
  
  # calculate basic statistics per brand --------------------------------------
  data_base1_statistics <- reactive({
    data_base1_filter() %>% group_by(brand) %>% 
    summarize(n = n(), mean = mean(salary)) %>% filter(brand %in% c("Elago", "Belkin"))
  })
  
  # organize the output for valueBox companies --------------------------------
  output$companies <- renderText({
    temp <- numeric(length = 2)
    
    if(length(unique(data_base1_statistics()$brand)) == 2){
      temp[1] <- data_base1_statistics()$n[1]
      temp[2] <- data_base1_statistics()$n[2]
    } else if(length(unique(data_base1_statistics()$brand)) == 1){
      if(data_base1_statistics()$brand == "Belkin"){
        temp[1] <- data_base1_statistics()$n[1]
      }else{
        temp[2] <- data_base1_statistics()$n[1]
      }
    }
    
    paste(prettyNum(temp[1], big.mark=","), "vs.", prettyNum(temp[2], big.mark=","))
  })
  
  # organize the output for valueBox average_salary ---------------------------
  output$average_salary <- renderText({
    temp <- numeric(length = 2)
    
    if(length(unique(data_base1_statistics()$brand)) == 2){
      temp[1] <- data_base1_statistics()$mean[1]
      temp[2] <- data_base1_statistics()$mean[2]
    } else if(length(unique(data_base1_statistics()$brand)) == 1){
      if(data_base1_statistics()$brand == "Belkin"){
        temp[1] <- data_base1_statistics()$mean[1]
      }else{
        temp[2] <- data_base1_statistics()$mean[1]
      }
    }
    paste(prettyNum(temp[1], big.mark=","), "vs.", prettyNum(temp[2], big.mark=","))
  })
  
  # Belgin vs. Elago - EDA (Plot) --------------------------------------------
  output$eda1_plot <- renderPlotly({
    data_base1_filter() %>% 
      p <- ggplot(aes_string(input$eda1_x, input$eda1_y))
      
      if(input$eda1_geom == "point"){
        p <- p + geom_point(aes_string(color = input$eda1_color))
      } else if(input$eda1_geom == "bar"){
        p <- p + geom_bar(aes_string(color = input$eda1_color)) 
      }
      p +
      labs(
        x = input$eda1_x,
        y = input$eda1_y,
        fill = ""
      )
  })
  
  # Belgin vs. Elago - Supervised algorithm ----------------------------------
  dataSetProcessed <- reactive({
    preProcValues <- data_base1_filter() %>% preProcess(method = input$ml1_preprocess)
    predict(preProcValues, data_base1_filter())
  })
  
  
  # Belgin vs. Elago - Spliting into two parts  ----------
  index <- reactive({
    createDataPartition(dataSetProcessed()$brand, p = input$ml1_proportion, list = FALSE)
  })
  
  trainSet <- reactive({
    dataSetProcessed() [index(),]
  })
  testSet <- reactive({
    dataSetProcessed()[-index(),]
  })
  
  # train Control -------------------------------
  fitControl <- reactive({
    trainControl(method = input$ml1_parameter, number = input$ml1_number, 
                 repeats = input$ml1_repeat, classProbs = TRUE, 
                 summaryFunction = twoClassSummary)
  })
  
  # Decision Tree --------------------
  model1 <- reactive({
    set.seed(123)
    cl <- makeCluster(2)
    registerDoParallel(cl)
    model1 <- train(brand ~ ., trainSet(), method = input$model1, trControl = fitControl(), 
                    metric = "ROC")
    stopCluster(cl)
    saveRDS(model1, "model1.RDS")
    model1
  })
  
  dtBrand <- reactive({
    predict(model1(), newdata = testSet())
  })
  
  output$confModel1 <- renderText({
    confusionMatrix(dtBrand(), testSet()$brand)
  })
  
  # Random forest --------------------------------
  model2 <- reactive({
    set.seed(123)
    cl <- makeCluster(2)
    registerDoParallel(cl)
    model2 <- train(brand ~ ., trainSet(), method = input$model1, trControl = fitControl(), 
                    metric = "ROC")
    stopCluster(cl)
    saveRDS(model2, "model2.RDS")
    model2
  })
  
  rfBrand <- reactive({
    predict(model2(), newdata = testSet())
  })
  
  output$confModel2 <- renderText({
    confusionMatrix(rfBrand(), testSet()$brand)
  })
  
  # Recomendation system -----------------------------------------------------
  # filter the database 2 ----------------------------------------------------
  data_base2_filter <- reactive({
    data_base2 %>% filter(brand %in% input$brand2) %>% 
      filter(manual_categories %in% input$category) %>%
      filter(total_paid >= input$total_paid[1], total_paid <= input$total_paid[2]) %>% 
      filter(product_quantity >= input$quantity[1], product_quantity <= input$quantity[2]) %>% 
      filter(date >= input$dates[1], date <= input$dates[2]) %>% 
      droplevels()
  })
  
  # show the database 2 ------------------------------------------------------
  output$table2 <- DT::renderDataTable({
    head(data_base2_filter(), 25)
  })
  
  # plot the database 2 (Salary distribution by brand preference) -------
  output$data2_plot1 <- renderPlotly({
    data_base2_filter() %>% group_by(brand) %>% 
      summarise(sum_total_paid = sum(total_paid)) %>% 
      ggplot(aes(brand, sum_total_paid)) + geom_bar(stat = "identity") +
      labs(
        x = "",
        y = "Sum of total paid",
        fill = ""
      ) + coord_flip()
  })
  
  # plot the database 2 (Age distribution by brand preference) -------
  output$data2_plot2 <- renderPlotly({
    data_base2_filter() %>%  group_by(month = floor_date(date, "month")) %>%  
      summarize(sum_total_paid = sum(total_paid)/100, sum_products = sum(product_quantity)) %>% 
      gather(key = "variable", value = "value", -month) %>% 
      ggplot(aes(x = month, y = value)) + 
      geom_area(aes(color = variable, fill = variable), 
                alpha = 0.5, position = position_dodge(0.8)) +
      scale_color_manual(values = c("#00AFBB", "#E7B800")) +
      scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
      labs(
        x = "",
        y = "Total paid (e+2)",
        fill = ""
      )
  })
  
  # calculate basic statistics per brand --------------------------------------
  data_base2_statistics <- reactive({
    data_base2_filter() %>% 
      summarize(n = n_distinct(sku), mean = mean(total_paid)) 
  })
  
  # organize the output for valueBox number of products -----------------------
  output$number_products <- renderText({
    prettyNum(data_base2_statistics()$n, big.mark=",")
  })
  
  # organize the output for valueBox average total_paid -----------------------
  output$average_total_paid <- renderText({
    prettyNum(data_base2_statistics()$mean, big.mark=",")
  })
  
  # Recommendation system - EDA (Plot) ----------------------------------------
  output$eda2_plot <- renderPlotly({
    data_base2_filter() %>% 
      ggplot(aes_string(input$eda2_x, input$eda2_y)) + 
      geom_point(aes_string(color = input$eda2_color)) +
      labs(
        x = input$eda2_x,
        y = input$eda2_y,
        fill = ""
      )
  })
  
  # Recommendation system - Apriori algorithm --------------------------------
  transectional_data <- reactive({
    product_freq <- data_base2_filter() %>% group_by(sku) %>% 
      summarise(quantity = sum(product_quantity))
    line_item_clean <- data_base2_filter() %>% filter(sku %in% product_freq$sku)
    
    transact <- line_item_clean %>% arrange(id_order) %>% 
      mutate(id_order_sku = paste(id_order, sku, sep = ";")) %>% 
      select(id_order_sku) %>% unlist(., use.names = FALSE) %>% paste(., collapse = "\n" )
    paste("item_id;trans_id", transact, sep = "\n") %>% write("item_list")
    
    transact <- read.transactions("item_list", format = "single", header = TRUE, sep = ";", cols = c("item_id", "trans_id"))
    transact
  })
  
  output$ml2_summary <- renderPrint({
    summary(transectional_data())
  })
  
  output$apriori_plot <- renderPlotly({
    itemFrequencyPlot(transectional_data(), type = "absolute", horiz= TRUE, top = 20, 
                      support = input$support, cex.names = .5)
  })
  
  apriori_model <- reactive({
    trans_rules  <- apriori(transectional_data(), parameter = list(support = input$ml2_support, 
            confidence = input$ml2_confidence, minlen = input$ml2_minlen), control = list(verbose = F))
    
    # remove redundant rules
    trans_rules[!is.redundant(trans_rules)]
  })
  
  output$ml2_apriori <- renderPrint({
    summary(apriori_model())
  })
  
  output$apriori_rules <- DT::renderDataTable({
    inspectDT(apriori_model())
  })
  
  output$apriori_plot_grouped <- renderPlot({
    plot(apriori_model(), method = "grouped")
  })
  
  output$apriori_plot_graph <- renderPlot({
    plot(apriori_model(), method = "graph")
  })
}

shinyApp(ui, server)