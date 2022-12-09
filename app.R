#Import Relevant Packages-------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(lubridate)

#Cascading Style Sheets---------------------------------------------------------
ui <- fluidPage(
  includeCSS("css/kss_style.css"),
  
#User Interface-----------------------------------------------------------------

    #Web App Title
    headerPanel("Kelly's Small Shop Analytics Dashboard - Powered By CPJD Associates"),
    
    #Divide The Dashboard Into Multiple Tabs
    tabsetPanel(
        
#Import Tab---------------------------------------------------------------------
      tabPanel("File Import",
               br(),
               headerPanel("File Import"),
               sidebarPanel(
                 #Products
                 fileInput("products", "Import Products Data (.csv)", 
                           accept = c(".csv"),
                           buttonLabel = "Select"),
                 DTOutput('kss_products_table'),
                 #Orders
                 fileInput("orders", "Import Orders Data (.csv)", 
                           accept = c(".csv"),
                           buttonLabel = "Select"),
                 DTOutput('kss_orders_table'),
                 #Customers
                 fileInput("customers", "Import Customer Data (.csv)",
                           accept = c(".csv"),
                           buttonLabel = "Select"),
                 DTOutput('kss_customers_table')
               )
              ),
#Products Tab-------------------------------------------------------------------
      tabPanel("Product Analytics",
               br(),
               headerPanel("Product Analytics"),
                 sidebarPanel(
                 #Input: Vertical
                 selectInput(inputId = "v",
                               label = "Choose A Continuous Variable (Y-Axis)",
                               choices = c("price", "size"),
                               selected = "price",
                               multiple = FALSE),
                   
                 #Input: Horizontal
                 selectInput(inputId = "h",
                               label = "Choose A Categorical Variable (X-Axis)",
                               choices = c("quantity", "category", "SKU"),
                               selected = "SKU",
                               multiple = FALSE),
                 #Input: Fill
                 selectInput(inputId = "f",
                             label = "Choose A Categorical Measurement (Fill)",
                             choices = c("quantity", "category", "SKU"),
                             selected = "category",
                             multiple = FALSE),
                 #Input: Bar Chart Type
                 radioButtons(inputId = "t",
                              label = "Choose The Type Of Chart Chart To Be Displayed",
                              choices = c("dodge", "stack", "fill"),
                              selected = "dodge"),
                 #Output: Display
                 plotOutput("barchart_products")
               )
              ),
#Orders Tab---------------------------------------------------------------------
      tabPanel("Order Analytics",
               br(),
               headerPanel("Order Analytics"),
               sidebarPanel(
                 selectInput(inputId = "xr",
                             label = "Choose A Time Frame (X-Axis)",
                             choices = c("Order Date", "Order Month"),
                             selected = "Order Date",
                             multiple = FALSE),
                 selectInput(inputId = "vr",
                             label = "Choose A Measurement (Y-Axis)",
                             choices = c("Price", "Number of Products"),
                             selected = "Price",
                             multiple = FALSE),
                 plotOutput("scatter_orders"), 
                 br(),
                 
                 selectInput(inputId = "hr",
                             label = "Choose A Categorical Variable (X-Axis)",
                             choices = c("Customer ID", "Delivery Method"),
                             selected = "Customer ID",
                             multiple = FALSE),
                 selectInput(inputId = "yr",
                             label = "Choose A Continuous Variable (Y-Axis)",
                             choices = c("Order Price", "Number of Products"),
                             selected = "Order Price",
                             multiple = FALSE),
                 plotOutput("barplot_orders"),
               )),

#Customers Tab------------------------------------------------------------------
      tabPanel("Customer Analytics",
               br(),
               headerPanel("Customer Analytics"),
               sidebarPanel(
                 #Input: Horizontal
                 selectInput(inputId = "x",
                             label = "Choose A Categorical Variable (X-Axis)",
                             choices = c("customerId", "first", "last", "phone",
                                "email", "address", "town", "zip", "state"),
                             selected = "town",
                             multiple = FALSE),
                 plotOutput("barchart_customers"),
               ))
    )
  )

#End Of UI Code-----------------------------------------------------------------

#Design Server Logic
server <- function(input, output) {
  
#Product Visualizations---------------------------------------------------------
  
    #Function To Display The Product Data In The UI
    output$kss_products_table <- renderDT({
      inFile <- input$products
      if (is.null(inFile))
        return(NULL)
      read.csv(inFile$datapath)},
      options = list(pageLength = 8))
  
    #Create Bar Plot
    output$barchart_products <- renderPlot({
      inFile <- input$products
      df <- read.csv(inFile$datapath)
      
      #Change SKU To Categorical
      df$SKU<-as.factor(df$SKU)
      
      #X Axis
      if(input$h == "quantity") { 
        h <- df$quantity
      } else if (input$h == "category") {
        h <- df$category
      } else if (input$h == "SKU") {
        h <- df$SKU
      }
      #Y Axis
      if(input$v == "price") { 
        v <- df$price
      } else if (input$v == "size") {
        v <- df$size
      } 
      #Fill
      if(input$f == "quantity") { 
        f <- df$quantity
      } else if (input$f == "category") {
        f <- df$category
      } else if (input$f == "SKU") {
        f <- df$SKU
      }
      
      ggplot(df,aes(fill = f, y = v, x = h)) +
              geom_bar(position = input$t, stat = "identity", colour="black") +
              ggtitle("Products Bar Chart") +
              xlab(input$h) + ylab(input$v) + labs(fill = input$f) + 
        theme_classic() +
        theme(
          plot.title = element_text(color="black", size=18, family = "Trebuchet MS", face="bold"),
          axis.title.x = element_text(color="black", size=14, family = "Trebuchet MS", face="bold"),
          axis.title.y = element_text(color="black", size=14, family = "Trebuchet MS", face="bold"),
          axis.text.x = element_text(color="black", family = "Trebuchet MS", size=12),
          axis.text.y = element_text(color="black", family = "Trebuchet MS", size=12),
          legend.title = element_text(color="black", family = "Trebuchet MS", size= 14, face ="bold"),
          legend.text = element_text(color="black", family = "Trebuchet MS", size= 12)
        ) + scale_colour_brewer(type = "seq", palette = "Spectral")
    })

#Orders Visualizations----------------------------------------------------------
    
    #Function To Display The Order Data In The UI
    output$kss_orders_table <- renderDT({
      inFile <- input$orders
      if (is.null(inFile))
        return(NULL)
      read.csv(inFile$datapath)},
      options = list(pageLength = 8))
    
    #Create Line Graph Plot
    output$scatter_orders <- renderPlot({
      inFile <- input$orders
      df <- read.csv(inFile$datapath)
      
      #Change Order Date To Date
      df$orderDate <- as.Date(df$orderDate, "%m/%d/%Y")
      df$orderMonth <- month(df$orderDate)
      
      #X Axis
      if(input$xr == "Order Date") { 
        xr <- df$orderDate
      } else if (input$xr == "Order Month") {
        xr <- df$orderMonth
      } 

      #Y Axis
      if(input$vr == "Price") { 
        vr <- df$orderPrice
      } else if (input$vr == "Number of Products") {
        vr <- df$numProd
      } 
      
      ggplot(df,aes(y = vr, x = xr, col = deliverMethod)) +
        geom_point() +
        stat_smooth() +
        ggtitle("Orders Scatter Plot") +
        xlab("Order Date") + ylab(input$vr) + labs(fill = df$deliverMethod) + 
        theme_classic() +
        theme(
          plot.title = element_text(color="black", size=18, family = "Trebuchet MS", face="bold"),
          axis.title.x = element_text(color="black", size=14, family = "Trebuchet MS", face="bold"),
          axis.title.y = element_text(color="black", size=14, family = "Trebuchet MS", face="bold"),
          axis.text.x = element_text(color="black", family = "Trebuchet MS", size=12),
          axis.text.y = element_text(color="black", family = "Trebuchet MS", size=12),
          legend.title = element_text(color="black", family = "Trebuchet MS", size= 14, face ="bold"),
          legend.text = element_text(color="black", family = "Trebuchet MS", size= 12)
        ) + scale_colour_brewer(type = "seq", palette = "Spectral")
    })
    
    #Create Bar Plot
    output$barplot_orders <- renderPlot({
      inFile <- input$orders
      df <- read.csv(inFile$datapath)
      
      #X Axis
      if(input$hr == "Customer ID") { 
        hr <- df$customerID
      } else if (input$hr == "Delivery Method") {
        hr <- df$deliverMethod
      }
    
      #Y Axis
      if(input$yr == "Order Price") { 
        yr <- df$orderPrice
      } else if (input$yr == "Number of Products") {
        yr <- df$numProd} 
      
      ggplot(df,aes(x = hr, y = yr, fill = hr)) +
        geom_bar(stat = "identity", colour="black") +
        ggtitle("Orders Bar Chart") +
        xlab(input$hr) + ylab(input$yr) +
        theme_classic()+
        theme(
          plot.title = element_text(color="black", size=18, family = "Trebuchet MS", face="bold"),
          axis.title.x = element_text(color="black", size=14, family = "Trebuchet MS", face="bold"),
          axis.title.y = element_text(color="black", size=14, family = "Trebuchet MS", face="bold"),
          axis.text.x = element_text(color="black", family = "Trebuchet MS", size=12),
          axis.text.y = element_text(color="black", family = "Trebuchet MS", size=12),
        ) + scale_colour_brewer(type = "seq", palette = "Spectral")
    })
    
#Customers Visualizations-------------------------------------------------------
    #Function To Display The Customer Data In The UI
    output$kss_customers_table <- renderDT({
      inFile <- input$customers
      if (is.null(inFile))
        return(NULL)
      read.csv(inFile$datapath)},
      options = list(pageLength = 8))
    
    output$barchart_customers <- renderPlot({
      inFile <- input$customers
      df <- read.csv(inFile$datapath)
      
      #Change SKU To Categorical
      df$zip <- as.factor(df$zip)
      
      #X Axis
      if(input$x == "customerId") { 
        x <- df$customerId
      } else if (input$x == "category") {
        x <- df$category
      } else if (input$x == "first") {
        x <- df$first
      } else if (input$x == "last") {
        x <- df$last
      } else if (input$x == "phone") {
        x <- df$phone
      } else if (input$x == "email") {
        x <- df$email
      } else if (input$x == "address") {
        x <- df$address
      } else if (input$x == "town") {
        x <- df$town 
      } else if (input$x == "zip") {
        x <- df$zip
      } else if (input$x == "state") {
        x <- df$state
      }
    
      ggplot(df,aes(x = x, fill = x)) +
        geom_bar(colour="black") +
        ggtitle("Customers Bar Chart") +
        xlab(input$x) + ylab("count") +
        theme_classic()+
        theme(
          plot.title = element_text(color="black", size=18, family = "Trebuchet MS", face="bold"),
          axis.title.x = element_text(color="black", size=14, family = "Trebuchet MS", face="bold"),
          axis.title.y = element_text(color="black", size=14, family = "Trebuchet MS", face="bold"),
          axis.text.x = element_text(color="black", family = "Trebuchet MS", size=12),
          axis.text.y = element_text(color="black", family = "Trebuchet MS", size=12),
          legend.title = element_text(color="black", family = "Trebuchet MS", size= 14, face ="bold"),
          legend.text = element_text(color="black", family = "Trebuchet MS", size= 12)
        ) + scale_colour_brewer(type = "seq", palette = "Spectral")
    })  
}

# Run the application----------------------------------------------------------- 
shinyApp(ui, server) 
