library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

# ---------------- PRODUCTS(BACKGROUND) ----------------
products <- reactiveVal(
  data.frame(
    name = c(
      "Python Programming Book",
      "Data Science Video Course",
      "Web Development Notes",
      "SQL Practice Workbook",
      "Statistics Online Course"
    ),
    price = c(750, 1200, 300, 450, 1000),
    stock = c(20, 15, 40, 25, 10),
    rating = c(4.8, 4.6, 4.5, 4.4, 4.7),
    stringsAsFactors = FALSE
  )
)

# ---------------- UI ----------------
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = tagList(
      tags$img(
        src = "https://cdn-icons-png.flaticon.com/512/2232/2232688.png",
        height = "22px",
        style = "margin-right:6px;"
      ),
      "EduStore"
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("About Us", tabName = "about", icon = icon("info-circle")),
      menuItem("Materials", tabName = "products", icon = icon("book")),
      menuItem("Shopping Cart", tabName = "cart", icon = icon("shopping-cart")),
      menuItem("Contact", tabName = "contact", icon = icon("envelope")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-bar")),
      menuItem("Data Analysis", tabName = "analysis", icon = icon("chart-area"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .skin-blue .main-header .logo { background-color:#7b5cff; }
        .skin-blue .main-header .navbar { background-color:#6ec6ff; }
        .skin-blue .sidebar-menu>li.active>a {
          background:#d9ccff; color:#3f2b96;
        }
      "))
    ),
    
    tabItems(
      
      # ---------------- HOME ----------------
      tabItem(
        tabName = "home",
        fluidRow(
          valueBoxOutput("total_products", 4),
          valueBoxOutput("total_sales", 4),
          valueBoxOutput("total_orders", 4)
        ),
        br(),
        h2("Welcome to EduStore"),
        p("EduStore is an online e-commerce platform that offers educational materials such as books, online courses, and digital learning resources."),
        p("Users can browse materials, add products to their shopping cart, and track total orders and sales in real time."),
        p("The system is developed using R Shiny and demonstrates basic e-commerce and data visualization features.")
      ),
      
      # ---------------- ABOUT ----------------
      tabItem(
        tabName = "about",
        h3("About Us"),
        p("EduStore provides high-quality educational materials for students and professionals."),
        h4("Vision"),
        p("To become a trusted digital marketplace for educational resources."),
        h4("Mission"),
        tags$ul(
          tags$li("Provide affordable educational materials"),
          tags$li("Support digital learning"),
          tags$li("Offer a user-friendly shopping experience")
        )
      ),
      
      # ---------------- PRODUCTS ----------------
      tabItem(
        tabName = "products",
        h3("Educational Materials"),
        textInput("search", "Search:"),
        tableOutput("product_table"),
        selectInput("selected_product", "Select product:", choices = NULL),
        actionButton("add_to_cart", "Add to Cart")
      ),
      
      # ---------------- CART ----------------
      tabItem(
        tabName = "cart",
        h3("Shopping Cart"),
        tableOutput("cart_table"),
        selectInput("remove_product", "Remove product:", choices = NULL),
        actionButton("remove_from_cart", "Remove"),
        br(), br(),
        textOutput("total_price"),
        actionButton("clear_cart", "Clear Cart")
      ),
      
      # ---------------- CONTACT ----------------
      tabItem(
        tabName = "contact",
        h3("Contact Us"),
        textInput("name", "Name"),
        textInput("email", "Email"),
        textAreaInput("message", "Message"),
        actionButton("send", "Send Message"),
        br(), br(),
        h4("Our Location (Izmir / Gaziemir)"),
        tags$iframe(
          src = "https://www.google.com/maps?q=38.3193,27.1276&z=14&output=embed",
          width = "100%",
          height = "300"
        )
      ),
      
      # ---------------- DASHBOARD ----------------
      tabItem(
        tabName = "dashboard",
        plotOutput("stock_plot"),
        plotOutput("price_plot")
      ),
      
      # ---------------- ANALYSIS ----------------
      tabItem(
        tabName = "analysis",
        h3("Price Distribution"),
        plotOutput("violin_plot")
      )
    )
  )
)

# ---------------- SERVER ----------------
server <- function(input, output, session) {
  
  observe({
    updateSelectInput(session, "selected_product", choices = products()$name)
  })
  
  filtered_products <- reactive({
    df <- products()
    if (input$search == "") df
    else df %>% filter(grepl(input$search, name, ignore.case = TRUE))
  })
  
  output$product_table <- renderTable({
    filtered_products() %>%
      mutate(No = row_number(),
             Rating = paste0(round(rating), " points")) %>%
      select(No, name, price, stock, Rating)
  })
  
  cart <- reactiveVal(
    data.frame(name=character(), price=numeric(), quantity=numeric())
  )
  
  observeEvent(input$add_to_cart, {
    df <- products()
    sel <- df[df$name == input$selected_product, ]
    
    if (sel$stock <= 0) {
      showModal(modalDialog("Out of stock", easyClose = TRUE))
      return()
    }
    
    df[df$name == sel$name, "stock"] <- sel$stock - 1
    products(df)
    
    cdf <- cart()
    if (sel$name %in% cdf$name) {
      cdf[cdf$name == sel$name, "quantity"] <- cdf[cdf$name == sel$name, "quantity"] + 1
      cart(cdf)
    } else {
      cart(rbind(cdf, data.frame(name=sel$name, price=sel$price, quantity=1)))
    }
    
    showModal(modalDialog("Added to cart", easyClose = TRUE))
  })
  
  observe({
    updateSelectInput(session, "remove_product", choices = cart()$name)
  })
  
  # -------- FIXED REMOVE (1 ADET AZALT) --------
  observeEvent(input$remove_from_cart, {
    
    cdf <- cart()
    idx <- which(cdf$name == input$remove_product)
    if (length(idx) == 0) return()
    
    # stok +1
    df <- products()
    df[df$name == cdf$name[idx], "stock"] <-
      df[df$name == cdf$name[idx], "stock"] + 1
    products(df)
    
    # quantity kontrol??
    if (cdf$quantity[idx] > 1) {
      cdf$quantity[idx] <- cdf$quantity[idx] - 1
    } else {
      cdf <- cdf[-idx, ]
    }
    
    cart(cdf)
  })
  
  observeEvent(input$clear_cart, {
    cdf <- cart()
    df <- products()
    
    for (i in 1:nrow(cdf)) {
      df[df$name == cdf$name[i], "stock"] <-
        df[df$name == cdf$name[i], "stock"] + cdf$quantity[i]
    }
    
    products(df)
    cart(cart()[0, ])
  })
  
  output$cart_table <- renderTable({
    df <- cart()
    if (nrow(df) > 0) df$total <- df$price * df$quantity
    df
  })
  
  output$total_price <- renderText({
    if (nrow(cart()) == 0) "Total Price: 0 TL"
    else paste("Total Price:", sum(cart()$price * cart()$quantity), "TL")
  })
  
  # -------- HOME SUMMARY --------
  output$total_products <- renderValueBox({
    valueBox(nrow(products()), "Total Products", icon("box"), color = "purple")
  })
  
  output$total_sales <- renderValueBox({
    total <- if (nrow(cart()) == 0) 0 else sum(cart()$price * cart()$quantity)
    valueBox(paste0(total, " TL"), "Total Sales", icon("lira-sign"), color = "aqua")
  })
  
  output$total_orders <- renderValueBox({
    orders <- if (nrow(cart()) == 0) 0 else sum(cart()$quantity)
    valueBox(orders, "Orders", icon("shopping-bag"), color = "yellow")
  })
  
  observeEvent(input$send, {
    showModal(modalDialog("Message sent successfully!", easyClose = TRUE))
  })
  
  output$stock_plot <- renderPlot({
    ggplot(products(), aes(name, stock)) +
      geom_col(fill="#6ec6ff") + theme_minimal()
  })
  
  output$price_plot <- renderPlot({
    ggplot(products(), aes(name, price)) +
      geom_col(fill="#7b5cff") + theme_minimal()
  })
  
  output$violin_plot <- renderPlot({
    ggplot(products(), aes(x="", y=price)) +
      geom_violin(fill="#d9ccff") +
      geom_jitter(width=0.1, color="#3f2b96") +
      theme_minimal()
  })
}

shinyApp(ui, server)
