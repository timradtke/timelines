# Code based on current example on https://shiny.rstudio.com

# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)

################################################################################

# First, create a synthetic data set of a group of products

set.seed(83823)

hierarchy <- data.frame(
    brand = c("aa", "aa", "aa", "aa", "aa", "bb", "bb", "bb"),
    product = c("718", "718", "919", "919", "919", "911", "911", "911"),
    color = c("red", "blue", "red", "blue", "orange", "red", "blue", "yellow")
)
hierarchy$sku <- paste(hierarchy$brand, hierarchy$product, 
                       hierarchy$color, sep = "-")

dates <- expand.grid(sku = unique(hierarchy$sku),
                     date = seq(as.Date("2015-01-01"), as.Date("2019-12-01"),
                         by = "month"),
                     stringsAsFactors = FALSE)

data <- inner_join(dates, hierarchy, by = "sku") %>%
    arrange(sku, date)
n_obs <- dim(data)[1]
n_sku <- length(unique(data$sku))
n_date <- length(unique(data$date))
data$value <- rpois(n_obs, rnorm(n_obs, rep(sqrt(1:n_date), times = n_sku) + 
                        rep(sinpi((1:n_date) / 6), times = n_sku), 0.3))

################################################################################

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("timelines"),
                sidebarLayout(
                    sidebarPanel(
                        
                        # Select type of trend to plot
                        selectInput(inputId = "sku", label = strong("SKU"),
                                    choices = unique(data$sku),
                                    selected = "bb-911-yellow"),
                        
                        # Select date range to be plotted
                        dateRangeInput("date", strong("Date range"), 
                                       start = "2015-01-01", end = "2019-12-01",
                                       min = "2015-01-01", max = "2019-12-01"),
                        
                        # Select whether to overlay smooth trend line
                        checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),
                        
                        # Display only if the smoother is checked
                        conditionalPanel(condition = "input.smoother == true",
                                         sliderInput(inputId = "f", label = "Smoother span:",
                                                     min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                     animate = animationOptions(interval = 100)),
                                         HTML("Higher values give more smoothness.")
                        )
                    ),
                    
                    # Output: Description, lineplot, and reference
                    mainPanel(
                        plotOutput(outputId = "lineplot", height = "300px"),
                        plotOutput(outputId = "lineplot2", height = "300px"),
                        textOutput(outputId = "desc"),
                        tags$a(href = "https://minimizeregret.com", "Source: Minimize Regret", target = "_blank")
                    )
                )
)

# Define server function
server <- function(input, output) {
    
    selected_sku <- reactive({
        req(input$date)
        validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), 
                      "Error: Please provide both a start and an end date."))
        validate(need(input$date[1] < input$date[2], 
                      "Error: Start date should be earlier than end date."))
        data %>%
            filter(
                sku == input$sku,
                date > as.Date(input$date[1]) & 
                    date < as.Date(input$date[2])
            )
    })
    
    selected_sku_group <- reactive({
        req(input$date)
        validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), 
                      "Error: Please provide both a start and an end date."))
        validate(need(input$date[1] < input$date[2], 
                      "Error: Start date should be earlier than end date."))
        # get the SKU's group
        input_group <- unique(data[data$sku == input$sku,]$brand)
        
        data %>%
            filter(
                brand == input_group,
                date > as.Date(input$date[1]) & 
                    date < as.Date(input$date[2])
            )
    })
    
    # Create scatterplot object the plotOutput function is expecting
    output$lineplot <- renderPlot({
        p <- ggplot(selected_sku(), aes(x = date, y = value)) +
            geom_line(color = "grey") +
            geom_point() +
            labs(x = "Date", y = "Value")
        
        # Display only if smoother is checked
        if(input$smoother){
            p <- p + geom_smooth()
        }
        print(p)
    })
    
    output$lineplot2 <- renderPlot({
        p <- ggplot(selected_sku_group(), aes(x = date, y = value)) +
            geom_line(color = "grey") +
            geom_point() +
            labs(x = "Date", y = "Value") +
            facet_wrap(~sku)
        
        print(p)
    })
}

# Create Shiny object
shinyApp(ui = ui, server = server)