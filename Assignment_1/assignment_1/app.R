# Shiny App: CSE780 Assignment 1
# Pankaj S
# LINK TO APP: https://singp37.shinyapps.io/assignment_1/

# Imports
library("this.path")
library(shiny)
library(tidyverse);
library(ggplot2)

# Load Data
load("raw_data_provinces.RData")

# Filter based on input for plot.
filter_plot_data_all <- function(input, type_of_mortality){
    return(raw_data_provinces |>   
       filter(Characteristics == "Rate") |>
       filter(Sex == input$sex) |>
       filter(`Infant or perinatal mortality` == type_of_mortality ))
}

filter_plot_data <- function(input, type_of_mortality){
    
    if(input$geographic_area == "All"){
        return(filter_plot_data_all(input, type_of_mortality))
    } else {
        return(filter_plot_data_all(input, type_of_mortality) |>   
         filter(GEO == input$geographic_area | GEO == "Canada"))
    }
    
}

provide_subtitle <- function(input){
    if(input$geographic_area == "All" | input$geographic_area == "Canada" ){
        return("")
    } else {
        return("Canada rate is provided for comparison.")
    }
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Mortality rate per 1000 live births"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            selectInput(inputId = "sex",
                        label = "Select sex",
                        choices = c("Both sexes", "Females", "Males"),
                        selected  = "Both sexes"),
            
            selectInput(inputId = "geographic_area",
                        label = "Select a geographic area",
                        choices = c("All", unique(raw_data_provinces$GEO)),
                        selected  = "All")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("statePlot"),
           plotOutput("wholeCanada")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    

    output$statePlot <- renderPlot({
            
            # Plotting mortality rates.
            ggplot(filter_plot_data(input, "Infant mortality"), aes(x = REF_DATE, y = VALUE)) +
            geom_point() +
            geom_smooth(size=0.5, aes(color = GEO)) +
            scale_color_discrete(name="Geographic area") + 
            scale_shape_discrete(name="Geographic area") + 
            ylim(0, 25) +
            labs(
                title = str_interp("Infant mortality ${input$geographic_area} for ${input$sex}"),
                subtitle = provide_subtitle(input),
                x = "Year", y = str_interp("Infant mortality rate"),
                color = "Sex", shape = "Sex"
            ) +
            theme(
                plot.title = element_text(size=22, hjust = 0.5),
                plot.subtitle = element_text(size=14, hjust = 0.5, color = "blue"),
                axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 12),
                axis.text = element_text(size = 12),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 12)
            )
        
    })
    
    
    output$wholeCanada <- renderPlot({
        
        # Plotting mortality rates.
        ggplot(filter_plot_data(input, "Perinatal mortality"), aes(x = REF_DATE, y = VALUE)) +
            geom_point() +
            geom_smooth(size=0.5, aes(color = GEO)) +
            scale_color_discrete(name="Geographic area") + 
            scale_shape_discrete(name="Geographic area") + 
            ylim(0, 25) +
            labs(
                title = str_interp("Perinatal mortality ${input$geographic_area} for ${input$sex}"),
                subtitle = provide_subtitle(input),
                x = "Year", y = str_interp("$Perinatal mortality rate"),
                color = "Sex", shape = "Sex"
            ) +
            theme(
                plot.title = element_text(size=22, hjust = 0.5),
                plot.subtitle = element_text(size=14, hjust = 0.5, color = "blue"),
                axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 12),
                axis.text = element_text(size = 12),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 12)
            )
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
