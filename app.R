library(shiny)
library(shinydashboard)
library(shinyWidgets)

library(plotly)
library(readxl)
library(tidyverse)
library(DT)
library(cowplot)

source("./global.R")    # Get & preprocess DB data

nms <- sort(names(data_df))

ui <- 
dashboardPage(
    
    dashboardHeader(title = paste0("Sample explorer: ", unique(data_df$PROJECT)[1])),  
    
    dashboardSidebar(
          selectInput('x', 'X', 
                      choices = nms[nms %in% c("kum_renteInntekt", "renteInntekt_mnd", "sparing_tot")], selected = "renteInntekt_mnd"),        
        # selectInput('x', 'X', 
        #             choices = nms, selected = "CLIN_TRL_VISIT"),
        # selectInput('y', 'Y', 
        #             choices = nms, selected = "SAMPLE_TYPE"),
        # sliderInput('size', 'Point size', 
        #             min = 0.5, max = 5, value = 3),
        # sliderInput('alpha', 'Point opacity', 
        #             min = 0.1, max = 1, value = 0.5),
        # selectInput('color', 'Color', 
        #             choices = nms, selected = "F_FASTING"),
        # selectInput('facet_row', 'Facet Row', 
        #             c(None = '.', nms), selected = "LOGIN_BY"),
        # selectInput('facet_col', 'Facet Column', 
        #             c(None = '.', nms)),
        # sliderInput('plotHeight', 'Height of plot (in pixels)', 
        #             min = 100, max = 2000, value = 900),
        # sliderInput('legendPositionX', 'Legend Position X', 
        #             min = -0.3, max = 1.05,
        #             value=-0.05, step=0.05, round=0),
        # sliderInput('legendPositionY', 'Legend Position Y', 
        #             min = -0.15, max = 1.1,
        #             value=-.15, step=0.05, round=0),
        # selectInput("selectGridlines",
        #             label = "Grid lines",
        #             choices = list("-" = "none",
        #                             "X" = "x",
        #                             "Y" = "y",
        #                             "X+Y" = "xy"),
        #             selected = "none")
    ),                                                                           # End dashboardSidebar
    dashboardBody(
        
        # actionBttn(
        #     inputId = "upd_bttn",
        #     label = "UPDATE", 
        #     style = "material-flat",
        #     color = "primary"
        # ),
        
        tabsetPanel(type = "tabs",
                    tabPanel("Plot", plotlyOutput('trendPlot', height = "900px")),
                    tabPanel("Table", DTOutput('table1')),
                    tabPanel("Summary", DTOutput("summary"))
        )
 
        
        # plotlyOutput('trendPlot', height = "900px"),
      
        # DTOutput('table1')
    )                                                                            # End dashboardBody
)

server <- function(input, output) {
    
    dataset <- data_df
    
    # count <- eventReactive(input$upd_button, {  
    #     dataset$update_column <- input$upd_bttn
    # })
    
    # dataset <- reactive({
    #     data_df[sample(nrow(data_df), input$sampleSize),]
    # })
    
    output$trendPlot <- renderPlotly({
        
        p <- ggplot(dataset, 
                    aes_string(
                            x = input$x, 
                            y = input$y,
                            color = input$color)) + 
            geom_jitter(size=input$size, 
                        alpha=input$alpha,
                        aes(text = paste0(
                                "ID: ", CLIN_TRL_SUBJECT_ID,
                                "<br>VISIT: ", CLIN_TRL_VISIT,
                                "<br>SAMPLED: ", SAMPLED_DATE)))+
            theme(axis.text.x=element_text(angle = 30, hjust = 0),
                  legend.position = 'bottom',
                  legend.title = element_blank(),
                  axis.title.y=element_blank(),
                  axis.title.x=element_blank())+
            background_grid(major = paste(input$selectGridlines))
            
        
        facets <- paste(input$facet_row, '~', input$facet_col)
        if (facets != '. ~ .') {p <- p + facet_grid(facets)}
        
        ggplotly(p, tooltip="text",
                height = input$plotHeight #, dynamicTicks = T
                ) %>% 
        layout( #xaxis = list(title="", automargin=TRUE), 
                #yaxis = list(title="", automargin=TRUE),
                legend = list(orientation = "h", 
                            x = input$legendPositionX,
                            y = input$legendPositionY)
               # margin = list(l=100, b=50)
               
            )
    })
    
    output$table1 <- renderDT(dataset,
                              plugins = 'natural', 
                              extensions = 'Buttons',
                                options = list(
                                    dom = 'Bfrtip',
                                    buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print'),
                                    pageLength = 20,
                                    lengthMenu = c(20, 50, 100, 250, 1000),
                                    columnDefs = list(list(type = 'natural', # Natural sorting helps sort mixes of alphabetical/numerical text columns
                                                           targets = 6))
                                        ))
    
    output$summary <- renderDT(dataset %>%  
                                   group_by(CLIN_TRL_VISIT) %>% 
                                   summarise(n=n()) %>% 
                                   arrange(desc(n)),
                               plugins = 'natural', 
                               extensions = 'Buttons',
                               options = list(
                                   dom = 'Bfrtip',
                                   buttons = c('pageLength', 'copy', 'csv', 'excel', 'pdf', 'print')))
                
                               
}

shinyApp(ui, server)
