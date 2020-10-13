# Read in the all required libraries

library(shiny); library(shinyWidgets); library(tidyverse); library(extrafont); 
library(ggrepel); library(ggthemes); library(scales); library(magrittr)

# Read in pre-processed dataframe (data from transfermarkt.com

J_scorers <- read.csv(file = 'J_scorers_tidy.csv') %>% na.omit()

# Define UI

ui <- fluidPage(
    
# Title and subtitle of the app

titlePanel(

    p(
        h1('Juventus FC top goalscorers',
        style = "font-family: 'Bahnschrift';
        font-size = 200%; font-weight: 500;
        color: #000000;"),

        h2('An interactive plot of Juventus top goalscorers in each season, by competition',
        style = "font-family: 'Bahnschrift';
        font-size = 100%; font-weight: 100;
        color: #000000;")
    )

),

# Define a slider that selects the range of seasons to be shown

headerPanel(
     h4('Explore the data:',
        style = "font-family: 'Bahnschrift';
     font-size = 80%; font-weight: 100;
     color: #000000;")
     ),
    
    hr(),

    fluidRow(
        
        column(width = 6,
               
               sliderInput(inputId = "Seasons",
                           label = "Select range of seasons:",
                           min = min(J_scorers$Season_start),
                           max = max(J_scorers$Season_start),
                           value = c(1989, 1999), sep = '',
                           width = '900px')
        )
        
    ),
    
    # Show the ggplot

    plotOutput(outputId = 'goals_plot', height = '600px', width = 'auto'),
    
    hr(),
    
    # Define a shinyWidgets input and a numeric input to subset the reactive data frame

    fluidRow(
        
        column(width = 3,
               pickerInput(inputId = 'Comps', 
                           label = 'Select competitions:', 
                           choices = unique(J_scorers$Competition), 
                           selected = c('Serie A', 'Coppa Italia'), inline = FALSE,
                           options = list(
                               `actions-box` = TRUE, 
                               size = 10, 
                               `selected-text-format` = 'count > 3'
                           ),
                           multiple = TRUE
                        ),
               
               ),
        

         column(width = 3,
                numericInput(inputId = 'Top_n_scorers',
                             label = "Select top 'n' scorers to label:",
                value = 1,
                min = 0, max = 5, step = 1)
                )

    ),
    

    # Provide a link to the code

    headerPanel(
        h4('Code available at: https://github.com/DomDF',
           style = "font-family: 'Bahnschrift', cursive;
        font-size = 80%; font-weight: 100; line-height: 0.6;
        color: #000000;")
    ),
    
)

# Define server logic

server <- function(input, output) {
    
    # Create a reactive dataframe based on the slider and picker inputs for selecting seasons and competitions
    
    J_scorers_plot <- reactive({
        J_scorers %>% 
            mutate(Position = factor(x = Position,
                                     levels = c('Centre-Back', 'Defensive Midfield', 'Midfielder', 
                                                'Central Midfield', 'Left Midfield', 'Right Midfield', 
                                                'Left Winger', 'Right Winger', 'Attacking Midfield', 
                                                'Second Striker', 'Forward', 'Centre-Forward'))) %>% 
            dplyr::filter(Season_start >= input$Seasons[1] & 
                              Season_start <= input$Seasons[2]) %>% 
            dplyr::filter(Competition %in% input$Comps) %>% 
            group_by(Competition)
    })
    
    # Defube an additional reactive dataframe based on the numeric input for selecting labels
    
    label_df <- reactive({
        J_scorers_plot() %>%
            group_by(Competition) %>% 
            top_n(n = input$Top_n_scorers, wt = Goals) %>% 
            ungroup() %>% 
            mutate(label = paste0(Player, ' (', as.character(Goals), ' goals)'))
    })
    
    output$goals_plot <- renderPlot({
        
        # Define the ggplot using the reactive dataframes
        
        ggplot(data = J_scorers_plot())+
            geom_point(mapping = aes(x = as.integer(Season_start), y = Goals, 
                                     col = Position),
                       alpha = 0.6, size = 5)+
            scale_color_viridis_d(direction = -1)+
            ggrepel::geom_text_repel(data = label_df(), 
                                     mapping = aes(x = Season_start, y = Goals, label = label),
                                     size = 5, family = 'Bahnschrift', 
                                     seed = 1008,
                                     vjust = -2, segment.alpha = 0.4)+
            facet_wrap(facets = ~ Competition)+
            ggthemes::theme_base(base_size = 18, base_family = 'Bahnschrift')+
            theme(legend.position = 'top', legend.title = element_blank(),
                  axis.text.x = element_text(angle = 90, size = 14), 
                  plot.background = element_rect(colour = NA))+
            scale_x_continuous(name = 'Season start', breaks = scales::pretty_breaks())+
            labs(caption = '@Domenic_DF  |  Data from transfermarkt.com')

        })
}

# Run the application 
shinyApp(ui = ui, server = server)
