# Read in the all required libraries

library(shiny); library(shinyWidgets); library(tidyverse); library(extrafont); library(DT)
library(DomDF)

# Define UI

ui <- fluidPage(

    # To display LaTeX-style equations
    withMathJax(),

# Title and subtitle of the app

titlePanel(

    p(
        h2('Prior predictive checks for Bayesian regression.',
           style = "font-family: 'Bahnschrift';
        font-size = 80%; font-weight: 100;
        color: #000000;")

    )

),

# A brief description of the app, and some instructions for use

headerPanel(
    HTML(
        paste(
            h4('Select values for distribution parameters of priors:',
               style = "font-family: 'Bahnschrift';
     font-size = 80%; font-weight: 100;
     color: #000000;"),
            h4('(Assumed to be normally distributed in current implementation)',
               style = "font-family: 'Bahnschrift';
     font-size = 80%; font-weight: 100;
     color: #000000;")
        ))),

    hr(),

# Add user inputs
    fluidRow(

        column(width = 4,

# Add a drop-down box to select from available GLMs
               selectInput(inputId = 'GLM',
                           label = 'Select the generalised linear model:',
                           choices = c('Linear regression',
                                       'Poisson regression',
                                       'Logistic regression',
                                       'GP regression with exp. quad. K'),
                           selected = 'Linear regression',
                           multiple = FALSE,
                           width = '600px'),

# An input for the user to specify how many samples to draw from the priors
               numericInput(inputId = 'n_samples',
                            label = 'Number of prior predictive samples',
                            value = 100, min = 1, max = 1000, step = 1,
                            width = '250px')

        ),

# Provide values for the mean and variance of the intercept (on the linear scale)
        column(width = 4,

               numericInput(inputId = 'mu_alpha',
                           label = 'Mean(\u03b1)',
                           value = 0, min = -100, max = 100, step = 1,
                           width = '250px'),

               numericInput(inputId = 'sigma_alpha',
                           label = 'Std.Dev(\u03b1)',
                           value = 1, min = 1e-3, max = 100, step = 1/10,
                           width = '250px')
        ),

# Provide values for the mean and variance of the slope (on the linear scale)
        column(width = 4,

               numericInput(inputId = 'mu_beta',
                           label = 'Mean(\u03b2)',
                           value = 0, min = -100, max = 100, step = 1,
                           width = '250px'),

               numericInput(inputId = 'sigma_beta',
                           label = 'Std.Dev(\u03b2)',
                           value = 1, min = 1e-3, max = 100, step = 1/10,
                           width = '250px')

               )
        ),

# Present the output corresponding to the equation representation of the model (see 'selection')
    uiOutput(outputId = 'selection'),

    hr(),

# Define the range of inputs to be considered
    fluidRow(column(10,

        sliderInput(inputId = 'x_range',
                    label = 'Select domain range for predictions:',
                    min = -20,
                    max = 20,
                    value = c(0, 10), sep = '',
                    width = '600px')
        )

    ),

# A reminder for the user to check if the model predictions are sensible and not contrary to any available information
    headerPanel(
        h4('Now, do these look reasonable?',
           style = "font-family: 'Bahnschrift';
            font-size = 80%; font-weight: 100; line-height: 0.6;
            color: #000000;")
    ),

# Present the plot
    plotOutput(outputId = 'priors_plot', height = '600px', width = 'auto'),

    hr(),

# Some customisation options for the plot: 'lines' or 'points', and transparency
    fluidRow(

        column(width = 4,

               selectInput(inputId = 'plot_type',
                           label = 'Select the type of plot:',
                           choices = c('Points',
                                       'Lines'),
                           selected = 'Points',
                           multiple = FALSE,
                           width = '400px')

        ),

        column(width = 4,

               numericInput(inputId = 'plot_alpha',
                            label = 'Plot Opacity',
                            value = 1/4, min = 0, max = 1, step = 1/20,
                            width = '250px')

        )

    ),

# Present the data as a data table
    dataTableOutput(outputId = 'priors_table')

)

# Define server logic

server <- function(input, output) {

# Define the functions for each of the GLMs
    logit_fun <- function(x, alpha, beta){
        exp(alpha + beta * x) / (1 + exp(alpha + beta * x))
    }

    pois_fun <- function(x, alpha, beta){
        rpois(n = 1, lambda = exp(alpha + beta * x))
    }

    lin_fun <- function(x, alpha, beta){
        alpha + beta * x
    }

    exp_quad_GP_fun <- function(x, alpha, beta){
        alpha^2 * exp(-1 / (2 * beta^2) * (0 - x)^2)
    }


# Define the equations, and filter based on the selected GLM
    output$selection <- renderUI({

        if (input$GLM == 'Logistic regression') {

            withMathJax(
                helpText('$$log-odds(y) = \\alpha +\\beta \\cdot X$$'))

        } else if (input$GLM == 'Poisson regression') {

            withMathJax(
                helpText('$$\\lambda_{Poisson} = exp(\\alpha +\\beta \\cdot X)$$'))

        } else if (input$GLM == 'GP regression with exp. quad. K') {

            withMathJax(
                helpText('$$K = \\alpha^{2} \\cdot \\exp{ \\bigg( \\frac{-1}{2 \\cdot \\beta^{2}} \\cdot (x_{i} - x_{j})^{2} \\bigg)}$$'))

        } else {

            withMathJax(
                helpText('$$y = \\alpha +\\beta \\cdot X$$'))

        }

    })

# Define a reactive data frame, populated by samples from the selected model
    set.seed(seed = 1008)

    data_df <- reactive({

        data.frame(alpha = rnorm(n = input$n_samples, mean = input$mu_alpha, sd = input$sigma_alpha),
                   beta = rnorm(n = input$n_samples, mean = input$mu_beta, sd = input$sigma_beta),
                   x = seq(from = input$x_range[1], to = input$x_range[2], length.out = input$n_samples))

    })

    plot_df <- reactive({

        if (input$GLM == 'Logistic regression') {
            GLM_fun <- logit_fun
        } else if (input$GLM == 'Poisson regression') {
            GLM_fun <- pois_fun
        } else if (input$GLM == 'GP regression with exp. quad. K') {
            GLM_fun <- exp_quad_GP_fun
        } else {
            GLM_fun <- lin_fun
        }

        y <- double()

        for(i in seq(from = 1, to = nrow(data_df()), by = 1)) {

            y[i] <- GLM_fun(x = data_df()$x[i],
                            alpha = data_df()$alpha[i],
                            beta = data_df()$beta[i])

        }

        data_df() %>%
            mutate(y = y)

    })

# The data frame is converted to a data table for aesthetic reasons
    output$priors_table <- renderDataTable({

        datatable(plot_df())

    })

# Define the ggplot, based on the reactive data frame
    output$priors_plot <- renderPlot({

        if (input$GLM == 'Logistic regression') {
            GLM_fun <- logit_fun
        } else if (input$GLM == 'Poisson regression') {
            GLM_fun <- pois_fun
        } else if (input$GLM == 'GP regression with exp. quad. K') {
            GLM_fun <- exp_quad_GP_fun
        } else {
            GLM_fun <- lin_fun
        }

# Define the base version of the plot
        p <- ggplot(data = plot_df())+
            scale_x_continuous(name = 'Input', limits = c(input$x_range[1], input$x_range[2]))+
            labs(x = 'Input', y = 'Outcome scale')+
            DomDF::theme_ddf_light(base_size = 14)

# Add samples as points, for the 'points' type plot
        if (input$plot_type == 'Points') {

            p +
                geom_point(mapping = aes(x = x, y = y),
                           alpha = input$plot_alpha, size = 4)

        } else if (input$plot_type == 'Lines'){

# Add the samples as functions, for the 'lines' type plot
# Note: perhaps this can be optimised as it can take a while to run for large numbers of samples
            for(i in seq(from = 1, to = input$n_samples, length.out = input$n_samples)){

                p <- p +
                    stat_function(geom = 'line', fun = GLM_fun, alpha = input$plot_alpha,
                                  args = list(alpha = plot_df()$alpha[i], beta = plot_df()$beta[i]))

            }

            p

        }

    })
}

# Run the application
shinyApp(ui = ui, server = server)
