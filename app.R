# Read in the all required libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(viridis)
library(DT)

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "prior predictive explorer",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    
    # Model selection
    selectInput(
      inputId = 'model_type',
      label = 'model type',
      choices = c(
        'linear regression' = 'linear',
        'logistic regression' = 'logistic', 
        'poisson regression' = 'poisson',
        'gaussian process (rbf)' = 'gp'
      ),
      selected = 'linear'
    ),
    
    # Preset configurations
    selectInput(
      inputId = 'preset',
      label = 'prior presets',
      choices = c(
        'custom' = 'custom',
        'weakly informative' = 'weakly',
        'informative' = 'informative',
        'vague' = 'vague'
      ),
      selected = 'custom'
    ),
    
    hr(),
    
    # Prior parameters
    h4("prior parameters", style = "color: #fff;"),
    
    fluidRow(
      column(6,
        numericInput(
          inputId = 'mu_alpha',
          label = HTML("&mu;<sub>&alpha;</sub>"),
          value = 0,
          step = 0.5
        )
      ),
      column(6,
        numericInput(
          inputId = 'sigma_alpha',
          label = HTML("&sigma;<sub>&alpha;</sub>"),
          value = 1,
          min = 0.01,
          step = 0.1
        )
      )
    ),
    
    fluidRow(
      column(6,
        numericInput(
          inputId = 'mu_beta',
          label = HTML("&mu;<sub>&beta;</sub>"),
          value = 0,
          step = 0.5
        )
      ),
      column(6,
        numericInput(
          inputId = 'sigma_beta',
          label = HTML("&sigma;<sub>&beta;</sub>"),
          value = 1,
          min = 0.01,
          step = 0.1
        )
      )
    ),
    
    hr(),
    
    # Simulation settings
    h4("simulation settings", style = "color: #fff;"),
    
    sliderInput(
      inputId = 'n_samples',
      label = 'number of samples',
      min = 10,
      max = 500,
      value = 50,
      step = 10
    ),
    
    sliderInput(
      inputId = 'x_range',
      label = 'input domain',
      min = -20,
      max = 20,
      value = c(-5, 5),
      step = 0.5
    ),
    
    br(),
    
    # Action buttons
    actionButton(
      inputId = 'generate',
      label = 'generate samples',
      icon = icon('play'),
      class = 'btn-primary',
      width = '100%'
    )
  ),
  
  dashboardBody(
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-top: 3px solid #3c8dbc;
        }
        .info-box {
          min-height: 60px;
        }
        .info-box-icon {
          height: 60px;
          line-height: 60px;
          font-size: 30px;
        }
        .info-box-content {
          padding: 5px 10px;
          margin-left: 60px;
        }
      "))
    ),
    
    fluidRow(
      # Model equation display
      box(
        title = "model specification",
        width = 12,
        status = "primary",
        solidHeader = FALSE,
        uiOutput("model_equation")
      )
    ),
    
    fluidRow(
      # Main visualization
      box(
        title = "prior predictive distribution",
        width = 8,
        status = "primary",
        solidHeader = FALSE,
        plotlyOutput("main_plot", height = "500px"),
        br(),
        conditionalPanel(
          condition = "input.plot_type == 'bands'",
          div(
            class = "alert alert-info",
            style = "font-size: 13px; margin-bottom: 15px;",
            strong("credible bands: "),
            "the shaded regions show where predictions are likely to fall under your prior:",
            tags$ul(
              tags$li("dark band: 50% credible interval (25th to 75th percentiles)"),
              tags$li("light band: 90% credible interval (5th to 95th percentiles)"),
              tags$li("black line: mean of all prior predictions")
            )
          )
        ),
        fluidRow(
          column(4,
            radioButtons(
              inputId = 'plot_type',
              label = 'visualization type',
              choices = c('lines' = 'lines', 'credible bands' = 'bands'),
              selected = 'lines',
              inline = TRUE
            )
          ),
          column(4,
            sliderInput(
              inputId = 'plot_alpha',
              label = 'opacity',
              min = 0.1,
              max = 1,
              value = 0.5,
              step = 0.1
            )
          ),
          column(4,
            downloadButton(
              outputId = 'download_plot',
              label = 'download plot',
              class = 'btn-sm'
            )
          )
        )
      ),
      
      # Prior distributions
      box(
        title = "prior distributions",
        width = 4,
        status = "info",
        solidHeader = FALSE,
        plotlyOutput("prior_dist_plot", height = "500px")
      )
    ),
    
    fluidRow(
      # Summary statistics
      box(
        title = "summary statistics",
        width = 12,
        status = "warning",
        solidHeader = FALSE,
        collapsible = TRUE,
        collapsed = TRUE,
        DTOutput("summary_table")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    data = NULL,
    generated = FALSE
  )
  
  # Update parameters based on preset
  observeEvent(input$preset, {
    if (input$preset == 'weakly') {
      updateNumericInput(session, 'mu_alpha', value = 0)
      updateNumericInput(session, 'sigma_alpha', value = 2.5)
      updateNumericInput(session, 'mu_beta', value = 0)
      updateNumericInput(session, 'sigma_beta', value = 2.5)
    } else if (input$preset == 'informative') {
      updateNumericInput(session, 'mu_alpha', value = 0)
      updateNumericInput(session, 'sigma_alpha', value = 1)
      updateNumericInput(session, 'mu_beta', value = 0)
      updateNumericInput(session, 'sigma_beta', value = 1)
    } else if (input$preset == 'vague') {
      updateNumericInput(session, 'mu_alpha', value = 0)
      updateNumericInput(session, 'sigma_alpha', value = 10)
      updateNumericInput(session, 'mu_beta', value = 0)
      updateNumericInput(session, 'sigma_beta', value = 10)
    }
  })
  
  # Model equation
  output$model_equation <- renderUI({
    eq <- switch(input$model_type,
      'linear' = withMathJax("$$y = \\alpha + \\beta x + \\epsilon, \\quad \\epsilon \\sim N(0, \\sigma^2)$$"),
      'logistic' = withMathJax("$$\\log\\left(\\frac{p}{1-p}\\right) = \\alpha + \\beta x$$"),
      'poisson' = withMathJax("$$\\log(\\lambda) = \\alpha + \\beta x, \\quad y \\sim \\text{Poisson}(\\lambda)$$"),
      'gp' = withMathJax("$$\\text{rbf kernel: } k(x, x') = \\alpha^2 \\exp\\left(-\\frac{(x-x')^2}{2\\beta^2}\\right)\\\\\\text{showing: } k(x, 0)$$")
    )
    
    tagList(
      eq,
      br(),
      p(
        "prior specifications: ",
        HTML(sprintf("&alpha; ~ N(%.1f, %.1f²), &beta; ~ N(%.1f, %.1f²)", 
                    input$mu_alpha, input$sigma_alpha, 
                    input$mu_beta, input$sigma_beta)),
        style = "font-size: 14px; color: #666;"
      )
    )
  })
  
  # Generate data when button is clicked
  observeEvent(input$generate, {
    set.seed(123)
    
    # Sample parameters from priors
    n <- input$n_samples
    alphas <- rnorm(n, input$mu_alpha, input$sigma_alpha)
    betas <- rnorm(n, input$mu_beta, input$sigma_beta)
    
    # Generate x values
    x_seq <- seq(input$x_range[1], input$x_range[2], length.out = 100)
    
    # Generate predictions based on model type
    data_list <- list()
    
    for (i in 1:n) {
      if (input$model_type == 'linear') {
        y <- alphas[i] + betas[i] * x_seq
      } else if (input$model_type == 'logistic') {
        y <- 1 / (1 + exp(-(alphas[i] + betas[i] * x_seq)))
      } else if (input$model_type == 'poisson') {
        lambda <- exp(alphas[i] + betas[i] * x_seq)
        # Sample from Poisson distribution for each x value
        y <- sapply(lambda, function(l) rpois(1, lambda = min(l, 1e6)))
      } else if (input$model_type == 'gp') {
        # RBF kernel function k(x, 0) - covariance with origin
        y <- alphas[i]^2 * exp(-0.5 * x_seq^2 / betas[i]^2)
      }
      
      data_list[[i]] <- data.frame(
        x = x_seq,
        y = y,
        sample_id = i,
        alpha = alphas[i],
        beta = betas[i]
      )
    }
    
    values$data <- bind_rows(data_list)
    values$generated <- TRUE
  })
  
  # Main plot
  output$main_plot <- renderPlotly({
    req(values$generated)
    
    p <- plot_ly()
    
    if (input$plot_type == 'lines') {
      # Plot individual lines
      for (i in unique(values$data$sample_id)) {
        data_i <- values$data[values$data$sample_id == i,]
        # Use points+lines for Poisson to show discrete nature
        if (input$model_type == 'poisson') {
          p <- p %>% add_trace(
            x = data_i$x,
            y = data_i$y,
            type = 'scatter',
            mode = 'lines+markers',
            opacity = input$plot_alpha,
            line = list(color = viridis(input$n_samples)[i], width = 0.5),
            marker = list(size = 3, color = viridis(input$n_samples)[i]),
            showlegend = FALSE,
            hoverinfo = 'skip'
          )
        } else {
          p <- p %>% add_trace(
            x = data_i$x,
            y = data_i$y,
            type = 'scatter',
            mode = 'lines',
            opacity = input$plot_alpha,
            line = list(color = viridis(input$n_samples)[i], width = 1),
            showlegend = FALSE,
            hoverinfo = 'skip'
          )
        }
      }
    } else if (input$plot_type == 'bands') {
      # Plot confidence bands
      summary_data <- values$data %>%
        group_by(x) %>%
        summarise(
          mean_y = mean(y),
          q05 = quantile(y, 0.05),
          q25 = quantile(y, 0.25),
          q75 = quantile(y, 0.75),
          q95 = quantile(y, 0.95)
        )
      
      p <- p %>%
        add_trace(
          x = summary_data$x,
          y = summary_data$q95,
          type = 'scatter',
          mode = 'lines',
          line = list(width = 0),
          showlegend = FALSE,
          hoverinfo = 'skip'
        ) %>%
        add_trace(
          x = summary_data$x,
          y = summary_data$q05,
          type = 'scatter',
          mode = 'lines',
          fill = 'tonexty',
          fillcolor = rgba(viridis(1), input$plot_alpha * 0.2),
          line = list(width = 0),
          name = '90% credible interval',
          legendgroup = '90CI',
          hoverinfo = 'skip'
        ) %>%
        add_trace(
          x = summary_data$x,
          y = summary_data$q75,
          type = 'scatter',
          mode = 'lines',
          line = list(width = 0),
          showlegend = FALSE,
          hoverinfo = 'skip'
        ) %>%
        add_trace(
          x = summary_data$x,
          y = summary_data$q25,
          type = 'scatter',
          mode = 'lines',
          fill = 'tonexty',
          fillcolor = rgba(viridis(1), input$plot_alpha * 0.4),
          line = list(width = 0),
          name = '50% credible interval',
          legendgroup = '50CI',
          hoverinfo = 'skip'
        ) %>%
        add_trace(
          x = summary_data$x,
          y = summary_data$mean_y,
          type = 'scatter',
          mode = 'lines',
          line = list(color = 'black', width = 2),
          name = 'mean prediction',
          hovertemplate = 'x: %{x}<br>y: %{y:.3f}<extra></extra>'
        )
    }
    
    # Layout
    y_label <- switch(input$model_type,
      'linear' = 'y',
      'logistic' = 'probability',
      'poisson' = 'count',
      'gp' = 'k(x, 0)'
    )
    
    p %>% layout(
      xaxis = list(title = 'x'),
      yaxis = list(title = y_label),
      hovermode = 'closest',
      plot_bgcolor = 'white',
      paper_bgcolor = 'white'
    )
  })
  
  # Prior distribution plot
  output$prior_dist_plot <- renderPlotly({
    x_alpha <- seq(input$mu_alpha - 4*input$sigma_alpha, 
                   input$mu_alpha + 4*input$sigma_alpha, 
                   length.out = 100)
    y_alpha <- dnorm(x_alpha, input$mu_alpha, input$sigma_alpha)
    
    x_beta <- seq(input$mu_beta - 4*input$sigma_beta, 
                  input$mu_beta + 4*input$sigma_beta, 
                  length.out = 100)
    y_beta <- dnorm(x_beta, input$mu_beta, input$sigma_beta)
    
    p1 <- plot_ly() %>%
      add_trace(
        x = x_alpha,
        y = y_alpha,
        type = 'scatter',
        mode = 'lines',
        fill = 'tozeroy',
        name = 'α',
        line = list(color = viridis(2)[1])
      ) %>%
      layout(
        xaxis = list(title = 'α'),
        yaxis = list(title = 'density'),
        showlegend = FALSE
      )
    
    p2 <- plot_ly() %>%
      add_trace(
        x = x_beta,
        y = y_beta,
        type = 'scatter',
        mode = 'lines',
        fill = 'tozeroy',
        name = 'β',
        line = list(color = viridis(2)[2])
      ) %>%
      layout(
        xaxis = list(title = 'β'),
        yaxis = list(title = 'density'),
        showlegend = FALSE
      )
    
    subplot(p1, p2, nrows = 2, shareY = TRUE) %>%
      layout(
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  })
  
  # Summary table
  output$summary_table <- renderDT({
    req(values$generated)
    
    summary_stats <- values$data %>%
      group_by(x) %>%
      summarise(
        mean = round(mean(y), 3),
        sd = round(sd(y), 3),
        `5%` = round(quantile(y, 0.05), 3),
        `25%` = round(quantile(y, 0.25), 3),
        `50%` = round(quantile(y, 0.50), 3),
        `75%` = round(quantile(y, 0.75), 3),
        `95%` = round(quantile(y, 0.95), 3)
      ) %>%
      filter(row_number() %% 10 == 1)  # Show every 10th point
    
    datatable(
      summary_stats,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('csv', 'excel')
      ),
      class = 'cell-border stripe'
    )
  })
  
  # Download handler
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("prior_predictive_", input$plot_type, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Create static ggplot version for download
      req(values$generated)
      
      # Get y-axis label based on model type
      y_label <- switch(input$model_type,
        'linear' = 'y',
        'logistic' = 'probability',
        'poisson' = 'count',
        'gp' = 'k(x, 0)'
      )
      
      if (input$plot_type == 'lines') {
        # Create lines plot
        p <- ggplot(values$data, aes(x = x, y = y, group = sample_id)) +
          geom_line(alpha = input$plot_alpha, color = viridis(1)) +
          theme_minimal() +
          labs(x = "x", y = y_label, title = "prior predictive distribution (lines)") +
          theme(
            plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 12),
            panel.grid.minor = element_blank()
          )
      } else {
        # Create credible bands plot
        summary_data <- values$data %>%
          group_by(x) %>%
          summarise(
            mean_y = mean(y),
            q05 = quantile(y, 0.05),
            q25 = quantile(y, 0.25),
            q75 = quantile(y, 0.75),
            q95 = quantile(y, 0.95)
          )
        
        p <- ggplot(summary_data, aes(x = x)) +
          geom_ribbon(aes(ymin = q05, ymax = q95, fill = "90% credible interval"), 
                     alpha = input$plot_alpha * 0.2) +
          geom_ribbon(aes(ymin = q25, ymax = q75, fill = "50% credible interval"), 
                     alpha = input$plot_alpha * 0.4) +
          geom_line(aes(y = mean_y, color = "mean prediction"), size = 1) +
          scale_fill_manual(values = c("90% credible interval" = viridis(1), 
                                      "50% credible interval" = viridis(1))) +
          scale_color_manual(values = c("mean prediction" = "black")) +
          theme_minimal() +
          labs(x = "x", y = y_label, title = "prior predictive distribution (credible bands)",
               fill = "Credible Intervals", color = "Mean") +
          theme(
            plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 12),
            panel.grid.minor = element_blank(),
            legend.position = "bottom"
          )
      }
      
      ggsave(file, p, width = 10, height = 6, dpi = 300)
    }
  )
}

# Helper function for rgba colors
rgba <- function(color, alpha) {
  rgb_vals <- col2rgb(color) / 255
  sprintf("rgba(%d, %d, %d, %.2f)", 
          rgb_vals[1] * 255, 
          rgb_vals[2] * 255, 
          rgb_vals[3] * 255, 
          alpha)
}

# Run the application
shinyApp(ui = ui, server = server)