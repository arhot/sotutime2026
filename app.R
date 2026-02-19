library(shiny)
library(tidyverse)

# Define UI
ui <- fluidPage(
  titlePanel("Otoksen tai näytteen poimintoja"),
  tabsetPanel(
    tabPanel("Perusjoukko",
             sidebarLayout(
               sidebarPanel(
                 numericInput("N", "Perusjoukon koko", value = 600, min = 1),
                 numericInput("proportion", "Osuus perusjoukossa:", value = 0.33, min = 0, max = 1, step = 0.01),
                 numericInput("sample_n", "Otoskoko:", value = 30, min = 1),
                 actionButton("generate", "Poimi otos"),
                 actionButton("draw10", "Poimi 10 otosta")  # added button
               ),
               mainPanel(
                 plotOutput("pointPlot"),
                 div(
                   verbatimTextOutput("sampleProportion"),
                   style = "margin-top: 20px; padding: 20px; border: 2px solid black; border-radius: 10px; background-color: #f9f9f9; font-size: 12em; text-align: center;"
                 ),
                 # small histogram for Perusjoukko panel (initially empty)
                 plotOutput("hist1", height = "160px")
               )
             )
    ),
    tabPanel("Näyte",
             sidebarLayout(
               sidebarPanel(
                 numericInput("N2", "Perusjoukon koko", value = 600, min = 1),
                 numericInput("proportion2", "Osuus perusjoukossa:", value = 0.33, min = 0, max = 1, step = 0.01),
                 numericInput("sample_n2", "Otoskoko:", value = 30, min = 1),
                 actionButton("generate2", "Poimi otos"),
                 actionButton("draw10_2", "Poimi 10 otosta")  # added button
               ),
               mainPanel(
                 plotOutput("pointPlot2"),
                 div(
                   verbatimTextOutput("sampleProportion2"),
                   style = "margin-top: 20px; padding: 20px; border: 2px solid black; border-radius: 10px; background-color: #f9f9f9; font-size: 12em; text-align: center;"
                 ),
                 # small histogram for Näyte panel (initially empty)
                 plotOutput("hist2", height = "160px"),
                 div(
                   verbatimTextOutput("outlineProportion"),
                   style = "margin-top: 20px; padding: 20px; border: 2px solid black; border-radius: 10px; background-color: #e0f7fa; font-size: 12em; text-align: center;"
                 ),
                 div(
                   "Näyte: katkoviivalla alue osoittaa näytteen, josta nyt voidaan poimia.",
                   style = "margin-top: 20px; padding: 20px; border: 2px solid black; border-radius: 10px; background-color: #fff3e0; font-size: 1.5em; text-align: center;"
                 )
               )
             )
    ),
    # New histogram tab: uses chosen settings and recomputes automatically when inputs change
    tabPanel("Histogrammi",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("hist_source", "Lähde", choices = c("Perusjoukko" = "pop1", "Näyte (katkoviiva-alue)" = "sample_region")),
                 numericInput("hist_reps", "Toistokerrat:", value = 500, min = 1, step = 1),
                 helpText("Histogrammi päivitetään automaattisesti kun asetuksia muutetaan.")
               ),
               mainPanel(
                 plotOutput("histPlot"),
                 div(
                   verbatimTextOutput("histSummary"),
                   style = "margin-top: 15px; padding: 10px; border: 1px solid #ccc; border-radius: 6px; background-color: #fafafa;"
                 )
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output) {
  plot_data <- eventReactive(input$generate, {
    N <- input$N
    proportion <- input$proportion
    sample_n <- input$sample_n
    
    # Generate grid
    grid_size <- ceiling(sqrt(N))
    grid <- expand.grid(x = 1:grid_size, y = 1:grid_size) %>%
      slice_head(n = N)
    
    # Assign colors
    n_colored <- round(N * proportion)
    grid <- grid %>%
      mutate(colored = if_else(row_number() <= n_colored, TRUE, FALSE)) %>%
      sample_frac(1) # Shuffle rows
    
    # Sample points for red squares
    sampled_indices <- sample(nrow(grid), size = min(sample_n, nrow(grid)))
    grid <- grid %>%
      mutate(selected = row_number() %in% sampled_indices)
    
    grid
  })
  
  plot_data2 <- eventReactive(input$generate2, {
    N <- input$N2
    proportion <- input$proportion2
    sample_n <- input$sample_n2
    
    # Generate grid
    grid_size <- ceiling(sqrt(N))
    grid <- expand.grid(x = 1:grid_size, y = 1:grid_size) %>%
      slice_head(n = N)
    
    # Assign colors
    n_colored <- round(N * proportion)
    grid <- grid %>%
      mutate(colored = if_else(row_number() <= n_colored, TRUE, FALSE)) %>%
      sample_frac(1) # Shuffle rows
    
    # Apply dashed outline region (40% x-axis, 80% y-axis)
    grid <- grid %>%
      mutate(dashed_region = x <= grid_size * 0.4 & y <= grid_size * 0.8)
    
    # Sample only from dashed region
    eligible_indices <- which(grid$dashed_region)
    sampled_indices <- sample(eligible_indices, size = min(sample_n, length(eligible_indices)))
    grid <- grid %>%
      mutate(selected = row_number() %in% sampled_indices)
    
    # Calculate bounding box for dashed region
    dashed_bbox <- grid %>% filter(dashed_region) %>%
      summarise(xmin = min(x) - 0.5, xmax = max(x) + 0.5, ymin = min(y) - 0.5, ymax = max(y) + 0.5)
    
    list(grid = grid, dashed_bbox = dashed_bbox)
  })
  
  output$pointPlot <- renderPlot({
    grid <- plot_data()
    
    ggplot(grid, aes(x = x, y = y)) +
      geom_point(aes(color = colored), size = 3, shape = 21, fill = if_else(grid$colored, "blue", "gray")) +
      geom_rect(
        data = grid %>% filter(selected),
        aes(xmin = x - 0.5, xmax = x + 0.5, ymin = y - 0.5, ymax = y + 0.5),
        color = "red", fill = NA, size = 1
      ) +
      coord_fixed() +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank()) +
      scale_color_manual(name = "Vastaus", values = c("TRUE" = "blue", "FALSE" = "gray"), labels = c("TRUE" = "Kyllä", "FALSE" = "Ei"))
  })
  
  output$pointPlot2 <- renderPlot({
    data <- plot_data2()
    grid <- data$grid
    dashed_bbox <- data$dashed_bbox
    
    ggplot(grid, aes(x = x, y = y)) +
      geom_point(aes(color = colored), size = 3, shape = 21, fill = if_else(grid$colored, "blue", "gray")) +
      geom_rect(
        aes(xmin = dashed_bbox$xmin, xmax = dashed_bbox$xmax, ymin = dashed_bbox$ymin, ymax = dashed_bbox$ymax),
        color = "black", linetype = "dashed", fill = NA, size = 1
      ) +
      geom_rect(
        data = grid %>% filter(selected),
        aes(xmin = x - 0.5, xmax = x + 0.5, ymin = y - 0.5, ymax = y + 0.5),
        color = "red", fill = NA, size = 1
      ) +
      coord_fixed() +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank()) +
      scale_color_manual(name = "Vastaus", values = c("TRUE" = "blue", "FALSE" = "gray"), labels = c("TRUE" = "Kyllä", "FALSE" = "Ei"))
  })
  
  output$sampleProportion <- renderText({
    grid <- plot_data()
    n_colored_sample <- grid %>% filter(selected & colored) %>% nrow()
    n_sample <- grid %>% filter(selected) %>% nrow()
    
    paste("Osuus otoksessa:", round(n_colored_sample / n_sample, 2))
  })
  
  output$sampleProportion2 <- renderText({
    grid <- plot_data2()$grid
    n_colored_sample <- grid %>% filter(selected & colored) %>% nrow()
    n_sample <- grid %>% filter(selected) %>% nrow()
    
    paste("Osuus otoksessa:", round(n_colored_sample / n_sample, 2))
  })
  
  output$outlineProportion <- renderText({
    grid <- plot_data2()$grid
    n_colored_dashed <- grid %>% filter(dashed_region & colored) %>% nrow()
    n_dashed <- grid %>% filter(dashed_region) %>% nrow()
    
    paste("Osuus näytteessä:", round(n_colored_dashed / n_dashed, 2))
  })
  
  # New reactive: simulate many repeated samples and return vector of sample proportions
  hist_proportions <- reactive({
    req(input$hist_reps, input$hist_source)
    
    reps <- as.integer(input$hist_reps)
    source <- input$hist_source
    
    run_sims <- function(N, proportion, sample_n, reps, dashed = FALSE) {
      grid_size <- ceiling(sqrt(N))
      # precompute grid indices and dashed region if needed
      grid <- expand.grid(x = 1:grid_size, y = 1:grid_size) %>% slice_head(n = N)
      n_colored <- round(N * proportion)
      
      # indices for dashed region if requested
      if (dashed) {
        dashed_idx <- which(grid$x <= grid_size * 0.4 & grid$y <= grid_size * 0.8)
      } else {
        dashed_idx <- seq_len(N)
      }
      
      # for each repetition, assign colors then sample without replacement from eligible indices
      sapply(seq_len(reps), function(i) {
        # create a logical vector of colored positions (shuffle by sampling indices)
        # We can sample a permutation of indices and mark first n_colored as colored
        perm <- sample.int(N)
        colored_idx <- perm[seq_len(n_colored)]
        # choose eligible for sampling
        eligible <- dashed_idx
        k <- min(sample_n, length(eligible))
        if (k == 0) return(NA_real_)
        sampled <- sample(eligible, size = k)
        mean(sampled %in% colored_idx)
      })
    }
    
    if (source == "pop1") {
      run_sims(input$N, input$proportion, input$sample_n, reps, dashed = FALSE)
    } else {
      # use dashed region sampling consistent with second panel
      run_sims(input$N2, input$proportion2, input$sample_n2, reps, dashed = TRUE)
    }
  })
  
  output$histPlot <- renderPlot({
    props <- hist_proportions()
    props <- props[!is.na(props)]
    req(length(props) > 0)
    
    tibble(prop = props) %>%
      ggplot(aes(x = prop)) +
      geom_histogram(binwidth = 1/40, color = "black", fill = "#56B4E9") +
      geom_vline(xintercept = mean(props), color = "red", linetype = "dashed", size = 1) +
      scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
      labs(x = "Osuus otoksessa", y = "Lukumäärä", title = "Histogrammi otososuuksista") +
      theme_minimal()
  })
  
  output$histSummary <- renderText({
    props <- hist_proportions()
    props <- props[!is.na(props)]
    req(length(props) > 0)
    paste0("Keskiarvo: ", round(mean(props), 3), "    Sd: ", round(sd(props), 3))
  })
  
  # --- ADDED: helper functions required by draw10 / draw10_2 ---
  sample_once_pop <- function(N, proportion, sample_n) {
    grid_size <- ceiling(sqrt(N))
    grid <- expand.grid(x = 1:grid_size, y = 1:grid_size) %>% slice_head(n = N)
    n_colored <- round(N * proportion)
    grid <- grid %>% mutate(colored = if_else(row_number() <= n_colored, TRUE, FALSE)) %>% sample_frac(1)
    sampled_indices <- sample(nrow(grid), size = min(sample_n, nrow(grid)))
    grid <- grid %>% mutate(selected = row_number() %in% sampled_indices)
    n_colored_sample <- grid %>% filter(selected & colored) %>% nrow()
    n_sample <- grid %>% filter(selected) %>% nrow()
    mean_prop <- if (n_sample > 0) n_colored_sample / n_sample else NA_real_
    list(grid = grid, mean = mean_prop)
  }
  
  sample_once_region <- function(N, proportion, sample_n) {
    grid_size <- ceiling(sqrt(N))
    grid <- expand.grid(x = 1:grid_size, y = 1:grid_size) %>% slice_head(n = N)
    n_colored <- round(N * proportion)
    grid <- grid %>% mutate(colored = if_else(row_number() <= n_colored, TRUE, FALSE)) %>% sample_frac(1)
    grid <- grid %>% mutate(dashed_region = x <= grid_size * 0.4 & y <= grid_size * 0.8)
    eligible_indices <- which(grid$dashed_region)
    sampled_indices <- if (length(eligible_indices) == 0) integer(0) else sample(eligible_indices, size = min(sample_n, length(eligible_indices)))
    grid <- grid %>% mutate(selected = row_number() %in% sampled_indices)
    dashed_bbox <- grid %>% filter(dashed_region) %>% summarise(xmin = min(x) - 0.5, xmax = max(x) + 0.5, ymin = min(y) - 0.5, ymax = max(y) + 0.5)
    n_colored_sample <- grid %>% filter(selected & colored) %>% nrow()
    n_sample <- grid %>% filter(selected) %>% nrow()
    mean_prop <- if (n_sample > 0) n_colored_sample / n_sample else NA_real_
    list(grid = grid, dashed_bbox = dashed_bbox, mean = mean_prop)
  }
  # --- end added helpers ---
  
  # store per-panel histogram data (means appended after each draw) and last-grid for display
  rv <- reactiveValues(
    hist1 = numeric(0),   # Perusjoukko means history
    hist2 = numeric(0),   # Näyte means history
    last_grid1 = NULL,    # last sampled grid for Perusjoukko (to show after draw10)
    last_grid2 = NULL     # last sampled grid+bbox for Näyte (to show after draw10)
  )

  # reset hist1 and last_grid1 when Perusjoukko settings change
  observeEvent(list(input$N, input$proportion, input$sample_n), {
    rv$hist1 <- numeric(0)
    rv$last_grid1 <- NULL
  }, ignoreInit = FALSE)

  # reset hist2 and last_grid2 when Näyte settings change
  observeEvent(list(input$N2, input$proportion2, input$sample_n2), {
    rv$hist2 <- numeric(0)
    rv$last_grid2 <- NULL
  }, ignoreInit = FALSE)

  # when user clicks single "Poimi otos" in Perusjoukko: append the sample mean and store that grid
  observeEvent(input$generate, {
    # use the eventReactive result (plot_data) which is triggered by input$generate
    grid <- plot_data()
    n_colored_sample <- grid %>% filter(selected & colored) %>% nrow()
    n_sample <- grid %>% filter(selected) %>% nrow()
    mean_prop <- if (n_sample > 0) n_colored_sample / n_sample else NA_real_
    rv$hist1 <- c(rv$hist1, mean_prop)
    rv$last_grid1 <- grid
  })

  # when user clicks single "Poimi otos" in Näyte: append the sample mean and store that grid + bbox
  observeEvent(input$generate2, {
    data <- plot_data2()
    grid <- data$grid
    dashed_bbox <- data$dashed_bbox
    n_colored_sample <- grid %>% filter(selected & colored) %>% nrow()
    n_sample <- grid %>% filter(selected) %>% nrow()
    mean_prop <- if (n_sample > 0) n_colored_sample / n_sample else NA_real_
    rv$hist2 <- c(rv$hist2, mean_prop)
    rv$last_grid2 <- list(grid = grid, dashed_bbox = dashed_bbox)
  })

  # when user clicks "draw10" in Perusjoukko: do 10 samples, append means, store last grid for display
  observeEvent(input$draw10, {
    req(input$N, input$proportion, input$sample_n)
    reps <- 10L
    means <- numeric(0)
    last_grid <- NULL
    for (i in seq_len(reps)) {
      s <- sample_once_pop(input$N, input$proportion, input$sample_n)
      means <- c(means, s$mean)
      last_grid <- s$grid
    }
    rv$hist1 <- c(rv$hist1, means)
    rv$last_grid1 <- last_grid
  })

  # when user clicks "draw10_2" in Näyte: do 10 samples from dashed region, append means, store last grid+bbox
  observeEvent(input$draw10_2, {
    req(input$N2, input$proportion2, input$sample_n2)
    reps <- 10L
    means <- numeric(0)
    last_grid <- NULL
    last_bbox <- NULL
    for (i in seq_len(reps)) {
      s <- sample_once_region(input$N2, input$proportion2, input$sample_n2)
      means <- c(means, s$mean)
      last_grid <- s$grid
      last_bbox <- s$dashed_bbox
    }
    rv$hist2 <- c(rv$hist2, means)
    rv$last_grid2 <- list(grid = last_grid, dashed_bbox = last_bbox)
  })

  # modify rendering to prefer last_grid when present
  output$pointPlot <- renderPlot({
    grid_to_plot <- NULL
    if (!is.null(rv$last_grid1)) {
      grid_to_plot <- rv$last_grid1
    } else {
      grid_to_plot <- plot_data()
    }
    ggplot(grid_to_plot, aes(x = x, y = y)) +
      geom_point(aes(color = colored), size = 3, shape = 21, fill = if_else(grid_to_plot$colored, "blue", "gray")) +
      geom_rect(
        data = grid_to_plot %>% filter(selected),
        aes(xmin = x - 0.5, xmax = x + 0.5, ymin = y - 0.5, ymax = y + 0.5),
        color = "red", fill = NA, size = 1
      ) +
      coord_fixed() +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank()) +
      scale_color_manual(name = "Vastaus", values = c("TRUE" = "blue", "FALSE" = "gray"), labels = c("TRUE" = "Kyllä", "FALSE" = "Ei"))
  })

  output$pointPlot2 <- renderPlot({
    # prefer rv$last_grid2 if present
    if (!is.null(rv$last_grid2)) {
      grid <- rv$last_grid2$grid
      dashed_bbox <- rv$last_grid2$dashed_bbox
    } else {
      data <- plot_data2()
      grid <- data$grid
      dashed_bbox <- data$dashed_bbox
    }

    ggplot(grid, aes(x = x, y = y)) +
      geom_point(aes(color = colored), size = 3, shape = 21, fill = if_else(grid$colored, "blue", "gray")) +
      geom_rect(
        aes(xmin = dashed_bbox$xmin, xmax = dashed_bbox$xmax, ymin = dashed_bbox$ymin, ymax = dashed_bbox$ymax),
        color = "black", linetype = "dashed", fill = NA, size = 1
      ) +
      geom_rect(
        data = grid %>% filter(selected),
        aes(xmin = x - 0.5, xmax = x + 0.5, ymin = y - 0.5, ymax = y + 0.5),
        color = "red", fill = NA, size = 1
      ) +
      coord_fixed() +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank()) +
      scale_color_manual(name = "Vastaus", values = c("TRUE" = "blue", "FALSE" = "gray"), labels = c("TRUE" = "Kyllä", "FALSE" = "Ei"))
  })

  # render small histogram for Perusjoukko panel
  output$hist1 <- renderPlot({
    props <- rv$hist1
    # show a simple "no data" message when empty
    if (length(props) == 0) {
      ggplot() + xlim(0,1) + ylim(0,1) +
        annotate("text", x = 0.5, y = 0.5, label = "Ei dataa", size = 6) +
        theme_void()
    } else {
      tibble(prop = props) %>%
        ggplot(aes(x = prop)) +
        geom_histogram(binwidth = 1/40, color = "black", fill = "#D55E00") +
        geom_vline(xintercept = mean(props, na.rm = TRUE), color = "red", linetype = "dashed") +
        scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
        labs(x = "Osuus", y = "Lukumäärä", title = "Aiemmat otokset (Perusjoukko)") +
        theme_minimal()
    }
  })

  # render small histogram for Näyte panel
  output$hist2 <- renderPlot({
    props <- rv$hist2
    if (length(props) == 0) {
      ggplot() + xlim(0,1) + ylim(0,1) +
        annotate("text", x = 0.5, y = 0.5, label = "Ei dataa", size = 6) +
        theme_void()
    } else {
      tibble(prop = props) %>%
        ggplot(aes(x = prop)) +
        geom_histogram(binwidth = 1/40, color = "black", fill = "#009E73") +
        geom_vline(xintercept = mean(props, na.rm = TRUE), color = "red", linetype = "dashed") +
        scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
        labs(x = "Osuus", y = "Lukumäärä", title = "Aiemmat otokset (Näyte)") +
        theme_minimal()
    }
  })
  
  # ...existing code for large histogram tab (histPlot, histSummary) ...
}  # end server

# Run the application 
shinyApp(ui = ui, server = server)
