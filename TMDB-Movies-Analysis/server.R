library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)

function(input, output, session) { 
  
  #Reset all filters button
  observeEvent(input$reset_filters, {
    #Reset all filters to default values
    updateSelectInput(session, "lang", selected = "All")
    updateSelectInput(session, "genre", selected = "All")
    updateDateRangeInput(session, "dateRange", 
                         start = min(movies_shiny$release_date), 
                         end = max(movies_shiny$release_date))
  })
  
  #Reactive expression to filter data based on input for Explore The Data (Table)
  filtered_data <- reactive({
    data <- movies_shiny
    
    #Format numbers with commas for thousands and decimals to 2 places
    data = data %>% mutate(vote_count = comma(vote_count), revenue = comma(revenue), 
                               budget = comma(budget), profit = comma(profit),
                               vote_average = round(vote_average,2),
                               popularity = round(popularity,2))
    
    # Filter by language (main_spoken_lang)
    if (input$lang != "All") {
      data <- data %>% filter(main_spoken_lang == input$lang)
    }
    
    # Filter by genre (main_genre)
    if (input$genre != "All") {
      data <- data %>% filter(main_genre == input$genre)
    }
    
    #Filter by production country (main_prod_country)
    if (input$prod_country != "All") {
      data <- data %>% filter(main_prod_country == input$prod_country)
    }
    
    #Filter by date (release_date)
    if (!is.null(input$dateRange) && length(input$dateRange) == 2) {
      start_date <- as.Date(input$dateRange[1])
      end_date <- as.Date(input$dateRange[2])
      
      data <- data %>% filter(release_date >= start_date & release_date <= end_date)
    }
    
    data  # Return the filtered data
  })
  
  #Create table based on filtered data for Explore The Data tab
  output$table <- DT::renderDataTable({
    filtered_data()
  }, options = list(
    scrollY = "400px",
    scrollX = TRUE,
    pageLength = 25,
    autoWidth = TRUE,
    responsive = TRUE,
    scrollCollapse = TRUE,
    columnDefs = list(        # Ensure no columns are hidden
      list(targets = "_all", visible = TRUE)
    ),
    extensions = 'FixedColumns',
    fixedHeader = TRUE
  ))
  
  #Reactive expression to filter data based on input for By Year/Genre tabs (Bar plots)
  filtered_data_by_year_genre <- reactive({
    data <- movies_shiny  # Assuming 'movies_shiny' is your dataset
    
    # Check which tab is selected
    if (input$chart_tabs == "by_year_genre") {
      # Filter by genre (main_genre) only for "By Genre" tab
      if (input$genre2 != "All") {
        data <- data %>% filter(main_genre == input$genre2)
      }
    }
    
    # Filter by year if a year is selected for the "By Year" chart
    if (input$year != "All" && input$chart_tabs == "by_year_genre") {
      data <- data %>% filter(Year == input$year)
    }
    
    data  # Return the filtered data
  })
  
  #Create bar plot for "By Year" tab (x = year, y = movie count, fill = genre)
  output$bar_plot_by_year <- renderPlot({
    data <- filtered_data_by_year_genre()
    
    if (nrow(data) == 0) {
      return(NULL)  # If no data, show nothing
    }
    
    data %>%
      group_by(Year, main_genre) %>%
      summarise(movie_count = n(), .groups = 'drop') %>%
      filter(Year == input$year) %>%
      arrange(movie_count) %>%
      ggplot(aes(x = reorder(main_genre, movie_count), y = movie_count, fill = main_genre)) +  # Reorder x-axis by movie_count
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = 'Number of Movies Per Genre by Year', x = 'Year', y = 'Movie Count') +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.ticks = element_line(color = "black"),
        legend.position = "right"
      ) +
      guides(fill = guide_legend(title = "Genre", title.position = "top", label.position = "right"))
  })
  
  #Create bar plot for "By Genre" tab (x = genre, y = movie count)
  output$bar_plot_by_genre <- renderPlot({
    data <- filtered_data_by_year_genre()
    
    if (nrow(data) == 0) {
      return(NULL)  # If no data, show nothing
    }
    
    # Filter by the selected genre (if not 'All') and group by Year and main_genre
    data %>%
      filter(main_genre == input$genre2 | input$genre2 == "All") %>%
      group_by(Year, main_genre) %>%
      summarise(movie_count = n(), .groups = 'drop') %>%
    #Create the plot using geom_bar with stacked position
    ggplot(aes(x = factor(Year), y = movie_count, fill = main_genre)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = 'Number of Movies Per Genre Over Time', x = 'Year', y = 'Movie Count') +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.ticks = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
      ) +
      scale_x_discrete(
        breaks = seq(from = min(data$Year), to = max(data$Year), by = 5),  #Adjust 'by' for spacing
        labels = function(x) {
          #Customize labels to show fewer years and avoid clutter
          as.character(x)
        },
        expand = c(0.05, 0.05)  #Add padding to the ends of the axis
      ) +
      guides(fill = guide_legend(title = "Genre", title.position = "top", label.position = "right"))
  })
  
  #Create bar plots for Top 10 Genres
  output$top20bybudget <- renderPlot({
    movies_shiny %>%
      group_by(main_genre) %>%
      summarise(Budget = sum(budget, na.rm = TRUE)) %>%
      arrange(desc(Budget)) %>%
      slice_head(n = 20) %>%
      ggplot(aes(x = Budget, y = reorder(main_genre, Budget), fill = main_genre)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Top 10 Genres by Budget", x = "$ in millions", y = "Genre") +
      scale_x_continuous(labels = comma) +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.ticks = element_line(color = "black"),
        legend.position = "right"
      ) +
      guides(fill = guide_legend(title = "Genre", title.position = "top", label.position = "right"))
  })
  
  output$top20byrevenue <- renderPlot({
    movies_shiny %>%
      group_by(main_genre) %>%
      summarise(Revenue = sum(revenue), na.rm = TRUE) %>%
      arrange(desc(Revenue)) %>%
      slice_head(n = 20) %>%
      ggplot(aes(x = Revenue, y = reorder(main_genre, Revenue), fill = main_genre)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Top 10 Genres by Revenue", x = "$ in millions", y = "Genre") +
      scale_x_continuous(labels = comma) +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.ticks = element_line(color = "black"),
        legend.position = "right"
      ) +
      guides(fill = guide_legend(title = "Genre", title.position = "top", label.position = "right"))
  })
  
  output$top20byprofit <- renderPlot({
    movies_shiny %>%
      group_by(main_genre) %>%
      summarise(Profit = sum(profit), na.rm = TRUE) %>%
      arrange(desc(Profit)) %>%
      slice_head(n = 20) %>%
      ggplot(aes(x = Profit, y = reorder(main_genre, Profit), fill = main_genre)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Top 10 Genres by Profit", x = "$ in millions", y = "Genre") +
      scale_x_continuous(labels = comma) +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.ticks = element_line(color = "black"),
        legend.position = "right"
      ) +
      guides(fill = guide_legend(title = "Genre", title.position = "top", label.position = "right"))
  })
  
  output$top20bycount <- renderPlot({
    movies_shiny %>%
      group_by(main_genre) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      slice_head(n = 20) %>%
      ggplot(aes(x = Count, y = reorder(main_genre, Count), fill = main_genre)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Top 10 Genres by Number of Movies", x = "Count", y = "Genre") +
      scale_x_continuous(labels = comma) +
      theme_minimal() + 
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.ticks = element_line(color = "black"),
        legend.position = "right"
      ) +
      guides(fill = guide_legend(title = "Genre", title.position = "top", label.position = "right"))
  })
  
  #Create scatter plot (y = average_rating)
  output$scatterplot <- renderPlot({
    req(input$x_variable)
    
    movies_shiny %>%
      ggplot(aes_string(x = input$x_variable, y = vote_average)) +  # Use aes_string for dynamic variable
      geom_point(alpha = 0.7, color = "blue") +
      labs(
        title = paste("Average Rating vs.", input$x_variable),
        x = input$x_variable,
        y = "Average Rating"
      ) +
      theme_minimal() + 
      scale_x_continuous(labels = scales::comma) +  # Format x-axis
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.ticks = element_line(color = "black"),
        legend.position = "none"
      )
  })
  
  #Violin plot of ratings distribution for each genre
  output$ratings_dist <- renderPlot({
      movies_shiny %>%
      filter(vote_count > 0) %>%
      ggplot(aes(x = main_genre, y = vote_average, fill = main_genre)) +
      geom_violin(trim = FALSE) + 
      labs(title = "Distribution of Average Rating by Genre", 
           x = "Genre", 
           y = "Average Rating") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.ticks = element_line(color = "black"),
        legend.position = "none"
      )
  })
  
  #Summary table of ratings by genre
  output$summary_table <- DT::renderDataTable({
    summary_table
  }, options = list(
    pageLength = 10,
    scrollX = TRUE
  ),
  rownames = FALSE)
  
  #Heatmap of average ratings by genre and year
  output$ratings_heatmap <- renderPlot({
    movies_shiny %>%
      filter(vote_count > 0) %>%
      group_by(main_genre, Year) %>%
      summarise(Average_Rating = mean(vote_average, na.rm = TRUE)) %>%
      ungroup() %>%
      ggplot(aes(x = Year, y = main_genre, fill = Average_Rating)) +
      geom_tile() +
      scale_fill_viridis(option = "plasma") +
      labs(title = "Heatmap of Average Rating by Genre and Year", 
           x = "Year", 
           y = "Genre") +
      theme_minimal()
  })
  
  #Correlation matrix
  output$corr_matrix <- renderDT({
    cor_matrix %>%
      round(2) %>%
      datatable(options = list(pageLength = 5))
  })
  
}

