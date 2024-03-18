#
# Created by: https://github.com/chocolatmint
#

library(shiny)

function(input, output, session) {
  shinyjs::enable("country")
  
  # ---- observe ----
  observe({
    if (input$overviewMode == "global") {
      shinyjs::disable("country")
    } else {
      shinyjs::enable("country")
    }
  })
  
  observe({
    sortedArtists <- sort(unique(spotify$artists))
    updateSelectizeInput(session, "artist_name", choices = sortedArtists, server = TRUE)
    updateSelectizeInput(session, "artist_name2", choices = sortedArtists, server = TRUE)
  })
  
  observe({
    artist_data <- spotify %>% filter(artists == input$artist_name)
    max_date <- max(artist_data$snapshot_date)
    updateDateInput(session, "date", value = max_date, min = min(artist_data$snapshot_date), max = max_date)
  })
  
  observe({
    artist_data2 <- spotify %>% 
      filter(artists == input$artist_name2)
    
    artists_country <-  merge(artist_data2, global, by.x = "country", by.y = "code_2", all.x = TRUE) %>%  
      arrange(country.y)
    
    countrySelected <- unique(artists_country$country.y)
    updateSelectInput(session, "countryDropdown", choices = countrySelected)
  })
  
  # ---- globalPlot ----
  output$globalPlot <- renderPlotly({
      top_artists_global <- spotify %>% 
        group_by(artists, country) %>%
        summarise(occurrences = n_distinct(snapshot_date), .groups = "drop") %>%
        group_by(artists) %>%
        summarise(total_countries = n(), .groups = "drop") %>%
        arrange(desc(total_countries)) %>%
        head(10) %>% 
        mutate(labeling = glue("Artists: {artists}
                         Songs played in: {total_countries} countries"))
      
      globalPlot <-ggplot(data = top_artists_global,
                          aes(x = total_countries,
                              y = reorder(artists, total_countries),
                              text = labeling)) +
        geom_col(aes(fill = total_countries)) +
        scale_fill_gradient(low="#C1A1D3", high="#2F58CD") +
        labs(title = "Top 10 Artists Global",
             x = "Number of Countries",
             y = NULL) +
        theme_minimal() +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5))
      
      ggplotly(globalPlot, tooltip = "text")
  })
  
  # ---- countryPlot ----
  output$countryPlot <- renderPlotly({
    top_artists_by_country <- merge(global, spotify, by.x = "code_2", by.y = "country", all.x = TRUE) %>% 
      filter(country == input$country) %>% 
      group_by(artists) %>%
      summarise(avg_popularity = mean(as.numeric(popularity), na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(avg_popularity)) %>%
      head(10) %>% 
      mutate(labeling = glue("Artists: {artists}
                         Average Popularity: {comma(round(avg_popularity, 2))}%"))
    
    countryPlot <- ggplot(top_artists_by_country,
                           aes(x = avg_popularity,
                               y = reorder(artists, avg_popularity),
                               color = avg_popularity,
                               text = labeling)) +
      geom_point(size = 3) +
      geom_segment(aes(x = 0,
                       xend = avg_popularity,
                       y = artists,
                       yend = artists),
                   size = 1.5) +
      scale_color_gradient(low = "#C1A1D3",
                           high = "#2F58CD") +
      scale_x_continuous(labels = comma) +
      labs(title = glue("Top 10 Artists in {input$country}"),
           x = "Popularity (%)",
           y = NULL) +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5))
    
    ggplotly(countryPlot, tooltip = "text")
  })
  
  # ---- artistsPlot ----
  output$artistsPlot <- renderPlotly({
    if (length(input$artist_name) > 0 & length(input$date) > 0 & input$artist_name != "") {
      top_artist_by_date <- spotify %>%
        filter(snapshot_date == input$date & artists == input$artist_name) %>% 
        group_by(country, artists, snapshot_date) %>%
        summarise(popularity = mean(as.numeric(popularity)), .groups = "drop") %>%
        group_by(country, snapshot_date) %>%
        slice_max(order_by = popularity, n = 1) %>%
        ungroup() %>%
        arrange(desc(snapshot_date))
      
      top_artist_by_date_country <- merge(world_map, top_artist_by_date, by.x = "code_2", by.y = "country", all.x = TRUE)
      
      top_artist_by_date_country <- top_artist_by_date_country %>% 
        mutate(
          popularity = ifelse(is.na(popularity), 0, popularity),
          labeling = glue("Country: {country}
                           Popularity: {comma(round(popularity, 2))}%")
        )
      
      l <- list(color = toRGB("#d7d8e8"), width = 0.5)
      
      g <- list(
        showframe = FALSE,
        showcoastlines = FALSE,
        projection = list(type = 'Mercator')
      )
      
      fig <- plot_geo(top_artist_by_date_country) %>% 
        add_trace(
          z = ~popularity, color = ~popularity, colors = 'BuPu',
          text = ~labeling, locations = ~code_3, marker = list(line = l),
          hovertemplate = ~ifelse(popularity == 0, "", "%{text}<extra></extra>")
        ) %>% 
        colorbar(title =  'Popularity', ticksuffix = '%') %>% 
        layout(
          title = glue('Popularity of {input$artist_name} By Country On {format(input$date, "%d %B %Y")}'),
          geo = g
        )
      fig
    }
  })
  
  # ---- rankPlot ----
  output$rankPlot <- renderPlotly({
    if (length(input$artist_name2) > 0 & length(input$countryDropdown) > 0 & input$artist_name2 != "") {
      artist_rank <- spotify %>% 
        filter(artists == input$artist_name2) %>%
        group_by(country, name, snapshot_date) %>%
        arrange(snapshot_date) %>% 
        mutate(
          labeling = glue("Song Title: {name}
                        Date: {snapshot_date}
                        Daily Rank: {daily_rank}"))
      
      artist_rank_by_country <- merge(global, artist_rank, by.x = "code_2", by.y = "country", all.x = TRUE) %>% 
        filter(country == input$countryDropdown)
      
      palette <- colorRampPalette(c("#FBB454", "#47297B"))
      
      rankPlot <- ggplot(
        artist_rank_by_country,
        aes(
          x = snapshot_date,
          y = daily_rank,
          color = name))+
        geom_line() +
        geom_point(aes(text = labeling)) +
        scale_x_date(date_breaks = "1 month", date_labels = "%d-%m-%Y") +
        scale_y_continuous(guide='none') +
        labs(
          title = glue("Daily Ranking of {input$artists_name}"),
          x = "Date",
          y = "Daily Rank",
          color = "Song Title") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5)) +
        scale_color_manual(values=palette(35))
      
      ggplotly(rankPlot, tooltip = "text")
    }
  })
  
  # ---- data table ----
  sorted_spotify <- spotify[order(spotify$snapshot_date, spotify$country, spotify$name), ]
  
  output$dataset <- renderDataTable(
    sorted_spotify,
    options = list( scrollX = T,
                    scrollY =T)
  )
}
