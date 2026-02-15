# Content: reactive logic, visualisations, summaries, and downloads.

source("global.R", local = TRUE)

server <- function(input, output, session) {
  # Make all the filter inputs back to the default values
  observeEvent(input$clear_all, {
    updateSliderInput(session, "month_range", value = c(as.Date("2018-01-01"), as.Date("2025-12-01")))
    updateNumericInput(session, "top_n", value = 5)
    updateSelectInput(session, "traveller_type", selected = "All")
    updateCheckboxInput(session, "use_avg", value = FALSE)
  })
  
  # Validate that N countries filter between 1 and 10.
  #observe({
  #  req(input$top_n)
  #  isolate({
  #   if (is.na(input$top_n) || input$top_n <= 0) {
  #      updateNumericInput(session, "top_n", value = 5)
  #    } else if (input$top_n > 10) {
  #      updateNumericInput(session, "top_n", value = 10)
  #    }
  #  })
  #})
  
  # Color used by traveller types
  traveller_colors <- c(
    "Visitors arriving" = "#1f77b4",
    "Residents returning" = "#ff7f0e"
  )
  
  # Format large numbers to 'K' label, with <1K exception
  to_k_label <- function(x, digits = 1) {
    ifelse(x < 1000, "<1K", paste0(formatC(round(x / 1e3, digits), format = "f", digits = digits, big.mark = ","), "K"))
  }
  
  # Date filter setting based on year and month
  filter_by_period <- function(df) {
    ym_date <- as.Date(paste0(df$year, "-", sprintf("%02d", df$month), "-01"))
    df <- df %>% filter(ym_date >= input$month_range[1], ym_date <= input$month_range[2])
    df
  }
  
  # Filter the dataset based on traveller type and period
  filtered_data <- reactive({
    traveller_filter <- if (input$traveller_type == "All") unique(sc_data$traveller_cat) else input$traveller_type
    
    filter_by_period(sc_data) %>%
      filter(traveller_cat %in% traveller_filter)
  })
  
  # Logic for top N countries based on total/average visitors
  top_countries <- reactive({
    n <- input$top_n
    if (is.null(n) || is.na(n) || n <= 0) return(tibble(country_res_name = character(0), 
                                                        value = numeric(0)))
    
    df <- filtered_data() |>
      group_by(country_res_name) |>
      summarise(
        value = if (input$use_avg) mean(observation_value, 
                                        na.rm = TRUE) else sum(observation_value, 
                                                               na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(desc(value)) |>
      slice_head(n = n)
    
    df
  })
  
  # Logic for bottom N countries based on total/average visitors  
  bottom_countries <- reactive({
    n <- input$top_n
    if (is.null(n) || is.na(n) || n <= 0) return(tibble(country_res_name = character(0), 
                                                        value = numeric(0)))
    
    df <- filtered_data() |>
      group_by(country_res_name) |>
      summarise(
        value = if (input$use_avg) mean(observation_value, 
                                        na.rm = TRUE) else sum(observation_value, 
                                                               na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(value) |>
      slice_head(n = n)
    
    df |> arrange(desc(value))
  })
  
  # Stacked-bar charts rendering logic for top and bottom countries
  render_stacked_bar <- function(data, countries) {
    country_order <- countries
    
    data <- data |>
      group_by(country_res_name, traveller_cat) |>
      summarise(visitors = if (input$use_avg) mean(observation_value, 
                                                   na.rm = TRUE)
                else sum(observation_value, 
                         na.rm = TRUE),
                .groups = "drop") |>
      group_by(country_res_name) |>
      mutate(total = sum(visitors),
             pct = paste0(round(100 * visitors / total, 1), 
                          "%"))
    
    y_label <- if (input$use_avg) "Average Visitors (K)" else "Visitors (K)"
    
    ggplot(data, aes(x = factor(country_res_name, 
                                levels = country_order),
                     y = visitors,
                     fill = traveller_cat)) +
      geom_bar(stat = "identity", 
               position = "stack") +
      
      # Add % labels inside each bar segment
      geom_text(aes(label = pct),
                position = position_stack(vjust = 0.5),
                color = "white",
                size = 3.5,
                fontface = "bold") +
      
      # Add total label on top of each full bar
      geom_text(aes(y = total,
                    label = to_k_label(total, 
                                       digits = 1)),
                vjust = -0.5, size = 3.5) +
      
      scale_fill_manual(values = traveller_colors) +
      labs(x = "Country", y = y_label, 
           fill = "Traveller Type") +
      scale_y_continuous(
        labels = to_k_label,
        expand = expansion(mult = c(0.05, 0.1))
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 16, family = "Arial", face = "bold")
      )
  }
  
  # Display top stacked bar chart
  output$top_bar <- renderPlot({
    df <- filtered_data() |>
      group_by(country_res_name, traveller_cat) |>
      summarise(visitors = if (input$use_avg) mean(observation_value, 
                                                   na.rm = TRUE)
                else sum(observation_value, 
                         na.rm = TRUE), .groups = "drop") |>
      group_by(country_res_name) |>
      summarise(total_visitors = sum(visitors), .groups = "drop") |>
      arrange(desc(total_visitors)) |>
      slice_head(n = input$top_n)
    
    countries <- df$country_res_name
    
    render_stacked_bar(
      filtered_data() |> filter(country_res_name %in% countries),
      countries
    )
  })
  
  # Download top stacked-bar chart
  output$download_top_bar <- downloadHandler(
    filename = function() {"top_bar.png"},
    content = function(file) {
      df <- filtered_data() |>
        group_by(country_res_name, 
                 traveller_cat) |>
        summarise(visitors = if (input$use_avg) mean(observation_value, 
                                                     na.rm = TRUE)
                  else sum(observation_value, na.rm = TRUE), .groups = "drop") |>
        group_by(country_res_name) |>
        summarise(total_visitors = sum(visitors), .groups = "drop") |>
        arrange(desc(total_visitors)) |>
        slice_head(n = input$top_n)
      
      countries <- df$country_res_name
      
      plot_data <- filtered_data() |> filter(country_res_name %in% countries)
      
      plot <- render_stacked_bar(plot_data, countries) +
        ggtitle("Top Visitors Per Country") +
        theme(plot.title = element_text(hjust = 0.5))
      
      ggsave(file, plot = plot, width = 8, height = 5, bg = "white")
    }
  )
  
  # Display bottom stacked-bar charts
  output$bottom_bar <- renderPlot({
    df <- filtered_data() |>
      group_by(country_res_name, traveller_cat) |>
      summarise(visitors = if (input$use_avg) mean(observation_value, 
                                                   na.rm = TRUE)
                else sum(observation_value, na.rm = TRUE), .groups = "drop") |>
      group_by(country_res_name) |>
      summarise(total_visitors = sum(visitors), .groups = "drop") |>
      arrange(total_visitors) |>
      slice_head(n = input$top_n) |>
      arrange(desc(total_visitors))
    
    countries <- df$country_res_name
    
    render_stacked_bar(
      filtered_data() |> filter(country_res_name %in% countries),
      countries
    )
  })
  
  # Download bottom stacked-bar chart
  output$download_bottom_bar <- downloadHandler(
    filename = function() {"bottom_bar.png"},
    content = function(file) {
      df <- filtered_data() %>%
        group_by(country_res_name, traveller_cat) %>%
        summarise(visitors = if (input$use_avg) mean(observation_value, na.rm = TRUE)
                  else sum(observation_value, na.rm = TRUE), .groups = "drop") %>%
        group_by(country_res_name) %>%
        summarise(total_visitors = sum(visitors), .groups = "drop") %>%
        arrange(total_visitors) %>%
        slice_head(n = input$top_n) %>%
        arrange(desc(total_visitors))
      
      countries <- df$country_res_name
      
      plot_data <- filtered_data() %>% filter(country_res_name %in% countries)
      
      plot <- render_stacked_bar(plot_data, countries) +
        ggtitle("Bottom Visitors Per Country") +
        theme(plot.title = element_text(hjust = 0.5))
      
      ggsave(file, plot = plot, width = 8, height = 5, bg = "white")
    }
  )
  
  # Box description stacked-bar chart
  output$bar_text <- renderText({
    top <- top_countries()
    bottom <- bottom_countries()
    
    top_country <- top$country_res_name[1]
    top_value <- round(top$value[1])
    bottom_country <- bottom$country_res_name[nrow(bottom)]
    bottom_value <- round(bottom$value[nrow(bottom)])
    
    label_type <- if (input$use_avg) "average number of visitors" else "total number of visitors"
    
    paste0(
      "These stacked bar charts display the ", label_type, " per country, broken down by traveller type.\n\n",
      "Based on the current filters:\n",
      "Top contributing country: ", top_country, " with ", format(top_value, big.mark = ","), " visitors.\n",
      "Least contributing country: ", bottom_country, " with ", format(bottom_value, big.mark = ","), " visitors."
    )
  })
  
  # Map rendering logic and visualisation
  output$map_plot <- renderPlotly({
    df <- filter_by_period(sc_data) |>
      group_by(country_res_name, traveller_cat, period, iso3) |>
      summarise(
        visitors = if (input$use_avg) mean(observation_value, na.rm = TRUE)
        else sum(observation_value, na.rm = TRUE),
        .groups = "drop"
      ) |>
      pivot_wider(
        names_from = traveller_cat,
        values_from = visitors,
        values_fill = 0
      ) |>
      mutate(
        visitors_arriving = `Visitors arriving`,
        residents_returning = `Residents returning`,
        total_visitors = visitors_arriving + residents_returning,
        hover_text = paste0(
          "<b>", country_res_name, "</b><br>",
          "🛬 Visitors arriving: ", to_k_label(visitors_arriving), "<br>",
          "🏡 Residents returning: ", to_k_label(residents_returning), "<br>",
          "🌐 Total visitors: ", to_k_label(total_visitors)
        )
      ) |>
      drop_na(iso3)
    
    label_countries <- unique(c(top_countries()$country_res_name, bottom_countries()$country_res_name))
    
    labels_df <- df |>
      filter(country_res_name %in% label_countries) |>
      left_join(label_geo, by = c("iso3" = "iso_a3")) |>
      mutate(
        visitor_text = to_k_label(total_visitors, digits = 1),
        name = country_res_name
      )
    
    plot_geo(df) |>
      add_trace(
        locations = ~iso3,
        z = ~total_visitors,
        color = ~total_visitors,
        colors = "Blues",
        text = ~hover_text,
        frame = ~period,
        hoverinfo = "text",
        type = "choropleth",
        colorbar = list(title = "Total Visitors")
      ) |>
      layout(
        geo = list(
          showframe = FALSE,
          showcoastlines = TRUE,
          projection = list(type = "natural earth"),
          showland = TRUE,
          landcolor = "rgb(240,240,240)",
          hoveron = "fills"
        ),
        height = 600
      ) |>
      add_trace(
        type = "scattergeo",
        mode = "markers",
        data = labels_df,
        lon = ~X,
        lat = ~Y,
        frame = ~period,
        marker = list(
          size = 60,
          color = "#b2f7b2",
          opacity = 0.8,
          line = list(width = 0)
        ),
        hoverinfo = "none",
        showlegend = FALSE,
        inherit = FALSE
      ) |>
      add_trace(
        type = "scattergeo",
        mode = "text",
        data = labels_df,
        text = ~name,
        lon = ~X,
        lat = ~I(Y + 3),
        textfont = list(size = 12, color = "black"),
        textposition = "middle center",
        hoverinfo = "none",
        showlegend = FALSE,
        inherit = FALSE
      ) |>
      add_trace(
        type = "scattergeo",
        mode = "text",
        data = labels_df,
        text = ~visitor_text,
        lon = ~X,
        lat = ~I(Y - 3),
        frame = ~period,
        textfont = list(size = 10, color = "black"),
        textposition = "middle center",
        hoverinfo = "none",
        showlegend = FALSE,
        inherit = FALSE
      )
  })
  
  # Box description of map chart
  output$map_text <- renderText({
    df <- filtered_data() |>
      group_by(country_res_name) |>
      summarise(
        visitors = if (input$use_avg) mean(observation_value, 
                                           na.rm = TRUE)
        else sum(observation_value, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(desc(visitors))
    
    if (nrow(df) == 0) return("No data available for the selected period.")
    
    top_country <- df$country_res_name[1]
    bottom_country <- df$country_res_name[nrow(df)]
    top_value <- format(round(df$visitors[1]), big.mark = ",")
    bottom_value <- format(round(df$visitors[nrow(df)]), big.mark = ",")
    label_type <- if (input$use_avg) "average number of visitors" else "total number of visitors"
    
    # Merge with coordinates
    df_with_coords <- df |>
      left_join(label_geo, by = c("country_res_name" = "name")) |>
      filter(!is.na(X), !is.na(Y))
    
    # Get coordinates of top and bottom countries
    top_coord <- df_with_coords |> filter(country_res_name == top_country) |> slice(1)
    bottom_coord <- df_with_coords |> filter(country_res_name == bottom_country) |> slice(1)
    
    # Get 5 nearest neighbors to top country (excluding itself)
    top_neighbors <- df_with_coords |>
      filter(country_res_name != top_country) |>
      mutate(distance = sqrt((X - top_coord$X)^2 + (Y - top_coord$Y)^2)) |>
      arrange(distance) |>
      slice_head(n = 5)
    
    # Get 5 nearest neighbors to bottom country (excluding itself)
    bottom_neighbors <- df_with_coords |>
      filter(country_res_name != bottom_country) |>
      mutate(distance = sqrt((X - bottom_coord$X)^2 + (Y - bottom_coord$Y)^2)) |>
      arrange(distance) |>
      slice_head(n = 5)
    
    # Format neighbor info
    format_neighbors <- function(df) {
      paste0(
        apply(df, 1, function(row) {
          paste0(row["country_res_name"], " (", format(round(as.numeric(row["visitors"])), big.mark = ","), ")")
        }),
        collapse = ", "
      )
    }
    
    top_near_text <- format_neighbors(top_neighbors)
    bottom_near_text <- format_neighbors(bottom_neighbors)
    
    paste(
      "This map displays the geographic distribution of international visitors by country.\n",
      "Color country area intensity represents the ", label_type, " over the selected date range.",
      "\n\nBased on the current filters:\n",
      "\nTop contributing country: ", top_country, " with ", top_value, " visitors.",
      "\nNearby countries: ", top_near_text, ".",
      "\n\nLeast contributing country: ", bottom_country, " with ", bottom_value, " visitors.",
      "\nNearby countries: ", bottom_near_text, ".",
      sep = ""
    )
  })
  
  # Multi-line chart rendering logic and visualisation
  output$duration_line <- renderPlot({
    rs_filtered <- filter_by_period(rs_data) |>
      filter(duration_stay != "Total")
    
    traveller_filter <- if (input$traveller_type == "All") unique(sc_data$traveller_cat) else input$traveller_type
    
    filtered_raw <- rs_filtered |>
      filter(traveller_cat %in% traveller_filter) |>
      mutate(date = as.Date(paste0(year, "-", sprintf("%02d", month), "-01")))
    
    df <- filtered_raw |>
      group_by(date, duration_stay) |>
      summarise(visitors = if (input$use_avg) mean(observation_value, na.rm = TRUE)
                else sum(observation_value, na.rm = TRUE),
                .groups = "drop")
    
    filtered_dates <- seq(from = input$month_range[1], to = input$month_range[2], by = "month")
    all_df <- expand.grid(date = filtered_dates, duration_stay = unique(df$duration_stay))
    df <- full_join(df, all_df, by = c("date", "duration_stay")) |>
      mutate(visitors = replace_na(visitors, 0))
    
    rects <- tibble::tibble(
      Period = factor(c("Pre-COVID", "COVID", "Post-COVID"), levels = c("Pre-COVID", "COVID", "Post-COVID")),
      xmin = as.Date(c("2018-01-01", "2020-01-01", "2022-01-01")),
      xmax = as.Date(c("2019-12-31", "2021-12-31", "2025-12-31")),
      fill = c("#d0f0fd", "#fde0dd", "#e6f5d0")
    ) |>
      mutate(
        xmin = pmax(xmin, input$month_range[1]),
        xmax = pmin(xmax, input$month_range[2])
      ) |>
      filter(xmin <= xmax)
    
    unique_durations <- sort(unique(df$duration_stay))
    safe_colors <- c("#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
    duration_colors <- setNames(rep(safe_colors, 
                                    length.out = length(unique_durations)), 
                                unique_durations)
    
    peak_point <- df |> filter(visitors == max(visitors, 
                                               na.rm = TRUE))
    low_point <- df |> filter(visitors == min(visitors[visitors > 0], 
                                              na.rm = TRUE))
    
    ggplot() +
      geom_rect(data = rects,
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = Period),
                alpha = 0.3, inherit.aes = FALSE) +
      
      geom_line(data = df, aes(x = date, y = visitors, color = duration_stay), size = 1) +
      
      geom_point(data = peak_point,
                 aes(x = date, y = visitors),
                 size = 3.5,
                 color = duration_colors[peak_point$duration_stay]) +
      geom_point(data = low_point,
                 aes(x = date, y = visitors),
                 size = 3.5,
                 color = duration_colors[low_point$duration_stay]) +
      
      geom_text(data = peak_point,
                aes(x = date, y = visitors,
                    label = to_k_label(visitors, digits = 0)),
                hjust = 0.5, vjust = -1.6,
                color = duration_colors[peak_point$duration_stay],
                fontface = "bold", size = 4.2) +
      
      geom_text(data = low_point,
                aes(x = date, y = visitors,
                    label = to_k_label(visitors, digits = 0)),
                hjust = 0.5, vjust = 2.2,
                color = duration_colors[low_point$duration_stay],
                fontface = "bold", size = 4.2) +
      
      scale_color_manual(values = duration_colors) +
      scale_fill_manual(values = setNames(rects$fill, rects$Period)) +
      
      scale_x_date(
        limits = c(input$month_range[1], input$month_range[2]),
        date_labels = "%b\n%Y",
        date_breaks = "6 months",
        expand = c(0, 0)
      ) +
      scale_y_continuous(
        labels = to_k_label,
        expand = expansion(mult = c(0.05, 0.1))  # add 5% bottom, 10% top space
      ) +
      
      labs(
        x = "Month-Year",
        y = "Number of Visitors",
        color = "Duration of Stay",
        fill = "Period"
      ) +
      
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, family = "Arial", face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        plot.margin = margin(20, 60, 40, 20)  # extra bottom
      ) +
      coord_cartesian(clip = "off")
  })
  
  # Box description of multi-line chart
  output$line_text <- renderText({
    rs_filtered <- filter_by_period(rs_data) |>
      filter(duration_stay != "Total")
    
    traveller_filter <- if (input$traveller_type == "All") unique(sc_data$traveller_cat) else input$traveller_type
    
    df <- rs_filtered |>
      filter(traveller_cat %in% traveller_filter) |>
      mutate(date = as.Date(paste0(year, "-", sprintf("%02d", month), "-01"))) |>
      group_by(date, duration_stay) |>
      summarise(
        visitors = if (input$use_avg) mean(observation_value, na.rm = TRUE)
        else sum(observation_value, na.rm = TRUE),
        .groups = "drop"
      )
    
    peak <- df |> filter(visitors == max(visitors, na.rm = TRUE)) |> slice(1)
    low <- df |> filter(visitors == min(visitors[visitors > 0], na.rm = TRUE)) |> slice(1)
    
    paste0(
      "This line chart visualises changes in visitor numbers\nacross different durations of stay over time.\n\n",
      "Based on the current filters:\n",
      "Peak: ", format(round(peak$visitors), big.mark = ","), " visitors in ", format(peak$date, "%b %Y"), " staying ", peak$duration_stay, ".\n",
      "Lowest: ", format(round(low$visitors), big.mark = ","), " visitors in ", format(low$date, "%b %Y"), " staying ", low$duration_stay, "."
    )
  })
  
  # Download multi-line chart
  output$download_duration_line <- downloadHandler(
    filename = function() {"duration_line.png"},
    content = function(file) {
      df <- filter_by_period(rs_data) |>
        filter(duration_stay != "Total") |>
        left_join(sc_data |> select(time_period, traveller_cat) |> distinct(), by = "time_period") |>
        group_by(year, duration_stay) |>
        summarise(visitors = sum(observation_value, 
                                 na.rm = TRUE), .groups = "drop")
      
      p <- ggplot(df, aes(x = year, y = visitors, color = duration_stay)) +
        geom_line(size = 1) +
        labs(
          title = "Trends in Visitor Duration Over Time",
          x = "Year",
          y = "Number of Visitors",
          color = "Duration of Stay"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(family = "Arial"),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
      
      ggsave(file, plot = p, width = 8, height = 5, bg = "white")
    }
  )
  
  # Word cloud chart rendering logic and visualisation
  output$reason_wordcloud <- renderPlot({
    library(wordcloud)
    library(RColorBrewer)
    
    df <- filter_by_period(rs_data) |>
      filter(!is.na(reason_journey), reason_journey != "Total") |>
      filter(if (input$traveller_type == "All") TRUE else traveller_cat %in% input$traveller_type) |>
      group_by(reason_journey) |>
      summarise(visitors = if (input$use_avg) mean(observation_value, 
                                                   na.rm = TRUE) else sum(observation_value, 
                                                                          na.rm = TRUE), .groups = "drop")
    
    if (nrow(df) == 0 || all(df$visitors == 0)) return(NULL)  #prevent crash on empty data
    
    pal <- brewer.pal(8, "Spectral")
    wordcloud(words = df$reason_journey,
              freq = df$visitors,
              min.freq = 1,
              random.order = FALSE,
              colors = pal,
              scale = c(4, 0.8))
  })
  
  # Download word cloud chart
  output$download_wordcloud <- downloadHandler(
    filename = function() {"reason_wordcloud.png"},
    content = function(file) {
      png(file, width = 800, height = 500)
      par(mar = c(2, 2, 4, 2))  # allow space for title
      df <- filter_by_period(rs_data) %>%
        filter(!is.na(reason_journey), reason_journey != "Total") %>%
        filter(if (input$traveller_type == "All") TRUE else traveller_cat %in% input$traveller_type) %>%
        group_by(reason_journey) %>%
        summarise(visitors = sum(observation_value, na.rm = TRUE), .groups = "drop")
      
      pal <- brewer.pal(8, "Spectral")
      
      wordcloud(
        words = df$reason_journey,
        freq = df$visitors,
        min.freq = 1,
        random.order = FALSE,
        colors = pal,
        scale = c(4, 0.8)
      )
      
      title("Reasons for Travel to Australia", cex.main = 1.6, font.main = 2)
      
      dev.off()
    }
  )
  
  # Box description of word cloud chart
  output$wordcloud_text <- renderText({
    df <- filter_by_period(rs_data) |>
      filter(!is.na(reason_journey), reason_journey != "Total") |>
      filter(if (input$traveller_type == "All") TRUE else traveller_cat %in% input$traveller_type) |>
      group_by(reason_journey) |>
      summarise(visitors = if (input$use_avg) mean(observation_value, na.rm = TRUE) else sum(observation_value, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(visitors))
    
    top <- df[1, ]
    bottom <- df[nrow(df), ]
    
    paste0(
      "This word cloud highlights the most common reasons for travel. \nLarger words represent higher visitor counts.\n\n",
      "Based on the current filters:\n",
      "Most common reason: ", top$reason_journey, " with ", format(round(top$visitors), big.mark = ","), " visitors.\n",
      "Least common reason: ", bottom$reason_journey, " with ", format(round(bottom$visitors), big.mark = ","), " visitors."
    )
  })
  
}

