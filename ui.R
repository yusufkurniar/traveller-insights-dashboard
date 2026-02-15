# Content: the layout of the overall visualisations.

source("global.R", local = TRUE)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .shiny-output-error { visibility: hidden; }
      .shiny-output-error:before { visibility: hidden; }
      .shiny-bound-output.shiny-plot-output::before {
        display: none !important;
      }

      body {
        font-family: Arial, sans-serif;
      }
      .textbox-area, .form-control, .btn {
        font-family: Arial, sans-serif;
      }
      h1, h2, h3, h4, h5, h6 {
        font-family: Arial, sans-serif;
        font-weight: bold;
      }

      /* Customize year slider angle */
      .js-irs-0 .irs-grid-text {
        transform: rotate(-30deg) translateX(-15px) translateY(10px);
        transform-origin: top left;
        font-size: 10px;
        white-space: nowrap;
      }

      /* Placeholder for future customizations */
      body {
        font-family: Arial, sans-serif;  /* You can customize later */
        padding: 15px;
      }

      h2.dashboard-title {
        font-weight: bold;
        text-align: center;
        margin-top: 20px;
        margin-bottom: 10px;
        font-size: 24px;
      }

      .subtitle-small {
        font-size: 12px;
        text-align: center;
        margin-bottom: 20px;
      }

      .textbox-title {
        font-weight: bold;
        margin-top: 20px;
        margin-bottom: 5px;
      }

      .textbox-area {
        width: 100%;
        height: 220px;
        padding: 10px;
        border: 1px solid #ccc;
        border-radius: 4px;
        font-family: inherit;
      }
      
      .footer-box {
        border: 1px solid #ccc;
        border-radius: 5px;
        padding: 15px;
        margin-top: 40px;
        background-color: #f9f9f9;
      }  
      
      /* Make <details><summary> look clickable + add chevron */
      details > summary {
        cursor: pointer;
        padding: 10px 12px;
        border: 1px solid #ddd;
        border-radius: 6px;
        background: #f7f7f7;
        user-select: none;
      }
      
      /* Remove default marker */
      details > summary::-webkit-details-marker { display: none; }
      details > summary { list-style: none; }
      
      /* Chevron icon */
      details > summary::after {
        content: \"▾\";
        float: right;
        font-size: 16px;
        margin-left: 10px;
        color: #555;
      }
      
      details[open] > summary::after {
        content: \"▴\";
      }
      
      /* hint text style */
      .details-hint {
        font-size: 12px;
        color: #777;
        margin-top: 6px;
      }
    "))
  ),
  
  # Project Title
  div(
    h1("Analysis of Overseas Travelling in Australia", style = "text-align: center; font-weight: bold;"),
    div("Yusuf Kurnia Romadhon | Data Analytics Portfolio", class = "subtitle-small")
  ),
  
  # Opening Sentences Section (collapsible)
  fluidRow(
    div(class = "footer-box",
        tags$details(
          tags$summary(tags$strong("Opening Sentences")),
          div(class = "details-hint", "Click to expand / collapse"),
          div(style = "white-space: pre-wrap; color: #333; font-size: 14px;",
              "• Australia is one of the most popular tourist destinations in the world, especially in 2024, when it was in the top five (Woodley, 2024).
            
• One way to maximise tourism in Australia is by analysing overseas arrivals and departures data, which may help organisations/people involved.
             
• As someone interested in travelling, I created a visualisation tool about traveller information in Australia through this project by answering these questions:
  1. What are the most common reasons for overseas people to visit Australia?
  2. What is the current visitor's condition in Australia?
  3. What are the sources of international tourism in Australia?
  4. How does the number of visitors vary over time?
                
• The objective of this project is to help people/organisations in Australia strategise how they will advertise tourism internationally.
             
• Specifically, the audience for this project is Tourism Officials, Travel Agents, Tourism Marketing Analysts, or professionals related.
             
• The focus on the visualisations is that the visualisation can provide the number of visitors based on:
  1. The top and bottom origin countries.
  2. Period category of visitors staying in Australia.
  3. Potential countries near the top and bottom origin countries.
  4. Reason for visiting Australia.
                
• The data used for this project is based on the data from ABS Statistics, and the period starts from 2018."
          )
        )
    )
  ),
  
  # User Guide Section (collapsible)
  fluidRow(
    div(class = "footer-box",
        tags$details(
          tags$summary(tags$strong("User Guide")),
          div(class = "details-hint", "Click to expand / collapse"),
          div(style = "white-space: pre-wrap; color: #333; font-size: 14px;",
              "• Filters:
  1. Year Range: Filters all visualisations (year-month slider).
  2. N Countries: Top/bottom country count (recommended ≤ 10).
  3. Traveller Type: Applies to all visualisations.
  4. Show Average Instead of Total: Switches sum to average.
  5. Clear All: Resets filters.
  6. Period Play: Map-only animation (month-year).

• Visualisations:
  1. Top Visitors (stacked bar)
  2. Bottom Visitors (stacked bar)
  3. Choropleth Map (origin distribution + nearby countries)
  4. Duration Stay (multi-line trend)
  5. Reason Visit (word cloud)

• Description boxes:
  1. Explain each chart.
  2. Auto insights update based on filters."
          )
        )
    )
  ),
  
  # Dashboard Title
  tags$div(style = "margin-top: 30px;"),
  h2("Traveller Insights Dashboard", class = "dashboard-title"),
  
  # Filters row
  fluidRow(
    column(4,
           sliderInput("month_range", "Year Range:",
                       min = min(month_seq),
                       max = max(month_seq),
                       value = c(min(month_seq), max(month_seq)),
                       step = 30,
                       timeFormat = "%Y-%m",
                       animate = FALSE,
                       width = "100%")),
    column(2,
           numericInput("top_n", "N Countries:", value = 5, min = 1, max = 10)),
    column(2,
           selectInput("traveller_type", "Traveller Type:",
                       choices = c("All", unique(sc_data$traveller_cat)),
                       selected = "All")),
    column(2,
           checkboxInput("use_avg", "Show Average Instead of Total", value = FALSE)),
    column(2,
           style = "margin-top: 25px;",
           actionButton("clear_all", "Clear All", class = "btn btn-primary", style = "height: 38px; width: 100%;"))
  ),
  
  #Visualisations Row
  fluidRow(
    tags$div(style = "margin-top: 30px;"),
    column(6,
           h4("Top Visitors by Country", style = "font-weight: bold; text-align: center;"),
           plotOutput("top_bar"),
           downloadButton("download_top_bar", "Download Chart")
    ),
    column(6,
           h4("Bottom Visitors by Country", style = "font-weight: bold; text-align: center;"),
           plotOutput("bottom_bar"),
           downloadButton("download_bottom_bar", "Download Chart")
    ),
    tags$div(style = "margin-bottom: 20px;")
  ),
  verbatimTextOutput("bar_text"),
  fluidRow(
    column(12,
           tags$div(style = "margin-top: 30px;"),
           h4("Geographic Distribution of Visitor Origins", style = "text-align: center; font-weight: bold;"),
           plotlyOutput("map_plot", height = "600px", width = "100%", inline = FALSE),
           tags$div(style = "margin-bottom: 20px;")
    )
  ),
  verbatimTextOutput("map_text"),
  fluidRow(
    column(6,
           h4("Visitor Trends by Duration of Stay", style = "font-weight: bold; text-align: center;"),
           plotOutput("duration_line"),
           downloadButton("download_duration_line", "Download Chart"),
           verbatimTextOutput("line_text")
    ),
    column(6,
           h4("Word Cloud of Travel Reasons", style = "font-weight: bold; text-align: center;"),
           plotOutput("reason_wordcloud"),
           downloadButton("download_wordcloud", "Download Chart"),
           verbatimTextOutput("wordcloud_text")
    )
  ),
  
  # Footers Row
  fluidRow(
    div(class = "footer-box",
        h4("Data Sources and Bibliography", style = "font-weight: bold;"),
        p("Data Source Link: ",
          tags$a(href = "https://dataexplorer.abs.gov.au/vis?tm=visitor%20arrivals&pg=0&df[ds]=ABS_ABS_TOPICS&df[id]=OAD_COUNTRY&df[ag]=ABS&df[vs]=1.0.0&pd=2020-06%2C&dq=...M&to[TIME_PERIOD]=false&ly[cl]=TIME_PERIOD&ly[rs]=CAT_TRAVELLER&ly[rw]=COUNTRY_RESID", target = "_blank", "Selected Countries Data")),
        p("Data Source Link: ",
          tags$a(href = "https://dataexplorer.abs.gov.au/vis?tm=visitor%20arrivals&pg=0&df[ds]=ABS_ABS_TOPICS&df[id]=OAD_REASON&df[ag]=ABS&df[vs]=1.0.0&pd=2020-06%2C&dq=....M&ly[cl]=TIME_PERIOD&ly[rs]=CAT_TRAVELLER%2CDURATION&ly[rw]=JOUR_REASON&to[TIME_PERIOD]=false", target = "_blank", "Reason Data")),
        p("Last Access On: ", span("2025-03-17", style = "color: #555;")),
        p(
          "Download Bibliography PDF: ",
          tags$a(href = "bibliography.pdf", target = "_blank", class = "btn btn-primary", "Download")
        )
    )
  )
)

