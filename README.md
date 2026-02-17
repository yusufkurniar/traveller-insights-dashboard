# Traveller Insights Dashboard: Overseas Travel to Australia

You can explore the interactive dashboard online without running any code locally.

An interactive Shiny dashboard analysing overseas visitor patterns to Australia using ABS data.  
This project focuses on exploratory analysis, data-driven insights, and effective visual communication.

🔗 **Live Dashboard:** [Traveller Insights Dashboard](https://yusufkurniar.shinyapps.io/traveller-insights-dashboard/)

## 🔍 Project Overview
The dashboard answers key questions such as:
- Which countries contribute the most and least visitors?
- How do visitor trends change over time?
- What are the main reasons for travel?
- How long do visitors typically stay?

The insights are designed to support tourism planning, marketing strategy, and policy analysis.

## 📊 Features
- Interactive filters (time range, traveller type, top N countries)
- Stacked bar charts (top & bottom visitor countries)
- Animated geographic map of visitor origins
- Time-series trends by duration of stay
- Word cloud of travel reasons
- Automatically generated insights based on filters

## 🛠 Tech Stack
- **R**
- **Shiny**
- **ggplot2**
- **plotly**
- **dplyr**
- **ABS Data Explorer**

## ▶️ How to Run Locally
```r
shiny::runApp()
