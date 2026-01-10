# app.R — Sydney Stations Foot Traffic (CSV-backed, with debug previews & export)
# ------------------------------------------------------------------------------

# Packages
library(shiny)
library(bslib)
library(tidyverse)   # dplyr, tidyr, readr, etc.
library(lubridate)
library(plotly)
library(DT)
library(scales)
library(janitor)
library(wordcloud2)
library(readxl)

# ---------- Helpers ----------
# 2-decimal abbreviated number: 1,234 -> 1.23K, 12,345,678 -> 12.35M, etc.
fmt_big <- function(x) {
  if (length(x) == 0 || is.na(x) || !is.finite(x)) return("0.00")
  s  <- ifelse(x < 0, "-", "")
  ax <- abs(x)
  if (ax >= 1e9)  return(paste0(s, sprintf("%.2fB", ax / 1e9)))  # billions
  if (ax >= 1e6)  return(paste0(s, sprintf("%.2fM", ax / 1e6)))  # millions
  if (ax >= 1e3)  return(paste0(s, sprintf("%.2fK", ax / 1e3)))  # thousands
  paste0(s, sprintf("%.2f", ax))                                 # < 1,000
}


six_month_slope <- function(d) {
  d <- arrange(d, date)
  if (nrow(d) < 6) return(NA_real_)
  d_tail <- tail(d, 6)
  idx <- seq_len(nrow(d_tail))
  as.numeric(coef(lm(trips ~ idx, data = d_tail))[2])
}

# Normalize Entry/Exit (handles case, plural, underscores/spaces)
normalize_entry_exit <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z]", "", x)  # keep letters only
  dplyr::case_when(
    x %in% c("entry", "entries", "enter", "in") ~ "entry",
    x %in% c("exit",  "exits",  "out",   "leave") ~ "exit",
    TRUE ~ NA_character_
  )
}

# If month can be stored as "7", "Jul", "July" etc., this is resilient
parse_month_int <- function(m) {
  if (is.numeric(m)) return(as.integer(m))
  m_chr <- tolower(trimws(as.character(m)))
  ab <- tolower(month.abb); nm <- tolower(month.name)
  idx <- suppressWarnings(as.integer(m_chr))
  idx[is.na(idx)] <- match(m_chr[is.na(idx)], ab)
  idx[is.na(idx)] <- match(m_chr[is.na(idx)], nm)
  as.integer(idx)
}

valid_or_na <- function(x, lo, hi) {
  x <- as.integer(x)
  if_else(is.na(x) | x < lo | x > hi, NA_integer_, x)
}

# Metric filter (builds a clean Date if needed)
make_metric_df <- function(d, metric = "Total") {
  d <- d %>%
    mutate(
      entry_exit = normalize_entry_exit(entry_exit),
      date = if ("date" %in% names(.)) as.Date(date) else
        make_date(
          year  = suppressWarnings(as.integer(year)),
          month = suppressWarnings(as.integer(month)),
          day   = 1L
        )
    )
  
  if (metric == "Entries") d <- filter(d, entry_exit == "entry")
  if (metric == "Exits")   d <- filter(d, entry_exit == "exit")
  
  d %>%
    filter(!is.na(date)) %>%
    group_by(station, date) %>%
    summarise(trips = sum(suppressWarnings(as.numeric(trip)), na.rm = TRUE),
              .groups = "drop")
}

# ---------- Facility Icon Mapping ----------
facility_icons <- list(
  "Baby change table" = "baby_change.png",
  "Bike lockers" = "bike_lockers.png",
  "Bike racks" = "bike_racks.png",
  "Bike shed" = "bike_shed.png",
  "Commuter car park" = "commuter_car_park.png",
  "Emergency help point" = "emergency_help_point.png",
  "Free mobile phone charging" = "mobile_charging.png",
  "Information point" = "information_point.png",
  "Kiss and ride stopping area" = "kiss_and_ride.png",
  "Luggage storage" = "luggage_storage.png",
  "Next service display" = "next_service_display.png",
  "No Opal card top up or single trip ticket sales" = "no_opal.png",
  "Opal card top up machine (Card Payment only)" = "opal_no_single.png",
  "Opal card top up or single trip ticket machine (Card Payment only)" = "opal_card_only.png",
  "Opal card top up or single trip ticket machine (Cash or Card Payment)" = "opal_both_payments.png",
  "Payphone" = "payphone.png",
  "Taxi rank" = "taxi_rank.png",
  "Toilets" = "toilets.png",
  "Transport Park&Ride" = "transport_park_ride.png",
  "Wheelchair accessible car space" = "wheelchair_carspace.png",
  "Wheelchair accessible payphone" = "wheelchair_payphone.png",
  "Wheelchair accessible toilet" = "wheelchair_toilet.png",
  "Wheelchair accessible toilet (MLAK)" = "MLAK.png"
)

# ---------- Helper: Render facility icons ----------
render_facility_icons <- function(fac_string) {
  if (is.null(fac_string) || is.na(fac_string) || fac_string == "") {
    return(HTML("<p><em>No facility data available.</em></p>"))
  }
  facs <- trimws(unlist(strsplit(fac_string, "\\|")))
  facs <- facs[facs != ""]
  
  htmltools::tags$div(
    style = "display:flex; flex-wrap:wrap; gap:8px;",
    lapply(facs, function(f) {
      img_file <- facility_icons[[f]]
      if (!is.null(img_file)) {
        htmltools::tags$img(
          src = file.path("facilities", img_file),
          title = f,
          alt = f,
          style = "width:144px; height:144px; object-fit:contain; border-radius:6px; border:1px solid #ddd; padding:2px;"
        )
      } else {
        htmltools::tags$span(f)
      }
    })
  )
}



# ---------- Load data ----------
candidate_paths <- c(
  "./datasets/entry_exit_sydney_merged.csv"
)
data_path <- candidate_paths[file.exists(candidate_paths)][1]
if (is.na(data_path)) {
  stop("Could not find 'entry_exit_sydney_merged.csv'. Tried: ", paste(candidate_paths, collapse = ", "))
}

raw_read <- readr::read_csv(data_path, show_col_types = FALSE) %>% clean_names()

# Expect: station, entry_exit, trip, month, year
required_cols <- c("station","entry_exit","trip","month","year")
missing <- setdiff(required_cols, names(raw_read))
if (length(missing) > 0) stop(sprintf("Missing required columns: %s", paste(missing, collapse = ", ")))

raw_tbl <- raw_read %>%
  mutate(
    station    = as.character(station),
    entry_exit = normalize_entry_exit(entry_exit),
    trip       = suppressWarnings(as.numeric(trip)),
    month      = parse_month_int(month),
    year       = suppressWarnings(as.integer(year)),
    month      = valid_or_na(month, 1L, 12L),
    year       = valid_or_na(year, 1900L, 2100L),
    date       = make_date(year = year, month = month, day = 1L)
  ) %>%
  filter(!is.na(trip), !is.na(date), !is.na(station))

fac_path <- "./datasets/locationfacilitydata_filtered.csv"
facilities_tbl <- readr::read_csv(fac_path, show_col_types = FALSE) %>%
  rename(station = LOCATION_NAME)

station_wordcount_path <- "./datasets/sydney_train_metro_station_counts.csv"
station_wordcount <- readr::read_csv(station_wordcount_path, show_col_types = FALSE)

# merge facilities info into the main table
raw_tbl <- raw_tbl %>%
  left_join(facilities_tbl, by = "station")

# ---------- Rent data ----------
rent_path <- "./datasets/rent.xlsx"

rent_tbl <- readxl::read_excel(rent_path) %>%
  janitor::clean_names() %>%
  mutate(
    station       = as.character(station),
    rent_aud_week = as.numeric(rent_aud_week)
  )
rent_tbl <- rent_tbl %>%
  distinct(station, .keep_all = TRUE)



# rent_tbl <- rent_tbl %>%
#   semi_join(raw_tbl, by = "station")

date_min <- min(raw_tbl$date, na.rm = TRUE)
date_max <- max(raw_tbl$date, na.rm = TRUE)






# ---------- UI ----------
app_theme <- bs_theme(version = 5, bootswatch = "flatly")

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  div(
    
    style = "
    background: linear-gradient(135deg, #2C7BE5 0%, #00D97E 100%);
    color: white; padding: 18px 22px; border-radius: 16px;
    margin-bottom: 14px;
    box-shadow: 0 8px 22px rgba(0,0,0,.12);
  ",
    h2(style="margin:0;font-weight:800;", "Sydney Train Usage & Rent Dashboard"),
    div(style="opacity:.9;margin-top:6px;",
        "Interactive exploration of station foot traffic, profiles, and rent relationships.")
  ),
  
  tags$head(
    tags$style(HTML("
    body {
      background: linear-gradient(
        180deg,
        #eef2f7 0%,
        #f7f9fc 100%
      );
    }

    .title-panel {
      background: rgba(255,255,255,0.92);
      backdrop-filter: blur(6px);
      padding: 18px 22px;
      border-radius: 16px;
      box-shadow: 0 10px 24px rgba(0,0,0,0.12);
      margin-bottom: 18px;
    }
  "))
  ),
  
  
  tabsetPanel(
    id = "tabs",
    
    # ---- Overview tab ----
    tabPanel(
      "Overview",
      # Controls for Overview at the top of this tab
      fluidRow(
        column(
          4,
          dateRangeInput(
            "date_range", "Select date range:",
            start = date_min, end = date_max,
            min   = date_min, max = date_max
          )
        ),
        column(
          4,
          selectInput("metric", "Metric", c("Total", "Entries", "Exits"))
        ),
        column(
          4,
          numericInput("top_n", "Top N busiest stations", 10, min = 1)
        )
      ),
      br(),
      # ---- Overview outputs below ----
      fluidRow(
        column(
          4,
          bslib::value_box(
            title = "Total trips (selected)",
            value = uiOutput("vb_total"),
            showcase = icon("person-walking")
          )
        ),
        column(
          4,
          bslib::value_box(
            title = "YoY growth (latest month)",
            value = uiOutput("vb_yoy"),
            showcase = icon("chart-line")
          )
        ),
        column(
          4,
          bslib::value_box(
            title = "Stations with ↑ trend (6-mo)",
            value = uiOutput("vb_rising"),
            showcase = icon("arrow-trend-up")
          )
        )
      )
      ,
      br(),
      plotlyOutput("top_bar", height = "350px"),
      br(),
      plotlyOutput("agg_ts", height = "350px"),
      br(),
      uiOutput("hover_info")
    ),
    
    # ---- Wordcloud tab ----
    tabPanel(
      "Wordcloud",
      fluidRow(
        column(
          4,
          numericInput(
            "wc_n", "Number of stations to show",
            30, min = 10, max = 100, step = 5
          )
        )
      ),
      br(),
      plotOutput("station_wordcloud", height = "800px"),
      tags$p(
        "Word cloud showing station prominence",
        style = "text-align:center; font-size:20px; color:#555;"
      )
    )
    ,
    
    # ---- Station profile tab ----
    tabPanel(
      "Station profile",
      fluidRow(
        column(
          4,
          selectInput(
            "profile_station", "Select station:",
            choices = sort(unique(raw_tbl$station))
          )
        )
      ),
      br(),
      plotlyOutput("profile_ts", height = "350px"),
      br(),
      plotlyOutput("profile_breakdown", height = "350px"),
      br(),
      uiOutput("profile_facilities")
    ),
    
    # ---- Compare tab ----
    tabPanel(
      "Compare",
      fluidRow(
        column(
          6,
          selectizeInput(
            "compare_stations", "Stations comparison",
            multiple = TRUE,
            choices = sort(unique(raw_tbl$station)),
            options  = list(maxItems = 10)
          )
        ),
        column(
          2,
          br(),  # vertical spacing so button aligns nicely
          actionButton("clear_compare", "Clear all", icon = icon("eraser"))
        )
      )
      ,
      br(),
      plotlyOutput("cmp_ts", height = "520px"),
      br(),
      h4("Weekly rent near selected stations"),
      DTOutput("cmp_rent_table"),
      br(),
      uiOutput("cmp_facilities")
    ),
    
    # ---- Rent vs Trips tab ----
    tabPanel(
      "Rent vs Trips",
      fluidRow(
        column(
          4,
          sliderInput(
            "bubble_size", "Bubble size scale",
            min = 1, max = 3, value = 2
          )
        )
      ),
      br(),
      plotlyOutput("rent_scatter", height = "520px"),
      br(),
      DTOutput("rent_table")
    ),
    
    # ---- Data tab ----
    tabPanel(
      "Data",
      DTOutput("table")
    )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  
#   observe({
#   cat("df rows = ", nrow(df()), "\n")
#   cat("rent_station rows = ", nrow(rent_station()), "\n")
# })
#   observe({
#     print(input$compare_stations)
#   })
  
  # Month-aligned selection (Dates already include year)
  sel_months <- reactive({
    req(input$date_range)
    start_date <- floor_date(as.Date(input$date_range[1]), unit = "month")
    end_date   <- floor_date(as.Date(input$date_range[2]), unit = "month")
    list(
      start_date  = start_date,
      end_date    = end_date,
      start_year  = year(start_date),
      start_month = month(start_date),
      end_year    = year(end_date),
      end_month   = month(end_date)
    )
  })

  
  # Metric-specific dataset (ensure Date type)
  df_metric <- reactive({
    make_metric_df(raw_tbl, input$metric) %>% mutate(date = as.Date(date))
  })
  
  # Filtered by month-aligned range
  df <- reactive({
    rng <- sel_months()
    df_metric() %>% filter(date >= rng$start_date, date <= rng$end_date)
  })
  
  # Entries/Exits breakdown
  df_breakdown <- reactive({
    
    rng <- sel_months()
    raw_tbl %>%
      mutate(entry_exit = factor(normalize_entry_exit(entry_exit), levels = c("entry","exit"))) %>%
      filter(date >= rng$start_date, date <= rng$end_date) %>%
      group_by(station, date, entry_exit) %>%
      summarise(trips = sum(trip), .groups = "drop")
  })
  
  rent_station <- reactive({
    
    # ---- 1. Normalize and dedupe rent table ----
    rent_clean <- rent_tbl %>%
      mutate(
        station_raw = station,                       # keep original name for display
        station = tolower(gsub("\\s+", " ", trimws(station))),  # normalize
        rent_aud_week = as.numeric(rent_aud_week)
      ) %>%
      distinct(station, .keep_all = TRUE)
    
    
    # ---- 2. Normalize station names in trips and aggregate ---
    d_trips <- df() %>%
      mutate(
        station_raw = station,                       # backup
        station = tolower(gsub("\\s+", " ", trimws(station)))
      ) %>%
      group_by(station) %>%
      summarise(
        total_trips = sum(trips, na.rm = TRUE),
        station_raw = first(station_raw),            # recover original nice name
        .groups = "drop"
      )
    
    
    # ---- 3. Join and produce final clean table ----
    d <- d_trips %>%
      inner_join(rent_clean %>% select(station, rent_aud_week), by = "station") %>%
      select(
        station = station_raw,
        total_trips,
        rent_aud_week
      ) %>%
      filter(!is.na(rent_aud_week))
    
    d
  })
  
  
  
  # ---- Compare tab: rent for selected stations ----
  cmp_rent <- reactive({
    req(input$compare_stations)
    
    unique_stations <- unique(input$compare_stations)
    
    rent_tbl %>%
      filter(station %in% unique_stations) %>%
      distinct(station, .keep_all = TRUE) %>%   
      arrange(station)
  })
  
  output$cmp_rent_table <- DT::renderDT({
    d <- cmp_rent()
    if (!nrow(d)) return(NULL)
    
    d %>%
      mutate(
        rent_aud_week = round(rent_aud_week, 0)
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(
          pageLength = 10,
          dom = "tip"   
        ),
        colnames = c(
          "Station",
          "Weekly rent (AUD)"
        )
      )
  })
  
  # Keep station inputs in sync with filtered data
  observe({
    st_choices <- df() %>% distinct(station) %>% arrange(station) %>% pull()
    if (length(st_choices)) {
      updateSelectInput(session, "profile_station", choices = st_choices, selected = st_choices[1])
      updateSelectizeInput(session, "compare_stations", choices = st_choices, server = TRUE)
      # ---- Reset filters button ----
      observeEvent(input$reset_filters, {
        updateDateRangeInput(
          session, "date_range",
          start = date_min,
          end   = date_max
        )
        
        # Metric default "Total"
        updateSelectInput(
          session, "metric",
          selected = "Total"
        )
        
        # Top N, default 10
        updateNumericInput(
          session, "top_n",
          value = 10
        )
        
        # Wordcloud station, the default number is 30
        updateNumericInput(
          session, "wc_n",
          value = 30
        )
        
        # Station profile,default choose first station
        st_choices <- sort(unique(raw_tbl$station))
        if (length(st_choices) > 0) {
          updateSelectInput(
            session, "profile_station",
            choices  = st_choices,
            selected = st_choices[1]
          )
        }
        
        # Compare tab 
        updateSelectizeInput(
          session, "compare_stations",
          selected = character(0)
        )
      })
    }
  })
  
  observeEvent(input$clear_compare, {
    updateSelectizeInput(session, "compare_stations", selected = character(0))
  })
  
  
  # Latest available month within the filtered data
  latest_date <- reactive({
    d <- df()
    if (!nrow(d)) return(NA)
    max(d$date)
  })

  # ----- KPIs -----
  output$vb_total <- renderUI({
    d <- df()
    if (!nrow(d) || all(is.na(d$trips))) return(span("0"))
    val <- suppressWarnings(sum(d$trips, na.rm = TRUE))
    if (!is.finite(val)) val <- 0
    span(fmt_big(val))
  })
  
  output$vb_yoy <- renderUI({
    rng <- sel_months()
    
    agg <- df() %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(total_trips = sum(trips, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(date)
    
    if (!nrow(agg)) return(span("N/A"))
    
    idx  <- seq.Date(from = rng$start_date, to = rng$end_date, by = "month")
    full <- tibble::tibble(date = idx) %>%
      dplyr::left_join(agg, by = "date") %>%
      dplyr::mutate(prev = dplyr::lag(total_trips, 12),
                    yoy  = dplyr::if_else(!is.na(total_trips) & !is.na(prev) & prev > 0,
                                          total_trips / prev - 1,
                                          NA_real_))
    
    last_yoy <- tail(stats::na.omit(full$yoy), 1)
    if (length(last_yoy) == 0) span("N/A") else span(scales::percent(last_yoy, accuracy = 0.1))
  })
  
  # ---- Rent vs Trips: Scatter plot ----
  output$rent_scatter <- renderPlotly({
    d <- rent_station()
    if (!nrow(d)) return(NULL)
    
    # Make sure the slider value is available
    req(input$bubble_size)
    
    d <- d %>%
      filter(total_trips > 0, rent_aud_week > 0)
    
    p <- ggplot(d, aes(
      x     = rent_aud_week,
      y     = total_trips,
      size  = total_trips,
      color = rent_aud_week,
      text  = paste0(
        "<b>", station, "</b><br>",
        "Weekly Rent: $", round(rent_aud_week), "<br>",
        "Total Trips: ", scales::comma(total_trips)
      )
    )) +
      geom_point(alpha = 0.7) +
      scale_y_log10(labels = scales::comma) +
      # ★ use the slider value to control bubble size
      scale_size_continuous(
        range = c(3 * input$bubble_size, 12 * input$bubble_size),
        guide = "none"
      ) +
      scale_color_viridis_c(option = "plasma") +
      labs(
        title = "Rent vs Foot Traffic (log-scaled Y axis)",
        x     = "Weekly Rent (AUD)",
        y     = "Total Trips (log10 scale)",
        color = "Rent (AUD)"
      ) +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = "text")
  })
  
  # ---- Rent vs Trips: Data table ----
  output$rent_table <- DT::renderDT({
    d <- rent_station()
    write.csv(d, "rent_station_debug.csv", row.names = FALSE)
    
    d <- distinct(d)
    if (!nrow(d)) return(NULL)
    
    d %>%
      arrange(desc(total_trips)) %>%
      mutate(
        rent_aud_week = round(rent_aud_week, 0),
        total_trips   = round(total_trips, 0)
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(
          pageLength = 10,
          order = list(list(2, "desc")) 
        ),
        colnames = c(
          "Station",
          "Total trips (selected)",
          "Weekly rent (AUD)"
        )
      )
  })
  
  output$vb_rising <- renderUI({
    rising <- df() %>%
      arrange(date) %>%
      group_by(station) %>%
      summarise(slope6 = six_month_slope(cur_data_all()), .groups = "drop") %>%
      summarise(n = sum(slope6 > 0, na.rm = TRUE), .groups = "drop") %>%
      pull(n)
    span(format(rising, big.mark = ","))
  })
  
  # ----- Overview: Top stations (latest month) -----
  output$top_bar <- renderPlotly({
    ld <- latest_date()
    validate(need(!is.na(ld), "No data in selection."))
    validate(need(is.numeric(input$top_n) && input$top_n > 0 && input$top_n <= 25,
                  "Top N must be between 1 and 25"))
    
    top_df <- df() %>%
      filter(date == ld) %>%
      group_by(station) %>%
      summarise(trips = sum(trips), .groups = "drop") %>%
      slice_max(trips, n = max(1, min(30, as.integer(input$top_n))))
    
    if (nrow(top_df) == 0) return(plotly_empty())
    
    plot_ly(top_df,
            x = ~trips,
            y = ~reorder(station, trips),
            type = "bar",
            orientation = "h",
            text = ~paste0("<b>", station, "</b><br>Trips (", format(ld, "%b %Y"), "): ", comma(trips)),
            hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = paste0(input$metric, " trips — latest month")),
        yaxis = list(title = "Stations"),
        margin = list(l = 120, r = 20, t = 20, b = 20)
      )
  })
  # ----- Overview: Wordcloud of stations -----
  output$station_wordcloud <- renderPlot({
    req(nrow(station_wordcount) > 0)
    validate(
      need(is.numeric(input$wc_n) && input$wc_n >= 10 && input$wc_n <= 100,
           "Number of stations in wordcloud must be between 10 and 100")
    )
    
    library(wordcloud)
    library(RColorBrewer)
    
    # --- Dynamic text size control ---
    max_size <- scales::rescale(input$wc_n, to = c(3, 2.8), from = c(10, 100))
    min_size <- max_size * 0.35
    
    
    wordcloud(
      words = station_wordcount$station_name,
      freq = station_wordcount$total_appearance,
      min.freq = 1,
      max.words = input$wc_n,
      random.order = FALSE,
      rot.per = 0.15,
      colors = brewer.pal(8, "Dark2"),
      scale = c(max_size, min_size)
    )
  })
 
  
  output$station_wc_container <- renderUI({
    req(input$wc_n)
    
    # Dynamic height formula for base wordcloud
    h <- 500 + input$wc_n * 4   # adjust as needed
    
    plotOutput(
      "station_wordcloud",
      height = paste0(h, "px"),
      width = "100%"
    )
  })
  
  
  
  
  # ----- Overview: Aggregate time series -----
  output$agg_ts <- renderPlotly({
    agg <- df() %>% group_by(date) %>% summarise(total_trips = sum(trips), .groups = "drop")
    plot_ly(agg, x = ~date, y = ~total_trips, type = "scatter", mode = "lines") %>%
      layout(yaxis = list(title = paste0(input$metric, " trips (total)")),
             xaxis = list(title = NULL)) %>%
      layout(
        xaxis = list(title = paste0(input$metric, " trips — across date range")),
        yaxis = list(title = NULL),
        margin = list(l = 120, r = 20, t = 50, b = 100)
      )
    
  })
  
  
  # ----- Station profile -----
  output$profile_ts <- renderPlotly({
    req(input$profile_station)
    d <- df() %>% filter(station == input$profile_station)
    plot_ly(d, x = ~date, y = ~trips, type = "scatter", mode = "lines+markers",
            text = ~paste0("<b>", station, "</b><br>",
                           "Trips: ", comma(trips), "<br>",
                           format(date, "%b %Y")), hoverinfo = "text") %>%
      layout(yaxis = list(title = paste0(input$metric, " trips")),
             xaxis = list(title = NULL))
  })
  
  output$profile_breakdown <- renderPlotly({
    req(input$profile_station)
    d <- df_breakdown() %>% filter(station == input$profile_station)
    if (nrow(d) == 0) return(NULL)
    plot_ly(d, x = ~date, y = ~trips, color = ~entry_exit, type = "scatter", mode = "lines") %>%
      layout(yaxis = list(title = "Trips (entries vs exits)"),
             xaxis = list(title = NULL),
             legend = list(title = list(text = "Type")))
  })
  
  # ----- Station facilities info -----
  output$profile_facilities <- renderUI({
    req(input$profile_station)
    fac <- raw_tbl %>%
      filter(station == input$profile_station) %>%
      distinct(FACILITIES) %>%
      pull(FACILITIES)
    
    if (length(fac) == 0 || is.na(fac) || fac == "") {
      HTML("<p><em>No facility data available for this station.</em></p>")
    } else {
      tags$div(
        style = "margin-bottom:100px;",
        tagList(
          h5("Facilities available:"),
          render_facility_icons(fac)
        )
      )
    }
    
  })
  
  
  
  # ----- Compare -----
  output$cmp_ts <- renderPlotly({
    req(input$compare_stations)
    validate(need(is.numeric(input$top_n) && input$top_n >= 0 && input$top_n <= 10,
                  "Number of stations to compare must be between 0 and 10"))
    d <- df() %>% filter(station %in% input$compare_stations)
    plot_ly(d, x = ~date, y = ~trips, color = ~station, type = "scatter", mode = "lines") %>%
      layout(yaxis = list(title = paste0(input$metric, " trips")),
             xaxis = list(title = NULL),
             legend = list(title = list(text = "Station")))
  })
  
  output$cmp_facilities <- renderUI({
    req(input$compare_stations)
    
    fac_df <- raw_tbl %>%
      filter(station %in% input$compare_stations) %>%
      distinct(station, FACILITIES)
    
    fac_list <- setNames(
      lapply(fac_df$FACILITIES, function(f) {
        if (is.null(f) || is.na(f) || f == "") return(character(0))
        trimws(unlist(strsplit(f, "\\|")))
      }),
      fac_df$station
    )
    
    common_facilities <- if (length(fac_list) > 0) Reduce(intersect, fac_list) else character(0)
    common_facilities <- sort(unique(common_facilities))
    
    diff_facilities <- lapply(names(fac_list), function(st) {
      setdiff(fac_list[[st]], common_facilities)
    })
    names(diff_facilities) <- names(fac_list)
    
    tagList(
      h4("Facilities common to all compared stations"),
      if (length(common_facilities) == 0) {
        HTML("<p><em>No facilities shared by all selected stations.</em></p>")
      } else {
        render_facility_icons(paste(common_facilities, collapse = "|"))
      },
      br(),
      h4("Other facilities by station (not common to all)"),
      lapply(names(diff_facilities), function(st) {
        if (length(fac_list[[st]]) == 0) {
          tags$div(style = "margin-bottom:100px;",tags$b(st), HTML("<em>No facility data available.</em>"))
        } else if (length(diff_facilities[[st]]) == 0) {
          tags$div(style = "margin-bottom:100px;",tags$b(st), HTML("<em>All facilities are common to all compared stations.</em>"))
        } else {
          tags$div(
            style = "margin-bottom:100px;",
            tags$b(st),
            render_facility_icons(paste(diff_facilities[[st]], collapse = "|"))
          )
        }
      })
    )
  })
  



  
  
  # ----- Data table (time-aware sorting) -----
  output$table <- renderDT({
    d <- df() %>%
      mutate(
        month_label = format(date, "%b %Y"),
        trips_display = ifelse(trips == 50, "< 50", as.character(trips)) # display value
      ) %>%
      select(date, month_label, station, trips, trips_display) %>%
      arrange(desc(date), desc(trips))
    
    datatable(
      d,
      colnames = c("date_sort", "month", "station", "trips_sort", "trips"),
      options = list(
        pageLength = 25,
        order = list(list(0, "desc")), # sort by hidden date
        columnDefs = list(
          list(visible = FALSE, targets = c(0, 3)), # hide sort columns
          list(orderData = 3, targets = 4),         # sort 'trips' by hidden numeric column
          list(orderData = 0, targets = 1)          # sort month by hidden date
        )
      ),
      rownames = FALSE
    )
  })
}

# ---------- Run ----------
shinyApp(ui, server)
