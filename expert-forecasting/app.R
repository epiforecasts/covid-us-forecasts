#
# This is a shiny app to visualise national and state-level reported deaths and forecasts,
# as well as visualise your own 'expert' forecasts.
#

library(shiny)
library(tidyverse)
library(googledrive)
library(googlesheets4)

## Set auth
options(gargle_oauth_cache = ".secrets")
drive_auth(cache = ".secrets", email = "sophie.meakin.00@gmail.com")
sheets_auth(token = drive_token())

## Load data
submission_sheet <- "https://docs.google.com/spreadsheets/d/1J2aOqv8NQ2Hl6GVyTmZbkZxH4tORwGAGrqW3S2K_PRw/edit#gid=0"
check_ids <- googlesheets4::read_sheet(ss = submission_sheet,
                                      sheet = "ids")

# Current model forecasts (from most recent Monday)
load_date <- lubridate::floor_date(Sys.Date(), unit = "week", week_start = 1) %>%
    as.character()
load_addr <- "https://raw.githubusercontent.com/epiforecasts/covid-us-forecasts/master/death-forecast/epiforecasts-ensemble1/"
raw_data <- readr::read_csv(file = paste0(load_addr, load_date, "-epiforecasts-ensemble1.csv"))

# Load and process most daily reported deaths data
deaths <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv") %>% 
    dplyr::select(Province_State, dplyr::matches("^\\d")) %>%
    tidyr::pivot_longer(cols = -Province_State, names_to = "date", values_to = "deaths") %>%
    dplyr::mutate(date = lubridate::mdy(date)) %>%
    dplyr::group_by(Province_State, date) %>%
    dplyr::summarise(deaths = sum(deaths)) %>%
    dplyr::rename(state = Province_State) %>%
    # De-cumulate to daily
    dplyr::arrange(date) %>% 
    dplyr::group_by(state) %>% 
    dplyr::mutate(deaths = c(0, diff(deaths)))%>%
    dplyr::mutate(deaths = replace(deaths, deaths < 0 , 0)) %>% 
    dplyr::ungroup() %>%
    dplyr::filter(!state %in% c("Diamond Princess", "Grand Princess"))
deaths_data <- deaths %>%
    dplyr::mutate(week = lubridate::floor_date(date, unit = "week", week_start = 7)) %>%
    dplyr::group_by(state, week) %>%
    dplyr::summarise(week_deaths = sum(deaths, na.rm = TRUE))
deaths_data <- deaths_data %>%
    bind_rows(deaths_data %>%
                  dplyr::group_by(week) %>%
                  dplyr::summarise(week_deaths = sum(week_deaths, na.rm = TRUE)) %>%
                  dplyr::mutate(state = "US")) %>%
    select(week_beginning = week, state, value = week_deaths) %>%
    mutate(state_name = state,
           type = "observed_data",
           target_end_date = week_beginning + 6) %>%
    filter(week_beginning < as.Date(load_date) - 1,
           week_beginning >= as.Date("2020-05-01"))

# Combine model forecasts and deaths data
df <- raw_data %>%
    left_join(tigris::fips_codes %>%
                  select(location = state_code, state_name) %>%
                  unique() %>%
                  rbind(c("US", "US")),
              by = "location") %>%
    filter(grepl("inc", target),
           quantile %in% c(0.05, 0.5, 0.95)) %>%
    bind_rows(deaths_data) %>%
    mutate(q_type = ifelse(type %in% c("point", "observed_data"), type, paste0(type, quantile))) %>%
    select(target_end_date, state_name, q_type, value)


## Define some inputs for the shiny app

# List of locations
list_states <- df %>%
    filter(q_type == "quantile0.5") %>%
    .$state_name %>%
    unique()

# Initial value for the quantiles (forecast model quantiles)
get_state_init <- function(state = "US"){
    
    max_vals <- df %>%
        filter(state_name == state,
               q_type == "observed_data") %>%
        .$value %>%
        max(., 1000) %>%
        round(digits = -3)
    
    out <- df %>%
        filter(state_name == state,
               q_type %in% c("quantile0.05", "quantile0.5", "quantile0.95")) %>%
        pivot_wider(id_cols = c(target_end_date, state_name), names_from = q_type, values_from = value) %>%
        mutate(lower_p = quantile0.05/quantile0.5,
               upper_p = quantile0.95/quantile0.5,
               median_max = max_vals,
               quantile_max = round(median_max*max(upper_p), -3)) %>%
        select(state_name, week = target_end_date, quantile0.05, quantile0.5, quantile0.95, lower_p, upper_p, median_max, quantile_max)
    
    return(out)
    
}

init_vals <- get_state_init("US")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Expert elicitation forecasting for COVID-19 in the United States"),
    
    singleton(tags$head(tags$script(src = "message-handler.js"))),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(width = 3,
                     h3("Information"),
                     p(),
                     hr(),
                     h4("Forecast inputs"),
                     p(strong("Location"),
                       "is the forecasting region (US or state)."),
                     p(strong("Median"),
                       "is your best estimate of the median weekly incident deaths for 1 - 4 weeks ahead. Default value is from the model forecast."),
                     p(strong("Quantiles"),
                       "are your best estimates of the 5% and 95% quantiles of weekly incident deaths for 1 - 4 weeks ahead. The chosen values should span the median. Default values are from the model forecast."),
                     hr(),
                     h4("Forecast visualisation"),
                     p(strong(span("Black", style = "color:black")),
                       "points and values (at bottom) show observed weekly incident deaths."),
                     p(strong(span("Grey", style = "color:grey")),
                       "crosses and values (at bottom) show the median weekly incident deaths forecast by the current model; grey ribbon shows the 90% credible interval."),
                     p(strong(span("Red", style = "color:red")),
                       "points show the median adjusted forecast determined by the forecast inputs, and values show percentage difference compared to the model forecast, for reference; red ribbon shows the 90% credible interval. Default values are the model forecast values."),
                     hr(),
                     h4("Forecast submission"),
                     textInput("f_id", "Forecaster ID:", value = ""),
                     actionButton("submit", "Submit forecast")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(width = 9,
                  h3("Forecast inputs"),
                  selectInput("location", "Location:", list_states, selected = "US"),
                  fluidRow(column(3,
                                  h4("1 week ahead"),
                                  sliderInput("pt_wk1", label = "Median", min = 0, max = init_vals$median_max[1], value = init_vals$quantile0.5[1]),
                                  sliderInput("qt_wk1", label = "Quantiles", min = 0, max = init_vals$quantile_max[1], value = c(init_vals$quantile0.05[1], init_vals$quantile0.95[1]))
                  ),
                  column(3,
                         h4("2 weeks ahead"),
                         sliderInput("pt_wk2", label = "Median", min = 0, max = init_vals$median_max[1], value = init_vals$quantile0.5[2]),
                         sliderInput("qt_wk2", label = "Quantiles", min = 0, max = init_vals$quantile_max[1], value = c(init_vals$quantile0.05[2], init_vals$quantile0.95[2]))
                  ),
                  column(3,
                         h4("3 weeks ahead"),
                         sliderInput("pt_wk3", label = "Median", min = 0, max = init_vals$median_max[1], value = init_vals$quantile0.5[3]),
                         sliderInput("qt_wk3", label = "Quantiles", min = 0, max = init_vals$quantile_max[1], value = c(init_vals$quantile0.05[3], init_vals$quantile0.95[3]))
                  ),column(3,
                           h4("4 weeks ahead"),
                           sliderInput("pt_wk4", label = "Median", min = 0, max = init_vals$median_max[1], value = init_vals$quantile0.5[4]),
                           sliderInput("qt_wk4", label = "Quantiles", min = 0, max = init_vals$quantile_max[1], value = c(init_vals$quantile0.05[4], init_vals$quantile0.95[4]))
                  )
                  ),
                  hr(),
                  h3("Forecast visualisation"),
                  plotOutput("distPlot")
        )
    )
    
)

# Define server logic required
server <- function(input, output, session) {
    
    observeEvent(input$submit, {
        
        if(input$f_id %in% check_ids$id){
            
            submit_df <- cbind(as.character(unique(raw_data$target_end_date)), rep(input$location, 4), rep("ee0.5", 4), c(input$pt_wk1, input$pt_wk2, input$pt_wk3, input$pt_wk4)) %>%
                rbind(cbind(as.character(unique(raw_data$target_end_date)), rep(input$location, 4), rep("ee0.05", 4), c(input$qt_wk1[1], input$qt_wk2[1], input$qt_wk3[1], input$qt_wk4[1]))) %>%
                rbind(cbind(as.character(unique(raw_data$target_end_date)), rep(input$location, 4), rep("ee0.95", 4), c(input$qt_wk1[2], input$qt_wk2[2], input$qt_wk3[2], input$qt_wk4[2]))) %>%
                data.frame() %>%
                set_names(colnames(df)) %>%
                mutate(submit_time = Sys.time(),
                       forecast_date = load_date) %>%
                select(submit_time, forecast_date, target_end_date, state_name, q_type, value) %>%
                pivot_wider(id_cols = c(submit_time, forecast_date, target_end_date, state_name), names_from = q_type, values_from = value)
            
            showNotification("Thank you for your submission!", duration = 3, type = "message")
            
            googlesheets4::sheet_append(data = submit_df,
                                        ss = submission_sheet,
                                        sheet = check_ids$name[check_ids$id == input$f_id])
            
            
        } else {
            
            showNotification("Please submit a valid ID number.", duration = 3, type = "error")
            
        }
        
    })
    
    observeEvent(input$location, {
        
        init_vals <- get_state_init(input$location)
        
        updateSliderInput(session, "pt_wk1", min = 0, max = init_vals$median_max[1], value = init_vals$quantile0.5[1])
        updateSliderInput(session, "pt_wk2", min = 0, max = init_vals$median_max[1], value = init_vals$quantile0.5[2])
        updateSliderInput(session, "pt_wk3", min = 0, max = init_vals$median_max[1], value = init_vals$quantile0.5[3])
        updateSliderInput(session, "pt_wk4", min = 0, max = init_vals$median_max[1], value = init_vals$quantile0.5[4])
        
        updateSliderInput(session, "qt_wk1", min = 0, max = init_vals$quantile_max[1], value = c(init_vals$quantile0.05[1],init_vals$quantile0.95[1]))
        updateSliderInput(session, "qt_wk2", min = 0, max = init_vals$quantile_max[1], value = c(init_vals$quantile0.05[2],init_vals$quantile0.95[2]))
        updateSliderInput(session, "qt_wk3", min = 0, max = init_vals$quantile_max[1], value = c(init_vals$quantile0.05[3],init_vals$quantile0.95[3]))
        updateSliderInput(session, "qt_wk4", min = 0, max = init_vals$quantile_max[1], value = c(init_vals$quantile0.05[4],init_vals$quantile0.95[4]))
        
        
    })
    
    observeEvent(input$pt_wk1, {
        
        init_vals <- get_state_init(input$location)
        updateSliderInput(session, "qt_wk1", min = 0, max = init_vals$quantile_max[1], value = input$pt_wk1*c(init_vals$lower_p[1], init_vals$upper_p[1]))
        
    })
    
    observeEvent(input$pt_wk2, {
        
        init_vals <- get_state_init(input$location)
        updateSliderInput(session, "qt_wk2", min = 0, max = init_vals$quantile_max[2], value = input$pt_wk2*c(init_vals$lower_p[2], init_vals$upper_p[2]))
        
    })
    
    observeEvent(input$pt_wk3, {
        
        init_vals <- get_state_init(input$location)
        updateSliderInput(session, "qt_wk3", min = 0, max = init_vals$quantile_max[3], value = input$pt_wk3*c(init_vals$lower_p[3], init_vals$upper_p[3]))
        
    })
    
    observeEvent(input$pt_wk4, {
        
        init_vals <- get_state_init(input$location)
        updateSliderInput(session, "qt_wk4", min = 0, max = init_vals$quantile_max[4], value = input$pt_wk4*c(init_vals$lower_p[4], init_vals$upper_p[4]))
        
    })
    
    output$distPlot <- renderPlot({
        
        input_df <- cbind(as.character(unique(raw_data$target_end_date)), rep(input$location, 4), rep("ee0.5", 4), c(input$pt_wk1, input$pt_wk2, input$pt_wk3, input$pt_wk4)) %>%
            rbind(cbind(as.character(unique(raw_data$target_end_date)), rep(input$location, 4), rep("ee0.05", 4), c(input$qt_wk1[1], input$qt_wk2[1], input$qt_wk3[1], input$qt_wk4[1]))) %>%
            rbind(cbind(as.character(unique(raw_data$target_end_date)), rep(input$location, 4), rep("ee0.95", 4), c(input$qt_wk1[2], input$qt_wk2[2], input$qt_wk3[2], input$qt_wk4[2]))) %>%
            data.frame() %>%
            set_names(colnames(df))
        
        g <- df %>%
            rbind(input_df) %>%
            filter(state_name == input$location) %>%
            mutate(value = as.numeric(value)) %>%
            pivot_wider(id_cols = c(target_end_date, state_name), names_from = q_type, values_from = value) %>%
            mutate(pchange = round(100*(ee0.5 - quantile0.5)/quantile0.5),
                   pchange_label = case_when(pchange == 0 ~ "  ",
                                             pchange < 0 ~ paste0(pchange, "%"),
                                             pchange > 0 ~ paste0("+", pchange, "%"))) %>%
            ggplot(aes(x = target_end_date)) +
            # Observed data
            geom_line(aes(y = observed_data), lwd = 1) +
            geom_point(aes(y = observed_data), size = 5) + 
            geom_text(aes(y = 0, label = observed_data), vjust = "bottom") +
            # Input values
            geom_line(aes(y = ee0.5), lwd = 1, lty = 2, col = "red") +
            # geom_point(aes(y = ee0.5), pch = 4, stroke = 1.5, size = 5, col = "red") +
            geom_ribbon(aes(ymin = ee0.05, ymax = ee0.95), alpha = 0.2, fill = "red") +
            geom_label(aes(y = ee0.5, label = pchange_label), label.r = unit(0.25, "lines"), label.size = 1, fontface = "bold", col = "red") +
            # Current model predictions
            geom_line(aes(y = quantile0.5), lwd = 1, lty = 2, col = "grey40") + 
            geom_point(aes(y = quantile0.5), pch = 4, stroke = 1.5, fill = "white", size = 4, col = "grey40") + 
            geom_ribbon(aes(ymin = quantile0.05, ymax = quantile0.95), alpha = 0.2, fill = "grey") +
            geom_text(aes(y = 0, label = quantile0.5), col = "grey40", vjust = "bottom") +
            #
            labs(x = "Week end", y = "Weekly incident deaths") +
            cowplot::theme_cowplot()
        
        g
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)