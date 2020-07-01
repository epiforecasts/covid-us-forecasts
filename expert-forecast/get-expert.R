# Download expert elicitation forecasts

get_expert_elicitation = function(){
  
  # Google sheet
  submission_sheet <- "https://docs.google.com/spreadsheets/d/1J2aOqv8NQ2Hl6GVyTmZbkZxH4tORwGAGrqW3S2K_PRw/edit#gid=191588196"
  
  # Get all names of sheets
  ss_metadata <- googlesheets4::gs4_get(ss = submission_sheet)
  ss_sheets <- setdiff(ss_metadata$sheets$name, c("control", "ids", "states", "test"))
  
  # Load expert data
  all_expert <- purrr::map_df(.x = ss_sheets,
                              ~ googlesheets4::read_sheet(ss = submission_sheet,
                                                          sheet = .x) %>%
                                mutate(submission_id = as.character(submission_id),
                                       submission_time = as.POSIXct(submission_time),
                                       forecast_date = as.Date(forecast_date),
                                       target_week_end = as.Date(target_week_end),
                                       location = as.character(location),
                                       quantile0.5 = as.numeric(quantile0.5),
                                       quantile0.05 = as.numeric(quantile0.05),
                                       quantile0.95 = as.numeric(quantile0.95)) %>%
                                group_by(forecast_date, location, target_week_end) %>%
                                arrange(desc(submission_time)) %>%
                                slice(1) %>%
                                ungroup()
  ) %>%
    mutate(model = "Individual experts") %>%
    select(model, submission_id, forecast_date, target_week_end, state = location, point = quantile0.5, quantile0.05, quantile0.95)
  
  # Anonymise submission IDs
  anon_id <- all_expert %>%
    select(submission_id) %>%
    unique() %>%
    mutate(expert_id = paste0("expert", 1:length(unique(all_expert$submission_id))))
  anon_expert <- all_expert %>%
    left_join(anon_id, by = "submission_id") %>%
    select(-submission_id)
  
  # Aggregated expert judgement (unweighted mean)
  final_expert <- all_expert %>%
    group_by(forecast_date, state, target_week_end) %>%
    summarise(n = n(),
              point = mean(point),
              quantile0.05 = mean(quantile0.05),
              quantile0.95 = mean(quantile0.95)) %>%
    ungroup() %>%
    mutate(model = "Expert consensus") %>%
    select(model, forecast_date, target_week_end, state, point, quantile0.05, quantile0.95)
  
  
  saveRDS(anon_expert, file = paste0("expert-forecast/raw-rds/", Sys.Date(), "-ind-expert.rds"))
  saveRDS(anon_expert, file = paste0("expert-forecast/raw-rds/latest-ind-expert.rds"))
  
  saveRDS(final_expert, file = paste0("expert-forecast/raw-rds/", Sys.Date(), "-agg-expert.rds"))
  saveRDS(final_expert, file = paste0("expert-forecast/raw-rds/latest-agg-expert.rds"))
  
  
  return(list("individual" = all_expert, "aggregated" = final_expert))
  
}

