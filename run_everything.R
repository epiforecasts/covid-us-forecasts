
# ============================================================================ #
# Update data used for the forecasts
# ============================================================================ #

source("utils/get_us_deaths.R")

# ============================================================================ #
# Run all models
# ============================================================================ #

# ----------- #
# Rt forecast #
# ----------- #

# update delays (not run)
# source("rt-forecast/update-delays.R")

# make new rt forecasts for US deaths
source("rt-forecast/update-us-deaths-rt.R")


# --------------- #
# direct forecast #
# --------------- #

# work in progress



# ============================================================================ #
# Prepare submission format
# ============================================================================ #

# load in function
source("utils/format_submission.R")

# run formatting function
# results are stored in death-forecast/epiforecasts-ensemble1
source("utils/update_report_submission.R")
source("utils/format_submission.R")



# ============================================================================ #
# Visualise submissions
# ============================================================================ #

date <- Sys.Date()
dir.create(here::here("visualisation", date))
source("utils/visualise_submission.R")

# visualise national death forecast
national <- plot_forecasts(national = TRUE, states = NULL, cutoff = 25) + 
  theme(text = element_text(family = "Sans Serif"))

ggsave(plot = national, path = here::here("visualisation", date), 
       filename = "death_national.png")

# visualise subnational death forecast
subnational <- plot_forecasts(national = FALSE, states = NULL, cutoff = 25) + 
  theme(text = element_text(family = "Sans Serif"))

ggsave(plot = subnational, path = here::here("visualisation", date), 
       filename = "death_subnational.png", height = 30, width = 30)
