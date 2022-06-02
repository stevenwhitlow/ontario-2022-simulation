setwd("/Users/steven/Documents/elections/ontario/")
# sink("log.txt", split = TRUE)

# Model without sigma parameter varying individual poll errors by firm
 # source("model.R", echo = TRUE)
 # source("figures.R", echo = TRUE)
 # source("simulation_figures.R", echo = TRUE)
 # source("ridings.R", echo = TRUE)
 # save(list = setdiff(ls(), c("campaign_data", "campaign_fit", "campaign_mod", "list_draws")),
 #      file = "saved.Rdata")

# Preferred model with sigma parameter
source("model_with_sigma.R", echo = TRUE)
source("figures.R", echo = TRUE)
source("simulation_figures.R", echo = TRUE)
source("ridings.R", echo = TRUE)
save(list = setdiff(ls(), c("campaign_data", "campaign_fit", "campaign_mod", "list_draws")),
     file = "saved_with_sigma.Rdata")
rmarkdown::render("dashboard.Rmd")
# sink()