setwd("/Users/steven/Documents/elections/ontario/")

# Model without sigma parameter which varies poll variances by firm
 # source("model_nosigma.R", echo = TRUE)
 # source("figures.R", echo = TRUE)
 # source("simulation_figures.R", echo = TRUE)
 # source("ridings.R", echo = TRUE)
 # save(list = setdiff(ls(), c("campaign_data", "campaign_fit", "campaign_mod", "list_draws")),
 #      file = "saved.Rdata")

# Preferred model with sigma parameter
source("model.R", echo = TRUE)
source("figures.R", echo = TRUE)
source("simulation_figures.R", echo = TRUE)
source("ridings.R", echo = TRUE)
save(list = setdiff(ls(), c("campaign_data", "campaign_fit", "campaign_mod", "list_draws")),
     file = "saved_with_sigma.Rdata")
rmarkdown::render("dashboard.Rmd")
