options(mc.cores = parallel::detectCores())
library(rvest)
library(plyr)
library(tidyverse)
library(magrittr)
library(forcats)
library(janitor)
library(lubridate)
library(rstan)
setwd("/Users/steven/Documents/elections/ontario/")

polling_ontario = read_html("https://en.wikipedia.org/wiki/2022_Ontario_general_election#Opinion_polls")
polling_ontario <- polling_ontario %>%
  html_nodes('table.wikitable') %>%
  #html_element("h3+ .wikitable, h3+ .wikitable") %>%
  html_table(header=TRUE) 

clean_table <- function(df) {
  df %>% slice(-1) %>%
    rename(Date ="Last dateof polling", Firm = "Polling firm", N = "Sample size") %>%
    filter(Firm != "2018 election") %>%
    mutate(Date = as.Date(str_remove(Date,","), "%B %d %Y")) %>%
    mutate(N = str_remove(N, "[,]")) %>%
    mutate(N = str_replace(N, " \\s*\\([^\\)]+\\)", "")) %>%
    filter(!is.na(Date)) %>%
    rename(NB = "New Blue") %>%
    mutate(NB = as.numeric(ifelse(NB == "—", 0, NB))) %>%
    mutate(Ontario = as.numeric(ifelse(Ontario == "—", 0, Ontario))) %>%
    mutate(Other = str_replace(Other, "\\[\\D\\]", "")) %>%
    mutate(Other = as.numeric(ifelse(Other == "—", 0, Other))) %>%
    mutate(Green = as.numeric(ifelse(Green == "—", 0, Green))) %>%
    mutate(Other = Other + NB + Ontario) %>%
    mutate(N = as.numeric(N)) %>%
    mutate_at(c("PC", "NDP", "Liberal", "Green"), as.numeric) %>%
    select(Firm, Date, PC, NDP, Liberal, Green, Other, N)
}

polling_election <- polling_ontario[[9]] %>%
  clean_table()

polling_preelection <- polling_ontario[[10]] %>%
  clean_table()

polling <- bind_rows(polling_election, polling_preelection)

results_2018 <- list(
  Date = ymd(20180607),
  PC = 0.4050,
  NDP = 0.3356,
  Liberal = 0.1959,
  Green = 0.0460,
  Other = 0.0178,
  N = 5806286
)

polling <- polling %>%
  mutate(across(-c("Firm", "Date", "N"), ~ .x/100)) %>%
  mutate(across(-c("Firm", "Date", "N"), .fns = list(se = ~sqrt(. * (1 - .) / N)))) %>%
  mutate(Firm = str_replace(Firm, "\u00A0", " ")) %>%
  mutate(time = as.integer(difftime(Date, ymd(20180607), units = "days")) + 1L,
         firmid = as.integer(factor(Firm))) %>%
  mutate(Green_se = if_else(Other_se == 0, 0.001, Other_se)) %>%
  mutate(Other_se = if_else(Other_se == 0, 0.001, Other_se))

campaign_data <- within(list(), {
  y <- polling %>%
    select(c(PC, NDP, Liberal, Green, Other)) %>%
    data.matrix() %>% t()
  se <- polling %>%
    select(c(PC_se, NDP_se, Liberal_se, Green_se, Other_se)) %>%
    data.matrix() %>% t()
  time <- polling$time
  house <- polling$firmid
  num_firms <- max(polling$firmid)
  num_polls <- ncol(y)
  num_parties <- nrow(y)
  num_days <- as.integer(max(polling$time))
  prev_election <- results_2018[2:6] %>% unlist(use.names=F)
  delta_loc <- 0
  tau_scale <- 2*sd(y)
  zeta_scale <- 5
})

campaign_mod <- stan_model("campaign_with_sigma.stan")

campaign_fit <- sampling(campaign_mod, data = campaign_data,
                         iter = 5000,
                         chains = 4
)

saveRDS(campaign_fit, file = "campaign_fit_sigma.RDS")