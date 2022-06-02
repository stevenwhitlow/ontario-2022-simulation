options(mc.cores = parallel::detectCores())
library(rvest)
library(tidyverse)
library(magrittr)
library(forcats)
library(janitor)
library(lubridate)
library(rstan)
library(collapse)
library(Hmisc)
setwd("/Users/steven/Documents/elections/ontario/")

num_ridings <- 124

riding_results_2018 <- read.csv("results_2018.csv") %>%
  filter(EventNameEnglish == "2018 General Election") %>%
  mutate(isWinner = ifelse(Plurality > 0, 1, 0)) %>%
  rename(districtid = ElectoralDistrictNumber,
         district = ElectoralDistrictNameEnglish,
         party = PoliticalInterestCode,
         candidateName = NameOfCandidates,
         votes = TotalValidBallotsCast,
         percent = PercentOfTotalValidBallotsCast,
         wasIncumbent = IsMemberOfPreviousLegislature) %>%
  select(districtid, district,
         party, candidateName,
         votes, percent,
         wasIncumbent, isWinner) %>%
  mutate(party = case_when(
    party == "GPO" ~ "Green",
    party == "LIB" ~ "Liberal",
    party == "NDP" ~ "NDP",
    party == "PCP" ~ "PC",
    TRUE ~ "Other"
  )) %>%
  group_by(districtid, party) %>%
  mutate(votes = if_else(party == "Other", sum(votes), votes),
         percent = if_else(party == "Other", sum(percent), percent),
         wasIncumbent = if_else(party == "Other", sum(wasIncumbent), wasIncumbent),
         isWinner = if_else(party == "Other", sum(isWinner), isWinner)) %>%
  ungroup() %>%
  mutate(candidateName = if_else(party == "Other", "", candidateName)) %>%
  distinct()

riding_results_2018 <- riding_results_2018 %>%
  mutate(partyid = case_when(
         party == "PC" ~ 1,
         party == "NDP" ~ 2,
         party == "Liberal" ~ 3,
         party == "Green" ~ 4,
         party == "Other" ~ 5)
  )

# Regions from BB

district_regions <- readRDS("regions.RDS")

riding_results_2018 <- riding_results_2018 %>%
  inner_join(district_regions, by = c("districtid", "district")) %>%
  mutate(votes_province = sum(votes)) %>%
  group_by(region) %>%
  mutate(seats_region = n(),
         votes_region = sum(votes)) %>%
  ungroup() %>%
  group_by(district) %>%
  mutate(votes_district = sum(votes)) %>%
  ungroup() %>%
  mutate(votes_province = sum(votes))

riding_results_2018_wide <- riding_results_2018 %>%
  select(districtid, district, party, region, regionid, percent, votes_district, votes_region, votes_province) %>%
  pivot_wider(names_from = "party", values_from = "percent") 

riding_results_2018_actual <- riding_results_2018_wide

riding_results_2018_wide <- riding_results_2018_wide %>%
  mutate(PC = if_else(is.na(Other), 0.99*PC, PC),
         NDP = if_else(is.na(Other), 0.99*NDP, NDP),
         Liberal = if_else(is.na(Other), 0.99*Liberal, Liberal),
         Green = if_else(is.na(Other), 0.99*Green, Green)
         ) %>%
  mutate(Other = if_else(is.na(Other), 0.01, Other)) %>%
  mutate(NDP = if_else(district == "Parry Sound—Muskoka"|district == "Timmins", NDP + (NDP*Liberal)/(1-Liberal-PC), NDP),
         Green = if_else(district == "Parry Sound—Muskoka"|district == "Timmins", Green + (Green*Liberal)/(1-Liberal-PC), Green),
         Other = if_else(district == "Parry Sound—Muskoka"|district == "Timmins", Other + (Other*Liberal)/(1-Liberal-PC), Other)
         ) %>%
  mutate(Liberal = if_else(district == "Parry Sound—Muskoka"|district == "Timmins", 0, Liberal)) %>%
  mutate(Other = if_else(district == "Cambridge"|district == "Kitchener—Conestoga", Other + PC/15, Other)) %>%
  mutate(PC = if_else(district == "Cambridge"|district == "Kitchener—Conestoga", PC*(14/15), PC))

region_results_2018 <- riding_results_2018_wide %>%
  group_by(region) %>%
  summarise(PC = weighted.mean(PC, votes_district),
            NDP = weighted.mean(NDP, votes_district),
            Liberal = weighted.mean(Liberal, votes_district),
            Green = weighted.mean(Green, votes_district),
            Other = weighted.mean(Other, votes_district),
            votes_region = mean(votes_region)) %>%
  mutate(Other = case_when(
    PC + NDP + Liberal + Green + Other == 1 ~ Other,
    TRUE ~ Other + 1 - (PC + NDP + Liberal + Green + Other))) %>%
  ungroup()

#riding_results_2018 %>%
#  group_by(region) %>%
#  mutate(seats_region = n()) %>%
#  ungroup() %>% filter(region == "905") %>% select(seats_region)

results_2018 <- list(
  Date = ymd(20180607),
  PC = 0.4050,
  NDP = 0.3356,
  Liberal = 0.1959,
  Green = 0.0460,
  Other = 0.0178,
  N = 5806286
) %>% as.data.frame()

simulate_regional_values <- function(percentage_df, N, region_vector){
  f <- function(x, levels) tabulate(factor(x, levels), length(levels))
  regional_values <- rMultinom(percentage_df, N) %>%
    t() %>%
    apply(2, f, colnames(percentage_df)) %>%
    .[]/N
  colnames(regional_values) <- region_vector
  return(regional_values %>% t())
}

simulate_regional_swing <- function(simulations, province_initial, region_initial, N, region_vector, corr_matrix) {
  simulations <- simulations %>% as.matrix() %>% as.array()
  region_initial <- region_initial %>% as.matrix() %>% as.array()
  province_initial <- province_initial %>% as.matrix() %>% as.array()
  
  #Proportional swing -- divide simulated percentage support by last election's percentages
  simulated_provincial_swing <- sweep(simulations, 2, province_initial, "/")
  
  add_error <- function(prob_vector, N, corr_matrix) {
    (MASS::mvrnorm(N, prob_vector, corr_matrix) %>% colMeans()) %>%
      return()
  }

  # Regional swing simulated: 
  simulated_regional_swing <- array(numeric(),c(dim(simulations)[1], dim(region_vector %>% as.array()), dim(simulations)[2]))
  print(dim(region_initial))
  print(dim(simulated_provincial_swing))
  for (i in 1:nrow(simulations)){
    #Multiply regional support last time by overall proportional swing
    simulated_regional_swing[i,,] <-
      sweep(region_initial, 2, simulated_provincial_swing[i,] %>% add_error(N, corr_matrix), "*") %>%
      t() %>% scale(center = FALSE, scale = colSums(.)) %>% t()
    if(i==1){writeLines("\nCalculating expected regional swings:")}
    pb <- txtProgressBar(min = 1, max = nrow(simulations), style = 3)
    setTxtProgressBar(pb, i)
  }
  
  return(simulated_regional_swing)
}

simulate_riding_swing <- function(region_simulations, region_initial, riding_initial, N, riding_name_vector, region_riding_pairing, corr_matrix) {
  region_initial <- region_initial %>% as.matrix() %>% as.array()
  riding_initial <- riding_initial %>% as.matrix() %>% as.array()
  
  add_error <- function(prob_vector, N, corr_matrix) {
    (MASS::mvrnorm(N, prob_vector, corr_matrix) %>% colMeans()) %>%
      return()
  }
  
  #Proportional swing -- divide simulated percentage support by last election's percentages
  simulated_regional_swing <- sweep(region_simulations, c(2,3), region_initial, "/")
  
  # Regional swing simulated: 
  simulated_riding_swing <- array(numeric(),c(dim(region_simulations)[1], dim(riding_name_vector %>% as.array()), dim(region_simulations)[3]))
  #print(dim(riding_initial))
  #print(dim(simulated_riding_swing))
  
  
  
  # Loop over simulations and ridings
  for (i in 1:dim(region_simulations)[1]){
    for (j in 1:dim(simulated_riding_swing)[2]){
      #Multiply regional support last time by overall proportional swing
      simulated_riding_swing[i,j,] <-
        (riding_initial[j,] * add_error(simulated_regional_swing[i,region_riding_pairing[[j,1]],],N,corr_matrix)) %>%
        as.array() %>% t() %>% t() %>% scale(center = FALSE, scale = colSums(.)) %>% t()
    }
    if(i==1){writeLines("\nCalculating expected riding swings:")}
    pb <- txtProgressBar(min = 1, max = dim(region_simulations)[1], style = 3)
    setTxtProgressBar(pb, i)
  }

  return(simulated_riding_swing)
}

start <- Sys.time()
#resample_indices <- round(runif(5000, 1, dim(simulations)[1]))
#resampled_simulations <- simulations[resample_indices,]

region_simulations <- simulate_regional_swing(simulations,
                                              results_2018 %>% select(-c(Date, N)) %>% as.matrix(),
                                              region_results_2018 %>% select(-c(region, votes_region)),
                                              250,
                                              region_results_2018 %>% pull(region),
                                              omega_matrix)
end <- Sys.time()
print(end-start)

ridings_initial <- riding_results_2018_wide %>% select(PC, NDP, Liberal, Green, Other)

start <- Sys.time()
riding_simulations <- simulate_riding_swing(region_simulations,
                                            region_results_2018 %>% select(-c(region, votes_region)),
                                            riding_results_2018_wide %>% select(PC, NDP, Liberal, Green, Other),
                                            200,
                                            riding_results_2018_wide %>% pull(district),
                                            riding_results_2018_wide %>% select(regionid),
                                            omega_matrix)
end <- Sys.time()
print(end-start)

get_riding_winners <- function(riding_simulations){
  riding_winners <- array(numeric(), c(dim(riding_simulations)[1], dim(riding_simulations)[2]))
  for (i in 1:dim(riding_winners)[1]){
    riding_winners[i,] <- riding_simulations[i,,] %>% apply(1, which.max)
  }
  return(riding_winners)
}

get_riding_percentages <- function(riding_winners, N) {
  f <- function(x, levels) tabulate(factor(x, levels), length(levels))
  apply(riding_winners, 2, f, c(1, 2, 3, 4, 5))/N %>%
    return()
}

riding_id_list <-
  riding_results_2018 %>% select(district, districtid) %>% distinct()

riding_winners <- get_riding_winners(riding_simulations) %>%
  as.data.frame() %>%
  rename_all(~(riding_id_list %>%
                 select(district) %>%
                 as.matrix() %>%
                 t())) %>%
  add_rownames(var = "Simulation") #%>%
  #inner_join(simulations %>% add_rownames(var = "Simulation"), by = "Simulation")

riding_win_percentages <- get_riding_percentages(riding_winners, dim(riding_simulations)[1]) %>%
  as.data.frame() %>%
  select(-c("Simulation")) %>%
  rename_all(~(riding_id_list %>%
                 select(district) %>%
                 as.matrix() %>%
                 t()))


riding_winners[c("PC_seats", "NDP_seats", "Liberal_seats", "Green_seats", "Other_seats")] <-
  t(apply(riding_winners,
          1,
          function(x) c(sum(x==1), sum(x==2), sum(x==3), sum(x==4), sum(x==5))
          )
    )


ggplot(riding_winners) +
  geom_histogram(aes(x=PC_seats), color = "dodgerblue4", fill = "dodgerblue4", alpha=0.5, binwidth = 0.5) +
  geom_histogram(aes(x=Liberal_seats), color = "Red4", fill = "Red4",alpha=0.5, binwidth = 0.5) +
  geom_histogram(aes(x=NDP_seats), color = "darkorange2", fill = "darkorange2",alpha=0.5, binwidth = 0.5) +
  theme_minimal() +
  xlab("Number of seats") + ylab("Density") +
  geom_hline(yintercept=0, colour="lightgrey", size=0.5)

ggplot(riding_winners) +
  geom_density(aes(x=PC_seats), color = "dodgerblue4", fill = "dodgerblue4", alpha=0.5) +
  geom_density(aes(x=NDP_seats), color = "darkorange2", fill = "darkorange2",alpha=0.5) +
  geom_density(aes(x=Liberal_seats), color = "Red4", fill = "Red4",alpha=0.5) +
  theme_minimal() +
  xlab("Number of seats") + ylab("Density") +
  geom_hline(yintercept=0, colour="lightgrey", size=0.5)

riding_weights <- riding_results_2018_wide %>%
  mutate(weight = votes_district/votes_province) %>%
  select(districtid, weight) %>%
  .[,2]

get_reweighted_percentages <- function(riding_simulations, riding_weights) {
  reweighted_provincial_sim <- array(numeric(), c(dim(riding_simulations)[1], dim(riding_simulations)[3]))
  for (i in 1:dim(riding_simulations)[1]){
    reweighted_provincial_sim[i,] <- (riding_simulations[i,,] %>% t()) %*% (riding_weights %>% as.matrix())
  }
  return(reweighted_provincial_sim)
}
reweighted_simulations <- get_reweighted_percentages(riding_simulations, riding_weights) %>%
  as.data.frame() %>%
  rename("PC" = "V1", "NDP" = "V2", "Liberal" = "V3", "Green" = "V4", "Other" = "V5")

simulations %>% as.data.frame() %>% summarise_all(mean)

vote_simulation_detailed <- ggplot(reweighted_simulations) +
  geom_histogram(aes(x=PC), color = "Black", fill = "dodgerblue4", alpha=0.5, binwidth = 0.0025) +
  geom_histogram(aes(x=Liberal), color = "Black", fill = "Red4",alpha=0.5, binwidth = 0.0025) +
  geom_histogram(aes(x=NDP), color = "Black", fill = "darkorange2",alpha=0.5, binwidth = 0.0025) +
  geom_histogram(aes(x=Green), color = "Black", fill = "green",alpha=0.5, binwidth = 0.0025) +
  geom_histogram(aes(x=Other), color = "Black", fill = "darkgrey",alpha=0.5, binwidth = 0.0025) +
  theme_minimal() +
  scale_y_continuous(labels = scales::label_percent(scale=1/50), expand = expansion(mult = c(0, 0.25))) +
  xlab("Percent of vote") + ylab("Percent of simulations") +
  scale_x_continuous(labels = scales::label_percent(scale=100)) +
  geom_hline(yintercept=0, colour="lightgrey", size=0.5) +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
ggsave("./figures/vote_simulation_detailed.png")

ggplot(riding_winners) +
  geom_histogram(aes(x=PC_seats), color = "dodgerblue4", fill = "dodgerblue4", alpha=0.5, binwidth = 0.5) +
  geom_histogram(aes(x=Liberal_seats), color = "Red4", fill = "Red4",alpha=0.5, binwidth = 0.5) +
  geom_histogram(aes(x=NDP_seats), color = "darkorange2", fill = "darkorange2",alpha=0.5, binwidth = 0.5) +
  theme_minimal() +
  xlab("Number of seats") + ylab("Density") +
  geom_hline(yintercept=0, colour="lightgrey", size=0.5)

seat_simulation_detailed <- riding_winners %>%
  ggplot() +
  geom_vline(xintercept = 63, colour="Black", size = 4, alpha=0.5) +
  geom_histogram(aes(x=PC_seats), color = "Black", fill = "dodgerblue4", alpha=0.7, binwidth = 1) +
  #geom_vline(xintercept = mean_seats_simulations[[1,1]], color = "dodgerblue4", alpha = 0.7, size = 3) +
  geom_histogram(aes(x=Liberal_seats), color = "Black", fill = "Red4",alpha=0.7, binwidth = 1) +
  #geom_vline(xintercept = mean_seats_simulations[[1,3]], color = "Red4", alpha = 0.7, size = 3) +
  geom_histogram(aes(x=NDP_seats), color = "Black", fill = "darkorange2",alpha=0.7, binwidth = 1) +
  #geom_vline(xintercept = mean_seats_simulations[[1,2]], color = "darkorange2", alpha = 0.7, size = 3) +
  #geom_text(label = "Majority", x = 63, y = 400, angle = 90, vjust = 1) +
  annotate(geom = "text", label = "Majority", size = 5,
           x = 63, y = Inf, angle = 90, 
           vjust = 1.5, hjust = 3) +
  #annotate(geom = "text", label = paste0("PCP best estimate: ", round(mean_seats_simulations[[1,1]])),
  #         x = mean_seats_simulations[[1,1]], y = Inf, angle = 90, 
  #         vjust = 1.5, hjust = 1.25) +
  #annotate(geom = "text", label = paste0("NDP best estimate: ", round(mean_seats_simulations[[1,2]])),
  #         x = mean_seats_simulations[[1,2]], y = Inf, angle = 90, 
  #         vjust = 1.5, hjust = 1.25) +
  #annotate(geom = "text", label = paste0("LPO best estimate: ", round(mean_seats_simulations[[1,3]])),
  #         x = mean_seats_simulations[[1,3]], y = Inf, angle = 90, 
  #         vjust = 1.5, hjust = 1.25) +
  theme_minimal() +
  scale_y_continuous(labels = scales::label_percent(scale=1/50), expand = expansion(mult = c(0, 0.25))) +
  xlab("Number of seats") + ylab("Percent of simulations") +
  geom_hline(yintercept=0, colour="lightgrey", size=0.5)
ggsave("./figures/seat_simulations_detailed.png")


mean_seats_simulations <- riding_winners %>%
  select(PC_seats, NDP_seats, Liberal_seats, Green_seats, Other_seats) %>%
  summarise_all(mean) %>%
  pivot_longer(cols = everything(),
               names_to = "Party",
               names_pattern = "(.*)_seats",
               values_to = "Mean")

percentiles_votes <- reweighted_simulations %>%
  summarise_all(.funs = list(P05 = ~ quantile(x = ., probs = 0.05),
                             P25 = ~ quantile(x = ., probs = 0.25),
                             P50 = ~ quantile(x = ., probs = 0.50),
                             P75 = ~ quantile(x = ., probs = 0.75),
                             P95 = ~ quantile(x = ., probs = 0.95)
  )
  ) %>%
  pivot_longer(everything(), names_to = c("Party", "Percentile"), names_sep = "_") %>%
  pivot_wider(everything(), names_from = "Percentile")

vote_simulation_summary <- percentiles_votes %>%
  ggplot() +
  geom_segment(aes(y = fct_reorder(Party, P50), x = P05, yend = Party, xend = P95, color = Party), size = 2) +
  geom_point(aes(x = P05, y = Party, color = Party), size = 4) +
  geom_point(aes(x = P50, y = Party, color = Party), size = 4) +
  geom_point(aes(x = P95, y = Party, color = Party), size = 4) +
  geom_text(aes(x = P05, y = Party, color = Party, label = scales::label_percent(suffix = "")(round(P05,3))), nudge_x = -0.0175) +
  geom_text(aes(x = P50, y = Party, color = Party, label = scales::label_percent(suffix = "")(round(P50,3))), nudge_y = 0.4) +
  geom_text(aes(x = P95, y = Party, color = Party, label = scales::label_percent(suffix = "")(round(P95,3))), nudge_x = 0.0175) +
  theme_minimal()  +
  scale_color_manual(values = c("PC" = "dodgerblue4",
                                "Liberal" = "Red",
                                "NDP" = "darkorange2",
                                "Green" = "forestgreen",
                                "Other" = "darkgrey"
  )
  )  + 
  scale_x_continuous(labels = scales::label_percent(scale=100)) +
  xlab("Percent of vote") + ylab("Party") +
  theme(aspect.ratio = 0.2)
ggsave("./figures/vote_simulation_summary.png", height=2.5, width=10)

percentiles_seat_simulations <- riding_winners %>%
  select(PC_seats, NDP_seats, Liberal_seats, Green_seats, Other_seats) %>%
  rename_with(~str_remove(., '_seats')) %>%
  summarise_all(.funs = list(P05 = ~ quantile(x = ., probs = 0.05),
                             P25 = ~ quantile(x = ., probs = 0.25),
                             P50 = ~ quantile(x = ., probs = 0.50),
                             P75 = ~ quantile(x = ., probs = 0.75),
                             P95 = ~ quantile(x = ., probs = 0.95)
                             )
                ) %>%
  pivot_longer(everything(), names_to = c("Party", "Percentile"), names_sep = "_") %>%
  pivot_wider(everything(), names_from = "Percentile")

seat_simulation_summary <- percentiles_seat_simulations %>%
  inner_join(mean_seats_simulations, on = "Party") %>%
  ggplot() +
  geom_vline(xintercept = 63, colour="Black", size = 2, alpha = 0.25) +
  geom_segment(aes(y = fct_reorder(Party, P50), x = P05, yend = Party, xend = P95, color = Party), size = 2) +
  geom_point(aes(x = P05, y = Party, color = Party), size = 4) +
  geom_text(aes(x = P05, y = Party, color = Party, label = round(P05)), nudge_x = -2.5) +
  geom_text(aes(x = Mean, y = Party, color = Party, label = round(Mean)), nudge_y = 0.4) +
  geom_text(aes(x = P95, y = Party, color = Party, label = round(P95)), nudge_x = 2.5) +
  geom_point(aes(x = Mean, y = Party, color = Party), size = 4) +
  geom_point(aes(x = P95, y = Party, color = Party), size = 4) +
  theme_minimal()  +
  scale_color_manual(values = c("PC" = "dodgerblue4",
                                "Liberal" = "Red",
                                "NDP" = "darkorange2",
                                "Green" = "forestgreen",
                                "Other" = "darkgrey"
                                )
                     )  + 
  xlab("Number of seats") + ylab("Party") +
  theme(aspect.ratio = 0.2)
ggsave("./figures/seat_simulation_summary.png", height=2.5, width=10)

#rm(campaign_data, campaign_fit, campaign_mod, list_draws)
#save(list = setdiff(ls(), c("campaign_data", "campaign_fit", "campaign_mod", "list_draws")), file = "saved_with_sigma.Rdata")
