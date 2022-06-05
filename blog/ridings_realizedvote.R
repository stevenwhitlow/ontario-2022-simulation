# Evaluate model performance with actual province-wide vote
district_region_list <- district_regions %>% select(district, districtid, region, regionid)

simulate_regional_swing_actual <- function(simulations, province_initial, region_initial, region_vector) {
  simulations <- simulations %>% as.matrix() %>% as.array()
  region_initial <- region_initial %>% as.matrix() %>% as.array()
  province_initial <- province_initial %>% as.matrix() %>% as.array()
  
  #Proportional swing -- divide simulated percentage support by last election's percentages
  simulated_provincial_swing <- sweep(simulations, 2, province_initial, "/")

  # Regional swing simulated: 
  simulated_regional_swing <- array(numeric(),c(dim(simulations)[1], dim(region_vector %>% as.array()), dim(simulations)[2]))
  print(dim(region_initial))
  print(dim(simulated_provincial_swing))
  for (i in 1:nrow(simulations)){
    #Multiply regional support last time by overall proportional swing
    simulated_regional_swing[i,,] <-
      sweep(region_initial, 2, simulated_provincial_swing[i,], "*") %>%
      t() %>% scale(center = FALSE, scale = colSums(.)) %>% t()

  }
  
  return(simulated_regional_swing)
}

simulate_riding_swing_actual <- function(region_simulations, region_initial, riding_initial, riding_name_vector, region_riding_pairing) {
  region_initial <- region_initial %>% as.matrix() %>% as.array()
  riding_initial <- riding_initial %>% as.matrix() %>% as.array()
  
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
        (riding_initial[j,] * simulated_regional_swing[i,region_riding_pairing[[j,1]],]) %>%
        as.array() %>% t() %>% t() %>% scale(center = FALSE, scale = colSums(.)) %>% t()
    }

  }
  
  return(simulated_riding_swing)
}



region_simulations_actual <- simulate_regional_swing_actual(matrix(c(40.8, 23.7, 23.8, 6.0, 5.7), nrow=1, ncol=5, byrow=TRUE),
                                              results_2018 %>% select(-c(Date, N)) %>% as.matrix(),
                                              region_results_2018 %>% select(-c(region, votes_region)),
                                              region_results_2018 %>% pull(region))
end <- Sys.time()
print(end-start)

ridings_initial <- riding_results_2018_wide %>% select(PC, NDP, Liberal, Green, Other)

start <- Sys.time()
riding_simulations_actual <- simulate_riding_swing_actual(region_simulations_actual,
                                            region_results_2018 %>% select(-c(region, votes_region)),
                                            riding_results_2018_wide %>% select(PC, NDP, Liberal, Green, Other),
                                            riding_results_2018_wide %>% pull(district),
                                            riding_results_2018_wide %>% select(regionid))

riding_winners_actual <- get_riding_winners(riding_simulations_actual) %>%
  as.data.frame() %>%
  rename_all(~(riding_id_list %>%
                 select(district) %>%
                 as.matrix() %>%
                 t())) %>%
  add_rownames(var = "Simulation") #%>%
#inner_join(simulations %>% add_rownames(var = "Simulation"), by = "Simulation")

riding_win_percentages_actual <- get_riding_percentages(riding_winners_actual, dim(riding_simulations_actual)[1]) %>%
  as.data.frame() %>%
  select(-c("Simulation")) %>%
  rename_all(~(riding_id_list %>%
                 select(district) %>%
                 as.matrix() %>%
                 t()))


riding_winners_actual[c("PC_seats", "NDP_seats", "Liberal_seats", "Green_seats", "Other_seats")] <-
  t(apply(riding_winners_actual,
          1,
          function(x) c(sum(x==1), sum(x==2), sum(x==3), sum(x==4), sum(x==5))
  )
  )

riding_percentages_actual <- (riding_win_percentages_actual %>% rownames_to_column(var = "Party") %>% pivot_longer(!Party, names_to = "district") %>% pivot_wider(names_from = Party) %>%
                         rename(PC = "1",NDP = "2",Liberal = "3",Green = "4",Other = "5")) %>%
  inner_join(district_region_list, on="district") %>%
  select(-c(regionid,districtid)) %>% 
  mutate(winner_proj = case_when(
    PC == pmax(PC, NDP, Liberal, Green, Other) ~ "PC",
    NDP == pmax(PC, NDP, Liberal, Green, Other) ~ "NDP",
    Liberal == pmax(PC, NDP, Liberal, Green, Other) ~ "Liberal",
    Green == pmax(PC, NDP, Liberal, Green, Other) ~ "Green",
    Other == pmax(PC, NDP, Liberal, Green, Other) ~ "Other",
  ))