library(extrafont)
district_region_list <- district_regions %>% select(district, districtid, region, regionid)


riding_percentages <- (riding_win_percentages %>% rownames_to_column(var = "Party") %>% pivot_longer(!Party, names_to = "district") %>% pivot_wider(names_from = Party) %>%
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

riding_percentages %>%
  inner_join(read.csv("results2022.csv"), on = "districtid") %>%
  mutate(correct = if_else(winner_proj == winner,"Correct","Incorrect")) %>%
  ggplot() +
  geom_bar(aes(y = correct, fill = correct)) + facet_wrap(vars(region)) +
  scale_fill_manual(values = c("forestgreen","red")) +
  geom_text(stat='count', aes(y = correct, label=..count..), nudge_x=2)

riding_percentages %>%
  inner_join(read.csv("results2022.csv"), on = "districtid") %>%
  mutate(correct = if_else(winner_proj == winner,"Correct","Incorrect")) %>%
  mutate(winner = factor(winner, levels=c("PC", "NDP", "Liberal", "Green", "Other"))) %>%
  mutate(winner_proj = factor(winner_proj, levels=c("PC", "NDP", "Liberal", "Green", "Other"))) %>%
  group_by(winner_proj, region) %>%
  mutate(count_bar = n()) %>%
  group_by(winner, winner_proj, region) %>%
  mutate(count_group = n()) %>%
  select(winner, winner_proj, count_group, count_bar, region) %>%
  distinct() %>%
  group_by(winner, region) %>%
  mutate(order = dense_rank(count_group)) %>%
  ggplot(aes(y = winner, x = count_group, fill = winner_proj, label = count_group, group = order)) +
  geom_bar(stat = "identity") +
  geom_text(size = 4, position = position_stack(vjust = 0.5), colour = "white") +
  #geom_text(aes(x = winner, label = paste0("Total: ", count_bar)), size = 4, colour = "black", vjust = 1) +
  scale_fill_manual(values = c("dodgerblue4", "darkorange2",  "red", "forestgreen")) +
  scale_x_continuous(limits = c(0,40), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  ylab("Actual winner") +
  xlab("Number of seats") +
  guides(fill=guide_legend(title="Projected winner")) +
  theme_minimal() + facet_wrap(vars(region)) + theme(legend.position = "bottom") +
  ggtitle("Accuracy of seat count estimates by region", subtitle = " ")  +
  theme(text=element_text(family="Ubuntu"))
ggsave("./blog_figures/ontario_forecast_accuracy_regions.png", width = 9, height = 6)

correct_plot <- riding_percentages %>%
  inner_join(read.csv("results2022.csv"), on = "districtid") %>%
  mutate(correct = if_else(winner_proj == winner,"Correct","Incorrect")) %>%
  mutate(winner = factor(winner, levels=c("PC", "NDP", "Liberal", "Green", "Other"))) %>%
  mutate(winner_proj = factor(winner_proj, levels=c("PC", "NDP", "Liberal", "Green", "Other"))) %>%
  group_by(winner_proj) %>%
  mutate(count_bar = n()) %>%
  group_by(winner, winner_proj) %>%
  mutate(count_group = n()) %>%
  select(winner, winner_proj, count_group, count_bar) %>%
  distinct() %>%
  group_by(winner) %>%
  mutate(order = dense_rank(count_group)) %>%
  ggplot(aes(y = winner, x = count_group, fill = winner_proj, label = count_group, group = order)) +
  geom_bar(stat = "identity") +
  geom_text(size = 4, position = position_stack(vjust = 0.5), colour = "white") +
  #geom_text(aes(x = winner, label = paste0("Total: ", count_bar)), size = 4, colour = "black", vjust = 1) +
  scale_fill_manual(values = c("dodgerblue4", "darkorange2",  "red", "forestgreen")) +
  scale_x_continuous(limits = c(0,90), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  ylab("Actual winner") +
  xlab("Number of seats") +
  guides(fill=guide_legend(title="Projected winner")) +
  theme_minimal()

correct_plot_actual <- riding_percentages_actual %>%
  inner_join(read.csv("results2022.csv"), on = "districtid") %>%
  mutate(correct = if_else(winner_proj == winner,"Correct","Incorrect")) %>%
  mutate(winner = factor(winner, levels=c("PC", "NDP", "Liberal", "Green", "Other"))) %>%
  mutate(winner_proj = factor(winner_proj, levels=c("PC", "NDP", "Liberal", "Green", "Other"))) %>%
  group_by(winner_proj) %>%
  mutate(count_bar = n()) %>%
  group_by(winner, winner_proj) %>%
  mutate(count_group = n()) %>%
  select(winner, winner_proj, count_group, count_bar) %>%
  distinct() %>%
  group_by(winner) %>%
  mutate(order = dense_rank(count_group)) %>%
  ggplot(aes(y = winner, x = count_group, fill = winner_proj, label = count_group, group = order)) +
  geom_bar(stat = "identity") +
  geom_text(size = 4, position = position_stack(vjust = 0.5), colour = "white") +
  #geom_text(aes(x = winner, label = paste0("Total: ", count_bar)), size = 4, colour = "black", vjust = 1) +
  scale_fill_manual(values = c("dodgerblue4", "darkorange2",  "red", "forestgreen")) +
  scale_x_continuous(limits = c(0,90), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  ylab("Actual winner") +
  xlab("Number of seats") +
  guides(fill=guide_legend(title="Projected winner")) +
  theme_minimal()

ggpubr::ggarrange(correct_plot + ggtitle("Accuracy of seat count estimates...", subtitle = "using estimates from model of vote intention on election day") +
                    labs(caption = "Most likely winners across 10,000 simulations") +
                    theme(plot.caption = element_text(hjust = 0)) +
                    theme(text=element_text(family="Ubuntu")),
                  correct_plot_actual + ggtitle("Accuracy of seat count estimates...", subtitle = "using actual province-wide results") +
                    labs(caption = "Most likely winner given province-wide vote") +
                    theme(plot.caption = element_text(hjust = 0)) +
                    theme(text=element_text(family="Ubuntu")),
                  nrow = 1, ncol = 2, common.legend = TRUE,  legend="bottom")
ggsave("./blog_figures/ontario_forecast_accuracy.png", width = 9, height = 6)
  
compare <- riding_percentages %>%
  inner_join(read.csv("results2022.csv"), on = "districtid") %>%
  mutate(correct = if_else(winner_proj == winner,1,0)) %>%
  pivot_longer(cols = c("PC", "NDP", "Liberal", "Green", "Other"),
               names_to = "Party",
               values_to = "Probability") %>%
  mutate(is_winner = if_else(winner == Party, 1, 0)) %>%
  mutate(bin = cut(Probability, breaks =  seq(0,1,0.01), right = T, labels = T))

compare %>% ggplot() +
  geom_abline() +
  geom_point(aes(x = Probability, y = is_winner)) +
  geom_smooth(aes(x = Probability, y = is_winner))

predictions_correct_figure <- compare %>% 
  mutate(bin = round(Probability*2,1)/2) %>%
  group_by(bin) %>%
  summarise(actual = mean(is_winner), n = n()) %>%
  ggplot() +
  geom_smooth(aes(x = bin, y = actual, weight = n, fill = "red"), alpha = 0.2, size = 3, colour = "red") +
  #geom_point(data = . %>% filter(n>=2), aes(x = bin, y = actual, size = n), colour = "black") +
  geom_abline(size = 4, colour = "forestgreen") + 
  theme_minimal() +
  scale_x_continuous(limits=c(0, 1), expand = c(0, 0), labels = scales::label_percent()) +
  scale_y_continuous(limits=c(0, 1), expand = c(0, 0), labels = scales::label_percent()) +
  xlab("Predicted probability") +
  ylab("Actual probability")

predictions_correct_plot <- ggplot_build(predictions_correct_figure)

# extract data for the loess lines from the 'data' slot
predictions_correct_df <-
  data.frame(x = predictions_correct_plot$data[[1]]$x,
             y = predictions_correct_plot$data[[1]]$y) %>%
  mutate(ymax = pmax(x,y),
         ymin = pmin(x,y))

predictions_correct_figure$layers <- c(geom_ribbon(data = predictions_correct_df, aes(x = x, ymin = ymin, ymax = ymax),
                            fill = "red", alpha = 1), predictions_correct_figure$layers)
  

# use the loess data to add the 'ribbon' to plot 
predictions_correct_figure +
  theme(legend.position = "none") + 
  ggtitle("Model predicted probability versus actual probability", subtitle = " ") +
  theme(text=element_text(family="Ubuntu"))
ggsave("./blog_figures/ontario_forecast_calibration.png", width = 9, height = 6)


riding_percentages %>%
  inner_join(riding_percentages_actual %>% select(district, winner_proj),
             by = c("district"="district"),
             suffix = c("", "_actual_provincewide")) %>%
  filter(winner_proj != winner_proj_actual_provincewide)