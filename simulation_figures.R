list_draws <- extract(campaign_fit)

list_parties <- c("PC", "NDP", "Liberal", "Green", "Other")

simulations <- list_draws$xi[,dim(list_draws$xi)[2],] %>% 
  as.data.frame() %>%
  rename_at(vars((1:5)), ~ list_parties)

ggplot(simulations) +
  geom_density(aes(x=PC), color = "dodgerblue4", fill = "dodgerblue4", alpha=0.4) +
  geom_density(aes(x=Liberal), color = "Red4", fill = "Red4",alpha=0.4) +
  geom_density(aes(x=NDP), color = "darkorange2", fill = "darkorange2",alpha=0.4) +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Voting intention (%)") + ylab("Density") +
  coord_cartesian(ylim=c(3, 100)) +
  geom_hline(yintercept=0, colour="lightgrey", size=0.5)

ggplot(simulations) +
  geom_density(aes(x=Other),color = "darkgrey", fill = "darkgrey", alpha=0.4) +
  geom_density(aes(x=Green),color = "forestgreen", fill = "forestgreen",alpha=0.4) +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Voting intention (%)") + ylab("Density") +
  geom_hline(yintercept=0, colour="lightgrey", size=0.5)

ggplot(simulations) +
  geom_histogram(aes(x=PC), color = "dodgerblue4", fill = "dodgerblue4", alpha=0.4, bins = 250) +
  geom_histogram(aes(x=Liberal), color = "Red4", fill = "Red4",alpha=0.4, bins = 250) +
  geom_histogram(aes(x=NDP), color = "darkorange2", fill = "darkorange2",alpha=0.4, bins = 250) +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Voting intention (%)") + ylab("Number of simulations") +
  geom_hline(yintercept=0, colour="lightgrey", size=0.5)

ggplot(simulations) +
  geom_histogram(aes(x=Other),color = "darkgrey", fill = "darkgrey", alpha=0.4, bins = 250) +
  geom_histogram(aes(x=Green),color = "forestgreen", fill = "forestgreen",alpha=0.4, bins = 250) +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Voting intention (%)") + ylab("Number of simulations") +
  geom_hline(yintercept=0, colour="lightgrey", size=0.5)

ggplot(simulations) +
  geom_histogram(aes(x=PC), color = "Black", fill = "dodgerblue4", alpha=0.5, bins = 250) +
  geom_histogram(aes(x=Liberal), color = "Black", fill = "Red4",alpha=0.5, bins = 250) +
  geom_histogram(aes(x=NDP), color = "Black", fill = "darkorange2",alpha=0.5, bins = 250) +
  geom_histogram(aes(x=Other),color = "Black", fill = "darkgrey", alpha=0.4, bins = 250) +
  geom_histogram(aes(x=Green),color = "Black", fill = "forestgreen",alpha=0.4, bins = 250) +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Voting intention (%)") + ylab("Number of simulations") +
  geom_hline(yintercept=0, colour="lightgrey", size=0.5)
ggsave("./figures/simulations.png")

simulations %>% 
  mutate(lib_higher_ndp = if_else(Liberal >= NDP, 1,0)) %>%
  mutate(lib_higher_pc = if_else(Liberal >= PC, 1,0)) %>%
  summarise(mean(lib_higher_ndp), mean(lib_higher_pc))