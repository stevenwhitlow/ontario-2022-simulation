xi <- summary(campaign_fit, par = "xi") %>%
  as.data.frame() %>%
  rownames_to_column("parameter") %>%
  mutate(day = as.numeric(str_extract(parameter, "(?<=\\[)\\d+")),
  partyid = as.numeric(str_extract(parameter, "\\d+(?=\\])"))) %>%  #rename_all(~str_replace(.,"^summary\\.","")) %>%
  mutate(date = ymd(20180607) + day) %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  rename(Date = date)

polling <- polling %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"))

delta <- summary(campaign_fit, par = "delta") %>%
  as.data.frame() %>%
  rownames_to_column("parameter") %>% 
  mutate(partyid = as.numeric(str_extract(parameter, "(?<=\\[)\\d+")),
         firmid = as.numeric(str_extract(parameter, "\\d+(?=\\])")))

omega <- summary(campaign_fit, par = "omega") %>%
  as.data.frame() %>%
  rownames_to_column("parameter") 

omega_matrix <- matrix(0, nrow = 5, ncol = 5)
for (row in 1:dim(omega_matrix)[1]){
  omega_matrix[row,] <- omega %>% select(summary.mean) %>% .[(1+(row*5)-5):(5+(row*5)-5),]
}
  
results_2018_df <- results_2018 %>%
  as.data.frame() %>%
  pivot_longer(!c(Date, N), names_to = "party", values_to = "actual_vote")

firm_list <- polling %>% select(Firm, firmid) %>% distinct()
party_list <- delta %>%
  select(partyid) %>% distinct() %>%
  mutate(party = case_when(
    partyid == 1 ~ "PC",
    partyid == 2 ~ "NDP",
    partyid == 3 ~ "Liberal",
    partyid == 4 ~ "Green",
    partyid == 5 ~ "Other")
    )

delta <- delta %>%
  inner_join(firm_list, by = "firmid") %>%
  inner_join(party_list, by = "partyid") %>%
  inner_join(results_2018_df, by = "party")

delta <- delta %>%
  mutate(relative = summary.mean/actual_vote*100)

delta <- delta %>%
  inner_join(polling %>%
  group_by(Firm) %>%
  tally(name = "num_polls_by_firm") %>%
  ungroup(), by = "Firm")

sigma <- summary(campaign_fit, par = "sigma") %>%
  as.data.frame() %>%
  rownames_to_column("parameter") %>%
  mutate(firmid = as.numeric(str_extract(parameter, "(?<=\\[)\\d+"))) %>%
  inner_join(firm_list, by = "firmid")

sigma %>% select(summary.mean, Firm) %>% arrange(desc(summary.mean))

sigma %>% select(summary.mean, Firm) %>%
  inner_join(delta %>% select(Firm, num_polls_by_firm), by = ("Firm")) %>%
  select(summary.mean, Firm, num_polls_by_firm) %>%
  distinct() %>%
  summarise(weighted.mean(summary.mean, num_polls_by_firm))

delta %>% filter(num_polls_by_firm >= 5) %>% View()

xi <- xi %>%
  inner_join(party_list, by = "partyid")

ggplot() +
  geom_point(data = polling,
             mapping = aes(x = Date, y = PC, colour = Firm)) +
  geom_ribbon(data = xi %>% filter(partyid==1), mapping = aes(x = Date, ymin = summary.2.5., ymax = summary.97.5.), alpha = 0.2) +
  geom_line(data = xi %>% filter(partyid==1), mapping = aes(x = Date, y = summary.mean)) +
  theme(legend.position="bottom") +
  scale_x_date(date_labels = "%b %Y") +
  geom_point(data = results_2018_df %>%
               pivot_wider(names_from = "party", values_from = "actual_vote"),
             mapping = aes(x = Date, y = PC)) +
  geom_text(data = results_2018_df %>%
              pivot_wider(names_from = "party", values_from = "actual_vote"),
            mapping = aes(x = Date, y = PC, label = "2018"), hjust=0.5,vjust=-1)
ggsave("./figures/pc.png")

ggplot() +
  geom_point(data = polling %>% filter(Date >= ymd(20220501)),
             mapping = aes(x = Date, y = PC, colour = Firm)) +
  geom_ribbon(data = xi %>% filter(partyid==1 & Date >= ymd(20220501)), mapping = aes(x = Date, ymin = summary.2.5., ymax = summary.97.5.), alpha = 0.2) +
  geom_line(data = xi %>% filter(partyid==1 & Date >= ymd(20220501)), mapping = aes(x = Date, y = summary.mean)) +
  theme(legend.position="bottom") +
  scale_x_date(date_labels = "%a %b %d")
ggsave("./figures/pc_election.png")

ggplot() +
  geom_point(data = results_2018_df %>%
               pivot_wider(names_from = "party", values_from = "actual_vote"),
             mapping = aes(x = Date, y = NDP)) +
  geom_text(data = results_2018_df %>%
              pivot_wider(names_from = "party", values_from = "actual_vote"),
            mapping = aes(x = Date, y = NDP, label = "2018"), hjust=0.5,vjust=-1) +
  geom_ribbon(data = xi %>% filter(partyid==2), mapping = aes(x = Date, ymin = summary.2.5., ymax = summary.97.5.), alpha = 0.2) +
  geom_line(data = xi %>% filter(partyid==2), mapping = aes(x = Date, y = summary.mean)) +
  geom_point(data = polling,
             mapping = aes(x = Date, y = NDP, colour = Firm)) +
  theme(legend.position="bottom") +
  scale_x_date(date_labels = "%b %Y")
ggsave("./figures/ndp.png")

ggplot() +
  geom_point(data = polling %>% filter(Date >= ymd(20220401)),
             mapping = aes(x = Date, y = NDP, colour = Firm)) +
  geom_ribbon(data = xi %>% filter(partyid==2 & Date >= ymd(20220401)), mapping = aes(x = Date, ymin = summary.2.5., ymax = summary.97.5.), alpha = 0.2) +
  geom_line(data = xi %>% filter(partyid==2 & Date >= ymd(20220401)), mapping = aes(x = Date, y = summary.mean)) +
  theme(legend.position="bottom") +
  scale_x_date(date_labels = "%a %b %d")
ggsave("./figures/ndp_election.png")

ggplot() +
  geom_point(data = polling,
             mapping = aes(x = Date, y = Liberal, colour = Firm)) +
  geom_point(data = results_2018_df %>%
               pivot_wider(names_from = "party", values_from = "actual_vote"),
             mapping = aes(x = Date, y = Liberal)) +
  geom_text(data = results_2018_df %>%
              pivot_wider(names_from = "party", values_from = "actual_vote"),
            mapping = aes(x = Date, y = Liberal, label = "2018"), hjust=0.5,vjust=-1) +
  geom_ribbon(data = xi %>% filter(partyid==3), mapping = aes(x = Date, ymin = summary.2.5., ymax = summary.97.5.), alpha = 0.2) +
  geom_line(data = xi %>% filter(partyid==3), mapping = aes(x = Date, y = summary.mean)) +
  theme(legend.position="bottom") +
  scale_x_date(date_labels = "%b %Y")
ggsave("./figures/liberal.png")

ggplot() +
  geom_point(data = polling %>% filter(Date >= ymd(20220401)),
             mapping = aes(x = Date, y = Liberal, colour = Firm)) +
  geom_ribbon(data = xi %>% filter(partyid==3 & Date >= ymd(20220401)), mapping = aes(x = Date, ymin = summary.2.5., ymax = summary.97.5.), alpha = 0.2) +
  geom_line(data = xi %>% filter(partyid==3 & Date >= ymd(20220401)), mapping = aes(x = Date, y = summary.mean)) +
  theme(legend.position="bottom") +
  scale_x_date(date_labels = "%a %b %d")
ggsave("./figures/liberal_election.png")

ggplot() +
  geom_point(data = results_2018_df %>%
               pivot_wider(names_from = "party", values_from = "actual_vote"),
             mapping = aes(x = Date, y = Green)) +
  geom_text(data = results_2018_df %>%
              pivot_wider(names_from = "party", values_from = "actual_vote"),
            mapping = aes(x = Date, y = Green, label = "2018"), hjust=0.5,vjust=-1) +
  geom_point(data = polling,
             mapping = aes(x = Date, y = Green, colour = Firm)) +
  geom_ribbon(data = xi %>% filter(partyid==4), mapping = aes(x = Date, ymin = summary.2.5., ymax = summary.97.5.), alpha = 0.2) +
  geom_line(data = xi %>% filter(partyid==4), mapping = aes(x = Date, y = summary.mean)) +
  theme(legend.position="bottom") +
  scale_x_date(date_labels = "%b %Y")
ggsave("./figures/green.png")

ggplot() +
  geom_ribbon(data = xi %>% filter(partyid==5 & Date >= ymd(20220401)), mapping = aes(x = Date, ymin = summary.2.5., ymax = summary.97.5.), alpha = 0.2) +
  geom_line(data = xi %>% filter(partyid==5 & Date >= ymd(20220401)), mapping = aes(x = Date, y = summary.mean)) +
  geom_point(data = polling %>% filter(Date >= ymd(20220401)),
             mapping = aes(x = Date, y = Other, colour = Firm)) +
  theme(legend.position="bottom") +
  scale_x_date(date_labels = "%a %b %d")
ggsave("./figures/other_election.png")

election_changes <- ggplot() +
  geom_ribbon(data = xi %>% filter(partyid==1 & Date >= ymd(20220501)), mapping = aes(x = Date, ymin = summary.2.5., ymax = summary.97.5.), alpha = 0.2, fill = "dodgerblue4") +
  geom_point(data = polling %>% filter(Date >= ymd(20220501)),
             mapping = aes(x = Date, y = PC), alpha = 0.2, colour = "dodgerblue4") +
  geom_line(data = xi %>% filter(partyid==1 & Date >= ymd(20220501)), mapping = aes(x = Date, y = summary.mean), size = 1.5, color = "dodgerblue4") +
  geom_ribbon(data = xi %>% filter(partyid==2 & Date >= ymd(20220501)), mapping = aes(x = Date, ymin = summary.2.5., ymax = summary.97.5.), alpha = 0.2, fill = "darkorange2") +
  geom_point(data = polling %>% filter(Date >= ymd(20220501)),
             mapping = aes(x = Date, y = NDP), alpha = 0.2, colour = "darkorange2") +
  geom_line(data = xi %>% filter(partyid==2 & Date >= ymd(20220501)), mapping = aes(x = Date, y = summary.mean), size = 1.5, color = "darkorange2") +
  geom_ribbon(data = xi %>% filter(partyid==3 & Date >= ymd(20220501)), mapping = aes(x = Date, ymin = summary.2.5., ymax = summary.97.5.), alpha = 0.2, fill = "Red") +
  geom_point(data = polling %>% filter(Date >= ymd(20220501)),
             mapping = aes(x = Date, y = Liberal), alpha = 0.2, colour = "Red") +
  geom_line(data = xi %>% filter(partyid==3 & Date >= ymd(20220501)), mapping = aes(x = Date, y = summary.mean), size = 1.5, color = "Red") +
  theme(legend.position="bottom") +
  scale_y_continuous(labels = scales::percent) +
  xlab("Date") + ylab("Estimated vote intention") +
  theme_minimal() +
  scale_x_date(date_labels = "%a %b %d")
ggsave("./figures/election_changes.png")