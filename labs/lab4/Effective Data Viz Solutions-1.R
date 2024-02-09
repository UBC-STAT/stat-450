# Solutions to Effective Data Visualization worksheet

# 0. Introduction ####

# Question 0.1
# A, C, D

# Question 0.2
# A

# Question 0.3
# D

# Question 0.4
# D


# 1. World Vaccination ####

# Question 1.0
# A, B, B

# Question 1.1.1
world_vaccination <- world_vaccination %>%
  filter(!is.na(pct_vaccinated), who_region != "(WHO) Global")
head(world_vaccination)

# Question 1.1.2
# A

# Question 1.2
world_vacc_plot <- world_vaccination %>% 
  ggplot(aes(x = year, y = pct_vaccinated)) +
  geom_point() +
  labs(x = "Year",
       y = "Percent of people vaccinated")
world_vacc_plot

# Question 1.3
# D

# Question 1.4
compare_vacc_plot <- world_vaccination %>% 
  ggplot(aes(x = year, y = pct_vaccinated)) +
  geom_point(aes(colour = vaccine, shape = vaccine)) +
  labs(x = "Year",
       y = "Percent of people vaccinated",
       colour = "Type of vaccine",
       shape = "Type of vaccine")
compare_vacc_plot

# Question 1.5
polio <- world_vaccination %>% 
  filter(vaccine == "polio")
head(polio)

# Question 1.6
polio_regions <- polio %>% 
  ggplot(aes(x = year, y = pct_vaccinated)) +
  geom_point(aes(colour = who_region, shape = who_region)) +
  labs(x = "Year",
       y = "Percent of people vaccinated")
polio_regions


# Question 1.7.1
polio_regions_line <- polio %>% 
  ggplot(aes(x = year, y = pct_vaccinated)) +
  geom_line(aes(colour = who_region)) +
  labs(x = "Year",
       y = "Percent of people vaccinated")
polio_regions_line

# Question 1.7.2
polio_regions_line <- polio_regions_line +
  labs(colour = "Region of the world")
polio_regions_line

# Question 1.8
side_by_side_world <- world_vaccination %>% 
  ggplot(aes(x = year, y = pct_vaccinated)) +
  geom_line(aes(colour = who_region)) +
  labs(x = "Year",
       y = "Percent of people vaccinated",
       colour = "Region of the world") +
  facet_grid(. ~ vaccine)
side_by_side_world

# Question 1.9.1
vertical_world <- world_vaccination %>% 
  ggplot(aes(x = year, y = pct_vaccinated)) +
  geom_line(aes(colour = who_region)) +
  labs(x = "Year",
       y = "Percent of people vaccinated",
       colour = "Region of the world") +
  facet_grid(vaccine ~ .)
vertical_world

# Question 1.9.1
# D


# 2. Fast Food Chains in the United States ####

# Question 2.1
# B

# Question 2.2.1
head(fast_food, n=10)

# Question 2.2.2
# C, B

# Question 2.3
top_restaurants <- fast_food %>% 
  filter(state %in% c("CA", "WA", "OR")) %>% 
  group_by(name) %>% 
  summarise(n = n()) %>% 
  top_n(9, wt = n) %>% 
  arrange(desc(n))
top_restaurants

# Question 2.4
# C

# Question 2.5
count_bar_chart <- top_restaurants %>% 
  ggplot(aes(x = name, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Restaurant",
       y = "Number of branches on the west coast")
count_bar_chart

# Question 2.6 A
count_bar_chart_A <- count_bar_chart +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
count_bar_chart_A

# Question 2.6 B
count_bar_chart_B <- count_bar_chart +
  coord_flip()
count_bar_chart_B

# Question 2.6 C
count_bar_chart_C <- top_restaurants %>% 
  ggplot(aes(x = reorder(name, n), y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Restaurant",
       y = "Number of branches on the west coast") +
  coord_flip()
count_bar_chart_C

# Question 2.7
top_n_state <- fast_food %>% 
  semi_join(top_restaurants) %>%
  filter(state %in% c("CA", "WA", "OR")) %>% 
  group_by(name, state) %>% 
  summarise(n = n())
top_n_state

# Question 2.8
top_n_state_plot <- top_n_state %>% 
  ggplot(aes(x = state, y = n, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "State",
       y = "Number of branches",
       fill = "Restaurant")
top_n_state_plot

# Question 2.9
top_n_state_plot <- top_n_state %>% 
  ggplot(aes(x = state, y = n, fill = name)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "State",
       y = "Number of branches",
       fill = "Restaurant")
top_n_state_plot

# Question 2.10.1
# A. Yes

# Question 2.10.2
# C

# Question 2.11
state_counts <- fast_food %>% 
  semi_join(top_restaurants) %>% 
  filter(state %in% c("CA", "WA", "OR")) %>% 
  group_by(state) %>% 
  summarise(n = n())
state_counts

# Question 2.12
state_counts_plot <- state_counts %>% 
  ggplot(aes(x = state, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "State",
       y = "Number of restaurants (top 9 franchises)")
state_counts_plot


# Question 2.13.1
# B

# Question 2.13.2
state_counts$population <- c(39.512, 4.217, 7.615)
state_counts %>%
  mutate(n.per.capita = n / population)

# Question 2.13.3
# A

# Question 2.14
# C
# A and B are both valid, although both have to be marked to get a correct answer