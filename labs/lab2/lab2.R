library(tidyverse)
library(palmerpenguins)

pgs <- penguins %>% drop_na()


# question for TA ####

pgs %>% 
  dplyr::group_by(sex) %>% 
  dplyr::summarise(avg_body_mass = mean(body_mass_g))


pgs %>% 
  ggplot(aes(body_mass_g, fill = sex)) +
  geom_histogram(position = 'identity', alpha = 0.6)


#pgs %>% 
#  ggplot(aes(body_mass_g, fill = sex)) +
#  geom_histogram(color = "grey20", alpha = 0.4, position = 'identity') +
#  scale_fill_viridis_d(begin = 0.15, end = 0.7)


pgs %>% 
  dplyr::group_by(species, sex) %>% 
  dplyr::summarise(avg_body_mass = mean(body_mass_g))


pgs %>% 
  ggplot(aes(x = sex, y = body_mass_g)) +
  geom_boxplot() +
  facet_wrap(.~species)


theme_set(theme_classic())
pgs %>% 
  ggplot(aes(x = sex, y = body_mass_g)) +
  geom_boxplot() +
  facet_wrap(.~species) +
  labs(x = "Sex",
       y = "Body mass (g)") +
  theme(text = element_text(16))
  

# Answer to client: Male penguins have a larger body mass than female penguins of the same species

# question for students ####

pgs %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point()


pgs %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise(avg_bill_length = mean(bill_length_mm),
                   avg_bill_depth = mean(bill_depth_mm))


pgs %>% 
  ggplot(aes(x = bill_length_mm, bill_depth_mm, color = species)) +
  geom_point()


pgs %>% 
  ggplot(aes(x = bill_length_mm, bill_depth_mm, color = species)) +
  geom_point(size = 3, alpha = 0.6) +
  labs(x = "Bill length (mm)",
       y = "Bill depth (mm)",
       color = "Species:")



# my answer:

theme_set(theme_classic())
pgs %>% 
  ggplot(aes(x = bill_length_mm, bill_depth_mm, color = species)) +
  geom_point(size = 3, alpha = 0.6) +
  labs(x = "Bill length (mm)",
       y = "Bill depth (mm)",
       color = "Species:") +
  scale_color_viridis_d(begin = 0.15, end = 0.85) +
  theme(legend.position = "top",
        text = element_text(size=16))

  
