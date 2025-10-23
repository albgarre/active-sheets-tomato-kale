
library(readxl)
library(tidyverse)
library(biogrowth)
library(cowplot)
library(GGally)
library(ggsci)

## Import the data

d_color <- c(10, 15, 22) %>%
  set_names(., .) %>%
  map(., ~ paste0(., "C_forR")) %>%
  map(.,
      ~ read_excel("./data/Tarea 8/Color Tomate .xlsx", sheet = .)
  ) %>%
  imap_dfr(., ~ mutate(.x, temp = .y))

d_caroteno <- c(10, 15, 22) %>%
  set_names(., .) %>%
  map(., ~ paste0(., "C_forR")) %>%
  map(.,
      ~ read_excel("./data/Tarea 11/Carotenos_T.cherry.xlsx", sheet = .)
  ) %>%
  imap_dfr(., ~ mutate(.x, temp = .y)) %>%
  filter(!is.na(carotenos))

## Arrange and combine

d <- d_color %>%
  pivot_longer(-c("temp", "treatment", "day")) %>%
  summarize(value = mean(value, na.rm = TRUE),
            .by = c("treatment", "day", "temp", "name")) %>%
  pivot_wider() %>%
  full_join(.,
            summarize(d_caroteno,
                      carotenos  = mean(carotenos , na.rm = TRUE),
                      .by = c("treatment", "day", "temp")
                      )
            )

## Correlations

d %>%
  select(-treatment, -day, -temp) %>%
  ggpairs()

d %>%
  select(-day, -temp) %>%
  ggpairs(mapping = aes(colour = treatment))

d %>%
  select(-day, -treatment) %>%
  ggpairs(mapping = aes(colour = temp))

#' Chroma is quite bad
#' The other 4 are quite good
#' The correlations are low for 10ºC because the carotenoids have no variance. So, we only see the noise.
#' 

## Supp. Figure X

p <- d %>%
  select(-day, -temp) %>%
  pivot_longer(-c("treatment", "carotenos")) %>%
  filter(name != "Color index") %>%
  mutate(carotenos = carotenos*10) %>%
  ggplot(aes(x = value, y = carotenos, colour = treatment)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  facet_wrap("name", scales = "free") +
  theme_bw(base_size = 14) +
  scale_color_manual(values = c("steelblue", "darkorange")) +
  theme(legend.position = "none") +
  labs(x = "Value (·)",
       y = "Total carotenoid content (mg/kg)")

p

ggsave(p, filename = "supp_figure_caroteno_color.png", width = 9, height = 6)

## Figure 5

p <- d %>%
  select(-day, -temp) %>%
  pivot_longer(-c("treatment", "carotenos")) %>%
  filter(name == "Color index") %>%
  mutate(carotenos = carotenos*10) %>%
  ggplot(aes(x = value, y = carotenos)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  # facet_wrap("name", scales = "free") +
  theme_bw(base_size = 14) +
  scale_color_manual(values = c("steelblue", "darkorange")) +
  theme(legend.position = "none") +
  labs(x = expression(Color~index~(2000~"·"~a^"*"/(L^"*"~"·"~Chroma))),
       y = "Total carotenoid content (mg/kg)")
  # labs(x = "Color index (2000·a/L/Chroma)",
  #      y = "Total carotenoid content (mg/kg)")

p

ggsave(p, filename = "Figure_5.png", width = 6, height = 4)


 ## Linear model

my_model <- lm(carotenos ~ `Color index`, data = d)

summary(my_model)












