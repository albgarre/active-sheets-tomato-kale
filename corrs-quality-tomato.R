
library(tidyverse)
library(readxl)
library(corrplot)
library(GGally)

## Load data

d_firmeza <- c("anova 10 º C", "anova 15 º C", "anova 22 º C ") %>%
  map(.,
      ~ read_excel("./data/Tarea 8/Firmeza T. cherry.xlsx", sheet = .)
  ) %>%
  map2_dfr(., c(10, 15, 22),
           ~ mutate(.x, temp = .y)
  ) %>%
  summarize(firmeza = mean(firmeza, na.rm = TRUE),
            .by = c("day", "treatment", "temp"))

d_fisico <- c("anova 10 º C", "anova  15 º C", "anova 22 º C") %>%
  map(.,
      ~ read_excel("./data/Tarea 8/SS_pH_AT T.cherry.xlsx",
                   sheet = .)
  ) %>%
  map2_dfr(., c(10, 15, 22),
           ~ mutate(.x, temp = .y)
  ) %>%
  pivot_longer(-c(treatment, day, temp))  %>%
  summarize(value = mean(value, na.rm = TRUE),
            .by = c("day", "treatment", "temp", "name")) %>%
  pivot_wider() %>%
  mutate(treatment = ifelse(treatment == "T", "Control", "Activa"))

d_color <- c(10, 15, 22) %>%
  set_names(., .) %>%
  map(., ~ paste0(., "C_forR")) %>%
  map(.,
      ~ read_excel("./data/Tarea 8/Color Tomate .xlsx", sheet = .)
  ) %>%
  imap_dfr(., ~ mutate(.x, temp = .y)) %>%
  pivot_longer(-c(treatment, day, temp))  %>%
  summarize(value = mean(value, na.rm = TRUE),
            .by = c("day", "treatment", "temp", "name")) %>%
  pivot_wider() %>%
  mutate(treatment = ifelse(treatment == "control", "Control", "Activa"),
         temp = as.numeric(temp))

d_carot <- c(10, 15, 22) %>%
  set_names(., .) %>%
  map(., ~ paste0(., "C_forR")) %>%
  map(.,
      ~ read_excel("./data/Tarea 11/Carotenos_T.cherry.xlsx", sheet = .)
  ) %>%
  imap_dfr(., ~ mutate(.x, temp = .y)) %>%
  filter(!is.na(carotenos)) %>%
  mutate(temp = as.numeric(temp)) %>%
  mutate(treatment = ifelse(treatment == "control", "Control", "Activa"))

## Arrange and combine

d <- full_join(d_firmeza, d_fisico) %>%
  full_join(d_color) %>%
  full_join(d_carot) %>%
  filter(!is.na(treatment))

## Correlations

d %>%
  select(-treatment) %>%
  ggpairs()

d %>%
  select(-treatment) %>%
  rename(`Storage time` = day,
         Temperature = temp,
         Firmness = firmeza,
         `Colour index` = `Color index`,
         `ºHue` = Hue,
         Chroma = Chroma,
         Carotenoids = carotenos,
         SSC = SST,
         TA = AT,
         pH = pH
         ) %>%
  cor(method = "spearman") %>%
  # corrplot.mixed()
  corrplot(method = "number", order = "AOE")

## Figure 3

png(filename="Figure_3.png", width = 1200, height = 600)

par(mfrow = c(1, 2))

d %>%
  select(`Storage time` = day,
         Temperature = temp,
         Firmness = firmeza,
         `Colour index` = `Color index`,
         `ºHue` = Hue,
         Chroma = Chroma,
         Carotenoids = carotenos,
         SSC = SST,
         TA = AT,
         pH = pH,
         treatment
  ) %>%
  split(.$treatment) %>%
  map(.,
      ~ select(., -treatment) 
      ) %>%
  map(., ~ cor(., method = "spearman")) %>%
  map(., ~ corrplot(., method = "number", order = "AOE"))

par(mfrow = c(1,1))
dev.off()

## 

d %>%
  select(-treatment) %>%
  select(-c("a/b", "(a/b)^2", "Hue", "Chroma")) %>%
  cor(method = "spearman") %>%
  # corrplot.mixed()
  corrplot(method = "number", order = "AOE") 

par(mfrow = c(1, 2))
d %>%
  select(-c("a/b", "(a/b)^2", "Hue", "Chroma")) %>%
  split(.$treatment) %>%
  map(.,
      ~ select(., -treatment) 
  ) %>%
  map(., ~ cor(., method = "spearman")) %>%
  map(., ~ corrplot(., method = "number"))

par(mfrow = c(1,1))

##

d %>%
  mutate(cond = paste(treatment, temp, sep = "-")) %>%
  split(.$cond) %>%
  map(.,
      ~ lm(firmeza ~ `Color index`, data = .)
  ) %>% 
  map(., ~ summary(.)$coef) %>%
  map(., ~ as_tibble(., rownames = "par")) %>%
  imap_dfr(., ~ mutate(.x, cond = .y)) %>%
  separate(cond, into = c("envase", "temp")) %>%
  ggplot(aes(x = temp, y = Estimate, colour = envase)) +
  geom_point() +
  geom_errorbar(aes(ymin = Estimate - `Std. Error`,
                    ymax = Estimate + `Std. Error`)) +
  facet_wrap("par", scales = "free")

d %>%
  split(.$temp) %>%
  map(.,
      ~ lm(firmeza ~ `Color index`, data = .)
  ) %>% 
  map(., ~ summary(.)$coef) %>%
  map(., ~ as_tibble(., rownames = "par")) %>%
  imap_dfr(., ~ mutate(.x, temp = .y)) %>%
  mutate(temp = as.numeric(temp)) %>%
  ggplot(aes(x = temp, y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = Estimate - `Std. Error`,
                    ymax = Estimate + `Std. Error`)) +
  facet_wrap("par", scales = "free")

lm(firmeza ~ `Color index`, data = d) %>% summary()

d %>%
  ggplot(aes(x = `Color index`, y = firmeza)) +
  geom_point() +
  geom_smooth(method = "lm")

##

d %>%
  ggplot(aes(x = `Color index`, y = day)) +
  geom_point(aes(colour = treatment)) +
  geom_smooth(method = "lm")

d %>%
  ggplot(aes(x = `Color index`, y = day, colour = factor(temp))) +
  geom_point() +
  geom_smooth(method = "lm")

d %>%
  ggplot(aes(x = `Color index`, y = day)) +
  geom_point(aes(colour = treatment)) +
  geom_smooth(method = "lm") +
  facet_wrap("temp")

d %>%
  mutate(temp = factor(temp)) %>%
  lm(day ~ `Color index`:temp, data = .) %>%
  summary()









