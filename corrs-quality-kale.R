
library(tidyverse)
library(readxl)
library(corrplot)
library(GGally)
library(stringr)

## Load data

d_fisico <- c(2, 8, 15, 22) %>%
  set_names(., .) %>%
  map(., ~ paste0(., "C_forR")) %>%
  map(.,
      ~ read_excel("./data/Tarea 8/SS_pH_AT_Kale.xlsx",
                   sheet = .)
  ) %>%
  imap_dfr(., ~ mutate(.x, temp = .y)) %>%
  pivot_longer(-c(treatment, day, temp)) %>%
  summarize(value = mean(value, na.rm = TRUE),
            .by = c("day", "treatment", "temp", "name")) %>%
  pivot_wider() %>%
  mutate(temp = as.numeric(temp),
         treatment = str_to_lower(treatment))

d_color <- c(2, 8, 15, 22) %>%
  set_names(., .) %>%
  map(., ~ paste0(., "C_forR")) %>%
  map(.,
      ~ read_excel("./data/Tarea 8/Color_Kale.xlsx", sheet = .)
  ) %>%
  imap_dfr(., ~ mutate(.x, temp = .y)) %>%
  pivot_longer(-c(treatment, day, temp))  %>%
  summarize(value = mean(value, na.rm = TRUE),
            .by = c("day", "treatment", "temp", "name")) %>%
  pivot_wider() %>%
  mutate(# treatment = ifelse(treatment == "control", "Control", "Activo"),
         temp = as.numeric(temp))

d_cloro <- c(2, 8, 15, 22) %>%
  set_names(., .) %>%
  map(., ~ paste0("forR_", ., "C")) %>%
  map(.,
      ~ read_excel("./data/Tarea 11/Clorofilas_Kale.xlsx", sheet = .)
  ) %>%
  imap_dfr(., ~ mutate(.x, temp = .y)) %>%
  mutate(temp = as.numeric(temp)) %>%
  select(-carotenos)

## Arrange and combine

d <- full_join(d_color, d_fisico) %>% 
  full_join(d_cloro) %>%
  filter(!is.na(treatment)) %>%
  na.omit()

## Correlations

d %>%
  select(-treatment) %>%
  ggpairs()

d %>%
  select(-treatment) %>%
  cor(method = "spearman") %>%
  # corrplot.mixed()
  corrplot(method = "number", order = "AOE")

## Figure 4

png(filename="Figure_4.png", width = 1200, height = 600)


par(mfrow = c(1, 2))
d %>%
  select(`Storage time` = day,
         Temperature = temp,
         `Yellowness Index` = `Yellowness Index`,
         `ºHue` = Hue,
         Chroma = Chroma,
         Chorophylls = clorofilas,
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


  
  
  
  
  
  
  
  
  
  