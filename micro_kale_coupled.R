
library(tidyverse)
library(readxl)
library(biogrowth)
library(cowplot)

## Import data

d_kale <- c(2, 8, 15, 22) %>%
  paste0(., "C") %>%
  set_names(., .) %>%
  map(.,
      ~ read_excel("./data/Tarea 9/Microbiología_Kale.xlsx",
                   sheet = .)
  ) %>%
  map(.,
      ~ pivot_longer(., -c("day", "condition"),
                     names_to = "bug", values_to = "logN")
  ) %>%
  imap_dfr(., ~ mutate(.x, temp = .y)) %>%
  filter(!is.na(logN)) %>%
  mutate(temp = gsub("C", "", temp),
         temp = as.numeric(temp)) %>%
  rename(time = day)

## Deal with day 0

d_kale <- rep(0, 6) %>%
  map(.,
      ~ filter(d_kale, time == .)
  ) %>%
  map2_dfr(., c(paste("Control", 1:3),
                paste("Activo", 1:3)
  ),
  ~ mutate(.x, condition = .y)
  ) %>%
  bind_rows(.,
            filter(d_kale, time != 0)
  )

d_kale %>%
  separate(condition, into = c("condition")) %>%
  ggplot(aes(x = time, y = logN, colour = condition)) +
  geom_point() +
  geom_line() +
  facet_wrap(bug ~ temp, scales = "free")

## Fit the models

aa <- d_kale %>%
  mutate(temp = ifelse(temp == 2, 3, temp),
         temp = ifelse(temp == 8, 2, temp),
         temp = ifelse(temp == 3, 8, temp)) %>%
  separate(condition, into = c("condition")) %>%
  mutate(c = paste(condition, bug, sep = "_")) %>%
  split(.$c)

models <- list(
  fit_coupled_growth(
    mode = "one_step",
    aa[[1]],
    # make_guess_coupled(aa[[1]], mode = "one_step")
    c(Tmin = 2, b = .04, logN0 = 2),
    known = c(logC0 = 8, logNmax = 7.5)
  ),
  fit_coupled_growth(
    mode = "one_step",
    aa[[2]],
    # make_guess_coupled(aa[[2]], mode = "one_step")
    c(Tmin = -10, b = .04, logN0 = 2, logNmax = 6),
    known = c(logC0 = 8)
  ),
  fit_coupled_growth(
    mode = "one_step",
    aa[[3]],
    # make_guess_coupled(aa[[3]], mode = "one_step")
    c(Tmin = -20, b = .02, logN0 = 4, logNmax = 6),
    known = c(logC0 = 8)
  ),
  fit_coupled_growth(
    mode = "one_step",
    aa[[4]],
    make_guess_coupled(aa[[4]], mode = "one_step")
    # c(Tmin = -10, b = .04, logN0 = 2, logNmax = 6),
    # known = c(logC0 = 8)
  ),
  fit_coupled_growth(
    mode = "one_step",
    aa[[5]],
    # make_guess_coupled(aa[[5]], mode = "one_step")
    c(Tmin = -15, b = .04, logN0 = 3, logNmax = 6),
    known = c(logC0 = 9)
  ),
  fit_coupled_growth(
    mode = "one_step",
    aa[[6]],
    # make_guess_coupled(aa[[6]], mode = "one_step")
    c(Tmin = -15, b = .04, logN0 = 3, logNmax = 6),
    known = c(logC0 = 9)
  ),
  fit_coupled_growth(
    mode = "one_step",
    aa[[7]],
    # make_guess_coupled(aa[[7]], mode = "one_step")
    c(Tmin = -21, b = .03, logN0 = 3, logNmax = 6),
    known = c(logC0 = 9)
  ),
  fit_coupled_growth(
    mode = "one_step",
    aa[[8]],
    # make_guess_coupled(aa[[8]], mode = "one_step")
    c(Tmin = -21, b = .03, logN0 = 3, logNmax = 6),
    known = c(logC0 = 9)
  ),
  fit_coupled_growth(
    mode = "one_step",
    aa[[9]],
    # make_guess_coupled(aa[[9]], mode = "one_step")
    c(Tmin = -21, b = .03, logN0 = 3, logNmax = 6),
    known = c(logC0 = 9)
  ),
  fit_coupled_growth(
    mode = "one_step",
    aa[[10]],
    # make_guess_coupled(aa[[10]], mode = "one_step")
    c(Tmin = -21, b = .03, logN0 = 3, logNmax = 6),
    known = c(logC0 = 9)
  )
)

names(models) <- names(aa)

##

models %>%
  map(., ~ summary(.)$par) %>%
  map(., ~ as_tibble(., rownames = "par")) %>%
  imap_dfr(., ~ mutate(.x, cond = .y)) %>%
  separate(cond, into = c("Pack", "Bug"), sep = "_") %>%
  ggplot() +
  geom_col(aes(x = Bug, fill = Pack, y = Estimate), position = "dodge") +
  facet_wrap("par", scales = "free")


## Table 1.B

models %>%
  map(., ~ summary(.)$par) %>%
  map(., ~ as_tibble(., rownames = "par")) %>%
  imap_dfr(., ~ mutate(.x, cond = .y)) %>%
  separate(cond, into = c("Pack", "Bug"), sep = "_") %>%
  mutate(est = paste0(round(Estimate, 2), "_", round(`Std. Error`, 2))) %>%
  select(par, est, Bug, Pack) %>%
  pivot_wider(names_from = "par", values_from = "est") %>%
  arrange(Bug, Pack) %>%
  write_excel_csv2(., file = "pars_micro_kale.csv")

## Figure 2

p <- models %>%
  map(.,
      ~ predict(.,
                newdata = tibble(
                  temp = c(rep(10, 100), rep(15, 100), rep(22, 100)),
                  time = c(seq(0, 15, length = 100), seq(0, 15, length = 100), seq(0, 8, length = 100))
                )
      )
  ) %>%
  imap_dfr(~ mutate(.x, cond = .y)) %>%
  separate(cond, into = c("Packaging", "bug"), sep = "_") %>%
  left_join(.,
            tribble(~bug, ~bug_name,
                    "AMT", "Mesophiles",
                    "enterobacteria", "Enterobacteria",
                    "levaduras", "Yeast",
                    "mohos", "Molds",
                    "psicrofilos", "Psychrophiles")
  ) %>%
  mutate(Packaging = ifelse(Packaging == "Activo", "Active", "Conventional")) %>%
  ggplot() +
  geom_line(aes(time, logN, linetype = factor(temp), colour = Packaging),
            linewidth = 1) +
  facet_wrap("bug_name", scales = "free") +
  labs(x = "Storage time (h)",
       linetype = "Temperature (ºC)",
       y = "Microbial load (log CFU/g)") +
  # ggthemes::theme_few(base_size = 14) +
  theme_bw(base_size = 14) +
  scale_linetype_manual(values = c(3,1,2)) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.8, .2)) +
  scale_color_manual(values = wes_palette("Darjeeling1", 2))

p

ggsave(p, filename = "Figure_2.png", width = 12, height = 8)

## Supp. Figure 2

p <- models %>%
  imap(~ plot(.x) + ggtitle(.y)) %>%
  ggarrange(plotlist = .)

ggsave(p, filename = "supp_Figure_2.png", width = 16, height = 8, bg = "white")






































