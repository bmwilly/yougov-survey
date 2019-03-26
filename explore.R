library(tidyverse)

source("dfp_ggplot_theme.R")

dat <- read_csv("dfp_wave3_201901_final.csv")

survey_opts <- c(
  "Strongly support",
  "Somewhat support",
  "Neither support nor oppose",
  "Somewhat oppose",
  "Strongly oppose",
  "Not sure"
)

colors <- c(
  "blue" = "#124073",
  "dark_blue" = "#0A2645",
  "green" = "#A8BF14",
  "purple" = "#6608D1",
  "red" = "#9C0C15",
  "dark_red" = "#6C1015",
  "light_gray" = "#B3B3B3"
)

survey_colors <- colors
names(survey_colors) <- survey_opts

answer_colors <- c(
  colors['dark_blue'],
  colors['blue'],
  colors['purple'],
  colors['red'],
  colors['dark_red'],
  colors['light_gray']
)
names(answer_colors) <- survey_opts

answer_colors_support <- c(
  'dark_blue',
  'blue',
  'purple',
  'red',
  'dark_red',
  'light_gray'
)
names(answer_colors_support) <- survey_opts

answer_colors_oppose <- c(
  'dark_red',
  'red',
  'purple',
  'blue',
  'dark_blue',
  'light_gray'
)
names(answer_colors_oppose) <- survey_opts

title_fourday <- paste(
  "Would you support or oppose a policy limiting the work week to four days?",
  "Employers in the US are not currently required to give employees any days off."
)

title_redflag <- paste(
  "Several states have recently enacted Extreme Risk Protection Order laws,",
  "also known as “red flag” laws, which allow courts to temporarily remove firearms",
  "from the homes of individuals who are deemed to pose a violent risk to themselves or others.",
  "Do you support or oppose these “red flag” laws?"  
)

title_bds <- paste(
  "Do you support or oppose laws forbidding Federal, state, or local employees",
  "and contractors from promoting boycotts of Israel?"
)

titles <- c(
  "FOURDAY" = title_fourday,
  "REDFLAG" = title_redflag,
  "BDS" = title_bds
)

df <- dat %>%
  select(FOURDAY, REDFLAG, BDS, weight) %>%
  gather(question, answer, FOURDAY, REDFLAG, BDS) %>%
  drop_na(answer) %>%
  group_by(question, answer) %>%
  summarize(n = sum(weight)) %>%
  mutate(p = n / sum(n)) %>%
  mutate(answer = fct_rev(factor(plyr::mapvalues(answer, 1:6, survey_opts), levels = survey_opts))) %>%
  mutate(color = case_when(
    question == "BDS" ~ plyr::mapvalues(answer, survey_opts, answer_colors_oppose),
    TRUE ~ plyr::mapvalues(answer, survey_opts, answer_colors_support)
  )) %>%
  ungroup() %>%
  mutate(question = fct_rev(factor(plyr::mapvalues(question, names(titles), str_wrap(titles, 80)))))

g_bar <- df %>%
  ggplot(aes(x = answer, y = p)) + 
  geom_col(aes(fill = color)) +
  geom_text(aes(label = scales::percent(p)), nudge_y = 0.015, family = 'Montserrat-Regular', size = 2.8) +
  coord_flip() + 
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
  scale_fill_manual(values = colors) + 
  facet_wrap(~question, ncol = 1) + 
  labs(x = "", y = "") + 
  theme_dfp() + 
  guides(fill = "none")
g_bar


dft <- dat %>%
  select(FOURDAY, REDFLAG, BDS, weight) %>%
  gather(question, answer, FOURDAY, REDFLAG, BDS) %>%
  drop_na(answer) %>%
  filter(answer != 6) %>%
  group_by(question, answer) %>%
  summarize(n = sum(weight)) %>%
  mutate(p = n / sum(n)) %>%
  mutate(answer = fct_rev(factor(plyr::mapvalues(answer, 1:6, survey_opts), levels = survey_opts))) %>%
  ungroup() %>%
  mutate(question = factor(plyr::mapvalues(question, names(titles), str_wrap(titles, 50))))

df_hi <- dft %>%
  filter(answer %in% c("Strongly support", "Somewhat support", "Neither support nor oppose")) %>%
  mutate(p = case_when(
    answer == "Neither support nor oppose" ~ p / 2,
    TRUE ~ p
  )) %>%
  mutate(answer = fct_rev(answer))

df_lo <- dft %>%
  filter(answer %in% c("Strongly oppose", "Somewhat oppose", "Neither support nor oppose")) %>%
  mutate(p = case_when(
    answer == "Neither support nor oppose" ~ p / 2,
    TRUE ~ p
  )) %>%
  mutate(p = -p)

g_likert <- ggplot() + 
  geom_col(data = df_hi, aes(question, p, fill = answer)) + 
  geom_col(data = df_lo, aes(question, p, fill = answer)) + 
  geom_hline(yintercept = 0, color = "white") +
  coord_flip() + 
  scale_y_continuous(
    labels = function(x) scales::percent(x, accuracy = 1), 
    breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75)
    # limits = c(-0.8, 0.8)
  ) +
  scale_fill_manual(values = answer_colors) +
  labs(x = "", y = "", fill = "") + 
  theme_dfp()
g_likert


