library(patchwork)

answer_colors_support <- c(
  colors['dark_blue'],
  colors['blue'],
  colors['purple'],
  colors['red'],
  colors['dark_red'],
  colors['light_gray']
)
names(answer_colors_support) <- survey_opts

answer_colors_oppose <- c(
  colors['dark_red'],
  colors['red'],
  colors['purple'],
  colors['blue'],
  colors['dark_blue'],
  colors['light_gray']
)
names(answer_colors_oppose) <- survey_opts

title_fourday <- "4-day work week"
title_redflag <- "Red flag laws"
title_bds <- "Laws forbidding boycotts of Israel"

titles <- c(
  "REDFLAG" = title_redflag,
  "FOURDAY" = title_fourday,
  "BDS" = title_bds
)

races <- c(
  "White",
  "Black or African-American",
  "Hispanic or Latino",
  "Asian or Asian-American",
  "Native American",
  "Mixed Race",
  "Other",
  "Middle Eastern"
)

## Race
df <- dat %>%
  mutate(
    race = fct_infreq(plyr::mapvalues(race, 1:8, races)),
    age = 2019 - birthyr,
    generation = cut(age, breaks = c(0, 37, 53, 72, Inf), labels = c("Millennial", "Gen X", "Boomers", "Silent"))
  ) %>%
  mutate(race = fct_other(race, keep = c("White", "Black or African-American", "Hispanic or Latino"))) %>%
  select(race, generation, FOURDAY, REDFLAG, BDS, weight) %>%
  gather(question, answer, FOURDAY, REDFLAG, BDS) %>%
  drop_na(answer) %>%
  filter(answer != 6) %>%
  group_by(race, question, answer) %>%
  summarize(n = sum(weight)) %>%
  mutate(p = n / sum(n)) %>%
  mutate(answer = fct_rev(factor(plyr::mapvalues(answer, 1:6, survey_opts), levels = survey_opts))) %>%
  ungroup() %>%
  mutate(
    question = fct_rev(factor(
      plyr::mapvalues(question, names(titles), titles),
      levels = titles
    ))
  )

# BDS
dft <- df %>%
  filter(question == titles[['BDS']]) %>%
  mutate(answer = fct_rev(answer))

df_hi <- dft %>%
  filter(answer %in% c("Strongly oppose", "Somewhat oppose", "Neither support nor oppose")) %>%
  mutate(p = case_when(
    answer == "Neither support nor oppose" ~ p / 2,
    TRUE ~ p
  )) %>%
  mutate(answer = fct_rev(answer))

df_lo <- dft %>%
  filter(answer %in% c("Strongly support", "Somewhat support", "Neither support nor oppose")) %>%
  mutate(p = case_when(
    answer == "Neither support nor oppose" ~ p / 2,
    TRUE ~ p
  )) %>%
  mutate(p = -p)

nudge_y <- 0.35

g_likert_bds <- ggplot() +
  geom_col(data = df_hi, aes(question, p, fill = answer)) +
  geom_text(
    data = df_hi %>%
      filter(answer != "Neither support nor oppose") %>%
      group_by(question, race) %>%
      summarize(p = sum(p)),
    aes(question, p, label = scales::percent(p, accuracy = 1)),
    # color = "white"
    nudge_y = nudge_y
  ) +
  geom_col(data = df_lo, aes(question, p, fill = answer)) +
  geom_text(
    data = df_lo %>%
      filter(answer != "Neither support nor oppose") %>%
      group_by(question, race) %>%
      summarize(p = sum(p)),
    aes(question, p, label = scales::percent(-p, accuracy = 1)),
    # color = "white"
    nudge_y = -nudge_y
  ) +
  geom_hline(yintercept = 0, color = "white") +
  coord_flip() +
  facet_wrap(~race, ncol = 1) +
  scale_x_discrete(labels = function(x) str_wrap(x, 50)) +
  scale_y_continuous(
    labels = function(x) scales::percent(x, accuracy = 1),
    limits = c(-1, 1)
  ) +
  scale_fill_manual(values = answer_colors_oppose) +
  labs(title = str_wrap(titles[['BDS']], 20), x = "", y = "", fill = "") +
  theme_dfp() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  guides(fill = FALSE)
g_likert_bds

# Red flag
dft <- df %>%
  filter(question == titles[['REDFLAG']])

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

nudge_y <- 0.2

g_likert_redflag <- ggplot() +
  geom_col(data = df_hi, aes(question, p, fill = answer)) +
  geom_text(
    data = df_hi %>%
      filter(answer != "Neither support nor oppose") %>%
      group_by(question, race) %>%
      summarize(p = sum(p)),
    aes(question, p, label = scales::percent(p, accuracy = 1)),
    # color = "white"
    nudge_y = nudge_y
  ) +
  geom_col(data = df_lo, aes(question, p, fill = answer)) +
  geom_text(
    data = df_lo %>%
      filter(answer != "Neither support nor oppose") %>%
      group_by(question, race) %>%
      summarize(p = sum(p)),
    aes(question, p, label = scales::percent(-p, accuracy = 1)),
    # color = "white"
    nudge_y = -nudge_y
  ) +
  geom_hline(yintercept = 0, color = "white") +
  facet_wrap(~race, ncol = 1) +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, 50)) +
  scale_y_continuous(
    labels = function(x) scales::percent(x, accuracy = 1),
    limits = c(-1, 1)
  ) +
  scale_fill_manual(values = answer_colors_support) +
  labs(title = titles[['REDFLAG']], x = "", y = "", fill = "") +
  theme_dfp() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  guides(fill = FALSE)
g_likert_redflag


# 4-day
dft <- df %>%
  filter(question == titles[['FOURDAY']])

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

nudge_y <- 0.3

g_likert_fourday <- ggplot() +
  geom_col(data = df_hi, aes(question, p, fill = answer)) +
  geom_text(
    data = df_hi %>%
      filter(answer != "Neither support nor oppose") %>%
      group_by(question, race) %>%
      summarize(p = sum(p)),
    aes(question, p, label = scales::percent(p, accuracy = 1)),
    # color = "white"
    nudge_y = nudge_y
  ) +
  geom_col(data = df_lo, aes(question, p, fill = answer)) +
  geom_text(
    data = df_lo %>%
      filter(answer != "Neither support nor oppose") %>%
      group_by(question, race) %>%
      summarize(p = sum(p)),
    aes(question, p, label = scales::percent(-p, accuracy = 1)),
    # color = "white"
    nudge_y = -nudge_y
  ) +
  geom_hline(yintercept = 0, color = "white") +
  facet_wrap(~race, ncol = 1) +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, 50)) +
  scale_y_continuous(
    labels = function(x) scales::percent(x, accuracy = 1),
    limits = c(-1, 1)
  ) +
  scale_fill_manual(values = answer_colors_support) +
  labs(title = str_wrap(titles[['FOURDAY']], 20), x = "", y = "", fill = "") +
  theme_dfp() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  guides(fill = FALSE)
g_likert_fourday

g_likert_redflag + g_likert_fourday + g_likert_bds

