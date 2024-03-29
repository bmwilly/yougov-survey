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

df <- dat %>%
  mutate(
    race = fct_infreq(plyr::mapvalues(race, 1:8, races)),
    age = 2019 - birthyr,
    generation = cut(age, breaks = c(0, 37, 53, 72, Inf), labels = c("Millennial", "Gen X", "Boomers", "Silent"))
  )

## Race
dft <- df %>%
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

g_likert_race <- ggplot() + 
  geom_col(data = df_hi, aes(question, p, fill = answer)) + 
  geom_col(data = df_lo, aes(question, p, fill = answer)) + 
  geom_hline(yintercept = 0, color = "white") +
  facet_wrap(~race) + 
  coord_flip() + 
  scale_x_discrete(labels = function(x) str_wrap(x, 50)) + 
  scale_y_continuous(
    labels = function(x) scales::percent(x, accuracy = 1),
    limits = c(-1, 1)
  ) +
  scale_fill_manual(values = answer_colors) +
  labs(x = "", y = "", fill = "") + 
  theme_dfp()


## Generation
dft <- df %>%
  select(race, generation, FOURDAY, REDFLAG, BDS, weight) %>%
  gather(question, answer, FOURDAY, REDFLAG, BDS) %>%
  drop_na(answer) %>%
  filter(answer != 6) %>%
  group_by(generation, question, answer) %>%
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

g_likert_age <- ggplot() + 
  geom_col(data = df_hi, aes(question, p, fill = answer)) + 
  geom_col(data = df_lo, aes(question, p, fill = answer)) + 
  geom_hline(yintercept = 0, color = "white") +
  facet_wrap(~generation) + 
  coord_flip() + 
  scale_x_discrete(labels = function(x) str_wrap(x, 50)) + 
  scale_y_continuous(
    labels = function(x) scales::percent(x, accuracy = 1),
    limits = c(-1, 1)
  ) +
  scale_fill_manual(values = answer_colors) +
  labs(x = "", y = "", fill = "") + 
  theme_dfp()

g_likert_race
g_likert_age

