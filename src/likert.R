source("src/setup.R")

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

df <- dat %>%
  select(FOURDAY, REDFLAG, BDS, weight) %>%
  gather(question, answer, FOURDAY, REDFLAG, BDS) %>%
  drop_na(answer) %>%
  filter(answer != 6) %>%
  group_by(question, answer) %>%
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

nudge_y <- 0.22

g_likert_bds <- ggplot() +
  geom_col(data = df_hi, aes(question, p, fill = answer)) +
  geom_text(
    data = df_hi %>%
      filter(answer != "Neither support nor oppose") %>%
      group_by(question) %>%
      summarize(p = sum(p)),
    aes(question, p, label = scales::percent(p, accuracy = 1)),
    nudge_y = nudge_y
  ) +
  geom_col(data = df_lo, aes(question, p, fill = answer)) +
  geom_text(
    data = df_lo %>%
      filter(answer != "Neither support nor oppose") %>%
      group_by(question) %>%
      summarize(p = sum(p)),
    aes(question, p, label = scales::percent(-p, accuracy = 1)),
    nudge_y = -nudge_y
  ) +
  geom_hline(yintercept = 0, color = "white") +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, 50)) +
  scale_y_continuous(
    labels = function(x) scales::percent(x, accuracy = 1),
    limits = c(-1, 1)
  ) +
  scale_fill_manual(values = answer_colors_oppose) +
  labs(x = "", y = "", fill = "") +
  theme_dfp() +
  # theme(axis.text.x = element_blank(), axis.ticks = element_blank()) +
  # guides(fill = FALSE)
  NULL
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

nudge_y <- 0.12

g_likert_redflag <- ggplot() +
  geom_col(data = df_hi, aes(question, p, fill = answer)) +
  geom_text(
    data = df_hi %>%
      filter(answer != "Neither support nor oppose") %>%
      group_by(question) %>%
      summarize(p = sum(p)),
    aes(question, p, label = scales::percent(p, accuracy = 1)),
    nudge_y = nudge_y
  ) +
  geom_col(data = df_lo, aes(question, p, fill = answer)) +
  geom_text(
    data = df_lo %>%
      filter(answer != "Neither support nor oppose") %>%
      group_by(question) %>%
      summarize(p = sum(p)),
    aes(question, p, label = scales::percent(-p, accuracy = 1)),
    nudge_y = -nudge_y
  ) +
  geom_hline(yintercept = 0, color = "white") +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, 50)) +
  scale_y_continuous(
    labels = function(x) scales::percent(x, accuracy = 1),
    limits = c(-1, 1)
  ) +
  scale_fill_manual(values = answer_colors_support) +
  labs(x = "", y = "", fill = "") +
  theme_dfp() +
  # theme(axis.text.x = element_blank(), axis.ticks = element_blank()) +
  # guides(fill = FALSE)
  NULL
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

nudge_y <- 0.22

g_likert_fourday <- ggplot() +
  geom_col(data = df_hi, aes(question, p, fill = answer)) +
  geom_text(
    data = df_hi %>%
      filter(answer != "Neither support nor oppose") %>%
      group_by(question) %>%
      summarize(p = sum(p)),
    aes(question, p, label = scales::percent(p, accuracy = 1)),
    nudge_y = nudge_y
  ) +
  geom_col(data = df_lo, aes(question, p, fill = answer)) +
  geom_text(
    data = df_lo %>%
      filter(answer != "Neither support nor oppose") %>%
      group_by(question) %>%
      summarize(p = sum(p)),
    aes(question, p, label = scales::percent(-p, accuracy = 1)),
    nudge_y = -nudge_y
  ) +
  geom_hline(yintercept = 0, color = "white") +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, 50)) +
  scale_y_continuous(
    labels = function(x) scales::percent(x, accuracy = 1),
    limits = c(-1, 1)
  ) +
  scale_fill_manual(values = answer_colors_support) +
  labs(x = "", y = "", fill = "") +
  theme_dfp() +
  # theme(axis.text.x = element_blank(), axis.ticks = element_blank()) +
  # guides(fill = FALSE)
  NULL
g_likert_fourday

# g_likert_redflag + g_likert_fourday + g_likert_bds +
#   plot_layout(ncol = 1)

g_likert_redflag + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
  guides(fill = FALSE) + 
  labs(title = str_wrap(title_redflag, 60))

g_likert_fourday + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
  guides(fill = FALSE) + 
  labs(title = str_wrap(title_fourday, 60))

g_likert_bds + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
  guides(fill = FALSE) + 
  labs(title = str_wrap(title_bds, 60))

