source("src/setup.R")

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

nudge_y <- 0.2

g_likert <- ggplot() +
  geom_col(data = df_hi, aes(question, p, fill = answer)) +
  geom_text(
    data = df_hi %>%
      filter(answer != "Neither support nor oppose") %>%
      group_by(question) %>%
      summarize(p = sum(p)),
    aes(question, p, label = scales::percent(p, accuracy = 1)),
    # color = "white"
    nudge_y = nudge_y
  ) +
  geom_col(data = df_lo, aes(question, p, fill = answer)) +
  geom_text(
    data = df_lo %>%
      filter(answer != "Neither support nor oppose") %>%
      group_by(question) %>%
      summarize(p = sum(p)),
    aes(question, p, label = scales::percent(-p, accuracy = 1)),
    # color = "white"
    nudge_y = -nudge_y
  ) +
  geom_hline(yintercept = 0, color = "white") +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, 50)) +
  scale_y_continuous(
    labels = function(x) scales::percent(x, accuracy = 1),
    limits = c(-1, 1)
  ) +
  scale_fill_manual(values = answer_colors) +
  labs(
    title = ,
    x = "",
    y = "",
    fill = ""
  ) +
  theme_dfp()


