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
  mutate(question_text = factor(plyr::mapvalues(question, names(titles), str_wrap(titles, 60))))

# BDS
dft <- df %>%
  filter(question == 'BDS') %>%
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

g_likert_bds <- ggplot() + 
  geom_col(data = df_hi, aes(question_text, p, fill = answer)) + 
  geom_col(data = df_lo, aes(question_text, p, fill = answer)) + 
  geom_hline(yintercept = 0, color = "white") +
  coord_flip() + 
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
  scale_fill_manual(values = answer_colors_oppose) +
  labs(x = "", y = "", fill = "") + 
  theme_dfp()
g_likert_bds

# Other
dft <- df %>%
  filter(question != 'BDS')

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

g_likert_other <- ggplot() + 
  geom_col(data = df_hi, aes(question_text, p, fill = answer)) + 
  geom_col(data = df_lo, aes(question_text, p, fill = answer)) + 
  geom_hline(yintercept = 0, color = "white") +
  coord_flip() + 
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
  scale_fill_manual(values = answer_colors_support) +
  labs(x = "", y = "", fill = "") + 
  theme_dfp()
g_likert_other

