plot_count <- function(data, question, title) {
  question <- enquo(question)
  
  dat %>%
    mutate(answer = fct_rev(factor(plyr::mapvalues(!!question, 1:6, survey_opts), levels = survey_opts))) %>%
    drop_na(answer) %>%
    group_by(answer) %>%
    summarize(n = n()) %>%
    mutate(p = n / sum(n)) %>%
    ggplot(aes(answer, p)) +
    geom_col() + 
    geom_text(aes(label = scales::percent(p)), nudge_y = 0.01, family = 'Montserrat-Regular', size = 2.8) +
    coord_flip() + 
    scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
    labs(title = str_wrap(title, 50), x = "", y = "") + 
    theme_dfp() + 
    theme(plot.title = element_text(hjust = 0, size = 10, margin = margin(b = 10), face = "bold", family = 'FuturaBT-Heavy'))
}

g1 <- plot_count(dat, FOURDAY, title)
g1

g2 <- plot_count(dat, REDFLAG, title)

g3 <- plot_count(dat, BDS, title)

cowplot::plot_grid(g1, g2, g3, ncol = 1)
