library(tidyverse)
library(patchwork)

source("src/dfp_ggplot_theme.R")

dat <- read_csv("data/dfp_wave3_201901_final.csv")

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
