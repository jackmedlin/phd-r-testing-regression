# phd-r-testing

This is me both rubberducking, and trying to explain my progress with messing around with R in terms of analysis.

So far, I've learnt how to set up my IDE. I'm using VSCode, just for the simplicity of having to workaround using multiple languages (Python, R, SQL) in order to acomplish everything within the project. I've set up my IDE to work pretty similarly to RStudio. 

Anyway, here is an explanation of some of my code so far - beginning with Regression Analysis using TEF awards as predictors and B3 metrics as the relevant dependant variable.

---

This first block shows the packages I am using, as well as setting the working directory and reading data from the dataset CSV as well as cleaning the variable names. If you have any suggestions on packages to use, let me know!

```
# Import packages
library(tidyverse)
library(janitor)
library(data.table)
library(broom)
library(gt)
library(ggplot2)

# Set working directory
setwd("C:\\RPhD")

# Read data from CSV
df <- read_csv("JM_PhD_DATA.csv", show_col_types = FALSE)

# Clean column names
df <- clean_names(df)

```
This following block is my regression analysis. I am mutating the predictors into factors and then completing the models for Completion, Continuation, and Outcomes. I then create tidy summaries for these models to allow for visualisation.
```
# Mutates predictor variables to factors
df <- df %>%
  mutate(
    tef23_award_overall = as.factor(tef23_award_overall),
    tef23_award_experience = as.factor(tef23_award_experience),
    tef23_award_outcome = as.factor(tef23_award_outcome),
  )

# Regression models for completion, continuation, and outcomes
model_completion <- lm(completion_value ~ tef23_award_overall + tef23_award_experience + tef23_award_outcome, data = df) # nolint
print(summary(model_completion))

model_continuation <- lm(continuation_value ~ tef23_award_overall + tef23_award_experience + tef23_award_outcome, data = df) # nolint
print(summary(model_continuation))

model_outcomes <- lm(outcomes_value ~ tef23_award_overall + tef23_award_experience + tef23_award_outcome, data = df) # nolint
print(summary(model_outcomes))

# Create tidy summaries of the models
model_summaries <- bind_rows(
  tidy(model_completion) %>% mutate(model = "Completion"),
  tidy(model_continuation) %>% mutate(model = "Continuation"),
  tidy(model_outcomes) %>% mutate(model = "Outcomes")
)
```

This final block is creating APA style tables for each of the models. This isn't pretty code, and it was a lot of trial and error as well as looking through Stack Overflow. I've tried to comment as I went, but the basis is it splits the models into individual tables, and the rest is aesthetics. There are definitely redundant parts from adding new bits i.e. in the ```select()``` function, I don't need to add a new label given they are then given a new label in the ```cols_label()``` function but I can fix that.

```
# Visualise model summaries in a table
model_summaries %>%
  select(model, term, estimate, std.error, statistic, p.value) %>%
  gt()

# Define term labels for better readability
term_labels <- c(
  "(Intercept)" = "Intercept",
  "tef23_award_overallSilver" = "Overall: Silver",
  "tef23_award_overallGold" = "Overall: Gold",
  "tef23_award_experienceSilver" = "Experience: Silver",
  "tef23_award_experienceGold" = "Experience: Gold",
  "tef23_award_outcomeSilver" = "Outcome: Silver",
  "tef23_award_outcomeGold" = "Outcome: Gold",
  "tef23_award_experienceRequires Improvement" = "Experience: Requires Improvement" # nolint
)

# Split model summaries by model and format for display
model_list <- split(model_summaries, model_summaries$model)

for (mod in names(model_list)) {
  gt_tbl <- model_list[[mod]] %>%
    select(model, term, "Coef." = estimate, "Std. Err" = std.error, "t value" = statistic, p_value = p.value) %>% # nolint
    mutate(
      term = recode(term, !!!term_labels),
      p_value_fmt = formatC(p_value, format = "f", digits = 3),
      p_value_fmt = ifelse(as.numeric(p_value) < 0.05, paste0(p_value_fmt, " *"), p_value_fmt) # nolint
    ) %>%
    select(-p_value) %>%
    rename(p_value = p_value_fmt) %>%
    gt() %>%
    tab_header(
      title = md(paste0("**", mod, " Model Summary**")),
    ) %>%
    cols_label(
      term = "Predictor",
      `Coef.` = "B",
      `Std. Err` = "SE",
      `t value` = "t",
      p_value = "p"
    ) %>%
    fmt_number(
      columns = c("Coef.", "Std. Err", "t value"),
      decimals = 2
    ) %>%
    tab_options(
      table.font.names = "Times New Roman",
      table.align = "center",
      heading.align = "center",
      table.border.top.width = px(2),
      table.border.bottom.width = px(2),
      table_body.hlines.width = px(1)
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(everything())
    )
  print(gt_tbl)
}

```

This leaves me with the following tables:

