install.packages("moderndive")
install.packages("skimr")
install.packages("gapminder")

library(tidyverse)
library(moderndive)
library(skimr)
library(gapminder)

# Selecting a subset of variables 
  
evals_ch5 <- evals %>%
  select(ID, score, bty_avg, age)

# Previewing the raw data

glimpse(evals_ch5)

# Selecting a random sample of 5 rows to preview

evals_ch5 %>%
  sample_n(size = 5)

# Computing summary statistics for beauty score and teaching score

evals_ch5 %>%
  summarize(mean_bty_avg = mean(bty_avg), mean_score = mean(score),
            median_bty_avg = median(bty_avg), median_score = median(score))

evals_ch5 %>% 
  select(score, bty_avg) %>%
  skim()

# Correlation coefficient of beauty score and teaching score

evals_ch5 %>%
  get_correlation(formula = score ~ bty_avg)

# Data visualization: Scatterplot of the relationship between teaching and beauty scores
# The positive slope of the regression line demonstrates a positive relationship, as instructors have a higher 
# beauty score they also receive higher evaluations.

ggplot(evals_ch5, aes(x = bty_avg, y = score)) +
  geom_point() + 
  labs(x = "Beauty Score",
       y = "Teaching Score",
       title = "Scatterplot of relationship between teaching and beauty scores.") +
  geom_smooth(method = "lm", se = FALSE)

# Fit regression model
score_model <- lm(score ~ bty_avg, data = evals_ch5)

# Get regression table
# For every increase of 1 unit of bty_avg there is an associated increase of, on average, 0.067 units of score.
get_regression_table(score_model)

# Dataframe of the residuals of the model for teaching and beauty scores
regression_points <- get_regression_points(score_model)
regression_points



# Computing summary statistics for age and teaching score
evals_ch5 %>%
  summarize(mean_age = mean(age), mean_score = mean(score),
            median_age = median(age), median_score = median(score))

evals_ch5 %>%
  select(age, score) %>%
  skim()

# Correlation coefficient of age and teaching score

evals_ch5 %>%
  get_correlation(formula = age ~ score)

# Data visualization: Scatterplot of the relationship between age and teaching scores
# The negative slope of the regression line demonstrates a negative relationship, as the age 
# decreases they also receive lower evaluations.

ggplot(evals_ch5, aes(x = age, y = score)) +
  geom_point() +
  labs(x = "age", 
       y = "score",
       title = "Scatterplot of relationship between age and teaching scores") +
  geom_smooth(method = "lm",se = FALSE)

# Fit regression model
age_model <- lm(score ~ age, data = evals_ch5)

# Get regression table
# For every increase of 1 unit of age there is an associated increase of, on average, -0.006 units of score.
get_regression_table(age_model)
  
# Dataframe of the residuals of the model for age and teaching scores
regression_points2 <- get_regression_points(age_model)
regression_points2
