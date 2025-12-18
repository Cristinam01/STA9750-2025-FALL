complaints_q4_logit <- complaints_q4 %>%
  # Remove tracts with very small complaint counts (unstable rates)
  filter(total_n >= 10) %>%
  # Create resolved/unresolved counts
  mutate(
    total_resolved = total_n - (hpd_unres + dob_unres),
    total_unres    = hpd_unres + dob_unres
  ) %>%
  # Add log income (better behaved)
  mutate(
    log_income = log(median_income),
    log_units  = log(housing_units)
  ) %>%
  # Remove missing
  filter(!is.na(log_income))


logit_model2 <- glm(
  cbind(total_resolved, total_unres) ~ 
    log_income +
    scale(rent_burden) +
    scale(housing_units) +
    scale(total_n),
  family = binomial,
  data = complaints_q4_logit
)

summary(logit_model2)

library(ggeffects)

pred_income <- ggpredict(logit_model2, terms = "log_income")

plot(pred_income) +
  labs(
    title = "Predicted Resolution Probability vs Income",
    subtitle = "Controlling for housing units, rent burden, agency caseload, and total complaints",
    x = "Log of Median Household Income",
    y = "Predicted Resolution Probability"
  ) +
  theme_minimal(base_size = 14)


library(ggeffects)
library(ggplot2)
library(scales)

pred_income <- ggpredict(logit_model2, terms = "log_income")

# Convert x from log_income to original scale
pred_income$x_income <- exp(pred_income$x)


ggplot(pred_income, aes(x = x_income, y = predicted)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              fill = "steelblue", alpha = 0.2) +
  scale_x_continuous(labels = dollar_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Predicted Resolution Probability by Median Household Income",
    x = "Median Household Income",
    y = "Predicted Resolution Probability"
  ) +
  theme_minimal(base_size = 14)