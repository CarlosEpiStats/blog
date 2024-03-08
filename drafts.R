# Drafts of posts

## Sample size calculation based on precision of confidence intervals

#install.packages("presize")
library(presize)
library(tidyverse)

pi = 0.80
alpha = 0.05
conf.width = 0.14
prevalence = 0.2

### Calcular el tamaño muestral estimado dada una precisión

n_wilson <- ceiling(prec_prop(p = pi, n = NULL, conf.width = 0.2, conf.level = 1 - alpha, method = "wilson")[["n"]])
n_agresti_coull <- ceiling(prec_prop(p = pi, n = NULL, conf.width = 0.2, conf.level = 1 - alpha, method = "agresti-coull")[["n"]])
n_exact <- ceiling(prec_prop(p = pi, n = NULL, conf.width = 0.2, conf.level = 1 - alpha, method = "exact")[["n"]])
n_wald <- ceiling(prec_prop(p = pi, n = NULL, conf.width = 0.2, conf.level = 1 - alpha, method = "wald")[["n"]])


sample_size_total <- prec_prop(p = pi, n = NULL, conf.width = conf.width, conf.level = 1 - alpha, method = "wilson")[["n"]] * 5

### Calcular precisión estimada a partir del tamaño muestral

perc_eswatini <- 250 / 650

sample_size_eswatini <- sample_size_total * perc_eswatini
sample_size_eswatini * prevalence


n <- 1:1000
error_margin <- prec_prop(p = pi, n = n * prevalence, conf.width = NULL, conf.level = 1 - alpha, method = "wilson")[["conf.width"]] / 2 * 100

data <- as.data.frame(cbind(n, error_margin)) %>% 
  mutate(n_confirmed = n * prevalence)

ggplot(data, aes(n, error_margin)) + 
  geom_line() + 
  geom_hline(aes(yintercept = 7), linetype = "dashed") +
  theme_minimal() + 
  coord_cartesian(ylim = c(0, 15)) +
  labs(x = "Number of patients recruited",
       y = "Estimation error margin (%)")

data_regression <- data %>% filter(n >= 250)

summary(lm(error_margin ~ n, data = data_regression))$coeff[2,1] * 10
