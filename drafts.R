# Drafts of posts

## Sample size calculation based on precision of confidence intervals

install.packages("presize")
library(presize)

n_wilson <- ceiling(prec_prop(p = pi, n = NULL, conf.width = 0.2, conf.level = 1 - alpha, method = "wilson")[["n"]])
n_agresti_coull <- ceiling(prec_prop(p = pi, n = NULL, conf.width = 0.2, conf.level = 1 - alpha, method = "agresti-coull")[["n"]])
n_exact <- ceiling(prec_prop(p = pi, n = NULL, conf.width = 0.2, conf.level = 1 - alpha, method = "exact")[["n"]])
n_wald <- ceiling(prec_prop(p = pi, n = NULL, conf.width = 0.2, conf.level = 1 - alpha, method = "wald")[["n"]])