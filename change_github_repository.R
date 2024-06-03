library(usethis)
library(gitcreds)

user_name <- "GraficosMedicos"
user_email <- "graficosmedicos@gmail.com"

use_git_config(user.name = user_name, user.email = user_email)
create_github_token()
gitcreds_set()
