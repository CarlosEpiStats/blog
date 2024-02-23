library(usethis)
library(gitcreds)

use_git_config(user.name = "GraficosMedicos", user.email = "graficosmedicos@gmail.com") #Seems that nothing happens, but it has changed behind the scenes
create_github_token()
gitcreds_set()
