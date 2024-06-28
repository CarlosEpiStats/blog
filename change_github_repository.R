library(usethis)
library(gitcreds)

user_name <- "CarlosEpiStats"
user_email <- "carlosepistats@gmail.com"


user_name <- "ISGlobal-TB"
user_email <- "carlos.fernandez@isglobal.org"

use_git_config(user.name = user_name, user.email = user_email)
create_github_token()
gitcreds_set()


