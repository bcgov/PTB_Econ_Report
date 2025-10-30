##Connect RStudio to Git to run this report

## 1) Replace the two sections marked 'x' in the PATH = paste('C:/x/Git/cmd','C:/x/Git/bin'...) function with the install path for Git on your workstation

Sys.setenv(PATH = paste(
  "C:/Program Files/Git/cmd",
  "C:/Program Files/Git/bin",
  Sys.getenv("PATH"),
  sep = ";"
))

## 2) Now set your GitHub creds via gitcreds
install.packages("gitcreds")       # if not installed yet
gitcreds::gitcreds_set()           # this time it should work


library(usethis)


create_github_token()

library(gitcreds)

gitcreds_set()

use_github()

## 3) Add in your IDIR username and government email where it is marked 'x'
use_git_config(user.name='x', user.email='x')

##Beginning of Econ Report Code


2:30