# PTB_Econ_Report

## Huibin testing init.
## peter is here.  5:52 pm , 2025_09_17




## 1) Put PortableGit on PATH for this session
Sys.setenv(PATH = paste(
  "C:/Git/PortableGit/cmd",
  "C:/Git/PortableGit/bin",
  Sys.getenv("PATH"),
  sep = ";"
))

## 2) Verify R can see Git
system("git --version")

## 3) (Optional but recommended) make GCM the helper
system('"C:/Git/PortableGit/cmd/git.exe" config --global credential.helper manager-core')

## 4) Now set your GitHub creds via gitcreds
install.packages("gitcreds")       # if not installed yet
gitcreds::gitcreds_set()           # this time it should work


library(usethis)


create_github_token()

library(gitcreds)

gitcreds_set()

use_github()

use_git_config(user.name='xx', user.email='xx@gov.bc.ca')
