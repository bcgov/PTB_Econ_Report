# How to Setup Git, GitHub and Rstudio



# Connet Rstuio to Gitbub

Sys.setenv(PATH = paste( "C:/Git/PortableGit/cmd", "C:/Git/PortableGit/bin", Sys.getenv("PATH"), sep = ";"))

install.packages("gitcreds") 
install.packages("usethis")     

library(gitcreds)
library(usethis)

create_github_token()
gitcreds_set()
use_github()
use_git_config(user.name='xx', user.email='xx@gov.bc.ca')



## settup keys 

## make a directory for a keys

mkdir %USERPROFILE%\.ssh

## generate private and public keys 

"C:\Git\PortableGit\usr\bin\ssh-keygen.exe" -t ed25519 -C "Peter.Tseng@gov.bc.ca" -f %USERPROFILE%\.ssh\id_ed25519

## confirm your keys exist
dir %USERPROFILE%\.ssh
type %USERPROFILE%\.ssh\id_ed25519.pub
type %USERPROFILE%\.ssh\id_ed25519

## enable SSH-format signing and point to your public key
git config --global gpg.format ssh
git config --global user.signingkey "%USERPROFILE%\.ssh\id_ed25519.pub"
git config --global commit.gpgsign true
