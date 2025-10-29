# PTB_Econ_Report

## Huibin testing init.
## peter is here.  5:52 pm , 2025_09_17

## Jala is here 9:56 am, 2025_09_28
## Huibin doing demo.
## Mohsen is here
## Nav is also here!
## Peter is here again. main
##testing
## xx x
# xyz zzzddddfdddddddddddddd



## 1) Put PortableGit on PATH for this session
Sys.setenv(PATH = paste( "C:/Git/PortableGit/cmd", "C:/Git/PortableGit/bin", Sys.getenv("PATH"), sep = ";"))

## 4) Now set your GitHub creds via gitcreds
install.packages("gitcreds")      
gitcreds::gitcreds_set()          

library(usethis)

create_github_token()

library(gitcreds)

gitcreds_set()

use_github()

use_git_config(user.name='xx', user.email='xx@gov.bc.ca')



## when you have issues with committing through Rsduio 
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
