#!/bin/sh
# configuration guidance (https://aws.amazon.com/blogs/big-data/running-r-on-aws/)
# first ssh into ubuntu@ip instance

# install rstudio server
sudo apt-get update
sudo apt-get install r-base
sudo apt-get install gdebi-core
wget https://download2.rstudio.org/server/bionic/amd64/rstudio-server-2022.07.1-554-amd64.deb
sudo gdebi rstudio-server-2022.07.1-554-amd64.deb

# to check server status and supported operation (https://support.rstudio.com/hc/en-us/articles/200532327-Managing-RStudio-Workbench-RStudio-Server)
sudo rstudio-server

# the default root username is ubuntu, to create a new user (https://www.cyberciti.biz/faq/create-a-user-account-on-ubuntu-linux/)
sudo adduser liget_test  # will promote to create password
cat /etc/passwd
grep 'liget_test' /etc/passwd  # check

# to change password (https://www.geeksforgeeks.org/chpasswd-command-in-linux-with-examples/)
# no need to worry about ssh public keys (.pub) of the new users for this competition
# by now should be able to remote access the rstudio server through url: Public-IPv4:8787
