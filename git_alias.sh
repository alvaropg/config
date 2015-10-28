#!/bin/bash

# User config
git config --global user.name "Álvaro Peña"
git config --global user.email alvaropg@gmail.com

# GNOME
git config --global url.ssh://[login@]git.gnome.org/git/.insteadof gnome:

# git color everywhere
git config --global color.ui auto

# git log with colors (use: "git lg" or "git lg -p")
git config --global alias.lg "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"

# "git up" for a pull with rebase
git config --global alias.up "pull --rebase"
