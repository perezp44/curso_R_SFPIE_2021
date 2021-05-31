#- script a tener en la primera clase
library(palmerpenguins)   #- install.packages("palmerpenguins")
library(rio)              #- install.packages("rio")
library(fs)               #- install.packages("fs")
library(eurostat)         #- install.packages("eurostat")
library(curl)             #- install.packages("curl")
library(rvest)            #- install.packages("rvest")
library(tidyverse)        #- install.packages("tidyverse")
library(pjpv.datos.01)    #- remotes::install_github("perezp44/pjpv.datos.01")


if (!require("pacman")) install.packages("pacman")
pacman::p_load(palmerpenguins, rio, fs, eurostat, curl, rvest, tidyverse)
pacman::p_load_gh("perezp44/pjpv.datos.01")
