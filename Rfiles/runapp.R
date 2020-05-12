# install packages
#install.packages("shiny")
#install.packages("tidyverse")
#install.packages("binaryLogic")
#install.packages("zoo")
# load packages
library(shiny)
library(tidyverse)
library(binaryLogic)
library(zoo)
rm(list=ls())

# change the following path to the path of the csv file
setwd("C:\\Github\bacondecomp\Rfiles")
RNGversion("3.5")
source("gui.R")
source("server.R")
shinyApp(ui,server)
