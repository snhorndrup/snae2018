setwd("~/GitHub/snae2018")
dir("data")

library(pacman)
p_load(matrix, tidyverse)


df <- read.csv("data/dipcon.csv")  

View(df)

summary(df)

names(df)

str(df)

head(df)

