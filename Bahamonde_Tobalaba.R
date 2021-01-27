############################## 
# Cleaning
##############################

## ---- constructing:data
# Load the data
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(foreign)

dat = read.dta("https://github.com/hbahamonde/Conjoint_US/raw/master/data_list.dta")
