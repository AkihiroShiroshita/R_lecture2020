##Readme file
install.packages("usethis") #to creat a readme file
install.packages("rmarkdown")
usethis::use_readme_rmd()
##Set up libraries
getwd() 
setwd("/cloud/project")
library(readxl)
install.packages("tidyverse")
install.packages("lubridate")
library(tidyverse)
library(lubridate)
install.packages("tidylog")
library(tidylog)
install.packages("naniar")
install.packages("visdat")
library(naniar)
library(visdat)
install.packages("ggplotgui")
library(ggplotgui)
install.packages("tableone")
library(tableone)
##Select the included patients
df <- read_csv("df_path") #df_path = data path
df_unique <- df %>% 
  group_by(ID) %>% 
  filter(n() >= 2)
length(df_unique$ID)
df_unique %>% write.csv(file="recurrent_ID.csv")
