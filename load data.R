# add file download here#

library(tidyverse)
library(magrittr)
library(data.table)
library(readxl)

#reading data  from excel files
d1 <- read_excel("Data/Updated PR sheet.xlsx")
d2 <- read_excel("Data/Updated PR sheet3.xlsx")

data <-  rbind(d1,d2)

data %<>% mutate(ID=1:159) %>% select(ID,2:9)

rm(d1,d2)
