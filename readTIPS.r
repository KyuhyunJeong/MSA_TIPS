#################################################################################
# Author: Kyuhyun Jeong
# Date: 6/25/2018
# Description:  produce readTIPS function which takes a list of word as an input
#               and output a datatable with a frequency of each word appeared in
#               the data frame containg all tips from 2011 to 2018.
################################################################################# 

library(utf8)
library(readxl)
library(cli)
library(magrittr)
library(tidyr)
library(dplyr)

readTIPS <- function(wordsList){
  # Read all data
  TIPS_combined <- data.frame(tip=character(),
                              year = integer()) # create an empty dataset
  
  for(i in 1:8){
    year <- 2019-i
    dfName <- paste("TIPS", year, sep = "_")
    tmp <- read_xlsx("Tips Project 2019 - Blue Team 9.xlsx", sheet = i, col_names = F)
    colnames(tmp) <- c("tip")
    TIPS_combined <- TIPS_combined %>% rbind(tmp %>% mutate(year = year))
    assign(dfName, tmp)
  }
  
  remove(tmp) # clean up the temporary data
  
  
  # list words we are interested in to find out
  # wordsList <- c("Ask", "python", "trust")
  
  summaryDF <- data.frame(year = 2011:2018)
  for(word in wordsList){
    summaryDF<- summaryDF %>% cbind(
      TIPS_combined %>% mutate(temp = grepl(word, tip, ignore.case = T)) %>%
        group_by(year) %>%
        summarise(count = sum(temp)) %>%
        select(count)
    )
    colnames(summaryDF)[which(names(summaryDF) == "count")] <- word  
  }
  
  summaryDF %>% cbind(
    TIPS_combined %>% group_by(year) %>%
      summarise(NObs = n()) %>%
      select(NObs)
  )
}
