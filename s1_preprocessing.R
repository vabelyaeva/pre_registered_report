
######### libraries #########
.libPaths('C:/Users/vbeliaev/Documents/r_packages')

knitr::opts_chunk$set(echo = TRUE)

rm(list = ls())

library(ggsignif)
library(ggplot2)
library(gridExtra)
library(lme4)
library(dplyr)
library(plyr)
library(boot)
library(data.table)

'%!in%' <- function(x,y)!('%in%'(x,y))

####################################
######### preprocessing #########

setwd('C:/Users/vbeliaev/Desktop/phd_courses/ZNZ symposium poster')
tab1 = read.csv("Data_collected_together.csv")

# set bottom choice to 0 (instead of -1), when top picture is chosen = 1 
tab1$Choice01 = tab1$Choice
tab1$Choice01[tab1$Choice01 == -1] = 0

# remove too slow answers
idx = which(tab1$RT_choice>0)
tab1 = tab1[idx,]

# create 8 tiles for size and taste trials 
# this tiles will be used for plots 
tab1$Size_diff.ntile = ntile(tab1$Size_diff,8)
tab1$Taste_diff.ntile = ntile(tab1$Taste_diff,8)

# take absolute value of differences in size and taste between top and bottom food items
tab1$Size_diff.abs = abs(tab1$Size_diff)
tab1$Taste_diff.abs = abs(tab1$Taste_diff)

# 1 subject is removed because they had 50% of correct trails - chance level 
idx = which(tab1$Participant %!in% c(2))
tab1 = tab1[idx,]

tab1$Stim = 1
idx = which(tab1$Session_type==0)         #Stim variable: active/sham sessions
tab1$Stim[idx] = -1

tab1$Block = 1
idx = which(tab1$Trial_Nr>=113)            # Block variable: first 112 and last 64 trials
tab1$Block[idx] = -1

# leave only taste trials
idx = which(tab1$Cue_Taste1_Size2 == 1)
dataVal = tab1[idx,]

# leave only size trials
idx = which(tab1$Cue_Taste1_Size2 == 2)
dataPer = tab1[idx,]

## Get the correct response for taste table
dataVal$corr = 0
idx = which( (dataVal$Taste_diff>0 & dataVal$Choice01==1) | (dataVal$Taste_diff<0 & dataVal$Choice01==0))
dataVal$corr[idx] = 1
dataVal$Taste_diff.abs = scale(dataVal$Taste_diff.abs)

## Get the correct response for size table
dataPer$corr = 0
idx = which( (dataPer$Size_diff>0 & dataPer$Choice01==1) | (dataPer$Size_diff<0 & dataPer$Choice01==0))
dataPer$corr[idx] = 1
dataPer$Size_diff.abs = scale(dataPer$Size_diff.abs)

write.csv(rbind(dataVal, dataPer), 'Data_collected_together_preprocessed.csv')
