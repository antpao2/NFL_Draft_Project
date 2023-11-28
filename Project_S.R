install.packages("tidyverse")
install.packages("nflreadr")
install.packages("nflfastR")
install.packages("gsisdecoder")
install.packages("qs")
library(tidyverse)
library(nflreadr)
library(nflfastR)
library(gsisdecoder)
library(qs)


#Loading Player Stats by Week
all <- load_player_stats(2003:2022)
wr <- all %>% filter(position == 'WR', season_type == 'REG')
wr <- wr %>% select(names(wr)[c(1,3,4,7,8,35:49)] )
#Go over why some player names won't pull
#wr is per week


#Draft Pick Data
draft_picks <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/draft_picks.csv")
draft_wr <- draft_picks %>% filter(position == 'WR' & season >= 2003 & season < 2023) 
draft_wr <- draft_wr %>% select(names(draft_wr)[c(1,2,3,4,5,6,10)])
draft_wr


#This accumulates stats for a player over a given season (wr data is per week)
p <- wr %>% group_by(player_id, season) %>% summarise(name = unique(player_display_name), epa = sum(receiving_epa))
#P has a row for each player in each season, pull more stats by adding to summarize
#May want to use yards per game or another statistic

head(p)
#Doesn't have UDFAs --> THIS IS A GOOD DATASET THO FOR DRAFT PICKS AND ROOKIE SEASON INF)
all <- draft_wr %>% left_join(p, by = c('pfr_name'= 'name', 'season' = 'season'))


#Go over how we should handle  (THIS ONLY GOES BACK TO 2003, so modify our data) (possibly could use this)
all_players <- load_players() %>% filter(position == 'WR' & rookie_year >= 2003)


#To get after year 1, we have to add a filter on season and rookie year


head(all_players)

dim(all_players)
colnames(all_players)
