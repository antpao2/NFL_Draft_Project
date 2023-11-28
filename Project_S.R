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

#This accumulates stats for a player over a given season (wr data is per week)
p <- wr %>% group_by(player_id, season) %>% summarise(name = unique(player_display_name),  epa = sum(receiving_epa, na.rm = T), rec = sum(receptions), yards = sum(receiving_yards), tds = sum(receiving_tds), targets = sum(targets), target_share = sum(target_share, na.rm=T) / length(target_share))
#P has a row for each player in each season, pull more stats by adding to summarize

#This adds total years played and years experience
p <- p %>% group_by(player_id) %>% mutate(total_seasons = length(player_id), year_exp = order(season)) 



#Draft Pick Data
draft_picks <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/draft_picks.csv")
draft_wr <- draft_picks %>% filter(position == 'WR' & season >= 2003 & season < 2023) 
draft_wr <- draft_wr %>% select(names(draft_wr)[c(1,2,3,4,5,6,10)])
#642 different names


#TESTING
l <- draft_wr %>% filter(season == 2003)
k <- p %>% filter(season == 2003)

not_2003 <- k[!(k$name %in% l$pfr_name),]
p <- p[!(p$player_id %in% not_2003$player_id),]


#We want all draft pick names in here

#Rookie Stats
all <- draft_wr %>% left_join(p, by = c('pfr_name'= 'name', 'season' = 'season'))
#All are all rookie stats with their draft position (go over something with the match)

#Rookie gets draft pick data (might have to ditch UDFAs)


#Non-Rookie Stats: After Year 1
non_rookie <- p %>% filter(year_exp > 1) 
nr <- non_rookie %>% left_join(draft_wr[c(3,4,6)], by = c('name'= 'pfr_name'))
nr <- na.omit(nr)









