#### Working with Yards Per Target

library(tidyverse)
library(nflreadr)
library(nflfastR)
library(gsisdecoder)
library(qs)
library(fuzzyjoin)
library(dplyr)

#Loading Player Stats by Week
all <- load_player_stats(2003:2022)
wr <- all %>% filter(position == 'WR', season_type == 'REG')
wr <- wr %>% dplyr::select(names(wr)[c(1,3,4,7,8,35:49)] )
#Go over why some player names won't pull
#wr is per week

#This accumulates stats for a player over a given season (wr data is per week)
p <- wr %>% group_by(player_id, season) %>% summarise(name = unique(player_display_name),  epa = sum(receiving_epa, na.rm = T), rec = sum(receptions), yards = sum(receiving_yards), tds = sum(receiving_tds), targets = sum(targets), target_share = sum(target_share, na.rm=T) / length(target_share))
#P has a row for each player in each season, pull more stats by adding to summarize

#This adds total years played and years experience
p <- p %>% group_by(player_id) %>% mutate(total_seasons = length(player_id), year_exp = order(season)) 

#Remove dup player with same name to avoid matching confusion
p <- p %>% filter(player_id != '00-0020337' & player_id != '00-0020452' & player_id != '00-0021425')

p$name <- ifelse(p$player_id == '00-0023452', 'Mike Williams1', p$name)
p$name <- ifelse(p$player_id == '00-0027702', 'Mike Williams2', p$name )

#Name and rookie szn (Not sure if needed)
o <- p %>% group_by(name) %>% summarise(l = min(season))


#Draft Pick Data
draft_picks <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/draft_picks.csv")
draft_wr <- draft_picks %>% filter(position == 'WR' & season >= 2003 & season < 2023) 
draft_wr <- draft_wr %>% select(names(draft_wr)[c(1,2,3,4,5,6,10)])

#After using a fuzzy match with max dist to compare strings, manually changed the strings to match
draft_wr$pfr_name <- ifelse(draft_wr$pfr_name == 'Gabriel Davis', 'Gabe Davis', draft_wr$pfr_name)
draft_wr$pfr_name <- ifelse(draft_wr$pfr_name == 'Henry Ruggs III', 'Henry Ruggs', draft_wr$pfr_name)

#Double Matches
draft_wr$pfr_name <- ifelse(draft_wr$pfr_id == 'WillMi03', 'Mike Williams1', draft_wr$pfr_name)
draft_wr$pfr_name <- ifelse(draft_wr$pfr_id == 'WillMi04', 'Mike Williams2', draft_wr$pfr_name)
draft_wr$pfr_name <- str_replace(draft_wr$pfr_name, 'KJ Hamler', 'K.J. Hamler')
draft_wr$pfr_name <- str_replace(draft_wr$pfr_name, 'D.K. Metcalf', 'DK Metcalf')
draft_wr$pfr_name <- str_replace(draft_wr$pfr_name, 'Equanimeous St.Brown','Equanimeous St. Brown')
draft_wr$pfr_name <- str_replace(draft_wr$pfr_name, 'JJ Nelson','J.J. Nelson')
draft_wr$pfr_name <- str_replace(draft_wr$pfr_name, 'DJ Chark','D.J. Chark')
draft_wr$pfr_name <- str_replace(draft_wr$pfr_name, 'Laviska Shenault Jr.','Laviska Shenault')
draft_wr$pfr_name <- str_replace(draft_wr$pfr_name, 'Terrace Marshall Jr.','Terrace Marshall')
draft_wr$pfr_name <- str_replace(draft_wr$pfr_name, 'Michael Pittman Jr.','Michael Pittman')
#Changed these based on my fuzzy join

#Rookie Stats
all <- draft_wr %>% left_join(p, by = c('pfr_name'= 'name', 'season' = 'season'))


##WE SHOULD PUT A MINIMUM OF TARGETS ON
all <- all %>% mutate(yards_per_target = yards/targets)

#REMOVE ALL NA
all <- all[!(is.na(all$yards_per_target)),]
dim(all)

#449 Rows, 17 columns



#Non-Rookie Stats: After Year 1
non_rookie <- p %>% filter(year_exp > 1) 
nr <- non_rookie %>% left_join(draft_wr[c(3,4,6)], by = c('name'= 'pfr_name'))
nr <- nr %>% mutate(yards_per_target = yards/targets)



nr <- nr[!(is.na(nr$yards_per_target)),]
nr <- nr[!(is.na(nr$round)),]
length(unique(nr$name))
#433 total players have stats from year two and on that were drafted

#Subset the rookie data
o <- all[c('pfr_name','epa', 'yards_per_target')]


#Matching to combine
t <- nr %>% left_join(o, by = c('name' = 'pfr_name'), multiple='first')
length(unique(t$name))

#SHOULD I ZERO THE NAs because they didnt play
t <- t[c(2,3,6,8,10:14,16)]

names(t)[9] = 'season_ypt'
names(t)[10] = 'rookie_ypt'

#Remove those who have no rookie ypt (NOT ZERO, DIDNT RECORD)
t <- t[!(is.na(t$rookie_ypt)),]


#EPA in rookie season plotted against epa in next season
library(ggplot2)



