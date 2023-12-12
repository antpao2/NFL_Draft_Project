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
#All are all rookie stats with their draft position

#Going to omit the ones that didnt match or didnt record a catch in their rookie season 
###May not have matched bc of name, position different, etc, or may be blank bc no stats (injured, etc.)
### Will have to explain this in our paper 
all <-(na.omit(all))


dim(all)
#have 453 total stats for rookies

#Non-Rookie Stats: After Year 1
non_rookie <- p %>% filter(year_exp > 1) 
nr <- non_rookie %>% left_join(draft_wr[c(3,4,6)], by = c('name'= 'pfr_name'))
#Row 1373 of x
#row 14 of y


#Go over this, fill with 0s???
nr <- na.omit(nr)

length(unique(nr$name))
#434 total players have stats from year two and on


o <- all[c('pfr_name','epa')]

#Matching to combine
t <- nr %>% left_join(o, by = c('name' = 'pfr_name'), multiple='first')
length(unique(t$name))

#SHOULD I ZERO THE NAs because they didnt play
t <- t[c(2,3,4,10:14)]

names(t)[8] = 'rookie_epa'
names(t)[3] = 'season_epa'


#EPA in rookie season plotted against epa in next season
library(ggplot2)

#Scatter of the Two against each other
qplot(x= rookie_epa, y = season_epa, data = t, geom = c("point","smooth"))

#Hist of Season_EPA
qplot(season_epa, data = t, geom='histogram')

#Fails the test, non-normal
shapiro.test(t$season_epa)

#There is significant correlation between the two
cor.test(t$season_epa, t$rookie_epa)

#Pick as well there is correlation --> Higher pick, lower epa
cor.test(t$season_epa, t$pick)

#Round as well --> Explore wihtin each round
cor.test(t$season_epa, t$round)

#Season EPA
t %>% ggplot(aes(x=season_epa, fill = as.factor(round))) + geom_histogram() + labs(fill="")

#Boxplots
bp_season <- ggplot(t, aes(x = round, y=season_epa, group = round, fill=round)) + geom_boxplot() + coord_cartesian(ylim=c(-60,160))
bp_rookie <- ggplot(t, aes(x = round, y=rookie_epa, group = round, fill=round)) + geom_boxplot() + coord_cartesian(ylim=c(-60,160))

mean(t$season_epa)
mean(t$rookie_epa, na.rm = T)

bp_season
bp_rookie


#Round with Rookie and Season
t %>% group_by(round) %>% summarise(N = length(round), Mean_Season_EPA = mean(season_epa), Variance_Season_EPA = var(season_epa), Mean_Rookie_EPA = mean(rookie_epa, na.rm =T), Variance_Rookie_EPA = var(rookie_epa, na.rm=T)) 


#Differences--> These are differences
t <- t %>% mutate(Diff_EPA = season_epa - rookie_epa)
t

bp_diff <- ggplot(t, aes(x = round, y= Diff_EPA, group = round, fill=round)) + geom_boxplot() + coord_cartesian(ylim=c(-100,130))
bp_diff

#With the Z-Scores
t %>% group_by(round) %>% summarise(N=length(round), Mean_Diff = mean(Diff_EPA, na.rm=T), Var_Diff = var(Diff_EPA, na.rm=T)) %>% mutate(Z = Mean_Diff - 0 / sqrt(Var_Diff/N))

#Significantly different from zero
t.test(t$Diff_EPA)


#Maybe we can use a KS Test to eengineer the rookie data following a gamma distribution
#gamma_test in goft package
#or do so with the exponential


#Work with the rookie data to get it to follow a gamma dist with specific hyperparameters

#lambda should be adjusted too


