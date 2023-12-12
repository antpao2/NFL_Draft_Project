
#Run project_ypt first to get all data

library(tidyverse)
library(nflreadr)
library(nflfastR)
library(gsisdecoder)
library(qs)
library(fuzzyjoin)
library(dplyr)


#EPA in rookie season plotted against epa in next season
library(ggplot2)

#Scatter of the Two against each other
qplot(x= rookie_ypt, y = season_ypt, data = t, geom = c("point","smooth"))

#Hist of Season_EPA
qplot(season_ypt, data = t, geom='histogram')


#Definitely use this plot as well
qplot(x= pick, y = season_ypt, data = t, geom = c("point","smooth"), xlab = 'Pick', ylab='Yards Per Target', main = 'Yards Per Target vs Draft Pick #')


#There is significant correlation between the two
cor.test(t$season_ypt, t$rookie_ypt)
#Pick as well there is correlation --> Higher pick, lower epa
cor.test(t$season_ypt, t$pick)

#NOW SHOW GRAPHS THIS WAY


#Season EPA --> This is useless
t %>% ggplot(aes(x=round, fill = as.factor(round))) + geom_histogram() + labs(fill="")
t %>% group_by(round) %>% summarise(N = length(round))

#Boxplots
bp_season <- ggplot(t, aes(x = round, y=season_ypt, group = round, fill=round)) + geom_boxplot() + coord_cartesian(ylim=c(-5,20)) + ggtitle('Non-Rookie Seasons') + ylab("Yards per Target") + xlab('Round')
bp_rookie <- ggplot(t, aes(x = round, y=rookie_ypt, group = round, fill=round)) + geom_boxplot() + coord_cartesian(ylim=c(-5,20)) + ggtitle('Rookie Seasons') + ylab("Yards per Target") + xlab('Round')

mean(t$season_ypt)
mean(t$rookie_ypt)

bp_season
bp_rookie

#Differences in Variance
anova(lm(season_ypt~factor(round), data = t))
cor.test(t$season_ypt, t$rookie_ypt)

#This is something that we can look at further with our sampling
summary(lm(season_ypt ~ rookie_ypt + factor(round), data=t))

var(t$season_ypt)


#Differences--> These are differences
t <- t %>% mutate(Diff_YPT = season_ypt - rookie_ypt)
t

bp_diff <- ggplot(t, aes(x = round, y= Diff_YPT, group = round, fill=round)) + geom_boxplot() + coord_cartesian(ylim=c(-20,20))
bp_diff

#With the Z-Scores
k <- t %>% group_by(round) %>% summarise(N=length(round), Mean_Diff = mean(Diff_YPT, na.rm=T), Var_Diff = var(Diff_YPT, na.rm=T)) %>% mutate(Z = Mean_Diff - 0 / sqrt(Var_Diff/N))

#Significantly different from zero
t.test(t$Diff_YPT)


k$Var_Diff


all %>% group_by(round) %>% summarise(m = mean(yards_per_target), v = var(yards_per_target), n = length(round), l = m - 1.96*sqrt(v/n), u = m + 1.96*sqrt(v/n))
