install.packages("XML")
install.packages("RSQLite")
install.packages("stringr")
install.packages("ggplot2")
library(XML)
library(RSQLite)
library(stringr)
library(ggplot2)
year <- 2013
url <-paste("http://sports.yahoo.com/nfl/stats/byteam?group=Offense&
cat=Total&conference=NFL&year=season_",year,"&sort=530&old_cat
egory=Total&old_group=Offense")
offense <- readHTMLTable(url, encoding = "UTF-8",
                         colClasses="character")[[7]]
offense <- offense[,-c(2,4,6,8,10,12,14,16,18,20,22,24,26,28)]
offense[,1] <- as.character(offense[,1])
offense[,2:13] <- apply(offense[,2:13],2,as.numeric)
offense[,14] <- as.numeric(substr(offense[,14], 1, 2))*60 +
  as.numeric(substr(offense[,14], 4, 6))
url <-
  paste("http://sports.yahoo.com/nfl/stats/byteam?group=Defense&
cat=Total&conference=NFL&year=season_",year,"&sort=530&old_cat
egory=Total&old_group=Defense")
defense <- readHTMLTable(url, encoding = "UTF-8",
                         +                          colClasses="character")[[7]]
defense <- defense[,-c(2,4,6,8,10,12,14,16,18,20,22,24,26)]
defense[,1] <- as.character(defense[,1])
defense[,2:13] <- apply(defense[,2:13],2,as.numeric)
means <- apply(defense[,2:13],2,mean)
combined <- merge(offense, defense, by.x="Team", by.y="Team")
colnames(combined)[2] <- "Games"
colnames(combined)[3] <- "OffPPG"
colnames(combined)[4] <- "OffYPG"
colnames(combined)[5] <- "OffPassYPG"
colnames(combined)[6] <- "OffRushYPG"
combined$G.y <- NULL
colnames(combined)[15] <- "DefPPG"
colnames(combined)[16] <- "DefYPG"
colnames(combined)[17] <- "DefRushYPG"
colnames(combined)[18] <- "DefPassYPG"
hist(combined$OffPPG, breaks=10, main="Offensive Points Per
Game", xlab="Offensive PPG",ylab="Number of Teams")
mean(combined$OffPPG)
sd(combined$OffPPG)
hist(combined$DefPPG, breaks=10, main="Defensive Points Per
Game", xlab="Defensive PPG",ylab="Number of Teams")
ppg <- transform(combined,Team=reorder(Team,combined$OffPPG))
ggplot(ppg,aes(x=Team, y=OffPPG)) +
  geom_bar(stat='identity',color="black",fill="blue") +
  coord_flip() + labs(x="Team",y="Avg Points per Game") +
  ggtitle("Avg Points per Game") + theme(plot.title =
                                           element_text(size=18, face="bold"))
ypg <- transform(combined,Team=reorder(Team,-combined$DefYPG))
ggplot(ypg,aes(x=Team, y=DefYPG)) +
  geom_bar(stat='identity',color="black",fill="blue") +
  coord_flip() + labs(x="Team",y="Avg Yards Allowed per Game") +
  ggtitle("Avg Yards Allowed per Game") + theme(plot.title =
                                                  element_text(size=18, face="bold"))

ggplot(combined, aes(x=combined$DefYPG, y=combined$DefPPG)) +
  geom_point(shape=5, size=2) + geom_smooth() +
  labs(x="Yards Allowed per Game",y="Points Alloed per Game")
+ ggtitle("Defense Yards vs. Points per Game") +
  theme(plot.title = element_text(size=18, face="bold"))
ggplot(combined, aes(x=combined$TOP, y=combined$OffPPG)) +
  geom_point(shape=5, size=2) + geom_smooth() +
  labs(x="Time of Possession (Seconds)",y="Points per Game") +
  ggtitle("Time of Possession vs. Points per Game") +
  theme(plot.title = element_text(size=18, face="bold"))
offense$OPassStrength <- max(offense[,5])-offense[,5]
offense$OPassStrength <- (1-
                            (offense$OPassStrength/max(offense$OPassStrength)))*100
offense$ORushStrength <- max(offense[,6])-offense[,6]
offense$ORushStrength <- (1-
                            (offense$ORushStrength/max(offense$ORushStrength)))*100
offense$OPPGStrength <- max(offense[,3])-offense[,3]
offense$OPPGStrength <- (1-
                           (offense$OPPGStrength/max(offense$OPPGStrength)))*100
offense$OYPGStrength <- max(offense[,4])-offense[,4]
offense$OYPGStrength <- (1-
                           (offense$OYPGStrength/max(offense$OYPGStrength)))*100
offense$OffStrength <-
  (offense$OPassStrength+offense$ORushStrength+offense$OPPGStren
   gth+offense$OYPGStrength)/4
defense$DPassStrength <- max(defense[,6])-defense[,6]
defense$DPassStrength <-
  defense$DPassStrength/max(defense$DPassStrength)*100
defense$DRushStrength <- max(defense[,5])-defense[,5]
defense$DRushStrength <-
defense$DRushStrength/max(defense$DRushStrength)*100
defense$DPPGStrength <- max(defense[,3])-defense[,3]
defense$DPPGStrength <-
defense$DPPGStrength/max(defense$DPPGStrength)*100
defense$DYPGStrength <- max(defense[,4])-defense[,4]
defense$DYPGStrength <-
defense$DYPGStrength/max(defense$DYPGStrength)*100
defense$DefStrength <-
(defense$DPassStrength+defense$DRushStrength+defense$DPPGStren
gth+defense$DYPGStrength)/4
offense$OffStrength <- (offense$OPPGStrength * 0.4) +
  (offense$OPassStrength * 0.25) + (offense$OYPGStrength * 0.2) +
  (offense$ORushStrength * 0.15)
home_team <- "Chicago Bears"
away_team <- "New Orleans Saints"
  off_game <- subset(offense,Team==home_team |
                       Team==away_team)[,c(1,15,16,19)]
def_game <- subset(defense,Team==home_team |
                     Team==away_team)[,c(1,14,15,18)]