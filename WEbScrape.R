library(tidyverse)
library(rvest)
library(nbastatR)
library(dplyr)
library(writexl)
library(ggplot2)
library(stringi)
library(tidyr)

#  -- Team Stats --
Team_Url = "https://www.basketball-reference.com/leagues/NBA_2023.html"
Team_Page = read_html(Team_Url)
Team = Team_Page %>% html_nodes("#totals-team .left") %>% html_text()

Games = Team_Page %>% html_nodes("#totals-team .left+ .right") %>% html_text() %>% as.numeric()
Team_Points = Team_Page %>% html_nodes("#totals-team .right:nth-child(25)") %>% html_text() %>% as.numeric()
FGA = Team_Page %>% html_nodes("#totals-team .right:nth-child(6)") %>% html_text() %>% as.numeric()
FGM = Team_Page %>% html_nodes("#totals-team .right:nth-child(5)") %>% html_text() %>% as.numeric()
ThreePercent = Team_Page %>% html_nodes("#totals-team .right:nth-child(10)") %>% html_text() %>% as.numeric()
TwoPercent = Team_Page %>% html_nodes("#totals-team .right:nth-child(13)") %>% html_text() %>% as.numeric()
FreeThrowAtt = Team_Page %>% html_nodes("#totals-team .right:nth-child(15)") %>% html_text() %>% as.numeric()
Offensive_Rebounds = Team_Page %>% html_nodes("#totals-team .right:nth-child(17)") %>% html_text() %>% as.numeric()
TurnOvers = Team_Page %>% html_nodes("#totals-team .right:nth-child(23)") %>% html_text() %>%as.numeric()
Minutes = Team_Page %>% html_nodes("#totals-team .right:nth-child(4)") %>% html_text() %>% as.numeric()
TwoAttempt = Team_Page %>% html_nodes("#totals-team .right:nth-child(12)") %>% html_text() %>% as.numeric()
ThreeAttempt = Team_Page %>% html_nodes("#totals-team .right:nth-child(9)") %>% html_text() %>% as.numeric()
# Adding Possesion Column
Possesions = FGA + 0.65*FreeThrowAtt - Offensive_Rebounds + TurnOvers
Possesions = as.numeric(Possesions)
Possesions_Per_Game = Possesions / Games
Pace = 48*(2*Possesions) /(2* (Minutes/5))

# Team Opponent Stats
Names_OPP_Stats = Team_Page %>% html_nodes("#totals-opponent .left") %>% html_text()
Opps_Points = Team_Page %>% html_nodes("#totals-opponent .right:nth-child(25)") %>% html_text() %>% as.numeric()
Opps_FGA = Team_Page %>% html_nodes("#totals-opponent .right:nth-child(6)") %>% html_text() %>% as.numeric()
Opps_FreeThrowAtt = Team_Page %>% html_nodes("#totals-opponent .right:nth-child(15)") %>% html_text() %>% as.numeric()
Opps_Offensive_Rebounds = Team_Page %>% html_nodes("#totals-opponent .right:nth-child(17)") %>% html_text() %>% as.numeric()
Opps_TurnOvers = Team_Page %>% html_nodes("#totals-opponent .right:nth-child(23)") %>% html_text() %>% as.numeric()
TwoAttemptOpp = Team_Page %>% html_nodes("#totals-opponent .right:nth-child(12)") %>% html_text() %>% as.numeric()
ThreeAttemptOpp = Team_Page %>% html_nodes("#totals-opponent .right:nth-child(9)") %>% html_text() %>% as.numeric()
OppFGM = Team_Page %>% html_nodes("#totals-opponent .right:nth-child(5)") %>% html_text() %>% as.numeric()
TwoPercentOpp <- Team_Page %>% html_nodes("#totals-opponent .right:nth-child(13)") %>% html_text() %>% as.numeric()
ThreePercentOpp <- Team_Page %>% html_nodes("#totals-opponent .right:nth-child(10)") %>% html_text() %>% as.numeric()
# Adding Opps Possesion Column
Opps_Possesions = Opps_FGA + 0.65*Opps_FreeThrowAtt - Opps_Offensive_Rebounds + Opps_TurnOvers %>% as.numeric()
Opps_Possesions_Per_Game = Opps_Possesions / Games
Opps_Pace = Opps_Possesions / Minutes *40
#Team Stats
Team_Stats <- data.frame(Team,Games,Team_Points,FGA,FreeThrowAtt,Offensive_Rebounds,TurnOvers,Minutes,Possesions,Possesions_Per_Game,Pace)
Opp_Team_Stats <- data.frame(Names_OPP_Stats,Opps_Points,Opps_FGA,Opps_FreeThrowAtt,Opps_Offensive_Rebounds,Opps_TurnOvers,Opps_Possesions,Opps_Possesions_Per_Game,Opps_Pace)
# Total Team Stats
Total_Team_Stats <- inner_join(Team_Stats,Opp_Team_Stats, by = c('Team'='Names_OPP_Stats'))
Total_Team_Stats$Offensive_Efficency = Total_Team_Stats$Team_Points /Total_Team_Stats$Possesions * 100
Total_Team_Stats$Deffensive_Efficency = Total_Team_Stats$Opps_Points /Total_Team_Stats$Possesions * 100
Total_Team_Stats$RbdRate <- Total_Team_Stats$Offensive_Rebounds/Total_Team_Stats$Opps_Offensive_Rebounds
 
View(Total_Team_Stats)
