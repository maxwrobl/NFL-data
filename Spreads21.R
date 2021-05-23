library(dplyr)
library(ggplot2)
library(ggimage)
library(nflfastR)
library(extrafont)

logos<-teams_colors_logos
myteam="BUF"
odds<-read.csv(url("https://raw.githubusercontent.com/danmorse314/nfl-stuff/main/nfl_odds_2021.csv"))

df<-odds %>% filter(team_abbr==myteam,opponent != "Bye")
df$game<-1:17
df<-merge(df,logos[,c(1,5,9)],by.x="opponent",by.y="team_abbr") 
df<-df %>% arrange(week)

home<-df %>% filter(home_team==myteam)
away<-df %>% filter(away_team==myteam)

home_opps = home$opponent
away_opps = away$opponent

sample<-odds %>% filter(away_team %in% home_opps |
                        home_team %in% away_opps)

home_avg<-sample %>%
  filter(home_team %in% away_opps,home_team==team_abbr) %>%
  group_by(home_team) %>%
  summarize(line=mean(line))

away_avg<-sample %>%
  filter(away_team %in% home_opps,away_team==team_abbr) %>%
  group_by(away_team) %>%
  summarize(line=mean(line))

home_avg<-merge(home_avg,df[,c(7,19)],by="home_team")
away_avg<-merge(away_avg,df[,c(8,19)],by="away_team")

home_dots<-sample %>%
  filter(opponent!=myteam,home_team %in% away_opps,home_team==team_abbr) 

away_dots<-sample %>%
  filter(opponent!=myteam,away_team %in% home_opps,away_team==team_abbr)

home_dots<-merge(home_dots,df[,c(7,19)],by="home_team")
away_dots<-merge(away_dots,df[,c(8,19)],by="away_team")

mytitle <- paste0("2021 Season Point Spreads for the ",df[1,2])

ggplot(df,aes(x=game,y=-line))+
  geom_hline(yintercept = 0, linetype="dashed")+
  geom_point(data=home_dots,aes(x=game,y=line),size=3,alpha=0.2,color="red")+
  geom_point(data=away_dots,aes(x=game,y=line),size=3,alpha=0.2,color="blue")+
  geom_image(aes(image=team_logo_wikipedia.x),size=0.04,asp=17/10)+
  scale_x_continuous(labels=df$week,breaks=1:17)+
  scale_y_continuous(breaks=seq(-15,15,5),labels=c("+15","+10","+5","0","-5","-10","-15"))+
  geom_image(aes(image=team_logo_wikipedia.y,y=min(away_dots$line,home_dots$line)-2),size=0.045,asp=17/10)+
  geom_point(data=home_avg,aes(x=game,y=line),pch=18,size=4,color="black")+
  geom_point(data=away_avg,aes(x=game,y=line),pch=18,size=4,color="black")+
  labs(x="Week/Opponent",y="Point Spread",title=mytitle,
       subtitle="Blue = Home  |   Red = Away  |   Black Diamond = Average spread for opponent",
       caption="Lines: @The_Oddsmaker")+
  theme(plot.background = element_rect(fill="gray90"),
        panel.background = element_rect(fill="gray90"),
        panel.border = element_rect(fill=NA,color="black",size=1.5),
        text=element_text(family="Microsoft PhagsPa",color="navyblue"),
        plot.title=element_text(face="bold",size=19),,
        plot.subtitle = element_text(size=12),
        axis.title=element_text(face="bold",size=14),
        axis.text=element_text(size=12))
