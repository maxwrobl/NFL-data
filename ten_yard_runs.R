library(dplyr)
library(ggplot2)
library(nflfastR)
library(ggimage)

season=2017

pbp <- readRDS(url(paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",season,".rds")))
players <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/roster-data/roster.rds')) %>%
  filter(teamPlayers.position=="RB",team.season==season)

ten_plus<-pbp %>%
  filter(!is.na(rusher),!is.na(yards_gained),id %in% players$teamPlayers.gsisId) %>%
  group_by(rusher,id) %>%
  summarize(att=sum(rush),over=sum(rush[yards_gained>9]),pct=100*over/att) %>%
  filter(att>99) %>%
  left_join(.,players[,c(12,21)],by=c("id" = "teamPlayers.gsisId")) %>%
  left_join(.,teams_colors_logos[,c(1,5,6,9)],by=c("team.abbr"="team_abbr")) %>%
  arrange(-pct)

title<-paste0("Percent of Carries for 10+ Yards, ",season)

ggplot(ten_plus,aes(x=pct,y=reorder(rusher,pct))) +
    geom_bar(stat="identity",width=0.8,fill=ten_plus$team_color,alpha=0.9)+
    coord_cartesian(xlim=c(min(ten_plus$pct)-0.5,max(ten_plus$pct)+0.5),ylim=c(1,nrow(ten_plus)+0.1))+
    geom_image(aes(image=team_logo_wikipedia,x=pct+0.05),size=0.03,asp=7/8)+
    labs(title=title,caption="Figure: @MafiaNumbers  |  Data: @nflfastR",
       x="% of Carries 10 Yards or More",subtitle="min. 100 attempts")+
    theme(plot.background = element_rect(fill="gray85"),
        panel.background = element_rect(fill="gray85"),
        panel.border = element_rect(fill=NA,color="black",size=1.5),
        plot.title = element_text(size=22),
        plot.subtitle = element_text(size=15),
        plot.caption=element_text(size=12,margin=margin(t=10)),
        axis.title.x=element_text(size=18,face="bold",margin=margin(t=5)),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=12),
        text=element_text(family="Bahnschrift",color="navyblue")) +
    ggsave("ten_over.png",height=9,width=7)
