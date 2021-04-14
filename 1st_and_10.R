library(dplyr)
library(ggplot2)
library(extrafont)
library(ggimage)
library(nflfastR)
library(ggtext)
library(toOrdinal)

pbp <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

team="BUF"

PR_by_week<-pbp %>%
  filter(week<18, pass==1 | rush==1, down==1, ydstogo==10) %>%
  group_by(posteam,defteam,week) %>%
  summarize(rate=mean(pass))

nfl_avg<-pbp %>%
  filter(week<18, pass==1 | rush==1, down==1, ydstogo==10) %>%
  group_by(posteam) %>%
  summarize(rate=mean(pass)) %>%
  arrange(-rate)

nfl_avg$rank<-1:32

team_week<-PR_by_week %>%
  filter(posteam==team) %>%
  left_join(.,teams_colors_logos[,c(1,2,5,6)],by=c("posteam"="team_abbr")) %>%
  left_join(.,teams_colors_logos[,c(1,9)],by=c("defteam"="team_abbr"))

prim=as.character(team_week[1,6])
sec=as.character(team_week[1,7])
teamname=as.character(team_week[1,5])

team_avg<-nfl_avg %>%
  filter(posteam==team) %>%
  summarize(rate=round(100*rate,0),rank=toOrdinal(rank))


X<-ggplot(PR_by_week,aes(x=week,y=100*rate))
X+geom_point(fill="grey50",pch=21,size=3.5,alpha=0.2)+
  geom_hline(yintercept=100*mean(nfl_avg$rate),color="black",linetype="dashed",size=1)+
  geom_hline(yintercept=mean(team_avg$rate),color=prim,linetype="dotted",size=1.2)+
  geom_line(data=team_week,color=prim,size=1.2)+
  geom_point(data=team_week,color=sec,size=8)+
  geom_image(data=team_week,aes(y=0,image=team_logo_wikipedia),size=0.04,asp=13/9)+
  geom_segment(aes(x=1,xend=1.9,y=100,yend=100),color=prim, size = 1.2,linetype="dotted")+
  geom_segment(aes(x=1,xend=1.9,y=95,yend=95),color="black", size = 1,linetype="dashed")+
  geom_text(aes(x=2,y=100,label=paste0(team," Avg = ",team_avg[1,1],"% (",team_avg[1,2],")")),color=prim,family="Trebuchet MS",size=5,hjust=0,fontface="bold")+
  geom_text(aes(x=2,y=95,label=paste0("NFL Avg = ",round(100*mean(nfl_avg$rate),0),"%")),color="black",family="Trebuchet MS",size=5,hjust=0,fontface="bold")+
  scale_x_continuous(breaks=1:17)+
  coord_cartesian(ylim=c(0,100))+
  labs(x="Week / Opp", y = "1st and 10 Pass Rate",
       title="1st and 10 Pass Frequency, 2020 Season",
       subtitle=teamname,caption="@MafiaNumbers  |  Data: @nflfastR")+
  theme(plot.background = element_rect(fill="gray93"),
        panel.background = element_rect(fill="gray96"),
        panel.border = element_rect(fill=NA,color="black",size=1),
        text=element_text(family="Trebuchet MS",color="navyblue"),
        axis.text = element_text(size=17,color="gray15"),
        axis.title.x=element_text(size=20,face="bold",margin=margin(t=10),color="gray15"),
        axis.title.y=element_text(size=20,face="bold",margin=margin(r=10),color="gray15"),
        plot.title = element_text(size=24,face="bold",margin=margin(b=10),color="gray15"),
        plot.subtitle=element_text(size=19,color=prim,face=4),
        plot.caption=element_text(size=13,color="gray15"))
