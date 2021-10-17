library(tidytuesdayR)
library(tidyverse)
library(reshape2)
library(RColorBrewer)
library(ggrepel)
library(scales)
library(gridExtra)
library(grid)

tuesdata <- tidytuesdayR::tt_load('2021-10-12')


tuesdata$`seafood-and-fish-production-thousand-tonnes`->prod
colnames(prod)<-c("Entity","Code","Year","Pelagic","Crustaceans","Cephalopods","Demersal","Freshwater","Molluscs","Marine")

prod%>%
  select(-Code)%>%
  rowwise()%>%
  mutate(Total=sum(Pelagic,Crustaceans,Cephalopods,Demersal,Freshwater,Molluscs,Marine))->prod1
         
data.frame(prod1)%>%
  filter(Year==2013)%>%
  arrange(desc(Total))->countries

countries[c(4,14,15,16,20),]->c1
c1%>%
  select(Entity,Total)->c1
c1$Total<-round(c1$Total/1000)
c1 <-
  c1 %>% 
  mutate(
    color = case_when(
      Entity == "India" ~ "FF6C91",
    ))

#Top 5 countries plot
ggplot(c1,aes(Total,reorder(Entity,Total, decreasing=TRUE)))+geom_col(aes(fill=color),width=0.6)+
  scale_x_continuous(breaks=c(5000,10000,15000,20000,25000,30000,35000,40000,45000,
                              50000,55000,60000))+
  theme(axis.title = element_text(colour = "white"),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  labs(x="Production in thousand tonnes",y="")+
  theme(axis.line = element_line(colour="white"),
        plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        axis.text =element_text(colour="white",face="bold",size=14))+
  labs(title="INDIA IS THE FOURTH LARGEST PRODUCER OF SEAFOOD IN THE WORLD",
       subtitle = str_wrap("The below data visualization shows the top five countries that produced the highest amount of seafood (in thousand tonnes) in 2013. The total figure includes all varieties and types of seafood produced that year in the region",150))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(colour = "white",face="bold",size=21, hjust=0.5),
        plot.subtitle = element_text(colour = "white",size=11,hjust=0.5))->p1


#India total production line graph
prod1%>%
  filter(Entity=="India")%>%
  select(-Entity)%>%
  distinct(Year,.keep_all = TRUE)%>%
  drop_na()->India

India$Total<-round(India$Total/1000)

data.frame(India)%>%
  ggplot(aes(Year,Total,label=Total))+
  geom_line(colour="#FF6C91", linetype=1)+
  geom_point(colour="#FF6C91")+
  geom_label_repel(max.overlaps = 3)+
  theme(axis.title = element_text(colour = "white"),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  scale_x_continuous(breaks=c(1961,1971,1981,1991,2001,2011))+
  theme(axis.line = element_line(colour = "white"),
        plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        axis.text = element_text(colour = "white"))+
  labs(y="Production in thousand tonnes",x="")+
  labs(title="THE QUANTITY OF SEAFOOD PRODUCED IN INDIA HAS SURGED OVER THE YEARS")+
  theme(plot.title = element_text(colour = "white",face="bold",size=21, hjust=0.5))->p2



#India production by type stacked bar graph
India[-c(9)]->India1
melt(India1, id.vars = "Year",measure.vars=c("Pelagic","Crustaceans","Cephalopods","Demersal","Freshwater","Molluscs","Marine"),value.name="Value")->m11
m11$Value<-round(m11$Value/1000)

ggplot(m11,aes(Year,Value,fill=variable))+
  geom_col(width=0.6,colour="white")+
  theme(axis.title = element_text(colour = "white"),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  scale_x_continuous(breaks=c(1961,1971,1981,1991,2001,2011))+
  labs(y="Production in thousand tonnes",x="")+
  theme(axis.line = element_line(colour = "white"),
        plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        axis.text = element_text(colour = "white"),
        legend.text = element_text(colour = "white", face="bold"),
        legend.background = element_rect("black"),
        legend.title = element_text(colour="white",face="bold"),
        legend.position = "top")+
  guides(fill = guide_legend(nrow = 1))+
  labs(fill = "Types of seafood produced: ")+
  scale_fill_discrete(labels = c("Pelagic Fish", "Crustaceans","Cephalopods","Demersal","Freshwater",
                               "Molluscs","Marine"))+
  labs(title="FRESHWATER FISH IS PRODUCED THE MOST IN THE COUNTRY")+
  theme(plot.title = element_text(colour = "white",face="bold",size=21, hjust=0.5))->p3


#consumption line graph
tuesdata$`fish-and-seafood-consumption-per-capita`%>%
  filter(Year==2017)->a
colnames(a)<-c("Entity","Code","Year","Total")
data.frame(a)%>%
  arrange(desc(Total))


tuesdata$`fish-and-seafood-consumption-per-capita`%>%
  filter(Entity=="India")%>%
  select(-Code)->cons
colnames(cons)<-c("Entity","Year","Total")
cons%>%
  filter(Year==2017)

data.frame(cons)%>%
  ggplot(aes(Year,Total,label=Total))+
  geom_line(colour="#00C0AF", linetype=1)+
  geom_point(colour="#00C0AF")+
  geom_label_repel(max.overlaps = 1,size = 3)+
  theme(axis.title = element_text(colour = "white"),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  scale_x_continuous(breaks=c(1961,1971,1981,1991,2001,2011))+
  theme(axis.line = element_line(colour = "white"),
        plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        axis.text = element_text(colour = "white"))+
  labs(y="Consumption in kilogram per capita",x="")+
  labs(title="HOWEVER, INDIA DOESN'T CONSUME SEA FOOD AS MUCH",
       subtitle="It stands in the 128th spot among 172 countries in consumption, and consumes only ~6.9 kilograms/per capita of seafood in a year",
       caption="Data from Our World in Data|Design and analysis: @annapurani93")+
  theme(plot.title = element_text(colour = "white",face="bold",size=21, hjust=0.5),
        plot.subtitle = element_text(colour = "white",size=11,hjust=0.5),
        plot.caption = element_text(colour = "white",size=8))->p4


grid.arrange(p1,p2,p3,p4,pp, nrow=4)->pf2
ggsave("pf2.png",pf2,width=15,height=30)

