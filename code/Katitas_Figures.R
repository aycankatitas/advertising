###################
# title: Politicizing Trade: How Economic Discontent and Identity Politics Shape Anti-Trade Campaign Appeals
# author: Aycan Katitas
# date: 09/21/2022
# RStudio version: 2022.07.1
# Figures
###################


#### Working Directory 
setwd("~/Dropbox/jmp")

#### Libraries 
library(dplyr)
library(devtools)
library(congress)
library(tidyverse)
library(bea.R)
library(readstata13)
library(ggplot2)
library(lubridate)
library(rgdal)
library(raster)
library(sf)
library(ggrepel)

### FIGURE 1 - Anti-Trade Ads Over Time 
load("genhspads0016.Rdata")
load("genintads0016.Rdata")

tradehsp <- totalads0016 %>% 
  left_join(.,intads0016) %>% 
  group_by(year) %>% 
  summarise(across(totalad_D:tradeintad,sum,na.rm=T)) %>% 
  mutate_at(vars(ends_with("D")),list(per=~(./totalad_D)*100)) %>% 
  mutate_at(vars(ends_with("_R")),list(per=~(./totalad_R)*100)) %>% 
  dplyr::select(year,ends_with("per"),-totalad_D_per,-totalad_R_per) %>% 
  pivot_longer(cols=tradead_D_per:chinaad_R_per,
               names_to="group",
               values_to="share") %>% 
  separate(group,c("issue","party","name"),sep="_") %>% 
  dplyr::select(-name) %>% 
  mutate(party=ifelse(issue=="tradeintad","Interest Groups",
                      ifelse(party=="D","Democrats","Republicans")))

tradehsp$year <- as.Date(ISOdate(year = tradehsp$year, month = 1, day = 1))

tradehsp$party <- factor(tradehsp$party,levels=c("Interest Groups","Republicans","Democrats"))
## Create figure 


tradehsp %>% 
  filter(issue=="tradead"|issue=="tradeintad") %>% 
  mutate(label = if_else(year == max(year), as.character(party), NA_character_)) %>% 
  ggplot(aes(x=year,y=share,group=party,color=party)) +
  geom_line(size=4) + 
  scale_color_manual(values=c("black","red","blue"), guide="none") +
  ylab("The Proportion of Anti-Trade Ads (%)") +
  xlab("") + 
  geom_vline(xintercept=c(as.numeric(as.Date(c("2007-12-01"))),as.numeric(as.Date(c("2009-06-30")))), linetype="dashed",size=2) + 
  scale_x_date(breaks=seq(as.Date("2000-01-01"), as.Date("2016-01-01"), by = "2 years"),date_labels = "%Y") + 
  theme_classic(base_size=66) + 
  geom_text_repel(aes(label = label),
                   nudge_x = -1,
                  nudge_y=0.3,
                  segment.alpha = 0,
                   na.rm = TRUE,
                  size=18,
                  direction="y") + 
  theme(plot.margin = unit(c(1,6,1,1), "lines")) 
  
ggsave(plot=last_plot(),filename="tradeadoveryears.png",
       width = 45, height = 25, units = "in")

### FIGURE 2 - Trade Ad Map - 2008-2016 

# compare dmas 
dmas <- readRDS("base_dmacountysvw.Rdata")

cross <- read.csv("dmacross.csv") %>% 
  mutate(DMA=trimws(DMA))

dmas <- dmas %>% 
  dplyr::select(DMA) %>% distinct() %>% 
  mutate(DMA=trimws(DMA)) %>% 
  left_join(.,cross) %>% 
  rename(market=DMA,
         DMA=dmaid)

tradehsp_d <- read.dta13("trade_dma_cum_analysis.dta")

tradehsp_d <- tradehsp_d %>% 
  left_join(.,dmas) %>% 
  dplyr::select(DMA,manuloss7007_pw,manuloss7000_pw,manuloss0007_pw,per_trade)
  

shp <- readOGR("~/Dropbox/jmp/dma_2008", "DMAs", stringsAsFactors = F)

df = data.frame(market=shp$NAME,dma=shp$DMA)

shp@data$id = rownames(shp@data)

map <- ggplot() + geom_polygon(data = shp, aes(x = long, y = lat, group = group), colour = "black", fill = NA)


# 2008-2016 dmas ad

dmashp08 <- merge(shp, tradehsp_d, by='DMA')

dmashp082 <- fortify(dmashp08)

dmashp082_join = plyr::join(x = dmashp082,y = dmashp08@data, by="id") # join by id


ggplot() + 
  geom_polygon(data = dmashp082_join, aes(x = long, y = lat, group = group, fill = per_trade),
               colour = "black", size=0.2) +
  scale_fill_gradient2(low="white", high="brown1",
                       name="Share of Trade Ads", na.value="gray80",
                       breaks=c(min(dmashp082_join$per_trade,na.rm=TRUE),max(dmashp082_join$per_trade, na.rm=TRUE)),labels=c("Low","High")) + 
  theme_void() + coord_fixed(1) +
  theme(legend.title= element_text(size=10),
        legend.position = "bottom") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5),
         size = guide_legend(title.position="top", title.hjust = 0.5)) + 
  scale_shape_discrete(solid=F) 

ggsave("dmatradead0816hsp.jpeg")



### FIGURE 3 - Marginal Effect graph Manufacturing Loss + White Counties 
plot <- read.csv("whitedummarg.csv")

plot %>% 
  mutate(whitedum=ifelse(whitedum==0,"Minority White Counties","Majority White Counties")) %>% 
  ggplot(aes(x=manuloss7007_pw,y=est,ymin=confmin,ymax=confmax,fill=whitedum)) +
  geom_ribbon() + 
  geom_line(aes(linetype=whitedum),color="black",size=4) + 
  geom_hline(yintercept=0,linetype="dashed",size=2) +
  facet_wrap(~whitedum) +
  ylab("Predicted Share of Anti-Trade Ads (%)") +
  xlab("Decline in % Manufacturing Employment per Worker (1970-2007)") +
  theme_classic(base_size=66) +
  scale_linetype_manual(values=c("solid","solid")) + 
  scale_fill_manual(values=c("azure3","azure4")) + 
  geom_hline(yintercept=0,linetype="dotted",size=2) +
  guides(fill="none")  +
  theme(legend.position="none") + 
  scale_x_continuous(breaks=seq(min(plot$manuloss7007_pw),max(plot$manuloss7007_pw),20))
  
ggsave(plot=last_plot(),filename="figwhitemanucounty.png",
         width = 45, height = 25, units = "in")

## FIGURE 4 - DIFFERENT TIMING IN MANUFACTURING DECLINE 

df <- data.frame(x=c("1970-2000","1970-2016","2000-2007","2000-2016"),
                 y=c(1.693693,0.7665362,1.81928,1.492863),
                 min=c(0.5537152,0.3972519,1.122861,0.9330755),
                 max=c(2.833672,1.13582,2.515698,2.05265))
df %>% 
  ggplot(aes(x=x,y=y,ymin=min,ymax=max)) +
  geom_pointrange(size=4) +
  xlab("Manufacturing Employment Decline Period") + 
  ylab("Percentage Point (%100) Increase in Anti-Trade Ads") + 
  ylim(0,3) +
  theme_classic(base_size=66) 

ggsave(plot=last_plot(),filename="figdifftime.png",
       width = 45, height = 25, units = "in")

## FIGURE A - China, immigration, jobs ads 


otherads <- totalads0016 %>% 
  group_by(year) %>% 
  summarise(across(totalad:immad,sum,na.rm=T)) %>% 
  mutate_at(vars(ends_with("ad")),list(per=~(./totalad)*100)) %>%  
  dplyr::select(year,ends_with("per"),-totalad_per) %>% 
  pivot_longer(cols=tradead_per:immad_per,
               names_to="group",
               values_to="share") %>% 
  separate(group,c("issue","name"),sep="_") %>% 
  dplyr::select(-name) %>% 
  mutate(issue=ifelse(issue=="tradead","Trade Ads",
                      ifelse(issue=="jobsad","Jobs Ads",
                             ifelse(issue=="chinaad","China Ads","Immigration Ads"))))


otherads$year <- as.Date(ISOdate(year = otherads$year, month = 1, day = 1))

otherads$issue <- factor(otherads$issue,levels=c("Trade Ads","China Ads","Jobs Ads","Immigration Ads"))
  

otherads %>% 
  ggplot(aes(x=year,y=share,color=issue)) +
  facet_wrap(~issue, scales = "free") + 
  geom_line(size=4) + 
  scale_color_manual(values=c("blue","red","aquamarine3","darkgray"), guide="none") +
  ylab("The Proportion of Issue Ads (%)") +
  xlab("") + 
  geom_vline(xintercept=c(as.numeric(as.Date(c("2007-12-01"))),as.numeric(as.Date(c("2009-06-30")))), linetype="dashed",size=2) + 
  scale_x_date(breaks=seq(as.Date("2000-01-01"), as.Date("2016-01-01"), by = "2 years"),date_labels = "%Y") + 
  theme_classic(base_size=66)

ggsave(plot=last_plot(),filename="alladoveryears.png",
       width = 45, height = 25, units = "in")


tradehsp %>% 
  filter(issue=="jobsad") %>% 
  mutate(label = if_else(year == max(year), as.character(party), NA_character_)) %>% 
  ggplot(aes(x=year,y=share,group=party,color=party)) +
  geom_line(size=4) + 
  scale_color_manual(values=c("red","blue"), guide="none") +
  ylab("The Proportion of Anti-Trade Ads (%)") +
  xlab("") + 
  geom_vline(xintercept=c(as.numeric(as.Date(c("2007-12-01"))),as.numeric(as.Date(c("2009-06-30")))), linetype="dashed",size=2) + 
  scale_x_date(breaks=seq(as.Date("2000-01-01"), as.Date("2016-01-01"), by = "2 years"),date_labels = "%Y") + 
  theme_classic(base_size=66) + 
  geom_text_repel(aes(label = label),
                  nudge_x = -1,
                  nudge_y=0.3,
                  segment.alpha = 0,
                  na.rm = TRUE,
                  size=18,
                  direction="y") + 
  theme(plot.margin = unit(c(1,6,1,1), "lines")) 

ggsave(plot=last_plot(),filename="chinaadoveryears.png",
       width = 45, height = 25, units = "in")


## FIGURE A - MANUFACTURING JOB LOSS MAP 


ggplot() + 
  geom_polygon(data = dmashp082_join, aes(x = long, y = lat, group = group, fill = manuloss7007_pw),
               colour = "black", size=0.2) +
  scale_fill_gradient2(low="azure", mid="cornflowerblue", high="blue4",
                       name="Decline in Manufacturing Employment", na.value="gray80",
                       breaks=c(min(dmashp082_join$manuloss7007_pw,na.rm=TRUE),max(dmashp082_join$manuloss7007_pw, na.rm=TRUE)),labels=c("Low","High")) + 
  theme_void() + coord_fixed(1) +
  theme(legend.title= element_text(size=10),
        legend.position = "bottom") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5),
         size = guide_legend(title.position="top", title.hjust = 0.5)) + 
  scale_shape_discrete(solid=F) 

ggsave("dmamanuloss7007.jpeg")


ggplot() + 
  geom_polygon(data = dmashp082_join, aes(x = long, y = lat, group = group, fill = manuloss7000_pw),
               colour = "black", size=0.2) +
  scale_fill_gradient2(low="azure", mid="cornflowerblue", high="blue4",
                       name="Decline in Manufacturing Employment", na.value="gray80",
                       breaks=c(min(dmashp082_join$manuloss7000_pw,na.rm=TRUE),max(dmashp082_join$manuloss7000_pw, na.rm=TRUE)),labels=c("Low","High")) + 
  theme_void() + coord_fixed(1) +
  theme(legend.title= element_text(size=10),
        legend.position = "bottom") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5),
         size = guide_legend(title.position="top", title.hjust = 0.5)) + 
  scale_shape_discrete(solid=F) 

ggsave("dmamanuloss7000.jpeg")

ggplot() + 
  geom_polygon(data = dmashp082_join, aes(x = long, y = lat, group = group, fill = manuloss0007_pw),
               colour = "black", size=0.2) +
  scale_fill_gradient2(low="azure", mid="cornflowerblue", high="blue4",
                       name="Decline in Manufacturing Employment", na.value="gray80",
                       breaks=c(min(dmashp082_join$manuloss0007_pw,na.rm=TRUE),max(dmashp082_join$manuloss0007_pw, na.rm=TRUE)),labels=c("Low","High")) + 
  theme_void() + coord_fixed(1) +
  theme(legend.title= element_text(size=10),
        legend.position = "bottom") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5),
         size = guide_legend(title.position="top", title.hjust = 0.5)) + 
  scale_shape_discrete(solid=F) 

ggsave("dmamanuloss0007.jpeg")

## FIGURE A 
tradehsp_d <- read.dta13("trade_dma_cum_analysis.dta")


tradehsp_d %>% 
  ggplot(aes(d_ip_d9_com,bartik_iv_0007)) +
  geom_point(size=8) +
  ylab("Bartik Instrument 1970-2000") + 
  xlab("China Trade Shock Instrument") + 
  theme_classic(base_size=66) 

ggsave(plot=last_plot(),filename="scatterbartikadh.png",
       width = 25, height = 15, units = "in")

## FIGURE A 
tradehsp_d %>% 
  ggplot(aes(permanu70,manuloss7007_pw)) +
  geom_point(size=8) +
  ylab("Manufacturing Decline 1970-2007") + 
  xlab("% Manufacturing Employment 1970") + 
  theme_classic(base_size=66) 

ggsave(plot=last_plot(),filename="scattermanu70.png",
       width = 25, height = 15, units = "in")

## FIGURE A 
tradehsp_d %>% 
  ggplot(aes(bartik_iv_07,manuloss7007_pw)) +
  geom_point(size=8) +
  ylab("Manufacturing Decline 1970-2007") + 
  xlab("Bartik Instrument 1970-2007") + 
  theme_classic(base_size=66) 

ggsave(plot=last_plot(),filename="scatterbartikmanu7007.png",
       width = 25, height = 15, units = "in")



  
   
 