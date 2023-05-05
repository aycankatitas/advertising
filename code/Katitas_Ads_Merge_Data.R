###################
# title: Politicizing Trade: How Economic Discontent and Identity Politics Shape Anti-Trade Campaign Appeals
# Program Name: Creating Ad Data for Regression
# author: Aycan Katitas
# date: 08/03/2022
# RStudio version: 2022.07.1
###################

## Ran the whole code to fix the error November 1

#### Working Directory 
setwd("~/Dropbox/jmp")

library(stringr)
library(lubridate)
library(readstata13)
library(foreign)
library(tidyverse)
library(dplyr)
# MARKETS 



# READ IN AD FILES 2000 - 2016

## 2000 
## q3: 1-55 House 60 Senate 80 Issue 95 Presidential
## If sponsor is 80 q3 is automatically 80
ad00 <- read.dta("~/Dropbox/Ads0016/jmp2/ad2000.dta",convert.factors = FALSE)

## merge dmas 
cross_ctydma_year <- readRDS("base_dmacountysvw.Rdata")

cross_ctydma_year <- cross_ctydma_year %>%
  dplyr::select(DMA,STATE,year,DMAINDEX) %>% 
  mutate(dma_clean=DMA) %>% 
  mutate(DMA=gsub("\\s*\\([^\\)]+\\)","",DMA),
         DMA=gsub("+ - +", "-", DMA)) %>%  # removes whitespace before and after - 
  distinct() %>% 
  # there are two columbus markets, differentiate them 
  mutate(DMA=ifelse(DMA=="COLUMBUS"&STATE=="OH","COLUMBUS OHIO",
                    ifelse(DMA=="COLUMBUS"&STATE!="OH","COLUMBUS GA",DMA))) %>% 
  dplyr::select(-STATE)   %>% 
  distinct()

# Fix DMA names and merge accordingly 
ad00_dma <- ad00 %>%
  mutate(year="2000") %>% 
  rename(DMA=marketlo) %>% 
  filter(DMA!="") %>%
  # fix DMA names before merging
  mutate(DMA=ifelse(DMA=="ST LOUIS","ST. LOUIS",
                    ifelse(DMA=="MOBILE-PENSACOLA","MOBILE-PENSACOLA-FT. WALTON",
                           ifelse(DMA=="TAMPA-ST PETERSBURG-SARASOTA","TAMPA-ST. PETERSBURG-SARASOTA",
                                  ifelse(DMA=="WASHINGTON DC-HAGERSTOWN","WASHINGTON-HAGERSTOWN",
                                         ifelse(DMA=="BOSTON","BOSTON-MANCHESTER",
                                                ifelse(DMA=="WEST PALM BEACH-FT PIERCE","WEST PALM BEACH-FT. PIERCE",
                                                       ifelse(DMA=="DALLAS-FT WORTH","DALLAS-FT. WORTH",
                                                              ifelse(DMA=="JACKSONVILLE-BRUNSWICK","JACKSONVILLE",
                                                                     ifelse(DMA=="RALEIGH-DURHAM","RALEIGH-DURHAM-FAYETTEVILLE",
                                                                            ifelse(DMA=="PORTLAND OR","PORTLAND",
                                                                                   ifelse(DMA=="CLEVELAND","CLEVELAND-AKRON-CANTON",
                                                                                          ifelse(DMA=="MINNEAPOLIS-ST PAUL","MINNEAPOLIS-ST. PAUL",
                                                                                                 ifelse(DMA=="ROCHESTER NY","ROCHESTER",
                                                                                                        ifelse(DMA=="MIAMI-FT. LAUDERDAL"|DMA=="MIAMI-FT LAUDERDALE","MIAMI-FT. LAUDERDALE",DMA
                                                                                                               ))))))))))))))) %>% 
  left_join(.,cross_ctydma_year)

## Filter and code Congressional ads 

# q3 office - <60 House, 60 Senate, 70 Governor, 80 Issue, 99 Other
ad00_H <- ad00_dma %>% 
  filter(q3<60) %>% 
  rename(market=DMA) %>% 
  # code for party 
  mutate(partyalt=ifelse(q5==1,"Democrat",
                      ifelse(q5==2,"Republican",
                             ifelse(q5==3,"Independent",
                                    ifelse(q5==9,"Interest",NA)))))  %>% 
  mutate(party=ifelse(sponsor==41|sponsor==43|sponsor==45|sponsor==48|sponsor==50|
                        sponsor==52,"Republican",
                      ifelse(sponsor==42|sponsor==44|sponsor==46|sponsor==49|sponsor==51|sponsor==53,"Democrat",
                             ifelse(sponsor==47,"Independent",NA))),
         
  # code for sponsor of the ad 
         sponsors=ifelse(sponsor==41|sponsor==42|sponsor==47|sponsor==50|sponsor==51,"candcoord",
                         ifelse(sponsor==43|sponsor==44,"party",
                                ifelse(sponsor==45|sponsor==46|sponsor==52|sponsor==53,"interest",NA))))

ad00_S <- ad00_dma %>% 
  filter(q3==60) %>% 
  rename(market=DMA) %>% 
  mutate(partyalt=ifelse(q5==1,"Democrat",
                      ifelse(q5==2,"Republican",
                             ifelse(q5==3,"Independent",
                                    ifelse(q5==9,"Interest",NA)))))  %>% 
  mutate(party=ifelse(sponsor==21|sponsor==23|sponsor==25|sponsor==28|sponsor==30|sponsor==32,"Republican",
                      ifelse(sponsor==22|sponsor==24|sponsor==26|sponsor==29|sponsor==31|sponsor==33,"Democrat",
                             ifelse(sponsor==27,"Independent",NA))),
         sponsors=ifelse(sponsor==21|sponsor==22|sponsor==27|sponsor==30|sponsor==31,"candcoord",
                         ifelse(sponsor==23|sponsor==24,"party",
                                ifelse(sponsor==25|sponsor==26|sponsor==32|sponsor==33,"interest",NA))))

## 2002 
ad02 <- read.dta("../Ads0016/jmp2/ad2002.dta",convert.dates=TRUE,convert.factors = FALSE)

# create crosswalk for 2002 
dma_link <- dma_link<-read.csv(file="dma_link.csv") %>% 
  mutate(dma_clean=trimws(dma_clean))

cross_02 <- left_join(cross_ctydma_year,dma_link) %>%
  dplyr::select(year,dma_raw,dma_clean,DMAINDEX)

## fix market names for merge 
ad02_dma <- ad02 %>% 
  # springfield AR and jackson AL could not be matched
  mutate(market=tolower(market),
    market=ifelse(market=="charleston","charleston, sc",
                  ifelse(market=="jackson"&state=="MS","jackson, ms",
                         ifelse(market=="columbia"&state=="SC","columbia, sc",
                                ifelse(market=="greenville"&state=="SC","greenville, sc",
                                       ifelse(market=="greenville"&state=="NC","greenville, nc",
                                              ifelse(market=="columbus","columbus, oh", 
                                                     ifelse(market=="rochester","rochester, ny",
                                                            ifelse(market=="albany","albany, ny",
                                                                   ifelse(market=="springfield"&state=="MO","springfield, mo",market)))))))))) %>% 
  rename(dma_raw=market) %>% 
  mutate(year="2002") %>% 
  left_join(.,cross_02)

# Filter and Code congressional ads 
# office 1 Presidential (not in data) 2 Senate 3 Congressional (House) 4 Gubernatorial

ad02_H <- ad02_dma %>% 
  filter(office==3) %>%
  rename(market=dma_clean,
         party2=party) %>% 
  # code for party Libertarian and Green party are coded as independents 
  mutate(party=ifelse(party2=="D","Democrat",
                      ifelse(party2=="R","Republican",
                             ifelse(party2=="I"|party2=="G"|party2=="L","Independent",NA))),
         # code for sponsor of the ad 
         sponsors=ifelse(sponsor==1|sponsor==4,"candcoord",
                         ifelse(sponsor==2,"party",
                                ifelse(sponsor==3,"interest",NA))))

ad02_S <- ad02_dma %>% 
  filter(office==2) %>% 
  rename(market=dma_clean,
         party2=party) %>% 
  # code for party Libertarian and Green party are coded as independents 
  mutate(party=ifelse(party2=="D","Democrat",
                      ifelse(party2=="R","Republican",
                             ifelse(party2=="I"|party2=="G"|party2=="L","Independent",NA))),
         # code for sponsor of the ad 
         sponsors=ifelse(sponsor==1|sponsor==4,"candcoord",
                         ifelse(sponsor==2,"party",
                                ifelse(sponsor==3,"interest",NA))))

## 2004 

ad04 <- read.dta("../Ads0016/jmp2/federalad2004.dta",convert.dates=TRUE,convert.factors = FALSE)


ad04_dma <- ad04 %>% 
  mutate(market=tolower(market)) %>% 
  mutate(market=ifelse(market=="charleston"&fips==21|market=="charleston"&fips==39|market=="charleston"&fips==54,"charleston, wv",
                       ifelse(market=="jackson"&fips==28,"jackson, ms",
                              ifelse(market=="manchester, nh"&fips==33,"boston",
                                     ifelse(market=="greenville"&fips==45,"greenville, sc",
                                            ifelse(market=="greenville"&fips==37,"greenville, nc",
                                                   ifelse(market=="columbia","columbia, sc",
                                                          ifelse(market=="rochester","rochester, ny",
                                                                 ifelse(market=="albany","albany, ny",
                                                                        ifelse(market=="springfield","springfield, mo",
                                                                               ifelse(market=="columbus","columbus, oh",market))))))))))) %>% 
  
  rename(dma_raw=market) %>%
  mutate(year="2004")  %>% 
  left_join(.,cross_02) 

ad04_H <- ad04_dma %>% 
  filter(office==3) %>% 
  rename(market=dma_clean,
         party2=party) %>%
  # code for party
  mutate(party=ifelse(party2=="D","Democrat",
                      ifelse(party2=="R","Republican",
                             ifelse(party2=="I"|party2=="G"|party2=="L","Independent",NA)))) %>% 
  # code for source 
  mutate(sponsors=ifelse(sponsor==1|sponsor==4,"candcoord",
                         ifelse(sponsor==2,"party",
                                ifelse(sponsor==3,"interest",NA))))
  

ad04_S <- ad04_dma %>% 
  filter(office==2) %>% 
  rename(market=dma_clean,
         party2=party) %>%
  # code for party
  mutate(party=ifelse(party2=="D","Democrat",
                      ifelse(party2=="R","Republican",
                             ifelse(party2=="I"|party2=="G"|party2=="L","Independent",NA)))) %>% 
  # code for source 
  mutate(sponsors=ifelse(sponsor==1|sponsor==4,"candcoord",
                         ifelse(sponsor==2,"party",
                                ifelse(sponsor==3,"interest",NA))))

## 2006 
ad06 <- read.dta13("../Ads0016/jmp2/ad2006.dta",convert.dates=TRUE,convert.factors = FALSE)

ad06_dma <- ad06 %>% 
  mutate(market=tolower(market)) %>% 
  rename(dma_raw=market) %>% 
  mutate(year="2006") %>% 
  # no need to fix any names
  left_join(.,cross_02) 

ad06_H <- ad06_dma %>% 
  filter(race=="HOUSE") %>% 
  rename(market=dma_clean,
         party2=party) %>% 
  # party
  mutate(party=ifelse(party2==1,"Democrat",
                      ifelse(party2==2,"Republican",
                             ifelse(party2==3,"Independent",NA)))) %>% 
  # sponsor 
  mutate(sponsors=ifelse(sponsor==1|sponsor==3,"candcoord",
                         ifelse(sponsor==2,"party",
                                ifelse(sponsor==4,"interest",NA))))

ad06_S <- ad06_dma %>% 
  filter(race=="USSEN") %>% 
  rename(market=dma_clean,
         party2=party) %>% 
  # party
  mutate(party=ifelse(party2==1,"Democrat",
                      ifelse(party2==2,"Republican",
                             ifelse(party2==3,"Independent",NA)))) %>% 
  # sponsor 
  mutate(sponsors=ifelse(sponsor==1|sponsor==3,"candcoord",
                         ifelse(sponsor==2,"party",
                                ifelse(sponsor==4,"interest",NA))))

## 2008 - most problematic year 

ad08 <- read.dta13("../Ads0016/jmp2/federalad2008.dta",convert.dates=TRUE,convert.factors = FALSE)

ad08_dma <- ad08 %>% 
  mutate(market=tolower(market),
         statefips=str_extract(statdist, "^\\d{2}")) %>% 
  mutate(market=ifelse(market=="lafayette, i","lafayette, in",
                       ifelse(market=="springfield,"&statefips==29,"springfield, mo",
                              ifelse(market=="springfield,"&statefips==25,"springfield, ma",
                                     ifelse(market=="greenville,"&statefips==37,"greenville, nc",
                                            ifelse(market=="greenville,"&statefips==45,"greenville, sc",
                                                   ifelse(market=="bismarck-min","bismarck-minot",
                                                          ifelse(market=="manchester,"&statefips==33,"boston",
                                                                 ifelse(market=="salt lake ci","salt lake city",
                                                                        ifelse(market=="charleston,"&statefips==54|
                                                                                 market=="charleston,"&statefips==21,"charleston, wv",
                                                                               ifelse(market=="charleston,"&statefips==45,"charleston, sc",
                                                                                      ifelse(market=="wheeling-ste","wheeling-steubenville",
                                                                                             ifelse(market=="bluefield-be","bluefield-beckley",
                                                                                                    ifelse(market=="chico-reddin","chico-redding",
                                                                                                           ifelse(market=="san francisc","san francisco",
                                                                                                                  ifelse(market=="santa barbar","santa barbara",
                                                                                                                         ifelse(market=="palm springs","los angeles",
                                                                                                                                ifelse(market=="colorado spr","colorado springs",
                                                                                                                                       ifelse(market=="west palm be","west palm beach",
                                                                                                                                              ifelse(market=="rochester, m","rochester, mn",
                                                                                                                                                     ifelse(market=="bowling gree","bowling green",
                                                                                                                                                            ifelse(market=="lafayette, l","lafayette, la",
                                                                                                                                                                   ifelse(market=="washington d","washington dc",
                                                                                                                                                                          ifelse(market=="traverse cit","traverse city",
                                                                                                                                                                                 ifelse(market=="columbus/tup","columbus/tupelo",
                                                                                                                                                                                        ifelse(market=="jackson"&statefips==28,"jackson, ms",
                                                                                                                                                                                               ifelse(market=="hattiesburg-","hattiesburg-laurel",
                                                                                                                                                                                                      ifelse(market=="rochester, n","rochester, ny",
                                                                                                                                                                                                             ifelse(market=="oklahoma cit","oklahoma city",
                                                                                                                                                                                                                    ifelse(market=="wichita fall","wichita falls",
                                                                                                                                                                                                                           ifelse(market=="medford-klam","medford-klamath falls",
                                                                                                                                                                                                                                  ifelse(market=="myrtle beach","florence",
                                                                                                                                                                                                                                         ifelse(market=="corpus chris","corpus christi",
                                                                                                                                                                                                                                                ifelse(market=="charlottesvi","charlottesville",
                                                                                                                                                                                                                                                       ifelse(market=="idaho falls-","idaho falls-pocatello",
                                                                                                                                                                                                                                                              ifelse(market=="grand juncti","grand junction",
                                                                                                                                                                                                                                                                     ifelse(market=="lafayette","lafayette, la",
                                                                                                                                                                                                                                                                            ifelse(market=="odessa/midla","odessa/midland",
                                                                                                                                                                                                                                                                                   ifelse(market=="abilene-swee","abilene-sweetwater",market
                                                                                                                                                                                                                                                                                          ))))))))))))))))))))))))))))))))))))))) %>% 
  rename(dma_raw=market) %>% 
  mutate(year="2008")  %>%
  left_join(.,cross_02) 

# office 1 Senate 2 House  3 Governor
ad08_H <- ad08_dma %>% 
  filter(office==2) %>% 
  rename(market=dma_clean,
         party2=party) %>% 
  # party
  mutate(party=ifelse(party2==1,"Democrat",
                      ifelse(party2==2,"Republican",
                             ifelse(party2==3|party2==4|party2==5,"Independent",NA)))) %>% 
  # sponsor 
  mutate(sponsors=ifelse(sponsor==1|sponsor==4,"candcoord",
                         ifelse(sponsor==2,"party",
                                ifelse(sponsor==3,"interest",NA))))

ad08_S <- ad08_dma %>% 
  filter(office==1) %>% 
  rename(market=dma_clean,
         party2=party) %>% 
  # party
  mutate(party=ifelse(party2==1,"Democrat",
                      ifelse(party2==2,"Republican",
                             ifelse(party2==3|party2==4|party2==5,"Independent",NA)))) %>% 
  # sponsor 
  mutate(sponsors=ifelse(sponsor==1|sponsor==4,"candcoord",
                         ifelse(sponsor==2,"party",
                                ifelse(sponsor==3,"interest",NA))))
  
## 2010 
ad10 <- read.dta13("../Ads0016/jmp2/federalad2010.dta",convert.dates=TRUE,convert.factors = FALSE)

ad10_dma <- ad10 %>% 
  mutate(market=tolower(market)) %>% 
  mutate(market=ifelse(market=="palm springs","los angeles",
                       ifelse(market=="manchester, nh"&state=="NH","boston",
                              ifelse(market=="myrtle beach","florence",
                                     ifelse(market=="st. joseph","st. joseph wheeling-ste",market))))) %>% 
  rename(dma_raw=market) %>% 
  mutate(year="2010")  %>%
  left_join(.,cross_02)  
  
ad10_H <- ad10_dma %>% 
  filter(race=="US House") %>% 
  rename(market=dma_clean,
         party2=party) %>%  
  mutate(party=ifelse(party2=="Democrat","Democrat",
                      ifelse(party2=="Republican","Republican",
                             ifelse(party2=="Constituti"|party2=="Green"|party2=="Libertaria"|party2=="Other","Independent",NA)))) %>%
  mutate(sponsors=ifelse(sponsor==1|sponsor==3,"candcoord",
                         ifelse(sponsor==2,"party",
                                ifelse(sponsor==4,"interest",NA))))

ad10_S <- ad10_dma %>% 
  filter(race=="US Senate") %>% 
  rename(market=dma_clean,
         party2=party) %>%  
  mutate(party=ifelse(party2=="Democrat","Democrat",
                      ifelse(party2=="Republican","Republican",
                             ifelse(party2=="Constituti"|party2=="Green"|party2=="Libertaria"|party2=="Other","Independent",NA)))) %>%
  mutate(sponsors=ifelse(sponsor==1|sponsor==3,"candcoord",
                         ifelse(sponsor==2,"party",
                                ifelse(sponsor==4,"interest",NA))))
  
## 2012 

# House 
ad12h <- read.dta13("../Ads0016/jmp2/housead2012.dta",convert.dates=TRUE,convert.factors = FALSE)

ad12h_dma <- ad12h %>% 
  mutate(market=tolower(market)) %>% 
  mutate(market=ifelse(market=="palm springs","los angeles",
                       ifelse(market=="wheeling-steuben","wheeling-steubenville",
                              ifelse(market=="idaho falls-poca","idaho falls-pocatello",
                                     ifelse(market=="manchester, nh","boston",
                                            ifelse(market=="myrtle beach","florence",
                                                   ifelse(market=="medford-klamath","medford-klamath falls",
                                                          ifelse(market=="bluefield-beckle","bluefield-beckley",market
                                                                )))))))) %>% 
  rename(dma_raw=market) %>% 
  mutate(year="2012") %>% 
  left_join(.,cross_02) 

ad12_H <- ad12h_dma %>% 
  rename(market=dma_clean,
         party2=party) %>% 
  mutate(party=ifelse(party2=="DEMOCRAT","Democrat",
                      ifelse(party2=="REPUBLICAN","Republican",
                             ifelse(party2=="LIBERTARIA"|party2=="OTHER","Independent",NA)))) %>% 
  mutate(sponsors=ifelse(sponsor==1|sponsor==3,"candcoord",
                         ifelse(sponsor==2,"party",
                                ifelse(sponsor==4,"interest",NA))))
  
# Senate 
ad12s <- read.dta13("../Ads0016/jmp2/senatead2012.dta",convert.dates=TRUE,convert.factors=FALSE)


ad12s_dma <- ad12s %>% 
  mutate(market=tolower(market)) %>% 
  mutate(market=ifelse(market=="wheeling-steuben","wheeling-steubenville",
                       ifelse(market=="st. joseph","st. joseph wheeling-ste",
                              ifelse(market=="hattiesburg-laur","hattiesburg-laurel",
                                     ifelse(market=="abilene-sweetwat","abilene-sweetwater",
                                            ifelse(market=="bluefield-beckle","bluefield-beckley",market
                                                          ))))))  %>% 
rename(dma_raw=market) %>% 
  mutate(year="2012") %>% 
  left_join(.,cross_02) 

ad12_S <- ad12s_dma %>% 
  rename(market=dma_clean,
         party2=party) %>% 
  mutate(party=ifelse(party2=="DEMOCRAT","Democrat",
                      ifelse(party2=="REPUBLICAN","Republican",
                             ifelse(party2=="OTHER","Independent",NA)))) %>% 
  mutate(sponsors=ifelse(sponsor==1|sponsor==3,"candcoord",
                         ifelse(sponsor==2,"party",
                                ifelse(sponsor==4,"interest",NA))))

# 2014 

# House
ad14h <- read.dta13("../Ads0016/jmp2/housead2014.dta",convert.dates=TRUE,convert.factors = FALSE)

ad14h_dma <- ad14h %>% 
  mutate(market=tolower(market)) %>% 
  mutate(market=ifelse(market=="palm springs","los angeles",
                       ifelse(market=="manchester, nh","boston",market))) %>% 
  rename(dma_raw=market) %>% 
  mutate(year="2014") %>% 
  left_join(.,cross_02) 

ad14_H <- ad14h_dma %>% 
  rename(market=dma_clean) %>% 
  mutate(party=ifelse(affiliation=="DEMOCRAT","Democrat",
                      ifelse(affiliation=="REPUBLICAN","Republican",
                             ifelse(affiliation=="LIBERTARIA"|affiliation=="OTHER","Independent",NA)))) %>% 
  mutate(sponsors=ifelse(sponsor==1|sponsor==3,"candcoord",
                         ifelse(sponsor==2,"party",
                                ifelse(sponsor==4,"interest",NA))))

# Senate 
ad14s <- read.dta13("../Ads0016/jmp2/senatead2014.dta",convert.dates=TRUE,convert.factors = FALSE)

ad14s_dma <- ad14s %>% 
  mutate(market=tolower(market)) %>% 
  mutate(market=ifelse(market=="manchester, nh","boston",
                       ifelse(market=="myrtle beach","florence",market))) %>% 
  rename(dma_raw=market) %>% 
  mutate(year="2014") %>% 
  left_join(.,cross_02) 

ad14_S <- ad14s_dma %>% 
  rename(market=dma_clean) %>% 
  mutate(party=ifelse(affiliation=="DEMOCRAT","Democrat",
                      ifelse(affiliation=="REPUBLICAN","Republican",
                             ifelse(affiliation=="OTHER","Independent",NA)))) %>% 
  mutate(sponsors=ifelse(sponsor==1|sponsor==3,"candcoord",
                         ifelse(sponsor==2,"party",
                                ifelse(sponsor==4,"interest",NA))))

# 2016 
  
#House
ad16h <- read.dta13("../Ads0016/jmp2/housead2016.dta",convert.dates=TRUE,convert.factors = FALSE)

ad16h_dma <- ad16h %>% 
  mutate(market=tolower(market)) %>% 
  mutate(market=ifelse(market=="palm springs","los angeles",
                       ifelse(market=="manchester, nh","boston",market))) %>%
  rename(dma_raw=market) %>% 
  mutate(year="2016") %>% 
  left_join(.,cross_02) 

ad16_H <- ad16h_dma %>% 
  rename(market=dma_clean,
         party2=party) %>% 
  mutate(party=ifelse(party2=="DEMOCRAT","Democrat",
                      ifelse(party2=="REPUBLICAN","Republican",
                             ifelse(party2=="OTHER","Independent",NA)))) %>% 
  mutate(sponsors=ifelse(sponsor==1|sponsor==3,"candcoord",
                         ifelse(sponsor==2,"party",
                                ifelse(sponsor==4,"interest",NA))))

# Senate 
ad16s <- read.dta13("../Ads0016/jmp2/senatead2016.dta",convert.dates=TRUE,convert.factors = FALSE)

ad16s_dma <- ad16s %>% 
  mutate(market=tolower(market)) %>% 
  mutate(market=ifelse(market=="palm springs","los angeles",
                       ifelse(market=="manchester, nh","boston",
                              ifelse(market=="st. joseph","st. joseph wheeling-ste",
                                     ifelse(market=="myrtle beach","florence",market))))) %>% 
  rename(dma_raw=market) %>% 
  mutate(year="2016") %>% 
  left_join(.,cross_02) 

ad16_S <- ad16s_dma %>% 
  rename(market=dma_clean,
         party2=party) %>% 
  mutate(party=ifelse(party2=="DEMOCRAT","Democrat",
                      ifelse(party2=="REPUBLICAN","Republican",
                             ifelse(party2=="OTHER"|party2=="LIBERTARIAN","Independent",NA)))) %>% 
  mutate(sponsors=ifelse(sponsor==1|sponsor==3,"candcoord",
                         ifelse(sponsor==2,"party",
                                ifelse(sponsor==4,"interest",NA))))
  
save(ad00_H,ad00_S,
     ad02_H,ad02_S,
     ad04_H,ad04_S,
     ad06_H,ad06_S,
     ad08_H,ad08_S,
     ad10_H,ad10_S,
     ad12_H,ad12_S,
     ad14_H,ad14_S,
     ad16_H,ad16_S,file="federalads0016.Rdata")

#load("federalads0016.Rdata")
 
### PRESIDENTIAL ELECTION ADS 

# 2000
ad00P <- read.dta13("../Ads0016/jmp2/presad2000.dta",convert.dates=TRUE,convert.factors=FALSE)

ad00P_dma <- ad00P %>%
  mutate(year="2000") %>% 
  rename(DMA=marketlo) %>% 
  filter(DMA!="") %>%
  mutate(DMA=ifelse(DMA=="BOSTON","BOSTON-MANCHESTER",
                    ifelse(DMA=="TAMPA-ST PETERSBURG-SARASOTA","TAMPA-ST. PETERSBURG-SARASOTA",
                           ifelse(DMA=="WASHINGTON DC-HAGERSTOWN","WASHINGTON-HAGERSTOWN",
                                  ifelse(DMA=="CLEVELAND","CLEVELAND-AKRON-CANTON",
                                         ifelse(DMA=="MINNEAPOLIS-ST PAUL","MINNEAPOLIS-ST. PAUL",
                                                ifelse(DMA=="ST LOUIS","ST. LOUIS",
                                                       ifelse(DMA=="ROCHESTER NY","ROCHESTER",
                                                                         ifelse(DMA=="PORTLAND OR","PORTLAND",
                                                                                ifelse(DMA=="WEST PALM BEACH-FT PIERCE","WEST PALM BEACH-FT. PIERCE",
                                                                                       ifelse(DMA=="MIAMI-FT. LAUDERDAL"|DMA=="MIAMI-FT LAUDERDALE","MIAMI-FT. LAUDERDALE",
                                                                                              ifelse(DMA=="JACKSONVILLE-BRUNSWICK","JACKSONVILLE",
                                                                                                     ifelse(DMA=="MOBILE-PENSACOLA","MOBILE-PENSACOLA-FT. WALTON",
                                                                                                            ifelse(DMA=="DALLAS-FT WORTH","DALLAS-FT. WORTH",
                                                                                                                   ifelse(DMA=="RALEIGH-DURHAM","RALEIGH-DURHAM-FAYETTEVILLE",DMA
                                                                                                                          ))))))))))))))) %>% 
  left_join(.,cross_ctydma_year) 

ad00_P <- ad00P_dma %>% 
  rename(market=DMA) %>% 
  mutate(party=ifelse(sponsor==1|sponsor==3|sponsor==5|sponsor==7|sponsor==9|
                        sponsor==13|sponsor==15|sponsor==17,"Republican",
                      ifelse(sponsor==2|sponsor==4|sponsor==6|sponsor==8|sponsor==14|sponsor==16|
                               sponsor==18,"Democrat",
                             ifelse(sponsor==10|sponsor==11|sponsor==12,"Independent",NA))),
         sponsors=ifelse(sponsor==1|sponsor==2|sponsor==7|sponsor==8|sponsor==9|sponsor==10|sponsor==11|sponsor==12|
                           sponsor==15|sponsor==16,"candcoord",
                         ifelse(sponsor==3|sponsor==4,"party",
                                ifelse(sponsor==5|sponsor==6|sponsor==17|sponsor==18,"interest",NA)))) 

# 2004 
ad04P <- read.dta13("../Ads0016/jmp2/presad2004.dta",convert.dates=TRUE,convert.factors=FALSE)


ad04P_dma <- ad04P %>% 
  mutate(market=tolower(market)) %>% 
  mutate(market=ifelse(market=="manchester, nh"&fips==33,"boston",
                       ifelse(market=="columbia","columbia, sc",
                              ifelse(market=="greenville"&fips==45,"greenville, sc",
                                     ifelse(market=="greenville"&fips==37,"greenville, nc",
                                            ifelse(market=="charleston"&fips==21|market=="charleston"&fips==39|market=="charleston"&fips==54,"charleston, wv",
                                                   ifelse(market=="columbus","columbus, oh",
                                                          ifelse(market=="springfield","springfield, mo",
                                                                 ifelse(market=="albany","albany, ny",
                                                                        ifelse(market=="rochester","rochester, ny",
                                                                               ifelse(market=="colorado spring","colorado springs",market
                                                                               ))))))))))) %>% 
  rename(dma_raw=market) %>%
  mutate(year="2004")  %>% 
  left_join(.,cross_02) 

ad04_P <- ad04P_dma %>%
  rename(market=dma_clean) %>% 
  mutate(party=ifelse(Party==0|Party==3,"Independent",
                      ifelse(Party==1,"Democrat",
                             ifelse(Party==2,"Republican",NA))),
         sponsors=ifelse(Sponsor==1|Sponsor==4,"candcoord",
                         ifelse(Sponsor==2,"party",
                                ifelse(Sponsor==3,"interest",NA))))

# 2008 
ad08P <- read.dta13("../Ads0016/jmp2/presad2008.dta",convert.dates=TRUE,convert.factors=FALSE)

ad08P_dma <- ad08P %>%
  mutate(market=tolower(market)) %>% 
  mutate(market=ifelse(market=="manchester, nh","boston",
                       ifelse(market=="west palm beac","west palm beach",
                              ifelse(market=="myrtle beach","florence",
                                     ifelse(market=="columbia","columbia, sc",
                                            ifelse(market=="springfield, m","springfield, mo",
                                                   ifelse(market=="palm springs","los angeles",
                                                          ifelse(market=="colorado sprin","colorado springs",
                                                                 ifelse(market=="rochester","rochester, ny",
                                                                        ifelse(market=="lafayette","lafayette, la",
                                                                               ifelse(market=="jackson","jackson, ms",
                                                                                      ifelse(market=="columbus/tupel","columbus/tupelo",
                                                                                             ifelse(market=="medford-klamat","medford-klamath falls",
                                                                                                    ifelse(market=="wheeling-steub","wheeling-steubenville",
                                                                                                           ifelse(market=="charlottesvill","charlottesville",
                                                                                                                  ifelse(market=="idaho falls-po","idaho falls-pocatello",
                                                                                                                         ifelse(market=="bluefield-beck","bluefield-beckley",
                                                                                                                                ifelse(market=="abilene-sweetw","abilene-sweetwater",
                                                                                                                                       ifelse(market=="hattiesburg-la","hattiesburg-laurel",market
                                                                                                                                       ))))))))))))))))))) %>% 
  rename(dma_raw=market) %>% 
  mutate(year="2008")  %>%
  left_join(.,cross_02) 

ad08_P <- ad08P_dma %>% 
  rename(market=dma_clean,
         party2=party) %>% 
  mutate(party=ifelse(party2==1,"Democrat",
                      ifelse(party2==2,"Republican",NA)),
         sponsors=ifelse(sponsor==1|sponsor==4,"candcoord",
                         ifelse(sponsor==2,"party",
                                ifelse(sponsor==3,"interest",NA)))) 

# 2012 
ad12P <- read.dta13("../Ads0016/jmp2/presad2012.dta",convert.dates=TRUE,convert.factors=FALSE)

ad12P_dma <- ad12P %>% 
  mutate(market=tolower(market)) %>%
  mutate(market=ifelse(market=="manchester, nh","boston",
                       ifelse(market=="wheeling-steuben","wheeling-steubenville",
                              ifelse(market=="myrtle beach","florence",
                                     ifelse(market=="hattiesburg-laur","hattiesburg-laurel",
                                            ifelse(market=="idaho falls-poca","idaho falls-pocatello",
                                                   ifelse(market=="bluefield-beckle","bluefield-beckley",
                                                          ifelse(market=="abilene-sweetwat","abilene-sweetwater",market)))))))) %>% 
  
  rename(dma_raw=market) %>% 
  mutate(year="2012") %>% 
  left_join(.,cross_02) 

ad12_P <- ad12P_dma %>%
  rename(market=dma_clean) %>%
  mutate(party=ifelse(affiliation=="DEMOCRAT","Democrat",
                      ifelse(affiliation=="REPUBLICAN","Republican",
                             ifelse(affiliation=="OTHER","Independent",NA))),
         sponsors=ifelse(sponsorwmp==1|sponsorwmp==3,"candcoord",
                         ifelse(sponsorwmp==2,"party",
                                ifelse(sponsorwmp==4,"interest",NA))))
# 2016 
ad16P <- read.dta13("../Ads0016/jmp2/presad2016.dta",convert.dates=TRUE,convert.factors=FALSE)

ad16P_dma <- ad16P %>%
  mutate(market=tolower(market)) %>% 
  mutate(market=ifelse(market=="palm springs","los angeles",
                       ifelse(market=="manchester, nh","boston",
                              ifelse(market=="myrtle beach","florence",
                                     ifelse(market=="st. joseph","st. joseph wheeling-ste",market))))) %>% 
  rename(dma_raw=market) %>% 
  mutate(year="2016") %>% 
  left_join(.,cross_02)

ad16_P <- ad16P_dma %>% 
  rename(market=dma_clean,
         party2=party) %>% 
  mutate(party=ifelse(party2=="DEMOCRAT","Democrat",
                      ifelse(party2=="REPUBLICAN","Republican",
                             ifelse(party2=="GREEN"|party2=="LIBERTARIAN"|party2=="OTHER","Independent",NA))),
         sponsors=ifelse(sponsor==1|sponsor==3,"candcoord",
                         ifelse(sponsor==2,"party",
                                ifelse(sponsor==4,"interest",NA))))
  

## Put together presidential ads 

save(ad00_P,
     ad04_P,
     ad04_P,
     ad12_P,
     ad16_P,file="presads0016.Rdata")

### AD CONTENT CODINGS 
#load("federalads0016.Rdata")

# 2000
# House 
ad00_H_con <- ad00_H

vars <- c("q32","q33","q34","q35")
ad00_H_con[vars][is.na(ad00_H_con[vars])] <- 99

ad00_H_con <- ad00_H_con %>%
  mutate(trade=ifelse(q32==18|q33==18|q34==18|q35==18,1,0),
         china=ifelse(q32==55|q33==55|q34==55|q35==55,1,0),
         imm=ifelse(q32==71|q32==71|q32==71|q32==71,1,0),
         jobs=ifelse(q32==16|q33==16|q34==16|q35==16,1,0)) %>% 
  # get rid of primaries 
  filter(q4==2) %>% 
  mutate(last60=ifelse(spotdate>"2000-09-07",1,0),
         name=custitle,
         tone=ifelse(q14==1,"Attack",
                     ifelse(q14==2,"Contrast",
                            ifelse(q14==3,"Promote",NA)))) %>%
  separate(name,c("state","surname1"),sep="/",extra="merge") %>% 
  separate(surname1,c("surname","name"),extra="merge") %>% 
  mutate(surname=toupper(surname),
         office="house",
         adname=custitle,
         district=q3,
         airdate=spotdate,
         statefips=statcode) %>% 
  dplyr::select(year,office,market,DMAINDEX,statefips,district,adname,surname,airdate,len,cost,sponsors,party,
                tone,trade,china,jobs,imm,last60)

# Senate 
ad00_S_con <- ad00_S

vars <- c("q32","q33","q34","q35")
ad00_S_con[vars][is.na(ad00_S_con[vars])] <- 99

ad00_S_con <- ad00_S_con %>%
  mutate(trade=ifelse(q32==18|q33==18|q34==18|q35==18,1,0),
         china=ifelse(q32==55|q33==55|q34==55|q35==55,1,0),
         imm=ifelse(q32==71|q32==71|q32==71|q32==71,1,0),
         jobs=ifelse(q32==16|q33==16|q34==16|q35==16,1,0)) %>% 
  # get rid of primaries 
  filter(q4==2) %>% 
  mutate(last60=ifelse(spotdate>"2000-09-07",1,0),
         name=custitle,
         tone=ifelse(q14==1,"Attack",
                     ifelse(q14==2,"Contrast",
                            ifelse(q14==3,"Promote",NA)))) %>%
  separate(name,c("state","surname1"),sep="/",extra="merge") %>% 
  separate(surname1,c("surname","name"),extra="merge") %>% 
  mutate(surname=toupper(surname),
         office="senate",
         adname=custitle,
         district=q3,
         airdate=spotdate,
         statefips=statcode) %>% 
  dplyr::select(year,office,market,DMAINDEX,statefips,adname,surname,airdate,len,cost,sponsors,party,
                tone,trade,china,jobs,imm,last60)       

# 2002 
# House 
ad02_H_con <- ad02_H

ad02_H_con <- ad02_H_con %>% 
  # doesn't have primary indicator
  mutate(trade=ifelse(ISSUE1==19|ISSUE2==19|ISSUE3==19|ISSUE4==19,1,0),
         china=ifelse(ISSUE1==57|ISSUE2==57|ISSUE3==57|ISSUE4==57,1,0),
         jobs=ifelse(ISSUE1==17|ISSUE2==17|ISSUE3==17|ISSUE4==17,1,0),
         imm = ifelse(ISSUE1==71|ISSUE2==71|ISSUE3==71|ISSUE4==71,1,0),
         tone=ifelse(AD_TONE==1,"Promote",
                     ifelse(AD_TONE==2,"Attack",
                            ifelse(AD_TONE==3,"Contrast",NA))),
         office="house") %>% 
  mutate(last60=ifelse(airdate>"2002-09-05",1,0)) %>% 
  rename(surname=SUR_NAME,
         cost=EST_COST,
         statefips=fips,
         adname=creative,
         len=spotleng) %>%
  dplyr::select(year,office,market,DMAINDEX,statefips,district,adname,surname,airdate,len,cost,sponsors,party,
                tone,trade,china,jobs,imm,last60)

# Senate 
ad02_S_con <- ad02_S 

ad02_S_con <- ad02_S_con %>% 
  # doesn't have primary indicator
  mutate(trade=ifelse(ISSUE1==19|ISSUE2==19|ISSUE3==19|ISSUE4==19,1,0),
         china=ifelse(ISSUE1==57|ISSUE2==57|ISSUE3==57|ISSUE4==57,1,0),
         jobs=ifelse(ISSUE1==17|ISSUE2==17|ISSUE3==17|ISSUE4==17,1,0),
         imm = ifelse(ISSUE1==71|ISSUE2==71|ISSUE3==71|ISSUE4==71,1,0),
         tone=ifelse(AD_TONE==1,"Promote",
                     ifelse(AD_TONE==2,"Attack",
                            ifelse(AD_TONE==3,"Contrast",NA))),
         office="senate") %>% 
  mutate(last60=ifelse(airdate>"2002-09-05",1,0)) %>% 
  rename(surname=SUR_NAME,
         cost=EST_COST,
         statefips=fips,
         adname=creative,
         len=spotleng) %>%
  dplyr::select(year,office,market,DMAINDEX,statefips,adname,surname,airdate,len,cost,sponsors,party,
                tone,trade,china,jobs,imm,last60)

# 2004 
# House 

ad04_H_con <- ad04_H

vars <- c("eissue1","eissue2","eissue3","eissue4")
ad04_H_con[vars][is.na(ad04_H_con[vars])] <- 0

ad04_H_con <- ad04_H_con %>% 
  # doesn't have primary indicator
  # create district variable 
  mutate(statdist=sprintf("%04d", statdist),
    district = as.integer(substr(statdist,3,4)),
    trade=ifelse(eissue1==18|eissue2==18|eissue3==18|eissue4==18,1,0),
    china=ifelse(eissue1==55|eissue2==55|eissue3==55|eissue4==55,1,0),
    jobs=ifelse(eissue1==16|eissue2==16|eissue3==16|eissue4==16,1,0),
    imm=ifelse(eissue1==71|eissue2==71|eissue3==71|eissue4==71,1,0),
    tone=ifelse(ead_tone==1,"Attack",
                ifelse(ead_tone==2,"Contrast",
                       ifelse(ead_tone==3,"Promote",NA)))) %>% 
    separate(cand_id,c("state","name"),sep="/",extra="merge") %>% 
      separate(name,c("surname","rest"),extra="merge") %>% 
      mutate(district=ifelse(state=="HI"&district==5&surname=="ABERCROMBIE",1,district),
             party=ifelse(state=="TX"&district==4&surname=="HALL","Republican",party)) %>% 
  mutate(last60=ifelse(date>"2004-09-02",1,0),
         office="house") %>% 
  rename(airdate=date,
         cost=est_cost,
         statefips=fips,
         adname=creative,
         len=spotleng) %>% 
  dplyr::select(year,office,market,DMAINDEX,statefips,district,adname,surname,airdate,len,cost,sponsors,party,
                tone,trade,china,jobs,imm,last60)
  
# Senate 
ad04_S_con <- ad04_S

vars <- c("eissue1","eissue2","eissue3","eissue4")
ad04_S_con[vars][is.na(ad04_S_con[vars])] <- 0

ad04_S_con <- ad04_S_con %>% 
  # doesn't have primary indicator
  mutate(trade=ifelse(eissue1==18|eissue2==18|eissue3==18|eissue4==18,1,0),
         china=ifelse(eissue1==55|eissue2==55|eissue3==55|eissue4==55,1,0),
         jobs=ifelse(eissue1==16|eissue2==16|eissue3==16|eissue4==16,1,0),
         imm=ifelse(eissue1==71|eissue2==71|eissue3==71|eissue4==71,1,0),
         tone=ifelse(ead_tone==1,"Attack",
                     ifelse(ead_tone==2,"Contrast",
                            ifelse(ead_tone==3,"Promote",NA)))) %>% 
  separate(cand_id,c("state","name"),sep="/",extra="merge") %>% 
  separate(name,c("surname","rest"),extra="merge") %>% 
  mutate(last60=ifelse(date>"2004-09-02",1,0),
         office="senate") %>% 
  rename(airdate=date,
         cost=est_cost,
         statefips=fips,
         adname=creative,
         len=spotleng) %>% 
  dplyr::select(year,office,market,DMAINDEX,statefips,adname,surname,airdate,len,cost,sponsors,party,
                tone,trade,china,jobs,imm,last60)

# 2006 
# House 

ad06_H_con <- ad06_H 

ad06_H_con <- ad06_H_con
vars <- c("issue53","issue12","issue20","issue65","issue66","issue67","issue70","issue71","issue72","issue69","issue68","issue13")
ad06_H_con[vars][is.na(ad06_H_con[vars])] <- 0
delpar <- "&RNC|&NRCC|NCDP&|RNC&"

# cross for state fips state abbs 

cross_stfipsab <- read.csv("stateabfips.csv") 

ad06_H_con <- ad06_H_con %>%
  mutate(creative2=tolower(creative),
         issue972=tolower(issue97_txt),
    trade=ifelse(issue20==1,1,0),
         china=ifelse(issue65==1,1,0),
         jobs=ifelse(issue18==1,1,0),
         imm=ifelse(grepl("immig",creative2)|grepl("border",creative2)|grepl("immig",issue972)|grepl("border",issue972),1,0),
         tone=ifelse(ad_tone==1,"Contrast",
                     ifelse(ad_tone==2,"Promote",
                            ifelse(ad_tone==3,"Attack",NA))),
         cost=NA,
         district=as.numeric(district),
         office="house") %>% 
  separate(creative2,c("race","rest"),extra="merge",sep="/") %>% 
  separate(rest,c("state2","surname","rest"),extra="merge",sep=" ") %>% 
  mutate(surname=toupper(surname)) %>% 
  mutate(surname=gsub(delpar,"",surname))  %>% 
  mutate(party=ifelse(categorystate=="AZ"&district==8&surname=="GRAF","Republican",
                      ifelse(categorystate=="CO"&district==5&surname=="FAWCETT","Democrat",
                             ifelse(categorystate=="ID"&district==1&surname=="SALI","Republican",
                                    ifelse(categorystate=="IL"&district==14&surname=="LAESCH","Democrat",
                                           ifelse(categorystate=="IL"&district==15&surname=="GILL","Democrat",
                                                  ifelse(categorystate=="IN"&district==8&surname=="ELLSWORTH","Democrat",
                                                         ifelse(categorystate=="KS"&district==1&surname=="MORAN","Republican",
                                                                ifelse(categorystate=="MI"&district==4&surname=="CAMP","Republican",
                                                                       ifelse(categorystate=="MN"&district==1&surname=="GUTKNECHT","Republican",
                                                                              ifelse(categorystate=="MN"&district==2&surname=="ROWLEY","Democrat",
                                                                                     ifelse(categorystate=="NC"&district==11&surname=="TAYLOR","Republican",
                                                                                            ifelse(categorystate=="NM"&district==2&surname=="PEARCE","Republican",
                                                                                                   ifelse(categorystate=="NY"&district==19&surname=="KELLY","Republican",
                                                                                                          ifelse(categorystate=="NY"&district==23&surname=="JOHNSON","Democrat",
                                                                                                                 ifelse(categorystate=="NY"&district==26&surname=="REYNOLDS","Republican",
                                                                                                                        ifelse(categorystate=="OH"&district==1&surname=="CHABOT","Republican",
                                                                                                                               ifelse(categorystate=="OH"&district==1&surname=="CRANLEY","Democrat",
                                                                                                                                      ifelse(categorystate=="OH"&district==7&surname=="HOBSON","Republican",
                                                                                                                                             ifelse(categorystate=="OK"&district==3&surname=="BARTON","Democrat",
                                                                                                                                                    ifelse(categorystate=="PA"&district==12&surname=="IREY","Republican",
                                                                                                                                                           ifelse(categorystate=="PA"&district==18&surname=="MURPHY","Republican",
                                                                                                                                                                  ifelse(categorystate=="PA"&district==3&surname=="PORTER","Republican",
                                                                                                                                                                         ifelse(categorystate=="TN"&district==7&surname=="MORRISON","Democrat",
                                                                                                                                                                                ifelse(categorystate=="VA"&district==11&surname=="DAVIS","Republican",
                                                                                                                                                                                       ifelse(categorystate=="VA"&district==2&surname=="KELLAM","Democrat",
                                                                                                                                                                                              ifelse(categorystate=="VT"&district==1&surname=="WELCH","Democrat",
                                                                                                                                                                                                     ifelse(categorystate=="WA"&district==5&surname=="GOLDMARK","Democrat",
                                                                                                                                                                                                            ifelse(categorystate=="WA"&district==8&surname=="REICHERT","Republican",
                                                                                                                                                                                                                   ifelse(categorystate=="WI"&district==5&surname=="KENNEDY","Democrat",party
                                                                                                                                                                                                                          )))))))))))))))))))))))))))))) %>% 
  rename(adname=creative,
         len=l,
         ) %>% 
  # merge state fips 
  rename(state=categorystate) %>% 
  left_join(.,cross_stfipsab)  %>% 
  mutate(last60=ifelse(airdate>"2006-09-07",1,0)) %>% 
  dplyr::select(year,office,market,DMAINDEX,statefips,district,adname,surname,airdate,len,cost,sponsors,party,
                tone,trade,china,jobs,imm,last60)

# Senate 

ad06_S_con <- ad06_S 

ad06_S_con <- ad06_S_con
vars <- c("issue53","issue12","issue20","issue65","issue66","issue67","issue70","issue71","issue72","issue69","issue68","issue13")
ad06_S_con[vars][is.na(ad06_S_con[vars])] <- 0
delpar <- "&RNC|&NRCC|NCDP&|RNC&"

ad06_S_con <- ad06_S_con %>%
  mutate(creative2=tolower(creative),
         issue972=tolower(issue97_txt),
         trade=ifelse(issue20==1,1,0),
         china=ifelse(issue65==1,1,0),
         jobs=ifelse(issue18==1,1,0),
         imm=ifelse(grepl("immig",creative2)|grepl("border",creative2)|grepl("immig",issue972)|grepl("border",issue972),1,0),
         tone=ifelse(ad_tone==1,"Contrast",
                     ifelse(ad_tone==2,"Promote",
                            ifelse(ad_tone==3,"Attack",NA))),
         cost=NA,
         district=as.numeric(district),
         office="senate") %>% 
  separate(creative2,c("race","rest"),extra="merge",sep="/") %>% 
  separate(rest,c("state2","surname","rest"),extra="merge",sep=" ") %>% 
  mutate(surname=toupper(surname)) %>% 
  mutate(surname=gsub(delpar,"",surname)) %>% 
  rename(adname=creative,
         len=l,
  ) %>% 
  # merge state fips 
  rename(state=categorystate) %>% 
  left_join(.,cross_stfipsab)  %>% 
  mutate(last60=ifelse(airdate>"2006-09-07",1,0)) %>% 
  dplyr::select(year,office,market,DMAINDEX,statefips,adname,surname,airdate,len,cost,sponsors,party,
                tone,trade,china,jobs,imm,last60)

# 2008 

# House 
ad08_H_con <- ad08_H

vars <- c("terror","hcare","recess","trade","china","mideast","iran","afghan","govspend","jobs")
ad08_H_con[vars][is.na(ad08_H_con[vars])] <- 0

stateab <- read.csv("stateabfips.csv") %>% 
  dplyr::select(statefips,state)

ad08_H_con <- ad08_H_con %>% 
  mutate(cre=creative) %>%
  mutate(airdate=mdy(date)) %>% 
  separate(cre,c("race","statdist","name"), extra="merge") %>% 
  mutate(office="house",
    statefips=substr(statdist,1,2),
         district=as.numeric(substr(statdist,3,4))) %>%
  rename(state=statefips) %>% 
  left_join(.,stateab) %>% 
  dplyr::select(-state) %>% 
  mutate(creative2=tolower(creative),
    imm=ifelse(grepl("immig",creative2)|grepl("border",creative2),1,0)) %>% 
  rename(cost=EST_SPENDING,
         len=l,
         adname=creative) %>% 
  separate(CAND_ID,c("state","name"),extra="merge",sep="/") %>% 
  separate(name,c("surname","name"),extra="merge") %>% 
  mutate(party=ifelse(state=="CA"&district==17&surname=="TAYLOR","Republican",
                      ifelse(state=="FL"&district==1&surname=="MAHONEY","Democrat",
                             ifelse(state=="IN"&district==6&surname=="PENCE","Republican",
                                    ifelse(state=="PA"&district==8&surname=="MURPHY","Democrat",party)))),
         tone=ifelse(AD_TONE==1,"Contrast",
                     ifelse(AD_TONE==2,"Promote",
                            ifelse(AD_TONE==3,"Attack",NA)))) %>% 
  mutate(last60=ifelse(airdate>"2008-09-04",1,0)) %>% 
  dplyr::select(year,office,market,DMAINDEX,statefips,district,adname,surname,airdate,len,cost,sponsors,party,
                tone,trade,china,jobs,imm,last60)

# Senate 
ad08_S_con <- ad08_S

vars <- c("terror","hcare","recess","trade","china","mideast","iran","afghan","govspend","jobs")
ad08_S_con[vars][is.na(ad08_S_con[vars])] <- 0

ad08_S_con <- ad08_S_con %>% 
  mutate(cre=creative) %>%
  mutate(airdate=mdy(date)) %>% 
  separate(cre,c("race","statdist","name"), extra="merge") %>% 
  mutate(office="senate",
         statefips=substr(statdist,1,2),
         district=as.numeric(substr(statdist,3,4))) %>% 
  rename(state=statefips) %>% 
  left_join(.,stateab) %>% 
  dplyr::select(-state) %>% 
  mutate(creative2=tolower(creative),
         imm=ifelse(grepl("immig",creative2)|grepl("border",creative2),1,0)) %>% 
  rename(cost=EST_SPENDING,
         len=l,
         adname=creative) %>% 
  separate(CAND_ID,c("state","name"),extra="merge",sep="/") %>% 
  separate(name,c("surname","name"),extra="merge") %>% 
  mutate(tone=ifelse(AD_TONE==1,"Contrast",
                     ifelse(AD_TONE==2,"Promote",
                            ifelse(AD_TONE==3,"Attack",NA)))) %>% 
  mutate(last60=ifelse(airdate>"2008-09-04",1,0)) %>% 
  dplyr::select(year,office,market,DMAINDEX,statefips,adname,surname,airdate,len,cost,sponsors,party,
                tone,trade,china,jobs,imm,last60)

# 2010 
# House 
ad10_H_con <- ad10_H
# First merge ad costs 
cost <- read.dta13("wmp-2010-spending-dmaid.dta")

costm <- cost %>% 
  filter(race=="US House") %>%
  rename(dma=dmaid) %>% 
  dplyr::select(airdate,creative,dma,state,district,station,est_cost) 

ad10_H_con$creative <- trimws(ad10_H_con$creative)

ad10_H_con <- left_join(ad10_H_con,costm)

delpar <- "RNC&|&NRCC|DCCC&|FLDP&|&HIRP|IADP&|&NRCC|&RNC|&IDRSCC|&HAYES|INDP&|&MNRP|MNDFL&|NCDP&|NHDP&|&DCCC|PADP&|TNDP&|VADP&|WIDP&|NRCC&"

vars <- c("issue53","issue12","issue20","issue65","issue66","issue67","issue70","issue71","issue72","issue69","issue13","issue18")
ad10_H_con[vars][is.na(ad10_H_con[vars])] <- 0

ad10_H_con <- ad10_H_con %>%
  mutate(office="house",
         trade=issue20,
         china=issue65,
         jobs=issue18,
         creative2=tolower(creative),
         issue972=tolower(issue97_txt),
         imm=ifelse(grepl("immig",creative2)|grepl("border",creative2)|grepl("immig",issue972)|grepl("border",issue972),1,0)) %>% 
  mutate(creative3=creative) %>% 
  separate(creative3,c("race","rest"),extra="merge",sep="/") %>% 
  separate(rest,c("state2","surname","rest"),extra="merge",sep=" ") %>% 
  mutate(surname=gsub(delpar,"",surname)) %>% 
  mutate(party=ifelse(state=="CA"&district==17&surname=="FARR","Democrat",
                      ifelse(state=="FL"&district==2&surname=="BOYD","Democrat",
                             ifelse(state=="MT"&district==1&surname=="MCDONALD","Democrat",
                                    ifelse(state=="ND"&district==1&surname=="BERG","Republican",
                                           ifelse(state=="NE"&district==2&surname=="WHITE","Democrat",
                                                  ifelse(state=="NY"&district==23&surname=="OWENS","Democrat",
                                                         ifelse(state=="OR"&district==1&surname=="WU","Democrat",
                                                                ifelse(state=="PA"&district==15&surname=="DENT","Republican",
                                                                       ifelse(state=="PA"&district==15&surname=="CALLAHAN","Democrat",
                                                                              ifelse(state=="VA"&district==7&surname=="CANTOR","Republican",
                                                                                     ifelse(state=="WI"&district==3&surname=="KIND","Democrat",party))))))))))),
         tone=ifelse(ad_tone==1,"Contrast",
                     ifelse(ad_tone==2,"Promote",
                            ifelse(ad_tone==3,"Attack",NA)))) %>% 
  rename(cost=est_cost,
         len=l,
         adname=creative) %>% 
  mutate(last60=ifelse(airdate>"2010-09-02",1,0)) %>% 
  # merge state fips 
  left_join(.,cross_stfipsab) %>% 
  dplyr::select(year,office,market,DMAINDEX,statefips,district,adname,surname,airdate,len,cost,sponsors,party,
                tone,trade,china,jobs,imm,last60)
  
# Senate 
ad10_S_con <- ad10_S
  # First merge ad costs 
costm <- cost %>% 
    filter(race=="US Senate") %>%
    rename(dma=dmaid) %>% 
  dplyr::select(airdate,creative,dma,state,district,station,est_cost) 
  
ad10_S_con$creative <- trimws(ad10_S_con$creative)
  
ad10_S_con <- left_join(ad10_S_con,costm)
  
delpar <- "RNC&|&NRCC|DCCC&|FLDP&|&HIRP|IADP&|&NRCC|&RNC|&IDRSCC|&HAYES|INDP&|&MNRP|MNDFL&|NCDP&|NHDP&|&DCCC|PADP&|TNDP&|VADP&|WIDP&|NRCC&|&NRSC|NRSC&|LADP&|&DNC|&DSCC|&WIDP|&ILDP|ILDP&|VTRFEC&"
vars <- c("issue53","issue12","issue20","issue65","issue66","issue67","issue70","issue71","issue72","issue69","issue13","issue18")
ad10_S_con[vars][is.na(ad10_S_con[vars])] <- 0

ad10_S_con <- ad10_S_con %>%
  mutate(office="senate",
         trade=issue20,
         china=issue65,
         jobs=issue18,
         creative2=tolower(creative),
         issue972=tolower(issue97_txt),
         imm=ifelse(grepl("immig",creative2)|grepl("border",creative2)|grepl("immig",issue972)|grepl("border",issue972),1,0)) %>% 
  mutate(creative3=creative) %>% 
  separate(creative3,c("race","rest"),extra="merge",sep="/") %>% 
  separate(rest,c("state2","surname","rest"),extra="merge",sep=" ") %>% 
  mutate(surname=gsub(delpar,"",surname), 
         tone=ifelse(ad_tone==1,"Contrast",
                     ifelse(ad_tone==2,"Promote",
                            ifelse(ad_tone==3,"Attack",NA)))) %>% 
  rename(cost=est_cost,
         len=l,
         adname=creative) %>% 
  mutate(last60=ifelse(airdate>"2010-09-02",1,0)) %>% 
  # merge state fips 
  left_join(.,cross_stfipsab) %>%
  dplyr::select(year,office,market,DMAINDEX,statefips,adname,surname,airdate,len,cost,sponsors,party,
              tone,trade,china,jobs,imm,last60)

# 2012

# House 
ad12_H_con <- ad12_H

ad12_H_con <- ad12_H_con %>% 
  # get rid of primaries
  filter(election=="GENERAL") %>% 
  mutate(office="house",
    trade=issue20,
         china=issue65,
         jobs=issue18,
         imm=issue95,
         district=as.numeric(district)) %>% 
  rename(cost=est_cost,
         len=l,
         adname=creative) %>% 
  separate(sponsorcmag,c("surname","name"),sep=",",extra="merge") %>% 
  mutate(district=ifelse(surname=="MCSALLY"&categorystate=="AZ"&district==8,2,
                         ifelse(surname=="MAFFEI"&categorystate=="NY"&district==25,24,
                                ifelse(surname=="SUTTON"&categorystate=="OH"&district==13,16,district))),
         categorystate=ifelse(surname=="GILLAN"&categorystate=="MO"&district==1,"MT",categorystate),
         party=ifelse(surname=="GILLAN"&categorystate=="MT"&district==1,"Democrat",party),
         tone=ifelse(ad_tone==1,"Contrast",
                     ifelse(ad_tone==2,"Promote",
                            ifelse(ad_tone==3,"Attack",NA)))) %>% 
  left_join(.,cross_stfipsab) %>%
  mutate(last60=ifelse(airdate>"2012-09-06",1,0)) %>% 
  dplyr::select(year,office,market,DMAINDEX,statefips,district,adname,surname,airdate,len,cost,sponsors,party,
                tone,trade,china,jobs,imm,last60)

# Senate 
ad12_S_con <- ad12_S

ad12_S_con <- ad12_S_con %>% 
  filter(election=="GENERAL") %>% 
  mutate(office="senate",
         trade=issue20,
         china=issue65,
         jobs=issue18,
         imm=issue95) %>% 
  rename(cost=est_cost,
         len=l,
         adname=creative) %>% 
  separate(sponsorcmag,c("surname","name"),sep=",",extra="merge") %>% 
  mutate(tone=ifelse(ad_tone==1,"Contrast",
                     ifelse(ad_tone==2,"Promote",
                            ifelse(ad_tone==3,"Attack",NA)))) %>% 
  left_join(.,cross_stfipsab) %>%
  mutate(last60=ifelse(airdate>"2012-09-06",1,0)) %>% 
  dplyr::select(year,office,market,DMAINDEX,statefips,adname,surname,airdate,len,cost,sponsors,party,
                tone,trade,china,jobs,imm,last60)

# 2014 

# House 
ad14_H_con <- ad14_H

ad14_H_con <- ad14_H_con %>% 
  filter(election=="GENERAL") %>% 
  mutate(office="house",
         trade=issue20,
         china=issue65,
         jobs=issue18,
         imm=issue95,
         district=as.numeric(district)) %>% 
  rename(cost=est_cost,
         len=l,
         adname=creative) %>% 
  separate(sponsorcmag,c("surname","name"),sep=",",extra="merge") %>% 
  mutate(surname=gsub("DEMOCRATIC CONGRESSIONAL CAMPAIGN COMMITTEE & ","",surname),
         surname=gsub("NATIONAL REPUBLICAN CONGRESSIONAL COMMITTEE & ","",surname),
         surname=gsub("NATIONAL REPUBLICAN CONGRESSIONAL COMMITTEE","",surname),
         surname=gsub("FLORIDA DEMOCRATIC PARTY & ","",surname),
         surname=gsub("TENNESSEE DEMOCRATIC PARTY & ","",surname),
         surname=gsub("MONTANA DEMOCRATIC PARTY & ","",surname),
         surname=gsub("NATIONAL REPUBLICAN SENATORIAL COMMITTEE & ","",surname),
         surname=trimws(surname),
         tone=ifelse(ad_tone==1,"Contrast",
                     ifelse(ad_tone==2,"Promote",
                            ifelse(ad_tone==3,"Attack",NA)))) %>% 
  left_join(.,cross_stfipsab) %>%
  mutate(last60=ifelse(airdate>"2014-09-04",1,0)) %>% 
  dplyr::select(year,office,market,DMAINDEX,statefips,district,adname,surname,airdate,len,cost,sponsors,party,
                tone,trade,china,jobs,imm,last60)

# Senate
ad14_S_con <- ad14_S

ad14_S_con <- ad14_S_con %>% 
  filter(election=="GENERAL") %>% 
  mutate(office="senate",
         trade=issue20,
         china=issue65,
         jobs=issue18,
         imm=issue95) %>% 
  rename(cost=est_cost,
         len=l,
         adname=creative) %>% 
  separate(sponsorcmag,c("surname","name"),sep=",",extra="merge") %>% 
  mutate(surname=gsub("DEMOCRATIC CONGRESSIONAL CAMPAIGN COMMITTEE & ","",surname),
         surname=gsub("NATIONAL REPUBLICAN CONGRESSIONAL COMMITTEE & ","",surname),
         surname=gsub("NATIONAL REPUBLICAN CONGRESSIONAL COMMITTEE","",surname),
         surname=gsub("FLORIDA DEMOCRATIC PARTY & ","",surname),
         surname=gsub("TENNESSEE DEMOCRATIC PARTY & ","",surname),
         surname=trimws(surname),
         tone=ifelse(ad_tone==1,"Contrast",
                     ifelse(ad_tone==2,"Promote",
                            ifelse(ad_tone==3,"Attack",NA)))) %>% 
  left_join(.,cross_stfipsab) %>%
  mutate(last60=ifelse(airdate>"2014-09-04",1,0)) %>% 
  dplyr::select(year,office,market,DMAINDEX,statefips,adname,surname,airdate,len,cost,sponsors,party,
                tone,trade,china,jobs,imm,last60)

# 2016 

# House
ad16_H_con <- ad16_H

ad16_H_con <- ad16_H_con %>% 
  filter(election=="GENERAL") %>% 
  mutate(office="house",
         trade=issue20,
         china=issue65,
         jobs=issue18,
         imm=issue95,
         district=as.numeric(district)) %>% 
  rename(cost=est_cost,
         len=l,
         adname=creative) %>% 
  separate(sponsorcmag,c("surname","name"),sep=",",extra="merge") %>% 
  mutate(state=ifelse(surname=="BUSTOS"&state=="IA"&district==17,"IL",
                      ifelse(surname=="BARICEVIC"&state=="KY"&district==12,"IL",
                             ifelse(surname=="BOST"&state=="KY"&district==12,"IL",
                                    ifelse(surname=="YODER"&state=="KY"&district==9,"IN",
                                           ifelse(surname=="HOLLINGSWORTH"&state=="KY"&district==9,"IN",
                                                  ifelse(surname=="SHEA-PORTER"&state=="MA"&district==1,"NH",
                                                         ifelse(surname=="BLUNT ROCHESTER"&state=="MD"&district==1,"DE",
                                                                ifelse(surname=="BARICEVIC"&state=="MO"&district==12,"IL",
                                                                       ifelse(surname=="JENKINS"&state=="MO"&district==2,"KS",
                                                                              ifelse(surname=="YODER"&state=="MO"&district==3,"KS",
                                                                                     ifelse(surname=="PERSON"&state=="NC"&district==5,"SC",
                                                                                            ifelse(surname=="MULVANEY"&state=="NC"&district==5,"SC",
                                                                                                   ifelse(surname=="GOTTHEIMER"&state=="NY"&district==5,"NJ",
                                                                                                          ifelse(surname=="GARRETT"&state=="NY"&district==5,"NJ",
                                                                                                                 ifelse(surname=="CASTILLO"&state=="NY"&district==9,"NJ",
                                                                                                                        ifelse(surname=="WALBERG"&state=="OH"&district==7,"MI",
                                                                                                                               ifelse(surname=="LOBION"&state=="PA"&district==2,"NJ",
                                                                                                                                      ifelse(surname=="HUDSON"&state=="SC"&district==8,"NC",
                                                                                                                                             ifelse(surname=="SOULES"&state=="TX"&district==2,"NM",
                                                                                                                                                    ifelse(surname=="DERRICK"&state=="VT"&district==21,"NY",
                                                                                                                                                           ifelse(surname=="STEFANIK"&state=="VT"&district==21,"NY",
                                                                                                                                                                  ifelse(surname=="PIOTROWSKI"&state=="WA"&district==1,"ID",state)))))))))))))))))))))),
         party=ifelse(surname=="LAPOLICE"&state=="KS"&district==1,"Democrat",party),
         tone=ifelse(ad_tone==1,"Contrast",
                     ifelse(ad_tone==2,"Promote",
                            ifelse(ad_tone==3,"Attack",NA)))) %>% 
  left_join(.,cross_stfipsab) %>%
  mutate(last60=ifelse(airdate>"2016-09-08",1,0)) %>% 
  dplyr::select(year,office,market,DMAINDEX,statefips,district,adname,surname,airdate,len,cost,sponsors,party,
                tone,trade,china,jobs,imm,last60)
  
# Senate 
ad16_S_con <- ad16_S

ad16_S_con <- ad16_S_con %>%
  filter(election=="GENERAL") %>% 
  mutate(office="senate",
         trade=issue20,
         china=issue65,
         jobs=issue18,
         imm=issue95,
         district=as.numeric(district)) %>% 
  rename(cost=est_cost,
         len=l,
         adname=creative) %>% 
  separate(sponsorcmag,c("surname","name"),sep=",",extra="merge") %>% 
  mutate(tone=ifelse(ad_tone==1,"Contrast",
                     ifelse(ad_tone==2,"Promote",
                            ifelse(ad_tone==3,"Attack",NA)))) %>% 
  left_join(.,cross_stfipsab) %>%
  mutate(last60=ifelse(airdate>"2016-09-08",1,0)) %>% 
  dplyr::select(year,office,market,DMAINDEX,statefips,adname,surname,airdate,len,cost,sponsors,party,
                tone,trade,china,jobs,imm,last60)

### SAVE HOUSE AND SENATE INTO TWO SEPARATE LISTS 

# House List 
datasets <- grep("^ad.*H_con$",names(.GlobalEnv),value=TRUE)
listhouse <- do.call("list",mget(datasets))

save(listhouse,file="houseads.Rdata")

# Senate List 
datasets2 <- grep("^ad.*S_con$",names(.GlobalEnv),value=TRUE)
listsenate <- do.call("list",mget(datasets2))

save(listsenate,file="senateads.Rdata")

#### PRESIDENT 

# 2000
ad00_P_con <- ad00_P
vars <- c("q32","q33","q34","q35")
ad00_P_con[vars][is.na(ad00_P_con[vars])] <- 99

ad00_P_con <- ad00_P_con %>% 
  mutate(trade=ifelse(q32==18|q33==18|q34==18|q35==18,1,0),
         china=ifelse(q32==55|q33==55|q34==55|q35==55,1,0),
         imm=ifelse(q32==71|q32==71|q32==71|q32==71,1,0),
         jobs=ifelse(q32==16|q33==16|q34==16|q35==16,1,0)) %>% 
  # get rid of primaries 
  filter(q4==2) %>% 
  mutate(last60=ifelse(spotdate>"2000-09-07",1,0),
         name=custitle,
         tone=ifelse(q14==1,"Attack",
                     ifelse(q14==2,"Contrast",
                            ifelse(q14==3,"Promote",NA)))) %>%
  separate(name,c("surname","adtitle"),sep="/",extra="merge") %>% 
  mutate(office="pres",
         adname=custitle,
         district=q3,
         airdate=spotdate,
         statefips=statcode) %>% 
  dplyr::select(year,office,market,DMAINDEX,statefips,adname,surname,airdate,len,cost,sponsors,party,
                tone,trade,china,jobs,imm,last60)

# 2004 

ad04_P_con <- ad04_P
vars <- c("EISSUE1","EISSUE2","EISSUE3","EISSUE4")
ad04_P_con[vars][is.na(ad04_P_con[vars])] <- 0

ad04_P_con <- ad04_P_con %>% 
  # doesn't have primary indicator
  # create district variable 
  mutate(trade=ifelse(EISSUE1==18|EISSUE2==18|EISSUE3==18|EISSUE4==18,1,0),
         china=ifelse(EISSUE1==55|EISSUE2==55|EISSUE3==55|EISSUE4==55,1,0),
         jobs=ifelse(EISSUE1==16|EISSUE2==16|EISSUE3==16|EISSUE4==16,1,0),
         imm=ifelse(EISSUE1==71|EISSUE2==71|EISSUE3==71|EISSUE4==71,1,0),
         tone=ifelse(EAD_TONE==1,"Attack",
                     ifelse(EAD_TONE==2,"Contrast",
                            ifelse(EAD_TONE==3,"Promote",NA)))) %>% 
  separate(Cand_ID,c("state","name"),sep="/",extra="merge") %>% 
  separate(name,c("surname","rest"),extra="merge")  %>% 
  mutate(last60=ifelse(date>"2004-09-02",1,0),
         office="pres") %>% 
  rename(airdate=date,
         cost=est_cost,
         statefips=fips,
         adname=creative,
         len=spotleng) %>% 
  dplyr::select(year,office,market,DMAINDEX,statefips,adname,surname,airdate,len,cost,sponsors,party,
                tone,trade,china,jobs,imm,last60)

# 2008 
ad08_P_con <- ad08_P

vars <- c("terror","hcare","recess","trade","china","mideast","iran","afghan","govspend","jobs")
ad08_P_con[vars][is.na(ad08_P_con[vars])] <- 0

ad08_P_con <- ad08_P_con %>% 
  # doesn't have primary indicator
  mutate(date2=date) %>% 
  separate(date2,c("m","d","y"),sep="/")  %>% 
  # fixing some years who were written incomplete 
  mutate(y=ifelse(y=="200","2007",y),
         date2=paste0(m,"/",d,"/",y)) %>% 
  mutate(airdate=mdy(date2),
         CAND_ID2=CAND_ID) %>% 
  separate(CAND_ID2,c("race","fullname"), extra="merge",sep="/") %>%
  separate(fullname,c("surname","name"),extra="merge") %>%
  mutate(office="pres") %>% 
  mutate(creative2=tolower(creative),
         imm=ifelse(grepl("immig",creative2)|grepl("border",creative2),1,0)) %>% 
  rename(cost=EST_SPENDING,
         len=l,
         adname=creative) %>% 
  mutate(tone=ifelse(AD_TONE==1,"Contrast",
                     ifelse(AD_TONE==2,"Promote",
                            ifelse(AD_TONE==3,"Attack",NA)))) %>% 
  mutate(last60=ifelse(airdate>"2008-09-04",1,0)) %>% 
  rename(state=STATE_1) %>% 
  left_join(.,cross_stfipsab) %>% 
  dplyr::select(year,office,market,DMAINDEX,statefips,adname,surname,airdate,len,cost,sponsors,party,
                tone,trade,china,jobs,imm,last60)

# 2012 
ad12_P_con <- ad12_P

ad12_P_con <- ad12_P_con %>% 
  # filter general elections
  filter(election=="GENERAL") %>% 
  separate(sponsorcmag,c("surname","name",sep="-")) %>%
  mutate(office="pres",
         trade=issue20,
         china=issue65,
         jobs=issue18,
         imm=issue95) %>% 
  rename(cost=est_cost,
         len=l,
         adname=creative) %>% 
  mutate(tone=ifelse(ad_tone==1,"Contrast",
                     ifelse(ad_tone==2,"Promote",
                            ifelse(ad_tone==3,"Attack",NA)))) %>% 
  mutate(last60=ifelse(airdate>"2012-09-06",1,0),
         statefips=NA) %>% 
  dplyr::select(year,office,market,DMAINDEX,statefips,adname,surname,airdate,len,cost,sponsors,party,
                tone,trade,china,jobs,imm,last60)

# 2016 
ad16_P_con <- ad16_P

ad16_P_con <- ad16_P_con %>% 
  filter(election=="GENERAL") %>% 
  mutate(office="pres",
         trade=issue20,
         china=issue65,
         jobs=issue18,
         imm=issue95,
         district=as.numeric(district)) %>% 
  rename(cost=est_cost,
         len=l,
         adname=creative) %>% 
  separate(sponsorcmag,c("surname","name"),sep=",",extra="merge") %>% 
  mutate(tone=ifelse(ad_tone==1,"Contrast",
                     ifelse(ad_tone==2,"Promote",
                            ifelse(ad_tone==3,"Attack",NA)))) %>% 
  left_join(.,cross_stfipsab) %>%
  mutate(last60=ifelse(airdate>"2016-09-08",1,0)) %>% 
  dplyr::select(year,office,market,DMAINDEX,statefips,adname,surname,airdate,len,cost,sponsors,party,
                tone,trade,china,jobs,imm,last60)

## PUT PRES ADS TOGETHER IN A LIST 

# Pres List
datasets3 <- grep("^ad.*P_con$",names(.GlobalEnv),value=TRUE)
listpres <- do.call("list",mget(datasets3))

save(listpres,file="presads.Rdata")







  






  
  
  

  
                              
                       
    