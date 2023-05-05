###################
# title: Politicizing Trade: How Economic Discontent and Identity Politics Shape Anti-Trade Campaign Appeals
# author: Aycan Katitas
# date: 08/03/2022
# RStudio version: 2022.07.1
###################

### LOOK AT OLD AND NEW MANUFACTURING HUBS AS SHORT CASE STUDIES: 
## OLD: GARY, DETROIT, CLEVELAND, BUFFALO, PITTSBURGH, NWEARK, BALTIMORE 
## NEW: ALABAMA, ARKANSAS, LENTUCKY, MISSISSIPPI, NORTH CAROLINA, SOTUH CAROLINA, TENNESSEE 

#### Working Directory 
setwd("~/Dropbox/jmp")

#### Libraries 
library(dplyr)
library(devtools)
library(congress)
library(tidyverse)
library(bea.R)
library(readstata13)

#### Keys 
beaKey <- "BF17843A-DBD2-4F35-B636-3E1528B908D6"
mycensuskey <- "d56f677e0de95f9ad357b62f7ea24d1a1ce31b28"

#### Functions
## create cumulative advertising for specified years
cum_create <- function(yearstart,yearend){
  
  df <- totalads0016 %>% 
    left_join(.,intads0016) %>% 
    filter(year>yearstart&year<yearend) %>% 
    group_by(market,race) %>% 
    summarise(across(totalad:tradeintad,sum,na.rm=T)) %>% 
    ungroup() %>% 
    pivot_wider(names_from=race,
                values_from=totalad:tradeintad,
                values_fill=0) %>% 
    mutate(totalad_hsp=totalad_house+totalad_senate+totalad_pres,
           tradead_hsp=tradead_house+tradead_senate+tradead_pres,
           jobsad_hsp=jobsad_house+jobsad_senate+jobsad_pres,
           chinaad_hsp=chinaad_house+chinaad_senate+chinaad_pres,
           immad_hsp=immad_house+immad_senate+immad_pres,
           tradeintad_hsp=tradeintad_house+tradeintad_senate+tradeintad_pres,
           totalad_hsp_D=totalad_D_house+totalad_D_senate+totalad_D_pres,
           tradead_hsp_D=tradead_D_house+tradead_D_senate+tradead_D_pres,
           jobsad_hsp_D=jobsad_D_house+jobsad_D_senate+jobsad_D_pres,
           chinaad_hsp_D=chinaad_D_house+chinaad_D_senate+chinaad_D_pres,
          # immad_hsp_D=immad_D_house+immad_D_senate+immad_D_pres,
           totalad_hsp_R=totalad_R_house+totalad_R_senate+totalad_R_pres,
           tradead_hsp_R=tradead_R_house+tradead_R_senate+tradead_R_pres,
           jobsad_hsp_R=jobsad_R_house+jobsad_R_senate+jobsad_R_pres,
           chinaad_hsp_R=chinaad_R_house+chinaad_R_senate+chinaad_R_pres,
           #immad_hsp_R=immad_R_house+immad_R_senate+immad_R_pres
          ) %>%
    filter(!is.na(market))
  
  return(df)
}

##### OUTCOME 
# Construction of the Outcome - Televised Ads at the DMA level
load("houseads.Rdata")
load("senateads.Rdata")
load("presads.Rdata")

dma100 <- read.csv("top100dma.csv")

last60=TRUE
top100=FALSE

# look at ads in the last two months of the general election 
if(last60==TRUE){
  listhouse = lapply(listhouse, function(x) x %>% filter(last60==1))
  listsenate= lapply(listsenate, function(x) x %>% filter(last60==1))
} 

if(top100==TRUE){
  listhouse = lapply(listhouse, function(x) x %>% filter(dma %in% dma100$dma))
  listsenate = lapply(listsenate, function(x) x %>% filter(dma %in% dma100$dma))
} 


## House Trade - China - Jobs Ads
houseprep <- lapply(listhouse,function(x) x %>% 
                      filter(sponsors=="candcoord"|sponsors=="party") %>%
                      mutate(party2=ifelse(party=="Democrat","D",
                                           ifelse(party=="Republican","R","I"))) %>% 
                      group_by(year,market) %>% 
                      summarise(totalad=n(), totalad_D=sum(party2=="D",na.rm=T),totalad_R=sum(party2=="R",na.rm=T),
                                tradead=sum(trade==1,na.rm=TRUE), tradead_D=sum(party2=="D"&trade==1,na.rm=TRUE), tradead_R=sum(party2=="R"&trade==1,na.rm=TRUE),
                                jobsad=sum(jobs==1,na.rm=TRUE), jobsad_D=sum(party2=="D"&jobs==1,na.rm=TRUE),jobsad_R=sum(party2=="R"&jobs==1,na.rm=TRUE),
                                chinaad=sum(china==1,na.rm=T), chinaad_D=sum(party2=="D"&china==1,na.rm=T),chinaad_R=sum(party2=="R"&china==1,na.rm=T),
                                immad=sum(imm==1,na.rm=T, immad_D=sum(party2=="D"&imm==1,na.rm=T),immad_R=sum(party2=="R"&imm==1,na.rm=T))
                      ) %>% 
                      ungroup() %>% 
                      mutate(race="house") %>%
                      dplyr::select(year,market,race,ends_with("ad"),ends_with("_D"),ends_with("R")))

houseads0016 <- bind_rows(houseprep)

## Senate Trade - China - Jobs Ads

senateprep <- lapply(listsenate,function(x) x %>% 
                       filter(sponsors=="candcoord"|sponsors=="party") %>%
                       mutate(party2=ifelse(party=="Democrat","D",
                                            ifelse(party=="Republican","R","I"))) %>% 
                       group_by(year,market) %>% 
                       summarise(totalad=n(), totalad_D=sum(party2=="D",na.rm=T),totalad_R=sum(party2=="R",na.rm=T),
                                 tradead=sum(trade==1,na.rm=TRUE), tradead_D=sum(party2=="D"&trade==1,na.rm=TRUE), tradead_R=sum(party2=="R"&trade==1,na.rm=TRUE),
                                 jobsad=sum(jobs==1,na.rm=TRUE), jobsad_D=sum(party2=="D"&jobs==1,na.rm=TRUE),jobsad_R=sum(party2=="R"&jobs==1,na.rm=TRUE),
                                 chinaad=sum(china==1,na.rm=T),chinaad_D=sum(party2=="D"&china==1,na.rm=T),chinaad_R=sum(party2=="R"&china==1,na.rm=T),
                                 immad=sum(imm==1,na.rm=T, immad_D=sum(party2=="D"&imm==1,na.rm=T),immad_R=sum(party2=="R"&imm==1,na.rm=T))
                       ) %>% ungroup() %>% 
                       mutate(race="senate") %>% 
                       dplyr::select(year,market,race,ends_with("ad"),ends_with("_D"),ends_with("R")))

senateads0016 <- bind_rows(senateprep)

congressads0016 <- rbind(houseads0016,senateads0016)

# Pres - Trade China Jobs 
presprep <- lapply(listpres,function(x) x %>% 
                       filter(sponsors=="candcoord"|sponsors=="party") %>%
                       mutate(party2=ifelse(party=="Democrat","D",
                                            ifelse(party=="Republican","R","I"))) %>% 
                       group_by(year,market) %>% 
                       summarise(totalad=n(), totalad_D=sum(party2=="D",na.rm=T),totalad_R=sum(party2=="R",na.rm=T),
                                 tradead=sum(trade==1,na.rm=TRUE), tradead_D=sum(party2=="D"&trade==1,na.rm=TRUE), tradead_R=sum(party2=="R"&trade==1,na.rm=TRUE),
                                 jobsad=sum(jobs==1,na.rm=TRUE), jobsad_D=sum(party2=="D"&jobs==1,na.rm=TRUE),jobsad_R=sum(party2=="R"&jobs==1,na.rm=TRUE),
                                 chinaad=sum(china==1,na.rm=T),chinaad_D=sum(party2=="D"&china==1,na.rm=T),chinaad_R=sum(party2=="R"&china==1,na.rm=T),
                                 immad=sum(imm==1,na.rm=T, immad_D=sum(party2=="D"&imm==1,na.rm=T),immad_R=sum(party2=="R"&imm==1,na.rm=T))
                       ) %>% ungroup() %>% 
                       mutate(race="pres") %>% 
                       dplyr::select(year,market,race,ends_with("ad"),ends_with("_D"),ends_with("R")))

presads0016 <- bind_rows(presprep)

# interest group trade ads 
# house 
houseintprep <- lapply(listhouse,function(x) x %>% 
                     filter(sponsors=="interest") %>%
                     group_by(year,market) %>% 
                     summarise(tradeintad=sum(trade==1&sponsors=="interest",na.rm=T)
                     ) %>% ungroup() %>% 
                     mutate(race="house") %>% 
                     dplyr::select(year,market,race,tradeintad))

houseint0016 <- bind_rows(houseintprep)
# senate
senateintprep <- lapply(listsenate,function(x) x %>% 
                         filter(sponsors=="interest") %>%
                         group_by(year,market) %>% 
                         summarise(tradeintad=sum(trade==1&sponsors=="interest",na.rm=T)
                         ) %>% ungroup() %>% 
                         mutate(race="senate") %>% 
                         dplyr::select(year,market,race,tradeintad))

senateint0016 <- bind_rows(senateintprep)
# pres 
presintprep <- lapply(listpres,function(x) x %>% 
                        filter(sponsors=="interest") %>%
                        group_by(year,market) %>% 
                        summarise(tradeintad=sum(trade==1&sponsors=="interest",na.rm=T)
                        ) %>% ungroup() %>% 
                        mutate(race="pres") %>% 
                        dplyr::select(year,market,race,tradeintad))

presint0016 <- bind_rows(presintprep)

intads0016 <- rbind(houseint0016,senateint0016,presint0016)

totalads0016 <- rbind(congressads0016,presads0016)

save(totalads0016,file="genhspads0016.Rdata")
save(intads0016,file="genintads0016.Rdata")



# Collapse into time periods - senate + house + pres 
## choose year gap
# 2000 has ads from only the top 100 dmas
yearstart=2007
yearend=2017

# TYPE HOUSE, SENATE, PRES ADS SEPARATELY 

# cumulative ads 08-2016
ads_cum <- cum_create(2007,2017)

# Cumulative ads 00-16 
ads_cum_0016 <- cum_create(1999,2017)

ads_cum_0016_m <- ads_cum_0016 %>% 
  mutate(totalad_hsp00=totalad_house+totalad_senate+totalad_pres,
         per_trade00=(tradead_house+tradead_senate+tradead_pres)/totalad_hsp00) %>% 
  dplyr::select(market,per_trade00,totalad_hsp00)

# Cumulative ads without year 2016
ads_cum_0814 <- cum_create(2007,2015)

ads_cum_0814_m <- ads_cum_0814 %>% 
  mutate(totalad_hsp14=totalad_house+totalad_senate+totalad_pres,
         per_trade14=(tradead_house+tradead_senate+tradead_pres)/totalad_hsp14) %>% 
  dplyr::select(market,per_trade14,totalad_hsp14)

# Difference between 2016 and 2008
add_diff_0816 <- totalads0016 %>% 
  group_by(year,market,race) %>%
  summarise(across(totalad:chinaad_R,sum,na.rm=T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from=race,
              values_from=totalad:chinaad_R,
              values_fill=0) %>% 
  filter(year==2008|year==2016) %>% 
  filter(!is.na(market)) %>%
  pivot_wider(names_from=year,
              values_from=totalad_house:chinaad_R_senate) %>%
  mutate(totalad_hsp_2008=totalad_house_2008+totalad_senate_2008+totalad_pres_2008,
         tradead_hsp_2008=tradead_house_2008+tradead_senate_2008+tradead_pres_2008,
         totalad_hsp_2016=totalad_house_2016+totalad_senate_2016+totalad_pres_2016,
         tradead_hsp_2016=tradead_house_2016+tradead_senate_2016+tradead_pres_2016,
         per_trade816=(tradead_hsp_2016/totalad_hsp_2016)-(tradead_hsp_2008/totalad_hsp_2008),
         d_totalad816=totalad_hsp_2016-totalad_hsp_2008) %>% 
  dplyr::select(market,per_trade816,d_totalad816)

# Ads at year-market level 
ads_year <- totalads0016 %>% 
  left_join(.,intads0016) %>% 
  group_by(year,market,race) %>%
  summarise(across(totalad:tradeintad,sum,na.rm=T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from=race,
              values_from=totalad:tradeintad,
              values_fill=0) %>% 
  mutate(totalad_hsp=totalad_house+totalad_senate+totalad_pres,
         tradead_hsp=tradead_house+tradead_senate+tradead_pres,
         jobsad_hsp=jobsad_house+jobsad_senate+jobsad_pres,
         chinaad_hsp=chinaad_house+chinaad_senate+chinaad_pres,
         immad_hsp=immad_house+immad_senate+immad_pres,
         tradeintad_hsp=tradeintad_house+tradeintad_senate+tradeintad_pres,
         totalad_hsp_D=totalad_D_house+totalad_D_senate+totalad_D_pres,
         tradead_hsp_D=tradead_D_house+tradead_D_senate+tradead_D_pres,
         jobsad_hsp_D=jobsad_D_house+jobsad_D_senate+jobsad_D_pres,
         chinaad_hsp_D=chinaad_D_house+chinaad_D_senate+chinaad_D_pres,
        # immad_hsp_D=immad_D_house+immad_D_senate+immad_D_pres,
         totalad_hsp_R=totalad_R_house+totalad_R_senate+totalad_R_pres,
         tradead_hsp_R=tradead_R_house+tradead_R_senate+tradead_R_pres,
         jobsad_hsp_R=jobsad_R_house+jobsad_R_senate+jobsad_R_pres,
         chinaad_hsp_R=chinaad_R_house+chinaad_R_senate+chinaad_R_pres,
        # immad_hsp_R=immad_R_house+immad_R_senate+immad_R_pres
         ) %>%
  filter(!is.na(market))

##### IVS 
# Construction of Deindustrialization Measure + Bartik Instrument 

## Use BEA API to pull employment - manufacturing and total for each county 
## Dataset is Regional, LineCodes are 10, 400

beaSpecs10 <- list(
  "UserID" = beaKey, # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "CAEMP25S", # Specify table within the dataset
  "LineCode" = 10, # Specify the line code
  "GeoFips" = "COUNTY", # Specify the geographical level
  "Year" = "ALL" # Specify the year
)

beaSpecs400 <- list(
  "UserID" = beaKey, # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "CAEMP25S", # Specify table within the dataset
  "LineCode" = 400, # Specify the line code
  "GeoFips" = "COUNTY", # Specify the geographical level
  "Year" = "ALL" # Specify the year
)

beaSpecs500 <- list(
  "UserID" = beaKey, # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "CAEMP25N", # Specify table within the dataset
  "LineCode" = 500, # Specify the line code
  "GeoFips" = "COUNTY", # Specify the geographical level
  "Year" = "ALL" # Specify the year
)

# Services 
# 1970 finance 
beaSpecs700 <- list(
  "UserID" = beaKey, # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "CAEMP25S", # Specify table within the dataset
  "LineCode" = 700, # Specify the line code
  "GeoFips" = "COUNTY", # Specify the geographical level
  "Year" = "ALL" # Specify the year
)
# 1970 services 
beaSpecs800 <- list(
  "UserID" = beaKey, # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "CAEMP25S", # Specify table within the dataset
  "LineCode" = 800, # Specify the line code
  "GeoFips" = "COUNTY", # Specify the geographical level
  "Year" = "ALL" # Specify the year
)

# 2007: lines: 1000,1100,1200,
get_services <- function(x){
  
  y <- as.numeric(x)
  
  beaSpecs <- list(
    "UserID" = beaKey, # Set up API key
    "Method" = "GetData", # Method
    "datasetname" = "Regional", # Specify dataset
    "TableName" = "CAEMP25N", # Specify table within the dataset
    "LineCode" = y, # Specify the line code
    "GeoFips" = "COUNTY", # Specify the geographical level
    "Year" = "ALL" # Specify the year
  )
  
  d <- beaGet(beaSpecs,asWide=TRUE)
  
  d <- d %>% 
    dplyr::select(Code,GeoFips,DataValue_2007)
  
}




## Employment values are missing for AK, 4012 - La Paz, AZ; 35006 - Cibola, NM; 55078; 55115/55901 - Shawano, WI 
## There are changes to counties which I will fix later 
## The API pulls (D) data as 0 - fix these to 5 to avoid missing values later 
bea_totempl <- beaGet(beaSpecs10,asWide=FALSE)
bea_manuempl <- beaGet(beaSpecs400,asWide=FALSE)
bea_totempw <- beaGet(beaSpecs10,asWide=TRUE)
bea_manuempw <- beaGet(beaSpecs400,asWide=TRUE)
bea_manuempnw <- beaGet(beaSpecs500,asWide=TRUE)

# services 
# 1970 
bea_finempw<- beaGet(beaSpecs700,asWide=TRUE)
bea_servempw<- beaGet(beaSpecs800,asWide=TRUE)

services70_prep <- rbind(bea_finempw,bea_servempw)

services70 <- services70_prep %>% 
  dplyr::select(Code,GeoFips,DataValue_1970) %>%
  pivot_wider(names_from=Code,
              values_from=DataValue_1970) %>% 
  mutate(service70 = rowSums(across(where(is.numeric)))) %>% 
  dplyr::select(GeoFips,service70)
  
# 2007
numlist=seq(1000,1900,100)
services07_prep <- lapply(numlist,get_services)

services07_bind <- bind_rows(services07_prep)

beaemp70 <- bea_totempw %>% 
  dplyr::select(GeoFips,DataValue_1970)

service7007 <- services07_bind %>% 
  pivot_wider(names_from="Code",
              values_from=DataValue_2007) %>% 
  mutate(service07 = rowSums(across(where(is.numeric)))) %>% 
  #merge 70 services 
  left_join(.,services70) %>% 
  # merge 70 tot emp
  left_join(.,beaemp70) %>% 
  rename(totemps70=DataValue_1970) %>%
  dplyr::select(GeoFips,service70,service07,totemps70)

## get suppressed values - fill in the suppressed values from one year -+ 

bea_sup <- read.csv("bea_suppressed.csv",skip=4,stringsAsFactors = F)
bea_sup01 <- read.csv("bea_sup01.csv",skip=4,stringsAsFactors = F)

## set manu employment to 1970 if there is data, 10 if not 
suplist_69 <- bea_sup %>% 
  filter(X1969=="(D)") %>% 
  mutate(code=ifelse(X1970!="(D)",X1970,10)) %>%
  dplyr::select(GeoFips,code)

## set manu to 69 or 71/10, for 70s 
suplist_70 <- bea_sup %>% 
  filter(X1970=="(D)") %>% 
  mutate(code=ifelse(X1969!="(D)",X1969,
                     ifelse(X1971!="(D)",X1971,10))) %>% 
  dplyr::select(GeoFips,code)

# set manu to 99, for 2000s
suplist_00 <- bea_sup %>% 
  left_join(.,bea_sup01) %>% 
  filter(X2000=="(D)")  %>% 
  mutate(code=ifelse(X1999!="(D)",X1999,
                     ifelse(X1999=="(D)"&X2001!="(D)",X2001,
                            ifelse(X1999=="(D)"&X2001=="(D)"&X1998!="(D)",X1998,20)))) %>%
  dplyr::select(GeoFips,code)

# set manu employment to 2000s or 98s, for 99 
suplist_99 <- bea_sup %>% 
  filter(X1999=="(D)") %>% 
  mutate(code=ifelse(X1998!="(D)",X1998,
                     ifelse(X1998=="(D)"&X2000!="(D)",X2000,20)))

# Creating Bartik instrument 

## calculate total employment 69 county 
bea_tot_c_69 <- bea_totempl %>% 
  filter(TimePeriod==1969) %>% 
  rename(totemp69=DataValue) %>% 
  dplyr::select(GeoFips,totemp69)


## calculate manu employment 69
## county 
bea_manu_c_69 <- bea_manuempl %>% 
  filter(TimePeriod==1969) %>% 
  left_join(.,suplist_69) %>% 
  mutate(DataValue=as.numeric(ifelse(!is.na(code),code,DataValue))) %>% 
  rename(manuemp69=DataValue) %>% 
  dplyr::select(GeoFips,manuemp69)


## Calculate total employment 69 in all counties but county j
## county 

bea_totexcl_c_69 <- bea_totempl %>% 
  filter(TimePeriod==1969) %>% 
  mutate(total69=sum(DataValue,na.rm=T)) %>% 
  mutate(totemp69excl=total69-DataValue) %>% 
  dplyr::select(GeoFips,totemp69excl)

### Do the same for the share of 2000
## calculate total employment 99 county 
bea_tot_c_99 <- bea_totempl %>% 
  filter(TimePeriod==1999) %>% 
  rename(totemp99=DataValue) %>% 
  dplyr::select(GeoFips,totemp99)

## calculate manu employment 99
## county 
bea_manu_c_99 <- bea_manuempl %>% 
  filter(TimePeriod==1999) %>% 
  left_join(.,suplist_99) %>% 
  mutate(DataValue=as.numeric(ifelse(!is.na(code),code,DataValue))) %>% 
  rename(manuemp99=DataValue) %>% 
  dplyr::select(GeoFips,manuemp99)

## Calculate total employment 99 in all counties but county j
## county 

bea_totexcl_c_99 <- bea_totempl %>% 
  filter(TimePeriod==1999) %>% 
  mutate(total99=sum(DataValue,na.rm=T)) %>% 
  mutate(totemp99excl=total99-DataValue) %>% 
  dplyr::select(GeoFips,totemp99excl)


## Calculate manu employment loss 1970-2000

bea_manuloss7000 <- bea_manuempw %>%
  left_join(.,suplist_00) %>% 
  mutate(DataValue_2000=as.numeric(ifelse(!is.na(code),code,DataValue_2000))) %>% 
  dplyr::select(-code) %>% 
  left_join(.,suplist_70) %>% 
  mutate(DataValue_1970=as.numeric(ifelse(!is.na(code),code,DataValue_1970))) 

# calculate change in manu employment at the county level 
bea_manuloss_c_7000 <- bea_manuloss7000 %>% 
  mutate(manuloss7000=DataValue_1970-DataValue_2000)  %>%
  mutate(totalmanuloss7000=sum(manuloss7000),
         manuloss7000excl=totalmanuloss7000-manuloss7000) %>% 
  dplyr::select(GeoFips,manuloss7000,manuloss7000excl)

## Calculate manu employment loss 1970-2007/2016
## 08014 - Broomfield County created after 2001, doesn't have 70 manu 
## Remove Alaska from estimations because of county changes through 70-07 
## 55901 is combination of Shawano + Menominee WI - I calculate them separately 
bea_sup0716 <- read.csv("bea_sup0716.csv",skip=4,stringsAsFactors = F)

suplist_07 <- bea_sup0716 %>% 
  filter(X2007=="(D)")  %>% 
  mutate(code=ifelse(X2006!="(D)",X2006,
                     ifelse(X2006=="(D)"&X2008!="(D)",X2008,20))) %>% 
  dplyr::select(GeoFips,code)
  
  
suplist_16 <- bea_sup0716 %>% 
  filter(X2016=="(D)")  %>% 
  mutate(code=ifelse(X2015!="(D)",X2015,
                     ifelse(X2015=="(D)"&X2017!="(D)",X2017,20))) %>% 
  dplyr::select(GeoFips,code)

manu70 <- bea_manuempw %>% 
  dplyr::select(GeoFips,DataValue_1970)

beamanuloss700716 <- bea_manuempnw %>% 
  left_join(.,manu70) %>% 
  left_join(.,suplist_07) %>% 
  mutate(DataValue_2007=as.numeric(ifelse(!is.na(code),code,DataValue_2007))) %>% 
  dplyr::select(-code) %>% 
  left_join(.,suplist_16) %>% 
  mutate(DataValue_2016=as.numeric(ifelse(!is.na(code),code,DataValue_2016)))

## county: calculate shift part manu losses excluding the county 70/07/16
beamanuloss_c_700716 <- beamanuloss700716 %>% 
  mutate(manuloss7007=DataValue_1970-DataValue_2007,
         manuloss7016=DataValue_1970-DataValue_2016) %>% 
  mutate(totalmanuloss7007=sum(manuloss7007,na.rm=T),
         manuloss7007excl=totalmanuloss7007-manuloss7007,
         totalmanuloss7016=sum(manuloss7016,na.rm=T),
         manuloss7016excl=totalmanuloss7016-manuloss7016) %>% 
  dplyr::select(GeoFips,manuloss7007,manuloss7016,manuloss7007excl,manuloss7016excl)

## calculate manuloss from 2000 to 07/16

manu00 <- bea_manuempw %>% 
  dplyr::select(GeoFips,DataValue_2000)

beamanuloss000716 <- bea_manuempnw %>%
  left_join(.,manu00) %>% 
  left_join(.,suplist_00) %>% 
  mutate(DataValue_2000=as.numeric(ifelse(!is.na(code),code,DataValue_2000))) %>% 
  dplyr::select(-code) %>% 
  left_join(.,suplist_07) %>% 
  mutate(DataValue_2007=as.numeric(ifelse(!is.na(code),code,DataValue_2007))) %>% 
  dplyr::select(-code) %>% 
  left_join(.,suplist_16) %>% 
  mutate(DataValue_2016=as.numeric(ifelse(!is.na(code),code,DataValue_2016)))

## county: calculate shift part manu losses excluding the county 00/07/16
beamanuloss_c_000716 <- beamanuloss000716 %>% 
  # calculate manufacturing losses in the county
  mutate(manuloss0007=DataValue_2000-DataValue_2007,
         manuloss0016=DataValue_2000-DataValue_2016) %>% 
  # calculate national manufacturing losses - county manufacturing losses 
  mutate(totalmanuloss0007=sum(manuloss0007,na.rm=T),
         manuloss0007excl=totalmanuloss0007-manuloss0007,
         totalmanuloss0016=sum(manuloss0016,na.rm=T),
         manuloss0016excl=totalmanuloss0016-manuloss0016) %>% 
  dplyr::select(GeoFips,manuloss0007,manuloss0016,manuloss0007excl,manuloss0016excl)
  

## combine all manu employment measures to make Bartik measure + IV
trade_exposure_c_7000prep <- bea_manu_c_69 %>% 
  left_join(.,bea_tot_c_69) %>%
  left_join(.,bea_manuloss_c_7000) %>% 
  left_join(.,bea_totexcl_c_69) %>% 
  left_join(.,beamanuloss_c_700716) %>% 
# add 99 employment as well to calculate 00-07/16
  left_join(.,bea_manu_c_99) %>% 
  left_join(.,bea_tot_c_99) %>% 
  left_join(.,beamanuloss_c_000716) %>% 
  left_join(.,bea_totexcl_c_99)

# Create Shift-share instruments 
trade_exposure_c_7000 <- trade_exposure_c_7000prep %>% 
  # create the Bartik instrument 
  mutate(# Bartik IV for 70-00
    # initial county share
    bartik_iv1=(manuemp69/totemp69)*100,
    # national shift - manu losses as a share of total national employment (county excluded)
    bartik_iv2=(manuloss7000excl/totemp69excl)*100,
    # Bartik shift-share
    bartik_iv_00=bartik_iv1*bartik_iv2,
        # Bartik IV for 70-07
         bartik_iv2_07=(manuloss7007excl/totemp69excl)*100,
         bartik_iv_07=bartik_iv1*bartik_iv2_07,
        # Bartik IV for 70-16
         bartik_iv2_16=(manuloss7016excl/totemp69excl)*100,
        bartik_iv_16=bartik_iv1*bartik_iv2_16,
      # Bartik IV for 00-07
    bartik_iv199=(manuemp99/totemp99)*100,
      bartik_iv2_0007=(manuloss0007excl/totemp99excl)*100,
    bartik_iv_0007=bartik_iv199*bartik_iv2_0007,
    # Bartik IV for 00-16
    bartik_iv2_0016=(manuloss0016excl/totemp99excl)*100,
    bartik_iv_0016=bartik_iv199*bartik_iv2_0016,
        # IV for 70-00
         manuloss7000_pw=(manuloss7000/totemp69)*100,
        # IV for 70-07
        manuloss7007_pw=(manuloss7007/totemp69)*100,
        # IV for 70-16
        manuloss7016_pw=(manuloss7016/totemp69)*100,
        # IV for 00-07
        manuloss0007_pw=(manuloss0007/totemp99)*100,
        # IV for 00-16
        manuloss0016_pw=(manuloss0016/totemp99)*100
    ) %>% 
  # filter out Alaska counties because their county boundaries changed so much
  mutate(statefips=substr(GeoFips,1,2)) %>% 
  filter(statefips!="02") %>%
  mutate(FIPS=as.numeric(GeoFips)) %>% 
  dplyr::select(FIPS,bartik_iv_00,bartik_iv_07,bartik_iv_16,bartik_iv_0007,
                bartik_iv_0016,ends_with("_pw")) %>% 
  # Fix Maui,Shannon,Bedford FIPS changes  code 
  mutate(FIPS=ifelse(FIPS==15901,15009,
                     ifelse(FIPS==46102,46113,FIPS)))

## Create total manu employment share 70 and services vars 
manuser <- service7007 %>% 
  left_join(.,manu70) %>%
  rename(manu70=DataValue_1970) %>% 
  mutate(permanu70=(manu70/totemps70)*100,
         perser70=(service70/totemps70)*100,
         d_ser7007=service07-service70,
         d_ser7007_per=(d_ser7007/totemps70)*100) %>% 
  mutate_if(is.numeric,coalesce,0) %>% 
  mutate(across(.cols = everything(), ~ ifelse(is.infinite(.x), NA, .x))) %>% 
  mutate(FIPS=as.numeric(GeoFips)) %>% 
  dplyr::select(FIPS,permanu70,perser70,d_ser7007_per)

# Top 10 counties that were the most vulnerable to manufacturing changes: 
# a) Calhoun, Arkansas b) Hancock, WV c) Kendall, IL d) Cameron, PA e) McDowell, NC  
#f <- trade_exposure_c_7000 %>% 
 #  arrange(desc(bartik_iv_07))

## Read in county-level things to create county-level ADH measure 
## The creation of this dataset is from Pierce 2022 paper - in replication... folder stata file
## ecj - county-industry employment ec county employment 
adh_county <- read.dta13("adh_1990w_countylevel.dta")

adh_county_m <- adh_county %>% 
  dplyr::select(sic4,countyid,ecj,ec,tv_1990,tm_1990,tx_1990,musa2000,moth2000,musa2007,moth2007,
                mexusa2000,mexoth2000,lowus2000,lowoth2000,mexusa2007,mexoth2007,lowus2007,lowoth2007,
                mexusa1994,mexoth1994) %>% 
  rename(FIPS=countyid)

# county-level trade shock 
adh_c <- adh_county_m %>%
  # mutate variables for each industry
  mutate(ip_usa_com_2000=((ecj/ec)*100)*(((musa2000*1000)/(tv_1990 + tm_1990 - tx_1990))*100),
         ip_d9_com_2000=((ecj/ec)*100)*(((moth2000*1000)/(tv_1990 + tm_1990 - tx_1990))*100),
         ip_usa_com_2007=((ecj/ec)*100)*(((musa2007*1000)/(tv_1990 + tm_1990 - tx_1990))*100),
         ip_d9_com_2007=((ecj/ec)*100)*(((moth2007*1000)/(tv_1990 + tm_1990 - tx_1990))*100),
         ip_mexus_com_2000=((ecj/ec)*100)*(((mexusa2000*1000)/(tv_1990 + tm_1990 - tx_1990))*100),
         ip_mexoth_com_2000=((ecj/ec)*100)*(((mexoth2000*1000)/(tv_1990 + tm_1990 - tx_1990))*100),
         ip_mexus_com_2007=((ecj/ec)*100)*(((mexusa2007*1000)/(tv_1990 + tm_1990 - tx_1990))*100),
         ip_mexoth_com_2007=((ecj/ec)*100)*(((mexoth2007*1000)/(tv_1990 + tm_1990 - tx_1990))*100),
         ip_lowus_com_2000=((ecj/ec)*100)*(((lowus2000*1000)/(tv_1990 + tm_1990 - tx_1990))*100),
         ip_lowoth_com_2000=((ecj/ec)*100)*(((lowoth2000*1000)/(tv_1990 + tm_1990 - tx_1990))*100),
         ip_lowus_com_2007=((ecj/ec)*100)*(((lowus2007*1000)/(tv_1990 + tm_1990 - tx_1990))*100),
         ip_lowoth_com_2007=((ecj/ec)*100)*(((lowoth2007*1000)/(tv_1990 + tm_1990 - tx_1990))*100),
         ip_mexus_com_1994=((ecj/ec)*100)*(((mexusa1994*1000)/(tv_1990 + tm_1990 - tx_1990))*100),
         ip_mexoth_com_1994=((ecj/ec)*100)*(((mexoth1994*1000)/(tv_1990 + tm_1990 - tx_1990))*100)) %>% 
  group_by(FIPS) %>% 
  summarise(across(ip_usa_com_2000:ip_mexoth_com_1994,sum,na.rm=T)) %>% 
  mutate(d_ip_usa_com=ip_usa_com_2007-ip_usa_com_2000,
         d_ip_d9_com=ip_d9_com_2007-ip_d9_com_2000,
         d_ip_mexusa_com=ip_mexus_com_2007-ip_mexus_com_2000,
         d_ip_mexoth_com=ip_mexoth_com_2007-ip_mexoth_com_2000,
         d_ip_lowus_com=ip_lowus_com_2007-ip_lowus_com_2000,
         d_ip_lowoth_com=ip_lowoth_com_2007-ip_lowoth_com_2000,
         d_ip_mexusa94_com=ip_mexus_com_2007-ip_mexus_com_1994,
         d_ip_mexoth94_com=ip_mexoth_com_2007-ip_mexoth_com_1994) %>% 
  dplyr::select(FIPS,starts_with("d"))

##### CONTROLS 

# Average Unemployment 2000/2008-2016

load("unempcounty9917.Rdata")

unempyear$statefips <- as.numeric(unempyear$statefips)
unempyear$countyfips <- as.numeric(unempyear$countyfips)

unempyear$Laborforce <- as.numeric(gsub(",","",unempyear$Laborforce))
unempyear$Unemployed <- as.numeric(gsub(",","",unempyear$Unemployed))

# Average Unemployment for sample years 
# Louisiana has 5 missing counties for 2005 and 2006
ave_unemp_0016 <- unempyear %>% 
  mutate(countyfips=sprintf("%03d",as.numeric(countyfips)),
         FIPS=as.numeric(paste0(statefips,countyfips)),
         unemp=as.numeric(unemp),
         year=as.numeric(Year)) %>% 
  filter(Year>1999&Year<2017) %>% 
  #exclude PR and AK 
  filter(statefips!=72&statefips!=2) %>% 
  group_by(FIPS) %>% 
  summarize(ave_unemp_0016_c=mean(unemp,na.rm=T)) 

ave_unemp_0816 <- unempyear %>% 
  mutate(countyfips=sprintf("%03d",as.numeric(countyfips)),
         FIPS=as.numeric(paste0(statefips,countyfips)),
         unemp=as.numeric(unemp),
         year=as.numeric(Year)) %>% 
  filter(Year>2007&Year<2017) %>% 
  #exclude PR and AK 
  filter(statefips!=72&statefips!=2) %>% 
  group_by(FIPS) %>% 
  summarize(ave_unemp_0816_c=mean(unemp,na.rm=T))

ave_unemp_000816_c <- left_join(ave_unemp_0016,ave_unemp_0816) %>% 
  # fix Shannon - Oglala county SD
  mutate(FIPS=ifelse(FIPS==46102,46113,FIPS))

# Competitiveness 1972 PVI 
## Republican two-party vote share weighted national average 64+68 (Goldwater and Nixon) - 0.25*38.65+0.75*50.50 = 47.53 - R ave vote
## Determine Cook scores depending on the two-party vote share Republican candidates received in each county 

## Dave Leip 64-68 vote shares 
## county name fips crosswalk is from https://www.nrcs.usda.gov/wps/portal/nrcs/detail/national/home/?cid=nrcs143_013697

cross_fips <- read.csv("countynamefips.csv")

cross_fips_m <- cross_fips %>% 
  mutate(countyname=paste0(Name," ","-"," ",State)) %>% 
  dplyr::select(countyname,FIPS) %>% 
  mutate(countyname=toupper(countyname))

presvote_6468 <- read.csv("Presidential Election Results - Vote Count from the United States Election Results Database.csv",skip=8,header=T)

names(presvote_6468) <- c("countyname","statename","demvote68","repvote68","demvote64","repvote64")

## fix county names for merging
## 107 close elections
presvote_6468_fips <- presvote_6468 %>% 
  mutate(countyname=trimws(countyname),
         countyname=gsub("Saint","St",countyname),
         countyname=toupper(countyname),
         countyname=gsub("DEKALB","DE KALB",countyname),
         countyname=ifelse(countyname=="DUPAGE - IL", "DU PAGE - IL",
                                  ifelse(countyname=="DEWITT - IL","DE WITT - IL",
                                         ifelse(countyname=="STE GENEVIEVE - MO","STE. GENEVIEVE - MO",
                                                ifelse(countyname=="OBRIEN - IA","O BRIEN - IA",
                                                       ifelse(countyname=="DESOTO - MS", "DE SOTO - MS",
                                                              ifelse(countyname=="LAGRANGE - IN","LA GRANGE - IN",
                                                                     ifelse(countyname=="LAMOURE - ND","LA MOURE - ND",
                                                                            ifelse(countyname=="DISTRICT OF COLUMBIA - DC","WASHINGTON - DC",countyname))))))))) %>% 
          
  left_join(.,cross_fips_m) %>% 
  filter(!is.na(FIPS))

comp_c <-presvote_6468_fips %>% 
  # calculate PVI
  mutate(totvote64=repvote64+demvote64,
         totvote68=repvote68+demvote68,
         natave=0.4753,
         repave=((repvote64/totvote64)*0.25+(repvote68/totvote68)*0.75)/2,
         # + Democratic leaning, - Republican leaning
         pvi_prep=(natave-repave)*100,
         #comp or not
         comp=ifelse(pvi_prep<0.501&pvi_prep>-0.501,1,0),
         # how competitive continuous - take the square of raw pvi
         comp_cont=pvi_prep^2,
         comp_cont_sd=as.vector(scale(comp_cont)),
         comp_cont_sd=comp_cont_sd*-1,
         comp_cont_scaled=scales::rescale(comp_cont,to=c(1,0)),
         # replean demlean comp -5 to 5
         comp_alt=ifelse(pvi_prep<5&pvi_prep>-5,"comp",
                         ifelse(pvi_prep>5,"demlean",
                                ifelse(pvi_prep<-5,"replean")))) %>%
  dplyr::select(FIPS,pvi_prep,starts_with("comp"))

# Census 1970 
# census70_vars.txt includes explanation for the variables

census70 <- read.csv("census70.csv")

census70_vars_c <- census70 %>% 
  mutate(per_male70=SE_T004_002/SE_T004_001,
         per_old70=SE_T008_005/SE_T008_001,
         per_white70=SE_T012_002/SE_T012_001,
         per_college70=SE_T029_005/SE_T029_001,
         per_male_highsc70=SE_T030_004/SE_T030_001,
         per_white_highsc70=SE_T032_004/SE_T032_001,
         unemprate70=SE_T056_003/SE_T056_001,
         per_vacant70=SE_T109_003/SE_T109_001,
         ) %>% 
  rename(avehouseval70=SE_T122_002,
         popdens70=SE_T002_001,
         totpop70=SE_T003_002,
         labforce70=SE_T052_001,
         FIPS=Geo_FIPS) %>% 
  dplyr::select(FIPS,totpop70,popdens70,labforce70,avehouseval70,per_male70,per_old70,per_white70,
                per_college70,per_male_highsc70,per_white_highsc70,unemprate70,per_vacant70) %>% 
  mutate(across(per_male70:per_vacant70, function(x) x*100)) %>% 
  # change Miami-Dade code, St Louis
  mutate(FIPS=ifelse(FIPS==12025,12086,
                     ifelse(FIPS==29193,29186,FIPS)))


# Census 2000 

census00 <- read.csv("census00.csv")

census00_vars_c <- census00 %>% 
  mutate(per_male00=SE_T005_002/SE_T005_001,
         per_old00=SE_T009_005/SE_T009_001,
         per_white00=SE_T014_002/SE_T014_001,
         per_white_nh00=SE_T015_003/SE_T015_001,
         per_college00=(SE_T043_005+SE_T043_006+SE_T043_008+SE_T043_007)/SE_T043_001,
         per_male_highsc00=SE_T044_003/SE_T044_001,
         per_white_highsc00=(SE_T046_005+SE_T046_006)/SE_T046_001,
         unemprate00=SE_T073_003/SE_T073_001,
         per_vacant00=SE_T157_003/SE_T157_001) %>% 
  rename(avehouseval00=SE_T163_001,
         popdens00=SE_T003_001,
         totpop00=SE_T003_002,
         labforce00=SE_T072_001,
         FIPS=Geo_FIPS) %>% 
  dplyr::select(FIPS,totpop00,popdens00,labforce00,avehouseval00,per_male00,per_old00,per_white00,
                per_college00,per_male_highsc00,per_white_highsc00,unemprate00,per_vacant00,per_white_nh00) %>% 
  mutate(across(per_male00:per_white_nh00, function(x) x*100))

#### First, transfer 98-2008 employment in NAICS6 code to SIC4 
#### Then, aggregate employment to calculate att SIC2 code industries 
naics9808 <- read.dta13("aycan_ctyind6dg_9808.dta")

#head(naics9808m)
naics9808m <- naics9808 %>% 
  #mutate(manu=substr(naics,1,2)) %>% 
  #filter(manu>30&manu<34) %>% 
  dplyr::select(fipstate,fipscty,naics,emp_imputed,myear) %>% 
  mutate(naics6=as.numeric(naics))

## merge naics to sic crosswalk 

cross_naicsic <- read.dta13("cw_n97_s87.dta")

sic9808 <- naics9808m %>% 
  left_join(.,cross_naicsic) 

sic9808_m <- sic9808 %>% 
  group_by(fipstate,fipscty,myear,sic4) %>% 
  summarise(emp=sum(emp_imputed*weight,na.rm=T)) %>% 
  ## aggregate to 2-digit sic code 
  mutate(sic2=substr(sic4,1,2)) %>% 
  group_by(fipstate,fipscty,myear,sic2) %>% 
  summarise(emp=sum(emp,na.rm=T)) %>% 
  filter(sic2>19&sic2<40)

## 1986-1997 

sic8697 <- read.dta13("aycan_ctyind4dg_8697.dta")

sic8697_m <- sic8697 %>% 
  filter(sic4>1999&sic4<4000) %>% 
  #arrange(fipstate,fipscty) %>% 
  #filter(myear==1986)
  mutate(sic2=substr(sic4,1,2)) %>% 
  group_by(fipstate,fipscty,myear,sic2) %>% 
  summarise(emp=sum(emp_imputed,na.rm=T))

## bring all years together - this data includes manufacturing employment only 
sic8608 <- rbind(sic8697_m,sic9808_m)

# total county employment 87+97
ctyemp <- sic8697 %>% 
  filter(myear==1986|myear==1997) %>% 
  group_by(fipstate,fipscty,myear) %>% 
  summarise(totemp=sum(emp_imputed,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(fipscty=sprintf("%03d",as.numeric(fipscty)),
         fips=as.numeric(paste0(fipstate,fipscty))) %>% 
  dplyr::select(fips,myear,totemp)

ctyemp_w <- ctyemp %>% 
  pivot_wider(names_from=myear,
            values_from=totemp,
            names_prefix="totemp")

## Creating the independent variable - change in manufacturing employment 87-07, 00-07 
cbp_iv <- sic8608 %>% 
  filter(myear==1987|myear==2000|myear==2007) %>% 
  mutate(fipscty=sprintf("%03d",as.numeric(fipscty)),
         fips=as.numeric(paste0(fipstate,fipscty))) %>% 
  group_by(fips,myear) %>% 
  summarise(manuemp=sum(emp,na.rm=T)) %>% 
  pivot_wider(names_from=myear,
              values_from=manuemp,
              names_prefix="manuemp") %>% 
  mutate(d_manuemp_8707=manuemp1987-manuemp2007,
         d_manuemp_0007=manuemp2000-manuemp2007) %>% 
  #merge total employment 
  left_join(.,ctyemp_w) %>% 
  mutate_if(is.numeric,coalesce,0) %>% 
  mutate(manuloss_cbp_8707_pw=(d_manuemp_8707/totemp1986)*100,
         manuloss_cbp_0007_pw=(d_manuemp_0007/totemp1997)*100)%>% 
  dplyr::select(fips,starts_with("manuloss")) %>%  
  mutate_if(is.numeric,coalesce,0)

    

## Creating county initial share of manufacturing employment at time t-1 (1986,1997) for share part
## calculate initial share in 1986/1999 - subindustry/county total emp 
base_manu  <- sic8608 %>% 
  ungroup() %>% 
  filter(myear==1986|myear==1997) %>%  
  # join total employment at county level
  mutate(fipscty=sprintf("%03d",as.numeric(fipscty)),
         fips=as.numeric(paste0(fipstate,fipscty))) %>%  
  left_join(.,ctyemp) %>% 
  mutate(initshare=(emp/totemp)*100) %>% 
  dplyr::select(myear,fips,sic2,initshare) %>% 
  pivot_wider(names_from=myear,
              values_from=initshare,
              values_fill=0,
              names_prefix="initshare")
  
### national industry total employment 
  
natindemp <- sic8608 %>% 
  ungroup() %>% 
  filter(myear==1986|myear==1997) %>% 
  mutate(fipscty=sprintf("%03d",as.numeric(fipscty)),
         fips=as.numeric(paste0(fipstate,fipscty))) %>%  
  group_by(myear,sic2) %>% 
  summarise(indemp=sum(emp,na.rm=T)) 

countyindemp <- sic8608 %>% 
  ungroup() %>% 
  filter(myear==1986|myear==1997) %>% 
  mutate(fipscty=sprintf("%03d",as.numeric(fipscty)),
         fips=as.numeric(paste0(fipstate,fipscty))) %>% 
  dplyr::select(fips,myear,sic2,emp)

countyindemp2 <- sic8608 %>% 
  ungroup() %>% 
  filter(myear==1987|myear==2000|myear==2007) %>% 
  mutate(fipscty=sprintf("%03d",as.numeric(fipscty)),
         fips=as.numeric(paste0(fipstate,fipscty))) %>% 
  dplyr::select(fips,myear,sic2,emp)

## Create national industry employment changes 
change_indemp <- sic8608 %>%  
  mutate(fipscty=sprintf("%03d",as.numeric(fipscty)),
         fips=as.numeric(paste0(fipstate,fipscty))) %>% 
  filter(myear==1987|myear==2000|myear==2007) %>% 
  ungroup() %>% 
  dplyr::select(fips,myear,sic2) %>% 
  tidyr::expand(fips,myear,sic2)  %>% 
  # merge county industry employment numbers 
  left_join(.,countyindemp2) %>% 
  mutate_if(is.numeric,coalesce,0) %>% 
  group_by(myear,sic2) %>% 
  # calculate total employment in j by year 
  mutate(totemp=sum(emp,na.rm=T)) %>% 
  # exclude county employment in j 
  mutate(empexc=totemp-emp) %>% 
  dplyr::select(fips,myear,sic2,empexc) %>% 
  pivot_wider(names_from=myear,
              values_from=empexc,
              names_prefix="empexc")


## Create sheet that includes all county-sic code combinations - some subindustries do not exist in counties
## First create the national industry denominator - National subindustry employment except county subindustry employment
# Then merge other measures 

base_cbp_870007 <- sic8608 %>% 
  # First calculate the second part denominator- base national industry employment excluding county in j
  mutate(fipscty=sprintf("%03d",as.numeric(fipscty)),
         fips=as.numeric(paste0(fipstate,fipscty))) %>% 
  filter(myear==1986|myear==1997) %>% 
  ungroup() %>% 
  dplyr::select(fips,myear,sic2) %>% 
  tidyr::expand(fips,myear,sic2) %>% 
  # merge national industry numbers 
  left_join(.,natindemp) %>% 
  # merge county industry employment numbers 
  left_join(.,countyindemp) %>% 
  mutate_if(is.numeric,coalesce,0) %>% 
  mutate(totindexc=indemp-emp)  %>% 
  dplyr::select(fips,myear,sic2,totindexc) %>% 
  pivot_wider(names_from=myear,
              values_from=totindexc,
              names_prefix="natind") %>% 
  # Second merge the numerator - change in national employment in industry j excluding county c
  left_join(.,change_indemp) %>% 
  # Calculate second part of Bartik instrument - larger numbers include larger decline in manu employment 
  mutate(shift_8707=empexc1987-empexc2007,
         shift_0007=empexc2000-empexc2007,
         # calculate per worker 
         shift_8707_pw=(shift_8707/natind1986)*100,
         shift_0007_pw=(shift_0007/natind1997)*100) %>% 
  # Finally, bring in the initial share of manufacturing employment in industry j 
  left_join(.,base_manu) %>% 
  mutate_if(is.numeric,coalesce,0) %>% 
  # Create final bartik IV 
  mutate(bartik_8707=initshare1986*shift_8707_pw,
         bartik_0007=initshare1997*shift_0007_pw) %>% 
  mutate_if(is.numeric,coalesce,0) %>% 
  group_by(fips) %>% 
  summarise(bartik_8707=sum(bartik_8707),
            bartik_0007=sum(bartik_0007)) %>% 
  left_join(.,cbp_iv) %>%
  rename(FIPS=fips)

##### MERGE ALL 
## Because some counties moved into different dmas, merge counties with their respective dmas for each year 
# removing Alaska from the analysis because the county situation is too complicated
# use 2000 population proportions to split manu decline to 2008 counties 
# 2000 county population from GeoCorr 2000 
# VA counties - Albemarle split into Albemarle _ Cville in 1988 
# not including CBP in this because those are already appropriated according to new county criteria 
base_county_dma <- readRDS("base_dmacountysvw.Rdata") 


vacountypop_00 <- read.csv("geocorr2000_19AUG1114651.csv",skip=1,h=T)

names(vacountypop_00) <- c("FIPS","countyname","totpop","afact")

va_split <- vacountypop_00 %>% 
  mutate(group=ifelse(FIPS==51003|FIPS==51540,51901,
                      ifelse(FIPS==51005|FIPS==51580,51903,
                             ifelse(FIPS==51015|FIPS==51790|FIPS==51820,51907,
                                    ifelse(FIPS==51031|FIPS==51680,51911,
                                           ifelse(FIPS==51035|FIPS==51640,51913,
                                                  ifelse(FIPS==51053|FIPS==51570|FIPS==51730,51918,
                                                         ifelse(FIPS==51059|FIPS==51600|FIPS==51610,51919,
                                                                ifelse(FIPS==51069|FIPS==51840,51921,
                                                                       ifelse(FIPS==51081|FIPS==51595,51923,
                                                                              ifelse(FIPS==51089|FIPS==51690,51929,
                                                                                     ifelse(FIPS==51095|FIPS==51830,51931,
                                                                                            ifelse(FIPS==51121|FIPS==51750,51933,
                                                                                                   ifelse(FIPS==51143|FIPS==51590,51939,
                                                                                                          ifelse(FIPS==51149|FIPS==51670,51941,
                                                                                                                 ifelse(FIPS==51153|FIPS==51683|FIPS==51685,51942,
                                                                                                                        ifelse(FIPS==51770|FIPS==51775,51944,
                                                                                                                               ifelse(FIPS==51163|FIPS==51530|FIPS==51678,51945,
                                                                                                                                    ifelse(FIPS==51165|FIPS==51660,51947,
                                                                                                                                           ifelse(FIPS==51175|FIPS==51620,51949,
                                                                                                                                                  ifelse(FIPS==51177|FIPS==51630,51951,
                                                                                                                                                         ifelse(FIPS==51191|FIPS==51520,51953,
                                                                                                                                                                ifelse(FIPS==51195|FIPS==51720,51955,
                                                                                                                                                                       ifelse(FIPS==51199|FIPS==51735,51958,0
                                                                                                                                                                              )))))))))))))))))))))))) %>% 
  filter(group!=0) %>% 
  group_by(group) %>% 
  mutate(totsum=sum(totpop)) %>% 
  arrange(desc(group),FIPS) %>% 
  mutate(prop=round(totpop/totsum,2)) %>% 
  rename(countyold=FIPS,
         FIPS=group) %>% 
  left_join(.,trade_exposure_c_7000) 

merge_va <- va_split %>% 
  mutate_at(vars(bartik_iv_00:manuloss7016_pw),~.*prop) %>%
  rename_at(vars(-FIPS,-countyold), function(x) paste0(x,"_va")) %>% 
  rename(group=FIPS,
         FIPS=countyold) %>% 
  ungroup() %>% 
  dplyr::select(FIPS,starts_with("bartik"),starts_with("manuloss"))

base_08 <- base_county_dma %>% 
  filter(year==2008) %>% 
  filter(STATE!="AK") %>% 
  mutate(FIPS=as.numeric(county_fips2)) %>%
  # MERGE TRADE EXPOSURE 70-00/7/16
  left_join(.,trade_exposure_c_7000) %>% 
  # Assign separated VA counties exposure score based on how they divided
  full_join(.,merge_va) %>%
  mutate(bartik_iv_00=ifelse(is.na(bartik_iv_00),bartik_iv_00_va,bartik_iv_00),
         bartik_iv_07=ifelse(is.na(bartik_iv_07),bartik_iv_07_va,bartik_iv_07),
         bartik_iv_16=ifelse(is.na(bartik_iv_16),bartik_iv_16_va,bartik_iv_16),
         bartik_iv_0007=ifelse(is.na(bartik_iv_0007),bartik_iv_0007_va,bartik_iv_0007),
         bartik_iv_0016=ifelse(is.na(bartik_iv_0016),bartik_iv_0016_va,bartik_iv_0016),
         manuloss7000_pw=ifelse(is.na(manuloss7000_pw),manuloss7000_pw_va,manuloss7000_pw),
         manuloss7007_pw=ifelse(is.na(manuloss7007_pw),manuloss7007_pw_va,manuloss7007_pw),
         manuloss7016_pw=ifelse(is.na(manuloss7016_pw),manuloss7016_pw_va,manuloss7016_pw),
         manuloss0007_pw=ifelse(is.na(manuloss0007_pw),manuloss0007_pw_va,manuloss0007_pw),
         manuloss0016_pw=ifelse(is.na(manuloss0016_pw),manuloss0016_pw_va,manuloss0016_pw)) %>%
  # ignore other counties - either they do not have a 69 manu value or they have merged into counties that BEA accounts for already
  dplyr::select(-ends_with("va")) %>% 
  # MERGE CBP 
  left_join(.,base_cbp_870007) %>% 
  # some places just do not have any manufacturing employment to begin with 
  mutate_if(is.numeric,coalesce,0) %>% 
  # MERGE ADH 
  left_join(.,adh_c) %>% 
  # MERGE UNEMP 
  left_join(.,ave_unemp_000816_c) %>% 
  # MERGE COMP 
  # independent cities of VA do not have voting data from 1964 and 1968 
  # same with Cibola, Baltimore, La Paz, St Louis, 
  left_join(.,comp_c) %>% 
  # MERGE CENSUS 
  # if counties merge into others I ignore 
  left_join(.,census70_vars_c) %>% 
  left_join(.,census00_vars_c) %>% 
  left_join(.,manuser)

base_10 <- base_county_dma %>% 
  filter(year==2010) %>% 
  filter(STATE!="AK") %>% 
  mutate(FIPS=as.numeric(county_fips2)) %>%
  # MERGE TRADE EXPOSURE 70-00/7/16
  left_join(.,trade_exposure_c_7000) %>% 
  full_join(.,merge_va) %>%
  mutate(bartik_iv_00=ifelse(is.na(bartik_iv_00),bartik_iv_00_va,bartik_iv_00),
         bartik_iv_07=ifelse(is.na(bartik_iv_07),bartik_iv_07_va,bartik_iv_07),
         bartik_iv_16=ifelse(is.na(bartik_iv_16),bartik_iv_16_va,bartik_iv_16),
         bartik_iv_0007=ifelse(is.na(bartik_iv_0007),bartik_iv_0007_va,bartik_iv_0007),
         bartik_iv_0016=ifelse(is.na(bartik_iv_0016),bartik_iv_0016_va,bartik_iv_0016),
         manuloss7000_pw=ifelse(is.na(manuloss7000_pw),manuloss7000_pw_va,manuloss7000_pw),
         manuloss7007_pw=ifelse(is.na(manuloss7007_pw),manuloss7007_pw_va,manuloss7007_pw),
         manuloss7016_pw=ifelse(is.na(manuloss7016_pw),manuloss7016_pw_va,manuloss7016_pw),
         manuloss0007_pw=ifelse(is.na(manuloss0007_pw),manuloss0007_pw_va,manuloss0007_pw),
         manuloss0016_pw=ifelse(is.na(manuloss0016_pw),manuloss0016_pw_va,manuloss0016_pw)) %>%
  # ignore other counties - either they do not have a 69 manu value or they have merged into counties that BEA accounts for already
  dplyr::select(-ends_with("va")) %>% 
  # MERGE CBP 
  left_join(.,base_cbp_870007) %>% 
  mutate_if(is.numeric,coalesce,0) %>% 
  left_join(.,adh_c) %>% 
  # MERGE UNEMP 
  left_join(.,ave_unemp_000816_c) %>% 
  # MERGE COMP 
  # independent cities of VA do not have voting data from 1964 and 1968 
  # same with Cibola, Baltimore, La Paz, St Louis, 
  left_join(.,comp_c) %>% 
  # MERGE CENSUS 
  # if counties merge into others I ignore 
  left_join(.,census70_vars_c) %>% 
  left_join(.,census00_vars_c) %>% 
  left_join(.,manuser)

base_12 <- base_county_dma %>% 
  filter(year==2012) %>% 
  filter(STATE!="AK") %>% 
  mutate(FIPS=as.numeric(county_fips2)) %>%
  # MERGE TRADE EXPOSURE 70-00/7/16
  left_join(.,trade_exposure_c_7000) %>% 
  full_join(.,merge_va) %>%
  mutate(bartik_iv_00=ifelse(is.na(bartik_iv_00),bartik_iv_00_va,bartik_iv_00),
         bartik_iv_07=ifelse(is.na(bartik_iv_07),bartik_iv_07_va,bartik_iv_07),
         bartik_iv_16=ifelse(is.na(bartik_iv_16),bartik_iv_16_va,bartik_iv_16),
         bartik_iv_0007=ifelse(is.na(bartik_iv_0007),bartik_iv_0007_va,bartik_iv_0007),
         bartik_iv_0016=ifelse(is.na(bartik_iv_0016),bartik_iv_0016_va,bartik_iv_0016),
         manuloss7000_pw=ifelse(is.na(manuloss7000_pw),manuloss7000_pw_va,manuloss7000_pw),
         manuloss7007_pw=ifelse(is.na(manuloss7007_pw),manuloss7007_pw_va,manuloss7007_pw),
         manuloss7016_pw=ifelse(is.na(manuloss7016_pw),manuloss7016_pw_va,manuloss7016_pw),
         manuloss0007_pw=ifelse(is.na(manuloss0007_pw),manuloss0007_pw_va,manuloss0007_pw),
         manuloss0016_pw=ifelse(is.na(manuloss0016_pw),manuloss0016_pw_va,manuloss0016_pw)) %>%
  # ignore other counties - either they do not have a 69 manu value or they have merged into counties that BEA accounts for already
  dplyr::select(-ends_with("va"))  %>% 
  # MERGE CBP 
  left_join(.,base_cbp_870007) %>% 
  mutate_if(is.numeric,coalesce,0) %>% 
  left_join(.,adh_c) %>% 
  # MERGE UNEMP 
  left_join(.,ave_unemp_000816_c) %>% 
  # MERGE COMP 
  # independent cities of VA do not have voting data from 1964 and 1968 
  # same with Cibola, Baltimore, La Paz, St Louis, 
  left_join(.,comp_c) %>% 
  # MERGE CENSUS 
  # if counties merge into others I ignore 
  left_join(.,census70_vars_c)  %>% 
  left_join(.,census00_vars_c) %>% 
  left_join(.,manuser)
  

base_14 <- base_county_dma %>% 
  filter(year==2014) %>% 
  filter(STATE!="AK") %>% 
  mutate(FIPS=as.numeric(county_fips2)) %>%
  # MERGE TRADE EXPOSURE 70-00/7/16
  left_join(.,trade_exposure_c_7000) %>% 
  full_join(.,merge_va) %>%
  mutate(bartik_iv_00=ifelse(is.na(bartik_iv_00),bartik_iv_00_va,bartik_iv_00),
         bartik_iv_07=ifelse(is.na(bartik_iv_07),bartik_iv_07_va,bartik_iv_07),
         bartik_iv_16=ifelse(is.na(bartik_iv_16),bartik_iv_16_va,bartik_iv_16),
         bartik_iv_0007=ifelse(is.na(bartik_iv_0007),bartik_iv_0007_va,bartik_iv_0007),
         bartik_iv_0016=ifelse(is.na(bartik_iv_0016),bartik_iv_0016_va,bartik_iv_0016),
         manuloss7000_pw=ifelse(is.na(manuloss7000_pw),manuloss7000_pw_va,manuloss7000_pw),
         manuloss7007_pw=ifelse(is.na(manuloss7007_pw),manuloss7007_pw_va,manuloss7007_pw),
         manuloss7016_pw=ifelse(is.na(manuloss7016_pw),manuloss7016_pw_va,manuloss7016_pw),
         manuloss0007_pw=ifelse(is.na(manuloss0007_pw),manuloss0007_pw_va,manuloss0007_pw),
         manuloss0016_pw=ifelse(is.na(manuloss0016_pw),manuloss0016_pw_va,manuloss0016_pw)) %>%
  # ignore other counties - either they do not have a 69 manu value or they have merged into counties that BEA accounts for already
  dplyr::select(-ends_with("va"))  %>% 
  # MERGE CBP 
  left_join(.,base_cbp_870007) %>% 
  mutate_if(is.numeric,coalesce,0) %>% 
  left_join(.,adh_c) %>% 
  # MERGE UNEMP 
  left_join(.,ave_unemp_000816_c) %>% 
  # MERGE COMP 
  # independent cities of VA do not have voting data from 1964 and 1968 
  # same with Cibola, Baltimore, La Paz, St Louis, 
  left_join(.,comp_c) %>% 
  # MERGE CENSUS 
  # if counties merge into others I ignore 
  left_join(.,census70_vars_c)  %>% 
  left_join(.,census00_vars_c) %>% 
  left_join(.,manuser)

base_16 <- base_county_dma %>% 
  filter(year==2016) %>% 
  filter(STATE!="AK") %>% 
  mutate(FIPS=as.numeric(county_fips2)) %>%
  # MERGE TRADE EXPOSURE 70-00/7/16
  left_join(.,trade_exposure_c_7000) %>% 
  full_join(.,merge_va) %>%
  mutate(bartik_iv_00=ifelse(is.na(bartik_iv_00),bartik_iv_00_va,bartik_iv_00),
         bartik_iv_07=ifelse(is.na(bartik_iv_07),bartik_iv_07_va,bartik_iv_07),
         bartik_iv_16=ifelse(is.na(bartik_iv_16),bartik_iv_16_va,bartik_iv_16),
         bartik_iv_0007=ifelse(is.na(bartik_iv_0007),bartik_iv_0007_va,bartik_iv_0007),
         bartik_iv_0016=ifelse(is.na(bartik_iv_0016),bartik_iv_0016_va,bartik_iv_0016),
         manuloss7000_pw=ifelse(is.na(manuloss7000_pw),manuloss7000_pw_va,manuloss7000_pw),
         manuloss7007_pw=ifelse(is.na(manuloss7007_pw),manuloss7007_pw_va,manuloss7007_pw),
         manuloss7016_pw=ifelse(is.na(manuloss7016_pw),manuloss7016_pw_va,manuloss7016_pw),
         manuloss0007_pw=ifelse(is.na(manuloss0007_pw),manuloss0007_pw_va,manuloss0007_pw),
         manuloss0016_pw=ifelse(is.na(manuloss0016_pw),manuloss0016_pw_va,manuloss0016_pw)) %>%
  # ignore other counties - either they do not have a 69 manu value or they have merged into counties that BEA accounts for already
  dplyr::select(-ends_with("va"))  %>% 
  # MERGE CBP 
  left_join(.,base_cbp_870007) %>% 
  mutate_if(is.numeric,coalesce,0) %>% 
  left_join(.,adh_c) %>% 
  # MERGE UNEMP 
  left_join(.,ave_unemp_000816_c) %>% 
  # MERGE COMP 
  # independent cities of VA do not have voting data from 1964 and 1968 
  # same with Cibola, Baltimore, La Paz, St Louis, 
  left_join(.,comp_c) %>% 
  # MERGE CENSUS 
  # if counties merge into others I ignore 
  left_join(.,census70_vars_c)  %>% 
  left_join(.,census00_vars_c) %>% 
  left_join(.,manuser)

base_all <- rbind(base_08,base_10,base_12,base_14,base_16)

#totpop00_c <- read.csv("geocorr2000_19AUG1311276.csv",skip=1) 

#names(totpop00_c) <- c("FIPS","countyname","totpop00","afact")

# totpop00_c <- totpop00_c %>% 
  # dplyr::select(-countyname,-afact)

##### CREATE DATA SET 
dma_analysis_cum <- base_all %>% 
  # create weighted averages of variables
  # create proportion of county in dma from population 2000
  group_by(year,DMA,DMAINDEX) %>% 
  mutate(dmapop=sum(totpop00,na.rm=T),
         w=totpop00/dmapop) %>% 
  # get rid of the categorial variable + population variables 
  dplyr::select(-comp_alt,-totpop70,-labforce70,-totpop00,-labforce00) %>% 
  # I'm doing the ADH weighting here before the regression 
  mutate_at(vars(bartik_iv_00:d_ser7007_per), ~.*w) %>% 
  group_by(year,DMA,DMAINDEX) %>% 
  summarise(across(bartik_iv_00:d_ser7007_per,sum,na.rm=T)) %>% 
  # look at cumulative - I'm averaging for media markets that had their counties changed
  group_by(DMA,DMAINDEX) %>% 
  summarise(across(bartik_iv_00:d_ser7007_per,mean,na.rm=T)) %>% 
  rename(market=DMA) %>% 
  left_join(.,ads_cum)  %>% 
  # create outcome variable 
  mutate(per_trade=(tradead_hsp/totalad_hsp)*100,
         per_china=(chinaad_hsp/totalad_hsp)*100,
         per_jobs=(jobsad_hsp/totalad_hsp)*100,
         per_immig=(immad_hsp/totalad_hsp)*100,
         # Total D,R
         per_trade_D=(tradead_hsp_D/totalad_hsp_D)*100,
         per_trade_R=(tradead_hsp_R/totalad_hsp_R)*100,
         per_china_D=(chinaad_hsp_D/totalad_hsp_D)*100,
         per_china_R=(chinaad_hsp_R/totalad_hsp_R)*100,
         per_jobs_D=(jobsad_hsp_D/totalad_hsp_D)*100,
         per_jobs_R=(jobsad_hsp_R/totalad_hsp_R)*100,
         # fix NAs for 0 division
         per_trade_h=(tradead_house/totalad_house)*100,
         per_trade_s=(tradead_senate/totalad_senate)*100,
         per_trade_p=(tradead_pres/totalad_pres)*100,
         per_trade_h=ifelse(is.na(per_trade_h),0,per_trade_h),
         per_trade_s=ifelse(is.na(per_trade_s),0,per_trade_s),
         per_trade_p=ifelse(is.na(per_trade_p),0,per_trade_p),
         # house, senate, pres D,R 
         per_trade_h_D=(tradead_D_house/totalad_D_house)*100,
         per_trade_s_D=(tradead_D_senate/totalad_D_senate)*100,
         per_trade_p_D=(tradead_D_pres/totalad_D_pres)*100,
         per_trade_h_R=(tradead_R_house/totalad_R_house)*100,
         per_trade_s_R=(tradead_R_senate/totalad_R_senate)*100,
         per_trade_p_R=(tradead_R_pres/totalad_R_pres)*100,
         # fix NAs with 0 division
         per_trade_h_D=ifelse(is.na(per_trade_h_D),0,per_trade_h_D),
         per_trade_s_D=ifelse(is.na(per_trade_s_D),0,per_trade_s_D),
         per_trade_p_D=ifelse(is.na(per_trade_p_D),0,per_trade_p_D),
         per_trade_h_R=ifelse(is.na(per_trade_h_R),0,per_trade_h_R),
         per_trade_s_R=ifelse(is.na(per_trade_s_R),0,per_trade_s_R),
         per_trade_p_R=ifelse(is.na(per_trade_p_R),0,per_trade_p_R),
         # create interest ads 
        per_inttrade_hsp=(tradeintad_hsp/(totalad_hsp+tradeintad_hsp)*100),
         per_inttrade_h=(tradeintad_house/(totalad_house+tradeintad_house)*100),
         per_inttrade_s=(tradeintad_senate/(totalad_senate+tradeintad_senate)*100),
         per_inttrade_p=(tradeintad_pres/(totalad_pres+tradeintad_pres)*100),
        # fix NAs with division by 0
         per_inttrade_h=ifelse(is.na(per_inttrade_h),0,per_inttrade_h),
         per_inttrade_s=ifelse(is.na(per_inttrade_s),0,per_inttrade_s),
         per_inttrade_p=ifelse(is.na(per_inttrade_p),0,per_inttrade_p)
         ) %>% 
  #mutate(across(c(starts_with("manuloss"),starts_with("bartik")), function(x) x*100)) %>% 
  # create white majority dummy 
  mutate(whitedum=ifelse(per_white70>69,1,0))  %>% 
  # merge % trade ads 00-16 
  left_join(.,ads_cum_0016_m) %>% 
  # merge ads without 2016 
  left_join(.,ads_cum_0814_m) %>% 
  mutate(per_trade14=ifelse(is.na(per_trade14),0,per_trade14),
         totalad_hsp14=ifelse(is.na(totalad_hsp14),0,totalad_hsp14)) %>% 
  # merge 16-8 ad difference 
  left_join(.,add_diff_0816) %>% 
  mutate(per_trade816=ifelse(is.na(per_trade816),0,per_trade816),
         d_totalad816=ifelse(is.na(d_totalad816),0,d_totalad816)) %>% 
  # multiply with 100 
  mutate(per_trade00=per_trade00*100,
         per_trade14=per_trade14*100,
         per_trade816=per_trade816*100) %>% 
  # log # of total ads aired in a dma
  mutate(across(starts_with("totalad_hsp"),list(log=~log(.))))  %>% 
  # log of total ads that contain 0
  mutate(across(c(totalad_house,totalad_senate,totalad_pres,
                  totalad_D_house,totalad_R_house,
                  totalad_D_senate,totalad_R_senate,
                  totalad_D_pres,totalad_R_pres,
                  totalad_hsp_D,totalad_hsp_R,
                  totalad_hsp00,totalad_hsp14), list(log=~log(.+1))))

write.dta(dma_analysis_cum,"trade_dma_cum_analysis.dta")

### Create county-level data to probe mechanisms 
county_analysis_cum <- base_all %>% 
  arrange(FIPS,year,DMA) %>% 
  # aggregate at the county dma level 
  group_by(FIPS,STATE,state_fips,DMA,DMAINDEX) %>% 
  dplyr::select(-comp_alt) %>%
  summarise(across(bartik_iv_00:d_ser7007_per,mean,na.rm=T))  %>% 
  # merge ads - counties will see the ads that are shown in their respective media markets
  rename(market=DMA) %>%
  left_join(.,ads_cum) %>% 
  # create outcome variable 
  mutate(per_trade=(tradead_hsp/totalad_hsp)*100,
         per_china=(chinaad_hsp/totalad_hsp)*100,
         per_jobs=(jobsad_hsp/totalad_hsp)*100,
         per_immig=(immad_hsp/totalad_hsp)*100,
         # Total D,R
         per_trade_D=(tradead_hsp_D/totalad_hsp_D)*100,
         per_trade_R=(tradead_hsp_R/totalad_hsp_R)*100,
         per_china_D=(chinaad_hsp_D/totalad_hsp_D)*100,
         per_china_R=(chinaad_hsp_R/totalad_hsp_R)*100,
         per_jobs_D=(jobsad_hsp_D/totalad_hsp_D)*100,
         per_jobs_R=(jobsad_hsp_R/totalad_hsp_R)*100,
         # fix NAs for 0 division
         per_trade_h=(tradead_house/totalad_house)*100,
         per_trade_s=(tradead_senate/totalad_senate)*100,
         per_trade_p=(tradead_pres/totalad_pres)*100,
         per_trade_h=ifelse(is.na(per_trade_h),0,per_trade_h),
         per_trade_s=ifelse(is.na(per_trade_s),0,per_trade_s),
         per_trade_p=ifelse(is.na(per_trade_p),0,per_trade_p),
         # house, senate, pres D,R 
         per_trade_h_D=(tradead_D_house/totalad_D_house)*100,
         per_trade_s_D=(tradead_D_senate/totalad_D_senate)*100,
         per_trade_p_D=(tradead_D_pres/totalad_D_pres)*100,
         per_trade_h_R=(tradead_R_house/totalad_R_house)*100,
         per_trade_s_R=(tradead_R_senate/totalad_R_senate)*100,
         per_trade_p_R=(tradead_R_pres/totalad_R_pres)*100,
         # fix NAs with 0 division
         per_trade_h_D=ifelse(is.na(per_trade_h_D),0,per_trade_h_D),
         per_trade_s_D=ifelse(is.na(per_trade_s_D),0,per_trade_s_D),
         per_trade_p_D=ifelse(is.na(per_trade_p_D),0,per_trade_p_D),
         per_trade_h_R=ifelse(is.na(per_trade_h_R),0,per_trade_h_R),
         per_trade_s_R=ifelse(is.na(per_trade_s_R),0,per_trade_s_R),
         per_trade_p_R=ifelse(is.na(per_trade_p_R),0,per_trade_p_R),
         # create interest ads 
         per_inttrade_hsp=(tradeintad_hsp/(totalad_hsp+tradeintad_hsp)*100),
         per_inttrade_h=(tradeintad_house/(totalad_house+tradeintad_house)*100),
         per_inttrade_s=(tradeintad_senate/(totalad_senate+tradeintad_senate)*100),
         per_inttrade_p=(tradeintad_pres/(totalad_pres+tradeintad_pres)*100),
         # fix NAs with division by 0
         per_inttrade_h=ifelse(is.na(per_inttrade_h),0,per_inttrade_h),
         per_inttrade_s=ifelse(is.na(per_inttrade_s),0,per_inttrade_s),
         per_inttrade_p=ifelse(is.na(per_inttrade_p),0,per_inttrade_p)
  ) %>% 
  #mutate(across(c(starts_with("manuloss"),starts_with("bartik")), function(x) x*100)) %>% 
  # create white majority dummy 
  mutate(whitedum=ifelse(per_white70>50,1,0))  %>% 
  # merge % trade ads 00-16 
  left_join(.,ads_cum_0016_m) %>% 
  # merge ads without 2016 
  left_join(.,ads_cum_0814_m) %>% 
  mutate(per_trade14=ifelse(is.na(per_trade14),0,per_trade14),
         totalad_hsp14=ifelse(is.na(totalad_hsp14),0,totalad_hsp14)) %>% 
  # merge 16-8 ad difference 
  left_join(.,add_diff_0816) %>% 
  mutate(per_trade816=ifelse(is.na(per_trade816),0,per_trade816),
         d_totalad816=ifelse(is.na(d_totalad816),0,d_totalad816)) %>% 
  # multiply with 100 
  mutate(per_trade00=per_trade00*100,
         per_trade14=per_trade14*100,
         per_trade816=per_trade816*100) %>% 
  # log # of total ads aired in a dma
  mutate(across(starts_with("totalad_hsp"),list(log=~log(.))))  %>% 
  # log of total ads that contain 0
  mutate(across(c(totalad_house,totalad_senate,totalad_pres,
                  totalad_D_house,totalad_R_house,
                  totalad_D_senate,totalad_R_senate,
                  totalad_D_pres,totalad_R_pres,
                  totalad_hsp_D,totalad_hsp_R,
                  totalad_hsp00,totalad_hsp14), list(log=~log(.+1))))

write.dta(county_analysis_cum,"trade_county_cum_analysis.dta")







dma_analysis_cum_sd <- dma_analysis_cum %>% 
  ungroup() %>% 
  # standardize variables 
  mutate(across(c(starts_with("per_trade"),per_china,per_jobs,
                 ends_with("_D"),ends_with("_R"),
                 starts_with("manuloss"),starts_with("d_"),starts_with("bartik"),-d_totalad816), 
                 function(x) (x-mean(x)) / sd(x)))

write.dta(dma_analysis_cum_sd,"trade_dma_cum_analysis_sd.dta")

## Look at dmas over years 

dma_analysis_year <- base_all %>% 
  # create weighted averages of variables
  # create proportion of county in dma from population 2000 %>% 
  dplyr::select(-totpop) %>% 
  # merge 2000 county population 
  left_join(.,totpop00_c) %>% 
  group_by(year,DMA,DMAINDEX) %>% 
  mutate(dmapop=sum(totpop00,na.rm=T),
         w=totpop00/dmapop) %>% 
  dplyr::select(-comp_alt) %>% 
  mutate_at(vars(bartik_iv:avehousingval70), ~.*w) %>% 
  group_by(year,DMA,DMAINDEX) %>% 
  summarise(across(bartik_iv:avehousingval70,sum,na.rm=T)) 

# look at change from 2016 to 2008 

# exclude 2016 from the regressions 

# look at house, senate, presidential separately 





     





  

