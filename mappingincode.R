
options(scipen=999)

setwd("/workspaces/Dataharvest2025")

#install.packages("tidyverse")
#install.packages("tidyverse")
#install.packages("corrplot")
#install.packages("GGally")
#install.packages("sf")
#install.packages("leaflet")
#install.packages("tigris")
#install.packages("ggridges")
#install.packages("formattable")

library(tidyverse)
library(corrplot)
library(GGally)
library(sf)
library(leaflet)
library(tigris)
library(ggridges)
library(formattable)


uscounties<-read_sf("https://raw.githubusercontent.com/loganpowell/census-geojson/refs/heads/master/GeoJSON/500k/2022/county.json")

themap<-leaflet()%>%
  addTiles()%>%
  addPolygons(data = uscounties , 
              fillColor = "yellow", 
              fillOpacity = 0.5, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = ~NAME) %>%
  setView(-98.483330, 38.712046, zoom = 3) 

themap

election<-read_csv("voting24.csv")%>%
  select(state_name,county_fips,trump=per_gop)

dhdemos<-read_csv("dhdemos.csv")%>%
  mutate(GEOID=Geo_FIPS,
         state=Geo_STUSAB,
         county=Geo_NAME,
         pop=SE_A00002_001,
         density=SE_A00002_002,
         med_age=SE_A01004_001,
         white=SE_A04001_003/SE_A04001_001,
         black=SE_A04001_004/SE_A04001_001,
         nativeam=SE_A04001_005/SE_A04001_001,
         asian=(SE_A04001_006+SE_A04001_007)/SE_A04001_001,
         hispanic=SE_A04001_010/SE_A04001_001,
         married_hh=SE_A10008_003/SE_A10008_001,
         hhsize=SE_A10003_001,
         college=SE_A12001_005/SE_A12001_001,
         nilf=SE_A17002_007/SE_A17002_001,
         unemp=SE_A17002_006/SE_A17002_004,
         income=SE_A14006_001,
         retirement=SE_A10017_002/SE_A10017_001,
         owned=SE_A10060_002/SE_A10060_001,
         yearbuilt=SE_A10057_001,
         rent=SE_A18009_001,
         commute=SE_A09003_001,
         noncit=SE_A06001_004/SE_A06001_001,
         uninsured=SE_A20001_002/SE_A20001_001,
         singleparent=SE_A10065_002/SE_A10065_001)%>%
  select(GEOID:singleparent)%>%
  filter(state!="pr")%>%
  left_join(election, by=c("GEOID"="county_fips"))%>%
  relocate(state_name, .after="GEOID")


counties_joined <- left_join(uscounties, dhdemos, by = "GEOID")

glimpse(counties_joined)

my_pal <- colorNumeric("Blues", domain=counties_joined$college)


themap2<-leaflet()%>%
  addTiles()%>%
  addPolygons(data = counties_joined , 
              fillColor =~my_pal(counties_joined$college), 
              fillOpacity = 0.75, 
              weight = 0.1, 
              smoothFactor = 0.1, 
              popup = ~NAME) %>%
  setView(-98.483330, 38.712046, zoom = 4)

themap2

themap3<-leaflet()%>%
  addTiles()%>%
  addPolygons(data = counties_joined , 
              fillColor =~my_pal(counties_joined$college), 
              fillOpacity = 0.75, 
              weight = 0.1, 
              smoothFactor = 0.1, 
              popup = ~NAME) %>%
  setView(-98.483330, 38.712046, zoom = 3) %>%
  addLegend(pal = my_pal, 
            values = counties_joined$college, 
            position = "bottomright", 
            title = "Percent College")

 themap3
 
 
 mybins <- c(0, 0.4, 0.5, 0.6, 0.7,Inf)
 mypalette <- colorBin(
   palette = "YlOrBr", domain =counties_joined$trump,
   na.color = "transparent", bins = mybins
 )
 
 popup<- paste0("Trump Voting Pct: ", percent(counties_joined$trump,0))
 
 
 themap4<-leaflet() %>%
   addTiles() %>%
   setView(-98.483330, 38.712046, zoom = 3) %>%
   addPolygons(
     data=counties_joined,
     fillColor = ~ mypalette(counties_joined$trump),
     stroke = F,
     fillOpacity = 0.50,
     label = popup,
     labelOptions = labelOptions(
       style = list("font-weight" = "normal", padding = "3px 8px"),
       textsize = "11px",
       direction = "auto"
     )
   ) %>%
   addLegend(
     pal = mypalette, values = counties_joined$trump, opacity = 0.9,
     title = "Trump Voting", position = "bottomleft"
   )
 
 themap4
 
 
 summer21<-read_csv("summer21.csv")
 
 themap5<-leaflet(summer21) %>%
   addTiles() %>% 
   setView(-98.483330, 38.712046, zoom = 3)%>%
   addCircles(~summer21$lon, ~summer21$lat, 
              popup=summer21$killed, weight = 3, radius=40, 
              color="red", stroke = TRUE, fillOpacity = 0.8) 
 
 themap5
 
 
 uscounties<-uscounties%>%
   st_transform(crs = 4326)
 

 #for our point file we will need to tell R that this is a 
 #mappable object first, and then transform to 
 summer21<-summer21%>%
   st_as_sf(
     coords = c("lon",
                "lat"),
     crs = 4326)
 
 shootings_joined<-summer21%>%
   st_join(uscounties,
           join = st_intersects, 
           left = TRUE)
 
 glimpse(shootings_joined)
 
 county_shootings<-shootings_joined%>%
   as_tibble()%>%
   group_by(GEOID,STATE_NAME,NAME)%>%
   summarize(shootings=sum(killed, na.rm=T))%>%
   arrange(desc(shootings))
 
 head(county_shootings)
 
 uscounties_lean<-counties_joined%>%
   select(GEOID,NAME,STATE_NAME, pop,geometry)
 
 shootings_lean<-county_shootings%>%
   select(GEOID,shootings)
 
 shootings_per_capita<-uscounties_lean%>%
   left_join(shootings_lean)%>%
   mutate(shootings=ifelse(is.na(shootings),0,shootings))%>%
   mutate(percap=(shootings/pop)*100000)%>%
   arrange(desc(percap))
 
 shootings_per_capita%>%
   filter(pop>=50000)%>%
   select(NAME,STATE_NAME,pop,shootings,percap)%>%
   head()
 
 
 mybins <- c(0,3,6,9,12,Inf)
 mypalette <- colorBin(
   palette = "Reds", domain =shootings_per_capita$percap,
   na.color = "transparent", bins = mybins
 )
 
 popup<- paste0(shootings_per_capita$NAME,", ","Shootings per 100K: ", comma(shootings_per_capita$percap,2),sep=" ")
 
 themap6<-leaflet() %>%
   addTiles() %>%
   setView(-98.483330, 38.712046, zoom = 3) %>%
   addPolygons(
     data=shootings_per_capita,
     fillColor = ~ mypalette(shootings_per_capita$percap),
     stroke = F,
     fillOpacity = 0.50,
     label = popup,
     labelOptions = labelOptions(
       style = list("font-weight" = "normal", padding = "3px 8px"),
       textsize = "11px",
       direction = "auto"
     )
   ) %>%
   addLegend(
     pal = mypalette, values = shootings_per_capita$percap, opacity = 0.9,
     title = "Shootings per 100K", position = "bottomleft"
   )
 
 themap6
 
schools<-read_csv("schools.csv")

schools<-schools%>%
  st_as_sf(
  coords = c("LON",
             "LAT"),
  crs = 4326)

nearest_idx <- st_nearest_feature(schools, summer21)

# 2. Pull the incident_id from summer21 using those indexes
schools$nearest_incident_id <- summer21$incident_id[nearest_idx]

# 3. Calculate the distance to that specific nearest incident
schools$distance_to_summer <- st_distance(
  schools, 
  summer21[nearest_idx, ], 
  by_element = TRUE
)

