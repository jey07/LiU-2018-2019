library(plotly)

#Create-MAPBOX-TOKEN
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoicHJ1cGU2OTAiLCJhIjoiY2ptZHU1NTNmMWFhdzN3cjE1Z21ndW5tcSJ9.jeQeNQEOlRul1wakn_KbpQ')

#Reading data

mosquito<-read.csv('aegypti_albopictus.csv')

year2004 <- mosquito %>% 
    select(VECTOR, X, Y, YEAR, COUNTRY) %>%
    filter(YEAR == 2004)

year2004 %>%
    plot_mapbox(x = ~X, y = ~Y,hoverinfo='COUNTRY',
                mode = 'scattermapbox',split=~VECTOR)

z_percountry<- (mosquito %>%
                    select(VECTOR,YEAR,COUNTRY,COUNTRY_ID) %>%
                    group_by(COUNTRY))
                