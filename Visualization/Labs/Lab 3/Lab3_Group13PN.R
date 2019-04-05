title: "Lab3_Group13"
author: "Prudhvi Pedmallu  (prepu690) , Naveen Gabriel (navga709)"
date: "18 September 2018"
output:
  html_document:
  df_print: paged
pdf_document: default
html_notebook:
  theme: journal
fontsize: 11 pt

#--------------------------------------------------------------------------------------------------------------------
#Assignment 1
library(ggplot2)
library(plotly)
#--------------------------------------------------------------------------------------------------------------------

#Creating Token to access MAPBOX
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoicHJ1cGU2OTAiLCJhIjoiY2ptZHU1NTNmMWFhdzN3cjE1Z21ndW5tcSJ9.jeQeNQEOlRul1wakn_KbpQ')

#Reading a file
mosquito<-read.csv('aegypti_albopictus.csv')
#-------------------------------------------------------------------------------------------------------------------
#Ass-1.1
#Plot-1_2004

year2004 <- mosquito %>% 
  select(VECTOR, X, Y, YEAR, COUNTRY) %>%
  filter(YEAR == 2004)

year2004_mosquito <-  year2004 %>%
  plot_mapbox(x = ~X, y = ~Y,
              split = ~VECTOR, hoverinfo='COUNTRY',
              mode = 'scattermapbox') %>%
  layout(title = '2004',
         font = list(color='white'),
         plot_bgcolor = '#ff9900', paper_bgcolor = '#d6d680',
         mapbox = list(style = 'dark'),
         legend = list(orientation = 'h',
                       font = list(size = 8)),
         margin = list(l = 25, r = 25,
                       b = 25, t = 25,
                       pad = 2))
year2004_mosquito

#Plot-2_2013

year2013 <- mosquito %>%
  select(VECTOR, X,Y, YEAR, COUNTRY) %>%
  filter(YEAR == 2013)

year2013_mosquito <-  year2013 %>%
  plot_mapbox(x = ~X, y = ~Y,
              split = ~VECTOR, hoverinfo='COUNTRY',
              mode = 'scattermapbox') %>%
  layout(title = '2013',
         font = list(color='white'),
         plot_bgcolor = '#ff9900', paper_bgcolor = '#80d695',
         mapbox = list(style = 'dark'),
         legend = list(orientation = 'h',
                       font = list(size = 8)),
         margin = list(l = 25, r = 25,
                       b = 25, t = 25,
                       pad = 2))
year2013_mosquito

#-------------------------------------------------------------------------------------------------------------------
#Ass-1.2

z_percountry<- (mosquito %>%
             select(VECTOR,YEAR,COUNTRY,COUNTRY_ID) %>%
             group_by(COUNTRY) %>%
             mutate(R = n()))[,c(-1,-2)] %>% distinct()

z_percountry%>% plot_geo() %>%
  add_trace(z= ~R, color = ~R, colors = 'Blues',name = 'mosquito(Z)',
            locations = ~COUNTRY_ID
  ) %>%
  layout(
    title = "types of mosquitos",
    geo = list(
      projection = list(type = 'equirectangular')
    )
  )

#-------------------------------------------------------------------------------------------------------------------
#Ass- 1.3
#(A)
z_percountry$logZ<-log(z_percountry$R)

z_percountry%>% plot_geo() %>%
  add_trace(z= ~logZ, color = ~logZ, colors = 'Blues',name = 'mosquito(Z)',
            locations = ~COUNTRY_ID
  ) %>%
  layout(
    title = "types of mosquitos",
    geo = list(
      projection = list(type = 'equidistant')
    )
  )
#---------------------------------------------------------------------------------------------------------------------

#(B)

z_percountry$logZ<-log(z_percountry$R)

z_percountry%>% plot_geo() %>%
  add_trace(z= ~logZ, color = ~logZ, colors = 'Blues',name = 'mosquito(Z)',
            locations = ~COUNTRY_ID
  ) %>%
  layout(
    title = "types of mosquitos",
    geo = list(
      projection = list(type = 'conic equal area')
    )
  )
#----------------------------------------------------------------------------------------------------------------------------
#Ass-1.4

p = mosquito%>%filter(COUNTRY_ID=="BRA" & YEAR==2013)

Brazil<- p %>% mutate(X1 = cut_interval(X, n= 100))%>% mutate(Y1=cut_interval(Y,n=100))%>% group_by(X1,Y1)%>%summarise(meanx = mean(X),meany = mean(Y),N= n())

Brazil%>%plot_mapbox(x= ~meanx, y=~meany,mode='scattermapbox', size = ~N, split = ~N)%>%add_markers()

#--------------------------------------------------------------------------------------------------------------------
#Assignment-2


