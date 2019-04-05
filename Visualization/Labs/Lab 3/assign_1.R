library(plotly)
library(purrr)

Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoibmF2Z2FicmllbCIsImEiOiJjam1ndWtieGYyYTE5M3ZzM2V1cnFwem8yIn0.PkzYRzGGUrTWncLZQKNYVQ')

mosq<-read.csv("aegypti_albopictus.csv")

yr<- mosq %>% select(VECTOR,X,Y,YEAR,COUNTRY) %>% filter(YEAR==2004 | YEAR==2013)

yr$YEAR<-droplevels(yr$YEAR)

z<-lapply(split(yr,yr$YEAR),function(df) {
     plot_mapbox(df,color=~VECTOR,x=~X,y=~Y,mode='scatterplot',hoverinfo='name')%>%
     layout(title="Hello",font=list(color='Black',size=10),
     paper_bgcolor = '#f7f3c3',
     mapbox = list(style = 'light'),
     legend = list(orientation = 'h',font = list(size = 11)))
}) %>% bscols()



numbr_mosq<-sapply(levels(mosq$COUNTRY_ID),function(x) nrow(mosq[mosq$COUNTRY_ID==x,]))
plotmos<-data.frame(names(numbr_mosq),unname(numbr_mosq))
colnames(plotmos)<-c("Country_Id","Mosquito_Count")
rownames(plotmos)<-plotmos$Country_Id

plotmos$COUNTRY<-plotmos[mosq$COUNTRY_ID,mosq$COUNTRY]
d<-as.numeric(rownames(mosq[mosq$COUNTRY=="West Bank",]))
mosq<-mosq[-d,]
