library(ggplot2)
library(plotly)
library(akima)
library(sf)



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
    layout(title = 'Distribution of Mosquitoes in 2004',
           font = list(color='white'),
           plot_bgcolor = '#ff9900', paper_bgcolor = '#848484',
           mapbox = list(style = 'dark'),
           legend = list(orientation = 'h',
                         font = list(size = 8)),
           margin = list(l = 25, r = 25,
                         b = 25, t = 25,
                         pad = 2))
year2004_mosquito
#--------------------------------------------------------------------------------------------------------------------
#Plot-2_2013

year2013 <- mosquito %>%
    select(VECTOR, X,Y, YEAR, COUNTRY) %>%
    filter(YEAR == 2013)

year2013_mosquito <-  year2013 %>%
    plot_mapbox(x = ~X, y = ~Y,
                split = ~VECTOR, hoverinfo='COUNTRY',
                mode = 'scattermapbox') %>%
    layout(title = 'Distribution of Mosquitoes in 2013',
           font = list(color='white'),
           plot_bgcolor = '#ff9900', paper_bgcolor = '#848484',
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
                    mutate(Z = n()))[,c(-1,-2)] %>% distinct()

z_percountry%>% plot_geo() %>%
    add_trace(z= ~Z, color = ~Z, colors = 'Blues',name = 'mosquito(Z)',
              locations = ~COUNTRY_ID
    ) %>%
    layout(
        title = "Mosquitoes by Country(total number)",
        geo = list(
            projection = list(type = 'equirectangular')
        )
    )

#-------------------------------------------------------------------------------------------------------------------
#Ass- 1.3
#(A)
z_percountry$logZ<-log(z_percountry$Z)

z_percountry%>% plot_geo() %>%
    add_trace(z= ~logZ, color = ~logZ, colors = 'Blues',name = 'mosquito(Z)',
              locations = ~COUNTRY_ID
    ) %>%
    layout(
        title = "Mosquitoes by Country(log)",
        geo = list(
            projection = list(type = 'equidistant')
        )
    )
#---------------------------------------------------------------------------------------------------------------------

#(B)

z_percountry$logZ<-log(z_percountry$Z)

z_percountry%>% plot_geo() %>%
    add_trace(z= ~logZ, color = ~logZ, colors = 'Blues',name = 'mosquito(Z)',
              locations = ~COUNTRY_ID
    ) %>%
    layout(
        title = "Mosquitoes by Country(log)",
        geo = list(
            projection = list(type = 'conic equal area')
        )
    )
#----------------------------------------------------------------------------------------------------------------------------
#Ass-1.4

p = mosquito%>%filter(COUNTRY_ID=="BRA" & YEAR==2013)

Brazil<- p %>% mutate(X1 = cut_interval(X, n= 100))%>%
    mutate(Y1=cut_interval(Y,n=100))%>% group_by(X1,Y1)%>%
    summarise(meanx = mean(X),meany = mean(Y),N= n())


Brazil %>% plot_mapbox(
    x = ~ meanx,
    y =  ~ meany,
    mode = 'scattermapbox',
    split = ~ N
) %>%
layout(
    title = 'Distribution of Mosquitoes in Brazil',
    font = list(color = 'white'),
    plot_bgcolor = '#ff9900',
    paper_bgcolor = '#848484',
    mapbox = list(style = 'dark'),
    legend = list(orientation = 'h',
                  font = list(size = 8)),
    margin = list(
        l = 25,
        r = 25,
        b = 25,
        t = 25,
        pad = 0.5
    )
)


#Reading file
swedfile <- read.csv("Swedish_Household.csv")
swedimage <- readRDS("Sweden_counties.rds")

#Renaming the levels of Age to more readable form and modifying the columns in table
levels(swedfile$age) <- c("Young", "Adult", "Senior")
colnames(swedfile) <- c("Region", "Age", "Mean_Income")

#Renaming the county to simpler form.
columnsplit <- strsplit(levels(swedfile$Region), " ")
levels(swedfile$Region) <- sapply(columnsplit, function(x)
    x[[2]])

#Spreading the data from long to wide based on factors of Age
new_swedfile <- tidyr::spread(swedfile, Age, Mean_Income)
#colnames(new_swedfile)<-c("Region","Yo","Mean_Income")
new_swedfile$Region = as.character(new_swedfile$Region)



#Changing the names of region in the file from csv to match region names in sf file
new_swedfile[new_swedfile$Region == "Örebro", ]$Region <- "Orebro"
new_swedfile[new_swedfile$Region == "Västra", ]$Region <- "Västra Götaland"
    

#Violin Plot for three Age group showing mean income in SEK
p <-
    ggplot(swedfile, aes(Age, Mean_Income, fill = Age)) +
    geom_violin() + geom_boxplot(width = 0.3,
                                 outlier.color = "black",
                                 fill = "white")
                                     
p + stat_summary(fun.y = median,
                 geom = "point",
                 color = "Red") + xlab("\n Age Group ") +
                 ylab("Mean Income in SEK\n")

#Doing a cubic interpolation to get continous variable and creating a surace plot
interpolated = interp(new_swedfile$Young,
                      new_swedfile$Adult,
                      new_swedfile$Senior,
                      duplicate = "mean")

#Creating a surface plot
plot_ly(
    x =  ~ interpolated$x,
    y =  ~ interpolated$y,
    z =  ~ interpolated$z,
    type = "surface"
   
    
) %>% layout(scene = list(
    xaxis = list(title = 'X',gridcolor="grey",gridwidth=2),
    yaxis = list(title = 'Y',gridcolor="grey",gridwidth=2),
    zaxis = list(title = 'Z',gridcolor="grey",gridwidth=2)),
    title= "Surface plot showing dependence of Z on X and Y\n"

    )



#Modifying the rownames
rownames(new_swedfile) <- new_swedfile$Region

#Adding Mean values of Young and Adult age group corresponding to the name from
#rds file
swedimage$Young <- new_swedfile[swedimage$NAME_1, "Young"]
swedimage$Adult <- new_swedfile[swedimage$NAME_1, "Adult"]

#Plotting chlorpeth plot of sweden counties based on mean income of Young and Adult respectively.)
p <- plot_ly(width=900) %>% layout(
    plot_bgcolor = "#ebfafa",
    title = "Distribution of Mean Income(SEK thousands) for age group 18-29 <br> in counties of Sweden ",
    margin = list(t = 100, 
                  pad = 10)
) %>%
    add_sf(
        data = swedimage,
        color =  ~ Young,
        colors = c("#66ccff","#000066"),
        split = ~ NAME_1,
        showlegend = FALSE
    ) 
p


p2 <- plot_ly(width = 900) %>% layout(
    plot_bgcolor = "#ebfafa",
    title = "Distribution of Mean Income(SEK thousands) for age group 30-49 <br> in counties of Sweden ",
    margin = list(t = 100, 
                  pad = 10)
) %>% add_sf(
    data = swedimage,
    color =  ~ Adult,
    split =  ~ NAME_1,
    colors = c("#66ccff","#000066"),
    showlegend = FALSE
) 
p2



#Plotting Linköping coordinate in pre existing map
p  %>% add_markers(
    x =  15.624525,
    #Longitude
    y =  58.409814,
    #Latitude
    hoverinfo = "text",
    text =  "Linköping", #City Name to Show
    color=I("#d10404")
) %>% layout(title = "Red dot showing the location of Linköping.")

