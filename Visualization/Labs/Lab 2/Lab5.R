library(tidyverse)
library(plotly)
library(crosstalk)

olive<-read.csv("olive.csv")

sharedolive<-SharedData$new(olive)
scatplt<-plot_ly(sharedolive,x=~linoleic,y=~eicosenoic,type="scatter")

barplt<-plot_ly(sharedolive,x=~as.factor(Region),type="histogram") %>% layout(barmode="overlay")

subplot(scatplt, barplt) %>% highlight(on = "plotly_select",
                                       persistent = T,
                                       dynamic = T,
                                       opacityDim = I(1)) %>% hide_legend()