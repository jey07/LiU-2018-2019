library(tidyverse)
library(readxl)
library(plotly)
library(MASS)


z<-read.csv("olive.csv")

#-------------------------------------Assignment:1---------------------------------------- 

##Question-1

cls<-cut_interval(z$linolenic, n = 4)

ggplot(z, aes(palmitic, oleic, color  = linolenic)) + geom_point()

ggplot(z, aes(palmitic, oleic, color  = factor(cls))) + geom_point() 


##Question-2

ggplot(z, aes(palmitic, oleic, color  = factor(cls))) + geom_point() 

ggplot(z, aes(palmitic, oleic, size  = factor(cls))) + geom_point() 


ggplot(z, aes(palmitic, oleic )) + geom_spoke(aes(angle = as.numeric(cls)), radius = 30,
                                              arrow=arrow(length = unit(0.2,"cm")))  


##Question-3

ggplot(z, aes(oleic, eicosenoic, color = Region)) + geom_point()

ggplot(z, aes(oleic, eicosenoic, color  = factor(Region))) + geom_point()


##Question-4

clss<-cut_interval(z$linoleic, n = 3)
cls1<-cut_interval(z$palmitic, n = 3)
cls2<-cut_interval(z$palmitoleic, n = 3)

ggplot(z, aes(oleic, eicosenoic, color  = factor(clss), shape = factor(cls1), size = factor(cls2))) + geom_point()


##Question-5

ggplot(z, aes(oleic, eicosenoic, color = factor(Region), shape = factor(cls1), size = factor(cls2))) + geom_point()



##Question-6

p<-plot_ly(z, labels = ~(z$Area), type = 'pie',textinfo='none') %>%
  layout(title = ' Proportions of oils coming from different areas', showlegend = F, textinfo = 'none',
         xaxis = list(showgrid = FALSE, zeroline = FALSE,showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE,showticklabels = FALSE))
p

##Question-7

ggplot(z, aes(linoleic, eicosenoic)) + geom_point()

ggplot(z,aes(linoleic, eicosenoic)) + geom_density2d() 

  
#-------------------------------------------Assignment:2--------------------------------------------------------------

#Reading the baseball dataset
baseball<-read_excel("baseball-2016.xlsx")

#Plotting  the dataset to check range of values
plot(density(as.matrix(baseball[,3:ncol(baseball)])),main="Density Plot of Quanitative variables(Before Scaling)")
baseballscaled=scale(baseball[,3:ncol(baseball)])
#Replotting after scaling
plot(density(baseballscaled),main="Density Plot of Quanitative variables(After Scaling)")

#Calculating the distance between different attributes and reducing tthe dimension of
#24 quantitative variables to 2 variables using Kruskal MDS
d<-dist(baseballscaled,method="minkowski",p=2)
res<-isoMDS(d,k=2,maxit=50)

baseballreduced<-res$points
baseballreduced1<-as.data.frame(baseballreduced)
baseballreduced1<-cbind(baseball[,1:2],baseballreduced1)

#Plotting the scatterplot of reduced 2 variable dimension , colored by third variable(League)
plot_ly(data=baseballreduced1,type="scatter",x=~V1,y=~V2,color=~League,hovertext=~Team,
        colors=c("#ff0509","#5b47ad"))



sh <- Shepard(d, baseballreduced)
delta <-as.numeric(d)
D<- as.numeric(dist(baseballreduced))

n=nrow(baseballreduced)
index=matrix(1:n, nrow=n, ncol=n)
index1=as.numeric(index[lower.tri(index)])


n=nrow(baseballreduced)
index=matrix(1:n, nrow=n, ncol=n, byrow = T)
index2=as.numeric(index[lower.tri(index)])


plot_ly()%>%
    add_markers(x=~delta, y=~D, hoverinfo = 'text',
                text = ~paste('Obj1: ', rownames(baseball)[index1],
                              '<br> Obj 2: ', rownames(baseball)[index2]))%>%
    #if nonmetric MDS inolved
    add_lines(x=~sh$x, y=~sh$yf)

baseballnew<-cbind(baseballreduced1,baseball[,3:ncol(baseball)])


plotmodel<-function(x,y)
{
    plt<- ggplot(baseballnew, aes_string(x,y,color="League"))+ geom_point()
    return(plt)
}
colnames(baseballnew)[colnames(baseballnew)=="2B"] <- "Doubles"
colnames(baseballnew)[colnames(baseballnew)=="3B"] <- "Triples"

plt1<-list()
plt1<-lapply(colnames(baseballnew)[5:ncol(baseball)],
             function(x) plotmodel(colnames(baseballreduced1)[4],x))

#grid.arrange(grobs=plt1)


ggplot(baseballnew, aes_string("V2","HR",color="League"))+ geom_point() + ggtitle("HR vs V2")
ggplot(baseballnew, aes_string("V2","HR.per.game",color="League"))+ geom_point() + ggtitle("HR per game vs V2")










