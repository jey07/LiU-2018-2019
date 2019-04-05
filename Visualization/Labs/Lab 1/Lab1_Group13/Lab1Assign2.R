library(gridExtra)
library(tidyverse)
library(plotly)
library(grid)


#Function to calculate IQR and permissible outliers 
getoutandquant <- function(x) {
    q1<-quantile(x)[[2]]
    q3<-quantile(x)[[4]]
    IQR<-q3-q1
    
    out1<-q3+(1.5)*IQR
    out2<-q1-(1.5)*IQR
    
    #Finding the list of points which are outliers ]
    out<-x[x>out1]
    out2<-x[x<out2]
    outliers<-tibble(x=c(out,out2),y=0)
    
    return(outliers)
}

plotmodel <- function(df2,exp,z,xnames){
    
    #Density plot for Infection with changes in x and y axis label and plotting the outliers of variables
    p1<-ggplot(df2,aes_string(exp)) + stat_density(geom="line") + xlab(xnames)+ylab("Density \n")+ geom_point(data=z,aes(x,y),shape=23)
    
    return(p1)
    
}

df<- read.table("SENIC.txt",colClasses = c("NULL",rep(NA,6),"NULL","NULL",rep(NA,3)))

#Giving names to column
colnames(df)<-c("Length_Stay","Age","Infection_Risk","Culture","Chest_X_ray","No_Beds","Census","Nurses_num","Facility")

infectoutlier<- getoutandquant(df$Infection_Risk)
plotinfec <-plotmodel(df,"Infection_Risk",infectoutlier,"Infection Risk")

df2 <- df[, colnames(df)!="Infection_Risk"]
le<-length(df2)
z<-apply(df2,2,getoutandquant)

xnames<-c("Length of Stays(Days)","Age(Yrs)","Routine Culture Ratio","Routine Chest_X_ray Ratio","Avg Number of Beds","Average Daily Census","Avg Number of Nurses","Avg Facilities")

myplot <- list()

for (i in 1:le) {
    myplot[[i]]<-plotmodel(df2,colnames(df2)[i],z[[i]],xnames[i])
}

title=textGrob("Density Plot of SENIC Datasets",gp=gpar(fontface="bold"))

grid.arrange(grobs=c(list(plotinfec),myplot),top=title)


sp<-ggplot(df,aes(Nurses_num,Infection_Risk,color=No_Beds)) +geom_point() + xlab("\n Number of Nurses") + ylab("Infection risk\n")

ggplotly(plotinfec)


#--------------------------------------------------------------------------------
x<-as.list(infectoutlier[,1])

plot_ly(x= ~df$Infection_Risk,type="histogram",name="Histogram",marker=list(line=list(color="black",width=1)))%>% add_markers(x=x[[1]],y=0,name="Outliers",marker=list(symbol="diamond",size=7))%>% layout(title="Histogram of Infection Risk",xaxis=list(title="Infection Risk"),yaxis=list(title="Frequency"))




