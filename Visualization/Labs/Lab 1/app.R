library(shiny)
library(ggplot2)
library(tidyverse)


df<- read.table("SENIC.txt",colClasses = c("NULL",
    rep(NA,6),"NULL","NULL",rep(NA,3)))

#Giving names to column
colnames(df)<-c("Length_Stay","Age","Infection_Risk","Culture","Chest_X_ray",
               "No_Beds","Census","Nurses_num","Facility")


ui<-fluidPage(
    
    titlePanel("Density Plots of Quantitative Variables"),
    sidebarLayout( sidebarPanel( sliderInput("bw","Slide to change bandwidth 
                   of Plot",min=0.1,max=10,value=3,step=0.2,animate=TRUE),
                                 
                   checkboxGroupInput("variableinp","Choose variables",
                   choices=colnames(df),selected = colnames(df)[1]),verbatimTextOutput("value")
                   ),
                   mainPanel( plotOutput("densityplot"))
        )
)


server<-function(input,output){
    
    # observeEvent(input$variableinp, {
    #      print((input$variableinp))
    #  })
        
    output$densityplot <- renderPlot({
        
        if(!is.null(input$variableinp)) {
            
        getoutandquant <- function(x) {
            q1<-quantile(x)[[2]]
            q3<-quantile(x)[[4]]
            IQR<-q3-q1
            
            out1<-q3+(1.5)*IQR
            out2<-q1-(1.5)*IQR
            
            #Finding the list of points which are outliers for a particular variable.
            out<-x[x>out1]
            out2<-x[x<out2]
            outliers<-tibble(x=c(out,out2),y=0)
            
            return(outliers)
        }
            nplot<-length(input$variableinp)
            x<-input$variableinp
            
            p<-list()
            for ( i in 1:nplot) {
                outlier<-getoutandquant(df[,x[i]])
                
                 p[[i]]<-ggplot(df,aes_string(x[i]))+
                 stat_density(geom="line",adjust=input$bw)+ ylab("Density\n")+
                     geom_point(data=outlier,aes(x,y),shape=23)

            }
            
            do.call(grid.arrange,p)
        }
    })
}


shinyApp(ui=ui,server=server)
