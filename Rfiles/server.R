library("shiny")
library("lfe")
library("bacondecomp")
library("tidyverse")
server <- function(input, output) {
output$distPlot <- renderPlot({                                         # Return plot  
        set.seed(input$seed)                                            # Set seed
        T<-30                                                           # Time periods
        T2<-input$group2treatment                                       # Treatment timing group 2
        T3<-input$group3treatment                                       # Treatment timing group 3
        m0=1                                                            # Treatment effect baseline
        m2=m0+input$group2treatmenteffect                               # Treatment effect group 2
        m3=m0+input$group3treatmenteffect                               # Treatment effect group 3
        mt2=(1+input$group2timeeffect)                                  # Treatment effect growth rate  group 2
        mt3=(1+input$group3timeeffect)                                  # Treatment effect growth rate  group 3
        G<-3                                                            # Groups
        N<-30                                                           # Time periods
        # Simulate data
        df<-tibble(id=rep(1:(N*G),T),                                   # id variable 1 2 3 ... 1 2 3 
                   t=rep(1:T,each=(N*G)))%>%                            # time variable 1 1 1 1 .... 2 2 2
            mutate(G=ifelse(id<=N,1,ifelse(id>N&id<=(N*2),2,3)),        # Treated: D==1
                   D=ifelse(G==2&t>T2,1,ifelse(G==3&t>T3,1,0)),         # Post treatment indicator 
                   mean=ifelse(D==1&G==2,m2*mt2^(t-T2),                 # Treatment effect group 2 
                        ifelse(D==1&G==3,m3*mt3^(t-T3),                 # Treatment effect group 3
                               1)),                                     # Treatment effect group 1
                   y=rnorm(n=N*G*T,mean=mean,sd=.1))%>%                 # outcome y
            group_by(G,D)%>%                                            # Group by group times post treatment
            mutate(ybar=mean(y))                                        # Group means for chart
      # Estimation
      beta_twowayDD<-felm(y ~ D | G+ t, df) # estimate two way DD
      output$DD<- renderUI({                                             # Post two way DD
            withMathJax(helpText(paste("$$\\hat{\\beta}^{DD}=" ,
            round(beta_twowayDD$beta[1,1],digits=4),"$$")))
            })
      output$RegSum2 <- renderPrint(summary(beta_twowayDD))              # Post output from regression
      df_bacon <- bacon(y ~ D,data = df,id_var = "id",time_var = "t")    # Bacon decomp
      if (T2!=T3){
      df_bacon<-df_bacon%>%
          mutate(type=ifelse(row_number()==1,"Group 2 as treated vs never treated.",
                      ifelse(row_number()==2,"Group 3 as treated vs never treated.",
                      ifelse(row_number()==3,"Group 3 as treated vs group 2 as control.",
                      "Group 2 as treated vs group 3 as control." ))))%>%
          select(-treated,-untreated)
      }
      else{
          df_bacon<-df_bacon%>%
             select(-treated,-untreated)  
      }
        output$RegSum1 <- renderPrint(                                   # Post Bacon decomposition
            df_bacon  )
        # Create chart for illustration
        ggplot(df,aes(x=t,y=y,colour=as.factor(G)))+geom_jitter(alpha=0.2)+
            geom_step(aes(x=t,y=ybar),size=2) +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"),
                  legend.position="top",legend.text = element_text(size=10, 
                                                                   face="bold"),
                  legend.key=element_blank(),legend.title = element_text(size=10,face="bold"))+
            labs(colour="Group")
        
    })
    
}