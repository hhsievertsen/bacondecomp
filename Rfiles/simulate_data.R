simulate_data<-function(seed,T2,T3,m2,m3,mt2,mt3,G2,G3){
  # set values
  set.seed(seed)                        # set seed
  mt2=(1+mt2)                           # Treatment effect growth group 2
  mt3=(1+mt3)                           # Treatment effect growth group 3
  m2=1+m2                               # Treatment effect group 2
  m3=1+m3                               # Treatment effect group 3
  T<-30                                 # Time periods
  G<-3                                  # Groups
  G1<-30                                # Group size group 1 (never treated)
  N<-G1+G2+G3   
  # Create tibble
  df<-tibble(id=rep(1:(N),T),                                     # Id variable 1 2 3 ... 1 2 3 
             t=rep(1:T,each=(N)))%>%                              # Time variable 1 1 1 1 .... 2 2 2
      mutate(G=ifelse(id<=G1,1,ifelse(id>G1&id<=(G1+G2),2,3)),    # Treated: D==1
             D=ifelse(G==2&t>=T2,1,ifelse(G==3&t>=T3,1,0)),         # Post treatment indicator 
             mean=ifelse(D==1&G==2,m2*mt2^(t-T2),                 # Treatment effect group 2 
                  ifelse(D==1&G==3,m3*mt3^(t-T3),                 # Treatment effect group 3
                              1)),                                # mean Y for untreated
              y=rnorm(n=N*T,mean=mean,sd=.1))%>%                  # Simulate Outcome y
    group_by(G,D)%>%                                              # Group by "group times post" treatment
    mutate(ybar=mean(y))                                          # Group means for chart
  return(df)
}
#df<-simulate_data(42,10,20,1,2,0,0,30,30)