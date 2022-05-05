rm(list=ls())
###import data
require(readxl)
require(dplyr)
require(lubridate)
require(tidyr)
require(ggplot2)
require(stringr)
require(reshape)
df.Price <- read_excel("D:\\桌面\\Research Report\\Data\\Data Excel\\Price.xlsx")
##df.RF is the risk-free rate
df.RF <- read_excel("D:\\桌面\\Research Report\\Data\\Data Excel\\91days T-Bill rate.xlsx", 
                    col_types = c("text", "numeric", "skip", 
                                  "skip", "skip", "skip", "skip", "skip"))
df.Size <- read_excel("D:\\桌面\\Research Report\\Data\\Data Excel\\Mar Cap.xlsx")
df.PE <- read_excel("D:\\桌面\\Research Report\\Data\\Data Excel\\PE.xlsx")
##df.MR is the dataframe of market return
df.MR <- read_excel("D:\\桌面\\Research Report\\Data\\Data Excel\\Indices.xlsx", 
                    col_types = c("date", "numeric", "skip", 
                                  "skip", "skip", "skip", "skip"))
df.DR <- read_excel("D:/桌面/Research Report/Data/Data Excel/Repo rate( SA discount rate).xls", 
                    col_types = c("text", "numeric"))
###change daily date to monthly interval
require(xts)
##Calculate the continuously compounded return
date1 <- as.data.frame(df.Price[,1])
df.Price <- as.data.frame(lapply(df.Price[,-1],as.numeric))
df.SR <- log(df.Price[2:nrow(df.Price),]/df.Price[1:(nrow(df.Price)-1),])*100 
df.SR <- cbind(Dates=date1[-1,],df.SR)
df.SR <- xts(df.SR[,-1], as.POSIXct(df.SR$Dates, "%Y-%m-%d"))
df.SR <- as.data.frame(apply.monthly(df.SR,colSums,na.rm=T)[-c(241,242),]) 
df.SR[df.SR==0] <- NA
df.SR <- round(df.SR,2)

##calculate the monthly market return
date2 <- as.data.frame(df.MR[,1])
df.MR <- as.data.frame(lapply(df.MR[,-1],as.numeric))
df.MR <- as.data.frame(log(df.MR[2:nrow(df.MR),]/df.MR[1:(nrow(df.MR)-1),])*100) 
df.MR <- data.frame(cbind(Dates=date2$Dates[-1],df.MR)) 
df.MR<- xts(df.MR[,-1], as.POSIXct(df.MR$Dates, "%Y-%m-%d"))
df.MR <- as.data.frame(apply.monthly(df.MR,colSums,na.rm=T)[-c(241,242),])
df.MR <- round(df.MR,2)
names(df.MR)<-"Market Return"
##repeat the above steps
df.PE <- xts(df.PE[,-1], as.POSIXct(df.PE$Dates, "%Y-%m-%d"))
df.PE <- as.data.frame(apply.monthly(df.PE,mean,na.rm=T))[-c(241,242,243),]

df.Size <- xts(df.Size[,-1], as.POSIXct(df.Size$Dates, "%Y-%m-%d"))
df.Size <- as.data.frame(apply.monthly(df.Size,mean,na.rm=T))[-c(241,242),]

df.RF <- xts(df.RF[,-1], as.POSIXct(df.RF$Date, "%Y-%m-%d"))
df.RF <- as.data.frame(apply.monthly(df.RF, mean,na.rm=T)[-c(241,242),])
df.RF <- round(1+(df.RF/100)^(1/12)-1,2)

df.MR_RF <- df.MR-df.RF
rm(date1)
rm(date2)
rm(df.Price)

###firstly,we build the SMB and HML portfolios separately and we re-balance every portfolio at the end of May for each year.
May<- seq(5,233,12)
Split_Size <- df.Size[May,]
Date<-rownames(Split_Size)
Names<-colnames(Split_Size)
##sorting the size data with "B","S"
Split_Size[Split_Size=="NaN"]<-NA
rownames(Split_Size)<-NULL
Split_Size1<-data.frame()
for(j in 1:dim(Split_Size)[2]){
  for(i in 1:dim(Split_Size)[1]){
    if (is.na(Split_Size[i,j])){Split_Size1[i,j]<-NA}
    else{
      if (Split_Size[i,j]>=mean(as.numeric(Split_Size[i,]),na.rm = T))
      {Split_Size1[i,j]<-"B"}
      else{Split_Size1[i,j]<-"S"}
    }
  }
}
Split_Size<-Split_Size1
row.names(Split_Size)<-Date
colnames(Split_Size)<-Names
##sorting the PE data with "H","M","L"
Split_PE <- round(df.PE[May,],2)
Split_PE[Split_PE=="NaN"]<-NA
Split_PE1<-data.frame()
for (j in 1:dim(Split_PE)[2]){
  for (i in 1:dim(Split_PE)[1]){
    if(is.na(Split_PE[i,j])){Split_PE1[i,j]<-NA}
    else{
      if(Split_PE[i,j]>=quantile(Split_PE[i,],0.7,na.rm = T))
      {Split_PE1[i,j]<-"H"}
      else if(Split_PE[i,j]<=quantile(Split_PE[i,],0.4,na.rm = T))
      {Split_PE1[i,j]<-"L"}
      else{Split_PE1[i,j]<-"M"}
    }
  } 
}
Split_PE<-Split_PE1
row.names(Split_PE)<-Date
colnames(Split_PE)<-Names
###sorting loser and winner data with "W","L"
##i=4 which means the first formation interval is starting from"2000-04-31"
Split_SR <-data.frame()
for(j in 1:dim(df.SR)[2]){
  i<-4
  h<-1
  while (i <= dim(df.SR)[1]-55){
    Split_SR[h,j] <- sum(df.SR[i:i+47,j],na.rm = T)
    i<-i+12
    h<-h+1
  }
}
Split_SR[Split_SR==0]<-NA
##Because the formation period of our LMW portfolio is from -60 to -12 months, thus the t is start from "2005-05-31"
##The reason for this process is to avoid the momentum influence
Split_SR1<-data.frame()
for(j in 1:dim(Split_SR)[2]){
  for(i in 1:dim(Split_SR)[1]){
    if(is.na(Split_SR[i,j])){Split_SR1[i,j]<-NA}
    else{
      if(Split_SR[i,j]>=quantile(as.numeric(Split_SR[i,]),0.8,na.rm = T)){Split_SR1[i,j]<-"W"}
      else if(Split_SR[i,j]>=quantile(as.numeric(Split_SR[i,]),0.6,na.rm = T)){Split_SR1[i,j]<-"P2"}
      else if(Split_SR[i,j]>=quantile(as.numeric(Split_SR[i,]),0.4,na.rm = T)){Split_SR1[i,j]<-"P3"}
      else if(Split_SR[i,j]>=quantile(as.numeric(Split_SR[i,]),0.2,na.rm = T)){Split_SR1[i,j]<-"P4"}
      else{Split_SR1[i,j]<-"L"}
    }
  }
}
##the last two rows are the data of 2019 and 2020,
Split_SR<-Split_SR1[-c(15,16),]
colnames(Split_SR)<-colnames(df.SR)
rownames(Split_SR)<-as.Date("2004-05-31")+years(1:14)
rm(Split_SR1,Split_Size1,Split_PE1,Names,Date)
#df.MC=market capital of each company
#df.MR=market benchmark return
#df.SR=stock return of each company
#split....PE=the define of HML portfolio
#split.Size=the define of SMB portfolio
#split.SR=the define of LMW portfolio
###Now, we are going to calculate the return of SMB,HML and LMW portfolios
##SMB
Split_Size1 <- Split_Size
y<-data.frame()
for(i in 1:dim(Split_Size)[1]){
  y<-Split_Size1[(i-1)*12+1,]
  for(j in 1:11){ 
    Split_Size1<-rbind(Split_Size1[1:(j+12*(i-1)),],y,Split_Size1[(j+12*(i-1)+1):nrow(Split_Size1),])} 
}
Split_Size<-Split_Size1[-c(230:242),]
Dates <- data.frame(Dates=row.names(df.Size))[5:233,]
row.names(Split_Size)<-Dates
##HML
Split_PE1 <- Split_PE
y<-data.frame()
for(i in 1:dim(Split_PE1)[1]){
  y<-Split_PE1[(i-1)*12+1,]
  for(j in 1:11){ 
    Split_PE1<-rbind(Split_PE1[1:(j+12*(i-1)),],y,Split_PE1[(j+12*(i-1)+1):nrow(Split_PE1),])} 
}
Split_PE1<-Split_PE1[-c(230:242),]
Dates <- data.frame(Dates=row.names(df.Size))[5:233,]
row.names(Split_PE1)<-Dates
Split_PE <- Split_PE1
##LMW
Split_SR1 <- Split_SR
y<-data.frame()
for(i in 1:dim(Split_SR1)[1]){
  y<-Split_SR1[(i-1)*12+1,]
  for(j in 1:11){ 
    Split_SR1<-rbind(Split_SR1[1:(j+12*(i-1)),],y,Split_SR1[(j+12*(i-1)+1):nrow(Split_SR1),])} 
}
Split_SR1<-Split_SR1[-169,]
Dates <- data.frame(Dates=row.names(df.Size[65:233,]))
row.names(Split_SR1)<-Dates$Dates
Split_SR <- Split_SR1
rm(y,Split_SR1,Split_PE1,Split_Size1)

###Building portfolios and calcuate there return in the same framework
##the following method work the same way as gather() and summarize() function
a<-as.matrix(Split_Size[-c(1:60),])
dim(a)<-c(169*458,1)
b<-as.data.frame.table(Split_Size[-c(1:60),])[,-c(3:460)]
c<-as.matrix(Split_SR)
dim(c)<-c(169*458,1)
d<-as.matrix(Split_PE[-c(1:60),])
dim(d)<-c(169*458,1)
e<-as.matrix(df.SR[-c(1:64),])[-c(170:176),]
dim(e)<-c(169*458,1)
f<-as.matrix(df.Size[-c(1:64),])[-c(170:176),]
dim(f)<-c(169*458,1) 
f[f=="NaN"]<-NA
g<-str_c(a,d,sep = "/")
df.all <-cbind(b,f,e,a,d,g,c)
colnames(df.all)<-c("Date","Name","Mar_Cap","Return","SMB","HML","Combined","LMW")
k <- as.data.frame(df.MR_RF[-c(1:64,234:240),])

###reshape the long dataframe to wide dataframe
x<-group_by(df.all,Date,Combined)
##We use equal-weighted to calculate the portfolio return
y<-summarise(x,mean=mean(Return,na.rm = T))
PR <- cast(y,Date~Combined)[,-8]
##The mean of small size group return=(S/L+S/H+S/M)/3
SMB1 <- apply(PR[,-c(1:4)],1,mean,na.rm=T)
Date <- PR[,1]
PR <- PR[,-1]
##The mean of Big size group return=(B/L+B/H+B/M)/3
SMB2 <- apply(PR[,-c(4:6)],1,mean,na.rm=T)
##And then we used Small size group minus Big size group to calculate the SMB portfolio return
SMB <- as.data.frame(SMB1-SMB2)
##HML1 equals to (S/H+B/H)/2
HML1 <- apply(PR[,c(1,4)],1,mean,na.rm=T)
##HML2 equals to (B/L+S/L)/2
HML2 <- apply(PR[,c(2,5)],1,mean,na.rm=T)
HML <- as.data.frame(HML1-HML2)
###instead of mean(), we used median here, because the original data got a lot of extreme data that will
##caused the mean to be biased
i<-group_by(df.all,Date,LMW)
j<-summarise(i,Mean=mean(Return,na.rm = T))
LW <- cast(j,Date~LMW)[,-7]
#LMW equals to losers minus winners
LMW <- LW$L-LW$W
df.port_R <- cbind(Date,k,SMB,HML,LMW,LW[,-1])
colnames(df.port_R)<-c("Date","RM_RF","SMB","HML","LMW","Loser","P2","P3","P4","Winner")

###"df.port_R" and "PR","LW" is the summaries result of above process
rm(a,b,c,d,e,f,i,j,h,k,Dates,x,y,Date,g,May,df.MR)

###The regression part
####Table 1####
#Panel A
##Firstly, we calculated the equally weighted average monthly return in(%)
##Here we used t.test() to exam whether the means of portfolio returns are significantly 
##different from zero, the Null hypothesis is that the mean of samples is equal to zero
P_V<-function(i){
  t.test(df.port_R[,i])[3]
}
T_S<-function(j){
  t.test(df.port_R[,j])[1]
}
Ave<-function(h){
  t.test(df.port_R[,h])[5]
}
P_Value<-unlist(lapply(5:dim(df.port_R)[2],P_V))
T_statistic<-unlist(lapply(5:dim(df.port_R)[2],T_S))
Mean<-unlist(lapply(5:dim(df.port_R)[2],Ave))
Name<-names(df.port_R[,5:10])

#Panel B
#In this panel, the OLS method is used to find the coefficient of each factor(portfolio returns and January Dummy) 
#Formula: Ri-Rf=alpha+b1,(RM_RF)+si,SMB+hi,HML+ji,January Dummy+ei
Date<-df.port_R[,1]
Jan_Dummy<-if_else(month(df.port_R$Date)==1,1,0)
Rf <- df.RF[-c(1:64,234:240),]
df.port_R<-cbind(df.port_R,Jan_Dummy,Rf)
linear_1 <- lm(df.port_R$Loser-Rf~df.port_R$RM_RF+df.port_R$SMB+df.port_R$HML+df.port_R$Jan_Dummy)
summary(linear_1)
linear_2 <- lm(df.port_R$P2-Rf~df.port_R$RM_RF+df.port_R$SMB+df.port_R$HML+df.port_R$Jan_Dummy)
summary(linear_2)
linear_3 <- lm(df.port_R$P3-Rf~df.port_R$RM_RF+df.port_R$SMB+df.port_R$HML+df.port_R$Jan_Dummy)
summary(linear_3)
linear_4 <- lm(df.port_R$P4-Rf~df.port_R$RM_RF+df.port_R$SMB+df.port_R$HML+df.port_R$Jan_Dummy)
summary(linear_4)
linear_5 <- lm(df.port_R$Winner-Rf~df.port_R$RM_RF+df.port_R$SMB+df.port_R$HML+df.port_R$Jan_Dummy)
summary(linear_5)
linear_6 <- lm(df.port_R$LMW-Rf~df.port_R$RM_RF+df.port_R$SMB+df.port_R$HML+df.port_R$Jan_Dummy)
summary(linear_6)

###Table 2 Long-Run reversal and January Returns
##PanelA:Full sample
#Dummy variables(losers,winners)
df.Ri <- as.data.frame(apply(df.SR,2,function(x){x-df.RF}))[-c(1:64,234:240),]
colnames(df.Ri)<-colnames(df.SR)
##Building the loser dummy
Split_SR1<-data.frame()
for(i in 1:dim(Split_SR)[1]){
  for(j in 1:dim(Split_SR)[2]){
    if(is.na(Split_SR[i,j])){Split_SR1[i,j]<-NA}
    else{if(as.character(Split_SR[i,j])=="L"){Split_SR1[i,j]<-1}
      else{Split_SR1[i,j]<-0}
    }
  }
}
Date<-rownames(Split_SR)
Name<-colnames(Split_SR)
rownames(Split_SR1)<-Date
colnames(Split_SR1)<-Name
L_Dummy <- as.data.frame(Split_SR1)
rm(Split_SR1)
##Building the winner dummy
Split_SR2<-data.frame()
for(i in 1:dim(Split_SR)[1]){
  for(j in 1:dim(Split_SR)[2]){
    if(is.na(Split_SR[i,j])){Split_SR2[i,j]<-NA}
    else{if(as.character(Split_SR[i,j])=="W"){Split_SR2[i,j]<-1}
      else{Split_SR2[i,j]<-0}
    }
  }
}
rownames(Split_SR2)<-Date
colnames(Split_SR2)<-Name
W_Dummy <- as.data.frame(Split_SR2)
rm(Split_SR2)
L_Dummy <- as.data.frame(apply(L_Dummy, 2, function(y){y<-as.numeric(y)}))
W_Dummy <- as.data.frame(apply(W_Dummy, 2, function(y){y<-as.numeric(y)}))
####Table 2####
T_df.Ri<-as.matrix(t(df.Ri))
colnames(T_df.Ri)<-Date
T_L_Dummy<-as.matrix(t(L_Dummy))
colnames(T_L_Dummy)<-Date
T_W_Dummy<-as.matrix(t(W_Dummy))
colnames(T_W_Dummy)<-Date
####PanelA####
###Fama-Macbeth regression
##Firstly, we run the cross-sectional regression to fetch the factor exposure of L_Dummy and W_Dummy at each time t
##Thus, the alphas, beta1 and beta2 should be time series
#Alphas
lm(T_df.Ri[,1]~T_L_Dummy[,1]+T_W_Dummy[,1])
Alphas <- as.matrix(
  round(as.numeric
        (lapply(1:dim(T_df.Ri)[2],
                function(j){lm <- lm(T_df.Ri[,j]~T_L_Dummy[,j]+T_W_Dummy[,j])$coefficients[1]})),4))
#Loser Betas
L_Betas <- as.matrix(
  round(as.numeric
        (lapply(1:dim(T_df.Ri)[2],
                function(j){lm <- lm(T_df.Ri[,j]~T_L_Dummy[,j]+T_W_Dummy[,j])$coefficients[2]})),4))
#Winner Betas
W_Betas <- as.matrix(
  round(as.numeric
        (lapply(1:dim(T_df.Ri)[2],
                function(j){lm <- lm(T_df.Ri[,j]~T_L_Dummy[,j]+T_W_Dummy[,j])$coefficients[3]})),4))
#Use the t.test() to test whether the mean of each coefficient is significantly different to 0
t.test(Alphas)
t.test(L_Betas)
t.test(W_Betas)

####PanelB:January Excluded####
Re_Jan<-function(x){
  x<-subset(x,month(as.Date((rownames(x))))!=1)
}
rownames(L_Dummy)<-Date
rownames(W_Dummy)<-Date
df.Ri_EJ<-t(Re_Jan(df.Ri))
L_Dummy_EJ<-t(Re_Jan(L_Dummy))
W_Dummy_EJ<-t(Re_Jan(W_Dummy))

lm(df.Ri_EJ[,1]~L_Dummy_EJ[,1]+W_Dummy_EJ[,1])
Alphas_EJ <- as.matrix(
  round(as.numeric
        (lapply(1:dim(df.Ri_EJ)[2],
                function(j){lm <- lm(df.Ri_EJ[,j]~L_Dummy_EJ[,j]+W_Dummy_EJ[,j])$coefficients[1]})),4))
L_Betas_EJ <-as.matrix(
  round(as.numeric
        (lapply(1:dim(df.Ri_EJ)[2],
                function(j){lm <- lm(df.Ri_EJ[,j]~L_Dummy_EJ[,j]+W_Dummy_EJ[,j])$coefficients[2]})),4))
W_Betas_EJ <- as.matrix(
  round(as.numeric
        (lapply(1:dim(df.Ri_EJ)[2],
                function(j){lm <- lm(df.Ri_EJ[,j]~L_Dummy_EJ[,j]+W_Dummy_EJ[,j])$coefficients[3]})),4))
t.test(Alphas_EJ)
t.test(L_Betas_EJ)
t.test(W_Betas_EJ)

####Table 3####
###The Repo rate was used to measure the monetary environment, which will be assigned to df.DR
## Expansive and restrictive condition in South Africa
df.DR <- xts(df.DR[,-1], as.POSIXct(df.DR$Date, "%Y-%m-%d"))
df.DR <- as.data.frame(apply.monthly(df.DR, mean,na.rm=T))
df.DR <- round(1+(df.DR/100)^(1/12)-1,4)
split.DR <- round(df.DR[2:nrow(df.DR),]-df.DR[1:(nrow(df.DR)-1),],4)
split.DR<- cbind(Date[-1],split.DR)
for(i in 1:dim(split.DR)[1]){
  if(split.DR[i,2]<0){split.DR[i,2]<-"E"}
  else if(split.DR[i,2]>0){split.DR[i,2]<-"R"}
  else{split.DR[i,2]<-"M"}
}
for(i in 2:dim(split.DR)[1]){
  if (split.DR[i,2]=="M"&&split.DR[i-1,2]=="R"){split.DR[i,2]<-"R"}
  else if (split.DR[i,2]=="M"&&split.DR[i-1,2]=="E"){split.DR[i,2]<-"E"}
}
split.DR<-as.data.frame(split.DR[-c(1:40,210:226),])
split.DR[,1]<-Date
colnames(split.DR)<-c("Date","Define")
#if Repo rate is decreased(increased) then we can define that month is expansive(restrictive). 
#if the Repo rate is neither decrease or increase then we can define that month is maintained
##"R" means restrictive env in "Split_DR" dataframe, while "E" means expansive env in "Split_DR" dataframe
Restrictive <- subset(split.DR,split.DR[,2]=="R")
Expansive <- subset(split.DR,split.DR[,2]=="E")
LMW<-mutate(as.data.frame(Date),LMW=df.port_R$LMW)
Restrictive<-merge(Restrictive,LMW)
Expansive<-merge(Expansive,LMW)
##We used the two-sample t.test to test whether the true mean of LMW portfolio return
#will be statistically different under expansive and restrictive monetary env. 
#The Null Hypothesis is that true difference in means is equal to 0 
t.test(Restrictive$LMW,Expansive$LMW,var.equal = T)

####Table 4####
####Panel.A full sample####
##Firstly, we bulid the Loser and winner dummy and then merge them to expansive and restrictive conditions.
L_Dummy<-mutate(as.data.frame(Date),L_Dummy)
W_Dummy<-mutate(as.data.frame(Date),W_Dummy)
Exp_L <- merge(Expansive[,-3],L_Dummy)
Res_L <- merge(Restrictive[,-3],L_Dummy)
Exp_W <- merge(Expansive[,-3],W_Dummy)
Res_W <- merge(Restrictive[,-3],W_Dummy)
rownames(df.Ri)<-NULL
df.Ri<-mutate(as.data.frame(Date),df.Ri)
Exp_Ri <- merge(Expansive[,-3],df.Ri)
Res_Ri <- merge(Restrictive[,-3],df.Ri)

####PartA Expansive condition####
Date2<-Exp_Ri[,1]
Exp_Ri<-as.data.frame(t(Exp_Ri[,-c(1,2)]))
Exp_L<-as.data.frame(t(Exp_L[,-c(1,2)]))
Exp_W<-as.data.frame(t(Exp_W[,-c(1,2)]))
colnames(Exp_Ri)<-Date2
colnames(Exp_L)<-Date2
colnames(Exp_W)<-Date2
#Alphas
lm(Exp_Ri[,1]~Exp_L[,1]+Exp_W[,1])$coefficient
Exp_Alphas <- as.matrix(
  round(as.numeric
        (lapply(1:dim(Exp_Ri)[2],function(j)
        {lm <- lm(Exp_Ri[,j]~Exp_L[,j]+Exp_W[,j])$coefficients[1]})),4))
#loser betas
Exp_L_Betas <- as.matrix(
  round(as.numeric
        (lapply(1:dim(Exp_Ri)[2],function(j)
        {lm <- lm(Exp_Ri[,j]~Exp_L[,j]+Exp_W[,j])$coefficients[2]})),4))
#winner betas
Exp_W_Betas <- as.matrix(
  round(as.numeric(
    lapply(1:dim(Exp_Ri)[2],function(j)
    {lm <- lm(Exp_Ri[,j]~Exp_L[,j]+Exp_W[,j])$coefficients[3]})),4))
##t.test()
t.test(Exp_Alphas)
t.test(Exp_L_Betas)
t.test(Exp_W_Betas)

####PartB Restrictive Condition####
Date3<-Res_Ri[,1]
Res_Ri<-as.data.frame(t(Res_Ri[,-c(1,2)]))
Res_L<-as.data.frame(t(Res_L[,-c(1,2)]))
Res_W<-as.data.frame(t(Res_W[,-c(1,2)]))
colnames(Res_Ri)<-Date3
colnames(Res_L)<-Date3
colnames(Res_W)<-Date3
#Alphas
lm(Res_Ri[,1]~Res_L[,1]+Res_W[,1])
Res_Alphas <- as.matrix(
  round(as.numeric(
    lapply(1:dim(Res_Ri)[2],function(j)
    {lm<-lm(Res_Ri[,j]~Res_L[,j]+Res_W[,j])$coefficients[1]})),4))
#Loser betas
Res_L_Betas <- as.matrix(
  round(as.numeric(
    lapply(1:dim(Res_Ri)[2],function(j)
    {lm<-lm(Res_Ri[,j]~Res_L[,j]+Res_W[,j])$coefficients[2]})),4))
#Winner Betas
Res_W_Betas <- as.matrix(
  round(as.numeric(
    lapply(1:dim(Res_Ri)[2],function(j)
    {lm<-lm(Res_Ri[,j]~Res_L[,j]+Res_W[,j])$coefficients[3]})),4))
##t.test()
t.test(Res_Alphas)
t.test(Res_L_Betas)
t.test(Res_W_Betas)
####PanelB January excluded#### 
####PartA Expansive environment####
#Excluding January
Exp_Ri<-t(Re_Jan(t(Exp_Ri)))
Exp_L<-t(Re_Jan(t(Exp_L)))
Exp_W<-t(Re_Jan(t(Exp_W)))

#Alphas
lm(Exp_Ri[,1]~Exp_L[,1]+Exp_W[,1])$coefficient
Exp_Alphas <- as.matrix(
  round(as.numeric
        (lapply(1:dim(Exp_Ri)[2],function(j)
        {lm <- lm(Exp_Ri[,j]~Exp_L[,j]+Exp_W[,j])$coefficients[1]})),4))
#loser betas
Exp_L_Betas <- as.matrix(
  round(as.numeric
        (lapply(1:dim(Exp_Ri)[2],function(j)
        {lm <- lm(Exp_Ri[,j]~Exp_L[,j]+Exp_W[,j])$coefficients[2]})),4))
#winner betas
Exp_W_Betas <- as.matrix(
  round(as.numeric(
    lapply(1:dim(Exp_Ri)[2],function(j)
    {lm <- lm(Exp_Ri[,j]~Exp_L[,j]+Exp_W[,j])$coefficients[3]})),4))
##t.test()
t.test(Exp_Alphas)
t.test(Exp_L_Betas)
t.test(Exp_W_Betas)

####PartB Restrictive env####
#Excluding January
Res_Ri<-t(Re_Jan(t(Res_Ri)))
Res_L<-t(Re_Jan(t(Res_L)))
Res_W<-t(Re_Jan(t(Res_W)))

#Alphas
lm(Res_Ri[,1]~Res_L[,1]+Res_W[,1])
Res_Alphas <- as.matrix(
  round(as.numeric(
    lapply(1:dim(Res_Ri)[2],function(j)
    {lm<-lm(Res_Ri[,j]~Res_L[,j]+Res_W[,j])$coefficients[1]})),4))
#Loser betas
Res_L_Betas <- as.matrix(
  round(as.numeric(
    lapply(1:dim(Res_Ri)[2],function(j)
    {lm<-lm(Res_Ri[,j]~Res_L[,j]+Res_W[,j])$coefficients[2]})),4))
#Winner Betas
Res_W_Betas <- as.matrix(
  round(as.numeric(
    lapply(1:dim(Res_Ri)[2],function(j)
    {lm<-lm(Res_Ri[,j]~Res_L[,j]+Res_W[,j])$coefficients[3]})),4))
##t.test()
t.test(Res_Alphas)
t.test(Res_L_Betas)
t.test(Res_W_Betas)


####Visualization####
###Visualized, Using level data (show the growth of a R1 investment in each portfolio))
##Define the shadow part under expansive and restrictive condition
Res_Date_End<-Restrictive[,1]
Res_Date_Start<-as.Date(Res_Date_End) %m-% months(1)
Exp_Date_End<-Expansive[,1]
Exp_Date_Start<-as.Date(Exp_Date_End) %m-% months(1)
##calculate the return level
SMB<-df.port_R$SMB/100
# Compute gross returns
SMB_R <- 1 + SMB
# Compute future values
SMB_F <- cumprod(SMB_R)
# Plot the evolution of the $1 invested in SMB as a function of time
plot(SMB_F, type = "l", col = "blue", lwd = 2, ylab = "Rands",xlab = "Months",
     main = "FV of 1 Rand invested in SMB Portfolio")

HML<-df.port_R$HML/100
HML_R <- 1 + HML
HML_F <- cumprod(HML_R)
plot(HML_F, type = "l", col = "blue", lwd = 2, ylab = "Rands",xlab = "Months",
     main = "FV of 1 Rand invested in HML Portfolio")

LMW<-df.port_R$LMW/100
LMW_R <- 1 + LMW
LMW_F <- cumprod(LMW_R)
plot(LMW_F, type = "l", col = "blue", lwd = 2, ylab = "Rands",xlab = "Months",
     main = "FV of 1 Rand invested in LMW Portfolio")

RM_RF<-df.port_R$RM_RF/100
RM_RF_R <- 1 + RM_RF
RM_RF_F <- cumprod(RM_RF_R)
plot(RM_RF_F, type = "l", col = "blue", lwd = 2, ylab = "Rands",xlab = "Months",
     main = "FV of 1 Rand invested in RM_RF Portfolio")

Loser<-df.port_R$Loser/100
Loser_R <- 1 + Loser
Loser_F <- cumprod(Loser_R)
plot(Loser_F, type = "l", col = "blue", lwd = 2, ylab = "Rands",xlab = "Months",
     main = "FV of 1 Rand invested in Loser Portfolio")

Winner<-df.port_R$Winner/100
Winner_R <- 1 + Winner
Winner_F <- cumprod(Winner_R)
plot(Winner_F, type = "l", col = "blue", lwd = 2, ylab = "Rands",xlab = "Months",
     main = "FV of 1 Rand invested in Winner Portfolio")

###Combined
Portfolio_F <- cbind(RM_RF_F,SMB_F,HML_F,LMW_F)
Portfolio_F <- as.data.frame(apply(Portfolio_F,2,function(x){x<-as.numeric(x)}))
Portfolio_F<-cbind(Date,Portfolio_F)
colnames(Portfolio_F)<-c("Date","RM_RF","SMB","HML","LMW")
Portfolio_F%>% 
  gather(key,value,RM_RF,SMB,HML,LMW)%>%
  ggplot(aes(x=as.Date(Date), y=value, colour=key))+
  geom_line(size=1.2)+
  xlab("Date")+
  ylab("Future Value")+
  ggtitle("The evolution of the R1 invested in 4 different portfolios")+
  theme_bw()+
  geom_hline(aes(yintercept=0),col="grey")+
  labs(colour="Portfolios")+
  annotate("rect", fill = "lightblue", alpha = 0.5,
           xmin = as.Date(Res_Date_Start),xmax = as.Date(Res_Date_End),
           ymin = -Inf, ymax = Inf)+
  geom_label(aes(x=max(as.Date(Date)),y=1.75,label = "Restrictive"),
             fill = "lightblue",size=2.5,color = 'black')+
  annotate("rect", fill = "lightpink", alpha = 0.5,
           xmin = as.Date(Exp_Date_Start),xmax = as.Date(Exp_Date_End),
           ymin = -Inf, ymax = Inf)+
  geom_label(aes(x=max(as.Date(Date)),y=2.0,label = "Expansive"),
             fill = "lightpink",size=2.5,color = 'black')

###Visualized Loser and Winner portfolio return under expansive and restrictive condition
Portfolio_F1 <- cbind(Loser_F,Winner_F,LMW_F)
Portfolio_F1 <- as.data.frame(apply(Portfolio_F1,2,function(x){x<-as.numeric(x)}))
Portfolio_F1<-cbind(Date,Portfolio_F1)
colnames(Portfolio_F1)<-c("Date","Loser","Winner","LMW")
Portfolio_F1%>% 
  gather(key,value,Loser,Winner,LMW)%>%
  ggplot(aes(x=as.Date(Date), y=value, colour=key))+
  geom_line(size=1.2)+
  xlab("Date")+
  ylab("Future Value")+
  ggtitle("The evolution of the R1 invested in 3 different portfolios")+
  theme_bw()+
  geom_hline(aes(yintercept=0),col="grey")+
  labs(colour="Portfolios")+
  annotate("rect", fill = "lightblue", alpha = 0.5,
           xmin = as.Date(Res_Date_Start),xmax = as.Date(Res_Date_End),
           ymin = -Inf, ymax = Inf)+
  geom_label(aes(x=max(as.Date(Date)),y=1.75,label = "Restrictive"),
             fill = "lightblue",size=2.5,color = 'black')+
  annotate("rect", fill = "lightpink", alpha = 0.5,
           xmin = as.Date(Exp_Date_Start),xmax = as.Date(Exp_Date_End),
           ymin = -Inf, ymax = Inf)+
  geom_label(aes(x=max(as.Date(Date)),y=2.0,label = "Expansive"),
             fill = "lightpink",size=2.5,color = 'black')





