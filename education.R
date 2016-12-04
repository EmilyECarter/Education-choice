
#table <- read.csv("~/Downloads/table.csv")
#for ( i in 1:nrow(table)){
#  if ( is.null(table[i,2])){
#    table[i,2] = table[i-1,2]
#  }
#}


#for ( i in 1:nrow(table)){
#  if (table[i,1] == ""){
#    table[i,1] = table[i-1,1]
#  }
#}
#table$total = table$Indigenous + table$Non.Indigenous +table$Not.known

#View(table)


#Uni
LG<- `LGA.HE.Attrition.Data,.2005.2013` <- read.csv("~/Downloads/Higher Education Attrition Rates/LGA HE Attrition Data, 2005-2013.csv")
year<- c(2005,2006,2007,2008,2009,2010,2011,2012,2013)
LG[LG == "-"] <- NA
LG[LG == "0"] <- NA

#Uni
for (i in 1:nrow(LG)){ 
  dat<-(LG[i,3:11])
  dat<-as.vector(t(dat))
  dat<-as.numeric(dat)
  sd = sd(dat,na.rm=TRUE)
  x<-as.vector(t(LG[i,11]))
  y<-as.vector(t(LG[i,3]))
  compare<-abs(as.numeric(x)-as.numeric(y))
  if (!anyNA(LG[i,3:11])){ 
      if(compare > sd){
        LG[i, 12]<-predict(lm(dat~year),newdata= data.frame(year = 2016))
        LG[i, 13]<-predict(lm(dat~year),newdata= data.frame(year = 2017))
        LG[i, 14]<-predict(lm(dat~year),newdata= data.frame(year = 2018)) 
        LG[i,15]<-dwtest(dat~year)
        #DW test critcal vaule, testing if model is valid
        if (LG[i,15]<1.32){
          LG[i,12]<- mean(dat,na.rm=TRUE)
          LG[i,13]<- mean(dat,na.rm=TRUE)
          LG[i,14]<- mean(dat, na.rm=TRUE)
        }
      }
      else{
        LG[i,12]<- mean(dat,na.rm=TRUE)
        LG[i,13]<- mean(dat,na.rm=TRUE)
        LG[i,14]<- mean(dat, na.rm=TRUE)
      }
    }
  else{ LG[i,12]<- mean(dat,na.rm=TRUE)
       LG[i,13]<- mean(dat,na.rm=TRUE)
       LG[i,14]<- mean(dat, na.rm=TRUE)  
  }  
}

Attrition<-`HE.Attrition.Data.Main,.2005.2013` <- read.csv("~/Downloads/Higher Education Attrition Rates/HE Attrition Data Main, 2005-2013.csv")
for (i in 1:37){
  dat<-(Attrition[1:9,i])
  dat<-as.vector(t(dat))
  dat<-as.numeric(dat)
  Attrition[10,i]<-predict(lm(dat~year),newdata= data.frame(year = 2016))
  Attrition[11,i]<-predict(lm(dat~year),newdata= data.frame(year = 2017))
  Attrition[12,i]<-predict(lm(dat~year),newdata= data.frame(year = 2018))
}
Attrition$average<-rowMeans(Attrition)
for (i in 26:36){
  for (j in 10:12){
    uni_cat<-data.frame(data.frame(matrix(NA, nrow = 3, 
                                          ncol = 10)))
    #rownames(uni_cat)<-c(2016,2017,2018)
    colnames(uni_cat)<-c()
    x<-i-25
    y<-j-9
    uni_cat[y,x]<-Attrition[j,i]/Attrition$average[j]
  }
}
#Assume VET enrollments are two years


#projection

for (i in 1:11){
  Year<-c(2009,2010,2011,2012,2013,2014,2015)
  dat<-(offers[1:7,i])
  dat<-paste0(dat)
  dat<-as.numeric(dat)
  sd<-sd(dat)
  compare<-abs(dat[1]-dat[7])
  if (compare>sd){
    offers[8,i]<-predict(lm(dat~Year),newdata= data.frame(Year = 2016))
    offers[9,i]<-predict(lm(dat~Year),newdata= data.frame(Year = 2017))
    offers[10,i]<-predict(lm(dat~Year),newdata= data.frame(Year = 2018))
  }
  else{
    offers[8,i]<-mean(dat)
    offers[9,i]<-mean(dat)
    offers[10,i]<-mean(dat)
    
  }
}


for (i in 1:96){
  Yeart<-c(2010,2011,2012,2013,2014)
  dat<-(tafepredictors[i,3:7])
  dat<-paste0(dat)
  dat<-as.numeric(dat)
  sd<-sd(dat)
  compare<-abs(dat[1]-dat[5])
  if (compare>sd){
    tafepredictors[i,8]<-predict(lm(dat~Yeart),newdata= data.frame(Yeart = 2016))
    tafepredictors[i,9]<-predict(lm(dat~Yeart),newdata= data.frame(Yeart = 2017))
    tafepredictors[i,10]<-predict(lm(dat~Yeart),newdata= data.frame(Yeart = 2018))
  }
  else{
    tafepredictors[i,8]<-mean(dat)
    tafepredictors[i,9]<-mean(dat)
    tafepredictors[i,10]<-mean(dat)
    
  }
}
TAFE2016<-data.frame(matrix(NA, nrow = nrow(TAFEfinal), 
                  ncol = ncol(TAFEfinal)))
predict2016<-paste(offers[1:96,1])
predict2016<-as.numeric(predict2016)
TAFE2016[,2:4]<-TAFEfinal[,2:4]
TAFE2016<-predict2016*TAFEfinal[,4:56]

TAFE2017<-data.frame(matrix(NA, nrow = nrow(TAFEfinal), 
                            ncol = ncol(TAFEfinal)))
predict2017<-paste(offers[1:96,2])
predict2017<-as.numeric(predict2017)
TAFE2017[,2:4]<-TAFEfinal[,2:4]
TAFE2017<-predict2017*TAFEfinal[,4:56]

TAFE2018<-data.frame(matrix(NA, nrow = nrow(TAFEfinal), 
                            ncol = ncol(TAFEfinal)))
predict2018<-paste(offers[1:96,3])
predict2018<-as.numeric(predict2016)
TAFE2018[,2:4]<-TAFEfinal[,2:4]
TAFE2018<-predict2016*TAFEfinal[,4:56]
