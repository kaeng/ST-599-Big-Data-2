# Are there any airlines with flight delays that are systematically worse
# than other airlines? Please examine this using 25 years of flight data for
# the entire country. 

library(dplyr)
library(RPostgreSQL)
library(ggplot2)

# --- setting up parameters to access the data base --- #
endpoint <- "flights.cwick.co.nz"
user <- "student"
password <- "password"

ontime <- src_postgres("ontime", 
                       host = endpoint,
                       port = 5432,
                       user = user,
                       password = password)
# -------- #

flights <- tbl(ontime, "flights")
#' `flights` is special object, it points to the remote database, but
#' you can work with it in dplyr like it is a data.frame.
str(flights)

#' This isn't your usual head function, but it acts the same way:
head(flights) 
# or you could use tbl_df to print it pretty
tbl_df(head(flights))
delay=flights %.% select(year:dayofweek,uniquecarrier,arrdelay,depdelay,carrierdelay:lateaircraftdelay,origin:dest)
tbl_df(head(delay))
# Getting number of flights by carrier
num=delay %.% group_by(uniquecarrier) %.% summarise(num=n())
# Getting all carriers with data frame (takes long time for big carriers)

df.a=data.frame(uniquecarrier=c(0),year=c(0),month=c(0),avgdely=c(0))
for(i in 1987:2013){
  df.a=rbind(df.a,as.data.frame(delay %.% 
  filter(year==i) %.%
  group_by(uniquecarrier,year,month) %.% 
  summarise(avgdely=mean(arrdelay))))
  print(i)
  }
head(df.a)
df.a$date=ISOdate(df.a$year,df.a$month,1)
df.a=df.a[-1,]
df.b = df.a %.% group_by(year,uniquecarrier) %.% summarise(avgdelz=mean(avgdely))
qplot(date,avgdely,data=df.a,group=uniquecarrier,color=uniquecarrier,geom="line")+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=24,face="bold"))
qplot(year,avgdelz,data=df.b,color=uniquecarrier,geom="line")
