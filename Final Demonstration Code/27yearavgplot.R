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
#str(flights)

#' This isn't your usual head function, but it acts the same way:
#head(flights) 
# or you could use tbl_df to print it pretty
#tbl_df(head(flights))
delay=flights %.% select(year:dayofweek,uniquecarrier,arrdelay,depdelay,carrierdelay:lateaircraftdelay,origin:dest)

#tbl_df(head(delay))

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







pop_size <- delay %.% group_by(uniquecarrier) %.% summarise(num=n())
popnsize=as.data.frame(pop_size)

# =============================================
# 27 year average plot, sample v population
# =============================================

caravg=df.a %.% group_by(uniquecarrier) %.% summarise(carrieravg=mean(avgdely))
sampledat=merge(sampledat,caravg,"uniquecarrier")
sampledat=merge(x=get2013dat,y=popnsize,by="uniquecarrier")
sampledat=merge(x=sampledat,y=caravg,by="uniquecarrier")
sampledat$varmean = (1-(sampledat$Size/sampledat$num))*(sampledat$AvgVar/sampledat$Size)

qplot(reorder(uniquecarrier,carrieravg),AvgDel,data=sampledat,xlab="Carrier",ylab="Average Arrival Delay Plus Error",size=2.5) + 
  geom_errorbar(aes(uniquecarrier,ymin=AvgDel-2*sqrt(varmean),ymax=AvgDel+2*sqrt(varmean)),size=1) +
  geom_point(aes(y=carrieravg),size=2.5,color="red")

# =============================================
# 27 year average plot, sample v population
# =============================================

newsampledat <- sampledat %.% mutate(uniquecarrier = as.character(uniquecarrier))
newsampledat <- inner_join(newsampledat,carriercodes,"uniquecarrier")
newsampledat[newsampledat$uniquecarrier=="US",]$carrier <- "US Airways Inc."

newsampledat$carrier <- with(newsampledat, reorder(carrier, desc(carrieravg)))

pdf("pop27yrplot.pdf",height=7,width=12)
ggplot(newsampledat,aes(carrier,carrieravg),size=2.5) + 
  geom_errorbar(aes(carrier,ymin=AvgDel-2*sqrt(varmean),ymax=AvgDel+2*sqrt(varmean)),size=1) +
  geom_point(aes(y=carrieravg),size=2.5,color="red") +
  geom_point(aes(y=AvgDel),size=2.5,color="black") +
  ylab("Average Arrival Delay") +
  xlab("") +
  ggtitle("27 Year Average and SE bars") +
  coord_flip()
dev.off()
###############   Population size 
1             9E  1342097
2             AA  17678497
3             AS  3598331
4             B6  1893914
5             DL  19922730
6             EV  4133489
7             F9  746265
8             FL  2406214
9             HA  627969
10            MQ  6190063
11            OO  6069532
12            UA  15368202
13            VX  111875
14            WN  21659743
15            YV  1704176
26            US  16122106
#############################
