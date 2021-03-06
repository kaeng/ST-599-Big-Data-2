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


#########
# getting data set and stratum stats calculations
#########

# have EnvKatie.RData loaded

carriers2013 <- unique(Avgs_by_month_local$uniquecarrier)

get2013dat <- delay %.% 
  select(year,uniquecarrier,arrdelay,depdelay) %.%
  filter(uniquecarrier == "US") %.% filter(random() < .001) %.% 
  summarise(sampleavg = mean(arrdelay), samplevar = var(arrdelay), samplesize = n())

#|
#           uniquecarrier == "MQ" |  #KT
#           uniquecarrier == "YV" |  #KT
#           uniquecarrier == "AS" |  #KT
#           uniquecarrier == "F9" |  #LW
#           uniquecarrier == "DL" |  #LW
#           uniquecarrier == "UA" |  #LW
#           uniquecarrier == "AA" |  #LW
#           uniquecarrier == "HA" |  #BB
#           uniquecarrier == "VX" |  #BB
#           uniquecarrier == "B6" |  #BB
#           uniquecarrier == "OO" |  #BB
#           uniquecarrier == "FL" |  #FW
#           uniquecarrier == "9E" |  #FW
#           uniquecarrier == "EV" |  #FW
#           uniquecarrier == "WN")   #FW

  
explain(get2013dat)


sample2013 <- collect(get2013dat)



##############################
# Lu's Code
##############################

summary_F9 <- delay %.% 
  select(year,uniquecarrier,arrdelay) %.%
  filter(uniquecarrier == "F9") %.% filter(random() < .001) %.% 
  summarise(sampleavg = mean(arrdelay), samplevar = var(arrdelay), samplesize = n())

summary_DL <- delay %.% 
  select(year,uniquecarrier,arrdelay) %.%
  filter(uniquecarrier == "DL") %.% filter(random() < .001) %.% 
  summarise(sampleavg = mean(arrdelay), samplevar = var(arrdelay), samplesize = n())

summary_UA <- delay %.% 
  select(year,uniquecarrier,arrdelay) %.%
  filter(uniquecarrier == "UA") %.% filter(random() < .001) %.% 
  summarise(sampleavg = mean(arrdelay), samplevar = var(arrdelay), samplesize = n())

summary_AA <- delay %.% 
  select(year,uniquecarrier,arrdelay) %.%
  filter(uniquecarrier == "AA") %.% filter(random() < .001) %.% 
  summarise(sampleavg = mean(arrdelay), samplevar = var(arrdelay), samplesize = n())


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


##############################
# Ben's Code
##############################

get2013dat=data.frame(sampleavg=0,samplevar=0,samplesize=0)
for (i in c("US","MQ","YV","AS","F9","DL","UA","AA","HA","VX","B6","OO","FL","9E","EV","WN")){
  get2013dat <- rbind(get2013dat, collect(delay %.% 
                                            select(year,uniquecarrier,arrdelay,depdelay) %.%
                                            filter(uniquecarrier == i) %.% filter(random() < .001) %.% 
                                            summarise(sampleavg = mean(arrdelay), samplevar = var(arrdelay), samplesize = n())))
  print(i)
}

##############################
# FangWu's Code
# getting sample data and analyzing

getdat_FL <- delay %.% 
  select(year,uniquecarrier,arrdelay,depdelay) %.%
  filter(uniquecarrier == "FL") %.% filter(random() < .001) %.% 
  summarise(sampleavg = mean(arrdelay), samplevar = var(arrdelay), samplesize = n())

sampledat_FL <- collect(getdat_FL)

getdat_9E <- delay %.% 
  select(year,uniquecarrier,arrdelay,depdelay) %.%
  filter(uniquecarrier == "9E") %.% filter(random() < .001) %.% 
  summarise(sampleavg = mean(arrdelay), samplevar = var(arrdelay), samplesize = n())

sampledat_9E <- collect(getdat_9E)

getdat_EV <- delay %.% 
  select(year,uniquecarrier,arrdelay,depdelay) %.%
  filter(uniquecarrier == "EV") %.% filter(random() < .001) %.% 
  summarise(sampleavg = mean(arrdelay), samplevar = var(arrdelay), samplesize = n())

sampledat_EV <- collect(getdat_EV)

getdat_WN <- delay %.% 
  select(year,uniquecarrier,arrdelay,depdelay) %.%
  filter(uniquecarrier == "WN") %.% filter(random() < .001) %.% 
  summarise(sampleavg = mean(arrdelay), samplevar = var(arrdelay), samplesize = n())

sampledat_WN <- collect(getdat_WN)


