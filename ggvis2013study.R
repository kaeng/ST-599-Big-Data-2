# ==================================================
# can load 2013study environment so that data are already loaded
# and can plot immediately
# ==================================================

library(dplyr)
library(RPostgreSQL)
library(ggplot2)
library(ggvis)

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

delay=flights %.% select(year:dayofweek,uniquecarrier,arrdelay)

# call = delay %.% 
#   select(year,month,uniquecarrier,arrdelay) %.%
#   filter(year == "2013" & uniquecarrier == i) %.% group_by(uniquecarrier,month) %.% filter(random() < .001) %.% 
#   summarise(sampleavg = mean(arrdelay), samplevar = var(arrdelay), samplesize = n())

# explain(call)

#test <- collect(call)

carriercodes <- read.csv("carriers.csv",header=TRUE,col.names=c("uniquecarrier","carrier"),
                       stringsAsFactors = FALSE)

carriercodes <- tbl_df(carriercodes)

# =================================
# Population in 2013
# =================================

Avgs_by_month <- delay %.%
  group_by(uniquecarrier,year,month) %.% filter(year=="2013") %.%
  summarise(avgdepdelay=mean(depdelay),
            avgarrdelay=mean(arrdelay))

#explain(Avgs_by_month)

Avgs_by_month_local <- collect(Avgs_by_month)

Avgs_by_month_local <- Avgs_by_month_local %.% mutate(date=ISOdate(year,month,1))


popandsim[popandsim$uniquecarrier=="US",]$carrier <- "US Airways Inc."

pdf("pop2013plot.pdf",height=7,width=12)
ggplot(popandsim,aes(date,avgarrdelay)) +
  geom_line(aes(group=carrier,colour=carrier),size=1.25)
dev.off()


# =================================
# The interactPlot for Pop 2013
# =================================

library(ggplot2); library(ggvis); library(dplyr)

dat2013 <- Avgs_by_month_local

#theme_set(theme_grey(base_size=18))

#pdf("2013plot.pdf",height=7,width=12)
#qplot(x=date,y=avg,data=alldata[alldata$year==2013,],geom="line",group=uniquecarrier,colour=uniquecarrier,
#      ylab="Average Departure Delay",ylim=c(-10,30),xlab="") + geom_hline(yintercept=0)
#dev.off()


# doing with ggplot, to model the ggvis
#h <- ggplot(dat2013, aes(date, avgarrdelay))
#h + geom_line(aes(group = uniquecarrier,colour=uniquecarrier))

# read in carrier codes
#carriercodes <- read.csv("carriers.csv",header=TRUE,col.names=c("uniquecarrier","carrier"),
#                         stringsAsFactors = FALSE)

new <- left_join(x=dat2013,y=carriercodes,by="uniquecarrier")

# creating label to only have carrier name appear once on plot
new <- new %.% mutate(carrier.y = ifelse(month==12,carrier,""))

# ggvis code to at least get the profile plot
ggvis(new, props(x = ~date, y = ~avgarrdelay, stroke = ~as.factor(carrier), strokeOpacity := 1)) +
  layer_line(props(strokeWidth.hover := 4, strokeWidth := 1, strokeOpacity.hover := 1)) +
  guide_legend(fill = "stroke") +
  layer_text(props(text := ~carrier.y, dx := 5, dy := 0, strokeOpacity := 0,
                   strokeOpacity.hover:= 1, fontSize := 5, fontSize.hover := 18))

# =================================
# Stratified sampling in 2013
# =================================
set.seed(599)

get2013dat <- NA
for (i in c("US","MQ","YV","AS","F9","DL","UA","AA","HA","VX","B6","OO","FL","9E","EV","WN")){
  get2013dat <- rbind(get2013dat, collect(delay %.% 
                                            select(year,month,uniquecarrier,arrdelay) %.%
                                            filter(year == "2013" & uniquecarrier == i) %.% group_by(year,month,uniquecarrier) %.% filter(random() < .01) %.% 
                                            summarise(sampleavg = mean(arrdelay), samplevar = var(arrdelay), samplesize = n())
  ))
}

get2013dat <- get2013dat[-1,]

pop_size <- collect(delay %.% filter(year=="2013") %.% group_by(uniquecarrier,month) %.%
                      summarise(num=n()))

all2013data <- inner_join(get2013dat,pop_size,by=intersect(names(get2013dat),names(pop_size)))

all2013data <- all2013data %.% mutate(StdError = (1-(samplesize/num))*(samplevar/samplesize),
                                      date = ISOdate(year,month,1))

all2013data <- inner_join(all2013data,carriercodes,"uniquecarrier")

# =================================
# Sample data 2013 ggvis plot... more sad
# =================================

popandsim <- left_join(x=new[,c("uniquecarrier","year","month","date","avgarrdelay")],
                       y=all2013data,by=c("date","uniquecarrier","year","month"))

ggvis(popandsim, props(x = ~date, y = ~sampleavg, stroke = ~as.factor(carrier), strokeOpacity := 0)) +
  layer(popandsim[popandsim$uniquecarrier=="FL",],
        props(x = ~date, y = ~avgarrdelay, stroke := "tomato", strokeWidth := 4,
              strokeOpacity := input_checkbox(label="AirTran Airways Corporation",
                                              map = function(val) ifelse(val,1,0))),
        layer_line()) + 
  layer(popandsim[popandsim$uniquecarrier=="AS",],
        props(x = ~date, y = ~avgarrdelay, stroke := "orange", strokeWidth := 4,
              strokeOpacity := input_checkbox(label="Alaska Airlines Inc.",
                                              map = function(val) ifelse(val,1,0))),
        layer_line()) +
  layer(popandsim[popandsim$uniquecarrier=="AA",],
        props(x = ~date, y = ~avgarrdelay, stroke := "Green", strokeWidth := 4,
              strokeOpacity := input_checkbox(label="American Airlines Inc.",
                                              map = function(val) ifelse(val,1,0))),
        layer_line()) +
    
  layer(popandsim[popandsim$uniquecarrier=="MQ",],
        props(x = ~date, y = ~avgarrdelay, stroke := "firebrick", strokeWidth := 4,
              strokeOpacity := input_checkbox(label="American Eagle Airlines Inc.",
                                              map = function(val) ifelse(val,1,0))),
        layer_line()) +
  layer(popandsim[popandsim$uniquecarrier=="EV",],
        props(x = ~date, y = ~avgarrdelay, stroke := "saddlebrown", strokeWidth := 4,
              strokeOpacity := input_checkbox(label="Atlantic Southeast Airlines",
                                              map = function(val) ifelse(val,1,0))),
        layer_line()) + 
  layer(popandsim[popandsim$uniquecarrier=="DL",],
        props(x = ~date, y = ~avgarrdelay, stroke := "steelblue", strokeWidth := 4,
              strokeOpacity := input_checkbox(label="Delta Airlines Inc.",
                                              map = function(val) ifelse(val,1,0))),
        layer_line()) + 
  layer(popandsim[popandsim$uniquecarrier=="F9",],
        props(x = ~date, y = ~avgarrdelay, stroke := "magenta", strokeWidth := 4,
              strokeOpacity := input_checkbox(label="Frontier Airlines Inc.",
                                              map = function(val) ifelse(val,1,0))),
        layer_line()) +
  layer(popandsim[popandsim$uniquecarrier=="HA",],
        props(x = ~date, y = ~avgarrdelay, stroke := "grey", strokeWidth := 4,
              strokeOpacity := input_checkbox(label="Hawaiian Airlines Inc.",
                                              map = function(val) ifelse(val,1,0))),
        layer_line()) +
  layer(popandsim[popandsim$uniquecarrier=="B6",],
        props(x = ~date, y = ~avgarrdelay, stroke := "darkkhaki", strokeWidth := 4,
              strokeOpacity := input_checkbox(label="JetBlue Airways",
                                              map = function(val) ifelse(val,1,0))),
        layer_line()) +
  layer(popandsim[popandsim$uniquecarrier=="YV",],
        props(x = ~date, y = ~avgarrdelay, stroke := "skyblue", strokeWidth := 4,
              strokeOpacity := input_checkbox(label="Mesa Airlines Inc.",
                                              map = function(val) ifelse(val,1,0))),
        layer_line()) +
  layer(popandsim[popandsim$uniquecarrier=="9E",],
        props(x = ~date, y = ~avgarrdelay, stroke := "slateblue", strokeWidth := 4,
              strokeOpacity := input_checkbox(label="Pinnacle Airlines Inc.",
                                              map = function(val) ifelse(val,1,0))),
        layer_line()) +
  layer(popandsim[popandsim$uniquecarrier=="OO",],
        props(x = ~date, y = ~avgarrdelay, stroke := "orangered", strokeWidth := 4,
              strokeOpacity := input_checkbox(label="Skywest Airlines Inc.",
                                              map = function(val) ifelse(val,1,0))),
        layer_line()) +  
  layer(popandsim[popandsim$uniquecarrier=="WN",],
        props(x = ~date, y = ~avgarrdelay, stroke := "olivedrab", strokeWidth := 4,
              strokeOpacity := input_checkbox(label="Southwest Airlines Co.",
                                              map = function(val) ifelse(val,1,0))),
        layer_line()) +
  layer(popandsim[popandsim$uniquecarrier=="US",],
        props(x = ~date, y = ~avgarrdelay, stroke := "red", strokeWidth := 4,
              strokeOpacity := input_checkbox(label="US Airways Inc.",
                                              map = function(val) ifelse(val,1,0))),
        layer_line()) +
  layer(popandsim[popandsim$uniquecarrier=="UA",],
        props(x = ~date, y = ~avgarrdelay, stroke := "purple", strokeWidth := 4,
              strokeOpacity := input_checkbox(label="United Air Lines Inc.",
                                              map = function(val) ifelse(val,1,0))),
        layer_line()) +

  layer(popandsim[popandsim$uniquecarrier=="VX",],
            props(x = ~date, y = ~avgarrdelay, stroke := "brown", strokeWidth := 4,
                  strokeOpacity := input_checkbox(label="Virgin America",
                                                  map = function(val) ifelse(val,1,0))),
        layer_line())



# ======================================================
# Static plot (population versus sample and error bars)
# ======================================================

sampfunc <- function(name){

  theme_set(theme_grey(base_size=18))
  carr <- carriercodes$uniquecarrier[which(carriercodes$carrier==name)]
  
qplot(x=date,y=sampleavg,data=popandsim[popandsim$uniquecarrier==carr,],xlab="",
      ylab="Average Arrival Delay",size=2.5,main=paste(name),ylim=c(-20,50)) +
  geom_errorbar(aes(date,ymin=sampleavg-2*sqrt(StdError),ymax=sampleavg+2*sqrt(StdError)),size=1) +
  geom_line(aes(x=date,y=avgarrdelay),size=1,color="red")

}

for (i in 1:16){
  airline <- sort(unique(popandsim$carrier))[i]
  print(sampfunc(airline))
}

