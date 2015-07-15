library(quantmod)
library(leaps)
library(Hmisc) #describe
library(psych) #describe
library(GPArotation)
library(pastecs) #stat.desc
library(corrgram) # for corralation analysis
library(gvlma)
library(relaimpo)
library(RSQLite)

library(xlsx)
library(RMySQL)
library(ggplot2) #add for ggplot
library(reshape2)
library(dplyr)

#library(bigmemory)


##deal with CSV file
##1. delete the "," in number
##2. change the date to 
par(mfrow=c(1,1),mar=c(4,4,0,0))
mycolor=rainbow(20)


# about the space
#gc()

#----------------------------------------------------------------------------------------------
#1# read and clean the data
#----------------------------------------------------------------------------------------------

conn <- dbConnect(MySQL(), dbname = "test", 
                  username="root", password="123456",host="127.0.0.1",port=3306)
project=dbGetQuery(conn,"set names gbk")
#project=dbGetQuery(conn,"set names utf8")
project=dbGetQuery(conn,"select * from simple_jd_zc")
user=dbGetQuery(conn,"select * from simple_jd_zc_data")
dbDisconnect(conn)

user=tbl_df(user)
names(project)=c( "kid" ,   "proid",     "name" ,   "status"  ,  "base" ,"raised"     ,"sup"    , "follower" ,    "like"    , "date"  ,  "corp" )
class(project$date)=c('POSIXt','POSIXct')
project=transform(project,kid=as.integer(kid),proid=as.integer(proid),
                  name=factor(name),status=factor(status),corp=factor(corp))
names(user)=c("kid" ,  "proid",   "funder"  ,"price", "start",  "support" )
user=transform(user, kid=as.integer(kid), proid=as.integer(proid),funder=factor(funder),price=as.integer(price), start=as.integer(start),support=as.integer(support))

project$funder=0
for(i in 1:nrow(project)){
  proid=project[i,"proid"]
  project[i,"funder"]=sum(user$proid==proid)
  
}


save(file="jdzc20150713.Rdata",user,project)






load(file="jdzc20150713.Rdata")

#----------------------------------------------------------------------------------------------
#2# overview project: summary
#----------------------------------------------------------------------------------------------

#p.gt : project group by name
p.gt=group_by(project,status)

p.sgt=summarise(p.gt, project.num=n(),finbase.pro=mean(base),finbase.tot=sum(base),finamout.pro=mean(raised),finamout.tot=sum(raised),
                raisedrate=sum(raised)/sum(base),supporter.pro=mean(sup),supporter.tot=sum(sup),follower.pro=mean(follower),
                follower.tot=sum(follower),liker.pro=mean(like),liker.tot=sum(like),uniquecorporation=length(unique(corp)))
p.sgt$status=as.character(p.sgt$status)

p.sgt[5,]=data.frame("Summary",nrow(project),mean(project$base),sum(project$base),mean(project$raised),sum(project$raised),
                     sum(project$raised)/sum(project$base),mean(project$sup),sum(project$sup),mean(project$follower),
                     sum(project$follower),mean(project$like),sum(project$like),length(unique(project$corp)),stringsAsFactors=F)
#as.data.frame(p.sgt)
knitr::kable(as.data.frame(p.sgt),caption="Summary Overview")

(p <- ggplot(data=project,aes(x=status,fill=factor(status))) + geom_bar(position='dodge')+labs(title="Project Number By Status"))

pic=select(project, name,status,base,raised)
mpic=melt(pic, id=c("name","status"))
(p <- ggplot(data=mpic,aes(x=status,weight=value,fill=factor(variable))) + geom_bar(position='dodge')+labs(title="Financing Amout By Status"))

pic=select(project, name,status,sup, follower, like)
mpic=melt(pic, id=c("name","status"))
(p <- ggplot(data=mpic,aes(x=status,weight=value,fill=factor(variable))) + geom_bar(position='dodge')+labs(title="Supporter By Status"))

(p <- ggplot(data=p.sgt,aes(x=status,weight=raisedrate,fill=factor(status))) + geom_bar(position='dodge')+labs(title="Raised Rate By Status"))
(p <- ggplot(data=p.sgt,aes(x=status,weight=uniquecorporation,fill=factor(status))) + geom_bar(position='dodge')+labs(title="Corporation Number By Status"))


#----------------------------------------------------------------------------------------------
#3# financing amout analysis
#----------------------------------------------------------------------------------------------

finance=select(project, name,status,base)
names(finance)=c( "name",     "status"  , "object"  )

#3.1 base analysis
table(finance$object)
range(finance$object)

breaks=c(0,6000,51000,501000,5001000)
finance$interval=0
finance[which(finance$object<=breaks[2]),"interval"]=1
finance[which(finance$object<=breaks[3] & finance$object>breaks[2]),"interval"]=2
finance[which(finance$object<=breaks[4] & finance$object>breaks[3]),"interval"]=3
finance[which(finance$object<=breaks[5] & finance$object>breaks[4]),"interval"]=4
finance[which(finance$object>breaks[5]),"interval"]=5

gf=group_by(finance,interval)
sgf=summarize(gf,project.num=n(),totamout=sum(object))
sgf=transform(sgf,pronum.ratio=project.num/nrow(finance),amout.ratio=totamout/sum(finance$object))
row.names(sgf)=breaks
knitr::kable(as.data.frame(sgf),caption="target financing amout table")
  
p <- ggplot(finance,aes(object))+xlim(0,530000)+labs(title="financing base amount distribution in 500,000")
#p + geom_histogram(position = 'identity',alpha=0.5,binwidth=20000,aes(y = ..density..))+ stat_density(geom = 'line',position = 'identity')
p + geom_histogram(position = 'identity',alpha=0.5,binwidth=5000,aes(fill = factor(status)))

p <- ggplot(finance,aes(object))+xlim(0,110000)+labs(title="financing base amount distribution in 100,000")
p + geom_histogram(position = 'identity',alpha=0.5,binwidth=1000,aes(fill = factor(finance$status)))



#3.2 Raised financing amount analysis----------------------------
finance=select(project, name,status,raised)
names(finance)=c( "name",     "status"  , "object"  )

table(finance$object)
range(finance$object)

breaks=c(0,50000,200000,1000000,5000000)
finance$interval=0
finance[which(finance$object<=breaks[2]),"interval"]=1
finance[which(finance$object<=breaks[3] & finance$object>breaks[2]),"interval"]=2
finance[which(finance$object<=breaks[4] & finance$object>breaks[3]),"interval"]=3
finance[which(finance$object<=breaks[5] & finance$object>breaks[4]),"interval"]=4
finance[which(finance$object>breaks[5]),"interval"]=5

gf=group_by(finance,interval)
sgf=summarize(gf,project.num=n(),totamout=sum(object))
sgf=transform(sgf,pronum.ratio=project.num/nrow(finance),amout.ratio=totamout/sum(finance$object))
row.names(sgf)=breaks
knitr::kable(as.data.frame(sgf),caption="Actural Financing Amout Table")

p <- ggplot(finance,aes(object))+xlim(0,30000000)+labs(title="financing raised amount distribution in 30,000,000")
p + geom_histogram(position = 'identity',alpha=0.5,binwidth=500000,aes(fill = factor(status)))

p <- ggplot(finance,aes(object))+xlim(0,1000000)+labs(title="financing raised amount distribution in 1,000,000")
p + geom_histogram(position = 'identity',alpha=0.5,binwidth=10000,aes(fill = factor(finance$status)))


#3.3 Raised ratio financing amount analysis-----------------------------------------------

finance=select(project, name,status, base,raised)
finance$raisedrate=finance$raised/finance$base
names(finance)=c( "name",     "status", "base","raised", "object"  )

p <- ggplot(finance,aes(object))+xlim(0,100)+labs(title="financing raisedrate amount distribution in 100")
p + geom_histogram(position = 'identity',alpha=0.5,binwidth=1,aes(fill = factor(status)))

p <- ggplot(finance,aes(object))+xlim(0,30)+labs(title="financing raisedrate amount distribution in 30")
p + geom_histogram(position = 'identity',alpha=0.5,binwidth=0.2,aes(fill = factor(status)))

breaks=c(0,5,10,20,30)
finance$interval=1
finance[which(finance$object<=breaks[2]),"interval"]=1
finance[which(finance$object<=breaks[3] & finance$object>breaks[2]),"interval"]=2
finance[which(finance$object<=breaks[4] & finance$object>breaks[3]),"interval"]=3
finance[which(finance$object<=breaks[5] & finance$object>breaks[4]),"interval"]=4
finance[which(finance$object>breaks[5]),"interval"]=5

gf=group_by(finance,interval)
sgf=summarize(gf,project.num=n())
sgf=transform(sgf,pronum.ratio=project.num/nrow(finance))
row.names(sgf)=breaks

knitr::kable(as.data.frame(sgf),caption="Raised Rate Table")

#----------------------------------------------------------------------------------------------
#4# supporter per project 
#----------------------------------------------------------------------------------------------

finance=select(project, name,status,sup,raised)
finance=transform(finance, perperson=raised/sup)
names(finance)=c( "name",     "status"  , "object"  ,"raised","perperson")


table(finance$object)
range(finance$object)

breaks=c(0,500,4000,10000,50000)
finance$interval=0
finance[which(finance$object<=breaks[2]),"interval"]=1
finance[which(finance$object<=breaks[3] & finance$object>breaks[2]),"interval"]=2
finance[which(finance$object<=breaks[4] & finance$object>breaks[3]),"interval"]=3
finance[which(finance$object<=breaks[5] & finance$object>breaks[4]),"interval"]=4
finance[which(finance$object>breaks[5]),"interval"]=5

gf=group_by(finance,interval)
sgf=summarize(gf,project.num=n(),totsup=sum(object),totamout=sum(raised),amoutperperson=sum(raised)/sum(object))
sgf=transform(sgf,pronum.ratio=project.num/nrow(finance),sup.ratio=totsup/sum(finance$object),amout.ratio=totamout/sum(finance$raised))
row.names(sgf)=breaks
knitr::kable(as.data.frame(sgf),caption="Supporter Table")


p <- ggplot(finance,aes(object))+xlim(0,10000)+labs(title="Supporter NO distribution in 10,000")
p + geom_histogram(position = 'identity',alpha=0.5,binwidth=100,aes(fill = factor(finance$status)))


#----------------------------------------------------------------------------------------------
#5# financial coporation analysis
#----------------------------------------------------------------------------------------------

p.gt=group_by(project,corp)
p.sgt=summarise(p.gt, project.num=n(),finbase.pro=mean(base),finbase.tot=sum(base),finamout.pro=mean(raised),finamout.tot=sum(raised),
                raisedrate=sum(raised)/sum(base),supporter.pro=mean(sup),supporter.tot=sum(sup),follower.pro=mean(follower),
                follower.tot=sum(follower),liker.pro=mean(like),liker.tot=sum(like))
p.sgt=arrange(p.sgt,desc(project.num),desc(raisedrate))
#head(as.data.frame(p.sgt))
knitr::kable(head(as.data.frame(p.sgt),10),caption=" financial coporation Top10 Table")

#----------------------------------------------------------------------------------------------
#6# Investor analysis
#----------------------------------------------------------------------------------------------

head(user)
u.gt=group_by(user,funder)
u.sgt=summarise(u.gt,project.num=n(),invest.pro=mean(price),invest.tot=sum(price),invest.max=max(price),
                start=sum(start),support=max(support))
u.sgt=arrange(u.sgt,desc(invest.tot))

knitr::kable(head(u.sgt,10),caption=" Investor Top10 Table")
#describe(u.sgt)
knitr::kable(summary(u.sgt),caption=" Investor Summary Table")

knitr::kable(as.data.frame(table(cut(u.sgt$invest.tot,breaks=c(0,100,1000,10000,100000,1000000,10000000,100000000)))),caption=" Investor tot investment distribution Table")
knitr::kable(as.data.frame(table(cut(u.sgt$invest.pro,breaks=c(0,100,1000,10000,100000,1000000,10000000,100000000)))),caption=" Investor investment/project distribution Table")

p <- ggplot(u.sgt,aes(project.num))+xlim(0,100)+labs(title="Investor projectnum distribution")
p + geom_histogram(position = 'identity',alpha=0.5,binwidth=1)

p <- ggplot(u.sgt,aes(invest.pro))+xlim(0,101)+labs(title="Investor investment per project distribution")
p + geom_histogram(position = 'identity',alpha=0.5,binwidth=1)

p <- ggplot(u.sgt,aes(invest.tot))+xlim(0,101)+labs(title="Investor tot investment distribution")
p + geom_histogram(position = 'identity',alpha=0.5,binwidth=1)

p <- ggplot(u.sgt,aes(invest.max))+xlim(0,101)+labs(title="Investor max investment distribution")
p + geom_histogram(position = 'identity',alpha=0.5,binwidth=1)

p <- ggplot(u.sgt,aes(start))+xlim(0,10)+labs(title="Investor financing activity distribution")
p + geom_histogram(position = 'identity',alpha=0.5,binwidth=1)



