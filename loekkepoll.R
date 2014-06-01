require(RCurl)
lp<-read.csv(text=getURL("https://raw.githubusercontent.com/fghjorth/loekkepoll/master/loekkepoll3.csv"))
lp$supopp01<-NA

#create support/oppose variable
lp$supopp01[lp$support==1]<-1
lp$supopp01[lp$oppose==1]<-0

#binary opposition
str(lp)
lp$oppose01<-0
lp$oppose01[lp$oppose==1]<-1
str(lp)

#linear variable
lp$oppdksup<-0
lp$oppdksup[lp$oppose==1]<- -1
lp$oppdksup[lp$support==1]<- 1

#fit models with tax base
summary(mod1<-glm(supopp01~taxbase,data=lp,family="binomial"))
summary(mod2<-glm(oppose01~taxbase,data=lp,family="binomial"))
summary(mod3<-lm(oppdksup~taxbase,data=lp))


