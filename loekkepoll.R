require(RCurl)
lp<-read.csv(text=getURL("https://raw.githubusercontent.com/fghjorth/loekkepoll/master/loekkepoll3.csv"))
#kilder: twitter, TV2, Politiken, og Kommunale Nøgletal, noegletal.dk
density<-read.csv(text=getURL("https://raw.githubusercontent.com/fghjorth/loekkepoll/master/density.csv"))
lp<-merge(lp,density,by="knr",all.x=T)
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

#summary stats
str(lp)
summary(lp$taxbase)
as.character(lp$name)
View(lp[order(lp$taxbase),])
lp[as.character(lp$name)=="Esbjerg Kommune",]
lp[as.character(lp$name)=="Silkeborg Kommune",]
summary(lp$taxbase[lp$oppdksup==-1])
lp[lp$oppdksup==-1,]
table(lp$oppdksup==-1,lp$taxbase>176800)
2/27
12/57
as.character(lp$name[lp$oppdksup==-1])

#fit models with tax base
summary(mod1<-glm(supopp01~taxbase,data=lp,family="binomial"))
summary(mod2<-glm(oppose01~taxbase,data=lp,family="binomial"))
summary(mod3<-lm(oppdksup~taxbase,data=lp))

#model with density
summary(mod4<-lm(oppdksup~taxbase+density,data=lp))
#random change here

require(ggplot2)
setwd("~/GitHub/loekkepoll")
pdf(file="taxplot.pdf")
ggplot(lp,aes(x=taxbase,y=oppdksup)) +
  geom_point(position=position_jitter(w = 0.1, h = 0.1),aes(color=factor(oppdksup)),alpha=.8) +
  geom_smooth(method="lm",color="black",alpha=.1) +
  theme_bw() +
  scale_color_manual(values=c("dark red","gray","dark green")) +
  theme(legend.position="none") +
  xlab("Beskatningsgrundlag pr. indb.") +
  ylab("Støtte til Løkke (-1='nej', 0='ved ikke', +1='ja')")
dev.off()

