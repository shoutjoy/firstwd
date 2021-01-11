

GA=c(1, 2, 2, 3, 3, 3, 3, 4, 4, 5 )
G1=c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
GB=c(3, 3, 4, 4, 4, 5, 5, 5, 5, 6 )
G2=c(rep(2,10))
G2
GC=c(2, 3, 3, 3, 4, 4, 4, 4, 5, 5 )
G3=c(rep(3,10))

gg1=cbind(GA,G1)
gg2=cbind(GB,G2)
gg3=cbind(GC,G3)

gg=rbind(gg1,gg2,gg3)

gg
gg <-as.data.frame(gg)
colnames(gg)=c("sat","grp")
gg$grp<-factor(gg$grp, levels=c(1,2,3),labels=c("control","High Treatment", "Low Treatment"))
str(gg)


# one way anova, t-test, & contrast
library(dplyr)
aov(sat ~ grp, gg) %>% summary()
gg.aov<- aov(sat ~ grp, gg)
t.test(GA,GB,var.equal = T)
t.test(GB,GC,var.equal = T)

library(emmeans)
gg.aov<- aov(sat ~ grp, gg)
contr.t =list("control - High treat = 0 "=c(1,-1,0),
              "Hign tr. - Low tr. = 0 "=c(0,1,-1))
emm =emmeans(gg.aov,"grp")

contrast( emm,contr.t, adjust="none")
contrast( emm,contr.t, adjust="none") %>% confint()
contrast( emm,contr.t, adjust="none") %>% plot()



contrast( emm,contr.t, adjust="bonferroni")
contrast( emm,contr.t, adjust="bonferroni") %>% confint()



#two way anova - Interaction analysis : contrast
#top school Nontop school
#Data Lecture 7 (Chapter 12-1) Publication
library(haven)
tn <- read_sav("Data Lecture 7 (Chapter 12-1) Publication Data.sav")
View(tn)

str(tn)
#변수정리
tn$gender<-factor(tn$gender,levels = c(1,2),labels = c("male","female"))
tn$TopSchool<-factor(tn$TopSchool, levels = c(1,2), labels =c("TopSchool", "NontopSchool"))
tn$grp <-as.factor(tn$grp)

#two way anova
aov(NumberOfPublication~TopSchool*gender,tn) %>% summary()


#interaction contrast
tn.aov <-aov(NumberOfPublication~grp,tn)
tn.aov %>% summary()

emm1 =emmeans(tn.aov, "grp")
emm1 #em means data
contr.top =list("H1:two way interaction effect"= c(1,-1,-1,1),
                "H2:main effect of Gender"     = c(1, 1,-1,-1),
                "H3:main effect of Top School" = c(1,-1,1,-1),
                "H4:simple main effect of Top school at male" = c(1,-1,0,0),
                "H5:simple main effect of Top school at female"=c(0,0,1,-1))

contrast(emm1, contr.top, adjust="none")
contrast(emm1, contr.top, adjust="none") %>% confint()
contrast(emm1, contr.top, adjust="none") %>% plot()

#Bonferroni adjust p
contrast(emm1, contr.top, adjust="bonferroni")
contrast(emm1, contr.top, adjust="bonferroni") %>% confint()
contrast(emm1, contr.top, adjust="bonferroni") %>% plot()

par(mar=c(3,10,3,10))
interaction.plot(tn$TopSchool,tn$gender,tn$NumberOfPublication, type="b",pch = c(21,19),col = c("blue","red"))
par(mar=c(2,2,2,2)) #원래대로





###

library(readr)
attractiviecsv <- read_csv("attractiviecsv.csv")
View(attractiviecsv)
att <-attractiviecsv
str(att)
att$grp<-as.factor(att$grp)
att$gender<- factor(att$gender,levels=c(1,2),labels=c("female","male"))
att$alcohol<- factor(att$alcohol, levels=c(1,2,3), labels = c("none","2Pints","4Pints"))

#two way anova
att.aov <- aov(attractiveness ~ alcohol*gender, att)
att.aov %>% summary()
model.tables(att.aov)

barplot(att)
boxplot(attractiveness~alcohol, att, col=c("yellow","orange","skyblue"))

#interaction plot
interaction.plot(att$alcohol,att$gender,att$attractiveness, type="b",pch = c(21,19),col = c("red","blue"))

library(HH)
interaction2wt(attractiveness~ alcohol*gender,att)
