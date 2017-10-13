library(UsingR)
data("ToothGrowth")

head(ToothGrowth)
str(ToothGrowth)
unique(ToothGrowth$dose)

summary(ToothGrowth)

library(ggplot2)
graph = ggplot(ToothGrowth,aes(x=dose,y=len,dose=factor(supp)))+
        geom_point(aes(color=supp))+
        labs(y="Tooth Length",x="Dosage",title="Tooth length by dosage and supplement")
graph

t.test(ToothGrowth$len[ToothGrowth$supp=="OJ"],ToothGrowth$len[ToothGrowth$supp=="VC"],
       paired=F,var.equal=F)

t.test(ToothGrowth$len[ToothGrowth$dose==0.5],ToothGrowth$len[ToothGrowth$dose==1],
       paired=F,var.equal=F)

t.test(ToothGrowth$len[ToothGrowth$dose==1],ToothGrowth$len[ToothGrowth$dose==2],
       paired=F,var.equal=F)