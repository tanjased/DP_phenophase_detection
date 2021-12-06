#T.test
#misto: vahy muzete pojmenovat data jakkoliv jinak, treba: data, ukol1, vzorek ...
fenofaze=read.table(file.choose(), header<-T) 
attach(fenofaze)
#Compute student's t-test
t.test(Reference,NDVI,paired = TRUE)
t.test(Reference,NDRE,paired = TRUE)
t.test(Reference,NDMI,paired = TRUE)
t.test(Reference,MCARI,paired = TRUE)
t.test(Reference,RENDVI,paired = TRUE)

#data distribution
roz1 = Reference - NDVI
roz2 = Reference - NDMI
roz3 = Reference - NDRE
roz4 = Reference - MCARI
roz5 = Reference - RENDVI

#Test normal distribution
shapiro.test(roz1)
shapiro.test(roz2)
shapiro.test(roz3)
shapiro.test(roz4)
shapiro.test(roz5)

#test variance in data
var.test(Reference3,NDVI3)
var.test(Reference2,NDMI2)
var.test(Reference2,MCARI2)
var.test(Reference2,RENDVI2)
var.test(Reference2,NDRE2)

#potential wilcoxon test
wilcox.test(Reference2,NDVI2,paired = TRUE)
wilcox.test(Reference2,NDRE2,paired = TRUE)
wilcox.test(Reference2,NDMI2,paired = TRUE)
wilcox.test(Reference2,MCARI2,paired = TRUE)
wilcox.test(Reference2,RENDVI2,paired = TRUE)

