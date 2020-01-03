par(mfrow=c(2,2))
for (i in a[,2]){
  li=which(lista_pow$map_id==i)
  stats::spectrum(lista_pow$t[li])
}

liniowy_size=lm(count~hum+forest+pig+forest*hum+pig*forest+pig*hum, data =zakaz)
anova(liniowy_size)
summary(liniowy_size)


liniowy_arrival=lm(var_1~hum+forest+pig+forest*hum+pig*forest+pig*hum, data =zakaz)
anova(liniowy_arrival)
summary(liniowy_arrival)

#without interactons
liniowy_arrival=lm(var_1~hum+forest+pig, data =zakaz)
anova(liniowy_arrival)
summary(liniowy_arrival)


liniowy_total=lm(total~hum+forest+pig+forest*hum+pig*forest+pig*hum, data =zakaz)
anova(liniowy_total)
summary(liniowy_total)

#border

liniowy_size=lm(count~hum+forest+pig+gra1+forest*hum+pig*forest+pig*hum, data =zakaz)
anova(liniowy_size)
summary(liniowy_size)


liniowy_arrival=lm(var_1~hum+forest+pig+gra1+forest*hum+pig*forest+pig*hum, data =zakaz)
anova(liniowy_arrival)
summary(liniowy_arrival)

#without interactons
liniowy_arrival=lm(var_1~hum+forest+pig+gra1, data =zakaz)
anova(liniowy_arrival)
summary(liniowy_arrival)


liniowy_total=lm(total~hum+forest+pig+gra1+forest*hum+pig*forest+pig*hum, data =zakaz)
anova(liniowy_total)
zakaz2=data.frame(id=numeric(58))
#zakaz2=zakaz
zakaz2$gra=0
zakaz2$gra[3]=1
zakaz2$gra[4]=1
zakaz2$gra[6]=1
zakaz2$gra[5]=1
zakaz2$gra[26]=1
zakaz2$gra[18]=1
zakaz2$gra[21]=1
zakaz2$gra[23]=1
zakaz2$gra[28]=1
zakaz2$gra[37]=1
zakaz2$gra[49]=1
zakaz2$gra[39]=1
zakaz$gra1=as.factor(zakaz$gra)
granica=data.frame(id=numeric(50),  gra=numeric(50))
granica2=data.frame(id=numeric(58),  gra=numeric(58), sta=numeric(58))
zakaz2$id1=zakaz$map_id
zakaz2$map_id=zakaz$map_id-1
granica$id=zakaz$id
granica$gra=zakaz$gra
granica$id=zakaz2$id1
granica$gra=zakaz2$gra
granica$sta=1
inf_pop=read.csv("infekcje_grepl_zmiana_new.csv", header = TRUE, sep = ",")
inf2$poviat=inf_pop$poviat
zak=unique(inf2$poviat)
granica2=data.frame(id=numeric(59))
granica2$id=zak
granica2$map_id=granica2$id
granica2_p=inner_join(granica2,counties_data, c("map_id","map_id"))
g=which(granica2$id==371|| granica2$id==27 || granica2$id==28 )

border=left_join(granica2,granica, c("id","id"))
granica2_p$id=granica2_p$id.x

inf2$id=inf2$poviat
inf2$map_id=inf2$poviat-1
inf2$t=as.numeric(inf2$t2)
border=left_join(granica2_p,granica, c("id","id"))

border2=border
border2$new=1
g=which(border2$id==371)
border2=border2[-g,]
g=which(border2$id==27)
border2=border2[-g,]

g=which(border2$id==28 )
border2=border2[-g,]

g=which(border2$id==358 )
border2=border2[-g,]
g=which(border2$id==204 )
border2=border2[-g,]
map2 <- inner_join(map, border2, by = c("id" = "map_id_0"))

p<-ggplot(data = map2, mapping = aes(x = long, y = lat)) +
  geom_polygon(mapping = aes(group = group, fill = new))

p
p<-ggplot(data = map2, mapping = aes(x = long, y = lat)) +
  geom_polygon(mapping = aes(group = group, fill = sta))
p

p<-ggplot(data = map2, mapping = aes(x = long, y = lat)) +
  geom_polygon(mapping = aes(group = group, fill = new))+
 geom_polygon(mapping = aes(group = group, fill = gra))
p
p<-ggplot(data = map2, mapping = aes(x = long, y = lat)) +
  geom_polygon(mapping = aes(group = group, fill = stat))+
  scale_fill_distiller("Border/Inerior Counties", palette = "RdPu", breaks = pretty_breaks(n = 2))

p
p<-ggplot(data = map_all, mapping = aes(x = long, y = lat)) +
  geom_polygon(mapping = aes(group = group, fill = stat))+
  scale_fill_distiller("Investigated Counties", palette = "RdPu", breaks = pretty_breaks(n = 4), trans= "reverse", labels = c("free","affected","affected-border","A1", "swine-district"))

p

p + theme_grey(base_size = 22)


wszystkie=data.frame(id=numeric(380))
wszystkie$id_m=c(0:379)
 wszystkie$id=c(1:380)
 wszystkie$gra=0
 wszystkie$A1=0
 wszystkie$inf=0
 wszystkie$inf2=0
 wszystkie$gra[gra]=1
 wszystkie$A1[A1]=1
 wszystkie$inf[granica$id]=1
 wszystkie$inf2[border2$id]=1
 wszystkie$inf3=0
 wszystkie$inf3=floor((wszystkie$inf+wszystkie$inf2+2.9)/3)
 map_all <- inner_join(map, wszystkie, by = c("id" = "id_m"))
 p<-ggplot(data = map_all, mapping = aes(x = long, y = lat)) +
   geom_polygon(mapping = aes(group = group, fill = A1))
 wszystkie$name=counties_data$id
library(scales)
library(ggplot2)
map_all$stat=0
sam=which(map_all$gra==1)
sam1=which(map_all$A1==1)
sam2=which(map_all$id==74)
licz2=rep(0, 115629)
for (j in sam2)
{licz2[j]=4}
licz=rep(0, 115629)
for (j in sam)
{licz[j]=1}
licz1=rep(0, 115629)
for (j in sam1)
{licz1[j]=3}
map_all$stat=map_all$stat+as.numeric(map_all$inf3)+licz+licz1+licz2
notyfikacja2=inner_join(inf2,border, c("id","id"))
#notyfikacja_kopia=notyfikacja
notyfikacja=notyfikacja2
dev.off()
par(mfrow=c(1,2))
l_b=which(notyfikacja$gra==1)
#hist(as.Date(l_b$t2), breaks="months")

not_b=notyfikacja[l_b,]
st_b=stats::spectrum(notyfikacja$t[l_b])
title(main = "border")
l_i=which(notyfikacja$gra!=1)
not_i=notyfikacja[l_i,]
st_i=stats::spectrum(notyfikacja$t[l_i])
title(main = "interior")
dev.off()
not_b$Month <- format(as.Date(not_b$t2), "%m")
hist(as.Date(not_b$t2), breaks="months")
dev.off()
ala=hist(as.Date(notyfikacja_kopia$t2), breaks="months", freq = TRUE, xlab = "Months", ylab = "Monthly incidence", main = paste("Monthly Incidence in Poland"))

al2=ggplot(notyfikacja_kopia, aes(fill=gra1, y=value, x=specie)) + 
  histogram( stat="identity", position="fill")
e<-ggplot(notyfikacja_kopia)+
e+histogram( xName='t2', groupName='gra1', breaks="months")

e <- ggplot(notyfikacja_kopia, aes(x = gra1, y = t2))
 e + hist()
 e <- ggplot(ala, aes(x = as.Date(breaks), y = counts))
 e+ geom_bar()
 
 plot(ala$mids, ala$counts, type = 'l')
 
 
ggplot2.histogram(data=notyfikacja_kopia, xName='t2', groupName='gra1', breaks="months")

hist(as.numeric(not_b$Month))
dev.off()
par(mfrow=c(1,2))
not_i$Month <- format(as.Date(not_i$t2), "%m")


hist(as.numeric(not_i$Month))
out_i=hist(as.Date(not_i$t2), breaks="months")
out_b=hist(as.Date(not_b$t2), breaks="months")
#regresions per months
inter= data.frame(count=numeric(56))
inter$month=c(5:60)%%12
inter$month_id=as.factor(inter$month)
inter$count=out_i$counts
border_c= data.frame(count=numeric(59))
border_c$count=out_b$counts
border_c$month=c(2:60)%%12
border_c$gra='b'
inter$gra='i'
border_c$month_id=as.factor(border_c$month)
total <- rbind(inter, border_c)
total$gra=as.factor(total$gra)
library(nlme)
mix_ <- lme(count ~ 1+month_id + gra, random = ~1|month_id, data = total, method = "ML")

#infect_miesiacami_= ddply(time_my2,.(Month, speciesDescription), summarise,  counts =length(Month))

lin_i=lm(count~month_id,inter)
lin_b=lm(count~month_id,border_c)

summary(lin_i)
summary(lin_b)

dev.off()

par(mfrow=c(1,2))

st_i=stats::spectrum(out_i$counts)
title(main = "interior")
st_b=stats::spectrum(out_b$counts)
title(main = "border")
dev.off()

par(mfrow=c(1,2))
histy=data.frame(id=numeric(12))
histy$id=c(1:12)
histy$i=0
histy$b=0
histy$i_plus=0
histy$b_minus=0

for (i in 1:12){
  histy$i[i]=length(which(as.numeric(not_i$Month)==i))
  histy$b[i]=length(which(as.numeric(not_b$Month)==i))
}
par(mfrow=c(1,2))

ggplot(histy, aes(x=id,y=i))+
geom_point(shape=1)
ggplot(histy, aes(x=id,y=b))+
  geom_point(shape=1)
dev.off()

par(mfrow=c(1,2))
plot(histy$id, histy$i, xlab = "Boarder (Month of year)", ylab = "Notifications counts")
plot(histy$id, histy$b, xlab = "Interior (Month of year)", ylab = "Notifications counts")
histy$b_plus=0
histy$i_minus=0
histy$id_factor=as.factor(histy$id)
infect_miesiacami_= ddply(time_my2,.(Month, speciesDescription), summarise,  counts =length(Month))

lin_i=lm(i~id_factor,histy)
lin_b=lm(b~id_factor,histy)
summary(lin_i)
summary(lin_b)


for (i in 1:11)
{
  histy$i_plus[i]=histy$i[i+1]
  histy$b_plus[i]=histy$b[i+1]
  

}
for (i in 2:12){
histy$b_minus[i]=histy$b[i-1]
histy$i_minus[i]=histy$i[i-1]

}
histy$b_minus[1]=histy$b[12]
histy$i_plus[12]=histy$i[1]
histy$i_minus[1]=histy$i[12]
histy$b_plus[12]=histy$b[1]
cor(histy[c(2,3,4,5,6,7)])
hist(as.numeric(not_i$Month))

hist(as.numeric(not_b$Month))

library(tidyverse)
library(broom)
st_i_test=data.frame(id=numeric(1250))
st_i_test$spec=st_i$spec

#observations scaling
st_i_test$freq=st_i$freq*1250/512

st_b_test=data.frame(id=numeric(512))
st_b_test$spec=st_b$spec
st_b_test$freq=st_b$freq

y0 = 45563704
yf = 8
alpha = 2761
fit <- nls(spec ~ SSasymp(spec,specf, spec0, log_alpha), data = st_b_test)
fit2<-nls(spec ~ yf + (y0 - yf) * exp(-alpha * freq), 
    data = st_b_test,
    start = list(y0 = 6079449, yf = 8, alpha = 500))
fit2_i<-nls(spec ~ yf + (y0 - yf) * exp(-alpha * freq), 
          data = st_i_test,
          start = list(y0 = 6079449, yf = 8, alpha = 500))

st_b_test$y=yf + (y0 - yf) * exp(-alpha * st_b_test$freq)
par(mfrow=c(1,2))
plot(st_b_test$freq,st_b_test$spec, log = "y")
plot(st_i_test$freq,st_i_test$spec, log = "y")
par(mfrow=c(1,2))
plot(st_b_test$freq*1000,st_b_test$spec, log = "y", xlim=c(0,100), type = "l", xlab="Period", ylab="Intensity")
plot(st_i_test$freq*1000,st_i_test$spec, log = "y", xlim=c(0,100), type = "l", xlab="Period", ylab="Intensity")
dev.off()
par(mfrow=c(1,2))

plot(st_b_test$freq,st_b_test$spec, log = "y", xlim=c(0.05,0.085))
plot(st_i_test$freq,st_i_test$spec, log = "y", xlim=c(0.05,0.085))

#period of a period

par(mfrow=c(1,2))
spec_of_spec_i=stats::spectrum(st_i_test$spec)
spec_of_spec_b=stats::spectrum(st_b_test$spec)

par(mfrow=c(1,2))
autocor_i=acf(notyfikacja$t[l_i]*1250/512)
autocor_b=acf(notyfikacja$t[l_b])
test=data.frame(id=numeric(31))
test$b=autocor_b$acf
temp=autocor_i$acf
temp2=temp[-c(32:34)]
test$id=c(0:30)
test$i=temp2
liniowy_auto_i=lm(i~id, data = test)
summary(liniowy_auto_i)
liniowy_auto_b=lm(b~id, data = test)
summary(liniowy_auto_b)
notyfikacja$gra1=as.factor(notyfikacja$gra)
notyfikacja_kopia$gra1=as.factor(notyfikacja_kopia$gra)

e <- ggplot(notyfikacja_kopia, aes(x = gra1, y = as.Date(t2)))
e + geom_violin() 