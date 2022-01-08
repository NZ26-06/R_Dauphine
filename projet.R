

d = read.csv("meteo.train.csv", na.strings = "")
options(max.print = 10000)
d.names=c("X"                                           
          ,"Year"                                        
          ,"Month"                                       
          ,"Day"                                         
          ,"Hour"                                        
          ,"Minute"                                      
          ,"Temperature.daily.mean..2.m.above.gnd."      
          ,"Relative.Humidity.daily.mean..2.m.above.gnd."
          ,"Mean.Sea.Level.Pressure.daily.mean..MSL."    
          ,"Total.Precipitation.daily.sum..sfc."         
          ,"Snowfall.amount.raw.daily.sum..sfc."         
          ,"Total.Cloud.Cover.daily.mean..sfc."          
          ,"High.Cloud.Cover.daily.mean..high.cld.lay."  
          ,"Medium.Cloud.Cover.daily.mean..mid.cld.lay." 
          ,"Low.Cloud.Cover.daily.mean..low.cld.lay."    
          ,"Sunshine.Duration.daily.sum..sfc."           
          ,"Shortwave.Radiation.daily.sum..sfc."         
          ,"Wind.Speed.daily.mean..10.m.above.gnd."      
          ,"Wind.Direction.daily.mean..10.m.above.gnd."  
          ,"Wind.Speed.daily.mean..80.m.above.gnd."      
          ,"Wind.Direction.daily.mean..80.m.above.gnd."  
          ,"Wind.Speed.daily.mean..900.mb."              
          ,"Wind.Direction.daily.mean..900.mb."          
          ,"Wind.Gust.daily.mean..sfc."                  
          ,"Temperature.daily.max..2.m.above.gnd."       
          ,"Temperature.daily.min..2.m.above.gnd."       
          ,"Relative.Humidity.daily.max..2.m.above.gnd." 
          ,"Relative.Humidity.daily.min..2.m.above.gnd." 
          ,"Mean.Sea.Level.Pressure.daily.max..MSL."     
          ,"Mean.Sea.Level.Pressure.daily.min..MSL."     
          ,"Total.Cloud.Cover.daily.max..sfc."           
          ,"Total.Cloud.Cover.daily.min..sfc."           
          ,"High.Cloud.Cover.daily.max..high.cld.lay."   
          ,"High.Cloud.Cover.daily.min..high.cld.lay."   
          ,"Medium.Cloud.Cover.daily.max..mid.cld.lay."  
          ,"Medium.Cloud.Cover.daily.min..mid.cld.lay."  
          ,"Low.Cloud.Cover.daily.max..low.cld.lay."     
          ,"Low.Cloud.Cover.daily.min..low.cld.lay."     
          ,"Wind.Speed.daily.max..10.m.above.gnd."       
          ,"Wind.Speed.daily.min..10.m.above.gnd."       
          ,"Wind.Speed.daily.max..80.m.above.gnd."       
          ,"Wind.Speed.daily.min..80.m.above.gnd."       
          ,"Wind.Speed.daily.max..900.mb."               
          ,"Wind.Speed.daily.min..900.mb."               
          ,"Wind.Gust.daily.max..sfc."                   
          ,"Wind.Gust.daily.min..sfc."                   
          ,"pluie.demain")

d.short=c("X"
          ,"Year"                                        
          ,"Month"                                       
          ,"Day"                                         
          ,"Hour"                                        
          ,"Minute"                                      
          ,"Temp.mean.2.m"   
          ,"Rel.Hum.mean.2.m"
          ,"Mean.Sea.Level.Press.mean"    
          ,"Total.Pre.sum" 
          ,"Snowfall.amount.raw.sum" 
          ,"Total.Cloud.Cover.mean"  
          ,"High.Cloud.Cover.mean.high"  
          ,"Medium.Cloud.Cover.mean.mid" 
          ,"Low.Cloud.Cover.mean.low"    
          ,"Sunshine.Duration.sum"   
          ,"Shortwave.Radiation.sum" 
          ,"Wind.Spd.mean.10.m"
          ,"Wind.Dir.mean.10.m"
          ,"Wind.Spd.mean.80.m" 
          ,"Wind.Dir.mean.80.m"
          ,"Wind.Spd.mean.900.m"          
          ,"Wind.Dir.mean.900.m"    
          ,"Wind.Gust.mean"  
          ,"Temp.max.2.m"     
          ,"Temp.min.2.m"     
          ,"Rel.Hum.max.2.m" 
          ,"Rel.Hum.min.2.m" 
          ,"Mean.Sea.Level.Press.max"
          ,"Mean.Sea.Level.Press.min"
          ,"Total.Cloud.Cover.max"   
          ,"Total.Cloud.Cover.min"   
          ,"High.Cloud.Cover.max.high"   
          ,"High.Cloud.Cover.min.high"   
          ,"Medium.Cloud.Cover.max.mid"  
          ,"Medium.Cloud.Cover.min.mid"  
          ,"Low.Cloud.Cover.max.low"     
          ,"Low.Cloud.Cover.min.low"     
          ,"Wind.Spd.max.10.m"     
          ,"Wind.Spd.min.10.m"     
          ,"Wind.Spd.max.80.m"     
          ,"Wind.Spd.min.80.m"     
          ,"Wind.Spd.max.900.m"               
          ,"Wind.Spd.min.900.m"              
          ,"Wind.Gust.max"  
          ,"Wind.Gust.min"   
          ,"pluie.demain")

names(d)=d.short
d$pluie.demain <- as.integer(d$pluie.demain)
d = d[,-c(1:6)]
attach(d)
summary(d)

library(leaps)
choix_modele=regsubsets(d$pluie.demain~.,intercept=TRUE,nbest=1,nvmax=40,method="exhaustive",data=d)
resume= summary(choix_modele)
# Le resume est trop long (40 variables)
#print(resume)
plot(choix_modele,scale="r2")
plot(choix_modele,scale="adjr2")
plot(choix_modele,scale="Cp")
plot(choix_modele,scale="bic")


library(MASS)
step(glm(d$pluie.demain~.,data=d), d$pluie.demain~.,
     data=d, family = binomial, direction="backward")



m1 = glm(formula =
             pluie.demain ~
             Temp.mean.2.m +
             Mean.Sea.Level.Press.mean +
             Snowfall.amount.raw.sum +
             Medium.Cloud.Cover.mean.mid +
             Wind.Spd.mean.80.m +
             Wind.Dir.mean.80.m +
             Wind.Dir.mean.900.m +
             Wind.Gust.mean +
             Temp.min.2.m +
             Mean.Sea.Level.Press.max +
             Mean.Sea.Level.Press.min +
             Total.Cloud.Cover.min +
             High.Cloud.Cover.max.high +
             Medium.Cloud.Cover.max.mid +
             Low.Cloud.Cover.max.low +
             Wind.Spd.max.10.m +
             Wind.Spd.min.10.m, family = binomial, data = d)
summary(m1)

# m2, je commente la variable Total.Cloud.Cover.min par rapport à m1
m2 = glm(formula =
             pluie.demain ~
             Temp.mean.2.m +
             Mean.Sea.Level.Press.mean +
             Snowfall.amount.raw.sum +
             Wind.Spd.mean.80.m +
             Wind.Dir.mean.80.m+
             Wind.Dir.mean.900.m+
             Wind.Gust.mean+
             Temp.min.2.m +
             Mean.Sea.Level.Press.max +
             Mean.Sea.Level.Press.min +
             #Total.Cloud.Cover.min +
             High.Cloud.Cover.max.high +
             Medium.Cloud.Cover.max.mid +
             Low.Cloud.Cover.max.low +
             Medium.Cloud.Cover.mean.mid + 
             Wind.Spd.max.10.m, family = binomial,data = d)
summary(m2)

#m3 est m1 mais en regression probit
m3 = glm(formula =
             pluie.demain ~
             Temp.mean.2.m +
             Mean.Sea.Level.Press.mean +
             Snowfall.amount.raw.sum +
             Wind.Spd.mean.80.m +
             Wind.Dir.mean.80.m+
             Wind.Dir.mean.900.m+
             Wind.Gust.mean+
             Temp.min.2.m +
             Mean.Sea.Level.Press.max +
             Mean.Sea.Level.Press.min +
             Total.Cloud.Cover.min +
             High.Cloud.Cover.max.high +
             Medium.Cloud.Cover.max.mid +
             Low.Cloud.Cover.max.low +
             Medium.Cloud.Cover.mean.mid + 
             Wind.Spd.max.10.m, family = binomial(link = "probit"),data = d)
summary(m3)

#m4 , etabli à partir du graphique Cp Mallows
m4 = glm(formula =
             pluie.demain ~
             Mean.Sea.Level.Press.mean +
             Snowfall.amount.raw.sum +
             Wind.Spd.mean.80.m +
             Wind.Dir.mean.80.m +
             Wind.Dir.mean.900.m+
             Wind.Gust.max+
             Temp.max.2.m +
             Total.Cloud.Cover.min+
             Mean.Sea.Level.Press.max +
             Mean.Sea.Level.Press.min +
             Medium.Cloud.Cover.mean.mid +
             Medium.Cloud.Cover.max.mid +
             Low.Cloud.Cover.max.low +
             Wind.Spd.max.10.m +
             Wind.Spd.min.10.m 
         , family = binomial,data = d)

#m5 , etabli à partir du graphique BIC
m5 = glm(formula =
             pluie.demain ~
             Temp.max.2.m +
             Total.Cloud.Cover.mean+
             Mean.Sea.Level.Press.min +
             Medium.Cloud.Cover.max.mid +
             Wind.Gust.max 
         , family = binomial,data = d)



AIC(m1)
AIC(m2)
AIC(m3)
AIC(m4)
AIC(m5)

# Seul la comparaison m4 et m1 permet de rejeter m1 (jusqu'à 10%)
anova(m1, test = "LRT")
anova(m2,m1, test = "LRT")
anova(m3,m1, test = "LRT")
anova(m4,m1, test = "LRT")
anova(m5,m1, test = "LRT")


#Division du nb de ligne des donnees sources en K zones et rangé dans vecteur index
k = 10
index = sample(1:k, nrow(d), replace=T)
# vecteur resultat pour chaque zone: on y place le resultat de la regression
res.m1 = rep(NA, k)
res.m3 = rep(NA, k)
res.m4 = rep(NA, k)
res.m5 = rep(NA, k)
res.m6 = rep(NA, k)


for(i in 1:k){
    
#on reprend chaque modèle, mais on ajuste la zone de données en excluant la zone indiqué par la boucle for
    
    reg.m1 = glm(formula =
                     pluie.demain ~
                     Temp.mean.2.m +
                     Mean.Sea.Level.Press.mean +
                     Snowfall.amount.raw.sum +
                     Medium.Cloud.Cover.mean.mid +
                     Wind.Spd.mean.80.m +
                     Wind.Dir.mean.80.m +
                     Wind.Dir.mean.900.m +
                     Wind.Gust.mean +
                     Temp.min.2.m +
                     Mean.Sea.Level.Press.max +
                     Mean.Sea.Level.Press.min +
                     Total.Cloud.Cover.min +
                     High.Cloud.Cover.max.high +
                     Medium.Cloud.Cover.max.mid +
                     Low.Cloud.Cover.max.low +
                     Wind.Spd.max.10.m +
                     Wind.Spd.min.10.m, family = binomial, data =  d[index != i, ])
    
 

    reg.m3 =  glm(formula =
                      pluie.demain ~
                      Temp.mean.2.m +
                      Mean.Sea.Level.Press.mean +
                      Snowfall.amount.raw.sum +
                      Wind.Spd.mean.80.m +
                      Wind.Dir.mean.80.m+
                      Wind.Dir.mean.900.m+
                      Wind.Gust.mean+
                      Temp.min.2.m +
                      Mean.Sea.Level.Press.max +
                      Mean.Sea.Level.Press.min +
                      Total.Cloud.Cover.min +
                      High.Cloud.Cover.max.high +
                      Medium.Cloud.Cover.max.mid +
                      Low.Cloud.Cover.max.low +
                      Medium.Cloud.Cover.mean.mid + 
                      Wind.Spd.max.10.m, family = binomial(link = "probit"),data = d[index != i, ])
    
    
    reg.m4 = glm(formula =
                     pluie.demain ~
                     Mean.Sea.Level.Press.mean +
                     Snowfall.amount.raw.sum +
                     Wind.Spd.mean.80.m +
                     Wind.Dir.mean.80.m +
                     Wind.Dir.mean.900.m+
                     Wind.Gust.max+
                     Temp.max.2.m +
                     Total.Cloud.Cover.min+
                     Mean.Sea.Level.Press.max +
                     Mean.Sea.Level.Press.min +
                     Medium.Cloud.Cover.mean.mid +
                     Medium.Cloud.Cover.max.mid +
                     Low.Cloud.Cover.max.low +
                     Wind.Spd.max.10.m +
                     Wind.Spd.min.10.m 
                 , family = binomial,data = d[index != i, ])
    
    reg.m5 =  glm(formula =       pluie.demain ~
                      Temp.max.2.m +
                      Total.Cloud.Cover.mean+
                      Mean.Sea.Level.Press.min +
                      Medium.Cloud.Cover.max.mid +
                      Wind.Gust.max 
                  , family = binomial,data = d[index != i, ])
    
   
    
# Les coefficients des reg a été defini pour les zones exclues par la boucle, soit des zones d'entrainement
# on predit alors sur la zone de test (celle definie par la boucle for)
    pred.m1 = predict(reg.m1, newdata=d[index == i, ],
                      type="response")
    pred.m3 = predict(reg.m3, newdata=d[index == i, ],
                      type="response")
    pred.m4 = predict(reg.m4, newdata=d[index == i, ],
                      type="response")
    pred.m5 = predict(reg.m5, newdata=d[index == i, ],
                      type="response")
    
    
    
#On place dans les vecteur resulats pour chaque zone, le nb de fois où il devrait plutot pleuvoir selon la prediction
    res.m1[i] = mean(d[index==i, "pluie.demain"] == (pred.m1 >.5), na.rm = T)
    res.m3[i] = mean(d[index==i, "pluie.demain"] == (pred.m3 >.5), na.rm = T)
    res.m4[i] = mean(d[index==i, "pluie.demain"] == (pred.m4 >.5), na.rm = T)
    res.m5[i] = mean(d[index==i, "pluie.demain"] == (pred.m5 >.5), na.rm = T)
  
    
}

# on fait la moyenne des resultats issus de chaque zone pour chaque modèle.
mean(res.m1)
mean(res.m3)
mean(res.m4)
mean(res.m5)





#Si on s'entraine sur 90% du fichier
#On s'apercoit que k-fold a permis d'améliorer les resultats

#m1
train = sample(c(T, F), nrow(d), replace = T, prob = c(.9, .1))
pred1 = predict(m1, d[!train, ], type = "response")
mean(abs(pred1 - d[!train, "pluie.demain"]), na.rm = T)
#[1] 0.3711695

#m3
pred3 = predict(m3, d[!train, ], type = "response")
mean(abs(pred3 - d[!train, "pluie.demain"]), na.rm = T)
#[1] 0.3750807

#m4
pred4 = predict(m4, d[!train, ], type = "response")
mean(abs(pred4 - d[!train, "pluie.demain"]), na.rm = T)
#[1] 0.3753281

#m5
pred5 = predict(m5, d[!train, ], type = "response")
mean(abs(pred5 - d[!train, "pluie.demain"]), na.rm = T)
#[1] 0.3849942

#Etude des residus



summary(m1)
#m1
pchisq(1635.4 - 1250.2, 1179 - 1162, lower = F)
#[1] 2.289637e-71
pchisq(1250.2, 1162, lower = F)
#[1] 0.03601114


summary(m3)
#m3
pchisq(1635.4 - 1261.3, 1179 - 1163, lower = F)
#9.615292e-70
pchisq(1261.3, 1163, lower = F)
#0.02290241

summary(m4)
#m4
pchisq(1635.4 - 1254.5, 1179 - 1164, lower = F)
#[1] 7.080406e-72
pchisq(1254.5, 1164, lower = F)
#0.03267293

summary(m5)
#m5
pchisq(1635.4 - 1300.9, 1179 - 1174, lower = F)
#3.797957e-70
pchisq(1300.9, 1174, lower = F)
#0.005484614



#Tous les modèles sont utiles, le modèle m1 est plus complet que le m4.
#Cependant ces pvaleurs restent faible 


# génération de la préduction sur la base de test



dtest = read.csv("meteo.test.csv", na.strings = "")
options(max.print = 10000)


dtest.names=c("X"                                           
          ,"Year"                                        
          ,"Month"                                       
          ,"Day"                                         
          ,"Hour"                                        
          ,"Minute"                                      
          ,"Temperature.daily.mean..2.m.above.gnd."      
          ,"Relative.Humidity.daily.mean..2.m.above.gnd."
          ,"Mean.Sea.Level.Pressure.daily.mean..MSL."    
          ,"Total.Precipitation.daily.sum..sfc."         
          ,"Snowfall.amount.raw.daily.sum..sfc."         
          ,"Total.Cloud.Cover.daily.mean..sfc."          
          ,"High.Cloud.Cover.daily.mean..high.cld.lay."  
          ,"Medium.Cloud.Cover.daily.mean..mid.cld.lay." 
          ,"Low.Cloud.Cover.daily.mean..low.cld.lay."    
          ,"Sunshine.Duration.daily.sum..sfc."           
          ,"Shortwave.Radiation.daily.sum..sfc."         
          ,"Wind.Speed.daily.mean..10.m.above.gnd."      
          ,"Wind.Direction.daily.mean..10.m.above.gnd."  
          ,"Wind.Speed.daily.mean..80.m.above.gnd."      
          ,"Wind.Direction.daily.mean..80.m.above.gnd."  
          ,"Wind.Speed.daily.mean..900.mb."              
          ,"Wind.Direction.daily.mean..900.mb."          
          ,"Wind.Gust.daily.mean..sfc."                  
          ,"Temperature.daily.max..2.m.above.gnd."       
          ,"Temperature.daily.min..2.m.above.gnd."       
          ,"Relative.Humidity.daily.max..2.m.above.gnd." 
          ,"Relative.Humidity.daily.min..2.m.above.gnd." 
          ,"Mean.Sea.Level.Pressure.daily.max..MSL."     
          ,"Mean.Sea.Level.Pressure.daily.min..MSL."     
          ,"Total.Cloud.Cover.daily.max..sfc."           
          ,"Total.Cloud.Cover.daily.min..sfc."           
          ,"High.Cloud.Cover.daily.max..high.cld.lay."   
          ,"High.Cloud.Cover.daily.min..high.cld.lay."   
          ,"Medium.Cloud.Cover.daily.max..mid.cld.lay."  
          ,"Medium.Cloud.Cover.daily.min..mid.cld.lay."  
          ,"Low.Cloud.Cover.daily.max..low.cld.lay."     
          ,"Low.Cloud.Cover.daily.min..low.cld.lay."     
          ,"Wind.Speed.daily.max..10.m.above.gnd."       
          ,"Wind.Speed.daily.min..10.m.above.gnd."       
          ,"Wind.Speed.daily.max..80.m.above.gnd."       
          ,"Wind.Speed.daily.min..80.m.above.gnd."       
          ,"Wind.Speed.daily.max..900.mb."               
          ,"Wind.Speed.daily.min..900.mb."               
          ,"Wind.Gust.daily.max..sfc."                   
          ,"Wind.Gust.daily.min..sfc."                   
)

dtest.short=c("X"
          ,"Year"                                        
          ,"Month"                                       
          ,"Day"                                         
          ,"Hour"                                        
          ,"Minute"                                      
          ,"Temp.mean.2.m"   
          ,"Rel.Hum.mean.2.m"
          ,"Mean.Sea.Level.Press.mean"    
          ,"Total.Pre.sum" 
          ,"Snowfall.amount.raw.sum" 
          ,"Total.Cloud.Cover.mean"  
          ,"High.Cloud.Cover.mean.high"  
          ,"Medium.Cloud.Cover.mean.mid" 
          ,"Low.Cloud.Cover.mean.low"    
          ,"Sunshine.Duration.sum"   
          ,"Shortwave.Radiation.sum" 
          ,"Wind.Spd.mean.10.m"
          ,"Wind.Dir.mean.10.m"
          ,"Wind.Spd.mean.80.m" 
          ,"Wind.Dir.mean.80.m"
          ,"Wind.Spd.mean.900.m"          
          ,"Wind.Dir.mean.900.m"    
          ,"Wind.Gust.mean"  
          ,"Temp.max.2.m"     
          ,"Temp.min.2.m"     
          ,"Rel.Hum.max.2.m" 
          ,"Rel.Hum.min.2.m" 
          ,"Mean.Sea.Level.Press.max"
          ,"Mean.Sea.Level.Press.min"
          ,"Total.Cloud.Cover.max"   
          ,"Total.Cloud.Cover.min"   
          ,"High.Cloud.Cover.max.high"   
          ,"High.Cloud.Cover.min.high"   
          ,"Medium.Cloud.Cover.max.mid"  
          ,"Medium.Cloud.Cover.min.mid"  
          ,"Low.Cloud.Cover.max.low"     
          ,"Low.Cloud.Cover.min.low"     
          ,"Wind.Spd.max.10.m"     
          ,"Wind.Spd.min.10.m"     
          ,"Wind.Spd.max.80.m"     
          ,"Wind.Spd.min.80.m"     
          ,"Wind.Spd.max.900.m"               
          ,"Wind.Spd.min.900.m"              
          ,"Wind.Gust.max"  
          ,"Wind.Gust.min"   
)

names(dtest)=dtest.short
dtest = dtest[,-c(1:6)]






testresult = predict(m4, dtest,type="response")
pluie.demain = as.integer(( (testresult >.5)))
dresultat = cbind(dtest,pluie.demain)
library(readr)
write_csv( dresultat, "meteo.prediction.csv")