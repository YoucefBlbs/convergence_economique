### Donnees de 16 pays GDP par tete

X1 <- read.csv("pib.csv",head = T)
X <- X1[,-1]

source("multimodality_src.R")


## Application ? la distribution des revenus par habitants pour 16 pays du monde #####
##################


###  En 1950 ###

gdp50<-M[,"X1950"] #on enlève les noms des pays
gdp90<-M[,"X1990"] 
gdp18<-M[,"X2018"] 



silversignif(gdp50,1,nboot=100)
silversignif(gdp50,2,nboot=100)
silversignif(gdp50,3,nboot=100)

silversignif(gdp90,1,nboot=100)
silversignif(gdp90,2,nboot=100)
silversignif(gdp90,3,nboot=100)

silversignif(gdp18,1,nboot=100)
silversignif(gdp18,2,nboot=100)
silversignif(gdp18,3,nboot=100)





plot(density(gdp50,from=0,bw=hcrit(gdp50,2)),main="Densité du PIB per Capita\n selon les anneés",xlab="PIB par tête rapporté a la moyenne ",ylab="Densité")
lines(density(gdp90,from = 0,bw=hcrit(gdp90,1)),col=6,lty=4)
lines(density(gdp18,from = 0,bw=hcrit(gdp18,1)),col=4,lty=3)
legend("topright",legend = c("1950","1990","2018"), lwd=2,col =c("1","2","3"),lty = 1:3)

######## valeur mode 1950#######
h3_50<-hcrit(gdp50,2)
h3_50
m3_50<-valpic_creux(gdp50,h3_50)
m3_50
m3_50$valcreux
m3_50$valmodes

####### valeur Mode 1990#####
h3_90<-hcrit(gdp90,1)
h3_90
m3_90<-valpic_creux(gdp90,h3_90)

m3_90$valcreux
m3_90$valmodes

####### valeur Mode 2018#######
h3_18<-hcrit(gdp18,1)
h3_18
m3_18<-valpic_creux(gdp18,h3_18)

m3_18$valcreux
m3_18$valmodes



plot(density(gdp50,from=0,bw=hcrit(gdp50,2)),main="Densite empirique du PIB per Capita\ en 1950",xlab="PIB per Capita",ylab="Densite")
abline(v=m3_50$valcreux,lwd=1,lty=3)
abline(v=m3_50$valmodes,lwd=2,lty=3)





########################################################################################
#####  Autres Outils : Beta et Sigm convergence et Matrice de mobilité ################# 

#### Calcul des rapport a la moyenne

M<-matrix(0,nrow=nrow(X),ncol=ncol(X))
for(i in 1:ncol(X)){
  M[,i]<-X[,i]/colMeans(X)[i]
}
     colnames(M)<-colnames(X)
head(M)
M<-data.frame(M)

attach(M)



### Beta convergence et sigma convergence

#Beta coonvergence avec annees de reference 1950

tx50<-numeric(0)
for(i in 1:nrow(M)){
  mult<-M[i,ncol(M)]/M[i,1]
  periode<-ncol(M)-1
  tx<-(mult^(1/periode)-1)*100
  tx50<-append(tx50,tx)
}

tx50


plot(M[,1],tx50,main="Beta convergence des pays de la RCEP\n 1950-2018" ,xlab="PIB par tête rapporté a la moyenne ",ylab="taux de croissance annuel moyen 1950-2018",type="p",pch=1)
abline(lm(tx50~M[,1]),col="red")

summary(lm(tx50~M[,1]))


#si on enleve le australie et nouvelle zelande

which(M[,1]>3.8)
X1[c(1,16),1]

M1<-M[-c(1,16),]


tx50_1<-tx50[-c(1,16)]

plot(M1[,1],tx50_1,main="Beta convergence des pays de la RCEP\n sans l'australie et la nouvelle zélande \n1950-2018",xlab="PIB par tête rapporté a la moyenne",ylab="taux de croissance annuel moyen 1950-2008",type="p",pch=1)
abline(lm(tx50_1~M1[,1]),col="red")

summary(lm(tx50_1~M1[,1]))
confint(lm(tx50_1~M1[,1]))

## Acces au minimum

which(tx50==min(tx50))

X1[16,1]

#Beta coonvergence avec annees de reference 1970

tx70<-numeric(0)
for(i in 1:nrow(M)){
  mult<-M[i,ncol(M)]/M[i,21]
  periode<-ncol(M)-21
  tx<-(mult^(1/periode)-1)*100
  tx70<-append(tx70,tx)
}


plot(M[,21],tx70,main="Beta convergence des pays de la RCEP\n 1970-2018",xlab="PIB par tête rapporté a la moyenne",ylab="taux de croissance annuel moyen 1970-2018",type="p",pch=1)
abline(lm(tx70~M[,21]),col="red")


which(M[,21]>2.5)
X1[c(1,5,16),1]

## Sans japon, australie , et nouvelle zelande 


plot(M[-c(1,5,16),21],tx70[-c(1,5,16)],main="Beta convergence des pays de la RCEP\n sans l'australie , la nouvelle zélande et le japon \n1970-2018",xlab="PIB par tête rapporté a la moyenne" ,ylab="taux de croissance annuel moyen 1970-2018",type="p",pch=1)
abline(lm(tx70[-c(1,5,16)]~M[-c(1,5,16),21]),col="red")

summary(lm(tx70[-c(1,5,16)]~M[-c(1,5,16),21]))
confint(lm(tx70[-c(1,5,16)]~M[-c(1,5,16),21]))

which(tx70[-c(1,5,16)]==min(tx70[-c(1,5,16)]))
X1[9,1]
#Beta convergence avec annees de reference 1990

tx90<-numeric(0)
for(i in 1:nrow(M)){
  mult<-M[i,ncol(M)]/M[i,41]#on divise par la 21éme colonne qui est celle de 1990
  periode<-ncol(M)-41
  tx<-(mult^(1/periode)-1)*100
  tx90<-append(tx90,tx)
}

summary(lm(tx90~M[,1]))
confint(lm(tx90~M[,1]))

plot(M[,41],tx90,main="Beta convergence des pays de la RCEP\n 1990-2018",xlab="PIB par tête rapporté a la moyenne",ylab="taux de croissance annuel moyen 1990-2018",type="p",pch=1)
abline(lm(tx90~M[,41]),col="red")




which(M[,41]>2)
X1[c(5,3,1),1]

## Sans japon, australie , et nouvelle zelande 


plot(M[-c(1,5,3),41],tx90[-c(1,5,3)],main = "Beta convergence des pays de la RCEP\n sans le japon, l'australie et la nouvelle zélande \n1990-2018",xlab="PIB par tête rapporté a la moyenne",ylab="taux de croissance annuel moyen 1990-2018",type="p",pch=1)
abline(lm(tx90[-c(1,5,3)]~M[-c(1,5,3),41]),col="red")

summary(lm(tx70[-c(1,5,16)]~M[-c(1,5,16),21]))
confint(lm(tx70[-c(1,5,16)]~M[-c(1,5,16),21]))

which(tx70[-c(1,5,16)]==min(tx70[-c(1,5,16)]))
X1[9,1]
#Beta coonvergence avec annees de reference 2010

tx10<-numeric(0)
for(i in 1:nrow(M)){
  mult<-M[i,ncol(M)]/M[i,61]#on divise par la 21éme colonne qui est celle de 1990
  periode<-ncol(M)-61
  tx<-(mult^(1/periode)-1)*100
  tx10<-append(tx10,tx)
}


plot(M[,61],tx10,main="Beta convergence des pays de la RCEP\n 2010-2018",xlab="PIB par tête rapporté a la moyenne",ylab="taux de croissance annuel moyen 2010-2018",type="p",pch=1)
abline(lm(tx10~M[,61]),col="red")


which(M[,61]>2.5)
X1[c(12),1]

## Sans singapour 


plot(M[-c(12),61],tx10[-c(12)],main = "Beta convergence des pays de la RCEP\n sans le Singapour \n2010-2018",xlab= "PIB par tête rapporté a la moyenne",ylab="taux de croissance annuel moyen 1990-2018",type="p",pch=1)
abline(lm(tx10[-c(12)]~M[-c(12),61]),col="red")

summary(lm(tx10[-c(12)]~M[-c(12),61]))
confint(lm(tx10[-c(12)]~M[-c(12),61]))

#Sigma convergence avec le coefficient de variation 1950

# Avec australie et nouvelle zelande

coefvar<-numeric(0)

for(i in 1:ncol(M)){
  v50<-sd(M[,i])
  coefvar<-append(coefvar,v50)
  
}
coefvar<-ts(coefvar,start=1950, end=2018)
plot(coefvar,xlab="Années",ylab="coefficient de variation",
     main = "Sigma convergence des pays de la RECP 1950-2018")
#### la y'a une sigma convergence

# Sans australie et nouvelle zelande

coefvar1<-numeric(0)
for(i in 1:ncol(M)){
  v501<-sd(M[-c(1,16),i])
  coefvar1<-append(coefvar1,v501)
}

coefvar1<-ts(coefvar1,start=1950, end=2018)
plot(coefvar1,xlab="Années",ylab="coefficient de variation",
     main = "Sigma convergence des pays de la RECP \n sans l'australie et la nouvelle zélande \n 1950-2018")

###### Sigma convergence avec le coefficient de variation 1970

# avec japon, australie , et nouvelle zelande 

coefvar70<-numeric(0)

for(i in 1:ncol(M)){
  v70<-sd(M[,i])
  coefvar70<-append(coefvar70,v70)
  
}
coefvar70<-ts(coefvar70,start=1970, end=2018)
plot(coefvar70)

# Sans australie et nouvelle zelande

coefvar701<-numeric(0)
for(i in 1:ncol(M)){
  v701<-sd(M[-c(1,5,16),i])
  coefvar701<-append(coefvar701,v701)
}

coefvar701<-ts(coefvar701,start=1970, end=2018)
plot(coefvar701)

###### Sigma convergence avec le coefficient de variation 1990


coefvar90<-numeric(0)

for(i in 1:ncol(M)){
  v90<-sd(M[,i])
  coefvar90<-append(coefvar90,v90)
  
}
coefvar90<-ts(coefvar90,start=1990, end=2018)
plot(coefvar90)
###### Sigma convergence avec le coefficient de variation 2010

coefvar10<-numeric(0)

for(i in 1:ncol(M)){
  v10<-sd(M[,i])
  coefvar10<-append(coefvar10,v10)
  
}
coefvar10<-ts(coefvar10,start=2010, end=2018)
plot(coefvar10)




###### Matrice de mobilit? ########
##################################

source("matmob.r")

ncol(M)

##### Matrice de mobilit? ? 5 ans

## Avec australie et nouvelle zelande


a<-seqAp(1,54,1)
M5_1<-as.matrix(M[,a])
head(M5_1)

b<-seq(6,59,1)
M5_2<-as.matrix(M[,b])
head(M5_2)

v5_1<-matrix(M5_1,ncol=1)       ##### On met toute les valeurs en une colonne
v5_2<-matrix(M5_2,ncol=1)     #### Pareil ici, mais 5 ans apr?s


# Création d'une grille pour la détermination des valeurs seuils

v<-cbind(v5_1,v5_2) 
head(v)

g<-c(min(v)-0.001, quantile(v,0.25),quantile(v,0.5),quantile(v,0.75),max(v)+0.0001)
g

# Estimation de la matrice de Mobiblité

M5<-trans(v,g)
M5

MC5<-Mcond(M5)
MC5


#### Sans le australie et nouvelle zelande

a<-seq(1,54,1)
M5_11<-as.matrix(M[-c(1,16),a])
head(M5_11)
b<-seq(6,59,1)
M5_21<-as.matrix(M[-c(1,16),b])
head(M5_21)

v5_11<-matrix(M5_11,ncol=1)
v5_21<-matrix(M5_21,ncol=1)

v<-cbind(v5_11,v5_21)
g<-c(min(v)-0.001, quantile(v,0.25),quantile(v,0.5),quantile(v,0.75),max(v)+0.0001)
g

M5<-trans(v,g)
M5

MC5<-Mcond(M5)
MC5

###### A dix ans 

# Avec australie et nouvelle zelande

a<-seq(1,49,1)
M10_1<-as.matrix(M[,a])
head(M10_1)
b<-seq(11,59,1)
M10_2<-as.matrix(M[,b])
head(M10_2)
v10_1<-matrix(M10_1,ncol=1)
v10_2<-matrix(M10_2,ncol=1)


v<-cbind(v10_1,v10_2)
g<-c(min(v)-0.001, quantile(v,0.25),quantile(v,0.5),quantile(v,0.75),max(v)+0.001)
g

M10<-trans(v,g)
M10

MC10<-Mcond(M10)

MC10

## Sans l'australie et nouvelle zelande

a<-seq(1,49,1)
M10_11<-as.matrix(M[-c(1,16),a])

b<-seq(11,59,1)
M10_21<-as.matrix(M[-c(1,16),b])

v10_11<-matrix(M10_11,ncol=1)
v10_21<-matrix(M10_21,ncol=1)


v<-cbind(v10_11,v10_21)
g<-c(min(v)-0.001, quantile(v,0.25),quantile(v,0.5),quantile(v,0.75),max(v)+0.001)
g

M10<-trans(v,g)
M10

MC10<-Mcond(M10)
MC10




### Distribution joint et conditionnelle à 10 ans

install.packages("hdrcde")
library("hdrcde")


# Sans australie et nouvelle zelande

## A cinq ans

xcde<-cde(v5_11,v5_21,nxmargin=15)
plot(xcde)
title(main="Densité condtionnelle à 5 ans")

plot(xcde,plot.fn="hdr")
abline(a=0,b=1)


### A dix ans

xcde<-cde(v10_11,v10_21,nxmargin=15)
plot(xcde)
title(main="Densité condtionnelle à 10 ans")

plot(xcde,plot.fn="hdr")
abline(a=0,b=1)

