d <- read.csv2("data/rhc_devoir_epidemio.csv")
head(d)
str(d)
dim(d)
summary(d)



d$SADMDTE <- as.Date(d$SADMDTE,"%d/%m/%Y") 
d$DSCHDTE <- as.Date(d$DSCHDTE,"%d/%m/%Y") 
d$DTHDTE <- as.Date(d$DTHDTE,"%d/%m/%Y") 
d$LSTCTDTE <- as.Date(d$LSTCTDTE,"%d/%m/%Y") 

for (i in c("DEATH", "DTH30", "DNR1", "RESP", "CARD", "NEURO", "GASTR", "RENAL", "META", "HEMA", "SEPS", "TRAUMA", "ORTHO")) {
  d[ ,i] <- as.character(d[,i])
  d[,i] <- ifelse (d[ ,i]=="No", 0, d[ ,i])
  d[,i] <- ifelse (d[ ,i]=="Yes", 1, d[ ,i])
  d[,i] <- as.numeric(d[,i])
}
#Pour transformer les "" en NA
#plus long que apply mais garde la bonne structure
for (x in colnames(d)){
  if(is.factor(d[ ,x])) {
    if(length(d[d[ ,x]=="", x])!=0) {
      d[d[ ,x]=="", x] <- NA
      d[ ,x] <- factor(d[,x])
    } 
  }
}

#variables descriptives
vardes <- c("AGE", "SEX", "RACE", "EDU", "INCOME", "NINSCLAS", "CAT1", "CAT2")
var_quali <- c("SEX", "RACE", "INCOME", "NINSCLAS", "CAT1", "CAT2")
var_quanti <- c("AGE","EDU")


var_outcome <- c("DEATH","SWANG1")
var_date <- c("SADMDTE", "DSCHDTE", "DTHDTE", "LSTCTDTE")
# variables de comorbidités
var_com <- c("CARDIOHX", "CHFHX", "DEMENTHX", "PSYCHHX", "CHRPULHX", "RENALHX",
             "LIVERHX", "GIBLEDHX", "MALIGHX", "IMMUNHX", "TRANSHX", "AMIHX")
#variables de l'examen clinique et paraclinique
var_exam <- c("ADLD3P", "DAS2D3PC", "DNR1", "CA", "SURV2MD1", "APS1", "SCOMA1", "WTKILO1", "TEMP1",
              "MEANBP1", "RESP1", "HRT1", "PAFI1", "PACO21", "PH1", "WBLC1", "HEMA1", "SOD1", "POT1",
              "CREA1", "BILI1", "ALB1", "URIN1")
#var admission diagnosis
var_ad <- c("RESP", "CARD", "NEURO", "GASTR", "RENAL", "META", "HEMA", "SEPS", "TRAUMA", "ORTHO")




#CA va poser problème pour le modèle linéaire car cancer oui sera comparé à métastase, alors que ce sont des cancers oui également
#je veux aussi comparer cancer à pas de cancer
d$CANCER <- NA
d$CANCER <- ifelse (!is.na(d$CA) & (d$CA=="Yes" | d$CA=="Metastatic"), 1, d$CANCER)
d$CANCER <- ifelse (!is.na(d$CA) & d$CA=="No" , 0, d$CANCER)
d$METASTASE <- ifelse (!is.na(d$CA) & d$CA=="Metastatic", 1, d$CA) 
d$METASTASE <- ifelse (!is.na(d$CA) & (d$CA=="Yes" | d$CA=="No"), 0, d$CA) 
var_exam <- c(var_exam, "CANCER", "METASTASE")

colnames(d)[!colnames(d) %in% c(var_ad, var_exam, var_com, vardes, var_outcome)]
#%2 variables ne sont pas dans le cahier des variables : T3D30(? de 2 à 30) et DTH30(0ou1, probablement décès à 30j)

#Outcomes
d$SWAN <- as.character(d$SWANG1)
d[d$SWAN=="No RHC","SWAN"] <- 0
d[d$SWAN=="RHC","SWAN"] <- 1
d$SWAN <- as.factor(d$SWAN)
d$DEATH <- as.factor(d$DEATH)
var_outcome <- c(var_outcome, "SWAN")

#var pour analyse de survie
d$ddn <- as_date(ifelse(is.na(d$DTHDTE), d$LSTCTDTE, d$DTHDTE))
d$time <- as.numeric(d$ddn - d$SADMDTE)
d$censor <- ifelse (!is.na(d$DTHDTE), 1, 0)

#recodage des var aberrantes:
d$WTKILO1 <- ifelse(d$WTKILO1<25, NA, d$WTKILO1) #kilo max 244 ok (j'en ai vu)
d$HRT1 <- ifelse(d$MEANBP1==0 & d$RESP1==0 & !is.na(d$RESP1) & !is.na(d$MEANBP1), 0, d$HRT1)
d$MEANBP1 <- ifelse(d$HRT1==0 & d$RESP1==0 & !is.na(d$RESP1) & !is.na(d$HRT1), 0, d$MEANBP1)
d$RESP1 <- ifelse(d$HRT1==0 & d$MEANBP1==0 & !is.na(d$HRT1) & !is.na(d$MEANBP1), 0, d$RESP1)

d$HRT1 <- ifelse(d$HRT1==0 & d$MEANBP1!= 0 & d$RESP1!= 0 & !is.na(d$RESP1) & !is.na(d$HRT1) & !is.na(d$MEANBP1), NA, d$HRT1)
d$MEANBP1 <- ifelse(d$MEANBP1==0 & d$HRT1!=0 & !is.na(d$HRT1) & !is.na(d$MEANBP1), NA, d$MEANBP1)
d$RESP1 <- ifelse(d$RESP1==0 & !is.na(d$RESP1) & d$HRT1!=0 & !is.na(d$HRT1), NA, d$RESP1)


varps <- c("RESP", "GASTR", "RENAL", "HEMA", "SEPS", "TRAUMA", "ADLD3P", "DAS2D3PC", "APS1",
           "SCOMA1", "WTKILO1", "TEMP1", "MEANBP1", "PACO21", "PH1", "HEMA1", "POT1", "CREA1",
           "BILI1", "ALB1", "CARDIOHX", "CHFHX", "DEMENTHX", "PSYCHHX", "CHRPULHX", 
           "LIVERHX", "MALIGHX", "IMMUNHX", "TRANSHX", "AGE", "CANCER", "METASTASE", "INCOME", "NINSCLAS", "CAT1" )
#, "URIN1" #bcp de NA


#score de propension
ps <- glm(formula(paste0("SWAN ~ ",paste(varps,collapse="+"))),d, family="binomial")
d <- d[apply(apply(d[ ,varps], 2, is.na),1,sum)==0, ]
d$psvalue <- as.vector(predict(ps, type = "response"))

dp <- d
#normaliser
dp$WTKILO1 <- (dp$WTKILO1 - mean(dp$WTKILO1))/sd(dp$WTKILO1)
dp$RESP <- (dp$RESP - mean(dp$RESP))/sd(dp$RESP)
dp$INCOME <- (dp$INCOME - mean(dp$INCOME))/sd(dp$INCOME)

names(dp)
for (i in varps[1:32]){
  dp[,i] <- (dp[,i] - mean(dp[,i]))/sd(dp[,i])
}
#quid des variables qualitatives?

dpSG <- dp [dp$SWAN==1,]
dpNSG <- dp [dp$SWAN==0,]
ICminSG <- mean(dpSG$RESP) - 1.96*sqrt(var(dpSG$RESP)/nrow(dpSG))
ICmaxSG <- mean(dpSG$RESP) + 1.96*sqrt(var(dpSG$RESP)/nrow(dpSG))
ICminNSG <- mean(dpNSG$RESP) - 1.96*sqrt(var(dpNSG$RESP)/nrow(dpNSG))
ICmaxNSG <- mean(dpNSG$RESP) + 1.96*sqrt(var(dpNSG$RESP)/nrow(dpNSG))

#plot 
df <- data.frame(mymean=mean(dpSG$RESP),
                 ICminSG = mean(dpSG$RESP) - 1.96*sqrt(var(dpSG$RESP)/nrow(dpSG)),
                 ICmaxSG = mean(dpSG$RESP) + 1.96*sqrt(var(dpSG$RESP)/nrow(dpSG)),
                 myy=1)
g <- ggplot() +geom_point(data=df, aes(mymean,myy)) 
g <- g + geom_segment(aes(x =ICminSG[1], y = myy[1], xend = ICmaxSG[1], yend = myy[1]),data=df)
g <- g + geom_segment(aes(x =ICminSG[1], y = myy[1]-0.1, xend = ICminSG[1], yend = myy[1]+0.1),data=df)
g <- g + geom_segment(aes(x =ICmaxSG[1], y = myy[1]-0.1, xend = ICmaxSG[1], yend = myy[1]+0.1),data=df)

#finir tableau :
# ligne = var
# colonne=
# SG 0/1
# myy
# mean
# ICmin
# ICmax


