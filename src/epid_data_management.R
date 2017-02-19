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

#marche pas
# d2 <- lapply(colnames(d), function(x) {
#   vx <- d[ ,x]
#   if(is.factor(vx)) {
#     if(length(d[d[ ,x]=="", x])!=0)  d[d[ ,x]=="", x] <- NA
#   }
#   return(d[ ,x])
# })
# d2 <- do.call(cbind,d2)
# 
# #transforme tout en facteur:
# d2 <- apply(d, 2, function(x) {
#   if(is.factor(x)) {
#     if(length(x[x==""])!=0)  x[x==""] <- NA
#     else x <-x
#   }
#   else x<-x
#   return(x)
# })
# d2 <- data.frame(d2)



#variables descriptives
vardes <- c("AGE", "SEX", "RACE", "EDU", "INCOME", "NINSCLAS", "CAT1", "CAT2")
var_quali_des <- c("SEX", "RACE", "INCOME", "NINSCLAS", "CAT1", "CAT2")
var_quanti_des <- c("AGE","EDU")


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




# #CA va poser problème pour le modèle linéaire car cancer oui sera comparé à métastase, alors que ce sont des cancers oui également
# #je veux aussi comparer cancer à pas de cancer
# d$CANCER <- NA
# d$CANCER <- ifelse (!is.na(d$CA) & (d$CA=="Yes" | d$CA=="Metastatic"), 1, d$CANCER)
# d$CANCER <- ifelse (!is.na(d$CA) & d$CA=="No" , 0, d$CANCER)
# d$METASTASE <- ifelse (!is.na(d$CA) & d$CA=="Metastatic", 1, d$CA) 
# d$METASTASE <- ifelse (!is.na(d$CA) & (d$CA=="Yes" | d$CA=="No"), 0, d$CA) 
# var_exam <- c(var_exam, "CANCER", "METASTASE")

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



  
# d$DEATH2 <- ifelse(!is.na(d$DTHDTE), 1, 0)
# table(d$DEATH==d$DEATH2) #all true => Je peux utiliser l'une ou l'autre des variables, ont probablement été créées de la meme marnière

#----------
#Description
t1 <- table_var_quali_des <- describe_qualitative(vec_var = var_quali_des_des, .data=d)
t2 <- table_var_quanti_des <- describe_quantitative(vec_var = var_quanti_des, .data=d)
t3 <- describe_qualitative(var_com, d)#idem # summary(apply(d[ ,var_com],2,as.factor))
t4 <- describe_quantitative(var_exam, d)#summary(d[ ,var_exam])
t5 <- describe_qualitative(c("CA","CANCER","DNR1"), d) 
t6 <- describe_qualitative(var_ad, d)
t7 <- describe_qualitative(c("DEATH","SWAN"), d)
obj <- rbind(t1,t2,t3,t4,t5,t6,t7)
write.table(print(obj), file="clipboard", sep="\t")
range(d$LSTCTDTE)
range(d$SADMDTE)
range(d$ddn)

#Nb de NA
#Nb de sujets avec 0, 1, 2, 3, 4 NA 
table(apply(apply(d,2,is.na),1,sum))
#cb de NA pour chaque colonne
apply(apply(d,2,is.na),2,sum)
table(apply(apply(d,2,is.na),2,sum))

#distribution des items
.l <- lapply(colnames(d), function(x){
  if (class(d[,x])=="Date") {
    #browser()
    qplot(d[ ,x], main=x, xlab=NULL, fill=I("navajowhite3"), col=I("pink4"))
  }
  else{
    if (length(names(table(d[,x])))>15) qplot(as.numeric(as.character(d[ ,x])), main=x, xlab=NULL, fill=I("navajowhite3"), col=I("pink4"))
    else qplot(as.factor(d[ ,x]), main=x, xlab=NULL, fill=I("navajowhite3"), col=I("pink4"))  
  }
  
})
ml <- marrangeGrob(.l,ncol=3,nrow=3,top = NULL)
ggsave(file="distrib bef dm.pdf", ml)
print(ml)
#-------------
#verif
HR0 <- ifelse(d$HRT1==0, "FC0", ifelse(!is.na(d$HRT1),0,NA))
FC0 <- ifelse(d$RESP1==0, "FR0", ifelse(!is.na(d$RESP1),0,NA))
TA0 <- ifelse(d$MEANBP1==0, "TA0", ifelse(!is.na(d$MEANBP1),0,NA))
#pas de NA de toutes façons pour ces valeurs
d$cleCR <- paste(HR0, FC0, TA0, sep="|")
table(d$cleCR)
d[d$cleCR=="FC0|FR0|0", c("HRT1", "RESP1", "MEANBP1", "DTH30", "SCOMA1", "TEMP1")]

#"0|0|0" :aucune valeure ne vaut 0, ok
#"0|0|TA0" : LA tension artérielle vaut 0 mais pas le reste : probable erreur de mesure
d$MEANBP1 <- ifelse(d$cleCR=="0|0|TA0", NA, d$MEANBP1)
#"0|FR0|0"  : réa probablement pas d'accord sur quoi mesurer faire en cas de respirateur : chercher variable respirateur, sinon garder telle quelle? 
#"0|FR0|TA0" : TA 0 et respi 0, je mets la FC à 0 aussi
d$HRT1 <- ifelse(d$cleCR=="0|FR0|TA0", 0, d$HRT1)
#"FC0|0|0" : seul la FC vaut0 : erreur, FC =NA
d$HRT1 <- ifelse(d$cleCR=="FC0|0|0", NA, d$HRT1)
#"FC0|0|TA0" : probablement encore une histoire de respirateur : si TA et FC vaut 0, alors FR aussi   
d$RESP1 <- ifelse(d$cleCR=="FC0|0|TA0", 0, d$RESP1)
#"FC0|FR0|0" : quelq'un qui ne respire pas et n'a pas de pouls a une tenion nulle également
d$MEANBP1 <- ifelse(d$cleCR=="FC0|FR0|0", 0, d$MEANBP1)
#"FC0|FR0|TA0" : toutes les valeurs sont à 0 ok 

#j'ai vérifié que d$DEATH idem que d$DEATH2 <- ifelse (!is.na(d$DTHDTE),1,0)

#Attention! les schéma ne donnent pas les NA pour les var quantitatives!
#Na : cat2, ADLD3P
#var bizarres : EDU : 30ans?(p-e...) ; URIN1: 9L? oui possible
#HRT1 : 0, >20 et plus de 200 ; MEANBP1: pic à 80 et pic à 140, pression >200? pression=0;
#RESP1 >50 oui c'est possible; temp1<30 oui possible; weight=0 le min et 244 le max (ok pour 244 mais pas pour 0)

#temp mini à 27° dans le fichier : c'est possible en cas d'hypothermie sévère (et hypothermie induite 30-32° parfois en réa dans cas particuliers)
#http://www.srlf.org/wp-content/uploads/2015/11/0505-Reanimation-Vol14-N3-p177_185.pdf
#var inconnues : surv2MD1, das2d3pc, t3d30, aps1, wblc1, pafi1
d[d$URIN1==5000 & !is.na(d$URIN1), var_exam]
#---------------
#refaire en coupant SCOMA1 en classe et recuperer la courbe de survie pour var qualitative
d$SCOMAsup90 <- ifelse(d$SCOMA1>90,1,0)
d$SCOMAcut <- cut(d$SCOMA1, breaks=5)
draw_surv_bin(var="SCOMAsup90", data=d, .time="time", .censor="censor", vec_time_IC= c(1, 3), type = "quali", surv_only=FALSE, pvalue = TRUE, dep_temps=FALSE, .transf=NULL)
plot(d$SCOMA1, d$APS1) #le score APS ne nous aide pas
#plus score est élevé, plus le risque de deces est eleve. 
ggsurv(survfit(Surv(time, censor)~SCOMAcut, data=d), order.legend =FALSE)

#Pas de doublons
d[duplicated(d$PTID),]

#recodage du poids:
d$WTKILO1 <- ifelse(d$WTKILO1<25, NA, d$WTKILO1) #kilo max 244 ok (j'en ai vu)

#HRT1

# oui impossible d'avoir une FR à 0 avec FC à 52 et vice versa. Si arrêt Cardio respi les 2 sont à 0.
# Natrémie en dessous de 110 possible et au dessus de 170 exceptionnel (ok si 1 seul). 
# Pas possible pour hématocrite à 2 pour cent. 
# Créât et bili 10 fois la normale : c'est possible.
# Albu 110 et 290 non possible...
#----------------
#refaire description

#Description
t1 <- table_var_quali_des <- describe_qualitative(vec_var = var_quali_des, .data=d)
t2 <- table_var_quanti_des <- describe_quantitative(vec_var = var_quanti_des, .data=d)
t3 <- describe_qualitative(var_com, d)#idem # summary(apply(d[ ,var_com],2,as.factor))
t4 <- describe_quantitative(var_exam, d)#summary(d[ ,var_exam])
t5 <- describe_qualitative(c("CA","CANCER","DNR1"), d) 
t6 <- describe_qualitative(var_ad, d)
t7 <- describe_qualitative(c("DEATH","SWAN"), d)
obj <- rbind(t1,t2,t3,t4,t5,t6,t7)
write.table(print(obj), file="clipboard", sep="\t")

# 
# table_var_quali_des <- describe_qualitative(vec_var = var_quali_des, .data=d)
# table_var_quanti_des <- describe_quantitative(vec_var = var_quanti_des, .data=d)
# table_var <- rbind(table_var_quali_des,table_var_quanti_des)
# kable(table_var)
# describe_qualitative(var_com, d)#idem # summary(apply(d[ ,var_com],2,as.factor))
# describe_quantitative(var_exam, d)#summary(d[ ,var_exam])
# describe_qualitative("CA", d)
# describe_qualitative(var_ad, d)
# describe_qualitative(c("DEATH","SWAN"), d)
# range(d$LSTCTDTE)
# range(d$SADMDTE)
# range(d$ddn)

#Nb de NA
#Nb de sujets avec 0, 1, 2, 3, 4 NA 
table(apply(apply(d,2,is.na),1,sum))
#cb de NA pour chaque colonne
apply(apply(d,2,is.na),2,sum)
table(apply(apply(d,2,is.na),2,sum))

#distribution des items
.l <- lapply(colnames(d), function(x){
  if (class(d[,x])=="Date") qplot(d[ ,x], main=x, xlab=NULL, fill=I("navajowhite3"), col=I("pink4"))
  else{
    if (length(names(table(d[,x])))>15) qplot(as.numeric(as.character(d[ ,x])), main=x, xlab=NULL, fill=I("navajowhite3"), col=I("pink4"))
    else qplot(as.factor(d[ ,x]), main=x, xlab=NULL, fill=I("navajowhite3"), col=I("pink4"))  
  }
  
})
ml <- marrangeGrob(.l,ncol=3,nrow=3,top = NULL)
ggsave(file="distrib aft dm.pdf", ml)

summary(d)
#------------
d$DAS2D3PC


#TESTS BIVARIES : 

#avec swanganz

list_swan <- lapply(c(var_ad, var_exam, var_com, vardes), function(x){
  #x <- "CA"
 # for (x in c(var_ad, var_exam, var_com, vardes)){
  print(x)
  d$var <- d[,x]
  if (all(levels(as.factor(d$var)) %in% c(0,1))) d$var <- as.factor(d$var) 
  mod <- glm(SWAN~var,d, family="binomial")
  test <- summary(mod)
  #browser()
  if (nrow(coef(test))>2){ #cas variable explicative qualitative
    test <- drop1(mod, .~., test="Chisq")
    ab <- test$`Pr(>Chi)`[2]
    #SI on veut tester chaque classe, mais finalement je fais un drop 1
    # for (i in (2:nrow(coef(test)))){
    #   ab1 <- coef(test)[i, "Pr(>|z|)"]
    #   ab <- if(i==2) ab1 else rbind(ab, ab1)
    # }
    ab <- round(ab, 3)
    ab <- data.frame(ab)
    ab$signif <- ifelse(ab$ab<0.05,"*","") 
    #ab$ab <- ifelse(ab$ab<0.001, "<0.001", ab$ab) #empeche de selectionner les bonnes variables après
    #rownames(ab) <- paste(x, levels(d$var)[-1], "ref", levels(d$var)[1])
    rownames(ab) <- x
  } else {
    #browser()
    ab <- coef(test)[2, "Pr(>|z|)"]
    ab <- round(ab, 3)
    ab <- data.frame(ab)
    ab$signif <- ifelse(ab$ab<0.05,"*","") 
    #ab$ab <- ifelse(ab$ab<0.001, "<0.001", ab$ab)
    rownames(ab) <- x  
    }
  colnames(ab) <- c("coef pvalue SWAN","significatif SWAN")
  
 # }
  return(ab)
})
list_swan <- do.call(rbind, list_swan)

list_death <- lapply(c(var_ad, var_exam, var_com, vardes), function(x){
  #x <- "CA"
  # for (x in c(var_ad, var_exam, var_com, vardes)){
  print(x)
  d$var <- d[,x]
  if (all(levels(as.factor(d$var)) %in% c(0,1))) d$var <- as.factor(d$var) 
  mod <- glm(DEATH~var,d, family="binomial")
  test <- summary(mod)
  #browser()
  if (nrow(coef(test))>2){ #cas variable explicative qualitative
    test <- drop1(mod, .~., test="Chisq")
    ab <- test$`Pr(>Chi)`[2]
    # for (i in (2:nrow(coef(test)))){
    #   ab1 <- coef(test)[i, "Pr(>|z|)"]
    #   ab <- if(i==2) ab1 else rbind(ab, ab1)
    # }
    ab <- round(ab, 3)
    ab <- data.frame(ab)
    ab$signif <- ifelse(ab$ab<0.05,"*","") 
    #ab$ab <- ifelse(ab$ab<0.001, "<0.001", ab$ab)
    #rownames(ab) <- paste(x, levels(d$var)[-1], "ref", levels(d$var)[1])
    rownames(ab) <- x
  } else {
    #browser()
    ab <- coef(test)[2, "Pr(>|z|)"]
    ab <- round(ab, 3)
    ab <- data.frame(ab)
    ab$signif <- ifelse(ab$ab<0.05,"*","") 
    #ab$ab <- ifelse(ab$ab<0.001, "<0.001", ab$ab)
    rownames(ab) <- x  
  }
  colnames(ab) <- c("coef pvalue DEATH","significatif DEATH")
  
  # }
  return(ab)
})

list_death <- do.call(rbind, list_death)

list_pval <- cbind(list_swan, list_death)

#prendre les valeurs : liées soit uniquement au décès, soit liées à la sonde et au décès (ce qui revient à prendre variable significative pour le décès ici) 
list_pval$select <- ifelse (list_pval$`coef pvalue DEATH`<0.05, 1, 0)
#list_pval$select <- ifelse (list_pval$`coef pvalue DEATH`<0.05 & list_pval$`coef pvalue SWAN`<0.05, 1, 0) #ces variables sont déjà sélectionné par la ligne du dessus
list_pval[list_pval$select==1, ]
nrow(list_pval[list_pval$select==1, ])
dput(rownames(list_pval[list_pval$select==1,]))
#c(rownames(list_pval[list_pval$select==1,])[c(1:8,12:36)],"CA","INCOME","NINSCLAS","CAT1")
#dput(rownames(list_pval[list_pval$select==1, ])) #pour éviter de tout taper à la main!ya plus qu'à copier coller
varps <- c("RESP", "GASTR", "RENAL", "HEMA", "SEPS", "TRAUMA", "ADLD3P", 
           "DAS2D3PC", "DNR1", "CA", "SURV2MD1", "APS1", "SCOMA1", "WTKILO1", 
           "TEMP1", "MEANBP1", "PACO21", "PH1", "HEMA1", "POT1", "CREA1", 
           "BILI1", "ALB1", "URIN1", "CARDIOHX", "CHFHX", "DEMENTHX", "PSYCHHX", 
           "CHRPULHX", "LIVERHX", "MALIGHX", "IMMUNHX", "TRANSHX", "AGE", 
           "INCOME", "NINSCLAS", "CAT1", "CAT2")
#cb de NA pour chaque colonne
percNA <- round(apply(apply(d[,varps],2,is.na),2,sum)/nrow(d)*100,0)
namesNA <- names(percNA[percNA>50])
#je retire du score les variables avec plus de 50% de NA :ADLD3P, URIN1  et CAT2
varps <- varps[!varps%in% namesNA]

#----------------
#score de propension
ps <- glm(formula(paste0("SWAN ~ ",paste(varps,collapse="+"))), data = d, family="binomial")

d2 <- d[apply(apply(d[ ,varps], 2, is.na),1,sum)==0, ] #J'elimine les lignes avec au moins 1 NA dans les variables slectionnes varps
d2$logitps <- as.vector(predict(ps, type = "response")) #response is the default for binomial model


#Histogramme du score de propension en fonction du groupe de traitement
prs_df <- data.frame(pr_score = predict(ps, type = "response"), 
                     SWAN = ps$model$SWAN)
labs <- paste("actual intervention:", c("no SWAN-GANZ", "SWAN-GANZ"))
prs_df %>%
  mutate(SWAN = ifelse(SWAN == 1, labs[2], labs[1])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~SWAN) +
  xlab("Probability of having a SWAN GANZ") +
  theme_bw()

#---------------------
#Analyse en utilisant la méthode des quantiles (non complet)

# QT <- quantile(prs_df$pr_score)
# prs_df$quantile <- ifelse (prs_df$pr_score<QT[2], 1, NA)
# prs_df$quantile <- ifelse (prs_df$pr_score>=QT[2] & prs_df$pr_score<QT[3], 2, prs_df$quantile)
# prs_df$quantile <- ifelse (prs_df$pr_score>=QT[3] & prs_df$pr_score<QT[4], 3, prs_df$quantile)
# prs_df$quantile <- ifelse (prs_df$pr_score>=QT[4] & prs_df$pr_score<QT[5], 4, prs_df$quantile)
# prs_df$quantile <- ifelse (prs_df$pr_score>QT[5], 5, prs_df$quantile)

d2$psgp <- cut(d2$logitps, breaks=quantile(d2$logitps, prob=0:4*0.25),
                    labels=c("Q1","Q2","Q3","Q4"), right=FALSE, include.lowest=TRUE)

#----------------
#Appariemment sur le score de propension:

#Package Matching : Le traitement (ici SWan Ganz) doit être en true false
#cours de David Hajage, MD PhD, département de biostatistiques de la Pitié Salpétriêre
d2$SWANT <- ifelse(d2$SWAN==1, T, F)

tmp <- Match(Tr = d2$SWANT, X = d2$logitps, M = 1, replace = FALSE, caliper = 0.2, ties = FALSE)

d2.app <- d2[c(tmp$index.treated, tmp$index.control),]#index.treated et index.control donne le numero des lignes selectionnees par le matching. Le premier individu de index.treated est matchée avec le 1er de index.ctrl. 
d2.app$paire <- rep(1:length(tmp$index.treated), 2) #lignes de d2.app : d'abord les traités de chaque paire puis les control de chaque paire, donc on répète le numéro de paire
d2.app <- d2.app[order(d2.app$paire, d2.app$SWAN==1),] #on réordonne selon la paire SG(1)NSG(1) SG(2)NSG(2) etc
#=> donc ce tableau prend toutes les variables, uniquement les lignes correspondant aux individus matché et pour chaque individu on connait son numéro de paire


#package MatchIt
#https://stanford.edu/~ejdemyr/r-tutorials-archive/tutorial8.html#exercise
#https://stanford.edu/~ejdemyr/r-tutorials-archive/matching.R

mod_match <- matchit(formula(paste0("SWAN ~ ",paste(varps,collapse="+"))),
                     method = "nearest", replace = FALSE, ratio = 1, m.order = "smallest", caliper=0.2, data = d2[,c("SWAN",varps,"PTID")])
#MatchIt ne sait pas gérer les NA (même si les colonnes de la formule n'ont pas de NA) => JE dois préciser les colonnes qui m'interess dans data


#Pour retrouver les paires
matches<-data.frame(mod_match$match.matrix)

# > dim(matches)
# [1] 2025    1
#2025 lignes, qui correspondent aux 2025 individus traités (avant matching)

# > head(matches)
#       X1
#   2  <NA>
#   5   927
#   10 2383
#le patient SWAN de la ligne nommée 2 du tableau d2  n'est matché avec aucun patient non SWAN : il faut eliminer la ligne
#le patient SWAN de la ligne nommée 5 du tableau d2 est matché avec le patient non SWAN de la ligne nommée 927

#J'élimine les les lignes avec NA (correspond aux traités qui n'ont pas été appariée)
matches <- na.omit(matches)

groupSG1<-match(row.names(matches), row.names(d2)) #donne la position de chaque patient traité dans le tableau d2
groupSG0<-match(matches$X1, row.names(d2)) #donne la position de chaque patient non traité dans d2

d2.appbis <- d2[c(groupSG1, groupSG0),]
d2.appbis$paire <- rep(1:length(groupSG1), 2) 
d2.appbis <- d2.appbis[order(d2.appbis$paire, d2.appbis$SWAN==1), ]

#si jamais j'ai finalement besoin des distances :
dta_m <- match.data(mod_match) 
dtm <- merge(d2.appbis, dta_m[,c("PTID","distance","weights")], by="PTID", all=T)


#-----------------
# Checking balance : 
#a faire uniquement sur les variables servant à construire le score de propension, car permet de voir
#si le score et le matching ont bien marché

MatchBalance(formula(paste0("SWANT ~ ",paste(varps,collapse="+"))), data=d)
#METHODE 1 : QQPLOT
plot(mod_match)

#METHODE 2 :moyenne pour chaque variable à chaque point de score de popension
fn_bal <- function(dta, variable) {
  #browser()
  dta$variable <- dta[, variable]
  dta$SWAN <- as.factor(dta$SWAN)
  ggplot(dta, aes(x = distance, y = variable, color = SWAN)) +
    geom_point(alpha = 0.2, size = 1.5) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw()
}

.l <- lapply(varps, function(variable){
  print(variable)
  num <- which(varps==variable)
  if (num %% 2 != 0) fn_bal(dta_m, variable)
  else fn_bal(dta_m, variable) + theme(legend.position = "none")
})
#ml <- marrangeGrob(.l, nrow=3, ncol=2, widths = c(1, 0.85), top = NULL)
ml <- marrangeGrob(.l, nrow=2, ncol=2, top = NULL)
ggsave(file="distrib mean variables after matching.pdf", ml)


#METHODE 3 : Print mean difference 
varps_noql <- varps[!varps %in% c("INCOME", "NINSCLAS", "CAT1")]
dta_mQ <- dta_m[ ,c(varps_noql,"SWAN")]
diff <- dta_mQ%>%
  group_by(SWAN) %>% 
  summarise_all(funs(mean))
#https://cran.r-project.org/web/packages/tableone/vignettes/smd.html
#https://github.com/kaz-yos/tableone/blob/1d47ec186b2e351937e5f9712dad3881380ab12e/vignettes/smd.Rmd

tabUnmatched <- CreateTableOne(vars = c(varps,"SWAN"), strata = "SWAN", data = d, test = FALSE)
tabMatched <- CreateTableOne(vars = c(varps,"SWAN"), strata = "SWAN", data = dta_m, test = FALSE)

## Construct a data frame containing variable name and SMD from all methods
dataPlot <- data.frame(variable  = names(ExtractSmd(tabUnmatched)),
                       Unmatched = ExtractSmd(tabUnmatched),
                       Matched   = ExtractSmd(tabMatched))

## Create long-format data for ggplot2
dataPlotMelt <- melt(data          = dataPlot,
                     id.vars       = c("variable"),
                     variable.name = "Method",
                     value.name    = "SMD")
colnames(dataPlotMelt) <- c("variable","Method","SMD")

varNames <- as.character(dataPlot$variable)[order(dataPlot$Unmatched)]

## Order factor levels in the same order
dataPlotMelt$variable <- factor(dataPlotMelt$variable,
                                levels = varNames)

## Plot using ggplot2
ggplot(data = dataPlotMelt, mapping = aes(x = variable, y = SMD,
                                          group = Method, color = Method)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.1, color = "black", size = 0.1) +
  coord_flip() +
  theme_bw() + theme(legend.key = element_blank())


#METHODE 3 BIS standardized mean difference sans tableone
smd <- summary(mod_match, standardize = TRUE) #fait la balance pour chaque binaire tirée de la variable quali
smd <- smd$sum.matched
smd$var <- rownames(smd)
smd$group <- "matched"
g <- ggplot(data = smd, aes(x=var, y= `Std. Mean Diff.`, group = group, color= group))
g <- g + geom_point() + geom_line() + geom_hline(yintercept = 0.1, color="red", size=0.1) +
  coord_flip() + theme_bw() + theme(legend.key = element_blank())
  


#METHODE 4 IC à la main

getMIC <- function(.data){
  dt <- lapply(names(.data), function(var){
    num <- which(var==names(.data))
    
    .data[,var] <- (.data[,var] - mean(.data[,var]))/sd(.data[,var]) #standardisation 
    
    x <- .data[.data$SWAN == 1, var]
    df1 <- data.frame(mymean = mean(x),
                      ICminSG = mean(x) - 1.96*sqrt(var(x)/nrow(.data)),
                      ICmaxSG = mean(x) + 1.96*sqrt(var(x)/nrow(.data)),
                      myy = num*2-0.5,
                      groupe = "SWAN",
                      colour = 1)
    
    x <- .data[.data$SWAN == 0, var]
    df0 <- data.frame(mymean = mean(x),
                      ICminSG = mean(x) - 1.96*sqrt(var(x)/nrow(.data)),
                      ICmaxSG = mean(x) + 1.96*sqrt(var(x)/nrow(.data)),
                      myy = num*2+0.5,
                      groupe = "NO SWAN",
                      colour = 2)
    
    df <- rbind(df1,df0)
  })
  dt <- do.call(rbind, dt)
  dt$var <- rep(names(.data),each=2)
  return(dt)
}

df <- getMIC(dta_m)
df <- na.omit(df)
head(df)

m<-tapply(df$myy, df$var, mean)
m<-data.frame(var=names(m), y=m)
head(m)

df2<-merge(df, m, by="var", all=T)
df2<-df2[order(df2$var, df2$group),]
df2$y<-ifelse(df2$groupe=="SWAN", 0.25, -0.25)+df2$y
head(df2)



laby<-unique(df2$var)

col <- hue_pal()(length(1:2))

g <- ggplot(data=df2, aes(x=mymean, xmin = ICminSG, y = y, xend = ICmaxSG, colour=groupe)) + geom_point() 
g <- g + labs(y = "variable")
g<-g+scale_y_continuous(name="Variables", breaks=m$y, labels=m$var)
g

for (i in 1:nrow(df)){
  g <- g + geom_segment(x = df2$ICminSG[i], y = df2$y[i], xend = df2$ICmaxSG[i], yend = df2$y[i], colour = col[df2$colour[i]])
  g <- g + geom_segment(x = df2$ICminSG[i], y = df2$y[i]-0.5, xend = df2$ICminSG[i], yend = df2$y[i]+0.5, colour = col[df2$colour[i]])
  g <- g + geom_segment(x = df2$ICmaxSG[i], y = df2$y[i]-0.5, xend = df2$ICmaxSG[i], yend = df2$y[i]+0.5, colour = col[df2$colour[i]])
}
g



#reste à faire pour le schéma : 
#- vérifier que j'ai bien standardisé
#- enlever les trous et séparer les variables (retirer les NA puis numéroter les y en séparant les groupes de variables) : (1:20)[1:20 %% 3 !=0] 



#---------
#analysis
#http://imai.princeton.edu/research/files/matchit.pdf
#journals.sfu.ca/jmde/index.php/jmde_1/article/download/431/414
#http://r.iq.harvard.edu/docs/matchit/2.4-20/matchit.pdf


#Analyse avec d2.app (methode David Hajage)
d2.app$DEATH <- as.numeric(as.character(d2.app$DEATH))
d2.app$SWAN <- as.numeric(as.character(d2.app$SWAN))
clogit(DEATH~SWAN + cluster(paire), method="efron", data=d2.app) #marche avec efron
coxph(Surv(rep(1, nrow(d2.app)),DEATH)~SWAN + cluster(paire), data=d2.app) #marche mais est-ce vraiment la même chose?
glm(DEATH~SWAN, data=d2.app, family="binomial")
glmer(DEATH~SWAN + (1|paire), data=d2.app, family = "binomial")



#analyse avec mod_matchet dtm
#Paired t-test
t.test(as.numeric(matched.cases$yT), as.numeric(matched.cases$yC), paired = TRUE)

 


#-------------------
#QUESTIONS

#J'ai trop de variables, je ne sais pas quoi faire
# var <- c("AGE", "SCOMA1", "WTKILO1", "TEMP1", "MEANBP1", "PACO21", "PH1",
#          "HEMA1", "POT1", "CREA1", "BILI1", "ALB1"     ,"URIN1" )

#"SURV2MD1" je ne comprends pas ce que c'est
#"CA" JE ne sais pas s'il faut la mettre quand on a CA aussi, s'il faut garder CA s'il faut la transformer cancer 0/1 et meta 0/1
#Pour les variables qualitatives, je fais drop ou je sélectionne par paramètre et je ne prends que les classes significatives? Est-ce que je les garde dans le SP en les transformant en var binaire?
#Comment faire pour les variables avec beaucoup de données manquantes?
#quelle formule utiliser pour différence standardisée des moyennes : moyenne ctrl - moyenne 
#faut-il faire la balance pour toutes les variables ou uniquement variables du score de ps
#comment checker la balance des variables qualitatives (tableone calcule une moyenne standardisée pour ces variables je ne vois pas comment?)
# que faire des var quali : quand on selectionne var pour score de propension, quand on regarde la balance: transformer en bianaire? faire drop pour la selection? que plotter pour vérifier la balance? tableone plot qqch mais je ene sais pas quoi...
#comment on met les paires dans le modèle?
#lien avec la mort : regression logistique ou cox??
#modele final : ne vaut-il pas mieux faire une analyse de survie??

table(table(unique(d$ROWNAMES)))
levels(d$CAT1)
levels(d$CAT2)
levels (d$CA)
l <- lapply(colnames(d), function(x) {
  vx <- d[ ,x]
  if(is.factor(vx)) {
    res <- levels(vx)
    return(c(x, res))
  } else NULL
})
do.call(rbind,l)
unlist(l)

range(d$SADMDTE)
range(d$DSCHDTE, na.rm=T)
range(d$DTHDTE, na.rm=T)
range(d$LSTCTDTE, na.rm=T)

d[1:5, c("SADMDTE", "DSCHDTE", "DTHDTE", "LSTCTDTE")]


#0.2695
summary(d$RESP)
