require(ggplot2)
library(scales)
library(stringr)

.dir <- getwd()

dta_m <- readRDS(paste0(.dir,"/dta_m.rds"))

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
#- mettre en ordonné le nom de la variable
#- enlever les trous et séparer les variables (retirer les NA puis numéroter les y en séparant les groupes de variables) : (1:20)[1:20 %% 3 !=0] 

