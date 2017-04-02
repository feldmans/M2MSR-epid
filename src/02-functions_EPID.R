######################
#   FUNCTIONS EPID   #
######################

describe_qualitative <- function(vec_var, .data){
  table_var_quali <- lapply(vec_var, function(i){
    data <- .data[,i]
    names_levels <- levels(as.factor(data))
    a <- lapply(names_levels, function(x) {
      tmp <- as.numeric(table(data)[x])
      tmpbis <- round(as.numeric(prop.table(table(data))[x]),3)*100
      tmptot <- paste0(tmp," (",tmpbis,"%)")
      
      nNA <- table(is.na(data))
      pNA <- round(prop.table(table(is.na(data))),3)
      if (is.na(nNA[2]))  {
        if (which(names_levels==x)==1) nNA <- paste0 (0," (0%)") #NA pour ligne 1
        else nNA <- ""
      }
      else {
        browser()
        if (which(names_levels==x)==1){   #NA pour ligne 1
          nNA <- as.numeric (nNA[names(nNA)==TRUE])
          pNA <- as.numeric (pNA[names(pNA)==TRUE])*100
          nNA <- paste0(nNA," (",pNA,"%)")  
        }
        else nNA <- ""
      }
      cbind(tmptot,nNA)
      
    })
    a <- do.call(rbind,a)
    #a <- cbind (a,nNA)
    rownames(a) <- paste0(i,"_",names_levels) 
    colnames(a) <- c("valeur","missing values")
    # a <- rbind (a,nNA)
    # rownames(a)[-nrow(a)] <- paste0(i,"_",names_levels) 
    return(a)
  })
  table_var_quali <- do.call(rbind,table_var_quali)
  table_var_quali <- data.frame(table_var_quali)
  table_var_quali$range <- NA
  colnames(table_var_quali) <- c("valeur", "missing values", "range")
  return (table_var_quali)
}

describe_quantitative <- function(vec_var, .data){
  table_var_quanti <- lapply(vec_var, function(i){ #median ou moyenne? (sachant qu'on ne verifie pas normalite des baselines)
    data <- .data[,i]
    if (!is.numeric(data)) {
      a <- data.frame("not num", "", "") 
      colnames(a) <- c("valeur", "missing values", "range")
      rownames(a) <- i
    } else {
      med <- round(median (data,na.rm=T),2)
      quant <- round(quantile(data,na.rm=T),2)
      Q1 <- quant[2]
      Q3 <- quant[4]
      a <- paste0(med," (",Q1,"-",Q3,")")
      #browser()
      
      nNA <- table(is.na(data))
      pNA <- round(prop.table(table(is.na(data))),3)
      if (is.na(nNA[2]))  nNA <- paste0 (0," (0%)")
      else {
        nNA <- as.numeric (nNA[names(nNA)==TRUE])
        pNA <- as.numeric (pNA[names(pNA)==TRUE])*100
        nNA <- paste0(nNA," (",pNA,"%)")
      }
      
      myrange <- range(data, na.rm=T)
      myrange <- paste0(myrange[1]," - ",myrange[2])
      # a <- rbind (a,nNA)
      # rownames(a)[-nrow(a)] <- paste0(i,"*") 
      a <- cbind (a, nNA, myrange)
      rownames(a) <- paste0(i,"*")
      colnames(a) <- c("valeur", "missing values", "range")
    }
    return(a)
  })
  table_var_quanti <- do.call(rbind,table_var_quanti)
  return (table_var_quanti)
}

describe_all <- function(var, data){
  vec <- data[ ,var]
  if (any(!is.na(as.numeric(as.character(vec)))) & length(levels(as.factor(vec))) != 2) res <- describe_quantitative(var, data)#génère warning si character
  else res <- describe_qualitative(var, data)
  return(res)
}



draw_surv_bin <- function(var, data, .time, .censor, vec_time_IC= c(1, 3), type = "quanti", surv_only=FALSE, pvalue = TRUE, dep_temps=FALSE, .transf=NULL, .scale="year") {
  #browser()
  s <- data
  s$a <- s[ ,var]
  s <- s[!is.na(s$a),]
  .title <- paste0("Survival by ", var)
  
  if (type=="quanti") {
    s$a_recode <- ifelse (s$a < median(s$a), 0, 1)
    #.title <- paste0 ("Survival by ", var, " superior to ", round(median(s$a),0))
  } else {
    s$a_recode <- s$a
  }
  
  s$censor <- s[ ,.censor]
  if(.scale=="year") s$tps <- (s[ ,.time]/365.25) + 0.001 # au cas ou un temps vaut 0 ce qui empêche survsplit de fonctionner
  if(.scale=="month")s$tps <- (s[ ,.time]/12) + 0.0001 # au cas ou un temps vaut 0 ce qui empêche survsplit de fonctionner
  if(.scale=="day") s$tps <- s[ ,.time] + 0.0001
  
  km <- survfit(Surv(tps,censor)~a_recode, data=s, conf.int=.95)
  km0 <- survfit(Surv(tps,censor)~a_recode, data=s[s$a_recode==0,], conf.int=.95)
  km1 <- survfit(Surv(tps,censor)~a_recode, data=s[s$a_recode==1,], conf.int=.95)
  
  #pour IC95%
  skmi0<-summary(km0, time=vec_time_IC-0.1)
  skmi1<-summary(km1, time=vec_time_IC+0.1) #plus d'évènement apres 1.94 ans
  
  if(type=="quali") {
    group0 <- paste0("\nIn group ", var, " = 0\n ")
    group1 <- paste0("\nIn group ", var, " = 1\n ")
  } else {
    group0 <- paste0("\nIn group ", var, " < ",round(median(s$a),0), "\n ")
    group1 <- paste0("\nIn group ", var, " >= ",round(median(s$a),0), "\n ")
  }
  #survies aux tps choisis
  cat(group0)
  sv <- summary(km0, time=vec_time_IC)
  df <- data.frame(time = sv$time, survival = sv$surv*100, LCI = sv$lower*100, UCI = sv$upper*100)
  df[,2:4] <- round(df[,2:4], 0)
  if(.scale=="year") cat(paste0("At ", df$time, " year, survival[95%CI] ", df$survival, "% [",df$LCI,"% - ",df$UCI, "%]\n"))
  if(.scale=="month") cat(paste0("At ", df$time, " months, survival[95%CI] ", df$survival, "% [",df$LCI,"% - ",df$UCI, "%]\n"))
  if(.scale=="day") cat(paste0("At ", df$time, " days, survival[95%CI] ", df$survival, "% [",df$LCI,"% - ",df$UCI, "%]\n"))
  
  cat(group1)
  sv <- summary(km1, time=vec_time_IC)
  df <- data.frame(time = sv$time, survival = sv$surv*100, LCI = sv$lower*100, UCI = sv$upper*100)
  df[,2:4] <- round(df[,2:4], 0)
  if(.scale=="year") cat(paste0("At ", df$time, " year, survival[95%CI] ", df$survival, "% [",df$LCI,"% - ",df$UCI, "%]\n"))
  if(.scale=="month") cat(paste0("At ", df$time, " months, survival[95%CI] ", df$survival, "% [",df$LCI,"% - ",df$UCI, "%]\n"))
  if(.scale=="day") cat(paste0("At ", df$time, " days, survival[95%CI] ", df$survival, "% [",df$LCI,"% - ",df$UCI, "%]\n"))
  
  
  if(surv_only==FALSE){
    #pour table de survie
    skm0 <- summary(km0, time=seq(0, 10, by=1))
    skm0 <- data.frame(time=skm0$time, n.risk=skm0$n.risk)
    skm1<-summary(km1, time=seq(0, 10, by=1))
    skm1 <- data.frame(time=skm1$time, n.risk=skm1$n.risk)
    if(.scale=="day"){
      skm0 <- summary(km0, time=seq(0, 30, by=10))
      skm0 <- data.frame(time=skm0$time, n.risk=skm0$n.risk)
      skm1<-summary(km1, time=seq(0, 30, by=10))
      skm1 <- data.frame(time=skm1$time, n.risk=skm1$n.risk)
    }
    
    #preparation legende
    if(type=="quali")leg<-str_sub(names(km$strata),-1,-1)
    
    if(type=="quanti")leg <- c(paste0(var, " < ", round(median(s$a),0)), paste0(var, " >= ", round(median(s$a),0)))
    col <- hue_pal()(length(leg))
    
    #courbe de survie
    if(.scale=="month"){
      g <- ggsurv(km, CI=FALSE, order.legend=FALSE, surv.col=col, cens.col=col) +
        #changement des axes
        scale_x_continuous(breaks=seq(0,max(s$tps),12), labels=0:(length(seq(0,max(s$tps),12))-1)) +
        scale_y_continuous(labels=percent) +
        labs(x="Time of follow-up, months", title=.title) +
        #changement legende
        guides (linetype = FALSE) +
        scale_colour_discrete( labels = leg) +
        theme(legend.position="right", legend.title=element_blank()) +
        #espace autour du schéma
        theme(plot.margin = unit(c(1,1,3,2), "cm")) #top, right, bottom, left
    }
      
    if(.scale=="year"){
    g <- ggsurv(km, CI=FALSE, order.legend=FALSE, surv.col=col, cens.col=col) +
      #changement des axes
      scale_x_continuous(breaks=seq(0,max(s$tps),1), labels=0:(length(seq(0,max(s$tps),1))-1)) +
      scale_y_continuous(labels=percent) +
      labs(x="Time of follow-up, year", title=.title) +
      #changement legende
      guides (linetype = FALSE) +
      scale_colour_discrete( labels = leg) +
      theme(legend.position="right", legend.title=element_blank()) +
      #espace autour du schéma
      theme(plot.margin = unit(c(1,1,3,2), "cm")) #top, right, bottom, left
    }
    if(.scale=="day"){
      g <- ggsurv(km, CI=FALSE, order.legend=FALSE, surv.col=col, cens.col=col) +
        #changement des axes
        #scale_x_continuous(breaks=seq(0,max(s$tps),1), labels=0:(length(seq(0,max(s$tps),1))-1)) +
        scale_y_continuous(labels=percent) +
        labs(x="Time of follow-up, days", title=.title) +
        #changement legende
        guides (linetype = FALSE) +
        scale_colour_discrete( labels = leg) +
        theme(legend.position="right", legend.title=element_blank()) +
        #espace autour du schéma
        theme(plot.margin = unit(c(1,1,3,2), "cm")) #top, right, bottom, left
    }
    
    #intervalle de confiance
    for (i in 1:2) {
      g <- g + geom_segment(x = skmi0$time[i], y = skmi0$lower[i], xend = skmi0$time[i], yend = skmi0$upper[i], colour = col[1])
      g <- g + geom_segment(x = skmi0$time[i] - 0.1, y = skmi0$lower[i], xend = skmi0$time[i] + 0.1, yend = skmi0$lower[i], colour = col[1])
      g <- g + geom_segment(x = skmi0$time[i] - 0.1, y = skmi0$upper[i], xend = skmi0$time[i] + 0.1, yend = skmi0$upper[i], colour = col[1])
      
      g <- g + geom_segment(x = skmi1$time[i], y = skmi1$lower[i], xend = skmi1$time[i], yend = skmi1$upper[i], colour = col[2])
      g <- g + geom_segment(x = skmi1$time[i] - 0.1, y = skmi1$lower[i], xend = skmi1$time[i] + 0.1, yend = skmi1$lower[i], colour = col[2])
      g <- g + geom_segment(x = skmi1$time[i] - 0.1, y = skmi1$upper[i], xend = skmi1$time[i] + 0.1, yend = skmi1$upper[i], colour = col[2])
    }
    #risk table
    for (ii in 1:nrow(skm0)) {
      g <- g + annotation_custom(grob = textGrob(skm0$n.risk[ii]), xmin = skm0$time[ii], xmax = skm0$time[ii], ymin= - 1.5 )
    }
    for (ii in 1:nrow(skm1)) {
      g <- g + annotation_custom(grob = textGrob(skm1$n.risk[ii]), xmin = skm1$time[ii], xmax = skm1$time[ii], ymin= - 1.7 )
    }
    #display group text
    g <- g + annotation_custom(grob = textGrob(leg[1]), xmin = -1.7, xmax = -1.7, ymin= - 1.5 )
    g <- g + annotation_custom(grob = textGrob(leg[2]), xmin = -1.7, xmax = -1.7, ymin= - 1.7 )
    
    if (pvalue==TRUE){
      if(dep_temps==TRUE){
        ti <- sort(unique(c(0,s$tps[s$censor==1])))
        s$tps <- (s[ ,.time]/365.25*12) + 0.001
        slat <- s
        slat$start <- 0
        slat$stop <- slat$tps
        slat$evt <- slat$censor
        slat <- survSplit(Surv(stop,evt)~.,slat,start="start",cut=ti)
        transf <- .transf
        if (transf=="log") slat$at<-slat$a_recode*log(slat$stop)
        if (transf=="sqrt")slat$at<-slat$a_recode*sqrt(slat$stop)
        if (transf=="*t")slat$at<-slat$a_recode*(slat$stop)
        if (transf=="/t")slat$at<-slat$a_recode/(slat$stop)
        if (transf=="*t^2") slat$at <-slat$a_recode*(slat$stop^2)
        if (transf=="*t^0.7") slat$at <-slat$a_recode*(slat$stop^0.7)
        mod <- coxph(Surv(tps, censor) ~ a_recode + at , data = slat)
      } else {
        mod <- coxph(Surv(tps, censor) ~ a_recode, data = s)
      }
      
      test <- summary (mod)
      pval <- round(test$sctest["pvalue"],3)
      pval <- ifelse(pval<0.05, paste0(pval, " *"), pval)
      pval <- ifelse(pval<0.01, "Score test \n p<0.01 *",paste0("Score test \n p=", pval))
      g <- g + annotate("text",
                        x=0.75*max(km$time),
                        y=0.75*max(km$surv),
                        label=pval)
    }
    
    gt <- ggplotGrob(g)
    gt$layout$clip[gt$layout$name=="panel"] <- "off"
    grid.draw(gt)
  }
}
