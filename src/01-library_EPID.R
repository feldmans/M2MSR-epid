#####################
# 01-library_EPID.R #
#####################


require(lubridate)
require(dplyr)
require(ggplot2)
library(grid)
require(gridExtra)
library(survival)
library(stringr)
library(scales)
library(GGally)
library(MatchIt)
library(Matching)
library(mice)
library(psy)
library(knitr)

############################
#   02- functions_EPID.R   #
############################

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


