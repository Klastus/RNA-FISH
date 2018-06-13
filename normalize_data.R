normalize_data <- function(data,
                           normalize_factor = 65535){
  
  data.intensity_colnames <- grepl("Intensity", colnames(data)) & !grepl("Location", colnames(data))
  data[,data.intensity_colnames] <- data[,data.intensity_colnames]*normalize_factor
  return(list(data = data))
}


cc.assign <- function(dane, cc.borders, DAPI.column){
  c.phase <- c()
  for (i in dane[[DAPI.column]]) {
    if (i < cc.borders[1]){
      c.phase <- c(c.phase, "outliers")
    } else if (i < cc.borders[2]){
      c.phase <- c(c.phase, "G1")
    } else if (i < cc.borders[3]){
      c.phase <- c(c.phase, "S")
    } else if (i < cc.borders[4]){
      c.phase <- c(c.phase, "G2/M")
    } else { 
      c.phase <- c(c.phase, "outliers")
    }
  }
  return(c.phase)
}

micro_blocking_DAPI <- function(title,
                                liczby,
                                xlim=c(0,4e6),
                                ylim=c(0,500),
                                bins=100,
                                DAPI.column) {
  
  g<-ggplot(liczby,
            aes_string(x=DAPI.column))+
    geom_histogram(bins=bins)+
    theme_jetka()+
    xlab(paste("DNA content, n=", NROW(liczby)))+
    ggtitle(title)+
    xlim(xlim)+
    ylim(ylim)
  print(g)
  return(g)
}

cc.to.factor <- function(phases=c("G1","S","G2/M"), df){
  df.subset <- df[df$phase %in% phases, ]
  df.subset$phase <- factor(df.subset$phase, levels = phases)
  return(df.subset)
}
variable.subset <- function(data, columns, new.columns){
  data.2 <- data[, colnames(data) %in% columns]
  colnames(data.2) <- new.columns
  return(data.2)
}
randomRows <- function(df, n){
  return(df[sample(nrow(df), n), ])
}

library(gridExtra)
library(flowCore)
library(reshape2)
library(ggplot2)
library(deamer)
library(foreach)
library(doParallel)

