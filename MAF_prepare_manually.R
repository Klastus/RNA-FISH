#### Script for preparing MAF files for LEICA AF6000 plate scanning #### 
## author: PT ##

#### loading packages ####
packages.list <- list("foreach", "doParallel")
sapply(packages.list, require, character.only = TRUE)

#### loading functions ####
## function for preparing names for wells ##

normalizeMetadata <- function(metadata_path, 
                              delimeter = ","){
  csv.list <- list.files(path = metadata_path, pattern = ".csv", 
                         recursive = TRUE, full.names = TRUE)
  
  for(csv in csv.list){
    line <- readLines(csv, n = 1)
    if(grepl("\t", line)){
      csv.data <- read.table(file = csv, header = FALSE, sep = "\t")
    } else if(grepl(",", line)){
      csv.data <- read.table(file = csv, header = FALSE, sep = ",")
    } else if(grepl(";", line)){
      csv.data <- read.table(file = csv, header = FALSE, sep = ";")
    } else {
      csv.data <- read.table(file = csv, header = FALSE, sep = " ")
    }
    write.table(csv.data, 
                file = csv, 
                sep = delimeter, 
                row.names = FALSE, 
                col.names = FALSE)
  }
}

which.coor <- function(file.path.active, file.path.middle, file.path.z){
  
  which.active <- function(file.path){
    mapplate <- read.csv(file.path, header = FALSE, sep=',')
    yes <- data.frame()
    for(i in 1:length(mapplate[, 1])){
      for(j in 1:length(mapplate[1, ])){
        if(mapplate[i, ][j] == 1){
          yes <- rbind(yes, data.frame(row=i, column=j))
        }
      }
    }
    return(yes)
  }
  
  
  
  yes <- which.active(file.path.active)
  middle <- read.csv(file.path.middle, header = FALSE, sep=',')
  focus <- read.csv(file.path.z, header = FALSE, sep=',')
  all.wells <- data.frame()
  for(i in 1:length(yes[, 1])){
    x <- 
      unlist(strsplit(as.character(middle[yes$row[i], ][yes$column[i]][[1]]), 
                         '/'))[1]
    y <- 
      unlist(strsplit(as.character(middle[yes$row[i], ][yes$column[i]][[1]]), 
                         '/'))[2]
    z <- unlist(focus[yes$row[i], ][yes$column[i]][[1]])
    all.wells <- rbind(all.wells, data.frame(x=as.numeric(x), 
                                             y=as.numeric(y),
                                             z=as.numeric(z)))
  }
  return(all.wells)
}

## final function for preparing the file ##
prepare.maf.file.middle <- function(path.input, path.output, 
                             all.wells= NA, x.grid, y.grid, 
                             dif.x, dif.y, afc){
  if(is.na(all.wells)){
    all.wells <- which.coor(paste(path.input, "args_active.csv", sep=''),
                               paste(path.input, "plate_middle.csv", sep=''),
                               paste(path.input, "fish_z.csv", sep=''))
  }
  
  
  calculate.coordinates.one <- function(PosX, PosY, x, y, dif.x, dif.y, z){
    
    PosX <- PosX - ((x/2) * dif.x)
    PosY <- PosY - ((y/2) * dif.y)
    PosX.start <- PosX
    PosY.start <- PosY
    positions <- data.frame()
    for(i in 1:x){
      for(j in 1:y){
        positions <- rbind(positions, data.frame(PositionX=PosX, 
                                                 PositionY=PosY, 
                                                 z=z))
        PosY <- PosY + dif.y
      }
      PosY <- PosY.start
      PosX <- PosX + dif.x
    }
    return(positions)
  }
  
  to.numeric <- function(x){
    as.numeric(levels(x))[x]
  } 
  calculate.coordinates.all <- function(all.wells.list, 
                                        x.grid, 
                                        y.grid, 
                                        dif.x, 
                                        dif.y){
    all.coordinates <- 
      foreach(j=1:length(all.wells.list[[1]]), .combine = rbind) %do% {
        calculate.coordinates.one(all.wells.list[, 1][j], 
                                  all.wells.list[, 2][j], 
                                  x=x.grid, y=y.grid, dif.x=dif.x, dif.y=dif.y,
                                  z=all.wells.list[, 3][j])
      }
    all.coordinates$frame <- 1:length(all.coordinates[[1]])
    return(all.coordinates)
  }

  prepare.maf.string <- function(calculated, 
                                 line=second.line, 
                                 afc){
    list.of.positions <- foreach(row = iter(calculated, by='row')) %do% {
      sprintf(line, 
              row$PositionX, 
              row$PositionY, 
              paste("Position", row$frame, sep=''), 
              row$frame, afc, row$z)
    }
    return(paste(unlist(list.of.positions), collapse=''))
  }
  
  second.line <- readLines(paste(path.input, "maf_line_second.txt", sep=''))
  
  first.line <- paste(readLines(paste(path.input, 
                                      "maf_line_first.txt", 
                                      sep='')), collapse='\n')
  
  all.coor <- calculate.coordinates.all(all.wells.list = all.wells, 
                                        dif.x=dif.x,
                                        dif.y=dif.y,
                                        x.grid=x.grid, 
                                        y.grid=y.grid)
  
  maf.string <- prepare.maf.string(all.coor, afc=afc)
  
  first <- "<XYZStagePointDefinitionList>"
  last <- "</XYZStagePointDefinitionList>"
  writeLines(paste(first.line, paste(first, maf.string, last, sep=''), 
                   sep='\n'), path.output)
}

#### file making ####

path.to.platemap <-
  "//148.81.53.180/Experiments/Pathway/RNA_FISH/piotrek/platemap/2018-08-07-PT-FISH02/metadata/"

# has to be path + filename.maf:
path.output <-
  "//148.81.53.180/Experiments/Pathway/RNA_FISH/piotrek/platemap/2018-08-07-PT-FISH02/metadata/tesowanie_zmian.maf"

dif.x <- 0.00011405 #has to be this exact value for 100x objective
dif.y <- 0.00011220 #has to be this exact value for 100x objective
afc <- 0


normalizeMetadata(metadata_path = path.to.platemap)

prepare.maf.file.middle(path.input = path.to.platemap, 
                        path.output = path.output,
                        x.grid = 10, y.grid = 10, 
                        dif.x = dif.x,
                        dif.y = dif.y,
                        afc = afc)

