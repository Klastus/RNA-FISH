#### Script for preparing MAF files for LEICA AF6000 plate scanning #### 
#### loading packages ####
packages.list <- list("foreach", "doParallel")
sapply(packages.list, require, character.only = TRUE)

#### function for preparing names for wells ####

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

#### final function forpreparing the file ####
prepare.maf.file.middle <- function(path.input, path.output, 
                             all.wells, x.grid, y.grid, 
                             dif.x, dif.y, afc, second.line.file){
  
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
      afc <- afc + 50
      sprintf(line, 
              row$PositionX, 
              row$PositionY, 
              paste("Position", row$frame, sep=''), 
              row$frame, afc, row$z)
      
    }
    return(paste(unlist(list.of.positions), collapse=''))
  }
  
  second.line <- readLines(paste(path.input, second.line.file, sep=''))
  
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

#### preparing variables ####
# for iOS:

# path.to.platemap <-
#   "//148.81.53.180/Experiments/Pathway/RNA_FISH/karolina/platemap/2018-07-13-KZ-FISH06/metadata/"
# path.output <-
#   "//148.81.53.180/Experiments/Pathway/RNA_FISH/karolina/platemap/2018-07-13-KZ-FISH06/metadata/13072018_test.maf"

# path.to.platemap <-
#   "//148.81.53.180/Experiments/Pathway/RNA_FISH/piotrek/platemap/2018-07-31-PT-AFC/metadata/"
# path.output <- paste(path.to.platemap, file.name, sep='')
  # "//148.81.53.180/Experiments/Pathway/RNA_FISH/piotrek/platemap/2018-07-31-PT-AFC/metadata/afc=1000.maf"


path.to.platemap <-
  "//148.81.53.180/Experiments/Pathway/RNA_FISH/piotrek/platemap/2018-07-31-PT-AFC/metadata/"


#### running ####
objective <- "100x"

normalizeMetadata(metadata_path = path.to.platemap)
active.wells <- which.coor(paste(path.to.platemap, "args_active_", objective, ".csv", sep=''),
                           paste(path.to.platemap, "plate_middle.csv", sep=''),
                           paste(path.to.platemap, "fish_z.csv", sep=''))

if(objective=="20x"){
  afc.list <- list(0, 10887)
  dif.x <- 0.00061405
  dif.y <- 0.00040220
} else { 
  # afc.list <- list(0, 20887)
  afc.list <- seq(0888, 03088, 50)
  # dif.x <- 0.00011405
  # dif.y <- 0.00011220
  dif.x <- 0
  dif.y <- 0
  }


for(afc in afc.list){
  file.name <- paste("same_2x2_", objective, "_", afc, ".maf", sep='')
  path.output <- paste(path.to.platemap, "same_not_ideal_0/", file.name, sep='')
  prepare.maf.file.middle(path.input = path.to.platemap,
                          second.line.file="maf_line_second.txt",
                          path.output = path.output,
                          all.wells = active.wells,
                          x.grid = 1, y.grid = 1,
                          dif.x = dif.x,
                          dif.y = dif.y,
                          afc = afc)

}
