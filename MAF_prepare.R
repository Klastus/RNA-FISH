
#### preparing names for wells ####

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
which.coor <- function(file.path.active, file.path.middle, file.path.z){
  
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

#### koncowa funkcja ####
prepare.maf.file.middle <- function(path.input, path.output, 
                             all.wells, x.grid, y.grid, 
                             dif.x, dif.y, afc){
  
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
  "/Users/piotrt/Documents/IPPT_PT/FISH/KZ-FISH02-2018-06-07/platemap/"
path.output <- 
  "/Users/piotrt/Documents/IPPT_PT/FISH/KZ-FISH02-2018-06-07/platemap/07062018.maf"
dif.x <- 0.00011405
dif.y <- 0.00011220
afc <- 0

normalizeMetadata(metadata_path = path.to.mapplate)
active.wells <- which.coor(paste(path.to.platemap, "fish_active.csv", sep=''),
                           paste(path.to.platemap, "plate_middle.csv", sep=''),
                           paste(path.to.platemap, "fish_z.csv", sep=''))


prepare.maf.file.middle(path.input = path, 
                        path.output = path.output, 
                        all.wells = active.wells, 
                        x.grid=10, y.grid=10, 
                        dif.x=dif.x,
                        dif.y=dif.y,
                        afc=afc)

