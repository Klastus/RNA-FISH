#### Script for preparing MAF files for LEICA AF6000 plate scanning #### 
## Approach: 
## first, create begin.maf (according to always the same values of the 
#  starting positions of each well from "plate_begin" and "fish_z_begin");
## second, find coordinates XYZ of starting postion with cells 
#  for each active well and click MaF on a microscope software;
## third, input this file to the 'extract.coor' foo and save positions from this 
#  file by 'save.XY.csv" foo to file "plate_end" and "fish_z_end";
## fourth, repeat first step, using new file ("plate_end")
## author: PT ##

#### loading packages ####
packages.list <- list("foreach", "doParallel")
sapply(packages.list, require, character.only = TRUE)

#### loading functions ####
extract.coor <- function(maf.input){
  
  extract.pattern <- function(string, pattern){
    return(regmatches(string, gregexpr(pattern, string)))
  }
  
  patterns <- list()
  patterns[[1]] <- "<XYZStagePointDefinition"
  patterns[[2]] <- "\"(\\d|\\.)*\""
  
  # to have each position as separate element:
  maf <- readLines(maf.input)[5]
  
  maf.sep.pos <- strsplit(maf, patterns[[1]])[[1]][-1:-2]
  
  Positions <- as.data.frame(matrix(-1, nrow = length(maf.sep.pos), ncol = 4),
                             col.names = c("Position", "X", "Y"))

  colnames(Positions) <- c("Position", "X", "Y", "Z")
  
  for(pos in (1:length(maf.sep.pos))){
    coordinates <- list("Position"=pos, "X"=-1, "Y"=-1, "Z"=-1)
  for (axis in c("X", "Y", "Z")){
    if ( axis=="Z"){
      pattern.pos <- paste(axis, "Position=", patterns[[2]], sep='')
    } else {
      pattern.pos <- paste("Stage", axis, "Pos=", patterns[[2]], sep='')
    }
    coor.pos <- extract.pattern(maf.sep.pos[pos], pattern.pos)
    exact.pos <- as.numeric(strsplit(extract.pattern(
      coor.pos[[1]], patterns[[2]])[[1]], "\"")[[1]][2])  
    coordinates[[axis]] <- exact.pos
  }  
    Positions[pos, ] <- coordinates
  }
  return(Positions)
}

subset.pos <- function(positions, which){
  if(which > length(positions[, 1])){ stop("not that much positions!")}
  return(positions[positions[["Position"]] == which, ])
}

save.XY.csv <- function(input.path, 
                        positions.from.maf, 
                        outpath.file.XY,
                        outpath.file.Z,
                        delimeter = ',',
                        output.path = input.path,
                        z.offset = 0){
  
  # input.active- path to args_active
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
  
  active <- which.active(paste(input.path, "args_active.csv", sep = ''))
  
  xy.df <- data.frame(matrix(0, 8, 12))
  z.df <- data.frame(matrix(0, 8, 12))
  
  for(row in (1:length(active[, 1]))){
    well <- active[row, ]
    xy.df[as.numeric(well[1]), as.numeric(well[2])] <- 
      paste(positions.from.maf[row, ]$X, positions.from.maf[row, ]$Y, sep='/')
    z.df[as.numeric(well[1]), as.numeric(well[2])] <- 
      positions.from.maf[row, ]$Z + (z.offset*0.000001)
  }
  
  write.table(xy.df, 
              file = paste(output.path, outpath.file.XY, sep = ''), 
              sep = delimeter, 
              row.names = FALSE, 
              col.names = FALSE)
  
  write.table(z.df, 
              file = paste(output.path, outpath.file.Z, sep = ''), 
              sep = delimeter, 
              row.names = FALSE, 
              col.names = FALSE)
}


## function for preparing names for wells ##

normalizeMetadata <- function(metadata_path, delimeter = ","){
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

prepare.maf.file.middle <- function(path.input, path.output, 
                                    filename.middle = "plate_corner_begin.csv",
                                    filename.z = "fish_z_begin.csv",
                                    all.wells= NA, 
                                    x.grid, y.grid, 
                                    dif.x = 0.00011405,
                                    dif.y = 0.00011220, 
                                    afc = 0, 
                                    middle = FALSE){
  ## final function for preparing the file ##
  if(is.na(all.wells)){
    all.wells <- which.coor(paste(path.input, "args_active.csv", sep=''),
                            paste(path.input, filename.middle, sep=''),
                            paste(path.input, filename.z, sep=''))
  }
  
  
  calculate.coordinates.one <- function(PosX, PosY, x, y, dif.x, dif.y, z,
                                        middle){
    # if you want to start the grid from the position from "plate_middle_begin"
    # positions as top left corner, "middle" should be FALSE. If you want to
    # the grid from the position from "plate_middle_begin" 
    # as the center of the grid, "middle" should be TRUE
    if(middle){
    PosX <- PosX - ((x/2) * dif.x)
    PosY <- PosY - ((y/2) * dif.y)
    }
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
                                  z=all.wells.list[, 3][j],
                                  middle=middle)
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


#### start whole procedure ####
path <- paste("/Volumes/Experiments/Pathway/RNA_FISH/piotrek", 
              "/platemap/2018-08-08-PT-FISH03/metadata/", sep = '')


prepare.maf.file.middle(path.input = path, 
                        path.output = paste(path, "MAF_begin.maf", sep = ''),
                        filename.middle = "plate_middle_begin.csv",
                        x.grid = 1, y.grid = 1, 
                        middle = TRUE)

save.XY.csv(input.path = path,
            positions.from.maf = extract.coor(paste(path,
                                                    "/MAF_leica.maf",
                                                    sep = '')),
            outpath.file.XY = "plate_end.csv",
            outpath.file.Z = "fish_z_end.csv",
            z.offset = 0)


prepare.maf.file.middle(path.input = path, 
                        filename.middle = "plate_end.csv",
                        filename.z = "fish_z_end.csv",
                        path.output = paste(path, "/MAF_end.maf", sep= ''),
                        x.grid = 10, y.grid = 10,
                        middle = TRUE)
