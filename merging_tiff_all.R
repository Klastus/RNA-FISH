#### merging photos ####

#### loading packages ####
packages.list <- list("tiff", "foreach", "doParallel")
sapply(packages.list, require, character.only = TRUE)

#### working functions ####
normalizeMetadata <- function(metadata_path, 
                              delimeter = ","){
  csv.list <- list.files(path = metadata_path,
                         pattern = ".csv", 
                         recursive = TRUE, 
                         full.names = TRUE)
  
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
    write.table(csv.data, file = csv, sep = delimeter, 
                row.names = FALSE, col.names = FALSE)
  }
}

which.wells <- function(file.path.active, file.path.wells){
  
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
  wells <- read.csv(file.path.wells, header = FALSE, sep=',')
  
  all.wells <- list()
  for(i in 1:length(yes[, 1])){
    well <- wells[yes$row[i], ][[yes$column[i]]]
    all.wells[i] <- as.character(well)
  }
  return(all.wells)
}

writeTIFFDefault <- function(what, where){
  writeTIFF(what = what, 
            where =  where,
            bits.per.sample = 16,
            compression = "none")
}

merge.multiple.z <- function(row.number, 
                             col.number, 
                             path.to.photos, 
                             z.range, 
                             subs.well, 
                             file.pattern){
  
  
  merge.one.z <- function(tiff.list, 
                          row.number, 
                          col.number,
                          path.to.photos){
    row.number.tmp <- 1
    col.number.tmp <- 0
    matrix.biggest <- matrix()
    for(file in tiff.list){
      col.number.tmp <- col.number.tmp + 1
      
      if(row.number.tmp <= row.number){
        photo <- readTIFF(paste(path.to.photos, '/', file, sep=""))
        
        if (row.number.tmp == 1){
          matrix.big <- photo 
        } else { matrix.big <- rbind(matrix.big, photo) }
        # close(photo)
        row.number.tmp <- row.number.tmp + 1
      } 
      if(row.number.tmp > row.number) {
        row.number.tmp <- 1
        if (is.na(matrix.biggest[1][1])){
          matrix.biggest <- matrix.big 
        } else { matrix.biggest <- cbind(matrix.biggest, matrix.big) }
      }
      if(col.number.tmp > row.number*col.number){
        break
      }
    }
    gc()
    return(matrix.biggest)
  }
  
  tiles.no <- row.number * col.number

  tiff.sublist <- list()
  # registerDoParallel(no.cores)
  image.list <- foreach(z=(z.range), .packages = "tiff") %do% {
    for(no.of.digits in c(3, 4, 5)){
      tiff.sublist[[no.of.digits]] <-  
        list.files(path = path.to.photos,
                   pattern = sprintf(file.pattern, no.of.digits, z))
    }
    tiff.list <- do.call(c, tiff.sublist)[
      (((subs.well-1)*tiles.no)+1):(tiles.no*subs.well)
      ]
    merge.one.z(tiff.list, 
                row.number = row.number, 
                col.number = col.number, path.to.photos)
  }
  # stopImplicitCluster()
  return(image.list)
}

getCollapseImageMatrix <- function(
  images.list,
  collapsing,
  nrow = NULL,
  ncol = NULL){
  if(is.null(nrow)){nrow <- nrow(images.list[[1]])}
  if(is.null(ncol)){ncol <- ncol(images.list[[1]])}
  images.mean <- matrix(data = rep(x = 0, times = nrow*ncol), 
                        nrow = nrow, 
                        ncol = ncol)
  if(collapsing=="mean"){
    collapse.function <-
      function(image.matrix.1, image.matrix.2){image.matrix.1 + image.matrix.2}
    normalise.function =
      function(image.matrix){image.matrix/length(images.list)}
  } else if(collapsing=="max"){
    collapse.function <-
      function(image.matrix.1, image.matrix.2){
        pmax(image.matrix.1,image.matrix.2)
        }
    normalise.function =
      function(image.matrix){image.matrix}
  } else {stop("bad collapse method")}
  
  for(i in 1:length(images.list)){
    images.mean <- collapse.function(images.mean,images.list[[i]])
  }
  images.mean <- normalise.function(images.mean)
  return(images.mean)
}

write.collapsed <- function(path.input, 
                            exp.pattern, 
                            collapsing, 
                            path.output,
                            filename){
  tiffs.list <- list.files(path = path.input, pattern = exp.pattern)
  
  tiff.matrix.list <-
    foreach::foreach(tiff.filename = tiffs.list) %do% {
      tiff.matrix <- readTIFF(source = 
                                paste(path.input, tiff.filename, sep = "/"))
    }
  
  tiff.matrix <- getCollapseImageMatrix(images.list = tiff.matrix.list,
                                        collapsing = 'max')
  
  if(!dir.exists(paste(path.output))){
    dir.create(paste(path.output))
  }
  
  writeTIFFDefault(what = tiff.matrix,
                   where = paste(path.output, filename, sep="/"))
}

save.separate.well <- function(row.number, col.number, path.to.photos, z.range, 
                               active.input, no.cores, path.to.mapplate,
                               path.to.save, file.pattern, channel, 
                               to.projection = 1){
  

  
  well.list <- which.wells(paste(path.to.mapplate, "args_active.csv", sep=''), 
                           paste(path.to.mapplate, "args_id.csv", sep=''))
  
  for(i in 1:length(well.list)){
    lista <- merge.multiple.z(row.number = row.number, 
                              col.number = col.number, 
                              path.to.photos = path.to.photos,
                              z.range = z.range, 
                              subs.well = i,
                              file.pattern = file.pattern)
    if(!dir.exists(paste(path.to.save, "Well ", well.list[[i]], sep=''))){
      dir.create(paste(path.to.save, "Well ", well.list[[i]], sep=''))
    }
    
    if(to.projection){
      tiff.projection <- getCollapseImageMatrix(images.list = lista,
                                                collapsing = 'max')
      
      writeTIFFDefault(what = tiff.projection,
                       where = paste(path.to.save, "Well ", well.list[[i]], '/', 
                                     "MAX_",
                                     channel, 
                                     sprintf("%02d", z.range[1]),
                                     '-', 
                                     sprintf("%02d", tail(z.range, n=1)), 
                                     "_Z-stack_fish.tif", sep=''))
    }
    writeTIFFDefault(lista, paste(path.to.save, "Well ", well.list[[i]], '/', 
                                  channel, 
                                  sprintf("%02d", z.range[1]),
                                  '-', 
                                  sprintf("%02d", tail(z.range, n=1)), 
                                  "_Z-stack_fish.tif", sep=''))
  }
}


z.list <- function(total.z = 20, photos.number.after){
  # foo to easily divide a range of z slices to list of ranges
  # args : 
  #   total.z: total number of z slices (usually 20)
  #   photos.number.after: desired number of photos in one stack
  if(total.z == 0){
    return("no z slices?")
  }
  divider <- total.z / photos.number.after
  pre_z.list <- foreach(i=(0:(divider))) %do% {
    start <- i * photos.number.after
    stop <- (start + photos.number.after - 1)
    if(stop < total.z){
      seq(start, stop, by=1)
    } else if (start < total.z & stop >= total.z){
      seq(start, (total.z - 1), by=1)
    }
  }
  pre_z.list[sapply(pre_z.list, is.null)] <- NULL
  return(pre_z.list)
}

range.to.regex <- function(range){
  # foo to create a regex from list of z ranges for file removing 
  return(paste(sprintf("%02d", head(range, 1)), 
               "-", 
               sprintf("%02d", tail(range, 1)), sep=''))
}


#### final function to merge, collapse, make projection and remove photos ####
mcpr <- function(path.to.photos,
                 path.to.mapplate,
                 path.to.save,
                 path.to.fiji,
                 path.to.output = path.to.save,
                 channel.list = list("A488_"="02","A546_"="01", "DAPI_"="00"),
                 z.ranges,
                 basic.filename.leica  = 
                   "Mark_and_Find_001_Pos\\d{%d}_S001_z%02d_ch",
                 macro.invoke =
                   "ImageJ-win64.exe --console --headless -macro merge_large_tiff_.ijm",
                 row.number = 10,
                 col.number = 10,
                 to.projection = 1) {
  # Merge, Collapse, make Projection and Remove photos
  
  normalizeMetadata(metadata_path = path.to.mapplate)
  # creating the REGEX part of z ranges for file removing:
  z.ranges.regex <- 
    paste("(",
          paste(lapply(z.ranges, range.to.regex), collapse="|"),
          ")", sep='')
  
  # creating a combination of z-slice's name for final file naming:
  new.range <- paste(sprintf("%02d", z.ranges[[1]][1]), 
                     sprintf("%02d", tail(z.ranges[[length(z.ranges)]], 1)), 
                     sep='-')
  
  ## command for fiji script ##
  command.list <- list()
  
  command.list[["arguments"]] <- list()
  
  command.list[["app"]] <- macro.invoke
  
  command.list[["arguments"]][["input"]] <- 
    gsub("/", "\\\\", path.to.save)
  
  command.list[["arguments"]][["output"]] <- 
    gsub("/", "\\\\", path.to.output)
  
  command.list[["arguments"]][["dyes"]] <- 
    paste(names(channel.list), collapse=',')
  
  command.list[["arguments"]][["ranges"]] <- 
    paste(lapply(z.ranges, range.to.regex), collapse=",")
  
  command <- 
    paste(command.list[["app"]], paste(command.list[["arguments"]], 
                                       collapse = ';'), sep=' ')
  
  well.list <- which.wells(paste(path.to.mapplate, "args_active.csv", sep=''),
                           paste(path.to.mapplate, "args_id.csv", sep=''))
  
  if(!dir.exists(paste(path.to.save))){
    dir.create(paste(path.to.save), recursive = TRUE)
  }
  
  for(channel.tmp in names(channel.list)){
    # each channel has a unique file name 
    pattern.channel <- paste(basic.filename.leica,
                             channel.list[[channel.tmp]],
                             ".tif",
                             sep='')
    
    for(z in z.ranges){
      save.separate.well(row.number = row.number,
                         col.number = col.number,
                         path.to.photos = path.to.photos,
                         path.to.mapplate = path.to.mapplate,
                         path.to.save = path.to.save,
                         z.range = z,
                         no.cores = 1,
                         channel = channel.tmp,
                         file.pattern = pattern.channel,
                         to.projection = to.projection)
      
    }
    if(to.projection == 1){
    foreach(folder = well.list) %do% {
      # first remove all  finished photos from any previous computations
      file.remove(
        list.files(paste(path.to.save, "Well ", folder, sep=""),
                   pattern = paste("MAX_", 
                                   channel.tmp, 
                                   new.range, 
                                   "_Z-stack_fish.tif", 
                                   sep=''), 
                   full.names = 1)
      )
      
      write.collapsed(
        path.input = paste(path.to.save, "Well ", folder, sep=""),
        exp.pattern = paste("MAX_", 
                            channel.tmp, 
                            ".*_Z-stack_fish.tif", 
                            sep=''),
        collapsing='max',
        path.output = paste(path.to.save, 
                            "Well ", 
                            folder, 
                            sep=""),
        filename = paste("MAX_",
                         channel.tmp, 
                         new.range,
                         "_Z-stack_fish.tif",
                         sep='')
      )
      
      file.remove(list.files(paste(path.to.save, "Well ", folder, sep=""),
                             pattern = paste("MAX_", 
                                             channel.tmp, 
                                             z.ranges.regex, 
                                             "_Z-stack_fish.tif", 
                                             sep=''), 
                             full.names=1))
    } 
    }
  }
  
  # invoke fiji script:
  setwd(path.to.fiji)
  system(command)
  
  foreach(folder=well.list) %do% {
    file.remove(list.files(paste(path.to.save, "Well ", folder, sep=""),
                           pattern = paste("(", 
                                           paste(names(channel.list), 
                                                 collapse = "|"),
                                           ")", 
                                           z.ranges.regex, 
                                           "_Z-stack_fish.tif", sep=''),
                           full.names = 1))
  }
}

#### variables ####

# set paths to photos, mapplate, path where photos should be save, 
# to fiji app and path where save merged photos
path.to.photos <- 
  "E:/PT/RNA-FISH/analysis_raw/photos/2018-08-21-PT-FISH03/"

path.to.mapplate <- 
  "E:/PT/RNA-FISH/analysis_raw/platemap/2018-08-21-PT-FISH03/metadata/"

path.to.save <- 
  "E:/PT/RNA-FISH/analysis_FISH_output/2018-08-21-PT-FISH03/"

path.to.fiji <- 
  "E:/PT/Fiji.app"

# list of dyes used:
channel.list <- list("DAPI_"="00")

# list of ranges to be used:
z.ranges <- z.list(total.z = 30, photos.number.after = 5)

# beginning of the photo filename from leica
basic.filename.leica <- "Mark_and_Find_003_Pos\\d{%d}_S001_z%02d_ch"

# beginning of invoking command
macro.invoke <- "ImageJ-win64.exe --console --headless -macro merge_large_tiff_.ijm"


#### final call of mcpr function ####
mcpr(path.to.photos = path.to.photos,
     path.to.mapplate = path.to.mapplate,
     path.to.save = path.to.save,
     path.to.fiji = path.to.fiji,
     channel.list = channel.list,
     z.ranges = z.ranges,
     basic.filename.leica = basic.filename.leica,
     macro.invoke = macro.invoke,
     row.number = 10,
     col.number = 10,
     to.projection = 1)




