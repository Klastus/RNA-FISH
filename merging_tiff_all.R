#### merging photos ####

#### loading packages ####
packages.list <- list("tiff", "foreach", "doParallel")
sapply(packages.list, require, character.only = TRUE)

#### previous functions ####
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


merge.multiple.z <- function(row.number, col.number, path.to.photos, 
                             z.range, subs.well, file.pattern){
  
  
  merge.one.z <- function(tiff.list, row.number, col.number, path.to.photos){
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
  # file.pattern <- "Mark_and_Find_001_Pos\\d{3}_S001_z%02d_ch00.tif"
  tiff.sublist <- list()
  # registerDoParallel(no.cores)
  image.list <- foreach(z=(z.range), .packages = "tiff") %do% {
    for(no.of.digits in c(3, 4, 5)){
      tiff.sublist[[no.of.digits]] <-  list.files(path = path.to.photos,
                                               pattern = sprintf(file.pattern, no.of.digits, z))
    }
    tiff.list <- do.call(c, tiff.sublist)[(((subs.well-1)*tiles.no)+1):(tiles.no*subs.well)]
    merge.one.z(tiff.list, row.number = row.number, col.number = col.number, path.to.photos)
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
      function(image.matrix.1, image.matrix.2){pmax(image.matrix.1,image.matrix.2)}
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
      tiff.matrix <- readTIFF(source = paste(path.input, tiff.filename, sep = "/"))
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
                               active.input, no.cores, path.to.mapplate, path.to.save,
                               file.pattern, channel, to.projection=0){
  
  well.list <- which.wells(paste(path.to.mapplate, "args_active.csv", sep=''), 
                           paste(path.to.mapplate, "args_id.csv", sep=''))
  
  for(i in 1:length(well.list)){
    lista <- merge.multiple.z(row.number=row.number, col.number=col.number, 
                              path.to.photos=path.to.photos, z.range=z.range, 
                              subs.well = i, file.pattern = file.pattern)
    if(!dir.exists(paste(path.to.save, "/Well ", well.list[[i]], sep=''))){
      dir.create(paste(path.to.save, "/Well ", well.list[[i]], sep=''))
    }
    
  if(to.projection){
    tiff.projection <- getCollapseImageMatrix(images.list = lista,
                                          collapsing = 'max')
    
    writeTIFFDefault(what = tiff.projection,
                     where = paste(path.to.save, "/Well ", well.list[[i]], '/', 
                     "MAX_",
                     channel, 
                     z.range[1], '-', tail(z.range, n=1),
                     "_Z-stack_fish.tif", sep=''))
  }
    writeTIFFDefault(lista, paste(path.to.save, "/Well ", well.list[[i]], '/', 
                                  channel, 
                                  z.range[1], '-', tail(z.range, n=1) , 
                                  "_Z-stack_fish.tif", sep=''))
  }
}


#### preparing merged photos ####

path.to.photos <- "//zmifp-nas1/Experiments/Pathway/RNA FISH/Experiments/KZ-FISH05-2018-07-11/"
path.to.mapplate <- "//zmifp-nas1/Experiments/Pathway/RNA FISH/karolina/platemap/2018-07-11-KZ-FISH05/metadata/"
path.to.mapplate <- "//zmifp-nas1/Experiments/Pathway/RNA FISH/karolina/platemap/2018-07-13-KZ-FISH06/metadata/"
normalizeMetadata(metadata_path = path.to.mapplate)
path.to.save <- "//zmifp-nas1/Experiments/Pathway/RNA FISH/karolina/merged/KZ-FISH05-merged"

# type the list of dye used:
channel.list <- list("A546_"="01", "DAPI_"="00")

for(channel.tmp in names(channel.list)){
  pattern.channel <- paste("Mark_and_Find_003_Pos\\d{%d}_S001_z%02d_ch",
                           channel.list[[channel.tmp]],
                           ".tif",
                           sep='')
  ## type the list of z combinations:
  for(z in list((0:9), (10:19))){
    save.separate.well(row.number = 10,
                       col.number = 10,
                       path.to.photos = path.to.photos,
                       path.to.mapplate = path.to.mapplate,
                       path.to.save = path.to.save,
                       z.range = z,
                       no.cores = 1,
                       channel = channel.tmp,
                       file.pattern = pattern.channel,
                       to.projection = 1)
    
  }
  well.list <- which.wells(paste(path.to.mapplate, "args_active.csv", sep=''), 
                           paste(path.to.mapplate, "args_id.csv", sep=''))
  
  foreach(folder=well.list) %do% {
    write.collapsed(path.input = paste(path.to.save, "/Well ", folder, sep=""),
                    exp.pattern = paste("MAX_", channel.tmp,".*_Z-stack_fish.tif", sep=''),
                    collapsing='max',
                    path.output = paste(path.to.photos, folder, sep="/"),
                    filename = paste("MAX_", 
                                     channel.tmp,
                                     "_all_stack_fish.tif"))
  }
  
}


# ### debugging ####
# path.to.save <- "//zmifp-nas1/Experiments/Pathway/RNA FISH/karolina/merged/KZ-FISH03-merged/Well F03"
# list.files(path = path.to.save,
#            pattern = paste("MAX_DAPI.*_Z-stack_fish.tif", sep=''))

