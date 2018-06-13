#### merging photos ####

#### loading packages ####
packages.list <- list("tiff", "foreach", "doParallel")
sapply(packages.list, require, character.only = TRUE)
#### ####
# path <- "/Users/piotrt/Documents/IPPT_PT/FISH/KZ_17.05.2018/"
# path.to.photos <- "/Users/piotrt/Documents/IPPT_PT/FISH/KZ_17.05.2018/B2/"
# setwd(paste(path, "R", sep=''))

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
    return(matrix.biggest)
  }
  
  tiles.no <- row.number * col.number
  # file.pattern <- "Mark_and_Find_001_Pos\\d{3}_S001_z%02d_ch00.tif"
  
  # registerDoParallel(no.cores)
  image.list <- foreach(z=(z.range), .packages = "tiff") %do% {
    tiff.list <-  list.files(path = path.to.photos,
                             pattern = sprintf(file.pattern, z))[(((subs.well-1)*tiles.no)+1):(tiles.no*subs.well)]
    merge.one.z(tiff.list, row.number = row.number, col.number = col.number, path.to.photos)
  }
  # stopImplicitCluster()
  return(image.list)
}
  
save.separate.well <- function(row.number, col.number, path.to.photos, z.range, 
                               active.input, no.cores, path.to.mapplate, path.to.save,
                               file.pattern){
  
  well.list <- which.wells(paste(path.to.mapplate, "fish_active.csv", sep=''), 
                           paste(path.to.mapplate, "args_id.csv", sep=''))
  
  for(i in 1:length(well.list)){
    lista <- merge.multiple.z(row.number=row.number, col.number=col.number, 
                              path.to.photos=path.to.photos, z.range=z.range, 
                              subs.well = i, file.pattern = file.pattern)
    if(!dir.exists(paste(path.to.save, '/', well.list[[i]], sep=''))){
      dir.create(paste(path.to.save, '/', well.list[[i]], sep=''))
    }
    writeTIFFDefault(lista, paste(path.to.save, '/', well.list[[i]], '/', 
                                  z.range[1], '-', tail(z.range, n=1) ,"_Z-stack_fish.tif", sep=''))
  }
}



path.to.photos <- "L:/FISH/KZ_17.05.2018/D4-G4_photos/"
path.to.mapplate <- "L:/FISH/mapplate_D4-G4/"
normalizeMetadata(metadata_path = path.to.mapplate)
path.to.save <- "Y:/PiotrT/FISH"

save.separate.well(row.number = 10,
                   col.number = 10,
                   path.to.photos = path.to.photos,
                   path.to.mapplate = path.to.mapplate,
                   path.to.save = path.to.save,
                   z.range = (10:19),
                   no.cores = 20)
