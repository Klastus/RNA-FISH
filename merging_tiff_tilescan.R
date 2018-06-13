#### merging photos ####

#### loading packages ####
packages.list <- list("tiff", "foreach", "doParallel")
sapply(packages.list, require, character.only = TRUE)

#### functions ####
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


save.separate.well <- function(row.number, col.number, path.to.photos, z.range, 
                               active.input, no.cores, path.to.save,
                               file.pattern, reverse=0, channel){
  writeTIFFDefault <- function(what, where){
    writeTIFF(what = what, 
              where =  where,
              bits.per.sample = 16,
              compression = "none")
  }
  
  merge.multiple.z <- function(row.number, col.number, path.to.photos, 
                               z.range, file.pattern, reverse=0){
    
    
    merge.one.z <- function(tiff.list, row.number, col.number, path.to.photos, reverse=0){
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
          
          row.number.tmp <- row.number.tmp + 1
        } 
        if(row.number.tmp > row.number) {
          row.number.tmp <- 1
          if (is.na(matrix.biggest[1][1])){
            matrix.biggest <- matrix.big 
          } else {matrix.biggest <- cbind(matrix.big, matrix.biggest)}
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
    
    # registerDoParallel(no.cores)
    image.list <- foreach(z=(z.range), .packages = "tiff") %do% {
      tiff.list <-  list.files(path = path.to.photos,
                               pattern = sprintf(file.pattern, z))
      merge.one.z(tiff.list, row.number = row.number, col.number = col.number, path.to.photos)
      
    }
    gc()
    # stopImplicitCluster()
    return(image.list)
  }
  
  
  well.list <- list.dirs(path=path.to.photos, recursive = FALSE, full.names = FALSE)
  foreach(i=1:length(well.list), .packages = c("tiff", "foreach")) %do% {
    lista <- merge.multiple.z(row.number=row.number, col.number=col.number, 
                              path.to.photos=paste(path.to.photos, well.list[[i]], sep=''), 
                              z.range=z.range, file.pattern = file.pattern)
    gc()
    if(!dir.exists(paste(path.to.save, well.list[[i]], sep=''))){
      dir.create(paste(path.to.save, well.list[[i]], sep=''))
    }
    writeTIFFDefault(lista, paste(path.to.save, well.list[[i]], '/', 
                                  z.range[1], '-', tail(z.range, n=1), "_", 
                                  channel, "_Z-stack_fish.tif", sep=''))
  }
}


#### running ####
path.to.photos <- "M:/KZ/RNA-FISH/RNA FISH 30.05.18/Experiment_B10/"


path.to.save <- "M:/KZ/RNA-FISH/reverse1/"
file.pattern <- "*s\\d{2}_z%02d_ch01.tif"

save.separate.well(row.number = 10,
                   col.number = 3,
                   path.to.photos = path.to.photos,
                   path.to.save = path.to.save,
                   z.range = (7:13),
                   no.cores = 4,
                   file.pattern = file.pattern,
                   channel="DAPI")

file.pattern <- "*s\\d{2}_z%02d_ch00.tif"


save.separate.well(row.number = 10,
                   col.number = 3,
                   path.to.photos = path.to.photos,
                   path.to.save = path.to.save,
                   z.range = (7:13),
                   no.cores = 4,
                   file.pattern = file.pattern,
                   channel="A546")

# if(reverse==0){ 
#   matrix.biggest <- cbind(matrix.biggest, matrix.big)
# } else if(reverse==1)


