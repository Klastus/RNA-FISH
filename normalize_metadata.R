### ###
### Normal;ize separators in metadata files ###
### ###


#### function ####
normalizeMetadata <- function(metadata_path, 
                              delimeter = ","){
  csv.list <- list.files(path = metadata_path, pattern = ".csv", recursive = TRUE, full.names = TRUE)
  
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
    write.table(csv.data, file = csv, sep = delimeter, row.names = FALSE, col.names = FALSE)
  }
}

normalizeMetadata(metadata_path = 
                    "/Volumes/Experiments/Pathway/m.komorowski/piotrek/mapplates/2017-07-05-PT21")



