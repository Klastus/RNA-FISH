##### updating the middle positions information ####
## load all functions from the MAF_prepare_automatic.R script ##

# create args_active file with all wells turned on (marked as 1) ##
path <- paste("/Volumes/Experiments/Pathway/RNA_FISH/piotrek", 
              "/platemap/2018-08-18-KZ_automatic_test/metadata/", sep = '')

prepare.maf.file.middle(path.input = path, 
                        path.output = paste(path, "MAF_begin.maf", sep = ''),
                        x.grid = 1, y.grid = 1, 
                        filename.middle = "plate_begin_middle.csv",
                        middle = FALSE)

corner.well <- extract.coor(paste(path, 
                                  "/MAF_begin.maf", 
                                  sep = ''))
x <- 10
y <- 10
dif.x = 0.00011405
dif.y = 0.00011220
corner.well$X <- corner.well$X - ((x/2) * dif.x)
corner.well$Y <- corner.well$Y - ((y/2) * dif.y)

save.XY.csv(input.path = path,
            positions.from.maf = corner.well,
            outpath.file.XY = "plate_end.csv",
            outpath.file.Z = "fish_z_end.csv")
