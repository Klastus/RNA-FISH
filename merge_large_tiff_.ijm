// Concatenate z-stack of a specific dyes from the 
// batch of directories inside the input directory

// below code returns an array of filenames of given dye of given ranges from one directory
// wrap it as function!!

function filenameListOneDir(input, ranges, dye){
	arrayOfNames = getFileList( input );
	toConcList = newArray();
	for(j=0; j < ranges.length; j++){
		for (i = 0; i < arrayOfNames.length; i++){
			idxDye = indexOf( arrayOfNames[i], dye );
			if ( idxDye >= 0 ){
				idxRange = indexOf( arrayOfNames[i], ranges[j] );
				if (idxRange >= 0){
					toConcList = Array.concat(toConcList, arrayOfNames[i]);	
				}
			}
		}
	}
	return toConcList;
}

// exact concatenating:
/*

input = "/Users/piotrt/Documents/IPPT_PT/ImageJ/scripting/images/";
output = "/Users/piotrt/Documents/IPPT_PT/ImageJ/scripting/output/";
DirList = listOfDirs(input);
DyeList = newArray("A488", "DAPI");
ranges = newArray("0-9", "10-19");
range1 = split(ranges[0], "-");
range2 = split(ranges[ranges.length-1], "-");
NewRange = range1[0] + "-" + range2[1];
dye = "A488"
 */

LenOfArgs = 4;

args = getArgument();
args = split(args, ";");


if(args.length != LenOfArgs && args.length < LenOfArgs){
	print("Not enough arguments");
	exit;
}

input = args[0];
output = args[1];
DyeList = split(args[2], ",");
ranges = split(args[3], ",");
range1 = split(ranges[0], "-");
range2 = split(ranges[ranges.length-1], "-");
NewRange = range1[0] + "-" + range2[1];

setBatchMode(true);
for( dye=0; dye < DyeList.length; dye++){
	MatchedFiles = filenameListOneDir(input, ranges, DyeList[dye]);
    for( image=0; image < MatchedFiles.length; image++){
		open(input + MatchedFiles[image]);
	}
	WorkingTitle = DyeList[dye] + NewRange + "_Z-stack_fish.tiff";
	ConcCommand = "all_open title = " + WorkingTitle;
	run("Concatenate...", ConcCommand);
	OutputPath = output + WorkingTitle;
	saveAs("Tiff", OutputPath);
	run("Close All");
	
}
setBatchMode(false);

exit
