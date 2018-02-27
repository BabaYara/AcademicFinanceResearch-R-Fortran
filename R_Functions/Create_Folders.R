# This script helps you quickly create folders for holding documents that should be stored in separate folders. 

library(stringr)
path = "D:/Dropbox/Asset Mgmt/2018/Grading_Related/Submissions/Email/"                   # Path to where you want Folders created. 

for(i in 1:37){
  
  path02 = print(paste("Group ", str_pad(paste(i), 2, "left", "0"),  sep = ""))          # Naming convention per folder
  path03 = print(paste(path, path02, sep=""))                                            # creating path for creating folder
  dir.create(path03)                                                                     # Saving Folder
}
