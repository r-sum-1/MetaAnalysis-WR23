# ============================================================================================
# WARNING..... ONLY run this if you're sure you want to nuke and restart the project...
# ============================================================================================

NUKE_WHAT <- "\n

"

# set CWD
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# Set a message for the archiving... write this to a text file for the archiving
ARCHIVE_NOTES <- "
This archive and nuke corresponds to the Version 1 analysis of the meta-analysis. \n
The new analysis is now on V2 branch, and all Version 1 data is now archived accordingly \n
To generate this data again, use the `data` sheet in the data_amended.xlsx excel file.
"


fileConn <- file("ARCHIVE_NOTES.txt")
writeLines(ARCHIVE_NOTES, fileConn)
close(fileConn)

files2zip <- c(dir('Outputs', full.names = TRUE), 'Analysis.R', 'ARCHIVE_NOTES.txt')

zip(zipfile = paste0("Archive/Outputs_", Sys.Date()), files = files2zip)



# ======================= Now start the cleaning up =======================
if (file.exists("ARCHIVE_NOTES.txt")) {
  #Delete file if it exists
  file.remove("ARCHIVE_NOTES.txt")
}

#Nuke files in the Output Folder
nuke_list <- c(list.files(path = "Outputs/", full.names=TRUE, recursive = TRUE), "ARCHIVE_NOTES.txt")

for (f in nuke_list){file.remove(f)}
