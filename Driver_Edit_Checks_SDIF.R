print(args)
reqPkgs = c("dplyr", "readxl", "openxlsx",  "summarytools", "haven", "tools")
if (length(setdiff(reqPkgs, rownames(installed.packages()))) > 0) {
	install.packages(
	  setdiff(reqPkgs, rownames(installed.packages())),
	  repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"
	)
}
for (pkg in reqPkgs) {
	library(pkg, character.only = TRUE)
}  

FOLDER <- file.path(".")
setwd(FOLDER)
source("Edit_Checks_SDIF.R")

file.copy(
  from = "../PIAAC-data-2023/result/CY2_Final_SDIF_LVA.csv",
  to = "CY2_FINAL_SDIF_LVA.CSV",
  overwrite = TRUE
)

COUNTRY <- "LVA"
COUNTRYNAME <- "Latvia"
COUNTRY_SDIF <- "CY2_FINAL_SDIF_LVA.CSV"
SAMPLE <- "SCREENER"
SAMPTYPE <- ""
   
sink(paste0("SDIF_checks_", COUNTRY,"_", SAMPLE, SAMPTYPE, ".Rout"))
    
print(paste0("PIAAC C2: SDIF CHECKS ON THE FILE ", basename(COUNTRY_SDIF), ", -- ", COUNTRYNAME, " -- ", SAMPLE))
CHK_SDIF(COUNTRY = COUNTRY, 
		 COUNTRYNAME = COUNTRYNAME, 
		 INPUTFILE = COUNTRY_SDIF, 
		 SAMPLE = SAMPLE)
sink()
    
