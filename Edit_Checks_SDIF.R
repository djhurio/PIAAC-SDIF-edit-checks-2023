CHK_SDIF = function(
  COUNTRY     
  ,COUNTRYNAME 
  ,INPUTFILE
  ,SAMPLE      
  ,CHECK = 1
  ,ProbVars 
  ,CAP
)
{
  "/" <- function(x,y) ifelse(y == 0, NaN, base:::"/"(x,y))
  `%>%` <- dplyr::`%>%`
  
  curdir = getwd()
  on.exit(setwd(curdir))

  options(tibble.print_max = Inf, tibble.width = Inf, width = 220, max.print = 10000, scipen = 14)
  
  allMiss = function(dsn, cnames, title="")
  {
    if (length(setdiff(cnames,colnames(dsn))) == 0) {
      cnames = intersect(colnames(dsn), cnames)
      res = sum(colSums(is.na(dsn[cnames])) == nrow(dsn[cnames]), na.rm = T) == length(cnames)
      if (res) {
        if (title != "") {
          cat(paste0(title, "\n"))
        }
      }
      return(res)
    } else {
      if (title != "") {
        cat(paste0(title, "\n"))
        print(paste0(paste(setdiff(cnames,colnames(dsn)), collapse = " "), ": NOT IN THE FILE."))
      } 
      return(TRUE)
    }
  }
  
  decimalplaces <- function(x) {
    y = x[!is.na(x)]
    if (length(y) == 0) {
      return(0)
    }
    if (any((y %% 1) != 0)) {
      info = strsplit(sub('0+$', '', as.character(y)), ".", fixed=TRUE)
      info = info[sapply(info, FUN=length) == 2]
      dec = nchar(unlist(info))[seq(2, length(unlist(info)), 2)]
      return(max(dec, na.rm=T))
    } else {
      return(0)
    }
  }
  
  printCond = function(dsn, cnames, stitle="", sfilter="") {
    cnames_old = cnames
    cnames = unlist(strsplit(cnames, ","))
    diffnames = setdiff(cnames, colnames(dsn))
    cnames = intersect(colnames(dsn), cnames)
    if (length(cnames) > 0) {
      if (sfilter != "") {
        dsn <- dsn %>%
          filter(eval(parse(text = sfilter)))
      }  
      if (nrow(dsn) > 0) {
        if (stitle != "") {
          print(stitle)
        }
        print(dsn %>% select(unlist(strsplit(cnames_old, ","))))
        writeLines("")
      } 
      
    }
    if (length(diffnames) > 0) {
      cat(paste0(diffnames, " are not in the FILE\n"))
    }
  }
  
  univ = function(dsn, cnames, title="", univDec=FALSE, univTitle=NULL, printMiss=0)
  {
    #summary(dsn[cnames], quantile.type=3)
    #if(univDec && !allMiss(dsn, cnames))
    cnames_old = cnames
    cnames = intersect(colnames(dsn), cnames)
    lastVar <- dplyr::last(cnames)
    if (length(cnames) != 0) {
      if (univDec) {
        print(paste0("Check: ", lastVar, " should have 12 decimal places."))
        if (length(intersect(lastVar, ProbVars)) == 0) {
          if (nrow(dsn) == sum(is.na(dsn[lastVar]))) {
            if (!is.null(univTitle)) print(univTitle)    
          }
        } else {
          dec = decimalplaces(dsn[lastVar])
          if (dec<12) {
            print(paste0("Check: ", lastVar, " should be provided with full precision (up to 12 decimal places). Please check that ", lastVar, " has not been rounded."))
            print(paste0(lastVar, " HAS ", dec, " DECIMAL PLACES. Please correct it if the values are rounded."))
          } else {
            print(paste0(lastVar, " HAS ", dec, " DECIMAL PLACES."))
          }
            
          writeLines("")     
        }
      }  
      if (nrow(dsn) == sum(is.na(dsn[lastVar]))) {
          if (!is.null(univTitle)) {
            print(univTitle)    
          } else {
            print(paste0("Check: All of ", lastVar, " is missing"))
            if (printMiss) {
              vfreq(dsn, lastVar)
            }
          }
      } else {
        if (title != "") {
          print(title)
        }
        print(descr(dsn[cnames], round.digits = 8, headings = FALSE))
      } 
        
      #print(psych::describe(dsn[cnames]))
      #print(quantile(dsn[cnames], type=3, na.rm=TRUE, probs=c(1, .99, .95, .9, .75, .5, .25,.1, .05, .1, 0)))
    } else {
      if (title != "") {
        cat(paste0(title, "\n"))
      }
      print(paste0(paste(cnames_old, collapse = " "),  ": NOT IN THE FILE.")) 
    }
    
  }
  
  dupRec = function(dsn, cnames, combUniq=FALSE, dupMsg="", dupTit="", dupMissTit="", chkLastMiss=TRUE, filter="", lnames=NULL)
  {
    #browser()
    lastName <- last(cnames)
    lastMissCnt <- 0
    
    if (length(setdiff(cnames, colnames(dsn))) == 0) {
      cnames = intersect(cnames, colnames(dsn))
      lnames = intersect(lnames, colnames(dsn))
      misdsn = dsn
      lastExist = length(intersect(lastName, cnames)) > 0
      
      if (!allMiss(dsn, cnames)) {
  		  doRun = TRUE

  		  if (chkLastMiss) {
  		    if (lastExist) {
      			if (allMiss(dsn, last(cnames))) {
      				doRun = FALSE
      			}
  		      lastMissCnt <- nrow(dsn %>% filter(eval(parse(text = paste0("is.na(", lastName, ")")))))
  		    } else {
  		      doRun = FALSE
  		    }
  		  }
  
  		  if (doRun) {
  			  if (dupTit != "") {
  				  print(dupTit)
  			  }
  		    if (filter != "") {
  		      dsn <- dsn %>% 
  		        dplyr::filter(eval(parse(text = filter)))
  		    }
  		    if (combUniq) {
  		      selnames <- union(lnames, cnames)
  				  dsn <- dsn %>% 
  				    select(all_of(selnames)) %>%
  				    distinct(across(all_of(cnames)), .keep_all = T)
  			  }

  		    cnames1 <- cnames 
  		    if (combUniq) {
  		      cnames1 <- cnames[-length(cnames)]
  		    }
  			  if (nrow(unique(dsn[cnames1])) == nrow(dsn)) {
    				if (!combUniq) {
    				  #ifelse(dupMsg != "", print(dupMsg), print(paste0(paste(cnames, collapse = " "), " Check - No duplicates")))
    				  if (cnames == "PERSID") {
    				    cat("PERSID Check - No duplicates\n")
    				  } else {
    				    cat(paste0("Total number of unique ", cnames, " records = ", nrow(dsn), " and there are no duplicates.\n"))
    				  }
    				} else {
    				  print(dupMsg) 
    				}
  			  } else {
     				if (!combUniq)
    				{
    				  cat(paste0(paste(cnames, collapse = " "), " Check - There are duplicates"))
    				}
  			    if (is.null(lnames)) {
  			      cat("Check: These are duplicates in the file\n")
			        print(as.data.frame(dsn %>% 
			                              group_by_at(cnames1) %>% 
			                              filter(n() > 1) %>% 
			                              select(all_of(cnames))))
  			    } else {
  			      cat("Please check these cases\n")
  			      print(as.data.frame(dsn %>% 
  			                            group_by_at(cnames1) %>% 
  			                            filter(n() > 1) %>% 
  			                            select(all_of(lnames))))
  			    }
   			  }
  		  }
  		  if (dupMissTit != "") {
  		    if (doRun) writeLines("")
  		    if (lastExist) fmtCols(misdsn, last(cnames), dupMissTit)
  		  } else {
    			if (chkLastMiss && lastExist) {
    			  if (lastMissCnt > 0 ) {
    			    cat(paste0("\nThe number of cases where ", lastName, " is missing: ", lastMissCnt, "\n"))
    			    #print(as.data.frame(dsn %>% select(lastName) %>% filter(eval(parse(text = paste0("is.na(", lastName, ")"))))))
    			  } else {
    			    cat("No missing values\n")
    			    #vfreq(dsn, last(cnames), paste0("Check on ", last(cnames), " - all missing"), runFreq = TRUE)
    			  }
    			}
  		  }
      } else {
        if(lastExist) vfreq(dsn, last(cnames), paste0("Check on ", last(cnames), " - all missing"), runFreq = TRUE)
      }
  	} else {
  		ifelse(dupTit != "", print(dupTit), print(paste0("Check on ", paste0(cnames, collapse = " "), collapse = "")))
  		print(paste0(paste(setdiff(cnames, colnames(dsn)), collapse = " "), ": NOT IN THE FILE.")) 
  		if (dupMissTit != "" && length(intersect(last(cnames), colnames(dsn))) > 0) {
  		  fmtCols(misdsn, last(cnames), dupMissTit)
  		}
  	}
  }
  
  vmean = function(dsn, cnames, title="", vtype=3, mtype=1) {
    cnames = intersect(colnames(dsn), cnames)
    if (mtype == 1) {
      t = rbind(rep(nrow(dsn), length(cnames)) - colSums(is.na(dsn[cnames])), colSums(is.na(dsn[cnames])), apply(dsn[cnames], 2, quantile , type = vtype, probs = c(0,.1,.25,.50) , na.rm = TRUE, names = TRUE, digits = 8), colMeans(dsn[cnames], na.rm = TRUE), apply(dsn[cnames], 2, quantile , type = vtype, probs = c(.75,.90,.95,1) , na.rm = TRUE, names = TRUE, digits = 8 ))
      rownames(t) <- c("N", "N Miss", "Minimum", "10th Pctl", "25th Pctl", "50th Pctl", "Mean", "75th Pctl", "90th Pctl", "95th Pctl",  "Maximum")
    }
    if (mtype == 2) {
      t = rbind(rep(nrow(dsn), length(cnames)) - colSums(is.na(dsn[cnames])), colSums(is.na(dsn[cnames])), apply(dsn[cnames], 2, quantile , type = vtype, probs = c(0) , na.rm = TRUE, names = TRUE, digits = 8), colMeans(dsn[cnames], na.rm = TRUE), apply(dsn[cnames], 2, quantile , type = vtype, probs = c(.50,1) , na.rm = TRUE, names = TRUE, digits = 8 ))
      rownames(t) <- c("N", "N Miss", "Minimum", "Mean", "Median", "Maximum")      
    }
    if (mtype == 3) {
      t = rbind(rep(nrow(dsn), length(cnames)) - colSums(is.na(dsn[cnames])), colSums(is.na(dsn[cnames])), apply(dsn[cnames], 2, quantile , type = vtype, probs = c(0) , na.rm = TRUE, names = TRUE, digits = 8), colMeans(dsn[cnames], na.rm = TRUE), apply(dsn[cnames], 2, quantile , type = vtype, probs = c(.50,1) , na.rm = TRUE, names = TRUE, digits = 8 ), colSums(dsn[cnames], na.rm = TRUE))
      rownames(t) <- c("N", "N Miss", "Minimum", "Mean", "Median", "Maximum", "Sum")      
    }
    
    if (title != "") {
      cat(paste0(title, "\n"))
    }
    print(t(t))
    writeLines("")
  }
  
  fmtCols = function(dsn, cnames, title="", tMiss=FALSE, missTitle="", ftibble=F, filter="", fmtnames=NULL)
  {
    #browser()
    if(length(setdiff(cnames, colnames(dsn)))==0){
      if (tMiss)
      {
        if (allMiss(dsn, last(cnames)))
        {
          print(missTitle)
        }else
        {
          cat(paste0(title, "\n"))
        }
      }else
      {
        if (title != "")
        {
          cat(paste0(title, "\n"))
        }else
        {
          print(paste0(cnames[1], " and ", cnames[2],  " - Verify that this agrees with your design"))
        }
        
      }
      if (filter != "") {
        dsn <- dsn %>% 
          filter(eval(parse(text = filter)))
      }
      if (is.null(fmtnames)) {
        fmtResults = as.data.frame(sapply(dsn[cnames], function(x) as.factor(ifelse(is.na(x), "M", "N")))) %>% group_by_at(all_of(cnames)) %>%
          summarise(COUNT = n()) %>%
          ungroup() %>%
          add_tally(wt=COUNT) %>%
          mutate(PERCENT = COUNT/n * 100, CUMTOTAL=cumsum(COUNT),  CUMPERCENT = cumsum(PERCENT))
      } else {
        diffnames = setdiff(cnames, fmtnames)
        fmtResults = cbind(dsn[diffnames], as.data.frame(sapply(dsn[fmtnames], function(x) as.factor(ifelse(is.na(x), "M", "N"))))) %>% 
          group_by_at(all_of(cnames)) %>%
          summarise(COUNT = n()) %>%
          ungroup() %>%
          add_tally(wt=COUNT) %>%
          mutate(PERCENT = COUNT/n * 100, CUMTOTAL=cumsum(COUNT),  CUMPERCENT = cumsum(PERCENT))
      }
      fmtResults <- fmtResults[, -which(names(fmtResults) %in% c("n"))]
      if(!ftibble)
      {
        print(as.data.frame(fmtResults))
      }else
      {
        print(fmtResults)
      }
      
    }else
    {
      if(title != ""){
        cat(paste0(title, "\n"))
      }else
      {
        if(missTitle!="") print(missTitle)
      } 
      if(title=="" && missTitle==""){
        print(paste0(cnames[1], " and ", cnames[2],  " - Verify that this agrees with your design"))
      }
      print(paste0(paste(setdiff(cnames, colnames(dsn)), collapse = " "), ": NOT IN THE FILE."))
    }
  }
  
  vfreq = function(dsn, cnames, title="", freq1=FALSE, runFreq=FALSE, ftibble=FALSE, filter="", misTitle="")
  {
    if (typeof(cnames) != "list") {
      lcnames = list(cnames)
    } else {
      lcnames = cnames
    }
    if (filter != "") {
      #print(filter)
      dsn = dsn %>% filter(eval(parse(text = filter)))
    }        
    if (nrow(dsn) == 0) {
      if (misTitle != "") {
        if (title != "") {
          print(title)
        } 
        print(misTitle)
        cat("\n")
      }
      #else print("There is no data")
    } else {
      
      for (l in 1:length(lcnames)) {
        cnames = lcnames[[l]]
        if (length(setdiff(cnames, colnames(dsn))) == 0) {
          if (typeof(title) == "list") {
            cat(paste0(title[[l]]), "\n")
          } else {
            if (title != "") {
              cat(paste0(title, "\n"))
            }
          }
          if (colSums(is.na(dsn[last(cnames)]))[1] == nrow(dsn)) {
            if (runFreq) {
              # resFreq = as.data.frame(dsn[cnames]) %>% group_by(.dots=cnames) %>%
              #   summarise(COUNT = n()) %>%
              #   ungroup() %>%
              #   mutate(PERCENT = COUNT/sum(COUNT) * 100, CUMTOTAL=cumsum(COUNT),  CUMPERCENT = cumsum(PERCENT))
              resFreq = as.data.frame(dsn[cnames]) %>% 
                group_by_at(all_of(cnames)) %>%
                summarise(COUNT = n()) %>%
                ungroup() %>%
                add_tally(wt = COUNT) %>%
                mutate(PERCENT = COUNT / n * 100, CUMTOTAL = cumsum(COUNT),  CUMPERCENT = cumsum(PERCENT))
              resFreq = resFreq[, -which(names(resFreq) %in% c("n"))]
              if (!ftibble) {
                print(as.data.frame(resFreq))    
              } else {
                print(resFreq)    
              }
            } else {
              print(paste0("All the values of ", last(cnames), " are missing"))
            }
          } else {
            if (length(cnames) > 0 ) {
              resFreq = as.data.frame(dsn[cnames]) %>% 
                group_by_at(all_of(cnames)) %>%
                summarise(COUNT = n()) %>%
                ungroup() %>%
                add_tally(wt = COUNT) %>%
                mutate(PERCENT = COUNT / n * 100, CUMTOTAL = cumsum(COUNT),  CUMPERCENT = cumsum(PERCENT))
              resFreq = resFreq[, -which(names(resFreq) %in% c("n"))]
            } else {
              resFreq = freq(dsn[cnames], style = "grid", report.nas = TRUE)
              resFreq = subset(resFreq, select = -c(2,3))
            }
            if (!ftibble) {
              print(as.data.frame(resFreq))
            } else {
              print(resFreq)
            }
            
            if (freq1) {
              if (is.data.frame(resFreq)) {
                valRows = nrow(resFreq)
              } else {
                valRows = nrow(resFreq) - 1
                if (resFreq["<NA>",1] == 0) {
                  valRows = nrow(resFreq) - 2
                }
              }
              
              if (valRows > 1) {
                print(paste0(valRows, " VALUES FOR ", cnames, ", MORE THAN ONE ..."))
                
              }
            }
          }
        } else {
          print(paste0("Check: ", paste(cnames, collapse = "*")) )
          print(paste0(paste(setdiff(cnames, colnames(dsn)), collapse = " "), ": NOT IN THE FILE."))
        }
        cat("\n")
      }  
        
    }
  }
  
  countN = function(dsn, cnames, countfreq=TRUE, cntTitle="", cntMissTitle="", filter="")
  {
    if (length(setdiff(cnames, colnames(dsn))) == 0) {
      
      lastvar = last(cnames)
      if (allMiss(dsn, lastvar))
      {
        if (countfreq)
        {
          #print(fmtCols(dsn, lastvar, paste0("Check on ", lastvar, " - all missing")))
          if (filter != "")
          {
            #vfreq(dsn, lastvar, paste0("Check on ", lastvar), runFreq = TRUE)
            fmtCols(dsn, lastvar, paste0("Check on ", lastvar))
          }else
          {
            vfreq(dsn, lastvar, paste0("Check on ", lastvar, " - all missing"), runFreq = TRUE)
          }
        }
        if (cntMissTitle != "")
        {
          print(cntMissTitle)
        }
        
      } else {
        if (cntTitle != "") {
          print(cntTitle)
        }
        if (length(setdiff(cnames, colnames(dsn))) == 0) {
          #print(paste0("Total number of unique ", cnames, " = ", nrow(unique(!is.na(dsn[cnames])))))
          if (filter != "")
          {
            dsn = dsn %>% filter(eval(parse(text = filter)))
            #print(dsn)
          }
          
          print(paste0("Total number of unique ", paste(cnames, collapse = ","), " = ", nrow(unique(dsn[cnames]))))
        }else
        {
          
          print(paste0(paste(setdiff(cnames, colnames(dsn)), collapse = " "), 
                       ifelse(length(setdiff(cnames, colnames(dsn))) == 1, " doesn't exist"," don't exist")))
        }
      }
    }else
    {
      ifelse(cntTitle != "", print(cntTitle), print(paste0("Check on ", paste0(cnames, collapse = " "), collapse = "")))
      print(paste0(paste(setdiff(cnames, colnames(dsn)), collapse = " "), ": NOT IN THE FILE."))
    }
      
  }
  
  toread <- "name,dtype
CNTRYID,numeric
CNTRY,character
CASEID,numeric
PERSID,numeric
ID_MAJDES,numeric
ID_PSU,numeric
ID_SSU,numeric
ID_HH,numeric
ADD_DU,numeric
SUBSAMP,numeric
SAMPTYPE,numeric
QCFLAG_ASSIGN,numeric
SMPFLG1,numeric
SMPFLG2,numeric
SMPFLG3,numeric
EXCFRM_PROP,numeric
REGION,numeric
URBRUR,numeric
PROB_PSU,numeric
PROB_SSU,numeric
PROB_HH,numeric
PROB_SMPFLG1,numeric
PROB_SMPFLG2,numeric
PROB_SMPFLG3,numeric
STRAT_PSU,numeric
STRAT_SSU,numeric
STRAT_HH,numeric
SORT_PSU,numeric
SORT_SSU,numeric
SORT_HH,numeric
ID_OTH,numeric
PROB_OTH,numeric
PROB_PERS,numeric
STRAT_PERS,numeric
SORT_PERS,numeric
INTVID,numeric
QCFLAG,numeric
QCREC,numeric
NUMELG1,numeric
NUMSEL1,numeric
NUMELG2,numeric
NUMSEL2,numeric
NUMELG3,numeric
NUMSEL3,numeric
SCQAGE,numeric
SCQAGERANGE,numeric
CI_AGE,numeric
CI_GENDER,numeric
DISP_SCR,character
V_AGE,numeric
V_GENDER,numeric
DISP_DS,character
DISP_CIBQ,character
DISP_TUT,character
DISP_LOC,character
DISP_CMP,character
DISP_CBA,character
ATTMPTTUT,numeric
ATTMPTLOC,numeric
RSLTLOC,numeric
ATTMPTCMP,numeric
ATTMPTCBA,numeric
CURBOBS1,numeric
CURBOBS2_01,numeric
CURBOBS2_02,numeric
CURBOBS2_03,numeric
CURBOBS2_04,numeric
CURBOBS3,numeric
ZZ1A_01,numeric
ZZ1A_02,numeric
ZZ1A_03,numeric
ZZ1B_01,numeric
ZZ1B_02,numeric
ZZ1B_03,numeric
ZZ2,numeric
ZZ3,numeric
ZZ4,numeric
ZZ5,numeric
ZZ6_01,numeric
ZZ6_02,numeric
ZZ6_03,numeric
ZZ6_04,numeric
ZZ6_05,numeric
ZZ6_06,numeric
ZZ6_07,numeric
ZZ7,numeric
ZZ8,numeric
ZZ9,numeric
CONTACTTIME1,numeric
CONTACTTIME2,numeric
CONTACTTIME3,numeric
CONTACTTOTAL,numeric
ENDSCR,character
PROB_OVERALL_HH,numeric
PROB_OVERALL_PERS,numeric
THEOR_HBWT,numeric
THEOR_PBWT,numeric
DOBYY,numeric
DOBMM,numeric
CALCAGE,numeric
DIAGERANGE,numeric
AGE_R,numeric
IMPFLGAGER,numeric
GENDER,numeric
GENDER_R,numeric
DISP_MAIN,character
COMPLETEFLG,numeric
WEIGHTFLG,numeric
EXCFLG,numeric
REGFLG,numeric
TECHPROB,numeric
TRIMGRPS,numeric
RAKEDIM1,numeric
RAKEDIM2,numeric
RAKEDIM3,numeric
RAKEDIM4,numeric
RAKEDIM5,numeric
RAKEDIM6,numeric
RAKEDIM7,numeric
PERSVAR1,numeric
PERSVAR2,numeric
PERSVAR3,numeric
PERSVAR4,numeric
PERSVAR5,numeric
DUVAR_SCRRESP1,numeric
DUVAR_SCRRESP2,numeric
DUVAR_SCRRESP3,numeric
DUVAR_SCRRESP4,numeric
DUVAR_SCRRESP5,numeric
DUVAR_ALL1,numeric
DUVAR_ALL2,numeric
DUVAR_ALL3,numeric
DUVAR_ALL4,numeric
DUVAR_ALL5,numeric
AREAVAR1,numeric
AREAVAR2,numeric
AREAVAR3,numeric
AREAVAR4,numeric
AREAVAR5,numeric
IMPFLGAG,numeric
IMPFLGGE,numeric
IFLG_PERSVAR1,numeric
IFLG_PERSVAR2,numeric
IFLG_PERSVAR3,numeric
IFLG_PERSVAR4,numeric
IFLG_PERSVAR5,numeric
IFLG_DUVAR_SCRRESP1,numeric
IFLG_DUVAR_SCRRESP2,numeric
IFLG_DUVAR_SCRRESP3,numeric
IFLG_DUVAR_SCRRESP4,numeric
IFLG_DUVAR_SCRRESP5,numeric
IFLG_DUVAR_ALL1,numeric
IFLG_DUVAR_ALL2,numeric
IFLG_DUVAR_ALL3,numeric
IFLG_DUVAR_ALL4,numeric
IFLG_DUVAR_ALL5,numeric
IFLG_CURBOBS1,numeric
IFLG_CURBOBS2,numeric
IFLG_CURBOBS3,numeric
IFLG_AREAVAR1,numeric
IFLG_AREAVAR2,numeric
IFLG_AREAVAR3,numeric
IFLG_AREAVAR4,numeric
IFLG_AREAVAR5,numeric
IFLG_RAKEDIM1,numeric
IFLG_RAKEDIM2,numeric
IFLG_RAKEDIM3,numeric
IFLG_RAKEDIM4,numeric
IFLG_RAKEDIM5,numeric
IFLG_RAKEDIM6,numeric
IFLG_RAKEDIM7,numeric"
  
  #convert all the parameters to upper case
  INPUTFILE = toupper(INPUTFILE)
  SAMPLE = toupper(SAMPLE)
  COUNTRY = toupper(COUNTRY)
  
  FULLSDIF = read.table(textConnection(toread), header=TRUE, sep=",", stringsAsFactors=FALSE)
  
  PATH_DATA = dirname(INPUTFILE) 
  DATA_NAME = basename(INPUTFILE)
  FILE_EXT = file_ext(INPUTFILE)
  if (missing(ProbVars)) {
    ProbVars <- ""
    if (COUNTRY == "AUT") {
      ProbVars <- unlist(strsplit("PROB_PERS PROB_OTH", " "))
    }
    if (COUNTRY == "BEL") {
      ProbVars <- unlist(strsplit("PROB_PERS", " "))
    }
    if (COUNTRY == "CAN") {
      ProbVars <- unlist(strsplit("PROB_PSU PROB_HH PROB_PERS", " "))
    }
    if (COUNTRY == "CHL") {
      ProbVars <- unlist(strsplit("PROB_PSU PROB_SSU PROB_HH PROB_PERS", " "))
    }
    if (COUNTRY == "HRV") {
      ProbVars <- unlist(strsplit("PROB_PSU PROB_PERS", " "))
    }
    if (COUNTRY == "CZE") {
      ProbVars <- unlist(strsplit("PROB_PSU PROB_HH PROB_PERS", " "))
    }
    if (COUNTRY == "DNK") {
      ProbVars <- unlist(strsplit("PROB_PERS", " "))
    }
    if (COUNTRY == "EST") {
      ProbVars <- unlist(strsplit("PROB_PERS", " "))
    }
    if (COUNTRY == "FIN") {
      ProbVars <- unlist(strsplit("PROB_PERS", " "))
    }
    if (COUNTRY == "FRA") {
      ProbVars <- unlist(strsplit("PROB_PSU PROB_PERS", " "))
    }
    if (COUNTRY == "DEU") {
      ProbVars <- unlist(strsplit("PROB_PSU PROB_PERS", " "))
    }
    if (COUNTRY == "HUN") {
      ProbVars <- unlist(strsplit("PROB_PSU PROB_PERS", " "))
    }
    if (COUNTRY == "IRL") {
      ProbVars <- unlist(strsplit("PROB_PSU PROB_HH PROB_PERS", " "))
    }
    if (COUNTRY == "ISR") {
      ProbVars <- unlist(strsplit("PROB_PSU PROB_PERS", " "))
    }
    if (COUNTRY == "ITA") {
      ProbVars <- unlist(strsplit("PROB_PSU PROB_PERS", " "))
    }
    if (COUNTRY == "JPN") {
      ProbVars <- unlist(strsplit("PROB_PSU PROB_PERS", " "))
    }
    if (COUNTRY == "KOR") {
      ProbVars <- unlist(strsplit("PROB_PSU PROB_HH PROB_PERS", " "))
    }
    if (COUNTRY == "LVA") {
      ProbVars <- unlist(strsplit("PROB_PSU PROB_HH PROB_PERS", " "))
    }
    if (COUNTRY == "LTU") {
      ProbVars <- unlist(strsplit("PROB_PSU PROB_SSU PROB_HH PROB_PERS", " "))
    }
    if (COUNTRY == "NLD") {
      ProbVars <- unlist(strsplit("PROB_PERS", " "))
    }
    if (COUNTRY == "NZL") {
      ProbVars <- unlist(strsplit("PROB_PSU PROB_HH PROB_PERS", " "))
    }
    if (COUNTRY == "NOR") {
      ProbVars <- unlist(strsplit("PROB_PERS", " "))
    }
    if (COUNTRY == "POL") {
      ProbVars <- unlist(strsplit("PROB_PSU PROB_PERS", " "))
    }
    if (COUNTRY == "PRT") {
      ProbVars <- unlist(strsplit("PROB_PSU PROB_SSU PROB_HH PROB_PERS", " "))
    }
    if (COUNTRY == "SGP") {
      ProbVars <- unlist(strsplit("PROB_PERS", " "))
    }
    if (COUNTRY == "SVK") {
      ProbVars <- unlist(strsplit("PROB_PSU PROB_HH PROB_PERS", " "))
    }
    if (COUNTRY == "ESP") {
      ProbVars <- unlist(strsplit("PROB_PSU PROB_HH PROB_PERS", " "))
    }
    if (COUNTRY == "SWE") {
      ProbVars <- unlist(strsplit("PROB_PERS", " "))
    }
    if (COUNTRY == "CHE") {
      ProbVars <- unlist(strsplit("PROB_PERS", " "))
    }
    if (COUNTRY == "GBR") {
      ProbVars <- unlist(strsplit("PROB_PSU PROB_HH PROB_PERS", " "))
    }
    if (COUNTRY == "USA") {
      ProbVars <- unlist(strsplit("PROB_PSU PROB_SSU PROB_HH PROB_PERS", " "))
    }
  }  
  if (missing(CAP)) {
    CAP <- as.numeric(NA)
    if (COUNTRY == "AUT") {
      CAP <- 100
    }
    if (COUNTRY == "BEL") {
      CAP <- 100
    }
    if (COUNTRY == "CAN") {
      CAP <- 250
    }
    if (COUNTRY == "CHL") {
      CAP <- 125
    }
    if (COUNTRY == "HRV") {
      CAP <- 115
    }
    if (COUNTRY == "CZE") {
      CAP <- 125
    }
    if (COUNTRY == "DNK") {
      CAP <- 100
    }
    if (COUNTRY == "EST") {
      CAP <- 100
    }
    if (COUNTRY == "FIN") {
      CAP <- 100
    }
    if (COUNTRY == "FRA") {
      CAP <- 115
    }
    if (COUNTRY == "DEU") {
      CAP <- 115
    }
    if (COUNTRY == "HUN") {
      CAP <- 115
    }
    if (COUNTRY == "IRL") {
      CAP <- 125
    }
    if (COUNTRY == "ISR") {
      CAP <- 100
    }
    if (COUNTRY == "ITA") {
      CAP <- 115
    }
    if (COUNTRY == "JPN") {
      CAP <- 115
    }
    if (COUNTRY == "KOR") {
      CAP <- 125
    }
    if (COUNTRY == "LVA") {
      CAP <- 125
    }
    if (COUNTRY == "LTU") {
      CAP <- 125
    }
    if (COUNTRY == "NLD") {
      CAP <- 115
    }
    if (COUNTRY == "NZL") {
      CAP <- 125
    }
    if (COUNTRY == "NOR") {
      CAP <- 100
    }
    if (COUNTRY == "POL") {
      CAP <- 115
    }
    if (COUNTRY == "PRT") {
      CAP <- 125
    }
    if (COUNTRY == "SGP") {
      CAP <- 100
    }
    if (COUNTRY == "SVK") {
      CAP <- 125
    }
    if (COUNTRY == "ESP") {
      CAP <- 115
    }
    if (COUNTRY == "SWE") {
      CAP <- 100
    }
    if (COUNTRY == "CHE") {
      CAP <- 100
    }
    if (COUNTRY == "GBR") {
      CAP <- 125
    }
    if (COUNTRY == "USA") {
      CAP <- 125
    }
  }
  setwd(PATH_DATA)
#  sdifSheet = getSheetNames(DATA_NAME)[intersect(grep(SAMPLE, toupper(getSheetNames(DATA_NAME))), grep("SDIF", toupper(getSheetNames(DATA_NAME))))]
  if(FILE_EXT==toupper("sas7bdat")) {
    FILE = read_sas(INPUTFILE)
  }else if (FILE_EXT=="CSV") {
  	if (stringr::str_count(readLines(DATA_NAME, n=1), ",")>30) {
    	FILE = read.csv(DATA_NAME, sep=",")
    } else {
    	FILE = read.csv(DATA_NAME, sep=";")
    }
    #FILE = readr::read_csv(DATA_NAME)
    FILE <- data.frame(lapply(FILE, function(x) if(is.logical(x)) { 
      return(as.numeric(x))
    } else {  
      return(x) 
    }
    ), stringsAsFactors=FALSE)    
  } else {
    sdifSheet = getSheetNames(DATA_NAME)[grep("SDIF", toupper(getSheetNames(DATA_NAME)))]
    FILE = read.xlsx(tolower(DATA_NAME), sheet = sdifSheet)
    # if (length(intersect(c("ENDSCR"), toupper(colnames(FILE)))) > 0) {
    # 	FILE$ENDSCR <- convertToDateTime(FILE$ENDSCR)
    # }
    #FILE = XLConnect::readWorksheetFromFile(DATA_NAME, sheet = sdifSheet)
    #FILE = read_excel(DATA_NAME, sheet = sdifSheet)
  }    
  #FILE = read.xlsx(DATA_NAME, sheet = paste0("SDIF for ", tolower(SAMPLE), " country"))
  fileColnames = toupper(colnames(FILE))
  ID_SSU_flag = length(setdiff(c("ID_SSU"), fileColnames)) == 0
  
  if(length(setdiff(FULLSDIF$name, fileColnames))==0)
  {
    print("All the variables in SDIF layout are included")
    writeLines("")
    
  }else
  {
    print("Variables not in the Country SDIF that should be included - even if they all should have missing/blank values")
    tmpName = as.matrix(sort(setdiff(FULLSDIF$name, fileColnames)))
    colnames(tmpName) = c("NAME") 
    print(tmpName)
    writeLines("")
    for (ti in 1:length(tmpName)) {
      if (FULLSDIF[which(tmpName[ti] == FULLSDIF$name),2] == "numeric") {
        FILE[,tmpName[ti]] = as.numeric(NA)
      } else {
        FILE[,tmpName[ti]] = as.character(NA)
      }
      
    }
  } 
  if (length(setdiff(fileColnames, FULLSDIF$name)) == 0) {
    print("All the variables in the Country SDIF are in SDIF layout")
    writeLines("")
  }else {  
    print("Variables in the Country SDIF that are not in SDIF layout - They need to be removed")
    tmpName = as.matrix(sort(setdiff(fileColnames, FULLSDIF$name)))
    colnames(tmpName) = c("NAME") 
    print(tmpName)    
    writeLines("")    
  }
  fileColnames = toupper(colnames(FILE))
  
  writeLines("DM(IEA) variables: You don't need to include them in the file that you will submit to IEA.

For the official Preliminary SDIF that will be sent to Westat:
These variables, that require BQ data, will be empty or only partially derived by IEA.
CALCAGE
AGE_R
IMPFLGAGER
GENDER_R
DOBYY
DOBMM
DIAGERANGE
GENDER

These variables may/may not be derived by IEA.
*PROB_OVERALL_HH
*PROB_OVERALL_PERS
*THEOR_HBWT
*THEOR_PBWT
*WEIGHTFLG
*EXCFLG
*DISP_MAIN
*COMPLETEFLG

Notes:
- Before you submit your file, it is recommended that you derive all of them to run these checks.
- For checking purposes, the program derives variablename_CHK variables for these * variables to help catching problems before the file is submitted to IEA.

- All these variables will be derived for your official Final SDIF.

")
  numChg = cbind.data.frame(name=colnames(FILE), datatype=as.character(sapply(FILE, class)),  stringsAsFactors=FALSE) %>% inner_join(FULLSDIF, by=c("name")) %>% filter(dtype=="numeric") %>% filter(datatype!=dtype)
  FILE[numChg$name] = lapply(FILE[numChg$name], as.numeric)
  
  #FILE <- FILE %>% 
  #  dplyr::mutate(across(starts_with("DISP_"), ~formatC(.x, width = 2, format = "d", flag = "0")))
  addzero <- function(x) {
    if (is.numeric(x)) {
      res <- formatC(x, width = 2, format = "d", flag = "0")      
    } else {
      res <- ifelse(grepl("[0-9]+",trimws(x)), formatC(suppressWarnings(as.numeric(x)), width = 2, format = "d", flag = "0"), x)   
    }
    return(res)
  }
  FILE <- FILE %>% 
    dplyr::mutate(across(starts_with("DISP_"), ~addzero(.x))) %>% 
    select(intersect(fileColnames, FULLSDIF$name))
  
  FILE$WEIGHTFLG_CHK <- dplyr::if_else(FILE$DISP_CIBQ %in% c("01") | (FILE$DISP_CIBQ %in% c("07") & FILE$DISP_DS %in% c("01")), 1, 0) 
  
  if (SAMPTYPE != "") {
    FILE <- FILE %>%
      dplyr::filter(SAMPTYPE == !!as.numeric(SAMPTYPE))
  }
  set.seed(1)
  THREE <- cbind(FILE, rannum=runif(nrow(FILE))) %>% arrange(rannum)
  print(paste0("Print of 10 random records in ", COUNTRYNAME, " SDIF - Spot check for possible errors"))
  print(head(THREE[, -which(names(THREE) %in% c("rannum"))], 10), digits=10)
  writeLines("")
  
  #comVars <- intersect(fileColnames, c("PROB_PSU", "PROB_SSU", "PROB_HH", "PROB_OTH","PROB_PERS"))
  allProbs <- c("PROB_PSU", "PROB_SSU", "PROB_HH", "PROB_OTH","PROB_PERS")
  comVars <- intersect(fileColnames, allProbs)
  title2_1 <- "Initial checks for variables that will help in weighting\n"
  title3 <- ""
  fmtCols(THREE, comVars, paste0(title2_1, "Checks on ", paste0(comVars, collapse = "*"), title3))
  writeLines("")
  
  PROB_VAR = "PROB_HH"
  if (SAMPLE == "REGISTRY") {
    PROB_VAR = "PROB_PERS"
  }
  
  if(colSums(is.na(THREE[PROB_VAR]))==nrow(THREE)) 
  {
    print(paste0(title2_1, "Checks on ", PROB_VAR, title3))
    print(paste0("WARNING: ", PROB_VAR, " IS MISSING FOR ALL THE RECORDS, PLEASE CHECK."))
  }
  multiply <- function(dsn, s, v, ...) {
    probSamp <- dsn %>%
      dplyr::select(c("PROB_SMPFLG1", "PROB_SMPFLG2", "PROB_SMPFLG3"))
    pSamp <- rep(NA, nrow(dsn))
    prob <- rep(1, nrow(dsn))
    for (j in 1:nrow(dsn)) {
      prob[j] <- dsn[j, v]
      for (i in list(...)) {
        res = dsn[j,i]
        #if (is.na(res) || res==0) res  = 1
        if (is.na(res)) res  = 1
        prob[j] = prob[j] * res
      }
      if (s == "SCREENER" && v == "PROB_PERS" && COUNTRY %in% c("USA", "NZL", "ZED") ) {
        if (!is.na(dsn[j,"STRAT_PERS"])) {
          if (dsn[j,"STRAT_PERS"] >= 1 && dsn[j,"STRAT_PERS"] <= 3) {
            pSamp[j] <- probSamp[j, dsn[j,"STRAT_PERS"]]   
            #if (is.na(pSamp[j]) || pSamp[j] == 0) pSamp[j] = 1;    
          } else {
            pSamp[j] = NA
          }
        }
        prob[j] = prob[j] * pSamp[j]
        #print(paste0(pSamp[j], probSamp[j,], dsn[j,"STRAT_PERS"], collapse = " * "))
      }
    }
    return(prob)
  }
  if (SAMPLE != "REGISTRY") {
    FILE <- FILE %>% 
      dplyr::mutate(PROB_OVERALL_HH_CHK = multiply(FILE, SAMPLE,"PROB_HH", "PROB_PSU", "PROB_SSU", "PROB_OTH")) %>%
      dplyr::mutate(THEOR_HBWT_CHK = ifelse(PROB_OVERALL_HH_CHK > 0, 1 / PROB_OVERALL_HH_CHK,ifelse(PROB_OVERALL_HH_CHK == 0, 0, NA)))
      #vmean(FILE, c("PROB_OVERALL_HH_CHK", "THEOR_HBWT_CHK"))
      vmean(FILE, c("PROB_OVERALL_HH_CHK"), vtype = 6, title = paste0(title2_1, "Checks on PROB_OVERALL_HH_CHK", title3))
  }
  FILE <- FILE %>% 
    dplyr::mutate(PROB_OVERALL_PERS_CHK = multiply(FILE, SAMPLE,"PROB_PERS", "PROB_HH", "PROB_PSU", "PROB_SSU", "PROB_OTH")) %>%
    dplyr::mutate(THEOR_PBWT_CHK = ifelse(PROB_OVERALL_PERS_CHK > 0, 1 / PROB_OVERALL_PERS_CHK, ifelse(PROB_OVERALL_PERS_CHK == 0, 0, NA)))
    #vmean(FILE, c("PROB_OVERALL_PERS_CHK", "THEOR_PBWT_CHK"))
    vmean(FILE, c("PROB_OVERALL_PERS_CHK"), vtype = 6, title = paste0(title2_1, "Checks on PROB_OVERALL_PERS_CHK", title3))
  THREE <- FILE
  colnames(THREE) <- toupper(colnames(THREE))
  
  writeLines("")
  if (SAMPLE != "REGISTRY") {
  cat(paste0(title2_1, "Print of 25 records checking: PROB_OVERALL_HH_CHK, PROB_OVERALL_PERS_CHK, THEOR_HBWT_CHK, THEOR_PBWT_CHK derived from the SDIF", title3, "\n"))
  print(head(cbind(THREE[,c("PROB_PSU", "PROB_SSU", "PROB_HH", "PROB_OTH", "PROB_SMPFLG1", "PROB_SMPFLG2", "PROB_SMPFLG3","STRAT_PERS", "PROB_PERS")], PROB_OVERALL_HH_CHK=formatC(THREE$PROB_OVERALL_HH_CHK, 10, format="fg"), PROB_OVERALL_PERS_CHK=formatC(THREE$PROB_OVERALL_PERS_CHK, 10, format="fg"), THREE[,c("THEOR_HBWT_CHK", "THEOR_PBWT_CHK")]), 25))
  } else {
    cat(paste0(title2_1, "Print of 25 records checking: PROB_OVERALL_PERS_CHK, THEOR_PBWT_CHK derived from the SDIF", title3, "\n"))
    print(head(cbind(THREE[,c("PROB_PSU", "PROB_SSU", "PROB_HH", "PROB_OTH", "PROB_SMPFLG1", "PROB_SMPFLG2", "PROB_SMPFLG3","STRAT_PERS", "PROB_PERS")], PROB_OVERALL_PERS_CHK=formatC(THREE$PROB_OVERALL_PERS_CHK, 10, format="fg"), THEOR_PBWT_CHK=THREE[,"THEOR_PBWT_CHK"]), 25))
  }
  writeLines("")
  
  if (SAMPLE != "REGISTRY") {
    cat(paste0(title2_1, "Checks on the created variable PROB_OVERALL_HH_CHK. See that results make sense according to your design\n"))
    univ(THREE, c("PROB_OVERALL_HH_CHK"))
    writeLines("")
    
  }
  
  cat(paste0(title2_1, "Checks on the created variable PROB_OVERALL_PERS_CHK. See that results make sense according to your design\n"))
  univ(THREE, c("PROB_OVERALL_PERS_CHK"))
  writeLines("")
  
  if (SAMPLE != "REGISTRY") {
    cat(paste0(title2_1, "Checks on the created variable THEOR_HBWT_CHK. See that results make sense according to your design\n"))
    univ(THREE, c("THEOR_HBWT_CHK"))
    writeLines("")
  }
  
  cat(paste0(title2_1, "Checks on the created variable THEOR_PBWT_CHK. See that results make sense according to your design\n"))
  univ(THREE, c("THEOR_PBWT_CHK"))
  writeLines("")

  cat(paste0(title2_1, "Checks on Literacy Related Cases\n"))
  if (SAMPLE == "REGISTRY") {
    NL1 <- THREE %>% 
      dplyr::filter((DISP_CIBQ %in% c("07") & DISP_DS %in% c("01")) | (DISP_CIBQ %in% c("01") & DISP_MAIN %in% c("07","08","09"))) %>%
      dplyr::count(name="NL1")
  
    NL2 <- THREE %>% 
      dplyr::filter((!(DISP_DS %in% c("01")) & DISP_CIBQ %in% c("07","08","09"))) %>%
      dplyr::count(name="NL2")
  
    cat(paste0("Verify that L1 is not substantially smaller than (L1 + L2)\n", title3))
    writeLines("")
    print(paste0("L1 = #[(DISP_CIBQ=07 and DISP_DS=01) or (DISP_CIBQ=01 and DISP_MAIN=07,08,09)]: ", NL1))
    writeLines("")
    print(paste0("L2 = #[DISP_CIBQ=07,08,09 and DISP_DS ne 01]: ", NL2))
    writeLines("")
    cat("L1: BQ literacy-related nonrespondent that completed the Doorstep Interview or assessment literacy-related nonrespondent,\nL2: BQ literacy-related nonrespondent that did not complete the Doorstep Interview\n")
    writeLines("")
    vfreq(THREE, c("DISP_CIBQ", "DISP_DS", "DISP_MAIN"), runFreq = TRUE, filter = "DISP_CIBQ %in% c('01','07','08','09','90')", 
          title = paste0(title2_1, "Checks on Literacy Related Cases\nFreqs on DISP_CIBQ*DISP_DS*DISP_MAIN where DISP_CIBQ in ('01', '07', '08', '09', '90')", title3))
  } else {
    
    NL1 <- THREE %>% 
      dplyr::filter((DISP_SCR %in% c("01","02") & ((DISP_CIBQ %in% c("07") & DISP_DS %in% c("01") ) | (DISP_CIBQ %in% c("01") & DISP_MAIN %in% c("07","08","09"))))) %>%
      dplyr::count(name="NL1")    

    NL2 <- THREE %>% 
      dplyr::filter(DISP_SCR %in% c("01","02") & DISP_CIBQ %in% c("07","08","09") & !(DISP_DS %in% c("01"))) %>%
      dplyr::count(name="NL2")
    
    LSCR <- THREE %>% 
      dplyr::filter(DISP_SCR %in% c("07")) %>%
      dplyr::count(name="LSCR")
    
    cat(paste0("Verify that L1 is not substantially smaller than (L1 + L2 + LSCR)\n", title3))
    writeLines("")
    print(paste0("L1 = #[DISP_SCR=01,02 and [(DISP_CIBQ=07 and DISP_DS=01) or (DISP_CIBQ=01 and DISP_MAIN=07,08,09)]]: ", NL1))
    writeLines("")
    print(paste0("L2 = #[DISP_SCR=01,02 and DISP_CIBQ=07,08,09 and DISP_DS ne 01]: ", NL2))
    writeLines("")
    print(paste0("LSCR = #[DISP_SCR=07]: ", LSCR))
    writeLines("")
    cat("L1: BQ literacy-related nonrespondent that completed the Doorstep Interview or assessment literacy-related nonrespondent,\nL2: BQ literacy-related nonrespondent that did not complete the Doorstep Interview\nLSCR: is the number of screener cases with disposition code 07 (Language Barrier).\n")
    
    writeLines("")
    
    vfreq(THREE, c("DISP_SCR", "DISP_CIBQ", "DISP_DS", "DISP_MAIN"), runFreq = TRUE, filter = "DISP_SCR %in% c('01','02','07')", 
          title = paste0(title2_1, "Checks on Literacy Related Cases.\nFreqs on DISP_SCR*DISP_CIBQ*DISP_DS*DISP_MAIN where DISP_SCR=01,02,07", title3))
  }
  writeLines("")
  NAssessLR <- THREE %>% 
    dplyr::filter(DISP_CIBQ %in% c("01") & DISP_MAIN %in% c("07","08","09")) %>%
    dplyr::count(name="NAssessLR")  
  cat(paste0(title2_1, "Checks on Literacy Related Cases\n\nAssessment LR: #[DISP_CIBQ=01 and DISP_MAIN=07,08,09] = ", NAssessLR, "\n"))

  writeLines("")
  NAssess <- THREE %>% 
    dplyr::filter(DISP_CIBQ %in% c("90") & DISP_MAIN %in% c("07","08","09")) %>%
    dplyr::count(name="NAssess")  
  cat(paste0(title2_1, "Checks on Literacy Related Cases\n\nAssessment LR: #[ DISP_CIBQ=90 and DISP_MAIN=07,08,09] = ", NAssess, "\n"))
  writeLines("")
  
  cat("\n")
  forLoop <- expand.grid(ifelse(allMiss(THREE, c("WEIGHTFLG")), c("WEIGHTFLG_CHK") , c("WEIGHTFLG_CHK", "WEIGHTFLG")), c("DISP_CIBQ"), c("DISP_DS"), c("RAKEDIM1","PERSVAR1", "DUVAR_SCRRESP1", "DUVAR_ALL1", "AREAVAR1"), stringsAsFactors = F)
  for (loop in 1:nrow(forLoop)) {
    if (SAMPLE == "REGISTRY") {
      vfreq(THREE, as.character(forLoop[loop,]), runFreq = TRUE, 
            title=paste0(title2_1, "Checks on Raking and NR variables\nFreqs on ", paste(forLoop[loop,],collapse = "*"), title3))
    } else {
      vfreq(THREE, as.character(forLoop[loop,]), runFreq = TRUE, filter = "DISP_SCR %in% c('01','02')", 
            title=paste0(title2_1, "Checks on Raking and NR variables - Cases where DISP_SCR = 01,02\nFreqs on ", paste(forLoop[loop,],collapse = "*"), title3))
    }
    #writeLines("")
  }
  
  fmttmp <- THREE %>%
    mutate(AGE_R = case_when(
      AGE_R >= 0 & AGE_R <= 15 ~ "<=15",
      AGE_R >= 16 & AGE_R <= 25 ~ "16-25",
      AGE_R >= 26 & AGE_R <= 35 ~ "26-35",
      AGE_R >= 36 & AGE_R <= 45 ~ "36-45",
      AGE_R >= 46 & AGE_R <= 55 ~ "46-55",
      AGE_R >= 56 & AGE_R <= 65 ~ "56-65",
      AGE_R >= 66 & AGE_R <= 150 ~ "66+",
      TRUE ~ as.character(AGE_R)
    ))
  
  cat("\n")
  forLoop <- cbind(ifelse(allMiss(THREE, c("WEIGHTFLG")), c("WEIGHTFLG_CHK") , c("WEIGHTFLG_CHK", "WEIGHTFLG")), rbind(expand.grid( c("DISP_CIBQ"), c("DISP_DS"), c("AGE_R","GENDER_R"), stringsAsFactors = F),
                         c("RAKEDIM1","GENDER_R","AGE_R"),
                         expand.grid( c("DISP_CIBQ"), c("DISP_DS"), paste0("RAKEDIM", 1:7))))
  for (loop in 1:nrow(forLoop)) {
    if (loop <= 3) {
      tit1 <- "Check: all cases with WEIGHTFLG_CHK=1 should have AGE_R and GENDER_R non-missing\nFreqs on "
    } else{
      tit1 <- "Check: all cases with WEIGHTFLG_CHK=1 should have at least RAKEDIM1 non-missing\nFreqs on "
    }
    
    if (SAMPLE == "REGISTRY") {
      vfreq(fmttmp, as.character(forLoop[loop,]), runFreq = TRUE, 
            title=paste0(title2_1, tit1 , paste(forLoop[loop,],collapse = "*")))
    } else {
      vfreq(fmttmp, as.character(forLoop[loop,]), runFreq = TRUE, filter = "DISP_SCR %in% c('01','02')", 
            title=paste0(title2_1, tit1, paste(forLoop[loop,],collapse = "*")))
    }
    #writeLines("")
  }
  
  vfreq(THREE, c("DISP_SCR","DISP_CIBQ","DISP_MAIN"), runFreq = TRUE, filter = "QCFLAG==2", 
        title=paste0(title2_1, "Check that all falsified cases (QCFLAG = 2) are coded as nonresponse", title3))

  vfreq(THREE, c("CNTRYID"), "Check: There should be no missing values. Same value for all records", TRUE)

  vfreq(THREE, list(c("CNTRY"), c("SAMPTYPE", "CNTRY")),  "Check: All cases should have the same value if only one language")

  if (SAMPLE == "REGISTRY") {
    fmtCols(THREE, c("CASEID"), "Check: All CASEID = missing")
  } else  {
    fmtCols(THREE, c("CASEID"), "Check: All CASEID = non-missing")
  }
  writeLines("")
  if (SAMPLE == "REGISTRY") {  
	  fmtCols(THREE, c("PERSID"), tMiss = TRUE, "Check: There should be no missing values for PERSID")
    writeLines("")
	  dupRec(THREE, c("PERSID"), dupTit = "Check: There should be no duplicate or missing values for PERSIDs", chkLastMiss = FALSE)
  } else {
  	 if (length(intersect("PERSID", fileColnames)) == 1)
  	 {
  	   fmtCols(THREE, c("PERSID"), fmtnames = c("PERSID"), title="Check: There should be no duplicate or missing values for PERSIDs for responding HHs")
  	   writeLines("")
  	   if (allMiss(THREE,c("PERSID")))
  	 	{
  	 		print("Check on PERSID - All missing")
  	 		print("PERSID Check - No duplicates because all are missing values")
  	 	}else
  	 	{
  	 		dupRec(THREE, c("PERSID"), FALSE, dupTit="Check: There should be no duplicate or missing values for PERSIDs for responding HHs", 
  	 		       filter = "!is.na(PERSID)")
  	 	  cat("\n")
  	 	  fmtCols(THREE, c("DISP_SCR", "PERSID"), fmtnames = c("PERSID"), title = "Check PERSID is nonmissing for cases with DISP_SCR=1 or 2",  filter = "DISP_SCR %in% c('01', '02')")
  	 	}
  	 }
  }	 
  writeLines("")
  
  if(allMiss(THREE, c("ID_MAJDES")))
  {
    print("Check: No Major Design Stratum") 
    vfreq(THREE, c("ID_MAJDES"), runFreq = TRUE)
  }else
  {
    vfreq(THREE, c("ID_MAJDES"), "Check: Is this your Major Design Stratum?")
  }
  
  countN(THREE, c("ID_PSU"), cntTitle = "Check: The number of unique PSU IDs should be equal to the number of selected PSUs")
  writeLines("")
  
  countN(THREE, c("ID_PSU", "ID_SSU"), cntTitle = "Check: The number of unique PSU and SSU ID combinations should be equal to the number of selected SSUs", cntMissTitle="No SSUs selected")
  writeLines("")
   
  if (SAMPLE == "REGISTRY") {
    vfreq(THREE, c("ID_HH"), "ID_HH  - All records should have missing values", runFreq = T)
  } else  {
    countN(THREE, union(c("ID_PSU"), union(intersect(c("ID_SSU"), fileColnames), c("ID_HH"))), cntTitle = paste0("Check: The number of unique PSU", ifelse(ID_SSU_flag, ", SSU, ", ''), " and HH ID combinations should be equal to the number of selected HHs"), cntMissTitle="No HHs selected")
    writeLines("")
  }  
  
  if(SAMPLE == "REGISTRY") {
    vfreq(THREE, c("ADD_DU"), "Check: Verify that all are missing values", runFreq = TRUE)
  } else {
    vfreq(THREE, c("ADD_DU"), "Check: Verify that all values are set to 0 or 1", runFreq = TRUE)
  }

  vfreq(THREE, c("SUBSAMP"), "Check: verify that SUBSAMP = (1, 2, 3, 4, 5, 6, 7, 8, 9) for all records", runFreq = TRUE)

  vfreq(THREE, c("SAMPTYPE"), "Check: verify that SAMPTYPE = (1, 2, 3,...,9) for all records", runFreq = TRUE)

  vfreq(THREE, c("QCFLAG_ASSIGN"), "Check: verify that QCFLAG_ASSIGN = (0, 1), and that 10% of the cases have a value of 1.", runFreq = TRUE)
  
  if (SAMPLE == "REGISTRY") {
    vfreq(THREE, list(paste0("SMPFLG", 1:3)), "Check: all values should be missing or zero", runFreq = TRUE)
  } else  {
    vfreq(THREE, append(strsplit(paste0("SMPFLG", 1:3), " "), list(paste0("SMPFLG", 1:3))), "Check: Verify values depending on your sample design; all values for SMPFLG1=1; for SMPFLGs 2 and 3 values can be 1 or  0", runFreq = TRUE)
  }

  vfreq(THREE, c("EXCFRM_PROP"), "Check: Nonmissing and constant for all records. It should match the value in your Sample Design Summary", runFreq = TRUE)
  
  vfreq(THREE, c("REGION"), "Check: verify that REGION is non-missing and takes values of 1, 2, ..., 24, 25", runFreq = TRUE)

  vfreq(THREE, list(c("URBRUR"), c('SAMPTYPE',"URBRUR")), "Check: Verify that URBRUR = (1, 2) for all the Core PIAAC Sample (SAMPTYPE=1) records", runFreq = TRUE)
  
  if (allMiss(THREE, c("ID_PSU", "PROB_PSU"))) {
    print("Check PROB_PSU: Your design didn't select PSUs. All missing values is OK")
    vfreq(THREE, c("PROB_PSU"), runFreq = T)
  } else {
    univ(THREE, c("PROB_PSU"), "Check: verify that 0 < PROB_PSU <= 1 if multi-stage designs", TRUE)
  }
  writeLines("")
  
  if (allMiss(THREE, c("ID_SSU", "PROB_SSU"))) {
    print("Check: No SSUs selected. Therefore, ID_SSU PROB_SSU with all missing values is OK.")
    vfreq(THREE, c("PROB_SSU"), runFreq = T)
  } else {
    univ(THREE, c("PROB_SSU"), "Check: This is a conditional probability of selection. Verify that 0 < PROB_SSU <= 1 if there is a second-stage sampling", TRUE, "Check: No SSUs selected. Therefore, PROB_SSU with all missing values is OK")    
  }
  writeLines("")
  
  #allMiss(THREE,  c("ID_SSU","PROB_SSU"), "Check: No SSUs selected. Therefore, PROB_SSU with all missing values is OK")
  #writeLines("")
  
  if (SAMPLE != "REGISTRY") {
    univ(THREE, c("PROB_HH"), "Check: verify that 0 < PROB_HH <= 1 for all records", TRUE, "Check: No HHs selected. Therefore, PROB_HH with all missing values is OK")
  } else {
    if (allMiss(THREE,  c("ID_HH", "PROB_HH"))) {
      print("Check: No HHs selected. Therefore, PROB_HH with all missing values is OK.")
      vfreq(THREE, c("PROB_HH"), runFreq = T)
    }   
  }
  writeLines("")
  
  if (SAMPLE == "REGISTRY") {
    vfreq(THREE, c("PROB_SMPFLG1", "PROB_SMPFLG2", "PROB_SMPFLG3"), "Check: All values should be missing values", runFreq = TRUE)
  } else {
    for (fcnt in 1:3) {
      vfreq(THREE, c(paste0("SMPFLG", fcnt), paste0("PROB_SMPFLG", fcnt)), paste0("Check: PROB_SMPFLG", fcnt, " is non-missing if SMPFLG", fcnt, " is non-missing"), runFreq = TRUE)
    }
  }
  fmtCols(THREE,c("STRAT_PSU"), "Checks on STRAT_PSU")
  writeLines("")
  if (!allMiss(THREE, "STRAT_PSU")) {
    dupRec(THREE, c("ID_PSU", "STRAT_PSU"), TRUE, "No PSU with multiple PSU strata found", "A PSU cannot have multiple PSU strata: i.e. each ID_PSU corresponds to only one STRAT_PSU value", "", FALSE)
    writeLines("")
    
    cat("This check provides information as given in the SS form. This is a reference when forming replicate weights\n(If applicable, for each of the STRAT and SORT variables, check that there are no missing values. This is important to check because we need these variables for variance strata/units)\n")
    vfreq(THREE, c("STRAT_PSU", "SORT_PSU", "ID_PSU"), "Check on STRAT_PSU*SORT_PSU*ID_PSU", runFreq = TRUE)
    
  }
  
  dupRec(THREE, c("ID_PSU", "ID_SSU", "STRAT_SSU"), TRUE, "No SSU with multiple STRAT_SSU found", "A SSU cannot have multiple SSU strata: i.e. each unique combination of ID_PSU and ID_SSU corresponds to only one STRAT_SSU","Check on STRAT_SSU")
  writeLines("")
  
  if (!allMiss(THREE,c("STRAT_SSU"))) {
    vfreq(THREE, c("ID_PSU","STRAT_SSU","SORT_SSU","ID_SSU"), "Check on ID_PSU*STRAT_SSU*SORT_SSU*ID_SSU", runFreq = TRUE)
  }

  if (SAMPLE == "REGISTRY") {
    sHH <- ""
    if (!allMiss(THREE, "STRAT_HH")) sHH <- "\nSTRAT_HH should be missing"
    if (allMiss(THREE, c("ID_HH", "PROB_HH", "STRAT_HH"))) {
      cat("\n")
      vfreq(THREE, c("STRAT_HH"), paste0("Check: No HHs selected. Therefore, STRAT_HH with all missing values is OK", sHH), runFreq = TRUE)
    } else {
      fmtCols(THREE, c("ID_HH", "PROB_HH", "STRAT_HH"), paste0("No HHs were selected. Verify that If ID_HH, PROB_HH and STRAT_HH have missing vales", sHH))
    }
  } else  {
    countN(THREE, union(c("ID_PSU"), union(intersect(c("ID_SSU"), fileColnames), c("STRAT_HH"))), FALSE, filter="!is.na(STRAT_HH)")
    writeLines("")
    fmtCols(THREE, c("STRAT_HH"), "Check on STRAT_HH")
    writeLines("")
  }
  
  dupRec(THREE, c("ID_PSU", "SORT_PSU"), TRUE, "No ID_PSU with multiple SORT_PSU found", "Each ID_PSU corresponds to only one SORT_PSU value", chkLastMiss=FALSE)
  writeLines("")
  fmtCols(THREE, c("SORT_PSU"), "Check on SORT_PSU")
  writeLines("")
  
  dupRec(THREE, union(c("ID_PSU"), union(intersect(c("ID_SSU"), fileColnames), c("SORT_SSU"))), TRUE, paste0("No ID_PSU", ifelse(ID_SSU_flag, " AND ID_SSU", ""), " with multiple SORT_SSU found"), ifelse(ID_SSU_flag, "Each unique combination of ID_PSU and ID_SSU corresponds to only one SORT_SSU value", "Each unique ID_PSU corresponds to only one SORT_SSU value"), chkLastMiss=FALSE)
  writeLines("")
  fmtCols(THREE, c("SORT_SSU"), "Check on SORT_SSU")
  writeLines("")
  
  if (SAMPLE == "REGISTRY") {
    if (allMiss(THREE, c("ID_HH", "ID_SSU", "ID_HH", "SORT_HH"))) {
      vfreq(THREE, c("SORT_HH"), "Check: No HHs selected. Therefore, SORT_HH with all missing values is OK", runFreq = TRUE)
    }
  } else  {
    dupRec(THREE, union(c("ID_PSU"), union(intersect(c("ID_SSU"), fileColnames), c("ID_HH", "SORT_HH"))), TRUE, ifelse(ID_SSU_flag, "No ID_PSU, ID_SSU AND ID_HH with multiple SORT_HH found", "No ID_PSU AND ID_HH with multiple SORT_HH found"), ifelse(ID_SSU_flag, "Check: Within each HH, all sampled persons should have the same SORT_HH. Checking that ID_PSU ID_SSU ID_HH SORT_HH is unique","Check: Within each HH, all sampled persons should have the same SORT_HH. Checking that ID_PSU ID_HH SORT_HH is unique"), chkLastMiss=FALSE, lnames =  union(c("ID_PSU"), union(intersect("ID_SSU", fileColnames), c("ID_HH","STRAT_PSU","STRAT_SSU","STRAT_HH","SORT_PSU","SORT_SSU","SORT_HH"))))
    writeLines("")
    dupRec(THREE, union(c("ID_PSU"), union(intersect(c("ID_SSU"), fileColnames), c("SORT_HH", "ID_HH"))), TRUE, ifelse(ID_SSU_flag, "No ID_PSU, ID_SSU AND SORT_HH with multiple ID_HH found", "No ID_PSU AND SORT_HH with multiple ID_HH found"), ifelse(ID_SSU_flag, "Check: Within each ID_PSU*ID_SSU combination, each ID_HH should have a unique value of SORT_HH","Check: Within each ID_PSU, each ID_HH should have a unique value of SORT_HH"), chkLastMiss=FALSE, lnames = union(c("ID_PSU"), union(intersect("ID_SSU", fileColnames), c("ID_HH","STRAT_PSU","STRAT_SSU","STRAT_HH","SORT_PSU","SORT_SSU","SORT_HH"))))
    writeLines("")
    fmtCols(THREE, c("SORT_HH"), "Check on SORT_HH")
    writeLines("")
  }  
  
  fmtCols(THREE, c("ID_OTH"), "Check: ID_OTH used for an additional stage of sampling", tMiss = TRUE, missTitle = "Check: ID_OTH not used, therefore, no additional stage of sampling")
  writeLines("")
  
  if (allMiss(THREE, c("ID_OTH", "PROB_OTH"), "Check: No additional sampling stage. Therefore, ID_OTH PROB_OTH with all missing values is OK")) {
    vfreq(THREE, c("PROB_OTH"), runFreq = T)
    } else {
    univ(THREE, c("PROB_OTH"), "Check: verify that 0 < PROB_OTH <= 1 for all records", TRUE)
  }
  writeLines("")
  
  if (SAMPLE == "REGISTRY") {
    univ(THREE, c("PROB_PERS"), "Check: This is a conditional probability of selection. Verify that  0 < PROB_PERS <= 1 for all records", TRUE)
    writeLines("")
    vmean(THREE, c("PROB_PERS"), mtype = 2)
  } else {
    univ(THREE, c("PROB_PERS"), "Check: This is a conditional probability of selection. Verify that 0 < PROB_PERS <= 1 for all records", TRUE)
    for (dcnt in 1:nrow(THREE)) {
      if (THREE[dcnt, c("DISP_SCR")] %in% c("01", "02")) {
      
        if (COUNTRY %in% c("NZL")) {
	        icol <- THREE[dcnt, c("STRAT_PERS")]
		      if (isTRUE(1 <= icol  &&  icol <= 3)) {
			      THREE[dcnt, c("PROB_PERS_CHK")] <- THREE[dcnt, c(paste0("NUMSEL", icol))] / THREE[dcnt, c(paste0("NUMELG", icol))]
		      }   
	      } else {
 	 	      THREE[dcnt, c("PROB_PERS_CHK")] <- THREE[dcnt, c("NUMSEL1")] / THREE[dcnt, c("NUMELG1")]
	      }
      }
    }
    writeLines("")
    
    vmean(THREE, c("PROB_PERS", "PROB_PERS_CHK"), "Check that these variables match, and that 0 < PROB_PERS <= 1 for all records", mtype = 2)
    cat(paste0("Print of 25 records checking: PROB_PERS_CHK NUMELG1-NUMELG3 NUMSEL1-NUMSEL3", "\n"))
    print(head(THREE[, intersect(c("CASEID", "PERSID", "STRAT_PERS", "PROB_PERS_CHK", paste0("NUMELG", 1:3), paste0("NUMSEL", 1:3)), colnames(THREE))], 25))
    writeLines("")
    vfreq(THREE, c("PROB_PERS", "PROB_PERS_CHK", paste0("NUMELG", 1:3), paste0("NUMSEL", 1:3)), runFreq = T, title = "Check: Verify that PROB_PERS is correctly calculated")
  }
  writeLines("")

  fmtCols(THREE, c("STRAT_PERS"), "Check: Nonmissing if stratification is used")
  writeLines("")
  if (!allMiss(THREE, c("STRAT_PERS"))) {
    if (SAMPLE == "REGISTRY") {
        countN(THREE, c("ID_PSU", "ID_SSU", "ID_HH", "STRAT_PERS"), filter = "!is.na(STRAT_PERS)")
    } else  {
      fmtTmp <- THREE %>% 
        mutate(CALCAGE = case_when(CALCAGE >= 16 & CALCAGE <= 65 ~ "16-65",
                                      CALCAGE >= 67 & CALCAGE <150 ~ "67",
                                      TRUE ~ as.character(CALCAGE)
                                      )) %>%
        mutate(across(starts_with("PROB_SMPFLG"), ~ case_when(. > 0 & . <= 1 ~ ">0", TRUE ~ as.character(.))))
      
      fmtCols(fmtTmp, c("CASEID", "PERSID", "CALCAGE", "SMPFLG1", "SMPFLG2", "SMPFLG3", "PROB_SMPFLG1","PROB_SMPFLG2", "PROB_SMPFLG3", "STRAT_PERS"), "Verify the creation of STRAT_PERS", fmtnames = c("CASEID", "PERSID"))
      writeLines("")
    }
  }
  
  fmtCols(THREE, c("SORT_PERS"), "Check on SORT_PERS")
  writeLines("")
  if (!allMiss(THREE, c("SORT_PERS"))) {
    # dupRec(THREE, c("ID_PSU", "ID_SSU", "ID_HH", "STRAT_PERS",  "SORT_PERS", "PERSID"), TRUE, "No ID_PSU ID_SSU ID_HH STRAT_PERS AND SORT_PERS with multiple PERSID found", "Each unique combination of ID_PSU ID_SSU ID_HH STRAT_PERS and SORT_PERS corresponds to only one PERSID value", chkLastMiss = F)
    dupRec(THREE, c("ID_PSU", "ID_SSU", "ID_HH", "STRAT_PERS",  "SORT_PERS", "PERSID"), TRUE, "No ID_PSU ID_SSU ID_HH STRAT_PERS AND SORT_PERS with multiple PERSID found", "Each unique combination of ID_PSU ID_SSU ID_HH STRAT_PERS and SORT_PERS corresponds to only one PERSID value", chkLastMiss = F)
    writeLines("")
  }

  fmtCols(THREE, c("INTVID"), "Check: verify that there are no missing values")
  writeLines("")
  
  vfreq(THREE, c("QCFLAG"), "Check on QCFLAG (valid values: 0, 1, 2, 3, 4)", runFreq = TRUE)
  vfreq(THREE, c("QCFLAG_ASSIGN", "QCFLAG"), "Verify that QC was done for the flagged cases, i.e., if QCFLAG_ASSIGN=1, then QCFLAG is greater than zero", runFreq = TRUE)
  
  if (SAMPLE == "REGISTRY") {
    vfreq(THREE, c("QCFLAG", "DISP_CIBQ", "DISP_MAIN", "COMPLETEFLG"), "Check on falsification of cases – If QCFLAG=2 and COMPLETEFLG = 0, then either DISP_CIBQ = 17 or DISP_MAIN = 17, depending on where the falsification occurred\nIf DISP_CIBQ = 1 for a case that has QCFLAG = 2, then you should verify that 1) the data for this stage is OK, and 2) that it is a later stage that was falsified.", runFreq = TRUE, filter = "QCFLAG==2")
  } else  {
    vfreq(THREE, c("QCFLAG", "DISP_SCR", "DISP_CIBQ", "DISP_MAIN", "COMPLETEFLG"), "Check on falsification of cases - If QCFLAG=2 and COMPLETEFLG=0, then either DISP_SCR=17, DISP_CIBQ = 17 or DISP_MAIN =17, depending on where the falsification occurred\nIf DISP_CIBQ = 1 for a case that has QCFLAG = 2, then you should verify that 1) the data for this stage is OK, and 2) that it is a later stage that was falsified.", runFreq = TRUE, filter = "QCFLAG==2")
  }

  vfreq(THREE, c("QCFLAG", "DISP_CIBQ", "DISP_DS", "EXCFLG"), "Check that if QCFLAG=2, there are no cases with [DISP_CIBQ=01,90 or (DISP_CIBQ=07 and DISP_DS=01, 90)]", runFreq = TRUE, filter = "QCFLAG==2")
  
  vfreq(THREE, c("QCFLAG", "DISP_MAIN", "DISP_CIBQ"), "Check that if QCFLAG=2, there are no cases with [DISP_MAIN = 01, 90, 07, 08, 09, 12, 13, 15, 16, 18]", filter = "QCFLAG == 2",  misTitle = "There are no cases with QCFLAG=2 and DISP_MAIN  in ( 01, 90, 07, 08, 09, 12, 13, 15, 16, 18)")
  
  vfreq(THREE,c("QCFLAG", "WEIGHTFLG_CHK"), "Check of QCFLAG on cases that will receive weight. That is, if WEIGHTFLG_CHK=1, then QCFLAG should have values 0, 1, 3, or 4\nNote that this check uses the toolkit created variable WEIGHTFLG_CHK", runFreq = TRUE)    
  
  vfreq(THREE, c("QCREC"), "Verify that results reflect the # of recording status as you planned", runFreq = TRUE)
  if (SAMPLE == "REGISTRY") {
    vfreq(THREE, c("DISP_CIBQ", "QCREC"), "Verify that results reflect the # of recording status as you planned", runFreq = T, filter = "DISP_CIBQ %in% ('01')")
  } else {
    vfreq(THREE, c("DISP_SCR", "DISP_CIBQ", "QCREC"), "Verify that results reflect the # of recording status as you planned", runFreq = T, filter = "DISP_SCR %in% ('01') & DISP_CIBQ %in% ('01')")
  }
  
  for (loop in 1:3) {
    if (SAMPLE == "REGISTRY") {
      vfreq(THREE, c(paste0("NUMELG", loop)), runFreq = TRUE, 
            title = "This variable should have only missing values")
      t2 <- "This variable should have only missing values"
    } else {
      t3 <- ""
      if (COUNTRY %in% c("NZL", "USA")) {
        t3 <- "\nThis check should be verified closely"
      }
      if (loop == 1) t1 <- "Check: Verify that NUMELG1>=0 for DISP_SCR in (1, 2)"
      if (loop == 2) t1 <- paste0("For countries with 2 or more strata within HH (STRAT_PERS), verify that NUMELG2>=0 if DISP_SCR in (1, 2)", t3)        
      if (loop == 3) t1 <- paste0("For countries with 3 strata within HH (STRAT_PERS), verify that NUMELG3>=0 if DISP_SCR in (1, 2)" , t3)       
      vfreq(THREE, c("DISP_SCR", paste0("NUMELG", loop)), runFreq = TRUE, title = t1)
      if (loop == 1) t2 <- "Check: Verify that 0<=NUMSEL1<=NUMELG1 if DISP_SCR in (1, 2)"
      if (loop == 2) t2 <- paste0("For countries with 2 or more strata within HH (STRAT_PERS), verify that 0<=NUMSEL2<=NUMELG2 if DISP_SCR in (1, 2)", t3)      
      if (loop == 3) t2 <- paste0("For countries with 3 strata within HH (STRAT_PERS), verify that 0<=NUMSEL3<=NUMELG3 if DISP_SCR in (1, 2)", t3)        
    }
    vfreq(THREE, c("DISP_SCR", paste0("NUMELG", loop), paste0("NUMSEL", loop)), runFreq = TRUE, title = t2)
  }
  
  sumna <- function(dsn, x) {
    x <- intersect(colnames(dsn), x)
    y <- rep(as.numeric(NA), nrow(dsn))
    for (irow in 1:nrow(dsn)) {
      if (sum(is.na(dsn[irow, x])) != length(x)) {
        y[irow] <- sum(dsn[irow, x], na.rm = TRUE)
      }
    }
    return(y)
  }
  THREE$TOT_NUMELG <- sumna(THREE, c("NUMELG1", "NUMELG2", "NUMELG3"))
  THREE$TOT_NUMSEL <- sumna(THREE, c("NUMSEL1", "NUMSEL2", "NUMSEL3"))
  if (SAMPLE == "SCREENER") {
    vfreq(THREE, list(c("DISP_SCR", "TOT_NUMELG", "TOT_NUMSEL"),
                      c("TOT_NUMELG","NUMELG1", "NUMELG2", "NUMELG3"),
                      c("TOT_NUMSEL", "NUMSEL1", "NUMSEL2", "NUMSEL3")), runFreq = TRUE,
          title = list("Verify that 0<TOT_NUMSEL<=TOT_NUMELG if DISP_SCR in (1, 2)", "Verify that TOT_NUMELG is the sum of NUMELG1-NUMELG3", "Verify that TOT_NUMSEL is the sum of NUMSEL1-NUMSEL3"))
  } 
  
  if (SAMPLE == "REGISTRY") {
    vfreq(THREE, list(c("SCQAGE"), c("SCQAGERANGE")), "Verify that all values are missing", runFreq = TRUE)
  } else  {
    fmtTmp <- THREE %>% 
      mutate(SCQAGE = case_when(SCQAGE >= 0 & SCQAGE <= 15 ~ "<16",
                                SCQAGE >= 16 & SCQAGE <= 25 ~ "16-25",
                                SCQAGE >= 26 & SCQAGE <= 35 ~ "26-35",
                                SCQAGE >= 36 & SCQAGE <= 45 ~ "36-45",
                                SCQAGE >= 46 & SCQAGE <= 55 ~ "46-55",
                                SCQAGE >= 56 & SCQAGE <= 65 ~ "56-65",
                                SCQAGE >= 66 & SCQAGE <150 ~ "66+",
                                TRUE ~ as.character(SCQAGE)
      )) 
    vfreq(fmtTmp, c("DISP_SCR", "SCQAGE", "SCQAGERANGE"), "Verify that if SCQAGE is missing, there's a value for SCQAGERANGE for cases where DISP_SCR=01,02", runFreq = TRUE)
  }
   vfreq(THREE, c("CI_AGE"), "Verify that all values are non-missing for all sampled persons (non-missing PERSID)", filter = "!is.na(PERSID)" , runFreq = TRUE)
   agebtm <- 16
   agetop <- 65
   if (COUNTRY %in% c("NZL", "NLD")) {
     agebtm <- 16
     agetop <- 75
   }
   if (COUNTRY %in% ("CHL")) {
     agebtm = 15
     agetop = 65
   }
   if (COUNTRY %in% ("USA")) {
     agebtm = 16
     agetop = 74
   }   

  printCond(THREE, "PERSID,CI_AGE", paste0("Cases outside the ", agebtm, "-", agetop, " range - please verify them"), paste0("(CI_AGE > ", agetop, " | CI_AGE < ", agebtm, " | is.na(CI_AGE)) & !is.na(PERSID)"))
  
  fmtCols(THREE, c("PERSID","CI_GENDER"), "Verify that CI_GENDER is not missing for nonmissing PERSID", fmtnames = c("PERSID"))
  writeLines("")
  
  vfreq(THREE, c("DISP_SCR"), paste0("", ifelse(SAMPLE == "REGISTRY", "Verify that all values are missing", "Verify that all values are non-missing")), runFreq = TRUE)
  
  vfreq(THREE, list(c("V_AGE"), c("DISP_CIBQ", "CI_AGE", "V_AGE")), "Verify that all values are non-missing for cases where DISP_CIBQ=1", runFreq = TRUE, filter = "DISP_CIBQ %in% ('01')")
  
  vfreq(THREE, list(c("V_GENDER"), c("DISP_CIBQ","V_GENDER","CI_GENDER","GENDER")), "Verify that all values are non-missing for cases where DISP_CIBQ=1", runFreq = TRUE, filter = "DISP_CIBQ %in% ('01')")

  printCond(THREE, "PERSID,V_GENDER,CI_GENDER,GENDER", "Check cases where V_GENDER=0; gender not confirmed correct by respondent. Please verify.", "V_GENDER==0")
  
  tl3 <- "Check that all DISP_* codes have valid values according to the tables in chapter 10 of the TSG, and that the distributions are reasonable. Refer to Figure 5-2 in the (TSG) as well.\n"
  
  vfreq(THREE, c("DISP_CIBQ", "DISP_DS"), paste0(tl3, "DISP_DS = non-missing if DISP_CIBQ = 07. It should be missing otherwise"), runFreq = TRUE)  
  if (SAMPLE == "REGISTRY") {
    vfreq(THREE, c("DISP_CIBQ"), paste0(tl3, "All DISP_CIBQ = non-missing"), runFreq = TRUE)  
  } else  {
    vfreq(THREE, c("DISP_SCR", "DISP_CIBQ"), paste0(tl3, "All DISP_CIBQ = non-missing for cases where DISP_SCR=01,02"), runFreq = TRUE)  
  }  

  vfreq(THREE, c("DISP_CIBQ", "DISP_TUT"), paste0(tl3, "Check that the variable takes valid values, for cases where DISP_CIBQ=1"), runFreq = TRUE, filter = "DISP_CIBQ %in% ('01')")  
  
  vfreq(THREE, c("DISP_TUT", "DISP_LOC"), paste0(tl3, "Check that the variable takes valid values, for cases where DISP_TUT=1"), runFreq = TRUE, filter = "DISP_TUT %in% ('01')")  
  
  vfreq(THREE, c("DISP_LOC","RSLTLOC","DISP_CMP"), paste0(tl3, "Check that the variable DISP_CMP takes valid values; required if DISP_LOC=1 and RSLTLOC=1, 2; should be non-missing for approximately 25% of cases with RSLTLOC = 3"), runFreq = TRUE)  
  
  vfreq(THREE, c("RSLTLOC","DISP_CMP","DISP_CBA"), paste0(tl3, "Check that the variable DISP_CBA takes valid values; required if (DISP_CMP=1 & RSLTLOC=2 or 3) or (DISP_CMP is blank & RSLTLOC = 3)"), runFreq = TRUE)  

  tl3 <- "Verify that if there is a non-missing disposition code then there is non-missing attempt flag, and vice versa. \n" 
  atm <- c("TUT", "LOC")
  for (fcnt in 1:length(atm)) {
    vfreq(THREE, c(paste0("ATTMPT", atm[fcnt]), paste0("DISP_", atm[fcnt])), paste0(tl3, "Verify that if DISP_", atm[fcnt], "=nonmissing then ATTMPT", atm[fcnt], "=1 "), runFreq = TRUE)
  }
  
  vfreq(THREE, c("ATTMPTLOC", "RSLTLOC", "DISP_LOC"), "If Locator attempted, there should be a result in RSLTLOC and DISP_LOC", runFreq = TRUE)  
  
  atm <- c("CMP", "CBA")
  for (fcnt in 1:length(atm)) {
    vfreq(THREE, c(paste0("ATTMPT", atm[fcnt]), paste0("DISP_", atm[fcnt])), paste0(tl3, "Verify that if DISP_", atm[fcnt], "=nonmissing then ATTMPT", atm[fcnt], "=1 "), runFreq = TRUE)
  }  
  
  vfreq(THREE, list(c("CURBOBS1"),c("CURBOBS2_01"),c("CURBOBS2_02"),c("CURBOBS2_03"),c("CURBOBS2_04"),c("CURBOBS3")), "This variable is optional. Verify that values make sense and non-missing if your country administered the curbside observations", runFreq = TRUE, filter = ifelse(SAMPLE != "REGISTRY","!is.na(ID_HH)", "!is.na(PERSID)"))
  
  vfreq(THREE, c("DISP_CIBQ", paste0("ZZ1A_0", 1:3)), "Verify that there are fewer than 5% missing values for all cases with completed BQs (DISP_CIBQ=01)", runFreq = T, filter = "DISP_CIBQ %in% ('01')")
  
  vfreq(THREE, paste0("ZZ1B_0", 1:3), "Verify that there are fewer than 5% missing values for all cases with ZZ1A_02=1 or ZZ1A_03=1", runFreq = T, filter = "(ZZ1A_02==1 | ZZ1A_03==1) & DISP_CIBQ %in% ('01')")
  
  vfreq(THREE, c("DISP_CIBQ","ZZ2"), "Verify that there are fewer than 5% missing values for all cases with completed BQs (DISP_CIBQ=01)", runFreq = T, filter = "DISP_CIBQ %in% ('01')")
  
  vfreq(THREE, append(strsplit(paste("DISP_MAIN", strsplit(paste0("ZZ", 3:5), " "), sep = " "), " "),append(list(c("DISP_MAIN",paste0("ZZ6_0", 1:7))), strsplit(paste("DISP_MAIN", strsplit(paste0("ZZ", 7:9), " "), sep = " "), " "))), "Verify that there are fewer than 5% missing values for all cases with completed assessments (DISP_MAIN=01)", runFreq = T, filter = "DISP_MAIN %in% ('01')")
  
  if (SAMPLE == "REGISTRY") {
    vfreq(THREE, strsplit(paste0("CONTACTTIME", 1:3), " ", fixed = TRUE), as.list(paste0(c("Countries using dashboard - Verify that number of contact attempts make sense - "), c("weekdays before 18:00","weekdays after 18:00","on the weekend"))), runFreq = TRUE)
  } else  {
    THREE <- THREE %>%
      mutate(CONTACTTIME_FLG = if_else(is.na(PERSID), "Household", "Person"))
    vfreq(THREE, strsplit(paste("CONTACTTIME_FLG", paste0("CONTACTTIME", 1:3), sep = "*"), "*", fixed = TRUE), as.list(paste0(c("Countries using dashboard - Verify that number of contact attempts make sense - "), c("weekdays before 18:00","weekdays after 18:00","on the weekend"))), runFreq = TRUE)
  }
  
  THREE$CONTACTTOTAL_CHK <- sumna(THREE, paste0("CONTACTTIME", 1:3))
  cat("Check the creation of CONTACTTOTAL_CHK = Sum of (CONTACTTIME#)\n")
  print(head(THREE[, intersect(c("CONTACTTOTAL_CHK", "CONTACTTOTAL", paste0("CONTACTTIME", 1:3)), colnames(THREE))], 10))
  
  if (SAMPLE == "REGISTRY") {
    vfreq(THREE, list(c("CONTACTTOTAL_CHK","CONTACTTOTAL"), c("DISP_CIBQ","CONTACTTOTAL")), list("Verify that CONTACTTOTAL_CHK = CONTACTTOTAL", "Verify that all sampled persons have been contacted"), runFreq = TRUE) 
  } else  {
    vfreq(THREE, list(c("CONTACTTIME_FLG","CONTACTTOTAL_CHK","CONTACTTOTAL"), c("CONTACTTIME_FLG","DISP_SCR","DISP_CIBQ","CONTACTTOTAL_CHK","CONTACTTOTAL")), list("Verify that CONTACTTOTAL_CHK = CONTACTTOTAL", "Verify that all sampled cases have been contacted"), runFreq = TRUE)  
  }
  
  if (SAMPLE == "REGISTRY") {
    vfreq(THREE, c("ENDSCR"), "Verify that all cases have missing values", runFreq = TRUE)
  } else  {
    cat("Check: printout of 20 cases to help verifying the format DD/MM/YYYY HH:MM\n")

    THREE %>% 
      dplyr::filter((DISP_SCR %in% c("01", "02")) & (is.na(ENDSCR) == F & ENDSCR != "")) %>%
      dplyr::select(ENDSCR) %>%
      head(20) %>%
      print()
    writeLines("")
    fmttmp <- THREE %>%
      dplyr::mutate(
        dplyr::across(ENDSCR, ~ case_when(is.na(ENDSCR) | ENDSCR == "" ~ "Missing", TRUE ~ "Non-Missing"))
      )
    vfreq(fmttmp, c("DISP_SCR", "ENDSCR"), "If your country is using the dashboard, verify that there are no missing values for cases where DISP_SCR in (01, 02, 19)", runFreq = TRUE)
    if (!allMiss(THREE, c("ENDSCR"))) {
      dupRec(THREE, c("CASEID","ENDSCR"), combUniq = TRUE, dupMsg = "No CASEID with multiple ENDSCR found", dupTit = "All records within CASEID have the same ENDSCR",filter = "DISP_SCR %in% c('01', '02')", lnames = c("CASEID", "PERSID", "DISP_SCR", "ENDSCR"), chkLastMiss = FALSE)
    }
  }
  writeLines("")
  if (SAMPLE == "REGISTRY") {
    vfreq(THREE, c("PROB_OVERALL_HH"), "All records should have missing values", runFreq = TRUE)
  } else  {
    if (sum(THREE$PROB_OVERALL_HH > 0, na.rm = T) > 0) {
      THREE$DIFF_PROB_OVERALL_HH <- THREE$PROB_OVERALL_HH - THREE$PROB_OVERALL_HH_CHK
      vmean(THREE, c("PROB_OVERALL_HH", "PROB_OVERALL_HH_CHK", "DIFF_PROB_OVERALL_HH"), "Verify that there's no difference between these two variables", mtype = 2)
    } else {
      vmean(THREE, c("PROB_OVERALL_HH_CHK"), "Check: verify that these results match your calculations for the HH overall Probability of selection.", mtype = 2)     
   }
  }
  if (sum(THREE$PROB_OVERALL_PERS > 0, na.rm = T) > 0) {
    vmean(THREE, c("PROB_OVERALL_PERS", "PROB_OVERALL_PERS_CHK"), "Verify that there's no difference between these two variables", mtype = 2)
  } else {
    vmean(THREE, c("PROB_OVERALL_PERS_CHK"), "Check: verify that these results match your calculations for the Person overall Probability of selection.", mtype = 2)
  }
  
  if (SAMPLE == "REGISTRY") {
    vfreq(THREE, c("THEOR_HBWT"), "All records should have missing values", runFreq = TRUE)
  } else  {
    if (sum(THREE$THEOR_HBWT > 0, na.rm = T) > 0) {
      vmean(THREE, c("THEOR_HBWT", "THEOR_HBWT_CHK"), "Verify that there's no difference between these two variables", mtype = 3)
    } else {
      vmean(THREE, c("THEOR_HBWT_CHK"), "VCheck: verify that these results match your calculations for the theoretical HH base weight.", mtype = 3)
    }
  }  
  
  if (sum(THREE$THEOR_PBWT > 0, na.rm = T) > 0) {
    vmean(THREE, c("THEOR_PBWT", "THEOR_PBWT_CHK"), "Verify that there's no difference between these two variables", mtype = 3)  
  } else {
    vmean(THREE, c("THEOR_PBWT_CHK"), "Check: verify that these results match your calculations for the theoretical Person base weight", mtype = 3)
  }
  fmtTmp <- THREE %>% 
    mutate(DOBYY = case_when(DOBYY >= 1948 & DOBYY <= 1957 ~ "1948-1957",
                             DOBYY >= 1958 & DOBYY <= 2006 ~ "1958-2006",
                              TRUE ~ as.character(DOBYY)
    )) 
  if (allMiss(THREE, "DOBYY")) {
    vfreq(THREE, c("DOBYY"), "DOBYY not available for checks - IEA will derive it", runFreq = T)
  } else {
    vfreq(fmtTmp, c("DOBYY"), "Cases where DISP_CIBQ=1 - Verify that all values are non-missing", runFreq = TRUE, filter = "DISP_CIBQ %in% c('01')")
  }
  fmtTmp <- THREE %>%
    dplyr::arrange(DOBYY,DOBMM)
  printCond(fmtTmp, "PERSID,CI_AGE,CALCAGE,DOBYY,DOBMM", "Check that cases with DOBYY<=1957 or DOBYY>=2007 are correct", "DISP_CIBQ %in% c('01') & (DOBYY <= 1957 | DOBYY >= 2007) & !is.na(PERSID) & !is.na(DOBYY)")
  
  if (allMiss(THREE, "DOBMM")) {
    vfreq(THREE, c("DOBMM"), "DOBMM not available for checks - IEA will derive it", runFreq = T)
  } else {
    vfreq(THREE, c("DISP_CIBQ", "DOBMM"), "Verify that DOB month has valid values. DOBMM=(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) if DISP_CIBQ = 1", runFreq = TRUE)
  }

  fmtTmp <- THREE %>% 
    mutate(CALCAGE = case_when(CALCAGE >= 16 & CALCAGE <= 65 ~ "16-65",
                             TRUE ~ as.character(CALCAGE)
    ))     
  vfreq(fmtTmp, c("CALCAGE"), "Cases where DISP_CIBQ=1 - Verify that all values are non-missing, and review cases that fall outside the target age range", filter = "DISP_CIBQ %in% ('01')", runFreq = TRUE)
  printCond(THREE, "PERSID,CALCAGE,DOBMM,DOBYY", paste0("Cases outside the ", agebtm, "-", agetop, " range- please verify them"), paste0("(CALCAGE > ", agetop, " | CALCAGE < ", agebtm, " | is.na(CALCAGE)) & !is.na(PERSID) & DISP_CIBQ %in% ('01')"))
  
  if (allMiss(THREE, "DIAGERANGE")) {
    vfreq(THREE, c("DIAGERANGE"), "DIAGERANGE not available for checks - IEA will derive it")
  } else {
    vfreq(THREE, c("DISP_DS", "DIAGERANGE"), "Verify that DIAGERANGE is non-missing (01-06) for Doorstep interview respondents", runFreq = TRUE)
  }
  printCond(THREE, "PERSID,DISP_DS,DIAGERANGE", "Verify cases outside range of 16-65", "DISP_DS %in% c('01') & (DIAGERANGE < 1 | DIAGERANGE > 6 | is.na(DIAGERANGE)) & !is.na(PERSID)")  
  
  if (allMiss(THREE, "AGE_R")) {
    vfreq(THREE, c("AGE_R"), "AGE_R not available for checks - IEA will derive it", runFreq = T)
  } else {
    vfreq(THREE, list(c("AGE_R"), c("DISP_CIBQ", "DISP_DS", "AGE_R", "CI_AGE", "CALCAGE", "DIAGERANGE")), "Verify creation of AGE_R, and AGE_R should be nonmissing when DISP_CIBQ=1 or DISP_DS=1", runFreq = TRUE, filter = "DISP_CIBQ %in% c('01') | DISP_DS %in% c('01')")
  }
  
  printCond(THREE, "PERSID,CI_AGE,V_AGE,CALCAGE,AGE_R,DIAGERANGE", "List of occurrences where CALCAGE (BQ) differs from AGE (CI) by more than 1. Verify cases with large differences", "!is.na(CI_AGE) & abs(CALCAGE-CI_AGE)>1 &(DISP_CIBQ %in% c('01') | DISP_DS %in% c('01'))")
  
  if (allMiss(THREE, "IMPFLGAGER")) {
    vfreq(THREE, c("IMPFLGAGER"), "IMPFLGAGER not available for checks - IEA will derive it", runFreq = T)
  } else {
    vfreq(THREE, list(c("IMPFLGAGER"), c("DISP_CIBQ", "DISP_DS", "IMPFLGAGER", "AGE_R")), "Verify that IMPFLGAGER is not missing for cases where DISP_CIBQ=1 or DISP_DS=1, and that the age imputed is in the range 16-65", runFreq = TRUE, filter = "DISP_CIBQ %in% c('01') | DISP_DS %in% c('01')")
  }
  
  if (allMiss(THREE, "GENDER")) {
    vfreq(THREE, c("GENDER"), "GENDER not available for checks - IEA will derive it", runFreq = T)
  } else {
    vfreq(THREE, list(c("GENDER"), c("DISP_CIBQ", "DISP_DS", "GENDER", "CI_GENDER", "V_GENDER")), "Verify that GENDER is non-missing for cases where DISP_CIBQ=1 or DISP_DS=1", runFreq = TRUE, filter = "DISP_CIBQ %in% ('01') | DISP_DS %in% ('01')")
  }
  
  if (allMiss(THREE, "GENDER_R")) {
      vfreq(THREE, c("GENDER_R"), "GENDER_R not available for checks - IEA will derive it", runFreq = T)
    } else {
      vfreq(THREE, list(c("GENDER_R"), c("DISP_CIBQ", "DISP_DS", "GENDER_R", "CI_GENDER", "V_GENDER", "GENDER")), "Verify the creation of GENDER_R and that it is non-missing for cases where DISP_CIBQ=1 or DISP_DS=1", runFreq = TRUE, filter = "DISP_CIBQ %in% ('01') | DISP_DS %in% ('01')")
    }  
 
  THREE <- THREE %>%
    mutate(DISP_MAIN_CHK = case_when(
      DISP_CBA %in% c("01") | RSLTLOC %in% c(1) & DISP_CMP %in% c("01") ~ "01",
      DISP_CBA != "NA" ~ DISP_CBA,
      DISP_CBA == "NA" & DISP_CMP != "NA" ~ DISP_CMP,
      DISP_CBA == "NA" & DISP_CMP == "NA" & DISP_LOC != "NA"  ~ DISP_LOC,
      DISP_CBA == "NA" & DISP_CMP == "NA" & DISP_LOC == "NA"  & DISP_TUT != "NA" ~ DISP_TUT,
      TRUE ~ "NA"
    ))
  
    vfreq(THREE, c("DISP_MAIN"), ifelse(!allMiss(THREE, "DISP_MAIN"), "Check on DISP_MAIN", "DISP_MAIN not available for checks - IEA will derive it\nDISP_MAIN_CHK shows an initial derivation of DISP_MAIN. Please verify its values"), runFreq = T)
    if (!allMiss(THREE, "DISP_MAIN")) {
  vfreq(THREE, c("DISP_MAIN_CHK","DISP_MAIN","DISP_CBA", "RSLTLOC","DISP_CMP","DISP_LOC","DISP_TUT"), "Check the derivation of DISP_MAIN as indicated in Table 4-4 of the TS and G", runFreq = TRUE)  
    } else {
      vfreq(THREE, c("DISP_MAIN_CHK","DISP_CBA", "RSLTLOC","DISP_CMP","DISP_LOC","DISP_TUT"), runFreq = TRUE)     
  } 

    vfreq(THREE, c("COMPLETEFLG"), ifelse(!allMiss(THREE, "COMPLETEFLG"), "Check on COMPLETEFLG", "COMPLETEFLG not available for checks - IEA will derive it\nCOMPLETEFLG_CHK shows an initial derivation of COMPLETEFLG. Please verify its values"), runFreq = T)
    THREE <- THREE %>%
    mutate( COMPLETEFLG_CHK =
              case_when(
                DISP_CIBQ %in% ("01") & ATTMPTTUT == 1 & ATTMPTLOC == 1 ~ 1,
                DISP_DS %in% ('01') ~ 2,
                DISP_CIBQ %in% ('90') | DISP_MAIN %in% ('90') ~ 3,
                DISP_DS %in% ('90') ~ 4,
                DISP_CIBQ %in% ('01') & DISP_MAIN %in% c('06', '07', '08', '09') ~ 5,
                QCFLAG == 2 ~ 0,
                TRUE ~ 0
              )
    )
    if (!allMiss(THREE, "COMPLETEFLG")) {
      completeVar <- c("COMPLETEFLG_CHK","COMPLETEFLG")
      completeStr <- "COMPLETEFLG"
    } else {
      completeVar <- c("COMPLETEFLG_CHK")
      completeStr <- "COMPLETEFLG_CHK"
    }
    vfreq(THREE, c(completeVar, "DISP_CIBQ","ATTMPTTUT","ATTMPTLOC","DISP_DS","DISP_MAIN", "QCFLAG"), paste0("Verify the creation of ", completeStr, " - as defined in TSG 4.3.3."), runFreq = TRUE)
    vfreq(THREE, c("WEIGHTFLG_CHK", completeVar), "All cases with WEIGHTFLG_CHK = 1 will receive a final weight.", runFreq = TRUE, filter = "WEIGHTFLG_CHK == 1")  
    
    
    vfreq(THREE, c("WEIGHTFLG"), ifelse(!allMiss(THREE, "WEIGHTFLG"), "Check on WEIGHTFLG", "WEIGHTFLG not available for checks - IEA will derive it\nWEIGHTFLG_CHK shows an initial derivation of WEIGHTFLG. Please verify its values"), runFreq = T)
    
    vfreq(THREE, c(ifelse(allMiss(THREE, "WEIGHTFLG"), "WEIGHTFLG_CHK", c("WEIGHTFLG_CHK", "WEIGHTFLG")), "DISP_CIBQ", "DISP_DS"), paste0("Verify that cases with DISP_CIBQ = 1, or (DISP_CIBQ = 7 and DISP_DS = 1) have a value of ", ifelse(allMiss(THREE, "WEIGHTFLG"), "WEIGHTFLG_CHK", "WEIGHTFLG"), " = 1 - cases to receive a weight."), runFreq = T)
    
    if (SAMPLE == "REGISTRY") {
      THREE <- THREE %>%
        mutate( EXCFLG_CHK =
                  case_when(
                    DISP_CIBQ %in% ('18') & REGFLG == 1 ~ 9,
                    DISP_CIBQ %in% ('25') & REGFLG %in% c(2,3) ~ 9,
                    DISP_CIBQ %in% ('23') & REGFLG %in% c(4,5,6) ~ 1,
                    DISP_CIBQ %in% ('20') & REGFLG == 7 ~ 2,
                    DISP_CIBQ %in% ('22') & REGFLG == 8 ~ 1,
                    TRUE ~ 0
                  )
        )      
      vfreq(THREE, c("EXCFLG_CHK", "EXCFLG"), "EXCFLG = (0, 1, 2, 9)", runFreq = T)
      vfreq(THREE, c("DISP_CIBQ", "EXCFLG", "EXCFLG_CHK", "REGFLG"), "Compare values to those in Table 4-6 on Annex 4-1 of the TS and G", runFreq = TRUE)
    } else {
      vfreq(THREE, c("EXCFLG"), "EXCFLG should be missing for Screener countries", runFreq = TRUE)      }
    
    
  if (SAMPLE == "REGISTRY") {
      vfreq(THREE, list(c("REGFLG"), c("DISP_CIBQ","EXCFLG", "REGFLG")), "Verify REGFLG values vs. Table 4-6 on Annex 4-1 of the TS and G", runFreq = TRUE)
  } else {
      vfreq(THREE, c("REGFLG"), "REGFLG - Verify that all cases have missing values", runFreq = TRUE)
  }
    
  vfreq(THREE, list(c("TECHPROB"), c("TECHPROB", "DISP_CIBQ", "DISP_MAIN", ifelse(allMiss(THREE, "WEIGHTFLG"), "WEIGHTFLG_CHK", "WEIGHTFLG"))), "Verify the values - There shouldn't be too many cases with technical problems", runFreq = TRUE)
  
  vfreq(THREE, ifelse(allMiss(THREE,"TRIMGRPS") | sum(THREE$TRIMGRPS == 1, na.rm = T) == nrow(THREE), c("TRIMGRPS"), list(c("TRIMGRPS"), c("TRIMGRPS", "SAMPTYPE"), c("TRIMGRPS","STRAT_PSU","STRAT_SSU","STRAT_HH","STRAT_PERS"))), "Verify that the trim groups were set according to the oversampling domains or strata; It should be set to 1 if no oversampling", runFreq = TRUE)
  
  if (allMiss(THREE, c("WEIGHTFLG"))) {
    vfreq(THREE, c("WEIGHTFLG_CHK","RAKEDIM1"), "Verify that RAKEDIM1 is not missing for cases where WEIGHTFLG_CHK=1 - You need to provide this variable", runFreq = TRUE)
    for (rdim in 2:7) {
      vfreq(THREE, c("WEIGHTFLG_CHK",paste0("RAKEDIM", rdim)), paste0("If provided by the country, verify that RAKEDIM", rdim, " is not missing for cases where WEIGHTFLG_CHK=1"), runFreq = TRUE)
    }
  } else {
    vfreq(THREE, c("WEIGHTFLG","RAKEDIM1"), "Verify that RAKEDIM1 is not missing for cases where WEIGHTFLG=1 - You need to provide this variable", runFreq = TRUE)
    for (rdim in 2:7) {
      vfreq(THREE, c("WEIGHTFLG",paste0("RAKEDIM", rdim)), paste0("If provided by the country, verify that RAKEDIM", rdim, " is not missing for cases where WEIGHTFLG=1"), runFreq = TRUE)
    }
    
  }
  
  vfreq(THREE, strsplit(paste(paste0("PERSVAR", 1:5), sep = "*"), "*", fixed = TRUE), c(list("Verify that values are 0, 1, ..., 9, and that there's no missing values"), rep(list("If provided by country, verify that values are 0, 1, ..., 9, and that there's no missing values"), 4)), runFreq = TRUE, filter = paste0("!is.na(PERSID)", ifelse(SAMPLE == "SCREENER"," & DISP_SCR %in% c('01','02')", "")))
  
  if (SAMPLE == "REGISTRY") {
    vfreq(THREE, strsplit(paste(paste0("DUVAR_SCRRESP", 1:5), sep = "*"), "*", fixed = TRUE), "All values should be missing", runFreq = TRUE)    
  } else  {
    vfreq(THREE, strsplit(paste("DISP_SCR", paste0("DUVAR_SCRRESP", 1:5), sep = "*"), "*", fixed = TRUE), "If provided by the country, verify that there are no missing values if DISP_SCR in (1,2). Since DUVAR_SCRRESP1-5 are optional, they all can be missing", runFreq = TRUE, filter = "DISP_SCR %in% c('01', '02')")   
    
  }
  if (SAMPLE == "REGISTRY") {
    for (duv in 1:5) {
      vfreq(THREE, paste0("DUVAR_ALL", duv), paste0("DUVAR_ALL", duv, " - Optional variable. If provided by the country, verify that there are no missing values for all sampled persons"), runFreq = TRUE)    
    }
    
  } else {
    for (duv in 1:5) {
      vfreq(THREE, paste0("DUVAR_ALL", duv), paste0("DUVAR_ALL", duv, " - Optional variable. If provided by the country, verify that there are no missing values for all sampled DUs"), runFreq = TRUE)    
    }
  }
  for (acnt in 1:5) {
    vfreq(THREE, paste0("AREAVAR", acnt), paste0("If AREAVAR", acnt," provided, verify that there are no missing values for all sampled ", ifelse(SAMPLE == "REGISTRY", "persons", "DUs")), runFreq = TRUE, filter = ifelse(SAMPLE == "REGISTRY", "!is.na(PERSID)", "!is.na(ID_HH)") )    
  }
  
  vfreq(THREE, list(c("IMPFLGAG"), c("IMPFLGAG", "CI_AGE")), "Verify that there is a reasonable number of cases imputed", runFreq = TRUE)  
  
  vfreq(THREE, list(c("IMPFLGGE"), c("IMPFLGGE", "CI_GENDER")), "Verify that there is a reasonable number of cases imputed", runFreq = TRUE)  
  
  WgtFlg <- "WEIGHTFLG"
  if (allMiss(THREE, "WEIGHTFLG")) {
    WgtFlg <- "WEIGHTFLG_CHK"
  }
  vfreq(THREE, strsplit(unlist(strsplit(paste(paste(WgtFlg, paste0("IFLG_PERSVAR", 1:5), sep = "*"), paste(WgtFlg, paste0("IFLG_PERSVAR", 1:5), paste0("PERSVAR", 1:5), sep = "*"), sep = "|"), "|", fixed = TRUE)), "*", fixed = T), "Verify that there is a reasonable number of cases imputed for weighting", runFreq = TRUE )    
  if (SAMPLE == "REGISTRY") {
    vfreq(THREE, strsplit(unlist(strsplit(paste(paste(paste0("IFLG_DUVAR_SCRRESP", 1:5), sep = "*"), paste(paste0("IFLG_DUVAR_SCRRESP", 1:5), paste0("DUVAR_SCRRESP", 1:5), sep = "*"), sep = "|"), "|", fixed = TRUE)), "*", fixed = T), "Verify that all values are missing", runFreq = TRUE )    
  } else {
    vfreq(THREE, strsplit(unlist(strsplit(paste(paste(WgtFlg,paste0("IFLG_DUVAR_SCRRESP", 1:5), sep = "*"), paste(WgtFlg, paste0("IFLG_DUVAR_SCRRESP", 1:5), paste0("DUVAR_SCRRESP", 1:5), sep = "*"), sep = "|"), "|", fixed = TRUE)), "*", fixed = T), "Verify that there is a reasonable number of cases imputed for weighting", runFreq = TRUE )    
  }  

  vfreq(THREE, strsplit(unlist(strsplit(paste(paste(WgtFlg,paste0("IFLG_DUVAR_ALL", 1:5), sep = "*"), paste(WgtFlg, paste0("IFLG_DUVAR_ALL", 1:5), paste0("DUVAR_ALL", 1:5), sep = "*"), sep = "|"), "|", fixed = TRUE)), "*", fixed = T), "Verify that there is a reasonable number of cases imputed for weighting", runFreq = TRUE ) 
  
  vfreq(THREE, strsplit(unlist(strsplit(paste(paste(paste0("IFLG_CURBOBS", 1:3), sep = "*"), paste(WgtFlg, paste0("IFLG_CURBOBS", 1:3), c("CURBOBS1","CURBOBS2_01*CURBOBS2_02*CURBOBS2_03*CURBOBS2_04","CURBOBS3" ), sep = "*"), sep = "|"), "|", fixed = TRUE)), "*", fixed = T), "Verify that there is a reasonable number of cases imputed for weighting", runFreq = TRUE )    

  vfreq(THREE, strsplit(unlist(strsplit(paste(paste(WgtFlg,paste0("IFLG_AREAVAR", 1:5), sep = "*"), paste(WgtFlg, paste0("IFLG_AREAVAR", 1:5), paste0("AREAVAR", 1:5), sep = "*"), sep = "|"), "|", fixed = TRUE)), "*", fixed = T), "Verify that there is a reasonable number of cases imputed for weighting", runFreq = TRUE ) 

  vfreq(THREE, strsplit(unlist(strsplit(paste(paste(WgtFlg,paste0("IFLG_RAKEDIM", 1:7), sep = "*"), paste(WgtFlg, paste0("IFLG_RAKEDIM", 1:7), paste0("RAKEDIM", 1:7), sep = "*"), sep = "|"), "|", fixed = TRUE)), "*", fixed = T), "Verify that there is a reasonable number of cases imputed for weighting", runFreq = TRUE ) 
  
  cat("Checks on actual yields - Table 4-2 in Annex 4-1 of the TS and G\n")
  if (SAMPLE == "SCREENER") {
    cat("Actual Screener Yield; cases where DISP_SCR(01,02)              = ", THREE %>%
          dplyr::filter(DISP_SCR %in% c('01','02')) %>%
          nrow(), "\n")    
  }

  cat("Actual BQ Yield; cases where DISP_CIBQ(01, 90) + DISP_DS(01,90) = ", THREE %>%
        dplyr::filter(DISP_CIBQ %in% c("01", "90") | DISP_DS %in% c("01", "90")) %>%
        nrow(), "\n")  
  
  fmtTmp <- THREE %>%
    mutate(cmpYield0 = case_when(DISP_CIBQ %in% c("01", "90") & (ATTMPTTUT %in% (1) & ATTMPTLOC %in% (1)) ~ 1,
                                 TRUE ~ 0))
  fmtTmp <- fmtTmp %>%
    mutate(cmpYield1 = case_when(cmpYield0 == 0 & (DISP_DS %in% c("01","90") | DISP_MAIN %in% c("06","07","08","09")) ~ 1,
                                 TRUE ~ 0))
  
  fmtTmp <- fmtTmp %>%
    mutate(cmpYield2 = case_when(cmpYield1 == 0 & cmpYield0 == 0 & (DISP_CIBQ %in% ("90") | DISP_MAIN %in% ("90")) ~ 1,
                                 TRUE ~ 0))  
  cat("{ATTMPTTUT(1) + ATTMPTLOC(1)} E (Cb)                            = ", sum(fmtTmp$cmpYield0), "\n") 
  cat("Min(Cap,DISP_DS(01,90) + DISP_MAIN(06,07,08,09))                = ", sum(fmtTmp$cmpYield1), "\n") 
  cat("DISP_CIBQ(90) or DISP_MAIN(90)                                  = ", sum(fmtTmp$cmpYield2), "\n") 
  cat("Actual Completed Cases Yield; see Table 4-2 for cases counted   = ", sum(fmtTmp$cmpYield0) + min(CAP,sum(fmtTmp$cmpYield1)) + sum(fmtTmp$cmpYield2), "\n") 
  cat("This takes into account your AICs, which for", COUNTRYNAME, "is", paste(rep(" ", 14 - nchar(COUNTRYNAME)), collapse = ""), "= ", CAP, "\n")
  
  cat("Actual Assessment Yield; DISP_MAIN(01,07-09, 90)                = ", THREE %>%
        dplyr::filter(DISP_MAIN %in% c("90", "01", "07", "08", "09")) %>%
        nrow(), "\n\n")  
  
  calRR <- function(dsn, wname) {
    wgtflag <- 1
    wstr <- "Weighted "
    if (typeof(wname) == "double") {
      wgtflag <- 0
      dsn[, "dummy"] <- rep(wname, nrow(dsn))
      wname <- "dummy"
      wstr <- "Unweighted "
    }
    if (SAMPLE == "REGISTRY") {
      if (wgtflag) {
        wname <- "THEOR_PBWT"
      }
      bq <- dsn %>%
        select(all_of(c("DISP_CIBQ", "DISP_DS", "PERSID", "SUBSAMP", "EXCFLG", "EXCFRM_PROP", wname)))
      c_b <- bq %>%
        filter(DISP_CIBQ %in% c("01", "90")) %>%
        summarise(c_b = sum(eval(parse(text = wname)), na.rm = TRUE))
      ds_b <- bq %>%
        filter(DISP_CIBQ %in% c("07") & DISP_DS %in% c("01", "90")) %>%
        summarise(ds_b = sum(eval(parse(text = wname)), na.rm = TRUE))
      sp_b <- bq %>%
        filter(!is.na(PERSID) & SUBSAMP %in% (1:9) & !DISP_CIBQ %in% c("27") ) %>%
        summarise(sp_b = sum(eval(parse(text = wname)), na.rm = TRUE))
      d_b <- bq %>%
        filter(DISP_CIBQ %in% c("12","13","15","16")) %>%
        summarise(d_b = sum(eval(parse(text = wname)), na.rm = TRUE))
      lrn_b <- bq %>%
        filter(DISP_CIBQ %in% c("07") & DISP_DS %in% c("07") | DISP_CIBQ %in% c("08","09")) %>%
        summarise(lrn_b = sum(eval(parse(text = wname)), na.rm = TRUE))
      i_b <- bq %>%
        filter(DISP_CIBQ %in% c("18","25")) %>%
        summarise(i_b = sum(eval(parse(text = wname)), na.rm = TRUE))
      u_b <- bq %>%
        filter(EXCFLG %in% c(2)) %>%
        summarise(u_b = sum(eval(parse(text = wname)), na.rm = TRUE))
      if ("EXCFLG" %in% names(THREE)) {
        k_b <- bq %>%
          filter(EXCFLG %in% c(0, 1, 9)) %>%
          summarise(k_b = sum(eval(parse(text = wname)), na.rm = TRUE))  
      } else {
        k_b <- bq %>%
          filter(DISP_CIBQ %in% c("20")) %>%
          summarise(k_b = sum(eval(parse(text = wname)), na.rm = TRUE))
        k_b <- sum(sp_b, -1 * k_b, na.rm = TRUE)  
      }
      excflg_b <- bq %>%
        filter(EXCFLG %in% c(1)) %>%
        summarise(excflg_b = sum(eval(parse(text = wname)), na.rm = TRUE))      
      excflg_b19 <- bq %>%
        filter(EXCFLG %in% c(1, 9)) %>%
        summarise(excflg_b19 = sum(eval(parse(text = wname)), na.rm = TRUE)) 
      
      EXCFRM_PROP <- unique(bq$EXCFRM_PROP)
      resbq <- as.data.frame(t(unlist(expand.grid(c_b, ds_b, sp_b, d_b, lrn_b, i_b, u_b, k_b, excflg_b, excflg_b19, EXCFRM_PROP))))
      resbq <- resbq %>%
        mutate(
          eligible = Var3.sp_b - Var4.d_b - Var5.lrn_b - Var6.i_b - Var7.u_b *((Var4.d_b + Var5.lrn_b + Var6.i_b) / Var8.k_b),
          exc_prop = min((Var9.excflg_b + Var8.k_b * (Var9.excflg_b / Var10.excflg_b19)) / eligible, 0.05 - Var11),
          exclude = eligible * exc_prop,
          complete = Var1.c_b + Var2.ds_b,
          bq_rr = complete / (eligible - exclude))
      
      resbq <- resbq[-11]
      cat(paste0(wstr, "BQ RR components",  ifelse(wgtflag, paste0(" - ", wname), ""), "\n" ))
      names(resbq) <- c("C_B", "DS_B", "SP_B", "D_B","LRN_B","I_B","U_B","K_B","EXCFLG_B","EXCFLG_B19","ELIGIBLE","EXC_PROP","EXCLUDE","COMPLETE", "BQ_RR")
      print(resbq) 
      writeLines("")
    } else {
      ss <- dsn %>%
        filter(!is.na(CASEID) & (EXCFLG != 1 | is.na(EXCFLG)))
      if (nrow(ss) > 0) {
        ss <- ss %>%
          select(all_of(c("CASEID", "DISP_SCR", "QCFLAG", "EXCFLG", wname))) %>%
          distinct(CASEID, .keep_all = TRUE)
        
        c_s <- ss %>%
          filter(DISP_SCR %in% c("01", "02")) %>%
          summarise(c_s = sum(eval(parse(text = wname)), na.rm = TRUE))
        
        hh_s <- ss %>%
          filter(!DISP_SCR %in% c("27")) %>%
          summarise(hh_s = sum(eval(parse(text = wname)), na.rm = TRUE))
        
        i_va <- ss %>%
          filter(DISP_SCR %in% c("22","26","28")) %>%
          summarise(i_va = sum(eval(parse(text = wname)), na.rm = TRUE))
        u_va <- ss %>%
          filter(DISP_SCR %in% c("05", "20" ,"21")) %>%
          summarise(u_va = sum(eval(parse(text = wname)), na.rm = TRUE))
        k_va <- ss %>%
          filter(suppressWarnings(as.numeric(DISP_SCR)) %in% c(1:4, 7, 9, 17, 19, 22, 24, 26, 28, 12:16)) %>%
          summarise(k_va = sum(eval(parse(text = wname)), na.rm = TRUE))
        i_age <- ss %>%
          filter(DISP_SCR %in% c("19")) %>%
          summarise(i_age = sum(eval(parse(text = wname)), na.rm = TRUE))
        u_age <- ss %>%
          filter(DISP_SCR %in% c("04", "17", "24")) %>%
          summarise(u_age = sum(eval(parse(text = wname)), na.rm = TRUE))
        k_age <- ss %>%
          filter(suppressWarnings(as.numeric(DISP_SCR)) %in% c(1:3, 7, 9, 19, 12:16)) %>%
          summarise(k_age = sum(eval(parse(text = wname)), na.rm = TRUE))
          
        # res <- expand.grid(scr_rr, complete, eligible, c_s, hh_s, i_va, u_va, k_va, i_age, u_age, k_age)
        res <- as.data.frame(t(unlist(expand.grid(c_s, hh_s, i_va, u_va, k_va, i_age, u_age, k_age))))
        
        res <- res %>%
          mutate(
              eligible = Var2.hh_s - Var3.i_va - (Var4.u_va * (Var3.i_va / Var5.k_va)) - Var6.i_age - (Var4.u_va * (1 - (Var3.i_va / Var5.k_va)) + Var7.u_age) * (Var6.i_age / Var8.k_age), 
              complete = Var1.c_s,
              scr_rr = complete / eligible)
        cat(paste0(wstr, "SCR RR components", ifelse(wgtflag, paste0(" - ", wname), ""), "\n" ))
        names(res) <- c("C_S","HH_S","I_VA"," U_VA"," K_VA"," I_AGE","U_AGE","K_AGE","ELIGIBLE","COMPLETE","SCR_RR")  
       print(res)
       writeLines("")
       if (wgtflag) {
        wname <- "THEOR_PBWT"
       }
        bq <- dsn %>%
          select(all_of(c("DISP_CIBQ", "DISP_DS", "PERSID", "SUBSAMP", wname)))
        c_b <- bq %>%
          filter(DISP_CIBQ %in% c("01", "90")) %>%
          summarise(c_b = sum(eval(parse(text = wname)), na.rm = TRUE))
        ds_b <- bq %>%
          filter(DISP_CIBQ %in% c("07") & DISP_DS %in% c("01", "90")) %>%
          summarise(ds_b = sum(eval(parse(text = wname)), na.rm = TRUE))
        if (COUNTRY %in% c("CAN", "GBR")) {
          sp_b <- bq %>%
            filter(!is.na(PERSID) & SUBSAMP %in% (1:9) & !DISP_CIBQ %in% c("27") & DISP_SCR %in% c("01", "02")) %>%
            summarise(sp_b = sum(eval(parse(text = wname)), na.rm = TRUE))
        } else {
          sp_b <- bq %>%
            filter(!is.na(PERSID) & SUBSAMP %in% (1:9) & !DISP_CIBQ %in% c("27") ) %>%
            summarise(sp_b = sum(eval(parse(text = wname)), na.rm = TRUE))
        }
        d_b <- bq %>%
          filter(DISP_CIBQ %in% c("12","13","15","16")) %>%
          summarise(d_b = sum(eval(parse(text = wname)), na.rm = TRUE))
        lrn_b <- bq %>%
          filter((DISP_CIBQ %in% c("07") & DISP_DS %in% c("07")) | DISP_CIBQ %in% c("08","09")) %>%
          summarise(lrn_b = sum(eval(parse(text = wname)), na.rm = TRUE))
        i_b <- bq %>%
          filter(DISP_CIBQ %in% c("18","25")) %>%
          summarise(i_b = sum(eval(parse(text = wname)), na.rm = TRUE))
        resbq <- as.data.frame(t(unlist(expand.grid(c_b, ds_b, sp_b, d_b, lrn_b, i_b))))
        resbq <- resbq %>%
          mutate(
            complete = Var1.c_b + Var2.ds_b,
            eligible = Var3.sp_b - Var4.d_b - Var5.lrn_b - Var6.i_b, 
            bq_rr = complete / eligible)
          #print(res$scr_rr)
        cat(paste0(wstr, "BQ RR components",  ifelse(wgtflag, paste0(" - ", wname), ""), "\n" ))
        names(resbq) <- c("C_B","DS_B","SP_B","D_B","LRN_B","I_B","COMPLETE","ELIGIBLE","BQ_RR")
        print(resbq)
        writeLines("")
      }

    }      
    #print(resbq$bq_rr)

    ass <- dsn
    c_a <- ass %>%
      filter(DISP_MAIN %in% c("01","90")) %>%
      summarise(c_a = sum(eval(parse(text = wname)), na.rm = TRUE))
    lr_a <- ass %>%
      filter(DISP_MAIN %in% c("07","08", "09")) %>%
      summarise(lr_a = sum(eval(parse(text = wname)), na.rm = TRUE))
    c_b <- ass %>%
      filter(DISP_CIBQ %in% c("01","90")) %>%
      summarise(c_b = sum(eval(parse(text = wname)), na.rm = TRUE))
    d_a <- ass %>%
      filter(DISP_MAIN %in% c("12","13", "15", "16")) %>%
      summarise(d_a = sum(eval(parse(text = wname)), na.rm = TRUE))
    t_a <- ass %>%
      filter(DISP_MAIN %in% c("06")) %>%
      summarise(t_a = sum(eval(parse(text = wname)), na.rm = TRUE))
    i_a <- ass %>%
      filter(DISP_MAIN %in% c("18")) %>%
      summarise(i_a = sum(eval(parse(text = wname)), na.rm = TRUE))
    resAss <- as.data.frame(t(unlist(expand.grid(c_a, lr_a, c_b, d_a, t_a, i_a))))
    resAss <- resAss %>%
      mutate(
        complete = Var1.c_a + Var2.lr_a,
        eligible = Var3.c_b - Var4.d_a - Var5.t_a -  Var6.i_a, 
        assmnt_rr = complete / eligible) 
    cat(paste0(wstr, "ASSESSMENT RR components",  ifelse(wgtflag, paste0(" - ", wname), ""), "\n" ))
    names(resAss) <- c( "C_A","LR_A","C_B","D_A","T_A","I_A","COMPLETE","ELIGIBLE", "ASSMNT_RR")
    print(resAss)
    writeLines("")
    if (SAMPLE == "REGISTRY") {
      resAll <- as.data.frame(cbind(resbq$BQ_RR, resAss$ASSMNT_RR))
      names(resAll) <- c("BQ_RR", "ASSMNT_RR")
      resAll <- resAll %>%
        mutate(
          ORR1 = BQ_RR * 100,
          ORR2 = BQ_RR * ASSMNT_RR * 100)
      
    } else  {
      resAll <- as.data.frame(cbind(res$SCR_RR, resbq$BQ_RR, resAss$ASSMNT_RR))
      names(resAll) <- c("SCR_RR", "BQ_RR", "ASSMNT_RR")
      resAll <- resAll %>%
        mutate(
          ORR1 = SCR_RR * BQ_RR * 100,
          ORR2 = SCR_RR * BQ_RR * ASSMNT_RR * 100)
      resAll$SCR_RR <- resAll$SCR_RR * 100
    }   
    resAll$BQ_RR <- resAll$BQ_RR * 100
    resAll$ASSMNT_RR <- resAll$ASSMNT_RR * 100
    if (wgtflag) {
      print(paste0("Weighted Response Rates Summary - ", SAMPLE))
    } else {
      print(paste0("Unweighted Response Rates Summary - ", SAMPLE))
    }
    #print(colnames(resAll))  
    
    resALL1 <- as.data.frame(strsplit(sprintf("%0.1f%%", resAll), " ", fixed = T))
    names(resALL1) <- names(resAll)
    print(resALL1)
    cat("\n")
    
  }
  
  calRR(THREE, "THEOR_HBWT")  
  calRR(THREE, 1)
  if (SAMPLE == "SCREENER") {
    cat("Listing of the largest 20 theoretical household base weight")
    THREE %>%
      dplyr::select(ID_PSU, ID_SSU, ID_HH, ID_OTH, THEOR_HBWT, PROB_PSU, PROB_SSU, PROB_HH, PROB_OTH) %>%
      dplyr::filter(THEOR_HBWT>0) %>%
      dplyr::arrange(desc(THEOR_HBWT), ID_PSU, ID_SSU, ID_HH, ID_OTH) %>%
      head(20) %>%
      print()
    writeLines("")
    
    cat("Listing of the smallest 20 PROB_PERS values")
    THREE %>%
      dplyr::select(ID_PSU, ID_SSU, ID_HH, CASEID, ID_OTH, PROB_SMPFLG1, PROB_SMPFLG2, PROB_SMPFLG3, PROB_PERS, NUMELG1, NUMSEL1, NUMELG2, NUMSEL2, NUMELG3, NUMSEL3) %>%
      dplyr::filter(!is.na(CASEID) & PROB_PERS>0) %>%
      dplyr::arrange(!is.na(PROB_PERS), PROB_PERS, ID_PSU, ID_SSU, ID_HH, CASEID, ID_OTH, PROB_SMPFLG1, PROB_SMPFLG2, PROB_SMPFLG3) %>%
      head(20) %>%
      print()
    writeLines("")
    
    cat("Listing of the largest 20 PROB_PERS values\n")
    THREE %>%
      dplyr::select(ID_PSU, ID_SSU, ID_HH, CASEID, ID_OTH, PROB_SMPFLG1, PROB_SMPFLG2, PROB_SMPFLG3, PROB_PERS, NUMELG1, NUMSEL1, NUMELG2, NUMSEL2, NUMELG3, NUMSEL3) %>%
      dplyr::filter(!is.na(CASEID) & PROB_PERS>0) %>%
      dplyr::arrange(desc(PROB_PERS),ID_PSU, ID_SSU, ID_HH, CASEID, ID_OTH, PROB_SMPFLG1, PROB_SMPFLG2, PROB_SMPFLG3) %>%
      head(20) %>%
      print()    
    writeLines("")
  }
  cat("Listing of the largest 20 theoretical person base weight\n")
  THREE %>%
    dplyr::select(ID_PSU, ID_SSU, ID_HH, ID_OTH, PERSID, THEOR_PBWT, PROB_PSU, PROB_SSU, PROB_HH, PROB_OTH, PROB_PERS ) %>%
    dplyr::filter(THEOR_PBWT>0) %>%
    dplyr::arrange(desc(THEOR_PBWT), ID_PSU, ID_SSU, ID_HH, ID_OTH, PERSID) %>%
    head(20) %>%
    print()
  writeLines("")
  
  print(DATA_NAME)
  print(str(THREE, list.len = ncol(THREE)))
}

