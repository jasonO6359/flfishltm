#' @import tidyr dplyr tibble 
#' @import ggplot2 viridis stringr
NULL
###############
## Functions ##
###############


################################################################################
################################################################################
#' LTM Data Summary
#' 
#' Summarize raw LTM data from "Standard Fish Query"
#' @param waterbodyname The name of the selected Waterbody as you would like it to appear in filenames and figure titles
#' @param file path to the raw query file, should be a .csv file
#' @param outtables option to specify which summary tables to save to working directory, default 0 exports no summary tables
#' @param printfigs option to specify which figures to save to print directory, default 0 prints no figures, 1 prints all figures
#' @param print_directory specify directory to save exported figures
#' @return list of summarized datasets
#' @examples
#' #Import demo data for Newnans Lake
#' data(newnans)
#' #dplyr::summarise Newnans Lake data
#' newn_sum <- ltm.data.summary("Newnans Lake", file = newnans)
#' # Access summary tables within the newn_sum object
#' newn_sum$SpeciesList
#' newn_sum$CPUE_number
#' newn_sum$RawData
#' newn_sum$Comp_num
#' @export
ltm.data.summary <- function(waterbodyname = "No Waterbody Specified",
                             file,
                             outtables = 0,
                             printfigs = 0,
                             print_directory = getwd()) {
# Import Data
  print("Importing Data")
  ImpData <- function(datafile) {
      #datafile = "Electrofishing/LMB/Orange_LMB_2021.csv"
        if(typeof(file) == "list") {rawdat = data.frame(file)} else {
        rawdat <- data.frame(read.csv(datafile, header = TRUE, colClasses = c(
        ID = 'character',
        WaterBody = 'factor',
        County = 'character',
        Date = 'character',
        Time = 'character', #revist
        SamplingType = 'factor',
        Target = 'factor',
        Season = 'factor',
        Gear = 'factor',
        Effort = 'integer',
        DistanceM = 'numeric',
        Site = 'character',
        BeginLat = "numeric",
        BeginLong = 'numeric',
        Stratum = 'factor',
        Grid = 'factor',
        MiniGrid = 'factor',
        Segment = 'factor',
        NumSegments = 'character',
        Reach = 'factor',
        TransType = 'factor',
        FS_ID = 'integer',
        FM_ID = 'integer',
        SpeciesCode = "factor",
        SpeciesCommon = 'factor',
        SpeciesScientific = 'factor',
        EcoType = 'factor',
        SizeClass ="factor",
        Count = 'integer',
        TotalLength = 'integer',
        TL_CM_Group = 'integer',
        TotalWeight = 'numeric',
        IsBatchWt = 'factor',
        BatchWtValue = 'numeric',
        ExcludeWt = 'factor',
        Sex = 'factor',
        Tag = 'character',
        FishNote = 'character',
        SequenceNote = 'character',
        FishHealthCode = 'character')))

      # names(rawdat) = c("ID",
      #                   "WaterBody" ,
      #                   "County",
      #                   "Date",
      #                   "Time",
      #                   "Type",
      #                   "Target",
      #                   "Season",
      #                   "Gear" ,
      #                   "Effort" ,
      #                   "DistanceM",
      #                   "Site"  ,
      #                   "BeginLat",
      #                   "BeginLong",
      #                   "Stratum",
      #                   "Grid",
      #                   "Minigrid",
      #                   "Segment",
      #                   "NumSegs",
      #                   "Reach",
      #                   "TransType",
      #                   "FS_ID",
      #                   "FM_ID",
      #                   "Species",
      #                   "SpeciesCommon",
      #                   "SpeciesSci",
      #                   "Ecotype",
      #                   "SizeClass",
      #                   "Count",
      #                   "TotalLength",
      #                   "TL_CM_Group" ,
      #                   "TotalWeight" ,
      #                   "IsBatchWt",
      #                   "BatchWtValue",
      #                   "ExcludeWt",
      #                   "Sex",
      #                   "Tag",
      #                   "FishNote",
      #                   "SequenceNote")
        }
    Targets = unique(rawdat$Target)
    
    #Replace undesirable characters in Species Common Names
    rawdat$SpeciesCommon = stringr::str_replace_all(rawdat$SpeciesCommon, "/", "-")
    rawdat$SpeciesCommon = stringr::str_replace_all(rawdat$SpeciesCommon, " x ", "-")
    
    outlist <- list(rawdat=rawdat)
    if("ALL" %in% Targets) {outlist$community = rawdat %>% 
                                                  dplyr::filter(Target == "ALL")}
    
    if("LMB" %in% Targets) {outlist$LMB = rawdat %>% dplyr::filter(Target == "LMB")}
    
    if("BLCR" %in% Targets) {outlist$LMB = rawdat %>% dplyr::filter(Target == "BLCR")}
    
    
    #outlist$community <- outlist$community %>% dplyr::filter(SpeciesCode != "MOSQ")
    return(outlist)
  }
  
# Create Summary Tables
  print("Creating Summary Tables")
  process_data <- function(rawfile,OutTab = 0, name) {
    #Function to add missing years into final summary table outputs
    addgapyears <- function(sumTable) {
      sumTable$Year <- as.character(sumTable$Year)
      temp <- list(Row = c(), Gap = c())
      temp$Row <- c(1:nrow(sumTable))
      temp$Gap1 <- c(sumTable$yr)
      temp$Gap2 <- c(sumTable$yr[2:length(sumTable$yr)],NA)
      temp <- data.frame(cbind(temp$Row, temp$Gap1, temp$Gap2))
      names(temp) <- c("Row", "Gap1", "Gap2")
      temp$Gap <- temp$Gap2 - temp$Gap1
      temp <- na.omit(temp)
      origLen <- nrow(sumTable)
      
      for (i in 1:nrow(temp)) { if(temp$Gap[i] > 1) {
        irow <- temp$Row[i]
        igap <- temp$Gap[i]
        chop <- c(1:irow)
        chop2 <- c((irow+1):nrow(sumTable))
        addedrows <- c((nrow(sumTable)+1):(nrow(sumTable)+(igap-1)))
        neworder <- c(chop,addedrows,chop2)
        sumTable = rbind(sumTable, matrix(ncol = ncol(sumTable),
                                          nrow = (temp$Gap[i]-1),
                                          dimnames = list(c((nrow(sumTable)+ 1):(nrow(sumTable) + 
                                                                                   (temp$Gap[i]-1))),c(names(sumTable)))))
        sumTable <- sumTable[neworder,]}}
      for (j in 1:nrow(sumTable)) {
        if(is.na(sumTable$Year[j])) {
          sumTable$Year[j] = base::paste((as.numeric(substr(sumTable$Year[j-1],1,4))+1),"-",(as.numeric(substr(sumTable$Year[j-1],1,4))+2))
          }
      }
      return(sumTable)
    }
    
    #create temp data
    #rawfile=outlist$LMB
    dat <- rawfile
    # dat = ds
    dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")

    #Create a factor variable to identify each sample year
    dat$LTM_sample <- ifelse(as.numeric(format(dat$Date, "%m")) %in% c(7:12),
                             base::paste(format(as.Date(dat$Date, format="%d/%m/%Y"),"%Y"),"-",(as.numeric(format(as.Date(dat$Date, format="%d/%m/%Y"),"%Y"))+1)),
                             base::paste((as.numeric(format(as.Date(dat$Date, format="%d/%m/%Y"),"%Y"))-1),"-",(as.numeric(format(as.Date(dat$Date, format="%d/%m/%Y"),"%Y"))))
    )
    
    #Remove rows without a date (should remove empty rows)
    dat <- dat[!dat$LTM_sample == "NA - NA",]
    # Fill empty counts with 0
    dat$Count[which(is.na(dat$Count))] <- 0
    
    

    # Create a species list without a blank first item
    SpeciesList <- unique(as.character(dat$SpeciesCommon))
    dat$Species = dat$SpeciesCommon
    
    # Create a table to calc CPUE by Year
    CPUE_Tab <- data.frame(matrix(ncol = 2 + length(SpeciesList)))
    names(CPUE_Tab) <- c("Sample", "Site", SpeciesList)
    
    # Summarize fish counts and weights by Site and year
    
    sum_tab <- data.frame(stats::aggregate(list(SiteCount = dat$Count, SiteWt = dat$TotalWeight), 
                                    by = list(Year = dat$LTM_sample, Site = dat$Site, Species = dat$Species, Effort = dat$Effort/60), 
                                    FUN = sum, na.rm = TRUE))
    sum_tab$Ctmin <- sum_tab$SiteCount/sum_tab$Effort
    sum_tab$Wtmin <- sum_tab$SiteWt/sum_tab$Effort
    year_site_sum <- stats::aggregate(list(Year = sum_tab$Year, Site= sum_tab$Site), by = list(Year = sum_tab$Year, Site= sum_tab$Site), FUN = identity)[,c(1,2)]
    TotalCount <- stats::aggregate(list(TotalCountAll = sum_tab$SiteCount), by = list(Year = sum_tab$Year), FUN = sum)
    # Finish setting up CPUE table
    # Add blank rows to allow year and site to be copied in
    CPUE_Tab[length(year_site_sum$Year),] <- rep(NA, length(CPUE_Tab))
    # Copy in year and site 
    CPUE_Tab$Sample <- year_site_sum$Year
    CPUE_Tab$Site <- year_site_sum$Site
    CPUE_Tab_number <- CPUE_Tab
    CPUE_Tab_weight <- CPUE_Tab
    
    # Transcrib sum_tab into CPUE_Tab
    for (x in 1:nrow(sum_tab)) {
      x <- x
      CPUE_Tab_number[which(CPUE_Tab$Sample == base::paste(sum_tab[x,"Year"]) & CPUE_Tab$Site == base::paste(sum_tab[x,"Site"])), base::paste(sum_tab[x,"Species"])] <- as.numeric(sum_tab[x,"Ctmin"])
      CPUE_Tab_weight[which(CPUE_Tab$Sample == base::paste(sum_tab[x,"Year"]) & CPUE_Tab$Site == base::paste(sum_tab[x,"Site"])), base::paste(sum_tab[x,"Species"])] <- as.numeric(sum_tab[x,"Wtmin"])
    }
    #Replace NA with 0
    CPUE_Tab_number[is.na(CPUE_Tab_number)] <- 0
    CPUE_Tab_weight[is.na(CPUE_Tab_weight)] <- 0
    #Create Total species count and weight field
    CPUE_Tab_number$All <- apply(CPUE_Tab_number[3:ncol(CPUE_Tab_number)], 1, sum)
    CPUE_Tab_weight$All <- apply(CPUE_Tab_weight[3:ncol(CPUE_Tab_weight)], 1, sum)
    
    #CPUE by number Table
    CPUE_number <- stats::aggregate(CPUE_Tab_number[3:ncol(CPUE_Tab_number)], by = list(Year = CPUE_Tab_number$Sample), FUN = mean)
    CPUE_number$yr <- as.numeric(substr(CPUE_number[,1],1,4))
    CPUE_number <- CPUE_number[,c(1,(ncol(CPUE_number)),2:((ncol(CPUE_number))-1))]
    CPUE_number <- addgapyears(CPUE_number)
    CPUE_number$yr <- as.numeric(substr(CPUE_number[,1],1,4))
    #CPUE by Number Standard Error
    CPUE_number_SE <- stats::aggregate(CPUE_Tab_number[3:ncol(CPUE_Tab_number)], by = list(Year = CPUE_Tab_number$Sample), FUN = function(x) sd(x)/sqrt(length(x)))
    CPUE_number_SE$yr <- as.numeric(substr(CPUE_number_SE[,1],1,4))
    CPUE_number_SE <- CPUE_number_SE[,c(1,(ncol(CPUE_number_SE)),2:((ncol(CPUE_number_SE))-1))]
    CPUE_number_SE <- addgapyears(CPUE_number_SE)
    #CPUE by Number Standard Deviation
    CPUE_number_SD <- stats::aggregate(CPUE_Tab_number[3:ncol(CPUE_Tab_number)], by = list(Year = CPUE_Tab_number$Sample), FUN = function(x) sd(x))
    CPUE_number_SD$yr <- as.numeric(substr(CPUE_number_SD[,1],1,4))
    CPUE_number_SD <- CPUE_number_SD[,c(1,(ncol(CPUE_number_SD)),2:((ncol(CPUE_number_SD))-1))]
    CPUE_number_SD <- addgapyears(CPUE_number_SD)
    #CPUE by number MIN Table
    CPUE_number_min <- stats::aggregate(CPUE_Tab_number[3:ncol(CPUE_Tab_number)], by = list(Year = CPUE_Tab_number$Sample), FUN = min)
    CPUE_number_min$yr <- as.numeric(substr(CPUE_number_min[,1],1,4))
    CPUE_number_min <- CPUE_number_min[,c(1,(ncol(CPUE_number_min)),2:((ncol(CPUE_number_min))-1))]
    CPUE_number_min <- addgapyears(CPUE_number_min)
    #CPUE by number MAX Table
    CPUE_number_max <- stats::aggregate(CPUE_Tab_number[3:ncol(CPUE_Tab_number)], by = list(Year = CPUE_Tab_number$Sample), FUN = max)
    CPUE_number_max$yr <- as.numeric(substr(CPUE_number_max[,1],1,4))
    CPUE_number_max <- CPUE_number_max[,c(1,(ncol(CPUE_number_max)),2:((ncol(CPUE_number_max))-1))]
    CPUE_number_max <- addgapyears(CPUE_number_max)
    
    CPUE_weight <- stats::aggregate(CPUE_Tab_weight[3:ncol(CPUE_Tab_weight)], by = list(Year = CPUE_Tab_weight$Sample), FUN = mean)
    CPUE_weight$yr <- as.numeric(substr(CPUE_weight[,1],1,4))
    CPUE_weight <- CPUE_weight[,c(1,(ncol(CPUE_weight)),2:((ncol(CPUE_weight))-1))]
    CPUE_weight <- addgapyears(CPUE_weight)
    #CPUE by Number Standard Error
    CPUE_weight_SE <- stats::aggregate(CPUE_Tab_weight[3:ncol(CPUE_Tab_weight)], by = list(Year = CPUE_Tab_weight$Sample), FUN = function(x) sd(x)/sqrt(length(x)))
    CPUE_weight_SE$yr <- as.numeric(substr(CPUE_weight_SE[,1],1,4))
    CPUE_weight_SE <- CPUE_weight_SE[,c(1,(ncol(CPUE_weight_SE)),2:((ncol(CPUE_weight_SE))-1))]
    CPUE_weight_SE <- addgapyears(CPUE_weight_SE)
    #CPUE by weight Standard Deviation
    CPUE_weight_SD <- stats::aggregate(CPUE_Tab_weight[3:ncol(CPUE_Tab_weight)], by = list(Year = CPUE_Tab_weight$Sample), FUN = function(x) sd(x))
    CPUE_weight_SD$yr <- as.numeric(substr(CPUE_weight_SD[,1],1,4))
    CPUE_weight_SD <- CPUE_weight_SD[,c(1,(ncol(CPUE_weight_SD)),2:((ncol(CPUE_weight_SD))-1))]
    CPUE_weight_SD <- addgapyears(CPUE_weight_SD)
    #CPUE by number MIN Table
    CPUE_weight_min <- stats::aggregate(CPUE_Tab_weight[3:ncol(CPUE_Tab_weight)], by = list(Year = CPUE_Tab_weight$Sample), FUN = min)
    CPUE_weight_min$yr <- as.numeric(substr(CPUE_weight_min[,1],1,4))
    CPUE_weight_min <- CPUE_weight_min[,c(1,(ncol(CPUE_weight_min)),2:((ncol(CPUE_weight_min))-1))]
    CPUE_weight_min <- addgapyears(CPUE_weight_min)
    #CPUE by weight MAX Table
    CPUE_weight_max <- stats::aggregate(CPUE_Tab_weight[3:ncol(CPUE_Tab_weight)], by = list(Year = CPUE_Tab_weight$Sample), FUN = max)
    CPUE_weight_max$yr <- as.numeric(substr(CPUE_weight_max[,1],1,4))
    CPUE_weight_max <- CPUE_weight_max[,c(1,(ncol(CPUE_weight_max)),2:((ncol(CPUE_weight_max))-1))]
    CPUE_weight_max <- addgapyears(CPUE_weight_max)
    
    #Create % Composition Tables
    if(ncol(CPUE_number) > 4) {Comp_Sum <- data.frame(list(Year = substr(CPUE_number$Year,1,4),
                                                           Tot_CPUE_num = as.numeric(apply(CPUE_number[,3:(ncol(CPUE_number)-1)], 1, sum)),
                                                           Tot_CPUE_wt = as.numeric(apply(CPUE_weight[,3:(ncol(CPUE_weight)-1)], 1, sum))))} else {Comp_Sum <- "No Composition table for Single Species data"
                                                           }
    
    if(is.data.frame(Comp_Sum)) {
      Comp_num <- CPUE_number[,3:(ncol(CPUE_number)-1)]/Comp_Sum$Tot_CPUE_num
      Comp_wt <- CPUE_weight[,3:(ncol(CPUE_weight)-1)]/Comp_Sum$Tot_CPUE_wt
      Comp_Sum$SimpsonsD <- (1/(apply(Comp_num^2,1,sum)))} else {
        Comp_num <- "No Composition tables for single species data"
        Comp_wt <- "No Composition tables for single species data"
      }
    
    #Summarize Sample size by year
    Year_Sites <- CPUE_Tab %>%
      dplyr::group_by(Sample) %>%
      dplyr::summarise(dplyr::n_distinct(Site))
    
    #FORMAT ANNUAL SUMMARY FOR MOST RECENT YR in DATASET
    
    a = CPUE_number %>% dplyr::filter(yr==max(yr, na.rm=T)) %>% tidyr::pivot_longer(names_to="Species", values_to="CPUE_num", c(-Year,-yr))
    b = CPUE_number_SE %>% dplyr::filter(yr==max(yr, na.rm=T)) %>% tidyr::pivot_longer(names_to="Species", values_to="CPUE_num_SE", c(-Year,-yr))
    c = CPUE_weight %>% dplyr::filter(yr==max(yr, na.rm=T)) %>% tidyr::pivot_longer(names_to="Species", values_to="CPUE_wt", c(-Year,-yr))
    d = CPUE_weight_SE %>% dplyr::filter(yr==max(yr, na.rm=T)) %>% tidyr::pivot_longer(names_to="Species", values_to="CPUE_wt_SE", c(-Year,-yr))
    e = CPUE_number_min %>% dplyr::filter(yr==max(yr, na.rm=T)) %>% tidyr::pivot_longer(names_to="Species", values_to="CPUE_num_min", c(-Year,-yr))
    f = CPUE_number_max %>% dplyr::filter(yr==max(yr, na.rm=T)) %>% tidyr::pivot_longer(names_to="Species", values_to="CPUE_num_max", c(-Year,-yr))
    g = CPUE_weight_min %>% dplyr::filter(yr==max(yr, na.rm=T)) %>% tidyr::pivot_longer(names_to="Species", values_to="CPUE_wt_min", c(-Year,-yr))
    h = CPUE_weight_max %>% dplyr::filter(yr==max(yr, na.rm=T)) %>% tidyr::pivot_longer(names_to="Species", values_to="CPUE_wt_max", c(-Year,-yr))
    i = CPUE_number_SD %>% dplyr::filter(yr==max(yr, na.rm=T)) %>% tidyr::pivot_longer(names_to="Species", values_to="CPUE_num_SD", c(-Year,-yr))
    j = CPUE_weight_SD %>% dplyr::filter(yr==max(yr, na.rm=T)) %>% tidyr::pivot_longer(names_to="Species", values_to="CPUE_wt_SD", c(-Year,-yr))
      
      
      
    annual_summary = a %>% dplyr::left_join(b) %>% dplyr::left_join(i) %>% dplyr::left_join(e) %>% dplyr::left_join(f) %>% 
                       dplyr::left_join(c) %>% dplyr::left_join(d)  %>% dplyr::left_join(j) %>% dplyr::left_join(g) %>% dplyr::left_join(h) %>%
                          dplyr::arrange(Species)
    #annual_summary$num_cv = annual_summary$CPUE_number_SD/annual_summary$CPUE_number
    #annual_summary$wt_cv = annual_summary$CPUE_weight_SD/annual_summary$CPUE_weight
    
    
    # Generate Species History Table
    
    species.history = CPUE_number %>% dplyr::select(-Year,-All) %>%
                        tibble::rownames_to_column() %>% dplyr::select(-rowname) %>%
                        tibble::column_to_rownames("yr")
    species.history = species.history %>% dplyr::relocate(sort(names(species.history)))
    species.history = data.frame(ifelse(species.history > 0, 1, 0))
    
     ## Print out tables
     PrintTables <- function(OutTab) {
       prtCPUENum <- function(x) {
         write.csv(CPUE_number, base::paste(name,"-","CPUE_number.csv",sep =""))
         write.csv(CPUE_number_SE, base::paste(name,"-","CPUE_number_SE.csv",sep =""))
       }
       prtCPUEWt <- function(x) {
         write.csv(CPUE_weight, base::paste(name,"-","CPUE_weight.csv",sep =""))
         write.csv(CPUE_weight_SE, base::paste(name,"-","CPUE_weight_SE.csv",sep =""))
       }
       prtSpeciesList <- function(x) {
         write.csv(SpeciesList, base::paste(name,"-","SpeciesList.csv",sep =""))
       }
       prtYearEffort <- function(x) {
         write.csv(Year_Sites, base::paste(name,"-","EffortByYear.csv",sep =""))
       }
       prtAnnSum <- function(x) {
         write.csv(annual_summary, base::paste(name,"-",max(annual_summary$yr,na.rm=T),"-","Annual_Summary.csv", sep=""))
       }
    
       if(OutTab == 0) {} else if (OutTab == 1) {
       prtCPUENum()
         prtCPUEWt()
         prtSpeciesList()
         prtYearEffort()
         prtAnnSum()
       } else if (OutTab == 2) {
         prtCPUENum()
       } else if (OutTab == 3) {
         prtCPUEWt()
       } else if (OutTab == 4) {
         prtSpeciesList()
       } else if (OutTab == 5) {
         prtYearEffort()
       }
     }
     PrintTables(OutTab)
     
     
      
    #### function output
     processed_data <- list(SpeciesList = SpeciesList,
                            SpeciesHistory = species.history,
                            CPUE_Tab_number = CPUE_Tab_number,
                            CPUE_number = CPUE_number,
                            CPUE_number_SE = CPUE_number_SE,
                            CPUE_Tab_weight = CPUE_Tab_weight,
                            CPUE_weight = CPUE_weight,
                            CPUE_weight_SE = CPUE_weight_SE,
                            Comp_num = Comp_num,
                            Comp_wt = Comp_wt,
                            Year_Sites = Year_Sites,
                            Comp_Sum = Comp_Sum,
                            TotalCount = TotalCount,
                            RawData = rawfile,
                            annual_summary = annual_summary
     )
    return(processed_data)
  }
#Create Figures
#processed_data = outlist$LMB
  print("creating figures")
  
  plot_figs <- function (processed_data, FigstoPrint = 0, printdirectory = getwd(), name = "No name Specified") {
      #require(ggplot2)
      #Function to create figures for CPUE by number for each species
      #processed_data = orange_all
       Ctfigs <- function(x, name) {
         print("...CPUE by number for each species")
         simpleCap <- function(x) {
           s <- strsplit(x, " ")[[1]]
           base::paste(toupper(substring(s, 1,1)), substring(s, 2),
                 sep="", collapse=" ")
         }
          for (i in 3:ncol(processed_data$CPUE_number)) {
            #i=3
            lower <- processed_data$CPUE_number[,c(i)]-(2*(processed_data$CPUE_number_SE[,i]))
            lower[lower < 0] <- 0
            upper <- processed_data$CPUE_number[,c(i)]+(2*processed_data$CPUE_number_SE[,i])
            xmn = min(na.omit(processed_data$CPUE_number$yr))
            xmx =  max(na.omit(processed_data$CPUE_number$yr))
            upperQ <- quantile(na.omit(processed_data$CPUE_number[,c(i)]),0.75)
            med <- median(na.omit(processed_data$CPUE_number[,c(i)]))
            lowerQ <- quantile(na.omit(processed_data$CPUE_number[,c(i)]),0.25)
            Fig <- ggplot2::ggplot(data = processed_data$CPUE_number, ggplot2::aes(x = yr, y = processed_data$CPUE_number[,c(i)])) + 
              geom_point() + 
              labs(title = base::paste(name,"-" ,simpleCap(names(processed_data$CPUE_number)[i])),
                   x = names(processed_data$CPUE_number)[1],
                   y = "Mean Catch Per Unit Effort by Number (#/minute \u00b1 2 SE)" ) +
              theme_bw() +
              scale_x_continuous(breaks= seq(xmn,xmx,1)) + #labels = c(substr(processed_data$CPUE_number[,c(1)],1,4))) + 
              geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper, width = 0.2), size = 1) + 
              geom_ribbon(ggplot2::aes(ymax=upperQ,ymin=lowerQ),alpha=0.2, stat="identity", fill = "red") +
              geom_hline(yintercept = med, color = "dark red", lwd=1, lty=2) +
              scale_y_continuous(limits = c(0, (max(na.omit(processed_data$CPUE_number[,c(i)]+(2*processed_data$CPUE_number_SE[,i])*1.05)))), expand = c(0,0)) +
              theme(plot.title = element_text(hjust = 0.5))
            tiff(base::paste(printdirectory,"/",name,"-" ,names(processed_data$CPUE_number)[i],"-","#CPUE",".tiff", sep = ""), 
                 height = 1800, 
                 width = 2100, 
                 res = 300, 
                 compression = 'lzw')
            print(Fig)
            dev.off()
          }
       }
      #Function to create figures for CPUE by Weight for each species
       Wtfigs <- function(x, name) {
         print("...CPUE by weight for each species")
         simpleCap <- function(x) {
           s <- strsplit(x, " ")[[1]]
           base::paste(toupper(substring(s, 1,1)), substring(s, 2),
                 sep="", collapse=" ")
         }
          for (i in 3:ncol(processed_data$CPUE_weight)) {
            lower <- processed_data$CPUE_weight[,c(i)]-(2*(processed_data$CPUE_weight_SE[,i]))
            lower[lower < 0] <- 0
            upper <- processed_data$CPUE_weight[,c(i)]+(2*processed_data$CPUE_weight_SE[,i])
            
            Fig <- ggplot2::ggplot(data = processed_data$CPUE_weight, ggplot2::aes(x = Year, y = processed_data$CPUE_weight[,c(i)])) + geom_point() + 
              labs(title = base::paste(name,"-" ,simpleCap(names(processed_data$CPUE_weight)[i])), x = names(processed_data$CPUE_weight)[1], y = "Mean Catch Per Unit Effort by Weight (g/minute \u00b1 2 SE)" ) +
              theme_bw() +
              scale_x_discrete(labels = c(substr(processed_data$CPUE_weight[,c(1)],1,4))) + 
              geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper, width = 0.2), size = 1) + 
              scale_y_continuous(limits = c(0, (max(na.omit(processed_data$CPUE_weight[,c(i)]+(2*processed_data$CPUE_weight_SE[,i])*1.05)))), expand = c(0,0)) +
              theme(plot.title = element_text(hjust = 0.5))
            tiff(base::paste(printdirectory,"/",name,"-" ,names(processed_data$CPUE_weight)[i],"-","WtCPUE",".tiff", sep = ""), 
                 height = 1800, 
                 width = 2100, 
                 res = 300, 
                 compression = 'lzw')
            print(Fig)
            dev.off()
          }
       }
       #Function to create summary figs for a sample (total CPUE, Simpsons D, etc)
       Sumfigs <- function(x,name) {
         print("...Summary Figures")
           for (i in 2:ncol(processed_data$Comp_Sum)) {
           sumname <- names(processed_data$Comp_Sum)[i]
           titlename <- 'text'
           yname = 'text'
           if(sumname == "Tot_CPUE_num") {titlename = "Total Catch Per Minute by Number (All Species)"
                                              yname = "Total Catch Per Unit Effort (#/min)"} else 
             if (sumname == "Tot_CPUE_wt") {titlename = "Total Catch Per Minute by Weight (All Species)"
                                                yname = "Total Catch Per Unit Effort (g/min)"} else 
               if(sumname == "SimpsonsD") {titlename = "Modified Simpson's Diversity Index"
                                               yname = "Modified Simpsons Diversity Index (1/D)"}
           Fig <- ggplot2::ggplot(data = processed_data$Comp_Sum, ggplot2::aes(x = Year, y = processed_data$Comp_Sum[,c(i)])) + geom_line(ggplot2::aes(group = 1)) + 
             geom_point() +
             labs(title = base::paste(name,titlename, sep = "\n"), x = names(processed_data$Comp_Sum$Year)[1], y = yname) +
             theme_bw() +
             scale_x_discrete(labels = processed_data$Comp_Sum$Year) + 
             scale_y_continuous(limits = c((min(na.omit(processed_data$Comp_Sum[,c(i)]*0.95))), (max(na.omit(processed_data$Comp_Sum[,c(i)]*1.05)))), expand = c(0,0)) +
             theme(plot.title = element_text(hjust = 0.5))
           tiff(base::paste(printdirectory,"/",name,"-",sumname,".tiff", sep = ""), 
                height = 1800, 
                width = 2100, 
                res = 300, 
                compression = 'lzw')
           print(Fig)
           dev.off()
         }
       }
       
       if(FigstoPrint == 0) {
         print("No Figures Printed")
       } else if(FigstoPrint == 1) {
         print("Printing All Figures")
         Ctfigs(processed_data, name)
         Wtfigs(processed_data, name)
         Sumfigs(processed_data, name)} else if(FigstoPrint ==2) {
           print('Printing CPUE by number figures only')
           Ctfigs(processed_data, name)} else if(FigstoPrint == 3) {
             print("Printing CPUE by wt figures only")
             Wtfigs(processed_data, name)} else if(FigstoPrint == 4) {
               print("printing summary figures only")
               Sumfigs(processed_data, name)} else {print ("Incorrect Figure type selection")}
  }
  
  
    WaterbodyName1 = waterbodyname
    FILE = file
    OutTables1 = outtables
    PrintFigs1 = printfigs
    ImportedData = ImpData(FILE)
    print(base::paste("Rows:",nrow(ImportedData)))
    print(base::paste("Columns:",ncol(ImportedData)))
    if("community" %in% names(ImportedData)) {
      Data.proc = process_data(ImportedData$community,OutTables1,WaterbodyName1)
      plot_figs(Data.proc, PrintFigs1,print_directory,WaterbodyName1)
    }
    if("LMB" %in% names(ImportedData)) {
      Data.proc = process_data(ImportedData$LMB,OutTables1,WaterbodyName1)
      plot_figs(Data.proc, PrintFigs1,print_directory,WaterbodyName1)
    }
    return(Data.proc)
}


#########################################################################################
################################################################################
#' Length Distribution Plot
#' 
#' Create Length Distribution histograms for species and years selected
#' @param datafile data, should be output from ltm.data.summary function
#' @param speciesList list of selected species, can specify by common name, scientific name or species code
#' @param years list of years to include in figure
#' @param seasons ---currently functionless, will update in future version, to avoid errors make sure that all seasons in input dataset are the same---
#' @param print boolean, if TRUE figure will be saved to file
#' @param figure_filename if print=TRUE, figure will be saved to this filename
#' @param fig_scale adjust to scale output figure size
#' @return Returns the summarized data used to construct the histogram plot
#' @examples
#' data(newnans)
#' newn_sum <- ltm.data.summary("Newnans",newnans)
#' newnans_LD <- len.dist(newn_sum, speciesList=c("BLUE","LMB", "BRBU"), years = c(2016:2020))
#' @export
len.dist<- function(datafile,
                         speciesList = list(),
                         years = list(),
                         seasons = list(),
                         print = FALSE,
                         figure_filename = NA,
                         fig_scale = 1) {
  require(ggplot2)
  require(tidyr)
  require(stringr)
  
  
  if(length(speciesList) == 0) {
    species = datafile$SpeciesList
  } else {
      species = speciesList}
  
## internal functions ----------------------------------------------------------  
  internal_get_year <- function(data, date_field) {
    formats <- 
      data %>% 
      pull({{ date_field }}) %>% 
      lubridate::guess_formats(orders = c("mdy", "ymd")) %>% 
      unique() %>% 
      .[!stringr::str_detect(.,"O")]
    
    if(length(formats) > 1) {
      cli::cli_abort("more than 1 date format detected")
    } else if (formats[[1]] == "%m/%d/%Y") {
      return(data %>%
               mutate(yr = lubridate::year(lubridate::mdy({{ date_field }}))) %>%
               pull(yr))
    } else if (formats[[1]] == "%Y-%m-%d") {
      return(data %>%
               mutate(yr = lubridate::year(lubridate::ymd({{ date_field }}))) %>%
               pull(yr))
    } else {
      cli::cli_abort("date format not recognized")
    }
    
  }
  
## Process ---------------------------------------------------------------------  
  workingdat <- 
    datafile$RawData %>% 
    dplyr::filter((SpeciesCode %in% species) | 
                  (SpeciesCommon %in% species)|
                  (SpeciesScientific %in% species)) %>% 
    mutate(Year = internal_get_year(., Date)) %>% 
    mutate(SeasYr = base::paste0(Season,"-", Year))
  

  if(length(years) != 0) {workingdat = workingdat[which(workingdat$Year %in% years), ]}
  if(length(seasons) != 0) { workingdat = workingdat[which(workingdat$Season %in% seasons), ]}
  

  workingsum = workingdat %>% 
    dplyr::group_by(SpeciesCode, SeasYr, TL_CM_Group) %>% 
    dplyr::summarise(Count = sum(Count, na.rm = TRUE))

 
  sl = unique(workingsum$SpeciesCode)

  tbl = workingsum[-c(1:nrow(workingsum)), -grep("Count", names(tbl))]
 # print("loop through each target species")
  for(x in 1:length(unique(workingsum$SpeciesCode))) {
  #  print(unique(workingsum$SpeciesCode)[x])
    subdat = workingsum[which(workingsum$SpeciesCode == sl[x]),]
    mx = max(subdat$TL_CM_Group, na.rm = TRUE)
  #  print(mx)
    splenvec = data.frame(TL_CM_Group = c(0:mx))
   #   print("loop through each target season")
      for(i in 1:length(unique(subdat$SeasYr))) {
    #    print(base::paste("season:",unique(subdat$SeasYr)[i]))
        newrows = splenvec
        newrows$SpeciesCode = subdat$SpeciesCode[1]
        newrows$SeasYr = unique(subdat$SeasYr)[i]
        newrows = newrows[, c("SpeciesCode", "SeasYr", "TL_CM_Group")]
        tbl = rbind(tbl, newrows)
      }
     
  }
 # print('merge data')
  workingsum = merge(tbl, workingsum, all.x = TRUE)
  workingsum$Count[is.na(workingsum$Count)] = 0
  workingsum$percent = NA
#  print('loop through each row in temp dataset and build ss table')
  for(x in 1:nrow(workingsum)) {
    ss = workingsum[which(workingsum$Species == workingsum[x,1] & workingsum$SeasYr == workingsum[x,2]),]
    workingsum[x,5] = (workingsum[x,4])/sum(ss$Count)
  }
  #### Working Sum formatted and ready to go!
  #print("create length distribution plot, facet by species")
  lendist = ggplot2::ggplot(data = workingsum, ggplot2::aes(x = TL_CM_Group, y = percent)) + 
    geom_bar(stat = "identity", width = 1) + 
    facet_grid(SeasYr ~ SpeciesCode, scales = "free_x") + 
    theme_bw() + scale_y_continuous(expand = expansion(add = c(0,0.05))) + 
    scale_x_continuous(breaks = scales::breaks_extended(n=5)) +
    ylab("Percent Total") + 
    xlab("Total Length (cm)")
  print(lendist)
  
  if(print == TRUE) {
  if(is.na(figure_filename)){figure_filename="length.distribution.tiff"}
  if(substr(figure_filename,-6,-1) != ".tiff") {figure_filename = base::paste(figure_filename,".tiff",collapse="")}
  figHt = length(years)
  figWid = length(speciesList)
  scaler= fig_scale
  tiff(figure_filename, res = 300, height = (figHt*scaler*300), width = (figWid*scaler*2*300), compression = 'lzw')
  print(lendist)
  dev.off()
  }
  return(workingsum)
}

#####################################################################################################################
#####################################################################################################################
#' Catch-Per-Unit-Effort Timeseries Plot
#' 
#' Create CPUE timeseries for species and years selected
#' @param datafile data, should be output from ltm.data.summary function
#' @param speciesList list of selected species, can specify by common name, scientific name or species code
#' @param species_size_strata optional argument, specifies size strata groups for which CPUEs should be calculated, see example below for proper convention. 
#' @param years list of years to include in figure
#' @param seasons ---currently functionless, will update in future version, to avoid errors make sure that all seasons in input dataset are the same---
#' @param print boolean, if TRUE figure will be saved to file
#' @param figure_filename if print=TRUE, figure will be saved to this filename
#' @param fig_scale adjust to scale output figure size
#' @return Returns the summarized data used to construct the plot
#' @examples
#' # import and format data
#' data(newnans)
#' newn_sum <- ltm.data.summary("Newnans Lake", newnans)
#' # CPUE plots for bluegill, largemouth bass, and brown bullheads
#' newnans_cpue <- cpue.plot(newn_sum,
#'  speciesList=c("BLUE","LMB", "BRBU"),
#'  years = c(2016:2020))
#' # CPUE plots by size class
#' newnans_cpue2 <- cpue.plot(newn_sum,
#'  speciesList=c("BLUE","LMB","BRBU"),
#'  species_size_strata = list(
#'   BLUE = list(
#'    YOY = c(0,8),
#'    Quality = c(18,50)),
#'   LMB = list(
#'    YOY = c(0,20),
#'    Quality = c(30,50),
#'    Trophy = c(51,100))
#'  ),
#'  years = c(2016:2020)
#' )
#' @export
cpue.plot   <- function(datafile,
                               speciesList = list(),
                               species_size_strata = list(),
                               years = list(),
                               seasons = list(),
                               print = FALSE,
                               figure_filename = NA,
                               fig_scale = 1) {
        require(ggplot2)
        require(tidyr)
        require(stringr)

         if(length(speciesList) == 0) {stop("No species list")} else {species = speciesList}

# Internal Functions -----------------------------------------------------------
          internal_get_year <- function(data, date_field) {
            formats <- 
              data %>% 
              pull({{ date_field }}) %>% 
              lubridate::guess_formats(orders = c("mdy", "ymd")) %>% 
              unique() %>% 
              .[!stringr::str_detect(.,"O")]
            
            if(length(formats) > 1) {
              cli::cli_abort("more than 1 date format detected")
            } else if (formats[[1]] == "%m/%d/%Y") {
              return(data %>%
                       mutate(yr = lubridate::year(lubridate::mdy({{ date_field }}))) %>%
                       pull(yr))
            } else if (formats[[1]] == "%Y-%m-%d") {
              return(data %>%
                       mutate(yr = lubridate::year(lubridate::ymd({{ date_field }}))) %>%
                       pull(yr))
            } else {
              cli::cli_abort("date format not recognized")
            }
            
          }
  
# Process ---------------------------------------------------------------------  
          workingdat <- 
            datafile$RawData %>% 
            mutate(Year = internal_get_year(., Date)) %>% 
            mutate(SeasYr = base::paste0(Season,"-", Year))
          
          od = workingdat
          
          if(length(years)==0) {years = as.numeric(unique(workingdat$Year))}
          
          #Create Year-Site-list
          YSL = workingdat %>% dplyr::select(WaterBody, Year, Site, Season) %>% distinct()
          
          #create species lookup table
          Species_lookup = workingdat %>% dplyr::select(SpeciesCode, SpeciesCommon, SpeciesScientific) %>% distinct()
          
          #create sizeStrata
          
          
          if(length(species_size_strata) != 0) {
            #print("Adding Specified Size Strata")
            workingdat$sizeStrata = NA
            #loop species within strata list
            for(ss in 1:length(species_size_strata)) {
              speciesSS = names(species_size_strata)[[ss]]
              #loop size classes within species slots
              for(sc in 1:length(species_size_strata[[ss]])) {
                sclass = names(species_size_strata[[ss]])[[sc]]
               # print(base::paste("Adding ", speciesSS, " size class: ", sclass))
                workingdat[which(workingdat$SpeciesCode == speciesSS & 
                                 workingdat$TL_CM_Group <= max(species_size_strata[[ss]][[sc]]) &
                                 workingdat$TL_CM_Group >= min(species_size_strata[[ss]][[sc]])
                                 ),c("sizeStrata")] = sclass
                
              }
            }
            #Identify species with No strata designated and assign "All"
            for(spi in 1:length(unique(workingdat$SpeciesCode))) {
              temp_species = unique(workingdat$SpeciesCode)[[spi]]
              if(!(temp_species %in% names(species_size_strata))) {
              workingdat[which(workingdat$SpeciesCode == temp_species), c("sizeStrata")] = "All"
              }
            }
          } else {workingdat$sizeStrata = "All"}
          
         # print("Done adding size strata")
          
         #print(base::paste("data rows:", nrow(workingdat)))
        
          #Subset target species
           # print("subset target species")
           workingdat = workingdat %>% 
             dplyr::filter((SpeciesCode %in% species)|(SpeciesCommon %in% species)|(SpeciesScientific %in% species))
# 
         
#         #Subset out by years and seasons specified in function call arguments
           # print("Subset out years and seasons specified in function call")
            if(length(years) != 0) {workingdat = workingdat[which(workingdat$Year %in% years), ]}
            if(length(seasons) != 0) { workingdat = workingdat[which(workingdat$Season %in% seasons), ]}

# Need to create CPUE by size group # then feed that into CTFigs which needs to be modified
            workingdat2 = workingdat %>% 
              dplyr::group_by(WaterBody, Site, Year, Season, SpeciesCode, sizeStrata) %>% 
              dplyr::summarise(Count = sum(Count, na.rm=T), Effort = mean(Effort, na.rm=T)) 
            
            spst_lkup <- workingdat2 %>% ungroup() %>%
              dplyr::select(SpeciesCode, sizeStrata) %>% distinct() %>%
              unite("Species_strata", c(SpeciesCode,sizeStrata),remove=FALSE)
              
            wd = workingdat2 %>% full_join(YSL)
            
              #Calculate CPUE by WaterBody, Year, Season, Species and Size Strata
              CPUE_Sum_sizeStrata = wd %>% 
                unite("Species_strata", c(SpeciesCode,sizeStrata),remove=TRUE) %>%
                dplyr::select(-Effort) %>%
                pivot_wider(names_from = Species_strata,
                            values_from = Count,
                            values_fill = 0) %>% 
                tidyr::pivot_longer(-c(WaterBody, Site, Year, Season),
                             names_to = "Species_strata", 
                             values_to = "Count") %>% dplyr::left_join(spst_lkup) %>%
                dplyr::filter(Species_strata != "NA_NA") %>%
                dplyr::left_join(Species_lookup) %>%
                dplyr::left_join(od %>% dplyr::group_by(WaterBody, Site, Year, Season) %>% summarize(Effort = mean(Effort)/60)) %>%
                mutate(SampleCPUE = Count/Effort) %>%
                #average cpue across sites within Waterbody-Year-Season-Species-sizeStrata groups 
                dplyr::group_by(WaterBody, Year, Season, SpeciesCode, SpeciesCommon, SpeciesScientific, sizeStrata) %>%
                summarize(mean_CPUE = mean(SampleCPUE, na.rm=T),
                          sd_CPUE = sd(SampleCPUE, na.rm=T ),
                          se_CPUE = sd(SampleCPUE,  na.rm=T)/sqrt(n()),
                          max_CPUE = max(SampleCPUE,  na.rm=T),
                          min_CPUE = min(SampleCPUE,  na.rm=T),
                          cv_CPUE = sd(SampleCPUE, na.rm=T)/mean(SampleCPUE, na.rm=T))
              
              #Create CPUE Summary, without size strata
              workingdat3 = workingdat %>% 
                dplyr::group_by(WaterBody, Site, Year, Season, SpeciesCode) %>% 
                dplyr::summarise(Count = sum(Count, na.rm=T), Effort = mean(Effort, na.rm=T))
              
              wd2 = workingdat3 %>% full_join(YSL)
              
              CPUE_Sum = wd2 %>% dplyr::select(-Effort) %>% 
                pivot_wider(names_from = c(SpeciesCode),
                            values_from = Count,
                            values_fill = 0) %>% 
                tidyr::pivot_longer(-c(WaterBody, Site, Year, Season),
                             names_to = "SpeciesCode", 
                             values_to = "Count") %>% 
                dplyr::filter(SpeciesCode != "NA" & !is.na(SpeciesCode)) %>%
                dplyr::left_join(Species_lookup) %>%
                dplyr::left_join(od %>% dplyr::group_by(WaterBody, Site, Year, Season) %>% summarize(Effort = mean(Effort)/60)) %>%
                mutate(SampleCPUE = Count/Effort) %>%
                #average cpue across sites within Waterbody-Year-Season-Species groups 
                dplyr::group_by(WaterBody, Year, Season, SpeciesCode, SpeciesCommon, SpeciesScientific) %>%
                summarize(mean_CPUE = mean(SampleCPUE, na.rm=T),
                          sd_CPUE = sd(SampleCPUE, na.rm=T),
                          se_CPUE = sd(SampleCPUE, na.rm=T)/sqrt(n()),
                          max_CPUE = max(SampleCPUE, na.rm=T),
                          min_CPUE = min(SampleCPUE, na.rm=T),
                          cv_CPUE = sd(SampleCPUE,na.rm=T)/mean(SampleCPUE, na.rm=T)) %>%
                mutate(sizeStrata = "All_Sizes")
            
              CPUE_Sum <- rbind(CPUE_Sum, CPUE_Sum_sizeStrata)
            
              ci_limits <- function(x) {lower = x$mean_CPUE - 2*x$se_CPUE
                                        upper = x$mean_CPUE + 2*x$se_CPUE
                                        return(cbind(x,lower = lower,upper = upper))}
              
              CPUE_Sum = ci_limits(CPUE_Sum)
              CPUE_Sum = CPUE_Sum %>% dplyr::filter((!(sizeStrata %in% c(1,"All_Sizes", NA, "NA"))) & (Year %in% years))
              
              
              xmn = min(na.omit(years))
              xmx =  max(na.omit(years))
              
              xlims = seq(xmn,xmx,1)
              
              hist_avg = CPUE_Sum %>% dplyr::group_by(WaterBody, Season, SpeciesCode, SpeciesScientific, SpeciesCommon, sizeStrata) %>% 
                summarize(upperQ = quantile(na.omit(mean_CPUE),0.75),
                          lowerQ = quantile(na.omit(mean_CPUE),0.25),
                          med = median(na.omit(mean_CPUE))) %>% 
                crossing(xlims)
    
              
              Fig <- ggplot2::ggplot(data = CPUE_Sum,ggplot2::aes(color=sizeStrata,
                                pch=sizeStrata,
                                fill=sizeStrata))  + 
                       
                  facet_wrap(~SpeciesCommon, scales = "free_y", ncol=2) +
                  scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
                  coord_cartesian(ylim = c(0,NA)) +
                  labs(x = "Year", y = "Mean Catch Per Unit Effort by Number (#/minute \u00b1 2 SE)") +
                  theme_bw() +
                  scale_x_continuous(breaks= scales::breaks_width(2)) +
                  geom_errorbar(ggplot2::aes(x = Year, ymin = lower, ymax = upper, width = 0.2), size = 1) + 
                geom_point(ggplot2::aes(x = Year,
                               y = mean_CPUE), color = "black", size =2) +
                geom_ribbon(data = hist_avg, ggplot2::aes(x=xlims, ymax=upperQ,ymin=lowerQ, fill=sizeStrata),
                              alpha=0.2, stat="identity") +
                  theme(plot.title = element_text(hjust = 0.5),
                        legend.title = element_blank())
              
              print(Fig)
             
              
              
              if(print == TRUE) {
                if(is.na(figure_filename)){figure_filename="cpue.plot.tiff"}
                if(substr(figure_filename,-6,-1) != ".tiff") {figure_filename = base::paste(figure_filename,".tiff",collapse="")}
                figHt = ceiling(length(speciesList)/2)
                figWid = ifelse(length(speciesList)>1,2,1)
                scale=fig_scale
                tiff(figure_filename, res = 300, height = (figHt*scale*300), width = (figWid*scale*300), compression = 'lzw')
                print(Fig)
                dev.off()
              }
              return(CPUE_Sum)
              }


################################################################################################################
################################################################################################################
#' Check if Outlier
#' 
#' Checks whether value is an outlier
#' @param x vector of numerical values
#' @return boolean
#' @examples
#' numbers = c(1:10,1:10,1:10,1000)
#' numbers_outlier <- is.outlier(numbers)
#' numbers[numbers_outlier]#'
#' @export
is.outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


####################################################################################################################################################
################################################################################
#' Guild Composition Plot
#' 
#' Create Guild Composition plot
#' @param ltm_dataset data, should be output from ltm.data.summary function
#' @param years list of years to include in figure
#' @param waterbody_list ---currently functionless, will update in future version---
#' @param color_scale color scale for output figure defaults to default r color scale, other options include "grayscale" for black and white, and "viridis" for viridis magma scale
#' @param save boolean, if TRUE figure will be saved to file
#' @return returns summarize data used to construct figures
#' @examples
#' data(newnans)
#' newn_sum <- ltm.data.summary(file=newnans)
#' newnans_guild <- guild.comp(newn_sum)
#' @export
guild.comp <- function(ltm_dataset,
                       waterbody_list = list(),
                       years = list(),
                       color_scale = "default",
                       save = FALSE) {
  require(ggplot2)
  require(tidyr)
  require(dplyr)
  require(viridis)
  
    #ltm_dataset=orange_all
import_guild_table <- function() {
  guilda=c("African Jewelfish","AFJE","Exotic","NonGame","Alabama Shad","ALSH","Other","NonGame","Alligator Gar","AGAR","Rough","NonGame","American Eel","AMEE","Other","NonGame","American Shad","AMSH","Other","Game","Atlantic Croaker","ATCR","Other","NonGame","Atlantic Needlefish","ATNE","Rough","NonGame","Atlantic Stingray","ATSR","Other","NonGame","Bay Anchovy","BAAN","Other","NonGame","Banded Sunfish","BASU","Sunfishes","NonGame","Banded Topminnow","BATO","Forage","NonGame","Blackbanded Sunfish","BBSU","Sunfishes","NonGame","Blue Tilapia","BETI","Exotic","NonGame","Bigmouth Sleeper","BISL","Other","NonGame","Black Acara","BKAC","Exotic","NonGame","Black Bullhead","BLBU","Catfish","NonGame","Black Crappie","BLCR","BLCR","Game","Blackbanded Darter","BLDA","Forage","NonGame","Blueback Herring","BLHE","Other","NonGame","Bluefin Killifish","BLKI","Forage","NonGame","Blacktail Redhorse","BLRE","Rough","NonGame","Blacktail Shiner","BLSH","Forage","NonGame","Bluegill","BLUE","Sunfishes","Game","bluegill-redear hybrid","CODE","Sunfishes","Game","Bluenose Shiner","BNSH","Forage","NonGame","Bowfin","BOW","Rough","NonGame","Banded Pygmy Sunfish","BPS","Other","NonGame","Brown Bullhead","BRBU","Catfish","Game","Brown Darter","BRDA","Forage","NonGame","Brown Hoplo","BRHO","Exotic","NonGame","Brook Silverside","BRSI","Forage","NonGame","Bluestripe Shiner","BSSH","Forage","NonGame","Bluespotted Sunfish","BSSU","Sunfishes","NonGame","Butterfly Peacock Bass","BUTT","Exotic","Game","Catfish- Ictaluridae","CASP","Catfish","Game","Channel Catfish","CHCA","Catfish","Game","Clear Chub","CLCH","Forage","NonGame","Clown Goby","CLGO","Forage","NonGame","Common Carp","COCA","Exotic","NonGame","Coastal Shiner","COSH","Forage","NonGame","Common Snook","COSN","Other","Game","Chain Pickerel","CPIK","Rough","NonGame","Crevalle Jack","CRJA","Other","NonGame","Dollar Sunfish","DOSU","Sunfishes","NonGame","Fathead Minnow","FAMI","Exotic","NonGame","Fat Snook","FASN","Other","Game","Florida Gar","FGAR","Rough","NonGame","Flagfish","FLAG","Forage","NonGame","Flathead Catfish","FLCA","Exotic","NonGame","Flier","FLIE","Sunfishes","Game","Flagfin Shiner","FLSH","Forage","NonGame")
  guildb=c("Freshwater Goby","FWGO","Forage","NonGame","Gizzard Shad","GISH","Forage","NonGame","Golden Shiner","GOSH","Forage","NonGame","Golden Topminnow","GOTO","Forage","NonGame","Gulf Coast Pygmy Sunfish","GPSU","Other","NonGame","Grass Carp","GRCA","Exotic","NonGame","Grass Pickerel","GRPK","Rough","NonGame","Greyfin Redhorse","GRRE","Rough","NonGame","Grey Snapper","GRSN","Other","Game","Green Sunfish","GRSU","Exotic","NonGame","Gulf Pipefish","GUPI","Forage","NonGame","Highfin Carpsucker","HICA","Rough","NonGame","Hickory Shad","HISH","Other","NonGame","Hogchoker","HOG","Other","NonGame","Inland Silverside","INSI","Forage","NonGame","Ironcolor Shiner","IRSH","Forage","NonGame","North African Jewelfish","JEWE","Exotic","NonGame","Lake Chubsucker","LACH","Forage","NonGame","Ladyfish","LADY","Other","NonGame","Least Killifish","LEKI","Forage","NonGame","Lake Eustis Pupfish","LEMI","Forage","NonGame","Longnose Gar","LGAR","Rough","NonGame","Orinoco Sailfin Catfish","LIMU","Exotic","NonGame","Lined Topminnow","LITO","Forage","NonGame","Largemouth Bass","LMB","LMB","Game","Longear Sunfish","LOSU","Sunfishes","Game","Mayan Cichlid","MACI","Exotic","NonGame","Madtom","MADT","Catfish","NonGame","Marsh Killifish","MAKI","Forage","NonGame","Mozambique-Blie Tilapia","MBTI","Exotic","NonGame","Midas Cichlid","MICI","Exotic","NonGame","Eastern Mosquitofish","MOSQ","Forage","NonGame")
  guildc=c("Mozambique Tilapia","MOTI","Exotic","NonGame","Naked Goby","NAGO","Forage","NonGame","Nile-Blue Tilapia","NBTI","Exotic","NonGame","Nile Tilapia","NITI","Exotic","NonGame","Other Freshwater Fish","OFF","Other","NonGame","Okefenokee Pygmy Sunfish","OPS","Other","NonGame","Oscar","OSCA","Exotic","NonGame","Other Saltwater Fish","OSF ","Other","NonGame","Pinfish","PIN ","Other","NonGame","Pirate Perch","PIPE","Forage","NonGame","Pugnose Minnow","PUMI","Forage","NonGame","Pygmy Sunfish","PYSU","Other","NonGame","Quillback","QUIL","Rough","NonGame","Rainwater Killifish","RAKI","Forage","NonGame","Redbreast Sunfish","RBSU","Sunfishes","Game","Red Drum","REDR","Other","Game","Redfin Needlefish","RENE","Other","NonGame","Redear Sunfish","RESU","Sunfishes","Game","Rough Silverside","ROSI","Forage","NonGame","Redfin Pickerel","RPIK","Rough","NonGame","Russetfin Topminnow","RUTO","Forage","NonGame","Sailfin Catfish","SACA","Exotic","NonGame","Sailfin Molly","SAMO","Forage","NonGame","Sand Seatrout","SASE","Other","NonGame","Striped Bass Hybrid","SBHY","Other","Game","Seminole Killifish","SEKI","Forage","NonGame","sunfish species","SFSP","Sunfishes","Game","Shortnose Gar","SGAR","Rough","NonGame","Sheepshead","SHEE","Other","Game","Shiner Species - Notropis","SHIN","Forage","NonGame","Silver Jenny","SIJE","Other","NonGame","Silver Seatrout","SISE","Other","Game","Skipjack Herring","SKHE","Other","NonGame","Snook - Centrapominae","SNOO","Other","Game","Southern Flouder","SOFL","Other","Game","Spotted Bass","SPBA","Other","Game","Spotted Bullhead","SPBU","Catfish","NonGame","Spotfin Mojarra","SPMO","Other","NonGame")
  guildd=c("Spotted Seatrout","SPSE","Other","Game","Spotted Sunfish Hybrid","SPSH","Sunfishes","NonGame","Spotted Sucker","SPSK","Rough","NonGame","Spotted Sunfish","SPSU","Sunfishes","Game","Spotted Tilapia","SPTI","Exotic","NonGame","Sunshine Bass","SSBA","Other","Game","Striped Bass","STBA","Other","Game","Striped Mojarra","STMO","Other","NonGame","Striped Mullet","STMU","Other","NonGame","Suckermouth Catfish","SUCA","Exotic","NonGame","Sunfish Species - Lepomis","SUSP","Sunfishes","Game","Shadow Bass","SWBA","Other","Game","Swamp Darter","SWDA","Forage","NonGame","Tadpole Madtom","TAMA","Catfish","NonGame","Tarpon","TARP","Other","Game","Taillight Shiner","TASH","Forage","NonGame","Threadfin Shad","THSH","Forage","NonGame","Tidewater Silverside","TISI","Forage","NonGame","Unnamed Shiner","UNSH","Forage","NonGame","Vermiculated Sailfin Catfish","VSCA","Exotic","NonGame","Walking Catfish","WACA","Exotic","NonGame","Warmouth","WAR","Sunfishes","Game","Weed Shiner","WESH","Forage","NonGame","White Bass","WHBA","Other","Game","White Catfish","WHCA","Catfish","Game","White Mullet","WHMU","Other","NonGame","Yellow Bullhead","YEBU","Catfish","Game","Yellowbelly Cichlid","YECI","Exotic","NonGame","Yellow Perch","YEPE","Forage","Game")
  
guild_lkup = c(guilda,guildb,guildc,guildd)
out = data.frame(matrix(ncol=4, data=guild_lkup, byrow=TRUE))
names(out) = c("SpeciesCommon", "SpeciesCode", "Group1", "Group2")
return(out)
}
guilds = import_guild_table()

  #convert cpue table from wide to long
   # cpue_long = orange_all$CPUE_number

cpue_long = ltm_dataset$CPUE_number
if(length(years) > 0) {cpue_long = cpue_long %>% dplyr::filter(yr %in% years)}
cpue_long <- na.omit(cpue_long %>% gather(key="Species", value="CPUE", -Year, -yr))

# join cpue table with guild table
cpue_long = cpue_long %>% 
  dplyr::left_join(guilds %>% dplyr::select(-SpeciesCode) %>% 
              rename(Species = SpeciesCommon))

#percent composition formula
percent_comp <- function(x) { x/sum(x)}

# remove the "total group (All)", and calculate percent compositions by year
cpue_long <- cpue_long %>% dplyr::filter(Species != "All") %>% dplyr::group_by(yr) %>% mutate(comp= percent_comp(CPUE)) 
guild_fig_data <- cpue_long %>% dplyr::group_by(yr,Group1) %>% dplyr::summarise(comp=sum(comp))
gfd2 <- cpue_long %>% dplyr::group_by(yr,Group2) %>% dplyr::summarise(comp=sum(comp))

# plot

guild_plot <- ggplot2::ggplot(data= guild_fig_data, ggplot2::aes(x=yr,
                                               y=comp,
                                               fill=Group1)) +
geom_bar(position="stack", stat="identity") + 
  theme_bw() + 
  scale_y_continuous(expand = expansion(add = c(0,0))) + 
  scale_x_continuous(expand= expansion(mult = c(0.01,.01)),
                     breaks= c(min(guild_fig_data$yr):max(guild_fig_data$yr))) +
  theme(axis.text.x = element_text(angle=325, vjust=0.25,hjust=0.25),
        legend.title = element_blank(),
        panel.grid.minor= element_blank(),
        panel.grid.major= element_blank(),
        panel.background = element_rect(fill="white",color="white"),
        legend.position= "bottom") + 
  ylab("Percent Composition by Number") + 
  xlab("Year")
if(color_scale == "grayscale") {guild_plot = guild_plot + scale_fill_grey()} else
  if(color_scale == "viridis") {guild_plot = guild_plot + scale_fill_viridis(
    discrete=TRUE, option="magma", alpha = 0.7, direction=-1)}
print(guild_plot)

print(cpue_long %>% ungroup() %>% select (Species, Group1, Group2) %>% distinct(), n = 100)

guild_plot2 <- ggplot2::ggplot(data= gfd2, ggplot2::aes(x=yr,
                                               y=comp,
                                               fill=Group2)) +
  geom_bar(position="stack", stat="identity") + 
  theme_bw() + 
  scale_y_continuous(expand = expansion(add = c(0,.0))) + 
  scale_x_continuous(expand= expansion(mult = c(0.01,.01)),
                     breaks= c(min(gfd2$yr):max(gfd2$yr))) +
  theme(axis.text.x = element_text(angle=325, vjust=0.25,hjust=0.25),
        legend.title = element_blank(),
        panel.grid.minor= element_blank(),
        panel.grid.major= element_blank(),
        panel.background = element_rect(fill="white",color="white"),
        legend.position = "bottom") + 
  ylab("Percent Composition by Number") + 
  xlab("Year")
if(color_scale == "grayscale") {guild_plot2 = guild_plot2 + scale_fill_grey()} else
  if(color_scale == "viridis") {guild_plot2 = guild_plot2 + scale_fill_viridis(
    discrete=TRUE, option="magma", alpha = 0.7, direction=-1)}
print(guild_plot2)


  if(save==TRUE) {
    tiff("Percent_Comp_Guild1.tiff", res = 300, height=1800, width=2100, compression = 'lzw')
    print(guild_plot)
    dev.off()
    tiff("Percent_Comp_Guild2.tiff", res = 300, height=1800, width=2100, compression = 'lzw')
    print(guild_plot2)
    dev.off()
  }

return(list(Guild1 = guild_fig_data,
            Guild2 = gfd2))
}

# species.history --------------------------------------------------------------

################################################################################
################################################################################
#' Species detection history
#' 
#' Create Species detection history plot
#' @param ltm_dataset data, should be output from ltm.data.summary function
#' @param save boolean, if TRUE figure will be saved to file
#' @param filename = filename for saved figure
#' @param fig_res = specify figure resolution
#' @param fig_width = specify figure width
#' @param fig_height = specify figure height  
#' @return returns figure
#' @examples
#' data(newnans)
#' newn_sum <- ltm.data.summary(file=newnans)
#' newnans_dethist <- species.history(newn_sum,save=FALSE)
#' @export
species.history <- function(LTMdataset,
                            save = FALSE,
                            filename = NA,
                            fig_res = NA,
                            fig_width = NA,
                            fig_height = NA) {
  
  require(tidyr)
  require(tibble)
  require(ggplot2)
  require(stringr)

  plotData <- data.frame(LTMdataset$SpeciesHistory) %>% 
    dplyr::select(-"sunfish.species") %>%
  rownames_to_column(var="Year") %>% 
  tidyr::pivot_longer(-Year, names_to = "Species", values_to = "detection")
  
  yr_ct = plotData %>% dplyr::group_by(Year) %>% dplyr::summarise(total = sum(detection,na.rm=T)) 
  sp_ct = plotData %>% dplyr::group_by(Species) %>% dplyr::summarise(total = sum(detection, na.rm=T))

    detection_plot <- ggplot2::ggplot(data = plotData, ggplot2::aes(x=Year,y=reorder(Species, desc(Species)), fill = as.factor(detection))) + 
      geom_tile(width = 0.95, height = 0.95) +
      ylab("Species") +  
      scale_fill_manual(name="Detection", 
                        labels= c("0" = "Not Detected","1" = "Detected"),
                        values= c("1" = "#689f38","0" = "#DEDEDE")) +
      scale_x_discrete(position="top") +
      coord_cartesian(clip= "off",
                      expand=FALSE,
                      ylim=c(0.5,nrow(sp_ct)+0.5),
                      xlim=c(0.5,nrow(yr_ct)+0.5)) + #, ylim=c(0,NA), expand = FALSE) # +
      geom_text(data = yr_ct, 
                ggplot2::aes(x=Year, y=-0.25, fill = NA,label=total), size =3) + 
      geom_text(ggplot2::aes(x= -1, y=-0.25, fill=NA, label = "Total Species:"), hjust=0.25,size=3) +
      geom_text(data = sp_ct, 
                ggplot2::aes(x=nrow(yr_ct)+1, y=Species, fill = NA,label=total), size =3) + 
      geom_text(ggplot2::aes(x= nrow(yr_ct)+1, y=nrow(sp_ct)+1, fill=NA, label = "Total\nYears"),
                hjust=0.5,
                vjust= 0.25,
                size=3) +
      theme_bw() + 
      theme(legend.title = element_blank(),
            panel.background = element_rect(fill ="black"),
            panel.grid = element_blank(),
            legend.position = "bottom",
            legend.background = element_blank(),
            axis.ticks = element_blank(),
            legend.box.margin = margin(t = 6),
            axis.text = element_text(color="black"),
            plot.margin= margin(r= 30))

print(detection_plot)

#
fn = "Species_Detection_History.tiff"
if(!is.na(filename)) {fn = filename}
if(substr(fn,-6,-1) != ".tiff") {fn = base::paste(fn,".tiff",collapse="")}

rs = ifelse(is.na(fig_res), 300, fig_res)
ht = ifelse(is.na(fig_height), length(unique(plotData$Species))*rs/4.5, fig_height)
wid = ifelse(is.na(fig_width), length(unique(plotData$Year))*rs/1.75, fig_width)

  if(save == TRUE) {  
      tiff(fn, 
           height = ht, 
           width = wid, res = rs,
           compression = "lzw")
      print(detection_plot)
      dev.off()
  }
}



# age.key --------------------------------------------------------


################################################################################
################################################################################
#' Create age-length key
#' 
#' Generate age-length key from a subsample of aged fish
#' @param dataset dataset containing at least 2 columns, one containing an age field, and one containing length field
#' @param age_column string specifying the name of the column containing ages (in years)
#' @param length_column string specifying the name of the column containing lengths
#' @param length_unit string specifying the length units options are: "mm" = millimeters; "cm" = centimeters, if none specified defaults to "mm"
#' @return returns age-length key
#' @examples 
#' data(loch_blcr_2020_age)
#' loch_blcr_alk <- age.key(dataset=loch_blcr_2020_age, age_column="age", length_column="tl", length_unit = 'mm')
#' @export
age.key <- function(dataset,age_column, length_column, length_unit = NA) {
  
  dat = dataset %>% dplyr::select(age = age_column, len = length_column) 
  
  if(is.na(length_unit)) {print("no length unit specified, assuming mm")
    length_unit = "mm"}
  
  #convert length to cm
  if(length_unit != "cm") {
    if(length_unit != "mm") {
      stop("length unit not recognized")
    } else {
      dat$len = floor(dat$len/10)
    }
  }
  
  # Add in missing length and age categories 
  len_range = data.frame(cbind(len = 0:max(dat$len,na.rm=T)))
  age_range = data.frame(cbind(age = min(dat$age, na.rm=T):max(dat$age,na.rm=T)))
  dat = dat %>% full_join(len_range) %>% full_join(age_range) 
  
  count_table = with(dat,table(len,age))
  prop_table = prop.table(count_table, margin = 1)
  prop_table = data.frame(matrix(ncol=ncol(prop_table), data = prop_table, byrow=FALSE))
  names(prop_table) = age_range$age
  prop_table[is.na(prop_table)] = 0
  len_props = apply(prop_table,1,sum)
  
  #set age of lengths smaller than the smallest aged fish to the age of the smallest aged fish
  step=1
  while(step < length(len_props)){
    if(len_props[step] == 0) {prop_table[step,1] = 1} else {break}
    step = step+1
  }
  
  
  return(prop_table)
  
}

################################################################################
################################################################################
#' Calculate Relative Weight
#' 
#' Calculates the Relative Weight for a sample of fish with both lengths and weights
#' @param dataset dataset containing at least 2 columns, one containing a length field, and one containing weight field
#' @param length_col string specifying the name of the column containing length
#' @param weight_col string specifying the name of the column containing weights
#' @param a value of the alpha parameter for relative weight calculation (specific to species of interest)
#' @param b value of the beta parameter for relative weight calculation (specific to species of interest)
#' @return returns vector of relative weights
#' @examples 
#' library(dplyr)
#' data(newnans)
#' #Calculate relative weight for all LMB collected from Newnans Lake
#' newn_lmb <- newnans %>% dplyr::filter(SpeciesCode == "LMB" )
#' newnans_lmb_Wr <- Wr(dataset = newn_lmb, 
#'                  length_col = "TotalLength",
#'                  weight_col = "TotalWeight",
#'                  a = .3245,
#'                  b = 3.21)
#' @export
Wr <- function(dataset = NA, 
               length_col = NA, 
               weight_col = NA,
               a= NA, 
               b= NA) {
  dat = dataset %>% dplyr::select(len = all_of(length_col),wt = all_of(weight_col))
  wt_pred = with(dat,10^((b*log10(len) + a)))
  return(dat$wt/wt_pred*100)
}