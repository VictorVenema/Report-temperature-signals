read_data_compute_trends <- function(baseDataDir, metaData, homStr, varStr, varFileStr, globalDatasetStr, firstYears, lastCommonYear) {
  
  source("compute_trends.R")
  
  noSets   = length(globalDatasetStr) # Number of global datasets
  noSeries = nrow(metaData) # Number of regional/national datasets. 
  noTrends = length(firstYears)
  
  for( iSeries in 1:noSeries ) {
    #   if(iSeries <= noSeries/2) {
    #     iHom = 1
    #   } else {
    #     iHom = 2
    #   }
    # Create national/regional input and output file names for this series
    if(metaData$homogenized[iSeries]==TRUE) {
      fileName = paste(metaData[iSeries, 3], metaData[iSeries, 2], homStr[1], varStr, varFileStr, "txt", sep=".")
    } else {
      fileName = paste(metaData[iSeries, 3], metaData[iSeries, 2], homStr[2], varStr, varFileStr, "txt", sep=".")    
    }
    inputDirFileNameNat = paste(baseDataDir, fileName, sep="/") # dirFileNames[[iSeries]]   
    if(file.exists(inputDirFileNameNat)) {
      dataNat = read.table(inputDirFileNameNat, header=FALSE, fill=FALSE, na.strings="-99.99")
      
      for(iSet in 1:noSets) {
        yearNat = dataNat$V1
        tempNat = dataNat$V2
        
        # Read data global/European collections
        # homStr is part of globalDatasetStr
        inputDirFileNameColl = paste(baseDataDir, paste(metaData[iSeries, 3], globalDatasetStr[[iSet]], varStr, varFileStr, "txt", sep="."), sep="/")
        
        if(file.exists(inputDirFileNameColl)) {
          print(inputDirFileNameColl)
          dataColl = read.table(inputDirFileNameColl, header=FALSE, fill=FALSE, na.strings="-99.99")
          
          yearGlobal = dataColl$V1
          tempGlobal = dataColl$V2
          #     plot(yearGlobal, tempGlobal)
          
          # Compute common period and cut the national and the global datasets to this period
          minGlobal = min(yearGlobal)
          minNat    = min(yearNat)  
          beginYears[iSet,iSeries] = max(c(minGlobal, minNat))
          maxGlobal = max(yearGlobal)
          maxNat    = max(yearNat)  
          lastYears[iSet,iSeries] = min(c(maxGlobal, maxNat))
          #         print(c(beginYears[iSet,iSeries], lastYears[iSet,iSeries]))
          
          index      = which(yearNat == beginYears[iSet,iSeries])  
          indexBegin = index[1]
          index      = which(yearNat == lastYears[iSet,iSeries])
          indexEnd   = index[length(index)]
          
          yearNat = yearNat[indexBegin:indexEnd]
          tempNat = tempNat[indexBegin:indexEnd]
          
          #         print("cut global")
          index      = which(yearGlobal == beginYears[iSet,iSeries])  
          indexBegin = index[1]
          index      = which(yearGlobal == lastYears[iSet,iSeries])
          indexEnd   = index[length(index)]
          
          year = yearGlobal[indexBegin:indexEnd] # This vector is the common year of the global and national dataset
          tempGlobal = tempGlobal[indexBegin:indexEnd]
          diff = tempNat-tempGlobal
          
          # Make plot
          if( FALSE) {
            # if( TRUE) {
            #       windows(bg = "white") #  X11()
            windows() #  X11()
            titleStr = paste("Temperature difference ", metaData[iSeries, 1], " (national - ", globalDatasetStr[[iSet]], ") ", sep="")
            
            plot(year, diff, type="l", col="black", lwd="6", xlab="Year", ylab="Temperature difference [°C]")
            title(main=titleStr) 
            #   legend("topleft", inset=.05, legend=c("Manual", "Automatic"), fill=c("black", "red"))
            
            fit = lm(diff~ year)
            abline(fit, col="#FF0000", lwd=2)
            sumfit=summary(fit)
            # Simplify
            ymin = min(min(diff, na.rm = TRUE), min(diff, na.rm = TRUE))
            ymax = max(max(diff, na.rm = TRUE), max(diff, na.rm = TRUE))
            xmin = min(min(year, na.rm = TRUE), min(year, na.rm = TRUE))
            xmax = max(max(year, na.rm = TRUE), max(year, na.rm = TRUE))
            spany = ymax-ymin
            spanx = xmax-xmin
            text(x=xmin+0.05*spanx,y=ymax-0.12*spany,  labels=paste("R2 = ",   format(sumfit$r.squared,             digits=2)), adj = c(0,0), col="gray40")
            text(x=xmin+0.05*spanx,y=ymax-0.025*spany, labels=paste("Coeff =", format(sumfit$coefficients[2,1]*100, digits=3)), adj = c(0,0), col="gray40")    
            
            #     globalDatasetStr[[iSet]]
            fileName = paste(metaData[iSeries, 2], "_difference_annual_", globalDatasetStr[[iSet]], ".png", sep="")
            dirFileName = paste(basePlotDir, fileName, sep="/")
            savePlot(filename = dirFileName, type="png")
            #       png(filename = dirFileName)
            dev.off()
          } # End if for bool make a plot
          yearg      [[iSet,iSeries]] = year
          tempNatg   [[iSet,iSeries]] = tempNat
          tempGlobalg[[iSet,iSeries]] = tempGlobal
          tempDiffg  [[iSet,iSeries]] = diff
          
          # Compute trends for the various begin years
          for(iTrend in 1:noTrends) {
            firstYearTrend = firstYears[iTrend]
            if(iTrend == 1) { # For the first trend, the entire period is used, for the others a fixed begin year (given by firstYears) and a common end year (first ending of all datasets)
              lastYearTrend = lastYears[iSet,iSeries]
            }
            else {          
              lastYearTrend = lastCommonYear
            }
            trends = compute_trends(trends, year, tempNat, tempGlobal, firstYearTrend, beginYears[iSet,iSeries], lastYearTrend, lastYears[iSet,iSeries], iTrend, iSet, iSeries)
          }
        } else {
          yearg      [[iSet,iSeries]] = NA
          tempNatg   [[iSet,iSeries]] = NA
          tempGlobalg[[iSet,iSeries]] = NA
          tempDiffg  [[iSet,iSeries]] = NA
        } # If global collection file exists
      } # End for over global sets
    } else {
      for(iSet in 1:noSets) {
        yearg      [[iSet,iSeries]] = NA
        tempNatg   [[iSet,iSeries]] = NA
        tempGlobalg[[iSet,iSeries]] = NA
        tempDiffg  [[iSet,iSeries]] = NA
      } # End for loop over all sets 
    } # End if national series exists 
  } # End for over national series
  print("Ready computing trends")

  output = list(trends=trends, yearg=yearg, tempNatg=tempNatg, tempGlobalg=tempGlobalg, tempDiffg=tempDiffg)
}