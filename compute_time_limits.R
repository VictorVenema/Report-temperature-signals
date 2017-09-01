compute_time_limits <- function(tempNatg, tempGlobalg) {
  
  minYear = array(1e4, c(noSets, noSeries))
  maxYear = array(0,   c(noSets, noSeries))
  for(iSet in 1:noSets) {
    for( iSeries in 1:noSeries ) {
      #     if(is.na(tempNatg[[iSet,iSeries]]) & is.finite(tempNatg[[iSet,iSeries]])) {
      if(is.finite(tempNatg[[iSet,iSeries]][1])) {
        tempNatg       [[iSet,iSeries]] = tempNatg   [[iSet,iSeries]] - mean(tempNatg   [[iSet,iSeries]], na.rm = TRUE)
        tempGlobalg    [[iSet,iSeries]] = tempGlobalg[[iSet,iSeries]] - mean(tempGlobalg[[iSet,iSeries]], na.rm = TRUE)
        
        year = yearg[[iSet,iSeries]]
        diff = tempNatg[[iSet,iSeries]] - tempGlobalg[[iSet,iSeries]] # tempDiffg[[iSet,iSeries]] 
        ymin = min(min(diff, na.rm = TRUE), min(tempNatg[[iSet,iSeries]], na.rm = TRUE), min(tempGlobalg[[iSet,iSeries]], na.rm = TRUE), na.rm = TRUE)
        ymax = max(max(diff, na.rm = TRUE), max(tempNatg[[iSet,iSeries]], na.rm = TRUE), max(tempGlobalg[[iSet,iSeries]], na.rm = TRUE), na.rm = TRUE)
        xmin = min(min(year, na.rm = TRUE))
        xmax = max(max(year, na.rm = TRUE))
        minYear[iSet, iSeries] = xmin
        maxYear[iSet, iSeries] = xmax
      }
    }
  }
  maxYear[maxYear==0] = NA
  minYear[minYear==1e4] = NA
  
  limits = list(maxYear=maxYear, minYear=minYear)
}
