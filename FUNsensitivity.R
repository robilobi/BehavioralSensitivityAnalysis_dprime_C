dprime <- function(response, signal, units = NULL, conditions = NULL, hitmax , famax ){
  
  #Checks for specification of units & conditions.  Assigns arbitrary values if not specified
  if(is.null(units)){
    tmp.units <- rep(1, length(response))
    unit.labels <- 1
  }
  else {
    tmp.units <- units
    unit.labels <- names(table(units))
  }
  if(is.null(conditions)){
    tmp.conditions <- rep(1, length(response))
    condition.labels <- 1
  }
  else {
    tmp.conditions <- conditions
    condition.labels <- names(table(conditions))
  }
  
  #Create table(s) of observations
  tmp<-table(response, signal, factor(tmp.units), tmp.conditions)
  #replace zeros to ensure no dividing by zero
  #tmp[tmp==0] <- .01 
  
  tp <- tmp[2,2,,]/(tmp[2,2,,]+tmp[1,2,,]) #hits
  fa <- tmp[2,1,,]/(tmp[2,1,,]+tmp[1,1,,]) #false alarms
  
  tp[tp==1]=1 - 1/(2*hitmax)
  fa[fa==1]=1 - 1/(2*famax)
  fa[fa==0]=1/(2*famax)
  tp[tp==0]=1/(2*hitmax)
  
  dprime <- qnorm(tp) - qnorm(fa) #dprime
  c <- -.5*((qnorm(tp) + qnorm(fa))) #c
  
  df<-data.frame('unit' = rep(unit.labels, length(condition.labels)),
                 'hit.rate' = c(tp), 
                 'false.alarm.rate' = c(fa), 
                 'dprime' = c(dprime), 
                 'criterion' = c(c), 
                 'condition' = rep(condition.labels, each=length(unit.labels))
  )
}