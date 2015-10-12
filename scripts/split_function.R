#function to split out the jags indexing
#@data x = what to run on (scalar)
#@param maxLength = number of columns to split into, must be greater
#than number of digits
#@param leftFill= T if fill NAs from L (default), F if fill from Right
#by Sarah Elmendorf

mysplit<-function(x,maxLength, leftFill=T){
  xtemp<-as.numeric(unlist(strsplit(gsub('[A-z]', '', x),split=',')))
  tofill<-maxLength-length(xtemp)
  if (leftFill){
    xtemp<-c(rep(NA, tofill), xtemp)}else{
      xtemp<-c(xtemp, rep(NA, tofill))
    }
  return(xtemp)
}