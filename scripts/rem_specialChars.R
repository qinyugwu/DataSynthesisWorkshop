rem_specialChars <- function(df, cols){
  # check that all cols exist in df
	notInDF <- which(!cols %in% names(df))
	if (length(notInDF) > 0) {
	  stop(paste("variable", cols[notInDF], "is not in data frame"))
	} 
	# convert to character 
	df[,cols] <- apply(df[,cols],2,as.character)

  #define subfunction to turn nonascii to spaces
  ASCIIfy<-function(string){
    require (stringi, quiet =T) 
    if(!is.na(string)){
      if(nchar(string)>0){
        splitString<-plyr::laply(seq(1,nchar(string),1), function(i) substr(string, i, i))
        nonASCII<-which(!stri_enc_isascii(splitString))
        splitString[nonASCII]<-' '
        string<-paste(splitString, collapse='')
      }
    }
    return (string)
  }
  
  colNums <- which(names(df) %in% cols)
  #Remove leading and trailing whitespace, newline characters, tab characters, etc.
  for (i in colNums){
    for (j in 1:nrow(df)){
      df[j,i] <- lapply (df[j,i], ASCIIfy)  #sub in whitespace for nonascii
      df[j,i] <- lapply (df[j,i], function(x)(gsub('\\n', ' ',x)))  #Remove newline characters
      df[j,i] <- lapply (df[j,i], function(x)(gsub('\\t', ' ',x)))  #Remove tab characters
      df[j,i] <- lapply (df[j,i], function(x)(gsub('\\r', ' ',x)))  #Remove carriage return characters
      df[j,i] <- lapply (df[j,i], function(x)(gsub('\\f', ' ',x)))  #Remove form feed characters
      
      #Convert double quotes to single quotes (for correct reading in excel)
      df[j,i] <- lapply (df[j,i], function(x)(gsub("\"", "\\\'",x)))
      
      #Convert two or more spaces to a single space
      df[j,i] <- lapply (df[j,i], function(x)(gsub("(?<=[\\s])\\s*|^\\s+$", "", x, perl=TRUE)))
      
      #Remove leading/trailing whitespace
      df[j,i] <- lapply (df[j,i], function(x)(gsub("^\\s+|\\s+$|\\s\\s", '',x)))
    }
  }
  return(df)
  
  #Description: Removes special characters from records in select columns, 
  # and removes non-ascii characters, replacing with a single white space.
  # Columns are those containing character data, and these must be specified as a list
  # of column names. Special characters removed are: newline characters, tab characters,
  # carriage return characters, form feed characters, leading & trailing whitespaces,
  # double quotes within the string ("" is converted to ''), and extra white spaces 
  # (if there is >1 space, only one is kept.)
  #
  # Author: Natalie Robinson, 7/20/2015 
  # Modified 9/9/2015 by SCE to add in informative error messages for 
  # when applied to non-character variables and to add in the de-asciification

  # Example: 
  ## Load data
  #inData<-read.csv("~/GitHub/organismalIPT/beetles/ExampleBeetleData/FakeBeetle_FieldData.csv")
  #
  ## Run function
  #outData<-rem_specialChars(df=inData,cols=c('plotID','recordedBy','sampleID'))
  
  #suggested ATBD text:
  # 1.  Replace all non-leading/trailing line endings, tabs, ctrl-R in remarks
  # fields with a single space
  # 2.  Replace double quotes in strings with single quotes
  # 3.	Replace any non-ASCII characters in all remarks fields with a single
  #space
  # 4.	Trim (i.e., remove all leading/trailing line endings, tabs, ctrl-R
  # and spaces) in remarks fields
}
