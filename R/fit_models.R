cleanAndReadCT2_out <- function(fName, colsExp){
  
  inpLines <- readLines(fName)
  headerLine <- paste0(unlist(str_split(inpLines[[1]],',')),collapse = ',')
  # adding a double quote before square bracket opening and and after a square bracket ending
  cleanted_fName <- gsub('.csv','_cleaned.csv',fName)
  writeLines(append(headerLine,unlist(lapply(2:length(inpLines), function(iLine) gsub("\\]",'\\]"',gsub("\\[",'"\\[',unlist(str_split(inpLines[[iLine]],',\\[\\['))[[1]]))))),
             cleanted_fName)
  interim <- read.csv(cleanted_fName) %>% data.table
  
  # Fucntion to straighten the data where there is array embedding
  #  dt_in is the data.table
  #  colsExp is the vector of columns (integer positions) that needs to be straightened
  dt <- copy(interim)
  for (j in colsExp)
    set(dt,NULL,j,gsub("\\]",'',gsub("\\[",'',dt[[j]])))
  numRepl <- unlist(lapply(dt[[colsExp[1]]],function(x) length(unlist(str_split(x,',')))))
  dt$numRepl <- numRepl
  dt_exp <- dt[,lapply(.SD,rep,numRepl)]
  for (j in colsExp){
    set(dt_exp,NULL,j,as.integer(unlist(lapply(dt[[j]],str_split,','))) )
  }
  return(dt_exp)
}