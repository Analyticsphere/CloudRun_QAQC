library(plumber)
library(lubridate)
library(bigrquery)
library(jsonlite)
library(stringr)
library(dplyr)
library(withr)
library(tibble)
library(rio)



#* heartbeat...
#* @get /
#* @post /
function(){
  return("alive")
}

#* @get /debug
#* @post /debug
#* @serializer json
function(){
  xx =list(indebug=TRUE)
  tryCatch({
    xx$start=TRUE
    cat("Attempting to aquire token ...\n")
    bq_auth()
    token = bq_token()
    xx$have_token=bq_has_token()
    cat("\tDo I have a token: ",xx$have_token,"\n")
    xx$done=TRUE
  },
  error=function(){
    cat("ERROR ",e,"\n")
  }
  )
  toJSON(xx,auto_unbox = T)
}

#* Runs PROD qa_qc
#* @get /qaqc
#* @post /qaqc
#* @serializer json
function() {
  retval=list()
  tryCatch(
    {
      #  read dictionary from bucket to an R object (warning, don't run out of RAM if its a big object)
      # the download type is guessed into an appropriate R object
      dictionary = rio::import("https://episphere.github.io/conceptGithubActions/aggregate.json",format = "json")
      retval["dictLen"] = length(dictionary)
      
      # BigQuery table where QC report will be saved---------------
      QC_report_location = "nih-nci-dceg-connect-prod-6d04.Connect.QC_report"
      
      # 2 part definition for querying the data sitting in BigQuery
      project = "nih-nci-dceg-connect-prod-6d04"
      sql = "SELECT * FROM `nih-nci-dceg-connect-prod-6d04.Connect.recruitment2`"
      
      # sites:
      # Sanford Health = 657167265
      # HealthPartners = 531629870
      # Henry Ford Health System = 548392715
      # Kaiser Permanente Colorado = 125001209
      # Kaiser Permanente Georgia = 327912200
      # Kaiser Permanente Hawaii = 300267574
      # Kaiser Permanente Northwest = 452412599
      # Marshfiled = 303349821
      # University of Chicago Medicine = 809703864
      # National Cancer Institute = 517700004
      # Other = 181769837
      
      # define site to run QC -----------
      # Sanford
      site= 657167265 
      retval['Sanford']=runQC(site= site, project= project, sql= sql, QC_report_location = QC_report_location, dictionary=dictionary)
      
      # define site to run QC----------
      # HealthPartners
      site= 531629870 
      retval['HealthPartners']=runQC(site= site, project= project, sql= sql, QC_report_location = QC_report_location, dictionary=dictionary)
      
      # define site to run QC----------
      # Henry Ford Health System
      site= 548392715 
      retval['HFHS']=runQC(site= site, project= project, sql= sql, QC_report_location = QC_report_location, dictionary=dictionary)
      
      # define site to run QC----------
      # Kaiser Permanente Colorado
      site= 125001209 
      retval['KPCO']=runQC(site= site, project= project, sql= sql, QC_report_location = QC_report_location, dictionary=dictionary)
      
      # define site to run QC----------
      # Kaiser Permanente Georgia
      site= 327912200 
      retval['KPGA']=runQC(site= site, project= project, sql= sql, QC_report_location = QC_report_location, dictionary=dictionary)
      
      # define site to run QC----------
      # Kaiser Permanente Hawaii
      site= 300267574 
      retval['KPHI']=runQC(site= site, project= project, sql= sql, QC_report_location = QC_report_location, dictionary=dictionary)
      
      # define site to run QC----------
      # Kaiser Permanente Northwest
      site= 452412599 
      retval['KPNW']=runQC(site= site, project= project, sql= sql, QC_report_location = QC_report_location, dictionary=dictionary)
      
      # define site to run QC----------
      # Marshfiled
      site= 303349821 
      retval['Marshfield']=runQC(site= site, project= project, sql= sql, QC_report_location = QC_report_location, dictionary=dictionary)
      
      # define site to run QC----------
      # University of Chicago Medicine
      site= 809703864 
      retval['UC']=runQC(site= site, project= project, sql= sql, QC_report_location = QC_report_location, dictionary=dictionary)
      
      # define site to run QC----------
      # National Cancer Institute
      site= 517700004 
      retval['NCI']=runQC(site= site, project= project, sql= sql, QC_report_location = QC_report_location, dictionary=dictionary)
      
      # define site to run QC----------
      # Other
      site= 181769837 
      retval['OTHER']=runQC(site= site, project= project, sql= sql, QC_report_location = QC_report_location, dictionary=dictionary)
    },
    error=function(e){
      message("caught error ",e)
      retval['note'] = paste0("caught error: ",e)
      retval['error'] = e
    })
  
  toJSON(retval,auto_unbox = T)
}


# function to translate QC report inside runQC function-----------------------------
TRANSLATE.COL <- function(report, translate.these.cols, new.col.names, dictionary ){
  
  # read dictionary json
  dict <- dictionary
  new_error_report = report
  # translate 1 or more columns
  for (columnIndex in 1:length(translate.these.cols)){
    translate.this.col = translate.these.cols[columnIndex]
    new.col.name = new.col.names[columnIndex]
    
    # add new translated column
    p1= paste0("add_column(new_error_report", ",")
    p2= new.col.name
    p3= paste0("=NA, .after = translate.this.col)")
    txt=paste0(p1,p2,p3)
    new_error_report <- eval(parse(text=txt))
    ############# initialize row counts for translation loop
    run = 1
    ############ begin translation -----------
    while(run <= length(new_error_report[[translate.this.col]])){
      for(row in new_error_report[[translate.this.col]]){
        if(grepl(pattern=",",row)){
          newRow2 = c()
          row2 = as.numeric(strsplit(row,",")[[1]])
        }else {
          newRow2 = c()
          row2 = as.numeric(row)
        }
        ############# START 1st "ELSE IF" LOGIC TO CHECK ROW FORMAT #############
        #("NA", string, integer list plus an "NA", or integer list,) 
        #----------------------------------------- if row is missing
        if(is.na(row)| row=="") {
          na_str = ""
          ab = ""
          #----------------------------------------- else if row is non number
        }else if (!is.na(row) & is.na(row2)) {
          na_str = ""
          ab= row
          #----------------------------------------- else if row is not blank because it has numbers and an NA!!
        }else if (( testInteger(as.numeric(strsplit(gsub(", NA", "" , row),",")[[1]])) |
                    testInteger(as.numeric(strsplit(gsub(", NA", "" , row),",")[[1]]))) &
                  !is.na(row2) & (grepl( ", NA", row, fixed = TRUE) | grepl( ", NA,", row, fixed = TRUE))) {
          row = gsub(", NA", "" , row)
          row = gsub(", NA,", "" , row) # remove na in row and redefine row2 (numbers)
          row2 = as.numeric(strsplit(row,",")[[1]])
          ab = paste0("dict$","\"",row2,"\"", collapse=NULL)
          na_str = ", NA"
          #----------------------------------------- else if row is number list
        }else if( testInteger(row2) & !is.na(row2) & !is.na(row) & row !=""){
          ab = paste0("dict$","\"",row2,"\"", collapse=NULL)
          na_str = ""
        }
        ############# START 2nd "ELSE IF" LOGIC TO TRANSLATE INTEGER LIST of 1 or more CIDs OR KEEP BLANK ROWS AND STRING ROWS AS IS ########
        # for list of concept IDs (fixed 0517 Lorena)
        if(length(ab)>1){
          for(cid in ab){
            newRow = paste(eval(parse(text=cid)),sep=",")
            newRow2 = c(newRow2,newRow)
          }
          # for single concept ID
        }else if(numbers_only(row)){
          newRow2 = eval(parse(text=ab))
          # for value other than single concept ID or ConceptID list 
        }else if(length(ab)==1){
          newRow2 = ab
        }          
        ############# BEGIN REPLACING TRANSLATED VALUES INTO NEW COLUMN ##########################
        newRow3 = paste0(toString(newRow2,sep = ", "), na_str)
        new_error_report[[new.col.name]][[run]] = newRow3
        run = run+1
      } }}
  
  return(new_error_report)
}

# function to test vector for integers
testInteger <- function(x){test <- all.equal(x, as.integer(x), check.attributes = FALSE)
if(test == TRUE){ return(TRUE)
} else { return(FALSE) }
}

# function to check that it does not match any non-number
numbers_only <- function(x) !grepl("\\D", x)

# function to exclude rows with certain values in QC (ie. "d" not in list "a,b,c")
"%!in%" <- function(x,y)!("%in%"(x,y))


# function to run QC by site--------------------------------------------
runQC = function(site,project, sql, QC_report_location, dictionary ){
  warning( "starting runQC")
  message(" QA/QC .... ",site)
  bq_auth()
  
  #GET RECRUITMENT TABLES FROM BIGQUERY IN PROD PROJECT
  # set project
  project <- project
  
  # set query
  sql <- sql
  tb <- bq_project_query(project, sql)
  connectData = bq_table_download(tb, bigint = c("character"))
  
  # FILTER DATA BY SITE CONCEPT ID
  # HealthPartners = 531629870
  # Henry Ford Health System = 548392715
  # Kaiser Permanente Colorado = 125001209
  # Kaiser Permanente Georgia = 327912200
  # Kaiser Permanente Hawaii = 300267574
  # Kaiser Permanente Northwest = 452412599
  # Marshfiled = 303349821
  # Sanford Health = 657167265
  # University of Chicago Medicine = 809703864
  # National Cancer Institute = 517700004
  # Other = 181769837
  
  # choose CID from above to filter by site
  site= 657167265
  connectData = connectData[connectData$d_827220437 == site & !is.na(connectData$d_827220437),]   
  #connectData = connectData %>% mutate(across(everything(), as.character)) # added 0527 to change int64 to string, but using newer version below 
  
  # changed int64 to string and leave dates as date type to prevent missing data. Integer blanks are NA, charater blanks are "".
  connectData = as_tibble(connectData) %>% mutate_if(~!is.POSIXct(.x), as.character)  
  
  # make qc dataframe
  df = data.frame(matrix( nrow=197, ncol=8))
  
  names(df) = c("ConceptID","QCtype","valid_values","condition", "invalid_values_found", "row_number", "token", "ConnectID")
  
  ######## QC d_512820379
  # valid value check
  
  d_512820379= c(180583933, 486306141, 854703046, NA)
  
  QCcheck1 =which(connectData$"d_512820379"%!in%d_512820379)
  
  d_512820379_invalid = addNA(connectData$"d_512820379"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[2,1]<-substr(paste0("d_512820379"),3,100)
  
  df[2,2]<-paste0("VALID")
  
  df[2,3]<-paste0(toString(d_512820379))
  
  df[2,4]<-paste0(toString("d_512820379 != "),toString(d_512820379),sep=" ")
  
  df[2,5]<-paste0(d_512820379_invalid, collapse=", ")
  
  df[2,6]<-paste0(rowNum, collapse=", ")
  
  df[2,7]<-paste0(token, collapse=", ")
  
  df[2,8]<-paste0(ID, collapse=", ")
  ######## QC d_471593703
  # cross valid equal to or less than char() check - checks values if condition one is met and checks values if condition is not met
  
  d_471593703_a = c(486306141, 854703046)
  
  
  mylist_a1 =  paste0(rep("connectData$d_512820379 == "), c(486306141, 854703046), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  
  valid_length= c(24)
  
  var.is.integer =suppressWarnings(testInteger(connectData$"d_471593703"[aa]))
  
  variable =connectData$"d_471593703[aa]"
  
  list_lengths = sapply(variable,nchar)
  
  d_471593703_invalid_char_length = list_lengths[list_lengths > valid_length]
  
  d_471593703_invalid_char_length = list_lengths[list_lengths > valid_length]
  
  df[3,1]<-substr(paste0("d_471593703"),3,100)
  
  df[3,2]<-paste0("CROSSVALID1 EQUAL TO OR LESS THAN CHAR() ",valid_length)
  
  df[3,3]<-paste0("char length",24)
  
  df[3,4]<-paste0("char length NOT =",24)
  
  df[3,5]<-paste0(d_471593703_invalid_char_length, collapse=", ") 
  ######## QC d_934298480
  # valid value check
  
  d_934298480= c(124276120, 450985724, 363147933, 636706443, 771230670, NA)
  
  QCcheck1 =which(connectData$"d_934298480"%!in%d_934298480)
  
  d_934298480_invalid = addNA(connectData$"d_934298480"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[4,1]<-substr(paste0("d_934298480"),3,100)
  
  df[4,2]<-paste0("VALID")
  
  df[4,3]<-paste0(toString(d_934298480))
  
  df[4,4]<-paste0(toString("d_934298480 != "),toString(d_934298480),sep=" ")
  
  df[4,5]<-paste0(d_934298480_invalid, collapse=", ")
  
  df[4,6]<-paste0(rowNum, collapse=", ")
  
  df[4,7]<-paste0(token, collapse=", ")
  
  df[4,8]<-paste0(ID, collapse=", ")
  ######## QC d_849518448
  # valid value check
  
  d_849518448= c(768826601, 181769837, 178420302, NA)
  
  QCcheck1 =which(connectData$"d_849518448"%!in%d_849518448)
  
  d_849518448_invalid = addNA(connectData$"d_849518448"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[5,1]<-substr(paste0("d_849518448"),3,100)
  
  df[5,2]<-paste0("VALID")
  
  df[5,3]<-paste0(toString(d_849518448))
  
  df[5,4]<-paste0(toString("d_849518448 != "),toString(d_849518448),sep=" ")
  
  df[5,5]<-paste0(d_849518448_invalid, collapse=", ")
  
  df[5,6]<-paste0(rowNum, collapse=", ")
  
  df[5,7]<-paste0(token, collapse=", ")
  
  df[5,8]<-paste0(ID, collapse=", ")
  ######## QC d_119643471
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_119643471_a = c(232334767, 211228524, 308427446, 635279662, 432722256, 232663805, 785578696, 200929978, 490725843, 965998904, 986445321, 746038746, 178420302, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_827220437 == "), c(657167265), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_119643471"[aa]%!in%d_119643471_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_119643471_invalid_cross = addNA(connectData$"d_119643471"[aa][QCcheck1])
  
  df[6,1]<-substr(paste0("d_119643471"),3,100)
  
  df[6,2]<-paste0("CROSSVALID1")
  
  df[6,3]<-paste0("232334767, 211228524, 308427446, 635279662, 432722256, 232663805, 785578696, 200929978, 490725843, 965998904, 986445321, 746038746, 178420302, NA")
  
  df[6,4]<-str_sub(mylist_a2, end =-4)
  
  df[6,5]<-paste0(d_119643471_invalid_cross, collapse=", ")
  
  df[6,6]<-paste0(rowNum, collapse=", ")
  
  df[6,7]<-paste0(token, collapse=", ")
  
  df[6,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_684926335
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_684926335_a = c(232334767, 635279662, 401335456, 178420302, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_827220438 == "), c(548392715), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_684926335"[aa]%!in%d_684926335_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_684926335_invalid_cross = addNA(connectData$"d_684926335"[aa][QCcheck1])
  
  df[7,1]<-substr(paste0("d_684926335"),3,100)
  
  df[7,2]<-paste0("CROSSVALID1")
  
  df[7,3]<-paste0("232334767, 635279662, 401335456, 178420302, NA")
  
  df[7,4]<-str_sub(mylist_a2, end =-4)
  
  df[7,5]<-paste0(d_684926335_invalid_cross, collapse=", ")
  
  df[7,6]<-paste0(rowNum, collapse=", ")
  
  df[7,7]<-paste0(token, collapse=", ")
  
  df[7,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_527823810
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_527823810_a = c(628733063, 319570637, 178420302, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_827220439 == "), c(548392715), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_527823810"[aa]%!in%d_527823810_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_527823810_invalid_cross = addNA(connectData$"d_527823810"[aa][QCcheck1])
  
  df[8,1]<-substr(paste0("d_527823810"),3,100)
  
  df[8,2]<-paste0("CROSSVALID1")
  
  df[8,3]<-paste0("628733063, 319570637, 178420302, NA")
  
  df[8,4]<-str_sub(mylist_a2, end =-4)
  
  df[8,5]<-paste0(d_527823810_invalid_cross, collapse=", ")
  
  df[8,6]<-paste0(rowNum, collapse=", ")
  
  df[8,7]<-paste0(token, collapse=", ")
  
  df[8,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_412947828
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_412947828_a = c(473807808, 269950058, 998778678, 871169055, 211847969, 613506991, 646444521, 480568177, 601070694, 724873055, 819077778, 228278549, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_827220440 == "), c(548392715), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_412947828"[aa]%!in%d_412947828_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_412947828_invalid_cross = addNA(connectData$"d_412947828"[aa][QCcheck1])
  
  df[9,1]<-substr(paste0("d_412947828"),3,100)
  
  df[9,2]<-paste0("CROSSVALID1")
  
  df[9,3]<-paste0("473807808, 269950058, 998778678, 871169055, 211847969, 613506991, 646444521, 480568177, 601070694, 724873055, 819077778, 228278549, NA")
  
  df[9,4]<-str_sub(mylist_a2, end =-4)
  
  df[9,5]<-paste0(d_412947828_invalid_cross, collapse=", ")
  
  df[9,6]<-paste0(rowNum, collapse=", ")
  
  df[9,7]<-paste0(token, collapse=", ")
  
  df[9,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_538553381
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_538553381_a = c(628733063, 319570637, 986445321, 746038746, 178420302, NA
                    
                    
                    
  )
  
  
  mylist_a1 =  paste0(rep("connectData$d_827220441 == "), c(657167265), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_538553381"[aa]%!in%d_538553381_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_538553381_invalid_cross = addNA(connectData$"d_538553381"[aa][QCcheck1])
  
  df[10,1]<-substr(paste0("d_538553381"),3,100)
  
  df[10,2]<-paste0("CROSSVALID1")
  
  df[10,3]<-paste0("628733063, 319570637, 986445321, 746038746, 178420302, NA
")
  
  df[10,4]<-str_sub(mylist_a2, end =-4)
  
  df[10,5]<-paste0(d_538553381_invalid_cross, collapse=", ")
  
  df[10,6]<-paste0(rowNum, collapse=", ")
  
  df[10,7]<-paste0(token, collapse=", ")
  
  df[10,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_706256705
  # valid value check
  
  d_706256705= c(536341288, 654207589, 830573274, 178420302, NA)
  
  QCcheck1 =which(connectData$"d_706256705"%!in%d_706256705)
  
  d_706256705_invalid = addNA(connectData$"d_706256705"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[11,1]<-substr(paste0("d_706256705"),3,100)
  
  df[11,2]<-paste0("VALID")
  
  df[11,3]<-paste0(toString(d_706256705))
  
  df[11,4]<-paste0(toString("d_706256705 != "),toString(d_706256705),sep=" ")
  
  df[11,5]<-paste0(d_706256705_invalid, collapse=", ")
  
  df[11,6]<-paste0(rowNum, collapse=", ")
  
  df[11,7]<-paste0(token, collapse=", ")
  
  df[11,8]<-paste0(ID, collapse=", ")
  ######## QC d_678756255
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_678756255_a = c(536341288, 654207589, 395528052, 181769837, 178420302, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_827220437 == "), c(125001209, 327912200, 300267574, 452412599), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_678756255"[aa]%!in%d_678756255_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_678756255_invalid_cross = addNA(connectData$"d_678756255"[aa][QCcheck1])
  
  df[12,1]<-substr(paste0("d_678756255"),3,100)
  
  df[12,2]<-paste0("CROSSVALID1")
  
  df[12,3]<-paste0("536341288, 654207589, 395528052, 181769837, 178420302, NA")
  
  df[12,4]<-str_sub(mylist_a2, end =-4)
  
  df[12,5]<-paste0(d_678756255_invalid_cross, collapse=", ")
  
  df[12,6]<-paste0(rowNum, collapse=", ")
  
  df[12,7]<-paste0(token, collapse=", ")
  
  df[12,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_435027713
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_435027713_a = c(536341288, 654207589, 178420302, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_827220437 == "), c(657167265, 548392715), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_435027713"[aa]%!in%d_435027713_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_435027713_invalid_cross = addNA(connectData$"d_435027713"[aa][QCcheck1])
  
  df[13,1]<-substr(paste0("d_435027713"),3,100)
  
  df[13,2]<-paste0("CROSSVALID1")
  
  df[13,3]<-paste0("536341288, 654207589, 178420302, NA")
  
  df[13,4]<-str_sub(mylist_a2, end =-4)
  
  df[13,5]<-paste0(d_435027713_invalid_cross, collapse=", ")
  
  df[13,6]<-paste0(rowNum, collapse=", ")
  
  df[13,7]<-paste0(token, collapse=", ")
  
  df[13,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_749475364
  
  # valid NA or equal to or less than character length check
  
  valid_length= 25
  
  list_lengths = sapply(connectData$d_749475364,nchar)
  
  d_749475364_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_749475364,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_749475364,nchar)))
  d_749475364_invalid_char_length = connectData$d_749475364[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[14,1]<-substr(paste0("d_749475364"),3,100)
  
  df[14,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[14,3]<-paste0("char length",25)
  
  df[14,4]<-paste0("char length NOT <=",25)
  
  df[14,5]<-paste0(d_749475364_invalid_char_length, collapse=", ")
  
  df[14,6]<-paste0(rowNum, collapse=", ")
  
  df[14,7]<-paste0(token, collapse=", ")
  
  df[14,8]<-paste0(ID, collapse=", ")
  ######## QC d_477091792
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_477091792_a = c(939572698, 512786135, 582670006 
  )
  
  
  mylist_a1 =  paste0(rep("connectData$d_512820379 == "), c(486306141), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_477091792"[aa]%!in%d_477091792_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_477091792_invalid_cross = addNA(connectData$"d_477091792"[aa][QCcheck1])
  
  df[15,1]<-substr(paste0("d_477091792"),3,100)
  
  df[15,2]<-paste0("CROSSVALID1")
  
  df[15,3]<-paste0("939572698, 512786135, 582670006 
")
  
  df[15,4]<-str_sub(mylist_a2, end =-4)
  
  df[15,5]<-paste0(d_477091792_invalid_cross, collapse=", ")
  
  df[15,6]<-paste0(rowNum, collapse=", ")
  
  df[15,7]<-paste0(token, collapse=", ")
  
  df[15,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_477091793
  # valid value check
  
  d_477091793= c(939572698, 512786135, 582670006, NA
  )
  
  QCcheck1 =which(connectData$"d_477091793"%!in%d_477091793)
  
  d_477091793_invalid = addNA(connectData$"d_477091793"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[16,1]<-substr(paste0("d_477091793"),3,100)
  
  df[16,2]<-paste0("VALID")
  
  df[16,3]<-paste0(toString(d_477091793))
  
  df[16,4]<-paste0(toString("d_477091793 != "),toString(d_477091793),sep=" ")
  
  df[16,5]<-paste0(d_477091793_invalid, collapse=", ")
  
  df[16,6]<-paste0(rowNum, collapse=", ")
  
  df[16,7]<-paste0(token, collapse=", ")
  
  df[16,8]<-paste0(ID, collapse=", ")
  ######## QC d_667474224
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_667474224_a = c(926338735, 348281054, 324692899, 351257378, 647148178, 834544960, 682916147, 153365143, 663706936, 181769837)
  
  
  mylist_a1 =  paste0(rep("connectData$d_512820379 == "), c(486306141), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_667474224"[aa]%!in%d_667474224_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_667474224_invalid_cross = addNA(connectData$"d_667474224"[aa][QCcheck1])
  
  df[17,1]<-substr(paste0("d_667474224"),3,100)
  
  df[17,2]<-paste0("CROSSVALID1")
  
  df[17,3]<-paste0("926338735, 348281054, 324692899, 351257378, 647148178, 834544960, 682916147, 153365143, 663706936, 181769837")
  
  df[17,4]<-str_sub(mylist_a2, end =-4)
  
  df[17,5]<-paste0(d_667474224_invalid_cross, collapse=", ")
  
  df[17,6]<-paste0(rowNum, collapse=", ")
  
  df[17,7]<-paste0(token, collapse=", ")
  
  df[17,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_667474225
  # valid value check
  
  d_667474225= c(926338735, 348281054, 324692899, 351257378, 647148178, 834544960, 682916147, 153365143, 663706936, 181769838, NA)
  
  QCcheck1 =which(connectData$"d_667474225"%!in%d_667474225)
  
  d_667474225_invalid = addNA(connectData$"d_667474225"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[18,1]<-substr(paste0("d_667474225"),3,100)
  
  df[18,2]<-paste0("VALID")
  
  df[18,3]<-paste0(toString(d_667474225))
  
  df[18,4]<-paste0(toString("d_667474225 != "),toString(d_667474225),sep=" ")
  
  df[18,5]<-paste0(d_667474225_invalid, collapse=", ")
  
  df[18,6]<-paste0(rowNum, collapse=", ")
  
  df[18,7]<-paste0(token, collapse=", ")
  
  df[18,8]<-paste0(ID, collapse=", ")
  ######## QC d_481139103
  # valid value check
  
  d_481139103= c(910533468, 456806539,NA)
  
  QCcheck1 =which(connectData$"d_481139103"%!in%d_481139103)
  
  d_481139103_invalid = addNA(connectData$"d_481139103"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[19,1]<-substr(paste0("d_481139103"),3,100)
  
  df[19,2]<-paste0("VALID")
  
  df[19,3]<-paste0(toString(d_481139103))
  
  df[19,4]<-paste0(toString("d_481139103 != "),toString(d_481139103),sep=" ")
  
  df[19,5]<-paste0(d_481139103_invalid, collapse=", ")
  
  df[19,6]<-paste0(rowNum, collapse=", ")
  
  df[19,7]<-paste0(token, collapse=", ")
  
  df[19,8]<-paste0(ID, collapse=", ")
  ######## QC d_158291096
  # valid value check
  
  d_158291096= c(104430631, 353358909, NA)
  
  QCcheck1 =which(connectData$"d_158291096"%!in%d_158291096)
  
  d_158291096_invalid = addNA(connectData$"d_158291096"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[20,1]<-substr(paste0("d_158291096"),3,100)
  
  df[20,2]<-paste0("VALID")
  
  df[20,3]<-paste0(toString(d_158291096))
  
  df[20,4]<-paste0(toString("d_158291096 != "),toString(d_158291096),sep=" ")
  
  df[20,5]<-paste0(d_158291096_invalid, collapse=", ")
  
  df[20,6]<-paste0(rowNum, collapse=", ")
  
  df[20,7]<-paste0(token, collapse=", ")
  
  df[20,8]<-paste0(ID, collapse=", ")
  ######## QC d_697256759
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_697256759_a = c(353358909, 104430631)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_697256759"[aa]%!in%d_697256759_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_697256759_invalid_cross = addNA(connectData$"d_697256759"[aa][QCcheck1])
  
  df[21,1]<-substr(paste0("d_697256759"),3,100)
  
  df[21,2]<-paste0("CROSSVALID1")
  
  df[21,3]<-paste0("353358909, 104430631")
  
  df[21,4]<-str_sub(mylist_a2, end =-4)
  
  df[21,5]<-paste0(d_697256759_invalid_cross, collapse=", ")
  
  df[21,6]<-paste0(rowNum, collapse=", ")
  
  df[21,7]<-paste0(token, collapse=", ")
  
  df[21,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_196038514
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_196038514_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_196038514"[aa]%!in%d_196038514_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_196038514_invalid_cross = addNA(connectData$"d_196038514"[aa][QCcheck1])
  
  df[22,1]<-substr(paste0("d_196038514"),3,100)
  
  df[22,2]<-paste0("CROSSVALID1")
  
  df[22,3]<-paste0("353358909, 104430631, NA")
  
  df[22,4]<-str_sub(mylist_a2, end =-4)
  
  df[22,5]<-paste0(d_196038514_invalid_cross, collapse=", ")
  
  df[22,6]<-paste0(rowNum, collapse=", ")
  
  df[22,7]<-paste0(token, collapse=", ")
  
  df[22,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_873405723
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_873405723_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_873405723"[aa]%!in%d_873405723_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_873405723_invalid_cross = addNA(connectData$"d_873405723"[aa][QCcheck1])
  
  df[23,1]<-substr(paste0("d_873405723"),3,100)
  
  df[23,2]<-paste0("CROSSVALID1")
  
  df[23,3]<-paste0("353358909, 104430631, NA")
  
  df[23,4]<-str_sub(mylist_a2, end =-4)
  
  df[23,5]<-paste0(d_873405723_invalid_cross, collapse=", ")
  
  df[23,6]<-paste0(rowNum, collapse=", ")
  
  df[23,7]<-paste0(token, collapse=", ")
  
  df[23,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_517101990
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_517101990_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_517101990"[aa]%!in%d_517101990_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_517101990_invalid_cross = addNA(connectData$"d_517101990"[aa][QCcheck1])
  
  df[24,1]<-substr(paste0("d_517101990"),3,100)
  
  df[24,2]<-paste0("CROSSVALID1")
  
  df[24,3]<-paste0("353358909, 104430631, NA")
  
  df[24,4]<-str_sub(mylist_a2, end =-4)
  
  df[24,5]<-paste0(d_517101990_invalid_cross, collapse=", ")
  
  df[24,6]<-paste0(rowNum, collapse=", ")
  
  df[24,7]<-paste0(token, collapse=", ")
  
  df[24,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_347614743
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_347614743_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_347614743"[aa]%!in%d_347614743_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_347614743_invalid_cross = addNA(connectData$"d_347614743"[aa][QCcheck1])
  
  df[25,1]<-substr(paste0("d_347614743"),3,100)
  
  df[25,2]<-paste0("CROSSVALID1")
  
  df[25,3]<-paste0("353358909, 104430631, NA")
  
  df[25,4]<-str_sub(mylist_a2, end =-4)
  
  df[25,5]<-paste0(d_347614743_invalid_cross, collapse=", ")
  
  df[25,6]<-paste0(rowNum, collapse=", ")
  
  df[25,7]<-paste0(token, collapse=", ")
  
  df[25,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_535928798
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_535928798_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_535928798"[aa]%!in%d_535928798_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_535928798_invalid_cross = addNA(connectData$"d_535928798"[aa][QCcheck1])
  
  df[26,1]<-substr(paste0("d_535928798"),3,100)
  
  df[26,2]<-paste0("CROSSVALID1")
  
  df[26,3]<-paste0("353358909, 104430631, NA")
  
  df[26,4]<-str_sub(mylist_a2, end =-4)
  
  df[26,5]<-paste0(d_535928798_invalid_cross, collapse=", ")
  
  df[26,6]<-paste0(rowNum, collapse=", ")
  
  df[26,7]<-paste0(token, collapse=", ")
  
  df[26,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_897366187
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_897366187_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291097 == "), c(353358910), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_897366187"[aa]%!in%d_897366187_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_897366187_invalid_cross = addNA(connectData$"d_897366187"[aa][QCcheck1])
  
  df[27,1]<-substr(paste0("d_897366187"),3,100)
  
  df[27,2]<-paste0("CROSSVALID1")
  
  df[27,3]<-paste0("353358909, 104430631, NA")
  
  df[27,4]<-str_sub(mylist_a2, end =-4)
  
  df[27,5]<-paste0(d_897366187_invalid_cross, collapse=", ")
  
  df[27,6]<-paste0(rowNum, collapse=", ")
  
  df[27,7]<-paste0(token, collapse=", ")
  
  df[27,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_415693436
  
  # valid NA or equal to or less than character length check
  
  valid_length= 800
  
  list_lengths = sapply(connectData$d_415693436,nchar)
  
  d_415693436_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_415693436,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_415693436,nchar)))
  d_415693436_invalid_char_length = connectData$d_415693436[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[28,1]<-substr(paste0("d_415693436"),3,100)
  
  df[28,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[28,3]<-paste0("char length",800)
  
  df[28,4]<-paste0("char length NOT <=",800)
  
  df[28,5]<-paste0(d_415693436_invalid_char_length, collapse=", ")
  
  df[28,6]<-paste0(rowNum, collapse=", ")
  
  df[28,7]<-paste0(token, collapse=", ")
  
  df[28,8]<-paste0(ID, collapse=", ")
  ######## QC d_719451909
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_719451909_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_719451909"[aa]%!in%d_719451909_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_719451909_invalid_cross = addNA(connectData$"d_719451909"[aa][QCcheck1])
  
  df[29,1]<-substr(paste0("d_719451909"),3,100)
  
  df[29,2]<-paste0("CROSSVALID1")
  
  df[29,3]<-paste0("353358909, 104430631, NA")
  
  df[29,4]<-str_sub(mylist_a2, end =-4)
  
  df[29,5]<-paste0(d_719451909_invalid_cross, collapse=", ")
  
  df[29,6]<-paste0(rowNum, collapse=", ")
  
  df[29,7]<-paste0(token, collapse=", ")
  
  df[29,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_377633816
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_377633816_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_377633816"[aa]%!in%d_377633816_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_377633816_invalid_cross = addNA(connectData$"d_377633816"[aa][QCcheck1])
  
  df[30,1]<-substr(paste0("d_377633816"),3,100)
  
  df[30,2]<-paste0("CROSSVALID1")
  
  df[30,3]<-paste0("353358909, 104430631, NA")
  
  df[30,4]<-str_sub(mylist_a2, end =-4)
  
  df[30,5]<-paste0(d_377633816_invalid_cross, collapse=", ")
  
  df[30,6]<-paste0(rowNum, collapse=", ")
  
  df[30,7]<-paste0(token, collapse=", ")
  
  df[30,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_211023960
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_211023960_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_211023960"[aa]%!in%d_211023960_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_211023960_invalid_cross = addNA(connectData$"d_211023960"[aa][QCcheck1])
  
  df[31,1]<-substr(paste0("d_211023960"),3,100)
  
  df[31,2]<-paste0("CROSSVALID1")
  
  df[31,3]<-paste0("353358909, 104430631, NA")
  
  df[31,4]<-str_sub(mylist_a2, end =-4)
  
  df[31,5]<-paste0(d_211023960_invalid_cross, collapse=", ")
  
  df[31,6]<-paste0(rowNum, collapse=", ")
  
  df[31,7]<-paste0(token, collapse=", ")
  
  df[31,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_209509101
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_209509101_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_209509101"[aa]%!in%d_209509101_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_209509101_invalid_cross = addNA(connectData$"d_209509101"[aa][QCcheck1])
  
  df[32,1]<-substr(paste0("d_209509101"),3,100)
  
  df[32,2]<-paste0("CROSSVALID1")
  
  df[32,3]<-paste0("353358909, 104430631, NA")
  
  df[32,4]<-str_sub(mylist_a2, end =-4)
  
  df[32,5]<-paste0(d_209509101_invalid_cross, collapse=", ")
  
  df[32,6]<-paste0(rowNum, collapse=", ")
  
  df[32,7]<-paste0(token, collapse=", ")
  
  df[32,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_363026564
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_363026564_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_363026564"[aa]%!in%d_363026564_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_363026564_invalid_cross = addNA(connectData$"d_363026564"[aa][QCcheck1])
  
  df[33,1]<-substr(paste0("d_363026564"),3,100)
  
  df[33,2]<-paste0("CROSSVALID1")
  
  df[33,3]<-paste0("353358909, 104430631, NA")
  
  df[33,4]<-str_sub(mylist_a2, end =-4)
  
  df[33,5]<-paste0(d_363026564_invalid_cross, collapse=", ")
  
  df[33,6]<-paste0(rowNum, collapse=", ")
  
  df[33,7]<-paste0(token, collapse=", ")
  
  df[33,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_405352246
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_405352246_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_405352246"[aa]%!in%d_405352246_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_405352246_invalid_cross = addNA(connectData$"d_405352246"[aa][QCcheck1])
  
  df[34,1]<-substr(paste0("d_405352246"),3,100)
  
  df[34,2]<-paste0("CROSSVALID1")
  
  df[34,3]<-paste0("353358909, 104430631, NA")
  
  df[34,4]<-str_sub(mylist_a2, end =-4)
  
  df[34,5]<-paste0(d_405352246_invalid_cross, collapse=", ")
  
  df[34,6]<-paste0(rowNum, collapse=", ")
  
  df[34,7]<-paste0(token, collapse=", ")
  
  df[34,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_755545718
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_755545718_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_755545718"[aa]%!in%d_755545718_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_755545718_invalid_cross = addNA(connectData$"d_755545718"[aa][QCcheck1])
  
  df[35,1]<-substr(paste0("d_755545718"),3,100)
  
  df[35,2]<-paste0("CROSSVALID1")
  
  df[35,3]<-paste0("353358909, 104430631, NA")
  
  df[35,4]<-str_sub(mylist_a2, end =-4)
  
  df[35,5]<-paste0(d_755545718_invalid_cross, collapse=", ")
  
  df[35,6]<-paste0(rowNum, collapse=", ")
  
  df[35,7]<-paste0(token, collapse=", ")
  
  df[35,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_831137710
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_831137710_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_831137710"[aa]%!in%d_831137710_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_831137710_invalid_cross = addNA(connectData$"d_831137710"[aa][QCcheck1])
  
  df[36,1]<-substr(paste0("d_831137710"),3,100)
  
  df[36,2]<-paste0("CROSSVALID1")
  
  df[36,3]<-paste0("353358909, 104430631, NA")
  
  df[36,4]<-str_sub(mylist_a2, end =-4)
  
  df[36,5]<-paste0(d_831137710_invalid_cross, collapse=", ")
  
  df[36,6]<-paste0(rowNum, collapse=", ")
  
  df[36,7]<-paste0(token, collapse=", ")
  
  df[36,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_496935183
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_496935183_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_496935183"[aa]%!in%d_496935183_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_496935183_invalid_cross = addNA(connectData$"d_496935183"[aa][QCcheck1])
  
  df[37,1]<-substr(paste0("d_496935183"),3,100)
  
  df[37,2]<-paste0("CROSSVALID1")
  
  df[37,3]<-paste0("353358909, 104430631, NA")
  
  df[37,4]<-str_sub(mylist_a2, end =-4)
  
  df[37,5]<-paste0(d_496935183_invalid_cross, collapse=", ")
  
  df[37,6]<-paste0(rowNum, collapse=", ")
  
  df[37,7]<-paste0(token, collapse=", ")
  
  df[37,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_491099823
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_491099823_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_491099823"[aa]%!in%d_491099823_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_491099823_invalid_cross = addNA(connectData$"d_491099823"[aa][QCcheck1])
  
  df[38,1]<-substr(paste0("d_491099823"),3,100)
  
  df[38,2]<-paste0("CROSSVALID1")
  
  df[38,3]<-paste0("353358909, 104430631, NA")
  
  df[38,4]<-str_sub(mylist_a2, end =-4)
  
  df[38,5]<-paste0(d_491099823_invalid_cross, collapse=", ")
  
  df[38,6]<-paste0(rowNum, collapse=", ")
  
  df[38,7]<-paste0(token, collapse=", ")
  
  df[38,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_836460125
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_836460125_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_836460125"[aa]%!in%d_836460125_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_836460125_invalid_cross = addNA(connectData$"d_836460125"[aa][QCcheck1])
  
  df[39,1]<-substr(paste0("d_836460125"),3,100)
  
  df[39,2]<-paste0("CROSSVALID1")
  
  df[39,3]<-paste0("353358909, 104430631, NA")
  
  df[39,4]<-str_sub(mylist_a2, end =-4)
  
  df[39,5]<-paste0(d_836460125_invalid_cross, collapse=", ")
  
  df[39,6]<-paste0(rowNum, collapse=", ")
  
  df[39,7]<-paste0(token, collapse=", ")
  
  df[39,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_163534562
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_163534562_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_163534562"[aa]%!in%d_163534562_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_163534562_invalid_cross = addNA(connectData$"d_163534562"[aa][QCcheck1])
  
  df[40,1]<-substr(paste0("d_163534562"),3,100)
  
  df[40,2]<-paste0("CROSSVALID1")
  
  df[40,3]<-paste0("353358909, 104430631, NA")
  
  df[40,4]<-str_sub(mylist_a2, end =-4)
  
  df[40,5]<-paste0(d_163534562_invalid_cross, collapse=", ")
  
  df[40,6]<-paste0(rowNum, collapse=", ")
  
  df[40,7]<-paste0(token, collapse=", ")
  
  df[40,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_331787113
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_331787113_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_331787113"[aa]%!in%d_331787113_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_331787113_invalid_cross = addNA(connectData$"d_331787113"[aa][QCcheck1])
  
  df[41,1]<-substr(paste0("d_331787113"),3,100)
  
  df[41,2]<-paste0("CROSSVALID1")
  
  df[41,3]<-paste0("353358909, 104430631, NA")
  
  df[41,4]<-str_sub(mylist_a2, end =-4)
  
  df[41,5]<-paste0(d_331787113_invalid_cross, collapse=", ")
  
  df[41,6]<-paste0(rowNum, collapse=", ")
  
  df[41,7]<-paste0(token, collapse=", ")
  
  df[41,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_705732561
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_705732561_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_705732561"[aa]%!in%d_705732561_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_705732561_invalid_cross = addNA(connectData$"d_705732561"[aa][QCcheck1])
  
  df[42,1]<-substr(paste0("d_705732561"),3,100)
  
  df[42,2]<-paste0("CROSSVALID1")
  
  df[42,3]<-paste0("353358909, 104430631, NA")
  
  df[42,4]<-str_sub(mylist_a2, end =-4)
  
  df[42,5]<-paste0(d_705732561_invalid_cross, collapse=", ")
  
  df[42,6]<-paste0(rowNum, collapse=", ")
  
  df[42,7]<-paste0(token, collapse=", ")
  
  df[42,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_381509125
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_381509125_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_381509125"[aa]%!in%d_381509125_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_381509125_invalid_cross = addNA(connectData$"d_381509125"[aa][QCcheck1])
  
  df[43,1]<-substr(paste0("d_381509125"),3,100)
  
  df[43,2]<-paste0("CROSSVALID1")
  
  df[43,3]<-paste0("353358909, 104430631, NA")
  
  df[43,4]<-str_sub(mylist_a2, end =-4)
  
  df[43,5]<-paste0(d_381509125_invalid_cross, collapse=", ")
  
  df[43,6]<-paste0(rowNum, collapse=", ")
  
  df[43,7]<-paste0(token, collapse=", ")
  
  df[43,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_497530905
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_497530905_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_497530905"[aa]%!in%d_497530905_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_497530905_invalid_cross = addNA(connectData$"d_497530905"[aa][QCcheck1])
  
  df[44,1]<-substr(paste0("d_497530905"),3,100)
  
  df[44,2]<-paste0("CROSSVALID1")
  
  df[44,3]<-paste0("353358909, 104430631, NA")
  
  df[44,4]<-str_sub(mylist_a2, end =-4)
  
  df[44,5]<-paste0(d_497530905_invalid_cross, collapse=", ")
  
  df[44,6]<-paste0(rowNum, collapse=", ")
  
  df[44,7]<-paste0(token, collapse=", ")
  
  df[44,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_627995442
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_627995442_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_627995442"[aa]%!in%d_627995442_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_627995442_invalid_cross = addNA(connectData$"d_627995442"[aa][QCcheck1])
  
  df[45,1]<-substr(paste0("d_627995442"),3,100)
  
  df[45,2]<-paste0("CROSSVALID1")
  
  df[45,3]<-paste0("353358909, 104430631, NA")
  
  df[45,4]<-str_sub(mylist_a2, end =-4)
  
  df[45,5]<-paste0(d_627995442_invalid_cross, collapse=", ")
  
  df[45,6]<-paste0(rowNum, collapse=", ")
  
  df[45,7]<-paste0(token, collapse=", ")
  
  df[45,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_208102461
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_208102461_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_208102461"[aa]%!in%d_208102461_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_208102461_invalid_cross = addNA(connectData$"d_208102461"[aa][QCcheck1])
  
  df[46,1]<-substr(paste0("d_208102461"),3,100)
  
  df[46,2]<-paste0("CROSSVALID1")
  
  df[46,3]<-paste0("353358909, 104430631, NA")
  
  df[46,4]<-str_sub(mylist_a2, end =-4)
  
  df[46,5]<-paste0(d_208102461_invalid_cross, collapse=", ")
  
  df[46,6]<-paste0(rowNum, collapse=", ")
  
  df[46,7]<-paste0(token, collapse=", ")
  
  df[46,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_579618065
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_579618065_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_579618065"[aa]%!in%d_579618065_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_579618065_invalid_cross = addNA(connectData$"d_579618065"[aa][QCcheck1])
  
  df[47,1]<-substr(paste0("d_579618065"),3,100)
  
  df[47,2]<-paste0("CROSSVALID1")
  
  df[47,3]<-paste0("353358909, 104430631, NA")
  
  df[47,4]<-str_sub(mylist_a2, end =-4)
  
  df[47,5]<-paste0(d_579618065_invalid_cross, collapse=", ")
  
  df[47,6]<-paste0(rowNum, collapse=", ")
  
  df[47,7]<-paste0(token, collapse=", ")
  
  df[47,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_702433259
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_702433259_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_702433259"[aa]%!in%d_702433259_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_702433259_invalid_cross = addNA(connectData$"d_702433259"[aa][QCcheck1])
  
  df[48,1]<-substr(paste0("d_702433259"),3,100)
  
  df[48,2]<-paste0("CROSSVALID1")
  
  df[48,3]<-paste0("353358909, 104430631, NA")
  
  df[48,4]<-str_sub(mylist_a2, end =-4)
  
  df[48,5]<-paste0(d_702433259_invalid_cross, collapse=", ")
  
  df[48,6]<-paste0(rowNum, collapse=", ")
  
  df[48,7]<-paste0(token, collapse=", ")
  
  df[48,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_771146804
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_771146804_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_771146804"[aa]%!in%d_771146804_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_771146804_invalid_cross = addNA(connectData$"d_771146804"[aa][QCcheck1])
  
  df[49,1]<-substr(paste0("d_771146804"),3,100)
  
  df[49,2]<-paste0("CROSSVALID1")
  
  df[49,3]<-paste0("353358909, 104430631, NA")
  
  df[49,4]<-str_sub(mylist_a2, end =-4)
  
  df[49,5]<-paste0(d_771146804_invalid_cross, collapse=", ")
  
  df[49,6]<-paste0(rowNum, collapse=", ")
  
  df[49,7]<-paste0(token, collapse=", ")
  
  df[49,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_163284008
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_163284008_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_163284008"[aa]%!in%d_163284008_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_163284008_invalid_cross = addNA(connectData$"d_163284008"[aa][QCcheck1])
  
  df[50,1]<-substr(paste0("d_163284008"),3,100)
  
  df[50,2]<-paste0("CROSSVALID1")
  
  df[50,3]<-paste0("353358909, 104430631, NA")
  
  df[50,4]<-str_sub(mylist_a2, end =-4)
  
  df[50,5]<-paste0(d_163284008_invalid_cross, collapse=", ")
  
  df[50,6]<-paste0(rowNum, collapse=", ")
  
  df[50,7]<-paste0(token, collapse=", ")
  
  df[50,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_387198193
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_387198193_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_387198193"[aa]%!in%d_387198193_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_387198193_invalid_cross = addNA(connectData$"d_387198193"[aa][QCcheck1])
  
  df[51,1]<-substr(paste0("d_387198193"),3,100)
  
  df[51,2]<-paste0("CROSSVALID1")
  
  df[51,3]<-paste0("353358909, 104430631, NA")
  
  df[51,4]<-str_sub(mylist_a2, end =-4)
  
  df[51,5]<-paste0(d_387198193_invalid_cross, collapse=", ")
  
  df[51,6]<-paste0(rowNum, collapse=", ")
  
  df[51,7]<-paste0(token, collapse=", ")
  
  df[51,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_566047367
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_566047367_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_566047367"[aa]%!in%d_566047367_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_566047367_invalid_cross = addNA(connectData$"d_566047367"[aa][QCcheck1])
  
  df[52,1]<-substr(paste0("d_566047367"),3,100)
  
  df[52,2]<-paste0("CROSSVALID1")
  
  df[52,3]<-paste0("353358909, 104430631, NA")
  
  df[52,4]<-str_sub(mylist_a2, end =-4)
  
  df[52,5]<-paste0(d_566047367_invalid_cross, collapse=", ")
  
  df[52,6]<-paste0(rowNum, collapse=", ")
  
  df[52,7]<-paste0(token, collapse=", ")
  
  df[52,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_400259098
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_400259098_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_400259098"[aa]%!in%d_400259098_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_400259098_invalid_cross = addNA(connectData$"d_400259098"[aa][QCcheck1])
  
  df[53,1]<-substr(paste0("d_400259098"),3,100)
  
  df[53,2]<-paste0("CROSSVALID1")
  
  df[53,3]<-paste0("353358909, 104430631, NA")
  
  df[53,4]<-str_sub(mylist_a2, end =-4)
  
  df[53,5]<-paste0(d_400259098_invalid_cross, collapse=", ")
  
  df[53,6]<-paste0(rowNum, collapse=", ")
  
  df[53,7]<-paste0(token, collapse=", ")
  
  df[53,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_260703126
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_260703126_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_260703126"[aa]%!in%d_260703126_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_260703126_invalid_cross = addNA(connectData$"d_260703126"[aa][QCcheck1])
  
  df[54,1]<-substr(paste0("d_260703126"),3,100)
  
  df[54,2]<-paste0("CROSSVALID1")
  
  df[54,3]<-paste0("353358909, 104430631, NA")
  
  df[54,4]<-str_sub(mylist_a2, end =-4)
  
  df[54,5]<-paste0(d_260703126_invalid_cross, collapse=", ")
  
  df[54,6]<-paste0(rowNum, collapse=", ")
  
  df[54,7]<-paste0(token, collapse=", ")
  
  df[54,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_744197145
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_744197145_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_744197145"[aa]%!in%d_744197145_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_744197145_invalid_cross = addNA(connectData$"d_744197145"[aa][QCcheck1])
  
  df[55,1]<-substr(paste0("d_744197145"),3,100)
  
  df[55,2]<-paste0("CROSSVALID1")
  
  df[55,3]<-paste0("353358909, 104430631, NA")
  
  df[55,4]<-str_sub(mylist_a2, end =-4)
  
  df[55,5]<-paste0(d_744197145_invalid_cross, collapse=", ")
  
  df[55,6]<-paste0(rowNum, collapse=", ")
  
  df[55,7]<-paste0(token, collapse=", ")
  
  df[55,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_950040334
  # valid value check
  
  d_950040334= c(104430631, 353358909,NA)
  
  QCcheck1 =which(connectData$"d_950040334"%!in%d_950040334)
  
  d_950040334_invalid = addNA(connectData$"d_950040334"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[56,1]<-substr(paste0("d_950040334"),3,100)
  
  df[56,2]<-paste0("VALID")
  
  df[56,3]<-paste0(toString(d_950040334))
  
  df[56,4]<-paste0(toString("d_950040334 != "),toString(d_950040334),sep=" ")
  
  df[56,5]<-paste0(d_950040334_invalid, collapse=", ")
  
  df[56,6]<-paste0(rowNum, collapse=", ")
  
  df[56,7]<-paste0(token, collapse=", ")
  
  df[56,8]<-paste0(ID, collapse=", ")
  ######## QC d_875549268
  # valid value check
  
  d_875549268= c(104430631, 353358909,NA)
  
  QCcheck1 =which(connectData$"d_875549268"%!in%d_875549268)
  
  d_875549268_invalid = addNA(connectData$"d_875549268"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[57,1]<-substr(paste0("d_875549268"),3,100)
  
  df[57,2]<-paste0("VALID")
  
  df[57,3]<-paste0(toString(d_875549268))
  
  df[57,4]<-paste0(toString("d_875549268 != "),toString(d_875549268),sep=" ")
  
  df[57,5]<-paste0(d_875549268_invalid, collapse=", ")
  
  df[57,6]<-paste0(rowNum, collapse=", ")
  
  df[57,7]<-paste0(token, collapse=", ")
  
  df[57,8]<-paste0(ID, collapse=", ")
  ######## QC d_230663853
  # valid value check
  
  d_230663853= c(104430631, 353358909,NA)
  
  QCcheck1 =which(connectData$"d_230663853"%!in%d_230663853)
  
  d_230663853_invalid = addNA(connectData$"d_230663853"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[58,1]<-substr(paste0("d_230663853"),3,100)
  
  df[58,2]<-paste0("VALID")
  
  df[58,3]<-paste0(toString(d_230663853))
  
  df[58,4]<-paste0(toString("d_230663853 != "),toString(d_230663853),sep=" ")
  
  df[58,5]<-paste0(d_230663853_invalid, collapse=", ")
  
  df[58,6]<-paste0(rowNum, collapse=", ")
  
  df[58,7]<-paste0(token, collapse=", ")
  
  df[58,8]<-paste0(ID, collapse=", ")
  ######## QC d_335767902
  # valid dateTime check
  d_335767902 = connectData$"d_335767902"
  
  QCcheck1 = which(!grepl("[1-2][0,9][0-9]?[0-9]-[0-9]?[1-9]-[0-9]?[0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]", d_335767902) & !is.na(connectData$d_335767902) )
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  d_335767902_dateTime_invalid = addNA(connectData$"d_335767902"[QCcheck1])
  
  df[59,1]<-substr(paste0("d_335767902"),3,100)
  
  df[59,2]<-paste0("NA OR DATETIME")
  
  df[59,3]<-paste0("MMDDYYYY 00:00:00")
  
  df[59,4]<-paste0("dateTime != YYYY-MM-DD 00:00:00")
  
  df[59,5]<-paste0(d_335767902_dateTime_invalid, collapse=", ")
  
  df[59,6]<-paste0(rowNum, collapse=", ")
  
  df[59,7]<-paste0(token, collapse=", ")
  
  df[59,8]<-paste0(ID, collapse=", ")
  ######## QC d_828729648
  # valid value check
  
  d_828729648= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_828729648"%!in%d_828729648)
  
  d_828729648_invalid = addNA(connectData$"d_828729648"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[60,1]<-substr(paste0("d_828729648"),3,100)
  
  df[60,2]<-paste0("VALID")
  
  df[60,3]<-paste0(toString(d_828729648))
  
  df[60,4]<-paste0(toString("d_828729648 != "),toString(d_828729648),sep=" ")
  
  df[60,5]<-paste0(d_828729648_invalid, collapse=", ")
  
  df[60,6]<-paste0(rowNum, collapse=", ")
  
  df[60,7]<-paste0(token, collapse=", ")
  
  df[60,8]<-paste0(ID, collapse=", ")
  ######## QC d_379080287
  
  # valid NA or equal to or less than character length check
  
  valid_length= 6
  
  list_lengths = sapply(connectData$d_379080287,nchar)
  
  d_379080287_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_379080287,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_379080287,nchar)))
  d_379080287_invalid_char_length = connectData$d_379080287[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[61,1]<-substr(paste0("d_379080287"),3,100)
  
  df[61,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[61,3]<-paste0("char length",6)
  
  df[61,4]<-paste0("char length NOT <=",6)
  
  df[61,5]<-paste0(d_379080287_invalid_char_length, collapse=", ")
  
  df[61,6]<-paste0(rowNum, collapse=", ")
  
  df[61,7]<-paste0(token, collapse=", ")
  
  df[61,8]<-paste0(ID, collapse=", ")
  ######## QC d_948195369
  # valid value check
  
  d_948195369= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_948195369"%!in%d_948195369)
  
  d_948195369_invalid = addNA(connectData$"d_948195369"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[62,1]<-substr(paste0("d_948195369"),3,100)
  
  df[62,2]<-paste0("VALID")
  
  df[62,3]<-paste0(toString(d_948195369))
  
  df[62,4]<-paste0(toString("d_948195369 != "),toString(d_948195369),sep=" ")
  
  df[62,5]<-paste0(d_948195369_invalid, collapse=", ")
  
  df[62,6]<-paste0(rowNum, collapse=", ")
  
  df[62,7]<-paste0(token, collapse=", ")
  
  df[62,8]<-paste0(ID, collapse=", ")
  ######## QC d_827220437
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_827220437_a = c(531629870, 548392715, 125001209, 327912200, 300267574, 452412599, 303349821, 657167265, 809703864, 517700004, 181769837)
  
  
  mylist_a1 =  paste0(rep("connectData$d_512820379 == "), c(854703046), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_827220437"[aa]%!in%d_827220437_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_827220437_invalid_cross = addNA(connectData$"d_827220437"[aa][QCcheck1])
  
  df[63,1]<-substr(paste0("d_827220437"),3,100)
  
  df[63,2]<-paste0("CROSSVALID1")
  
  df[63,3]<-paste0("531629870, 548392715, 125001209, 327912200, 300267574, 452412599, 303349821, 657167265, 809703864, 517700004, 181769837")
  
  df[63,4]<-str_sub(mylist_a2, end =-4)
  
  df[63,5]<-paste0(d_827220437_invalid_cross, collapse=", ")
  
  df[63,6]<-paste0(rowNum, collapse=", ")
  
  df[63,7]<-paste0(token, collapse=", ")
  
  df[63,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_461488577
  # valid value check
  
  d_461488577= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_461488577"%!in%d_461488577)
  
  d_461488577_invalid = addNA(connectData$"d_461488577"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[64,1]<-substr(paste0("d_461488577"),3,100)
  
  df[64,2]<-paste0("VALID")
  
  df[64,3]<-paste0(toString(d_461488577))
  
  df[64,4]<-paste0(toString("d_461488577 != "),toString(d_461488577),sep=" ")
  
  df[64,5]<-paste0(d_461488577_invalid, collapse=", ")
  
  df[64,6]<-paste0(rowNum, collapse=", ")
  
  df[64,7]<-paste0(token, collapse=", ")
  
  df[64,8]<-paste0(ID, collapse=", ")
  ######## QC d_942255248
  # valid value check
  
  d_942255248= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_942255248"%!in%d_942255248)
  
  d_942255248_invalid = addNA(connectData$"d_942255248"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[65,1]<-substr(paste0("d_942255248"),3,100)
  
  df[65,2]<-paste0("VALID")
  
  df[65,3]<-paste0(toString(d_942255248))
  
  df[65,4]<-paste0(toString("d_942255248 != "),toString(d_942255248),sep=" ")
  
  df[65,5]<-paste0(d_942255248_invalid, collapse=", ")
  
  df[65,6]<-paste0(rowNum, collapse=", ")
  
  df[65,7]<-paste0(token, collapse=", ")
  
  df[65,8]<-paste0(ID, collapse=", ")
  ######## QC d_607081902
  # valid value check
  
  d_607081902= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_607081902"%!in%d_607081902)
  
  d_607081902_invalid = addNA(connectData$"d_607081902"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[66,1]<-substr(paste0("d_607081902"),3,100)
  
  df[66,2]<-paste0("VALID")
  
  df[66,3]<-paste0(toString(d_607081902))
  
  df[66,4]<-paste0(toString("d_607081902 != "),toString(d_607081902),sep=" ")
  
  df[66,5]<-paste0(d_607081902_invalid, collapse=", ")
  
  df[66,6]<-paste0(rowNum, collapse=", ")
  
  df[66,7]<-paste0(token, collapse=", ")
  
  df[66,8]<-paste0(ID, collapse=", ")
  ######## QC d_639721694
  # valid value check
  
  d_639721694= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_639721694"%!in%d_639721694)
  
  d_639721694_invalid = addNA(connectData$"d_639721694"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[67,1]<-substr(paste0("d_639721694"),3,100)
  
  df[67,2]<-paste0("VALID")
  
  df[67,3]<-paste0(toString(d_639721694))
  
  df[67,4]<-paste0(toString("d_639721694 != "),toString(d_639721694),sep=" ")
  
  df[67,5]<-paste0(d_639721694_invalid, collapse=", ")
  
  df[67,6]<-paste0(rowNum, collapse=", ")
  
  df[67,7]<-paste0(token, collapse=", ")
  
  df[67,8]<-paste0(ID, collapse=", ")
  ######## QC d_196856782
  # valid value check
  
  d_196856782= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_196856782"%!in%d_196856782)
  
  d_196856782_invalid = addNA(connectData$"d_196856782"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[68,1]<-substr(paste0("d_196856782"),3,100)
  
  df[68,2]<-paste0("VALID")
  
  df[68,3]<-paste0(toString(d_196856782))
  
  df[68,4]<-paste0(toString("d_196856782 != "),toString(d_196856782),sep=" ")
  
  df[68,5]<-paste0(d_196856782_invalid, collapse=", ")
  
  df[68,6]<-paste0(rowNum, collapse=", ")
  
  df[68,7]<-paste0(token, collapse=", ")
  
  df[68,8]<-paste0(ID, collapse=", ")
  ######## QC d_177402915
  # valid value check
  
  d_177402915= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_177402915"%!in%d_177402915)
  
  d_177402915_invalid = addNA(connectData$"d_177402915"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[69,1]<-substr(paste0("d_177402915"),3,100)
  
  df[69,2]<-paste0("VALID")
  
  df[69,3]<-paste0(toString(d_177402915))
  
  df[69,4]<-paste0(toString("d_177402915 != "),toString(d_177402915),sep=" ")
  
  df[69,5]<-paste0(d_177402915_invalid, collapse=", ")
  
  df[69,6]<-paste0(rowNum, collapse=", ")
  
  df[69,7]<-paste0(token, collapse=", ")
  
  df[69,8]<-paste0(ID, collapse=", ")
  ######## QC d_791389099
  # valid value check
  
  d_791389099= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_791389099"%!in%d_791389099)
  
  d_791389099_invalid = addNA(connectData$"d_791389099"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[70,1]<-substr(paste0("d_791389099"),3,100)
  
  df[70,2]<-paste0("VALID")
  
  df[70,3]<-paste0(toString(d_791389099))
  
  df[70,4]<-paste0(toString("d_791389099 != "),toString(d_791389099),sep=" ")
  
  df[70,5]<-paste0(d_791389099_invalid, collapse=", ")
  
  df[70,6]<-paste0(rowNum, collapse=", ")
  
  df[70,7]<-paste0(token, collapse=", ")
  
  df[70,8]<-paste0(ID, collapse=", ")
  ######## QC d_684726272
  # valid value check
  
  d_684726272= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_684726272"%!in%d_684726272)
  
  d_684726272_invalid = addNA(connectData$"d_684726272"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[71,1]<-substr(paste0("d_684726272"),3,100)
  
  df[71,2]<-paste0("VALID")
  
  df[71,3]<-paste0(toString(d_684726272))
  
  df[71,4]<-paste0(toString("d_684726272 != "),toString(d_684726272),sep=" ")
  
  df[71,5]<-paste0(d_684726272_invalid, collapse=", ")
  
  df[71,6]<-paste0(rowNum, collapse=", ")
  
  df[71,7]<-paste0(token, collapse=", ")
  
  df[71,8]<-paste0(ID, collapse=", ")
  ######## QC d_241590841
  # valid value check
  
  d_241590841= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_241590841"%!in%d_241590841)
  
  d_241590841_invalid = addNA(connectData$"d_241590841"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[72,1]<-substr(paste0("d_241590841"),3,100)
  
  df[72,2]<-paste0("VALID")
  
  df[72,3]<-paste0(toString(d_241590841))
  
  df[72,4]<-paste0(toString("d_241590841 != "),toString(d_241590841),sep=" ")
  
  df[72,5]<-paste0(d_241590841_invalid, collapse=", ")
  
  df[72,6]<-paste0(rowNum, collapse=", ")
  
  df[72,7]<-paste0(token, collapse=", ")
  
  df[72,8]<-paste0(ID, collapse=", ")
  ######## QC d_206879104
  # valid value check
  
  d_206879104= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_206879104"%!in%d_206879104)
  
  d_206879104_invalid = addNA(connectData$"d_206879104"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[73,1]<-substr(paste0("d_206879104"),3,100)
  
  df[73,2]<-paste0("VALID")
  
  df[73,3]<-paste0(toString(d_206879104))
  
  df[73,4]<-paste0(toString("d_206879104 != "),toString(d_206879104),sep=" ")
  
  df[73,5]<-paste0(d_206879104_invalid, collapse=", ")
  
  df[73,6]<-paste0(rowNum, collapse=", ")
  
  df[73,7]<-paste0(token, collapse=", ")
  
  df[73,8]<-paste0(ID, collapse=", ")
  ######## QC d_642287621
  # valid value check
  
  d_642287621= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_642287621"%!in%d_642287621)
  
  d_642287621_invalid = addNA(connectData$"d_642287621"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[74,1]<-substr(paste0("d_642287621"),3,100)
  
  df[74,2]<-paste0("VALID")
  
  df[74,3]<-paste0(toString(d_642287621))
  
  df[74,4]<-paste0(toString("d_642287621 != "),toString(d_642287621),sep=" ")
  
  df[74,5]<-paste0(d_642287621_invalid, collapse=", ")
  
  df[74,6]<-paste0(rowNum, collapse=", ")
  
  df[74,7]<-paste0(token, collapse=", ")
  
  df[74,8]<-paste0(ID, collapse=", ")
  ######## QC d_520301146
  # valid value check
  
  d_520301146= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_520301146"%!in%d_520301146)
  
  d_520301146_invalid = addNA(connectData$"d_520301146"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[75,1]<-substr(paste0("d_520301146"),3,100)
  
  df[75,2]<-paste0("VALID")
  
  df[75,3]<-paste0(toString(d_520301146))
  
  df[75,4]<-paste0(toString("d_520301146 != "),toString(d_520301146),sep=" ")
  
  df[75,5]<-paste0(d_520301146_invalid, collapse=", ")
  
  df[75,6]<-paste0(rowNum, collapse=", ")
  
  df[75,7]<-paste0(token, collapse=", ")
  
  df[75,8]<-paste0(ID, collapse=", ")
  ######## QC d_285130077
  # valid value check
  
  d_285130077= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_285130077"%!in%d_285130077)
  
  d_285130077_invalid = addNA(connectData$"d_285130077"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[76,1]<-substr(paste0("d_285130077"),3,100)
  
  df[76,2]<-paste0("VALID")
  
  df[76,3]<-paste0(toString(d_285130077))
  
  df[76,4]<-paste0(toString("d_285130077 != "),toString(d_285130077),sep=" ")
  
  df[76,5]<-paste0(d_285130077_invalid, collapse=", ")
  
  df[76,6]<-paste0(rowNum, collapse=", ")
  
  df[76,7]<-paste0(token, collapse=", ")
  
  df[76,8]<-paste0(ID, collapse=", ")
  ######## QC d_549687190
  # valid value check
  
  d_549687190= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_549687190"%!in%d_549687190)
  
  d_549687190_invalid = addNA(connectData$"d_549687190"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[77,1]<-substr(paste0("d_549687190"),3,100)
  
  df[77,2]<-paste0("VALID")
  
  df[77,3]<-paste0(toString(d_549687190))
  
  df[77,4]<-paste0(toString("d_549687190 != "),toString(d_549687190),sep=" ")
  
  df[77,5]<-paste0(d_549687190_invalid, collapse=", ")
  
  df[77,6]<-paste0(rowNum, collapse=", ")
  
  df[77,7]<-paste0(token, collapse=", ")
  
  df[77,8]<-paste0(ID, collapse=", ")
  ######## QC d_326825649
  # valid value check
  
  d_326825649= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_326825649"%!in%d_326825649)
  
  d_326825649_invalid = addNA(connectData$"d_326825649"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[78,1]<-substr(paste0("d_326825649"),3,100)
  
  df[78,2]<-paste0("VALID")
  
  df[78,3]<-paste0(toString(d_326825649))
  
  df[78,4]<-paste0(toString("d_326825649 != "),toString(d_326825649),sep=" ")
  
  df[78,5]<-paste0(d_326825649_invalid, collapse=", ")
  
  df[78,6]<-paste0(rowNum, collapse=", ")
  
  df[78,7]<-paste0(token, collapse=", ")
  
  df[78,8]<-paste0(ID, collapse=", ")
  ######## QC d_819377306
  # valid value check
  
  d_819377306= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_819377306"%!in%d_819377306)
  
  d_819377306_invalid = addNA(connectData$"d_819377306"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[79,1]<-substr(paste0("d_819377306"),3,100)
  
  df[79,2]<-paste0("VALID")
  
  df[79,3]<-paste0(toString(d_819377306))
  
  df[79,4]<-paste0(toString("d_819377306 != "),toString(d_819377306),sep=" ")
  
  df[79,5]<-paste0(d_819377306_invalid, collapse=", ")
  
  df[79,6]<-paste0(rowNum, collapse=", ")
  
  df[79,7]<-paste0(token, collapse=", ")
  
  df[79,8]<-paste0(ID, collapse=", ")
  ######## QC d_829269606
  # valid value check
  
  d_829269606= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_829269606"%!in%d_829269606)
  
  d_829269606_invalid = addNA(connectData$"d_829269606"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[80,1]<-substr(paste0("d_829269606"),3,100)
  
  df[80,2]<-paste0("VALID")
  
  df[80,3]<-paste0(toString(d_829269606))
  
  df[80,4]<-paste0(toString("d_829269606 != "),toString(d_829269606),sep=" ")
  
  df[80,5]<-paste0(d_829269606_invalid, collapse=", ")
  
  df[80,6]<-paste0(rowNum, collapse=", ")
  
  df[80,7]<-paste0(token, collapse=", ")
  
  df[80,8]<-paste0(ID, collapse=", ")
  ######## QC d_967372009
  # valid value check
  
  d_967372009= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_967372009"%!in%d_967372009)
  
  d_967372009_invalid = addNA(connectData$"d_967372009"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[81,1]<-substr(paste0("d_967372009"),3,100)
  
  df[81,2]<-paste0("VALID")
  
  df[81,3]<-paste0(toString(d_967372009))
  
  df[81,4]<-paste0(toString("d_967372009 != "),toString(d_967372009),sep=" ")
  
  df[81,5]<-paste0(d_967372009_invalid, collapse=", ")
  
  df[81,6]<-paste0(rowNum, collapse=", ")
  
  df[81,7]<-paste0(token, collapse=", ")
  
  df[81,8]<-paste0(ID, collapse=", ")
  ######## QC d_462314689
  # valid value check
  
  d_462314689= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_462314689"%!in%d_462314689)
  
  d_462314689_invalid = addNA(connectData$"d_462314689"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[82,1]<-substr(paste0("d_462314689"),3,100)
  
  df[82,2]<-paste0("VALID")
  
  df[82,3]<-paste0(toString(d_462314689))
  
  df[82,4]<-paste0(toString("d_462314689 != "),toString(d_462314689),sep=" ")
  
  df[82,5]<-paste0(d_462314689_invalid, collapse=", ")
  
  df[82,6]<-paste0(rowNum, collapse=", ")
  
  df[82,7]<-paste0(token, collapse=", ")
  
  df[82,8]<-paste0(ID, collapse=", ")
  ######## QC d_412000022
  
  # valid NA or equal to or less than character length check
  
  valid_length= 23
  
  list_lengths = sapply(connectData$d_412000022,nchar)
  
  d_412000022_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_412000022,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_412000022,nchar)))
  d_412000022_invalid_char_length = connectData$d_412000022[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[83,1]<-substr(paste0("d_412000022"),3,100)
  
  df[83,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[83,3]<-paste0("char length",23)
  
  df[83,4]<-paste0("char length NOT <=",23)
  
  df[83,5]<-paste0(d_412000022_invalid_char_length, collapse=", ")
  
  df[83,6]<-paste0(rowNum, collapse=", ")
  
  df[83,7]<-paste0(token, collapse=", ")
  
  df[83,8]<-paste0(ID, collapse=", ")
  ######## QC d_558435199
  # valid value check
  
  d_558435199= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_558435199"%!in%d_558435199)
  
  d_558435199_invalid = addNA(connectData$"d_558435199"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[84,1]<-substr(paste0("d_558435199"),3,100)
  
  df[84,2]<-paste0("VALID")
  
  df[84,3]<-paste0(toString(d_558435199))
  
  df[84,4]<-paste0(toString("d_558435199 != "),toString(d_558435199),sep=" ")
  
  df[84,5]<-paste0(d_558435199_invalid, collapse=", ")
  
  df[84,6]<-paste0(rowNum, collapse=", ")
  
  df[84,7]<-paste0(token, collapse=", ")
  
  df[84,8]<-paste0(ID, collapse=", ")
  ######## QC d_262613359
  # valid dateTime check
  d_262613359 = connectData$"d_262613359"
  
  QCcheck1 = which(!grepl("[1-2][0,9][0-9]?[0-9]-[0-9]?[1-9]-[0-9]?[0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]", d_262613359) & !is.na(connectData$d_262613359) )
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  d_262613359_dateTime_invalid = addNA(connectData$"d_262613359"[QCcheck1])
  
  df[85,1]<-substr(paste0("d_262613359"),3,100)
  
  df[85,2]<-paste0("NA OR DATETIME")
  
  df[85,3]<-paste0("MMDDYYYY 00:00:00")
  
  df[85,4]<-paste0("dateTime != YYYY-MM-DD 00:00:00")
  
  df[85,5]<-paste0(d_262613359_dateTime_invalid, collapse=", ")
  
  df[85,6]<-paste0(rowNum, collapse=", ")
  
  df[85,7]<-paste0(token, collapse=", ")
  
  df[85,8]<-paste0(ID, collapse=", ")
  ######## QC d_454205108
  
  # valid NA or equal to or less than character length check
  
  valid_length= 23
  
  list_lengths = sapply(connectData$d_454205108,nchar)
  
  d_454205108_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_454205108,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_454205108,nchar)))
  d_454205108_invalid_char_length = connectData$d_454205108[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[86,1]<-substr(paste0("d_454205108"),3,100)
  
  df[86,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[86,3]<-paste0("char length",23)
  
  df[86,4]<-paste0("char length NOT <=",23)
  
  df[86,5]<-paste0(d_454205108_invalid_char_length, collapse=", ")
  
  df[86,6]<-paste0(rowNum, collapse=", ")
  
  df[86,7]<-paste0(token, collapse=", ")
  
  df[86,8]<-paste0(ID, collapse=", ")
  ######## QC d_471168198
  
  # valid NA or equal to or less than character length check
  
  valid_length= 50
  
  list_lengths = sapply(connectData$d_471168198,nchar)
  
  d_471168198_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_471168198,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_471168198,nchar)))
  d_471168198_invalid_char_length = connectData$d_471168198[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[87,1]<-substr(paste0("d_471168198"),3,100)
  
  df[87,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[87,3]<-paste0("char length",50)
  
  df[87,4]<-paste0("char length NOT <=",50)
  
  df[87,5]<-paste0(d_471168198_invalid_char_length, collapse=", ")
  
  df[87,6]<-paste0(rowNum, collapse=", ")
  
  df[87,7]<-paste0(token, collapse=", ")
  
  df[87,8]<-paste0(ID, collapse=", ")
  ######## QC d_736251808
  
  # valid NA or equal to or less than character length check
  
  valid_length= 50
  
  list_lengths = sapply(connectData$d_736251808,nchar)
  
  d_736251808_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_736251808,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_736251808,nchar)))
  d_736251808_invalid_char_length = connectData$d_736251808[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[88,1]<-substr(paste0("d_736251808"),3,100)
  
  df[88,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[88,3]<-paste0("char length",50)
  
  df[88,4]<-paste0("char length NOT <=",50)
  
  df[88,5]<-paste0(d_736251808_invalid_char_length, collapse=", ")
  
  df[88,6]<-paste0(rowNum, collapse=", ")
  
  df[88,7]<-paste0(token, collapse=", ")
  
  df[88,8]<-paste0(ID, collapse=", ")
  ######## QC d_436680969
  
  # valid NA or equal to or less than character length check
  
  valid_length= 50
  
  list_lengths = sapply(connectData$d_436680969,nchar)
  
  d_436680969_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_436680969,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_436680969,nchar)))
  d_436680969_invalid_char_length = connectData$d_436680969[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[89,1]<-substr(paste0("d_436680969"),3,100)
  
  df[89,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[89,3]<-paste0("char length",50)
  
  df[89,4]<-paste0("char length NOT <=",50)
  
  df[89,5]<-paste0(d_436680969_invalid_char_length, collapse=", ")
  
  df[89,6]<-paste0(rowNum, collapse=", ")
  
  df[89,7]<-paste0(token, collapse=", ")
  
  df[89,8]<-paste0(ID, collapse=", ")
  ######## QC d_480305327
  # valid value check
  
  d_480305327= c(612166858, 255907182, 226924545, 270793412, 959021713, 643664527, 537892528, NA)
  
  QCcheck1 =which(connectData$"d_480305327"%!in%d_480305327)
  
  d_480305327_invalid = addNA(connectData$"d_480305327"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[90,1]<-substr(paste0("d_480305327"),3,100)
  
  df[90,2]<-paste0("VALID")
  
  df[90,3]<-paste0(toString(d_480305327))
  
  df[90,4]<-paste0(toString("d_480305327 != "),toString(d_480305327),sep=" ")
  
  df[90,5]<-paste0(d_480305327_invalid, collapse=", ")
  
  df[90,6]<-paste0(rowNum, collapse=", ")
  
  df[90,7]<-paste0(token, collapse=", ")
  
  df[90,8]<-paste0(ID, collapse=", ")
  ######## QC d_982402227
  # valid date check
  
  d_982402227 = connectData$"d_982402227"
  
  QCcheck1 = which(!grepl("[1-2][0,9][0-9]?[1-9][0-9]?[1-9][0-9]?[0-9]", d_982402227) & !is.na(connectData$d_982402227))
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  d_982402227_date_invalid = connectData$"d_982402227"[QCcheck1]
  
  df[91,1]<-substr(paste0("d_982402227"),3,100)
  
  df[91,2]<-paste0("NA OR DATE")
  
  df[91,3]<-paste0("YYYYMMDD")
  
  df[91,4]<-paste0("date != MMDDYYYY")
  
  df[91,5]<-paste0(d_982402227_date_invalid, collapse=", ")
  
  df[91,6]<-paste0(rowNum, collapse=", ")
  
  df[91,7]<-paste0(token, collapse=", ")
  
  df[91,8]<-paste0(ID, collapse=", ")
  ######## QC d_919254129
  # valid value check
  
  d_919254129= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_919254129"%!in%d_919254129)
  
  d_919254129_invalid = addNA(connectData$"d_919254129"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[93,1]<-substr(paste0("d_919254129"),3,100)
  
  df[93,2]<-paste0("VALID")
  
  df[93,3]<-paste0(toString(d_919254129))
  
  df[93,4]<-paste0(toString("d_919254129 != "),toString(d_919254129),sep=" ")
  
  df[93,5]<-paste0(d_919254129_invalid, collapse=", ")
  
  df[93,6]<-paste0(rowNum, collapse=", ")
  
  df[93,7]<-paste0(token, collapse=", ")
  
  df[93,8]<-paste0(ID, collapse=", ")
  ######## QC d_492983562
  
  # valid NA or equal to or less than character length check
  
  valid_length= 240
  
  list_lengths = sapply(connectData$d_492983562,nchar)
  
  d_492983562_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_492983562,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_492983562,nchar)))
  d_492983562_invalid_char_length = connectData$d_492983562[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[94,1]<-substr(paste0("d_492983562"),3,100)
  
  df[94,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[94,3]<-paste0("char length",240)
  
  df[94,4]<-paste0("char length NOT <=",240)
  
  df[94,5]<-paste0(d_492983562_invalid_char_length, collapse=", ")
  
  df[94,6]<-paste0(rowNum, collapse=", ")
  
  df[94,7]<-paste0(token, collapse=", ")
  
  df[94,8]<-paste0(ID, collapse=", ")
  ######## QC d_421823980
  
  # valid NA or equal to or less than character length check
  
  valid_length= 120
  
  list_lengths = sapply(connectData$d_421823980,nchar)
  
  d_421823980_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_421823980,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_421823980,nchar)))
  d_421823980_invalid_char_length = connectData$d_421823980[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[95,1]<-substr(paste0("d_421823980"),3,100)
  
  df[95,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[95,3]<-paste0("char length",120)
  
  df[95,4]<-paste0("char length NOT <=",120)
  
  df[95,5]<-paste0(d_421823980_invalid_char_length, collapse=", ")
  
  df[95,6]<-paste0(rowNum, collapse=", ")
  
  df[95,7]<-paste0(token, collapse=", ")
  
  df[95,8]<-paste0(ID, collapse=", ")
  ######## QC d_348474836
  
  # valid NA or equal to or less than character length check
  
  valid_length= 11
  
  list_lengths = sapply(connectData$d_348474836,nchar)
  
  d_348474836_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_348474836,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_348474836,nchar)))
  d_348474836_invalid_char_length = connectData$d_348474836[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[96,1]<-substr(paste0("d_348474836"),3,100)
  
  df[96,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[96,3]<-paste0("char length",11)
  
  df[96,4]<-paste0("char length NOT <=",11)
  
  df[96,5]<-paste0(d_348474836_invalid_char_length, collapse=", ")
  
  df[96,6]<-paste0(rowNum, collapse=", ")
  
  df[96,7]<-paste0(token, collapse=", ")
  
  df[96,8]<-paste0(ID, collapse=", ")
  ######## QC d_348474836
  # missing pin check
  d_348474836=connectData$"d_348474836"
  
  d_348474836_a = c("phone should not be missing if email missing")
  
  
  QCcheck1 =  eval(parse(text=which(is.na(connectData$d_421823980) | connectData$d_421823980 == "" & connectData$d_919254129==353358909 & (connectData$d_348474836 == "" | is.na(connectData$d_348474836)))))
  
  df[97,1]<-substr(paste0("d_348474836"),3,100)
  
  df[97,2]<-paste0("CUSTOM")
  
  df[97,3]<-paste0("phone should not be missing if email missing")
  
  df[97,4]<-paste0("missing phone value(s) found in row(s)")
  
  df[97,5]<-paste0(QCcheck1, collapse=", ")
  
  ######## QC d_995036844
  
  # valid NA or equal to or less than character length check
  
  valid_length= 50
  
  list_lengths = sapply(connectData$d_995036844,nchar)
  
  d_995036844_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_995036844,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_995036844,nchar)))
  d_995036844_invalid_char_length = connectData$d_995036844[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[99,1]<-substr(paste0("d_995036844"),3,100)
  
  df[99,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[99,3]<-paste0("char length",50)
  
  df[99,4]<-paste0("char length NOT <=",50)
  
  df[99,5]<-paste0(d_995036844_invalid_char_length, collapse=", ")
  
  df[99,6]<-paste0(rowNum, collapse=", ")
  
  df[99,7]<-paste0(token, collapse=", ")
  
  df[99,8]<-paste0(ID, collapse=", ")
  ######## QC d_399159511
  
  # valid NA or equal to or less than character length check
  
  valid_length= 49
  
  list_lengths = sapply(connectData$d_399159511,nchar)
  
  d_399159511_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_399159511,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_399159511,nchar)))
  d_399159511_invalid_char_length = connectData$d_399159511[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[100,1]<-substr(paste0("d_399159511"),3,100)
  
  df[100,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[100,3]<-paste0("char length",49)
  
  df[100,4]<-paste0("char length NOT <=",49)
  
  df[100,5]<-paste0(d_399159511_invalid_char_length, collapse=", ")
  
  df[100,6]<-paste0(rowNum, collapse=", ")
  
  df[100,7]<-paste0(token, collapse=", ")
  
  df[100,8]<-paste0(ID, collapse=", ")
  ######## QC d_231676651
  
  # valid NA or equal to or less than character length check
  
  valid_length= 50
  
  list_lengths = sapply(connectData$d_231676651,nchar)
  
  d_231676651_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_231676651,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_231676651,nchar)))
  d_231676651_invalid_char_length = connectData$d_231676651[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[101,1]<-substr(paste0("d_231676651"),3,100)
  
  df[101,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[101,3]<-paste0("char length",50)
  
  df[101,4]<-paste0("char length NOT <=",50)
  
  df[101,5]<-paste0(d_231676651_invalid_char_length, collapse=", ")
  
  df[101,6]<-paste0(rowNum, collapse=", ")
  
  df[101,7]<-paste0(token, collapse=", ")
  
  df[101,8]<-paste0(ID, collapse=", ")
  ######## QC d_996038075
  
  # valid NA or equal to or less than character length check
  
  valid_length= 50
  
  list_lengths = sapply(connectData$d_996038075,nchar)
  
  d_996038075_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_996038075,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_996038075,nchar)))
  d_996038075_invalid_char_length = connectData$d_996038075[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[102,1]<-substr(paste0("d_996038075"),3,100)
  
  df[102,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[102,3]<-paste0("char length",50)
  
  df[102,4]<-paste0("char length NOT <=",50)
  
  df[102,5]<-paste0(d_996038075_invalid_char_length, collapse=", ")
  
  df[102,6]<-paste0(rowNum, collapse=", ")
  
  df[102,7]<-paste0(token, collapse=", ")
  
  df[102,8]<-paste0(ID, collapse=", ")
  ######## QC d_506826178
  # valid value check
  
  d_506826178= c(612166858, 255907182, 226924545, 270793412, 959021713, 643664527, 537892528, NA)
  
  QCcheck1 =which(connectData$"d_506826178"%!in%d_506826178)
  
  d_506826178_invalid = addNA(connectData$"d_506826178"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[103,1]<-substr(paste0("d_506826178"),3,100)
  
  df[103,2]<-paste0("VALID")
  
  df[103,3]<-paste0(toString(d_506826178))
  
  df[103,4]<-paste0(toString("d_506826178 != "),toString(d_506826178),sep=" ")
  
  df[103,5]<-paste0(d_506826178_invalid, collapse=", ")
  
  df[103,6]<-paste0(rowNum, collapse=", ")
  
  df[103,7]<-paste0(token, collapse=", ")
  
  df[103,8]<-paste0(ID, collapse=", ")
  ######## QC d_153211406
  
  # valid NA or equal to or less than character length check
  
  valid_length= 50
  
  list_lengths = sapply(connectData$d_153211406,nchar)
  
  d_153211406_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_153211406,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_153211406,nchar)))
  d_153211406_invalid_char_length = connectData$d_153211406[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[104,1]<-substr(paste0("d_153211406"),3,100)
  
  df[104,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[104,3]<-paste0("char length",50)
  
  df[104,4]<-paste0("char length NOT <=",50)
  
  df[104,5]<-paste0(d_153211406_invalid_char_length, collapse=", ")
  
  df[104,6]<-paste0(rowNum, collapse=", ")
  
  df[104,7]<-paste0(token, collapse=", ")
  
  df[104,8]<-paste0(ID, collapse=", ")
  ######## QC d_564964481
  
  # valid NA or equal to or less than character length check
  
  valid_length= 50
  
  list_lengths = sapply(connectData$d_564964481,nchar)
  
  d_564964481_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_564964481,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_564964481,nchar)))
  d_564964481_invalid_char_length = connectData$d_564964481[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[105,1]<-substr(paste0("d_564964481"),3,100)
  
  df[105,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[105,3]<-paste0("char length",50)
  
  df[105,4]<-paste0("char length NOT <=",50)
  
  df[105,5]<-paste0(d_564964481_invalid_char_length, collapse=", ")
  
  df[105,6]<-paste0(rowNum, collapse=", ")
  
  df[105,7]<-paste0(token, collapse=", ")
  
  df[105,8]<-paste0(ID, collapse=", ")
  ######## QC d_795827569
  
  # valid NA or equal to or less than character length check
  
  valid_length= 2
  
  list_lengths = sapply(connectData$d_795827569,nchar)
  
  d_795827569_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_795827569,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_795827569,nchar)))
  d_795827569_invalid_char_length = connectData$d_795827569[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[106,1]<-substr(paste0("d_795827569"),3,100)
  
  df[106,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[106,3]<-paste0("char length",2)
  
  df[106,4]<-paste0("char length NOT <=",2)
  
  df[106,5]<-paste0(d_795827569_invalid_char_length, collapse=", ")
  
  df[106,6]<-paste0(rowNum, collapse=", ")
  
  df[106,7]<-paste0(token, collapse=", ")
  
  df[106,8]<-paste0(ID, collapse=", ")
  ######## QC d_544150384
  
  # valid NA or equal to or less than character length check
  
  valid_length= 4
  
  list_lengths = sapply(connectData$d_544150384,nchar)
  
  d_544150384_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_544150384,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_544150384,nchar)))
  d_544150384_invalid_char_length = connectData$d_544150384[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[107,1]<-substr(paste0("d_544150384"),3,100)
  
  df[107,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[107,3]<-paste0("char length",4)
  
  df[107,4]<-paste0("char length NOT <=",4)
  
  df[107,5]<-paste0(d_544150384_invalid_char_length, collapse=", ")
  
  df[107,6]<-paste0(rowNum, collapse=", ")
  
  df[107,7]<-paste0(token, collapse=", ")
  
  df[107,8]<-paste0(ID, collapse=", ")
  ######## QC d_371067537
  
  # valid NA or equal to or less than character length check
  
  valid_length= 8
  
  list_lengths = sapply(connectData$d_371067537,nchar)
  
  d_371067537_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_371067537,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_371067537,nchar)))
  d_371067537_invalid_char_length = connectData$d_371067537[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[108,1]<-substr(paste0("d_371067537"),3,100)
  
  df[108,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[108,3]<-paste0("char length",8)
  
  df[108,4]<-paste0("char length NOT <=",8)
  
  df[108,5]<-paste0(d_371067537_invalid_char_length, collapse=", ")
  
  df[108,6]<-paste0(rowNum, collapse=", ")
  
  df[108,7]<-paste0(token, collapse=", ")
  
  df[108,8]<-paste0(ID, collapse=", ")
  ######## QC d_117249500
  
  # valid NA or equal to or less than character length check
  
  valid_length= 2
  
  list_lengths = sapply(connectData$d_117249500,nchar)
  
  d_117249500_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_117249500,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_117249500,nchar)))
  d_117249500_invalid_char_length = connectData$d_117249500[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[109,1]<-substr(paste0("d_117249500"),3,100)
  
  df[109,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[109,3]<-paste0("char length",2)
  
  df[109,4]<-paste0("char length NOT <=",2)
  
  df[109,5]<-paste0(d_117249500_invalid_char_length, collapse=", ")
  
  df[109,6]<-paste0(rowNum, collapse=", ")
  
  df[109,7]<-paste0(token, collapse=", ")
  
  df[109,8]<-paste0(ID, collapse=", ")
  ######## QC d_388711124
  
  # valid NA or equal to character length check
  
  valid_length= 10
  
  list_lengths = sapply(connectData$d_388711124,nchar)
  
  d_388711124_invalid= list_lengths[list_lengths != valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_388711124,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_388711124,nchar)))
  d_388711124_invalid_char_length = connectData$d_388711124[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[110,1]<-substr(paste0("d_388711124"),3,100)
  
  df[110,2]<-paste0("NA OR EQUAL TO CHAR() ",valid_length)
  
  df[110,3]<-paste0("char length",10)
  
  df[110,4]<-paste0("char length NOT =",10)
  
  df[110,5]<-paste0(d_388711124_invalid_char_length, collapse=", ")
  
  df[110,6]<-paste0(rowNum, collapse=", ")
  
  df[110,7]<-paste0(token, collapse=", ")
  
  df[110,8]<-paste0(ID, collapse=", ")
  ######## QC d_271757434
  # valid value check
  
  d_271757434= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_271757434"%!in%d_271757434)
  
  d_271757434_invalid = addNA(connectData$"d_271757434"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[111,1]<-substr(paste0("d_271757434"),3,100)
  
  df[111,2]<-paste0("VALID")
  
  df[111,3]<-paste0(toString(d_271757434))
  
  df[111,4]<-paste0(toString("d_271757434 != "),toString(d_271757434),sep=" ")
  
  df[111,5]<-paste0(d_271757434_invalid, collapse=", ")
  
  df[111,6]<-paste0(rowNum, collapse=", ")
  
  df[111,7]<-paste0(token, collapse=", ")
  
  df[111,8]<-paste0(ID, collapse=", ")
  ######## QC d_646873644
  # valid value check
  
  d_646873644= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_646873644"%!in%d_646873644)
  
  d_646873644_invalid = addNA(connectData$"d_646873644"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[112,1]<-substr(paste0("d_646873644"),3,100)
  
  df[112,2]<-paste0("VALID")
  
  df[112,3]<-paste0(toString(d_646873644))
  
  df[112,4]<-paste0(toString("d_646873644 != "),toString(d_646873644),sep=" ")
  
  df[112,5]<-paste0(d_646873644_invalid, collapse=", ")
  
  df[112,6]<-paste0(rowNum, collapse=", ")
  
  df[112,7]<-paste0(token, collapse=", ")
  
  df[112,8]<-paste0(ID, collapse=", ")
  ######## QC d_438643922
  
  # valid NA or equal to character length check
  
  valid_length= 10
  
  list_lengths = sapply(connectData$d_438643922,nchar)
  
  d_438643922_invalid= list_lengths[list_lengths != valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_438643922,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_438643922,nchar)))
  d_438643922_invalid_char_length = connectData$d_438643922[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[113,1]<-substr(paste0("d_438643922"),3,100)
  
  df[113,2]<-paste0("NA OR EQUAL TO CHAR() ",valid_length)
  
  df[113,3]<-paste0("char length",10)
  
  df[113,4]<-paste0("char length NOT =",10)
  
  df[113,5]<-paste0(d_438643922_invalid_char_length, collapse=", ")
  
  df[113,6]<-paste0(rowNum, collapse=", ")
  
  df[113,7]<-paste0(token, collapse=", ")
  
  df[113,8]<-paste0(ID, collapse=", ")
  ######## QC d_187894482
  # valid value check
  
  d_187894482= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_187894482"%!in%d_187894482)
  
  d_187894482_invalid = addNA(connectData$"d_187894482"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[114,1]<-substr(paste0("d_187894482"),3,100)
  
  df[114,2]<-paste0("VALID")
  
  df[114,3]<-paste0(toString(d_187894482))
  
  df[114,4]<-paste0(toString("d_187894482 != "),toString(d_187894482),sep=" ")
  
  df[114,5]<-paste0(d_187894482_invalid, collapse=", ")
  
  df[114,6]<-paste0(rowNum, collapse=", ")
  
  df[114,7]<-paste0(token, collapse=", ")
  
  df[114,8]<-paste0(ID, collapse=", ")
  ######## QC d_793072415
  
  # valid NA or equal to or less than character length check
  
  valid_length= 10
  
  list_lengths = sapply(connectData$d_793072415,nchar)
  
  d_793072415_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_793072415,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_793072415,nchar)))
  d_793072415_invalid_char_length = connectData$d_793072415[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[115,1]<-substr(paste0("d_793072415"),3,100)
  
  df[115,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[115,3]<-paste0("char length",10)
  
  df[115,4]<-paste0("char length NOT <=",10)
  
  df[115,5]<-paste0(d_793072415_invalid_char_length, collapse=", ")
  
  df[115,6]<-paste0(rowNum, collapse=", ")
  
  df[115,7]<-paste0(token, collapse=", ")
  
  df[115,8]<-paste0(ID, collapse=", ")
  ######## QC d_983278853
  # valid value check
  
  d_983278853= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_983278853"%!in%d_983278853)
  
  d_983278853_invalid = addNA(connectData$"d_983278853"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[116,1]<-substr(paste0("d_983278853"),3,100)
  
  df[116,2]<-paste0("VALID")
  
  df[116,3]<-paste0(toString(d_983278853))
  
  df[116,4]<-paste0(toString("d_983278853 != "),toString(d_983278853),sep=" ")
  
  df[116,5]<-paste0(d_983278853_invalid, collapse=", ")
  
  df[116,6]<-paste0(rowNum, collapse=", ")
  
  df[116,7]<-paste0(token, collapse=", ")
  
  df[116,8]<-paste0(ID, collapse=", ")
  ######## QC d_869588347
  # cross valid equal to or less than char() check - checks values if condition one is met and checks values if condition is not met
  
  d_869588347_a = c(353358909)
  
  
  mylist_a1 =  paste0(rep("connectData$d_699625233 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  
  valid_length= c(119)
  
  var.is.integer =suppressWarnings(testInteger(connectData$"d_869588347"[aa]))
  
  variable =connectData$"d_869588347[aa]"
  
  list_lengths = sapply(variable,nchar)
  
  d_869588347_invalid_char_length = list_lengths[list_lengths > valid_length]
  
  d_869588347_invalid_char_length = list_lengths[list_lengths > valid_length]
  
  df[117,1]<-substr(paste0("d_869588347"),3,100)
  
  df[117,2]<-paste0("CROSSVALID1 EQUAL TO OR LESS THAN CHAR() ",valid_length)
  
  df[117,3]<-paste0("char length",119)
  
  df[117,4]<-paste0("char length NOT =",119)
  
  df[117,5]<-paste0(d_869588347_invalid_char_length, collapse=", ") 
  ######## QC d_849786503
  # cross valid equal to or less than char() check - checks values if condition one is met and checks values if condition is not met
  
  d_849786503_a = c(353358909)
  
  
  mylist_a1 =  paste0(rep("connectData$d_699625233 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  
  valid_length= c(120)
  
  var.is.integer =suppressWarnings(testInteger(connectData$"d_849786503"[aa]))
  
  variable =connectData$"d_849786503[aa]"
  
  list_lengths = sapply(variable,nchar)
  
  d_849786503_invalid_char_length = list_lengths[list_lengths > valid_length]
  
  d_849786503_invalid_char_length = list_lengths[list_lengths > valid_length]
  
  df[118,1]<-substr(paste0("d_849786503"),3,100)
  
  df[118,2]<-paste0("CROSSVALID1 EQUAL TO OR LESS THAN CHAR() ",valid_length)
  
  df[118,3]<-paste0("char length",120)
  
  df[118,4]<-paste0("char length NOT =",120)
  
  df[118,5]<-paste0(d_849786503_invalid_char_length, collapse=", ") 
  ######## QC d_635101039
  # cross valid equal to or less than char() check - checks values if condition one is met and checks values if condition is not met
  
  d_635101039_a = c(353358909)
  
  
  mylist_a1 =  paste0(rep("connectData$d_699625233 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  
  valid_length= c(120)
  
  var.is.integer =suppressWarnings(testInteger(connectData$"d_635101039"[aa]))
  
  variable =connectData$"d_635101039[aa]"
  
  list_lengths = sapply(variable,nchar)
  
  d_635101039_invalid_char_length = list_lengths[list_lengths > valid_length]
  
  d_635101039_invalid_char_length = list_lengths[list_lengths > valid_length]
  
  df[119,1]<-substr(paste0("d_635101039"),3,100)
  
  df[119,2]<-paste0("CROSSVALID1 EQUAL TO OR LESS THAN CHAR() ",valid_length)
  
  df[119,3]<-paste0("char length",120)
  
  df[119,4]<-paste0("char length NOT =",120)
  
  df[119,5]<-paste0(d_635101039_invalid_char_length, collapse=", ") 
  ######## QC d_714419972
  # cross valid equal to or less than char() check - checks values if condition one is met and checks values if condition is not met
  
  d_714419972_a = c(353358909)
  
  
  mylist_a1 =  paste0(rep("connectData$d_699625233 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  
  valid_length= c(120)
  
  var.is.integer =suppressWarnings(testInteger(connectData$"d_714419972"[aa]))
  
  variable =connectData$"d_714419972[aa]"
  
  list_lengths = sapply(variable,nchar)
  
  d_714419972_invalid_char_length = list_lengths[list_lengths > valid_length]
  
  d_714419972_invalid_char_length = list_lengths[list_lengths > valid_length]
  
  df[120,1]<-substr(paste0("d_714419972"),3,100)
  
  df[120,2]<-paste0("CROSSVALID1 EQUAL TO OR LESS THAN CHAR() ",valid_length)
  
  df[120,3]<-paste0("char length",120)
  
  df[120,4]<-paste0("char length NOT =",120)
  
  df[120,5]<-paste0(d_714419972_invalid_char_length, collapse=", ") 
  ######## QC d_524461170
  # valid value check
  
  d_524461170= c(357184057, 127547625, NA
                 
  )
  
  QCcheck1 =which(connectData$"d_524461170"%!in%d_524461170)
  
  d_524461170_invalid = addNA(connectData$"d_524461170"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[121,1]<-substr(paste0("d_524461170"),3,100)
  
  df[121,2]<-paste0("VALID")
  
  df[121,3]<-paste0(toString(d_524461170))
  
  df[121,4]<-paste0(toString("d_524461170 != "),toString(d_524461170),sep=" ")
  
  df[121,5]<-paste0(d_524461170_invalid, collapse=", ")
  
  df[121,6]<-paste0(rowNum, collapse=", ")
  
  df[121,7]<-paste0(token, collapse=", ")
  
  df[121,8]<-paste0(ID, collapse=", ")
  ######## QC d_521824358
  # cross valid equal to or less than char() check - checks values if condition one is met and checks values if condition is not met
  
  d_521824358_a = c(353358908)
  
  
  mylist_a1 =  paste0(rep("connectData$d_699625232 == "), c(353358908), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  
  valid_length= c(70)
  
  var.is.integer =suppressWarnings(testInteger(connectData$"d_521824358"[aa]))
  
  variable =connectData$"d_521824358[aa]"
  
  list_lengths = sapply(variable,nchar)
  
  d_521824358_invalid_char_length = list_lengths[list_lengths > valid_length]
  
  d_521824358_invalid_char_length = list_lengths[list_lengths > valid_length]
  
  df[122,1]<-substr(paste0("d_521824358"),3,100)
  
  df[122,2]<-paste0("CROSSVALID1 EQUAL TO OR LESS THAN CHAR() ",valid_length)
  
  df[122,3]<-paste0("char length",70)
  
  df[122,4]<-paste0("char length NOT =",70)
  
  df[122,5]<-paste0(d_521824358_invalid_char_length, collapse=", ") 
  ######## QC d_442166669
  # cross valid equal to or less than char() check - checks values if condition one is met and checks values if condition is not met
  
  d_442166669_a = c(353358909)
  
  
  mylist_a1 =  paste0(rep("connectData$d_699625233 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  
  valid_length= c(70)
  
  var.is.integer =suppressWarnings(testInteger(connectData$"d_442166669"[aa]))
  
  variable =connectData$"d_442166669[aa]"
  
  list_lengths = sapply(variable,nchar)
  
  d_442166669_invalid_char_length = list_lengths[list_lengths > valid_length]
  
  d_442166669_invalid_char_length = list_lengths[list_lengths > valid_length]
  
  df[123,1]<-substr(paste0("d_442166669"),3,100)
  
  df[123,2]<-paste0("CROSSVALID1 EQUAL TO OR LESS THAN CHAR() ",valid_length)
  
  df[123,3]<-paste0("char length",70)
  
  df[123,4]<-paste0("char length NOT =",70)
  
  df[123,5]<-paste0(d_442166669_invalid_char_length, collapse=", ") 
  ######## QC d_703385619
  # cross valid equal to or less than char() check - checks values if condition one is met and checks values if condition is not met
  
  d_703385619_a = c(353358909)
  
  
  mylist_a1 =  paste0(rep("connectData$d_699625233 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  
  valid_length= c(45)
  
  var.is.integer =suppressWarnings(testInteger(connectData$"d_703385619"[aa]))
  
  variable =connectData$"d_703385619[aa]"
  
  list_lengths = sapply(variable,nchar)
  
  d_703385619_invalid_char_length = list_lengths[list_lengths > valid_length]
  
  d_703385619_invalid_char_length = list_lengths[list_lengths > valid_length]
  
  df[124,1]<-substr(paste0("d_703385619"),3,100)
  
  df[124,2]<-paste0("CROSSVALID1 EQUAL TO OR LESS THAN CHAR() ",valid_length)
  
  df[124,3]<-paste0("char length",45)
  
  df[124,4]<-paste0("char length NOT =",45)
  
  df[124,5]<-paste0(d_703385619_invalid_char_length, collapse=", ") 
  ######## QC d_634434746
  # cross valid equal to or less than char() check - checks values if condition one is met and checks values if condition is not met
  
  d_634434746_a = c(353358909)
  
  
  mylist_a1 =  paste0(rep("connectData$d_699625233 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  
  valid_length= c(48)
  
  var.is.integer =suppressWarnings(testInteger(connectData$"d_634434746"[aa]))
  
  variable =connectData$"d_634434746[aa]"
  
  list_lengths = sapply(variable,nchar)
  
  d_634434746_invalid_char_length = list_lengths[list_lengths > valid_length]
  
  d_634434746_invalid_char_length = list_lengths[list_lengths > valid_length]
  
  df[125,1]<-substr(paste0("d_634434746"),3,100)
  
  df[125,2]<-paste0("CROSSVALID1 EQUAL TO OR LESS THAN CHAR() ",valid_length)
  
  df[125,3]<-paste0("char length",48)
  
  df[125,4]<-paste0("char length NOT =",48)
  
  df[125,5]<-paste0(d_634434746_invalid_char_length, collapse=", ") 
  ######## QC d_892050548
  # cross valid equal to or less than char() check - checks values if condition one is met and checks values if condition is not met
  
  d_892050548_a = c(353358909)
  
  
  mylist_a1 =  paste0(rep("connectData$d_699625233 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  
  valid_length= c(5)
  
  var.is.integer =suppressWarnings(testInteger(connectData$"d_892050548"[aa]))
  
  variable =connectData$"d_892050548[aa]"
  
  list_lengths = sapply(variable,nchar)
  
  d_892050548_invalid_char_length = list_lengths[list_lengths > valid_length]
  
  d_892050548_invalid_char_length = list_lengths[list_lengths > valid_length]
  
  df[126,1]<-substr(paste0("d_892050548"),3,100)
  
  df[126,2]<-paste0("CROSSVALID1 EQUAL TO OR LESS THAN CHAR() ",valid_length)
  
  df[126,3]<-paste0("char length",5)
  
  df[126,4]<-paste0("char length NOT =",5)
  
  df[126,5]<-paste0(d_892050548_invalid_char_length, collapse=", ") 
  ######## QC d_452166062
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_452166062_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_699625233 == "), c(353358909), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_452166062"[aa]%!in%d_452166062_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_452166062_invalid_cross = addNA(connectData$"d_452166062"[aa][QCcheck1])
  
  df[127,1]<-substr(paste0("d_452166062"),3,100)
  
  df[127,2]<-paste0("CROSSVALID1")
  
  df[127,3]<-paste0("353358909, 104430631, NA")
  
  df[127,4]<-str_sub(mylist_a2, end =-4)
  
  df[127,5]<-paste0(d_452166062_invalid_cross, collapse=", ")
  
  df[127,6]<-paste0(rowNum, collapse=", ")
  
  df[127,7]<-paste0(token, collapse=", ")
  
  df[127,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_650597106
  
  # valid NA or equal to or less than character length check
  
  valid_length= 4
  
  list_lengths = sapply(connectData$d_650597106,nchar)
  
  d_650597106_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_650597106,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_650597106,nchar)))
  d_650597106_invalid_char_length = connectData$d_650597106[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[128,1]<-substr(paste0("d_650597106"),3,100)
  
  df[128,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[128,3]<-paste0("char length",4)
  
  df[128,4]<-paste0("char length NOT <=",4)
  
  df[128,5]<-paste0(d_650597106_invalid_char_length, collapse=", ")
  
  df[128,6]<-paste0(rowNum, collapse=", ")
  
  df[128,7]<-paste0(token, collapse=", ")
  
  df[128,8]<-paste0(ID, collapse=", ")
  ######## QC d_266952173
  
  # valid NA or equal to or less than character length check
  
  valid_length= 800
  
  list_lengths = sapply(connectData$d_266952173,nchar)
  
  d_266952173_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_266952173,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_266952173,nchar)))
  d_266952173_invalid_char_length = connectData$d_266952173[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[129,1]<-substr(paste0("d_266952173"),3,100)
  
  df[129,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[129,3]<-paste0("char length",800)
  
  df[129,4]<-paste0("char length NOT <=",800)
  
  df[129,5]<-paste0(d_266952173_invalid_char_length, collapse=", ")
  
  df[129,6]<-paste0(rowNum, collapse=", ")
  
  df[129,7]<-paste0(token, collapse=", ")
  
  df[129,8]<-paste0(ID, collapse=", ")
  ######## QC d_494982282
  
  # valid NA or equal to or less than character length check
  
  valid_length= 800
  
  list_lengths = sapply(connectData$d_494982282,nchar)
  
  d_494982282_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_494982282,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_494982282,nchar)))
  d_494982282_invalid_char_length = connectData$d_494982282[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[130,1]<-substr(paste0("d_494982282"),3,100)
  
  df[130,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[130,3]<-paste0("char length",800)
  
  df[130,4]<-paste0("char length NOT <=",800)
  
  df[130,5]<-paste0(d_494982282_invalid_char_length, collapse=", ")
  
  df[130,6]<-paste0(rowNum, collapse=", ")
  
  df[130,7]<-paste0(token, collapse=", ")
  
  df[130,8]<-paste0(ID, collapse=", ")
  ######## QC d_699625233
  # valid value check
  
  d_699625233= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_699625233"%!in%d_699625233)
  
  d_699625233_invalid = addNA(connectData$"d_699625233"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[131,1]<-substr(paste0("d_699625233"),3,100)
  
  df[131,2]<-paste0("VALID")
  
  df[131,3]<-paste0(toString(d_699625233))
  
  df[131,4]<-paste0(toString("d_699625233 != "),toString(d_699625233),sep=" ")
  
  df[131,5]<-paste0(d_699625233_invalid, collapse=", ")
  
  df[131,6]<-paste0(rowNum, collapse=", ")
  
  df[131,7]<-paste0(token, collapse=", ")
  
  df[131,8]<-paste0(ID, collapse=", ")
  ######## QC d_430551721
  
  # valid NA or equal to or less than character length check
  
  valid_length= 24
  
  list_lengths = sapply(connectData$d_430551721,nchar)
  
  d_430551721_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_430551721,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_430551721,nchar)))
  d_430551721_invalid_char_length = connectData$d_430551721[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[132,1]<-substr(paste0("d_430551721"),3,100)
  
  df[132,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[132,3]<-paste0("char length",24)
  
  df[132,4]<-paste0("char length NOT <=",24)
  
  df[132,5]<-paste0(d_430551721_invalid_char_length, collapse=", ")
  
  df[132,6]<-paste0(rowNum, collapse=", ")
  
  df[132,7]<-paste0(token, collapse=", ")
  
  df[132,8]<-paste0(ID, collapse=", ")
  ######## QC d_204002618
  
  # valid NA or equal to or less than character length check
  
  valid_length= 18
  
  list_lengths = sapply(connectData$d_204002618,nchar)
  
  d_204002618_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_204002618,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_204002618,nchar)))
  d_204002618_invalid_char_length = connectData$d_204002618[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[133,1]<-substr(paste0("d_204002618"),3,100)
  
  df[133,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[133,3]<-paste0("char length",18)
  
  df[133,4]<-paste0("char length NOT <=",18)
  
  df[133,5]<-paste0(d_204002618_invalid_char_length, collapse=", ")
  
  df[133,6]<-paste0(rowNum, collapse=", ")
  
  df[133,7]<-paste0(token, collapse=", ")
  
  df[133,8]<-paste0(ID, collapse=", ")
  ######## QC d_765924958
  
  # valid NA or equal to or less than character length check
  
  valid_length= 18
  
  list_lengths = sapply(connectData$d_765924958,nchar)
  
  d_765924958_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_765924958,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_765924958,nchar)))
  d_765924958_invalid_char_length = connectData$d_765924958[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[134,1]<-substr(paste0("d_765924958"),3,100)
  
  df[134,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[134,3]<-paste0("char length",18)
  
  df[134,4]<-paste0("char length NOT <=",18)
  
  df[134,5]<-paste0(d_765924958_invalid_char_length, collapse=", ")
  
  df[134,6]<-paste0(rowNum, collapse=", ")
  
  df[134,7]<-paste0(token, collapse=", ")
  
  df[134,8]<-paste0(ID, collapse=", ")
  ######## QC d_821247024
  # valid value check
  
  d_821247024= c(875007964, 197316935, 219863910, 922622075, 160161595, NA)
  
  QCcheck1 =which(connectData$"d_821247024"%!in%d_821247024)
  
  d_821247024_invalid = addNA(connectData$"d_821247024"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[135,1]<-substr(paste0("d_821247024"),3,100)
  
  df[135,2]<-paste0("VALID")
  
  df[135,3]<-paste0(toString(d_821247024))
  
  df[135,4]<-paste0(toString("d_821247024 != "),toString(d_821247024),sep=" ")
  
  df[135,5]<-paste0(d_821247024_invalid, collapse=", ")
  
  df[135,6]<-paste0(rowNum, collapse=", ")
  
  df[135,7]<-paste0(token, collapse=", ")
  
  df[135,8]<-paste0(ID, collapse=", ")
  ######## QC d_914594314
  
  # valid NA or equal to or less than character length check
  
  valid_length= 24
  
  list_lengths = sapply(connectData$d_914594314,nchar)
  
  d_914594314_invalid= list_lengths[list_lengths > valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_914594314,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_914594314,nchar)))
  d_914594314_invalid_char_length = connectData$d_914594314[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[136,1]<-substr(paste0("d_914594314"),3,100)
  
  df[136,2]<-paste0("NA OR EQUAL TO OR LESS THAN CHAR()",valid_length)
  
  df[136,3]<-paste0("char length",24)
  
  df[136,4]<-paste0("char length NOT <=",24)
  
  df[136,5]<-paste0(d_914594314_invalid_char_length, collapse=", ")
  
  df[136,6]<-paste0(rowNum, collapse=", ")
  
  df[136,7]<-paste0(token, collapse=", ")
  
  df[136,8]<-paste0(ID, collapse=", ")
  ######## QC d_139603724
  
  # valid NA or equal to character length check
  
  valid_length= 2
  
  list_lengths = sapply(connectData$d_139603724,nchar)
  
  d_139603724_invalid= list_lengths[list_lengths != valid_length & !is.na(list_lengths)]
  
  QCcheck1 = which(sapply(connectData$d_139603724,nchar)== d_749475364_invalid & !is.na(sapply(connectData$d_139603724,nchar)))
  d_139603724_invalid_char_length = connectData$d_139603724[QCcheck1]
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[137,1]<-substr(paste0("d_139603724"),3,100)
  
  df[137,2]<-paste0("NA OR EQUAL TO CHAR() ",valid_length)
  
  df[137,3]<-paste0("char length",2)
  
  df[137,4]<-paste0("char length NOT =",2)
  
  df[137,5]<-paste0(d_139603724_invalid_char_length, collapse=", ")
  
  df[137,6]<-paste0(rowNum, collapse=", ")
  
  df[137,7]<-paste0(token, collapse=", ")
  
  df[137,8]<-paste0(ID, collapse=", ")
  ######## QC d_444699761
  # valid value check
  
  d_444699761= c(734437214, 426360242, NA)
  
  QCcheck1 =which(connectData$"d_444699761"%!in%d_444699761)
  
  d_444699761_invalid = addNA(connectData$"d_444699761"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[138,1]<-substr(paste0("d_444699761"),3,100)
  
  df[138,2]<-paste0("VALID")
  
  df[138,3]<-paste0(toString(d_444699761))
  
  df[138,4]<-paste0(toString("d_444699761 != "),toString(d_444699761),sep=" ")
  
  df[138,5]<-paste0(d_444699761_invalid, collapse=", ")
  
  df[138,6]<-paste0(rowNum, collapse=", ")
  
  df[138,7]<-paste0(token, collapse=", ")
  
  df[138,8]<-paste0(ID, collapse=", ")
  ######## QC d_188797763
  # valid value check
  
  d_188797763= c(353358909, 104430631, NA)
  
  QCcheck1 =which(connectData$"d_188797763"%!in%d_188797763)
  
  d_188797763_invalid = addNA(connectData$"d_188797763"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[139,1]<-substr(paste0("d_188797763"),3,100)
  
  df[139,2]<-paste0("VALID")
  
  df[139,3]<-paste0(toString(d_188797763))
  
  df[139,4]<-paste0(toString("d_188797763 != "),toString(d_188797763),sep=" ")
  
  df[139,5]<-paste0(d_188797763_invalid, collapse=", ")
  
  df[139,6]<-paste0(rowNum, collapse=", ")
  
  df[139,7]<-paste0(token, collapse=", ")
  
  df[139,8]<-paste0(ID, collapse=", ")
  ######## QC d_953614051
  # valid value check
  
  d_953614051= c(734437214, 426360242, NA)
  
  QCcheck1 =which(connectData$"d_953614051"%!in%d_953614051)
  
  d_953614051_invalid = addNA(connectData$"d_953614051"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[140,1]<-substr(paste0("d_953614051"),3,100)
  
  df[140,2]<-paste0("VALID")
  
  df[140,3]<-paste0(toString(d_953614051))
  
  df[140,4]<-paste0(toString("d_953614051 != "),toString(d_953614051),sep=" ")
  
  df[140,5]<-paste0(d_953614051_invalid, collapse=", ")
  
  df[140,6]<-paste0(rowNum, collapse=", ")
  
  df[140,7]<-paste0(token, collapse=", ")
  
  df[140,8]<-paste0(ID, collapse=", ")
  ######## QC d_148197146
  # valid value check
  
  d_148197146= c(638335430, 654558118, NA)
  
  QCcheck1 =which(connectData$"d_148197146"%!in%d_148197146)
  
  d_148197146_invalid = addNA(connectData$"d_148197146"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[141,1]<-substr(paste0("d_148197146"),3,100)
  
  df[141,2]<-paste0("VALID")
  
  df[141,3]<-paste0(toString(d_148197146))
  
  df[141,4]<-paste0(toString("d_148197146 != "),toString(d_148197146),sep=" ")
  
  df[141,5]<-paste0(d_148197146_invalid, collapse=", ")
  
  df[141,6]<-paste0(rowNum, collapse=", ")
  
  df[141,7]<-paste0(token, collapse=", ")
  
  df[141,8]<-paste0(ID, collapse=", ")
  ######## QC d_793822265
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_793822265_a = c(132080040, 854903954, 965707001, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_821247024 == "), c(922622075), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_793822265"[aa]%!in%d_793822265_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_793822265_invalid_cross = addNA(connectData$"d_793822265"[aa][QCcheck1])
  
  df[142,1]<-substr(paste0("d_793822265"),3,100)
  
  df[142,2]<-paste0("CROSSVALID1")
  
  df[142,3]<-paste0("132080040, 854903954, 965707001, NA")
  
  df[142,4]<-str_sub(mylist_a2, end =-4)
  
  df[142,5]<-paste0(d_793822265_invalid_cross, collapse=", ")
  
  df[142,6]<-paste0(rowNum, collapse=", ")
  
  df[142,7]<-paste0(token, collapse=", ")
  
  df[142,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_147176963
  # valid value check
  
  d_147176963= c(356674370, 219803804, NA)
  
  QCcheck1 =which(connectData$"d_147176963"%!in%d_147176963)
  
  d_147176963_invalid = addNA(connectData$"d_147176963"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[143,1]<-substr(paste0("d_147176963"),3,100)
  
  df[143,2]<-paste0("VALID")
  
  df[143,3]<-paste0(toString(d_147176963))
  
  df[143,4]<-paste0(toString("d_147176963 != "),toString(d_147176963),sep=" ")
  
  df[143,5]<-paste0(d_147176963_invalid, collapse=", ")
  
  df[143,6]<-paste0(rowNum, collapse=", ")
  
  df[143,7]<-paste0(token, collapse=", ")
  
  df[143,8]<-paste0(ID, collapse=", ")
  ######## QC d_557461333
  # valid value check
  
  d_557461333= c(356674370, 219803804, NA)
  
  QCcheck1 =which(connectData$"d_557461333"%!in%d_557461333)
  
  d_557461333_invalid = addNA(connectData$"d_557461333"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[144,1]<-substr(paste0("d_557461333"),3,100)
  
  df[144,2]<-paste0("VALID")
  
  df[144,3]<-paste0(toString(d_557461333))
  
  df[144,4]<-paste0(toString("d_557461333 != "),toString(d_557461333),sep=" ")
  
  df[144,5]<-paste0(d_557461333_invalid, collapse=", ")
  
  df[144,6]<-paste0(rowNum, collapse=", ")
  
  df[144,7]<-paste0(token, collapse=", ")
  
  df[144,8]<-paste0(ID, collapse=", ")
  ######## QC d_725929722
  # valid value check
  
  d_725929722= c(356674370, 219803804, NA)
  
  QCcheck1 =which(connectData$"d_725929722"%!in%d_725929722)
  
  d_725929722_invalid = addNA(connectData$"d_725929722"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[145,1]<-substr(paste0("d_725929722"),3,100)
  
  df[145,2]<-paste0("VALID")
  
  df[145,3]<-paste0(toString(d_725929722))
  
  df[145,4]<-paste0(toString("d_725929722 != "),toString(d_725929722),sep=" ")
  
  df[145,5]<-paste0(d_725929722_invalid, collapse=", ")
  
  df[145,6]<-paste0(rowNum, collapse=", ")
  
  df[145,7]<-paste0(token, collapse=", ")
  
  df[145,8]<-paste0(ID, collapse=", ")
  ######## QC d_711794630
  # valid value check
  
  d_711794630= c(356674370, 219803804, NA)
  
  QCcheck1 =which(connectData$"d_711794630"%!in%d_711794630)
  
  d_711794630_invalid = addNA(connectData$"d_711794630"[QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[QCcheck1]
  df[146,1]<-substr(paste0("d_711794630"),3,100)
  
  df[146,2]<-paste0("VALID")
  
  df[146,3]<-paste0(toString(d_711794630))
  
  df[146,4]<-paste0(toString("d_711794630 != "),toString(d_711794630),sep=" ")
  
  df[146,5]<-paste0(d_711794630_invalid, collapse=", ")
  
  df[146,6]<-paste0(rowNum, collapse=", ")
  
  df[146,7]<-paste0(token, collapse=", ")
  
  df[146,8]<-paste0(ID, collapse=", ")
  ######## QC d_679832994
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_679832994_a = c(353358909, 104430631, NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_827220437 == "), c(548392715, 125001209, 327912200, 300267574, 452412599, 657167265
                                                            
                                                            
                                                            
                                                            
  ), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_679832994"[aa]%!in%d_679832994_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_679832994_invalid_cross = addNA(connectData$"d_679832994"[aa][QCcheck1])
  
  df[147,1]<-substr(paste0("d_679832994"),3,100)
  
  df[147,2]<-paste0("CROSSVALID1")
  
  df[147,3]<-paste0("353358909, 104430631, NA")
  
  df[147,4]<-str_sub(mylist_a2, end =-4)
  
  df[147,5]<-paste0(d_679832994_invalid_cross, collapse=", ")
  
  df[147,6]<-paste0(rowNum, collapse=", ")
  
  df[147,7]<-paste0(token, collapse=", ")
  
  df[147,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_148197146
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_148197146_a = c(638335430, 654558118)
  
  
  mylist_a1 =  paste0(rep("connectData$d_821247024 == "), c(922622075), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_148197146"[aa]%!in%d_148197146_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_148197146_invalid_cross = addNA(connectData$"d_148197146"[aa][QCcheck1])
  
  df[148,1]<-substr(paste0("d_148197146"),3,100)
  
  df[148,2]<-paste0("CROSSVALID1")
  
  df[148,3]<-paste0("638335430, 654558118")
  
  df[148,4]<-str_sub(mylist_a2, end =-4)
  
  df[148,5]<-paste0(d_148197146_invalid_cross, collapse=", ")
  
  df[148,6]<-paste0(rowNum, collapse=", ")
  
  df[148,7]<-paste0(token, collapse=", ")
  
  df[148,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_148197146
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_148197146_a = c(NA)
  
  
  mylist_a1 =  paste0(rep("connectData$d_821247024 == "), c(875007964, 197316935, 219863910, 160161595), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_148197146"[aa]%!in%d_148197146_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_148197146_invalid_cross = addNA(connectData$"d_148197146"[aa][QCcheck1])
  
  df[149,1]<-substr(paste0("d_148197146"),3,100)
  
  df[149,2]<-paste0("CROSSVALID1")
  
  df[149,3]<-paste0("NA")
  
  df[149,4]<-str_sub(mylist_a2, end =-4)
  
  df[149,5]<-paste0(d_148197146_invalid_cross, collapse=", ")
  
  df[149,6]<-paste0(rowNum, collapse=", ")
  
  df[149,7]<-paste0(token, collapse=", ")
  
  df[149,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_188797763
  # cross valid value check - checks values if condition one is met and checks values if condition is not met
  
  d_188797763_a = c(104430631)
  
  
  mylist_a1 =  paste0(rep("connectData$d_444699761 == "), c(426360242), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_188797763"[aa]%!in%d_188797763_a)
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  d_188797763_invalid_cross = addNA(connectData$"d_188797763"[aa][QCcheck1])
  
  df[150,1]<-substr(paste0("d_188797763"),3,100)
  
  df[150,2]<-paste0("CROSSVALID1")
  
  df[150,3]<-paste0("104430631")
  
  df[150,4]<-str_sub(mylist_a2, end =-4)
  
  df[150,5]<-paste0(d_188797763_invalid_cross, collapse=", ")
  
  df[150,6]<-paste0(rowNum, collapse=", ")
  
  df[150,7]<-paste0(token, collapse=", ")
  
  df[150,8]<-paste0(ID, collapse=", ")
  
  ######## QC d_706256705
  # cross valid 2
  
  
  d_706256705_a = c(536341288, 654207589, 830573274, 178420302)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141, 854703046 ), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  
  QCcheck1 =which(connectData$"d_706256705"[aa]%!in%d_706256705_a)
  
  d_706256705_invalid_cross2_a = addNA(connectData$"d_706256705"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[151,1]<-substr(paste0("d_706256705"),3,100)
  
  df[151,2]<-paste0("CROSSVALID2")
  
  df[151,3]<-paste0("536341288, 654207589, 830573274, 178420302")
  
  df[151,4]<-str_sub(mylist_a, end =-3)
  
  df[151,5]<-paste0(d_706256705_invalid_cross2_a, collapse=", ")
  
  df[151,6]<-paste0(rowNum, collapse=", ")
  
  df[151,7]<-paste0(token, collapse=", ")
  
  df[151,8]<-paste0(ID, collapse=", ")
  ######## QC d_934298480
  # cross valid 2
  
  
  d_934298480_a = c(124276120, 450985724, 363147933, 636706443, 771230670)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141, 854703046 ), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  
  QCcheck1 =which(connectData$"d_934298480"[aa]%!in%d_934298480_a)
  
  d_934298480_invalid_cross2_a = addNA(connectData$"d_934298480"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[152,1]<-substr(paste0("d_934298480"),3,100)
  
  df[152,2]<-paste0("CROSSVALID2")
  
  df[152,3]<-paste0("124276120, 450985724, 363147933, 636706443, 771230670")
  
  df[152,4]<-str_sub(mylist_a, end =-3)
  
  df[152,5]<-paste0(d_934298480_invalid_cross2_a, collapse=", ")
  
  df[152,6]<-paste0(rowNum, collapse=", ")
  
  df[152,7]<-paste0(token, collapse=", ")
  
  df[152,8]<-paste0(ID, collapse=", ")
  ######## QC d_849518448
  # cross valid 2
  
  
  d_849518448_a = c(768826601, 181769837, 178420302)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141, 854703046 ), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  
  QCcheck1 =which(connectData$"d_849518448"[aa]%!in%d_849518448_a)
  
  d_849518448_invalid_cross2_a = addNA(connectData$"d_849518448"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[153,1]<-substr(paste0("d_849518448"),3,100)
  
  df[153,2]<-paste0("CROSSVALID2")
  
  df[153,3]<-paste0("768826601, 181769837, 178420302")
  
  df[153,4]<-str_sub(mylist_a, end =-3)
  
  df[153,5]<-paste0(d_849518448_invalid_cross2_a, collapse=", ")
  
  df[153,6]<-paste0(rowNum, collapse=", ")
  
  df[153,7]<-paste0(token, collapse=", ")
  
  df[153,8]<-paste0(ID, collapse=", ")
  ######## QC d_119643471
  # cross valid 3
  
  
  d_119643471_a = c(232334767, 211228524, 308427446, 635279662, 432722256, 232663805, 785578696, 200929978, 490725843, 965998904, 986445321, 746038746, 178420302)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_827220437 == "), c(657167265), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_aaa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141, 854703046 ), sep =" | ")
  
  mylist_aaa2 = str_c(mylist_aaa1, sep = "", collapse ="") # make many or statements
  
  mylist_aaa3 = paste0("(",str_sub(mylist_aaa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,mylist_aaa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_119643471"[aa]%!in%d_119643471_a)
  
  d_119643471_invalid_cross3_a = addNA(connectData$"d_119643471"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[154,1]<-substr(paste0("d_119643471"),3,100)
  
  df[154,2]<-paste0("CROSSVALID3")
  
  df[154,3]<-paste0("232334767, 211228524, 308427446, 635279662, 432722256, 232663805, 785578696, 200929978, 490725843, 965998904, 986445321, 746038746, 178420302")
  
  df[154,4]<-str_sub(mylist_a, end =-1)
  
  df[154,5]<-paste0(d_119643471_invalid_cross3_a, collapse=", ")
  
  df[154,6]<-paste0(rowNum, collapse=", ")
  
  df[154,7]<-paste0(token, collapse=", ")
  
  df[154,8]<-paste0(ID, collapse=", ")
  ######## QC d_684926335
  # cross valid 3
  
  
  d_684926335_a = c(232334767, 635279662, 401335456, 178420302)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_827220437 == "), c(548392715), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_aaa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141, 854703046 ), sep =" | ")
  
  mylist_aaa2 = str_c(mylist_aaa1, sep = "", collapse ="") # make many or statements
  
  mylist_aaa3 = paste0("(",str_sub(mylist_aaa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,mylist_aaa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_684926335"[aa]%!in%d_684926335_a)
  
  d_684926335_invalid_cross3_a = addNA(connectData$"d_684926335"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[155,1]<-substr(paste0("d_684926335"),3,100)
  
  df[155,2]<-paste0("CROSSVALID3")
  
  df[155,3]<-paste0("232334767, 635279662, 401335456, 178420302")
  
  df[155,4]<-str_sub(mylist_a, end =-1)
  
  df[155,5]<-paste0(d_684926335_invalid_cross3_a, collapse=", ")
  
  df[155,6]<-paste0(rowNum, collapse=", ")
  
  df[155,7]<-paste0(token, collapse=", ")
  
  df[155,8]<-paste0(ID, collapse=", ")
  ######## QC d_678756255
  # cross valid 3
  
  
  d_678756255_a = c(536341288, 654207589, 395528052, 181769837, 178420302)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_827220437 == "), c(125001209, 327912200, 300267574, 452412599), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_aaa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141, 854703046 ), sep =" | ")
  
  mylist_aaa2 = str_c(mylist_aaa1, sep = "", collapse ="") # make many or statements
  
  mylist_aaa3 = paste0("(",str_sub(mylist_aaa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,mylist_aaa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_678756255"[aa]%!in%d_678756255_a)
  
  d_678756255_invalid_cross3_a = addNA(connectData$"d_678756255"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[156,1]<-substr(paste0("d_678756255"),3,100)
  
  df[156,2]<-paste0("CROSSVALID3")
  
  df[156,3]<-paste0("536341288, 654207589, 395528052, 181769837, 178420302")
  
  df[156,4]<-str_sub(mylist_a, end =-1)
  
  df[156,5]<-paste0(d_678756255_invalid_cross3_a, collapse=", ")
  
  df[156,6]<-paste0(rowNum, collapse=", ")
  
  df[156,7]<-paste0(token, collapse=", ")
  
  df[156,8]<-paste0(ID, collapse=", ")
  ######## QC d_435027713
  # cross valid 3
  
  
  d_435027713_a = c(536341288, 654207589178420302)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_827220437 == "), c(657167265, 548392715), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_aaa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141, 854703046 ), sep =" | ")
  
  mylist_aaa2 = str_c(mylist_aaa1, sep = "", collapse ="") # make many or statements
  
  mylist_aaa3 = paste0("(",str_sub(mylist_aaa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,mylist_aaa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_435027713"[aa]%!in%d_435027713_a)
  
  d_435027713_invalid_cross3_a = addNA(connectData$"d_435027713"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[157,1]<-substr(paste0("d_435027713"),3,100)
  
  df[157,2]<-paste0("CROSSVALID3")
  
  df[157,3]<-paste0("536341288, 654207589178420302")
  
  df[157,4]<-str_sub(mylist_a, end =-1)
  
  df[157,5]<-paste0(d_435027713_invalid_cross3_a, collapse=", ")
  
  df[157,6]<-paste0(rowNum, collapse=", ")
  
  df[157,7]<-paste0(token, collapse=", ")
  
  df[157,8]<-paste0(ID, collapse=", ")
  ######## QC d_444699761
  # cross valid 2
  
  
  d_444699761_a = c(734437214, 426360242)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141, 854703046 ), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  
  QCcheck1 =which(connectData$"d_444699761"[aa]%!in%d_444699761_a)
  
  d_444699761_invalid_cross2_a = addNA(connectData$"d_444699761"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[158,1]<-substr(paste0("d_444699761"),3,100)
  
  df[158,2]<-paste0("CROSSVALID2")
  
  df[158,3]<-paste0("734437214, 426360242")
  
  df[158,4]<-str_sub(mylist_a, end =-3)
  
  df[158,5]<-paste0(d_444699761_invalid_cross2_a, collapse=", ")
  
  df[158,6]<-paste0(rowNum, collapse=", ")
  
  df[158,7]<-paste0(token, collapse=", ")
  
  df[158,8]<-paste0(ID, collapse=", ")
  ######## QC d_188797763
  # cross valid 2
  
  
  d_188797763_a = c(353358909, 104430631)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141, 854703046 ), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  
  QCcheck1 =which(connectData$"d_188797763"[aa]%!in%d_188797763_a)
  
  d_188797763_invalid_cross2_a = addNA(connectData$"d_188797763"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[159,1]<-substr(paste0("d_188797763"),3,100)
  
  df[159,2]<-paste0("CROSSVALID2")
  
  df[159,3]<-paste0("353358909, 104430631")
  
  df[159,4]<-str_sub(mylist_a, end =-3)
  
  df[159,5]<-paste0(d_188797763_invalid_cross2_a, collapse=", ")
  
  df[159,6]<-paste0(rowNum, collapse=", ")
  
  df[159,7]<-paste0(token, collapse=", ")
  
  df[159,8]<-paste0(ID, collapse=", ")
  ######## QC d_953614051
  # cross valid 2
  
  
  d_953614051_a = c(734437214, 426360242)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141, 854703046 ), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  
  QCcheck1 =which(connectData$"d_953614051"[aa]%!in%d_953614051_a)
  
  d_953614051_invalid_cross2_a = addNA(connectData$"d_953614051"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[160,1]<-substr(paste0("d_953614051"),3,100)
  
  df[160,2]<-paste0("CROSSVALID2")
  
  df[160,3]<-paste0("734437214, 426360242")
  
  df[160,4]<-str_sub(mylist_a, end =-3)
  
  df[160,5]<-paste0(d_953614051_invalid_cross2_a, collapse=", ")
  
  df[160,6]<-paste0(rowNum, collapse=", ")
  
  df[160,7]<-paste0(token, collapse=", ")
  
  df[160,8]<-paste0(ID, collapse=", ")
  ######## QC d_148197146
  # cross valid 2
  
  
  d_148197146_a = c(638335430, 654558118 )#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141, 854703046 ), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  
  QCcheck1 =which(connectData$"d_148197146"[aa]%!in%d_148197146_a)
  
  d_148197146_invalid_cross2_a = addNA(connectData$"d_148197146"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[161,1]<-substr(paste0("d_148197146"),3,100)
  
  df[161,2]<-paste0("CROSSVALID2")
  
  df[161,3]<-paste0("638335430, 654558118 ")
  
  df[161,4]<-str_sub(mylist_a, end =-3)
  
  df[161,5]<-paste0(d_148197146_invalid_cross2_a, collapse=", ")
  
  df[161,6]<-paste0(rowNum, collapse=", ")
  
  df[161,7]<-paste0(token, collapse=", ")
  
  df[161,8]<-paste0(ID, collapse=", ")
  ######## QC d_793822265
  # cross valid 2
  
  
  d_793822265_a = c(132080040, 854903954, 965707001)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141, 854703046 ), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  
  QCcheck1 =which(connectData$"d_793822265"[aa]%!in%d_793822265_a)
  
  d_793822265_invalid_cross2_a = addNA(connectData$"d_793822265"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[162,1]<-substr(paste0("d_793822265"),3,100)
  
  df[162,2]<-paste0("CROSSVALID2")
  
  df[162,3]<-paste0("132080040, 854903954, 965707001")
  
  df[162,4]<-str_sub(mylist_a, end =-3)
  
  df[162,5]<-paste0(d_793822265_invalid_cross2_a, collapse=", ")
  
  df[162,6]<-paste0(rowNum, collapse=", ")
  
  df[162,7]<-paste0(token, collapse=", ")
  
  df[162,8]<-paste0(ID, collapse=", ")
  ######## QC d_147176963
  # cross valid 2
  
  
  d_147176963_a = c(356674370, 219803804)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141, 854703046 ), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  
  QCcheck1 =which(connectData$"d_147176963"[aa]%!in%d_147176963_a)
  
  d_147176963_invalid_cross2_a = addNA(connectData$"d_147176963"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[163,1]<-substr(paste0("d_147176963"),3,100)
  
  df[163,2]<-paste0("CROSSVALID2")
  
  df[163,3]<-paste0("356674370, 219803804")
  
  df[163,4]<-str_sub(mylist_a, end =-3)
  
  df[163,5]<-paste0(d_147176963_invalid_cross2_a, collapse=", ")
  
  df[163,6]<-paste0(rowNum, collapse=", ")
  
  df[163,7]<-paste0(token, collapse=", ")
  
  df[163,8]<-paste0(ID, collapse=", ")
  ######## QC d_557461333
  # cross valid 2
  
  
  d_557461333_a = c(356674370, 219803804)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141, 854703046 ), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  
  QCcheck1 =which(connectData$"d_557461333"[aa]%!in%d_557461333_a)
  
  d_557461333_invalid_cross2_a = addNA(connectData$"d_557461333"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[164,1]<-substr(paste0("d_557461333"),3,100)
  
  df[164,2]<-paste0("CROSSVALID2")
  
  df[164,3]<-paste0("356674370, 219803804")
  
  df[164,4]<-str_sub(mylist_a, end =-3)
  
  df[164,5]<-paste0(d_557461333_invalid_cross2_a, collapse=", ")
  
  df[164,6]<-paste0(rowNum, collapse=", ")
  
  df[164,7]<-paste0(token, collapse=", ")
  
  df[164,8]<-paste0(ID, collapse=", ")
  ######## QC d_725929722
  # cross valid 2
  
  
  d_725929722_a = c(356674370, 219803804)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141, 854703046 ), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  
  QCcheck1 =which(connectData$"d_725929722"[aa]%!in%d_725929722_a)
  
  d_725929722_invalid_cross2_a = addNA(connectData$"d_725929722"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[165,1]<-substr(paste0("d_725929722"),3,100)
  
  df[165,2]<-paste0("CROSSVALID2")
  
  df[165,3]<-paste0("356674370, 219803804")
  
  df[165,4]<-str_sub(mylist_a, end =-3)
  
  df[165,5]<-paste0(d_725929722_invalid_cross2_a, collapse=", ")
  
  df[165,6]<-paste0(rowNum, collapse=", ")
  
  df[165,7]<-paste0(token, collapse=", ")
  
  df[165,8]<-paste0(ID, collapse=", ")
  ######## QC d_711794630
  # cross valid 2
  
  
  d_711794630_a = c(356674370, 219803804)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141, 854703046 ), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  
  QCcheck1 =which(connectData$"d_711794630"[aa]%!in%d_711794630_a)
  
  d_711794630_invalid_cross2_a = addNA(connectData$"d_711794630"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[166,1]<-substr(paste0("d_711794630"),3,100)
  
  df[166,2]<-paste0("CROSSVALID2")
  
  df[166,3]<-paste0("356674370, 219803804")
  
  df[166,4]<-str_sub(mylist_a, end =-3)
  
  df[166,5]<-paste0(d_711794630_invalid_cross2_a, collapse=", ")
  
  df[166,6]<-paste0(rowNum, collapse=", ")
  
  df[166,7]<-paste0(token, collapse=", ")
  
  df[166,8]<-paste0(ID, collapse=", ")
  ######## QC d_679832994
  # cross valid 2
  
  
  d_679832994_a = c(356674370, 219803804)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141, 854703046 ), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  
  QCcheck1 =which(connectData$"d_679832994"[aa]%!in%d_679832994_a)
  
  d_679832994_invalid_cross2_a = addNA(connectData$"d_679832994"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[167,1]<-substr(paste0("d_679832994"),3,100)
  
  df[167,2]<-paste0("CROSSVALID2")
  
  df[167,3]<-paste0("356674370, 219803804")
  
  df[167,4]<-str_sub(mylist_a, end =-3)
  
  df[167,5]<-paste0(d_679832994_invalid_cross2_a, collapse=", ")
  
  df[167,6]<-paste0(rowNum, collapse=", ")
  
  df[167,7]<-paste0(token, collapse=", ")
  
  df[167,8]<-paste0(ID, collapse=", ")
  ######## QC d_559534463
  # cross valid 2
  
  
  d_559534463_a = c(356674370, 219803804)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141, 854703046 ), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  
  QCcheck1 =which(connectData$"d_559534463"[aa]%!in%d_559534463_a)
  
  d_559534463_invalid_cross2_a = addNA(connectData$"d_559534463"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[168,1]<-substr(paste0("d_559534463"),3,100)
  
  df[168,2]<-paste0("CROSSVALID2")
  
  df[168,3]<-paste0("356674370, 219803804")
  
  df[168,4]<-str_sub(mylist_a, end =-3)
  
  df[168,5]<-paste0(d_559534463_invalid_cross2_a, collapse=", ")
  
  df[168,6]<-paste0(rowNum, collapse=", ")
  
  df[168,7]<-paste0(token, collapse=", ")
  
  df[168,8]<-paste0(ID, collapse=", ")
  ######## QC d_570452130
  # cross valid 2
  
  
  d_570452130_a = c(539025306, 427405444)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141, 854703046 ), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  
  QCcheck1 =which(connectData$"d_570452130"[aa]%!in%d_570452130_a)
  
  d_570452130_invalid_cross2_a = addNA(connectData$"d_570452130"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[169,1]<-substr(paste0("d_570452130"),3,100)
  
  df[169,2]<-paste0("CROSSVALID2")
  
  df[169,3]<-paste0("539025306, 427405444")
  
  df[169,4]<-str_sub(mylist_a, end =-3)
  
  df[169,5]<-paste0(d_570452130_invalid_cross2_a, collapse=", ")
  
  df[169,6]<-paste0(rowNum, collapse=", ")
  
  df[169,7]<-paste0(token, collapse=", ")
  
  df[169,8]<-paste0(ID, collapse=", ")
  ######## QC d_629484663
  # cross valid 2
  
  
  d_629484663_a = c(539025306, 427405444)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141, 854703046 ), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  
  QCcheck1 =which(connectData$"d_629484663"[aa]%!in%d_629484663_a)
  
  d_629484663_invalid_cross2_a = addNA(connectData$"d_629484663"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[170,1]<-substr(paste0("d_629484663"),3,100)
  
  df[170,2]<-paste0("CROSSVALID2")
  
  df[170,3]<-paste0("539025306, 427405444")
  
  df[170,4]<-str_sub(mylist_a, end =-3)
  
  df[170,5]<-paste0(d_629484663_invalid_cross2_a, collapse=", ")
  
  df[170,6]<-paste0(rowNum, collapse=", ")
  
  df[170,7]<-paste0(token, collapse=", ")
  
  df[170,8]<-paste0(ID, collapse=", ")
  ######## QC d_547895941
  # cross valid 2
  
  
  d_547895941_a = c(539025306, 427405444)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141, 854703046 ), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  
  QCcheck1 =which(connectData$"d_547895941"[aa]%!in%d_547895941_a)
  
  d_547895941_invalid_cross2_a = addNA(connectData$"d_547895941"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[171,1]<-substr(paste0("d_547895941"),3,100)
  
  df[171,2]<-paste0("CROSSVALID2")
  
  df[171,3]<-paste0("539025306, 427405444")
  
  df[171,4]<-str_sub(mylist_a, end =-3)
  
  df[171,5]<-paste0(d_547895941_invalid_cross2_a, collapse=", ")
  
  df[171,6]<-paste0(rowNum, collapse=", ")
  
  df[171,7]<-paste0(token, collapse=", ")
  
  df[171,8]<-paste0(ID, collapse=", ")
  ######## QC d_119643471
  # cross valid 2
  
  
  d_119643471_a = c(232334767, 211228524, 308427446, 635279662, 432722256, 232663805, 785578696, 200929978, 490725843, 965998904, 986445321, 746038746, 178420302)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_827220437 == "), c(657167265), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  
  QCcheck1 =which(connectData$"d_119643471"[aa]%!in%d_119643471_a)
  
  d_119643471_invalid_cross2_a = addNA(connectData$"d_119643471"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[172,1]<-substr(paste0("d_119643471"),3,100)
  
  df[172,2]<-paste0("CROSSVALID2")
  
  df[172,3]<-paste0("232334767, 211228524, 308427446, 635279662, 432722256, 232663805, 785578696, 200929978, 490725843, 965998904, 986445321, 746038746, 178420302")
  
  df[172,4]<-str_sub(mylist_a, end =-3)
  
  df[172,5]<-paste0(d_119643471_invalid_cross2_a, collapse=", ")
  
  df[172,6]<-paste0(rowNum, collapse=", ")
  
  df[172,7]<-paste0(token, collapse=", ")
  
  df[172,8]<-paste0(ID, collapse=", ")
  ######## QC d_684926335
  # cross valid 2
  
  
  d_684926335_a = c(232334767, 635279662, 401335456, 178420302)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_827220437 == "), c(548392715), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  
  QCcheck1 =which(connectData$"d_684926335"[aa]%!in%d_684926335_a)
  
  d_684926335_invalid_cross2_a = addNA(connectData$"d_684926335"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[173,1]<-substr(paste0("d_684926335"),3,100)
  
  df[173,2]<-paste0("CROSSVALID2")
  
  df[173,3]<-paste0("232334767, 635279662, 401335456, 178420302")
  
  df[173,4]<-str_sub(mylist_a, end =-3)
  
  df[173,5]<-paste0(d_684926335_invalid_cross2_a, collapse=", ")
  
  df[173,6]<-paste0(rowNum, collapse=", ")
  
  df[173,7]<-paste0(token, collapse=", ")
  
  df[173,8]<-paste0(ID, collapse=", ")
  ######## QC d_678756255
  # cross valid 2
  
  
  d_678756255_a = c(536341288, 654207589, 395528052, 181769837, 178420302)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_827220437 == "), c(125001209, 327912200, 300267574, 452412599), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  
  QCcheck1 =which(connectData$"d_678756255"[aa]%!in%d_678756255_a)
  
  d_678756255_invalid_cross2_a = addNA(connectData$"d_678756255"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[174,1]<-substr(paste0("d_678756255"),3,100)
  
  df[174,2]<-paste0("CROSSVALID2")
  
  df[174,3]<-paste0("536341288, 654207589, 395528052, 181769837, 178420302")
  
  df[174,4]<-str_sub(mylist_a, end =-3)
  
  df[174,5]<-paste0(d_678756255_invalid_cross2_a, collapse=", ")
  
  df[174,6]<-paste0(rowNum, collapse=", ")
  
  df[174,7]<-paste0(token, collapse=", ")
  
  df[174,8]<-paste0(ID, collapse=", ")
  ######## QC d_435027713
  # cross valid 2
  
  
  d_435027713_a = c(536341288, 654207589178420302)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_827220437 == "), c(657167265, 548392715), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  
  QCcheck1 =which(connectData$"d_435027713"[aa]%!in%d_435027713_a)
  
  d_435027713_invalid_cross2_a = addNA(connectData$"d_435027713"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[175,1]<-substr(paste0("d_435027713"),3,100)
  
  df[175,2]<-paste0("CROSSVALID2")
  
  df[175,3]<-paste0("536341288, 654207589178420302")
  
  df[175,4]<-str_sub(mylist_a, end =-3)
  
  df[175,5]<-paste0(d_435027713_invalid_cross2_a, collapse=", ")
  
  df[175,6]<-paste0(rowNum, collapse=", ")
  
  df[175,7]<-paste0(token, collapse=", ")
  
  df[175,8]<-paste0(ID, collapse=", ")
  ######## QC d_119643471
  # cross valid 3
  
  
  d_119643471_a = c(232334767, 211228524, 308427446, 635279662, 432722256, 232663805, 785578696, 200929978, 490725843, 965998904, 986445321, 746038746, 178420302)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_827220437 == "), c(657167265), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_aaa1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_aaa2 = str_c(mylist_aaa1, sep = "", collapse ="") # make many or statements
  
  mylist_aaa3 = paste0("(",str_sub(mylist_aaa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,mylist_aaa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_119643471"[aa]%!in%d_119643471_a)
  
  d_119643471_invalid_cross3_a = addNA(connectData$"d_119643471"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[176,1]<-substr(paste0("d_119643471"),3,100)
  
  df[176,2]<-paste0("CROSSVALID3")
  
  df[176,3]<-paste0("232334767, 211228524, 308427446, 635279662, 432722256, 232663805, 785578696, 200929978, 490725843, 965998904, 986445321, 746038746, 178420302")
  
  df[176,4]<-str_sub(mylist_a, end =-1)
  
  df[176,5]<-paste0(d_119643471_invalid_cross3_a, collapse=", ")
  
  df[176,6]<-paste0(rowNum, collapse=", ")
  
  df[176,7]<-paste0(token, collapse=", ")
  
  df[176,8]<-paste0(ID, collapse=", ")
  ######## QC d_684926335
  # cross valid 3
  
  
  d_684926335_a = c(232334767, 635279662, 401335456, 178420302)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_827220437 == "), c(548392715), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_aaa1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_aaa2 = str_c(mylist_aaa1, sep = "", collapse ="") # make many or statements
  
  mylist_aaa3 = paste0("(",str_sub(mylist_aaa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,mylist_aaa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_684926335"[aa]%!in%d_684926335_a)
  
  d_684926335_invalid_cross3_a = addNA(connectData$"d_684926335"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[177,1]<-substr(paste0("d_684926335"),3,100)
  
  df[177,2]<-paste0("CROSSVALID3")
  
  df[177,3]<-paste0("232334767, 635279662, 401335456, 178420302")
  
  df[177,4]<-str_sub(mylist_a, end =-1)
  
  df[177,5]<-paste0(d_684926335_invalid_cross3_a, collapse=", ")
  
  df[177,6]<-paste0(rowNum, collapse=", ")
  
  df[177,7]<-paste0(token, collapse=", ")
  
  df[177,8]<-paste0(ID, collapse=", ")
  ######## QC d_678756255
  # cross valid 3
  
  
  d_678756255_a = c(536341288, 654207589, 395528052, 181769837, 178420302)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_827220437 == "), c(125001209, 327912200, 300267574, 452412599), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_aaa1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_aaa2 = str_c(mylist_aaa1, sep = "", collapse ="") # make many or statements
  
  mylist_aaa3 = paste0("(",str_sub(mylist_aaa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,mylist_aaa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_678756255"[aa]%!in%d_678756255_a)
  
  d_678756255_invalid_cross3_a = addNA(connectData$"d_678756255"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[178,1]<-substr(paste0("d_678756255"),3,100)
  
  df[178,2]<-paste0("CROSSVALID3")
  
  df[178,3]<-paste0("536341288, 654207589, 395528052, 181769837, 178420302")
  
  df[178,4]<-str_sub(mylist_a, end =-1)
  
  df[178,5]<-paste0(d_678756255_invalid_cross3_a, collapse=", ")
  
  df[178,6]<-paste0(rowNum, collapse=", ")
  
  df[178,7]<-paste0(token, collapse=", ")
  
  df[178,8]<-paste0(ID, collapse=", ")
  ######## QC d_435027713
  # cross valid 3
  
  
  d_435027713_a = c(536341288, 654207589178420302)#a: if cid2 is relevant
  
  mylist_a1 =  paste0(rep("connectData$d_827220437 == "), c(657167265, 548392715), sep =" | ")
  
  mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements
  
  mylist_a3 = paste0("(",str_sub(mylist_a2, end =-4),")") #remove extra " |" at the end of string
  
  mylist_aa1 =  paste0(rep("connectData$d_512820379 == "), c(486306141), sep =" | ")
  
  mylist_aa2 = str_c(mylist_aa1, sep = "", collapse ="") # make many or statements
  
  mylist_aa3 = paste0("(",str_sub(mylist_aa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_aaa1 =  paste0(rep("connectData$d_821247024 == "), c(197316935), sep =" | ")
  
  mylist_aaa2 = str_c(mylist_aaa1, sep = "", collapse ="") # make many or statements
  
  mylist_aaa3 = paste0("(",str_sub(mylist_aaa2, end =-3),")") #remove extra " |" at the end of string
  
  mylist_a = paste(mylist_a3, mylist_aa3,mylist_aaa3,sep=" &  ")
  
  aa = which(eval(parse(text=mylist_a))) # remove quotes to make logical expression
  
  QCcheck1 =which(connectData$"d_435027713"[aa]%!in%d_435027713_a)
  
  d_435027713_invalid_cross3_a = addNA(connectData$"d_435027713"[aa][QCcheck1])
  
  rowNum<-QCcheck1
  token<- connectData$"token"[QCcheck1]
  ID = connectData$Connect_ID[aa][QCcheck1]
  df[179,1]<-substr(paste0("d_435027713"),3,100)
  
  df[179,2]<-paste0("CROSSVALID3")
  
  df[179,3]<-paste0("536341288, 654207589178420302")
  
  df[179,4]<-str_sub(mylist_a, end =-1)
  
  df[179,5]<-paste0(d_435027713_invalid_cross3_a, collapse=", ")
  
  df[179,6]<-paste0(rowNum, collapse=", ")
  
  df[179,7]<-paste0(token, collapse=", ")
  
  df[179,8]<-paste0(ID, collapse=", ")
  
  # filter df to show QC errors only
  
  qc_errors = filter(df, (!is.na(df$"invalid_values_found") ))
  
  qc_errors = filter(qc_errors, (qc_errors$"invalid_values_found" != "" ))
  
  # TRANSLATE REPORT 
  
  qc_errors=TRANSLATE.COL(report= qc_errors , 
                          translate.these.cols = c("ConceptID", "valid_values"),
                          new.col.names = c("ConceptID_translated","valid_values_translated"),
                          dictionary = dictionary)
  
  # add "no errors found" row if no rows found in QC report
  if (nrow(qc_errors)==0){
    qc_errors[1, ] = c("no errors found")
  }
  
  # add site column
  qc_errors = add_column(qc_errors, site = as.character(site) , .before=1)
  # add date column
  qc_errors = add_column(qc_errors, date = Sys.Date() , .before=1)
  ######## SAVE QC SCRIPT TO BIGQUERY QC_report table
  
  
  ######## upload report to bigquery ##############################
  
  #Append data to an existing table or create a table if if doesnt exist
  bq_table_upload(x=QC_report_location,
                  values=qc_errors,
                  fields = qc_errors,
                  create_disposition="CREATE_IF_NEEDED",
                  write_disposition="WRITE_APPEND")
  return(qc_errors)
}