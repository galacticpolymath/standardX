require(openxlsx);require(dplyr);require(stringr);require(XLConnect)# require(stringi);
c3<-read.csv("data/formatted_c3SocialStudies.csv")
ela<-read.csv("data/formatted_CommonCoreELA.csv")
math<-read.csv("data/formatted_CommonCoreMath.csv")
sci<-read.csv("data/formatted_ngss.csv",quote="\"")
sdg_w_verbose_targets<-read.csv("data/SDG-targets.csv")
#just keep the basic targets (no 1.1, etc)
sdg<-subset(sdg_w_verbose_targets,!grepl("\\.",target))
names(sdg)[c(2,4)]<-c("code","statement")
sdg$subcategory=sapply(sdg$goal,function(x) switch(x,"1"="No Poverty","2"="Zero Hunger","3"="Good Health and Well-Being","4"="Quality Education","5"="Gender Equality","6"="Clean Water and Sanitation","7"="Affordable and Clean Energy","8"="Decent Work and Economic Growth","9"="Industry, Innovation, and Infrastructure","10"="Reduced Inequalities","11"="Sustainable Cities and Communities","12"="Responsible Consumption and Production","13"="Climate Action","14"="Life Below Water","15"="Life on Land","16"="Peace, Justice, and Strong Institutions","17"="Partnerships for the Goals"))
# sdg$dim<-gsub("[a-z]| |-|,","",sdg$subcat)
sdg$code<-paste0("Goal ",sdg$code)
sdg<-sdg %>% select(-goal)
sci<-sci %>% select(-subcat)

c3$subject<-"Social Studies"
c3$set<-"C3"
ela$subject<-"ELA"
ela$set<-"Common Core ELA"
math$subject <- "Math"
math$set<-"Common Core Math"
sci$subject <- "Science"
sci$set<-"NGSS"
sdg$set <- "SDGs"
sdg$subject<-"Sustainability"
sdg$grade <- "K,1,2,3,4,5,6,7,8,9,10,11,12"


#NGSS statement codes have weird apostrophes and long em dash that caused probs ’ b4 I saved as csv w/ utf-8! This code is left over from those weird import issues
# test<-sci$statement[66]
# grep("\xd5",test,fixed=T,useBytes=T)
# test2 <- stri_replace_all(test,"'",fixed="\xd5")
# grep("\xd5",test2,fixed=T,useBytes=T)
# sapply(sci$statement,function(x) length(grep("\xd5",x,fixed=T,useBytes=T))) %>% sum()
# sci<-sci %>% apply(.,c(1,2),function(x) stri_replace_all(x,"'",fixed="\xd5")) %>% as_tibble()
# sapply(sci$statement,function(x) length(grep("\xd5",x,fixed=T,useBytes=T))) %>% sum()
# 
# #now fix directional quotes “”
# sci$statement %>% sapply(.,function(x) stri_detect(x,fixed="\xd1")) %>% which() %>% unname()
# sapply(sci$statement,function(x) length(grep("\xd2|\xd3",x,fixed=F,useBytes=T))) %>% sum()
# sci<-sci %>% apply(.,c(1,2),function(x) stri_replace_all(x,"\"",regex="\xd2|xd3")) %>% as_tibble()
# sapply(sci$statement,function(x) length(grep("\xd2|\xd3",x,fixed=F,useBytes=T))) %>% sum()


allSubj<-full_join(c3,ela) %>% full_join(math) %>% full_join(sci) %>% full_join(sdg)%>% select(code,set,dim,grade,statement,everything())
#convert  #N/A to NA
allSubj<-apply(allSubj,c(1,2),function(x) ifelse(x=="#N/A",NA,x)) %>% as_tibble()

######
#remove HTML tags! (these are in common core standards, adding italics and whatnot,
#but they're problematic and don't render in javascript)

#special problem is Common Core has footnotes (i.e. <sup>1</sup> referring to source 1)
#also sometimes <sup> refers to an exponent, which we want to keep...so first delete footnotes
#which fortunately always happen at the end of a "."

#example:
allSubj$statement[1290] #1 example of ^1 footnote in the middle of a sentence
allSubj$statement <-allSubj$statement %>% stringr::str_replace_all("<sup>1</sup>","") #no reason to exponent 1 except as a footnote
allSubj$statement[1290] #it worked

allSubj$statement[1313] # example of footnote at end of sentence
allSubj$statement <-allSubj$statement %>% stringr::str_replace_all("\\.<sup>.<\\/sup>","\\.")
allSubj$statement[1313]



#Still remaining problem of exponents that we actually want to keep

#example:
allSubj$statement[1603]
allSubj$statement <-allSubj$statement %>% stringr::str_replace_all("<sup>","^")
allSubj$statement <-allSubj$statement %>% stringr::str_replace_all("</sup>","")
allSubj$statement[1603] #successfully changed "s<sup>2</sup>" to "s^2"

#Remaining problem--all other HTML tags...
#start with italics, which we can change to markdown
allSubj$statement[1603] # i and em tags need change to *text*
allSubj$statement<-allSubj$statement %>% stringr::str_replace_all(pattern="<[\\/?i|\\/?em]*>","*")

#check if it worked:
#show example:
allSubj$statement[1603]
#it did; but still leaving in special character codes like SQRT, i.e. "&radic;"

#Are there other HTML tags? Remove dem
allSubj$statement<-allSubj$statement %>% stringr::str_remove_all(pattern="<[^>]*>")

head(allSubj)
tail(allSubj)
#check
sum(sapply(list(c3,ela,math,sci,sdg),nrow))
nrow(allSubj)

# Export data -------------------------------------------------------------
######################
#Reorganize column order
allSubj_final<-allSubj %>% relocate(code,statement, set, dim, grade, subject,dimension,subcategory,PEcode,performanceExpectation)
# output CSV
write.csv(allSubj_final,"allStandards.csv",row.names=F)

# Make Excel version 
write.xlsx(allSubj_final,"allStandards.xlsx",row.names=F,freezePane=list(firstRow=T),overwrite = T)


# Save align to standards template ----------------------------------------
alignmentTemplate<-allSubj_final %>% mutate(ALIGN=NA) %>% relocate(ALIGN)

#Load template
alignToStndz<- XLConnect::loadWorkbook("align-to-all-subject-standards.xlsx")

#save without overwriting formatting
XLConnect::setStyleAction(alignToStndz,XLConnect::XLC$"STYLE_ACTION.NONE")

#save backup
XLConnect::saveWorkbook(alignToStndz,"data/align-to-all-subject-standards-OLD.xlsx")
message("  *Old version of file saved at 'data/align-to-all-subject-standards-OLD.xlsx\n'")

#Clear old data and update "align to all subjects template"
deleteRange<-c(2,1,nrow(allSubj_final)+1000,ncol(allSubj_final)+10) #added extra in case we ever simplify the data set...it all gets delted
XLConnect::clearRange(alignToStndz,sheet="Sheet1",coords=deleteRange)
XLConnect::writeWorksheet(alignToStndz,data=allSubj_final,sheet="Sheet1",startRow = 1,startCol=2,header=T)
XLConnect::saveWorkbook(alignToStndz,"align-to-all-subject-standards.xlsx")
message("  @File 'align-to-all-subject-standards.xlsx' Cleared and updated")



# Make GP-specific version ------------------------------------------------
grades_to_include<-c(5:12,"K,1,2,3,4,5,6,7,8,9,10,11,12")
good_rows <- sapply(allSubj_final$grade, function(gr) {
  test1<-gr %in% grades_to_include 
  vectorized_gr<-ifelse(grepl(",",gr,fixed = T), as.numeric(unlist(strsplit(gr,"[,]"))),as.numeric(gr))
  test2<-vectorized_gr %in% grades_to_include
  if (test1| test2) {
    TRUE
  } else{
    FALSE
  }
})
#Test it
data.frame(grade=allSubj_final$grade,included=ifelse(good_rows,"YES","no"))
gp_standards<-allSubj_final[good_rows,]
#write it
openxlsx::write.xlsx(gp_standards,"data/gp_standards.xlsx",overwrite=T)

