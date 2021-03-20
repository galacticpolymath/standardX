require(openxlsx);require(tidyverse);require(stringi);require(stringr)
c3<-read.csv("data/formatted_c3SocialStudies.csv")
ela<-read.csv("data/formatted_CommonCoreELA.csv")
math<-read.csv("data/formatted_CommonCoreMath.csv")
sci<-read.csv("data/formatted_ngss.csv",quote="\"")
sdg<-read.csv("data/SDG-targets.csv")
names(sdg)[c(2,3)]<-c("code","statement")
sdg$subcat=sapply(sdg$goal,function(x) switch(x,"1"="No Poverty","2"="Zero Hunger","3"="Good Health and Well-Being","4"="Quality Education","5"="Gender Equality","6"="Clean Water and Sanitation","7"="Affordable and Clean Energy","8"="Decent Work and Economic Growth","9"="Industry, Innovation, and Infrastructure","10"="Reduced Inequalities","11"="Sustainable Cities and Communities","12"="Responsible Consumption and Production","13"="Climate Action","14"="Life Below Water","15"="Life on Land","16"="Peace, Justice, and Strong Institutions","17"="Partnerships for the Goals"))
# sdg$dim<-gsub("[a-z]| |-|,","",sdg$subcat)

c3$subject<-"Social Studies"
c3$set<-"C3"
ela$subject<-"ELA"
ela$set<-"Common Core ELA"
math$subject <- "Math"
math$set<-"Common Core Math"
sci$subject <- "Science"
sci$set<-"NGSS"
sdg$set <- "SDGs"
sdg$subject<-NA

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
allSubj$statement[1385]
allSubj$statement <-allSubj$statement %>% stringr::str_replace_all("\\.<sup>.</sup>","\\.")
allSubj$statement[1385] #it worked


#Still remaining problem of exponents that we actually want to keep

#example:
allSubj$statement[1500]
allSubj$statement <-allSubj$statement %>% stringr::str_replace_all("<sup>","^")
allSubj$statement <-allSubj$statement %>% stringr::str_replace_all("</sup>","")
allSubj$statement[1500] #successfully changed "s<sup>2</sup>" to "s^2"

#Remaining problem--all other HTML tags...
#start with italics, which we can change to markdown
allSubj$statement[1587] # i and em tags need change to *text*
allSubj$statement<-allSubj$statement %>% stringr::str_replace_all(pattern="<[\\/?i|\\/?em]*>","*")

#check if it worked:
#show example:
allSubj$statement[1587]
#it did; but still leaving in special character codes like SQRT, i.e. "&radic;"

#Are there other HTML tags? Remove dem
allSubj$statement<-allSubj$statement %>% stringr::str_remove_all(pattern="<[^>]*>")



head(allSubj)
tail(allSubj)
#check
sum(sapply(list(c3,ela,math,sci,sdg),nrow))
nrow(allSubj)
write.csv(allSubj,"data/allStandards.csv",row.names=F)

# Make Excel version 
write.xlsx(allSubj,"data/allStandards.xlsx",row.names=F,freezePane=list(firstActiveRow=2,firstActiveCol=6))

