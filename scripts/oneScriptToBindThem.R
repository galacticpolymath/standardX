require(openxlsx);require(tidyverse);require(stringi)
c3<-read.csv("data/formatted_c3SocialStudies.csv")
ela<-read.csv("data/formatted_CommonCoreELA.csv")
math<-read.csv("data/formatted_CommonCoreMath.csv")
sci<-read.csv("data/formatted_ngss.csv",quote="\"")
sdg<-read.csv("data/SDG-targets.csv")
names(sdg)[c(2,3)]<-c("code","statement")
sdg$dimension=sapply(sdg$goal,function(x) switch(x,"1"="No Poverty","2"="Zero Hunger","3"="Good Health and Well-Being","4"="Quality Education","5"="Gender Equality","6"="Clean Water and Sanitation","7"="Affordable and Clean Energy","8"="Decent Work and Economic Growth","9"="Industry, Innovation, and Infrastructure","10"="Reduced Inequalities","11"="Sustainable Cities and Communities","12"="Responsible Consumption and Production","13"="Climate Action","14"="Life Below Water","15"="Life on Land","16"="Peace, Justice, and Strong Institutions","17"="Partnerships for the Goals"))
sdg$dim<-gsub("[a-z]| |-|,","",sdg$dimension)

c3$subject<-"Social Studies"
c3$framework<-"C3"
ela$subject<-"ELA"
ela$framework<-"Common Core ELA"
math$subject <- "Math"
math$framework<-"Common Core Math"
sci$subject <- "Science"
sci$framework<-"NGSS"
sdg$framework <- "SDGs"
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


steam<-full_join(c3,ela) %>% full_join(math) %>% full_join(sci) %>% full_join(sdg)%>% select(subject,framework,grade,dimension, dim,subcategory,everything())
#convert  #N/A to NA
steam<-apply(steam,c(1,2),function(x) ifelse(x=="#N/A",NA,x))
head(steam)
tail(steam)
#check
sum(sapply(list(c3,ela,math,sci,sdg),nrow))
nrow(steam)
write.csv(steam,"data/formatted_allSubjects.csv",row.names=F)

#Standards alignment template
steam2<-steam %>% as_tibble()%>%rename(set=framework) %>%  mutate(ALIGN=NA,FormativeQ=NA,A_Level1=NA,A_Level2=NA,A_Level3=NA,A_Level4=NA) %>% 
  select(ALIGN,code,set,dim,grade,statement,FormativeQ,starts_with("A_"),everything())
write.xlsx(steam2,"data/alignToAll.xlsx",row.names=F,freezePane=list(firstActiveRow=2,firstActiveCol=6))
