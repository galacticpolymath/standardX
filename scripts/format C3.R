require(stringi);require(tidyverse)
c3.0<-read.csv("data/c3_rosetta.csv")


tmp.k2<-sapply(c3.0$K.2,function(x) {stri_split_regex(x,"\\. ",fixed=F) }) #%>% as.list() 
names(tmp.k2) <- NULL

code.k2<-lapply(tmp.k2,function(x) x[1]) %>% unlist()
statement.k2<-lapply(tmp.k2,function(x) x[2]) %>% unlist()

tmp.G3.5<-sapply(c3.0$G3.5,function(x) {stri_split_regex(x,"\\. ",fixed=F) })
code.G3.5<-lapply(tmp.G3.5,function(x) x[1]) %>% unlist()
statement.G3.5<-lapply(tmp.G3.5,function(x) x[2]) %>% unlist()

tmp.G6.8<-sapply(c3.0$G6.8,function(x) {stri_split_regex(x,"\\. ",fixed=F) })
code.G6.8<-lapply(tmp.G6.8,function(x) x[1]) %>% unlist()
statement.G6.8<-lapply(tmp.G6.8,function(x) x[2]) %>% unlist()

tmp.G9.12<-sapply(c3.0$G9.12,function(x) {stri_split_regex(x,"\\. ",fixed=F) })
code.G9.12<-lapply(tmp.G9.12,function(x) x[1]) %>% unlist()
statement.G9.12<-lapply(tmp.G9.12,function(x) x[2]) %>% unlist()

code=c(code.k2,code.G3.5,code.G6.8,code.G9.12)
statement=c(statement.k2,statement.G3.5,statement.G6.8,statement.G9.12)
nr<-nrow(c3.0)
grade<-c(rep("K,1,2",nr),rep("3,4,5",nr),rep("6,7,8",nr),rep("9,10,11,12",nr))


statement <- sapply(statement,function(x) gsub("- ","",x,fixed=T))

c3.0<-tibble(grade=grade,dimension0=rep(c3.0$Dimension,4),code=code,statement=statement) %>% mutate(dim0=gsub("[a-z ]","", dimension0)) %>% filter(!is.na(statement),code!="") %>% arrange(code) %>% select(1:2,last_col(),everything())

#Save space changing and to &
c3.0$dimension0<-sapply(c3.0$dimension0,function(x) gsub(" and ", " & ",x))

#collapse to 3 dimensions
c3.0$dim<-sapply(c3.0$dim0,function(x) switch(x,"DQPI"="DQPI","ADCT"="CEGH","ESUE"="ESCCTA","CCTIA"="ESCCTA"))

c3.0$dimension<-sapply(c3.0$dim,function(x) switch(x,"DQPI"="Developing Questions & Planning Inquiries","CEGH"="Civics, Economics, Geography & History","ESCCTA"="Evaluating Sources, Communicating Conclusions & Taking Action"))

c3.out<-c3.0%>% mutate(orig_dimension=dimension0,orig_dim=dim0) %>% select("grade","dimension","dim","code","statement","orig_dimension","orig_dim") 

write.csv(c3.out,"data/formatted_c3SocialStudies.csv",row.names=F)
