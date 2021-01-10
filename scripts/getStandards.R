#This code downloads and formats live common core ela and math standards & UN Sustainable Development Goal Standards

#There is a separate C3 standard script format c3.R
#NGSS were acquired from Ted Willard: TWillard@discoveryed.com
# format ngss.R reformats Ted's file into the way that we need it


#Install/load pacman
if(!require(pacman)){install.packages("pacman");require(pacman)}
#Install/load tons of packages
p_load(tidyverse,xml2,plyr,fs,XLConnect)

###############
# Commmon Core
#############
download.file("http://www.corestandards.org/wp-content/uploads/ccssi.zip",destfile=paste0(tempdir(),"/ccssi.zip"))
#updated common core ELA and math standards now in tempdir as XML
unzip(paste0(tempdir(),"/ccssi.zip"),exdir=tempdir())
file_copy(paste0(tempdir(),"/ccssi/xml/math.xml"),"data/math.xml")
file_copy(paste0(tempdir(),"/ccssi/xml/ela-literacy.xml"),"data/ela.xml")
ccMath.xml<-read_xml("data/math.xml")
ccELA.xml<-read_xml("data/ela.xml")

# get all the <record>s


ccMath0<-data.frame(
            GradeJumble=xml_find_all(ccMath.xml,".//LearningStandardItem//GradeLevel")%>%  xml_parent()%>% xml_text(),
            code=xml_find_all(ccMath.xml,".//StatementCode")%>% xml_text() %>% gsub(pattern="CCSS.Math.Content.",replacement="",fixed=T),
            statement=xml_find_all(ccMath.xml,".//Statement")%>% xml_text(),
            link=xml_find_all(ccMath.xml,".//RefURI")%>% xml_text(),
            stringsAsFactors = F
            )
#bad strings for multi-grade standards
badstrings<-c("^K010203040506070809101112$","^0910$","^1112$","060708$","^09101112")
goodstrings<-c("K,1,2,3,4,5,6,7,8,9,10,11,12","9,10","11,12","6,7,8","9,10,11,12")
ccMath0$grade=ccMath0$GradeJumble
for (i in 1:length(badstrings))
{
  ccMath0$grade<-gsub(badstrings[i],goodstrings[i],ccMath0$grade)
}

#Annoyingly the grade comes 1st in the math CC standards, and nowhere else. Gotta switch it
ccMath0$code2<-sapply(ccMath0$code,function(x) {
        if(length(grep("^[[:alnum:]]\\.",x))==1){
            str<-stri_split_fixed(x,".",n=2,simplify=T)
            paste0(str[2],"_",str[1])}else{
              if(length(grep("^HS",x))==1){
            str<-stri_split_fixed(x,"-",n=2,simplify=T)
            paste0(str[2],"_",str[1])}else{gsub("CCSS.Math.Practice.","",x,fixed=T)}
              }
            }) %>% gsub("_.*$","",x=.)  #optionally remove _Grade in last step
#Extract dimension abbrevs from code
ccMath0$dim0<-stri_extract_first_regex(ccMath0$code2,"^[A-Z]*?(?=\\.)|^MP")

unique(ccMath0$dim0)
#holy heck! there are 33 dimensions of the math standards!

#make longer names
ccMath0$dimension0<-sapply(ccMath0$dim0,function(x) {
            switch(x,"CC"="Counting & Cardinality","G"="Geometry","MD"="Measurement & Data","NBT"=
                     "Numbers & Operations Base 10","OA"="Operations & Algebraic Thinking","NF"="Number & Operations_Fractions","EE"="Expressions & Equations","NS"="Number System","RP"="Ratios & Proportions","SP"="Statistics & Probability","F"="Functions","APR"="Arithmetic with Polynomials & Rational Expressions","CED"="Creating Equations","REI"="Reasoning with Equations & Inequalities","SSE"="Seeing Structure in Expressions","BF"="Building Functions","IF"="Interpreting Functions","LE"="Linear, Quadratic & Exponential Models","TF"="Trig Functions","C"="Circles","CO"="Congruence","GMD"="Geometric Measurement & Dimension","GPE"="Geometric Properties with Equations","MG"="Modeling with Geometry","SRT"="Similarity, Right Triangles & Trig","CN"="Complex Numbers","Q"="Quantities","RN"="Real Numbers","VM"="Vector & Matrix Quantities","CP"="Conditional Probability","IC"="Inferences & Justifying Conclusions","ID"="Interpreting Categorical & Quantitative Data","MP"="Math Practice Anchor Standards")
          })

#Now need to collapse these 33 dimensions into 3!!
# Read in conversion table
mathRosetta<-read.csv("data/wkshtCollapsingMathStandards.csv")
ccMath0$dimension<-sapply(ccMath0$dimension0,function(x) {
                    row<-match(x,mathRosetta$dimension0)
                    mathRosetta$dimension[row]
                    })

ccMath<-ccMath0 %>% mutate(dim=gsub("[a-z |,]","",dimension))%>% mutate(orig_dimension=dimension0,orig_dim=dim0) %>%  select("grade","dimension","dim","code","statement","link","orig_dimension","orig_dim","code2") 

write.csv(ccMath,"./data/formatted_CommonCoreMath.csv",row.names = F)


######
### English
ccELA<-data.frame(
            GradeJumble=xml_find_all(ccELA.xml,".//LearningStandardItem//GradeLevel")%>%  xml_parent()%>% xml_text(),
            Standard=xml_find_all(ccELA.xml,".//StatementCode")%>% xml_text(),
            Statement=xml_find_all(ccELA.xml,".//Statement")%>% xml_text(),
            Link=xml_find_all(ccELA.xml,".//RefURI")%>% xml_text()
            )
ccELA$Grade=ccELA$GradeJumble
for (i in 1:length(badstrings))
{
  ccELA$Grade<-gsub(badstrings[i],goodstrings[i],ccELA$Grade)
}

ccELA<-ccELA %>% mutate(grade=Grade,code=gsub("CCSS.ELA-Literacy.","",Standard,fixed=T),statement=Statement,link=Link) 


# First, need to add anchor standards back into their respective subcatagories

#Need to create a new vector where CCRA comes after subject, so they sort into their proper subcategories
ccELA$code2 <- sapply(ccELA$code,function(x) {
              if(length(grep("CCRA",x,fixed=T))==0){x}else{paste0(gsub("CCRA.","",x,fixed=T),".CCRA")}},USE.NAMES = F)


ccELA<-ccELA %>% mutate(dim0=gsub("\\..*$","",code2))%>% filter(dim0!="")

unique(ccELA$dim0)
#Add longer text for acronyms
ccELA$dimension0<-sapply(ccELA$dim0, function(x) ifelse(x=="",NA, switch(x,"L"="Language","R"="Reading", "RF"="Reading: Foundational Skills","RH"="Reading: History/Social Studies","RI"="Reading: Informational Text","RL"="Reading: Literature","RST"="Reading: Science & Technical Subjects","SL"="Speaking & Listening","W"="Writing","WHST"="Writing in History, Science & Technical Subjects"))) %>% unlist() 

unique(ccELA$dimension0)
# Collapse the 10 dimensions into 3: Reading, Writing, & Language, Speaking & Listening


ccELA$dimension=case_when(
  ccELA$dim0%in% c("R", "RF","RH","RI","RL","RST")~"Reading",
  ccELA$dim0%in% c("L","SL") ~ "Language, Speaking & Listening",
  ccELA$dim0%in% c("W","WHST") ~"Writing"
  )

ccELA.out <- ccELA %>% mutate(dim=gsub("[a-z |,]","",dimension)) %>%filter(dim0!="CCRA")%>% mutate(orig_dimension=dimension0,orig_dim=dim0) %>%  select("grade","dimension","dim","code","statement","link","orig_dimension","orig_dim") 

write.csv(ccELA.out,"./data/formatted_CommonCoreELA.csv",row.names = F)

###############
# NGSS
#############
#This doesn't work, because the XML is poorly formatted!

# download.file("http://asn.jesandco.org/resources/D2454348_full.xml",destfile="data/NGSS.xml")
# ngss.xml<-read_xml("data/NGSS.xml")
# xml_structure(ngss.xml)
# 
# ngss<-data.frame(
#             Level=xml_find_all(ngss.xml,".//asn:Statement/dcterms:educationLevel/@rdf:resource") %>% xml_text() %>% gsub(pattern=".*Level\\/(.*)$",replacement="\\1",fixed=F),
#             
#             Standard=xml_find_all(ngss.xml,"//asn:Statement")%>% xml_text(), #/asn:statementNotation
#             Statement=xml_find_all(ngss.xml,"asn:Statement/dcterms:description")%>% xml_text(), 
#             StatementLabel=xml_find_all(ngss.xml,"asn:Statement/asn:statementLabel")%>% xml_text(),
#             ClarifyingStatement=xml_find_all(ngss.xml,".//asn:comment")%>% xml_text(),
#             Description=xml_find_all(ngss.xml,"./asn:Statement/dcterms:description")%>% xml_text(),
#             
#     x=xml_find_all(ngss.xml,".//asn:Statement rdf:about")%>% xml_text(),
#             stringsAsFactors = F
#             )
# standards=
# 
#   
# length(xml_path(xml_find_all(ngss.xml,"//asn:Statement")))
#   
# xml_children(ngss.xml,".//asn:Statement/")
# #Extract child node info from each statement entry
# ngss<-data.frame(level=NA,standard=NA,disciplinaryCoreIdea=NA,performanceExpectation=NA,clarifyingStatement=NA)
# for(i in 1:length(xml_find_all(ngss.xml,"//asn:Statement"))){
#   entry_i<-paste0("/rdf:RDF/asn:Statement[",i,"]")
#   ngss$Level[i]=xml_find_all(ngss.xml,paste0(entry_i,"/dcterms:educationLevel/@rdf:resource")) %>% xml_text() %>% gsub(pattern=".*Level\\/(.*)$",replacement="\\1",fixed=F) %>% paste(sep=",")
#   ngss$standard[i]=
#   description=
#   descriptionType=xml_find_all(ngss.xml,"asn:Statement/asn:statementLabel")%>% xml_text()
#   ngss$disciplinaryCoreIdea[i]=if(descriptionType=="Disciplinary Core Idea"){description}else{NA}
#   ngss$performanceExpectation[i]=if(descriptionType=="Disciplinary Core Idea"){description}else{NA}
#   ngss$clarifyingStatement[i]=
#     
# }
# 


##############
# Sustainable Development Goals
#############
download.file("https://raw.githubusercontent.com/datapopalliance/SDGs/master/SDG-targets.csv",destfile = "data/SDG-targets.csv")
download.file("https://raw.githubusercontent.com/datapopalliance/SDGs/master/SDG-indicators_proposed-2016-03-24.csv",destfile = "data/SDG-indicators_proposed-2016-03-24.csv")

sdg<-read.csv("data/SDG-targets.csv")
sdg_indicators<-read.csv("data/SDG-indicators_proposed-2016-03-24.csv")

# #combine standards in one XLSX file
# fileOut<-"data/STEAMstandards.xlsx"
# xlsx<-loadWorkbook(fileOut,create=T)
# standardsL<-list(ccELA,ccMath,sdg)
# lapply(standardsL,function(x),
#        {createSheet(xlsx,name=basename(tools::file_path_sans_ext(fileOut)))})
# combStandards<-do.call(rbind.data.frame,list(ccELA,ccMath,sdg))


