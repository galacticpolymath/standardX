#Install/load pacman
if(!require(pacman)){install.packages("pacman");require(pacman)}
#Install/load tons of packages
p_load(tidyverse,xml2,plyr,fs)

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


ccMath<-data.frame(
            GradeJumble=xml_find_all(ccMath.xml,".//LearningStandardItem//GradeLevel")%>%  xml_parent()%>% xml_text(),
            Standard=xml_find_all(ccMath.xml,".//StatementCode")%>% xml_text(),
            Statement=xml_find_all(ccMath.xml,".//Statement")%>% xml_text(),
            Link=xml_find_all(ccMath.xml,".//RefURI")%>% xml_text(),
            stringsAsFactors = F
            )
#bad strings for multi-grade standards
badstrings<-c("^K010203040506070809101112$","^0910$","^1112$","060708$","^09101112")
goodstrings<-c("K,1,2,3,4,5,6,7,8,9,10,11,12","9,10","11,12","6,7,8","9,10,11,12")
ccMath$Grade=ccMath$GradeJumble
for (i in 1:length(badstrings))
{
  ccMath$Grade<-gsub(badstrings[i],goodstrings[i],ccMath$Grade)
}
write.csv(ccMath[,c("Grade","Standard","Statement","Link")],"./data/CommonCoreMath.csv")

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

write.csv(ccELA[,c("Grade","Standard","Statement","Link")],"./data/CommonCoreELA.csv")

###############
# NGSS
#############


##############
# Sustainable Development Goals
#############
download.file("https://raw.githubusercontent.com/datapopalliance/SDGs/master/SDG-targets.csv",destfile = "data/SDG-targets.csv")
download.file("https://raw.githubusercontent.com/datapopalliance/SDGs/master/SDG-indicators_proposed-2016-03-24.csv",destfile = "data/SDG-indicators_proposed-2016-03-24.csv")

