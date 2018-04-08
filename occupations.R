library(readxl)
download.file("https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/employmentbyoccupationemp04/apriltojune2017/emp04aug2017.xls","emp04aug2017.xls")
download.file("https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/employmentbyoccupationemp04/apriljune2001/ukallinemploybysocapr-jun2001_tcm77-60429.xls","ukallinemploybysocapr-jun2001_tcm77-60429.xls")
read2001 = function(gender) {
  data=readxl::read_xls("ukallinemploybysocapr-jun2001_tcm77-60429.xls",sheet = gender)
  data$gender=gender
  data$year=2001
  names(data)=c("occupation_text","total","unused0","emp.full","emp.part","emp.total","unused","self.full","self.part","self.total")
  data$occupation=gsub("^([0-9]+).*","\\1",data$occupation_text)
  data[grep("^[0-9]+", data$occupation),]
}
read2017 = function(gender) {
  data=readxl::read_xls("emp04aug2017.xls",sheet = gender)
  data$gender=gender
  data$year=2017
  names(data)=c("occupation_text","total","emp.full","emp.part","emp.total","unused","self.full","self.part","self.total")
  data$occupation=gsub("^([0-9]+).*","\\1",data$occupation_text)
  data[grep("^[0-9]+", data$occupation),]
}

data2001=merge(read2001("male"),read2001("female"),by="occupation",suffixes = c(".male",".female"))
data2001$female_ratio=as.numeric(data2001$total.female)/(as.numeric(data2001$total.male)+as.numeric(data2001$total.female))
data2017=merge(read2017("Men"),read2017("Women"),by="occupation",suffixes = c(".male",".female"))
data2017$female_ratio=as.numeric(data2017$total.female)/(as.numeric(data2017$total.male)+as.numeric(data2017$total.female))
data=merge(data2001,data2017,by=c("occupation"),suffixes=c(".2001",".2017"))
data$y01_17_female_change=1/(data$female_ratio.2001/data$female_ratio.2017)
data$y01_17_change=1/((as.numeric(data$total.male.2001)+as.numeric(data$total.female.2001))/(as.numeric(data$total.male.2017)+as.numeric(data$total.female.2017)))
data$occupation_text=data$occupation_text.male.2017
View(data[!is.na(data$y01_17_female_change),grep("occupation$|occupation_text$|ratio|change|^total",names(data))])
write.csv(data[!is.na(data$y01_17_female_change),grep("occupation$|occupation_text$|ratio|change|^total",names(data))],"result.csv")
data=data[!is.na(data$y01_17_female_change),]
setwd("~/Documents/github/ons_occupation_gender/")
download.file("https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/grossweeklyearningsbyoccupationearn06/current/earn06feb2018.xls","earn06feb2018.xls")

read_weekly_salary=function(gender) {
  data=readxl::read_xls("earn06feb2018.xls",sheet = paste0(gender," - Weekly Earnings"))
  occupations=as.character(data[5,2:ncol(data)])
  x=data.frame(data[36:nrow(data),])
  names(x)=c("date",occupations)
  x=x[grep("^[a-zA-Z]{3}-",x$date),]
  x$date=as.Date(strptime(format="%d %b %Y", gsub(".*?-(.*)","01 \\1", x$date)))
  x[,occupations]=sapply(x[,occupations],as.numeric)
  x
}
male=read_weekly_salary("Men")
female=read_weekly_salary("Women")
ratio=male[,2:ncol(male)] / female[,2:ncol(female)]
ratio$date = male[,"date"]
View(ratio)
ggplot(melt(ratio[,grep("date|profession|manager|All",names(ratio),ignore.case = T)],"date"),aes(x=date,y=value,colour=variable))+geom_point()+stat_smooth()+ggtitle("Male/female weekly earnings by occupation")
ggplot(melt(ratio[,grep("profession|manager|All",names(ratio),invert = T, ignore.case = T)],"date"),aes(x=date,y=value,colour=variable))+geom_point()+stat_smooth()+ggtitle("Male/female weekly earnings by occupation")
