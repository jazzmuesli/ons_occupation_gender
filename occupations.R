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
write.csv(data,"result.csv")