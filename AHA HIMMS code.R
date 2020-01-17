library(car)
library(plyr)
library(dplyr)
library(ROSE)
library(readxl)
library(corrplot)


#loading data

aha_data<-read.csv("E:/Healthcare project/AHA HIMMS Project/Merged Data_new.csv", header = TRUE,sep = ",")
aha_overall<-read.csv("E:/Healthcare project/AHA HIMMS Project/OVERALLDATAOUTPUT.", header = TRUE,sep = ",")
himms_data<-read.csv("E:/Healthcare project/AHA HIMMS Project/HIMMS data.csv", header = TRUE,sep = ",")

View(aha_data)

#Exploratory data analysis

min(aha_data$Total.hospital.beds)
max(aha_data$Total.hospital.beds)

plot(aha_data$Total.hospital.beds)

aha_data$Telemedicine <- ifelse(aha_data$Telemedicine =="Yes", 1, 0)
table(aha_data$Telemedicine)

table(aha_data$Health.System.Type)
View(aha_data$Health.System.Type)

table(aha_data$Organization.Primary.Service)
View(aha_data$Organization.Primary.Service)

table(aha_data$Is.the.hospital.a.participant.in.a.network)

table(aha_data$Mobile.Health.Services...hospital)

table(aha_data$Patient.education.center...hospital)

table(aha_data$Patient.representative.services...hospital)

plot(aha_data$Total.hospital.beds)
summary(aha_data$Total.hospital.beds)

aha_new<-aha_data[!rowSums(is.na(aha_data[aha_data$Number.of.Operating.Rooms])), ]
is.na(aha_data$Number.of.Operating.Rooms)

plot(aha_data$Telemedicine)
str(aha_data$Telemedicine)

View(aha_data)
aha_data$Hospital.Size<-round(aha_data$Hospital.Size)
table(aha_data$Hospital.Size)

#Bivariate analysis
#Chisquare Test

chisq.test(aha_data$Telemedicine,aha_data$Is.the.hospital.a.participant.in.a.network)

chisq.test(aha_data$Telemedicine,aha_data$CBSA.Type)

chisq.test(aha_data$Telemedicine,aha_data$Assistive.technology.center...hospital)

chisq.test(aha_data$Telemedicine,aha_data$Mobile.Health.Services...hospital)

#correlation plot


my_num_data <- aha_comp[, sapply(aha_comp, is.numeric)]
res<-cor(my_num_data, use = "complete.obs")
View(res)
corrplot(res)


#Hospitals with Telemedicine across all years
attach(aha_data)
aha_new<-select(aha_data, Is.the.hospital.a.participant.in.a.network,Hospital.Size,Admissions...As.Hosp.,
                Average.daily.census,Emergency.room.visits,Total.outpatient.visits,CBSA.Type,
                Mobile.Health.Services...hospital,Assistive.technology.center...hospital,Telemedicine)


aha_comp<-aha_new[complete.cases(aha_new), ]
table(aha_comp$Telemedicine)
str(aha_comp)

aha_comp$Telemedicine<-as.factor(aha_comp$Telemedicine)
aha_comp$Is.the.hospital.a.participant.in.a.network<-as.factor(aha_comp$Is.the.hospital.a.participant.in.a.network)
aha_comp$Mobile.Health.Services...hospital<-as.factor(aha_comp$Mobile.Health.Services...hospital)
aha_comp$Assistive.technology.center...hospital<-as.factor(aha_comp$Assistive.technology.center...hospital)


#Model
attach(aha_comp)
Model_aha<- glm(Telemedicine~.,data = aha_comp,family = binomial)
summary(Model_aha)
exp(cbind("Odds ratio" = coef(Model_aha), confint.default(Model_aha, level = 0.95)))

predict_result <- predict(Model_aha,aha_comp,type = 'response')
fitted.results <- ifelse(predict_result > 0.5,1,0)
table(fitted.results)
c(fitted.results)

#Metro Hospitals

aha_metro <- subset(aha_data, CBSA.Type == 'Metro')
aha_metro$Telemedicine <- ifelse(aha_metro$Telemedicine =="Yes", 1, 0)
table(aha_metro$Telemedicine)

attach(aha_metro)
aha_metro1<-select(aha_metro, Is.the.hospital.a.participant.in.a.network,Hospital.Size,Admissions...As.Hosp.,
                Average.daily.census,Emergency.room.visits,Total.outpatient.visits,
                Mobile.Health.Services...hospital,Assistive.technology.center...hospital,Telemedicine)


aha_c<-aha_metro1[complete.cases(aha_metro1), ]
View(aha_c)
table(aha_c$Telemedicine)

aha_c$Telemedicine<-as.factor(aha_c$Telemedicine)
aha_c$Is.the.hospital.a.participant.in.a.network<-as.factor(aha_c$Is.the.hospital.a.participant.in.a.network)
aha_c$Mobile.Health.Services...hospital<-as.factor(aha_c$Mobile.Health.Services...hospital)
aha_c$Assistive.technology.center...hospital<-as.factor(aha_c$Assistive.technology.center...hospital)

str(aha_c)

#Model
attach(aha_c)
Model_metro<- glm(Telemedicine~.,data = aha_c,family = binomial)
summary(Model_metro)
exp(cbind("Odds ratio" = coef(Model_metro), confint.default(Model_metro, level = 0.95)))


#Rural Hospitals

aha_rural <- subset(aha_data, CBSA.Type == 'Rural')
aha_rural$Telemedicine <- ifelse(aha_rural$Telemedicine =="Yes", 1, 0)
table(aha_rural$Telemedicine)

attach(aha_rural)
aha_rural1<-select(aha_rural, Is.the.hospital.a.participant.in.a.network,Hospital.Size,Admissions...As.Hosp.,
                   Average.daily.census,Emergency.room.visits,Total.outpatient.visits,
                   Mobile.Health.Services...hospital,Assistive.technology.center...hospital,Telemedicine)

str(aha_rural1)
aha_r<-aha_rural1[complete.cases(aha_rural1), ]
table(aha_r$Telemedicine)

aha_r$Telemedicine<-as.factor(aha_r$Telemedicine)
aha_r$Is.the.hospital.a.participant.in.a.network<-as.factor(aha_r$Is.the.hospital.a.participant.in.a.network)
aha_r$Mobile.Health.Services...hospital<-as.factor(aha_r$Mobile.Health.Services...hospital)
aha_r$Assistive.technology.center...hospital<-as.factor(aha_r$Assistive.technology.center...hospital)
str(aha_r)

#Model
attach(aha_r)
Model_rural<- glm(Telemedicine~.,data = aha_r,family = binomial)
summary(Model_rural)
exp(cbind("Odds ratio" = coef(Model_rural), confint.default(Model_rural, level = 0.95)))

#HIMMS data
attach(himms_data)
View(himms_data)
table(himms_data$Telemedicine)

himms_data1<-select(himms_data,Health.System.ID2,Organization.Primary.Service,Telemedicine)
himms_data2<-select(himms_data,ER.Visits,Outpatient.Visits,Net.Patient.Revenues,Telemedicine)

himms_c<-himms_data1[complete.cases(himms_data1), ]

str(himms_c)
himms_c$Telemedicine<-as.factor(himms_c$Telemedicine)
table(himms_c$Telemedicine)

Model_himms<- glm(Telemedicine~.,data = himms_c,family = binomial)
summary(Model_himms)
exp(cbind("Odds ratio" = coef(Model_himms), confint.default(Model_himms, level = 0.95)))


#Yearly Models

yearly_data<-read.csv("E:/Healthcare project/AHA HIMMS Project/Year data himms.csv", header = TRUE,sep = ",")
attach(yearly_data)
View(yearly_data)
yearly_data1<-select(yearly_data,Health.System.Type,Organization.Primary.Service,Beds..Licensed,Beds..Staffed,
                     ER.Visits,Outpatient.Visits,Net.Patient.Revenues,Year.2010,Year.2011,
                     Year.2012,Year.2013,Year.2014,Year.2015,Year.2015,Year.2016)

yearly_c<-yearly_data1[complete.cases(yearly_data1), ]
table(yearly_c$Year.2010)
table(yearly_c$Year.2011)
table(yearly_c$Year.2012)
table(yearly_c$Year.2013)
table(yearly_c$Year.2014)
table(yearly_c$Year.2015)
table(yearly_c$Year.2016)

Model_yearly<- glm(Year.2013~ Health.System.Type+Organization.Primary.Service+Beds..Licensed+
                  Beds..Staffed+ER.Visits+Outpatient.Visits+Net.Patient.Revenues,data = yearly_c,family = binomial)
summary(Model_yearly)
exp(coef(Model_yearly))

#Yrly data

yr_2017<-read_excel("E:/Healthcare project/AHA HIMMS Project/Tele 2017.xlsx")

yr_2017_c<-yr_2017[complete.cases(yr_2017), ]

table(yr_2017$TELEMED)
View(yr_2017)

str(yr_2017)

attach(yr_2017)

yr_2017$TELEMED<-as.factor(yr_2017$TELEMED)
yr_2017$`CBSA Type`<-as.factor(yr_2017$`CBSA Type`)
yr_2017$HIE<-as.factor(yr_2017$HIE)
yr_2017$`Patient Portal`<-as.factor(yr_2017$`Patient Portal`)
yr_2017$OrganizationPrimaryService<-as.factor(yr_2017$OrganizationPrimaryService)
yr_2017$HealthSystemID2<-as.factor(yr_2017$HealthSystemID2)

Model_yr<- glm(TELEMED ~ `Emergency room visits` + `Total outpatient visits`+`Average daily census`
               ,data = yr_2017,family = binomial)
summary(Model_yr)
exp(cbind("Odds ratio" = coef(Model_yr), confint.default(Model_yr, level = 0.95)))


#loading the data

ER_data<-read.csv("E:/Healthcare project/AHA HIMMS Project/ER 2014.csv", header = TRUE,sep = ",")

class(ER_data)

#Plotting and t test
View(ER_data)
plot(ER_data,type = "o", col = "red", xlab = "Year", ylab = "Outpatient Visits",
     main = "Performance Before and After 2015")


t.test(ER_data$Outpatient.y.1.,ER_data$Outpatient.y.1..1)

