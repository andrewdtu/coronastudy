library(tidyverse)
library(lubridate)
library(ggplot2)

cases.raw<-read.csv("daily-cases-covid-19.csv") 
cases = cases.raw %>%
  mutate(confirmed = cases.raw$Daily.confirmed.cases..cases.)%>%
  mutate(date = mdy(Date))%>%
  filter(date > "2020-02-01")%>%
  #filter(date < "2020-04-05")%>%
  select(date, confirmed, Code)%>%
  mutate(Code = as.character(Code))


tests.raw <- read.csv("full-list-covid-19-tests-per-day.csv")
tests = tests.raw %>%
  mutate(tested = tests.raw$Daily.change.in.total.tests )%>%
  mutate(date = mdy(Date))%>%
  filter(date > "2020-02-01")%>%
  #filter(date < "2020-04-05")%>%
  select(date, tested, Code) %>%
  mutate(Code = as.character(Code))


data = full_join(cases,tests)
#USA
data.usa = data %>%
  filter(Code == "USA") %>%
  select(date, confirmed, tested)%>%
  arrange(date) %>%
  mutate(cumulconfirmed = cumsum(confirmed))%>%
  mutate(growth = (confirmed/cumulconfirmed)*100)%>%
  mutate(cases.per.test = confirmed/tested) %>%
  filter(growth < 100)


par(mfrow=c(2,2))

plot(data.usa$date, log(data.usa$confirmed/data.usa$tested),pch=3,xlab = "Date"
     ,ylab = "log of cases per test in the US", main = "USA")

##Italy

data.ita = data %>%
  filter(Code == "ITA") %>%
  select(date, confirmed, tested)%>%
  arrange(date) %>%
  mutate(cumulconfirmed = cumsum(confirmed))%>%
  mutate(growth = (confirmed/cumulconfirmed)*100)%>%
  mutate(cases.per.test = confirmed/tested) %>%
  filter(growth < 100)

plot(data.ita$date, log(data.ita$confirmed/data.ita$tested),pch=3,xlab = "Date"
     ,ylab = "log of cases per test in Italy", main = "Italy")

## south korea
data.kor = data %>%
  filter(Code == "KOR") %>%
  select(date, confirmed, tested)%>%
  arrange(date) %>%
  mutate(cumulconfirmed = cumsum(confirmed))%>%
  mutate(growth = (confirmed/cumulconfirmed)*100)%>%
  mutate(cases.per.test = confirmed/tested) %>%
  filter(growth < 100)



plot(data.kor$date, log(data.kor$confirmed/data.kor$tested),pch=3,xlab = "Date"
     ,ylab = "log of cases per test in South Korea", main = "South Korea")
# uk
data.gbr = data %>%
  filter(Code == "GBR") %>%
  select(date, confirmed, tested)


plot(data.gbr$date, log(data.gbr$confirmed/data.gbr$tested),pch=3,xlab = "Date"
     ,ylab = "log of cases per test in the UK", main = "United Kingdom")

# israel
data.isr = data %>%
  filter(Code == "ISR") %>%
  select(date, confirmed, tested)



plot(data.isr$date, log(data.isr$confirmed/data.isr$tested),pch=3,xlab = "Date"
     ,ylab = "log of cases per test in Israel", main = "Israel")

#turkey
data.tur = data %>%
  filter(Code == "TUR") %>%
  select(date, confirmed, tested)


plot(data.tur$date, log(data.tur$confirmed/data.tur$tested),pch=3,xlab = "Date"
     ,ylab = "log of cases per test in Turkey", main = "Turkey")

#india
data.ind = data %>%
  filter(Code == "IND") %>%
  select(date, confirmed, tested)


plot(data.ind$date, log(data.ind$confirmed/data.ind$tested),pch=3,xlab = "Date"
     ,ylab = "log of cases per test in India", main = "India")
     
     

# south africa
data.zaf = data %>%
filter(Code == "ZAF") %>%
  select(date, confirmed, tested)


plot(data.zaf$date, log(data.zaf$confirmed/data.zaf$tested),pch=3, xlab = "Date",
     ylab = "log of cases per test in South Africa", main = "South Africa")





ggplot(data.kor, mapping = aes(log(cases.per.test), log(growth)))+
  geom_point()+
  geom_smooth(method = lm)


ggplot(data.ita, mapping = aes((cases.per.test), (growth)))+
  geom_point()+
  geom_smooth(method = lm)




