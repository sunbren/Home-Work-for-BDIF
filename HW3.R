Homework3 
Question1.
#install.packages("digest",repos='http://cran.us.r-project.org')
library(digest)
digest("I learn a lot from this class when I am proper listening to the professor","sha256")

Question3
library(readr)
data <- read_csv("~/R/Home-Work-for-BDIF/data.csv")
json_ratedata <- toJSON(data,method = "C")

Question4
#install.packages("rjson", repos="http://cran.us.r-project.org")
library(rjson)
json_file = "http://crix.hu-berlin.de/data/crix.json"
json_data = fromJSON(file=json_file)
crix <- Reduce(rbind,json_data)
crix_data_frame <- as.data.frame(crix)
lst <- lapply(json_data,function(x){
  df<-data.frame(date=x$date,price=x$price)
  return(df)
})
crix_data_frame <- Reduce(rbind,lst)
plot(crix_data_frame$date,crix_data_frame$price)
#library(forecast)
#library(tseries)
plot(crix_data_frame)
Acf(crix_data_frame$price)
#Find the data is highly autocorrelated.
ndiffs(crix_data_frame$price)
dcrix <- diff(crix_data_frame$price, lag = 2)
plot(dcrix)
#After lagging 2 periods, there is still autocorrelated.