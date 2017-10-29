
##### 시간대별 흐름 데이터 ######################
x = seq.POSIXt(as.POSIXct("2015-01-01"),   #, tz="GMT"), 영국시간일 경우 GMT
               as.POSIXct("2015-01-02"), by="10 min")
y = runif(length(x),min = 51, max = 100)
z = runif(length(x),min = 10, max = 50)

data_time = as.data.frame(as.xts(data.frame(y,z),x))
