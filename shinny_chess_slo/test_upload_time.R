data_time_stamps <- read.csv(file = "./data/round1.csv", header = TRUE, sep = ";")

x <- 2
time_stamps <- matrix(unlist(strsplit(as.character(data_time_stamps$time[2]), ";")))
time_stamps <- as.POSIXct(time_stamps, origin="1970-01-01", format="%H:%M:%S", tz = "Europe/Prague")

time_stamps[2]
for (i in 1:length(time_stamps)){
  # time_stamps[i] <- as.POSIXct(time_stamps[i],format="%H:%M:%S")
  day(time_stamps[i]) <- 29
  month(time_stamps[i]) <- 10
  year(time_stamp[i]) <- 2019
}
