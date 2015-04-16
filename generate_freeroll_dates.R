
freerolls <- data.table(roll_date = seq.Date(as.Date("2004-01-01"),
                                        as.Date("2009-12-31"),
                                        by="days"))


freerolls[, day_of_week := weekdays(roll_date)]
freerolls[, month := as.integer(format(roll_date, "%m"))]

freerolls <- freerolls[day_of_week %in% c("Saturday","Sunday")]
freerolls <- freerolls[month %in% c(2,3,4,9,10,11)]

write.csv(freerolls, file = "freeroll_dates.csv", row.names=F)



