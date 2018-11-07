
library(jsonlite)
JsonData<-fromJSON("hotelSurveyBarriot.json")
hotelSurvey <- data.frame(JsonData)

hotelSurvey$CustSatBuckets <- replicate(length(hotelSurvey$overallCustSat), "Average")
hotelSurvey$CustSatBuckets[hotelSurvey$overallCustSat > 7] <- "High"
hotelSurvey$CustSatBuckets[hotelSurvey$overallCustSat < 7] <- "Low"
str(hotelSurvey)

hotelSurvey$checkInSatBuckets <- replicate(length(hotelSurvey$checkInSat), "Average")
hotelSurvey$checkInSatBuckets[hotelSurvey$checkInSat > 7] <- "High"
hotelSurvey$checkInSatBuckets[hotelSurvey$checkInSat < 7] <- "Low"

hotelSurvey$hotelCleanBuckets <- replicate(length(hotelSurvey$hotelClean), "Average")
hotelSurvey$hotelCleanBuckets[hotelSurvey$hotelClean > 7] <- "High"
hotelSurvey$hotelCleanBuckets[hotelSurvey$hotelClean < 7] <- "Low"

hotelSurvey$hotelFriendlyBuckets <- replicate(length(hotelSurvey$hotelFriendly), "Average")
hotelSurvey$hotelFriendlyBuckets[hotelSurvey$hotelFriendly > 7] <- "High"
hotelSurvey$hotelFriendlyBuckets[hotelSurvey$hotelFriendly < 7] <- "Low"

q <- quantile(hotelSurvey$hotelSize, c(0.4, 0.6))
hotelSurvey$hotelSizeBuckets <- replicate(length(hotelSurvey$hotelSize), "Average")
hotelSurvey$hotelSizeBuckets[hotelSurvey$hotelSize <= q[1]] <- "Low"
hotelSurvey$hotelSizeBuckets[hotelSurvey$hotelSize > q[2]] <- "High"

q <- quantile(hotelSurvey$guestAge, c(0.4, 0.6))
hotelSurvey$guestAgeBuckets <- replicate(length(hotelSurvey$guestAge), "Average")
hotelSurvey$guestAgeBuckets[hotelSurvey$guestAge <= q[1]] <- "Low"
hotelSurvey$guestAgeBuckets[hotelSurvey$guestAge > q[2]] <- "High"

q <- quantile(hotelSurvey$lengthOfStay, c(0.4, 0.6))
hotelSurvey$lengthOfStayBuckets <- replicate(length(hotelSurvey$lengthOfStay), "Average")
hotelSurvey$lengthOfStayBuckets[hotelSurvey$lengthOfStay <= q[1]] <- "Low"
hotelSurvey$lengthOfStayBuckets[hotelSurvey$lengthOfStay > q[2]] <- "High"

q <- quantile(hotelSurvey$whenBookedTrip, c(0.4, 0.6))
hotelSurvey$whenBookedTripBuckets <- replicate(length(hotelSurvey$whenBookedTrip), "Average")
hotelSurvey$whenBookedTripBuckets[hotelSurvey$whenBookedTrip <= q[1]] <- "Low"
hotelSurvey$whenBookedTripBuckets[hotelSurvey$whenBookedTrip > q[2]] <- "High"

str(hotelSurvey)

table(hotelSurvey$guestAgeBuckets)
prop.table(table(hotelSurvey$guestAgeBuckets, dnn = "Percentage in different age groups"))
table(hotelSurvey$hotelFriendlyBuckets)
prop.table(table(hotelSurvey$hotelFriendlyBuckets, dnn = "Percentage in different hotel friendly buckets"))

prop.table(table(hotelSurvey$hotelFriendlyBuckets, hotelSurvey$CustSatBuckets, dnn = c("hotelFriendly", "Customer Satisfaction")))

