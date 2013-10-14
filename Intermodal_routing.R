##Intermodal Routing
#1 Import CSV of cost and time for each trip and notation
time_cost <- read.csv(file="intermodal_cost_time.csv",
                      as.is=T)
notation <- read.csv(file="notation.csv",
                     as.is=T)

#2 Look for the Cost and time for any from-to-route in form of a vector
cost_time_one_trip <- function(trip){
 df <- time_cost
 #redefine the variables to the names in the cost_time DF
 if(any(notation["Abbreviation"]==trip[1])){
  trip[1] <- notation[notation["Abbreviation"]==trip[1],1]
 } else {
  stop("No data found for trip, no city with abbreviation in origin")
 }
 if(any(notation["Abbreviation"]==trip[2])){
  trip[2] <- notation[notation["Abbreviation"]==trip[2],1]
 } else {
  stop("No data found for trip, no city with abbreviation in destination")
 }
 if(any(notation["Abbreviation"]==trip[3])){
  trip[3] <- notation[notation["Abbreviation"]==trip[3],1]
 } else {
  stop("No data found for trip, no transportation method")
 }
 #Find the trip 
 selection <- df[c(df[["Start_node"]]==trip[1] & 
      df[["End_node"]]==trip[2] & 
      df[["Mode"]]==trip[3]),]
 #Get Cost and Time for trip
 if(dim(selection)[1]==0){
  stop("No data found for trip")
 } else {
  cost_time_vector <- c(selection[[4]],selection[[5]])
  cost_time_vector
 }
}

#3 make a function that handles a list and sums the cost and times of trips
cost_time_trip_list <- function(...){
 full_trip_list <- list(...)
 cost_time_trips <- sapply(full_trip_list,cost_time_one_trip,simplify=T,
                           USE.NAMES=F)
 row.names(cost_time_trips) <- c("Cost_Usdlls","Time_days")
 apply(cost_time_trips,1,sum)
}

##Testing the function
trip1 <- c("VAN","L.A.","t")
trip2 <- c("L.A.","E.P.","r")
trip3 <- c("E.P.","M.C.","t")
a <- cost_time_trip_list(trip1,trip2,trip3)
plot(c(-100,-100),c(100,100))
abline(a=a[1],b=a[2])


