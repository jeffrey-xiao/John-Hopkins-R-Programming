rankall <- function(outcome, num = "best") {
	# Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE)
	# Changing outcome to a numeric value and checking if outcome is valid
	if (outcome == "heart attack")
		numOutcome <- 11
	else if (outcome == "heart failure")
		numOutcome <- 17
	else if (outcome == "pneumonia") 
		numOutcome <- 23
	else
		stop("invalid outcome")
	
	# Return hospital name in that state with lowest 30-day death rate according to num for each state
	data[, numOutcome] <- suppressWarnings(as.numeric(as.character(data[, numOutcome])))
	data <- data[!is.na(data[numOutcome]), ]
	data <- data[order(data[, numOutcome], data$Hospital.Name), ]
	data <- split(data[, 2], data$State)
	getNum <- function(x, num) {
		if (num == "best")
			return(head(x, n = 1))
		else if (num == "worst")
			return(tail(x, n = 1))
		else
			return(x[num])
	}
	res <- lapply(data, getNum, num)
	data.frame(hospital = unlist(res), state = names(res))
}