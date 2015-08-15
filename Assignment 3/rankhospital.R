rankhospital <- function(state, outcome, num = "best") {
	# Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE)
	# Check that state and outcome are valid
	if (!state %in% data$State) {
		stop("invalid state")
	}
	# Changing outcome to a numeric value
	if (outcome == "heart attack")
		numOutcome <- 11
	else if (outcome == "heart failure")
		numOutcome <- 17
	else if (outcome == "pneumonia") 
		numOutcome <- 23
	else
		stop("invalid outcome")
	
	# Return hospital name in that state with lowest 30-day death rate according to num
	data[, numOutcome] <- suppressWarnings(as.numeric(as.character(data[, numOutcome])))
	data <- data[!is.na(data[numOutcome]), ]
	data <- data[data$State == state, ]
	data <- data[order(data[, numOutcome], data$Hospital.Name), ]
	if (num == "best")
		return(c(data[1, ]$Hospital.Name))
	else if (num == "worst")
		return(c(data[nrow(data), ]$Hospital.Name))
	else
		if (num > nrow(data))
			return(NA)
		else
			return(c(data[num, ]$Hospital.Name))
}