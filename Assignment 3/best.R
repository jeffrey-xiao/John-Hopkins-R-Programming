best <- function (state, outcome) {
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
	
	# Return hospital name in that state with lowest 30-day death rate
	data[, numOutcome] <- suppressWarnings(as.numeric(as.character(data[, numOutcome])))
	data <- data[data$State == state, ]
	data <- data[order(data$Hospital.Name), ]
	data <- data[order(data[, numOutcome]), ]
	c(data[1, ]$Hospital.Name)
}
