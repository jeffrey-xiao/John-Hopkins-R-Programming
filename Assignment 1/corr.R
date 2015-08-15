corr <- function(directory, threshold = 0) {
	res <- numeric(length(332))
	cnt <- 1
	for (i in 1:332) {
		fid = formatC(i, width = 3, flag = "0")
		filename = paste(directory, fid, sep = "/")
		filename1 = paste(filename, "csv", sep = ".")
		data = read.csv(file = filename1, header = TRUE)
		subsetComplete <- !is.na(data["sulfate"]) & !is.na(data["nitrate"])
		cntComplete <- sum(subsetComplete)
		if (cntComplete > threshold) {
			res[cnt] <- cor(data$nitrate, data$sulfate, use = "complete.obs")
			cnt <- cnt + 1
		}
	}
	res[1:cnt-1]
}