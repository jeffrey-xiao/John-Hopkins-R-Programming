complete <- function (directory, id = 1:332) {
	nobs <- integer(length(id))
	cnt = 1
	for (i in id) {
		fid = formatC(i, width = 3, flag = "0")
		filename = paste(directory, fid, sep = "/")
		filename1 = paste(filename, "csv", sep = ".")
		data = read.csv(file = filename1, header = TRUE)
		nobs[cnt] <- nobs[cnt] + sum(!is.na(data["sulfate"]) & !is.na(data["nitrate"]))
		cnt <- cnt + 1
	}
	data.frame(id, nobs)
}