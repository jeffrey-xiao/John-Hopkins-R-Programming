pollutantmean <- function(directory, pollutant, id = 1:332) {
    res <- 0
	cnt <- 0
	for (i in id) {
		fid = formatC(i, width = 3, flag = "0")
		filename = paste(directory, fid, sep = "/")
		filename1 = paste(filename, "csv", sep = ".")
		data = read.csv(file = filename1, header = TRUE)
		res <- res + sum(data[[pollutant]], na.rm = TRUE)
		cnt <- cnt + sum(!is.na(data[[pollutant]]))
	}
	res / cnt
}
