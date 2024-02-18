# Hamza El Sousi
# 040982818

# Load the CSV file
CerealsDF <- read.csv("C:\\Users\\Hamza\\Desktop\\Algonquin college\\Semester Four\\Numerical Computing\\Assignment1\\assignment1.csv", stringsAsFactors = FALSE, sep = ";")

# Display the structure of CerealsDF
str(CerealsDF)

# Display the first ten rows of CerealsDF
head(CerealsDF, 10)

# Deleting the second row (data types)
CerealsDF <- CerealsDF[-2, ]

# Convert columns to correct data types if necessary
CerealsDF$carbo <- as.numeric(as.character(CerealsDF$carbo))
CerealsDF$sugars <- as.numeric(as.character(CerealsDF$sugars))

# Adding a new column "totalcarbo"
CerealsDF$totalcarbo <- CerealsDF$carbo + CerealsDF$sugars

# Print the number of rows and columns
cat("Rows:", nrow(CerealsDF), "Columns:", ncol(CerealsDF), "\n")

# Finding the number of hot cereals
hotCereals <- nrow(subset(CerealsDF, type == "Hot"))
cat("Number of hot cereals:", hotCereals, "\n")

# Finding the number of unique manufacturers
uniqueMfrs <- length(unique(CerealsDF$mfr))
cat("Unique manufacturers:", uniqueMfrs, "\n")

# Extracting cereals manufactured by Kellogg's
cereals_K <- subset(CerealsDF, mfr == "K")
print("Cereals manufactured by Kellogg’s:")
print(cereals_K)

# Extracting cereals with <= 90 calories and > 2 units of fat
filteredCereals <- subset(CerealsDF, calories <= 90 & fat > 2)

# Saving the filtered cereals as a CSV file
write.csv(filteredCereals, "filteredCereals.csv", row.names = FALSE)
