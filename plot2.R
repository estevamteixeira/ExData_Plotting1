##-----------------
## Loading the Data
##-----------------

temp <- read.table(file.path(path, files[1]), header = TRUE, sep = ";",
                   na.strings = "?",
                   colClasses = c("character", "character", "numeric", "numeric",
                                  "numeric", "numeric", "numeric", "numeric",
                                  "numeric"))
head(temp)
# The dataset has 2,075,259 rows and 9 columns
dim(temp)
summary(temp)

# date conversion

temp$Date <- as.POSIXct(temp$Date, format="%d/%m/%Y")

temp$Time <- format(temp$Time, format = "%H:%M:%S")

## Filtering data from the dates 2007-02-01 and 2007-02-02

data <- subset(temp, temp$Date >= "2007-02-01" & temp$Date <= "2007-02-02")
head(data);dim(data)
summary(data)

# Creating a DateTime variable
data$DateTime <- as.POSIXct(paste(data$Date, data$Time, sep = " "),
                            format="%Y-%m-%d %H:%M:%S")

## Removing unused information to save memory

rm(list = c("temp"))

## Looking for missing information in each variable

apply(sapply(data, is.na),2,sum)

##-------
## Plot 2
##-------

plot(data$Global_active_power~data$DateTime,
     type = "l",
     ylab = "Global Active Power (in kilowatts)",
     xlab = "")

## Save file as .png and close device

dev.copy(png,'plot2.png', width = 480, height = 480)
dev.off()