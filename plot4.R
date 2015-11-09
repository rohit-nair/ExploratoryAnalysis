downloadAndUnzipFile <- function() {
    # Configuration
    url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    fileName <- "ElectricPowerConsumption.zip"
    
    # If file doesn't exist download
    if (!file.exists(fileName)) {
        download.file(url, fileName, method="curl")
        unzip(fileName)
    }
}

readData <- function() {
    fileName <- "household_power_consumption.txt"
    
    # Step1: Download file and unzip
    downloadAndUnzipFile()
    
    # Step2: Get line numbers which starts with the relevant dates
    rowsToLoad <- grep('^[1,2]/2/2007', readLines(fileName))
    
    # Step3: read header
    header <- read.csv2(fileName, header=FALSE, nrows=1)
    
    # Step4: Read relevant rows
    dat <- read.csv(fileName, header=FALSE, sep=";",
                    skip=min(rowsToLoad)-1, 
                    nrows=max(rowsToLoad)-min(rowsToLoad),
                    colClasses=c(rep('character',2),rep('numeric',7)),
                    na.strings='?')
    
    # Step5: Clean headers
    colnames(dat) <- gsub("_","",unlist(header, use.names=FALSE))
    
    # Step6: Create datetime variable
    dat$DateTime <- strptime(paste(dat$Date, dat$Time, sep=" "), "%d/%m/%Y %H:%M:%S")
    
    # Step7: Plot the chart
    par(mfrow=c(2,2))
    with(dat,{
        plot(DateTime, Globalactivepower, type='l',
             xlab="",
             ylab="Global Active Power")
        
        plot(DateTime, Voltage, type='l',
             xlab="datetime",
             ylab="Voltage")
        
        plot(dat$DateTime, dat$Submetering1, type='l',
             xlab="",
             ylab="Energy sub metering")
        lines(dat$DateTime, dat$Submetering2, col="red")
        lines(dat$DateTime, dat$Submetering3, col="blue")
        legend("topright", col=c("black", "red", "blue"), 
               legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
               lwd=1, cex=0.25)  
        
        plot(DateTime, Globalreactivepower, type='l',
             xlab="datetime",
             ylab="Global_reactive_power")
        
    })
    
    # Step8: Save plot as png file
    dev.copy(device=png, "plot4.png")
    
    #Step9: Close device.
    dev.off()
}