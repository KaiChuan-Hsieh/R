args <- commandArgs(TRUE)
if (length(args) != 1) {
    print("input: Rscript --vanilla cpuUsage.R <filename>")
    quit()
}

# read data from file
mydata <- read.csv(args)
names(mydata) <- c("timestamp", "process", "percentage")

# change file structure for computation
mydata$percentage <- sub("%", "", mydata$percentage)
mydata$timestamp <- as.POSIXlt(strptime(mydata$timestamp, format = "%Y-%m-%d %H:%M:%S"))
mydata$percentage <- as.numeric(mydata$percentage)

# record unique process names
procs <- unique(mydata$process)
nproc <- length(procs)

# record capture time
times <- unique(mydata$timestamp)
ntime <- length(times)

# sort process by cpu usage
ratios <- NULL
for (i in 1:nproc) {
    ratios[i] <- sum(mydata[mydata$process==procs[i],]$percentage)
}

procdata <- data.frame( process=procs, ratio=ratios)
procdata <- procdata[order(procdata$ratio, decreasing=TRUE),]
procs <- procdata$process
printline <- 20

# denote x,y range in graph
xrange <- range(mydata$timestamp)
yrange <- range(mydata$percentage)

# allocate color linetype and plotchar
colors <- rev(rainbow(printline))
linetype <- c(1:printline)
plotchar <- c(1:printline)

# open jpg file to record the plot
jpgname <- sub("csv", "jpg", args)
jpeg(filename = jpgname, width=1280, height=800)

# change times structure to POSIXct for axis
times <- as.POSIXct(times)

# describe layout
layout(matrix(c(1,2), nrow=2), heights=c(3,1))

# plot the scale of the plot
plot(xrange, yrange, type="n", main="CPU Usage/Timestamp", xlab="timestamp", ylab="percentage", xaxt="n")
axis(side=1, at=ts(times), labels=format(times, format="%H:%M:%S"))

# print each line
s <- NULL
for (i in 1:printline) {
    s[1:ntime] <- 0
    j <- 1
    for (k in 1:ntime) {
        comtime = mydata[mydata$process==procs[i], 1][j]
        if (!is.na(comtime) && times[k]==comtime) {
            s[k] <- mydata[mydata$process==procs[i], 3][j]
            j=j+1
        }
    }

    lines(times, s, type="b", lty=linetype[i], col=colors[i], pch=plotchar[i])
}

# margin setup
par(mar=c(5,4,0,2)+0.1)

# plot the legend
plot(xrange, yrange, type="n", axes=FALSE, ann=FALSE)
legend("center", legend=procs[1:printline], col=colors, pch=plotchar, lty=linetype, ncol=3)

# save the jpg file
dev.off()

quit()
