#Define the file names for your CD spectra here
#Set the working directory to the directory where this file is, and place your CD .csv files here
#Edit this part of the code to define your input file names
cd1.file <- '4hfosw-homo.csv'
cd2.file <- 'cjun.csv'
cdmixed.file <- 'cjun-4hFosW.csv'

#Edit your input names
cd1.name <- "4hFosW"
cd2.name <- "cJun"

#Do not edit below this point!
#Below is the script

observed.name <- paste(c(cd1.name,"-",cd2.name," (Observed)"),sep="",collapse="")
expected.name <- paste(c(cd1.name,"-",cd2.name," (Expected)"),sep="",collapse="")

#Extract CD into vector function
extract.cd.to.vector.function <- function(x)
  {
  return(as.numeric(x['Average.0']))
  }

#Read CD function
get.cd.function <- function(file)
  {
  rawtable <- read.csv(file,skip=28)
  rawtable = rawtable[1:111,1:2]
  
  vector <- apply(rawtable,1,extract.cd.to.vector.function)
  }

#Process each point on the line for average and RMSD
process.points.function <- function(x)
  {
  mean <- (x['CD_1']+x['CD_2'])/2
  print(mean)
  sd <- (mean-x['Mixed'])^2
  
  output <- c("Mean"=mean,"SD"=sd)
  return(output)
  }

#Get spectra data from each file and import into vectors
cd1.vector <- get.cd.function(cd1.file)
cd2.vector <- get.cd.function(cd2.file)
cd.mixed.vector <- get.cd.function(cdmixed.file)

#Create dataframe of all the mixtures in the dimer exchange
cds.all.data.frame <- data.frame("Wavelength"=300:190,"CD_1"=cd1.vector,"CD_2"=cd2.vector,"Mixed"=cd.mixed.vector)

#Generate mixture line and RMSD
cd.calculated.parameters <- apply(cds.all.data.frame,1,process.points.function)
cds.all.data.frame$Predicted <- cd.calculated.parameters['Mean.CD_1',]
rmsd <- sqrt(mean(cd.calculated.parameters['SD.CD_1',]))

#Generate dimer exchange plot

png("dimerexchangeplot.png")
plot(cds.all.data.frame[,'Wavelength'],cds.all.data.frame[,'Mixed'],type="l",ylim=c(-250,250),xaxt="n",xaxs="i",yaxt="n",yaxs="i",xlab="Wavelength",ylab="CD",lwd=2,col="green")
axis(1,at=seq(190,300,by=10))
axis(2,at=seq(-250,250,by=250))
lines(cds.all.data.frame[,'Wavelength'],cds.all.data.frame[,'CD_1'],type="l",lwd=2,col="blue")
lines(cds.all.data.frame[,'Wavelength'],cds.all.data.frame[,'CD_2'],type="l",lwd=2,col="yellow")
lines(cds.all.data.frame[,'Wavelength'],cds.all.data.frame[,'Predicted'],type="l",lty=2)
legend(legend=c(cd1.name,cd2.name,observed.name,expected.name),col=c("yellow","blue","green","black"),lwd=c("2","2","2","2"),lty=c(1,1,1,2),"topright")
dev.off()

print(paste(c("The RMSD of this dimer exchange is: ",rmsd),sep="",collapse=""))