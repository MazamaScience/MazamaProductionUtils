#!/usr/local/bin/Rscript

# This Rscript will find CPU hogs

# This script is desgined to be run on demand as a cron job or 'at' job, see the
# example below

# 1 2 3 4 5 /Users/jonathan/Projects/PWFSL/monitoring-data-ingest-v4/cpuHogs_exec.R --year=2017 --month=01 --outputDir=/Users/jonathan/Data/AirNow/RData --logDir=/Users/jonathan/Data/AirNow/RData

# You can test things by firing up the docker image interactively with bash and
# then Running R and testing this script a few lines at a time:
#
# docker run --rm -v /home/monitoring/Projects/monitoring-data-ingest-v4:/monitoring/v4 -v /data/monitoring:/monitoring/v4/data -w /monitoring/v4 -ti monitoring-data-ingest-v4 bash

VERSION = "1.0.0" # --- . --- . first pass

# The following packages are attached here so they show up in the sessionInfo
suppressPackageStartupMessages({
  library(methods)            # always included for Rscripts
  library(optparse)           # to parse command line flags

  library(dplyr)
  library(MazamaCoreUtils)
  library(MazamaProductionUtils)
})

# Set up OptionParser
option_list <- list(
  make_option(c("-o","--outputDir"), default=getwd(), help="Output directory for generated .RData files [default=\"%default\"]"),
  make_option(c("-l","--logDir"), default=getwd(), help="Output directory for generated .log file [default=\"%default\"]"),
  make_option(c("-V","--version"), action="store_true", default=FALSE, help="Print out version number [default=\"%default\"]")
)

# Parse arguments
opt <- parse_args(OptionParser(option_list=option_list))

# For debugging
if ( FALSE ) {

  # Desktop
  opt <- list(
    outputDir = getwd(),
    logDir = getwd(),
    version = FALSE
  )

}

# Print out version and quit
if ( opt$version ) {
  cat(paste0('cpuHogs_exec.R ',VERSION,'\n'))
  quit()
}

# Sanity checks
if ( !dir.exists(opt$outputDir) ) stop(paste0("outputDir not found:  ",opt$outputDir))
if ( !dir.exists(opt$logDir) ) stop(paste0("logDir not found:  ",opt$logDir))

# Make sure the year/month subdirectories exist
dir.create(opt$outputDir, recursive=TRUE, showWarnings=FALSE)
dir.create(opt$logDir, recursive=TRUE, showWarnings=FALSE)

# Assign log file names
debugLog <- file.path(opt$logDir, 'cpuHogs_DEBUG.log')
infoLog  <- file.path(opt$logDir, 'cpuHogs_INFO.log')
errorLog <- file.path(opt$logDir, 'cpuHogs_ERROR.log')

# Set up logging
logger.setup(debugLog=debugLog, infoLog=infoLog, errorLog=errorLog)

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error

logger.info('Running cpuHogs_exec.R version %s',VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse='\n')
logger.debug('R session:\n\n%s\n', sessionString)

# ------ Downloading and processing data --------------------------

result <- try({

  # CPU hogs
  cpu_hogs <- getProcessStatus() %>%
    select(pcpu,user) %>%
    group_by(user) %>%
    summarise(cpu_usage = sum(pcpu)) %>%
    arrange(desc(cpu_usage))

  print(head(cpu_hogs))

}, silent=TRUE)

# Handle errors
if ( "try-error" %in% class(result) ) {
  msg <- paste("Error finding CPU hogs: ", geterrmessage())
  logger.fatal(msg)
} else {
  # Guarantee that the errorLog exists
  if ( !file.exists(errorLog) ) dummy <- file.create(errorLog)
  logger.info("Completed successfully!")
}
