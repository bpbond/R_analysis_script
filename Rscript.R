# Template for R analysis script
# Ben Bond-Lamberty March 2015

# This is my starting point for most analysis or data processing scripts
# Most critically, it provides lightweight logging, with sessionInfo() 
# written at the bottom of every log; easy ggplot and data saving; 
# logged csv[.gz|zip] read/write; and a few other handy things.

SCRIPTNAME		<- "Rscript.R"
OUTPUT_DIR		<- "outputs/"
RANDOM_SEED		<- 12345		# comment out to not set seed
CHECKPOINTDATE	<- "2015-03-05" # comment out to not use checkpoint
SEPARATOR		<- "-------------------"

# -----------------------------------------------------------------------------
# Time-stamped output function
printlog <- function(msg="", ..., ts=TRUE, cr=TRUE) {
    if(ts) cat(date(), " ")
    cat(msg, ...)
    if(cr) cat("\n")
} # printlog

# -----------------------------------------------------------------------------
# Print dimensions of data frame
print_dims <- function(d, dname=deparse(substitute(d))) {
    stopifnot(is.data.frame(d))
    printlog(dname, "rows =", nrow(d), "cols =", ncol(d))
} # print_dims

# -----------------------------------------------------------------------------
# Return matrix of memory consumption
object_sizes <- function() {
    rev(sort(sapply(ls(envir=.GlobalEnv), function(object.name) 
        object.size(get(object.name)))))
} # object_sizes

# -----------------------------------------------------------------------------
# Return output directory (perhaps inside a script-specific folder)
# If caller specifies `scriptfolder=FALSE`, return OUTPUT_DIR
# If caller specifies `scriptfolder=TRUE` (default), return OUTPUT_DIR/SCRIPTNAME
outputdir <- function(scriptfolder=TRUE) {
    output_dir <- OUTPUT_DIR
    if(scriptfolder) output_dir <- file.path(output_dir, sub(".R$", "", SCRIPTNAME))
    if(!file.exists(output_dir)) dir.create(output_dir)
    output_dir
} # outputdir

# -----------------------------------------------------------------------------
# Save a ggplot figure
save_plot <- function(pname, p=last_plot(), ptype=".pdf", scriptfolder=TRUE, ...) {
    fn <- file.path(outputdir(scriptfolder), paste0(pname, ptype))
    printlog("Saving", fn)
    ggsave(fn, p, ...)
} # save_plot

# -----------------------------------------------------------------------------
# Save a data frame
save_data <- function(df, fname=paste0(deparse(substitute(df)), ".csv"), scriptfolder=TRUE, gzip=FALSE, ...) {
  fn <- file.path(outputdir(scriptfolder), fname)
  if(gzip) {
    printlog("Saving", fn, "[gzip]")    
    fn <- gzfile(paste0(fn, ".gz"))
  } else {
    printlog("Saving", fn)    
  }
  write.csv(df, fn, row.names=FALSE, ...)
} # save_data

# -----------------------------------------------------------------------------
# Open a netCDF file and return handle (using ncdf4 package)
open_ncdf <- function(fn, datadir=".") {
	if(is.null(datadir)) {  # NULL signifies absolute path
		fqfn <- fn 
  	} else {
    	fqfn <- file.path(datadir, fn)      
  	}
  	printlog("Opening", fqfn)
    nc_open(fqfn)
} # open_ncdf

# -----------------------------------------------------------------------------
# Open a (possibly compressed) csv file and return data
read_csv <- function(fn, datadir=".", ...) {
	if(is.null(datadir)) {  # NULL signifies absolute path
		fqfn <- fn 
  	} else {
    	fqfn <- file.path(datadir, fn)      
  	}
  	printlog("Opening", fqfn)
    if(grepl(".gz$", fqfn)) {
        fqfn <- gzfile(fqfn)
    } else if(grepl(".zip$", fqfn)) {
        fqfn <- unz(fqfn)
    }
    invisible(read.csv(fqfn, stringsAsFactors=F, ...))
} # read_csv

# -----------------------------------------------------------------------------
# Read data from the clipboard
paste_data <- function(header=TRUE) {
    read.table(pipe("pbpaste"), header=header)
} # paste_data

# -----------------------------------------------------------------------------
is_outlier <- function(x, devs=3.2) {
    # See: Davies, P.L. and Gather, U. (1993).
    # "The identification of multiple outliers" (with discussion)
    # J. Amer. Statist. Assoc., 88, 782-801.
    
    x <- na.omit(x)
    lims <- median(x) + c(-1, 1) * devs * mad(x, constant = 1)
    x < lims[ 1 ] | x > lims[2]
} # is_outlier

if(!file.exists(OUTPUT_DIR)) {
    printlog("Creating", OUTPUT_DIR)
    dir.create(OUTPUT_DIR)
}


# ==============================================================================
# Main (or more commonly, put in a separate script that sources this one)

# Setup, packages, reproducibility
sink(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), split=T) # open log

printlog("Welcome to", SCRIPTNAME)

if(exists("RANDOM_SEED")) {
    printlog("Setting random generator seed to", RANDOM_SEED)
    set.seed(RANDOM_SEED)
}

if(require(checkpoint) & exists("CHECKPOINTDATE"))
    try(checkpoint(CHECKPOINTDATE)) # 'try' b/c errors w/o network (issue #171)
library(ggplot2)
theme_set(theme_bw())
library(reshape2)
library(dplyr)
#library(ncdf4)

# ----- Main script goes here...

# Expressions...
# expression(SR[annual]~(g~C~m^{-2}~yr^{-1}))
# text(x, y, bquote(R^{2} == .(r2)))
# expression(italic(T)[5]~group("(",paste(degree,C),")"))


printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink() # close log
