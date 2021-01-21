# This project uses the Drake workflow manager.
# https://ropenscilabs.github.io/drake-manual/walkthrough.html
# Call make() to (re-)generate the project.

# The "targets" will be stored in a hidden .drake/ cache,
# and you can read them back into memory with loadd() and read().
# Drake's data target cache is ignored by git to avoid large files.

# All input data are already tracked with git.
# A hash log is saved as drake_cache.csv, which git does track.

# Scaling/Parallel Computing
# options(clustermq.scheduler = "multicore") # optional parallel
# computing. Also needs parallelism = "clustermq"

source("R/packages.R")  # Loads packages, e.g. library(drake).
source("R/functions.R") # Custom code as a bunch of functions.
source("R/plan.R")      # Creates the drake plan, i.e. the project.
# Run vis_drake_graph to plot the workflow.
text_drake_graph(plan, nchar = 3)
make(plan, verbose = 2, cache_log_file = TRUE) # Build the project.
text_drake_graph(plan, nchar = 3)
# beep(1)
