# prepareGenomeData.R
#
# Purpose:  Read source data from provider databases, map to HUGO symbols
#           and store as tab-delimied text.
#
#           Currently: linear data from HGNC only
#
# Version:  0.1
# Date:     2018 02 20
# Author:   Boris Steipe <boris.steipe@utoronto.ca>
#
# Dependencies:
#           readr package
#
# License: GPL-3 (https://www.gnu.org/licenses/gpl-3.0.en.html)
#
# Version history:
#   0.1  First draft
#
# ToDo:
#    - add different types of data
#
# ==============================================================================


# ====  PARAMETERS  ============================================================
# ...



# ====  PACKAGES  ==============================================================
# Load all required packages.

if (!require(readr, quietly=TRUE)) {
  install.packages("readr")
  library(readr)
}


# ====  FUNCTIONS  =============================================================

# <functionName> <- function(<argumentName> = <defaultValue>,
#                            <argumentName> = <defaultValue>,
#                            <argumentName> = <defaultValue>) {
#   # Purpose:
#   #     <describe ...>
#   #
#   # Parameters:
#   #     <name>:   <type>   <description
#   #
#   # Details:
#   #     <description, notes, see-also ...>
#   #
#   # Value:
#   #     <type, structure etc. of the single return value. Or:
#   #     NA - function is invoked for its side effect of ... <describe>. >
#
#   # <code ...>
#
#   return(<result>)
# }



# ====  PROCESS  ===============================================================


# Read file obtained via custom download from ensembl biomart
# Custom download:
# Filters: Chromosome/scaffold: 20
#          With HGNC Symbol ID(s): Only
# Attrib.: Gene stable ID
#          Gene start (bp)
#          Gene end (bp)
#          Strand
#          GOSlim GOA Accession(s)
#          GOSlim GOA Description
#          HGNC symbol
rawData <- read_tsv("mart_export.txt")
mySymbols <- sort(unique(rawData$`HGNC symbol`))

myGOslimsTable <- table(rawData$`GOSlim GOA Accession(s)`)

N <- length(mySymbols)
mygenomeData <- data.frame(sym = mySymbols,
                           chr = rep(20, N),
                           start = numeric(N),
                           end = numeric(N),
                           strand = numeric(N),
                           GOAid = character(N),
                           GOAdescr = character(N),
                           stringsAsFactors = FALSE)
row.names(mygenomeData) <- mySymbols

# Process everything into a dataframe
for (sym in mySymbols) {
  sel <- which(rawData$`HGNC symbol` == sym)
  # choose the most specific (lowest frequency) GOA
  GOAs <- rawData$`GOSlim GOA Accession(s)`[sel]
  topGOA <- names(which(myGOslimsTable[GOAs] == min(myGOslimsTable[GOAs]))[1])
  idx <- (sel[GOAs == topGOA])[1]

  mygenomeData[sym, "start"] <- rawData$`Gene start (bp)`[idx]
  mygenomeData[sym, "end"] <- rawData$`Gene end (bp)`[idx]
  mygenomeData[sym, "strand"] <- rawData$Strand[idx]
  mygenomeData[sym, "GOAid"] <- rawData$`GOSlim GOA Accession(s)`[idx]
  mygenomeData[sym, "GOAdescr"] <- rawData$`GOSlim GOA Description`[idx]

}

write_tsv(mygenomeData, path = "chr20_data.tsv")

# ====  TESTS  =================================================================
# ...





# [END]
