# prepareGenomeData.R
#
# Purpose:  Read source data from provider databases, map to HUGO symbols
#           and store as tab-delimied text.
#
#           Currently: linear data from HGNC only
#
# Version:  0.2
# Date:     2018 02 20
# Author:   Boris Steipe <boris.steipe@utoronto.ca>
#
# Dependencies:
#           packages: readr, stringr
#
# License: GPL-3 (https://www.gnu.org/licenses/gpl-3.0.en.html)
#
# Version history:
#   0.2  add STRING data
#   0.1  First draft: Gene data from BioMart
#
# ToDo:
#    - add different types of data
#
# ==============================================================================


#TOC> ==========================================================================
#TOC> 
#TOC>   Section  Title                              Line
#TOC> --------------------------------------------------
#TOC>   1        PARAMETERS                           39
#TOC>   2        PACKAGES                             49
#TOC>   3        BASIC GENE DATA (ENSEMBL)            64
#TOC>   4        STRING DATA                         122
#TOC> 
#TOC> ==========================================================================


# =    1  PARAMETERS  ==========================================================

# Paths to the directories that contains the various data sets. The source files
# are described below, but some of them are quite large so we are not
# loading them into the repo.
#
BIOMARTDIR <- "./"
STRINGDIR  <- "./"


# =    2  PACKAGES  ============================================================
# Load all required packages.

if (!require(readr, quietly=TRUE)) {
  install.packages("readr")
  library(readr)
}

if (!require(stringr, quietly=TRUE)) {
  install.packages("stringr")
  library(stringr)
}



# =    3  BASIC GENE DATA (ENSEMBL)  ===========================================


# Read file obtained via custom download from ensembl biomart
# http://useast.ensembl.org/
#
# Click on biomart and select:
#
# Ensembl Genes 91
#
# Dataset: Human genes (GRCh38.p10)
#
# Filters: REGION: Chromosome/scaffold: 20
#          GENE: With HGNC Symbol ID(s): Only
# Attrib.: Gene stable ID
#          Gene start (bp)
#          Gene end (bp)
#          Strand
#          GOSlim GOA Accession(s)
#          GOSlim GOA Description
#          HGNC symbol

tmp <- read_tsv(paste0(BIOMARTDIR, "mart_export.txt"))
mySymbols <- sort(unique(tmp$`HGNC symbol`))

myGOslimsTable <- table(tmp$`GOSlim GOA Accession(s)`)

N <- length(mySymbols)
myGeneData <- data.frame(sym = mySymbols,
                         chr = rep(20, N),
                         start = numeric(N),
                         end = numeric(N),
                         strand = numeric(N),
                         GOAid = character(N),
                         GOAdescr = character(N),
                         stringsAsFactors = FALSE)
row.names(myGeneData) <- mySymbols

# Process everything into a dataframe
for (sym in mySymbols) {
  sel <- which(tmp$`HGNC symbol` == sym)
  # choose the most specific (lowest frequency) GOA
  GOAs <- tmp$`GOSlim GOA Accession(s)`[sel]
  topGOA <- names(which(myGOslimsTable[GOAs] == min(myGOslimsTable[GOAs]))[1])
  idx <- (sel[GOAs == topGOA])[1]

  myGeneData[sym, "start"]    <- tmp$`Gene start (bp)`[idx]
  myGeneData[sym, "end"]      <- tmp$`Gene end (bp)`[idx]
  myGeneData[sym, "strand"]   <- tmp$Strand[idx]
  myGeneData[sym, "GOAid"]    <- tmp$`GOSlim GOA Accession(s)`[idx]
  myGeneData[sym, "GOAdescr"] <- tmp$`GOSlim GOA Description`[idx]

}

write_tsv(myGeneData, path = "chr20_data.tsv")



# =    4  STRING DATA  =========================================================

# Source data was downloaded from STRING database via organism specific
# download.
#
# https://string-db.org/
#
#   9606.protein.links.v10.5.txt  (522.2 MB)  - contains relationships
#   9606.protein.aliases.v10.5.txt (157.7 MB) - contains IDs
#
#

# Read the alias data: we need that to find which ENSP IDs map to which
# HUGO symbols
tmp <- read_tsv(paste0(STRINGDIR, "9606.protein.aliases.v10.5.txt"),
                skip = 1,
                col_names = c("ENSP", "ID", "source"))  # 2,055,779 rows

tmp <- tmp[grep("BioMart_HUGO", tmp$source), ]  # 19,119 rows

tmp <- tmp[tmp$ID %in% myGeneData$sym, ] # 498 of 525 symbols mapped
ENS2symMap <- tmp$ID                     # extract symbols...
names(ENS2symMap) <- tmp$ENSP            # ... and use ENSP IDs as names


# Read the interaction graph data: this is a weighted graph defined as an
# edge list with gene a, gene b, confidence score (0, 999).
tmp <- read_delim(paste0(STRINGDIR, "9606.protein.links.v10.5.txt"),
                  delim = " ",
                  skip = 1,
                  col_names = c("a", "b", "score"))  # 11,353,056 rows

tmp <- tmp[tmp$score >= 900, ]  # 547,620 rows of high-confidence edges

# Extract edges where both genes are in the mapped Chr 20 genes
Chr20funcIntx <- tmp[(tmp$a %in% names(ENS2symMap)) &
                     (tmp$b %in% names(ENS2symMap)), ]  # 564 rows

# Use ENS2symMap to translate ENSP IDs to HUGO symbols
Chr20funcIntx$a <- ENS2symMap[Chr20funcIntx$a]
Chr20funcIntx$b <- ENS2symMap[Chr20funcIntx$b]

# Done
write_tsv(Chr20funcIntx, path = "Chr20funcIntx.tsv")



# ====  TESTS  =================================================================
# ...


# [END]
