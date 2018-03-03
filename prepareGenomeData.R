# prepareGenomeData.R
#
# Purpose:  Read source data from provider databases, map to HGNC symbols
#           and store as tab-delimied text.
#
#           Currently: linear data from HGNC only
#
# Version:  0.3.1
# Date:     2018 03 03
# Author:   Boris Steipe <boris.steipe@utoronto.ca>
#
# Dependencies:
#           packages: readr, stringr
#
# License: GPL-3 (https://www.gnu.org/licenses/gpl-3.0.en.html)
#
# Version history:
#   0.3.1   Add gene type to basic data. Use HGNC symbols as authoritative
#           source for gene annotations
#   0.3     Remove BioMart GO data from basic gene data, prepare separate
#             GO data file and populate Gene data with GOslim from BP
#             ontology only.
#   0.2     add STRING data
#   0.1     First draft: Gene data from BioMart
#
# ToDo:
#    - add GWAS and Protein data
#
# ==============================================================================


#TOC> ==========================================================================
#TOC> 
#TOC>   Section  Title                                     Line
#TOC> ---------------------------------------------------------
#TOC>   1        PARAMETERS                                  45
#TOC>   2        PACKAGES                                    57
#TOC>   3        HGNC SYMBOLS AND CROSSREFERENCES            72
#TOC>   4        BIOMART GENE ANNOTATIONS                   127
#TOC>   5        STRING DATA                                171
#TOC> 
#TOC> ==========================================================================


# =    1  PARAMETERS  ==========================================================

# Paths to the directories that contains the various data sets. The source files
# are described below, but some of them are quite large so we are not
# loading them into the repo.
#
HGNCDIR    <- "./"
BIOMARTDIR <- "./"
STRINGDIR  <- "./"
GODIR      <- "./"


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



# =    3  HGNC SYMBOLS AND CROSSREFERENCES  ====================================

# The HGNC (Human Gene Nomeclature Committee) is the authoritative source for
# recognized genes. Source data is downloaded from the custom download page
# at https://www.genenames.org/cgi-bin/download
#
# - HGNC Approved Symbol
# - Approved Name
# - Locus Type
# - Chromosome
# - Ensembl Gene ID
# - Entrez Gene ID (external)
# - OMIM ID (external)
# - RefSeq ID (external)
# - UniProt ID (external)
# - Ensembl ID (external)
# - UCSC ID (external)

# Download only "Approved" status, and Chr 20

# read source data
tmp <- read_tsv(paste0(HGNCDIR, "HGNC_data.tsv"))   # 1,041 rows

# check which Locus types appear in this data
unique(tmp$`Locus Type`)

# subset rows with gene types of interest
myLocusTypes <- c("gene with protein product",
                  "RNA, transfer")

tmp <- tmp[tmp$`Locus Type` %in% myLocusTypes, ]    # 529 rows

# subset columns of interest
Chr20GeneData <- data.frame(sym = tmp$`Approved Symbol`,
                            name = tmp$`Approved Name`,
                            type = tmp$`Locus Type`,
                            EnsemblID = tmp$`Ensembl Gene ID`,
                            EntrezGeneID = tmp$`Entrez Gene ID(supplied by NCBI)`,
                            OMIMID = tmp$`OMIM ID(supplied by OMIM)`,
                            RefSeqID = tmp$`RefSeq(supplied by NCBI)`,
                            UniProtID = tmp$`UniProt ID(supplied by UniProt)`,
                            UCSCID = tmp$`UCSC ID(supplied by UCSC)`,
                            stringsAsFactors = FALSE)

rownames(Chr20GeneData) <- Chr20GeneData$sym

Chr20GeneData$type <- gsub("gene with protein product",
                           "protein",
                           Chr20GeneData$type)
Chr20GeneData$type <- gsub("RNA, transfer",
                           "tRNA",
                           Chr20GeneData$type)



# =    4  BIOMART GENE ANNOTATIONS  ============================================


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
# Attrib.: Gene stable ID
#          Gene start (bp)
#          Gene end (bp)
#          Strand
#          HGNC symbol

# read source data
tmp <- read_tsv(paste0(BIOMARTDIR, "mart_export.txt"))   # 5,379 rows

# use only rows that match Ensembl IDs in Chr20GeneData
tmp <- tmp[(tmp$`Gene stable ID` %in% Chr20GeneData$EnsemblID), ]  # 521 rows

# Add gene start, end, and strand information:
# Initialize columns
Chr20GeneData$start <- as.numeric(NA)
Chr20GeneData$end <- as.numeric(NA)
Chr20GeneData$strand <- as.numeric(NA)

# Add data
for (i in 1:nrow(Chr20GeneData)) {
  iRow <- which(tmp$`Gene stable ID` == Chr20GeneData$EnsemblID[i])
  if (length(iRow) == 1) {
    Chr20GeneData$start[i] <- tmp$`Gene start (bp)`[iRow]
    Chr20GeneData$end[i] <- tmp$`Gene end (bp)`[iRow]
    Chr20GeneData$strand[i] <- tmp$Strand[iRow]
  }
}




# =    5  STRING DATA  =========================================================

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
# HGNC symbols
tmp <- read_tsv(paste0(STRINGDIR, "9606.protein.aliases.v10.5.txt"),
                skip = 1,
                col_names = c("ENSP", "ID", "source"))  # 2,055,779 rows

tmp <- tmp[grep("BioMart_HUGO", tmp$source), ]  # 19,119 rows

tmp <- tmp[tmp$ID %in% Chr20GeneData$sym, ] # 497 of 521 symbols mapped
ENS2symMap <- tmp$ID                        # extract symbols...
names(ENS2symMap) <- tmp$ENSP               # ... and use ENSP IDs as names


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

# Use ENS2symMap to translate ENSP IDs to HGNC symbols
Chr20funcIntx$a <- ENS2symMap[Chr20funcIntx$a]
Chr20funcIntx$b <- ENS2symMap[Chr20funcIntx$b]

# Done
write_tsv(Chr20funcIntx, path = "Chr20funcIntx.tsv")



# ====  TESTS  =================================================================
# ...


# [END]
