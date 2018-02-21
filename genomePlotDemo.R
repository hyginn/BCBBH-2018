# genomePlotDemo.R
#
# Purpose:  Demo a genome plot of genes in a circle plot with functional
#           connections.
#
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
#    - ...
#
# ==============================================================================


#TOC> ==========================================================================
#TOC>
#TOC>   Section  Title                            Line
#TOC> ------------------------------------------------
#TOC>   1        PARAMETERS                         48
#TOC>   2        PACKAGES                           62
#TOC>   3        FUNCTIONS                          71
#TOC>   4        PROCESS                           272
#TOC>   4.1      INITIALIZE                        281
#TOC>   4.2      ANNOTATE                          300
#TOC>   4.3      LAYOUT                            330
#TOC>   4.4      OPTIMIZE AESTHETICS               392
#TOC>   4.5      CREATE SVG                        439
#TOC>   4.5.1    Compute scale and translation     442
#TOC>   4.5.2    Write SVG header                  464
#TOC>   4.5.3    Render global layout elements     469
#TOC>   4.5.4    Render edges                      478
#TOC>   4.5.5    Render genes                      490
#TOC>   4.5.6    Write SVG footer                  513
#TOC>   4.6      WRITE SVG TO FILE AND VIEW        517
#TOC>
#TOC> ==========================================================================


# =    1  PARAMETERS  ==========================================================

CHR20LENGTH   <- 64444167  # basepairs

# UTPoster prints from 24" x 36" to 60" x 300" (which is actually quite large).
# let's assume letter size for this demo, and allow a 1" margin.
PAGEWIDTH  <- ( 8.5 - 2) * 2.54    # in cm
PAGEHEIGHT <- (11.0 - 2) * 2.54    # in cm
RESOLUTION <- 150 # pixels per 2.54 cm

SVGFILE <- "test.svg"



# =    2  PACKAGES  ============================================================
# Load all required packages.

if (!require(readr, quietly=TRUE)) {
  install.packages("readr")
  library(readr)
}


# =    3  FUNCTIONS  ===========================================================

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


ang2rad <- function(a) {
  # Purpose:
  #    Convert a rotation angle in degrees from vertical in clockwise direction
  #    to radians.
  x <- ((2 * a) / 360) * pi  # degrees to radians
  x <- -x                    # change direction of rotation
  x <- x + (pi / 2)          # add 90°
  return(x)
}


coord2circle <- function(start, end, l) {
 # Purpose:
 #     Convert start and end coordinates to a rotation angle on a unit circle of
 #     length l, also return the angle. We define 0° to be at the top, and
 #     the positive direction is clockwise.
  rot <- (mean(c(start, end)) / l) * 360
  th  <- ang2rad(rot)
   return(c(cos(th), sin(th), rot))
}


SVGheader <- function() {
  s <-    "<?xml version=\"1.0\" standalone=\"no\"?>"
  s[2] <- paste("<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"",
                "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">")
  return(s)
}


SVGdefinePage <- function(w, h) {
  s <- paste("<svg",
             sprintf("width=\"%f\"",  w),
             sprintf("height=\"%f\"", h),
             sprintf("viewbox=\"0 0 %f %f\"", w, h),
             "version=\"1.1\"",
             "xmlns=\"http://www.w3.org/2000/svg\">",
             collapse = " ")
  return(s)
}


SVGdrawCircle <- function(cx, cy, r,
                          fill = "#FFFFFF",
                          stroke= "#000000",
                          sw = 1.0) {
  s <- paste( sprintf("<circle cx=\"%f\"", cx),
              sprintf("cy=\"%f\"", cy),
              sprintf("r=\"%f\"", r),
              sprintf("fill=\"%s\"", fill),
              sprintf("stroke=\"%s\"", stroke),
              sprintf("stroke-width=\"%f\" />", sw),
              collapse = " ")
  return(s)
}


SVGdrawText <- function(x, y,
                        font,
                        size,
                        fill = "#FFFFFF",
                        text) {
  s <- paste( sprintf("<text x=\"%f\"", x),
              sprintf("y=\"%f\"", y),
              "alignment-baseline=\"middle\"", # this centers the string on
              "text-anchor=\"middle\"",        # the (x, y) coordinates
              sprintf("font-family=\"%s\"", font),
              sprintf("font-size=\"%f\"", size),
              sprintf("fill=\"%s\">", fill),
              collapse = " ")
  s <- c(s, text)
  s <- c(s, "</text>")
  return(s)
}


SVGdrawLine <- function(x1, y1, x2, y2,
                        stroke = "#FFFFFF",
                        sw = 1.0) {

  s <- paste( sprintf("<line x1=\"%f\"", x1),
              sprintf("y1=\"%f\"", y1),
              sprintf("x2=\"%f\"", x2),
              sprintf("y2=\"%f\"", y2),
              sprintf("style=\"stroke:%s;", stroke),
              sprintf("stroke-width:%f\" />", sw),
              collapse = " ")
  return(s)
}


SVGdrawRect <- function(x, y, w, h, ang,
                        fill = "#FFFFFF",
                        stroke = "none",
                        sw = 0.0) {
  s <- paste( "<g transform=\"",
              sprintf("rotate(%f, %f, %f)", ang, x, y),
              "\">",
              collapse = " ")
  s <- c(s, paste( sprintf("<rect x=\"%f\"", x - (w / 2)),
                   sprintf("y=\"%f\"", y - (h / 2)),
                   sprintf("width=\"%f\"", w),
                   sprintf("height=\"%f\"", h),
                   sprintf("fill=\"%s\"", fill),
                   sprintf("stroke=\"%s\"", stroke),
                   ifelse(stroke != "none",
                          sprintf("stroke-width=\"%f\"", sw),
                          ""),
                   "/>",
                   collapse = " "))
  s <- c(s, "</g>")

  return(s)
}


st <- function(xy, s, t, yPage) {
  # simple scaling and tranlation - but could be extended to rotations,
  # perspective skews, projections etc. etc.
  # xy is a two-element vector
  # s and t are scalars
  # yPage is the height of the page
  #
  # Note that the SVG coordinate system has its origin top-left.
  xy <- s * xy                      # scale
  xy <- c(xy[1], yPage - xy[2])     # flip
  xy <- xy + c(t[1], - t[2])        # translate
  return(xy)
}


SVGrenderElement <- function(li, s, t, Y) {
  if (li$type == "circle") {
    xy <- st(li$centre, s, t, Y)
    s <- SVGdrawCircle(cx = xy[1],
                       cy = xy[2],
                       r = li$radius * s,
                       fill = li$fill,
                       stroke = li$stroke,
                       sw = li$sw)
  } else if (li$type == "text") {
    xy <- st(li$centre, s, t, Y)
    s <- SVGdrawText(x = xy[1],
                     y = xy[2],
                     font = li$font,
                     size = li$size,
                     fill = li$fill,
                     text = li$text)
  }  else if (li$type == "line") {
    xy1 <- st(li$p1, s, t, Y)
    xy2 <- st(li$p2, s, t, Y)
    s <- SVGdrawLine(x1 = xy1[1],
                     y1 = xy1[2],
                     x2 = xy2[1],
                     y2 = xy2[2],
                     stroke = li$stroke,
                     sw = li$sw)
  }  else if (li$type == "rect") {
    xy <- st(li$corner, s, t, Y)
    s <- SVGdrawRect(x = xy[1],
                     y = xy[2],
                     w = li$w * s,
                     h = li$h * s,
                     ang = li$ang,
                     fill = li$fill,
                     stroke = li$stroke,
                     sw = li$sw)
  } else {
    stop(sprintf("Unsupported SVG element type \"%s\"", li$type == "line"))
  }

  return(s)
}




# =    4  PROCESS  =============================================================

# This demo will plot genes as rectangles on a circle, color the boxes, and
# connect genes with the same GOA annotations with a line



myData <- read_tsv("chr20_data.tsv")

# ==   4.1  INITIALIZE  ========================================================

# Create a table of gene objects to draw:

N <- length(myData$sym)

# Entity annotations: date for each gene
myGenes <- data.frame(sym = myData$sym,
                      start = numeric(N),
                      end = numeric(N),
                      GOAid = myData$GOAid,
                      stringsAsFactors = FALSE)

# Relationship annotations: from <symbol> to <symbol>
myEdges <- data.frame(from = character(),
                      to = character(),
                      stringsAsFactors = FALSE)


# ==   4.2  ANNOTATE  ==========================================================

N <- nrow(myGenes)

# Entity annotations: start and end coordinates (in fractional
# coordinates of the chromosome), and GOAid
for (i in 1:N) {  # for all rows
  sel <- which(myData$sym == myGenes$sym[i])
  myGenes$GOAid[i] <- myData$GOAid[sel]
  myGenes$start[i] <- myData$start[sel]
  myGenes$end[i]   <- myData$end[sel]
}


# Relationship annotations: edges from <symbol> to <symbol>
for (i in 1:N) {  # for all rows
  thisGOA <- myGenes$GOAid[i]   # fetch the GOAid
  sel <- which(myGenes$GOAid == thisGOA) # fetch all rows that have the same GOAid
  sel <- sel[!(sel == i)] # remove self-edges
  if (length(sel) > 0) {  # if there are any
    for (j in sel) { # add an edge between the current row and all
      # others with the same GOAid
      thisRow <- nrow(myEdges) + 1
      myEdges[thisRow, 1] <- myGenes$sym[i]
      myEdges[thisRow, 2] <- myGenes$sym[j]
    }
  }
}


# ==   4.3  LAYOUT  ============================================================

# We will draw everything on a virtual canvas, centred on (0, 0) ranging from -1
# to +1 in x and y. Only later, when we output it to SVG, we will scale the
# layout to the page.

# To define global layout elements, we can e.g. describe them in a
# list-of-lists. Here are two sample elements: a circle (the chromosome
# backbone), and a piece of text (the chromosome number).
myLayoutElements <- list()
myLayoutElements[[1]] <- list(type = "circle",
                         centre = c(0, 0),
                         radius = 1.0,
                         fill = "#FFFFFF",
                         stroke = "#4499AA",
                         sw = 7.0)

myLayoutElements[[2]] <- list(type = "text",
                                text = "CHR 20",
                                centre = c(0, 0),
                                size = 48,
                                font = "Times",
                                fill = "#33AAFF")


# Per-entity layout for genes: each gene will be drawn as a colored box, placed
# on the circle and rotated appropriately. We store the (x, y) of the centre, as
# well as width and height of the rectangle, and the angle through which it
# should be rotated.
N <- nrow(myGenes)
myGenes$x   <- numeric(N)
myGenes$y   <- numeric(N)
myGenes$ang <- numeric(N)
myGenes$w   <- numeric(N)
myGenes$h   <- numeric(N)

for (i in 1:N) {
  xya <- coord2circle(myGenes$start[i], myGenes$end[i], CHR20LENGTH)
  myGenes$x[i]   <- xya[1]
  myGenes$y[i]   <- xya[2]
  myGenes$ang[i] <- xya[3]
  myGenes$w[i]   <- abs(myGenes$start[i] - myGenes$end[i]) / CHR20LENGTH
  myGenes$h[i]   <- 0.05 # 5% of the radius
}

# Per-entity layout for edges: add coordinates to the Edges
N <- nrow(myEdges)
myEdges$x1 <- numeric(N)
myEdges$y1 <- numeric(N)
myEdges$x2 <- numeric(N)
myEdges$y2 <- numeric(N)

for (i in 1:N) {
  j <- which(myEdges$from[i] == myGenes$sym)
  k <- which(myEdges$to[i]   == myGenes$sym)
  myEdges$x1[i] <- myGenes$x[j]
  myEdges$y1[i] <- myGenes$y[j]
  myEdges$x2[i] <- myGenes$x[k]
  myEdges$y2[i] <- myGenes$y[k]
}


# ==   4.4  OPTIMIZE AESTHETICS  ===============================================

# Add color values to the gene annotations
N <- nrow(myGenes)
nCol <- length(unique(myGenes$GOAid))

myGenes$col <- character(N)
myGOAs <- sort(unique(myGenes$GOAid))

for (i in 1:N) {
  myGenes$col[i] <- rainbow(nCol)[which(myGenes$GOAid[i] == myGOAs)]
}

# Add color values from gene to the edges
N <- nrow(myEdges)
myEdges$col <- character(N)

for (i in 1:N) {
  idx <- which(myGenes$sym == myEdges$from[i])
  myEdges$col[i] <- myGenes$col[idx]
}

# Encode information in edge thickness (stroke-width). We could also
# add an alpha channel to the edge color and make edges transparent. Just
# append a two digit hex value to the color, where 00 is fully transparent
# and FF is fully opaque. Eg. "#FF00007F" is a 50% transparent red.
#
# Just to demonstrate, we'll draw frequently observed GOAids with a thin line
# (0.05) and rarely observed GOAids with a thick line (0.5).

# Make a table of GOAid frequencies, log() the values
GOAtable <- log(table(myGenes$GOAid))
# Scale them to [1, 0]
GOAtable <- 1 - ((GOAtable - min(GOAtable)) / (max(GOAtable) - min(GOAtable)))
# Scale the result to [0.05, 0.5]
GOAtable <- (GOAtable * (0.5 - 0.05)) + 0.05

N <- nrow(myEdges)
myEdges$sw <- numeric(N)

for (i in 1:N) {
  idx <- which(myGenes$sym == myEdges$from[i])
  myEdges$sw[i] <- GOAtable[myGenes$GOAid[idx]]
}



# ==   4.5  CREATE SVG  ========================================================
# cf. https://www.w3.org/TR/SVG

# ===  4.5.1  Compute scale and translation

# Caution: the SVG coordinate system has its origin (0, 0) in the TOP LEFT
# corner, positive X goes right, and positive Y goes down. Our
# transformation routine takes care of this.

dX <- range(myGenes$x)[2] - range(myGenes$x)[1] # range in cm
dY <- range(myGenes$y)[2] - range(myGenes$y)[1] #

sXY <- min((RESOLUTION * (PAGEWIDTH / 2.54)) / dX, # scale: unit circle (in cm)
         (RESOLUTION * (PAGEHEIGHT / 2.54)) / dY)  # to fill page (in pixel)

sXY <- sXY * 0.95 # tweak it a bit smaller to allow for stroke widths

Xpx <- RESOLUTION * (PAGEWIDTH  / 2.54)
Ypx <- RESOLUTION * (PAGEHEIGHT / 2.54)

tXY <- c(Xpx / 2, Ypx / 2)  # translate




# ===  4.5.2  Write SVG header
mySVG <- SVGheader()
mySVG <- c(mySVG, SVGdefinePage(Xpx, Ypx))


# ===  4.5.3  Render global layout elements
for (i in 1:length(myLayoutElements)) {
   mySVG <- c(mySVG, SVGrenderElement(myLayoutElements[[i]],
                                      s = sXY,
                                      t = tXY,
                                      Y = Ypx))
}


# ===  4.5.4  Render edges
for (i in 1:nrow(myEdges)) {
  mySVG <- c(mySVG, SVGrenderElement(list(type = "line",
                                          p1 = c(myEdges$x1[i], myEdges$y1[i]),
                                          p2 = c(myEdges$x2[i], myEdges$y2[i]),
                                          stroke = myEdges$col[i],
                                          sw = myEdges$sw[i]),
                                     s = sXY,
                                     t = tXY,
                                     Y = Ypx))
}

# ===  4.5.5  Render genes
for (i in 1:nrow(myGenes)) {
  mySVG <- c(mySVG, SVGrenderElement(list(type = "rect",
                                          corner = c(myGenes$x[i], myGenes$y[i]),
                                          w = myGenes$w[i],
                                          h = myGenes$h[i],
                                          ang = myGenes$ang[i],
                                          fill = myGenes$col[i],
                                # At this scale, a typical gene is about a
                                # hair's width. We "stroke" the rectangle, to
                                # give it a minimum width for visibility.
                                          stroke = myGenes$col[i],
                                          sw = 0.5),
                                     s = sXY,
                                     t = tXY,
                                     Y = Ypx))
}






# ===  4.5.6  Write SVG footer
mySVG <- c(mySVG, "</svg>")


# ==   4.6  WRITE SVG TO FILE AND VIEW  ========================================

writeLines(mySVG, con = SVGFILE)

# Visualize
system(sprintf("open -a \"Google Chrome\" %s", SVGFILE))   # For MacOS
# Windows ???
# Linux ???



# ====  TESTS  =================================================================
# ...





# [END]
