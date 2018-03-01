# genomePlotFunctions.R
#
# Functions for genome plot starter code.
#
# License: GPL-3 (https://www.gnu.org/licenses/gpl-3.0.en.html)
#
# Author:  where not otherwise stated:
#          Boris Steipe <boris.steipe@utoronto.ca>
# Notes:
#
# ==============================================================================
#


#TOC> ==========================================================================
#TOC>
#TOC>   Section  Title                                      Line
#TOC> ----------------------------------------------------------
#TOC>   1        PACKAGES                                     38
#TOC>   2        FUNCTIONS TO INITIALIZE DATA STRUCTURES      46
#TOC>   3        ANNOTATION FUNCTIONS                         49
#TOC>   4        LAYOUT FUNCTIONS                             52
#TOC>   4.1      ang2rad()                                    54
#TOC>   4.2      coord2circle()                               84
#TOC>   5        PLOTTING FUNCTIONS                          114
#TOC>   5.1      SVGheader()                                 116
#TOC>   5.2      SVGdefinePage()                             142
#TOC>   5.3      SVGdrawCircle()                             173
#TOC>   5.4      SVGdrawText()                               207
#TOC>   5.5      SVGdrawLine()                               245
#TOC>   5.6      SVGdrawRect()                               278
#TOC>   5.7      st()                                        322
#TOC>   5.8      SVGrenderElement()                          356
#TOC>
#TOC> ==========================================================================


# =    1  PACKAGES  ============================================================

if (!require(readr, quietly=TRUE)) {
  install.packages("readr")
  library(readr)
}


# =    2  FUNCTIONS TO INITIALIZE DATA STRUCTURES  =============================


# =    3  ANNOTATION FUNCTIONS  ================================================

linMap <- function(x, low, high) {
  # map a vector of numbers x to values between min and max.

  d <- high - low
  x <- x  - min(x)
  x <- x * (d / max(x))
  x <- x + low
  return(x)
}



# =    4  LAYOUT FUNCTIONS  ====================================================


category2colour <- function(Cs,
                            col,
                            B = 1,            # bias
                            I = "spline",     # interpolate
                            A = FALSE) {      # use alpha channel too
  # Description:
  #    Define a named vector of colours for the categories in Cs according to
  #    the colour palette in pal.
  #
  # Arguments:
  #     Cs:          a vector items that can be used as rownames
  #     pal:  char   a palette of colour values
  #     B:    num    see bias in colorRampPalette() help
  #     I:    char   <"linear", "spline">
  #     A:    bool   return alpha channel values or not
  #
  # Details:
  #     Cs can have any atomic type. If pal has only one element, all the
  #     colours are the same. Two elements will create a gradient, more
  #     then two elements will make a colour spectrum. The other parameters
  #     have reasonable defaults but can be adjusted.
  #
  # Value:
  #     A named character vector of colour values, where the names are
  #     the elements of Cs.
  #
  # Notes:
  #     <...>
  #

  fCol <- colorRampPalette(colors = col,
                           bias = B,
                           interpolate = I,
                           alpha = A)

  cV <- fCol(length(Cs))
  names(cV) <- Cs

  return(cV)
}





# ==   4.1  ang2rad()  =========================================================
ang2rad <- function(a) {
  # Purpose:
  #    Convert a rotation angle in degrees from vertical in clockwise direction
  #    to radians.
  #
  # Description:
  #     <describe purpose...>
  #
  # Arguments:
  #     <name>:   <type>   <description
  #
  # Details:
  #     <description, notes, see-also ...>
  #
  # Value:
  #     <type, structure etc. of the single return value. Or:
  #     NA - function is invoked for its side effect of ... <describe>. >
  #
  # Notes:
  #     <...>
  #


  x <- ((2 * a) / 360) * pi  # degrees to radians
  x <- -x                    # change direction of rotation
  x <- x + (pi / 2)          # add 90°
  return(x)
}

# ==   4.2  coord2circle()  ====================================================
coord2circle <- function(coord, l, ori, rad) {
  # Purpose:
  #     Convert linear coordinates on a line of length l to
  #     positions on a circle centred on ori with radius rad. Return the
  #     coordinates and the rotation angle. We define 0° to be at the top, and
  #     the positive direction is clockwise.
  #
  # Description:
  #     <describe purpose...>
  #
  # Arguments:
  #     <name>:   <type>   <description
  #
  # Details:
  #     <description, notes, see-also ...>
  #
  # Value:
  #     <type, structure etc. of the single return value. Or:
  #     NA - function is invoked for its side effect of ... <describe>. >
  #
  # Notes:
  #     <...>
  #

  rot <- (coord / l) * 360
  th  <- ang2rad(rot)
  x <- (cos(th) * rad) + ori[1]
  y <- (sin(th) * rad) + ori[2]

  return(c(x, y, rot))
}


# =    5  PLOTTING FUNCTIONS  ==================================================

# ==   5.1  SVGheader()  =======================================================
SVGheader <- function() {
  #
  # Description:
  #     <describe purpose...>
  #
  # Arguments:
  #     <name>:   <type>   <description
  #
  # Details:
  #     <description, notes, see-also ...>
  #
  # Value:
  #     <type, structure etc. of the single return value. Or:
  #     NA - function is invoked for its side effect of ... <describe>. >
  #
  # Notes:
  #     <...>
  #

  s <-    "<?xml version=\"1.0\" standalone=\"no\"?>"
  s[2] <- paste("<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"",
                "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">")
  return(s)
}

# ==   5.2  SVGdefinePage()  ===================================================
SVGdefinePage <- function(w, h) {
  #
  # Description:
  #     <describe purpose...>
  #
  # Arguments:
  #     <name>:   <type>   <description
  #
  # Details:
  #     <description, notes, see-also ...>
  #
  # Value:
  #     <type, structure etc. of the single return value. Or:
  #     NA - function is invoked for its side effect of ... <describe>. >
  #
  # Notes:
  #     <...>
  #

  s <- paste("<svg",
             sprintf("width=\"%f\"",  w),
             sprintf("height=\"%f\"", h),
             sprintf("viewbox=\"0 0 %f %f\"", w, h),
             "version=\"1.1\"",
             "xmlns=\"http://www.w3.org/2000/svg\">",
             collapse = " ")
  return(s)
}


# ==   5.3  SVGdrawCircle()  ===================================================
SVGdrawCircle <- function(cx, cy, r,
                          fill = "#FFFFFF",
                          stroke= "#000000",
                          sw = 1.0) {
  #
  # Description:
  #     <describe purpose...>
  #
  # Arguments:
  #     <name>:   <type>   <description
  #
  # Details:
  #     <description, notes, see-also ...>
  #
  # Value:
  #     <type, structure etc. of the single return value. Or:
  #     NA - function is invoked for its side effect of ... <describe>. >
  #
  # Notes:
  #     <...>
  #

  s <- paste( sprintf("<circle cx=\"%f\"", cx),
              sprintf("cy=\"%f\"", cy),
              sprintf("r=\"%f\"", r),
              sprintf("fill=\"%s\"", fill),
              sprintf("stroke=\"%s\"", stroke),
              sprintf("stroke-width=\"%f\" />", sw),
              collapse = " ")
  return(s)
}


# ==   5.4  SVGdrawText()  =====================================================
SVGdrawText <- function(x, y,
                        font,
                        size,
                        fill = "#FFFFFF",
                        text) {
  #
  # Description:
  #     <describe purpose...>
  #
  # Arguments:
  #     <name>:   <type>   <description
  #
  # Details:
  #     <description, notes, see-also ...>
  #
  # Value:
  #     <type, structure etc. of the single return value. Or:
  #     NA - function is invoked for its side effect of ... <describe>. >
  #
  # Notes:
  #     <...>
  #

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


# ==   5.5  SVGdrawLine()  =====================================================
SVGdrawLine <- function(x1, y1, x2, y2,
                        stroke = "#FFFFFF",
                        sw = 1.0) {
  #
  # Description:
  #     <describe purpose...>
  #
  # Arguments:
  #     <name>:   <type>   <description
  #
  # Details:
  #     <description, notes, see-also ...>
  #
  # Value:
  #     <type, structure etc. of the single return value. Or:
  #     NA - function is invoked for its side effect of ... <describe>. >
  #
  # Notes:
  #     <...>
  #

  s <- paste( sprintf("<line x1=\"%f\"", x1),
              sprintf("y1=\"%f\"", y1),
              sprintf("x2=\"%f\"", x2),
              sprintf("y2=\"%f\"", y2),
              sprintf("style=\"stroke:%s;", stroke),
              sprintf("stroke-width:%f\" />", sw),
              collapse = " ")
  return(s)
}


# ==   5.6  SVGdrawRect()  =====================================================
SVGdrawRect <- function(x, y, w, h, ang,
                        fill = "#FFFFFF",
                        stroke = "none",
                        sw = 0.0) {
  #
  # Description:
  #     <describe purpose...>
  #
  # Arguments:
  #     <name>:   <type>   <description
  #
  # Details:
  #     <description, notes, see-also ...>
  #
  # Value:
  #     <type, structure etc. of the single return value. Or:
  #     NA - function is invoked for its side effect of ... <describe>. >
  #
  # Notes:
  #     <...>
  #

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


# ==   5.7  st()  ==============================================================
st <- function(xy, s, t, yPage) {
  # simple scaling and tranlation - but could be extended to rotations,
  # perspective skews, projections etc. etc.
  # xy is a two-element vector
  # s and t are scalars
  # yPage is the height of the page
  #
  # Note that the SVG coordinate system has its origin top-left.
  # #
  # Description:
  #     <describe purpose...>
  #
  # Arguments:
  #     <name>:   <type>   <description
  #
  # Details:
  #     <description, notes, see-also ...>
  #
  # Value:
  #     <type, structure etc. of the single return value. Or:
  #     NA - function is invoked for its side effect of ... <describe>. >
  #
  # Notes:
  #     <...>
  #

  xy <- s * xy                      # scale
  xy <- c(xy[1], yPage - xy[2])     # flip
  xy <- xy + c(t[1], - t[2])        # translate
  return(xy)
}


# ==   5.8  SVGrenderElement()  ================================================
#
# Description:
#     <describe purpose...>
#
# Arguments:
#     <name>:   <type>   <description
#
# Details:
#     <description, notes, see-also ...>
#
# Value:
#     <type, structure etc. of the single return value. Or:
#     NA - function is invoked for its side effect of ... <describe>. >
#
# Notes:
#     <...>
#
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
    xCorner <- li$centre[1] - (li$w / 2)
    yCorner <- li$centre[2] - (li$h / 2)
    xy <- st(c(xCorner, yCorner), s, t, Y)
    s <- SVGdrawRect(x = xy[1],
                     y = xy[2],
                     w = li$w * s,
                     h = li$h * s,
                     ang = li$ang,
                     fill = li$fill,
                     stroke = li$stroke,
                     sw = li$sw)
  } else {
    stop(sprintf("Unsupported SVG element type \"%s\"", li$type))
  }

  return(s)
}












# [END]

