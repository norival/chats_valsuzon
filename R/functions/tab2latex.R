################################################################################
# file          : functions/tab2latex.R
# subject       : stage m1 2016
# description   : convert R data.frame into a latex tabularx environment
# arguments     : x           = data.frame to convert
#                 fileName    = path to tex file
#                 showColumns = wether or not show colnames
#                 col.names   = names to print for columns
#                 boldNames   = whether or not colnames are bold
#                 showRows    = wether or not show rownames
#                 naString    = string to pass instead of NA's
#                 aligne      = alignement of columns
#                 caption     = Caption. None if not defined
#                 label       = Label. None if not defined
#                 float       = Boolean: make a table environment ?
#                 round       = Vector: round the content of the column
#                                   The first number is the column
#                                   The second is the number of digits
#                                   It can be repeated
#                                   If just one number, all the columns are
#                                   rounded to that umber
# return value  : none. Save ouptut in the file specified with 'fileName'
################################################################################


tab2latex <- function(
  x,
  fileName    = "tablatex.tex",
  showColumns = TRUE,
  col.names   = "default",
  boldNames   = TRUE,
  showRows    = FALSE,
  naString    = "NA",
  align       = "X",
  caption     = "",
  label       = "",
  float       = FALSE,
  round       = c(-1),
  header      = TRUE,
  con.mod     = "w",
  group.col   = NA,
  group.mod   = "skip"
  )
{
  is.odd <- function(x) {
    x %% 2 != 0
  }

  if (showRows == TRUE) {
    nColumns = length(x) + 1
  } else {
    nColumns = length(x)
  }

  if (round[1] != -1) {
    if (length(round) == 1) {
      for(i in 1:length(x)) {
        if (is.numeric(x[,i])) {
          x[i] <- round(x[i], digits = round[1]);
        }
      }
    } else {
      for(i in (1:length(x))[which(is.odd(1:length(x)))]) {
        x[round[i]] <- round(x[round[i]], digits = round[i+1])
      }
    }
  }

  con <- file(description = fileName, open = con.mod)

  if(header == TRUE) {
    if (float == TRUE) {
      cat("\\begin{table}\n", file = con)
    }
    if (caption != "") {
      cat("\\caption{", caption, "}\n", sep = "", file = con)
    }
    if (label != "") {
      cat("\\label{", label, "}\n", sep = "", file = con)
    }

    cat("\\begin{tabularx}{\\textwidth}", file = con)

    if(length(align) == 1) {
      cat("{", rep(x = align, length(align)), "}\n", sep = "", file = con)
    } else {
      cat("{", align, "}\n", sep = "", file = con)
    }

    cat("\\hlinewd{1pt}\n", file = con)
  }

  if(col.names[1] == "default") {
    col.names <- colnames(x)
  }

  if(boldNames == TRUE) {
      col.names <- paste("\\textbf{", col.names, "}", sep = "")
  }

  if (showRows == TRUE & showColumns == TRUE) {
    cat(" ", col.names, sep = " & ", file = con)
    cat(" \\\\\n\\hline\n", file = con)
  } else {
    cat(col.names, sep = " & ", file = con)
    cat(" \\\\\n\\hline\n", file = con)
  }

  if(!is.na(group.col)) {
    group.delim <- switch(group.mod,
                          skip = "\\noalign{\\smallskip}\n",
                          line = "\\hline\n",
                          both = "\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")

    tmp_lev <- as.factor(x[[group.col]])
    ii <- 1

    for(i in levels(tmp_lev)) {
      write.table(x         = x[which(x == i),],
                  file      = con,
                  na        = naString,
                  col.names = FALSE,
                  row.names = showRows,
                  sep       = " & ",
                  quote     = FALSE,
                  eol       = " \\\\\n")

      if(ii < length(tmp_lev)) {
        cat(group.delim, file = con)
      }
    }
  } else {
    write.table(x         = x,
                file      = con,
                na        = naString,
                col.names = FALSE,
                row.names = showRows,
                sep       = " & ",
                quote     = FALSE,
                eol       = " \\\\\n")
  }

cat("\\hlinewd{1pt}\n", file = con)
  cat("\\end{tabularx}\n", file = con)

  if (float == TRUE) {
    cat("\\end{table}\n", file = con)
  }

  close(con)
}

################################################################################
# tab2latex.cbind function
################################################################################

tab2latex.cbind <- function(x1,
                            x2,
                            align         = "X",
                            fileName      = "tablatex.tex",
                            x1.name       = "tab1",
                            x2.name       = "tab2",
                            col.names.x1  = "default",
                            col.names.x2  = "default")
{

  x <- cbind.data.frame(x1, x2)

  con <- file(description = fileName, open = "w")

  cat("\\begin{tabularx}{\\textwidth}", file = con)

  if(length(align) == 1) {
    cat("{", rep(x = align, length(x)), "}\n", sep = "", file = con)
  } else {
    cat("{", align, "}\n", sep = "", file = con)
  }

  cat("\\hlinewd{1pt}\n", file = con)

  cat("\\multicolumn{", length(x1), "}{X}{\\textbf{", x1.name, "}} &", sep = "",
      file = con)
  cat("\\multicolumn{", length(x2), "}{X}{\\textbf{", x2.name, "}} \\\\\n", sep = "",
      file = con)

  if(col.names.x1 != "default" & col.names.x2 != "default") {
    colNames <- c(col.names.x1, col.names.x2)
  } else {
    colNames <- "default"
  }


  close(con)

  tab2latex(x, header = FALSE, fileName = fileName,
            col.names = colNames,
            con.mod = "a")

}

################################################################################
# as.math function
################################################################################

as.math <- function(x, del = TRUE) {

  if(del == TRUE){
    x <- paste("$", as.character(x), "$", sep = "")
  } else {
    x <- as.character(x)
  }

  x <- gsub(".", ",", x, fixed = TRUE)

  return(x)
}

################################################################################
# p_parse function
################################################################################

p_parse <- function(p,
                    sign_color = "black",
                    sign_face = "normal",
                    sign_alpha = 0.05)
{
  if(p < 0.001) {
    p <- "<0.001"
  } else {
    p <- round(p, 3)
  }

  if(p < sign_alpha | p == "<0.001") {
    p <- switch(sign_face,
                normal = paste("\\mathrm{", p, "}", sep = ""),
                bold   = paste("\\bm{", p, "}", sep = ""),
                it     = paste("\\mathit{", p, "}", sep = ""))

    p <- as.math(p)
    p <- paste("\\textcolor{", sign_color, "}{", p, "}", sep = "")
  } else {
    p <- as.math(p)
  }

  return(p)
}

################################################################################
# tab2latex.cbind4
################################################################################

tab2latex.cbind4 <- function(x1,
                             x2,
                             x3,
                             x4,
                             lscape       = TRUE,
                             common.col    = c(0),
                             common.colname= "Common",
                             align         = "X",
                             fileName      = "tablatex.tex",
                             x1.name       = "tab1",
                             x2.name       = "tab2",
                             x3.name       = "tab3",
                             x4.name       = "tab4",
                             col.names.x1  = "default",
                             col.names.x2  = "default",
                             col.names.x3  = "default",
                             col.names.x4  = "default")
{

  if(common.col == 0) { 
    x <- cbind.data.frame(x1, x2, x3, x4)
  } else {
    x <- cbind.data.frame(x1[common.col],
                          x1[-common.col],
                          x2[-common.col],
                          x3[-common.col],
                          x4[-common.col])
  }

  con <- file(description = fileName, open = "w")

  if(lscape) {
    cat("\\begin{tabularx}{\\linewidth}", file = con)
  } else {
    cat("\\begin{tabularx}{\\textwidth}", file = con)
  }

  if(length(align) == 1) {
    cat("{", rep(x = align, length(x)), "}\n", sep = "", file = con)
  } else {
    cat("{", align, "}\n", sep = "", file = con)
  }

  cat("\\hlinewd{1pt}\n", file = con)

  if(common.col != 0) {
    cat(rep("&", length(common.col)), sep = "", file = con)
  }

  cat("\\multicolumn{", length(x1)-1, "}{X}{\\textbf{", x1.name, "}} &", sep = "",
      file = con)
  cat("\\multicolumn{", length(x2)-1, "}{X}{\\textbf{", x2.name, "}} &", sep = "",
      file = con)
  cat("\\multicolumn{", length(x3)-1, "}{X}{\\textbf{", x3.name, "}} &", sep = "",
      file = con)
  cat("\\multicolumn{", length(x4)-1, "}{X}{\\textbf{", x4.name, "}} \\\\\n", sep = "",
      file = con)

  colNames <- c(common.colname, col.names.x1, col.names.x2, col.names.x3, col.names.x4)

  close(con)

  tab2latex(x, header = FALSE, fileName = fileName,
            col.names = colNames,
            con.mod = "a")

}

################################################################################
# export_value function
################################################################################

export_value <- function(x,
                         name,
                         digits = 2)
{
  x <- as.math(round(x, digits), del = FALSE)
  x <- paste("\\DeclareRobustCommand{\\",
             name,
             "}{",
             x,
             "}\n",
             sep = "")
  return(x)
}
