#!/usr/bin/env Rscript

if(! "librarian" %in% rownames(installed.packages())) {
  install.packages("librarian", repos = "http://cran.us.r-project.org")
}

# libs --------------------------------------------------------------------
librarian::shelf(rstudioapi,
                 renv,
                 ggplot2,
                 ggcorrplot,
                 stringr,
                 olsrr,
                 cran_repo = 'https://cran.r-project.org')

#renv::clean()
setwd(dirname(getSourceEditorContext()$path))

d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)

# setdiff(names(d1), names(d2))
d3=merge(d1,d2, by=c("school",
                    "sex",
                    "age",
                    "address",
                    "famsize",
                    "Pstatus",
                    "Medu",
                    "Fedu",
                    "Mjob",
                    "Fjob",
                    "reason",
                    "nursery",
                    "internet"
                    ))

names(d1)
jc<-names(d1[,-c(15,18,30:33)])

d3=merge(d1,d2, by=jc, suffixes = c(".math", ".portugese"))
print(nrow(d3)) # 382 students

d3
write.csv(d3,"student-data.csv", row.names = FALSE)