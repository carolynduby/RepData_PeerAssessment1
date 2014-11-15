gen_html <- function() {
    library(knitr)
    base_name <- "PA1_template"
    knit2html(paste(base_name, ".Rmd", sep=""))
    browseURL(paste(base_name, ".html", sep=""))
}