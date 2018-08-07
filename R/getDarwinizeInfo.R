getDarwinzeInfo <- function(path = "http://tdwg.github.io/dwc/terms/") {
    dRaw <- readLines(path)
    dRaw <- grep("Term Name:", dRaw, value = TRUE)
    foo <- gsub(".*Term Name: dcterms:([A-z]+).*", "\\1", dRaw)
    foo <- gsub(".*Term Name: ([A-z]+).*", "\\1", foo)
    bar <- gsub(".*<TD>Definition:</TD><TD>(.*)</TD></TR>.*<TR><TD>Comment:.*", "\\1", dRaw)
    bar <- gsub('"', "", bar)
    bar <- gsub("'", "", bar)
    data.frame(name = foo, definition = bar, stringsAsFactors = FALSE)
}