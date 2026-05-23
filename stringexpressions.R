#STRING PARSING

#load libraries to deal with pdfs and add more string functions to our arsenal
library(tidyverse,pdftools,purr)



thefile="https://www.govinfo.gov/content/pkg/USCOURTS-ca4-18-01886/pdf/USCOURTS-ca4-18-01886-0.pdf"
thecase=pdf_text(thefile)

thecase

# clear console 
cat("\014")

#explore via split
str_split(thecase,"Before")


#get our slice of interest on page 1, section 2
judgestring<- str_split(thecase, "Before ")[[1]][2]

cat("\014")

judgestring

#Let's split it one more time
judgestring2<-str_split(judgestring, "\n")

cat("\014")

judgestring2

#and now get the vector that has our data
judgestring3 <- judgestring2[[1]][1]

cat("\014")

judgestring3

#let's clean this up to get just the judgesnames

#gsub(search,replace,fromwhat)

judgestring4<- gsub("Chief|Judge|Circuit|Judges|and|\\.| ","",judgestring3)

cat("\014")

judgestring4

judgestring5 <- gsub(",,", ",", judgestring4)

cat("\014")

judgestring5

judgestring6<-str_remove(judgestring5, ",$")

cat("\014")

judgestring6

names_to_check <- str_split_1(judgestring6, ",")

cat("\014")

names_to_check

#calculate the second positions

second_positions <- names_to_check %>% 
  
  # Give each element of the vector a 'name' attribute identical to its value.
  # This ensures our final output retains the judge names as labels.
  set_names() %>% 
  
  # Loop through each name (.x) and return a numeric value (a double) for each
  map_dbl(~ {
    
    # Search the document (thecase) for the current name, ignoring uppercase/lowercase.
    # [[1]] grabs the vector of character match positions from gregexpr's list output.
    pos <- gregexpr(.x, thecase, ignore.case = TRUE)[[1]]
    
    # If the name appears 2 or more times, extract the number at index 2 (the 2nd appearance).
    # Otherwise, if it appears 0 or 1 times, assign an NA so the code doesn't crash.
    if (length(pos) >= 2) pos[2] else NA
  })

# Look at the resulting numeric values, find the smallest number (the earliest position), 
#    and extract the text label associated with that position.
earliest_name <- names(second_positions)[which.min(second_positions)]

