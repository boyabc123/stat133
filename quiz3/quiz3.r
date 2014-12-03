here=T
# Quiz 3
# Number of Problems: 6
# The quiz is out of 22 points.

# Function 1 (3 points)
# Write a function called numAtElements. Your function should take the following
# arguments
#   <chvec>: A character vector containing strings of letters, possibly possibly
#     with the "@" symbol
#
# and return the following
#   <num.at>: an integer indicating how many elements of <chvec> contain the "@"
#     symbol. For example: numAtElements(c('karl', 'k@rl', '@@@')) should return 2
numAtElements <- function(chvec){
  length(grep('@',chvec))
  # your code here
}

# Function 2 (3 points)
# Write a function called unexclaim. Your function should take the following
# arguments
#   <chstring>: a character vector of length 1 (contains only one string).
#
# and return the following
#   <newstring>: a character vector of length 1 where all ! symbols have been
#     replaced by . symbols
unexclaim <- function(chstring) {
  gsub('!','.',chstring)
  # your code here
}

# Function 3 (3 points)
# Write a function called updateDate. Your function should take the following
# arguments
#   <dates>: a character vector of dates of the form "month, year" (e.g. "May, 2001")
#   <old.yr>: a string indicating the year for which elements will be updated
#     (e.g. "2002")
#
# and return the following
#   <updated.dates>: a character vector of dates where <old.yr> has been replaced by
#     '2015'. This vector should only contain the elements whose date has been
#     updated. For example updateDate(c('May, 2010', 'June, 2011'), '2010') should
#     return 'May, 2015'.
updateDate <- function(dates, old.yr) {
  gsub(old.yr,'2015',dates[grep(old.yr,dates)])
  # your code here
}

# Function 4 (4 points)
# Write a function called countcatdog that counts the number of instances of
# "cat" (case-insensitive) and the number of instances of "dog"
# (case-insensitive) in a string. Your function should take the following
# arguments 
#   <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and return the following
#   <counts>: An integer vector of length 2 with names "cat" and "dog".
#             For example, countcatdog("doGCATcat abcAt") returns:
#                    cat dog
#                     3   1
countcatdog<- function(chvec){
  temp = tolower(chvec)
  temp = strsplit(temp,'')
  temp = unlist(temp)
  n = length(temp)
  cat=0
  dog=0
  for (i in 1:n-2){
    if ((temp[i]=='c')&&(temp[i+1]=='a')&&(temp[i+2]=='t'))
      {cat = cat+1}
    if ((temp[i]=='d')&&(temp[i+1]=='o')&&(temp[i+2]=='g'))
    {dog = dog+1}
  }
  k=c(cat,dog)
  names(k)=c('cat','dog')
  return(k)
}

# Function 5 (3 points)
# Write a function called sumDigits that compute the sum of all the digits in
# a string.  The function should return 0 if there is no digit in the
# string. Your function should take the following arguments:
#   <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and return the following
#   <total>: A single number (the sum of all the digits in chvec)

sumDigits <- function(chvec){
temp = gsub('[^0-9]','',chvec)
temp = strsplit(temp,'')
sum(as.numeric(unlist(temp)))
  # your code here
}

# Some test cases:
#all.equal(sumDigits("1z3p ! 21"), 7)
#all.equal(sumDigits("abcdefg"), 0)


# Function 6 (6 points)
# DNA.vec is a character vector of strings of DNA. It contains at least two
# strings of DNA. For simplicity, each string has only 10 characters.  Note that
# a DNA is always made up of A, T, C, G's. Write a function called dnaTransform
# that performs the following:

# Step 1: Find the first two DNA strings in DNA.vec that contains the sequence
# "ATTA"; call them DNA1 and DNA2. If there are less than two DNA strings that
# contains the sequence "ATTA", the function ends immediately and returns the
# first two elements in DNA.vec

# Step 2: Split DNA1 into two halves (i.e. strings of length 5 each).
# (For example, if DNA1 is "ATTATAGCCA", then we have "ATTAT" as the first half
# and "AGCCA" as the second half)

# Step 3: Split DNA2 into two halves (as in Step 2).

# Step 4: Return a character vector of two strings:
# --first string: the first half of DNA1 combined with the second half of DNA2
# --second string: the first half of DNA2 combined with the second half of DNA1

# Input:
#   <DNA_vec>: A character vector of DNAs
# Output:
#   <DNA_final>: A character vector of two DNAs

dnaTransform <- function(DNA.vec){
    a = grep('ATTA',DNA.vec)
    if (length(a)<2){
      DNA.vec[1:2]
    }
    else{
    b = DNA.vec[a]
    c = unlist(strsplit(b,' '))
    DNA1 = c[1]
    DNA2 = c[2]
    DNA1_1 = substr(DNA1,1,5)
    DNA1_2 = substr(DNA1,6,10)
    
    DNA2_1 = substr(DNA2,1,5)
    DNA2_2 = substr(DNA2,6,10)
    
    a = paste0(DNA1_1,DNA2_2)
    b = paste0(DNA2_1,DNA1_2)
    c = c(a,b)
    }
  # your code here
}

# Some test cases:
#all.equal(dnaTransform(c("AAAAAAAAAA", "ATTAGATACT", "ATACATTACG")), c("ATTAGTTACG", "ATACAATACT"))
#all.equal(dnaTransform(c("ATCGATCGAT", "TCGATCGATT", "ATTTTTTTTT")), c("ATCGATCGAT", "TCGATCGATT"))

# End of Quiz 3
