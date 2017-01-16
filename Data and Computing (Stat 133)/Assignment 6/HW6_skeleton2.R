#######################################
# HOMEWORK 6, STAT 133 F14
# YOUR  NAME: Knopf, Michael
#######################################

# load(url("http://www.stat.berkeley.edu/~nolan/stat133/data/Emails.rda"))
Sys.setlocale('LC_ALL','C')

# Produce summary statistics for a single e-mail 
spamCounts = function(subject, body){
  # assume subject is a character string
  # assume body is a character vector, where each element is a line of body text
  
  # the number of exclamation marks in the subject of the e-mail
  num_exclam = length(regmatches(subject,gregexpr("!",subject))[[1]])
  
  body = paste(body, collapse = " ")
  
  # the proportion of uppercase characters in the body or subject
  frac_upper_body =
    length(regmatches(body,gregexpr("[[:upper:]]", body))[[1]]) / length(regmatches(body,gregexpr("[[:alpha:]]", body))[[1]])
  
  # the proportion of lowercase characters in the body or subject
  frac_upper_sub =
    length(regmatches(subject,gregexpr("[[:upper:]]", subject))[[1]]) / length(regmatches(subject,gregexpr("[[:alpha:]]", subject))[[1]])
  
  # return a vector with num_exclam, frac_upper_body, frac_upper_sub
  # (in that order)
  
  return(c(num_exclam, frac_upper_body, frac_upper_sub))
}



# Check if the Reply-To e-mail contains an underscore
spamReplyTo = function(reply_to){
  # assume reply_to is a character string
  
  # logical, TRUE if reply_to contains an underscore
  hasUnderscore = grepl("_", reply_to)
  
  # return hasUnderscore
  return (hasUnderscore)
}


# Check if the subject contains mixed characters, like V1agra or Cia1is
spamSubject1 = function(subject){
  # assume subject is a character string
  
  # hasMixedChar is TRUE if subject contains punctuation or digits
  # surrounded by characters
  
  ### Remove apostrophes
  subject = gsub("'", "", subject)
  
  hasMixedChar = grepl("[[:alpha:]][[:punct:][:digit:]][[:alpha:]]", subject)
  
  # return hasMixedChar
  return(hasMixedChar)
}



# Check if the subject is on a different topic than the body
spamSubject2 = function(subject, body){
  # assume subject is a character string
  # assume body is a character vector, where each element is a line of body text
  
  ### Check that subject has the form "Re: blah"
  if (grepl("[[:blank:]]*[Rr][Ee][[:blank:]]*:", subject) != 1) return (NA)
  
  subject = sub("[[:blank:]]*[Rr][Ee][[:blank:]]*:[[:blank:]]*", "", subject)
  
  ### Find characers that require double backslashes and insert them
  for (ch in c("\\", ".", "^", "$", "+", "?", "(", ")", "[", "]", "{", "}", "|"))
  {
    subject = gsub(ch, paste("\\", ch, sep = ""), fixed=TRUE, subject)
  }
  
  subject = paste("(^|[[:blank:]]*)", subject, "($|[^[:alnum:]@])", sep = "")
  
  # isOffTopic is TRUE if the subject has the form "Re: blah" and 
  # "blah" is NOT in the body text
  isOffTopic = !grepl(subject, paste(body, collapse = ""), ignore.case = TRUE)
  
  # return isOffTopic
  return(isOffTopic)
}



#######################################
# Put all of these functions together to build a basic spam filter

spamFilter = function(email, max_exclam = 10**5, max_up_body = 1, max_up_sub = 1){
  # email is a list with at least two components, header and body (as
  # described in the assignment PDF, or Emails[[1]] for an example)  
  # max_exclam is the upper threshold for the number of exclamation marks
  # in the subject
  # max_up_body, max_up_sub are upper thresholds for the fraction of
  # uppercase characters in the body and subject, respectively.
  
  isSpam = FALSE
  
  # isSpam is TRUE if at least one of these is true 
  # 1. num exclamation marks in subject > max_exclam  
  # 2. fraction of uppercase in body > max_up_body
  # 3. fraction of uppercase in subject > max_up_sub
  # 4. spamReplyTo() returns TRUE
  # 5. spamSubject1() returns TRUE
  # 6. spamSubject2() returns TRUE 
  
  
  body = email$body[[1]]
  subject = email$header["Subject"]
  reply_to = email$header["Reply-To"]
  
  ### Extract reply_to address
  if (!is.na(reply_to)) reply_to = strsplit(reply_to, "(*<[[:blank:]]*)|[[:blank:]]*>")[[1]][2]
  
  ### Check for invalid characters
  
  counts = spamCounts(subject = subject, body = body)
  spamSub2 = spamSubject2(subject, body)
  if (is.na(spamSub2)) spamSub2 = FALSE
  
  if (all(is.nan(counts[2]), is.nan(counts[3]))) {       # Check that email is not completely blank
    
    isSpam = TRUE
  
  } else if(counts[1] > max_exclam) {
    
    isSpam = TRUE
    
  } else if (!is.nan(counts[2]) && counts[2] > max_up_body) {
    
    isSpam = TRUE
    
  } else if (!is.nan(counts[3]) && counts[3] > max_up_body) {
    
    isSpam = TRUE
    
  } else if (spamReplyTo(reply_to)) {
    ### If the email does not have a "Reply-To" field then, conveniently, the above condition will
    ### actually return FALSE.  So we do not have to make any special accomodations for this case.
    
    isSpam = TRUE
    
  } else if (spamSubject1(subject)) {
    
    isSpam = TRUE
    
  } else if (spamSub2) {
    
    isSpam = TRUE
    
  }
  
  # return isSpam
  return(isSpam)
}


test = function(){
  for(i in 1:length(Emails))
  {
    print(i)
    print(Emails[[i]]$header["Subject"])
    print(spamFilter(Emails[[i]]))
#     if (is.na(Emails[[i]]$body[[1]]) | length(Emails[[i]]$body[[1]]) == 1) print(i)
  }
}

test2 = function(){
  given = as.logical(lapply(Emails, function(x) x$spam))
  spams = as.logical(lapply(Emails, spamFilter))
  return(c(spams,given, mean(spams == given)))
}

test3 = function(){
 
  for(i in 1:length(Emails))
  {
    email = Emails[[i]]
    subject = email$header["Subject"]
    if (!spamFilter(Emails[[i]]) & grepl("[[:blank:]]*[Rr][Ee][[:blank:]]*:", subject) == 1) print(i)
  }
  
}
