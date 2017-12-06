#This is the functions I have added for my Independent Study
#Keith Cafiero


#' createYaml - example function
#' @param yaml - doesn't do anything
#' export
createYaml <- function(yaml) {
  #making the yaml file searching for anything that isn't "text" in the $class column
  #which will allow for everything else to be questions. 
  yaml <- paste(yaml,"/lesson.yaml", sep="")
  myYaml = parse_content(yaml,envornment())
  l <- c(myYaml$Class != "text")
  s <- split(myYaml,l)
  l <- s[[2]]
  
  #Warning set up for questions that have no way to be graded
  for(count in 1:nrow(l)){
    if((grepl("omnitest",l$AnswerTests[count]))|(grepl("var.has.value",l$AnswerTests[count]))){
    }else{
      cat("Warning grading question ", count, "has not yet been implemented. \n")
    }
  }
  return(l)
}

#' checkAssignment - 
#' @param orgYaml -
#' @param stuAns
#' @export 
checkAssignment <- function(orgYaml, stuAns){
  
  # This function does not check if questions are in Order. 
  # This will cause grading errors, if questions are out of order.
  
  
  orgYaml <- createQuestions(orgYaml)
  
  x <- readLines(stuAns)
  answer <- data.frame(matrix(x))
  
  z <- c()
  for(nn in 1:length(orgYaml)){
    #checking to see if vectors match what a "question" is.
    #>; is the end of the question
    if((grepl("<",orgYaml[nn]))&(!grepl(">;",orgYaml[nn]))){ 
      
      r <-grepl(orgYaml[nn],answer[1:nrow(answer),])
      q <- regexpr("<Question [0-9]*>",orgYaml[nn])
      q <- substring(orgYaml[nn],q,q+attr(q,"match.length")-1)
      
      #If any of the Values in R = True, the "Answer" has the question
      if(any(r)){
        z[nn] <- "D"
        if((!length(r[r==TRUE])>1)){
          z[nn] <- "OK"
        }
        #print(paste(q,"Is here"))
      }else{ #If ALL values are FALSE than the "Answer" doesn't have question
        z[nn] <- "M"
      }
    }
  }
  z <- z[!is.na(z)]
  
  if(all(z=="OK")){
    cat("Ready for Submission")
  }else{
    cat("Please fix the following: \n")
    for(k in 1:length(z)){
      if(z[k] == "D"){
        cat("Question ", k, " is Duplicated. \n")
      }else if(z[k] == "M"){
        cat("Question ", k, " is Missing.\n")
      }
    }
  }
}

#' createQuestions
#' @param yaml - directory of yaml
#' export
createQuestions <- function(yaml) {

  #make the yaml
  theYaml <- createYaml(yaml)
  
  #length number of questions in yaml
  qNum <- c(1:length(theYaml$Output))
  
  #proper spacing and formatting
  questions1 <- paste("< Question",qNum,">", theYaml$Output,theYaml$AnswerChoices, "< Question",qNum,">;\n")
  questions1 <- gsub("NA"," ",questions1)
  questions1 <- gsub("< ","<",questions1)
  questions1 <- gsub(" >",">",questions1)
  questions2 <- paste(strwrap(questions1,width = 80,prefix ="#"))
  questions2 <- gsub(">;",">;\n\n\n",questions2)

  return(questions2)  
}

#' createAssignment
#' @param directory - directory of yaml
#' @param assignment - name of assignment
#' @export
createAssignment <- function(directory,assignment) {
  
  myYaml <- createQuestions(directory)
  
  #this allows the user to just name it w/e and it will make it a .R file
  myAssignment <- paste(assignment,".R", sep="")
  write(myYaml,file= myAssignment)
  
  #showing the assignment can be optional
  file.show(myAssignment)
  
}

#' gradeAssignment
#' @param directory - directory of yaml
#' @param assignment - name of assignment
#' @export
gradeAssignment <- function(directory,assignment) {

  myYaml <- createYaml(directory)
  orgAnswer <- readLines(assignment)
  correct <- 0
  answer <- paste(orgAnswer, collapse = "\n")
  answer <- strsplit(answer, "#<Question [0-9]*>(.*?)<Question [0-9]*>;")
  answer <- data.frame(matrix(unlist(answer)))

  myYaml <- cbind(myYaml, StudentAnswer = answer[2:nrow(answer),])
  #these are optional
  #   View(myYaml)
  #   View(orgAnswer)
  #this e.RData is important and for gradeAssignment to work the workspace has to have
  #this file in the directory
  load("e.RData")
  for( x in 1:nrow(myYaml)){
    current.row <- myYaml[x,]
    userAnswer <- current.row$StudentAnswer
    # if test is a omnitest, then
    g <- grep("omnitest", current.row$AnswerTests)
    if (length(g) > 0) {
      #     set e$expr to quote(user's answer)
      p<- paste0("quote(", userAnswer, ")")
      e$expr <- eval(parse(text = p))
      if(gradeFunction(current.row,e))
        correct <- correct+1
    } else {
      # if test is a var_has_value, then
      g <- grep("var.has.value", current.row$AnswerTests)
      if (length(g) > 0) {
        #   evaluate the user's expression
        if(gradeFunction(current.row,e))
          correct <- correct+1
      }
      else {
        print("No valid test for this question")
      }
    }
  }


  return(correct/nrow(myYaml)) #change this to myYaml
}

#' gradeFunction
#' @param current.row - directory of yaml
#' @param e - name of assignment
#' export
gradeFunction <- function(current.row, e){

  tests <- current.row[,"AnswerTests"]
  if(is.na(tests) || tests == ""){
    results <- is(e, "dev")
    if(!results){
      stop(s()%N%"BUG: There are no tests for this question!")
    }
  } else {
    tests <- str_trim(unlist(strsplit(tests,";")))
    results <- lapply(tests, function(keyphrase){testMe(keyphrase,e)})
  }
  correct <- !(FALSE %in% unlist(results))
  if(correct){

    e$iptr <- 1
    return(TRUE)
  } 
  return(FALSE)
}