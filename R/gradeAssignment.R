#' gradeAssignment
#' @param directory - directory of yaml
#' @importFrom dplyr filter mutate
#' @importFrom stringr str_split
#' @export
#' 
gradeAssignment <- function(directory) {

  yaml.file <- Sys.glob(paste0(directory,"/instr/*_yaml.csv"))
  if (length(yaml.file) >1) {
    cat("ERROR: multiple yaml.csv files detected in", directory,"/instr/\n")
    invisible(NULL)
  } else if (length(yaml.file) <1) {
    cat("ERROR: yaml.csv file not found in", directory,"/instr/\n")
  }

  cat("The following yaml file will be used for grading:\n", yaml.file, "\n")
    
  myYaml <- read.csv(yaml.file)
  myYaml <- dplyr::filter(myYaml, Class!="text")
  myYaml$AnswerTests <- as.character(myYaml$AnswerTests)
  
  submission.file <- Sys.glob(paste0(directory,"/submissions/*"))
  if (length(submission.file) >1) {
    cat("ERROR: grading of multiple submission files not yet implemented\n")
    invisible(NULL)
  } else if (length(yaml.file) <1) {
    cat("ERROR: submission file not found in", directory,"/submissions/\n")
    invisible(NULL)
  }
  
  cat("Grading the following submission:\n", submission.file, "\n")
  
  answers <- readLines(submission.file)
  answers <- paste(answers, collapse = "\n")
  answers <- str_split(answers, "# <Q[0-9]*>.*\n*")
  answers <- answers[[1]][-1]
  
  answers <- strsplit(answers, "\n")

  rm.comments <- function(x) {
    x <- gsub("#.*", "", x)
    paste(x[x!=""], collapse = "\n")
  }

  # add submitted answers to table
  answers <- sapply(answers, rm.comments)
  myYaml <- mutate(myYaml, SubmittedAnswer = answers)
  
  
  # save and clear global environment so it does not interfere with
  # users answers
  global_objects <- mget(ls(envir = globalenv()), globalenv())
  rm(list = ls(envir = globalenv()), envir = globalenv())
  
  # environment needed for some user responses
  e <- environment()
  correct <- rep(NA,nrow(myYaml))
  for( i in 1:nrow(myYaml)){
    current.row <- myYaml[i,]
    userAnswer <- current.row$SubmittedAnswer
   
    # set up environment for grading
    is.omnitest <- length(grep("omnitest", current.row$AnswerTests)) > 0
    is.var.has.value <- length(grep("var_has_value", current.row$AnswerTests)) > 0
    
    if (is.omnitest) {
        #   for omnitest, set e$expr to user's answer
        if (userAnswer == "") {
          e$expr <- NA
        } else {
          p<- paste0("quote(", userAnswer, ")")
          e$expr <- eval(parse(text = p))
        }
    } else if (is.var.has.value) {
        #evaluate code, store variables in global environment
        eval(parse(text = userAnswer), envir = globalenv())
    } else {
        print("ERROR: No valid test for this question")
        invisible(NULL)
    }
    cat("Q", i, "\n", sep = "")
    cat("AnswerTests: ", current.row$AnswerTests, "\n")
    cat("Submitted: ", current.row$SubmittedAnswer, "\n")
    if (testResponse(current.row, e, TRUE)) {
      cat("correct!\n")
      correct[i] <- TRUE
    } else {
      cat("incorrect\n")
      correct[i] <- FALSE
    }
    cat("\n")
  }

  # clear global environment, since submission may  have changed it
  rm(list = ls(envir = globalenv()), envir = globalenv())
  
  # restore original global environment
  for (n in names(global_objects)) {
    assign(n, global_objects[[n]], envir=globalenv())
  }
  
  list(n = length(correct), correct = sum(correct), incorrect = sum(!correct))
}




#' checkAssignmentFormat 
#' @param submission - the file name of the student's submission
#' @param numQuestions - the actual number of questions in the assignment
#' 
#' @return TRUE if the question format is correct and the assignment can be graded; returns FALSE otherwise
#' 
#' @importFrom stringr str_extract
#' @export 
checkAssignmentFormat <- function(submission, numQuestions){
  
  x <- readLines(submission)
  tags <- str_extract(x, "^# <Q[0-9]+>")
  tags <- gsub("^# ", "", tags)
  tags <- tags[!is.na(tags)]
  
  errors <- FALSE
  t <- table(tags)
  if (any(t>1)) {
    cat("ERROR: the following duplicate questions were detected:\n", names(which(table(tags)>1)), "\n")
    errors <- TRUE
  }
  
  correctTags <- paste0("<Q", 1:numQuestions, ">")
  missing <- setdiff(correctTags, tags)
  if (length(missing) > 0) {
    cat("ERROR: the following questions are missing:\n", missing, "\n")
    errors <- TRUE
  }
  
  extra <- setdiff(tags, correctTags)
  if (length(extra) > 0) {
    cat("ERROR: the following extra questions were found:\n", extra, "\n")
    errors <- TRUE
  }
  
  if(!errors) {
    if (!all(tags == correctTags)) {
      cat("ERROR: questions are out of order, check beginning with", 
          tags[which(!tags==correctTags)][1], "\n")
      errors <- TRUE
    }
  }
  
  if (errors) {
    cat("The above errors must be fixed or assignment cannot be graded\n")
    return (FALSE)
  }
  
  return(TRUE)
}


