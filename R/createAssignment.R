
#' formats Yaml file for use in assignment
#' createYamlForAssignment 
#' @param yaml - the lesson file in yaml format
formatYamlForAssignment <- function(yaml) {
  
  # read in yaml file
  myYaml = parse_content(yaml,NULL)
  
  #Warning set up for questions that have no way to be graded
  tests <- myYaml$AnswerTests

  valid <- c("text", "omnitest", "var.has.value")
  tests <- myYaml$AnswerTests
  tests <- tests[!is.na(tests)]
  tests <- gsub("\\(.*\\)", "", tests)
  invalid <- setdiff(unique(tests), valid)
  if (length(invalid) > 0) {
    cat("ERORR: Assignment cannot be created; the following tests are not implemented: ")
    cat(invalid, sep = ", ")
    return(NULL)
  }
  cat("WARNING: assuming 1 test per question")

  return(myYaml)
}

#' createQuestions
#' @param lesson - data.frame of lesson
#' @param width - max number of characters per line (not including tags)
createQuestions <- function(lesson, width = 80) {

  # proper spacing and formatting (will need to handle 
  # lesson$AnswerChoices for # multiple choice questions )
  questions <- paste0(lesson$Output)

  # only add <Q>> tags to questions
  
  qs <- !(lesson$Class%in%"text")
  
  qnum <- 1:sum(qs)
  questions[qs] <- paste0("<Q",qnum,"> ", 
                          questions[qs], 
                          #"\n# </Question ",qnum,
                          "\n")
  questions <- paste0(questions, "\n")  
  questions <- strwrap(questions,width = width,prefix ="# ", simplify = FALSE)
  questions <- sapply(questions, paste0, collapse = "\n")
  questions <- paste0(questions, "\n")
  
  list(questions = questions, numQuestions = sum(qs))
}

#' createAssignment
#' @param yaml_file - lesson file in yaml format
#' @param R_file - name of assignment file (.R will be added)
#' @param width - max number of characters per line in R_file (not including tags)
#' @param show - if TRUE, open the assignment (defaults to FALSE)
#' @export
createAssignment <- function(yaml_file, R_file, width = 80,  show = FALSE) {
  
  myYaml <- formatYamlForAssignment(yaml_file)
  if (is.null(myYaml)) {
    return(NULL)
  }
  questions <- createQuestions(myYaml, width = width)
  
  
  #this allows the user to just name it w/e and it will make it a .R file
  write(questions$questions,file= R_file)
  
  # optionally, show the R file
  if (show) {
    file.edit(R_file)
  }
  
  list(yaml = myYaml, numQuestions = questions$numQuestions)
}

