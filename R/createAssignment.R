#' createAssignment
#' @param yaml.file - lesson file in yaml format
#' @param directory - the directory to store the assignment
#' @param name - the name of the assignment
#' @param width - max number of characters per line in R_file (not including tags)
#' @param show - if TRUE, open the assignment (defaults to FALSE)
#' 
#' @return None
#' 
#' @description creates an auto-gradable assignment by setting up the assignment directory 
#'   structure. The directory '/instr/' contains the converted yaml file and 
#'   the assignment as an R script; an empty '/submission/' and '/feedback/' directory are 
#'   also created. 
#' 
#'   
#' @export
createAssignment <- function(yaml.file, directory, name = "assignment", width = 80,  show = FALSE) {

  if (dir.exists(directory)) {
    cat("ERROR: directory", directory, "already exists.\n")
    cat("Use a different directory or delete the current one, and try again")
    return (invisible(NULL))
  }
    
  myYaml <- formatYamlForAssignment(yaml.file)
  if (is.null(myYaml)) {
    return(invisible(NULL))
  }
  
  questions <- createQuestions(myYaml, width = width)
  
  dir.create(directory)
  dir.create(paste0(directory,"/instr/"))
  dir.create(paste0(directory,"/submissions/"))
  dir.create(paste0(directory,"/feedback/"))
  
  # save the yaml file
  write.table(myYaml, file = paste0(directory,"/instr/",name,"_yaml.csv"), row.names = FALSE, sep =",")
  
  # save the R file
  write(questions$questions,file= paste0(directory,"/instr/",name, ".R"))
  
  cat("Assignment created in directory", directory, "with", questions$numQuestions, "questions.")
  
  # optionally, show the R file
  if (show) {
    file.edit(paste0(directory,"/instr/",name, ".R"))
  }
  
}

#' formats Yaml file for use in assignment
#' createYamlForAssignment 
#' @param yaml - the lesson file in yaml format

# TO DO: currently assumes one test per question
formatYamlForAssignment <- function(yaml) {
  
  # read in yaml file
  myYaml = parse_content(yaml,NULL)
  
  token.list <- NULL
  for (i in 1:nrow(myYaml)) {
    tt = token.generate(myYaml[i,], token.list)
    token.list <- tt$token.list
    myYaml[i,] <- tt$row
  }
  
  #Warning set up for questions that have no way to be graded
  tests <- myYaml$AnswerTests

  valid <- c("text", "omnitest", "var_has_value")
  tests <- myYaml$AnswerTests
  tests <- tests[!is.na(tests)]
  tests <- gsub("\\(.*\\)", "", tests)
  invalid <- setdiff(unique(tests), valid)
  if (length(invalid) > 0) {
    cat("ERORR: Assignment cannot be created; the following tests are not implemented: ")
    cat(invalid, sep = ", ")
    return(NULL)
  }

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

