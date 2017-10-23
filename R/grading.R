#' createYaml - example function
#' @param yaml - doesn't do anything
#' export
createYaml <- function(yaml) {
  yaml <- paste(yaml,"/lesson.yaml", sep="")
  myYaml = parse_content(yaml,envornment())
  l <- c(myYaml$Class != "text")
  s <- split(myYaml,l)
  l <- s[[2]]
  return(l)
}

#' createQuestions
#' @param yaml - directory of yaml
#' export
createQuestions <- function(yaml) {
  cat("update1.0")
  l <- createYaml(yaml)
  #yaml <- paste(yaml,"/lesson.yaml", sep="")
  #myYaml = parse_content(yaml,envornment())
  #l <- c(myYaml$Class != "text")
  #s <- split(myYaml,l)
  #l <- s[[2]]
  
  qNum <- c(1:length(l$Output))
  
  questions1 <- paste("< Question",qNum,">", l$Output,l$AnswerChoices, "< Question",qNum,">;\n")
  questions1 <- gsub("NA"," ",questions1)
  questions1 <- gsub("< ","<",questions1)
  questions1 <- gsub(" >",">",questions1)
  questions2 <- paste(strwrap(questions1,width = 80,prefix ="#"))
  questions2 <- gsub(">;",">;\n\n\n",questions2)
  #C:/Users/DeCafe/Documents/GitHub/R_Programming/Basic_Building_Blocks
  return(questions2)  
}

#' createAssignment
#' @param directory - directory of yaml
#' @param assignment - name of assignment
#' @export
createAssignment <- function(directory,assignment) {
  myYaml <- createQuestions(directory)
  myAssignment <- paste(assignment,".R", sep="")
  write(myYaml,file= myAssignment)
  file.show(myAssignment)
}

#' gradeAssignment
#' @param directory - directory of yaml
#' @param assignment - name of assignment
#' @export
gradeAssignment <- function(directory,assignment) {
  cat("update3.2")
  myYaml <- createYaml(directory)
  answer <- readLines(assignment)
  answer <- paste(answer, collapse = "\n")
  answer <- strsplit(answer, "#<Question [0-9]*>(.*?)<Question [0-9]*>;")
  answer <- data.frame(matrix(unlist(answer)))
  myYaml <- cbind(myYaml, StudentAnswer = answer[2:nrow(answer),])
  
  load("e.RData")

#  e = environment()
  for( x in 1:nrow(myYaml)){
    current.row <- myYaml[x,]
    userAnswer <- current.row$StudentAnswer
    # if test is a omnitest, then
    g <- grep("omnitest", current.row$AnswerTests)
    if (length(g) > 0) {
      #     set e$expr to quote(user's answer)
      p<- paste0("quote(", userAnswer, ")")
      e$expr <- eval(parse(text = p))
      testResponse(current.row,e)
    } else {
      # if test is a var_has_value, then
      g <- grep("var.has.value", current.row$AnswerTests)
      if (length(g) > 0) {
        #   evaluate the user's expression
        testResponse(current.row,e)
      }
     else {
        print("No valid test for this question")
      }
    }
  }
  
  return(myYaml)
  
}

