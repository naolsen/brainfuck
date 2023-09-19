




#' Brainfuck
#'
#' R interpreter for the esoteric programming language "Brainfuck"
#'
#' @param code Brainfuck code 
#' @param memory Memory size
#' @param debug Debug. \code{debug = 1} activates the debug. 
#' @param input Input function
#' @param output Output function
#'
#' @details 
#' Please eee \link{https://en.wikipedia.org/wiki/Brainfuck} for an introduction to Brainfuck. Brainfuck code consists of the eight characters
#' "\code{+-<>.,[]}", all other characters are ignored. "d" is a special debug character only used if debug is active. 
#' 
#' Implementation details: The data range is \code[0, 255]}. 0-1 = 255, ie. if you decrement a "0", you will get 255.
#' The pointer always starts at 1; valid pointer positions are \code{[1, memory]}. Exceeding this range will throw an error.
#' 
#' I/O: the default input behaviour is to read a single character (and ignore the rest) and convert it from ASCII to integer.
#' The default output behaviour is to convert the value to ASCII and print the result using \code{cat} 
#' (note: the letters of the alphabet start at 65 and the numbers at 48).
#'
#' Debug: When debug is on, the character "d" flags a debug: the console prints the current data, pointer position and code position. 
#' 
#' @export
#'
#' @examples 
#' ## Prints 'Hello World!'
#' brainfuck("++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]
#' >>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
#' 
#' ## Read two input characters and print them (note: not the shortest way of doing this)
#' brainfuck(",>,<.>.")
#' 
#' ## A simple example with debug active (the code prints the letter "H")
#' brainfuck("++++++++d[>+++++++++<-]d>.", debug = 1)
#' 
brainfuck <- function(code, memory = 1024, debug = 0, 
                      input = function() utf8ToInt(scan(n = 1, what = character(), strip.white = FALSE, quiet = TRUE, sep="\n"))[1], 
                      output = function(i) cat(intToUtf8(i))) {
  
  check.input <- function(i) if (!is.integer(i) || length(i) > 1 || i < 0 || i > 255) stop("Input error") else i
  
  data <- integer(memory)
  pointer <- 1
  
  i <- 1
  pmark <- c()
  while(i <= nchar(code)) {
    switch(substr(code, i, i),
           "+" = (data[pointer] <- data[pointer] +1),
           "-" = (data[pointer] <- data[pointer] - 1),
           ">" = (pointer <- pointer+1),
           "<" = (pointer <- pointer-1),
           "." = output(data[pointer]),
           "," = data[pointer] <- check.input(input()), 
           "[" = if (data[pointer]) (pmark <- c(pmark,i)) else (i <- find.closing(code, i+1)),
           "]" = if (data[pointer]) i <- pmark[length(pmark)]  else pmark <- pmark[seq_len(length(pmark)- 1)],
           "d" = if (debug > 0) {
             cat("\ndata: ", data[seq_len(max(which(data != 0)))], "\n")
             cat("pointer position: ", pointer, "\n")
             cat("code position: ", i, "\n")
           }
    )
    if (pointer == 0 || is.na(data[pointer])) {
      if (debug > 0) {
        cat("data: ", data[seq_len(max(which(data != 0)))], "\n")
        cat("pointer position: ", pointer, "\n")
        cat("code position: ", i, "\n")
      }
      if (pointer == memory + 1) stop("Index error: memory limit exceeded")
      else stop("Index error")
    }
    else if (data[pointer] == -1) data[pointer] <- 255
    else if (data[pointer] == 256) data[pointer] <- 0
    
    
    i <- i+1
  }
}
