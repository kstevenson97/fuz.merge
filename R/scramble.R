#' @title Scramble Function
#' @description Function to change a selected number of characters for elements in a list
#' @param col Data frame column or list to change
#' @param chars Number of characters to change
#' @return New list with some of the characters of random elements changed
#' @details Function will change the inputed number of characters from a list or column
#' @examples
#' \dontrun{
#' congress$name <- scramble(congress$name, 2)
#'  }
#' @rdname scramble
#' @export

scramble <- function(col, chars){
  for (i in seq_along(col)){
    change <- sample(0:1, 1)
    if (change == 1){
      for (j in seq_along(1:chars)){
        letter <- sample(1:26)
        letter <- letters[letter]
        loc = sample(1:nchar(col[i]), 1)
        substr(col[i],loc,loc) <- letter
      }
    }
  }
  return(col)
}


# scramble <- function(data, column, chars){
#   column <- deparse(substitute(column))
#   col <- data[column]
#   for (i in seq_along(col)){
#     change <- sample(0:1, 1)
#     if (change == 1){
#       for (j in seq_along(1:chars)){
#         letter <- sample(1:26)
#         letter <- letters[letter]
#         loc = sample(1:nchar(col[i]), 1)
#         substr(col[i],loc,loc) <- letter
#       }
#     }
#   }
#   return(data)
# }
