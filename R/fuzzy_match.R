#' @title fuzzy_match function
#' @description Function to join tables where the values we are matching by do
#' not match exactly
#' @param df.x Left table to be joined
#' @param df.y Right table to be joined
#' @param by.x A character vector of variables to join the left table by
#' @param by.y A character vector of variables to join the right table by
#' @param method method to calculate string distance by. See help for
#' stringdist::stringdist , Default: 'jw'
#' @param cutoff Maximum string distance to allow matching by, 0 requires exact matches
#' @param join_type Type of join to perform. Accepts left, right, inner, full,
#' semi, and anti. Default: 'left'
#' @param unique If true will only match unique values, Default: F
#' @param match_vals Create a column to display the string distance, Default: TRUE
#' @param sort Will sort the table based on string distance. Accepts
#' "desc", "asc" and NULL. Default: NULL
#' @param useBytes If \code{TRUE}, the matching is done byte-by-byte rather than
#' character-by-character, Default: FALSE
#' @param p Penalty factor for Jaro-Winkler distance. The valid range for p
#'  is 0 <= p <= 0.25. If p=0 (default), the Jaro-distance is returned.
#'  Applies only to method='jw', Default: 0
#' @param weight For method='osa' or 'dl', the penalty for deletion, insertion,
#'  substitution and transposition, in that order. When method='lv', the penalty
#'  for transposition is ignored. When method='jw', the weights associated with
#'  characters of a, characters from b and the transposition weight, in that
#'  order. Weights must be positive and not exceed 1. weight is ignored
#'  completely when method='hamming', 'qgram', 'cosine', 'Jaccard', 'lcs', or
#'  soundex., Default: c(d = 1, i = 1, s = 1, t = 1)
#' @param q Size of the q-gram; must be nonnegative. Only applies to
#'  method='qgram', 'jaccard' or 'cosine'., Default: 1
#' @param bt Winkler's boost threshold. Winkler's penalty factor is only applied
#'  when the Jaro distance is larger than bt. Applies only to method='jw'
#'  and p>0., Default: 0
#' @return Returns a data.frame with two data.frames input joined
#' @details Function to join tables where the columns to join by don't match exactly.
#' Should use the clean function prior to running fuzzy_match
#' @examples
#' \dontrun{
#' congress <- clean(congress, name, selected = ",", prefixes = T, suffixes = T)
#' politwoops <- clean(politwoops, full_name, selected = ",", prefixes = T, suffixes = T)
#' fuzzy_match(congress, politwoops, name, full_name, join_type = "inner", cutoff = .1)
#' }
#' @seealso
#'  \code{\link[varhandle]{unfactor}}
#'  \code{\link[stringdist]{stringdist}}
#'  \code{\link[dplyr]{join}},\code{\link[dplyr]{arrange}}
#' @rdname fuzzy_match
#' @export
#' @importFrom varhandle unfactor
#' @importFrom stringdist stringdistmatrix
#' @importFrom dplyr left_join right_join inner_join full_join semi_join anti_join arrange

fuzzy_match <- function(df.x, df.y, by.x, by.y, method = "jw", cutoff, join_type = "left", unique = F, match_vals = TRUE, sort = NULL, useBytes = FALSE, p = 0, weight = c(d = 1, i = 1, s = 1, t = 1), q = 1, bt = 0) {

  by.x_str = as.character(substitute(by.x))
  by.y_str = as.character(substitute(by.y))
  by.x = df.x[as.character(substitute(by.x))][[1]]
  by.y = df.y[as.character(substitute(by.y))][[1]]

  if (is.factor(df.x[,by.x_str])) {
    df.x[,by.x_str] = varhandle::unfactor(df.x[,by.x_str])
  }

  if (is.factor(df.y[,by.y_str])) {
    df.y[,by.y_str] = varhandle::unfactor(df.y[,by.y_str])
  }


  if (!is.data.frame(df.x)) {
    stop("Argument df.x must be a data frame")
  } else if (!is.data.frame(df.y)) {
    stop("Argument df.y must be a data frame")
  } else if (is.null(by.x)) {
    stop("Argument by.x must be a valid column name in dataframe x")
  } else if (is.null(by.y)) {
    stop("Argument by.y must be a valid column name in dataframe y")
  } else if (!is.numeric(p)) {
    stop("Argument p must be a number")
  } else if (p > .25) {
    stop("Argument p must be less than or equal to .25")
  } else if (!is.numeric(q)) {
    stop("Argument q must be a number")
  } else if (!is.numeric(bt)) {
    stop("Argument bt must be a number")
  } else if (!is.numeric(cutoff)) {
    stop("Argument cutoff must be a number")
  } else if (!is.boolean(unique)) {
    stop("Argument unique must be a boolean")
  } else if (!is.boolean(useBytes)) {
    stop("Argument unique must be a boolean")
  }  else if (!is.boolean(match_vals)) {
    stop("Argument match_vals must be a boolean")
  } else if (!is.character(join_type)){
    stop("Argument join_type must be a string")
  }


  if (is.factor(by.x)) {
    by.x = varhandle::unfactor(by.x)
  }

  if(is.factor(by.y)) {
    by.y = varhandle::unfactor(by.y)
  }

  count = 1


  tryCatch( m <- stringdist::stringdistmatrix(by.x, by.y, method=method, useBytes=useBytes, weight=weight, p=p, q=q, bt=bt, useNames = T),
            error = function(e) {
              print("follow https://stackoverflow.com/questions/51295402/r-on-macos-error-vector-memory-exhausted-limit-reached to increase vector max")})

  x = as.data.frame(which(m == 0, arr.ind = T))
  m_check = m
  for (i in 1:nrow(x)) {
    m_check[x[i,1],x[i,2]] = 100
  }

  if (min(m_check) > cutoff) {
    warning("there are no observations with string distances under the cutoff so there is nothing to fuzzy match, strict matching instead")

    colnames(df.x)[colnames(df.x) == by.x_str]<- "by.x"
    colnames(df.y)[colnames(df.y) == by.y_str]<- "by.y"

    if (join_type == "left") {
      merged = dplyr::left_join(df.x, df.y, by = c("by.x"= "by.y"))
      colnames(merged)[colnames(merged) == "by.x"]<- by.x_str
      colnames(merged)[colnames(merged) == "by.y"]<- by.y_str
    } else if (join_type == "right") {
      merged = dplyr::right_join(df.x, df.y, by = c("by.x" = "by.y"))
      colnames(merged)[colnames(merged) == "by.x"]<- by.x_str
      colnames(merged)[colnames(merged) == "by.y"]<- by.y_str
    } else if (join_type == "inner") {
      merged = dplyr::inner_join(df.x, df.y, by = c("by.x" = "by.y"))
      colnames(merged)[colnames(merged) == "by.x"]<- by.x_str
      colnames(merged)[colnames(merged) == "by.y"]<- by.y_str
    } else if (join_type == "full") {
      merged = dplyr::full_join(df.x, df.y, by = c("by.x" = "by.y"))
      colnames(merged)[colnames(merged) == "by.x"]<- by.x_str
      colnames(merged)[colnames(merged) == "by.y"]<- by.y_str
    } else if (join_type == "semi") {
      merged = dplyr::semi_join(df.x, df.y, by = c("by.x" = "by.y"))
      colnames(merged)[colnames(merged) == "by.x"]<- by.x_str
      colnames(merged)[colnames(merged) == "by.y"]<- by.y_str
    } else if (join_type == "anti") {
      merged = dplyr::anti_join(df.x, df.y, by = c("by.x" = "by.y"))
      colnames(merged)[colnames(merged) == "by.x"]<- by.x_str
      colnames(merged)[colnames(merged) == "by.y"]<- by.y_str
    } else {
      stop("join_type must be a valid form of dpylr joining")
    }

  } else {
    if (match_vals) {
      temp = data.frame(by.y = NA, matched = NA, match_vals =  NA)

      for (i in 1:length(by.x)) {
        for (p in 1:length(by.y)){
          if (m[i,p] < cutoff) {
            temp[count,] <- c(by.y[p], by.x[i], m[i,p])
            count = count + 1
          }
        }
      }
    } else {

      temp = data.frame(by.y = NA, matched = NA)


      for (i in 1:length(by.x)) {
        for (p in 1:length(by.y)){
          if (m[i,p] < cutoff) {
            temp[count,] <- c(by.x[i], by.y[p])
            count = count + 1
          }
        }
      }
    }


    colnames(df.x)[colnames(df.x) == by.x_str]<- "by.x"
    colnames(df.y)[colnames(df.y) == by.y_str]<- "by.y"


    left = dplyr::left_join(temp, df.y, by = ("by.y"))


    if (join_type == "left") {
      merged = dplyr::left_join(df.x, left, by = c("by.x"= "matched"))
      colnames(merged)[colnames(merged) == "by.x"]<- by.x_str
      colnames(merged)[colnames(merged) == "by.y"]<- by.y_str
    } else if (join_type == "right") {
      merged = dplyr::right_join(df.x, left, by = c("by.x" = "matched"))
      colnames(merged)[colnames(merged) == "by.x"]<- by.x_str
      colnames(merged)[colnames(merged) == "by.y"]<- by.y_str
    } else if (join_type == "inner") {
      merged = dplyr::inner_join(df.x, left, by = c("by.x" = "matched"))
      colnames(merged)[colnames(merged) == "by.x"]<- by.x_str
      colnames(merged)[colnames(merged) == "by.y"]<- by.y_str
    } else if (join_type == "full") {
      merged = dplyr::full_join(df.x, left, by = c("by.x" = "matched"))
      colnames(merged)[colnames(merged) == "by.x"]<- by.x_str
      colnames(merged)[colnames(merged) == "by.y"]<- by.y_str
    } else if (join_type == "semi") {
      merged = dplyr::semi_join(df.x, left, by = c("by.x" = "matched"))
      colnames(merged)[colnames(merged) == "by.x"]<- by.x_str
      colnames(merged)[colnames(merged) == "by.y"]<- by.y_str
    } else if (join_type == "anti") {
      merged = dplyr::anti_join(df.x, left, by = c("by.x" = "matched"))
      colnames(merged)[colnames(merged) == "by.x"]<- by.x_str
      colnames(merged)[colnames(merged) == "by.y"]<- by.y_str
    } else {
      stop("join_type must be a valid form of dpylr joining")
    }
  }

  if (unique) {
    merged = unique(merged)
  }
  if (!is.null(sort)){
    if (tolower(sort) == "asc"){
      merged <- dplyr::arrange(merged, match_vals)
    }
    if (tolower(sort) == "desc"){
      merged <- dplyr::arrange(merged, desc(match_vals))
    }
  }

  return(merged)
}



