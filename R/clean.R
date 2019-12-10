#' @title clean function
#' @description Function to clean the column to be used in joining.
#' Will remove certain parts of elements in a column
#' @param df A table the column is in
#' @param column The column to be cleaned
#' @param replacement What the cleaned portion of elements will be changed to, Default: ''
#' @param selected Characters to removed, Default: NULL
#' @param prefixes If true, prefixes will be removed, Default: FALSE
#' @param suffixes If true, suffixes will be removed, Default: FALSE
#' @param switch_order Should be set to True if one of the columns is
#'  Last Name First and the other is not. Will change the order of names.
#'  Ignore if you not joining by names, Default: FALSE
#' @param ignore.case If true, will ignore capitalization, Default: FALSE
#' @return Will return a table which has specified characters removed from the column
#' @details This should be run prior to fuzzy_match as it will ensure a higher degree of accuracy.
#' @examples
#' \dontrun{
#' congress <- clean(congress, name, selected = ",", prefixes = T, suffixes = T)
#' politwoops <- clean(politwoops, full_name, selected = ",", prefixes = T, suffixes = T)
#' }
#' @seealso
#'  \code{\link[rapportools]{is.boolean}}
#'  \code{\link[varhandle]{unfactor}}
#' @rdname clean
#' @export
#' @importFrom rapportools is.boolean
#' @importFrom varhandle unfactor


clean <-function(df, column, replacement = "", selected = NULL, prefixes = FALSE, suffixes = FALSE, switch_order = FALSE, ignore.case = FALSE) {

  column_str = as.character(substitute(column))

  if (!is.data.frame(df)) {
    stop("Arguement df must be a data frame")
  } else if (is.null(df[, column_str])) {
    stop("Arguement column must be a valid column name in dataframe x")
  } else if (!is.character(replacement)) {
    stop("Arguement replacement must be in character format")
  } else if (!rapportools::is.boolean(ignore.case)) {
    stop("Arguement ignore.case must be a boolean")
  } else if (!is.null(selected)) {
    if (!(is.list(selected) | is.vector(selected))) {
      stop("arguement selected must be a list or vector")
    }
  } else if (!rapportools::is.boolean(prefixes)) {
    stop("Arguement ignore.case must be a boolean")
  } else if (!rapportools::is.boolean(suffixes)) {
    stop("Arguement ignore.case must be a boolean")
  } else if (!rapportools::is.boolean(switch_order)) {
    stop("Arguement ignore.case must be a boolean")
  }

  if(is.factor(df[, column_str])) {
    df[, column_str] = varhandle::unfactor(df[, column_str])
  }


  things_to_replace = c()

  {x = c("^Abbot",	"^Admiral",	"^Adm.", "^Adm", "^Amb.", "^Amb", "^Ambassador", "^Baron", "^Baroness",	 "^Brnss.", "^Brnss",
         "^Bishop", "^Brigadier General", "^Brig. Gen.", "^Brig Gen", "^BG", "^Brother", "^Br.", "^Br", "^Captain", "^Cpt.",
         "^Cpt", "^Capt.", "^Capt", "^Chancellor", "^Chan.", "^Chan", "^Chaplain",	 "^Chapln.", "^Chapln", "^Chief Petty Officer",
         "^CPO", "^Commander", "^Cmdr.", "^Cdr.", "^Cdr", "^CDR", "^Colonel",	 "^Col.", "^Col", "^Colonel (Retired)",	 "^Col. (Ret.)",
         "^Col (Ret)", "^Corporal", "^Cpl.", "^Cpl", "^Countess", "^Count", "^Cntss.", "^Cntss", "^Dean", "^Drs.", "^Drs", "^Dr.",
         "^Dr", "^Duke"	, "^Ensign", "^Ens."	, "^Ens"	, "^Father", "^Friar", "^Frau", "^Fr.", "^Fr", "^General", "^Gen.",
         "^Gen", "^Governor" , "^Gov.", "^Gov", "^Judge", "^Justice", "^Lord", "^Lieutenant"	, "^2nd Lieutenant", "^2Lt.", "^2dLt.",
         "^2Lt", "^2dLt", "^Lieutenant Commander", "^Lt. Cmdr.", "^Lt Cmdr", "^Ltc.", "^Ltc", "^Lieutenant Colonel"	, "^Lt. Col." ,
         "^Lt Col", "^Lieutenant General"	, "^Lt. Gen." , "^Ltg.", "^Lt Gen" , "^Ltg", "^Lieutenant junior grade", "^Lt. j.g.",
         "^Lt jg", "^Lt.", "^Lt", "^Mademoiselle", "^Mlle.", "^Mlle", "^Major", "^Maj.", "^Maj", "^Master"	, "^Master Sergeant",
         "^Master Sgt.", "^Master Sgt", "^Miss"	, "^Madame", "^Mme.", "^Mme", "^Midshipman", "^MIDN", "^Monsieur", "M.","^M", "^Monsignor",
         "^Msgr.", "^Msgr", "^Mrs.", "^Mrs", "^Mr."	, "^Mr", "^Ms.", "^Mx."	, "^President", "^Pres.", "^Pres", "^Princess",
         "^Professor", "^Prof.", "^Prof.", "^Rabbi", "^Rear Admiral", "^R.Adm.", "^R.Adm", "^Right Reverend", "^Rt.Rev.", "^Rt Rev",
         "^Representative", "^Rep.", "^Rep", "^Reverend", "^Rev.", "^Rev", "^Sergeant", "^Sgt.", "^Sgt", "^Senora", "^Sra.", "^Sra",
         "^Senorita", "^Srta.", "^Srta", "^Senor", "^Sr.", "^Sr", "^Senator", "^Sen.", "^Sen", "^Sheikh",	 "^Sir", "^Sister",
         "^Staff Sergeant", "^S. Sgt.", "^Sgt.", "^S Sgt","^Sgt", "^The Honorable", "^The Hon.", "^The Hon", "^The Venerable",
         "^Trustee", "^Vice Admiral", "^V.Adm.", "^V Adm")}
  prefixes_L = paste(x, " ", sep = "")

  { suffixes_L = c(" III$", " II$", " IV$", " V$", " VI$", " Esq$", " Sr.$", " Sr$", " Jr.$", " Jr$", " 2nd$", " 3rd$", " CPA$",
                   " (Ret.)$", " Ret.$", " Ret$", " PhD$"	, " USN$", " MD$", " USMCR$", 	 " JD$", " USMC$", 	" DDS$",
                   " USAF$", " VM$", " DC$")}


  if (prefixes) {
    things_to_replace = append(prefixes_L, things_to_replace)
  }

  if (suffixes) {
    things_to_replace = append(suffixes_L, things_to_replace)
  }

  if (!is.null(selected)) {
    things_to_replace = append(selected, things_to_replace)
  }

  df[, column_str] = gsub("\\s+", " ", df[, column_str])

  if (switch_order) {
    split = strsplit(df[, column_str], split = " ")
    for (i in 1:length(split)) {
      str = split[[i]]
      last = str[length(split[[i]])]
      oth = paste(str[1:(length(split[[i]]) - 1)], collapse = " ")
      split[[i]] = paste(last, oth, collapse = " ")
      split[[i]] = paste(split[[i]], collapse = " ")
    }

    df$switched = split
  }


  df[, column_str] = trimws(df[, column_str])
  df[, column_str] = gsub("\\n+", " ", df[, column_str])

  for (x in 1:length(things_to_replace)) {
    df[,column_str] = gsub(things_to_replace[x], replacement, df[,column_str], ignore.case = ignore.case)
  }
  df[, column_str] = trimws(df[, column_str])
  return(df)
}

