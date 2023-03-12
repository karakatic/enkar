##### VARIBALE NAMES
get_names <- function(syntax_lines, clean=T){
  variable_lines <- grep("^VARIABLE LABELS", syntax_lines, value=T) # Use regular expressions to extract the lines that start with "VARIABLE LABELS"
  variable_names <- sub("^VARIABLE LABELS ([^ ]+).*$", "\\1", variable_lines) # Extract the variable names using regular expressions
  variable_labels <- sub("^VARIABLE LABELS [^ ]+ (.*)$", "\\1", variable_lines) # Extract the variable labels using regular expressions
  variable_labels <- trimws(variable_labels, whitespace = "['.\\s]") # Remove leading and trailing single quotes, periods, and whitespace using trimws()
  variable_vector <- setNames(trimws(variable_labels), variable_names) # Combine the variable names and labels into a named vector

  if(clean){
    # Remove the ending ':' from strings in the vector
    variable_vector <- gsub(':$', '', variable_vector)
    variable_vector <- trimws(variable_vector)
  }

  return(variable_vector)
}


##### VARIABLE TYPES
get_types <- function(syntax_lines){
  res <- list()

  for (i in seq_along(syntax_lines)) {
    # Check if the line starts with "VALUE LABELS"
    if (grepl("  /VARIABLES = ", syntax_lines[i])) {
      # Initialize a temporary variable to store the current substring
      current_substring <- ''

      # Loop through the following lines until a line with a single "." is found
      while (i < length(syntax_lines) && !grepl("    \\.$", syntax_lines[i+1])) {
        i <- i + 1
        str_s <- strsplit(trimws(syntax_lines[i]), ' ')
        var_type <- NULL

        if(startsWith(str_s[[1]][2], 'A')){
          var_type <- 'str'
        } else if (startsWith(str_s[[1]][2], 'F')){
          if(endsWith(str_s[[1]][2], '.0')){
            var_type <- 'int'
          } else{
            var_type <- 'float'
          }
        }

        res[str_s[[1]][1]] <- var_type
      }

      return(res)
    }
  }
}

##### VALUE LABELS
get_value_labels <- function(syntax_lines){
  value_labels <- c()

  # Loop through each line in the input string
  for (i in seq_along(syntax_lines)) {
    # Check if the line starts with "VALUE LABELS"
    if (grepl("^(ADD )?VALUE LABELS", syntax_lines[i])) {
      # Initialize a temporary variable to store the current substring
      current_substring <- syntax_lines[i]

      # Loop through the following lines until a line with a single "." is found
      while (i < length(syntax_lines) && !grepl("^\\.$", syntax_lines[i+1])) {
        i <- i + 1
        current_substring <- paste0(current_substring, "\n", syntax_lines[i])
      }

      s_tmp <- paste0(current_substring,'\n.')

      values <- stringr::str_extract_all(s_tmp, "\\n[-'?\\d]* '.*?'")[[1]]
      values <- stringr::str_replace_all(values, '\n', '')

      lbls <- stringr::str_replace(s_tmp, 'ADD VALUE LABELS ', '')
      lbls <- stringr::str_replace(lbls, 'VALUE LABELS ', '')
      lbls <- stringr::str_replace(lbls, '\\.$', '')
      lbls <- stringr::str_replace_all(lbls, '\n', '')

      for (value in values){
        lbls <- stringr::str_replace(lbls, gsub("([().])", "\\\\\\1", value), '')
      }

      lbls <- strsplit(lbls, " ")[[1]]
      values <- strsplit(values, " '") # Extract the value labels from the string
      values <- lapply(values, function(x) sub("^'", "", x)) # Strating ending '
      values <- lapply(values, function(x) sub("'$", "", x)) # Remove ending '

      for(lbl in lbls){
        if (lbl %in% names(value_labels)){
          named_vectors <- value_labels[[lbl]]
        }else{
          named_vectors <- list()
        }

        for (element in values) {
          named_vectors[[element[[2]]]] <- element[[1]]
        }

        value_labels[[lbl]] <- named_vectors
      }
    }
  }

  return(value_labels)
}

##### MISSING
get_missing <- function(syntax_lines){
  s <- stringr::str_extract(paste0(syntax_lines, collapse='\n'), "(?s)MISSING VALUES.*?\\.")

  # Extract the missing variable names from the string
  missing_names <- stringr::str_extract(s, "(?<=MISSING VALUES )[\\w\\s]+?(?=  \\(-\\d+ thru -\\d+\\)\\.)")
  missing_names <- trimws(strsplit(missing_names, " ")[[1]])

  # Extract the missing minimum and maximum values from the string
  s1 <- strsplit(s, '\\(|thru|)')[[1]]
  missing_min <- as.integer(s1[2])
  missing_max <- as.integer(s1[3])
  v <- trimws(strsplit(s1[[1]],' ')[[1]])

  return(c(variables=list(v[3:(length(v)-1)]), min=missing_min, max=missing_max))
}

##### ALL TOGETHER
#' Read the .sps and .txt files from 1ka
#'
#' @param file_name_sps File name of a .sps file
#' @param file_name_txt File name of a .txt file. If empty, it changes the file_name_sps file name accordingly (adds '_podatki' and changes the extension from 'sps' to 'txt').
#' @param user_na If the missing values should be replaced with NA (default) or left with their original values.
#' @param as.tibble If the returning dataframe should be returned as tibble (provides additional variable information).
#'
#' @return The data frame of read data.
#' @export
#'
#' @examples data <- read_1ka('data.sps')
#' @examples data <- read_1ka('data.sps', 'data_podatki.txt', user_na=FALSE)
#' @examples data <- read_1ka('data.sps', as.tibble=TRUE)
read_1ka <- function(file_name_sps, file_name_txt=NULL, user_na=TRUE, as.tibble=FALSE){
  if (is.null(file_name_txt)){
    file_name_txt <- stringr::str_replace(file_name_sps, '\\.sps$', '_podatki.txt')
  }

  df_raw <- read.csv2(file_name_txt, sep=' ', header=F, quote="'")
  syntax_lines <- readLines(file_name_sps)

  variable_types <- get_types(syntax_lines)
  variable_names <- get_names(syntax_lines, clean=T)
  value_lbls <- get_value_labels(syntax_lines)
  missing_values <- get_missing(syntax_lines)

  result_spss <- NULL

  for(i in seq_along(df_raw)){
    col_name <- names(variable_names)[[i]]
    col_lbl <- variable_names[[i]]
    col_type <- variable_types[[col_name]]

    # Column values and column labels
    col_values <- df_raw[[i]]
    col_value_lbls <- unlist(value_lbls[[col_name]])

    if(col_type =='int'){
      col_values <- as.integer(col_values)
      col_value_lbls <- as.integer(unlist(value_lbls[[col_name]]))
      col_value_lbls <- setNames(col_value_lbls, names(unlist(value_lbls[[col_name]])))
    } else if(col_type =='float'){
      col_values <- as.double(col_values)
      col_value_lbls <- as.double(unlist(value_lbls[[col_name]]))
      col_value_lbls <- setNames(col_value_lbls, names(unlist(value_lbls[[col_name]])))
    } else {
      col_values <- as.character(col_values)
    }

    # Column missing
    col_missing_min <- NA
    col_missing_max <- NA

    if (col_name %in% missing_values$variables){
      col_missing_min <- missing_values$min
      col_missing_max <- missing_values$max
      missing_range <- c(col_missing_min, col_missing_max)
    }

    if(is.na(col_missing_min)){
      missing_range <- NULL
    } else if (user_na) {
      col_values[col_values>=col_missing_min & col_values<=col_missing_max] <- NA
    }

    # Create column
    col_spss <- haven::labelled_spss(col_values,
                                     labels=col_value_lbls,
                                     na_range=missing_range,
                                     label=col_lbl)

    # Add column to results
    if(is.null(result_spss)){
      if(as.tibble){
        result_spss <- tibble(!!col_name := col_spss)
      } else {
        result_spss <- data.frame(col_spss)
        names(result_spss) <- col_name
      }
    } else{
      result_spss[[col_name]] <- col_spss
    }
  }

  return(result_spss)
}
