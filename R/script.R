#' Extract ggplot from qmd/rmd
#' @param file_path Path to the QMD/RMD file
#' @import knitr
extract_ggplot_code <- function(file_path) {
  # Return: List of a list for each chunk info (chunk label, code, and reference paragraph)

  content <- readLines(file_path)
  temp_file <- tempfile(fileext = ".txt")
  knitr::purl(file_path, output = temp_file, documentation = 1) # no documentation just code boundary


  # Grab all code chunks from the .txt file
  all_code_chunks <-   parse_code(temp_file)

  results <- list()


  # Check if the chunk generate ggplot objects
  for (i in seq_along(all_code_chunks)) {
    chunk <- all_code_chunks[[i]]

    if (stringr::str_detect(chunk$code, "ggplot|geom") & stringr::str_detect(chunk$code, "aes")) {
      # If so, find location of the code chunk in the qmd file and
      original_location <- find_chunk_location(content, chunk$chunk_label, chunk$code)
      # Grab the paragraph that references the chunk label (@fig-)
      reference_paragraph <- find_reference_text(content, chunk$chunk_label,
                                                 original_location$start,
                                                 original_location$end)



      chunk_info <- list(
        chunk_label = chunk$chunk_label,
        chunk_code = chunk$code,
        reference_paragraph = reference_paragraph
      )

      results[[length(results) + 1]] <- chunk_info
    }

  }

  unlink(temp_file)
  return(results)

}



#' Function to extract all code chunks from a .txt file
#' @param purled_content Output from knitr::purl() function
parse_code <- function(purled_content) {

  chunks <- list()
  current_chunk <- NULL
  purled_lines <- readLines(purled_content)

  for (i in seq_along(purled_lines)) {
    line <- purled_lines[i]
    chunk_start_pattern <- "^## ----|^#\\| label:"

    if (stringr::str_detect(line, chunk_start_pattern)){

      if (stringr::str_detect(line, "^## ----")) {
        # starts with ## ---- and ends with either , or --
        chunk_label <- stringr::str_extract(line, "(?<=^## ----).*?(?=--|,)")
      } else {
        chunk_label <- stringr::str_extract(line, "(?<=#\\| label: ).*")
      }

      if (is.na(chunk_label)){
        chunk_label <- paste0("chunk_",length(chunks) +1) # chunk_i
      }
      chunk_label <- stringr::str_trim(chunk_label) # Else: chunk_label

      if (!is.null(current_chunk)) {
        chunks[[length(chunks) + 1]] <- current_chunk
      }

      # Initialize current chunk
      current_chunk <- list(
        chunk_label = chunk_label,
        code = character()
      )
    } else if (!is.null(current_chunk) && !stringr::str_detect(line, "^##")) {
      # Add all codes until the next chunk starts
      current_chunk$code <- c(current_chunk$code, line)
    }

  }

  if (!is.null(current_chunk)) {
    chunks[[length(chunks) + 1]] <- current_chunk
  }

  for (i in seq_along(chunks)) {
    chunks[[i]]$code <- stringr::str_trim(paste0(chunks[[i]]$code, collapse = "\n"))
  }

  return(chunks)

}

#' Function to find chunk location in the original document
#' @param content QMD/RMD file in .txt format
#' @param chunk_label Chunk label
#' @param code Source code
#' @import stringr
find_chunk_location <- function(content, chunk_label, code) {
  # content: readLines(".qmd")
  # chunk_label: a single label name
  # code: all syntax in a single code chunk
  # Return: Start and end position of each code chunk. If it is an empty chunk, start and end position are set to NA.


  start_pattern <- paste0("^```\\{r.*", chunk_label, "|", "^#\\| label: ", chunk_label)
  potential_chunk_start <- which(stringr::str_detect(content, start_pattern))


  # if there is no chunk label
  if (length(potential_chunk_start) == 0) {
    all_syntax <- stringr::str_split(code, "\n")[[1]]
    if (length(all_syntax) > 0){
      first_line <- stringr::str_trim(all_syntax[1])

      # Iterate through the entire document and find the first line
      if (nchar(first_line) > 0) {
        for (i in seq_along(content)){
          if (stringr::str_detect(stringr::str_trim(content[i]), stringr::fixed(first_line))) { #fixed() to compare literal bytes

            # chunk_start is where "^{r" is
            chunk_start <- i
            while (chunk_start > 1 && !stringr::str_detect(content[chunk_start - 1], "^```\\{r")) {
              chunk_start <- chunk_start - 1
            }
            chunk_start <- chunk_start - 1

            # chunk_end is where "^```" is
            chunk_end <- i
            while (chunk_end > 1 && !stringr::str_detect(content[chunk_end - 1], "^```$")) {
              chunk_end <- chunk_end + 1
            }
            chunk_end <- chunk_end + 1

            return(list(start = chunk_start, end = chunk_end))
          }
        }
      }
    }
    # if there is no syntax in the code chunk
    return(list(start = NA, end = NA))
  }

  # if there is a chunk label
  start_line <- potential_chunk_start[1] - 1
  chunk_ends <- which(stringr::str_detect(content, "^```$"))
  end_line <- chunk_ends[chunk_ends > start_line][1] # grab the first one

  if (is.na(end_line)) {
    end_line <- length(content)
  }

  return(list(start = start_line, end = end_line))

}

#' Function to find reference text
#' @param content QMD/RMD file in .txt format
#' @param chunk_label Chunk label
#' @param chunk_start Line number that the chunk starts
#' @param chunk_end Line number that the chunk ends
#' @import stringr
find_reference_text <- function(content, chunk_label, chunk_start, chunk_end) {
  # content: readLines(".qmd")
  # chunk_label: a single label name
  # chunk_start, chunk_end: line where the boundary of the code begins/end. Output from find_chunk_location()
  # Return: Start and end position of each code chunk. If it is an empty chunk, start and end position are set to NA.


  paragraph <- NULL

  reference_pattern <- c(
    paste0("@fig-", chunk_label),
    paste0("\\{#fig-", chunk_label, "\\}"),
    paste0("Figure.*", chunk_label),
    paste0("figure.*", chunk_label),
    chunk_label
    # "figure.*above", "figure.*below",
    # "plot.*above","plot.*below",
    # "following.*figure", "following.*plot",
    # "graph.*above", "graph.*below"
  )


  for (i in seq_along(content)) {

    if (i >= chunk_start && i <= chunk_end) next # Skip code chunks

    line <- content[i]

    for (p in reference_pattern) {
      if (stringr::str_detect(line, p)) {

        # Not all paragraphs are written in one long line. So a paragraph is defined by white space at both end.
        paragraph <- extract_paragraph(content, i)
        break
      }
    }

  }

  return(paragraph)

}

#' Function that extract reference text using chunk_label
#' @param content QMD/RMD file in .txt format
#' @param line_number Line number that contains `@chunk_label`
#' @import stringr
extract_paragraph <- function(content, line_number) {

  start_line <- line_number
  end_line <- line_number

  # go backwards to find paragraph start: stop when we hit white space
  while (start_line > 1 &&
         !stringr::str_detect(content[start_line - 1], "^\\s*$") &&
         !stringr::str_detect(content[start_line - 1], "^#+\\s")) {
    start_line <- start_line - 1
  }

  # go forwards to find paragraph end: stop when we hit white space or beginning of a new code chunk
  while (end_line < length(content) &&
         !stringr::str_detect(content[end_line + 1], "^\\s*$") &&
         !stringr::str_detect(content[end_line + 1], "^#+\\s") &&
         !stringr::str_detect(content[end_line + 1], "^```")) {
    end_line <- end_line + 1
  }

  paragraph <- paste(content[start_line:end_line], collapse = " ")
  paragraph <- stringr::str_trim(stringr::str_squish(paragraph))

  return(paragraph)
}










