# Accepted extension
DOC_EXT <- c("qmd", "rmd")
IMG_EXT <- c("png", "jpg", "jpeg", "webp", "gif")

# Function to determine class of input
determine_class <- function(input) {
  files <- expand_paths(input)

  exts <- tolower(tools::file_ext(files))

  # Determine if the input is a Doc (RMD/QMD) or Image
  is_doc <- exts %in% DOC_EXT
  is_img <- exts %in% IMG_EXT

  if (!any(is_doc | is_img)) {
    return(structure(input, class = "alt_unknown"))
  }

  cls <- if (all(is_doc)) {
    if (all(exts[is_doc] == "rmd")) "rmd" else "qmd"
  } else if (all(is_img)) {
    "image"
  } else {
    "mixed"
  }

  structure(input, class = cls)
}

# Rationale: Ideally, we want mulitple input (null plot and line up plot)
expand_paths <- function(input) {
  files <- unlist(lapply(input, function(p) {
    if (dir.exists(p)) {
      list.files(
        p,
        pattern = paste0(
          "\\.(",
          paste(c(DOC_EXT, IMG_EXT), collapse = "|"),
          ")$"
        ),
        full.names = TRUE,
        ignore.case = TRUE
      )
    } else {
      p
    }
  }))

  missing <- files[!file.exists(files)]
  if (length(missing) > 0) {
    stop("File(s) not found: ", paste(missing, collapse = ", "))
  }

  files
}

validate_input <- function(outfile, api) {
  if (is.null(outfile)) {
    outfile <- "alt-text.txt"
    warning("Writing to alt-text.txt")
  }

  if (is.null(api)) {
    stop(
      "Missing OpenAI API key. Information on how to obtain an API key can be found here: https://help.openai.com/en/collections/3675931-api"
    )
  }

  if (!is.character(api)) {
    stop(
      "API key needs to be in a character string. Information on how to obtain an API key can be found here: https://help.openai.com/en/collections/3675931-api"
    )
  }

  return(outfile)
}

new_alt_item <- function(kind, label, source, reference_paragraph = NULL, ...) {
  structure(
    list(
      label = label,
      source = source,
      reference_paragraph = reference_paragraph,
      ...
    ),
    class = c(paste0("alt_item_", kind), "alt_item")
  )
}

client_responses <- function(body_list, content) {
  if (length(content) == 0) {
    stop("No plots found in the supplied input")
  }

  kind <- class(content[[1]])[1] # class "alt_item_code" or "alt_item_image"

  chat <- ellmer::chat_openai(
    model = body_list$model,
    api_key = body_list$api_key,
    system_prompt = paste(body_list$user_instruct, system_prompt(kind))
  )

  usage_tag <- if (kind == "alt_item_image") "Visualisation" else "BrailleR"

  output <- data.frame(
    chunk_label = character(0),
    response = character(0),
    reference_paragraph = character(0),
    usage = character(0)
  )

  total_cost <- 0
  total_token <- 0

  for (i in seq_along(content)) {
    # For image input
    if (kind == "image") {
      client_input <- list(
        ellmer::content_image_file(
          path = content[[i]]$image_path,
          content_type = "auto",
          resize = "high"
        )
      )
      # For RMD/QMD input
    } else if (nzchar(paste(content[[i]]$chunk_code, collapse = ""))) {
      # Token limit : 30000 / 7500 char
      # System prompt: 763 char

      env <- new.env(parent = globalenv())

      # BrailleR only works for self-contained example hence tryCatch
      braille_text <- tryCatch(
        {
          user_expr <- parse(
            text = paste(content[[i]]$chunk_code, collapse = "\n")
          )
          plot_obj <- eval(user_expr, envir = env)

          brailleR_output <- BrailleR::VI(plot_obj)

          if (sum(nchar(brailleR_output$text)) >= body_list$max_token) {
            " "
          } else {
            paste(brailleR_output$text, collapse = "\n")
          }
        },
        # In case BrailleR throws an error
        error = function(e) ""
      )

      client_input <- if (!nzchar(braille_text)) {
        list(paste0(
          "Interpret this code and use the interpretation to generate alt-text: ",
          paste(content[[i]]$chunk_code, collapse = "\n")
        ))
      } else {
        list(paste0("BrailleR input: ", braille_text))
      }
    }

    if (!is.null(content[[i]]$reference_paragraph)) {
      reference_text <- paste0(
        "Reference text: ",
        content[[i]]$reference_paragraph,
        collapse = ""
      )
    } else {
      reference_text <- ""
    }

    # HTTP request
    respond <- do.call(
      chat$chat,
      c(client_input, if (nzchar(reference_text)) list(reference_text))
    )

    total_cost <- total_cost + chat$get_cost()[1]
    total_tokens <- total_token + sum(chat$get_tokens()[1:2])[1]

    usage <- paste0(
      usage_tag,
      ", Cummulated cost: ",
      round(total_cost, 3),
      ", Cummulated token usage: ",
      total_tokens
    )

    output[nrow(output) + 1, ] <- list(
      content[[i]]$label,
      respond,
      if (is.null(content[[i]]$reference_paragraph)) {
        NA_character_
      } else {
        content[[i]]$reference_paragraph
      },
      usage
    )

    # reset chat
    chat$set_turns(list())
  }

  return(output)
}


write_alt_text <- function(input, outfile) {
  alt_text <- glue::glue(
    "# Chunk label: {input$chunk_label} --------------------",
    "\n## Alt-text: {input$response}",
    "\n\n## Caption (for reference): {input$reference_paragraph}",
    "\n\n## Usage: {input$usage}",
  ) |>
    paste(collapse = "\n\n\n")

  writeLines(alt_text, outfile)
  message(paste0("Output saved to ", outfile))
}


# System Prompt ---------------------------------------------------------------------

altText_guideline <- "
Naturally describes:
    - the chart type
    - the variables on each axis
    - approximate axis ranges
    - how data are mapped to visual elements (e.g. colour, shape, size, facets)
    - any visible patterns, relationships, clusters, trends, or notable outliers
    - Includes brief interpretation of the data where this is relevant to understanding the plot.
    - Avoids starting with phrases such as “Alt-text:” and does not use labels like “Iteration”.

If the prompt lacks detail, make reasonable assumptions. Clearly flag these assumptions in a short note after the alt-text.
After you have written the alt-text, generate a short checklist confirming whether you have covered the following items for this specific graph:

1. Identified chart type.
2. Named axes and variables.
3. Mentioned approximate ranges or scales (where meaningful).
4. Described data mappings (e.g. colour/shape/size/facets).
5. Described main patterns, trends, or clusters.
6. Explicitly noted any assumptions.

For each checklist item, respond with “YES” or “NO”.
"

system_prompt <- function(kind) {
  begin <- switch(
    kind,
    alt_item_code = " You are a researcher tasked with generating one concise version of alt-text for a graph, based on R code, BrailleR output, and reference text.
    Your role is to analyse the available information (R code, BrailleR output, statistical summaries, and reference text) and produce clear, informative alt-text that: ",
    alt_item_image = " You are a researcher tasked with generating one concise version of alt-text for a graph, based on image provided and reference text.
    Read the value directly from the image. Produce clear, informative alt-text that: "
  )

  end <- switch(
    kind,
    alt_item_code = "Do not provide separate explanations or interpretations of the R code, reference text, or BrailleR output. Use them only as sources to inform the single piece of alt-text and the checklist.",
    alt_item_image = "Do not guess at number you cannot see. If any part of the image is illegible or ambigious, say so in a short note after the alt-text rather than inventing a value. 
    Do not provide separate explanations or interpretations of the image or reference text. Use them only as sources to inform the single piece of alt-text and the checklist."
  )

  paste0(begin, altText_guideline, end)
}

#

# Generic ---------------------------------------------------------------------------

#' Function to generate alt-text for data visualisations in a Quarto or R Markdown file
#' @param flnm Character string. Path and file name for the qmd or rmd file containing the plots.
#' @param outfile Character string. Path and file name for the output file of alt-text. If not provided will write to alt-text.txt in current folder.
#' @param openai_model Character string. Name of the OpenAI model used to generate alt-text.
#' @param api Character string. OpenAI API key used for authentication.
#' @param user_instruct Character string (optional). Additional user instructions to refine the style or content of the alt-text; to be appended the default system prompt.
#' @import glue
#' @export
generate_alt_text <- function(
  flnm = NULL,
  outfile = NULL,
  openai_model = "gpt-5.1",
  api = NULL,
  user_instruct = ""
) {
  if (is.null(flnm)) {
    stop("Missing input file.")
  }

  UseMethod(
    "generate_alt_text",
    determine_class(flnm)
  )
}


#' Default reserved for unknown class
#' @export
generate_alt_text.default <- function(input = NULL, ...) {
  stop(
    "Don't know how to generate alt-text from that input. ",
    "Supported: ",
    paste0(".", c(DOC_EXT, IMG_EXT), collapse = ", "),
    "."
  )
}

#' @export
generate_alt_text.alt_unknown <- generate_alt_text.default


# Method: Quarto/RMD ---------------------------------------------------------------

generate_alt_text.rmd <- function(flnm = NULL, ...) {
  stop("Incomplete")
}

#' @export
generate_alt_text.qmd <- function(
  flnm = NULL,
  outfile = NULL,
  openai_model = "gpt-5.1",
  api = NULL,
  user_instruct = ""
) {
  outfile <- validate_input(outfile, api)

  files <- expand_paths(flnm)

  # for i in item: extract_ggplot_code(i)
  items <- unlist(
    lapply(files, function(f) {
      # For chunks in extract_ggplot_code(i): new_alt_item(ch) to determine an object of class "alt_item_code" or "alt_item_image"
      lapply(extract_ggplot_code(f), function(ch) {
        new_alt_item(
          kind = "code",
          label = ch$chunk_label,
          source = f,
          reference_paragraph = ch$reference_paragraph,
          chunk_code = ch$chunk_code
        )
      })
    }),
    recursive = FALSE
  )

  body_list <- list(
    model = openai_model,
    api_key = api,
    user_instruct = user_instruct,
    max_token = 2048
  )

  result <- client_responses(body_list, items)
  write_alt_text(result, outfile)
}

generate_alt_text.img <- function(flnm = NULL, ...) {
  stop("Incomplete")
}

#   content <- extract_ggplot_code(flnm)

#   body_list <- list(
#     model = openai_model,
#     api_key = api,
#     user_instruct = user_instruct,
#     max_token = 2048
#   )

#   result <- client_responses(body_list, content)

#   alt_text <- glue::glue(
#     "# Chunk label: {result$chunk_label} --------------------",
#     "\n## Alt-text: {result$response}",
#     "\n\n## Caption (for reference): {result$reference_paragraph}",
#     "\n\n## Usage: {result$usage}",
#   ) |>
#     paste(collapse = "\n\n\n")

#   writeLines(alt_text, outfile)
#   message(print(paste("Output saved to ", outfile)))
# }

# #' Function to sent HTTP request to OpenAI
# #' @param body_list default OpenAI parameters
# #' @param content Parsed content
# client_responses <- function(body_list, content) {
#   sys_prompt <- "You are a researcher tasked with generating one concise version of alt-text for a graph, based on R code, BrailleR output, and reference text.

# Your role is to analyse the available information (R code, BrailleR output, statistical summaries, and reference text) and produce clear, informative alt-text that:

# - Naturally describes:
#   - the chart type
#   - the variables on each axis
#   - approximate axis ranges
#   - how data are mapped to visual elements (e.g. colour, shape, size, facets)
#   - any visible patterns, relationships, clusters, trends, or notable outliers
# - Includes brief interpretation of the data where this is relevant to understanding the plot.
# - Avoids starting with phrases such as “Alt-text:” and does not use labels like “Iteration”.

# If the prompt lacks detail, make reasonable assumptions. Clearly flag these assumptions in a short note after the alt-text.

# After you have written the alt-text, generate a short checklist confirming whether you have covered the following items for this specific graph:

# 1. Identified chart type.
# 2. Named axes and variables.
# 3. Mentioned approximate ranges or scales (where meaningful).
# 4. Described data mappings (e.g. colour/shape/size/facets).
# 5. Described main patterns, trends, or clusters.
# 6. Explicitly noted any assumptions.

# For each checklist item, respond with “YES” or “NO”.

# Do not provide separate explanations or interpretations of the R code, reference text, or BrailleR output. Use them only as sources to inform the single piece of alt-text and the checklist."

#   chat <- ellmer::chat_openai(
#     model = body_list$model,
#     api_key = body_list$api_key,
#     system_prompt = paste(body_list$user_instruct, sys_prompt)
#   )

#   output <- data.frame(
#     chunk_label = character(0),
#     response = character(0),
#     reference_paragraph = character(0),
#     usage = character(0)
#   )

# for (i in seq_along(content)) {
#   if (nzchar(content[[i]]$chunk_code)) {
#     client_input <- " "

#     # Token limit : 30000 / 7500 char
#     # System prompt: 763 char
#     tryCatch(
#       {
#         user_expr <- parse(
#           text = paste(content[[i]]$chunk_code, collapse = "\n")
#         )
#         # vi_expression <-  paste0("VI({\n", paste(body_list$input_code, collapse = "\n"), "\n})")
#         # brailleR_output <- eval(parse(text = vi_expression))
#         plot_obj <- eval(user_expr)

#         brailleR_output <- VI(plot_obj)

#         if (sum(nchar(brailleR_output$text)) >= body_list$max_token) {
#           client_input <- ""
#         } else {
#           client_input <- brailleR_output$text
#         }
#       },
#       error = function(e) {
#         class(client_input) <- "error"
#       }
#     )

#     # In case BrailleR throws an error
#     if (inherits(client_input, "error") || !nzchar(client_input)) {
#       client_input <- paste0(
#         "Interpret this code and use the interpreation to generate alt-text",
#         content[[i]]$chunk_code
#       )
#     }
#   } else {
#     client_input <- " "
#   }

#     client_input <- paste0("BrailleR input: ", client_input, collapse = "")

#     if (!is.null(content[[i]]$reference_paragraph)) {
#       reference_text <- paste0(
#         "Reference text: ",
#         content[[i]]$reference_paragraph,
#         collapse = ""
#       )
#     } else {
#       reference_text <- ""
#     }

#     # HTTP request
#     message(paste0("Evaluating ", content[[i]]$chunk_label, "..."))
#     response <- chat$chat(paste0(sys_prompt, client_input, reference_text))
#     usage <- paste0(
#       "BrailleR",
#       ", Cummulated cost: ",
#       round(chat$get_cost()[1], 3),
#       ", Cummulated token usage: ",
#       sum(chat$get_tokens()[3])[1]
#     )

#     output[nrow(output) + 1, ] <- list(
#       content[[i]]$chunk_label,
#       response,
#       content[[i]]$reference_paragraph,
#       usage
#     )
#   }

#   return(output)
# }
