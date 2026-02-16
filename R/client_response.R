#' Function to generate alt-text for data visualisations in a Quarto or R Markdown file
#' @param flnm Character string. Path and file name for the qmd or rmd file containing the plots.
#' @param outfile Character string. Path and file name for the output file of alt-text. If not provided will write to alt-text.txt in current folder.
#' @param openai_model Character string. Name of the OpenAI model used to generate alt-text.
#' @param api Character string. OpenAI API key used for authentication.
#' @param user_instruct Character string (optional). Additional user instructions to refine the style or content of the alt-text; to be appended the default system prompt.
#' @import glue
#' @export
generate_alt_text <- function(flnm = NULL, outfile = NULL, openai_model = "gpt-5.1", api = NULL, user_instruct = ""){

  if (is.null(flnm)){
    stop("Missing input file.")
  }

  if (is.null(outfile)){
    outfile = "alt-text.txt"
    warning("Writing to alt-text.txt")
  }

  if (is.null(api)){
    stop("Missing OpenAI API key. Information on how to obtain an API key can be found here: https://help.openai.com/en/collections/3675931-api")
  }

  if (!is.character(api)) {
    stop("API key needs to be in a character string. Information on how to obtain an API key can be found here: https://help.openai.com/en/collections/3675931-api")
  }



  content <- extract_ggplot_code(flnm)


  body_list <- list(
    model = openai_model,
    api_key = api,
    user_instruct = user_instruct,
    max_token = 2048
  )


  result <- client_responses(body_list , content)

  alt_text <- glue::glue(
    "# Chunk label: {result$chunk_label} --------------------",
    "\n## Alt-text: {result$response}",
    "\n\n## Caption (for reference): {result$reference_paragraph}",
    "\n\n## Usage: {result$usage}",
  ) |>
    paste(collapse = "\n\n\n")

  writeLines(alt_text, outfile)
  message(print(paste("Output saved to ", outfile)))


}

#' Function to sent HTTP request to OpenAI
#' @param body_list default OpenAI parameters
#' @param content Parsed content
client_responses <- function(body_list , content){

  sys_prompt <- "You are a researcher tasked with generating one concise version of alt-text for a graph, based on R code, BrailleR output, and reference text.

Your role is to analyse the available information (R code, BrailleR output, statistical summaries, and reference text) and produce clear, informative alt-text that:

- Naturally describes:
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

Do not provide separate explanations or interpretations of the R code, reference text, or BrailleR output. Use them only as sources to inform the single piece of alt-text and the checklist."

  chat <- ellmer::chat_openai(
    model = body_list$model,
    api_key = body_list$api_key, #TODO [api_key] arg is now deprecated in chat_openai(). To be refactored to [credentials] arg.
    system_prompt = paste(body_list$user_instruct, sys_prompt)
  )


  output <- data.frame(
    chunk_label = character(0),
    response = character(0),
    reference_paragraph = character(0),
    usage = character(0)
  )


  for (i in seq_along(content)) {
    if (nzchar(content[[i]]$chunk_code)) {

      client_input <- " "

      # Token limit : 30000 / 7500 char
      # System prompt: 763 char
      tryCatch(
        {

          user_expr <- parse(text = paste(content[[i]]$chunk_code, collapse = "\n"))
          # vi_expression <-  paste0("VI({\n", paste(body_list$input_code, collapse = "\n"), "\n})")
          # brailleR_output <- eval(parse(text = vi_expression))
          plot_obj <- eval(user_expr)

          brailleR_output <- VI(plot_obj)


          if (sum(nchar(brailleR_output$text)) >= body_list$max_token) {
            client_input <- ""
          } else {
            client_input <- brailleR_output$text
          }
        },
        error = function(e){
          class(client_input) <- "error"
        }
      )



      # In case BrailleR throws an error
      if(inherits(client_input, "error") || !nzchar(client_input)) {
        client_input <- paste0("Interpret this code and use the interpreation to generate alt-text", content[[i]]$chunk_code)
      }

    } else {
      client_input <- " "
    }

    client_input <- paste0("BrailleR input: ", client_input, collapse = "")

    if (!is.null(content[[i]]$reference_paragraph)) {
      reference_text <- paste0("Reference text: ", content[[i]]$reference_paragraph, collapse = "")
    } else {
      reference_text <- ""
    }

    # HTTP request
    message(paste0("Evaluating ", content[[i]]$chunk_label, "..."))
    response <- chat$chat(paste0(sys_prompt, client_input, reference_text))
    usage <- paste0("BrailleR",
                    ", Cummulated cost: ", round(chat$get_cost()[1], 3),
                    ", Cummulated token usage: ", sum(chat$get_tokens()[3])[1]
    )

    output[nrow(output) + 1, ] <- list(
      content[[i]]$chunk_label,
      response,
      content[[i]]$reference_paragraph,
      usage
    )


  }


  return(output)

}
