#' Function to generate alt-text
#' @param file_path Path to the QMD/RMD file
#' @param api API key
#' @param userinstruct Optional user instruction. This will be appended to the system prompt.
generate_alt_text <- function(file_path = NULL, api = NULL, userinstruct = ""){

  if (is.null(file_path)){
    stop("Missing file path")
  }

  if (is.null(api)){
    stop("Missing Open-AI API key. Information on how to obtain an API key can be found here https://help.openai.com/en/collections/3675931-api")
  }

  if (!is.character(api)) {
    stop("API needs to be in a character string.  Information on how to obtain an API key can be found here https://help.openai.com/en/collections/3675931-api")
  }


  content <- extract_ggplot_code(file_path)

  body_list <- list(
    model = "gpt-4.1",
    api_key = api,
    userinstruct = userinstruct,
    max_token = 2048
  )


  result <- client_responses(body_list , content)

  alt_text <- paste(
    "Chunk label: ", result$chunk_label,
    "\nAlt-text: ", result$response,
    "\nUsage: ", result$usage,
    "\n",
    sep = "", collapse = "\n"
  )

  writeLines(alt_text, "alt-text.txt")
  print("Output saved to alt-text.txt")


}

#' Function to sent HTTP request to OpenAI
#' @param body_list default OpenAI parameters
#' @param content Parsed content
client_responses <- function(body_list , content){

  sysprompt <- "You are a researcher tasked with generating one concise version of alt-text for a graph, based on R code, BrailleR output, and reference text.

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

1. Chart type identified.
2. Axes and variables clearly named.
3. Approximate ranges or scales mentioned (where meaningful).
4. Data mappings (e.g. colour/shape/size/facets) described.
5. Main patterns, trends, or clusters described.
6. Any assumptions explicitly noted.

For each checklist item, respond with “Yes” or “No”.

Do not provide separate explanations or interpretations of the R code, reference text, or BrailleR output. Use them only as sources to inform the single piece of alt-text and the checklist."

  chat <- ellmer::chat_openai(
    model = body_list$model,
    api_key = body_list$api_key,
    system_prompt = paste(body_list$userinstruct, sysprompt)
  )


  output <- data.frame(
    chunk_label = character(0),
    response = character(0),
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
    print(paste0("Evaluating ", content[[i]]$chunk_label, "..."))
    response <- chat$chat(paste0(sysprompt, client_input, reference_text))
    usage <- paste0("BrailleR",
                    ", Cummulated cost: ", round(chat$get_cost()[1], 3),
                    ", Cummulated token usage: ", sum(chat$get_tokens()[3])[1]
    )

    output[nrow(output) + 1,] <-  list(content[[i]]$chunk_label, response,usage)


  }


  return(output)

}
