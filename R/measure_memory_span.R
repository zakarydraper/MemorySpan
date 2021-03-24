#' Measure Memory Span
#'
#' Does a memory span test using the R console.
#'
#' @param span_length the length of the first sequence of digits or letters.
#' Increments by 1 after each correct response.
#' @param type measure "digit" (default) or "letter" span?
#' @param order should the response be returned in "forward" order (default),
#' "reverse" order, or "random" (i.e., switch randomly between forward and
#' reverse order)?
#'
#' @return The length of the longest sequence the user correctly repeated.
#' @export
#'
#' @examples

measure_memory_span <- function(span_length = 1, type = c("digit", "letter"),
  order = c("forward", "reverse", "random")) {
  type <- match.arg(type)
  order <- match.arg(order)

  if (order == "random") {
    random <- TRUE
  } else {
    random <- FALSE
  }

  if (type == "digit") {
    chunks <- 0:9
  } else if (type == "letter") {
    chunks <- LETTERS
  }

  fail <- FALSE

  message(sprintf("\014Measuring %s order %s span.", order, type))

  readline("Press enter when you are ready to start.")

  while (isFALSE(fail)) {
    if (random) {
      order <- sample(c("forward", "reverse"), 1)
    }
    prompt <- sample(chunks, span_length, replace = TRUE)
    message(sprintf("\014Get ready for %i %ss...", span_length, type))
    Sys.sleep(2)
    for (d in seq_along(prompt)) {
      message("\014", prompt[[d]])
      Sys.sleep(1)
      cat("\014")
      Sys.sleep(0.5)
    }

    # Get response from subject
    response <- readline(sprintf("repeat the %ss in %s order: ", type, order))

    # Format response
    if (order == "reverse") {
      prompt <- rev(prompt)
    }

    prompt <- paste(prompt, collapse = "")

    if (type == "digit") {
      response <- as.character(response)
    } else if (type == "letter") {
      prompt <- tolower(prompt)
      response <- tolower(response)
    }

    # message("\014")

    # Was the response correct?
    if (response != prompt) {
      if (type == "letter") {
        prompt <- toupper(prompt)
      }

      final_message <- paste0(
        "The correct response was \"%s\". ",
        "Your %s order %s span for this trial was:"
      )
      message(sprintf(final_message, prompt, order, type))
      fail <- TRUE
    } else {
      span_length <- span_length + 1
    }
  }
  return(span_length - 1)
}
