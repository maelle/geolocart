#' Find locations in text
#'
#' @import openNLP
#' @import NLP
#' @import openNLPmodels.en
#' @param text a string
#'
#' @return vector of countries
#' @export
#'
#' @examples
#' text <- "I live in Barcelona but I was born in Vannes and I have worked in Berlin in the past."
#' geolocart_locate(text)
geolocart_locate <- function(text){

  # preparation for text mining
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  annotatorLocation <- Maxent_Entity_Annotator(language = "en", kind = "location")

  text <- as.String(text)
  a2 <- annotate(text, list(sent_token_annotator, word_token_annotator))
  where <- annotatorLocation(text, a2)


  return(text[where])
}
