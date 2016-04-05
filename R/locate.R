#' Find locations in text and returns corresponding countries
#'
#' @import openNLP
#' @import NLP
#' @import openNLPmodels.en
#'
#' @param text a string
#' @param geonameUser your geoname username
#'
#' @return vector of countries
#' @export
#'
#' @examples
#' text <- "I live in Barcelona but I was born in Vannes and I have worked in Berlin in the past."
#' geolocart_locate(text, "insertYourName")
geolocart_locate <- function(text, geonameUser){
  # geonames requires username
  options(geonamesUsername=geonameUser)

  # preparation for text mining
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  annotatorLocation <- Maxent_Entity_Annotator(language = "en", kind = "location")

  text <- as.String(text)
  a2 <- annotate(text, list(sent_token_annotator, word_token_annotator))
  where <- annotatorLocation(text, a2)

  countries <- NULL
  # and only if the annotator could find a location
  if(length(where)>0){

    # loop over these words
    # using rpackage geonames
    for (place in text[where]){

      geonamesResult <- GNsearch(q=place)
      geonamesResult <- geonamesResult[geonamesResult$name==place,]
      if(ncol(geonamesResult)>0){
        # one has to choose a result from geonames
        pays <- geonamesResult[1,"countryCode"]
        countries <- c(countries, as.character(pays))
      }

    }


  }

  return(countries)
}
