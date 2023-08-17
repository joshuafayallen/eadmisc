#' Wrapper for Will's Extract JSON function
#' @param your_list is a list of json files
#' @export


extract_json <- function(your_list) {
  if(!isTRUE(is.list(your_list))){
    stop(paste("This must be a list silly but it is ", typeof(your_list)))
  } else{
  output <- purrr::map(your_list, ~ {
    x <- jsonlite::fromJSON(stringr::str_replace(.x, "\r\n", ""))
    x <- x$value
    x <- tidyr::unnest(x, Document, names_repair = "unique")
    x <- tidyr::unnest(x, Source, names_repair = "unique")
    x <- dplyr::select(x, Byline, WordLength, Content, Section, Date, Title, Source = Name)
    return(x)
  })
  output = output |> purrr::list_rbind()
  return(output)
} }

#' @export
