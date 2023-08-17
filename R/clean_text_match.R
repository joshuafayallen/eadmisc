#' Cleaner for the text stuff
#'
#' @description
#' This function makes use of str_match. This is probably the function that will
#' the heavy lifting
#' @param df is a tibble or data.frame that is the result of extract json
#' @param our_pattern is a regular expression to capture texts
#' @export




cleaning_text_columns_match = function(df,  our_pattern = "<bodyText><p nitf:lede=\\\"true\\\">\\s*(.*?)\\s*</p></bodyText></nitf:body.content>"){
  our_pattern = rlang::englue("{our_pattern}")

  ln_df = df |>
    dplyr::mutate(extract = stringr::str_match(Content, our_pattern),
           Text = extract[,2]) |>
    dplyr::select(-starts_with("extract"))

  clean_ln_df = ln_df |>
   dplyr::mutate(art_id = paste(Source, Date, seq(1:nrow(df)), sep = "_")) |>
   dplyr::mutate(Text = stringr::str_remove(Text, "</p><p nitf:lede=\"true\">")) |>
    tidyr::separate_rows(Text, sep = "</p><p>") |>
    dplyr::mutate(Text = textutils::HTMLdecode(Text))  |>
    dplyr::mutate(Text = stringr::str_replace_all(Text, "<.*?>", " "),
           characters = nchar(Text),
           par_id = paste(art_id, seq(1:nrow(ln_df)), sep ="_"))



  return(clean_ln_df)
}

#' @export

