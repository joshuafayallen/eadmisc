#' This is a wrapper around a thing that i trouble shot
#' @description
#' This function uses  extract this should be used if you can't find a regex that works
#' @param df this is a df or tibble
#' @param our_pattern this is a regex defaults to bodytext
#' @export

cleaning_text_columns_extract = function(df,  our_pattern = "<bodyText>.*?<p.*?>(.*?)</p>.*?</bodyText>"){
  our_pattern = rlang::englue("{our_pattern}")

  ln_df = df |>
    dplyr::mutate(extract = stringr::str_extract_all(Content, our_pattern),
           Text = extract) |>
    dplyr::select(-starts_with("extract"))

  clean_ln_df = ln_df |>
    dplyr::mutate(art_id = paste(Source, Date, dplyr::row_number(), sep = "_")) |>
    dplyr::mutate(Text = stringr::str_remove(Text, "</p><p nitf:lede=\"true\">")) |>
    tidyr::separate_rows(Text, sep = "</p><p>") |>
    dplyr::mutate(Text = textutils::HTMLdecode(Text))  |>
    dplyr::mutate(Text = stringr::str_replace_all(Text, "<.*?>", " "),
           characters = nchar(Text),
           par_id = paste(art_id,dplyr::row_number(), sep ="_"))



  return(clean_ln_df)
}


#' @export
