

#' Query SAGES codebook
#'
#' This function searches the SAGES codebook for the specified query terms.  Its
#'   goal is to make it easier to find variables of interest, and to find which
#'   data files contain those variables.
#'
#' @param qterm string The term to search for in the codebook.
#' @param gt_table logical Should the results be returned as a tibble or in a gt() table?
#' @param searchfields string The column names of the codebook to search.
#' @param codebook_location string The location of the codebook.
#'
#' @export
#' @return A tibble containing the rows of the codebook that contain the search term.
#'
#' @examples
#' qsagesmetadata("ID")

qsagesmetadata <- function(qterm, gt_table = FALSE,
                           searchfields = c("variablefieldname",
                                            "fieldlabel",
                                            "choicescalculationsorsliderlabel",
                                            "fieldnote",
                                            "variablefieldname_3_99",
                                            "choicescalculationssliderlabels"),
                           codebook_location = "https://quantsci.s3.amazonaws.com/Work/SAGES/SAGES_metadata.csv") {

  codebook <- readr::read_csv(codebook_location, show_col_types = FALSE)
  queried_codebook <- codebook %>%
    dplyr::filter_at(.vars = searchfields,
              .vars_predicate = dplyr::any_vars(stringr::str_detect(. , qterm)))

  queried_codebook <- queried_codebook %>%
    dplyr::select(c("variablefieldname", "formname", "sectionheader", "fieldlabel", "var2char", "variablefieldname_3_99"))

  if (gt_table == TRUE) {
    queried_codebook <- gt::gt(queried_codebook)
  }

  return(queried_codebook)
}


