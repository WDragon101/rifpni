#' basic_infragenus is used to merge initial URL for infragenus method.
#'
#' @param name The name or partial name of infragenus taxon.
#' @param searchOnlyByFirstSymbols Whether to filter records whose Name starts with name parameter, default no.
#' @param order Default is "name". Decide to sort the results by name("name") or year("yearFrom").
#' @param orderDirection Default is "asc". Determines whether the results are in positive("asc") or reverse("desc") order.
#' @param author Select the record authorized by specific person.
#' @param originalSpelling Select the record whose original spelling is given string.
#' @param yearFrom Records published after the year of "yearFrom".
#' @param yearTo Records published before the year of "yearTo".
#' @param rank Select records in one or more specific classification levels.
#' Default is FALSE, not specifying any classification level means not filtering.
#' Either character type or numeric type can be used. The supported characters of classification levels are c('Genus', 'Infragenus', 'Subgenus', 'Section', 'Subsection', 'Supersection', 'Series', 'Subseries', 'Superseries', 'Unranked'), and the corresponding numbers are c(70, 47, 48, 49, 50, 51, 52, 53, 54, 55).
#'
#' @return URL
#' @export
#'
#' @examples
#' basic_infragenus_url <- basic_infragenus("ty")
#'
#' @import rvest
#' @import magrittr
#' @import checkmate
#' @import dplyr


basic_infragenus <-
  function(name=NULL,
           searchOnlyByFirstSymbols = "",
           order = "name",
           orderDirection = "asc",
           author = "",
           originalSpelling = "",
           yearFrom = "",
           yearTo = "",
           rank = FALSE) {
    checkChoice(searchOnlyByFirstSymbols, c("","on"))
    checkChoice(order, c("name","yearFrom"))
    checkChoice(orderDirection,c("desc","asc"))
    infragenus_rank <- c(70, 47, 48, 49, 50, 51, 52, 53, 54, 55)
    names(infragenus_rank) <- c('Genus', 'Infragenus', 'Subgenus', 'Section', 'Subsection', 'Supersection', 'Series', 'Subseries', 'Superseries', 'Unranked')
    if (all(rank != FALSE)){
      if (all(rank %in% names(infragenus_rank))){
        rank <- paste0("rankID[]=", infragenus_rank[rank], collapse = "&")
      }
      else if (all(rank %in% infragenus_rank)){
        rank <- paste0("rankID[]=", rank, collapse = "&")
      }}else{rank=""}

    form <- paste(paste0("name=",name), paste0("searchOnlyByFirstSymbols=",searchOnlyByFirstSymbols),
                  paste0("order=",order),paste0("orderDirection=",orderDirection),
                  paste0("author=",author),paste0("originalSpelling=",originalSpelling),
                  paste0("yearFrom=",yearFrom),paste0("yearTo=",yearTo), rank,
                  sep = "&")

    base_url <-
      "http://ifpni.org/infragenus.htm?formIndex=def&submitForm=Search&isExtended=1&"

    return(paste0(base_url, form))
  }

#' infragenus() is used to implement retrieval from IFPNI's infragenus list accroding to different filting conditions.
#'
#' @param name The name or partial name of infragenus taxon.
#' @param searchOnlyByFirstSymbols Whether to filter records whose Name starts with name parameter, default no.
#' @param order Default is "name". Decide to sort the results by name("name") or year("yearFrom").
#' @param orderDirection Default is "asc". Determines whether the results are in positive("asc") or reverse("desc") order.
#' @param author Select the record authorized by specific person.
#' @param originalSpelling Select the record whose original spelling is given string.
#' @param yearFrom Records published after the year of "yearFrom".
#' @param yearTo Records published before the year of "yearTo".
#' @param rank Select records in one or more specific classification levels.
#' Default is FALSE, not specifying any classification level means not filtering.
#' Either character type or numeric type can be used. The supported characters of classification levels are c('Genus', 'Infragenus', 'Subgenus', 'Section', 'Subsection', 'Supersection', 'Series', 'Subseries', 'Superseries', 'Unranked'), and the corresponding numbers are c(70, 47, 48, 49, 50, 51, 52, 53, 54, 55).
#'
#'
#' @return A dataframe with detailed information of result.
#' @export
#'
#' @examples
#' # let's see how many taxa names contain "ty" in the infragenus list on IFPNI.
#' ty_infragenus <- infragenus("ty")
#' nrow(ty_infragenus)
#' # [1] 16
#' # What are they?
#' ty_infragenus$Name
#'  #[1] "Anomozamites Platypterygium"        "Calamites Arthropitys"
#'  #[3] "Calamites Arthropitys"              "Calamites Stylocalamites"
#'  #[5] "Calamites Stylocalamites"           "Dictyophyllum Camptopteris"
#'  #[7] "Filicites Dictyopteris"             "Fucoides Dictyotites"
#'  #[9] "Phlebopteris Dictyopteris"          "Pinites Pityanthus"
#'  #[11] "Pinites Pityocladus"                "Pinites Pityolepis"
#'  #[13] "Pinites Pityophyllum"               "Pinites Pityospermum"
#'  #[15] "Pissadendron Pitys"                 "Zosterophyllum Platyzosterophyllum"
#'
#' # see data/data/ty_infragenus.rda for details.
#'
#' # Let's try the most daring operation.
#' # If you do not pass a value to any argument, you get a dataframe that stores the details of all the taxa in the infragenus list on IFPNI.
#' # all_infragenus_20231109 <- infragenus()
#' # This operation may take more than ten minutes. See data/all_infragenus_20231109.rda for the result.

infragenus <- function(name=NULL,
                  searchOnlyByFirstSymbols = "",
                  order = "name",
                  orderDirection = "asc",
                  author = "",
                  originalSpelling = "",
                  yearFrom = "",
                  yearTo = "",
                  rank = FALSE){
  url <- basic_infragenus(name = name,searchOnlyByFirstSymbols = searchOnlyByFirstSymbols,
                     order=order, orderDirection = orderDirection,author=author,
                     originalSpelling = originalSpelling,yearFrom = yearFrom,
                     yearTo = yearTo, rank = rank)
  return <- basic_graber(url)
}
