#' basic_genus is used to merge initial URL for genus method.
#'
#' @param name The name or partial name of genus taxon.
#' @param searchOnlyByFirstSymbols Whether to filter records whose Name starts with name parameter, default no.
#' @param order Default is "name". Decide to sort the results by name("name") or year("yearFrom").
#' @param orderDirection Default is "asc". Determines whether the results are in positive("asc") or reverse("desc") order.
#' @param author Select the record authorized by specific person.
#' @param originalSpelling Select the record whose original spelling is given string.
#' @param yearFrom Records published after the year of "yearFrom".
#' @param yearTo Records published before the year of "yearTo".
#' @param rank Select records in one or more specific classification levels.
#' Default is FALSE, not specifying any classification level means not filtering.
#' Either character type or numeric type can be used. The supported characters of classification levels are c('Genus', 'Supergenus', 'Group', 'Subgroup', 'Unranked'), and the corresponding numbers are c(68, 43, 69, 45, 56).
#'
#' @return URL
#' @export
#'
#' @examples
#' basic_genus_url <- basic_genus("sor", searchOnlyByFirstSymbols = "on")
#'
#' @import rvest
#' @import magrittr
#' @import checkmate
#' @import dplyr


basic_genus <-
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
    genus_rank <- c(68, 43, 69, 45, 56)
    names(genus_rank) <- c('Genus', 'Supergenus', 'Group', 'Subgroup', 'Unranked')
    if (all(rank != FALSE)){
      if (all(rank %in% names(genus_rank))){
        rank <- paste0("rankID[]=", genus_rank[rank], collapse = "&")
      }
      else if (all(rank %in% genus_rank)){
        rank <- paste0("rankID[]=", rank, collapse = "&")
      }}else{rank=""}

    form <- paste(paste0("name=",name), paste0("searchOnlyByFirstSymbols=",searchOnlyByFirstSymbols),
                  paste0("order=",order),paste0("orderDirection=",orderDirection),
                  paste0("author=",author),paste0("originalSpelling=",originalSpelling),
                  paste0("yearFrom=",yearFrom),paste0("yearTo=",yearTo), rank,
                  sep = "&")

    base_url <-
      "http://ifpni.org/genus.htm?formIndex=def&submitForm=Search&isExtended=1&"

    return(paste0(base_url, form))
  }

#' genus() is used to implement retrieval from IFPNI's genus list accroding to different filting conditions.
#'
#' @param name The name or partial name of genus taxon.
#' @param searchOnlyByFirstSymbols Whether to filter records whose Name starts with name parameter, default no.
#' @param order Default is "name". Decide to sort the results by name("name") or year("yearFrom").
#' @param orderDirection Default is "asc". Determines whether the results are in positive("asc") or reverse("desc") order.
#' @param author Select the record authorized by specific person.
#' @param originalSpelling Select the record whose original spelling is given string.
#' @param yearFrom Records published after the year of "yearFrom".
#' @param yearTo Records published before the year of "yearTo".
#' @param rank Select records in one or more specific classification levels.
#' Default is FALSE, not specifying any classification level means not filtering.
#' Either character type or numeric type can be used. The supported characters of classification levels are c('Genus', 'Supergenus', 'Group', 'Subgroup', 'Unranked'), and the corresponding numbers are c(68, 43, 69, 45, 56).
#'
#'
#' @return A dataframe with detailed information of result.
#' @export
#'
#' @examples
#' # let's see how many taxa start with "sor" in the genus list on IFPNI.
#' sor_genus <- genus("sor", searchOnlyByFirstSymbols = "on")
#'
#' # Let's try the most daring operation.
#' # If you do not pass a value to any argument, you get a dataframe that stores the details of all the taxa in the genus list on IFPNI.
#'
#' # all_genus_url <- basic_genus()
#' # brief_all_genus <- brief_information(all_genus_url)
#' # write.csv(brief_all_genus, "/data/brief.all_genus.csv")
#'
#' # ✔ There are 1754 pages found.
#' # Collected 1754 pages from IFPNI. Elapsed Time: 01:31:21.3
#'
#' # all_genus_urls <- as.character(brief_all_genus$URL)
#' # all_genus <- detail_graber(all_genus_urls)

genus <- function(name=NULL,
                       searchOnlyByFirstSymbols = "",
                       order = "name",
                       orderDirection = "asc",
                       author = "",
                       originalSpelling = "",
                       yearFrom = "",
                       yearTo = "",
                       rank = FALSE){
  cli_h1(paste("Executing",col_green(style_italic("genus")) ,"function" ))
  url <- basic_genus(name = name,searchOnlyByFirstSymbols = searchOnlyByFirstSymbols,
                          order=order, orderDirection = orderDirection,author=author,
                          originalSpelling = originalSpelling,yearFrom = yearFrom,
                          yearTo = yearTo, rank = rank)
  return <- basic_graber(url)
}
