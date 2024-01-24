#' basic_supragenus is used to merge initial URL for supragenus method.
#'
#' @param name The name or partial name of supragenus taxon.
#' @param searchOnlyByFirstSymbols Whether to filter records whose Name starts with name parameter, default no.
#' @param order Default is "name". Decide to sort the results by name("name") or year("yearFrom").
#' @param orderDirection Default is "asc". Determines whether the results are in positive("asc") or reverse("desc") order.
#' @param author Select the record authorized by specific person.
#' @param originalSpelling Select the record whose original spelling is given string.
#' @param yearFrom Records published after the year of "yearFrom".
#' @param yearTo Records published before the year of "yearTo".
#' @param rank Select records in one or more specific classification levels.
#' Default is FALSE, not specifying any classification level means not filtering.
#' Either character type or numeric type can be used. The supported characters of classification levels are c('Regnum', 'Subregnum', 'Phylum', 'Subphylum', 'Superphylum', 'Class', 'Subclass', 'Superclass', 'Infraclass', 'Order', 'Superorder', 'Suborder', 'Infraorder', 'Family', 'Superfamily', 'Subfamily', 'Infrafamily', 'Tribe', 'Supertribe', 'Supersubtribe', 'Subtribe', 'Infratribe', 'Turma', 'Anteturma', 'Suprasubturma', 'Subturma', 'Infraturma', 'Unranked'), and the corresponding numbers are c(2, 3, 5, 6, 7, 9, 10, 11, 12, 66, 14, 15, 16, 18, 19, 20, 21, 23, 24, 25, 26, 27, 67, 29, 30, 31, 32, 59).
#'
#' @return URL
#' @export
#' @examples
#' # basic_supragenus_url <- basic_supragenus("cla")
#'
#' @import rvest
#' @import magrittr
#' @import checkmate
#' @import dplyr

basic_supragenus <- function(name=NULL,
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
    supragenus_rank <- c(2, 3, 5, 6, 7, 9, 10, 11, 12, 66, 14, 15, 16, 18, 19, 20, 21, 23, 24, 25, 26, 27, 67, 29, 30, 31, 32, 59)
    names(supragenus_rank) <- c('Regnum', 'Subregnum', 'Phylum', 'Subphylum', 'Superphylum', 'Class', 'Subclass', 'Superclass', 'Infraclass', 'Order', 'Superorder', 'Suborder', 'Infraorder', 'Family', 'Superfamily', 'Subfamily', 'Infrafamily', 'Tribe', 'Supertribe', 'Supersubtribe', 'Subtribe', 'Infratribe', 'Turma', 'Anteturma', 'Suprasubturma', 'Subturma', 'Infraturma', 'Unranked')
    if (all(rank != FALSE)){
      if (all(rank %in% names(supragenus_rank))){
        rank <- paste0("rankID[]=", supragenus_rank[rank], collapse = "&")
        } else if (all(rank %in% supragenus_rank)){
          rank <- paste0("rankID[]=", rank, collapse = "&")
        }
      }else{rank=""}

    form <- paste(paste0("name=",name), paste0("searchOnlyByFirstSymbols=",searchOnlyByFirstSymbols),
                  paste0("order=",order),paste0("orderDirection=",orderDirection),
                  paste0("author=",author),paste0("originalSpelling=",originalSpelling),
                  paste0("yearFrom=",yearFrom),paste0("yearTo=",yearTo), rank,
                  sep = "&")

    base_url <- "http://ifpni.org/supragenus.htm?formIndex=def&submitForm=Search&isExtended=1&"

    return(paste0(base_url, form))
  }

#' supragenus() is used to implement retrieval from IFPNI's supragenus list accroding to different filting conditions.
#'
#' @param name The name or partial name of supragenus taxon.
#' @param searchOnlyByFirstSymbols Whether to filter records whose Name starts with name parameter, default no.
#' @param order Default is "name". Decide to sort the results by name("name") or year("yearFrom").
#' @param orderDirection Default is "asc". Determines whether the results are in positive("asc") or reverse("desc") order.
#' @param author Select the record authorized by specific person.
#' @param originalSpelling Select the record whose original spelling is given string.
#' @param yearFrom Records published after the year of "yearFrom".
#' @param yearTo Records published before the year of "yearTo".
#' @param rank Select records in one or more specific classification levels.
#' Default is FALSE, not specifying any classification level means not filtering.
#' Either character type or numeric type can be used. The supported characters of classification levels are c('Regnum', 'Subregnum', 'Phylum', 'Subphylum', 'Superphylum', 'Class', 'Subclass', 'Superclass', 'Infraclass', 'Order', 'Superorder', 'Suborder', 'Infraorder', 'Family', 'Superfamily', 'Subfamily', 'Infrafamily', 'Tribe', 'Supertribe', 'Supersubtribe', 'Subtribe', 'Infratribe', 'Turma', 'Anteturma', 'Suprasubturma', 'Subturma', 'Infraturma', 'Unranked'), and the corresponding numbers are c(2, 3, 5, 6, 7, 9, 10, 11, 12, 66, 14, 15, 16, 18, 19, 20, 21, 23, 24, 25, 26, 27, 67, 29, 30, 31, 32, 59).
#'
#' @return A dataframe with detailed information of result.
#' @export
#'
#' @examples
#' # For example: find all taxon names start with "cla"
#' cla <- supragenus("cla", searchOnlyByFirstSymbols = "on")
#'
#' ✔ There are 2 pages found.
#' ✔ There are 18items found.
#' Collected 18 items from IFPNI. Costed 00:00:20.5
#'
#'
#' # Get all items of supragenus
#' # all_supragenus <- supragenus()
#' #
#' # ✔ There are 205 pages found.
#' # Collected 205 pages from IFPNI. Elapsed Time: 00:05:31.9
#' # ✔ There are 2049items found.
#' # Collected 2049 items from IFPNI. Costed 00:51:41
#' # see rifpni/data/all_supragenus_info.csv

supragenus <- function(name=NULL,
                       searchOnlyByFirstSymbols = "",
                       order = "name",
                       orderDirection = "asc",
                       author = "",
                       originalSpelling = "",
                       yearFrom = "",
                       yearTo = "",
                       rank = FALSE){
  cli_h1(paste("Executing",col_green(style_italic("supragenus")) ,"function" ))
  url <- basic_supragenus(name = name,searchOnlyByFirstSymbols = searchOnlyByFirstSymbols,
                          order=order, orderDirection = orderDirection,author=author,
                          originalSpelling = originalSpelling,yearFrom = yearFrom,
                          yearTo = yearTo, rank = rank)
  return <- basic_graber(url)
}
