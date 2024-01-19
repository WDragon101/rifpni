#' basic_infraspecies is used to merge initial URL for infraspecies method.
#'
#' @param name The name or partial name of infraspecies taxon.
#' @param searchOnlyByFirstSymbols Whether to filter records whose Name starts with name parameter, default no.
#' @param order Default is "name". Decide to sort the results by name("name") or year("yearFrom").
#' @param orderDirection Default is "asc". Determines whether the results are in positive("asc") or reverse("desc") order.
#' @param author Select the record authorized by specific person.
#' @param originalSpelling Select the record whose original spelling is given string.
#' @param yearFrom Records published after the year of "yearFrom".
#' @param yearTo Records published before the year of "yearTo".
#' @param rank Select records in one or more specific classification levels.
#' Default is FALSE, not specifying any classification level means not filtering.
#' Either character type or numeric type can be used. The supported characters of classification levels are c('Subspecies', 'Supervarietas', 'Varietas', 'Subvarietas', 'Forma', 'Subforma', 'Infraspecies', 'Unranked'), and the corresponding numbers are c(35, 36, 37, 38, 39, 40, 41, 57).
#' @param paleoregion Select records distributed in one or more paleoregions.
#' Default is FALSE, not specifying any paleoregion means not filtering.
#' Either character type or numeric type can be used. The supported characters of paleoregions are c('Africa', 'Africa (East)', 'Africa (Equatorial)', 'Africa (North)', 'Africa (South)', 'Africa (West)', 'Altaida (Altaides)', 'Altaj', 'America', 'America (Caribbean)', 'America (North)', 'America (North - Greenland)', 'America (South)', 'Anatolia', 'Angarida', 'Angarida (Mongolia)', 'Antarctica', 'Arctic', 'Australia', 'Australia (New Zealand)', 'Avalonia', 'Baltica', 'Cathaysia', 'Cathaysia (Kitakamiland)', 'Cathaysia (North)', 'Cathaysia (Sino-Korea)', 'Cathaysia (South)', 'Caucasus', 'China (North)', 'China (South)', 'Chingiz', 'Columbia (Amazonia)', 'Columbia (Australia)', 'Columbia (Baltica)', 'Columbia (East Antarctica)', 'Columbia (Greenland)', 'Columbia (Indostan)', 'Columbia (Kalahari)', 'Columbia (Laurentia)', 'Columbia (North Australia)', 'Columbia (North China)', 'Columbia (South Australia)', 'Columbia (South China)', 'Columbia (West Africa)', 'Columbia (West Australia)', 'Congo-São Francisco (Congo)', 'Eurasia', 'Eurasia (Anatolia)', 'Eurasia (Arabian peninsula)', 'Eurasia (Asia)', 'Eurasia (Asia Minor)', 'Eurasia (Caucasus)', 'Eurasia (Central Asia)', 'Eurasia (Central Asia and Caucasus)', 'Eurasia (Central Asia and Urals)', 'Eurasia (China)', 'Eurasia (Europe)', 'Eurasia (Europe & Urals)', 'Eurasia (Europe and Central Asia)', 'Eurasia (Europe and W Siberia)', 'Eurasia (Far East)', 'Eurasia (Far East & Central Asia)', 'Eurasia (Far East & China)', 'Eurasia (Far East and Japanese Archipelago)', 'Eurasia (Greenland)', 'Eurasia (Himalayas)', 'Eurasia (Indochina)', 'Eurasia (Indostan)', 'Eurasia (Japanese Archipelago)', 'Eurasia (Japanese Archipelago and Sakhalin)', 'Eurasia (Java Island)', 'Eurasia (Maritime Southeast Asia)', 'Eurasia (Mesopotamian Plain)', 'Eurasia (Middle East)', 'Eurasia (Novaja Zemlja Archipelago)', 'Eurasia (S Asia)', 'Eurasia (S Urals)', 'Eurasia (SE Asia)', 'Eurasia (Siberia)', 'Eurasia (Siberia and Central Asia)', 'Eurasia (Siberia and Far East)', 'Eurasia (South China)', 'Eurasia (South East)', 'Eurasia (Tarim Basin)', 'Eurasia (Tibet)', 'Eurasia (Urals)', 'Eurasia (W Asia)', 'Eurasia (W Asia & Caucasus)', 'Eurasia (W Siberia)', 'Eurasia (W Siberia & Central Asia)', 'Europe', 'Europe (Cis-Caspian)', 'Gondwana (Africa)', 'Gondwana (Antarctica)', 'Gondwana (Arabia)', 'Gondwana (Armorica)', 'Gondwana (Australia)', 'Gondwana (Indostan)', 'Gondwana (N Africa)', 'Gondwana (N Africa [Tethys palaeocean])', 'Gondwana (New Caledonia)', 'Gondwana (New Zealand)', 'Gondwana (Perunica)', 'Gondwana (S America)', 'Gondwana (Sardinia)', 'Gondwana (Saxo-Thuringia)', 'Gondwana (South Africa)', 'Gondwana (South America)', 'Kazakhstania', 'Kazakhstania (Xinjiang)', 'Kenorland (Baltica)', 'Kenorland (Kalaharia)', 'Kenorland (Laurentia)', 'Kenorland (Western Australia)', 'Kitakamiland', 'Laurentia', 'Laurussia', 'Laurussia (Anatolia)', 'Laurussia (Armorica)', 'Laurussia (Avalonia)', 'Laurussia (Avalonia & Baltica)', 'Laurussia (Baltica)', 'Laurussia (Cantabria)', 'Laurussia (Iberica)', 'Laurussia (Laurentia)', 'Laurussia (Moesia)', 'Laurussia (Moldanubia)', 'Laurussia (Perunica)', 'Laurussia (Rhenano-Hercynia)', 'Laurussia (Saxo-Thuringia)', 'Moldanubia', 'Mongolia (Western)', 'not specified', 'Pacific', 'Pamir-Alaj', 'Pannotia (Antarctica)', 'Pannotia (Indostan)', 'Pannotia (Siberia)', 'Perunica', 'Proangarida', 'Rodinia', 'Rodinia (Amazonia)', 'Rodinia (Australia)', 'Rodinia (Baltica)', 'Rodinia (Indostan)', 'Rodinia (Laurentia)', 'Rodinia (North China)', 'Rodinia (Rio Plato)', 'Rodinia (Siberia)', 'Rodinia (South China)', 'Siberia', 'Sibumasu', 'Superior Craton (Superior Craton)', 'Tibet', 'Tien Shan', 'Tien Shan (North)', 'Tien Shan (South)', 'Tuva', 'unknown', 'Vaalbara (Kaapvaal)', 'Vaalbara (Pilbara)', 'Xinjiang'), and the corresponding numbers are c(1, 2, 3, 4, 5, 6, 7, 8, 122, 9, 10, 104, 11, 12, 13, 136, 14, 15, 16, 17, 18, 19, 20, 21, 22, 105, 23, 24, 26, 25, 27, 158, 147, 156, 159, 157, 137, 154, 151, 164, 155, 160, 153, 152, 161, 172, 28, 29, 30, 32, 31, 33, 36, 34, 35, 37, 41, 38, 39, 40, 45, 42, 43, 44, 46, 47, 48, 49, 51, 50, 52, 53, 54, 55, 56, 123, 57, 58, 61, 59, 60, 62, 63, 134, 64, 65, 66, 67, 69, 68, 71, 72, 73, 74, 126, 75, 76, 77, 78, 79, 131, 132, 80, 81, 113, 82, 124, 83, 84, 85, 168, 169, 167, 170, 86, 87, 88, 89, 90, 93, 92, 94, 95, 96, 97, 98, 125, 90, 100, 101, 102, 103, 106, 107, 108, 174, 175, 150, 109, 110, 111, 141, 146, 148, 138, 140, 112, 142, 149, 139, 114, 115, 171, 116, 117, 118, 133, 119, 120, 165, 166, 121).
#'
#'
#' @return URL
#' @export
#'
#'
#' @examples
#' basic_infraspecies_url <- basic_infraspecies("bil")
#'
#' @import rvest
#' @import magrittr
#' @import checkmate
#' @import dplyr


basic_infraspecies <-
  function(name=NULL,
           searchOnlyByFirstSymbols = "",
           order = "name",
           orderDirection = "asc",
           author = "",
           originalSpelling = "",
           yearFrom = "",
           yearTo = "",
           rank = FALSE,
           paleoregion = FALSE) {
    checkChoice(searchOnlyByFirstSymbols, c("","on"))
    checkChoice(order, c("name","yearFrom"))
    checkChoice(orderDirection,c("desc","asc"))

    infraspecies_rank <- c(35, 36, 37, 38, 39, 40, 41, 57)
    names(infraspecies_rank) <- c('Subspecies', 'Supervarietas', 'Varietas', 'Subvarietas', 'Forma', 'Subforma', 'Infraspecies', 'Unranked')
    if (all(rank != FALSE)){
      if (all(rank %in% names(infraspecies_rank))){
        rank <- paste0("rankID[]=", infraspecies_rank[rank], collapse = "&")
      }
      else if (all(rank %in% infraspecies_rank)){
        rank <- paste0("rankID[]=", rank, collapse = "&")
      }}else{rank=""}

    paleo_rank <- c(1, 2, 3, 4, 5, 6, 7, 8, 122, 9, 10, 104, 11, 12, 13, 136, 14, 15, 16, 17, 18, 19, 20, 21, 22, 105, 23, 24, 26, 25, 27, 158, 147, 156, 159, 157, 137, 154, 151, 164, 155, 160, 153, 152, 161, 172, 28, 29, 30, 32, 31, 33, 36, 34, 35, 37, 41, 38, 39, 40, 45, 42, 43, 44, 46, 47, 48, 49, 51, 50, 52, 53, 54, 55, 56, 123, 57, 58, 61, 59, 60, 62, 63, 134, 64, 65, 66, 67, 69, 68, 71, 72, 73, 74, 126, 75, 76, 77, 78, 79, 131, 132, 80, 81, 113, 82, 124, 83, 84, 85, 168, 169, 167, 170, 86, 87, 88, 89, 90, 93, 92, 94, 95, 96, 97, 98, 125, 90, 100, 101, 102, 103, 106, 107, 108, 174, 175, 150, 109, 110, 111, 141, 146, 148, 138, 140, 112, 142, 149, 139, 114, 115, 171, 116, 117, 118, 133, 119, 120, 165, 166, 121)
    names(paleo_rank) <- c('Africa', 'Africa (East)', 'Africa (Equatorial)', 'Africa (North)', 'Africa (South)', 'Africa (West)', 'Altaida (Altaides)', 'Altaj', 'America', 'America (Caribbean)', 'America (North)', 'America (North - Greenland)', 'America (South)', 'Anatolia', 'Angarida', 'Angarida (Mongolia)', 'Antarctica', 'Arctic', 'Australia', 'Australia (New Zealand)', 'Avalonia', 'Baltica', 'Cathaysia', 'Cathaysia (Kitakamiland)', 'Cathaysia (North)', 'Cathaysia (Sino-Korea)', 'Cathaysia (South)', 'Caucasus', 'China (North)', 'China (South)', 'Chingiz', 'Columbia (Amazonia)', 'Columbia (Australia)', 'Columbia (Baltica)', 'Columbia (East Antarctica)', 'Columbia (Greenland)', 'Columbia (Indostan)', 'Columbia (Kalahari)', 'Columbia (Laurentia)', 'Columbia (North Australia)', 'Columbia (North China)', 'Columbia (South Australia)', 'Columbia (South China)', 'Columbia (West Africa)', 'Columbia (West Australia)', 'Congo-São Francisco (Congo)', 'Eurasia', 'Eurasia (Anatolia)', 'Eurasia (Arabian peninsula)', 'Eurasia (Asia)', 'Eurasia (Asia Minor)', 'Eurasia (Caucasus)', 'Eurasia (Central Asia)', 'Eurasia (Central Asia and Caucasus)', 'Eurasia (Central Asia and Urals)', 'Eurasia (China)', 'Eurasia (Europe)', 'Eurasia (Europe & Urals)', 'Eurasia (Europe and Central Asia)', 'Eurasia (Europe and W Siberia)', 'Eurasia (Far East)', 'Eurasia (Far East & Central Asia)', 'Eurasia (Far East & China)', 'Eurasia (Far East and Japanese Archipelago)', 'Eurasia (Greenland)', 'Eurasia (Himalayas)', 'Eurasia (Indochina)', 'Eurasia (Indostan)', 'Eurasia (Japanese Archipelago)', 'Eurasia (Japanese Archipelago and Sakhalin)', 'Eurasia (Java Island)', 'Eurasia (Maritime Southeast Asia)', 'Eurasia (Mesopotamian Plain)', 'Eurasia (Middle East)', 'Eurasia (Novaja Zemlja Archipelago)', 'Eurasia (S Asia)', 'Eurasia (S Urals)', 'Eurasia (SE Asia)', 'Eurasia (Siberia)', 'Eurasia (Siberia and Central Asia)', 'Eurasia (Siberia and Far East)', 'Eurasia (South China)', 'Eurasia (South East)', 'Eurasia (Tarim Basin)', 'Eurasia (Tibet)', 'Eurasia (Urals)', 'Eurasia (W Asia)', 'Eurasia (W Asia & Caucasus)', 'Eurasia (W Siberia)', 'Eurasia (W Siberia & Central Asia)', 'Europe', 'Europe (Cis-Caspian)', 'Gondwana (Africa)', 'Gondwana (Antarctica)', 'Gondwana (Arabia)', 'Gondwana (Armorica)', 'Gondwana (Australia)', 'Gondwana (Indostan)', 'Gondwana (N Africa)', 'Gondwana (N Africa [Tethys palaeocean])', 'Gondwana (New Caledonia)', 'Gondwana (New Zealand)', 'Gondwana (Perunica)', 'Gondwana (S America)', 'Gondwana (Sardinia)', 'Gondwana (Saxo-Thuringia)', 'Gondwana (South Africa)', 'Gondwana (South America)', 'Kazakhstania', 'Kazakhstania (Xinjiang)', 'Kenorland (Baltica)', 'Kenorland (Kalaharia)', 'Kenorland (Laurentia)', 'Kenorland (Western Australia)', 'Kitakamiland', 'Laurentia', 'Laurussia', 'Laurussia (Anatolia)', 'Laurussia (Armorica)', 'Laurussia (Avalonia)', 'Laurussia (Avalonia & Baltica)', 'Laurussia (Baltica)', 'Laurussia (Cantabria)', 'Laurussia (Iberica)', 'Laurussia (Laurentia)', 'Laurussia (Moesia)', 'Laurussia (Moldanubia)', 'Laurussia (Perunica)', 'Laurussia (Rhenano-Hercynia)', 'Laurussia (Saxo-Thuringia)', 'Moldanubia', 'Mongolia (Western)', 'not specified', 'Pacific', 'Pamir-Alaj', 'Pannotia (Antarctica)', 'Pannotia (Indostan)', 'Pannotia (Siberia)', 'Perunica', 'Proangarida', 'Rodinia', 'Rodinia (Amazonia)', 'Rodinia (Australia)', 'Rodinia (Baltica)', 'Rodinia (Indostan)', 'Rodinia (Laurentia)', 'Rodinia (North China)', 'Rodinia (Rio Plato)', 'Rodinia (Siberia)', 'Rodinia (South China)', 'Siberia', 'Sibumasu', 'Superior Craton (Superior Craton)', 'Tibet', 'Tien Shan', 'Tien Shan (North)', 'Tien Shan (South)', 'Tuva', 'unknown', 'Vaalbara (Kaapvaal)', 'Vaalbara (Pilbara)', 'Xinjiang')
    if (all(paleoregion != FALSE)){
      if (all(paleoregion %in% names(paleo_rank))){
        paleoregion <- paste0("paleoID=", paleo_rank[paleoregion], collapse = "&")
      }
      else if (all(paleoregion %in% paleo_rank)){
        rank <- paste0("paleoID=", paleoregion, collapse = "&")
      }}else{paleoregion=""}

    form <- paste(paste0("name=",name), paste0("searchOnlyByFirstSymbols=",searchOnlyByFirstSymbols),
                  paste0("order=",order),paste0("orderDirection=",orderDirection),
                  paste0("author=",author),paste0("originalSpelling=",originalSpelling),
                  paste0("yearFrom=",yearFrom),paste0("yearTo=",yearTo), rank, paleoregion,
                  sep = "&")

    base_url <-
      "http://ifpni.org/infraspecies.htm?formIndex=def&submitForm=Search&isExtended=1&"

    return(paste0(base_url, form))
  }

#' infraspecies() is used to implement retrieval from IFPNI's infraspecies list accroding to different filting conditions.
#'
#' @param name The name or partial name of infraspecies taxon.
#' @param searchOnlyByFirstSymbols Whether to filter records whose Name starts with name parameter, default no.
#' @param order Default is "name". Decide to sort the results by name("name") or year("yearFrom").
#' @param orderDirection Default is "asc". Determines whether the results are in positive("asc") or reverse("desc") order.
#' @param author Select the record authorized by specific person.
#' @param originalSpelling Select the record whose original spelling is given string.
#' @param yearFrom Records published after the year of "yearFrom".
#' @param yearTo Records published before the year of "yearTo".
#' @param rank Select records in one or more specific classification levels.
#' Default is FALSE, not specifying any classification level means not filtering.
#' Either character type or numeric type can be used. The supported characters of classification levels are c('Subspecies', 'Supervarietas', 'Varietas', 'Subvarietas', 'Forma', 'Subforma', 'Infraspecies', 'Unranked'), and the corresponding numbers are c(35, 36, 37, 38, 39, 40, 41, 57).
#' @param paleoregion Select records distributed in one or more paleoregions.
#' Default is FALSE, not specifying any paleoregion means not filtering.
#' Either character type or numeric type can be used. The supported characters of paleoregions are c('Africa', 'Africa (East)', 'Africa (Equatorial)', 'Africa (North)', 'Africa (South)', 'Africa (West)', 'Altaida (Altaides)', 'Altaj', 'America', 'America (Caribbean)', 'America (North)', 'America (North - Greenland)', 'America (South)', 'Anatolia', 'Angarida', 'Angarida (Mongolia)', 'Antarctica', 'Arctic', 'Australia', 'Australia (New Zealand)', 'Avalonia', 'Baltica', 'Cathaysia', 'Cathaysia (Kitakamiland)', 'Cathaysia (North)', 'Cathaysia (Sino-Korea)', 'Cathaysia (South)', 'Caucasus', 'China (North)', 'China (South)', 'Chingiz', 'Columbia (Amazonia)', 'Columbia (Australia)', 'Columbia (Baltica)', 'Columbia (East Antarctica)', 'Columbia (Greenland)', 'Columbia (Indostan)', 'Columbia (Kalahari)', 'Columbia (Laurentia)', 'Columbia (North Australia)', 'Columbia (North China)', 'Columbia (South Australia)', 'Columbia (South China)', 'Columbia (West Africa)', 'Columbia (West Australia)', 'Congo-São Francisco (Congo)', 'Eurasia', 'Eurasia (Anatolia)', 'Eurasia (Arabian peninsula)', 'Eurasia (Asia)', 'Eurasia (Asia Minor)', 'Eurasia (Caucasus)', 'Eurasia (Central Asia)', 'Eurasia (Central Asia and Caucasus)', 'Eurasia (Central Asia and Urals)', 'Eurasia (China)', 'Eurasia (Europe)', 'Eurasia (Europe & Urals)', 'Eurasia (Europe and Central Asia)', 'Eurasia (Europe and W Siberia)', 'Eurasia (Far East)', 'Eurasia (Far East & Central Asia)', 'Eurasia (Far East & China)', 'Eurasia (Far East and Japanese Archipelago)', 'Eurasia (Greenland)', 'Eurasia (Himalayas)', 'Eurasia (Indochina)', 'Eurasia (Indostan)', 'Eurasia (Japanese Archipelago)', 'Eurasia (Japanese Archipelago and Sakhalin)', 'Eurasia (Java Island)', 'Eurasia (Maritime Southeast Asia)', 'Eurasia (Mesopotamian Plain)', 'Eurasia (Middle East)', 'Eurasia (Novaja Zemlja Archipelago)', 'Eurasia (S Asia)', 'Eurasia (S Urals)', 'Eurasia (SE Asia)', 'Eurasia (Siberia)', 'Eurasia (Siberia and Central Asia)', 'Eurasia (Siberia and Far East)', 'Eurasia (South China)', 'Eurasia (South East)', 'Eurasia (Tarim Basin)', 'Eurasia (Tibet)', 'Eurasia (Urals)', 'Eurasia (W Asia)', 'Eurasia (W Asia & Caucasus)', 'Eurasia (W Siberia)', 'Eurasia (W Siberia & Central Asia)', 'Europe', 'Europe (Cis-Caspian)', 'Gondwana (Africa)', 'Gondwana (Antarctica)', 'Gondwana (Arabia)', 'Gondwana (Armorica)', 'Gondwana (Australia)', 'Gondwana (Indostan)', 'Gondwana (N Africa)', 'Gondwana (N Africa [Tethys palaeocean])', 'Gondwana (New Caledonia)', 'Gondwana (New Zealand)', 'Gondwana (Perunica)', 'Gondwana (S America)', 'Gondwana (Sardinia)', 'Gondwana (Saxo-Thuringia)', 'Gondwana (South Africa)', 'Gondwana (South America)', 'Kazakhstania', 'Kazakhstania (Xinjiang)', 'Kenorland (Baltica)', 'Kenorland (Kalaharia)', 'Kenorland (Laurentia)', 'Kenorland (Western Australia)', 'Kitakamiland', 'Laurentia', 'Laurussia', 'Laurussia (Anatolia)', 'Laurussia (Armorica)', 'Laurussia (Avalonia)', 'Laurussia (Avalonia & Baltica)', 'Laurussia (Baltica)', 'Laurussia (Cantabria)', 'Laurussia (Iberica)', 'Laurussia (Laurentia)', 'Laurussia (Moesia)', 'Laurussia (Moldanubia)', 'Laurussia (Perunica)', 'Laurussia (Rhenano-Hercynia)', 'Laurussia (Saxo-Thuringia)', 'Moldanubia', 'Mongolia (Western)', 'not specified', 'Pacific', 'Pamir-Alaj', 'Pannotia (Antarctica)', 'Pannotia (Indostan)', 'Pannotia (Siberia)', 'Perunica', 'Proangarida', 'Rodinia', 'Rodinia (Amazonia)', 'Rodinia (Australia)', 'Rodinia (Baltica)', 'Rodinia (Indostan)', 'Rodinia (Laurentia)', 'Rodinia (North China)', 'Rodinia (Rio Plato)', 'Rodinia (Siberia)', 'Rodinia (South China)', 'Siberia', 'Sibumasu', 'Superior Craton (Superior Craton)', 'Tibet', 'Tien Shan', 'Tien Shan (North)', 'Tien Shan (South)', 'Tuva', 'unknown', 'Vaalbara (Kaapvaal)', 'Vaalbara (Pilbara)', 'Xinjiang'), and the corresponding numbers are c(1, 2, 3, 4, 5, 6, 7, 8, 122, 9, 10, 104, 11, 12, 13, 136, 14, 15, 16, 17, 18, 19, 20, 21, 22, 105, 23, 24, 26, 25, 27, 158, 147, 156, 159, 157, 137, 154, 151, 164, 155, 160, 153, 152, 161, 172, 28, 29, 30, 32, 31, 33, 36, 34, 35, 37, 41, 38, 39, 40, 45, 42, 43, 44, 46, 47, 48, 49, 51, 50, 52, 53, 54, 55, 56, 123, 57, 58, 61, 59, 60, 62, 63, 134, 64, 65, 66, 67, 69, 68, 71, 72, 73, 74, 126, 75, 76, 77, 78, 79, 131, 132, 80, 81, 113, 82, 124, 83, 84, 85, 168, 169, 167, 170, 86, 87, 88, 89, 90, 93, 92, 94, 95, 96, 97, 98, 125, 90, 100, 101, 102, 103, 106, 107, 108, 174, 175, 150, 109, 110, 111, 141, 146, 148, 138, 140, 112, 142, 149, 139, 114, 115, 171, 116, 117, 118, 133, 119, 120, 165, 166, 121).
#'
#'
#' @return A dataframe with detailed information of result.
#' @export
#'
#' @examples
#' # let's see how many taxa names contain "comm" in the infraspecies list on IFPNI.
#' comm_infraspecies <- infraspecies("comm")
#' nrow(comm_infraspecies)
#' # [1] 8
#' # What are they?
#' comm_infraspecies$Name
#' # [1] "Arthropitys communis septata"       "Calamites communis approximatus"
#' # [3] "Calamites communis canniformis"     "Calamites communis decoratus"
#' # [5] "Caryae-pollenites simplex communis" "Caryae-pollenites simplex communis"
#' # [7] "Pyrus communis fossilis"            "Viburnum lesquereuxii commune"
#'
#' # see data/data/comm_infraspecies.rda for details.
#'
#' # Let's try the most daring operation.
#' # If you do not pass a value to any argument, you get a dataframe that stores the details of all the taxa in the species list on IFPNI.
#' # all_infrapseices_20231109 <- infraspecies()
#' # This operation may take more than one hour. See data/all_infrapseices_20231109.rda for the result.

infraspecies <- function(name=NULL,
                    searchOnlyByFirstSymbols = "",
                    order = "name",
                    orderDirection = "asc",
                    author = "",
                    originalSpelling = "",
                    yearFrom = "",
                    yearTo = "",
                    rank = FALSE,
                    paleoregion=FALSE){
  url <- basic_infraspecies(name = name,searchOnlyByFirstSymbols = searchOnlyByFirstSymbols,
                       order=order, orderDirection = orderDirection,author=author,
                       originalSpelling = originalSpelling,yearFrom = yearFrom,
                       yearTo = yearTo, rank = rank, paleoregion = paleoregion)
  return <- basic_graber(url)
}
