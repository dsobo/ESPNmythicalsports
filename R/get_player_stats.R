#' @title GET Request for Player Statistics
#' @description  A function for accessing NFL player statistics via the Coinbase API.
#'
#' @param season The year of the season you wish to get stats for
#' @param week The week you wish to get stats for. If blank all availble statistics for the season chosen will be returned
#' @param league_id The ID of you ESPN fantasy league
#' @return A dataframe containing player projections and actual game statistics for the timeframe chosen.
#' @export


get_player_stats <- function(week, season, league_id){

  if (as.numeric(substr(Sys.Date(),1,4)) == season) {

    url <- paste0("https://fantasy.espn.com/apis/v3/games/ffl/seasons/2020/segments/0/leagues/",league_id,"?view=kona_player_info")

    player_stats <- httr::GET(url,
                              httr::add_headers(.headers =
                                                  c(
                                                    "X-Fantasy-Filter" = r"({"players":{"filterSlotIds":{"value":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,23,24]},"filterRanksForScoringPeriodIds":{"value":[2]},"sortPercOwned":{"sortAsc":false,"sortPriority":1},"sortDraftRanks":{"sortPriority":100,"sortAsc":true,"value":"STANDARD"},"filterRanksForRankTypes":{"value":["PPR"]},"filterRanksForSlotIds":{"value":[0,2,4,6,17,16]},"filterStatsForTopScoringPeriodIds":{"value":2,"additionalValue":["002020","102020","002019","1120202","022020"]}}})"
                                                  ))) %>%
      httr::content(as = 'text', type = 'JSON', encoding = 'UTF-8') %>%
      jsonlite::fromJSON(simplifyDataFrame = T) %>%
      purrr::pluck("players") %>%
      dplyr::bind_rows()



  } else {

    url <- paste0("https://fantasy.espn.com/apis/v3/games/ffl/leagueHistory/",league_id,"?seasonId=",season)

    player_stats <- httr::GET(url,
                             query = list("view" = "kona_player_info"),
                             httr::add_headers(.headers =
                                                 c(
                                                   "X-Fantasy-Filter" = r"({"players":{"filterSlotIds":{"value":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,23,24]},"filterRanksForScoringPeriodIds":{"value":[2]},"sortPercOwned":{"sortAsc":false,"sortPriority":1},"sortDraftRanks":{"sortPriority":100,"sortAsc":true,"value":"STANDARD"},"filterRanksForRankTypes":{"value":["PPR"]},"filterRanksForSlotIds":{"value":[0,2,4,6,17,16]},"filterStatsForTopScoringPeriodIds":{"value":2,"additionalValue":["002020","102020","002019","1120202","022020"]}}})"
                                                 )))%>%
      httr::content(as = 'text', type = 'JSON', encoding = 'UTF-8') %>%
      jsonlite::fromJSON(simplifyDataFrame = T) %>%
      purrr::pluck("players") %>%
      dplyr::bind_rows()



  }

  return(player_stats)

}

