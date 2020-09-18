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

    player_stats <- tibble( data = httr::GET(url,
                                             httr::add_headers(.headers =
                                                                 c(
                                                                   "X-Fantasy-Filter" = r"({"players":{"filterSlotIds":{"value":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,23,24]},"filterRanksForScoringPeriodIds":{"value":[2]},"sortPercOwned":{"sortAsc":false,"sortPriority":1},"sortDraftRanks":{"sortPriority":100,"sortAsc":true,"value":"STANDARD"},"filterRanksForRankTypes":{"value":["PPR"]},"filterRanksForSlotIds":{"value":[0,2,4,6,17,16]},"filterStatsForTopScoringPeriodIds":{"value":2,"additionalValue":["002020","102020","002019","1120202","022020"]}}})"
                                                                 ))) %>%
                              httr::content() %>%
                              purrr::pluck("players")) %>%
      tidyr::unnest_wider(col = data) %>%
      select(onTeamId, player) %>%
      tidyr::unnest_wider(col = player) %>%
      select(onTeamId, active, defaultPositionId, firstName, lastName, fullName, stats) %>%
      rename(stats_group = stats) %>%
      tidyr::unnest_longer(col = stats_group) %>%
      tidyr::unnest_wider(col = stats_group) %>%
      tidyr::hoist(.col = stats,
                   pass_comp = '1',
                   pass_att = '0',
                   pass_yds = '3',
                   pass_td = '4',
                   pass_2pt = '19',
                   pass_int = '20',
                   rush_att = '23',
                   rush_yds = '24',
                   rush_td = '25',
                   rush_2pt = '26',
                   rec = '53',
                   rec_tgt = '58',
                   rec_yds = '42',
                   rec_td = '43',
                   rec_2pt = '44',
                   fumbles = '72',
                   fg_0_39 = '80',
                   fg_miss_0_39 = '82',
                   fg_40_49 = '77',
                   fg_miss_40_49 = '79',
                   fg_50 = '74',
                   fg_miss_50 = '76',
                   xpt = '86',
                   xpt_miss = '88',
                   sacks = '99',
                   fum_forced = '106',
                   fum_recovered = '96',
                   def_int = '95',
                   safeties = '98',
                   blocks = '97',
                   block_td = '93',
                   kick_ret_td = '101',
                   punt_ret_td = '102',
                   fum_ret_td = '103',
                   int_ret_td = '104',
                   pts_allow_0 = '89',
                   pts_allow_1_6 = '90',
                   pts_allow_7_13 = '91',
                   pts_allow_14_17 = '92',
                   pts_allow_17_20 = '121',
                   pts_allow_21_27 = '122',
                   pts_allow_28_34 = '123',
                   pts_allow_35_45 = '124',
                   pts_allow_45_plus = '125') %>%
      mutate(statSourceId = if_else(statSourceId == 0, "Actual","Projected")) %>%
      mutate(defaultPositionId = case_when(
        defaultPositionId == 16 ~ "D/ST",
        defaultPositionId == 1 ~ "QB",
        defaultPositionId == 3 ~ "WR",
        defaultPositionId == 2 ~ "RB",
        defaultPositionId == 4 ~ "TE",
        defaultPositionId == 5 ~ "K",
        T ~ "Other"))



  } else {

    url <- paste0("https://fantasy.espn.com/apis/v3/games/ffl/leagueHistory/",league_id,"?seasonId=",season)

    player_stats <- tibble(data = httr::GET(url,
                             query = list("view" = "kona_player_info"),
                             httr::add_headers(.headers =
                                                 c(
                                                   "X-Fantasy-Filter" = r"({"players":{"filterSlotIds":{"value":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,23,24]},"filterRanksForScoringPeriodIds":{"value":[2]},"sortPercOwned":{"sortAsc":false,"sortPriority":1},"sortDraftRanks":{"sortPriority":100,"sortAsc":true,"value":"STANDARD"},"filterRanksForRankTypes":{"value":["PPR"]},"filterRanksForSlotIds":{"value":[0,2,4,6,17,16]},"filterStatsForTopScoringPeriodIds":{"value":2,"additionalValue":["002020","102020","002019","1120202","022020"]}}})"
                                                 )))%>%
      httr::content() %>%
      purrr::flatten() %>%
      purrr::pluck("players")) %>%
      tidyr::unnest_wider(col = data) %>%
      select(onTeamId, player) %>%
      tidyr::unnest_wider(col = player) %>%
      select(onTeamId, active, defaultPositionId, firstName, lastName, fullName, stats) %>%
      rename(stats_group = stats) %>%
      tidyr::unnest_longer(col = stats_group) %>%
      tidyr::unnest_wider(col = stats_group) %>%
      tidyr::hoist(.col = stats,
                   pass_comp = '1',
                   pass_att = '0',
                   pass_yds = '3',
                   pass_td = '4',
                   pass_2pt = '19',
                   pass_int = '20',
                   rush_att = '23',
                   rush_yds = '24',
                   rush_td = '25',
                   rush_2pt = '26',
                   rec = '53',
                   rec_tgt = '58',
                   rec_yds = '42',
                   rec_td = '43',
                   rec_2pt = '44',
                   fumbles = '72',
                   fg_0_39 = '80',
                   fg_miss_0_39 = '82',
                   fg_40_49 = '77',
                   fg_miss_40_49 = '79',
                   fg_50 = '74',
                   fg_miss_50 = '76',
                   xpt = '86',
                   xpt_miss = '88',
                   sacks = '99',
                   fum_forced = '106',
                   fum_recovered = '96',
                   def_int = '95',
                   safeties = '98',
                   blocks = '97',
                   block_td = '93',
                   kick_ret_td = '101',
                   punt_ret_td = '102',
                   fum_ret_td = '103',
                   int_ret_td = '104',
                   pts_allow_0 = '89',
                   pts_allow_1_6 = '90',
                   pts_allow_7_13 = '91',
                   pts_allow_14_17 = '92',
                   pts_allow_17_20 = '121',
                   pts_allow_21_27 = '122',
                   pts_allow_28_34 = '123',
                   pts_allow_35_45 = '124',
                   pts_allow_45_plus = '125') %>%
      mutate(statSourceId = if_else(statSourceId == 0, "Actual","Projected")) %>%
      mutate(defaultPositionId = case_when(
        defaultPositionId == 16 ~ "D/ST",
        defaultPositionId == 1 ~ "QB",
        defaultPositionId == 3 ~ "WR",
        defaultPositionId == 2 ~ "RB",
        defaultPositionId == 4 ~ "TE",
        defaultPositionId == 5 ~ "K",
        T ~ "Other"))



  }

  return(player_stats)

}
