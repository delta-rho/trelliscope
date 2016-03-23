#' Lahman's Batting Data for 2014
#'
#' \itemize{
#'   \item playerID  Player ID code
#'   \item yearID    Year
#'   \item stint     player's stint (order of appearances within a season)
#'   \item teamID    Team; a factor
#'   \item lgID      League; a factor with levels AA AL FL NL PL UA
#'   \item G         Games: number of games in which a player played
#'   \item AB        At Bats
#'   \item R         Runs
#'   \item H         Hits: times reached base because of a batted, fair ball without error by the defense
#'   \item X2B       Doubles: hits on which the batter reached second base safely
#'   \item X3B       Triples: hits on which the batter reached third base safely
#'   \item HR        Homeruns
#'   \item RBI       Runs Batted In
#'   \item SB        Stolen Bases
#'   \item CS        Caught Stealing
#'   \item BB        Base on Balls
#'   \item SO        Strikeouts
#'   \item IBB       Intentional walks
#'   \item HBP       Hit by pitch
#'   \item SH        Sacrifice hits
#'   \item SF        Sacrifice flies
#'   \item GIDP      Grounded into double plays
#' }
#'
#' @docType data
#' @description
#' 2014 Batting statistics from Sean Lahman's baseball statistics database (released under CC BY-SA 3.0 license).
#' @source Lahman, S. (2015) Lahman's Baseball Database, 1871-2014, 2015 version, \url{http://baseball1.com/statistics/}.
#' @keywords datasets
#' @name batting
#' @usage data(batting)
#' @example man-roxygen/ex-splod.R
#' @format A data frame with 1435 rows and 22 variables
NULL


