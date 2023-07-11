#' Historical Strike Data from Trials in CT Federal District Court for Fiscal Years 2013-2017
#'
#' A de-identified data set on use of peremptory strikes on prospective jurorss
#'
#' @format ## `dat0`
#' A data frame with 979 rows and 15 columns:
#' \describe{
#'   \item{ID}{Trial Identification Number}
#'   \item{R_No}{Juror ID for trial}
#'   \item{Pool_Seq}{Juror ID for jury pool}
#'   \item{race}{self-reported juror race}
#'   \item{sex}{self-reported juror sex: M = Male, F = Female}
#'   \item{Disp}{seated on jury or struck: J* = seated, PP = prosecutor strike, PD = defense strike}
#'   \item{strike_num}{nth strike used by party - PP or PD}
#'   \item{strike_seq}{nth strike used by any party}
#'   \item{alternate}{whether seated as alternate}
#'   \item{courthouse}{location of courthouse where jury selection occurred}
#'   \item{juror}{indicator of whether seated or struck}
#'   \item{raceb}{indicator of whether racial minority or not}
#'   \item{P_atty_l}{aliases for prosecuting attorneys}
#'   \item{D_atty_l}{aliases for defense attorneys}
#' }
#' @source Jury Pool Data and Strike Sheets, U.S. District Court, District of Connecticut 
"dat0"