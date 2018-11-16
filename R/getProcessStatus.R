#' @name getProcessStatus
#'
#' @title Return process status information
#'
#' @param psPath Full path of the \code{ps} command.
#'
#' @return A dataframe of information from the \code{ps} command.
#'
#' @description Returns a dataframe of information from the \code{ps} command.
#'
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # CPU hogs
#' cpu_hogs <- getProcessStatus() %>%
#'   select(pcpu,user) %>%
#'   group_by(user) %>%
#'   summarise(cpu_usage = sum(pcpu)) %>%
#'   arrange(desc(cpu_usage))
#'
#' head(cpu_hogs)
#'
#' # Long processes
#' long_running <- ps_DF %>%
#'   select(user, esecs, pid, command) %>%
#'   filter(user != "root") %>%
#'   arrange(desc(esecs))
#'
#' head(long_running)
#' }

getProcessStatus <- function(psPath = "/bin/ps") {

  # TODO: try blocks and error handling

  # Debugging ------------------------------------------------------------------

  if (FALSE) {

    # setup of incoming parameters

  }

  # Gather stats ---------------------------------------------------------------

  # NOTE:  ps adjusts column spacing depending on the contents. Thus, we need
  # NOTE:  to ask for results with variable string length (e.g. user, command)
  # NOTE:  separately to make things easier to parse.

  # ----- Process IDs, CPU and memory -----
  #
  # $ ps -axo pid,ppid,pgid,pcpu,pmem | head -3
  #   PID  PPID  PGID  %CPU %MEM
  #     1     0     1   1.9  0.1
  #    40     1    40   0.0  0.0
  #<123456789012345678901234567890>
  #<         111111111122222222223>

  ps_raw <- sys::exec_internal(psPath, "-axo pid,ppid,pgid,pcpu,pmem")
  ps_char <- base::rawToChar(ps_raw$stdout)
  col_positions <- readr::fwf_positions(
    start = c(1,7,13,19,25),
    end = c(5,11,17,23,28),
    col_names = c("pid","ppid","pgid","pcpu","pmem")
  )
  df1 <- readr::read_fwf(
    ps_char,
    col_positions,
    col_types = c('cccdd'),
    skip = 1
  )

  # ----- User information -----
  #
  # $ ps -axo pid,uid,user | head -3
  #   PID   UID USER
  #     1     0 root
  #    40     0 root
  #<123456789012345678901234567890>
  #<         111111111122222222223>

  ps_raw <- sys::exec_internal(psPath, "-axo pid,uid,user")
  ps_char <- base::rawToChar(ps_raw$stdout)
  col_positions <- readr::fwf_positions(
    start = c(1,7,13),
    end = c(5,11,NA),
    col_names = c("pid","uid","user")
  )
  df2 <- readr::read_fwf(
    ps_char,
    col_positions,
    col_types = c('ccc'),
    skip = 1
  )

  # ----- Command -----
  #
  # $ ps -axo pid,command | head -3
  #   PID COMMAND
  #     1 /sbin/launchd
  #    40 /usr/sbin/syslogd
  #<123456789012345678901234567890>
  #<         111111111122222222223>

  ps_raw <- sys::exec_internal(psPath, "-axo pid,command")
  ps_char <- base::rawToChar(ps_raw$stdout)
  col_positions <- readr::fwf_positions(
    start = c(1,7),
    end = c(5,NA),
    col_names = c("pid","command")
  )
  df3 <- readr::read_fwf(
    ps_char,
    col_positions,
    col_types = c('cc'),
    skip = 1
  )

  # ----- CPU time -----
  #
  # NOTE:  By itself because it has optional [dd-] days
  #
  # $ ps -axo pid,cputime | head -3
  #   PID     TIME
  #     1 00:02:38
  #     2 00:00:00
  #<123456789012345678901234567890>
  #<         111111111122222222223>

  ps_raw <- sys::exec_internal(psPath, "-axo pid,cputime")
  ps_char <- base::rawToChar(ps_raw$stdout)
  col_positions <- readr::fwf_positions(
    start = c(1,7),
    end = c(5,NA),
    col_names = c("pid","cputime")
  )
  df4 <- readr::read_fwf(
    ps_char,
    col_positions,
    col_types = c('cc'),
    skip = 1
  )

  # Convert into seconds
  # Get the day part by matching any "..-*" and grouping the first two characters
  dayPart <- stringr::str_match(df4$cputime, '(..)-')[,2]
  # Get the time part by stripping off any day part
  hmsPart <- stringr::str_replace(df4$cputime, "^.*-", "")
  days <- as.integer(dayPart)
  daySecs <- 60*60*24 * replace(days, is.na(days), 0)
  # Use lubridate::hms()
  # TODO:  Handle incomplete hour format
  suppressWarnings({
    hmsSecs <- as.integer(lubridate::as.duration(lubridate::hms(hmsPart)))
  })
  # NOTE:  On a Mac, hmsPart may or may not have hours which will cause
  # NOTE:  lubridate::hms() to return NA. Just assign a small 100 secs here.
  hmsSecs <- replace(hmsSecs, is.na(hmsSecs), 100)
  df4$cpusecs <- daySecs + hmsSecs
  # Remove cputime
  ###df4$cputime <- NULL # keep it around while debugging

  # ----- Elapsed time -----
  #
  # NOTE:  By itself because it has optional [dd-] days
  #
  # $ ps -axo pid,etime | head -3
  #   PID     TIME
  #     1 12-00:02:38
  #     2 12-00:00:00
  #<123456789012345678901234567890>
  #<         111111111122222222223>

  ps_raw <- sys::exec_internal(psPath, "-axo pid,etime")
  ps_char <- base::rawToChar(ps_raw$stdout)
  col_positions <- readr::fwf_positions(
    start = c(1,7),
    end = c(5,NA),
    col_names = c("pid","etime")
  )
  df5 <- readr::read_fwf(
    ps_char,
    col_positions,
    col_types = c('cc'),
    skip = 1
  )

  # Convert into seconds
  # Get the day part by matching any "..-*" and grouping the first two characters
  dayPart <- stringr::str_match(df5$etime, '(..)-')[,2]
  # Get the time part by stripping off any day part
  hmsPart <- stringr::str_replace(df5$etime, "^.*-", "")
  days <- as.integer(dayPart)
  daySecs <- 60*60*24 * replace(days, is.na(days), 0)
  # TODO:  Handle incomplete hour format
  suppressWarnings({
    hmsSecs <- as.integer(lubridate::as.duration(lubridate::hms(hmsPart)))
  })
  # NOTE:  On a Mac, hmsPart may or may not have hours which will cause
  # NOTE:  lubridate::hms() to return NA. Just assign a small 100 secs here.
  hmsSecs <- replace(hmsSecs, is.na(hmsSecs), 100)
  df5$esecs <- daySecs + hmsSecs
  # Remove etime
  ###df5$etime <- NULL # keep it around while debugging


  # Return ---------------------------------------------------------------------

  ps_DF <- left_join(df1, df2, by = 'pid')
  ps_DF <- left_join(ps_DF, df3, by = 'pid')
  ps_DF <- left_join(ps_DF, df4, by = 'pid')
  ps_DF <- left_join(ps_DF, df5, by = 'pid')

  return(ps_DF)

}

