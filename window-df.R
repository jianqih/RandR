library(data.table)
dat <- data.table(dat)
setkey(dat, time)

# function to compute a given stat over a time window on a given data.table
window_summary <- function(start_tm, window_len, stat_fn, my_dt) {
  pos_vec <- my_dt[, which(time>=start_tm & time<=start_tm+window_len)]
  return(stat_fn(my_dt$measure[pos_vec]))
}

# a vector of window start times
start_vec <- seq(from=-2.5, to=dat$time[nrow(dat)], by=2.5)

# sapply'ing the function above over vector of start times 
# (in this case, getting mean over 5 second windows)
result <- sapply(start_vec, window_summary, 
                 window_len=5, stat_fn=mean, my_dt=dat)


rolling_summary <- function(DF, time_col, fun, window_size, step_size, min_window=min(DF[, time_col])) {
  # time_col is name of time column
  # fun is function to apply to the subsetted data frames
  # min_window is the start time of the earliest window
  
  times <- DF[, time_col]
  
  # window_starts is a vector of the windows' minimum times
  window_starts <- seq(from=min_window, to=max(times), by=step_size)
  
  # The i-th element of window_rows is a vector that tells us the row numbers of
  # the data-frame rows that are present in window i 
  window_rows <- lapply(window_starts, function(x) { which(times>=x & times<x+window_size) })
  
  window_summaries <- sapply(window_rows, function(w_r) fun(DF[w_r, ]))
  data.frame(start_time=window_starts, end_time=window_starts+window_size, summary=window_summaries)
}

rolling_summary(DF=dat,
                time_col="time",
                fun=function(DF) mean(DF$measure),
                window_size=5,
                step_size=2.5,
                min_window=-2.5)