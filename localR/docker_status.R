library(stringr)
library(dplyr)

parseDockerStats <- function(text) {
  # Read stats into table
  col_positions <- readr::fwf_positions(
    start = c(1,21,41,65,85,107,129),
    end = c(19,39,63,83,105,107,NA),
    col_names = c("container", "cpu_perc", "mem_usage", "mem_perc", "net_io", "block_io", "pids")
  )
  df1 <- readr::read_fwf(
    text,
    col_positions,
    col_types = c('cccccci'),
    skip = 1
  )
  
  # Parse columns
  cpu_perc <- str_replace(df1$cpu_perc, "%", "")
  mem_use <- str_extract(df1$mem_usage, "\\d*\\.\\d*") %>% as.numeric()
  mem_use_units <- str_extract(df1$mem_usage, "[[:alpha:]]+\\b") 
  mem_use_multiplier <- ifelse(str_detect(mem_use_units, "M"), 1, # Mb
                               ifelse(str_detect(mem_use_units, "K"), .001, # Kb
                                      ifelse(str_detect(mem_use_units, "G"), 1000, # Gb
                                             .000001 # bytes
                                      )))
  mem_use_mb <- mem_use*mem_use_multiplier
  mem_perc <- str_replace(df1$mem_perc, "%", "") %>% as.numeric()
  
  df <- data_frame(containerID = df1$container,
                   cpuPercent = cpu_perc, 
                   mem_use_mb = mem_use_mb,
                   mem_use_perc = mem_perc,
                   PIDs = df1$pids)
  return(df)
}



parseDockerPs <- function(text) {
  # Read stats into table
  col_positions <- readr::fwf_positions(
    start = c(1,21,58,83,103,123,179),
    end = c(19,56,81,101,121,177,NA),
    col_names = c("containerID", "image", "command", "created", "status", "ports", "name")
  )
  df1 <- readr::read_fwf(
    text,
    col_positions,
    col_types = c('ccccccc'),
    skip = 1
  )
  df <- mutate(df1, command = str_remove_all(command, '"'))
  return(df)
  
}


stats_text <- readr::read_file("docker_stats.txt")
ps_text <- readr::read_file("docker_ps.txt")

stats <- parseDockerStats(stats_text)
ps <- parseDockerPs(ps_text)
docker_status <- left_join(stats, ps, by = "containerID")
docker_status

docker_status %>% arrange(desc(mem_use_perc))
docker_status %>% arrange(desc(cpuPercent))
docker_status %>% arrange(desc(mem_use_mb))
docker_status %>% arrange(desc(PIDs))

docker_status %>% 
  filter(stringr::str_detect(image, "^monitor-")) %>% 
  arrange(desc(cpuPercent)) %>%
  select(name, cpuPercent, mem_use_perc, status)





