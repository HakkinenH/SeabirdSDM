library(log4r)
#> 
#> Attaching package: 'log4r'
#> The following object is masked from 'package:base':
#> 
#>     debug

nowD<-format(Sys.time(), format='%y%m%d-%H-%M-%S')

my_logfile = paste0("./logs/SDMlog-",nowD,".log")

my_console_appender = console_appender(layout = default_log_layout())
my_file_appender = file_appender(my_logfile, append = TRUE, 
                                 layout = default_log_layout())

#set logging level to warning
my_logger <- log4r::logger(threshold = "INFO", 
                           appenders= list(my_console_appender,my_file_appender))


#create manual function for when you want to insert stuff
logManual <- function(x) {
  log4r::info(my_logger, x)
}


logBreak <- function() {
  log4r::info(my_logger, "\n\n")
}
