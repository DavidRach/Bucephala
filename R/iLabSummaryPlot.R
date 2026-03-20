#' Returns a instrument summary plot for the particular day
#' 
#' @param x TBD
#' @param data TBD
#' @param instruments TBD
#' 
#' @importFrom dplyr filter mutate
#' @importFrom tidyr replace_na
#' @importFrom lubridate parse_date_time hour minute
#' @importFrom ggplot2 ggplot geom_tile aes scale_y_continuous labs geom_hline
#'  theme_bw theme element_text geom_rect geom_text ggsave
#' @importFrom stringr str_wrap
#' 
#' @return A summary plot
#' 
#' @export
#' 
#' @examples 
#' 
#' A <- 2+2
#' 
iLabSummaryPlot <- function(data){
  TodaysDate <- Sys.Date()
  CurrentDay <- Data |> filter(Date %in% TodaysDate)
  CurrentDay <- Data |> filter(Date %in% TodaysDate) |> 
    mutate(User = replace_na(User, "Staff"))

  CurrentDay <- CurrentDay |>
    mutate(
      Start_dt = parse_date_time(Start, orders = c("I:M p", "I:M:S p"), tz = "UTC"),
      Stop_dt  = parse_date_time(Stop,  orders = c("I:M p", "I:M:S p"), tz = "UTC"),
      Start_Hour = hour(Start_dt) + minute(Start_dt)/60,
      Stop_Hour  = hour(Stop_dt)  + minute(Stop_dt)/60
    )

  instruments <- sort(unique(CurrentDay$Instrument))
  Schedule <- expand.grid(hour = 0:23, instrument = instruments)
  TheCaption <- Sys.time()

  Calendar <- ggplot() +
    geom_tile(
      data = Schedule,
      aes(x = instrument, y = hour),
      fill = "white", color = "gray80", linewidth = 0.5, height = 1
    ) +
    scale_y_continuous(
      breaks = seq(0, 23, 1),
      trans = "reverse",
      labels = c("12 AM","1 AM","2 AM","3 AM","4 AM","5 AM","6 AM","7 AM","8 AM","9 AM",
                "10 AM","11 AM","12 PM","1 PM","2 PM","3 PM","4 PM","5 PM","6 PM",
                "7 PM","8 PM","9 PM","10 PM","11 PM")
    ) +
    labs(
      x = NULL, y = NULL,
      title = paste(TodaysDate),
      caption = TheCaption
    ) +
    geom_hline(yintercept = 9,  color = "blue", linetype = "solid", linewidth = 1) +   # 9 AM
    geom_hline(yintercept = 17, color = "blue", linetype = "solid", linewidth = 1) +  # 5 PM
    geom_hline(
      yintercept = hour(TheCaption) + minute(TheCaption)/60,
      color = "red", linetype = "dashed", linewidth = 1
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
    )

  if (nrow(CurrentDay) > 0) {
    Calendar <- Calendar +
      geom_rect(
        data = CurrentDay,
        aes(
          xmin = as.numeric(factor(Instrument, levels = instruments)) - 0.45,
          xmax = as.numeric(factor(Instrument, levels = instruments)) + 0.45,
          ymin = Stop_Hour,
          ymax = Start_Hour,
          fill = User
        ),
        color = "black", alpha = 0.7
      ) +
      geom_text(
        data = CurrentDay,
        aes(
          x = as.numeric(factor(Instrument, levels = instruments)),
          y = (Start_Hour + Stop_Hour) / 2,
          label = stringr::str_wrap(User, width = 10)
        ),
        size = 3, color = "white"
      )
  }
  
    filename <- paste0("AmnisZeta", ".png")
    StorageLocation <- file.path("CurrentSchedule", filename)
    ggsave(
    filename = StorageLocation,
    plot = Calendar,
    width = 8.5,
    height = 11,
    units = "in",
    dpi = 300,
    device = "png"
    )

    filename <- paste0("AuroraZeta", ".png")
    StorageLocation <- file.path("CurrentSchedule", filename)
    ggsave(
    filename = StorageLocation,
    plot = Calendar,
    width = 8.5,
    height = 11,
    units = "in",
    dpi = 300,
    device = "png"
    )
}
