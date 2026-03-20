#' Generates the schedule plots in an iLab format
#' 
#' @param x The name of the instrument
#' @param data The returned parsed booking data from BookingRetrieval
#' @param FolderName Default ScheduleMonitor, can switch 
#' out as desired
#' @param AlternateDirectory Default is your Documents folder,
#'  can switch out if desired
#' @param width plot default 8.5
#' @param height plot default 11
#' @param units plot default "in"
#' @param dpi plot default 300
#' @param device "png"
#' 
#' @importFrom dplyr filter mutate across
#' @importFrom tidyr replace_na
#' @importFrom ggplot2 ggplot geom_tile aes scale_x_discrete scale_y_continuous 
#' labs geom_hline theme_bw theme element_text geom_rect geom_text ggsave
#' element_blank element_text
#' @importFrom stringr str_wrap
#' 
#' @return A plot for display
#' 
#' @export
#' 
#' @examples
#' 
#' A <- 2 + 2
#' 
iLabPlots <- function(x, data, FolderName="ScheduleMonitor", AlternateDirectory=NULL,
    width = 8.5, height = 11, units = "in", dpi = 300, device = "png"){

  # Checking for existing images folder in ScheduleMonitor folder
  FolderPattern <- paste0("^", FolderName, "$")
  if(!is.null(AlternateDirectory)){(DocumentsPath <- AlternateDirectory)
    } else {DocumentsPath <- OperatingSystemCheck()}
  MonitorFolder <- list.files(DocumentsPath, pattern=FolderPattern,
    full.names=TRUE)
  if (length(MonitorFolder) == 1){
    ImagesFolder <- list.files(MonitorFolder, pattern="images",full.names=TRUE)
    if (length(ImagesFolder) == 1){
      # Successfully found the images folder
    } else {stop(FolderName, " images folder not Found")}
  } else {stop(FolderName, " not Found")}

internal <- data |> filter(Instrument %in% x)
  
if (nrow(internal) > 0){
internal <- internal |> mutate(across(everything(), ~ tidyr::replace_na(., "Staff")))
internal <- internal |> mutate(Start_Hour = as.numeric(substr(Start, 1, 2)) +
     as.numeric(substr(Start, 4, 5))/60 +
      ifelse(grepl("PM", Start) & as.numeric(substr(Start, 1, 2)) != 12, 12, 0),
       Start_Hour = ifelse(Start_Hour == 12 & grepl("AM", Start), 0, Start_Hour),
       Stop_Hour = as.numeric(substr(Stop, 1, 2)) + as.numeric(substr(Stop, 4, 5))/60 +
        ifelse(grepl("PM", Stop) & as.numeric(substr(Stop, 1, 2)) != 12, 12, 0),
         Stop_Hour = ifelse(Stop_Hour == 12 & grepl("AM", Stop), 0, Stop_Hour),
         Midpoint = (Start_Hour + Stop_Hour) / 2, 
         Day = factor(Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday",
         "Thursday", "Friday", "Saturday")))
}

Schedule <- expand.grid(
  hour = 0:23,
  day = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
) |>
  mutate(Day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))
  
Now <- Sys.time()
TheCaption <-  format(Now, "%Y-%m-%d %H:%M")

Calendar <- ggplot() +
  geom_tile(
    data = Schedule,
    aes(x = Day, y = hour),
    fill = "white", color = "gray80", linewidth = 0.5, height = 1
  ) +
  scale_x_discrete(position = "top") +
  scale_y_continuous(
    breaks = seq(0, 23, 1),
    trans = "reverse",    # so 23 (11 PM) is at top, 0 (midnight) at bottom
    labels = c("12 AM","1 AM","2 AM","3 AM","4 AM","5 AM","6 AM","7 AM","8 AM","9 AM",
               "10 AM","11 AM","12 PM","1 PM","2 PM","3 PM","4 PM","5 PM","6 PM",
               "7 PM","8 PM","9 PM","10 PM","11 PM")
  ) +
  labs(x = NULL, y = NULL, title = x, caption = TheCaption) + 
  geom_hline(yintercept = 9, color = "blue", linetype = "solid", linewidth = 1) +   # 9 AM
  geom_hline(yintercept = 17, color = "blue", linetype = "solid", linewidth = 1) +  # 5 PM
  geom_hline(
    yintercept = as.numeric(format(Now, "%H")) + as.numeric(format(Now, "%M")) / 60,
    color = "red", linetype = "dashed", linewidth = 1
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

  if (nrow(internal) > 0){
    Calendar <- Calendar +
      geom_rect(
        data = internal,
        aes(
          xmin = as.numeric(Day) - 0.45,
          xmax = as.numeric(Day) + 0.45,
          ymin = Stop_Hour,
          ymax = Start_Hour,
          fill = User
        ),
        color = "black", alpha = 0.7
      ) + geom_text(
        data = internal,
        aes(
          x = as.numeric(Day),
          y = (Start_Hour + Stop_Hour) / 2,
          label = stringr::str_wrap(User, width = 10)
        ),
        size = 3, color = "white"
      )
  }

    filename <- paste0(x, ".png")
    StorageLocation <- file.path(ImagesFolder, filename)

    ggsave(
    filename = StorageLocation,
    plot = Calendar,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    device = device
    )
}