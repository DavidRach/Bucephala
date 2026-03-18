InstrumentSet <- data.frame(TheInstrument=c("Canto", "LSRII",
  "Aria", "Aurora3", "Aurora4", "Aurora5", "AuroraCS", "MACSQuant10", "Amnis"),
  Code=c("282295#", "282294#", "282292#", "349299#", "456489#", "512453#",
  "512475#", "454785#", "305467#"))

DayOffset <- c(
  Sunday    = 0,
  Monday    = 1,
  Tuesday   = 2,
  Wednesday = 3,
  Thursday  = 4,
  Friday    = 5,
  Saturday  = 6
  )

DayMap <- list(
  Sunday    = c(0, 85),  
  Monday    = c(86, 230),
  Tuesday   = c(231, 348),
  Wednesday = c(349, 466),
  Thursday  = c(467, 584),
  Friday    = c(585, 703),
  Saturday  = c(704, 820)
)
