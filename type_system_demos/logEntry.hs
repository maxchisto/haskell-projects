import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar
import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime Data.Time.Format.defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info

data LogEntry = LogEntry {timestamp :: UTCTime, logLevel :: LogLevel, message :: String}

logLevelToString :: LogLevel -> String
logLevelToString Error  = "Error"
logLevelToString Warning = "Warning"
logLevelToString Info = "Info"

logEntryToString :: LogEntry -> String
logEntryToString log = (show $ timestamp log) ++ ": " ++ (show $ logLevel log ) ++ ": " ++ (show $ message log)

instance Show LogLevel where
    show x = show $ logLevelToString x
    
instance Show LogEntry where
    show x = show $ logEntryToString x