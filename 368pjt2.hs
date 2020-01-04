import Data.List
import System.IO
import Data.Char
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

edgeLinePrinter = concat ["+-----" | r<-[1..7]] ++ "+" ++ "\n"

--find the first day of the month in week, and how many days in this month
createMonthArray :: Integer -> Int -> [[Char]]
createMonthArray year month = (replicate preDays " ") ++ [ show theDay | theDay<-[1..lastDay] ] ++ (replicate restDays " ")
                            where fstDay = digitToInt(last(showWeekDate(fromGregorian year month 01)))
                                         where fstDay = if fstDay == 7
                                                          then 0
                                                          else fstDay
                                  ttlDays = gregorianMonthLength year month
                                  preDays = fstDay
                                  lastDay = ttlDays
                                  restDays = if (fstDay + ttlDays) > 35
                                               then 42 - fstDay - ttlDays
                                               else 35 - fstDay - ttlDays

--draw the weekly schedule of this month
weekDraw :: [[Char]] -> IO()
weekDraw (days) = if days == []
                    then do putStrLn $ edgeLinePrinter
                    else do putStrLn $ edgeLinePrinter ++ ( concat["| "++ x ++(replicate (4-(length x)) ' ')|x <- take 7 days] ++"|") 
                            weekDraw (drop 7 days)





--main funciton to print the month
--it takes the prevoius two functions to get the required month 
calendar :: Integer -> Int -> IO()
calendar theYear monthNdx = do putStrLn $ "\n" ++ edgeLinePrinter ++ concat["        ",theMonth," ",show theYear,"\n"] ++ edgeLinePrinter ++ concat[weekTitle]
                               weekDraw (createMonthArray theYear monthNdx)
                          where theMonth = months!!(monthNdx-1)
                                         where months = ["JANUARY","FEBRUARY","MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER"]
                                weekTitle = "| Sun | Mon | Tue | Wed | Thu | Fri | Sat |"



























