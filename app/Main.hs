module Main where

import Lib
import Db
import Types

--res is [Minnie Mouse], ie. Name Minnie Mouse 
finalResult :: [Name]
finalResult = _hinq (_select (teacherName . fst)) 
                    (_join teachers courses teacherId teacher)
                    (_where ((== "English") . courseTitle . snd))

{-
[(Teacher {teacherId = 100, teacherName = Mickey Mouse},Course {courseId = 101, courseTitle = "French", teacher = 100}),(Teacher {teacherId = 101, teacherName = Minnie Mouse},Course {courseId = 102, courseTitle = "English", teacher = 101}),(Teacher {teacherId = 102, teacherName = Goofy Goof},Course {courseId = 103, courseTitle = "Math", teacher = 102}),(Teacher {teacherId = 103, teacherName = Pluto Boy},Course {courseId = 104, courseTitle = "History", teacher = 103})]
-}
teacherAndCourses = (_join teachers courses teacherId teacher)

-- get first names from your finalResult for all teachers nor just those teaching English

selectQueryfirstName = (_select firstName) --
joinQueryFinalResult = finalResult
whereQueryAlwaysTrue = (_where (\_ -> True))
teacherFirstName :: [String]
-- selectQuery joinQuery whereQuery
teacherFirstName = _hinq selectQueryfirstName joinQueryFinalResult whereQueryAlwaysTrue

-- monad: [], type of data: tuple, type of res: Name
query1 :: HINQ [] (Teacher, Course) Name
query1 = HINQ (_select (teacherName .fst)) 
              (_join teachers courses teacherId teacher) 
              (_where ((== "English") . courseTitle . snd))


query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName)
         teachers

-- use a placholder main
main :: IO ()
main = do
    let nName = _where (startsWith 'N' . firstName) (_select studentName students)
    let onlyN = head nName
    putStrLn ((firstName onlyN) ++ " " ++ (lastName onlyN) ++ " is the only student with a first name starting with N")
    let engTeacherFirstName = firstName $ head finalResult
    let engTeacherLastName = lastName $ head finalResult
    putStrLn ("English teacher is " ++ engTeacherFirstName ++ " " ++ engTeacherLastName)
