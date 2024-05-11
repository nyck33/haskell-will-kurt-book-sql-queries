module Lib where

--need import to use guard
import Control.Monad
import Control.Applicative

import Types
import Db 


-- in do notation can treat list as single value
{-
_select (firstName . studentName) students
["Norman","Lisa","Mary","Sarah","Dad","Mom"] 
-}
--_select :: (a -> b) -> [a] -> [b]
_select :: Monad m => (a -> b) -> m a -> m b
_select prop vals = do 
    val <- vals
    return (prop val)

-- _where (a -> Bool) -> [a] -> [a]
_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where test vals = do 
    val <- vals 
    guard (test val) --filters out values that don't pass the test
    return val 

-- test _where with startsWith
startsWith :: Char -> String -> Bool
startsWith char string = char == (head string)

_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a,b)
_join data1 data2 prop1 prop2 = do 
    d1 <- data1 
    d2 <- data2 
    let dpairs = (d1, d2)
    -- only where prop1 from data1 equals prop2 from data2
    guard ((prop1 (fst dpairs)) == (prop2 (snd dpairs)))
    return dpairs
{-
teachersJoinCourses = _join teachers courses teacherId teacher 

joinData = (_join teachers courses teacherId teacher)
whereResult = _where ((== "English") .courseTitle . snd) joinData
selectResult = _select (teacherName .fst) whereResult
-}

_hinq selectQuery joinQuery whereQuery = do 
    (\joinData -> 
        (\whereResult -> selectQuery whereResult)
        (whereQuery joinData)) joinQuery


runHINQ :: (Monad m,  Alternative m) => HINQ m a b -> m b 
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause 
runHINQ (HINQ_ sClause jClause) = _hinq sClause jClause (_where (\_ -> True))

-- HINQ with Maybe types
possibleTeacher :: Maybe Teacher 
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

maybeQuery1 :: HINQ Maybe (Teacher, Course) Name 
maybeQuery1 = HINQ (_select (teacherName . fst)) (_join possibleTeacher possibleCourse teacherId teacher) (_where ((=="French") . courseTitle . snd))

missingCourse :: Maybe Course
missingCourse = Nothing

-- monad type, data type, return type
maybeQuery2 :: HINQ Maybe (Teacher, Course) Name 
maybeQuery2 = HINQ (_select (teacherName .fst))
                   (_join possibleTeacher missingCourse teacherId teacher)
                   (_where ((=="French") . courseTitle . snd))

--check just join statement below
joinStBelow :: [(Student, Enrollment)]
joinStBelow = _join students enrollments studentId student
--output:
{-
[(Student {studentId = 1, gradeLevel = Freshman, studentName = Norman Gold},Enrollment {student = 1, course = 101}),
(Student {studentId = 2, gradeLevel = Sophomore, studentName = Lisa Gold},Enrollment {student = 2, course = 101}),
(Student {studentId = 3, gradeLevel = Junior, studentName = Mary Gold},Enrollment {student = 3, course = 201}),
(Student {studentId = 4, gradeLevel = Senior, studentName = Sarah Gold},Enrollment {student = 4, course = 101}),
(Student {studentId = 5, gradeLevel = Senior, studentName = Dad Super},Enrollment {student = 5, course = 201}),
(Student {studentId = 6, gradeLevel = Senior, studentName = Mom Super},Enrollment {student = 6, course = 101})]
-}
--queries students and courses enrolled in
-- data HINQ m a b = HINQ (m a -> m b) (m a) (m a -> m a) | HINQ_ (m a -> m b) (m a)
--studentEnrollmentsQ :: HINQ [] (Student, Enrollment) (Student, Enrollment)
studentEnrollmentsQ = HINQ_ (_select (\(st, en) -> (studentName st, course en))) (_join students enrollments studentId student)

studentEnrollments :: [(Name, Int)]
studentEnrollments = runHINQ studentEnrollmentsQ
--output
{-
ghci> studentEnrollments
[(Norman Gold,101),(Lisa Gold,101),(Mary Gold,201),(Sarah Gold,101),(Dad Super,201),(Mom Super,101)]
-}

-- join studentEnrollments with courses

-- first check join statement
englishStudentsQ :: HINQ [] ((Name, Int), Course) Name
englishStudentsQ = HINQ (_select (fst . fst)) (_join studentEnrollments courses snd courseId) (_where ((== "English") . courseTitle . snd))


-- Find all students taking English
englishStudents :: [Name]
englishStudents = runHINQ englishStudentsQ
