module Lib where

--need import to use guard
import Control.Monad
import Control.Applicative


data Name = Name {
    firstName :: String,
    lastName :: String
}

instance Show Name where 
    show (Name first last) = mconcat [first, " ", last]

data GradeLevel = Freshman | Sophomore | Junior | Senior deriving (Eq, Ord, Enum, Show) 

data Student = Student 
    { studentId :: Int
    , gradeLevel :: GradeLevel 
    , studentName :: Name } deriving Show 

students :: [Student]
-- 6 students, use Nobu, Lina, Mayu, Sayaka, Dad and Mom, all have last name Kim or Super
students = [
    Student 1 Freshman (Name "Norman" "Gold"),
    Student 2 Sophomore (Name "Lisa" "Gold"),
    Student 3 Junior (Name "Mary" "Gold"),
    Student 4 Senior (Name "Sarah" "Gold"),
    Student 5 Senior (Name "Dad" "Super"),
    Student 6 Senior (Name "Mom" "Super")]

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

nName = _where (startsWith 'N' . firstName) (_select studentName students)

data Teacher = Teacher 
    { teacherId :: Int
    , teacherName :: Name } deriving Show

-- teacher names should be Disney character names
teachers :: [Teacher]
teachers = [
    Teacher 100 (Name "Mickey" "Mouse"),
    Teacher 101 (Name "Minnie" "Mouse"),
    Teacher 102 (Name "Goofy" "Goof"),
    Teacher 103 (Name "Pluto" "Boy")]

data Course = Course 
    { courseId :: Int
    , courseTitle :: String
    , teacher :: Int } deriving Show

courses :: [Course]
courses = [Course 101 "French" 100
         , Course 102 "English" 101
         , Course 103 "Math" 102
         , Course 104 "History" 103]

_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a,b)
_join data1 data2 prop1 prop2 = do 
    d1 <- data1 
    d2 <- data2 
    let dpairs = (d1, d2)
    -- only where prop1 from data1 equals prop2 from data2
    guard ((prop1 (fst dpairs)) == (prop2 (snd dpairs)))
    return dpairs

teachersJoinCourses = _join teachers courses teacherId teacher 

joinData = (_join teachers courses teacherId teacher)
whereResult = _where ((== "English") .courseTitle . snd) joinData
selectResult = _select (teacherName .fst) whereResult

_hinq selectQuery joinQuery whereQuery = do 
    (\joinData -> 
        (\whereResult -> selectQuery whereResult)
        (whereQuery joinData)) joinQuery

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

--p.426 Listing 33.16
--_selectM :: Monad m =>(a -> b) -> m a -> m b
-- above function will take a function and a monad and return a monad
-- the function will take a value from the monad and return a value
-- the monad will be a list in this case


--_whereM :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
-- above function will take a function and a monad and return a monad
-- the function will take a value from the monad and return a boolean
-- the monad will be a list in this case

--_joinM :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a,b)
-- above function will take 2 monads and 2 functions and return a monad
-- the first monad will be a list in this case
-- the second monad will be a list in this case
-- the first function will take a value from the first monad and return a value
-- the second function will take a value from the second monad and return a value
-- these two values are of the same type
-- the return value will be a tuple of the two values in the monad

-- m is type of monad, a is type of data, c is type of result
-- takes three functions, _select, _join (or plain data), _where 
-- HINQ_  data constructor omits the _where type signature 
data HINQ m a b = HINQ (m a -> m b) (m a) (m a -> m a) | HINQ_ (m a -> m b) (m a)

{-
_hinq selectQuery joinQuery whereQuery = do 
    (\joinData -> 
        (\whereResult -> selectQuery whereResult)
        (whereQuery joinData)) joinQuery 
-}
runHINQ :: (Monad m,  Alternative m) => HINQ m a b -> m b 
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause 
runHINQ (HINQ_ sClause jClause) = _hinq sClause jClause (_where (\_ -> True))

-- monad: [], type of data: tuple, type of res: Name
query1 :: HINQ [] (Teacher, Course) Name
query1 = HINQ (_select (teacherName .fst)) 
              (_join teachers courses teacherId teacher) 
              (_where ((== "English") . courseTitle . snd))


query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName)
         teachers
    
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

-- Joining multiple lists to get all enrollments
data Enrollment = Enrollment 
    { student :: Int
    , course :: Int } deriving Show 

enrollments :: [Enrollment]
-- 10 randome enrolments incrementing
enrollments = 
    [ (Enrollment 1 101)
    , (Enrollment 2 101)
    , (Enrollment 3 201)
    , (Enrollment 4 101)
    , (Enrollment 5 201)
    , (Enrollment 6 101)
    , (Enrollment 7 101)
    , (Enrollment 8 102)
    , (Enrollment 9 201)]

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
englishStudentsQ = HINQ (_select (fst . fst)) (_join studentEnrollments courses snd courseId) (_where ((== "English") . courseTitle . snd))



-- Find all students taking English
englishStudents :: [Name]
englishStudents = runHINQ englishStudentsQ
