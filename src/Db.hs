module Db where 

import Types 

students :: [Student]
-- 6 students, use Nobu, Lina, Mayu, Sayaka, Dad and Mom, all have last name Kim or Super
students = [
    Student 1 Freshman (Name "Norman" "Gold"),
    Student 2 Sophomore (Name "Lisa" "Gold"),
    Student 3 Junior (Name "Mary" "Gold"),
    Student 4 Senior (Name "Sarah" "Gold"),
    Student 5 Senior (Name "Dad" "Super"),
    Student 6 Senior (Name "Mom" "Super")]

-- teacher names should be Disney character names
teachers :: [Teacher]
teachers = [
    Teacher 100 (Name "Mickey" "Mouse"),
    Teacher 101 (Name "Minnie" "Mouse"),
    Teacher 102 (Name "Goofy" "Goof"),
    Teacher 103 (Name "Pluto" "Boy")]

courses :: [Course]
courses = [Course 101 "French" 100
         , Course 102 "English" 101
         , Course 103 "Math" 102
         , Course 104 "History" 103]

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