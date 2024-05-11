module Types where 

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

data Teacher = Teacher 
    { teacherId :: Int
    , teacherName :: Name } deriving Show

data Course = Course 
    { courseId :: Int
    , courseTitle :: String
    , teacher :: Int } deriving Show

-- m is type of monad, a is type of data, c is type of result
-- takes three functions, _select, _join (or plain data), _where 
-- HINQ_  data constructor omits the _where type signature 
data HINQ m a b = HINQ (m a -> m b) (m a) (m a -> m a) | HINQ_ (m a -> m b) (m a)

-- Joining multiple lists to get all enrollments
data Enrollment = Enrollment 
    { student :: Int
    , course :: Int } deriving Show 