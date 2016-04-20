import Data.List.Split
import Text.Read

data Error = ParsingError | IncompleteDataError | IncorrectDataError String
    deriving (Show)

data Person = Person { firstName :: String, lastName :: String, age :: Int }
    deriving(Show)

splitLines :: String -> [String]
splitLines text = splitOn "\n" text

lineToKeyValuePair :: String -> Either Error (String, String)
lineToKeyValuePair x | isValid      = Right (key, value)
                     | otherwise    = Left ParsingError
    
                    where   words =  splitOn "=" x
                            isValid =  length words == 2
                            key = head words
                            value = last words
            
textToKeyValuePairs :: String -> [Either Error (String, String)]
textToKeyValuePairs x = map lineToKeyValuePair (splitLines x)
 

findField :: String -> [Either Error (String, String)] -> Either Error String
findField searchTerm [] = Left IncompleteDataError
findField searchTerm ((Left error):xs) = Left error
findField searchTerm ((Right (key,value)):xs) | searchTerm == key  = Right value
                                              | otherwise          = findField searchTerm xs

findFirstName = findField "firstName"
findLastName = findField "lastName"
findAge = findField "age"

isValidAge :: Either Error String -> Either Error Int
isValidAge (Left error) = Left error
isValidAge (Right s) =  case result of 
                        (Just x) -> Right x
                        Nothing -> Left (IncorrectDataError s)
                        where result = readMaybe s :: Maybe Int

setFirstName ::Either Error Person ->  Either Error String -> Either Error Person
setFirstName (Left error) _ = Left error
setFirstName _ (Left error) = Left error
setFirstName (Right person) (Right firstNameValue) = Right person{firstName=firstNameValue}

setLastName ::Either Error Person ->  Either Error String -> Either Error Person
setLastName (Left error) _ = Left error
setLastName _ (Left error) = Left error
setLastName (Right person) (Right lastNameValue) = Right person{lastName=lastNameValue}

setAge ::Either Error Person ->  Either Error Int -> Either Error Person
setAge (Left error) _ = Left error
setAge _ (Left error) = Left error
setAge (Right person) (Right ageValue) = Right person{age=ageValue}

parsePerson :: String -> Either Error Person
parsePerson text = person
    where   keyValues = textToKeyValuePairs text
            person = (Right Person {}) 
                     `setFirstName` (findFirstName keyValues) 
                     `setLastName` (findLastName keyValues)
                     `setAge` (isValidAge $ findAge keyValues)
            