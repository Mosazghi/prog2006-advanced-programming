
-- | Placeholder to get input
getInput :: Either String String
getInput = Right "{'name': Mariusz, 'email': example.com}"

-- | Placeholder to parse input and get email
parseInputAndGetEmail :: String -> Either String String
parseInputAndGetEmail input =
    case input of
        "{'name': Mariusz, 'email': example.com}" -> Right "example.com"
        _ -> Left "Error: Invalid input format"

-- | Placeholder to validate email
validateEmail :: String -> Either String String
validateEmail email =
    case email of
        "example.com" -> Left "Error: Invalid email"
        _ -> Right email

-- | Placeholder to  to process notification
processNotification :: String -> Either String String
processNotification email = Right "Notification sent"

-- | Main processor function
processor :: Either String String
processor = getInput >>= 
            parseInputAndGetEmail >>= 
            validateEmail >>= 
            processNotification
