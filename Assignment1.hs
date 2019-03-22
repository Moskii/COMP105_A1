-- Do not alter the following line
module Assignment1 (char_to_int, repeat_char, decode, int_to_char, length_char, drop_char, encode, complex_encode, complex_decode) where


-- Part A

char_to_int :: Char -> Integer
-- A function which converts a character into an integer.
char_to_int x 
    | x == '0' = 0
    | x == '1' = 1
    | x == '2' = 2
    | x == '3' = 3
    | x == '4' = 4
    | x == '5' = 5
    | x == '6' = 6
    | x == '7' = 7
    | x == '8' = 8
    | x == '9' = 9
    | x == ' ' = 0
    | otherwise = 123456 -- Consider it as 'stub' case

repeat_char :: Char -> Integer -> String
-- This is the base case. While the recursive function
-- (or if the user enters '0') decreases 'n' by 1 and reaches 0,
-- the function returns a spring with the characters. 
-- Simple encoding allows values from 0 to 9 so if the user
-- enters a value greater than 9, the function will return an 
-- a string with 9 characters.
repeat_char _ 0 = ""
repeat_char c n 
    | c == ' ' = ""
    | n >= 10 = c:c:c:c:c:c:c:c:c:""
    | otherwise = repeat
    where repeat = c : repeat_char c (n-1)

decode :: String -> String
-- The string splits into 3 segments. Each segments is used in the previous functions
-- to formulate the deocode recursion function. 
decode [] = ""
decode (head:int:rest) =
    let
        numb = char_to_int int  
        rep = repeat_char head numb
    in
        --(or repeat_char head numb)
        rep ++ decode rest    

-- Part B

int_to_char :: Integer -> Char
-- A function which converts a integer into an character.
int_to_char 0 = '0'
int_to_char 1 = '1' 
int_to_char 2 = '2'
int_to_char 3 = '3'
int_to_char 4 = '4'   
int_to_char 5 = '5' 
int_to_char 6 = '6' 
int_to_char 7 = '7' 
int_to_char 8 = '8' 
int_to_char 9 = '9' 

length_char :: Char -> String -> Integer
-- Each time the head is equal to the chosen character, value increases by 1.
length_char _ [] = 0
length_char c (x:xs)
    | x == c = 1 + length_char c xs
    -- The function below handles with strings that begin with spaces.
    | x == ' ' = length_char c xs
    | otherwise = length_char c []

drop_char :: Char -> String -> String
-- When the head of string is equal to the desired character, the recursive function
-- will be called and repeats until to the point where the head is no longer the desired
-- character
drop_char _ [] = ""
drop_char c (x:xs) 
    | x == c = drop_char c xs
    | x == ' ' = drop_char c xs
    | otherwise = (x:xs)

encode :: String -> String
-- Base case
encode [] = ""
encode (x:rest) = 
    let
        len = length_char x (x:rest)
        charNum = int_to_char len
        droppo = drop_char x (x:rest)
    in
        x : charNum : encode droppo --Head of string is combined with the number. The function recursively repeats this on the remaining chracters.

-- Part C

complex_encode :: String -> String
complex_encode = error "Not implemented"
-- TBC

complex_decode :: String -> String
complex_decode = error "Not Implemented"
-- TBC

