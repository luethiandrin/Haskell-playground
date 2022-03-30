# Haskell-playground



# Intro
Start a new haskell project in the current directory via: cabal init --interactive

Install other GHC versions and tools via:

  ghcup list,
  ghcup install \<tool> \<version>

To install system libraries and update msys2/mingw64,
open the "Mingw haskell shell"
and the "Mingw package management docs"
desktop shortcuts.

## Links
https://www.haskell.org/ghcup/install/#first-steps

https://github.com/haskell-beginners-2022/course-plan

https://andrew.gibiansky.com/blog/haskell/haskell-syntax/

http://learnyouahaskell.com/chapters <-- 4. Syntax in Functions

# Haskell

## GHCi

* :? -- list commands
* :quit (or :q)
* :load [filename] (or :l [filename])
* :reload -- reload/compile the current file set after you'ved edited it (or :r)
* :cd -- change to another directory
* :type [expr] -- give the type of an expression (or -- just :t)
* :set +t tell Haskell to print type information after evaluation

## Topics

### Type declaration

```haskell
string :: String -- string is of type String
string = "String" -->
string2 = "String2" :: String

integer :: Int
integer = 12 -->
integer2 = 12 :: Int

character :: Char
character = 'H' -->
character2 = 'H' :: Char

tuple :: (String, Int, Char)
tuple = (string, integer, character) -->
tuple2 = (string, integer, character) :: (String, Int, Char)

main :: IO ()
main = print 12 -->
main = print 12 :: IO ()
```

### Main
```haskell
main = putStrLn "Hello!" :: IO ()
```

### Enum
```haskell
ghci> ['a'..'e']  
"abcde"  

ghci> [LT .. GT]  
[LT,EQ,GT]  

ghci> [3 .. 5]  
[3,4,5]  

ghci> succ 'B'  
'C'
```

### Bounded
```haskell
ghci> minBound :: Int  
-2147483648

ghci> maxBound :: Char  
'\1114111'

ghci> minBound :: Bool  
False
```

### List
```haskell
head [5,4,5,3] --> 5
tail [5,4,5,3] --> [4,5,3]
last [5,4,5,3] --> 3
init [5,4,5,3] --> [5,4,5]

length "Hello" --> 5
null [] --> True

take 2 [3,4,5,3] --> [3,4]
drop 1 [3,4,5,3] --> [4,5,3]

maximum [3,4,5,3] --> 5
minimum [3,4,5,3] --> 3
sum [3,4,5,3] --> 15
product [3,4,5,3] --> 180

-- checks if the list contains 16
16 `elem` [12,3,56,16,8,13] --> True

[1..8] --> [1,2,3,4,5,6,7,8]
[3,6..20] --> [3,6,9,12,15,18]
[0.1, 0.3 .. 1] --> [0.1,0.3,0.5,0.7,0.8999999999999999,1.0999999999999999]
['a'..'z'] --> "abcdefghijklmnopqrstuvwxyz"
['K'..'Z'] --> "KLMNOPQRSTUVWXYZ"
```

### List comprehension (predicates)
```haskell
-- excludes every number from the given list which doesnt match the modulus comparison the number 1 is specifically excluded. The if now only evaluates the newly create list
predicates xs = [ if x < 10 then "< 10" else "> 10!" |x <- xs, x `mod` 3 == 1, x /= 1]
ghci>predicates [1..20] --> ["< 10","< 10","> 10!","> 10!","> 10!","> 10!"]

length' xs = sum [1 | _ <- xs] -- the _ is used when the var is not important / used
length' "ydfgdfghjsdfdhfyj" --> 17

-- calculates every possible combination of a triangle where each side is less or equal to 10 and has a 90° angle
ghci>[ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
[(3,4,5),(6,8,10)]
```
### Functions

```haskell
toInt :: Float -> Int
toInt x = round x
toInt 12.3 --> returns 12 as an Int 
-- or
toInt :: Float -> Int
toInt = round
```

#### Syntax

#### Other stuff
If you want to add a char to a string you need to put the char inside an array!
```haskell
"Hell" ++ ['o']
"Hell" ++ "o"
```

##### Pattern matching

```haskell
num :: (Integral a) => a -> String
num 7 = "This is seven"
num 6 = "This is six"
num 5 = "This is five"
num x = "Anything but 7, 6 or 5"

factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)  

f' :: (a,b,c) -> a
f' (x, _, _) = x

s' :: (a,b,c) -> b
s' (_, y, _) = y

t' :: (a, b, c) -> c  
t' (_, _, z) = z

add3DVector :: (Num a) => (a,a,a) -> (a,a,a) -> (a,a,a)
add3DVector v1 v2 = (f' v1 + f' v2, s' v1 + s' v2,t' v1 + t' v2)

capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
-- all@() is an alias for the complete array

--Guards
bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight!"   
    | otherwise   = "You're heavy!" 

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight!"  

isOdd :: (Integral a) => a -> [Char]
isOdd a
    | num == odd = "True"
    | num == even = "False!"
    | otherwise = "somethings wrong!" -- does not make much sense -> only for syntax structure
    where num = a `mod` 2
          (odd, even) = (1, 0)

initials :: String -> String -> String  
initials firstname lastname = [x] ++ ". " ++ [y] ++ "."  
    where (x:_) = firstname  
          (y:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmiCalc w h | (w, h) <- xs]  
    where bmiCalc weight height = weight / height ^ 2

--let <bindings> in <expression>
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea


```

#### read
```haskell
-- The read function takes a string and returns a type which is a member of Read
ghci>read "8.2" + 3.8  
12.0

ghci>read "5" - 2  
3  

ghci>read "[1,2,3,4]" ++ [3]  
[1,2,3,4,3]

ghci>read "5" :: Float  
5.0
```

#### :
```haskell
addString = 
    'H':"i" --> "Hi"
```

#### !!
```haskell
getElementByIndex =
    "Haskell" !! 0 --> "H"
getElementByIndex =
    [3,5,4,3] !! 2 --> 4
```

#### fromIntegral

```haskell
print (fromIntegral (12 :: Int) + (3.2 :: Float))
```

```haskell
show 12 -- converts 12 to a string --> "12"
```
#### Output
```haskell 
putStrLn "string" -- displays a given string
```  

```haskell 
print value -- converts a given value to a string and displays it
print x = putStrLn (show x) 
-- print cannot display ASCII chars because 'show' converts it to their unicode representation: print "␀" --> "\9216"
```  

```haskell
integer = 10
str = "Hello!"
character = 'H'
tuple = (integer, str, character)

main = 
  print tuple
```
#### zip
```haskell
zip [1,2,3,4,5] [5,5,5,5,5] --> [(1,5),(2,5),(3,5),(4,5),(5,5)]

zip [1..] ["apple", "orange", "cherry", "mango"] --> [(1,"apple"),(2,"orange"),(3,"cherry"),(4,"mango")]
```
