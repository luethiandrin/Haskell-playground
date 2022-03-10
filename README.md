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

https://andrew.gibiansky.com/blog/haskell/haskell-syntax/ <-- Current Topic: Local Variable Bindings

# Haskell

## GHCi

* :? -- list commands
* :quit (or :q)
* :load [filename] (or :l [filename])
* :reload -- reload/compile the current file set after you'ved edited it (or :r)
* :cd -- change to another directory
* :type [expr] -- give the type of an expression (or -- just :t)
* :set +t tell Haskell to print type information after evaluation

## Syntax

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

### Functions

```haskell
toInt :: Float -> Int
toInt x = round x
toInt 12.3 --> returns 12 as an Int -- or

toInt :: Float -> Int
toInt = round
```

#### fromIntegral

```haskell
main = print (fromIntegral (12 :: Int) + (3.2 :: Float)) :: IO ()
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
