multiply x y = x * y

expo y x = y^x

expoSum x y = (expo x y) + (expo y x)

hundredChecker num = do
    if num == 100 
    then putStrLn "Yes, this is hundred" 
    else putStrLn "That's not hundred"

list = ['H','e','l','l','o']
world = ['W','o','r','l','d']
hw = list ++ ", " ++ world

-- fuzes the values 
addString = 
    'H':"i"


-- !! gets an element by index
getElementByIndex =
    "Haskell" !! 0
getElementByIndexNum =
    [3,5,4,3] !! 2

len = length "dfgtrdg"

nullCheck = null []

prod = product [3,4,5,3]

infixFunction = 16 `elem` [12,3,56,16,8,13]

predicates xs = [ if x < 10 then "< 10" else "> 10!" |x <- xs, x `mod` 3 == 1, x /= 1]

length' xs = sum [1 | _ <- xs]

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
capital "" = "Empty string!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

isOdd :: (Integral a) => a -> [Char]
isOdd a
    | num == odd = "True"
    | num == even = "False!"
    | otherwise = "somethings wrong!"
    where num = a `mod` 2
          odd = 1
          even = 0

initials :: String -> String -> String  
initials firstname lastname = [x] ++ ". " ++ [y] ++ "."  
    where (x:_) = firstname  
          (y:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmiCalc w h | (w, h) <- xs]  
    where bmiCalc weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea

data Person = Person { fName :: [Char], age :: Int} deriving (Show)