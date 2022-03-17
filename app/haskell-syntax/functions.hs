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