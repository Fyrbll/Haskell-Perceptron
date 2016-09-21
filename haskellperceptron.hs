-- converts a 2D String array to a 2D Int array 
stringToIntMatrix :: [[String]] -> [[Int]]
stringToIntMatrix sm = map (map (\x -> read x :: Int)) sm

-- converts a 2D String array to a 2D Double array
stringToDoubMatrix :: [[String]] -> [[Double]]
stringToDoubMatrix sm = map (map (\x -> read x :: Double)) sm

-- converts a 2D Int array to a 2D Double array
intToDoubleArray :: [[Int]] -> [[Double]]
intToDoubleArray i = map (map fromIntegral) i

-- HELPER FOR split
split_help :: String -> Char -> [String] -> [String]
split_help ([]) sep acc = acc
split_help (x:xs) sep acc = case (x == sep) of
                      True -> split_help xs sep ([]:acc)
                      _ -> split_help xs sep ((x:(head acc)):tail acc)

-- splits a String xs into a String array using Char c as
-- delimiter 
split :: Char -> String -> [String]
split sep xs = split_help (reverse xs) sep [[]]

-- splits a String str into a 2D string array using Char rowsep as
-- a row delimiter and Char colsep as column delimiter  
arrayFromString :: String -> Char -> Char -> [[String]]
arrayFromString str rowsep colsep = init (map (split colsep) 
                                    (split rowsep str))

-- gets the contents from FilePath file as a String,
-- then applies a (String -> a) function called operation
-- on the String, and prints out the result of
-- applying operation to the file contents
fileop :: Show a => (String -> a) -> FilePath -> IO ()
fileop operation file = readFile file >>= 
                        \s -> putStrLn (show (operation s)) 

-- datatype we will use to represent vectors
data Vector = V [Double]
              deriving (Show)

-- takes an array of integers and converts it into a Vector
-- (which is basically a Double array)
intListToVector :: [Int] -> Vector
intListToVector intlist = V (map fromIntegral intlist)

-- given an (Int -> Double) function called gen and an
-- Int len, outputs the vector 
-- [f(len),f(len-1),f(len-2),...,f(1)]  
create :: Int -> (Int -> Double) -> Vector 
create 0 gen = V []  
create len gen = 
  let V rest = create (len - 1) gen in V (gen (len) : rest)

-- finds the dot product of two vectors
-- please give it two equal-length vectors to
-- guarantee that there is no undesirable behaviour
dot :: Vector -> Vector -> Double
dot (V (x:xs)) (V (y:ys)) = x * y + dot (V xs) (V ys)
dot _          _          = 0

-- multiplies a vector by a scalar
mult :: Double -> Vector -> Vector
mult scalar (V (v:vs)) = 
  let V rest = mult scalar (V vs) in V ((scalar * v) : rest) 
mult _      _ = V []

-- adds two vectors
-- please give it two equal-length vectors to
-- guarantee that there is no undesirable behaviour
add :: Vector -> Vector -> Vector
add (V (v:vs)) (V (w:ws)) = 
  let V xs = add (V vs) (V ws) in V ((v + w) : xs)
add _          _          = V []

-- finds the dot product of v = values and w = weights
-- and if it is over the threshold returns 1.0 else -1.0
predict :: Vector -> Vector -> Double -> Double
predict v w z = if dot v w > z then 1.0 else -1.0 

-- goes through each row and updates weights
perceptron :: [[Double]] -> Vector -> Double -> Double -> Vector
perceptron ([]) (V w) eta z = V w   
perceptron (row:rows) (V w) eta z = 
  let
    (x, label) = (init row, last row)  
    diff = label - predict (V w) (V x) z
    w' = add (V w) (mult (eta * diff) (V x))
  in
    perceptron rows w' eta z

-- takes the text from a properly formatted CSV file and
-- executes the a simple perceptron classifier on the data,
-- with the weights of the perceptron after processing
-- being the output
classify :: Vector -> Double -> Double -> String -> Vector
classify w eta z src = perceptron
             (stringToDoubMatrix (arrayFromString src '\n' ','))
             w eta z

repeatClassify :: Int -> Vector -> Double -> Double -> String -> Vector
repeatClassify (0) w eta z src = w
repeatClassify (n)  w eta z src = let
                                    w' = (perceptron
                                         (stringToDoubMatrix (arrayFromString src '\n' ','))
                                          w eta z)
                                  in
                                    repeatClassify (n-1) w' eta z src

-- EXECUTE fileop (repeat (V [0.0, 0.0]) 0.0005 0.0) 'processedforpp.csv'

-- DON'T LOOK RELEVANT ->
colTotals :: [[Int]] -> Vector
colTotals matrix = foldr add (V [0.0,0.0,0.0])
                   (map intListToVector matrix)

findColTotals :: String -> Vector
findColTotals s = colTotals (stringToIntMatrix 
                  (arrayFromString s '\n' ','))

printColTotals :: FilePath -> IO ()
printColTotals file = fileop findColTotals file
-- DON'T LOOK RELEVANT <-
