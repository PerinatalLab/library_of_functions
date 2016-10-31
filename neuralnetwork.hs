main = do
        df <- readFile "anntest.txt"
        print . trainOnePass . processFile $ df

readInt :: String -> Double
readInt = read

processFile :: String -> [([Double], Double)]
processFile txt = map processLine (lines txt)

processLine :: String -> ([Double], Double)
processLine l = (init w, last w)
    where w = map readInt (words l)
    
trainOnePass :: [([Double], Double)] -> [([Double], Double)] -- list of ([weights], b)
trainOnePass df = scanl trainOneLine ([-0.1,0.1], 0) df

-- ([weights], b) ([predictors], trueclass) -> ([weights], b)
trainOneLine :: ([Double], Double) -> ([Double], Double) -> ([Double], Double)
trainOneLine wtsb (preds, cl) = feedBackward (feedForward wtsb preds) cl

-- ([weights], b) [predictors] -> ([weights], b, [[activations]])
feedForward :: ([Double], Double) -> [Double] -> ([Double], Double, [[Double]])
feedForward (wts, b) preds = (wts, b, [preds, [activate(weigh preds wts b)]])

weigh :: [Double] -> [Double] -> Double -> Double
weigh preds wts b = sum (zipWith (*) preds wts) + b

activate :: Double -> Double
activate z = 1 / (1 + exp(-z))

feedBackward :: ([Double], Double, [[Double]]) -> Double -> ([Double], Double)
feedBackward (wts, b, activations) cl = (, b - alpha*gradB)
    where alpha = 0.1
          deltaOut = [(p - cl) * activate' p | p <- last activations]
          gradB = head deltaOut
          gradW = [dl * pl | dl <- deltaOut, pl <- head activations]
          
--updateWeights :: ([Double], Double) -> ([Double], Double)
--updateWeights (gradW, gradB) = (zipWith (-) wts . map (* alpha) $ gradW, b - alpha*gradB)

activate' :: Double -> Double
activate' a = a * (1-a)

--calcErrorOut :: [(Double, Double)] -> Double
--calcErrorOut list = map (\(predcl, truecl) -> -(truecl - predcl) * (activate' predcl) ) list



