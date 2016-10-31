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
    
splitBatches :: Int -> [a] -> [[a]]
splitBatches n [] = []
splitBatches n list =
    let (batch, rest) = splitAt n list
    in batch : splitBatches n rest
  
-- list of ([predictors], truecl) for each sample -> list of ([weights], b) for each batch
trainOnePass :: [([Double], Double)] -> [([Double], Double)]
trainOnePass df = scanl trainOneBatch ([-0.2,0.2], 0) batches
    where batches = splitBatches 10 df

-- ([weights], b) [([predictors], trueclass)] for samples in one batch -> ([weights], b) for one batch
trainOneBatch :: ([Double], Double) -> [([Double], Double)] -> ([Double], Double)
trainOneBatch wtsb batch = updateWeights [gW | (gW, gB) <- gradsBatch] [gB | (gW, gB) <- gradsBatch] wtsb
    where gradsBatch = map (trainOneLine wtsb) batch

-- ([weights], b) ([predictors], trueclass) for one sample -> ([gradWeights], gradB) for one sample
trainOneLine :: ([Double], Double) -> ([Double], Double) -> ([Double], Double)
trainOneLine wtsb (preds, cl) = feedBackward (feedForward wtsb preds) cl

-- ([weights], b) [predictors] -> ([weights], b, [[activations]])
feedForward :: ([Double], Double) -> [Double] -> [[Double]]
feedForward (wts, b) preds = [preds, [activate(weigh preds wts b)]]

weigh :: [Double] -> [Double] -> Double -> Double
weigh preds wts b = sum (zipWith (*) preds wts) + b

activate :: Double -> Double
activate z = 1 / (1 + exp(-z))

activate' :: Double -> Double
activate' a = a * (1-a)

feedBackward :: [[Double]] -> Double -> ([Double], Double)
feedBackward activations cl = (gradW, gradB)
    where deltaOut = [(p - cl) * activate' p | p <- last activations]
          gradB = head deltaOut
          gradW = [dl * pl | dl <- deltaOut, pl <- head activations]
          
-- processes the entire list of grads for a batch
updateWeights :: [[Double]] -> [Double] -> ([Double], Double) -> ([Double], Double)
updateWeights gradsW gradsB (oldW, oldB) = (zipWith (-) oldW sumGradW, oldB - sumGradB)
    where alpha = 0.3
          sumGradW = map ((*) (alpha / fromIntegral(length gradsW)) . foldl (+) 0) gradsW
          sumGradB = alpha / fromIntegral(length gradsB) * foldl (+) 0 gradsB


