module ActivationFunctions (
      ActivationFn ( fn, deriv )
    , fnSigmoid
    , fnTanhShifted
    , fnStep
    ) where

data ActivationFn = ActivationFn {
      fn :: Double -> Double
    , deriv :: Double -> Double
    }

fnSigmoid :: ActivationFn
fnSigmoid = ActivationFn f df
    where
        f x = 1 / (1 + exp(-x))
        df x = f x * (1 - f x)

fnTanhShifted :: ActivationFn
fnTanhShifted = ActivationFn f df
    where
        f x = tanh x / 2 + 0.5
        df x = (1 / cosh x) ** 2 / 2

-- WARNING: No derivative. Should not be used for backpropagation.
fnStep :: ActivationFn
fnStep = ActivationFn f df
    where
        f x
            | x < 0     = 0
            | otherwise = 1
        df _ = 0
