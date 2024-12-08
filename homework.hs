-- MyModule.hs
data Forward a = Forward a a
    deriving (Show)

lift :: Num a => a -> Forward a 
lift x = Forward x 1

example_g :: Num a => Forward a -> Forward a
example_g (Forward y y') = Forward ((y - 1) * (y - 1) + 1) ((2 * y - 2) * y')

example_f :: Floating a => Forward a -> Forward a
example_f (Forward y y') = Forward (y * sin y) ((y * cos y + sin y) * y')
