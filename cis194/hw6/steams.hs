data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
    show stream = show (zip (streamToList stream) [1..10])

streamToList :: Stream a -> [a]
streamToList (Stream val stream) = [val] ++ (streamToList stream)

-- Turns input value into infinite repeat stream
streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x stream) = Stream (f x) (streamMap f stream)

-- Takes transformation function and seed, and return infinite stream of new value where n+1 = f(n)
-- Auto-increment example:  streamFromSeed (+1) 0
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream (x) (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+1) 0
