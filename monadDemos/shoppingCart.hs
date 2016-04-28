-- Shopping cart total amout due calculator based on the Writer monad.
-- Purchases represent items in the cart

import Control.Monad
import Control.Monad.Writer

type ShoppingCart = Writer (Sum Integer) ()

purchase :: String -> Integer -> ShoppingCart
purchase name price = do tell (Sum price)
                         return ()

total :: ShoppingCart -> Integer
total = getSum . execWriter
    
listOfPurchases :: ShoppingCart
listOfPurchases = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328
  
  
-- Usage: `total listOfPurchases`
-- return: 19708