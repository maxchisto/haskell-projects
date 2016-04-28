-- find every user who's password matches the lookup string
import Control.Monad
import Control.Monad.Reader

type User = String
type Password = String
type UsersTable = [(User, Password)]

passTable = [("user", "123456"), ("x", "hi"), ("root", "123456")]

lookupPass :: Password -> UsersTable -> [User]
lookupPass pass table = [u | (u,p) <- table , p == pass]

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = do
    users <- asks (lookupPass "123456")
    return users
    
findUsers :: UsersTable -> [User]
findUsers = runReader usersWithBadPasswords

-- Usage: `findUsers passTable`
-- returns: ["user","root"]