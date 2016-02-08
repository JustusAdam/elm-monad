module Monad.Writer where


import Monad.State as S


type alias Writer w a = S.State [w] a


runWriter : Writer w a -> ([w], a)
runWriter = S.runState []


write : w -> Writer w ()
write w s = (w::s, ())


andThen : Writer w a -> (a -> Writer w b) -> Writer w b
andThen = S.andThen


thenDo : Writer w a -> Writer w b -> Writer w b
thenDo = S.thenDo


map : (a -> b) -> Writer w a -> Writer w b
map = S.map


return : a -> Writer w a
return = S.return


ap : Writer w (a -> b) -> Writer w a -> Writer w b
ap = S.ap
