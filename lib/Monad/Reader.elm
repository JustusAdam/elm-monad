module Monad.Reader where


import Monad.State as S


type alias Reader env a = S.State env a


runReader : env -> Reader env a -> a
runReader env r = S.execState


andThen : Reader env a -> (a -> Reader env b) -> Reader env b
andThen = S.andThen


thenDo : Reader env a -> Reader env b -> Reader env b
thenTo = S.thenDo


return : a -> Reader env a
return = S.return


ask : Reader env env
ask = S.get


reader : (env -> a) -> Reader env a
reader f = S.map f ask


local : (env -> env) -> Reader env a -> Reader env a
local f r =
  ask `andThen` \old ->
  S.modify f
  `S.thenDo` r
  `andThen` \res ->
  S.put old `S.thenDo` return res


map : (a -> b) -> Reader env a -> Reader env b
map = S.map


ap : Reader env (a -> b) -> Reader env a -> Reader env b
ap = S.ap
