module Monad.Reader where


import Monad.State as S


type alias Reader env a = S.State env a


runReader : env -> Reader env a -> a
runReader env r =
  let (_, a) = r env in a


andThen : Reader env a -> (a -> Reader env b) -> Reader env b
andThen = S.andThen


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
