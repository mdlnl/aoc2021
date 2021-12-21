import Data.Maybe
import Debug.Trace

data Explosion = Bang Int String | Whimper String deriving Show

isBang (Bang _ _) = True
isBang _ = False

prepend c (Bang a cs) = Bang a $ c : cs
prepend c (Whimper cs) = Whimper $ c : cs

absorb r (Bang a cs) = Whimper $ show (r + a) ++ cs
absorb r (Whimper cs) = Whimper $ show r ++ cs

explode :: Int -> String -> Explosion
explode 4 s@('[':cs)
    | isJust rp = Bang a $ '0' : propagate b rpRem
    | otherwise = Whimper $ '[' : res -- don't need to propagate bang here because
                                      -- there must be a Regular number to the right
                                      -- of a [
    where rp = parseRegularPair s
          (a,b,rpRem) = fromJust rp
          Whimper res = explode 5 cs
explode d ('[':cs) = prepend '[' $ explode (d+1) cs
explode d (',':cs) = prepend ',' $ explode d cs
explode d (']':cs) = prepend ']' $ explode (d-1) cs
explode d cs = absorb r $ explode d rRem -- can only be Regular at this point!
    where Just (r, rRem) = --trace ("absorb " ++ show d ++ " " ++ cs) $
                           parseRegular cs

parseRegularPair ('[':cs) = Just (a, b, bRem)
    where Just (a, ',':aRem) = parseRegular cs
          Just (b, ']':bRem) = parseRegular aRem
parseRegularPair _ = Nothing 

parseRegular ('[':_) = Nothing
parseRegular (',':_) = Nothing
parseRegular (']':_) = Nothing
parseRegular (r:s:rRem)
    | isDigit s = Just (read [r,s], rRem)
    | otherwise = Just (read [r], s:rRem)

isDigit c = elem c "0123456789" 

propagate b ('[':cs) = '[' : propagate b cs
propagate b (',':cs) = ',' : propagate b cs
propagate b (']':cs) = ']' : propagate b cs
propagate b cs = show (r + b) ++ rRem
    where Just (r, rRem) = --trace ("propagate " ++ show b ++ cs) $
                           parseRegular cs