import Data.Maybe
import Debug.Trace

------------
-- Common --

parseRegular :: String -> Maybe (Int, String)
parseRegular ('[':_) = Nothing
parseRegular (',':_) = Nothing
parseRegular (']':_) = Nothing
parseRegular (r:s:rRem)
    | isDigit s = Just (read [r,s], rRem)
    | otherwise = Just (read [r], s:rRem)
parseRegular s = error $ "parseRegular " ++ s

isDigit c = elem c "0123456789" 

---------------
-- Explosion --

data Explosion = Bang Int String | Whimper String deriving Show

isBang (Bang _ _) = True
isBang _ = False

prepend c (Bang a cs) = Bang a $ c : cs
prepend c (Whimper cs) = Whimper $ c : cs

absorb r (Bang a cs) = Whimper $ show (r + a) ++ cs
absorb r (Whimper cs) = Whimper $ show r ++ cs

explode :: Int -> String -> Explosion
explode _ "" = Whimper ""
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

parseRegularPair ('[':cs)
    | isJust aPR && isJust bPR = Just (a, b, bRem)
    | otherwise = Nothing
    where aPR = parseRegular cs
          Just (a, ',':aRem) = aPR
          bPR = parseRegular aRem
          Just (b, ']':bRem) = bPR
parseRegularPair _ = Nothing 

propagate _ "" = ""
propagate b ('[':cs) = '[' : propagate b cs
propagate b (',':cs) = ',' : propagate b cs
propagate b (']':cs) = ']' : propagate b cs
propagate b cs = show (r + b) ++ rRem
    where Just (r, rRem) = --trace ("propagate " ++ show b ++ cs) $
                           parseRegular cs
                           
-----------                           
-- Split --

