pyth :: [(Integer,Integer,Integer)]
pyth = [(a,b,c) | c <- [1..], a <- [1..c], a <= c,  b <- [1..(c^2-a^2)],a^2+b^2 == c^2]

