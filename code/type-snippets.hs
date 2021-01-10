module Snippets where

x1 = ['a', 'b', 'c']

x2 = ('a', 'b', 'c')

x3 = [(False, 0), (True, 10)]

x4 = ([False, True], [0, 1])

x5 = [tail, reverse, init]

swap (x, y) = (y, x)

pair x y = (x, y)

double x = x*x

palindrome xs = xs == reverse xs

twice f x = f (f x)
