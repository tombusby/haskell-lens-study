# Haskel Lens Study Notes

## The lens laws?

Yep. Like the monad laws, these are expectations you should have about lenses. Lenses that violate them are weird. Here they are

1. Get-Put: If you modify something by changing its subpart to exactly what it was before... then nothing happens
1. Put-Get: If you modify something by inserting a particular subpart and then viewing the result... you'll get back exactly that subpart
1. Put-Put: If you modify something by inserting a particular subpart a, and then modify it again inserting a different subpart b... it's exactly as if you only did the second step.

Lenses that follow these laws are called "very well-behaved".

## Potentially Interesting Libraries:

[Data.Aeson.Lens](http://hackage.haskell.org/package/lens-aeson-1.1/docs/Data-Aeson-Lens.html)

## Operators

1. Operators that begin with `^` are kinds of views. The only example we've seen so far is `(^.)` which is... well, it's just view exactly.
1. Operators that end with `~` are like over or set. In fact, `(.~)` == set and `(%~)` is over.
1. Operators that have `.` in them are usually somehow "basic"
1. Operators that have `%` in them usually take functions
1. Operators that have `=` in them are just like their cousins where `=` is replaced by `~`, but instead of taking the whole object as an argument, they apply their modifications in a State monad.

## Some Examples

```haskell
type Degrees = Double
type Latitude = Degrees
type Longitude = Degrees

data Meetup = Meetup {
    _name :: String
  , _location :: (Latitude, Longitude)
  } deriving Show

meetup = meetup = Meetup {_name = "some meetup", _location = (0.345, 34.56546)}
```

Get:

```haskell
λ> view (location . _1) meetup
0.345
λ> meetup ^. location . _1
0.345
```

Set:

```haskell
λ> set (location . _2) 3.14 meetup
Meetup {_name = "some meetup", _location = (0.345,3.14)}
λ> location . _2 .~ 3.14 $ meetup
Meetup {_name = "some meetup", _location = (0.345,3.14)}
```

