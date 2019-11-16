# Haskel Lens Study Notes

## The lens laws?

Yep. Like the monad laws, these are expectations you should have about lenses. Lenses that violate them are weird. Here they are

1. Get-Put: If you modify something by changing its subpart to exactly what it was before... then nothing happens
1. Put-Get: If you modify something by inserting a particular subpart and then viewing the result... you'll get back exactly that subpart
1. Put-Put: If you modify something by inserting a particular subpart a, and then modify it again inserting a different subpart b... it's exactly as if you only did the second step.

Lenses that follow these laws are called "very well-behaved".
