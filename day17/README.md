Single-step drag function:

```
vx -> vx - sign(vx0)
vy -> vy - 1

```

where `sign` can be -1, 0, or +1.

Multistep drag function:

```
vx(n) = vx0 - n sign(vx0)
vy(n) = vy0 - n
```

Position after `n` steps (launch from `(0,0)`):

```
sx = sign(vx0)

x(n) = vx(0) + vx(1)    + vx(2)      + ... + vx(n-1)
     = vx0   + vx0 - sx + vx0 - 2 sx + ... + vx0 - (n-1) sx
     = n vx0 - n (n-1) sx / 2
     = n vx0 - sx/2 n² + sx/2 n
     = -sx/2 n² + (vx0 + sx/2) n

y(n) = vy(0) + vy(1)   + vy(2)   + ... + vy(n-1)
     = vy0   + vy0 - 1 + vy0 - 2 + ... + vy0 - (n-1)
     = n vy0 - n (n-1) / 2
     = n vy0 - 1/2 n² + 1/2 n
     = -1/2 n² + (vy0 + 1/2) n
```

Note these will both always be integers, even if `n` is odd, because in that case each term
will have a .5.

Solution for target area:

```
Exists n : left <= x(n)                      <= right
           left <= -sx/2 n² + (vx0 + sx/2) n <= right
           sx/2 n² - (vx0 + sx/2) + left  <= 0
           sx/2 n² - (vx0 + sx/2) + right >= 0

           left <= y(n)                    <= right
           left <= -1/2 n² + (vy0 + 1/2) n <= right
           1/2 n² - (vy0 + 1/2) n + left  >= 0
           1/2 n² - (vy0 + 1/2) n + right >= 0
```
