# Position functions

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

# Solution for target area

## X search space

Assuming the target area is to the right, for what values of `vx0` will the probe ever even pass
the left edge of the target area?

`left` and `right`, as well as anything in between, will work. Anything greater than `right` will
satisfy this first requirement, but not the other (<= `right`), so we'll ignore those.

Which values below `left` will work? We can just brute-force them.

## Y search space

Given a candiate `vx0` for which `[n1, n2, ...]` satifsfy `left <= x(n) <= right`, what potential
`vy0` values do we need to check?

```
y(n) = -1/2 n² + (vy0 + 1/2) n

y(n) >= bottom
-n²/2 + (vy0 + 1/2) n >= bottom
-n²/2 + vy0 n + n/2 >= bottom
vy0 n >= n²/2 - n/2 + bottom
if n != 0 ... vy0 >= (n-1)/2 + bottom/n
   ow     ... 0 >= bottom

y(n) <= top
if n != 0 ... vy0 <= (n-1)/2 + top/n
   ow     ... 0 <= top
```

## ugh

```
Exists n : left <= x(n)                      <= right
           left <= -sx/2 n² + (vx0 + sx/2) n <= right
           sx/2 n² - (vx0 + sx/2) n + left  >= 0
           sx/2 n² - (vx0 + sx/2) n + right <= 0
           Ax = sx / 2
           Bx = - (vx0 + sx / 2)
           nxL* = [-Bx +- sqrt(Bx² - 4 Ax left)] / (2 Ax)
                = [-Bx +- sqrt(Bx² - 2 sx left)] / sx
           nxR* = [-Bx +- sqrt(Bx² - 2 sx right)] / sx

           bottom <= y(n)                    <= top
           bottom <= -1/2 n² + (vy0 + 1/2) n <= top
           1/2 n² - (vy0 + 1/2) n + bottom  <= 0
           1/2 n² - (vy0 + 1/2) n + top     >= 0
           Ay = 1/2
           By = - (vy0 + 1/2)
           nyB* = [-By +- sqrt(By2 - 4 Ay bottom)] / (2 Ax)
                =  -By +- sqrt(By2 - 4 Ay bottom)
           nyT* =  -By +- sqrt(By2 - 4 Ay top)
```

The `*` values each produce a pair of possible constraints, though it's expected that only one will
physically meaningful. Hopefully there will always be one positive and one negative. Otherwise,
might just need to check all combinations where `nxL* <= nxR*` and `nyB* <= nyT*`. Those inequalities
provide constraints on the number of steps that would carry the probe into the target area for the
given initial velocity.

If any of the discriminants involved is negative, the probe will never terminate in the target area,
and in fact will probably not pass through it.

If any of the results doesn't produce a valid interval (`nxL* <= nxR*` and `nyB* <= nyT*`), then
the probe will never terminate in the target area.

NOTE: constraint for `nx*` requires `vx0 != 0`. If `vx0 = 0`, then the x-constraint is that the target
area's x range must include 0.

# Algorithm

1. If the target area includes the origin, initial velocity is zero.

2. If the target area is directly below the origin:

    a. `vx0 = 0`. Do not compute `nxL*` or `nxR*`.

    b. Otherwise, compute `nxL*` or `nxR*` (two each). Generate all four pairings, discarding
       those that do not satisfy `nxL* <= nxR*`.

3. Compute `nyL*` and `nyR*` values (two each). Generate all four pairings, discarding
   those that do not satisfy `nyL* <= nyR*`.

4. 
