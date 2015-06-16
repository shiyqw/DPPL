x: 1;
y: 1;
z: 1;
succ: (0,0)->(0,0), (1,0)->(1,0), (1,1)->(1,1);
pred: (0,0)->(0,0), (1,0)->(1,0), (1,1)->(1,1);
greater_than_zero: (0,0)->(0,0), (1,0)->(1,0), (1,1)->(1,1);
sub: (1,0)->(1,0)->(1,0);
gt: (1,0)->(1,0)->(1,0);

if (greater_than_zero(x)) {
    while (greater_than_zero(y)) {
        if (gt(x,y)) {
            x = sub(x,y);
        }
        else {
            y = sub(y,x);
        }
    }
    z = x;
}
else {
    z = y;
}
