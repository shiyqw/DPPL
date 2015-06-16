x: 1;
y: 1;
z: 0;
succ: (0,0)->(0,0), (1,0)->(1,0), (1,1)->(1,1);
pred: (0,0)->(0,0), (1,0)->(1,0), (1,1)->(1,1);
greater_than_zero: (0,0)->(0,0), (1,0)->(1,0), (1,1)->(1,1);

z = 0;
while (greater_than_zero(x)) {
    x = pred(x);
    u = y;
    while (greater_than_zero(y)) {
        y = pred(y);
        z = succ(z);
    }
    y = u;
}










