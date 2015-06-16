x: 1;
find: 1;
loop: 1;
true: (1,0);
false: (1,0);
eq_v: (0,0)->(0,0),(1,0)->(1,0),(1,1)->(1,1);
nil: (0,0)->(0,0),(1,0)->(1,0),(1,1)->(1,1);
pred: (0,0)->(0,0), (1,0)->(1,0), (1,1)->(1,1);

find = false;
loop = true;
while (loop) {
    if (eq_v(x)) {
        find = true;
        loop = false;
    }
    else {
        if (nil(x)) {
            loop = false;
        }
        else {
            x = pred(x);
        }
    }
}
