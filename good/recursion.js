function sum(to, acc) {
    if (to > 0) {
        return sum(to - 1, acc + to);
    } else {
        return acc;
    }
}

log("Sum from 1 to 5 is", sum(5, 0));