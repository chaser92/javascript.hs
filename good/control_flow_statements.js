var number = 0; //falsy
var truth = true; //truthy
var nothing = undefined; //falsy
var name = "JS"; //truthy

// should print numbers 1..10, no WRONGs should be shown

if (truth || number) {
    log("1");
}

if (nothing || number) {
    log("WRONG");
} else {
    log("2");
}

if (truth && name) {
    log("3");
}

log(number || 4);

log(5 || "WRONG");

number = 6;

while (number <= 10) {
    log(number);
    ++number;
}

while (nothing) {
    log("WRONG");
}