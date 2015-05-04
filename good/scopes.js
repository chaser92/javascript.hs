var b = "global";

function a() {
    log("b is now", b);
    var b = "declared inside a()";
    log("b is now", b);
}

a();
log("b is now again", b);

function c() {
    var b = "from c(), even though it's called outside c()";
    return function() {
        log("b is now", b);
    };
}

var closure = c();
closure();