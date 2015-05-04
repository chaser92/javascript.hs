var divider = function(a, b) {
    if (b === 0) {
        throw "Please do not divide by 0!";
    }
    else {
        return a / b;
    }
};

try {
    try {
        divider(3, 0);
    } catch(e){
        log("Oops!", e);
    }  
} catch(p) {
    log("I will not be called, for that matter has been already handled!");
}

