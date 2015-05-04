var person = {
    firstName: "John",
    numFriends: 3,
    friends: {
        "0": "Chris",
        "1": "James",
        "2": "Jane"
    }
};

var i=0;

function getFriends(pers) {
    var all = "";
    while (i < pers.numFriends) {
        all = all + person.friends[i] + ","; // objects used as arrays!
        ++i;
    }
    return all;
}

log(person.firstName, "has following friends:", getFriends(person));
