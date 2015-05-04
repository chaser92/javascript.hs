function downloadFile(fileName, onCompleted) {
    if (fileName === "cat.txt")
        onCompleted("^__^");
    else if (fileName === "dog.txt")
        onCompleted("-__-");
}

function getAnimals(onAllFinished) {
    var anims = "";
    // this callback has access to a variable declared locally!
    var addAnim = function(jpg) {
        anims = anims + jpg; 
    };
    downloadFile("cat.txt", addAnim);
    downloadFile("dog.txt", addAnim);
    onAllFinished(anims);
}

getAnimals(function(all) {
    log("Downloaded animals:", all);
});