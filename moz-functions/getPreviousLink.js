function () {
    var links = content.document.getElementsByTagName('a');
    links = Array.prototype.slice.call(links);

    var index = links.indexOf(content.document.activeElement);

    return links[(index+links.length-1)%links.length];
}
