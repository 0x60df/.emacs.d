function (increment) {
    var links = content.document.getElementsByTagName('a');
    links = Array.prototype.slice.call(links);

    var index = links.indexOf(content.document.activeElement);

    return links[(index+links.length+increment)%links.length];
}
