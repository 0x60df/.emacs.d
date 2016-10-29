function (pattern) {
    var links = content.document.getElementsByTagName('a');
    links = Array.prototype.slice.call(links);

    return links.filter(function(e, i, a) {
        return (e.getBoundingClientRect().top > 0 &&
                e.getBoundingClientRect().top < content.window.innerHeight);
    }).find(function(e, i, a) {
        var re = new RegExp(pattern);
        return re.test(e.innerHTML);
    });
}
