function () {
    var links = content.document.getElementsByTagName('a');
    links = Array.prototype.slice.call(links);

    return links.filter(function(e, i, a) {
        return (e.getBoundingClientRect().top <
                content.document.activeElement.getBoundingClientRect().top);
    }).reduce(function(p, c, i, a) {
        var yp = p.getBoundingClientRect().top;
        var yc = c.getBoundingClientRect().top;
        return ((yc>yp) ? c : p);
    });
}