function () {
    var links = content.document.getElementsByTagName('a');
    links = Array.prototype.slice.call(links);

    return links.reduce(function(p, c, i, a) {
        var yp = p.getBoundingClientRect().top;
        var yc = c.getBoundingClientRect().top;

        var ym = content.window.innerHeight / 2;

        var lp = Math.abs(yp-ym);
        var lc = Math.abs(yc-ym);
        return ((lc<lp) ? c : p);
    });
}
