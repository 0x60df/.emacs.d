function (target, foreground, background) {
    if (target){
        var evt = content.document.createEvent('HTMLEvents');
        evt.initEvent('blur', false, false);
        content.document.activeElement.dispatchEvent(evt);

        target.focus();

        var fg = target.style.color;
        fg = (fg ? fg : '');
        target.addEventListener('blur', (function (e) {
            var t = e.currentTarget;
            t.style.color = fg;
            t.removeEventListener(e.type, arguments.callee);
        }));
        target.style.color = foreground;

        var bg = target.style.backgroundColor;
        bg = (bg ? bg : '');
        target.addEventListener('blur', (function (e) {
            var t = e.currentTarget;
            t.style.backgroundColor = bg;
            t.removeEventListener(e.type, arguments.callee);
        }));
        target.style.backgroundColor = background;
    }
}
