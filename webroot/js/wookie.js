var modal = null;

var wookie = {
	init: function()
	{
		wookie.init_img_hover();
		HighlightLisp.highlight_auto();
		modal = new modal_interface({
			attach_links: true,
			overlay: true
		});
	},

	manual_highlight: function()
	{
		var codes = document.getElements('code');
		codes.each(function(el) {
			var lang = el.className;
			if(lang)
			{
				// NOTE: highlight.js chokes on the "  => return-val" blocks i like
				// to put in my function definitions. this transforms the return-val
				// block from "=> return-val" to "((return return-val))" (which hl.js
				// thinks is lisp) then AFTERthe highlighting is done, transforms it
				// back.
				if(lang == 'lisp')
				{
					// convert " => return-val" to " ((return return-val))"
					var html = el.get('html');
					html = html.replace(/(\n\s*)=&gt;\s*([\w-]+)(\n|$)/i, '$1((return $2))');
					el.set('html', html);
				}
				hljs.highlightBlock(el);
				if(lang == 'lisp')
				{
					// convert highlighted " ((return return-val))" back to " => return-val"
					var html = el.get('html');
					console.log('hl: ', html);
					html = html.replace(/<span class="list">\(<span class="body"><span class="list">\(<span class="title">return<\/span><span class="body"> *([<> "\w\/=-]+)\)<\/span><\/span>\)<\/span><\/span>/i, '=&gt; $1');
					el.set('html', html);
				}
			}
			else
			{
				hljs.highlightBlock(el);
			}
		});
	},

	init_img_hover: function()
	{
		var wimg = $(document).getElement('#wookie a');
		if(!wimg) return;
		var fx = new Fx.Tween(wimg, {duration: 1000, transition: 'bounce:out', property: 'right'});
		wimg.addEvent('mouseenter', function(e) {
			fx.stop()
			fx.start(0);
		});
		wimg.addEvent('mouseleave', function(e) {
			fx.stop()
			fx.start(-120);
		});
	}
};

document.addEvent('domready', function() {
	wookie.init();

	//hljs.tabReplace = '    ';
	//hljs.initHighlightingOnLoad();
	//SyntaxHighlighter.all();
	// prefer manual highlighting so we can run some aftermarket mods >=]
	//wookie.manual_highlight();
});
