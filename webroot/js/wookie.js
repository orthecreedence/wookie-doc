var wookie = {
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
	}
};

document.addEvent('domready', function() {
	// prefer manual highlighting so we can run some aftermarket mods >=]

	HighlightLisp.highlight_auto();
	/*
	document.getElements('pre code.lisp').each(function(el) {
		HighlightLisp.highlight_element(el);
	});
	*/

	//SyntaxHighlighter.all();

	//wookie.manual_highlight();
	//hljs.tabReplace = '    ';
	//hljs.initHighlightingOnLoad();
});
