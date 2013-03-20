document.addEvent('domready', function() {
	hljs.tabReplace = '    ';
	//hljs.initHighlightingOnLoad();
	var codes = document.getElements('code');
	codes.each(function(el) {
		var lang = el.className;
		var html = el.get('html');
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
				html = html.replace(/(\n\s*)=&gt;\s*([\w-]+)(\n|$)/i, '$1((return $2))');
			}
			var hl = hljs.highlight('lisp', html).value;
			if(lang == 'lisp')
			{
				// convert highlighted " ((return return-val))" back to " => return-val"
				hl = hl.replace(/<span class="list">\(<span class="body"><span class="list">\(<span class="title">return<\/span><span class="body"> *(([\w-]+))\)<\/span><\/span>\)<\/span><\/span>/i, '=&gt; $1');
			}
		}
		else
		{
			var hl = hljs.highlightAuto(html).value;
		}
		el.set('html', hl);
	});
});
