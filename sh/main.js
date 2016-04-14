function path()
{
	var args = arguments,
		result = []
		;
		
	for(var i = 0; i < args.length; i++)
		result.push(args[i].replace('@', 'js/sh/brushs/'));
	
	return result
};

SyntaxHighlighter.autoloader.apply(null, path(
	'applescript			@shBrushAppleScript.js',
	'actionscript3 as3		@shBrushAS3.js',
	'bash shell				@shBrushBash.js',
	'coldfusion cf			@shBrushColdFusion.js',
	'cpp c					@shBrushCpp.js',
	'c# c-sharp csharp		@shBrushCSharp.js',
	'css					@shBrushCss.js',
	'delphi pascal			@shBrushDelphi.js',
	'diff patch pas			@shBrushDiff.js',
	'erl erlang				@shBrushErlang.js',
	'groovy					@shBrushGroovy.js',
	'java					@shBrushJava.js',
	'jfx javafx				@shBrushJavaFX.js',
	'js jscript javascript	@shBrushJScript.js',
	'perl pl				@shBrushPerl.js',
	'php					@shBrushPhp.js',
	'text plain				@shBrushPlain.js',
	'py python				@shBrushPython.js',
	'powershell ps posh		@shBrushPowerShell.js',
	'ruby rails ror rb		@shBrushRuby.js',
	'sass scss				@shBrushSass.js',
	'scala					@shBrushScala.js',
	'sql					@shBrushSql.js',
	'vb vbnet				@shBrushVb.js',
	'xml xhtml xslt html	@shBrushXml.js'
));
SyntaxHighlighter.all();

$(document).ready(function()
{
	$('.autoinclude').each(function()
	{
		var $this = $(this),
			path = $this.find('a:first').attr('href'),
			brushes = { js: 'JScript', css: 'CSS' }
			;
		
		$.ajax({
			url: path,
			type: 'GET',
			dataType: 'text',
			success: function(code)
			{
				var ext = path.match(/\w+$/)[0],
					name = brushes[ext],
					brush = new SyntaxHighlighter.brushes[name]()
					;
				brush.init({ toolbar: false });
				$this.append(brush.getHtml(code));
			}
		});
	});
});