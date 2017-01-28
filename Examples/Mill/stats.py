import os

def count(path, string):
	cmd = 'grep -c -E -r --include "*.scala" "%s" "%s"' % (string, path)
	grep = os.popen(cmd).read().splitlines()
	return sum(int(r.split(':')[-1]) for r in grep)

	
Events = 'src/millgame/versions/events'
Signals = 'src/millgame/versions/signals'
	
Any = '//#'
HDL = '//#HDL'
SIG = '//#SIG'
VAR = '//#VAR'
EVT = '//#EVT'
IF = '//#IF'

for (name, path) in [('Events', Events), ('Signals', Signals)]:
	print name + ":"
	for t in [EVT, HDL, SIG, VAR, IF]:
		print t, count(path, t)