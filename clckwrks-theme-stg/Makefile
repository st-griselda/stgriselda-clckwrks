LESSC=./node_modules/less/bin/lessc

data/css/clckwrks-theme.css: clckwrks-theme.less Makefile
	${LESSC} clckwrks-theme.less  data/css/clckwrks-theme.css
	${LESSC} --compress clckwrks-theme.less data/css/clckwrks-theme.min.css
