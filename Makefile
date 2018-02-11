# Handy Makefile tocleanup and push changes out to MATFOS705 website 

default:
	cp FOS705.html index.html
	git add --all
	git commit -m "update"
	git push -u origin master

copy:
	cp FOS705.html index.html
