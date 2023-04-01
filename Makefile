# Handy Makefile tocleanup and push changes out to website 

default:
	cp FOS705.html index.html
	./copyscripts.sh
	git add --all
	git commit -m "update"
	git push -u origin master

copy:
	cp FOS705.html index.html
