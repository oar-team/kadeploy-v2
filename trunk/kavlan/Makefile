install :
	mkdir /etc/kavlan
	cp -r *.conf /etc/kavlan
	cp man/man1/kavlan.1 /usr/share/man/man1/
	mkdir /usr/local/bin/kavlan
	cp src/* /usr/local/bin/kavlan
	ln -s /usr/local/bin/kavlan/kavlan.pl /usr/bin/kavlan

uninstall :
	rm -r /etc/kavlan
	rm /usr/share/man/man1/kavlan.1
	rm -r /usr/local/bin/kavlan	
	rm /usr/bin/kavlan

