tmpInsPath = /tmp/dnsmonitor
targetInsPath = /usr/bin
executable = dnsmonitor
tmpInsExe = $(tmpInsPath)/$(executable)
targetInsExe = $(targetInsPath)/$(executable)

all:
	stack build

install:
	rm -rf $(tmpInsPath)
	mkdir $(tmpInsPath)
	stack install --local-bin-path $(tmpInsPath)
	sudo -- sh -c 'mv $(tmpInsExe) $(targetInsExe); chown root:root $(targetInsExe); chmod 4755 $(targetInsExe)'

clean:
	stack clean

uninstall:
	sudo rm $(targetInsExe)
