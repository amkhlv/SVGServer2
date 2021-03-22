# SVGServer2

## Description

A program for showing SVG whiteboard on a website. Similar to [MathPump](https://github.com/amkhlv/mathpump3) but one-way only.
Does not require installation on the receiving end.


## Setup

### Haskell setup

1. [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install GHC: `stack setup`
3. Build libraries: `stack build`

### Copy executable:

    stack install

### C++ setup

First we need to get diff-match-patch submodule:

    git submodule init
    git submodule update

First install build tools. For example, on Debian, as root:

    aptitude install qt4-qmake libqt4-dev

then build:

    mkdir build
    cd build
    qmake -qt=qt4  -makefile ../compute-patch-to/
    make

The resulting executable `compute-patch-to` should be copied somewere; 
its location should be put in `common.xml` under `<diffprog>`
(see [Configuration files](#configuration-files))

## Certificates preparation

    openssl genrsa -out key.pem 2048
    openssl req -new -key key.pem -out certificate.csr

-- will ask questions ^^^

    openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem

## Invocation

### Sample `systemd` unit

    [Unit]
    Description=SVGServer2 for %i
    
    [Service]
    WorkingDirectory= /home/www-data/svgserver2/%i
    ExecStart=/usr/local/bin/svgserver2 -c /home/www-data/svgserver2/common.xml -i /home/www-data/svgserver2/%i/instance.xml
    PIDFile=/tmp/svgserver2_%i.pid
    User=www-data
    
    [Install]
    WantedBy=multi-user.target

### Port forwarding

    ssh -R 11111:localhost:11111  myserver.com

--- dont forget `GatewayPorts yes` in `/etc/ssh/sshd_config` !

### Nginx

    location /talk/ws {
            rewrite /talk/ws /  break;
            proxy_pass https://localhost:11111;
            proxy_http_version 1.1;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "upgrade";
        proxy_read_timeout 3600s;
        }
    # serve assets or request page from proxy (if asset not found)
    location /talk {
            rewrite /talk(.*) $1  break;
            proxy_pass https://localhost:11111;
    }

## Configuration files

Sample configuration files:

### common.xml

    <config>
      <proto>https</proto>
      <site>andreimikhailov.com</site>
      <GoogleClientID>■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■</GoogleClientID>
      <GoogleClientSecret>■■■■■■■■■■■■■■■■■■■■■■■■■■■</GoogleClientSecret>
      <diffprog>/usr/local/lib/amkhlv/compute-patch-to</diffprog>
      <cert>/home/www-data/svgserver2/certificate.pem</cert>
      <key>/home/www-data/svgserver2/key.pem</key>
    </config>

### instance.xml

    <config>
      <remotePort>443</remotePort>
      <urlPath>/lecture</urlPath>
      <localPort>11111</localPort>
      <dir>/home/www-data/svgserver2/Example/svgs</dir>
      <users><user>a.mkhlv@gmail.com</user></users>
      <log>/home/www-data/svgserver2/Example/log.txt</log>
    </config>

When an element `<isPublic></isPublic>` is present in `instance.xml`, the access restriction is disabled (program does not ask to login).

## Audio channel

One way audio streaming can, in principle, be done
[by VLC](https://github.com/amkhlv/usr/blob/master/share/notes/audio-video/VLC.md)
