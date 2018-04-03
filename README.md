# Description

A program for showing SVG whiteboard on a website. Similar to [MathPump](https://github.com/amkhlv/mathpump3) but one-way only.
Does not require installation on the receiving end.


# Setup

## Haskell setup

1. [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install GHC: `stack setup`
3. Build libraries: `stack build`

## Copy executable:

    cp .stack-work/dist/x86_64-linux-nopie/Cabal-2.0.1.0/build/svgserver2/svgserver2 /usr/local/bin/

## C++ setup

    mkdir build
    cd build
    qmake -qt=qt4  -makefile ../compute-patch-to/
    make

The resulting executable `compute-patch-to` should be copied somewere; its location should be put in `common.xml` under `<diffprog>`.

# Certificates preparation

    openssl genrsa -out key.pem 2048
    openssl req -new -key key.pem -out certificate.csr

-- will ask questions ^^^

    openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem

# Invocation

## Sample `systemd` unit:

    [Unit]
    Description=SVGServer2 for %i

    [Service]
    WorkingDirectory= /var/www/svgserver2/%i
    ExecStart= /usr/local/bin/svgserver2 -c /var/www/svgserver2/common.xml -i /var/www/svgserver2/%i/instance.xml
    PIDFile=/tmp/svgserver2_%i.pid
    User=www-data

    [Install]
    WantedBy=multi-user.target

## Port forwarding

    ssh -R 11111:localhost:11111  myserver.com


# Configuration files

Sample configuration files:

## common.xml

    <config>
      <proto>https</proto>
      <site>andreimikhailov.com</site>
      <GoogleClientID>■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■</GoogleClientID>
      <GoogleClientSecret>■■■■■■■■■■■■■■■■■■■■■■■■■■■</GoogleClientSecret>
      <diffprog>/usr/local/lib/amkhlv/compute-patch-to</diffprog>
      <cert>/var/www/svgserver2/certificate.pem</cert>
      <key>/var/www/svgserver2/key.pem</key>
    </config>

## instance.xml

    <config>
      <remotePort>11111</remotePort>
      <localPort>11111</localPort>
      <dir>/var/www/svgserver2/Example/svgs</dir>
      <users><user>"a.mkhlv@gmail.com"</user></users>
      <log>/var/www/svgserver2/Example/log.txt</log>
    </config>
