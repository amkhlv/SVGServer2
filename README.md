# Description

A program for showing SVG whiteboard on a website. Similar to [MathPump](https://github.com/amkhlv/mathpump3) but one-way only.
Does not require installation on the receiving end.


# Haskell Setup

1. [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install GHC: `stack setup`
3. Build libraries: `stack build`

# Certificates preparation

    openssl genrsa -out key.pem 2048
    openssl req -new -key key.pem -out certificate.csr

-- will ask questions ^^^

    openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem
