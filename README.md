# guile-gemini

An implementation of the [Gemini][gemini] protocol for [GNU Guile][guile],
using [fibers][] and [gnutls][].

## Installation

This project provides a package definition for [GNU Guix][guix], making it
easy to install from a cloned repository:

``` sh
guix package -f path/to/guile-gemini/guix.scm
```

## Development

This project provides a `.envrc` for [direnv][] to simplify setting up a
development environment, using the Guix package definition to automatically
set up dependencies and paths when working in the project directory.

To enable direnv for this project (after installing direnv itself):

``` sh
cd guile-gemini
direnv allow
```

If you would prefer to do things manually, the following will set up an
equivalent environment:

``` sh
cd guile-gemini
guix environment -l guix.scm
export GUILE_LOAD_PATH="$PWD/src:$GUILE_LOAD_PATH"
```

## Documentation

Coming soon!

## Examples

Run the example server:

``` sh
# Generate a self-signed certificate for host "localhost"
openssl req -x509 -newkey rsa:4096 \
 -keyout server-key.pem \
 -out server-cert.pem \
 -nodes -days 365 -subj /CN=localhost

# Run the server, listening on localhost:1965 by default
examples/hello-world.scm --cert server-cert.pem --key server-key.pem
```

Make a request to the local server using the example client:

``` sh
examples/gem-fetch.scm localhost
```

The example client works on real-world Gemini URIs:

``` sh
# Short URIs are supported
examples/gem-fetch.scm gemini.circumlunar.space

# Full URIs are also supported
examples/gem-fetch.scm gemini://gemini.circumlunar.space/
```

The example client supports client certificates:

``` sh
# Generate a self-signed certificate for user "Anonymous"
openssl req -x509 -newkey rsa:4096 \
 -keyout client-key.pem \
 -out client-cert.pem \
 -nodes -days 365 -subj /CN=Anonymous

# Request localhost using these credentials
examples/gem-fetch.scm localhost -c client-cert.pem -k client-key.pem
```

## License

This library is free software: you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

This library is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
details.

You should have received a copy of the GNU Lesser General Public License along
with this program.  If not, see <http://www.gnu.org/licenses/>.

See [COPYING](COPYING) and [COPYING.LESSER](COPYING.LESSER) for details.

[direnv]: https://direnv.net/
[fibers]: https://github.com/wingo/fibers
[gemini]: https://gemini.circumlunar.space/
[gnutls]: https://gitlab.com/gnutls/gnutls
[guile]: https://www.gnu.org/software/guile/guile.html
[guix]: https://guix.gnu.org/
