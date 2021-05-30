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

## Examples

Generate a server certificate:

``` sh
certtool --generate-privkey --outfile server-pkey.pem
certtool --generate-self-signed --load-privkey server-pkey.pem --outfile server-cert.pem
# Answer "yes" to web server certificate
# Enter "localhost" as the DNS name
```

Run the example server:

``` sh
examples/hello-world.scm --cert server-cert.pem --pkey server-pkey.pem
```

Run the example client:

``` sh
examples/gem-fetch.scm localhost
```

The client also works on real-world Gemini URIs:

``` sh
examples/gem-fetch.scm gemini.circumlunar.space
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
