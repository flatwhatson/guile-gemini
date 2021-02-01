# guile-gemini

An implementation of the [Gemini][gemini] protocol for [GNU Guile][guile], using [fibers][] and [gnutls][].

WARNING: This library is alpha quality.  Don't use it for *anything*.

## Examples

Install dependencies:

``` sh
guix install guile guile-fibers gnutls
```

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

[gemini]: https://gemini.circumlunar.space/
[guile]: https://www.gnu.org/software/guile/guile.html
[fibers]: https://github.com/wingo/fibers
[gnutls]: https://gitlab.com/gnutls/gnutls
