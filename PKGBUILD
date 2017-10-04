# -*- shell-script -*-
# Maintainer: Eric Schulte <schulte.eric@gmail.com>
pkgname=libtrace-git
_srcdir=libtrace_pkg
pkgver=XXXXXXXX
pkgrel=1
pkgdesc="Writing, reading, storing, and searching of program traces (source and binary)"
url="https://git.grammatech.com/research/trace-db"
arch=('i686' 'x86_64')
license=('GPL3')
depends=()
makedepends=('git' 'pandoc')
provides=('libtrace')
source=("${_srcdir}::git+https://git.grammatech.com/research/trace-db")
sha256sums=('SKIP')

pkgver() {
  cd "$_srcdir"
  printf "r%s.%s" "$(git rev-list --count HEAD)" "$(git rev-parse --short HEAD)"
}

# prepare() { }

build() {
  cd $_srcdir
  make
}

package() {
  cd "$_srcdir"
  make DESTDIR="$pkgdir/usr/" install
}

# vim: ts=2 sw=2 et:
