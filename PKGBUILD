# -*- shell-script -*-
# Maintainer: Eric Schulte <schulte.eric@gmail.com>
pkgname=trace-db-git
_srcdir=trace-db_pkg
pkgver=r51.81a06d3
pkgrel=1
pkgdesc="Writing, reading, storing, and searching of program traces (source and binary)"
url="https://git.grammatech.com/research/trace-db"
arch=('i686' 'x86_64')
license=('GPL3')
depends=()
makedepends=('git' 'pandoc')
provides=('trace-db')
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
