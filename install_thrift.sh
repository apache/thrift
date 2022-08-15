export CXXFLAGS=-Wno-error
if [[ $(uname -m) == 'arm64' ]]; then
  export LDFLAGS="-L/opt/homebrew/opt/openssl/lib"
  export CPPFLAGS="-I/opt/homebrew/opt/openssl/include"
else
  export LDFLAGS="-L/usr/local/opt/openssl/lib"
  export CPPFLAGS="-I/usr/local/opt/openssl/include"
fi
export PKG_CONFIG_PATH="/usr/local/opt/libffi/lib/pkgconfig"
export PATH="/usr/local/opt/bison/bin:$PATH"
export PATH="/usr/local/opt/openssl/bin:$PATH"

rm -rf /usr/local/bin/thrift
./bootstrap.sh
./configure --without-cpp --without-qt4 --without-qt5 --with-c_glib --without-csharp --without-erlang \
    --without-nodejs --without-lua --without-python --without-perl --without-php --without-php_extension \
    --without-ruby --without-haskell --without-go --without-haxe --without-d --without-py3 --without-swift \
    --with-java \
    && make
    
make install
