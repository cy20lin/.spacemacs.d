layer_help() {
    echo "app/cquery"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v cquery >/dev/null
}

layer_install() {
    git clone https://github.com/cquery-project/cquery /tmp/archer/source/cquery
    mkdir -p /tmp/archer/build
    cp /tmp/archer/source/cquery /tmp/archer/build -R
    pushd /tmp/archer/build/cquery
    git checkout v20180302
    git submodule update --init
    CXXFLAGS=-g \
            /usr/bin/python ./waf configure build install \
            --variant=system \
            --variant=release \
            --prefix=/mingw64/local \
            --llvm-config=llvm-config
    popd
}
