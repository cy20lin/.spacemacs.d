layer_help() {
    echo "app/cquery"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        lang/python
        app/gcc
        app/clang
        app/llvm
        app/cmake
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
    # git checkout v20180302
    git checkout 94a1a5c9d1911e919da3f5ab085fa3b938854505 # Fix bad memory usage by partially reverting
    git submodule update --init
    mkdir build
    cmake -Bbuild -H . -DCMAKE_INSTALL_PREFIX=/usr/local -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
    cmake --build build
    sudo cmake --build build --target install
    popd
}
