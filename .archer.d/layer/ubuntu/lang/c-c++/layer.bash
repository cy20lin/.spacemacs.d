layer_help() {
    echo "lang/c-c++"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        app/gcc
        app/clang
        app/llvm
        app/cmake
        app/global
        # app/irony-server
        # app/rtags
        # app/cquery
    )
}

layer_is_installed() {
    true
}

layer_install() {
    true
}
