layer_help() {
    echo "lang/c-c++"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        app/gcc
        app/clang
        app/cmake
        # app/global
        # app/irony-server
        app/cquery
        app/make
        app/autotools
        app/ninja
        # app/rtags
    )
}

layer_is_installed() {
    true
}

layer_install() {
    true
}
