layer_help() {
    echo "/msys2"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        lang/c-c++
        lang/javascript
        lang/python
        lang/restructuredtext
        app/fakecygpty
        app/spacemacs
        /generic/script/setup
    )
}

layer_is_installed() {
    true
}

layer_install() {
    true
}
