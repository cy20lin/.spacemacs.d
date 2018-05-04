layer_help() {
    echo "lang/c-c++"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        app/spacemacs
        lang/c-c++
        lang/javascript
        lang/python
    )
}

layer_is_installed() {
    true
}

layer_install() {
    true
}
