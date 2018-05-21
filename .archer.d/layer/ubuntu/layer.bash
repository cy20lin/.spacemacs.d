layer_help() {
    echo "lang/c-c++"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        base
        app/spacemacs
        lang/c-c++
        lang/javascript
        lang/python
        font
    )
}

layer_is_installed() {
    true
}

layer_install() {
    true
}
