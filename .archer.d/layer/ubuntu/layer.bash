layer_metadata() {
    LAYER_DEPENDENCIES=(
        base
        lang/c-c++
        lang/javascript
        lang/python
        font
        app/spacemacs
        app/kpie
        /generic/script/setup
    )
}

layer_is_installed() {
    true
}

layer_install() {
    true
}
