layer_metadata() {
    LAYER_DEPENDENCIES=(
        base
        lang/c-c++
        lang/javascript
        lang/python
        font
        app/git
        app/spacemacs
        app/kpie
        app/xephyr
        app/sommelier
        /generic/script/setup
    )
}

layer_is_installed() {
    true
}

layer_install() {
    true
}
