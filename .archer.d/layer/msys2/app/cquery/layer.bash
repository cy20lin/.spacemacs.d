layer_help() {
    echo "app/cquery"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v cquery
}

layer_install() {
    false
}
