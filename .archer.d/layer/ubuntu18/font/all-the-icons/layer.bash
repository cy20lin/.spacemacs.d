layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    FONT_HOME="${HOME}/.local/share/fonts"
    find "$FONT_HOME" -iname '*.ttf' | grep 'all-the-icons'
}

layer__download_font() {
    mkdir -p "${HOME}/.local/share/fonts"
    pushd "${HOME}/.local/share/fonts/" 1>/dev/null
    wget  -O "${1}" "https://github.com/domtronn/all-the-icons.el/blob/master/fonts/${1}?raw=true"
    popd 1>/dev/null
}

layer_install() {
    for font in \
        "material-design-icons.ttf" \
        "weathericons.ttf" \
        "octicons.ttf" \
        "fontawesome.ttf" \
        "file-icons.ttf" \
        "all-the-icons.ttf"
    do
        layer__download_font "${font}"
    done
    fc-cache -f -v "$FONT_HOME/adobe-fonts/source-code-pro"
}
