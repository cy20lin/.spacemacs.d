layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    FONT_HOME="${HOME}/.local/share/fonts"
    find "$FONT_HOME" -iname '*.ttf' | grep 'SourceCodePro'
}

layer_install() {
    # Userland mode (~$USER/), (~/).

    # modified and taken from
    # https://gist.github.com/enzinier/8d00d3f37d2e23985dcfa65662d163fa
    # Author: Jason Park (enzinier)

    # ~/.fonts is now deprecated and that
    #FONT_HOME="${HOME}/.fonts"
    # ~/.local/share/fonts should be used instead
    FONT_HOME="${HOME}/.local/share/fonts"

    echo "installing fonts at $PWD to $FONT_HOME"
    mkdir -p "${FONT_HOME}/adobe-fonts/source-code-pro"
    # find "$FONT_HOME" -iname '*.ttf' -exec echo '{}' \;

    test -d "${FONT_HOME}/adobe-fonts/source-code-pro" && rm -rf "${FONT_HOME}/adobe-fonts/source-code-pro"

    git clone \
         --branch release \
         --depth 1 \
         'https://github.com/adobe-fonts/source-code-pro.git' \
         "${FONT_HOME}/adobe-fonts/source-code-pro" \
    && fc-cache -f -v "$FONT_HOME/adobe-fonts/source-code-pro"
}
