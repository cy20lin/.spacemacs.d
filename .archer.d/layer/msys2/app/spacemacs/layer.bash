layer_help() {
    echo "app/spacemacs"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        app/git
        app/emacs
    )
}

layer_is_installed() {
    test -f ~/.emacs.d/spacemacs.mk
}

layer_install() {
    if test ! -e ~/.emacs.d/
    then
        git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d/
    fi
    pushd ~/.emacs.d/
    git checkout 0fa3658cd8e283825dcd0a54ce1579dec55eb568
    popd
}
