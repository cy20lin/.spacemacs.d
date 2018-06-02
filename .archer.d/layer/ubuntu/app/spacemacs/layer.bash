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
    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d/
    pushd ~/.emacs.d/
    git checkout 0fa3658cd8e283825dcd0a54ce1579dec55eb568
    popd
    # FIXME:
    # How to gracefully kill-emacs and get the spacemacs bootstrap status,
    # rather than running into following infomation, even if spacemacs is
    # successfully bootstraped.
    # Error: server did not start correctly
    yes | emacs --daemon --eval '
    (progn
      (message "[INFO] End of installation.")
      (setq dotspacemacs-enable-server nil)
      (kill-emacs))
    '
    true # workaround for now
}
