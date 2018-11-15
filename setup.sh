#!/bin/sh

# launch emacs without waiting in terminal
emacs_launch() {
    # https://superuser.com/questions/358037/emacsclient-create-a-frame-if-a-frame-does-not-exist
    # https://www.reddit.com/r/emacs/comments/8iq4ho/how_make_emacsclient_create_gui_frame_by_default/
    # https://medium.com/@bobbypriambodo/blazingly-fast-spacemacs-with-persistent-server-92260f2118b7
    # https://gist.github.com/alexmurray/337ac19014d769f4b219
    # https://news.ycombinator.com/item?id=9395056
    # https://emacs.stackexchange.com/questions/13485/something-like-delete-frame-but-that-would-also-delete-the-last-frame-like-al
    # (visible-frame-list) v.s. (frame-list)
    # emacsclient options for reference
    # -a : alternative editior
    # -c : create-frame
    # -n : no-wait, return immediately, returns control back to the terminal
    # -t : terminal
    # -q : quite
    # -e : eval the script
    # -a "" : starts emacs daemon and reattaches
    # NOTE:
    # Emacs daemon always has a visible frame called F1
    # TODO:
    # Should start emacs "*scratch*" buffer or bring most recent frame into focus
    # if no arguments is specified
    emacsclient -n -e "(if (> (length (frame-list)) 1) 't)" | grep t >/dev/null

    new_frame_flag=
    if [ "$?" = "1" ]; then
        # if frame not exists, create new frame
        new_frame_flag=-c
    fi

    wait_flag=
    if [ "$DISPLAY" != "" ]; then
        # dont wait on gui mode, just return immediately
        wait_flag=-n
    fi

    # NOTE:
    # (select-frame-set-input-focus (selected-frame)) ;; raise frame and set input focus
    # (other-frame 0) ;; raise frame and set input focus
    # (raise-frame) ;; raise frame only
    # (switch-to-buffer "*scratch*") ;; switch to scratch buffer
    # (toggle-frame-fullscreen) ;; to fullscreen
    # FIXME:
    # In gnome3 app is not allowed to steal focus,
    # and therefore (raise-window) has no effect,
    # gotta find a way to enable this in this environment.
    # TODO:
    # Make a another version of emacs_launch that doesn't steal user's focus.

    # if no file argument is given,
    # create new frame or open existing frame, and set focus to that frame.
    if [ -z ${@} ]; then
        emacsclient ${new_frame_flag} ${wait_flag} -a "" -e '(select-frame-set-input-focus (selected-frame))'
    else
        emacsclient ${new_frame_flag} ${wait_flag} -a "" "${@}"
    fi
}

emacs_run() {
    # https://superuser.com/questions/358037/emacsclient-create-a-frame-if-a-frame-does-not-exist
    # https://www.reddit.com/r/emacs/comments/8iq4ho/how_make_emacsclient_create_gui_frame_by_default/
    # https://medium.com/@bobbypriambodo/blazingly-fast-spacemacs-with-persistent-server-92260f2118b7
    # https://gist.github.com/alexmurray/337ac19014d769f4b219
    # (visible-frame-list) v.s. (frame-list)
    # emacsclient options for reference
    # -a : alternative editior
    # -c : create-frame
    # -n : no-wait, return immediately, returns control back to the terminal
    # -t : terminal
    # -q : quite
    # -e : eval the script
    # -a "" : starts emacs daemon and reattaches
    # NOTE:
    # Emacs daemon always has a visible frame called F1
    emacsclient -n -e "(if (> (length (frame-list)) 1) 't)" | grep t
    # if frame not exists
    if [ "$?" = "1" ]; then
        emacsclient -c -a "" "${@}"
    else
        emacsclient -a "" "${@}"
    fi
}

# emacsclient terminal
emacs_terminal() {
    emacsclient -t -a "" "${@}"
}

# emacsclient gui
emacs_gui() {
    emacsclient -c -a "" "${@}"
}

# emacsclient gui
emacs_gui_launch() {
    emacsclient -c -n -a "" "${@}"
}

# save and kill
# write and quit
emacs_write_and_quit() {
    emacsclient -e '(save-buffers-kill-emacs)'  "${@}"
}

# quit
emacs_quit() {
    emacsclient -e '(kill-emacs)'  "${@}"
}

# predicate
emacs_predicate() {
    # emacsclient -ca false -e '(delete-frame)'
    emacsclient -q -a false -e t 1>/dev/null 2>/dev/null
}

# start or restart daemon
emacs_server() {
    emacs_predicate || emacs --daemon "${@}"
}

# start emacs daemon in foreground
emacs_daemon() {
    # TODO:
    # Wait on daemon while there is already one.
    # Maybe exit the daemon while this program is terminated.
    emacs_predicate || emacs --fg-daemon "${@}"
}

emacs_restart() {
    emacsclient -e '(restart-emacs)' -a "" "${@}"
}

emacs_vim() {
    vi "${@}"
}

emacs_vi() {
    vi "${@}"
}

alias e=emacs_launch
alias ee=emacs_run
alias et=emacs_terminal
alias eg=emacs_gui
alias es=emacs_server
alias er=emacs_restart
alias eq=emacs_quit
alias ep=emacs_predicate
alias ewq=emacs_write_and_quit
alias evi=emacs_vi
alias evim=emacs_vim
