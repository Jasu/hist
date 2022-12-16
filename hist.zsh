# NOTE: This requires an external menu application.
#       See instructions in README.md
#
# ZSTYLE CONFIGURATION:
#   GLOBAL:
#     :hist hist_bin     - Path to the hist binary.
#     :hist ignored_dirs - Dirs not to select from.
#
#   PER WIDGET:
#     :hist:cd max_matches  - Max matches to show in menu.
#     :hist:cd menu         - Will be evaluated with matches piped in.
#     :hist:cd shorten_home - If set, shorten the home dir to ~.
#     :hist:cd markup       - Format the output with Pango markup

autoload -U add-zsh-hook
zmodload zsh/zutil

default_hist_bin="${0:a:h}/build/hist"

_hist_run() {
    local hist_bin
    zstyle -s :hist hist_bin_path hist_bin || hist_bin="$default_hist_bin"
    $hist_bin "$@"
}

_hist_put_pwd() {
    local dir="${PWD}"

    # Skip patterns in :hist ignored_dirs
    local -a ignored_dirs
    zstyle -a :hist ignored_dirs ignored_dirs
    for pat in "${ignored_dirs[@]}"; do
        if eval "[[ '$dir' = $pat ]]"; then
            return 0
        fi
    done
    _hist_run put "$dir"
}

_hist_top() {
    local num
    zstyle -s ":hist:${1:-select}" max_matches num
    _hist_run top ${num:-64}
}

_hist_menu() {
    local context="${1:-menu}"
    local menu markup cmd="_hist_top $context"
    zstyle -s :hist:$context menu menu || return 1
    zstyle -b :hist:$context markup markup
    if [[ -n "$2" ]]; then
        cmd+="|grep -vxF '$PWD'"
    fi

    if [[ $markup = yes ]]; then
        cmd+="|sed -E 's#^$home\$#<span weight=\"bold\" color=\"\\#F0FF77\">~</span>#'"
        cmd+="|sed -E 's#([^/>]+)\$#<span weight=\"bold\" color=\"\\#F0FF77\">\\1</span>#'"
    fi
    if zstyle -T :hist:$context shorten_home; then
        if [[ $markup = yes ]]; then
            cmd+="|sed -E 's#^$HOME/#<span color=\"\\#A0A0E0\">~/</span>#'"
        else
            cmd+="|sed 's#^$HOME#~#'"
        fi
    fi
    cmd+="|$menu"
    eval "$cmd"
}

hist-cd() {
    zle -R "Selecting from history..."
    local result="$(_hist_menu cd t)"
    if [[ -z $result ]]; then
        return 1
    fi
    cd "${result/\~/$HOME}"
    zle -I
    _hist_put_pwd
    # Have to use a private API here, otherwise the directory
    # in the prompt won't change.
    if whence _p9k_fetch_cwd >/dev/null; then
        _p9k_fetch_cwd
        _p9k_set_prompt
    fi
    zle reset-prompt
    zle -R
}
zle -N hist-cd

add-zsh-hook chpwd _hist_put_pwd

# Note: this is hardcoded in the binary too
mkdir -p "$HOME/.local/share/hist"
