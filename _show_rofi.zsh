#!/bin/zsh

# Wrapper to use Rofi fuzzy searcher in extended DMenu mode.
# Written for usage with hist, among other things.

_rofi_gen_awk() {
    local has_icon default_icon has_color default_color cur_index=2
    zparseopts -E - -icon=has_icon -default-icon:=default_icon -color=has_color -default-color:=default_color

    if (( ! $#has_color && ! $#has_icon )); then
        return
    fi

    if (( $#has_color )); then
        local terminator=''
        if (( ! $#has_icon )); then
            terminator='\n'
        fi

        if (( $#default_color )); then
            echo -E '{ printf("<span color='"'#%s'"'>%s</span>'$terminator'", $2 == "" ? "'${default_color[-1]}'" : $2, $1); }'
        else
            echo -E '{ if ($2 == "") printf("%s'$terminator'", $1); else printf("<span color='"'#%s'"'>%s</span>'$terminator'", $2, $1); }'
        fi
        cur_index=3
    fi

    if (( $#has_icon )); then
        local icon_var='$'$cur_index
        if (( $#default_icon )); then
            echo -E '{ print "\0icon\037" ('$icon_var' == "" ? "'${default_icon[-1]}'" : '$icon_var'); }'
        else
            echo -E '{ if ('$icon_var' == "") printf("\n"); else print "\0icon\037" '$icon_var'; }'
        fi 
    fi
}

_rofi_run_awk () {
    if (( $2 )); then
        echo -E "$1" >&2
    fi
    local cmd=cat
    if [ -n "$1" ]; then
        cmd=(awk -F';' "$1")
    fi
    if (( $3 )); then
        $cmd | tee /dev/stderr
    else
        $cmd
    fi
}

rofi_select () {
    if [ -t 0 ]; then
      # Rofi grabs X11 input while waiting for selectable options come from terminal input.
      # This hangs the machine rather badly, i.e. SysBrk, reboot, or something similar is required
      echo -E "Input to rofi_select cannot be from terminal." >&2
      return 1
    fi

    local -a dump_awk dump_processed separate_label markup rofi_prompt
    zparseopts -D -E - -dump-awk=dump_awk -dump-processed=dump_processed -separate-label=separate_label -markup=markup -prompt:=rofi_prompt

    local awk_script=$(_rofi_gen_awk "$@")
    local -a rofi_params=(-dmenu -i)
    if [ -n "$awk_script" ] || (( $#markup )); then
        rofi_params+=(-markup-rows)
    fi
    if (( $#rofi_prompt )); then
        rofi_params+=(-p "${rofi_prompt[-1]}")
    fi
    if (( $#separate_label )); then
        # Note that the input might need to be stored somewhere. If STDIN would just be teed,
        # the stream for results would block the stream for rofi (which needs to read the whole stream).
        local input=$(cat)
        local nums=$(echo -E "$input" \
                        | cut -d';' -f2- \
                        | _rofi_run_awk "$awk_script" $#dump_awk $#dump_processed \
                        | rofi $rofi_params -format d | paste -s -d,)
        if [ -z "$nums" ]; then
            return 1
        fi
        echo -E "$input" | cut -d$'\n' "-f$nums" | cut -d';' -f1
    else 
        _rofi_run_awk "$awk_script" $#dump_awk $#dump_processed | rofi $rofi_params -format p
    fi
}

# Allow running as a command, mostly for testing.
if [[ $ZSH_EVAL_CONTEXT == 'toplevel' ]]; then
    rofi_select "$@"
fi
