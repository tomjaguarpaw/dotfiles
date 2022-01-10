DIFF_SO_FANCY=$HOME/diff-so-fancy/diff-so-fancy

if [ -x $DIFF_SO_FANCY ]; then
  git -c diff-so-fancy.markEmptyLines=false \
      -c diff-so-fancy.stripLeadingSymbols=false \
      -c core.pager="$DIFF_SO_FANCY | less --tabs=4 -RFXS" \
      -c interactive.diffFilter="$DIFF_SO_FANCY --patch" \
      -c color.ui=true \
      -c color.diff-highlight.oldNormal="red" \
      -c color.diff-highlight.oldHighlight="red bold" \
      -c color.diff-highlight.newNormal="green" \
      -c color.diff-highlight.newHighlight="green bold" \
      -c color.diff.meta="11" \
      -c color.diff.frag="magenta" \
      -c color.diff.func="146" \
      -c color.diff.commit="yellow" \
      -c color.diff.old="red" \
      -c color.diff.new="green" \
      -c color.diff.whitespace="red reverse" \
      "$@"
else
    git "$@"
fi
