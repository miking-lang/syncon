function syncon-watch
    ls $argv | entr -c -r -s "syncon-parser "(string join " " $argv)" --html=out.html --two-level; and reload-browser Chrome"
end
function syncon-watch-dev
    ls $argv | entr -c -r -s "stack build; and stack exec syncon-parser -- "(string join " " $argv)' --html=out.html --two-level; echo $status'
end
