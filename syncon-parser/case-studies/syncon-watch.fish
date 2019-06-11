function syncon-watch
    ls $argv | entr -c -r -s "syncon-parser "(string join " " $argv)" --html=out.html --two-level; and reload-browser Chrome"
end
