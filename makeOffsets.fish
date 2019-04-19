#!/usr/bin/fish

for f in index.*; grep -boP '^[^ ].*$' "$f" | sed -E 's/^(.+):.*/\1/' > "indexOffsets."(echo "$f" | grep -oP '\.\K.+$'); end
for f in indexOffsets.*; wc -l < "$f" | cat - "$f" > "$f.tmp"; mv "$f.tmp" "$f"; end

