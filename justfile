readme:
  cargo readme > README.md
  sed -i 's/\[\(`[a-zA-Z0-9:]*`\)]/\1/g' README.md
