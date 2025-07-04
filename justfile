watch-js:
    npx shadow-cljs watch app
watch-css:
    npx @tailwindcss/cli -i src/tailwind.css -o public/resources/css/tailwind.css --watch -v
dev-server:
    clojure -M -m shufflefast-server.core -p 8280 -d
tmux:
    tmux new-session \; send-keys 'just dev-server' \; \
      split-window -v \; send-keys 'just watch-js' \; \
      split-window -v \; send-keys 'just watch-css' \; \
      select-pane -t 0 \; select-layout even-vertical \; \
      rename-session 'shufflefast' \; \
      attach
