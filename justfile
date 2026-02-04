watch-js:
    npx shadow-cljs watch app
watch-css:
    npx @tailwindcss/cli -i src/tailwind.css -o public/resources/css/tailwind.css --watch -v
dev-server:
    DEBUG=1 clojure -M -m rglinks-server.core -p 8280
tmux:
    tmux new-session \; send-keys 'just dev-server' C-m \; \
      split-window -v \; send-keys 'just watch-js' C-m \; \
      split-window -v \; send-keys 'just watch-css' C-m \; \
      select-pane -t 0 \; select-layout even-vertical \; \
      rename-session 'rglinks' \; \
      attach
