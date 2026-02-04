(ns rglinks-server.config)

(def middleware-diff-logging false)

(def debug? (some? (System/getenv "DEBUG")))
