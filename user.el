(setq ale-debug-p nil)

(setq ale-files-dir-alist
      '(((title . "  Shows")        (path . "/mnt/HDD/Share"))
        ((title . "  Coding")       (path . "/mnt/HDD/Dev"))
        ((title . "  Books")        (path . "/mnt/HDD/Book"))
        ((title . "輸  Videos")       (path . "/mnt/HDD/Video"))
        ((title . "  Movies")       (path . "/mnt/Cloud/Movies"))
        ((title . "  Notes")        (path . "~/Documents/notes"))
        ((title . "  Photos")       (path . "~/Pictures"))
        ((title . "  Downloads")    (path . "~/Downloads"))))

(setq! ale-dired-routes
       '(("o" "Home"        "~")
         ("d" "Dotfiles"    "/opt/dotfiles")
         ("u" "Emacs cache" "~/.cache/emacs")
         ("p" "Code"        "~/Code")
         ("n" "Downloads"   "~/Downloads")
         ("w" "Wallpaper"   "~/Pictures/wallpaper")
         ("m" "Drives"      "/mnt")
         ("t" "Trash"       "~/.local/share/Trash")))
