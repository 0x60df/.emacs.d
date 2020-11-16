
;;;; inst-evil.el


(premise init)
(premise init-el-get)

(el-get-bundle evil
  :build (list (delete "info" (car (plist-get
                                    (el-get-read-recipe 'evil)
                                    :build))))
  :build/berkeley-unix (list (delete "info" (car (plist-get
                                                  (el-get-read-recipe 'evil)
                                                  :build/berkeley-unix))))
  :build/darwin (list (delete "info" (car (plist-get
                                           (el-get-read-recipe 'evil)
                                           :build/darwin))))
  :info nil)


(resolve inst-evil)
