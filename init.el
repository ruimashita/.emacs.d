;; .emacs.d を再帰的に読み込み
(let ((default-directory "~/.emacs.d"))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))

;;(setq load-path (cons (expand-file-name "~/.emacs.d") load-path))
;; デバッグ
(setq debug-on-error nil)

;; PATH の設定 http://sakito.jp/emacs/emacsshell.html#path
;; より下に記述した物が PATH の先頭に追加されます
(dolist (dir (list
              "/sbin"
              "/usr/sbin"
              "/bin"
              "/usr/bin"
              "/opt/local/bin"
              "/usr/local/bin"
              (expand-file-name "~/bin")
              (expand-file-name "~/.emacs.d/bin")
              ))
  ;; PATH と exec-path に同じ物を追加します
  (when (and (file-exists-p dir) (not (member dir exec-path)))
	(setenv "PATH" (concat dir ":" (getenv "PATH")))
	(setq exec-path (append (list dir) exec-path))))


;; for m-x shell-command
(setq shell-file-name "/bin/zsh")
;; for m-x shell
(setq explicit-shell-file-name "/bin/zsh")


;; ==============================================
;; Misc
;; ===============================================
;; スタートアップページを表示しない
(setq inhibit-startup-message t)

;; バックアップファイルを作らない
(setq make-backup-files nil)

;; メニューバーを表示しない
(setq menu-bar-mode nil)

;; フレームのタイトル
(setq frame-title-format
      `( " %b " (buffer-file-name "( %f )") " on ",(system-name)
         )
      )

;; カーソルのある行番号を表示
(setq line-number-mode t)

;; 日本語infoの文字化け防止
(auto-compression-mode t)

;; スクロールバーを右に表示
(set-scroll-bar-mode 'right)

;;buffer-menuのキーバインドを設定
(global-set-key "\C-x\C-b" 'buffer-menu)

;;ピープ音を消す
;;(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; 対応する括弧を光らせる。
(show-paren-mode 1)

;; ウィンドウ内に収まらないときだけ括弧内も光らせる。
(setq show-paren-style 'mixed)

;; バッファ自動再読み込み
(global-auto-revert-mode 1)

;; 同名ファイルのバッファ名の識別文字列を変更する
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)


;; wdired.el
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; grep-edit
(require 'grep-edit)


;; 言語・文字コード関連の設定
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq file-name-coding-system 'utf-8)


;; =======================================================================
;; 全角 TAB に色をつける
;; =======================================================================
(defface my-face-b-1 '((t (:background "gray40"))) nil)
(defface my-face-b-2 '((t (:background "gray15"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)

(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("\t" 0 my-face-b-2 append)
     ("　" 0 my-face-b-1 append)
     ("[ \t]+$" 0 my-face-u-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)


;; =======================================================================
;;  OSX or Ubunt
;; =======================================================================

;; Check if system is Darwin/Mac OS X
(defun system-type-is-darwin ()
  (interactive)
  "Return true if system is darwin-based (Mac OS X)"
  (string-equal system-type "darwin")
  )

;; Check if system is GNU/Linux
(defun system-type-is-gnu ()
  (interactive)
  "Return true if system is GNU/Linux-based"
  (string-equal system-type "gnu/linux")
  )

;;=======================================================================
;;  if Mac OS X
;;=======================================================================
(if (system-type-is-darwin)
    (progn
      ;; Command-Key and Option-Key
      (setq ns-command-modifier (quote meta))
      (setq ns-alternate-modifier (quote super))

      ;; 円期号をバックスラッシュに変更
      (define-key global-map [165] nil)
      (define-key global-map [67109029] nil)
      (define-key global-map [134217893] nil)
      (define-key global-map [201326757] nil)
      (define-key function-key-map [165] [?\\])
      (define-key function-key-map [67109029] [?\C-\\])
      (define-key function-key-map [134217893] [?\M-\\])
      (define-key function-key-map [201326757] [?\C-\M-\\])
      )
  )

;;=======================================================================
;;  if Ubuntu
;;=======================================================================
(if (system-type-is-gnu)
    (progn
      ;; ホイールマウス
      (mouse-wheel-mode t)
      (setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control) . nil)))
      (setq mouse-wheel-progressive-speed nil)

      ;; gnome clipboard
      (cond (window-system
             (setq x-select-enable-clipboard t)
             ))

      ;; ibus
      ;; Ref: http://www11.atwiki.jp/s-irie/pages/21.html, http://d.hatena.ne.jp/iRiE/20100530/1275212234
      ;;
      (require 'ibus)
      (add-hook 'after-init-hook 'ibus-mode-on)
      ;; Toggle input status by alt + SPC
      (global-set-key "\M- " 'ibus-toggle)
      ;; すべてのバッファで入力状態を共有
      (setq ibus-mode-local nil)
      ;; Busがオンの時のカーソル色
      (setq ibus-cursor-color "aquamarine")
      ;; C-SPC は Set Mark , C-/ は Undo に使う. C-zも消す
      (ibus-define-common-key [?\C-\  ?\C-/ ?\C-z]  nil)
      ;; シフト + 矢印で範囲選択
      (setq pc-select-selection-keys-only t)
      (pc-selection-mode 1)


      )
  )


;;=====================================================
;; ウィンドウ設定
;;=====================================================

;;(if window-system (progn
;;   (setq initial-frame-alist '((top .5)(left . 10) (width . 260) (height . 40)))
;;))

;;フレームサイズを記憶する
(defun my-window-size-save ()
  (let* ((rlist (frame-parameters (selected-frame)))
         (ilist initial-frame-alist)
         (nCHeight (frame-height))
         (nCWidth (frame-width))
         (tMargin (if (integerp (cdr (assoc 'top rlist)))
                      (cdr (assoc 'top rlist)) 0))
         (lMargin (if (integerp (cdr (assoc 'left rlist)))
                      (cdr (assoc 'left rlist)) 0))
         buf
         (file "~/.emacs.d/.framesize.el"))
    (if (get-file-buffer (expand-file-name file))
        (setq buf (get-file-buffer (expand-file-name file)))
      (setq buf (find-file-noselect file)))
    (set-buffer buf)
    (erase-buffer)
    (insert (concat
             ;; 初期値をいじるよりも modify-frame-parameters
             ;; で変えるだけの方がいい?
             "(delete 'width initial-frame-alist)\n"
             "(delete 'height initial-frame-alist)\n"
             "(delete 'top initial-frame-alist)\n"
             "(delete 'left initial-frame-alist)\n"
             "(setq initial-frame-alist (append (list\n"
             "'(width . " (int-to-string nCWidth) ")\n"
             "'(height . " (int-to-string nCHeight) ")\n"
             "'(top . " (int-to-string tMargin) ")\n"
             "'(left . " (int-to-string lMargin) "))\n"
             "initial-frame-alist))\n"
             ;;"(setq default-frame-alist initial-frame-alist)"
             ))
    (save-buffer)
    ))
(defun my-window-size-load ()
  (let* ((file "~/.emacs.d/.framesize.el"))
    (if (file-exists-p file)
        (load file))))
(my-window-size-load)
;; Call the function above at C-x C-c.
(defadvice save-buffers-kill-emacs
  (before save-frame-size activate)
  (my-window-size-save))

;; ウィンドウを透明化
(add-to-list 'default-frame-alist '(alpha . (0.95 0.95)))

;; popwin
(require 'popwin) 
(setq display-buffer-function 'popwin:display-buffer)
(setq anything-samewindow nil) 
(push '("*anything*" :height 30) popwin:special-display-config)
(push '("*grep*" :noselect nil) popwin:special-display-config)

;; other-window
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-z") 'other-window-or-split)


;;=======================================================================
;; elscreen
;;=====================================================================
(setq elscreen-prefix-key "\C-t") 
(require 'elscreen)
(global-set-key "\C-t" 'elscreen-clone)
;;(global-set-key "\C-zk" 'elscreen-kill)
(global-set-key [(C-tab)] 'elscreen-next)
(global-set-key [(C-S-iso-lefttab)] 'elscreen-previous)


;;=======================================================================
;; tramp
;;=====================================================================
(require 'tramp)
;;(setq tramp-default-method "ssh")
(add-to-list 'tramp-default-proxies-alist
             '(".*" "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '("localhost" "\\`root\\'" nil))
(add-to-list 'tramp-default-proxies-alist
             '((regexp-quote (system-name)) "\\`root\\'" nil))


;;=======================================================================
;; multi-term
;;=====================================================================
(require 'multi-term)
(setq multi-term-program "/usr/bin/zsh")
(global-set-key (kbd "C-c t") '(lambda ()
								 (interactive)
								 (multi-term)))


;;=======================================================================
;; forward, backward
;;=====================================================================
;; http://d.hatena.ne.jp/khiker/20090604/forward_word
(defun my-forward-word (arg)
  (interactive "p")
  (let ((char-category
         '(lambda (ch)
            (when ch
              (let* ((c (char-category-set ch))
                     ct)
                (cond
                 ((aref c ?a)
                  (cond
                   ((or (and (>= ?z ch) (>= ch ?a))
                        (and (>= ?Z ch) (>= ch ?A))
                        (and (>= ?9 ch) (>= ch ?0))
                        (= ch ?-) (= ch ?_))
                    'alphnum)
                   (t
                    'ex-alphnum)))
                 ((aref c ?j) ; Japanese
                  (cond
                   ((aref c ?K) 'katakana)
                   ((aref c ?A) '2alphnum)
                   ((aref c ?H) 'hiragana)
                   ((aref c ?C) 'kanji)
                   (t 'ja)))
                 ((aref c ?k) 'hankaku-kana)
                 ((aref c ?r) 'j-roman)
                 (t 'etc))))))
        (direction 'char-after)
        char type)
    (when (null arg) (setq arg 1))
    (when (> 0 arg)
      (setq arg (- arg))
      (setq direction 'char-before))
    (while (> arg 0)
      (setq char (funcall direction))
      (setq type (funcall char-category char))
      (while (and (prog1 (not (eq (point) (point-max)))
                    (cond ((eq direction 'char-after)
                           (goto-char (1+ (point))))
                          (t
                           (goto-char (1- (point))))))
                  (eq type (funcall char-category (funcall direction)))))
      (setq arg (1- arg)))
    type))
(defun my-backward-word (arg)
  (interactive "p")
  (my-forward-word (- (or arg 1))))

;; 素のforward-word, backward-wordを潰す
(global-set-key "\M-f" 'my-forward-word)
(global-set-key "\M-b" 'my-backward-word)


;;=======================================================================
;; ido-mode
;;=====================================================================
(require 'ido)
(ido-mode t)


;;=======================================================================
;; session
;;=====================================================================
;; kill-ringやミニバッファで過去に開いたファイルなどの履歴を保存する
(when (require 'session nil t)
  (setq session-initialize '(de-saveplace session keys menus places)
        session-globals-include '((kill-ring 50)
                                  (session-file-alist 500 t)
                                  (file-name-history 10000)))
  ;; これがないと file-name-history に500個保存する前に max-string に達する
  (setq session-globals-max-string 100000000)
  ;; デフォルトでは30!
  (setq history-length t)
  (add-hook 'after-init-hook 'session-initialize)
  ;; 前回閉じたときの位置にカーソルを復帰
  (setq session-undo-check -1))


;;==============================================
;; indent
;;===============================================

;; (setq-default tab-width 4)
(setq default-tab-width 4)
(setq tab-width 4)
;; (setq-default indent-tabs-mode t)
(setq indent-tabs-mode t)
;; (setq c-tab-always-indent t)
(setq c-basic-offset 4)
;; (setq indent-line-function 'indent-relative-maybe) ;; 前と同じ行の幅にインデント


;;=======================================================================
;; auto-complete
;;=====================================================================
;;(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")
(ac-config-default)

;; (global-auto-complete-mode t)
(ac-set-trigger-key "TAB")

;; 大文字小文字を区別しない
(setq ac-ignore-case t)
(put 'downcase-region 'disabled nil)



;;=======================================================================
;; yasnippet
;;=====================================================================
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippets-rails/rails-snippets")
(yas/load-directory "~/.emacs.d/yasnippet-0.6.1c/snippets/text-mode")




;;=========================
;; css-mode
;;=================================
(autoload 'css-mode "css-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))


;;=================================================
;; js2-mode
;;=====================================================
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

                                        ; fixing indentation
                                        ; refer to http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
(autoload 'espresso-mode "espresso")

(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      (save-excursion

        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))

        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun my-indent-sexp ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point)))
             (ovl (make-overlay beg end)))
        (set-marker end-marker end)
        (overlay-put ovl 'face 'highlight)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          ;; don't reindent blank lines so we don't set the "buffer
          ;; modified" property for nothing
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))
        (run-with-timer 0.5 nil '(lambda(ovl)
                                   (delete-overlay ovl)) ovl)))))

(defun my-js2-mode-hook ()
  (require 'espresso)
  (setq espresso-indent-level 4
        indent-tabs-mode nil
		js2-mirror-mode nil ;; http://8-p.info/emacs-javascript.html
        c-basic-offset 4)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
                                        ; (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
  (define-key js2-mode-map "\C-m" 'newline-and-indent)
                                        ; (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
                                        ; (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map "\C-\M-q" 'my-indent-sexp)
  (if (featurep 'js2-highlight-vars)
      (js2-highlight-vars-mode))
  (message "My JS2 hook"))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)


;;=================================================
;; ruby-mode
;;
;; Ref: http://pub.cozmixng.org/~the-rwiki/rw-cgi.rb?cmd=view;name=Emacs
;;=====================================================
(c-add-style "ruby" '("bsd"
                      (c-basic-offset . 4)
                      (c-offsets-alist (case-label . 2)
                                       (label . 2)
                                       (statement-case-intro . 2)
                                       (statement-case-open . 2))))

(defun my-c-mode-hook ()
  (c-set-style "ruby"))
(add-hook 'c-mode-hook 'my-c-mode-hook)


;;=======================================================================
;; yaml-mode
;;=====================================================================
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;;=================================================
;; rinari
;;=====================================================
(require 'rinari)

(require 'rhtml-mode)
(setq auto-mode-alist (cons '("\\.erb$" . rhtml-mode) auto-mode-alist))
(add-hook 'rhtml-mode-hook
          (lambda () (rinari-launch)
            (setq tab-width 2)
            (setq sgml-indent-step 2)
            (setq indent-tabs-mode nil)
            (setq sgml-basic-offset 2)
            )
          )


;;=======================================================================
;; python-mode
;;=====================================================================

;; 改行でインデント
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))


;; django用 パス・環境変数 設定
(defun get-django-setting-file (location filename)
  (let* ((dir (file-name-directory location))
         (path (concat dir filename)))
    (if (file-exists-p path)
        path
      (if (not (equal dir "/"))
          (get-django-setting-file (expand-file-name (concat dir "../")) filename)
        ))))

(defun get-django-setting-dir (location dirname)
  (let* ( (dir (file-name-directory location))
          (path (concat dir dirname)))
    (if (file-directory-p path)
        path
      (if (not (equal dir "/"))
          (get-django-setting-dir (expand-file-name (concat dir "/../")) dirname)
        )
      )))

(defadvice run-python (before possibly-setup-django-project-environment)
  (let* (
         (settings-file (get-django-setting-file buffer-file-name "settings.py"))
         (settings-dir (get-django-setting-dir buffer-file-name "settings"))
         )
    (if settings-dir 
        (progn
          (let* ( (project-dir (expand-file-name (concat settings-dir "/../"))) )
            (setenv "PYTHONPATH" project-dir)))
      (progn 
        (let* (  (project-dir (file-name-directory settings-file)) )
          (setenv "PYTHONPATH" project-dir))))
    (setenv "DJANGO_SETTINGS_MODULE" "settings")
    )
  )

(add-hook 'python-mode-hook
		  '(lambda()
			 (setq indent-tabs-mode nil)
			 (setq python-indent 4)
			 ))

;; Simple Python Completion Source for Auto-Complete
;; http://chrispoole.com/project/ac-python/
(require 'ac-python)

;; https://code.launchpad.net/~eopadoan/+junk/django-html-mode
(require 'django-html-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . django-html-mode))


(require 'pony-mode)


;; (require 'django-mode)
;; ;; (yas/load-directory "path-to/django-mode/snippets")
;; (defun django-runserver ()
;;   (interactive)
;;   (django-manage "runserver"))

;; (define-key django-mode-map (kbd "C-t") nil)
;; (define-key django-mode-map (kbd "C-x j") nil)
;; (define-key django-mode-map (kbd "C-c m") nil)
;; (define-key django-mode-map (kbd "C-c t") nil)
;; (define-key django-mode-map (kbd "C-c s") nil)
;; (define-key django-mode-map (kbd "C-c a") nil)

;; (define-key django-mode-map (kbd "C-c C-t") 'django-insert-transpy)
;; (define-key django-mode-map (kbd "C-c C-p j") 'django-jump)
;; (define-key django-mode-map (kbd "C-c C-p m") 'django-manage)
;; (define-key django-mode-map (kbd "C-c C-p t") 'django-test)
;; (define-key django-mode-map (kbd "C-c C-p s") 'django-syncdb)
;; (define-key django-mode-map (kbd "C-c C-p a") 'django-startapp)
;; (define-key django-mode-map (kbd "C-c C-p r") 'django-runserver)

;; http://d.hatena.ne.jp/sou-i/20120531/1338419106
(defun my-short-buffer-file-coding-system (&optional default-coding)
  (let ((coding-str (format "%S" buffer-file-coding-system)))
    (cond ((string-match "shift-jis" coding-str) 'shift_jis)
          ((string-match "euc-jp" coding-str) 'euc-jp)
          ((string-match "utf-8" coding-str) 'utf-8)
          (t (or default-coding 'utf-8)))))

(defun my-insert-file-local-coding ()
  "ファイルの先頭に `coding:' を自動挿入する"
  (interactive)
  (save-excursion
    (goto-line 2) (end-of-line) ; ２行目の行末の移動
    (let ((limit (point)))
      (goto-char (point-min))
      (unless (search-forward "coding:" limit t) ; 2行目以内に `coding:'がない
        (goto-char (point-min))
        ;; #!で始まる場合２行目に記述
        (when (and (< (+ 2 (point-min)) (point-max))
                   (string= (buffer-substring (point-min) (+ 2 (point-min))) "#!"))
          (unless (search-forward "\n" nil t) ; `#!'で始まり末尾に改行が無い場合
            (insert "\n"))) ; 改行を挿入
        (let ((st (point)))
          (insert (format "-*- coding: %S -*-\n" (my-short-buffer-file-coding-system)))
          (comment-region st (point)))))))

(add-hook 'python-mode-hook 'my-insert-file-local-coding)



;;=======================================================================
;; rvm
;;=====================================================================
(if (file-exists-p "~/.rvm/bin/rvm")
    (progn
      (require 'rvm)
      (rvm-use-default) ;; use rvm’s default ruby for the current Emacs session
      )
  )


;;=======================================================================
;; coffee-mode
;;=====================================================================
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(defun coffee-custom ()
  "coffee-mode-hook"

  ;; CoffeeScript uses two spaces.
  (make-local-variable 'tab-width)
  (set 'tab-width 2)
  (setq coffee-tab-width 2)


  ;; If you don't have js2-mode
  ;; (setq coffee-js-mode 'javascript-mode)

  ;; If you don't want your compiled files to be wrapped
  (setq coffee-args-compile '("-c" "--bare"))

  ;; *Messages* spam
  (setq coffee-debug-mode t)

  ;; Emacs key binding
  (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)

  ;; Riding edge.
  (setq coffee-command (expand-file-name "~/node_modules/.bin/coffee"))

  ;; Compile '.coffee' files on every save
  ;; (and (file-exists-p (buffer-file-name))
  ;;      (file-exists-p (coffee-compiled-file-name))
  ;;      (coffee-cos-mode t))

  ;; https://github.com/bodil/emacs.d/blob/master/bodil-js.el
  (define-key coffee-mode-map (kbd "<tab>") 'coffee-indent)
  (define-key coffee-mode-map (kbd "<backtab>") 'coffee-unindent)
  )

(add-hook 'coffee-mode-hook 'coffee-custom)

;; https://github.com/bodil/emacs.d/blob/master/bodil-js.el
(defun shift-region (numcols)
  (setq region-start (region-beginning))
  (setq region-finish (region-end))
  (save-excursion
    (if (< (point) (mark)) (exchange-point-and-mark))
    (let ((save-mark (mark)))
      (indent-rigidly region-start region-finish numcols))))

(defun coffee-indent-block ()
  (shift-region coffee-tab-width)
  (setq deactivate-mark nil))

(defun coffee-unindent-block ()
  (shift-region (- coffee-tab-width))
  (setq deactivate-mark nil))

(defun coffee-indent ()
  (interactive)
  (if (and (boundp 'ac-trigger-command-p) (ac-trigger-command-p last-command))
      (auto-complete)
    (if mark-active
        (coffee-indent-block)
      (indent-for-tab-command))))

(defun coffee-unindent ()
  (interactive)
  (if mark-active
      (coffee-unindent-block)
    (progn
      (indent-line-to (- (current-indentation) coffee-tab-width)))))


(add-to-list 'ac-modes 'coffee-mode)
;;=======================================================================
;; haml-mode
;;=====================================================================
(require 'haml-mode)
(add-hook 'haml-mode-hook
		  '(lambda ()
			 (setq indent-tabs-mode nil)
			 (define-key haml-mode-map "\C-m" 'newline-and-indent)))


;;=======================================================================
;; as-mode
;;=====================================================================
;;(require 'actionscript-mode)
(autoload 'actionscript-mode "actionscript-mode" "Major mode for actionscript." t)
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))
(add-to-list 'ac-modes 'actionscript-mode) ;; auto-complete
(add-hook 'actionscript-mode-hook
		  '(lambda ()
			 (setq tab-width 4)
			 (setq c-basic-offset 4)
			 (setq indent-tabs-mode t)
			 (c-set-offset 'arglist-intro 4)
			 (c-set-offset 'arglist-close 0)
			 (c-set-offset 'arglist-cont-nonempty 0)
			 (c-set-offset 'statement-cont 0)
			 (c-set-offset 'substatement-open 0)
			 (c-set-offset 'block-open 0)     
			 (c-set-offset 'case-label '+)
			 (c-set-offset 'statement-case-open 0)
			 ))

;;=======================================================================
;; scss-mode
;;=====================================================================
(require 'scss-mode)
(setq exec-path (cons (expand-file-name "~/.rvm/gems/ruby-1.9.3-head
/bin") exec-path))
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(add-hook 'scss-mode-hook 'ac-css-mode-setup)
(add-hook 'scss-mode-hook
          (lambda ()
            (setq css-indent-offset 2)
			(setq scss-compile-at-save nil)
			))
(add-to-list 'ac-modes 'scss-mode) ;; auto-complete


;;=======================================================================
;; less-css-mode
;;=====================================================================
(require 'less-css-mode)
(autoload 'less-css-mode "less-css-mode")
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))
(add-to-list 'ac-modes 'less-css-mode) ;; auto-complete


;;===========================================================
;; sgml-mode
;;=========================================================
(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)
(setq auto-mode-alist (cons '("\\.tpl$" . sgml-mode) auto-mode-alist))
(add-hook
 'sgml-mode-hook
 '(lambda ()
    (setq tab-width 2)
    (setq sgml-indent-step 2)
    (setq indent-tabs-mode t)
    (setq sgml-basic-offset 2)
    ))


;;=====================================================
;; phpmode
;;==============================================
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)

(add-hook
 'php-mode-hook
 '(lambda ()
    (setq tab-width 2)
    (setq c-basic-offset 2)
    (setq indent-tabs-mode t)
	(c-set-offset 'case-label' 2) 
	(c-set-offset 'arglist-intro' 2) 
	(c-set-offset 'arglist-cont-nonempty' 2)
	(c-set-offset 'arglist-close' 0)
    )
 )


;;=========================
;; mmm-mode
;;=================================
;; (autoload 'mmm-mode)
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)

(setq mmm-submode-decoration-level 1)				;; mmm-modeをカラフルに
(set-face-bold-p 'mmm-default-submode-face t)			;; mmm-modeのフェイスを変更
(set-face-background 'mmm-default-submode-face "gray10")	;; submodeの時の背景色
;;(invert-face 'mmm-default-submode-face)			;; mmm-modeの前景色と背景色を入れ換える
(mmm-add-mode-ext-class 'sgml-mode "\\.php$" 'sgml-php)
(mmm-add-classes '(
				   (sgml-php
					:submode php-mode
					:front "<\\?\\(php\\)?"
					:back "\\?>"
					)    ))
(add-to-list 'auto-mode-alist '("\\.php$" . sgml-mode))
;; ;;php-modeでtab出来ない問題を解決
;; (defun save-mmm-c-locals ()
;;   (with-temp-buffer
;;     (php-mode)
;;     (dolist (v (buffer-local-variables))
;;       (when (string-match "\\`c-" (symbol-name (car v)))
;;         (add-to-list 'mmm-save-local-variables `(,(car v) nil ,mmm-c-derived-modes))
;;         )
;;       )
;;     )
;;   )

;; (save-mmm-c-locals)



;;=======================================================================
;; color
;;=====================================================================
;; カーソル位置のフェースを調べる関数
(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))


(require 'color-theme)								;; ウィンドウカラーテーマ
;;(color-theme-initialize)
;;(color-theme-charcoal-black)
(set-face-background 'region "#192C35")					;; リージョンの背景色

(defun my-color-theme ()
  "Color theme by mikio, created 2010-04-26."
  (interactive)
  (color-theme-install
   '(my-color-theme
     ((background-color . "black")
      (background-mode . dark)
      (border-color . "Grey")
      (cursor-color . "Grey")
      (foreground-color . "Grey")
      (mouse-color . "Grey"))
     ((compilation-message-face . underline)
      (list-matching-lines-buffer-name-face . underline)
      (list-matching-lines-face . bold)
      (php-default-face . default)
      (tags-tag-face . default)
      (view-highlight-face . highlight)
      (widget-mouse-face . highlight))
     (default ((t (:stipple nil :background "black" :foreground "Grey" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "apple" :family "Inconsolata"))))
     (Info-title-1-face ((t (:bold t :weight bold :height 1.728))))
     (Info-title-2-face ((t (:bold t :weight bold :height 1.44))))
     (Info-title-3-face ((t (:bold t :weight bold :height 1.2))))
     (Info-title-4-face ((t (:bold t :weight bold))))
     (bg:erc-color-face0 ((t (nil))))
     (bg:erc-color-face1 ((t (nil))))
     (bg:erc-color-face10 ((t (nil))))
     (bg:erc-color-face11 ((t (nil))))
     (bg:erc-color-face12 ((t (nil))))
     (bg:erc-color-face13 ((t (nil))))
     (bg:erc-color-face14 ((t (nil))))
     (bg:erc-color-face15 ((t (nil))))
     (bg:erc-color-face2 ((t (nil))))
     (bg:erc-color-face3 ((t (nil))))
     (bg:erc-color-face4 ((t (nil))))
     (bg:erc-color-face5 ((t (nil))))
     (bg:erc-color-face6 ((t (nil))))
     (bg:erc-color-face7 ((t (nil))))
     (bg:erc-color-face8 ((t (nil))))
     (bg:erc-color-face9 ((t (nil))))
     (bold ((t (:bold t :weight bold))))
     (bold-italic ((t (:bold t :foreground "beige" :weight bold))))
     (border ((t (:background "Grey"))))
     (buffer-menu-buffer ((t (:bold t :weight bold))))
     (button ((t (:underline t))))
     (calendar-today-face ((t (:underline t))))
     (comint-highlight-input ((t (:bold t :weight bold))))
     (comint-highlight-prompt ((t (:foreground "cyan"))))
     (compilation-column-number ((t (:bold t :foreground "sky blue" :weight bold))))
     (compilation-error ((t (:bold t :foreground "Red" :weight bold))))
     (compilation-info ((t (:bold t :foreground "Green1" :weight bold))))
     (compilation-line-number ((t (:bold t :foreground "turquoise" :weight bold))))
     (compilation-warning ((t (:bold t :foreground "Orange" :weight bold))))
     (completions-common-part ((t (:stipple nil :background "Grey15" :foreground "Grey" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 118 :width normal :foundry "unknown"))))
     (completions-first-difference ((t (:bold t :weight bold))))
     (cperl-array-face ((t (:bold t :foreground "light salmon" :weight bold))))
     (cperl-hash-face ((t (:italic t :bold t :foreground "beige" :slant italic :weight bold))))
     (cperl-nonoverridable-face ((t (:foreground "aquamarine"))))
	 (css-property ((t (:bold t :foreground "pale turquoise" :weight bold))))           	 (css-selector ((t (:bold t :foreground "turquoise" :weight bold))))
     (cursor ((t (:background "Grey"))))
     (custom-button-face ((t (:foreground "gainsboro"))))
     (custom-button-pressed-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style pressed-button)))))
     (custom-changed-face ((t (:background "blue" :foreground "white"))))
     (custom-comment-face ((t (:background "dim gray"))))
     (custom-comment-tag-face ((t (:foreground "gray80"))))
     (custom-documentation-face ((t (:foreground "light blue"))))
     (custom-face-tag-face ((t (:underline t))))
     (custom-group-tag-face ((t (:bold t :foreground "pale turquoise" :weight bold))))
     (custom-group-tag-face-1 ((t (:foreground "pale turquoise" :underline t))))
     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))
     (custom-modified-face ((t (:background "blue" :foreground "white"))))
     (custom-rogue-face ((t (:background "black" :foreground "pink"))))
     (custom-saved-face ((t (:underline t))))
     (custom-set-face ((t (:background "white" :foreground "blue"))))
     (custom-state-face ((t (:foreground "light salmon"))))
     (custom-variable-button-face ((t (:bold t :underline t :weight bold))))
     (custom-variable-tag-face ((t (:bold t :foreground "turquoise" :weight bold))))
     (diary-face ((t (:foreground "red"))))
     (dired-face-directory ((t (:bold t :foreground "sky blue" :weight bold))))
     (dired-face-executable ((t (:foreground "green yellow"))))
     (dired-face-flagged ((t (:foreground "tomato"))))
     (dired-face-marked ((t (:foreground "light salmon"))))
     (dired-face-permissions ((t (:foreground "aquamarine"))))
     (dropdown-list-face ((t (:stipple nil :background "lightyellow" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 118 :width normal :foundry "unknown"))))
     (dropdown-list-selection-face ((t (:stipple nil :background "purple" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 118 :width normal :foundry "unknown"))))
     (erb-comment-delim-face ((t (:bold t :background "gray20" :foreground "#a2e5cf" :weight bold))))
     (erb-comment-face ((t (:bold t :background "gray20" :foreground "#287b60" :weight bold))))
     (erb-delim-face ((t (:background "gray10"))))
     (erb-exec-delim-face ((t (:bold t :background "gray10" :weight bold))))
     (erb-exec-face ((t (:background "gray10"))))
     (erb-face ((t (:background "gray20"))))
     (erb-out-delim-face ((t (:bold t :background "gray10" :foreground "#a9bde6" :weight bold))))
     (erb-out-face ((t (:background "gray10"))))
     (erc-action-face ((t (nil))))
     (erc-bold-face ((t (:bold t :weight bold))))
     (erc-default-face ((t (nil))))
     (erc-direct-msg-face ((t (:foreground "pale green"))))
     (erc-error-face ((t (:bold t :foreground "IndianRed" :weight bold))))
     (erc-highlight-face ((t (:bold t :foreground "pale green" :weight bold))))
     (erc-input-face ((t (:foreground "light blue"))))
     (erc-inverse-face ((t (:background "steel blue"))))
     (erc-notice-face ((t (:foreground "light salmon"))))
     (erc-pal-face ((t (:foreground "pale green"))))
     (erc-prompt-face ((t (:bold t :foreground "light blue" :weight bold))))
     (escape-glyph ((t (:foreground "cyan"))))
     (eshell-ls-archive-face ((t (:bold t :foreground "medium purple" :weight bold))))
     (eshell-ls-backup-face ((t (:foreground "dim gray"))))
     (eshell-ls-clutter-face ((t (:foreground "dim gray"))))
     (eshell-ls-directory-face ((t (:bold t :foreground "medium slate blue" :weight bold))))
     (eshell-ls-executable-face ((t (:bold t :foreground "aquamarine" :weight bold))))
     (eshell-ls-missing-face ((t (:foreground "black"))))
     (eshell-ls-picture-face ((t (:foreground "violet"))))
     (eshell-ls-product-face ((t (:foreground "light steel blue"))))
     (eshell-ls-readonly-face ((t (:foreground "aquamarine"))))
     (eshell-ls-special-face ((t (:foreground "gold"))))
     (eshell-ls-symlink-face ((t (:foreground "white"))))
     (eshell-ls-unreadable-face ((t (:foreground "dim gray"))))
     (eshell-prompt-face ((t (:bold t :foreground "light sky blue" :weight bold))))
     (excerpt ((t (:italic t :slant italic))))
     (fg:erc-color-face0 ((t (:foreground "white"))))
     (fg:erc-color-face1 ((t (:foreground "beige"))))
     (fg:erc-color-face10 ((t (:foreground "pale goldenrod"))))
     (fg:erc-color-face11 ((t (:foreground "light goldenrod yellow"))))
     (fg:erc-color-face12 ((t (:foreground "light yellow"))))
     (fg:erc-color-face13 ((t (:foreground "yellow"))))
     (fg:erc-color-face14 ((t (:foreground "light goldenrod"))))
     (fg:erc-color-face15 ((t (:foreground "lime green"))))
     (fg:erc-color-face2 ((t (:foreground "lemon chiffon"))))
     (fg:erc-color-face3 ((t (:foreground "light cyan"))))
     (fg:erc-color-face4 ((t (:foreground "powder blue"))))
     (fg:erc-color-face5 ((t (:foreground "sky blue"))))
     (fg:erc-color-face6 ((t (:foreground "dark sea green"))))
     (fg:erc-color-face7 ((t (:foreground "pale green"))))
     (fg:erc-color-face8 ((t (:foreground "medium spring green"))))
     (fg:erc-color-face9 ((t (:foreground "khaki"))))
     (file-name-shadow ((t (:foreground "grey70"))))
     (fixed ((t (:bold t :weight bold))))
     (fixed-pitch ((t (:family "courier"))))
     (flyspell-duplicate-face ((t (:bold t :foreground "Gold3" :underline t :weight bold))))
     (flyspell-incorrect-face ((t (:bold t :foreground "OrangeRed" :underline t :weight bold))))
     (font-lock-builtin-face ((t (:foreground "aquamarine"))))
     (font-lock-comment-delimiter-face ((t (:foreground "light blue"))))
     (font-lock-comment-face ((t (:foreground "light blue"))))
     (font-lock-constant-face ((t (:foreground "pale green"))))
     (font-lock-doc-face ((t (:foreground "light sky blue"))))
     (font-lock-doc-string-face ((t (:foreground "sky blue"))))
     (font-lock-function-name-face ((t (:bold t :foreground "aquamarine" :weight bold))))
     (font-lock-keyword-face ((t (:bold t :foreground "pale turquoise" :weight bold))))
     (font-lock-negation-char-face ((t (nil))))
     (font-lock-preprocessor-face ((t (:foreground "aquamarine"))))
     (font-lock-reference-face ((t (:foreground "pale green"))))
     (font-lock-regexp-grouping-backslash ((t (:bold t :weight bold))))
     (font-lock-regexp-grouping-construct ((t (:bold t :weight bold))))
     (font-lock-string-face ((t (:foreground "light sky blue"))))
     (font-lock-type-face ((t (:bold t :foreground "sky blue" :weight bold))))
     (font-lock-variable-name-face ((t (:bold t :foreground "turquoise" :weight bold))))
     (font-lock-warning-face ((t (:bold t :foreground "Red" :weight bold))))
     (fringe ((t (:background "Grey15"))))
     (gnus-cite-face-1 ((t (:foreground "LightSalmon"))))
     (gnus-cite-face-2 ((t (:foreground "Khaki"))))
     (gnus-cite-face-3 ((t (:foreground "Coral"))))
     (gnus-cite-face-4 ((t (:foreground "yellow green"))))
     (gnus-cite-face-5 ((t (:foreground "dark khaki"))))
     (gnus-cite-face-6 ((t (:foreground "bisque"))))
     (gnus-cite-face-7 ((t (:foreground "peru"))))
     (gnus-cite-face-8 ((t (:foreground "light coral"))))
     (gnus-cite-face-9 ((t (:foreground "plum"))))
     (gnus-emphasis-bold ((t (:bold t :weight bold))))
     (gnus-emphasis-bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
     (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))
     (gnus-emphasis-italic ((t (:italic t :slant italic))))
     (gnus-emphasis-strikethru ((t (nil))))
     (gnus-emphasis-underline ((t (:underline t))))
     (gnus-emphasis-underline-bold ((t (:bold t :underline t :weight bold))))
     (gnus-emphasis-underline-bold-italic ((t (:italic t :bold t :underline t :slant italic :weight bold))))
     (gnus-emphasis-underline-italic ((t (:italic t :underline t :slant italic))))
     (gnus-group-mail-1-empty-face ((t (:foreground "White"))))
     (gnus-group-mail-1-face ((t (:bold t :foreground "White" :weight bold))))
     (gnus-group-mail-2-empty-face ((t (:foreground "light cyan"))))
     (gnus-group-mail-2-face ((t (:bold t :foreground "light cyan" :weight bold))))
     (gnus-group-mail-3-empty-face ((t (:foreground "LightBlue"))))
     (gnus-group-mail-3-face ((t (:bold t :foreground "LightBlue" :weight bold))))
     (gnus-group-mail-low-empty-face ((t (:foreground "Aquamarine"))))
     (gnus-group-mail-low-face ((t (:bold t :foreground "Aquamarine" :weight bold))))
     (gnus-group-news-1-empty-face ((t (:foreground "White"))))
     (gnus-group-news-1-face ((t (:bold t :foreground "White" :weight bold))))
     (gnus-group-news-2-empty-face ((t (:foreground "light cyan"))))
     (gnus-group-news-2-face ((t (:bold t :foreground "light cyan" :weight bold))))
     (gnus-group-news-3-empty-face ((t (:foreground "LightBlue"))))
     (gnus-group-news-3-face ((t (:bold t :foreground "LightBlue" :weight bold))))
     (gnus-group-news-4-empty-face ((t (:foreground "Aquamarine"))))
     (gnus-group-news-4-face ((t (:bold t :foreground "Aquamarine" :weight bold))))
     (gnus-group-news-5-empty-face ((t (:foreground "MediumAquamarine"))))
     (gnus-group-news-5-face ((t (:bold t :foreground "MediumAquamarine" :weight bold))))
     (gnus-group-news-6-empty-face ((t (:foreground "MediumAquamarine"))))
     (gnus-group-news-6-face ((t (:bold t :foreground "MediumAquamarine" :weight bold))))
     (gnus-group-news-low-empty-face ((t (:foreground "MediumAquamarine"))))
     (gnus-group-news-low-face ((t (:bold t :foreground "MediumAquamarine" :weight bold))))
     (gnus-header-content-face ((t (:foreground "LightSkyBlue3"))))
     (gnus-header-from-face ((t (:bold t :foreground "light cyan" :weight bold))))
     (gnus-header-name-face ((t (:bold t :foreground "LightBlue" :weight bold))))
     (gnus-header-newsgroups-face ((t (:italic t :bold t :foreground "MediumAquamarine" :slant italic :weight bold))))
     (gnus-header-subject-face ((t (:bold t :foreground "light cyan" :weight bold))))
     (gnus-server-agent-face ((t (:bold t :foreground "PaleTurquoise" :weight bold))))
     (gnus-server-closed-face ((t (:italic t :foreground "Light Steel Blue" :slant italic))))
     (gnus-server-denied-face ((t (:bold t :foreground "Pink" :weight bold))))
     (gnus-server-offline-face ((t (:bold t :foreground "Yellow" :weight bold))))
     (gnus-server-opened-face ((t (:bold t :foreground "Green1" :weight bold))))
     (gnus-signature-face ((t (:foreground "Grey"))))
     (gnus-splash-face ((t (:foreground "ForestGreen"))))
     (gnus-summary-cancelled-face ((t (:background "Black" :foreground "Yellow"))))
     (gnus-summary-high-ancient-face ((t (:bold t :foreground "MediumAquamarine" :weight bold))))
     (gnus-summary-high-read-face ((t (:bold t :foreground "Aquamarine" :weight bold))))
     (gnus-summary-high-ticked-face ((t (:bold t :foreground "LightSalmon" :weight bold))))
     (gnus-summary-high-unread-face ((t (:italic t :bold t :foreground "beige" :slant italic :weight bold))))
     (gnus-summary-low-ancient-face ((t (:italic t :foreground "DimGray" :slant italic))))
     (gnus-summary-low-read-face ((t (:foreground "slate gray"))))
     (gnus-summary-low-ticked-face ((t (:foreground "Pink"))))
     (gnus-summary-low-unread-face ((t (:foreground "LightGray"))))
     (gnus-summary-normal-ancient-face ((t (:foreground "MediumAquamarine"))))
     (gnus-summary-normal-read-face ((t (:foreground "Aquamarine"))))
     (gnus-summary-normal-ticked-face ((t (:foreground "LightSalmon"))))
     (gnus-summary-normal-unread-face ((t (nil))))
     (gnus-summary-selected-face ((t (:underline t))))
     (header-line ((t (:background "grey20" :foreground "grey90" :box nil))))
     (help-argument-name ((t (nil))))
     (highlight ((t (:background "dark slate blue" :foreground "light blue"))))
     (highline-face ((t (:background "DeepSkyBlue4"))))
     (holiday-face ((t (:background "pink"))))
     (ido-first-match ((t (:bold t :weight bold))))
     (ido-incomplete-regexp ((t (:bold t :foreground "Red" :weight bold))))
     (ido-indicator ((t (:background "red1" :foreground "yellow1" :width condensed))))
     (ido-only-match ((t (:foreground "ForestGreen"))))
     (ido-subdir ((t (:foreground "red1"))))
     (info-header-node ((t (:bold t :weight bold))))
     (info-header-xref ((t (:bold t :foreground "sky blue" :weight bold))))
     (info-menu-5 ((t (:underline t))))
     (info-menu-header ((t (:bold t :weight bold))))
     (info-node ((t (:bold t :weight bold))))
     (info-xref ((t (:bold t :foreground "sky blue" :weight bold))))
     (isearch ((t (:background "slate blue"))))
     (isearch-fail ((t (:background "red4"))))
     (italic ((t (:foreground "sky blue"))))
     (jde-bug-breakpoint-cursor ((t (:background "brown" :foreground "cyan"))))
     (jde-bug-breakpoint-marker ((t (:background "yellow" :foreground "red"))))
     (jde-java-font-lock-api-face ((t (:foreground "light goldenrod"))))
     (jde-java-font-lock-bold-face ((t (:bold t :weight bold))))
     (jde-java-font-lock-code-face ((t (nil))))
     (jde-java-font-lock-constant-face ((t (:foreground "Aquamarine"))))
     (jde-java-font-lock-doc-tag-face ((t (:foreground "light coral"))))
     (jde-java-font-lock-italic-face ((t (:italic t :slant italic))))
     (jde-java-font-lock-link-face ((t (:foreground "blue" :underline t :slant normal))))
     (jde-java-font-lock-modifier-face ((t (:foreground "LightSteelBlue"))))
     (jde-java-font-lock-number-face ((t (:foreground "LightSalmon"))))
     (jde-java-font-lock-package-face ((t (:foreground "steelblue1"))))
     (jde-java-font-lock-pre-face ((t (nil))))
     (jde-java-font-lock-underline-face ((t (:underline t))))
     (lazy-highlight ((t (:background "paleturquoise4"))))
     (link ((t (:foreground "cyan1" :underline t))))
     (link-visited ((t (:foreground "violet" :underline t))))
     (makefile-space-face ((t (:background "hotpink"))))
     (match ((t (:background "RoyalBlue3"))))
     (menu ((t (:background "MidnightBlue" :foreground "Grey"))))
     (message-cited-text-face ((t (:foreground "LightSalmon"))))
     (message-header-cc-face ((t (:foreground "light cyan"))))
     (message-header-name-face ((t (:foreground "LightBlue"))))
     (message-header-newsgroups-face ((t (:italic t :bold t :foreground "MediumAquamarine" :slant italic :weight bold))))
     (message-header-other-face ((t (:foreground "MediumAquamarine"))))
     (message-header-subject-face ((t (:bold t :foreground "light cyan" :weight bold))))
     (message-header-to-face ((t (:bold t :foreground "light cyan" :weight bold))))
     (message-header-xheader-face ((t (:foreground "MediumAquamarine"))))
     (message-mml-face ((t (:foreground "ForestGreen"))))
     (message-separator-face ((t (:foreground "chocolate"))))
     (minibuf-isearch-comp-face ((t (:background "navy" :underline t))))
     (minibuf-isearch-face ((t (:bold t :background "blue" :underline t :weight bold))))
     (minibuffer-prompt ((t (:foreground "cyan"))))
     (mmm-cleanup-submode-face ((t (:background "Wheat"))))
     (mmm-code-submode-face ((t (:background "LightGray"))))
     (mmm-comment-submode-face ((t (:background "SkyBlue"))))
     (mmm-declaration-submode-face ((t (:background "Aquamarine"))))
     (mmm-default-submode-face ((t (:bold t :background "gray20" :weight bold))))
     (mmm-delimiter-face ((t (nil))))
     (mmm-init-submode-face ((t (:background "Pink"))))
     (mmm-output-submode-face ((t (:background "Plum"))))
     (mmm-special-submode-face ((t (:background "MediumSpringGreen"))))
     (mode-line ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))
     (mode-line-buffer-id ((t (:bold t :weight bold))))
     (mode-line-emphasis ((t (:bold t :weight bold))))
     (mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
     (mode-line-inactive ((t (:background "grey30" :foreground "grey80" :box (:line-width -1 :color "grey40" :style nil) :weight light))))
     (mouse ((t (:background "Grey"))))
     (next-error ((t (:background "#0d191e"))))
     (nobreak-space ((t (:foreground "cyan" :underline t))))
     (ns-working-text-face ((t (:underline t))))
     (query-replace ((t (:background "slate blue"))))
     (region ((t (:background "#0d191e"))))
     (scroll-bar ((t (:background "grey75"))))
     (secondary-selection ((t (:background "steel blue"))))
     (semantic-dirty-token-face ((t (:background "gray10"))))
     (semantic-unmatched-syntax-face ((t (:underline "red"))))
     (sgml-namespace ((t (:foreground "aquamarine"))))
     (shadow ((t (:foreground "grey70"))))
     (show-paren-match-face ((t (:background "light slate blue" :foreground "white"))))
     (show-paren-mismatch-face ((t (:background "red" :foreground "white"))))
     (speedbar-button-face ((t (:foreground "seashell2"))))
     (speedbar-directory-face ((t (:foreground "seashell3"))))
     (speedbar-file-face ((t (:foreground "seashell4"))))
     (speedbar-highlight-face ((t (:background "dark slate blue" :foreground "wheat"))))
     (speedbar-selected-face ((t (:foreground "seashell1" :underline t))))
     (speedbar-separator-face ((t (:background "blue" :foreground "white" :overline "gray"))))
     (speedbar-tag-face ((t (:foreground "antique white"))))
     (tabbar-button-face ((t (:background "gray72" :foreground "dark red" :box (:line-width 2 :color "white" :style released-button) :height 0.8))))
     (tabbar-default-face ((t (:background "gray72" :foreground "gray60" :height 0.8))))
     (tabbar-selected-face ((t (:background "gray72" :foreground "blue" :box (:line-width 2 :color "white" :style released-button) :height 0.8))))
     (tabbar-separator-face ((t (:background "gray72" :foreground "gray60" :height 0.16000000000000003))))
     (tabbar-unselected-face ((t (:background "gray72" :foreground "gray60" :box (:line-width 2 :color "white" :style pressed-button) :height 0.8))))
     (tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))
     (tooltip ((t (:background "lightyellow" :foreground "black"))))
     (trailing-whitespace ((t (:background "red"))))
     (underline ((t (:underline t))))
     (variable-pitch ((t (:family "Sans Serif"))))
     (vertical-border ((t (nil))))
     (which-func ((t (:foreground "Blue1"))))
     (widget-button ((t (:bold t :weight bold))))
     (widget-button-pressed ((t (:foreground "red"))))
     (widget-documentation ((t (:foreground "light blue"))))
     (widget-field ((t (:background "RoyalBlue4" :foreground "wheat"))))
     (widget-inactive ((t (:foreground "dim gray"))))
     (widget-single-line-field ((t (:background "slate blue" :foreground "wheat"))))
     (woman-bold-face ((t (:bold t :foreground "sky blue" :weight bold))))
     (woman-italic-face ((t (:foreground "deep sky blue"))))
     (woman-unknown-face ((t (:foreground "LightSalmon"))))
     (yaml-tab-face ((t (:bold t :background "red" :foreground "red" :weight bold))))
     (yas/field-debug-face ((t (nil))))
     (yas/field-highlight-face ((t (:background "DimGrey"))))
     (zmacs-region ((t (:background "DarkSlateBlue")))))))

(my-color-theme)

;;=======================================================================
;; font http://d.hatena.ne.jp/kakurasan/20090807/p1
;;=====================================================================
(if (>= emacs-major-version 23)
    (progn
      (set-default-font "Inconsolata-14")
      (set-fontset-font (frame-parameter nil 'font)
                        'japanese-jisx0208
                        '("Hiragino Kaku Gothic Pro" . "unicode-bmp")))
  )






;;=======================================================================
;; auto-install
;;=====================================================================
 (require 'auto-install)
 (setq auto-install-directory "~/.emacs.d/auto-install/")
 (auto-install-update-emacswiki-package-name t)
 (auto-install-compatibility-setup)             ; 互換性確保


(require 'nginx-mode)


;;=======================================================================
;; anything
;;=====================================================================
(require 'anything-startup)
(require 'anything-grep)


;; キーバインド
(global-set-key "\C-x\ b" 'anything-buffers-list)
(global-set-key (kbd "C-;") 'anything-filelist+)
(define-key global-map (kbd "M-y") 'anything-show-kill-ring)

(setq anything-c-filelist-file-name "/tmp/all.filelist")
(setq anything-grep-candidates-fast-directory-regexp "^/tmp")


(defadvice agp-command-line (after insert-LANG=C-into-agp-command-line)
  (while (string-match "\\(^\\|| \\)\\(grep -ih\\)" ad-return-value)
    (setq ad-return-value (replace-match "LANG=C \\2" t nil ad-return-value 2))))


(defadvice agp-command-line (after insert-Redirection-of-STDERR-into-agp-command-line)
  (when (string-match "| head -n" ad-return-value)
    (setq ad-return-value (replace-match "2>/dev/null \\&" t nil ad-return-value nil))))

(ad-activate 'agp-command-line)


;; http://d.hatena.ne.jp/akisute3/20120409/1333899842
;;; recentf の表示数を 100 まで拡張
(setq recentf-max-saved-items 100)
(defvar anything-c-source-recentf
  `((name . "Recentf")
    (init . (lambda ()
              (require 'recentf)
              (or recentf-mode (recentf-mode 1))))
    ;; Needed for filenames with capitals letters.
    (disable-shortcuts)
    (candidates . recentf-list)
    (keymap . ,anything-generic-files-map)
    (help-message . anything-generic-file-help-message)
	(candidate-number-limit . ,recentf-max-saved-items) ; 標準定義にこれを追加した
    (mode-line . anything-generic-file-mode-line-string)
    (match anything-c-match-on-basename)
    (type . file))
  "See (info \"(emacs)File Conveniences\").
Set `recentf-max-saved-items' to a bigger value if default is too small.")

;; anything git 
;; http://shibayu36.hatenablog.com/entry/2012/12/22/161727
(defun anything-git-project-is-git-repository ()
  (let ((error-message (shell-command-to-string "git rev-parse")))
    (if (string= error-message "")
        t
      nil)
    ))
(defun anything-git-project-project-dir ()
  (chomp
   (shell-command-to-string "git rev-parse --show-toplevel")
   ))
(defun anything-c-sources-git-project-for ()
  (cond ((anything-git-project-is-git-repository)
         (loop for elt in
               '(("Modified files (%s)" . "--modified")
                 ("Untracked files (%s)" . "--others --exclude-standard")
                 ("All controlled files in this project (%s)" . ""))
               collect
               `((name . ,(format (car elt) (anything-git-project-project-dir)))
                 (init . (lambda ()
                           (setq current-git-project-dir
                                 (anything-git-project-project-dir))
                           (unless (and ,(string= (cdr elt) "") ;update candidate buffer every time except for that of all project files
                                        (anything-candidate-buffer))
                             (with-current-buffer
                                 (anything-candidate-buffer 'global)
                               (insert
                                (shell-command-to-string
                                 ,(format "git ls-files --full-name $(git rev-parse --show-cdup) %s"
                                          (cdr elt))))))))
                 (candidates-in-buffer)
                 (display-to-real . (lambda (name)
                                      (format "%s/%s"
                                              current-git-project-dir name)))
                 (type . file))
               ))
        ((list))
        ))
(defun anything-git-project ()
  (interactive)
  (let* ((sources (anything-c-sources-git-project-for)))
    (anything-other-buffer sources
                           (format "*Anything git project in %s*"
                                   (anything-git-project-project-dir)))))


(defun anything-filelist+ ()
  "Preconfigured `anything' to open files/buffers/bookmarks instantly.

This is a replacement for `anything-for-files'.
See `anything-c-filelist-file-name' docstring for usage."
  (interactive)
  (anything-other-buffer
     (append
      '(anything-c-source-ffap-line
        anything-c-source-ffap-guesser
		anything-c-source-buffers-list
        )
      (anything-c-sources-git-project-for)
      '(anything-c-source-recentf
        anything-c-source-bookmarks
        anything-c-source-file-cache
        anything-c-source-filelist
        ))
   "*anything file list*"))


;; sourceの設定
;; (setq anything-sources
;;       '(
;; 		(anything-c-sources-git-project-for)
;;         anything-c-source-recentf
;;         anything-c-source-files-in-current-dir+
;; 		)
;; )


(defun kill-all-buffers()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

