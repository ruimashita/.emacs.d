;; デバッグ
(setq debug-on-error nil)


;; .dir-locals.elの設定例
;; プロジェクトのトップディレクトリに .dir-locals.elを置く
;; (
;;  (nil . (
;;          (indent-tabs-mode . t)
;;          (tab-width . 4)

;;          ;; quickrunでRmdファイルをrmd/dockerと紐付ける
;;          (eval . (eval-after-load "quickrun"
;;                    '(add-to-list 
;;                      'quickrun-file-alist '("\\.Rmd$" . "rmd/docker")
;;                      )
;;                    ))
;;          ))

;;  ;; flake8rcの設定
;;  (python-mode . (
;;                  (flycheck-flake8rc . "/home/takuya/Sites/tetote/setup.cfg")
;;                  ))
;; )

;; ;; flycheck cpp 
;; ((c++-mode . ((flycheck-gcc-include-path . ("/home/<user>/<project>/include" "/home/<user>/<project>/include2")))))
;;


;; for m-x shell-command
(setq shell-file-name "/bin/zsh")
;; for m-x shell
(setq explicit-shell-file-name "/bin/zsh")

;; emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))
;;  C-c C-cに割り当てる
(global-set-key (kbd "C-c C-c") 'server-edit)

;; ~/.emacs.d/submodules を再帰的に読み込み
(let ((default-directory "~/.emacs.d/submodules"))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))


;; Add package repo
(require 'package)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)


;; package auto install
(require 'cl)
(require 'eieio)
(package-initialize)
(defvar installing-package-list
  '(
    ;; ここに使っているパッケージを書く。
    actionscript-mode
    ag
    apache-mode
    auto-complete
    coffee-mode
    color-theme-modern
    dockerfile-mode
    editorconfig
    elscreen
    ess
    exec-path-from-shell
    expand-region
    flycheck
    flycheck-pos-tip
    flycheck-color-mode-line
    go-mode
    google-c-style 
    haml-mode
    helm
    helm-ag
    helm-c-yasnippet
    helm-git-files
    jedi
    julia-mode 
    less-css-mode
    lua-mode
    markdown-mode
    multiple-cursors
    nginx-mode
    php-mode
    polymode
    poly-R
    popup
    popwin
    python
    quickrun
    rainbow-mode
    rvm
    scss-mode
    undo-tree
    visual-regexp
    wakatime-mode
    web-mode
    wgrep
    wgrep-ag
    yaml-mode
    yasnippet
    yasnippet-snippets
    ))

(let ((not-installed (loop for x in installing-package-list
                           when (not (package-installed-p x))
                          collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))


;; ==============================================
;; Misc
;; ===============================================
;; yes or no を y or n に
(fset 'yes-or-no-p 'y-or-n-p)

;; スタートアップページを表示しない
(setq inhibit-startup-message t)

;; バックアップファイルを作らない
(setq make-backup-files nil)

;;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)

;; メニューバーを表示しない
(setq menu-bar-mode nil)

;; ツールバー非表示
(tool-bar-mode 0)


;; フレームのタイトル
(setq frame-title-format
      `( " %b " (buffer-file-name "( %f )") " on ",(system-name)
         )
      )

;; カーソルのある行番号を表示
(setq line-number-mode t)

;; 全てのバッファで行番号が表示
(global-linum-mode t)

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

;; シンボリックリンクを開いたときの質問を消す
(setq vc-follow-symlinks nil)

;; シンボリックリンクを開いたとき、Lockファイルをつくらない
(setq create-lockfiles nil)

;;;大文字化[C-x C-u]・小文字化[C-x C-l]の時、問い合わせなしで実行
;;;http://masao.jpn.org/etc/.emacs.el
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; kill all buffers
(defun kill-all-buffers()
  (interactive)
  (loop for buffer being the buffers
        do (kill-buffer buffer)))


;; 言語・文字コード関連の設定
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(setq-default save-buffer-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; path を設定
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; major-modeを変更したあとも、.dir-locals.elの値を有効にする
(add-hook 'after-change-major-mode-hook 'hack-local-variables)


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

      ;; browserのコマンドをopenにする
      (setq browse-url-generic-program "open")

      ;; fix locale warning. http://qiita.com/ShingoFukuyama/items/cdfa516011171d6aad4a
      (setenv "LANG" "en_US.UTF-8")
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
      ;; (require 'ibus)
      ;; (add-hook 'after-init-hook 'ibus-mode-on)
      ;; ;; Toggle input status by alt + SPC
      ;; (global-set-key "\M- " 'ibus-toggle)
      ;; ;; すべてのバッファで入力状態を共有
      ;; (setq ibus-mode-local nil)
      ;; ;; Busがオンの時のカーソル色
      ;; (setq ibus-cursor-color "aquamarine")
      ;; ;; C-SPC は Set Mark , C-/ は Undo に使う. C-zも消す
      ;; (ibus-define-common-key [?\C-\  ?\C-/ ?\C-z]  nil)

      ;; (require 'mozc)
      ;; (set-language-environment "Japanese")
      ;; (setq default-input-method "japanese-mozc")
      ;; (setq mozc-candidate-style 'echo-area)

      (global-set-key (kbd "M-SPC") nil)
      ;; Toggle input status by alt + SPC
      ;; (global-set-key "\M- " 'toggle-input-method)

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



      ))


;;=====================================================
;; ウィンドウ設定
;;=====================================================

;; ウィンドウを透明化 (Ubuntuの時だけ)
(if (system-type-is-gnu)
    (progn
      (add-to-list 'default-frame-alist '(alpha . (0.95 0.95)))
      ))

;; popwin
;; (require 'popwin)
;; (popwin-mode 1)

;; (push '("*helm for files*" :height 30) popwin:special-display-config)
;; (push '("*helm M-x*" :height 20) popwin:special-display-config)
;; (push '("*grep*" :noselect nil) popwin:special-display-config)

;; 反対側のウィンドウにいけるように
(setq windmove-wrap-around t)

;;; windmove
;; (windmove-default-keybindings) ; 引数なしの場合は Shift
;; Alt + 矢印でウィンドウを移動する
(windmove-default-keybindings 'meta) ; Alt の場合は meta を指定
;; Mac の Command + 矢印でウィンドウを移動する
;; (windmove-default-keybindings 'super) ; Macの人はこちらをオススメ




;;=======================================================================
;; elscreen
;;=====================================================================
(setq elscreen-prefix-key "\C-t")
(require 'elscreen)
(elscreen-start)
(global-set-key "\C-t" 'elscreen-clone)
;;(global-set-key "\C-zk" 'elscreen-kill)
(global-set-key [(C-tab)] 'elscreen-next)
(global-set-key [(C-S-iso-lefttab)] 'elscreen-previous)


;;=======================================================================
;; tramp
;;=====================================================================
(require 'tramp)
(setq tramp-default-method "ssh")
(add-to-list 'tramp-default-proxies-alist
             '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '("localhost" "\\`root\\'" nil))
(add-to-list 'tramp-default-proxies-alist
             '((regexp-quote (system-name)) "\\`root\\'" nil))


;; ssh1, ssh2を削除する
(delete '("ssh1" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("-1") ("-e" "none") ("%h"))) (tramp-async-args (("-q"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c")) (tramp-gw-args (("-o" "GlobalKnownHostsFile=/dev/null") ("-o" "UserKnownHostsFile=/dev/null") ("-o" "StrictHostKeyChecking=no"))) (tramp-default-port 22)) tramp-methods)

(delete '("ssh2" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("-2") ("-e" "none") ("%h"))) (tramp-async-args (("-q"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c")) (tramp-gw-args (("-o" "GlobalKnownHostsFile=/dev/null") ("-o" "UserKnownHostsFile=/dev/null") ("-o" "StrictHostKeyChecking=no"))) (tramp-default-port 22)) tramp-methods)

;;=======================================================================
;; forward, backward
;;=====================================================================
;; 漢字/ひらがな/カタカナを分けてforward, backwardする
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
;; visual-regexp
;;=====================================================================
(global-set-key (kbd "C-M-%") 'vr/query-replace)


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
;; multiple-cursors
;;===============================================
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;;==============================================
;; indent
;;===============================================
(setq-default default-tab-width 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

;;=======================================================================
;; auto-complete
;;=====================================================================
;;(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;; 補完メニュー表示時のキーマップを有効にする
(setq ac-use-menu-map t)

;; (global-auto-complete-mode t)
(ac-set-trigger-key "TAB")

;; 大文字小文字を区別しない
(setq ac-ignore-case t)
(put 'downcase-region 'disabled nil)

;;=======================================================================
;; editorconfig
;;=====================================================================
(require 'editorconfig)
(editorconfig-mode 1)


;;=======================================================================
;; wgrep
;;=====================================================================
(require 'wgrep)


;;=======================================================================
;; ag silver-search
;;=====================================================================
(require 'ag)
(setq ag-highlight-search t)
(setq ag-reuse-window 'nil)

;; wgrep
(autoload 'wgrep-ag-setup "wgrep-ag")
(add-hook 'ag-mode-hook 'wgrep-ag-setup)


;;=======================================================================
;; undo-tree
;;=====================================================================
(require 'undo-tree)
(global-undo-tree-mode)


;;=======================================================================
;; wakatime
;;=====================================================================
;; (setq wakatime-api-key "")
;; (setq wakatime-cli-path "/usr/local/bin/wakatime")
;; (global-wakatime-mode)

;;=======================================================================
;; markdown-mode
;;=====================================================================
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(setq markdown-xhtml-header-content "<meta charset=\"utf-8\" />
<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css\" /> ")


;;=======================================================================
;; yasnippet
;;=====================================================================
(require 'yasnippet)
(yas-global-mode 1)


;;=======================================================================
;; flycheck
;;=====================================================================
(require 'flycheck)


(defun flycheck-may-enable-mode ()
  "tramp経由で開いたファイルで、flycheckが有効になるように、overrideする"
  (and (pcase flycheck-global-modes
         ;; Whether `major-mode' is disallowed by `flycheck-global-modes'
         (`t t)
         (`(not . ,modes) (not (memq major-mode modes)))
         (modes (memq major-mode modes)))
       (not (or (minibufferp)
                (eq major-mode 'fundamental-mode)
                (eq (get major-mode 'mode-class) 'special)
                (flycheck-ephemeral-buffer-p)
                (flycheck-encrypted-buffer-p)
                ))))


(add-hook 'after-init-hook #'global-flycheck-mode)

(setq flycheck-highlighting-mode 'lines)

(require 'flycheck-pos-tip)
(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
)

(require 'flycheck-color-mode-line)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
)

(set-face-attribute 'flycheck-error nil 
                    :underline `(:color "pink" :style wave))
(set-face-attribute 'flycheck-color-mode-line-warning-face nil
                    :inherit 'flycheck-fringe-warning :background "dark orange" :foreground "black" :weight 'normal)
(set-face-attribute 'flycheck-color-mode-line-error-face nil
                    :inherit 'flycheck-fringe-error :background "pink" :foreground "black" :weight 'normal)


;;=======================================================================
;; quickrun
;;=====================================================================
(require 'quickrun)
(setq quickrun-debug t)
(setq quickrun-timeout-seconds 60)

(global-set-key (kbd "<f5>") 'quickrun)
(global-set-key (kbd "M-<f5>") 'quickrun-compile-only)

(push '("*quickrun*") popwin:special-display-config)

;; rmdをdockerで実行して、output.htmlを吐き出し、ブラウザでみる。
(quickrun-add-command "rmd/docker"
                      '(
                        (:command . "docker-compose run r Rscript")
                        (:exec    . "%c -e 'library (rmarkdown); rmarkdown::render (\"%s\", output_file=\"output.html\" );'")
                        (:outputter . (lambda () (browse-url "output.html")))
                        ))


;;=======================================================================
;; expand-region
;;=====================================================================
(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)
(global-set-key (kbd "C-M-@") 'er/contract-region) ;; リージョンを狭める


;;=========================
;; ess-site(r-mode)
;;=================================

(autoload 'R-mode "ess-site" "Emacs Speaks Statistics mode" t)
(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))

(add-hook 'ess-mode-hook
          (lambda ()
            (setq ess-ask-for-ess-directory nil)
            (setq ess-tab-always-indent t)
            (setq ess-indent-with-fancy-comments nil)
            )
          )

;;=========================
;; poly-rmarkdown-mode
;;=================================
(require 'poly-R)
(require 'poly-markdown)
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))


;;=========================
;; css-mode
;;=================================
(autoload 'css-mode "css-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))


;;=================================================
;; js-mode
;;=====================================================
(autoload 'js-mode "js-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(add-hook 'js-mode-hook
          (lambda ()
            (setq js-indent-level 4)
            )
)

;; プロジェクトのトップディレクトリに .dir-locals.elを置く
;; flyacheckでeslintを使う設定
;; ((js-mode
;;  (flycheck-checker . javascript-eslint)
;;  ))

;;=================================================
;; ruby-mode
;;
;;
;;=====================================================
(autoload 'ruby-mode "ruby-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))
(add-hook 'ruby-mode-hook
          (lambda ()
            (setq tab-width 2)
            (setq indent-tabs-mode nil)
            (setq ruby-insert-encoding-magic-comment nil)
            (setq ruby-deep-indent-paren-style nil)
            )
          )
;; defadviceで既存のインデント関数ruby-indent-lineに対する追加処理を定義する
;; after -> 既存の関数の処理の後に実行される
;; unindent-closing-paren -> このアドバイスの名前
;; activate -> このアドバイスがすぐに有効になる
(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      ;; ポイント(カーソル)をインデントの位置に移動する
      (back-to-indentation)
      ;; syntax-ppssはparserの状態を表すリストを返す
      ;; 1番目の要素は括弧の深さ、2番目の要素は一番内側の開始括弧の位置を表す
      (let ((state (syntax-ppss)))
        ;; ポイントの初期状態とインデントの位置との差をoffsetとする
        (setq offset (- column (current-column)))
        ;; ポイントの位置の文字が')'で括弧の中にある場合
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          ;; 一番内側の'('に移動
          (goto-char (cadr state))
          ;; インデント幅を取得
          (setq indent (current-indentation)))))
    (when indent
      ;; インデントする
      (indent-line-to indent)
      ;; オフセットが存在する場合、その分だけポイントを移動する
      ;; つまり、インデント修正後のポイントのあるべき場所に戻る
      (when (> offset 0) (forward-char offset)))))


;;=======================================================================
;; yaml-mode
;;=====================================================================
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;;=======================================================================
;; python-mode
;;=====================================================================
;;https://github.com/fgallina/python.el
(require 'python)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
;; (add-hook 'python-mode-hook 'jedi:ac-setup)

(setq jedi:complete-on-dot t)

(setq jedi:get-in-function-call-delay 300)

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
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(add-hook 'scss-mode-hook 'ac-css-mode-setup)
(add-hook 'scss-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq css-indent-offset 4)
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


;;=====================================================
;; cpp mode
;;==============================================

(require 'clang-format)
(global-set-key (kbd "C-c i") 'clang-format-region)
(global-set-key (kbd "C-c u") 'clang-format-buffer)


; Disable clang check, gcc check works better
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(c/c++-clang)))

(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)


(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     ;; Add Google C++ Style checker.
    (flycheck-add-next-checker 'c/c++-gcc
                               '(warning . c/c++-googlelint))))

(custom-set-variables
 '(flycheck-c/c++-googlelint-executable "~/.pyenv/shims/cpplint"))


;;=====================================================
;; phpmode
;;==============================================
(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

(setq flycheck-phpcs-standard "PSR2")

;;(setq flycheck-phpcs-args "--encoding=utf8")
;;
;; :command ("phpcs" "--report=checkstyle"
;;           (option "--standard=" flycheck-phpcs-standard concat)
;;           (eval flycheck-phpcs-args)
;;           source)

(add-hook 'php-mode-hook
          (lambda ()
            (subword-mode 1)
            (php-enable-psr2-coding-style)
            ))

;;=====================================================
;; lua-mode
;;==============================================
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))


;;=========================
;; web-mode
;;=================================
(require 'web-mode)
(add-hook 'web-mode-hook
          '(lambda ()
             (setq indent-tabs-mode t)
             (setq web-mode-markup-indent-offset 4)
             (setq web-mode-css-indent-offset 4)
             (setq web-mode-code-indent-offset 4)
             (setq web-mode-script-padding 4)
             (setq web-mode-disable-auto-pairing nil)
             ))

(add-to-list 'auto-mode-alist '("\\.blade\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(setq web-mode-engines-alist
      '(
        ("django" . "tetote.*?\\.html$") ;; tetoteのhtmlはdjango
        ("django" . "DOLSTA.*?\\.html$") ;; DOLSTAのhtmlはdjango
        )
      )

(add-to-list 'ac-modes 'web-mode)


;;=========================
;; rainbow-mode
;;=================================
(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'scss-mode-hook 'rainbow-mode)
(add-hook 'less-css-mode-hook 'rainbow-mode)



;;=======================================================================
;; color
;;=====================================================================
;; カーソル位置のフェースを調べる関数
(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))

(load-theme 'my-color t t)
(enable-theme 'my-color)



;; =======================================================================
;; whitespace mode
;; =======================================================================
(require 'whitespace)
(setq whitespace-style
      '(face
        trailing
        tabs
        tab-mark
        spaces
        ))

;; スペースは全角のみ
(setq whitespace-space-regexp "\\(　+\\)")
(global-whitespace-mode t)

;;=======================================================================
;; font
;;=====================================================================
;;  if Ubuntu
(if (system-type-is-gnu)
    (progn

      (set-default-font "Ricty-13.5")
      (add-to-list 'default-frame-alist '(font . "ricty-13.5"))
      ))

;; for osx
(if (system-type-is-darwin)
    (progn

      (set-face-attribute 'default nil
                          :family "Ricty"
                          :height 135)
      (set-fontset-font (frame-parameter nil 'font)
                        'japanese-jisx0208
                        (cons "Ricty" "iso10646-1"))
      (set-fontset-font (frame-parameter nil 'font)
                        'japanese-jisx0212
                        (cons "Ricty" "iso10646-1"))
      (set-fontset-font (frame-parameter nil 'font)
                        'katakana-jisx0201
                        (cons "Ricty" "iso10646-1"))

      ;; (create-fontset-from-ascii-font "Ricty-13.5:weight=normal:slant=normal" nil "ricty")
      ;; (set-fontset-font "fontset-ricty"
      ;;                   'unicode
      ;;                   (font-spec :family "Ricty" :size 13.5)
      ;;                   nil
      ;;                   'append)
      ;; (add-to-list 'default-frame-alist '(font . "fontset-ricty"))
      ))

;;=======================================================================
;; custom-set-faces
;;=====================================================================
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-space ((t (:background "gray40"))))
 '(whitespace-tab ((t (:background "gray15" :foreground "black"))))
 '(whitespace-trailing ((t (:foreground "SteelBlue" :underline t :weight bold)))))


;;=======================================================================
;; apache-mode
;;=====================================================================
(require 'apache-mode)


;;=======================================================================
;; nginx-mode
;;=====================================================================
(require 'nginx-mode)


;;=======================================================================
;; dockerfile-mode
;;=====================================================================
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))


;;=======================================================================
;; helm
;;=====================================================================
(require 'helm-config)
(require 'helm-git-files)
(helm-mode t)

(define-key global-map (kbd "M-x") 'helm-M-x)
(define-key global-map (kbd "C-x b") 'helm-buffers-list)
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)
(define-key global-map (kbd "C-;") 'helm-for-files)

;; タブ補完
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

(add-to-list 'helm-completing-read-handlers-alist '(write-file . nil))

(setq helm-for-files-preferred-list
      '(
        helm-source-buffers-list
        helm-source-recentf
        ;; helm-git-files:modified-source
        ;; helm-git-files:untracked-source
        ;; helm-git-files:all-source
        helm-source-bookmarks
        helm-source-file-cache
        helm-source-files-in-current-dir
        helm-source-locate
        )
      )

(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c y") 'helm-yas-complete)
