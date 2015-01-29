;; デバッグ
(setq debug-on-error nil)

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

;; packageのリポジトを追加
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; package auto install
(require 'cl)
(package-initialize)
(defvar installing-package-list
  '(
    ;; ここに使っているパッケージを書く。
    actionscript-mode
    ag
    apache-mode
    auto-complete
    coffee-mode
    color-theme
    editorconfig
    elscreen
    exec-path-from-shell
    helm
    ;; helm-ls-git
    flymake-easy
    flymake-phpcs
    flymake-python-pyflakes
    foreign-regexp
    haml-mode
    jinja2-mode
    less-css-mode
    markdown-mode
    nginx-mode
    php-mode
    popup
    popwin
    psgml
    python
    rhtml-mode
    rvm
    scss-mode
    web-mode
    wgrep
    wgrep-ag
    yaml-mode
    yasnippet
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

      ;; path を設定
      (require 'exec-path-from-shell)
      (exec-path-from-shell-initialize)

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

;; ウィンドウを透明化
(add-to-list 'default-frame-alist '(alpha . (0.95 0.95)))

;; popwin
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

(push '("*helm for files*" :height 30) popwin:special-display-config)
(push '("*helm M-x*" :height 20) popwin:special-display-config)
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
(elscreen-start)
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
             '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '("localhost" nil nil))
(add-to-list 'tramp-default-proxies-alist
             '((regexp-quote (system-name)) "\\`root\\'" nil))



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
;; foreign-regexp
;;===============================================
(require 'foreign-regexp)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(foreign-regexp/regexp-type (quote ruby))
 '(reb-re-syntax (quote foreign-regexp)))


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
(load "editorconfig")


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
;; markdown-mode
;;=====================================================================
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(setq markdown-css-path "/home/takuya/local/bootstrap/dist/css/bootstrap.min.css")
;;(setq markdown-xhtml-header-content " <link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\"/home/takuya/local/bootstrap/dist/css/bootstrap.min.css\" /> ")

;;=======================================================================
;; yasnippet
;;=====================================================================
;;(require 'yasnippet)
;;(yas-global-mode 1)


;;=======================================================================
;; flymake
;;=====================================================================
(require 'flymake)
(require 'flymake-easy)

(defun my-flymake-show-next-error()
  (interactive)
  (flymake-goto-next-error)
  (flymake-display-err-menu-for-current-line)
  )

(global-set-key "\C-c\C-v" 'my-flymake-show-next-error)

(if (system-type-is-darwin)
    (progn
      ;;      (setq flymake-log-level 3)
      (setq temporary-file-directory "/private/tmp/")
      ))

;; 色の設定
(set-face-attribute 'flymake-errline nil :underline `(:color "#ff3366" :style wave))
(set-face-attribute 'flymake-warnline nil :underline `(:color "#ffd700" :style wave))

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

(eval-after-load 'js
  '(progn
     (font-lock-add-keywords
      'js-mode '(("\t" 0 my-face-b-2 append)
                 ("　" 0 my-face-b-1 append)
                 ("[ \t]+$" 0 my-face-u-1 append)
                 )))
  )



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


;;=================================================
;; rhtml-mode
;;=====================================================
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
;;https://github.com/fgallina/python.el
(require 'python)

(require 'flymake-python-pyflakes)

;; flake8を使う
(setq flymake-python-pyflakes-executable "flake8")

;; オプションの設定
(setq flymake-python-pyflakes-extra-arguments '("--max-line-length=99"))

(add-hook 'python-mode-hook
          '(lambda ()
             (flymake-python-pyflakes-load)
             (define-key python-mode-map  "\C-c\C-v" 'my-flymake-show-next-error)
             )
          )



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
    (setq indent-tabs-mode nil)
    (setq sgml-basic-offset 2)
    ))


;;=====================================================
;; phpmode
;;==============================================
(require 'php-mode)
(require 'flymake-phpcs)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

(setq flymake-phpcs-standard "PSR2")
(setq flymake-phpcs-location 'tempdir)

(add-hook 'php-mode-hook
          (lambda ()
            (subword-mode 1)
            (php-enable-psr2-coding-style)
            (flymake-phpcs-load)
            ))


;;=========================
;; multi-web-mode
;;=================================
;; (require 'multi-web-mode)
;; (setq mweb-default-major-mode 'sgml-mode)
;; (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
;;                   (js-mode "<script[^>]*>" "</script>")
;;                   (css-mode "<style[^>]*>" "</style>")))
;; (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
;; (multi-web-global-mode 1)

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

(setq web-mode-engines-alist
      '(
        ("django" . "tetote.*?\\.html$") ;; tetoteのhtmlはdjango
        ("django" . "DOLSTA.*?\\.html$") ;; DOLSTAのhtmlはdjango
        )
      )

(add-to-list 'ac-modes 'web-mode)


;;=========================
;; jinja2-mode
;;=================================
(require 'jinja2-mode)
(add-hook 'jinja2-mode-hook
          '(lambda ()
             (setq tab-width 2)
             (setq sgml-indent-step 2)
             (setq indent-tabs-mode nil)
             (setq sgml-basic-offset 2)
             ))

;;=======================================================================
;; color
;;=====================================================================
;; カーソル位置のフェースを調べる関数
(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))


(require 'color-theme)                                ;; ウィンドウカラーテーマ
;;(color-theme-initialize)
;;(color-theme-charcoal-black)
(set-face-background 'region "#192C35")                    ;; リージョンの背景色

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
     (css-property ((t (:bold t :foreground "pale turquoise" :weight bold))))                (css-selector ((t (:bold t :foreground "turquoise" :weight bold))))
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

      (create-fontset-from-ascii-font "Ricty-13.5:weight=normal:slant=normal" nil "ricty")
      (set-fontset-font "fontset-ricty"
                        'unicode
                        (font-spec :family "Ricty" :size 13.5)
                        nil
                        'append)
      (add-to-list 'default-frame-alist '(font . "fontset-ricty"))
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
;; helm
;;=====================================================================
(require 'helm-config)
;; (require 'helm-ls-git)
(helm-mode t)

(define-key global-map (kbd "M-x") 'helm-M-x)
(define-key global-map (kbd "C-x b") 'helm-buffers-list)
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)
(define-key global-map (kbd "C-;") 'helm-for-files)

(setq helm-for-files-preferred-list
      '(helm-source-buffers-list
        helm-source-recentf
        ;;helm-source-ls-git-status
        ;;helm-source-ls-git
        helm-source-bookmarks
        helm-source-file-cache
        helm-source-files-in-current-dir
        helm-source-locate)
)
