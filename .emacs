(let ((default-directory "~/.emacs.d"))
(setq load-path (cons default-directory load-path))
(normal-top-level-add-subdirs-to-load-path))

;;(setq load-path (cons (expand-file-name "~/.emacs.d") load-path))


;; ==============================================
;; Misc
;; ===============================================
(setq inhibit-startup-message t)				;; スタートアップページを表示しない
(setq make-backup-files nil)					;; バックアップファイルを作らない
      
								;; フレームのタイトル
(setq frame-title-format
      `( " %b " (buffer-file-name "( %f )") " on ",(system-name)
        )
)
;; シフト + 矢印で範囲選択
(setq pc-select-selection-keys-only t)
(pc-selection-mode 1)


;; =======================================================================
;; Ubuntu Misc
;; =======================================================================

;; gnuserv の設定
;;

(mouse-wheel-mode t)						;; ホイールマウス
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)


(setq line-number-mode t)					;; カーソルのある行番号を表示
(auto-compression-mode t)					;; 日本語infoの文字化け防止
(set-scroll-bar-mode 'right)					;; スクロールバーを右に表示
								;; gnome clipboard
(cond (window-system
       (setq x-select-enable-clipboard t)
       ))

;;(load-library "anthy")						;; Anthy CTRL-\で入力モード切替え
;;(setq default-input-method "japanese-anthy")


;; 言語・文字コード関連の設定
(when (equal emacs-major-version 21) (require 'un-define))
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq file-name-coding-system 'utf-8)


;;=======================================================================
;;Mac Misc
;;=======================================================================
;; Macのキーバインドを使う。optionをメタキーにする。
;; (mac-key-mode 1)
;; (setq mac-option-modifier 'meta)
;; フォント設定
;; (if (eq window-system 'mac) (require 'carbon-font))
;; (fixed-width-set-fontset "hirakaku_w3" 14)
;; (setq fixed-width-rescale nil)


;;=====================================================
;; ウィンドウ設定
;;=====================================================
(require 'color-theme)								;; ウィンドウカラーテーマ
;;(color-theme-initialize)
(color-theme-charcoal-black)
(set-face-background 'region "#0d191e")					;; リージョンの背景色


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


(add-to-list 'default-frame-alist '(alpha . (0.95 0.95)))			;; ウィンドウを透明化

;;=====================================================
;; バッファタブ
;;=====================================================

(require 'tabbar)
(global-set-key [(control shift tab)] 'tabbar-backward)
(global-set-key [(control tab)]       'tabbar-forward)
(tabbar-mode)

;;==============================================
;; indent
;;===============================================
(setq-default tab-width 4)
(setq default-tab-width 4)
(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)
(setq c-tab-always-indent nil)
(setq c-basic-offset 4)
;; (setq indent-line-function 'indent-relative-maybe) ;; 前と同じ行の幅にインデント]




;;=====================================================
;; phpmode
;;==============================================
(require 'php-mode) 
(add-hook 
	'php-mode-user-hook 
	'(lambda () 
		(setq tab-width 4)
		(setq c-basic-offset 4)
		(setq indent-tabs-mode nil)
))

;;===========================================================
;; sgml-mode
;;=========================================================
(require 'sgml-mode)
(add-hook 
	'sgml-mode-hook 
	'(lambda () 
		(setq tab-width 4)
		(setq sgml-indent-step 4)
		(setq indent-tabs-mode nil)
		(setq sgml-basic-offset 4)
))
;;=========================
;; mmm-mode
;;=================================
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 1)				;; mmm-modeをカラフルに
(set-face-bold-p 'mmm-default-submode-face t)			;; mmm-modeのフェイスを変更
(set-face-background 'mmm-default-submode-face "gray20")	;; submodeの時の背景色
;;(invert-face 'mmm-default-submode-face)			;; mmm-modeの前景色と背景色を入れ換える
(add-to-list 'auto-mode-alist '("\\.php?\\'" . sgml-mode))
(mmm-add-mode-ext-class nil "\\.php?\\'" 'html-php)
(mmm-add-classes '(
   
    (html-php
    :submode php-mode 
    :front "<\\?php *echo " 
    :back "\\?>" 
    )
    (html-php 
    :submode php-mode 
    :front "<\\?\\(php\\)?" 
    :back "\\?>" 
    )    ))

;;php-modeでtab出来ない問題を解決
(defun save-mmm-c-locals ()
(with-temp-buffer
(php-mode)
(dolist (v (buffer-local-variables))
(when (string-match "\\`c-" (symbol-name (car v)))
(add-to-list 'mmm-save-local-variables `(,(car v) nil
,mmm-c-derived-modes))))))

(save-mmm-c-locals)

;;=================================================
;; js2-mode
;;=====================================================
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
          '(lambda ()
             (setq js2-basic-offset 4)))

;;=================================================
;; rinari
;;=====================================================
(require 'ido)
(ido-mode t)

(require 'rinari)

(defface erb-face
  `((t (:background "gray20")))
  "Default inherited face for ERB tag body"
  :group 'rhtml-faces)

(defface erb-delim-face
  `((t (:background "gray30")))
  "Default inherited face for ERB tag delimeters"
  :group 'rhtml-faces)

(defface erb-out-delim-face
  `((t (:inherit erb-delim-face :weight bold :foreground "#a9bde6")))
  "Basic face for Ruby embedded into HTML"
  :group 'rhtml-faces)

(defface erb-comment-face
  `((t (:inherit erb-face :weight bold :foreground "#287b60")))
  "Basic face for Ruby embedded into HTML"
  :group 'rhtml-faces)

(defface erb-comment-delim-face
  `((t (:inherit erb-delim-face :weight bold :foreground "#a2e5cf")))
  "Basic face for Ruby embedded into HTML"
  :group 'rhtml-faces)

(require 'rhtml-mode)
(setq auto-mode-alist (cons '("\\.erb$" . rhtml-mode) auto-mode-alist))
(add-hook 'rhtml-mode-hook
    (lambda () (rinari-launch)))



;;(setq erb-face 'my-erb-face)


;;=======================================================================
;; yasnippet
;;=====================================================================
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/eschulte-yasnippets-rails-9b12c79/rails-snippets")

;;=======================================================================
;; minibuf-isearch
;;=====================================================================
(require 'minibuf-isearch)

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

;;=======================================================================
;; yaml-mode
;;=====================================================================
 (require 'yaml-mode)
 (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;;=======================================================================
;; Mouth
;;=====================================================================


