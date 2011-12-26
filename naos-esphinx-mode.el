;;; naos-esphinx-mode.el --- Major-Mode for "sphinx" editer

;; Copyright (C) 2011  Nao SATO

;; Author: Nao SATO <naosato0918@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(setq naos-esphinx-root-directory "/path/to/sphinx/dir/")

; naos-esphinx-rireki という名前のハッシュテーブルを生成する.
(setq naos-esphinx-rireki (make-hash-table :test 'equal))


(let ((string (concat naos-esphinx-root-directory "index.rst")))
      (puthash 1 string naos-esphinx-rireki))

(setq naos-esphinx-current-buffer-number 1)



(define-derived-mode naos-esphinx-rst-mode rst-mode "ReST-Eshphinx"
  "sphinx プラットームで rst ファイルを作成するバッファのメジャーモード")


(defun naos-esphinx-jump-to-link ()
  "現在行に記述されているリンクに飛ぶ関数."
  (interactive)
  (let (next-rst-file (current-directory default-directory))
    (setq next-rst-file (naos-esphinx-catch-up-first-string)) ; 現在行に書かれている最初の文字列
    (if  (or (equal next-rst-file "...") (equal next-rst-file "..") (equal next-rst-file ".") (equal next-rst-file ""))
	(message "移動できません!")
      (setq next-rst-file (format "%s.rst" next-rst-file))
      (cond ((file-exists-p next-rst-file)
	     (naos-esphinx-make-memory naos-esphinx-current-buffer-number (concat current-directory next-rst-file)) ; 履歴を残す.
	     (setq naos-esphinx-current-buffer-number (1+ naos-esphinx-current-buffer-number)) ; 履歴番号を一つあげる.
	     (find-file next-rst-file)
	     (naos-esphinx-rst-mode))
	    (t
	     (naos-esphinx-analyze-argument next-rst-file)))
      ) ; if
    ) ;let
  )


(defun naos-esphinx-catch-up-first-string ()
  "現在行の最初の文字列を返す関数."
  (save-excursion
   (beginning-of-line)   ;; 先頭に移動して,
   (buffer-substring  
    (progn (skip-chars-forward " ") (point)) ;;空白でないところまで移動して,
    (progn (skip-chars-forward "-a-zA-Z0-9/._") (point) ;; 文字のないところまで移動して,その間の文字列を返す.
    ))))


(defun naos-esphinx-analyze-argument (string)
  "新しいディレクトリや rst ファイルを作成する関数"
  (cond ((not (string-match "[^/]+/.*" string)) ; string が / を含めなかった場合.
	 (naos-esphinx-create-new-rst-file string))
	(t
	 (let (first-arg second-arg) ; string が / を含める場合.
	   (when (string-match "\\([^/]+\\)/\\(.*\\)" string)
	     (setq first-arg (match-string 1 string))
	     (setq second-arg (match-string 2 string)))
	   (naos-esphinx-search-directory-deeper first-arg second-arg)))

	) ;cond
  )


(defun naos-esphinx-search-directory-deeper (1st-arg 2nd-arg)
  "より深層へ"
  (cond ((not (file-exists-p 1st-arg)) ; 第一引数のディレクトリが存在しなければ,
	 (if (naos-esphinx-create-new-directory 1st-arg)
	 (naos-esphinx-search-directory-deeper 1st-arg 2nd-arg)))
	((not (string-match "[^/]+/.*" 2nd-arg)) ; 第二引数に / が含まれていないなら.
	 (naos-esphinx-create-new-rst-file (concat 1st-arg "/" 2nd-arg)))
	(t ; 第二引数に / が含まれるなら.
	 (let (first-str second-str third-str) 
	   (when (string-match "\\([^/]+\\)/\\(.*\\)" 2nd-arg)
	     (setq first-str 1st-arg)
	     (setq second-str (match-string 1 2nd-arg))
	     (setq third-str (match-string 2 2nd-arg)))
	   (naos-esphinx-search-directory-deeper (concat first-str "/" second-str) third-str)))
	) ; cond
)



(defun naos-esphinx-create-new-rst-file (string)
  "新しい rst ファイルを作成する関数"
  (cond ((y-or-n-p (format "%s が存在しません. 新規作成しますか?" string))

	 (naos-esphinx-make-memory naos-esphinx-current-buffer-number (concat default-directory string))
	 (setq naos-esphinx-current-buffer-number (1+ naos-esphinx-current-buffer-number))
	 (find-file string)
	 (naos-esphinx-rst-mode)
	 t)
	(t
	 (message "ファイル %s を作成しませんでした" string)
	 nil)
	) ; cond
  )


(defun naos-esphinx-create-new-directory (string)
  "新しいディレクトリを作成する関数"
  (cond ((y-or-n-p (format "ディレクトリ %s が存在しません. 新規作成しますか?" string))
	 (make-directory string)
	 t)
	(t
	 (message "ディレクトリ %s を作成しませんでした." string)
	 nil)
	)
  )



(define-key naos-esphinx-rst-mode-map (kbd "C-; C-;") 'naos-esphinx-jump-to-link)


(defun naos-esphinx-make-html ()
  "コンパイル($ make html)する関数"
  (interactive)
  (let ((current-directory default-directory))
    (cd naos-esphinx-root-directory) ; 作業ディレクトリに一旦移動して
    (shell-command "make html &") ; コンパイルして
    (cd current-directory))) ; もとのディレクトリに戻る.


(define-key naos-esphinx-rst-mode-map (kbd "C-; m") 'naos-esphinx-make-html)


(defun naos-esphinx-make-memory (num string)
  "現在のバッファの履歴番号を得て,その次に履歴を作成する関数"
  (puthash (+ 1 num) string naos-esphinx-rireki)
  )


(defun naos-esphinx-back-buffer ()
  "移動した前のバッファに戻る関数"
  (interactive)
  (if (= naos-esphinx-current-buffer-number 1)
      (message "戻れません!")
    (setq naos-esphinx-current-buffer-number (- naos-esphinx-current-buffer-number 1))
    (find-file (gethash naos-esphinx-current-buffer-number naos-esphinx-rireki))
    (naos-esphinx-rst-mode)))


(define-key naos-esphinx-rst-mode-map (kbd "C-; o") 'naos-esphinx-back-buffer)
(define-key naos-esphinx-rst-mode-map (kbd "C-; C-o") 'naos-esphinx-back-buffer)


(defun naos-esphinx-go ()
  "esphinx モードに移行しつつ, index.rst にとぶ関数"
  (interactive)
  (find-file (format "%s/index.rst" naos-esphinx-root-directory))
  (naos-esphinx-rst-mode)
)


(global-set-key (kbd "C-c C-; C-;") 'naos-esphinx-go)

(provide 'naos-esphinx-mode)
;;; naos-esphinx-mode.el ends here
