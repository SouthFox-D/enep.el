;;; enep.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 SouthFox
;;
;; Author: SouthFox <master@southfox.me>
;; Maintainer: SouthFox <master@southfox.me>
;; Created: April 27, 2025
;; Modified: April 27, 2025
;; Version: 0.0.1
;; Keywords: comm
;; Homepage: https://git.southfox.me/elisp/enep.el
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'request)
(require 'browse-url)
(require 'emms-source-file)
(require 'emms-player-mpv)


(defun enep--generate-secret-key ()
  "Generate 16 base62 string."
  (let ((base62 "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
        (secret-key ""))
    (dotimes (_ 16)
      (let ((index (random 62)))
        (setq secret-key (concat secret-key (substring base62 index (1+ index))))))
    secret-key))

(defun enep--elisp-xxd (input-string)
  (mapconcat
   (lambda (char)
     (format "%02x" (aref (string-to-unibyte input-string) char)))
   (number-sequence 0 (1- (length input-string)))
   ""))

;; Pick for emscs-rsa: https://github.com/skeeto/emacs-rsa
(defun enep--rsa-mod-pow (base exponent modulus)
  "Modular exponentiation using right-to-left binary method."
  (let ((result 1))
    (setf base (calc-eval "$1 % $2" nil base modulus))
    (while (calc-eval "$1 > 0" 'pred exponent)
      (when (calc-eval "$1 % 2 == 1" 'pred exponent)
        (setf result (calc-eval "($1 * $2) % $3" nil result base modulus)))
      (setf exponent (calc-eval "$1 \\ 2" nil exponent)
            base (calc-eval "($1 * $1) % $2" nil base modulus)))
    result))

(defun enep--rsa-encrypt (text)
  ;; -----BEGIN PUBLIC KEY-----
  ;; MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDgtQn2JZ34ZC28NWYpAUd98iZ37BUrX/aKzmFbt7clFSs6sXqHauqKWqdtLkF2KexO40H1YTX8z2lSgBBOAxLsvaklV8k4cBFK9snQXE9/DDaFt6Rr7iVZMldczhC0JNgTz+SHXT6CBHuX3e9SdB1Ua44oncaTWz7OBGLbCiK45wIDAQAB
  ;; -----END PUBLIC KEY-----
  ;; openssl asn1parse -inform PEM -strparse 18
  (let* ((key "010001")
         (prim "E0B509F6259DF8642DBC35662901477DF22677EC152B5FF68ACE615BB7B725152B3AB17A876AEA8A5AA76D2E417629EC4EE341F56135FCCF695280104E0312ECBDA92557C93870114AF6C9D05C4F7F0C3685B7A46BEE255932575CCE10B424D813CFE4875D3E82047B97DDEF52741D546B8E289DC6935B3ECE0462DB0A22B8E7")
         (hex-encoded (mapconcat (lambda (byte) (format "%02x" byte))
                                 (string-to-vector text)
                                 ""))
         (enc-text (format "%x" (string-to-number
                                 (enep--rsa-mod-pow
                                  (string-to-number hex-encoded 16)
                                  (string-to-number key 16)
                                  (string-to-number prim 16))))))
    (if (> (- (length enc-text) 256) 0)
        (concat (make-string (- (length enc-text) 256) ?0) enc-text)
      enc-text)))

(defun enep--send-linuxapi-request (json-object)
  "Send linux request to api."
  (let ((eparams (shell-command-to-string
                  (concat "echo -n '"
                          (string-replace "\\\"" "\"" (json-serialize json-object))
                          "' | openssl enc -e -aes-128-ecb -K 7246674226682325323f5e6544673a51 --nosalt | "
                          "xxd -p -u | "
                          "tr -d '\n'")))
        (result))
    ;; (message (concat "eparams=" eparams))
    (request
      "https://music.163.com/api/linux/forward"
      :type "POST"
      :headers '(("Content-Type" . "application/x-www-form-urlencoded")
                 ("User-Agent" . "Mozilla/5.0 (X11; Linux x86_64; rv:137.0) Gecko/20100101 Firefox/137.0")
                 ("Accept" . "*/*"))
      :data (concat "eparams=" eparams)
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (prin1 data)
                  (setq result data)))
      :sync t)
    result))

(defun enep--get-csrf-token ()
  (if-let (token (cdr (assoc "__csrf" (request--curl-get-cookies ".music.163.com" "/" t))))
      token
    ""))

(defun enep--send-webapi-request (api-url json-object)
  "Send web request to api."
  (let* ((secret-key (enep--generate-secret-key))
         (payload (plist-put json-object :csrf_token (enep--get-csrf-token)))
         (eparams (shell-command-to-string
                   (concat "echo -n '"
                           (json-serialize payload)
                           ;; -K (enep--elisp-xxd "0CoJUm6Qyw8W8jud")
                           "' | openssl enc -e -aes-128-cbc -K 30436f4a556d365179773857386a7564"
                           ;; -iv (enep--elisp-xxd "0102030405060708")
                           " -iv 30313032303330343035303630373038 --nosalt -A -a")))
         (params (browse-url-url-encode-chars
                  (shell-command-to-string
                   (concat "echo -n '"
                           eparams
                           "' | openssl enc -e -aes-128-cbc -K " (enep--elisp-xxd secret-key)
                           " -iv 30313032303330343035303630373038 --nosalt -A -a"))
                  "[/+]"))
         (enc-seckey (enep--rsa-encrypt (reverse secret-key)))
         (result))
    (request
      api-url
      :type "POST"
      :headers '(("Content-Type" . "application/x-www-form-urlencoded")
                 ("User-Agent" . "Mozilla/5.0 (X11; Linux x86_64; rv:137.0) Gecko/20100101 Firefox/137.0")
                 ("Accept" . "*/*")
                 ("Referer" . "https://music.163.com"))
      :data (concat "params=" params "&encSecKey=" enc-seckey)
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq result data)))
      :sync t)
    result))

(defun enep-qr-login ()
  (let ((unikey
         (plist-get (json-parse-string
                     (enep--send-webapi-request "https://music.163.com/weapi/login/qrcode/unikey"
                                          '(:type 3))
                     :object-type 'plist)
                    :unikey)))
    (with-current-buffer (get-buffer-create "*enep-login*")
        (goto-char (point-min))
        (insert (concat "Go to\n"))
        (insert (concat "https://api.qrserver.com/v1/create-qr-code/?size=200x200&data="
                        "https://music.163.com/login?codekey=" unikey "\n"))
        (insert "and scan qr code via app, then run:\n")
        (insert (concat "(enep-check-qr-login \"" unikey "\")"))
        (switch-to-buffer-other-window (current-buffer)))))

(defun enep-check-qr-login (unikey)
  (message
   (enep--send-webapi-request "https://music.163.com/weapi/login/qrcode/client/login"
                              `(:type 3 :key ,unikey))))

(defun enep-download-music (id &optional callback)
  (let* ((download-url (plist-get
                        (plist-get
                         (json-parse-string
                          (enep--send-webapi-request
                           "https://music.163.com/weapi/song/enhance/download/url"
                           `(:id ,id
                             :br 192000))
                          :object-type 'plist) :data) :url))
         (song-info (aref (plist-get (json-parse-string
                                      (enep--send-webapi-request
                                       "https://music.163.com/weapi/v3/song/detail"
                                       `(:c ,(concat "[" (format "{\"id\":%s}" id) "]")))
                                      :object-type 'plist) :songs)
                          0))
         (lrc (plist-get (plist-get (json-parse-string (enep--send-webapi-request
                                                        "https://music.163.com/weapi/song/lyric"
                                                        `(:id ,id :tv -1 :lv -1 :rv -1 :kv -1))
                                                       :object-type 'plist)
                                    :lrc) :lyric))
         (song-file-name (concat (expand-file-name "~/Music/") (plist-get song-info :name)
                                 "-" (plist-get (plist-get song-info :al) :name)
                                 "-" (plist-get (aref (plist-get song-info :ar) 0) :name)
                                 ".mp3")))
    (url-copy-file (string-replace "http://" "https://" download-url) (concat "/tmp/" (plist-get song-info :name) ".mp3") t)
    (url-copy-file (plist-get (plist-get song-info :al) :picUrl)
                   (concat "~/Music/" (plist-get (plist-get song-info :al) :name) ".jpg") t)
    (with-current-buffer (find-file-noselect
                          (concat (expand-file-name "~/Music/") (plist-get song-info :name)
                                  "-" (plist-get (plist-get song-info :al) :name)
                                  "-" (plist-get (aref (plist-get song-info :ar) 0) :name)
                                  ".lrc"))
      (set-buffer-file-coding-system 'utf-8)
      (erase-buffer)
      (insert lrc)
      (save-buffer)
      (kill-buffer))
    (let ((process (start-process
                    "lame-process" "*lame*" "lame"
                    "--ti" (concat (expand-file-name "~/Music/")
                                   (plist-get (plist-get song-info :al) :name) ".jpg")
                    "--tt" (plist-get song-info :name)
                    "--ta" (plist-get (aref (plist-get song-info :ar) 0) :name)
                    "--tl" (plist-get (plist-get song-info :al) :name)
                    (concat "/tmp/" (plist-get song-info :name) ".mp3")
                    song-file-name)))
      (set-process-sentinel
       process
       (lambda (process event)
         (message "Process: %s had the event '%s'" process event)
         (let ((_ (accept-process-output process)))
           (when callback
             (message song-file-name)
             (funcall callback song-file-name))))))))

(defvar enep--my-like-song '())

(defun enep--get-like-song ()
  (or enep--my-like-song
      (let* ((my-uid (plist-get (plist-get (json-parse-string
                                            (enep--send-webapi-request "https://music.163.com/weapi/nuser/account/get"
                                                                       '())
                                            :object-type 'plist)
                                           :account)
                                :id))
             (like-song-list (plist-get (json-parse-string (enep--send-webapi-request "https://music.163.com/weapi/song/like/get"
                                                                                `(:uid ,my-uid))
                                                           :object-type 'plist) :ids)))
        (setq enep--my-like-song like-song-list)
        like-song-list)))

;;;###autoload
(defun enep-play-next-like-song ()
  (interactive)
  (when (eq (alist-get 'start (cdr emms-player-playing-p))
            #'emms-player-mpv-start)
    (setq emms-player-mpv-idle-delay 10)
    (cancel-timer emms-player-mpv-idle-timer))
  (let ((song-id (seq-random-elt (enep--get-like-song))))
    (enep-download-music song-id
                    (lambda (song-filename)
                      (when emms-player-playing-p
                        (emms-player-stop))
                      (emms-add-file song-filename)
                      (emms-playlist-current-select-last)
                      (emms-start)))))

;; (setq emms-player-next-function #'enep-play-next-like-song)
(provide 'enep)
;;; enep.el ends here
