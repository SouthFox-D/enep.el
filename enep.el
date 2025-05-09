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


(defgroup enep nil
  "Netease Cloud Music request in Emacs."
  :group 'comm
  :prefix "enep-")

(defcustom enep-api-function 'enep--send-api-request
  "A function to send api request"
  :group 'enep
  :type 'function
  :options '(enep--send-webapi-request
             enep--send-api-request))

(defcustom enep-music-quality 128000
  "Default music quality."
  :type '(choice
          (integer :tag "128Kbps" 128000)
          (integer :tag "192Kbps" 192000)
          (integer :tag "320Kbps" 320000)
          (integer :tag "flac-350Kbps" 350000)))

(defvar enep-repeat-number 0
  "Repeate playlist current number.")

(defvar enep-api-debug nil
  "Enable to print sent/received request to *Messages* buffer.")

(defcustom enep-player-started-chorus-hook nil
  "Hook run when enep starts play chorus."
  :group 'enep
  :type 'hook)

(defvar enep-player-start-chorus-timer nil)

(defcustom enep-player-stoped-chorus-hook nil
  "Hook run when enep stoped play chorus."
  :group 'nep
  :type 'hook)

(defvar enep-player-stop-chorus-timer nil)

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
    (if (> (- 256 (length enc-text)) 0)
        (concat (make-string (- 256 (length enc-text)) ?0) enc-text)
      enc-text)))

(defun enep--get-cookie (cookie-name)
  (if-let (token (cdr (assoc cookie-name (request--curl-get-cookies ".music.163.com" "/" t))))
      token
    ""))

(defun enep--send-webapi-request (api-url json-object)
  "Send web request to api."
  (let* ((secret-key (enep--generate-secret-key))
         (payload (push `(csrf_token . ,(enep--get-cookie "__csrf")) json-object))
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
    (when enep-api-debug
      (message (format "URL -> %s" api-url))
      (message (format "DATA -> %s" json-object)))
    (request
      (concat "https://music.163.com/weapi" api-url)
      :type "POST"
      :headers '(("Content-Type" . "application/x-www-form-urlencoded")
                 ("User-Agent" . "Mozilla/5.0 (X11; Linux x86_64; rv:137.0) Gecko/20100101 Firefox/137.0")
                 ("Accept" . "*/*")
                 ("Origin" . "https://music.163.com")
                 ("Referer" . "https://music.163.com/")
                 ("Cookie" . "WEVNSM=1.0.0")
                 ("Cookie" . "__remember_me=true"))
      :data (concat "params=" params "&encSecKey=" enc-seckey)
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq result (json-parse-string data :object-type 'plist))))
      :sync t)
    (when enep-api-debug
      (message (format "RESP <- %S" result)))
    result))

(defun enep--send-api-request (api-url json-object)
  "Send api request."
  (let (result)
    (when enep-api-debug
      (message (format "URL -> %s" api-url))
      (message (format "DATA -> %S" json-object)))
    (request
      (concat "https://interface.music.163.com/api" api-url)
      :type "POST"
      :headers `(("User-Agent" . "NeteaseMusic/9.1.65.240927161425(9001065);Dalvik/2.1.0 (Linux; U; Android 14; 23013RK75C Build/UKQ1.230804.001)")
                 ("Accept" . "*/*")
                 ("Referer" . "https://music.163.com/")
                 ("Cookie" . ,(format "__csrf=%s" (enep--get-cookie "__csrf")))
                 ("Cookie" . "os=android")
                 ("Cookie" . "osver=14")
                 ("Cookie" . "appver=8.20.20.231215173437")
                 ("Cookie" . "channel=xiaomi")
                 ("Cookie" . "mobilename=")
                 ("Cookie" . ,(format "requestId=%s_%04d"
                                      (number-to-string (floor (float-time)))
                                      (random 1000)))
                 ("Cookie" . ,(format "buildver=%s"
                                      (substring (number-to-string (floor (float-time))) 0 10)))
                 ("Cookie" . ,(format "deviceId=%s"
                                      "C0D2371BCEB0EF25D0F781854C14E777BB068204641929F6CAE0")))
      :data json-object
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq result (json-parse-string data :object-type 'plist))))
      :sync t)
    (when enep-api-debug
      (message (format "RESP <- %S" result)))
    result))

(defmacro enep--request-callback-chain (url encoding callback &rest foarms)
  `(request
     ,url
     :encoding ,encoding
     :headers '(("User-Agent" . "Mozilla/5.0 (X11; Linux x86_64; rv:137.0) Gecko/20100101 Firefox/137.0")
                ("Accept" . "*/*")
                ("Referer" . "https://music.163.com"))
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall (lambda () ,callback))
                 ,(when (not (null foarms))
                    (pcase foarms
                      (`(,next-url ,next-encoding ,next-callback)
                       `(enep--request-callback-chain ,next-url ,next-encoding ,next-callback nil))
                      (`(,next-url ,next-encoding ,next-callback . ,rest)
                       `(enep--request-callback-chain ,next-url ,next-encoding ,next-callback ,rest))))))))

(defun enep-qr-login ()
  (let ((unikey
         (plist-get (funcall enep-api-function "/login/qrcode/unikey"
                                               '((type . 3)))
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
   (funcall enep-api-function "/login/qrcode/client/login"
                              `((type . 3) (key . ,unikey)))))

(defun enep-download-music (id &optional callback)
  (let* ((download-url (plist-get
                        (plist-get
                         (funcall enep-api-function
                          "/song/enhance/download/url"
                          `((id . ,id)
                            (br . ,enep-music-quality)))
                         :data) :url))
         (song-info (aref (plist-get (funcall enep-api-function
                                      "/v3/song/detail"
                                      `((c . ,(concat "[" (format "{\"id\":%s}" id) "]"))))
                                     :songs) 0))
         (lrc (plist-get (plist-get (funcall enep-api-function
                                     "/song/lyric"
                                     `((id . ,id) (tv . -1) (lv . -1) (rv . -1) (kv . -1)))
                                    :lrc) :lyric))
         (song-name (string-replace "/" "" (plist-get song-info :name)))
         (album-name (string-replace "/" "" (plist-get (plist-get song-info :al) :name)))
         (artist-name (string-replace "/" "" (plist-get (aref (plist-get song-info :ar) 0) :name)))
         (song-file-name (concat (expand-file-name "~/Music/") song-name
                                 "-" album-name
                                 "-" artist-name
                                 ".mp3")))
    (enep--request-callback-chain
     (string-replace "http://" "https://" download-url)
     'binary
     (let ((coding-system-for-write 'no-conversion))
       (with-temp-buffer
         (toggle-enable-multibyte-characters)
         (set-buffer-file-coding-system 'raw-text)
         (insert data)
         (write-region nil nil (concat "/tmp/" song-name ".mp3"))))
     (plist-get (plist-get song-info :al) :picUrl)
     'binary
     (progn
       (let ((coding-system-for-write 'no-conversion))
         (with-temp-buffer
           (toggle-enable-multibyte-characters)
           (set-buffer-file-coding-system 'raw-text)
           (insert data)
           (write-region nil nil (concat "~/Music/" album-name ".jpg"))))
       (with-temp-buffer
         (set-buffer-file-coding-system 'utf-8)
         (insert lrc)
         (write-region nil nil (concat (expand-file-name "~/Music/") song-name
                                       "-" album-name
                                       "-" artist-name
                                       ".lrc")))
       (if (executable-find "lame")
           (let ((process (start-process
                           "lame-process" "*lame*" "lame"
                           "--ti" (concat (expand-file-name "~/Music/") album-name ".jpg")
                           "--tt" song-name
                           "--tl" album-name
                           "--ta" artist-name
                           (concat "/tmp/" song-name ".mp3")
                           song-file-name)))
             (set-process-sentinel
              process
              (lambda (process event)
                (message "Process: %s had the event '%s'" process event)
                (let ((_ (accept-process-output process)))
                  (when callback
                    (message song-file-name)
                    (funcall callback song-file-name)
                    (delete-file (concat "/tmp/" song-name ".mp3")))))))
         (copy-file (concat "/tmp/" song-name ".mp3") song-file-name)
         (delete-file (concat "/tmp/" song-name ".mp3")))))))

(defvar enep--my-like-song '())

(defun enep--get-like-song ()
  (or enep--my-like-song
      (let* ((my-uid (plist-get (plist-get (funcall enep-api-function
                                            "/nuser/account/get"
                                            '())
                                           :account) :id))
             (like-song-list (plist-get (funcall enep-api-function
                                         "/song/like/get"
                                         `((uid . ,my-uid)))
                                        :ids)))
        (setq enep--my-like-song like-song-list)
        like-song-list)))

(defun enep--play-song (&optional song-id)
    (enep-download-music song-id
                         (lambda (song-filename)
                           (when emms-player-playing-p
                             (emms-player-stop))
                           (emms-add-file song-filename)
                           (emms-playlist-current-select-last)
                           (emms-start)))
    (let ((chorus-info (aref (plist-get (funcall enep-api-function
                                         "/song/chorus"
                                         `((ids . [,song-id])))
                                        :chorus)
                             0)))
      (setq enep-player-start-chorus-timer
            (run-at-time (/ (plist-get chorus-info :startTime) 1000)
                         nil
                         (lambda ()
                           (run-hooks 'enep-player-started-chorus-hook))))
      (setq enep-player-stop-chorus-timer
            (run-at-time (/ (plist-get chorus-info :endTime) 1000)
                         nil
                         (lambda ()
                           (run-hooks 'enep-player-stoped-chorus-hook))))))

;;;###autoload
(defun enep-play-next-like-song ()
  (interactive)
  (when emms-player-mpv-proc
    (setq emms-player-mpv-idle-delay 10)
    (run-at-time 5 nil (lambda () (cancel-timer emms-player-mpv-idle-timer))))
  (when (called-interactively-p 'any)
    (setq enep-repeat-number 0))
  (when enep-player-start-chorus-timer
    (cancel-timer enep-player-start-chorus-timer))
  (when enep-player-stop-chorus-timer
    (cancel-timer enep-player-stop-chorus-timer))
  (if (not (equal 0 enep-repeat-number))
      (progn
        (setq enep-repeat-number (1- enep-repeat-number))
        (when emms-player-playing-p
          (emms-player-stop))
        (emms-start)))
  (enep--play-song (seq-random-elt (enep--get-like-song))))

(defun enep-playlist-repeat-current (number)
  (interactive (list (read-number "Input repeat number:")))
  (setq enep-repeat-number number))

(provide 'enep)
;;; enep.el ends here
