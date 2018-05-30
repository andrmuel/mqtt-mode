;;; mqtt-mode.el --- major mode and commands for interaction with MQTT servers  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Andreas Müller

;; Author: Andreas Müller <code@0x7.ch>
;; Keywords: tools
;; Version: 0.1.0
;; URL: https://github.com/andrmuel/mqtt-mode
;; Package-Requires: ((emacs "25") (dash "2.14.1"))

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

;; mqtt-mode provides a wrapper around mosquitto_sub and mosquitto_pub
;; to interact with MQTT servers from within Emacs.

;;; Code:

(require 'dash)

(defgroup mqtt nil
  "MQTT support."
  :group 'tools)

(defconst mqtt-pub-bin "mosquitto_pub")

(defconst mqtt-sub-bin "mosquitto_sub")

(defcustom mqtt-host "localhost"
  "MQTT server host name."
  :group 'mqtt
  :type 'string)

(defcustom mqtt-port 1883
  "Port number of MQTT server."
  :group 'mqtt
  :type 'integer)

(defcustom mqtt-username nil
  "User name for MQTT server."
  :group 'mqtt
  :type '(choice string (const nil)))

(defcustom mqtt-password nil
  "Password for MQTT server."
  :group 'mqtt
  :type '(choice string (const nil)))

(defcustom mqtt-subscribe-topic "#"
  "Topic to subscribe to."
  :group 'mqtt
  :type 'string)

(defcustom mqtt-publish-topic "emacs"
  "Topic to publish to."
  :group 'mqtt
  :type 'string)

(defcustom mqtt-subscribe-qos-level 0
  "Topic to publish to."
  :group 'mqtt
  :type 'integer)

(defcustom mqtt-publish-qos-level 0
  "Topic to publish to."
  :group 'mqtt
  :type 'integer)

(defcustom mqtt-mosquitto-pub-arguments '()
  "Additional arguments to mosquitto_pub."
  :group 'mqtt
  :type '(repeat string))

(defcustom mqtt-mosquitto-sub-arguments '("-v")
  "Additional arguments to mosquitto_sub."
  :group 'mqtt
  :type '(repeat string))

(defcustom mqtt-timestamp-format "[%y-%m-%d %H:%M:%S]\n"
  "Format for timestamps for incoming messages (input for
format-time-string)."
  :group 'mqtt
  :type 'string)

(defcustom mqtt-comint-prompt "---> "
  "Format for timestamps for incoming messages (input for
format-time-string)."
  :group 'mqtt
  :type 'string)

(define-derived-mode mqtt-mode comint-mode "MQTT Mode"
  "Major mode for MQTT interaction.

\\<mqtt-mode-map>"
  (setq-local comint-prompt-regexp (regexp-quote mqtt-comint-prompt))
  (setq-local comint-prompt-read-only t)
  (add-hook 'comint-preoutput-filter-functions 'mqtt-comint-output-filter t t)
  (setq-local comint-input-sender 'mqtt-comint-input-sender))

(defun mqtt-comint-output-filter (string)
  (alert string)
  (concat "\n"
          (propertize (format-time-string mqtt-timestamp-format) 'read-only t 'font-lock-face 'font-lock-comment-face)
          (string-trim string)
          "\n"
          mqtt-comint-prompt))

(defun mqtt-comint-input-sender (proc string)
  (mqtt-send-message string))

(defun run-mqtt ()
  "Run an inferior instance of 'mosquitto_sub' inside Emacs to receive
  MQTT messages and use 'mosquitto_pub' to publish messages."
  (interactive)
  (let* ((args (append `("mqtt-client" ,nil ,mqtt-sub-bin ,nil)
                       (-flatten `(,mqtt-mosquitto-sub-arguments
                                   "-h" ,mqtt-host
                                   "-p" ,(int-to-string mqtt-port)
                                   ,(if (and mqtt-username mqtt-password)
                                        `("-u" ,mqtt-username
                                          "-P" ,mqtt-password))
                                   "-t" ,mqtt-subscribe-topic
                                   "-q" ,(int-to-string mqtt-subscribe-qos-level)))))
         (buffer (apply #'make-comint-in-buffer args)))
    (with-current-buffer buffer
      (display-buffer (current-buffer))
      (mqtt-mode)
      (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
      (setq-local header-line-format
                  (format "server: %s:%d topic: '%s' / '%s' qos level: %d / %d"
                          mqtt-host
                          mqtt-port
                          mqtt-subscribe-topic
                          mqtt-publish-topic
                          mqtt-subscribe-qos-level
                          mqtt-publish-qos-level)))))

(defun mqtt-start-consumer ()
  (interactive)
  (let ((command (-flatten `(,mqtt-sub-bin
                             ,mqtt-mosquitto-sub-arguments
                             "-h" ,mqtt-host
                             "-p" ,(int-to-string mqtt-port)
                             ,(if (and mqtt-username mqtt-password)
                                  `("-u" ,mqtt-username
                                    "-P" ,mqtt-password))
                             "-t" ,mqtt-subscribe-topic
                             "-q" ,(int-to-string mqtt-subscribe-qos-level))))
        (name "mqtt-consumer")
        (buffer "*mqtt-consumer*"))
    (let ((process
           (make-process
            :name "mqtt-consumer"
            :buffer "*mqtt-consumer*"
            :command command
            :filter 'mqtt-consumer-filter)))
      (set-process-query-on-exit-flag process nil)
      (with-current-buffer (process-buffer process)
        (display-buffer (current-buffer))
        (setq-local header-line-format (format "server: %s:%d subscribe topic: '%s'" mqtt-host mqtt-port mqtt-subscribe-topic))))))

(defun mqtt-consumer-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert (concat (propertize (format-time-string mqtt-timestamp-format) 'face 'font-lock-comment-face)
                          string))
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))
    (alert string)))

(defun mqtt-send-message (message &optional topic)
  (let* ((topic (if topic topic mqtt-publish-topic))
         (command (-flatten `(,mqtt-pub-bin
                              ,mqtt-mosquitto-pub-arguments
                              "-h" ,mqtt-host
                              "-p" ,(int-to-string mqtt-port)
                              ,(if (and mqtt-username mqtt-password)
                                   `("-u" ,mqtt-username
                                     "-P" ,mqtt-password))
                              "-t" ,topic
                              "-q" ,(int-to-string mqtt-publish-qos-level)
                              "-m" ,message))))
    (make-process
     :name "mqtt-publisher"
     :command command
     :buffer "*mqtt-publisher*")))

(defun mqtt-send-region (start end)
  "Publish region contents as MQTT message."
  (interactive "r")
  (mqtt-send-message (buffer-substring start end)))

(provide 'mqtt-mode)
;;; mqtt-mode.el ends here


;; TODO
;; - initial prompt
;; - hook for messages
;;   + alert via hook
;; - better & configurable alert
