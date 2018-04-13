;;; mqtt-mode.el --- major mode and commands for interaction with MQTT servers  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Andreas Müller

;; Author: Andreas Müller <code@0x7.ch>
;; Keywords: tools
;; Version: 0.1.0
;; URL: https://github.com/andrmuel/mqtt-mode
;; Package-Requires: ((emacs "25"))

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


(defgroup mqtt nil
  "MQTT support."
  :group 'tools)

(defcustom mqtt-parent-mode 'fundamental-mode
  "Parent mode for MQTT mode."
  :package-version '(mqtt-mode . "0.1.0")
  :group 'mqtt
  :type 'function)

(defcustom mqtt-host "localhost"
  "MQTT server host name."
  :package-version '(mqtt-mode . "0.1.0")
  :group 'mqtt
  :type 'string)

(defcustom mqtt-port 1883
  "Port number of MQTT server."
  :package-version '(mqtt-mode . "0.1.0")
  :group 'mqtt
  :type 'integer)

(defcustom mqtt-subscribe-topic "#"
  "Topic to subscribe to."
  :package-version '(mqtt-mode . "0.1.0")
  :group 'mqtt
  :type 'string)

(defcustom mqtt-publish-topic "/" ;; TODO what to use here?
  "Topic to publish to."
  :package-version '(mqtt-mode . "0.1.0")
  :group 'mqtt
  :type 'string)

(defcustom mqtt-username nil
  "User name for MQTT server."
  :package-version '(mqtt-mode . "0.1.0")
  :group 'mqtt
  :type 'string)

(defcustom mqtt-password nil
  "Password for MQTT server."
  :package-version '(mqtt-mode . "0.1.0")
  :group 'mqtt
  :type 'string)

(defconst mqtt-pub-bin "mosquitto_pub")

(defconst mqtt-sub-bin "mosquitto_sub")

(define-derived-mode mqtt-mode comint-mode "MQTT Mode"
  "Major mode for MQTT interaction.

\\<mqtt-mode-map>"
  (setq-local comint-prompt-regexp "---> ")
  (setq-local comint-prompt-read-only t)
  (add-hook 'comint-preoutput-filter-functions 'mqtt-output-filter t t)
  (setq-local comint-input-sender 'mqtt-comint-input-sender))

(defun mqtt-output-filter (string)
  (alert string)
  (concat (propertize (format-time-string "[%y-%m-%d %H:%M:%S]") 'read-only t 'face 'font-lock-comment-face) ": " (string-trim string) "\n---> "))

(defun mqtt-comint-input-sender (proc string)
  (mqtt-send-message string))


(defun run-mqtt ()
  "Run an inferior instance of 'mosquitto_sub' inside Emacs to receive
  MQTT messages and use 'mosquitto_pub' to publish messages."
  (interactive)
    (let ((buffer
           (make-comint-in-buffer "mqtt-client" nil mqtt-sub-bin nil
                                  "-v"
                                  "-h" mqtt-host
                                  "-p" (int-to-string mqtt-port)
                                  ;; TODO username & password optional
                                  "-u" mqtt-username
                                  "-P" mqtt-password
                                  "-t" mqtt-subscribe-topic)))
      (with-current-buffer buffer
        (display-buffer (current-buffer))
        (mqtt-mode)
        (setq-local header-line-format (format "MQTT client (host: %s; port: %d; subscribe topic: %s; publish topic: %s)" mqtt-host mqtt-port mqtt-subscribe-topic mqtt-publish-topic)))))

(defun mqtt-start-consumer ()
  (interactive)
  (let ((command `(,mqtt-sub-bin
                   "-v"
                   "-h" ,mqtt-host
                   "-p" ,(int-to-string mqtt-port)
                   ;; TODO username & password optional
                   "-u" ,mqtt-username
                   "-P" ,mqtt-password
                   "-t" ,mqtt-subscribe-topic))
        (name "mqtt-consumer")
        (buffer "*mqtt-consumer*"))
    (let ((process
           (make-process
            :name "mqtt-consumer"
            :buffer "*mqtt-consumer*"
            :command command
            :filter 'mqtt-consumer-filter)))
      (with-current-buffer (process-buffer process)
        (display-buffer (current-buffer))
        (setq-local header-line-format (format "MQTT consumer (host: %s; port: %d; topic: %s)" mqtt-host mqtt-port mqtt-subscribe-topic))))))

(defun mqtt-consumer-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))
    (alert string)))

(defun mqtt-send-message (message &optional topic)
  (let* ((topic (if topic topic mqtt-publish-topic))
         (command `(,mqtt-pub-bin
                    "-h" ,mqtt-host
                    "-p" ,(int-to-string mqtt-port)
                    ;; TODO username & password optional
                    "-u" ,mqtt-username
                    "-P" ,mqtt-password
                    "-t" ,topic
                    "-m" ,message)))
    (make-process
     :name "mqtt-publisher"
     :command command
     :buffer "*mqtt-publisher*")))

(provide 'mqtt-mode)
;;; mqtt-mode.el ends here
