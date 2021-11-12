;; -*- lexical-binding: t; -*-
(require 'json)

(defcustom stock-mode-update-interval 600
  "stock mode update interval"
  :type 'integer)

(defcustom stock-mode-symbols (list "NOKIA.HE")
  "stock mode list of symbols"
  :type '(repeat string))

(defvar stock-mode-update-timer nil)
(defvar stock-mode-status-string "")
(defvar stock-mode-yahoo-finance-api "https://query1.finance.yahoo.com/v8/finance/chart/%s")
(defvar stock-mode-info-hash (make-hash-table :test 'equal))

(defun stock-mode-get-price (raw-data)
  (let* ((chart (gethash "chart" raw-data))
         (result (car (gethash "result" chart)))
         (meta (gethash "meta" result))
         (price (gethash "regularMarketPrice" meta)))
    price))

(defun stock-mode-get-currency (raw-data)
  (let* ((chart (gethash "chart" raw-data))
         (result (car (gethash "result" chart)))
         (meta (gethash "meta" result))
         (cur (gethash "currency" meta)))
    cur))

(defun stock-mode-format-currency (currency)
  (cond
   ((equal "CNY" currency) "¥")
   ((equal "EUR" currency) "€")
   ((equal "USD" currency) "$")))

(defun stock-mode-get-status-string-for (symbol)
  (let ((price (gethash (format "%s.PRICE" symbol) stock-mode-info-hash nil))
        (cur (gethash (format "%s.CUR" symbol) stock-mode-info-hash nil)))
    (if (or (eq price nil) (eq cur nil))
        ""
      (format "%s %s%.2f " symbol (stock-mode-format-currency cur) price))))

(defun stock-mode-update-status ()
  (setq stock-mode-status-string
        (apply 'cl-concatenate 'string (mapcar 'stock-mode-get-status-string-for stock-mode-symbols))))

(defun stock-mode-update-price (symbol)
  (with-current-buffer (get-buffer-create (format "*stock-%s*" symbol))
    (when (< 0 (length (buffer-string)))
      (let* ((json-object-type 'hash-table)
             (json-array-type 'list)
             (json-key-type 'string)
             (stock-raw-data (json-read-from-string (buffer-string)))
             (price (stock-mode-get-price stock-raw-data))
             (cur (stock-mode-get-currency stock-raw-data)))
        (puthash (format "%s.PRICE" symbol) price stock-mode-info-hash)
        (puthash (format "%s.CUR" symbol) cur stock-mode-info-hash)
        (stock-mode-update-status)
        (force-mode-line-update 'all)))))

(defun stock-mode-fetcher-callback (symbol process event)
  (when (not (process-live-p process))
    (let ((exit-code (process-exit-status process)))
      (if (= 0 exit-code)
          (stock-mode-update-price symbol)))))

(defun stock-mode-fetch-price-for (symbol)
  (let ((stock-buffer (format "*stock-%s*" symbol))
        (saved-symbol symbol))
    (with-current-buffer (get-buffer-create stock-buffer)
      (erase-buffer)
      (make-process :name "stock-fetcher"
                    :buffer stock-buffer
                    :command (list "curl" "-s"
                                   (format stock-mode-yahoo-finance-api symbol))
                    :sentinel (lambda (p e) (stock-mode-fetcher-callback saved-symbol p e))))))

(defun stock-mode-update ()
  (interactive)
  (mapcar 'stock-mode-fetch-price-for stock-mode-symbols))

(define-minor-mode stock-mode
  "stock-mode"
  :lighter ""
  (and stock-mode-update-timer (cancel-timer stock-mode-update-timer))
  (setq stock-mode-update-timer nil)
  (setq stock-mode-status-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (when stock-mode
    (setq stock-mode-status-string "")
    (or (memq 'stock-mode-status-string global-mode-string)
        (setq global-mode-string
              (append global-mode-string '(stock-mode-status-string))))
    (setq stock-mode-update-timer
          (run-at-time "0 sec" stock-mode-update-interval
                       'stock-mode-update))))

(provide 'stock-mode)
