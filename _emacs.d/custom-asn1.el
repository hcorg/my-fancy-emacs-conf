;;; ASN.1 Mode
(autoload 'asn1-diff-mode "asn1-diff"
  "Major mode for editing comparison of ASN.1 specifications." t)
(autoload 'asn1-diff "asn1-diff"
  "For comparing ASN.1 specifications." t)
(autoload 'asn1-mode "asn1-mode"
  "Major mode for editing ASN.1 specifications." t)
(setq auto-mode-alist
      (cons '("\\.[Aa][Ss][Nn]\\([1]\\|[pP][pP]?\\)?$" . asn1-mode)
            auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.[Aa][Ss][Nn][dD]$" . asn1-diff-mode2) auto-mode-alist))
