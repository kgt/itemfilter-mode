;;; itemfilter-mode.el --- Major mode for editing Path of Exile item filters -*- lexical-binding: t; -*-

;; Copyright (C) 2024 kgt <kagetsu.34@gmail.com>

;; Author: kgt <kagetsu.34@gmail.com>
;; Version: 1.0
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for editing Path of Exile item filters.

;;; Code:

(defgroup itemfilter nil
  "Major mode for editing Path of Exile item filters."
  :prefix "itemfilter-"
  :group 'data)

(defcustom itemfilter-indent-level 4
  "Number of spaces for indentation."
  :type 'integer
  :safe 'integerp)

(defconst itemfilter-block-keywords
  '("Show"
    "Hide"
    "Minimal"))

(defconst itemfilter-block-like-keywords
  '("Import"))

(defconst itemfilter-condition-keywords
  '("AlternateQuality"
    "AnyEnchantment"
    "ArchnemesisMod"
    "AreaLevel"
    "BaseArmour"
    "BaseDefencePercentile"
    "BaseEnergyShield"
    "BaseEvasion"
    "BaseType"
    "BaseWard"
    "BlightedMap"
    "Class"
    "Corrupted"
    "CorruptedMods"
    "DropLevel"
    "ElderItem"
    "ElderMap"
    "EnchantmentPassiveNode"
    "EnchantmentPassiveNum"
    "FracturedItem"
    "GemLevel"
    "HasCruciblePassiveTree"
    "HasEaterOfWorldsImplicit"
    "HasEnchantment"
    "HasExplicitMod"
    "HasImplicitMod"
    "HasInfluence"
    "HasSearingExarchImplicit"
    "Height"
    "Identified"
    "ItemLevel"
    "LinkedSockets"
    "MapTier"
    "Mirrored"
    "Quality"
    "Rarity"
    "Replica"
    "Scourged"
    "ShapedMap"
    "ShaperItem"
    "SocketGroup"
    "Sockets"
    "StackSize"
    "SynthesisedItem"
    "TransfiguredGem"
    "UberBlightedMap"
    "Width"))

(defconst itemfilter-action-keywords
  '("PlayAlertSound"
    "PlayAlertSoundPositional"
    "CustomAlertSound"
    "CustomAlertSoundOptional"
    "DisableDropSound"
    "EnableDropSound"
    "DisableDropSoundIfAlertSound"
    "EnableDropSoundIfAlertSound"
    "MinimapIcon"
    "PlayEffect"
    "SetBackgroundColor"
    "SetBorderColor"
    "SetFontSize"
    "SetTextColor"))

(defconst itemfilter-action-like-keywords
  '("Continue"))

(defconst itemfilter-constant-keywords
  '("True"
    "False"
    "None"
    ;; Import
    "Optional"
    ;; Rarity
    "Normal"
    "Magic"
    "Rare"
    "Unique"
    ;; HasInfluence
    "Shaper"
    "Elder"
    "Crusader"
    "Hunter"
    "Redeemer"
    "Warlord"
    ;; MinimapIcon, PlayEffect - Colour
    "Red"
    "Green"
    "Blue"
    "Brown"
    "White"
    "Yellow"
    "Cyan"
    "Grey"
    "Orange"
    "Pink"
    "Purple"
    ;; MinimapIcon - Shape
    "Circle"
    "Diamond"
    "Hexagon"
    "Square"
    "Star"
    "Triangle"
    "Cross"
    "Moon"
    "Raindrop"
    "Kite"
    "Pentagon"
    "UpsideDownHouse"
    ;; PlayEffect - Temp
    "Temp"))

(defconst itemfilter-block-re
  (concat "^\\s-*" (regexp-opt itemfilter-block-keywords t) "\\>"))

(defconst itemfilter-block-like-re
  (concat "^\\s-*"
	  (regexp-opt (append itemfilter-block-keywords
			      itemfilter-block-like-keywords)
		      t)
	  "\\>"))

(defconst itemfilter-condition-re
  (concat "^\\s-*" (regexp-opt itemfilter-condition-keywords t) "\\>"))

(defconst itemfilter-action-re
  (concat "^\\s-*" (regexp-opt itemfilter-action-keywords t) "\\>"))

(defconst itemfilter-action-like-re
  (concat "^\\s-*" (regexp-opt itemfilter-action-like-keywords t) "\\>"))

(defconst itemfilter-indent-re
  (concat "^\\s-*"
	  (regexp-opt (append itemfilter-condition-keywords
			      itemfilter-action-keywords
			      itemfilter-action-like-keywords))
	  "\\>"))

(defconst itemfilter-constant-re
  (concat "\\<" (regexp-opt itemfilter-constant-keywords) "\\>"))

(defvar itemfilter-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table))

(defvar itemfilter-font-lock-keywords
  `((,itemfilter-block-like-re 1 font-lock-type-face)
    (,itemfilter-condition-re 1 font-lock-variable-name-face)
    (,itemfilter-action-re 1 font-lock-function-name-face)
    (,itemfilter-action-like-re 1 font-lock-negation-char-face)
    (,itemfilter-constant-re . font-lock-keyword-face)))

(defun itemfilter-indent-line ()
  "Indent the current line."
  (interactive)
  (let ((indent (itemfilter--calculate-indent)))
    (when indent
      (save-excursion
	(indent-line-to indent))))
  (when (< (current-column) (current-indentation))
    (skip-chars-forward " \t")))

(defun itemfilter--calculate-indent ()
  "Return the indentation level of the current line."
  (let ((case-fold-search nil))
    (save-excursion
      (forward-line 0)
      (cond
       ;; block line
       ((looking-at-p itemfilter-block-like-re)
	0)
       ;; condition or action line
       ((looking-at-p itemfilter-indent-re)
	itemfilter-indent-level)
       ;; For a blank or comment line, test the previous line.
       ((= (forward-line -1) 0)
	(cond
	 ;; line in a block
	 ((or (looking-at-p itemfilter-block-re)
	      (looking-at-p itemfilter-indent-re))
	  itemfilter-indent-level)
	 ;; line after a comment line
	 ((looking-at-p "^\\s-*\\s<")
	  (current-indentation))))))))

;;;###autoload
(define-derived-mode itemfilter-mode prog-mode "ItemFilter"
  "Major mode for editing Path of Exile item filters."
  (setq-local comment-start "# ")
  (setq-local comment-end "")

  (setq font-lock-defaults '(itemfilter-font-lock-keywords))

  (setq tab-width itemfilter-indent-level)
  (setq-local indent-line-function 'itemfilter-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(?:ruthless\\)?filter\\'" . itemfilter-mode))

(provide 'itemfilter-mode)

;;; itemfilter-mode.el ends here
