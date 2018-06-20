(TeX-add-style-hook
 "configuration"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "inputenc"
    "fontenc"
    "graphicx"
    "grffile"
    "longtable"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "textcomp"
    "amssymb"
    "capt-of"
    "hyperref")
   (LaTeX-add-labels
    "sec:orgb134231"
    "sec:orga49d08b"
    "sec:org413b11b"
    "sec:orged5eef5"
    "sec:orga9c4b2b"
    "sec:org3420e89"
    "sec:org3fae141"
    "sec:orgccc69ed"
    "sec:org43ed115"
    "sec:orgb46e03c"
    "sec:org32f599b"
    "sec:org2e7a0f1"
    "sec:org5ff7c99"
    "sec:orga14046d"
    "sec:org8137152"
    "sec:org7edb6c5"
    "sec:orgbaefc77"
    "sec:orga81a25d"
    "sec:org0750d42"
    "sec:org3f0cd8f"
    "sec:org48a5974"
    "sec:org3909f6b"
    "sec:orgbdb454b"
    "sec:orgfe533d7"
    "sec:org7c4554b"
    "sec:org7b3134b"
    "sec:orga7c8fd1"
    "sec:org7203746"
    "sec:orgd4113d4"
    "sec:org4e00172"
    "sec:org2fc4c0f"
    "sec:orga322c04"
    "sec:org62968ca"
    "sec:orga1a59e5"
    "sec:org742df63"
    "sec:orgb5eb1ee"
    "sec:orgb06f316"
    "sec:org31cec0f"
    "sec:orgc1afe38"
    "sec:orge4f4ebb"
    "sec:orgf9b9edb"
    "sec:org6a7a503"
    "sec:org39dd0a2"
    "sec:org3b482ad"
    "sec:org2515b6d"
    "sec:org8dc4089"
    "sec:org043c99e"
    "sec:org44e6f80"
    "sec:org5f25adf"
    "sec:org369e28d"
    "sec:org39acbe7"
    "sec:orgd4c33dc"
    "sec:org4f61b43"
    "sec:org70b271b"
    "sec:orgbe33a77"
    "sec:org0cdb77a"
    "sec:org8378911"
    "sec:org9d67f6a"
    "sec:org11ac996"))
 :latex)

