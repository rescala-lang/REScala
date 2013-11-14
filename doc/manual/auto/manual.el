(TeX-add-style-hook "manual"
 (lambda ()
    (LaTeX-add-bibliographies)
    (LaTeX-add-environments
     "bluetext"
     "redtext")
    (LaTeX-add-labels
     "sum"
     "nomore"
     "update"
     "sumS"
     "updated"
     "sigexpr"
     "conversion"
     "increasetime"
     "sec:technicalities"
     "sec:pitfalls"
     "assign"
     "same"
     "sec:related")
    (TeX-add-symbols
     '("str" 1)
     '("gs" 1)
     '("jd" 1)
     '("mm" 1)
     '("commentbygerold" 1)
     '("bygerold" 1)
     '("codefs" 1)
     '("code" 1)
     "REScala")
    (TeX-run-style-hooks
     "xspace"
     "listings"
     "color"
     "verbatim"
     "todonotes"
     "multirow"
     "float"
     "booktabs"
     "times"
     "subfigure"
     "babel"
     "USenglish"
     "bussproofs"
     "amsmath"
     "bm"
     "amssymb"
     "array"
     "epsfig"
     "graphicx"
     "algorithm2e"
     "linesnumbered"
     "ruled"
     "vlined"
     "latex2e"
     "art10"
     "article"
     "10pt"
     "a4paper")))

