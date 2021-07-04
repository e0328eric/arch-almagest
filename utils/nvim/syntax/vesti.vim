syntax match vestiDelimiter '[(){}\[\]|\.,:;]\+'

" Comments
syntax region vestiComment start="#" end="$"
syntax region vestiComment start="#\*" end="\*#"
syntax region vestiVerbatim start="#-" end="-#"
syntax region vestiVerbatimInline start="##-" end="-##"

" docclass
syntax keyword vestiDocClass docclass nextgroup=vestiTypeDef skipwhite skipempty
syntax keyword vestiDocument document skipwhite skipempty

syntax match vestiTypeDef ' \v[A-Z][A-Za-z0-9]*'
            \ contained
            \ nextgroup=vestiTypeDefParams

syntax region vestiTypeDefParams
            \ matchgroup=vestiDelimiter
            \ start='('
            \ end=')'
            \ keepend
            \ contains=TOP

highlight default link vestiComment Comment
highlight default link vestiVerbatim PreProc
highlight default link vestiVerbatimInline PreProc
highlight default link vestiDocClass Identifier
highlight default link vestiDocument Identifier
highlight default link vestiTypeDef Label

let b:current_syntax = "vesti"
