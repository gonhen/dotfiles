execute pathogen#infect()
syntax on
filetype plugin indent on

set t_Co=256
set nocompatible 
set wildmenu 
set cursorline 
set number
set tabstop=4

"""Colorscheme
set termguicolors
set background=dark
colorscheme solarized8

"""Airline
set encoding=utf-8
let g:airline_powerline_fonts=1
let g:airline_theme='solarized'

"""BASIC TOOLS
"Navigating with guides
inoremap <Space><Tab> <Esc>/<++><Enter>"_c4l
vnoremap <Space><Tab> <Esc>/<++><Enter>"_c4l
map <Space><Tab> <Esc>/<++><Enter>"_c4l
inoremap ;gui <++>

"""Goyo and Limelight
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!

"""vim-pencil
let g:pencil#wrapModeDefault = 'soft'
augroup pencil
  autocmd!
  autocmd FileType markdown,mkd call pencil#init()
  autocmd FileType text         call pencil#init()
  autocmd FileType tex          call pencil#init()
augroup END

"""vimwiki calendar
au BufRead,BufNewFile *.wiki set filetype=vimwiki
:autocmd FileType vimwiki map d :VimwikiMakeDiaryNote
function! ToggleCalendar()
  execute ":Calendar"
  if exists("g:calendar_open")
    if g:calendar_open == 1
      execute "q"
      unlet g:calendar_open
    else
      g:calendar_open = 1
    end
  else
    let g:calendar_open = 1
  end
endfunction
:autocmd FileType vimwiki map c :call ToggleCalendar()

"""vimwiki Dropbox
let wiki = {}
let g:vimwikidir = $HOME . "/Dropbox/vimwiki"
let wiki.path = g:vimwikidir
let g:vimwiki_list=[wiki]

"""latex filetype
let g:tex_flavor = "latex"

"""latex header
autocmd bufnewfile *.tex so /home/gh/Documents/Header/latex_header.txt
autocmd bufnewfile *.tex exe "1," . 10 . "g/File Name :.*/s//File Name : " .expand("%")
autocmd bufnewfile *.tex exe "1," . 10 . "g/Creation Date :.*/s//Creation Date : " .strftime("%d-%m-%Y")
autocmd Bufwritepre,filewritepre *.tex execute "normal ma"
autocmd Bufwritepre,filewritepre *.tex exe "1," . 10 . "g/Last Modified :.*/s/Last Modified :.*/Last Modified : " .strftime("%c")
autocmd bufwritepost,filewritepost *.tex execute "normal `a"

"""latex shortcuts 
autocmd FileType tex inoremap ;dc \documentclass[]{<++>}<++><Esc>T[i
autocmd FileType tex inoremap ;ip \input{}<++><Esc>T{i
autocmd FileType tex inoremap ;% %-----------------------------------------------------------
autocmd FileType tex inoremap ;up \usepackage[]{<++>}<++><Esc>T[i
autocmd FileType tex inoremap ;sec \section{}<Enter><Enter><++><Esc>2kf}i
autocmd FileType tex inoremap ;ssec \subsection{}<Enter><Enter><++><Esc>2kf}i
autocmd FileType tex inoremap ;bi \begin{itemize}<Enter><Enter>\end{itemize}<Enter><Enter><++><Esc>3kA\item<Space>
autocmd FileType tex inoremap ;i <Enter>\item<Space>

"""Markdown
autocmd Filetype rmd map <F5> :!echo<space>"require(rmarkdown);<space>render('<c-r>%')"<space>\|<space>R<space>--vanilla<enter>
autocmd Filetype markdown map <F5> :!pandoc<space><C-r>%<space>-o<space><C-r>%.pdf<Enter><Enter>
