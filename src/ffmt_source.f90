module ffmt_source
!! A source text read for formatting: its tokens, its statements and which of them formatter
!! directives protect. Every pass that works on the text (formatting, line splitting, relational
!! spelling, `use` sorting) starts from one of these.
use ffmt_tokens, only: token_list_t
use ffmt_lexer, only: tokenize
use ffmt_statements, only: statement_list_t, statement_view_t, set_view, split_statements, SK_CODE
use ffmt_directives, only: protected_items
implicit none
private

public :: parse_source, statement_views, set_views

type, public :: source_t
    !! A source text with its tokens and statements.
    character(len=:), allocatable :: text   !! the text
    type(token_list_t) :: tokens            !! its tokens
    type(statement_list_t) :: stmts         !! its statements and standalone lines (items)
    logical, allocatable :: keep(:)         !! for each item: protected by a formatter directive, so it
                                            !! is written as it is
end type source_t

contains

subroutine parse_source(text, s)
    !! Read `text` into `s`: tokenize it, group the tokens into items and find the protected ones.
    character(len=*), intent(in) :: text   !! the source text
    type(source_t), intent(out) :: s       !! the result
    s%text = text
    call tokenize(s%text, s%tokens)
    call split_statements(s%text, s%tokens, s%stmts)
    s%keep = protected_items(s%text, s%tokens, s%stmts)
end subroutine parse_source

function statement_views(s) result(views)
    !! The view of each item of `s` (see [[set_views]], which fills them in place, without a copy).
    type(source_t), intent(in) :: s     !! the source
    type(statement_view_t), allocatable :: views(:)
    call set_views(s, views)
end function statement_views

subroutine set_views(s, views)
    !! Make `views` the view of each item of `s` (see [[statement_view_t]]); empty for items that are
    !! not code.
    type(source_t), intent(in) :: s                                 !! the source
    type(statement_view_t), allocatable, intent(out) :: views(:)    !! the views
    integer :: k
    allocate (views(s%stmts%n))
    do k = 1, s%stmts%n
        if (s%stmts%s(k)%kind == SK_CODE) call set_view(s%text, s%tokens, s%stmts%s(k), views(k))
    end do
end subroutine set_views

end module ffmt_source
