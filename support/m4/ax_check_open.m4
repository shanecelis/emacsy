
AC_DEFUN([AX_CHECK_OPEN],
[
AC_REQUIRE([AC_CANONICAL_HOST])
AS_CASE([${host}],
        [*-darwin*],
        [
        AC_PATH_PROG([OPEN], open)
        AC_SUBST(OPEN)
        ],
        [
        AC_PATH_PROG([OPEN], gnome-open)
        AC_SUBST(OPEN)
        ])
])
