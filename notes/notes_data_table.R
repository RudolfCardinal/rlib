#!/usr/bin/env Rscript

# =============================================================================
# data.table
# =============================================================================

library(data.table)
# stopifnot(packageVersion("data.table") >= package_version("1.16.0"))


# -----------------------------------------------------------------------------
# Joins in data.table (RIGHT OUTER JOIN, INNER JOIN, etc.): generic
# -----------------------------------------------------------------------------
#
# See also:
# - Joins in data.table:
#   https://medium.com/analytics-vidhya/r-data-table-joins-48f00b46ce29
# - Definitions of joins, from an SQL context:
#   https://en.wikipedia.org/wiki/Join_(SQL)
# - Left join by modification in place:
#   https://stackoverflow.com/questions/34598139/left-join-using-data-table

test_data_table_joins <- function() {
    # testing with data.table 1.16.0
    staff <- data.table(
        staffid = 1:12,
        staffname = c(
            "Alice", "Bob", "Celia", "Daniel", "Elizabeth", "Frank",
            "Georgina", "Harold", "Isabelle", "Jack", "Kiera", "Lance"
        ),
        deptcode = c(1:5, 15, 1:5, 15),
        salary = 101:112
    )
    setkey(staff, staffid)
    print(staff)

        # Key: <staffid>
        #     staffid staffname deptcode salary
        #       <int>    <char>    <num>  <int>
        #  1:       1     Alice        1    101
        #  2:       2       Bob        2    102
        #  3:       3     Celia        3    103
        #  4:       4    Daniel        4    104
        #  5:       5 Elizabeth        5    105
        #  6:       6     Frank       15    106
        #  7:       7  Georgina        1    107
        #  8:       8    Harold        2    108
        #  9:       9  Isabelle        3    109
        # 10:      10      Jack        4    110
        # 11:      11     Kiera        5    111
        # 12:      12     Lance       15    112

    depts <- data.table(
        deptcode = c(1:7),
        deptname = c(
            "Astronomy", "Biology", "Chemistry", "Dentistry",
            "Engineering", "French", "Geography"
        )
    )
    setkey(depts, deptcode)
    print(depts)

        # Key: <deptcode>
        #    deptcode    deptname
        #       <int>      <char>
        # 1:        1   Astronomy
        # 2:        2     Biology
        # 3:        3   Chemistry
        # 4:        4   Dentistry
        # 5:        5 Engineering
        # 6:        6      French
        # 7:        7   Geography

    # Now, be very careful.
    #
    # x[i] is: "x RIGHT OUTER JOIN i", or "i LEFT OUTER JOIN x".
    #
    # The default value of "nomatch" is NA, meaning that if a row in i has no match
    # in x, NA is returned [for the x values].
    #
    # x[i, nomatch = NULL] is: "x INNER JOIN i", or "i INNER JOIN x".
    #
    # With nomatch = NULL, then if a row in i has no match in x, no rows are
    # returned.

    print(staff[depts, on = .(deptcode)])
    # "staff RIGHT OUTER JOIN depts", or "depts LEFT OUTER JOIN staff"
    # We get all the depts, and each can match 0, 1, or many rows of staff.
    # (Note the ordering is first by depts$deptcode, i.e. by the key for depts.)
    # We do not get staff without a matching lookup (Frank and Lance are missing).
    # It is department-focused, not staff-focused.

        #     staffid staffname deptcode salary    deptname
        #       <int>    <char>    <int>  <int>      <char>
        #  1:       1     Alice        1    101   Astronomy
        #  2:       7  Georgina        1    107   Astronomy
        #  3:       2       Bob        2    102     Biology
        #  4:       8    Harold        2    108     Biology
        #  5:       3     Celia        3    103   Chemistry
        #  6:       9  Isabelle        3    109   Chemistry
        #  7:       4    Daniel        4    104   Dentistry
        #  8:      10      Jack        4    110   Dentistry
        #  9:       5 Elizabeth        5    105 Engineering
        # 10:      11     Kiera        5    111 Engineering
        # 11:      NA      <NA>        6     NA      French
        # 12:      NA      <NA>        7     NA   Geography

    print(staff[depts, on = .(deptcode), nomatch = NULL])
    # "staff INNER JOIN depts", or "depts INNER JOIN staff"
    # We get all the staff and all the depts (combined), where there is a match.
    # We don't get staff without a department, or departments without staff.
    # (Note the ordering is first by depts$deptcode, i.e. by the key for depts.)

        #     staffid staffname deptcode salary    deptname
        #       <int>    <char>    <int>  <int>      <char>
        #  1:       1     Alice        1    101   Astronomy
        #  2:       7  Georgina        1    107   Astronomy
        #  3:       2       Bob        2    102     Biology
        #  4:       8    Harold        2    108     Biology
        #  5:       3     Celia        3    103   Chemistry
        #  6:       9  Isabelle        3    109   Chemistry
        #  7:       4    Daniel        4    104   Dentistry
        #  8:      10      Jack        4    110   Dentistry
        #  9:       5 Elizabeth        5    105 Engineering
        # 10:      11     Kiera        5    111 Engineering

    print(depts[staff, on = .(deptcode)])
    # "depts RIGHT OUTER JOIN staff", or "staff LEFT OUTER JOIN depts"
    # We get all the staff, with lookup information attached where present (and
    # NA where absent).
    # (Note the ordering is first by staff$staffid, i.e. by the key for staff.)

        #     deptcode    deptname staffid staffname salary
        #        <int>      <char>   <int>    <char>  <int>
        #  1:        1   Astronomy       1     Alice    101
        #  2:        2     Biology       2       Bob    102
        #  3:        3   Chemistry       3     Celia    103
        #  4:        4   Dentistry       4    Daniel    104
        #  5:        5 Engineering       5 Elizabeth    105
        #  6:       15        <NA>       6     Frank    106
        #  7:        1   Astronomy       7  Georgina    107
        #  8:        2     Biology       8    Harold    108
        #  9:        3   Chemistry       9  Isabelle    109
        # 10:        4   Dentistry      10      Jack    110
        # 11:        5 Engineering      11     Kiera    111
        # 12:       15        <NA>      12     Lance    112

    print(depts[staff, on = .(deptcode), nomatch = NULL])
    # "depts INNER JOIN staff", or "staff INNER JOIN depts"
    # As above, except for ordering.
    # (Note the ordering is first by staff$staffid, i.e. by the key for staff.)

        #     deptcode    deptname staffid staffname salary
        #        <int>      <char>   <int>    <char>  <int>
        #  1:        1   Astronomy       1     Alice    101
        #  2:        2     Biology       2       Bob    102
        #  3:        3   Chemistry       3     Celia    103
        #  4:        4   Dentistry       4    Daniel    104
        #  5:        5 Engineering       5 Elizabeth    105
        #  6:        1   Astronomy       7  Georgina    107
        #  7:        2     Biology       8    Harold    108
        #  8:        3   Chemistry       9  Isabelle    109
        #  9:        4   Dentistry      10      Jack    110
        # 10:        5 Engineering      11     Kiera    111

    staffcopy <- copy(staff)
    staffcopy[depts, deptname := i.deptname, on = .(deptcode)]
    print(staffcopy)
    # Effectively, "staffcopy LEFT JOIN depts". This is an efficient way.
    # Note that modification in place via ":=" does not alter the number of rows of
    # its table.

        # Key: <staffid>
        #     staffid staffname deptcode salary    deptname
        #       <int>    <char>    <num>  <int>      <char>
        #  1:       1     Alice        1    101   Astronomy
        #  2:       2       Bob        2    102     Biology
        #  3:       3     Celia        3    103   Chemistry
        #  4:       4    Daniel        4    104   Dentistry
        #  5:       5 Elizabeth        5    105 Engineering
        #  6:       6     Frank       15    106        <NA>
        #  7:       7  Georgina        1    107   Astronomy
        #  8:       8    Harold        2    108     Biology
        #  9:       9  Isabelle        3    109   Chemistry
        # 10:      10      Jack        4    110   Dentistry
        # 11:      11     Kiera        5    111 Engineering
        # 12:      12     Lance       15    112        <NA>
}

# -----------------------------------------------------------------------------
# Joins in data.table: The application in label_patient_earliest_diagnoses()
# -----------------------------------------------------------------------------

test_data_table_joins_earliest <- function() {

    players <- data.table(pid = 1:10, sex = rep(c("M", "F"), times = 5))
    setkey(players, pid)
    games <- data.table(
        pid = rep(1:8, each=2),
        is_game = c(rep(FALSE, 4), rep(TRUE, 8), rep(FALSE, 4)),
        event_date = c(101:108, 109, NA, NA, 112, 113:116)
    )
    print(players)

        # Key: <pid>
        #       pid    sex
        #     <int> <char>
        #  1:     1      M
        #  2:     2      F
        #  3:     3      M
        #  4:     4      F
        #  5:     5      M
        #  6:     6      F
        #  7:     7      M
        #  8:     8      F
        #  9:     9      M
        # 10:    10      F

    print(games)

        #       pid is_game event_date
        #     <int>  <lgcl>      <num>
        #  1:     1   FALSE        101
        #  2:     1   FALSE        102
        #  3:     2   FALSE        103
        #  4:     2   FALSE        104
        #  5:     3    TRUE        105
        #  6:     3    TRUE        106
        #  7:     4    TRUE        107
        #  8:     4    TRUE        108
        #  9:     5    TRUE        109
        # 10:     5    TRUE         NA
        # 11:     6    TRUE         NA
        # 12:     6    TRUE        112
        # 13:     7   FALSE        113
        # 14:     7   FALSE        114
        # 15:     8   FALSE        115
        # 16:     8   FALSE        116

    # CALCULATE EARLIEST GAME FOR EACH PLAYER.
    suppressWarnings(
        earliest_games_by_player <- games[,
            .(
                earliest_game = min(
                    fifelse(is_game, event_date, NA),
                    na.rm = TRUE
                )
            ),
            by = pid
        ]
    )
    print(earliest_games_by_player)
        #      pid earliest_game
        #    <int>         <num>
        # 1:     1           Inf
        # 2:     2           Inf
        # 3:     3           105
        # 4:     4           107
        # 5:     5           109
        # 6:     6           112
        # 7:     7           Inf
        # 8:     8           Inf
    earliest_games_by_player[,
        earliest_game := fifelse(is.infinite(earliest_game), NA, earliest_game)
    ]
    print(earliest_games_by_player)
        #      pid earliest_game
        #    <int>         <num>
        # 1:     1            NA
        # 2:     2            NA
        # 3:     3           105
        # 4:     4           107
        # 5:     5           109
        # 6:     6           112
        # 7:     7            NA
        # 8:     8            NA

    # ADD TO PLAYER TABLE.

    # METHOD 1: JOIN.
    # We want all players, so for a join, players should be "i", not "x",
    # in x[i].
    all_players_with_earliest_games <- earliest_games_by_player[
        players,
        on = .(pid)
        # earliest_games_by_player RIGHT OUTER JOIN players
    ]
    setkey(all_players_with_earliest_games, pid)
    column_order <- c("pid", "sex", "earliest_game")
    setcolorder(all_players_with_earliest_games, column_order)
    print(all_players_with_earliest_games)
        # Key: <pid>
        #       pid    sex earliest_game
        #     <int> <char>         <num>
        #  1:     1      M            NA
        #  2:     2      F            NA
        #  3:     3      M           105
        #  4:     4      F           107
        #  5:     5      M           109
        #  6:     6      F           112
        #  7:     7      M            NA
        #  8:     8      F            NA
        #  9:     9      M            NA
        # 10:    10      F            NA

    # METHOD 2: MODIFY IN PLACE.
    # But to modify in place:
    in_place_players <- copy(players)  # 10 players
    in_place_players[
        earliest_games_by_player,
        earliest_game := i.earliest_game,
        on = .(pid)
        # bad_players RIGHT OUTER JOIN earliest_games_by_player; wrong
    ]
    setkey(in_place_players, pid)
    setcolorder(in_place_players, column_order)
    print(in_place_players)  # still 10 players
        # Key: <pid>
        #       pid    sex earliest_game
        #     <int> <char>         <num>
        #  1:     1      M            NA
        #  2:     2      F            NA
        #  3:     3      M           105
        #  4:     4      F           107
        #  5:     5      M           109
        #  6:     6      F           112
        #  7:     7      M            NA
        #  8:     8      F            NA
        #  9:     9      M            NA
        # 10:    10      F            NA

    # ENSURE EQUIVALENCE.
    stopifnot(all.equal(all_players_with_earliest_games, in_place_players))
}


# -----------------------------------------------------------------------------
# Progress bars
# -----------------------------------------------------------------------------
#
# - Manual methods: https://github.com/Rdatatable/data.table/issues/1409
#
# - Automatic method: available from version 1.16.0 (25 Aug 2024).
#   See https://cran.r-project.org/web/packages/data.table/news/news.html.
#   Availability is indicated by the "showProgress" option in ?data.table,
#   which defaults to:
#       showProgress = getOption("datatable.showProgress", interactive())]


test_data_table_progress_bar <- function(
    simple_by = TRUE,
    with_assign = TRUE,
    eliminate_reproduce_by_vars = TRUE
) {
    # Tested with v1.18.6.1, 12 Sep 2006.

    n <- 10
    d <- data.table(grouper = LETTERS[1:n], value = 1:n)

    groupfn1 <- function(BY, SD, sleep = 1) {
        # cat("- Sleeping for", sleep, "\n")
        Sys.sleep(sleep)
        return(list(p = 1, q = 1))
    }

    if (simple_by) {
        cat("- Testing data.table progress bar: (1) Standard 'by'\n")
        g = d[, groupfn1(.BY, .SD), by = grouper]
        print(g)
        cat("  ... outcome: DISPLAYS progress bar.\n")
    }

    if (with_assign) {
        cat("- Testing data.table progress bar: (2) Direct ':='\n")
        dc <- copy(d)
        dc[, c("p", "q") := groupfn1(.BY, .SD), by = grouper]
        print(dc)
        cat("  ... outcome: DOES NOT DISPLAY progress bar.\n")
    }

    groupfn2 <- function(BY, SD) {
        # cat("- Sleeping for", sleep, "\n")
        # cat(".BY:\n"); print(BY)
        cat(".SD:\n"); print(SD)
        return(list(p = 1, q = 1, value = SD$value))
    }

    if (eliminate_reproduce_by_vars) {
        cat("- Testing data.table progress bar: (3) .BY not used, only .SD\n")
        g = d[, groupfn2(.BY, .SD), by = grouper, .SDcols = c("value")]
        print(g)
        # cat("  ... outcome: DISPLAYS progress bar.\n")
    }
}


# -----------------------------------------------------------------------------
# List encapsulation
# -----------------------------------------------------------------------------

test_list_encapsulation <- function() {
    # List wrapping within tables.
    #
    # See also: https://stackoverflow.com/questions/22531477/.

    line1 <- paste(c(rep("=", 79), "\n"), collapse = "")
    line2 <- paste(c(rep("-", 79), "\n"), collapse = "")
    cat1 <- function(...) { cat(line1, ..., "\n", line1, sep = "") }
    cat2 <- function(...) { cat(line2, ..., "\n", line2, sep = "") }

    base_d <- data.table(p = rep(1:3, times = 2), q = rep(10:11, each = 3))
    setkeyv(base_d, c("p", "q"))
    cat1("- Starting d:")
    print(base_d)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cat1(
        "+++ ONE list(): data.table uses list(.) to look for values to assign",
        " to columns by reference; i.e. expanded over columns\n"
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    d <- copy(base_d)
    cat2(" d[, x = list(5), by = .(p, q)] -- error ...")
    # print(d[, x = list(5), by = .(p, q)])

    d <- copy(base_d)
    cat2(" d[, x = list(5, 6), by = .(p, q)] -- error -- unused argument")
    # print(d[, x = list(5, 6), by = .(p, q)])

    d <- copy(base_d)
    cat2(" d[, x := list(5, 6), by = .(p, q)]:")
    d[, x := list(5, 6), by = .(p, q)]
    print(d)

    d <- copy(base_d)
    cat2('- d[, list(x = 5, y = 6), by = .(p, q)]:')
    print(d[, list(x = 5, y = 6), by = .(p, q)])

    d <- copy(base_d)
    cat2('- d[, c("x", "y") := list(5, 6), by = .(p, q)]:')
    d[, c("x", "y") := list(5, 6), by = .(p, q)]
    print(d)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cat1("+++ TWO lists(): outer expanded over columns, inner expanded over rows")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    d <- copy(base_d)
    cat2(" d[, x = list(list(5, 6)), by = .(p, q)] -- error -- unused argument")
    # print(d[, x = list(list(5, 6)), by = .(p, q)])

    d <- copy(base_d)
    cat2(" d[, list(x = list(5, 6), y = list(7, 8)), by = .(p, q)]:")
    print(d[, list(x = list(5, 6), y = list(7, 8)), by = .(p, q)])

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cat1("+++ THREE lists(): the innermost creates a list of items in each cell")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    d <- copy(base_d)
    cat2('- d[, x := list(list(list(5, 6))), by = .(p, q)]:')
    d[, x := list(list(list(5, 6))), by = .(p, q)]

    print(d)
    cat('... d[1, ]$q:\n')
    print(d[1, ]$q)
    cat('... d[1, ]$q[[1]]:\n')
    print(d[1, ]$q[[1]])

    # https://stackoverflow.com/questions/22531477/using-lists-inside-data-table-columns
    # ... but I think the last one does not apply within `:=`().
    # So we remove one list level.
}
