/*
 * scriptlcom
 * Copyright (C) 2012  Ryan Pavlik
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser General Public
 * License (LGPL) version 2.1 which accompanies this distribution, and
 * is available at http://www.gnu.org/licenses/lgpl-2.1.html
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

#include "scriptl.h"

#ifdef HAVE_LIBREADLINE

#include <readline/readline.h>

char *scl_readline(char *prompt) {
    return readline(prompt);
}

void scl_addhistory(char *line) {
    add_history(line);
}

#else /* HAVE_LIBREADLINE */

char *scl_readline(char *prompt) {
    printf("%s", prompt);
    return scl_read_line(STDIN_FILENO);
}

void scl_addhistory(char *line) {

}

#endif /* HAVE_LIBREADLINE */
