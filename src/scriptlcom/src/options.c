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
#include <string.h>
#include <unistd.h>
#include <getopt.h>

#include "scriptl.h"

enum long_options {
    SL_VERSION
};

static int long_opt = 0;

static struct option options[] = {
    { "sl-version", required_argument, &long_opt, SL_VERSION },
    { "funcall",    required_argument, 0, 'f' },
    { "eval",       required_argument, 0, 'e' },
    { "load",       required_argument, 0, 'l' },
    { "errors",     required_argument, 0, 'E' },
    { "host",       required_argument, 0, 'H' },
    { "port",       required_argument, 0, 'P' },
    { "uds",        optional_argument, 0, 'U' },
    { "help",       no_argument, 0, 'h' },
    { 0, 0, 0, 0 }
};

static char default_uds[FILENAME_MAX+1] = { 0 };

void scl_default_config(scl_config_t *config) {
    int cur = FILENAME_MAX;
    strncat(default_uds, getenv("HOME"), cur);   cur -= strlen(default_uds);
    strncat(default_uds, "/", cur);              cur--;
    strncat(default_uds, DEFAULT_UDS_FILE, cur);

    config->version  = DEFAULT_VERSION;
    config->op       = OP_NOOP;
    config->conntype = CON_UDS;
    config->function = NULL;
    config->errors   = NULL;
    config->system   = NULL;
    config->host     = DEFAULT_HOST;
    config->port     = DEFAULT_PORT;
    config->uds_path = default_uds;
}

static void usage(void) {
    printf("Usage: scriptlcom [options ...] {operation ARG} [args ...]\n\n"
           "       scriptlcom [--funcall FUNCTION] args ...\n"
           "       scriptlcom [--eval STRING]\n"
           "\n"
           "Operations:\n"
           "      -e, --eval       Eval some lisp\n"
           "      -f, --funcall    Call a function\n"
           "\n"
           "Options:\n"
           "      -E, --errors     Errors handled by the specified function\n"
           "      -H, --host       Specify a host\n"
           "      -P, --port       Specify a port\n"
           "      -I, --inet       Use an inet socket, connecting to --host and --port;\n"
           "                       defaults to %s:%s\n"
           "      -U, --uds        Use a Unix Domain Socket (with optional filename);\n"
           "                       defaults to %s/%s\n"
           "      -l, --load       ASDF:LOAD-SYSTEM before a FUNCALL\n"
           "\n"
           "      --sl-version     Call using a specific ScriptL version\n"
           "\n"
           "      -h, --help       Show this message\n",
           DEFAULT_HOST, DEFAULT_PORT, getenv("HOME"), DEFAULT_UDS_FILE);
}

int scl_process_args(int argc, char *argv[], scl_config_t *config) {
    int o, index;

    config->script = argv[0];

    for(;;) {
        o = getopt_long(argc, argv, "e:f:l:E:H:P:IU::h", options, &index);

        if(o == -1)
            break;

        switch(o) {
            case 'f':
                config->op       = OP_FUNCALL;
                config->function = optarg;
                break;

            case 'e':
                config->op       = OP_EVAL;
                config->function = optarg;
                break;

            case 'l':
                config->system = optarg;
                break;

            case SL_VERSION:
                config->version = strtol(optarg, NULL, 10);
                break;

            case 'E':
                config->errors = optarg;
                break;

            case 'H':
                config->host = optarg;
                break;
                
            case 'P':
                config->port = optarg;
                break;

            case 'I':
                config->conntype = CON_INET;
                break;

            case 'U':
                config->conntype = CON_UDS;
                if(optarg)
                    config->uds_path = optarg;
                break;

            case 'h':
            default:
                usage();
                exit(1);
        }
    }

    if(config->op == OP_NOOP) {
        printf("%s: No operation specified.\n", argv[0]);
        usage();
        exit(1);
    }

    config->argc = argc - optind;
    config->argv = &argv[optind];

    return index;
}
