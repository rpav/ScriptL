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

int main(int argc, char *argv[]) {
    scl_config_t config;
    int sockfd = 0, index = 0;

    scl_default_config(&config);
    scl_process_args(argc, argv, &config);

    sockfd = scl_connect(&config);

    if(sockfd == 0) {
        printf("scriptlcom: can't connect\n");
        exit(1);
    }

    switch(config.op) {
        case OP_EVAL:
            scl_eval1(&config, sockfd);
            break;

        case OP_FUNCALL:
            switch(config.version) {
                case 1: scl_funcall1(&config, sockfd); break;
                case 2: scl_funcall2(&config, sockfd); break;
                default:
                    printf("Unknown version for funcall: %d\n",
                           config.version);
            }
            break;
    }

    if(sockfd)
        close(sockfd);

    return 0;
}
