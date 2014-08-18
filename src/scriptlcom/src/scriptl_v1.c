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

/* Crappy mimicking of the original shell version */

void scl_write_cwd_v1(int fd) {
    char dirbuf[FILENAME_MAX+1] = { 0 };
    
    if(!getcwd(dirbuf, FILENAME_MAX+1)) {
        printf("scriptlcom: getcwd: path too long\n");
        exit(1);
    }

    scl_writef_packet(fd, "(:cwd #P\"%s/\")", dirbuf);
}

void scl_eval1(scl_config_t *config, int fd) {
    char *status = NULL, *ret = NULL, *out = NULL;

    scl_write_packet(fd, "(:scriptl 1)", 0);
    scl_write_cwd_v1(fd);
    scl_writef_packet(fd, "(:eval #P\"%s/\")", config->script);
    scl_write_packet(fd, "(:errors nil)", 0);
    scl_write_packet(fd, config->function, 0);

    status = scl_read_packet(fd, NULL);

    if(!strcmp(status, ":ok")) {
        ret = scl_read_packet(fd, NULL);
        out = scl_read_packet(fd, NULL);

        if(out && strlen(out) > 0)
            printf("%s\n", out);
        else
            printf("%s\n", ret);
    } else if(!strcmp(status, ":error")) {
        ret = scl_read_packet(fd, NULL);
        out = scl_read_packet(fd, NULL);

        printf("Error: %s\n\n%s\n", ret, out);
    }

    free(ret);
    free(out);
    free(status);
}

void scl_funcall1(scl_config_t *config, int fd) {
    char *status = NULL, *ret = NULL, *out = NULL;
    int i = 0;

    scl_write_packet(fd, "(:scriptl 1)", 0);
    scl_write_cwd_v1(fd);    
    scl_writef_packet(fd, "(:funcall %d #P\"%s/\")", config->argc,
                      config->script);
    scl_writef_packet(fd, "(:errors %s)",
                      config->errors ? config->errors : "nil");
    scl_write_packet(fd, config->function, 0);

    for(i = 0; i < config->argc; i++)
        scl_write_packet(fd, config->argv[i], 0);

    status = scl_read_packet(fd, NULL);

    if(!strcmp(status, ":ok")) {
        ret = scl_read_packet(fd, NULL);
        out = scl_read_packet(fd, NULL);

        if(out && strlen(out) > 0)
            printf("%s\n", out);
        else
            printf("%s\n", ret);
    } else if(!strcmp(status, ":error")) {
        ret = scl_read_packet(fd, NULL);
        out = scl_read_packet(fd, NULL);

        printf("Error: %s\n\n%s\n", ret, out);
    }

    free(ret);
    free(out);
    free(status);
}
