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

#ifndef SCRIPTL_H
#define SCRIPTL_H

typedef enum {
    OP_NOOP = 0,
    OP_EVAL,
    OP_FUNCALL,

    OP_MAX
} scl_op_t;

typedef enum {
    CON_INET,
    CON_UDS
} scl_conntype_t;

typedef struct scl_config {
    unsigned int version;

    scl_conntype_t conntype;
    char *host;
    char *port;
    char *uds_path;

    int argc;
    char **argv;

    scl_op_t op;
    char *system;
    char *script;
    char *function;
    char *errors;
} scl_config_t;

#define DEFAULT_VERSION 2
#define DEFAULT_CONNTYPE CON_UDS
#define DEFAULT_HOST "localhost"
#define DEFAULT_PORT "4010"
#define DEFAULT_UDS_FILE ".scriptl-sock"

/* Config */
void scl_default_config(scl_config_t *config);
int scl_process_args(int argc, char *argv[], scl_config_t *config);

/* Misc */

char *scl_readline(char *prompt);
void scl_addhistory(char *line);

/* Net */
int scl_connect(scl_config_t *config);
int scl_read_n(int fd, char *buf, size_t len);
int scl_write_n(int fd, char *buf, size_t len);
int scl_write_packet(int fd, char *str, size_t len);
char *scl_read_packet(int fd, int *len);
char *scl_read_line(int fd);
int scl_write_str(int fd, char *str);
int scl_write_line(int fd, char *str);
int scl_writef(int fd, const char *fmt, ...);
int scl_writef_packet(int fd, const char *fmt, ...);

/* V1 stuff */

void scl_eval1(scl_config_t *config, int sockfd);
void scl_funcall1(scl_config_t *config, int sockfd);

/* V2 stuff */
void scl_funcall2(scl_config_t *config, int sockfd);

#endif /* SCRIPTL_H */
