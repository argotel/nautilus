/*
 * The author of this software is William Dorsey.
 * Copyright (c) 1993, 1994, 1995 by William Dorsey.  All rights reserved.
 *
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, THE AUTHOR DOES NOT MAKE ANY CLAIM OR
 * WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
 * ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/* config.c - configuration file reader and parsing functions
 *
 * SCCS ID: @(#)config.c 1.1 96/02/29
 *
 * REVISION HISTORY
 *
 * DATE      RESPONSIBLE PARTY  DESCRIPTION
 * -------------------------------------------------------------------------
 * 93/12/31  B. Dosrey          Module created
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include "nautilus.h"

extern long     strtol();
extern struct config_t config_tbl[];

enum tokens {
    IDENT, ASSIGN, NUMERIC, STRING, EOF_T, ERROR
};

struct token_t {
    enum tokens     type;
    union {
	int             numeric;
	char           *string;
    }               value;
};

struct status_t {
    int             status;
    char           *msg;
};

typedef struct token_t TOKEN;
typedef struct status_t STATUS;

static TOKEN   *
Lex(FILE * input)
{
    int             c, hflg = 0, state = 0;
    char            *tp;
    static TOKEN    token;
    static char     id[256];

    tp = id;
    while ((c = getc(input)) != EOF) {
	switch (state) {
	case 0:
	    if (c == '#') {
		state = 1;
	    }
	    else if (isalpha(c)) {
		state = 2;
		*tp++ = c;
	    }
	    else if (c == '=') {
		token.type = ASSIGN;
		token.value.string = "";
		return &token;
	    }
	    else if (isdigit(c)) {
		state = 3;
		*tp++ = c;
	    }
	    else if (c == '"') {
		state = 4;
	    }
	    else if (!isspace(c)) {
		token.type = ERROR;
		token.value.string = "bad input";
		return &token;
	    }
	    break;
	case 1:
	    if (c == '\n')
		state = 0;
	    break;
	case 2:
	    if (isalnum(c) || c == '_') {
		*tp++ = c;
	    }
	    else {
		ungetc(c, input);
		*tp = '\0';
		token.type = IDENT;
		token.value.string = id;
		return &token;
	    }
	    break;
	case 3:
	    if (isdigit(c)) {
		*tp++ = c;
	    }
	    else if (hflg && (((c >= 'a') && (c <= 'f')) ||
			      ((c >= 'A') && (c <= 'F')))) {
		*tp++ = c;
	    }
	    else if (!hflg && (c == 'x' || c == 'X')) {
		hflg = 1;
	    }
	    else {
		ungetc(c, input);
		*tp = '\0';
		token.type = NUMERIC;
		if (hflg)
		    token.value.numeric = (int) strtol(id, (char **) NULL, 16);
		else
		    token.value.numeric = atoi(id);
		return &token;
	    }
	    break;
	case 4:
	    if (c != '"') {
		*tp++ = c;
	    }
	    else {
		*tp = '\0';
		token.type = STRING;
		token.value.string = id;
		return &token;
	    }
	    break;
	}
    }
    if (state != 0) {
	token.type = ERROR;
	token.value.string = "unexpected EOF";
	return &token;
    }
    else {
	token.type = EOF_T;
	token.value.string = "";
	return &token;
    }
}

static int
LookupID(char *id)
{
    int             i;

    for (i = 0; config_tbl[i].value; i++)
	if (!Strcasecmp(config_tbl[i].name, id))
	    return i;

    return -1;			/* identifier not found */
}

static STATUS  *
Parse(FILE * input)
{
    int             i;
    TOKEN          *tok;
    static STATUS   parse;

    tok = Lex(input);
    switch (tok->type) {
    case IDENT:
	if ((i = LookupID(tok->value.string)) == -1) {
	    parse.status = -1;
	    parse.msg = "undefined identifier";
	    break;
	}
	tok = Lex(input);
	if (tok->type != ASSIGN) {
	    parse.status = -1;
	    parse.msg = "parse error";
	    break;
	}
	tok = Lex(input);
	if (config_tbl[i].type == CONFIG_TYPE_STRING) {
	    if (tok->type != STRING) {
		parse.status = -1;
		parse.msg = "parse error";
		break;
	    }
	    strcpy((char *) config_tbl[i].value, tok->value.string);
	}
	else {
	    if (tok->type != NUMERIC) {
		parse.status = -1;
		parse.msg = "parse error";
		break;
	    }
	    switch (config_tbl[i].type) {
	    case CONFIG_TYPE_UINT16:
		*((UINT16 *) config_tbl[i].value) = (UINT16) tok->value.numeric;
		break;
	    default:
		*((int *) config_tbl[i].value) = tok->value.numeric;
		break;
	    }
	}
	parse.status = 1;
	parse.msg = NULL;
	break;
    case EOF_T:
	parse.status = 0;
	parse.msg = NULL;
	break;
    case ERROR:
	parse.status = -1;
	parse.msg = tok->value.string;
	break;
    default:
	parse.status = -1;
	parse.msg = "parse error";
	break;
    }

    return &parse;
}

int
ReadConfigFile(char *fname)
{
    STATUS         *parse;
    FILE           *input;
    char           msg[100];

    if ((input = fopen(fname, "r")) == NULL) {
	sprintf(msg, "Failed to open configuration file '%s' for reading.",
		fname);
	error(MSG_ERROR, msg);
	return FAIL;
    }

    do {
	parse = Parse(input);
    } while (parse->status == 1);

    fclose(input);

    if (parse->status == -1) {
        strcpy(msg, "Error in config file: ");
        strcat(msg, parse->msg);
	error(MSG_ERROR, msg);
	return FAIL;
    }

    return SUCCESS;
}
