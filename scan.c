/****************************************************/
/* File: scan.c                                     */
/* The scanner implementation for the TINY compiler */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "util.h"
#include "scan.h"

/* states in scanner DFA */
typedef enum
   { START,INCOMMENT,INNUM,INID,DONE,INNE,AFOVER,AFTIMES,
	 AFEQ,AFLT,AFGT }
   StateType;

/* lexeme of identifier or reserved word */
char tokenString[MAXTOKENLEN+1];

/* BUFLEN = length of the input buffer for
   source code lines */
#define BUFLEN 256

static char lineBuf[2][BUFLEN]; /* holds the current line */
static int flag = 1;
static int old_flag;
static int load_flag = TRUE;
static int old_bufsize;
static int linepos = 0; /* current position in LineBuf */
static int fore_linepos = 0;
static int bufsize = 0; /* current size of buffer string */
static int EOF_flag = FALSE; /* corrects ungetNextChar behavior on EOF */

/* getNextChar fetches the next non-blank character
   from lineBuf, reading in a new line if lineBuf is
   exhausted */
static int getNextChar(void)
{ if (!(linepos < bufsize))
		{ lineno++;
				flag = 1 - flag;
				if(load_flag){
						if (fgets(lineBuf[flag],BUFLEN-1,source))
						{ if (EchoSource) fprintf(listing,"%4d: %s",lineno,lineBuf[flag]);
								bufsize = strlen(lineBuf[flag]);
								fore_linepos = linepos = 0;
								return lineBuf[flag][linepos++];
						}
						else
						{ EOF_flag = TRUE;
								return EOF;
						}
				}else {
						bufsize = strlen(lineBuf[flag]);
						fore_linepos = linepos = 0;
						return lineBuf[flag][linepos++];	
				}
		}
		else return lineBuf[flag][linepos++];
}

/* ungetNextChar backtracks one character
   in lineBuf */
static void ungetNextChar(void)
{ if (!EOF_flag) linepos-- ;}

/* lookup table of reserved words */
static struct
    { char* str;
      TokenType tok;
    } reservedWords[MAXRESERVED]
   = {{"if",IF},{"else",ELSE},{"return",RETURN},{"void",VOID},
      {"int",INT},{"char",CHAR},{"bool",BOOLEAN},{"while",WHILE}};

/* lookup an identifier to see if it is a reserved word */
/* uses linear search */
static TokenType reservedLookup (char * s)
{ int i;
  for (i=0;i<MAXRESERVED;i++)
    if (!strcmp(s,reservedWords[i].str))
      return reservedWords[i].tok;
  return ID;
}

/****************************************/
/* the primary function of the scanner  */
/****************************************/
/* function getToken returns the 
 * next token in source file
 */
TokenType getToken(void)
{  /* index for storing into tokenString */
   int tokenStringIndex = 0;
   /* holds current token to be returned */
   TokenType currentToken;
   /* current state - always begins at START */
   StateType state = START;
   /* flag to indicate save to tokenString */
   int save;
   fore_linepos = linepos;
   while (state != DONE)
   { int c = getNextChar();
     save = TRUE;
     switch (state)
     { case START:
         if (isdigit(c))
           state = INNUM;
         else if (isalpha(c)){
           state = INID;
		 }
         else if (c == '!')
           state = INNE;
		 else if (c == '=')
		   state = AFEQ;
		 else if (c == '<')
		   state = AFLT;
		 else if (c == '>')
		   state = AFGT;
         else if ((c == ' ') || (c == '\t') || (c == '\n')){
           save = FALSE;
		 }
         else if (c == '/')
         { save = FALSE;
           state = AFOVER;
         }
         else
         { state = DONE;
           switch (c)
           { case EOF:
               save = FALSE;
               currentToken = ENDFILE;
               break;
             case '+':
               currentToken = PLUS;
               break;
             case '-':
               currentToken = MINUS;
               break;
             case '*':
               currentToken = TIMES;
               break;
             case '(':
               currentToken = LPAREN;
               break;
             case ')':
               currentToken = RPAREN;
               break;
			 case '[':
			   currentToken = LBRACKET;
			   break;
			 case ']':
			   currentToken = RBRACKET;
			   break;
			 case '{':
			   currentToken = LBRACE;
			   break;
			 case '}':
			   currentToken = RBRACE;
			   break;
             case ';':
               currentToken = SEMI;
               break;
			 case ',':
			   currentToken = COMMA;
			   break;
             default:
               currentToken = ERROR;
               break;
           }
         }
         break;
	   case AFOVER:
	     if (c == '*'){
            state = INCOMMENT;
			save = FALSE;
		 }
		 else{
			c = '/';
			currentToken = OVER;
            ungetNextChar();
			state = DONE;
		 }
		 break;
       case INCOMMENT:
         save = FALSE;
         if (c == EOF)
         { state = DONE;
           currentToken = ENDFILE;
         }
         else if (c == '*') state = AFTIMES;
         break;
	   case AFTIMES:
		 save = FALSE;
         if (c == EOF)
         { state = DONE;
           currentToken = ENDFILE;
         }
 		 else if(c == '/'){
			state = START;
		 }
		 else{
			state = INCOMMENT;
		 }
		 break;
	   case AFEQ:
		 state = DONE;
		 if(c == '=')
			currentToken = EQ;
		 else{
			currentToken = ASSIGN;
		    ungetNextChar();
			save = FALSE;
		 }
         break;	
	   case AFLT:
		 state = DONE;
		 if(c == '=')
			currentToken = LE;
		 else{
			currentToken = LT;
		    ungetNextChar();
			save = FALSE;
		 }
         break;	
	   case AFGT:
		 state = DONE;
		 if(c == '=')
			currentToken = GE;
		 else{
			currentToken = GT;
		    ungetNextChar();
			save = FALSE;
		 }
         break;	
	   case INNE:
		 state = DONE;
		 if(c == '=')
			currentToken = NE;
		 else{
			currentToken = ERROR;
		    ungetNextChar();
			save = FALSE;
		 }
         break;	
       case INNUM:
         if (!isdigit(c))
         { /* backup in the input */
           ungetNextChar();
           save = FALSE;
           state = DONE;
           currentToken = NUM;
         }
         break;
       case INID:
         if (!isalpha(c))
         { /* backup in the input */
           ungetNextChar();
           save = FALSE;
           state = DONE;
           currentToken = ID;
         }
         break;
       case DONE:
       default: /* should never happen */
         fprintf(listing,"Scanner Bug: state= %d\n",state);
         state = DONE;
         currentToken = ERROR;
         break;
     }
     if ((save) && (tokenStringIndex <= MAXTOKENLEN))
       tokenString[tokenStringIndex++] = (char) c;
     if (state == DONE)
     { tokenString[tokenStringIndex] = '\0';
       if (currentToken == ID)
         currentToken = reservedLookup(tokenString);
     }
   }
   if (TraceScan) {
     fprintf(listing,"\t%d: ",lineno);
     printToken(currentToken,tokenString);
   }
   return currentToken;
} /* end getToken */

int keepTrack(){
	old_bufsize = bufsize;
	old_flag = flag;
	return fore_linepos;
}

void backToTrack(int old_linepos){
	if(old_flag != flag)
		load_flag = FALSE;
	flag = old_flag;
	/*restore the old value*/
	linepos = old_linepos;
	bufsize = old_bufsize;
}
