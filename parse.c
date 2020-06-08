/****************************************************/
/* File: parse.c                                    */
/* The parser implementation for the TINY compiler  */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "util.h"
#include "scan.h"
#include "parse.h"

static TokenType token; /* holds current token */

/* function prototypes for recursive calls */
static TreeNode * program(void);
static TreeNode * declaration_list(void);
static TreeNode * declaration(void);
static TreeNode * var_declaration(void);
static TreeNode * fun_declaration(void);
static TreeNode * params(void);
static TreeNode * param_list(void);
static TreeNode * param(void);
static TreeNode * compound_stmt(void);
static TreeNode * local_declaration(void);
static TreeNode * type_specifier(NodeKind kind);
static TreeNode * stmt_sequence(void);
static TreeNode * statement(void);
static TreeNode * additive_exp(void);
static TreeNode * expression_stmt(void);
static TreeNode * var(void);
static TreeNode * selection_stmt(void);
static TreeNode * iteration_stmt(void);
static TreeNode * return_stmt(void);
static TreeNode * exp(void);
static TreeNode * simple_exp(void);
static TreeNode * term(void);
static TreeNode * factor(void);
static TreeNode * call(void);
static TreeNode * args(void);

static void syntaxError(char * message)
{ fprintf(listing,"\n>>> ");
  fprintf(listing,"Syntax error at line %d: %s",lineno,message);
  Error = TRUE;
}

static void match(TokenType expected)
{ if (token == expected) token = getToken();
  else {
    syntaxError("unexpected token -> ");
    printToken(token,tokenString);
    fprintf(listing,"      ");
  }
}

TreeNode * program(void)
{
/* i am lazy,so keep the prognode */ 
	TreeNode * t = newProgNode();
	TreeNode * p = declaration_list();
	t->child[0] = p;
	return t;
}

TreeNode * declaration_list(void)
{
  TreeNode * t = declaration();
  TreeNode * p = t;
  while ((token == INT) || (token == CHAR) || (token == VOID))
  { TreeNode * q;
    q = declaration();
    if (q!=NULL) {
      if (t==NULL) t = p = q;
      else /* now p cannot be NULL either */
      { p->sibling = q;
        p = q;
      }
    }
  }
  return t;
	
}

TreeNode * declaration(void)
{
		TreeNode * t = NULL;
		if((token == INT) || (token == CHAR) || (token == VOID)){
				int old_linepos = keepTrack();
				match(token);
				match(ID);
				switch(token){
						case SEMI:
						case LBRACKET:
								backToTrack(old_linepos);
								token = getToken();
								t = var_declaration();	
								break;
						case LPAREN:
								backToTrack(old_linepos);
								token = getToken();
								t = fun_declaration();
								break;
						default : syntaxError("unexpected token -> ");
								  printToken(token,tokenString);
								  token = getToken();
								  break;
				}
		}else{
				syntaxError("unexpected token -> ");
				printToken(token,tokenString);
				token = getToken();
		}	
		return t;
}

TreeNode * var_declaration(void)
{
		TreeNode * t = type_specifier(DeclK);
		if ((t!=NULL) && (token==ID))
				t->attr.name = copyString(tokenString);
		match(ID);
		if(token == SEMI){
			match(SEMI);
		}else if(token == LBRACKET){
				match(LBRACKET);
				if(t->kind.decl == ID_IntK)
						t->kind.decl = Array_IntK;
				else
						t->kind.decl = Array_CharK;
				if(token == NUM){
						TreeNode * p = newExpNode(ConstK);
						if ((t!=NULL) && (token==NUM))
								p->attr.val = atoi(tokenString);
						match(NUM);
						t->child[0] = p;	
						match(RBRACKET);
						match(SEMI);
				}else{
						syntaxError("unexpected token -> ");
						printToken(token,tokenString);
						token = getToken();
				}	

		}
		return t;
}

TreeNode * fun_declaration(void)
{
		TreeNode * t = type_specifier(DeclK);
		if(t->kind.decl == ID_IntK)
			t->kind.decl = FUN_IntK;
		else if(t->kind.decl == ID_CharK)
			t->kind.decl = FUN_CharK;
		if ((t!=NULL) && (token==ID))
				t->attr.name = copyString(tokenString);
		match(ID);
		match(LPAREN);
		TreeNode * t1 = params();
		match(RPAREN);
		TreeNode * t2 = compound_stmt();
		t->child[0] = t1;
		t->child[1] = t2;	
		return t;
}

TreeNode * params(void)
{
	TreeNode * t = NULL;
	if(token == VOID){
	/* equal to t = type_specifier(ParamK); */
		t = newParamNode(VoidK);	
		match(VOID);
	}else{
		t = param_list();
	}
	return t;
}

TreeNode * param_list(void)
{ 
		TreeNode * t = param();
		TreeNode * p = t;
		while (token == COMMA)
		{
			 	TreeNode * q;
				match(COMMA);
				q = param();
				if (q!=NULL) {
						if (t==NULL) 
								t = p = q;
						else /* now p cannot be NULL either */
						{ 
								p->sibling = q;
								p = q;
						}
				}
		}
		return t;
}

TreeNode * param(void)
{
		TreeNode * t = type_specifier(ParamK);
		if ((t!=NULL) && (token==ID))
				t->attr.name = copyString(tokenString);
		match(ID);
		if(token == LBRACKET){
				/*change it to array type*/
				if(t->kind.param == PID_IntK)
						t->kind.param = PArray_IntK;
				else
						t->kind.param = PArray_CharK;
				match(LBRACKET);
				match(RBRACKET);
		}
		return t;	
}

TreeNode * compound_stmt(void)
{
	match(LBRACE);
	TreeNode * t = newStmtNode(CompoundK);
	t->child[0] = local_declaration();
	t->child[1] = stmt_sequence();
	match(RBRACE);
	return t;
}

TreeNode * local_declaration(void)
{
		TreeNode * t = NULL;
		if((token == INT) || (token == CHAR)){
				t = var_declaration();
		}
		TreeNode * p = t;
		while((token == INT) || (token == CHAR)){
				/* local_declaration -> var_declaration local_declaration */
				TreeNode * q;
				q = var_declaration();
				if (q!=NULL) {
						if (t==NULL) t = p = q;
						else /* now p cannot be NULL either */
						{ p->sibling = q;
								p = q;
						}
				}

		}	
		return t;
}

TreeNode * type_specifier(NodeKind kind)
{
	TreeNode * t = NULL;
	if(kind == DeclK){
			if(token == INT){
					t = newDeclNode(ID_IntK);
					match(INT);
			}
			else if(token == CHAR){
					t = newDeclNode(ID_CharK);
					match(CHAR);
			}else{
					t = newDeclNode(FUN_VoidK);
					match(VOID);
			}
	}else if(kind == ParamK){
			if(token == INT){
					t = newParamNode(PID_IntK);
					match(INT);
			}
			else if(token == CHAR){
					t = newParamNode(PID_CharK);
					match(CHAR);
			}
	}
	return t;
}

TreeNode * stmt_sequence(void)
{
		TreeNode * t = NULL;
		while(token != RBRACE){

				t = statement();
				TreeNode * p = t;
				while (token!= RBRACE)	
				{ 
						TreeNode * q;
						q = statement();
						if (q!=NULL) {
								if (t==NULL) 
									t = p = q;
								else /* now p cannot be NULL either */
								{ p->sibling = q;
										p = q;
								}
						}
				}
		}
		return t;
}

TreeNode * statement(void)
{ TreeNode * t = NULL;
  switch (token) {
    case IF : t = selection_stmt(); break;
    case ID : 
	case SEMI:
		t = expression_stmt(); break;
	case LBRACE : t = compound_stmt(); break;
	case WHILE : t = iteration_stmt(); break;
	case RETURN : t = return_stmt(); break;
    default : syntaxError("unexpected token -> ");
              printToken(token,tokenString);
              token = getToken();
              break;
  } /* end case */
  return t;
}

TreeNode * expression_stmt(void)
{
	TreeNode * t = NULL;
	if(token == SEMI){
		//do nothing
	}else if(token == ID){
		t = exp();
		match(SEMI);
	}
	return t;	
}

TreeNode * selection_stmt(void)
{ TreeNode * t = newStmtNode(IfK);
  match(IF);
  match(LPAREN);
  if (t!=NULL) t->child[0] = exp();
  match(RPAREN);
  if (t!=NULL) t->child[1] = statement();
  if (token==ELSE) {
    match(ELSE);
    if (t!=NULL) t->child[2] = statement();
  }
  return t;
}

TreeNode * iteration_stmt(void)
{
 	TreeNode * t = newStmtNode(WhileK);
	match(WHILE);
	match(LPAREN);
	t->child[0] = exp();
	match(RPAREN);
	t->child[1] = statement();
	return t;	
}

TreeNode * return_stmt(void)
{
	TreeNode * t = newStmtNode(ReturnK);
	match(RETURN);
	if(token != SEMI)
		t->child[0] = exp();
	match(SEMI);
	return t;	
}

TreeNode * exp(void)
{
	TreeNode * t = NULL;
	TreeNode * q = NULL;
	int old_linepos = keepTrack();
	t = var();
	if(t != NULL && token == ASSIGN){ //exp-> var = expression
		q = newExpNode(AssignK);
		match(ASSIGN);
		TreeNode * p = exp();	
		q->child[0] = t;
		q->child[1] = p;
		t = q;
	}else{
		free(t);
		t = NULL;
		backToTrack(old_linepos);
		token = getToken();
		t = simple_exp();
	}	
	return t;
}

TreeNode * var(void)
{
		TreeNode * t = NULL; 
		if (token==ID){
				t = newExpNode(IdK);
				t->attr.name = copyString(tokenString);
				match(ID);
				if(token == LBRACKET){
						match(LBRACKET);
						TreeNode * p = exp();
						t->child[0] = p;
						match(RBRACKET);
				}
		}
		return t;
}

TreeNode * simple_exp(void)
{ TreeNode * t = additive_exp();
  if ((token==LT)||(token==EQ) || (token==LE) || (token == GE)
		||(token==GT) || (token==NE)) {
    TreeNode * p = newExpNode(OpK);
    if (p!=NULL) {
      p->child[0] = t;
      p->attr.op = token;
      t = p;
    }
    match(token);
    if (t!=NULL)
      t->child[1] = additive_exp();
  }
  return t;
}

TreeNode * additive_exp(void)
{ TreeNode * t = term();
  while ((token==PLUS)||(token==MINUS))
  { TreeNode * p = newExpNode(OpK);
    if (p!=NULL) {
      p->child[0] = t;
      p->attr.op = token;
      t = p;
      match(token);
      t->child[1] = term();
    }
  }
  return t;
}

TreeNode * term(void)
{ TreeNode * t = factor();
  while ((token==TIMES)||(token==OVER))
  { TreeNode * p = newExpNode(OpK);
    if (p!=NULL) {
      p->child[0] = t;
      p->attr.op = token;
      t = p;
      match(token);
      p->child[1] = factor();
    }
  }
  return t;
}

TreeNode * factor(void)
{ TreeNode * t = NULL;
		switch (token) {
				case NUM :
						t = newExpNode(ConstK);
						if ((t!=NULL) && (token==NUM))
								t->attr.val = atoi(tokenString);
						match(NUM);
						break;
				case ID :{
						int old_linepos = keepTrack();
						match(ID);
						if(token == LPAREN){
								backToTrack(old_linepos);
								token = getToken();
								t = call();
						}else{ //[ or other thing
								backToTrack(old_linepos);
								token = getToken();
								t = var();	  
						}
						break;
			    }	
				case LPAREN :
						match(LPAREN);
						t = exp();
						match(RPAREN);
						break;
				default:
						syntaxError("unexpected token -> ");
						printToken(token,tokenString);
						token = getToken();
						break;
		}
		return t;
}

TreeNode * call(void)
{
	TreeNode * t = newExpNode(CallK);
	t->attr.name = copyString(tokenString);
	match(ID);
	match(LPAREN);
	TreeNode * p = args();
	match(RPAREN);
	t->child[0] = p;
	return t;
}

TreeNode * args(void)
{
	TreeNode * t = NULL;
	while(token != RPAREN){
				t = exp();
				TreeNode * p = t;
				while (token!= RPAREN)	
				{ 
						match(COMMA);
						TreeNode * q;
						q = exp();
						if (q!=NULL) {
								if (t==NULL) 
									t = p = q;
								else /* now p cannot be NULL either */
								{ p->sibling = q;
										p = q;
								}
						}
				}
		
	}
	return t;
}
/****************************************/
/* the primary function of the parser   */
/****************************************/
/* Function parse returns the newly 
 * constructed syntax tree
 */
TreeNode * parse(void)
{ TreeNode * t;
  token = getToken();
  t = program();
  if (token!=ENDFILE)
    syntaxError("Code ends before file\n");
  return t;
}
