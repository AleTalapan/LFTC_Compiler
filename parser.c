#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include "ad.h"
#include "parser.h"
#include "utils.h"
#include "at.h"

Token *iTk;		// the iterator in the tokens list
Token *consumedTk;		// the last consumed token

Symbol *owner = NULL;

void tkerr(const char *fmt,...){
  fprintf(stderr,"error in line %d: ",iTk->line);
  va_list va;
  va_start(va,fmt);
  vfprintf(stderr,fmt,va);
  va_end(va);
  fprintf(stderr,"\n");
  exit(EXIT_FAILURE);
}

bool consume(int code){
  if(iTk->code==code){
    consumedTk=iTk;
    iTk=iTk->next;
    return true;
  }
  return false;
}

// typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
bool typeBase(Type *t){
  t->n = -1;
  Token *start = iTk;
  if(consume(TYPE_INT)){
    t->tb=TB_INT;
    return true;
  }
  if(consume(TYPE_DOUBLE)){
    t->tb=TB_DOUBLE;
    return true;
  }
  if(consume(TYPE_CHAR)){
    t->tb=TB_CHAR;
    return true;
  }
  if(consume(STRUCT)){
    if(consume(ID)){
      Token *tkName = consumedTk;
			t->tb = TB_STRUCT;
			t->s = findSymbol(tkName->text);
			if(!t->s) tkerr("structura nedefinita: %s",tkName->text);
      return true;
    } else tkerr("lipsa id dupa struct");
  }
  iTk=start;
  return false;
}

// unit: ( structDef | fnDef | varDef )* END
bool unit(){
  for(;;){
      if(structDef()) {}
      else if(fnDef()) {}
      else if(varDef()) {}
      else break;
  }
  if(consume(END)){
    return true;
  }
  else tkerr("eroare de sintaxa");
  return false;
}

//structDef: STRUCT ID LACC varDef* RACC SEMICOLON
bool structDef(){
  Token *t = iTk;
  if(consume(STRUCT)){
    if(consume(ID)){
      Token *tkName = consumedTk;
      if(consume(LACC)){
        Symbol *s = findSymbolInDomain(symTable, tkName->text);
        if (s) {
          tkerr("symbol redefinition: %s", tkName->text);
        }
        s = addSymbolToDomain(symTable, newSymbol(tkName->text, SK_STRUCT));
        s->type.tb = TB_STRUCT;
        s->type.s = s;
        s->type.n = -1;
        pushDomain();
        owner = s;
        while(varDef()){}
	    if(consume(RACC)){
	        if(consume(SEMICOLON)){
            owner=NULL;
            dropDomain();
	          return true;
	        }
	        else tkerr("lipsa ; dupa }");
	        }
	    else tkerr("lipsa } de la struct");
        }
    }
    else tkerr("lipsa identificator");
  }
  iTk=t;
  return false;
}

//varDef: typeBase ID arrayDecl? SEMICOLON
bool varDef(){
  Token *start = iTk;
  Type t;
  if(typeBase(&t)){
    if(consume(ID)){
      Token *tkName = consumedTk;
      if(arrayDecl(&t)){
        if (t.n == 0) tkerr("a vector variable must have a specified dimension");
      }
      if(consume(SEMICOLON)){
        Symbol *var=findSymbolInDomain(symTable,tkName->text);
				if(var)tkerr("symbol redefinition: %s",tkName->text);
				var=newSymbol(tkName->text,SK_VAR);
				var->type=t;
				var->owner=owner;
				addSymbolToDomain(symTable,var);
				if (owner) {
					switch(owner->kind) {
						case SK_FN:
							var->varIdx=symbolsLen(owner->fn.locals);
							addSymbolToList(&owner->fn.locals,dupSymbol(var));
							break;
						case SK_STRUCT:
							var->varIdx=typeSize(&owner->type);
							addSymbolToList(&owner->structMembers,dupSymbol(var));
							break;
						default:
							break;
					}
				} else {
					var->varMem=safeAlloc(typeSize(&t));
				}
	      return true;
      }
      else tkerr("lipsa ; dupa declaratie");
    } 
    else tkerr("lipsa id");
  } 
  iTk=start;
  return false;
}

//arrayDecl: LBRACKET INT? RBRACKET
bool arrayDecl(Type *t){
  Token *start = iTk;
  if(consume(LBRACKET)){
    if(consume(INT)) {
      Token *tkSize=consumedTk;
      t->n=tkSize->i;
    }
    else{
      t->n=0;
    }
    if(consume(RBRACKET)){
      return true;
    }else tkerr("lipsa ] la declararea vectorului");
  }
  iTk=start;
  return false;
}

/*fnDef: ( typeBase | VOID ) ID
LPAR ( fnParam ( COMMA fnParam )* )? RPAR
stmCompoun
*/

bool fnDef(){
  Type t;
  Token *start = iTk;
  if(typeBase(&t) || consume(VOID)){
      if (consumedTk->code == VOID){
      t.tb = TB_VOID;
      }
      if(consume(ID)){
          Token *tkName = consumedTk;
          if(consume(LPAR)){
              Symbol *fn=findSymbolInDomain(symTable,tkName->text);
				      if(fn)tkerr("symbol redefinition: %s",tkName->text);
				      fn=newSymbol(tkName->text,SK_FN);
				      fn->type=t;
				      addSymbolToDomain(symTable,fn);
				      owner=fn;
				      pushDomain();
              if(fnParam()){
                  for(;;){
                      if(consume(COMMA)){
                          if(fnParam()){}
                          else{
                              iTk=start;
                              tkerr("eroare la declararea parametrilor functiei");
                              return false;
                          }
                      }
		                  else if(fnParam()){
                        tkerr("lipsa ,");
                      }
                      else break;
                  }
              }
              if(consume(RPAR)){
                  if(stmCompound(false)){
                      dropDomain();
						          owner=NULL;
                      return true;
                  }
              }
              else tkerr("lipsa ) de inchidere a functiei");
          }
      }
  }
    iTk=start;
    return false;
}


//fnParam: typeBase ID arrayDecl?
bool fnParam(){
  Token *start = iTk;
  Type t;
  if(typeBase(&t)){
    if(consume(ID)){
      Token *tkName = consumedTk;
      if(arrayDecl(&t)){
        t.n=0;
      }
      Symbol *param=findSymbolInDomain(symTable,tkName->text);
			if(param)tkerr("symbol redefinition: %s",tkName->text);
			param=newSymbol(tkName->text,SK_PARAM);
			param->type=t;
			param->owner=owner;
			param->paramIdx=symbolsLen(owner->fn.params);
			// parametrul este adaugat atat la domeniul curent, cat si la parametrii fn
			addSymbolToDomain(symTable,param);
			addSymbolToList(&owner->fn.params,dupSymbol(param));
	    return true;
    } 
    else tkerr("lipsa id in declararea argumentelor");
  }
  iTk=start;
  return false;
}

/*stm: stmCompound
| IF LPAR expr RPAR stm ( ELSE stm )?
| WHILE LPAR expr RPAR stm
| RETURN expr? SEMICOLON
| expr? SEMICOLON
*/

bool stm(){
    Token *t = iTk;
    Ret rCond, rExpr;
    if(stmCompound(true)) {return true;}
    if(consume(IF)){
          if(consume(LPAR)){
              if(expr(&rCond)){
                  if (!canBeScalar(&rCond)) {
					          tkerr("the if condition must be a scalar value");
				          }
                  if(consume(RPAR)){
                      if(stm()){
                          if(consume(ELSE)){
                              if(stm()){
                                 return true;
                              }
                              else{
                                  tkerr("lipsa statement dupa ELSE");
                                  iTk=t;
                                  return false;
                              }					     
                          }
                          return true;
                      }
                  } else tkerr("lipsa ) dupa IF");
              } else tkerr("expresie invalida");
          }
    }
    if(consume(WHILE)){
          if(consume(LPAR)){
              if(expr(&rCond)){
                  if(!canBeScalar(&rCond)){
                    tkerr("the while condition must be a scalar value");
                  }
                  if(consume(RPAR)){
                      if(stm()){
                          return true;
                      }
                  } else tkerr("lipsa ) dupa WHILE");
              } else tkerr("expresie invalida");
          }
    }
    if(consume(RETURN)){
          if(expr(&rExpr)){
            if(owner->type.tb==TB_VOID){
              tkerr("a void function cannot return a value");
            }
            if(!canBeScalar(&rExpr)){
              tkerr("the return value must be a scalar value");
            }
            if(!convTo(&rExpr.type,&owner->type)){
              tkerr("cannot convert the return expression type to the function return type");
            }
          }
          else{
            if(owner->type.tb!=TB_VOID){
              tkerr("a non-void function must return a value");
            }
          }
          if(consume(SEMICOLON)){
              return true;
          } 
          else tkerr("lipsa ; dupa return");
    }
    if(expr(&rExpr)) {
        if(consume(SEMICOLON)){
        return true;
    } else tkerr("lipsa ; ");
    }
    if(consume(SEMICOLON)){
        return true;
    }
  iTk=t;
  return false;
}
  
//stmCompound: LACC ( varDef | stm )* RACC

bool stmCompound(bool newDomain){
    Token *t = iTk;
  if(consume(LACC)){
      if (newDomain) {
			pushDomain();
		  }
      for(;;){
	    if(varDef() || stm()){}
      else break;
      }
    if(consume(RACC)){
      if (newDomain) {
				dropDomain();
			}
      return true;
    } else tkerr("statement invalid");
  }
  iTk=t;
  return false;
}

//expr: exprAssign

bool expr(Ret *r){
    Token *t = iTk;
    if(exprAssign(r)){
      return true;
    }
    else{
        iTk=t;
        return false;
    }
}

//exprAssign: exprUnary ASSIGN exprAssign | exprOr

bool exprAssign(Ret *r){
  Ret rDst;
  Token *t = iTk;
  if(exprUnary(&rDst)){
     if(consume(ASSIGN)){
        if(exprAssign(r)){
            if (!rDst.lval) {
				    	tkerr("the assign destination must be a left-value");
				    }
				    if (rDst.ct) {
				    	tkerr("the assign destination cannot be constant");
				    }
				    if (!canBeScalar(&rDst)) {
				    	tkerr("the assign destination must be scalar");
				    }
				    if (!canBeScalar(r)) {
				    	tkerr("the assign source must be scalar");
				    }
				    if (!convTo(&r->type,&rDst.type)) {
				    	tkerr("the assign source cannot be converted to destination");
				    }
				    r->lval = false;
				    r->ct = true;
            return true;
        }
        else tkerr("lipsa expresie assign");
     }
  }
  iTk=t;
  if(exprOr(r)) 
    return true;
  iTk=t;
  return false;
}

bool exprOrPrim(Ret *r){
    Token *t = iTk;
  if(consume(OR)){
    Ret right;
    if(exprAnd(&right)){
      Type tDst;
      if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for ||");
      *r=(Ret){{TB_INT,NULL,-1},false,true};
      if(exprOrPrim(r))
	    return true;
    } tkerr("lipsa id dupa OR");
  }
  iTk=t;
  return true;
}

bool exprOr(Ret *r){
    Token *t = iTk;
  if(exprAnd(r)){
    if(exprOrPrim(r)) 
      return true;
  }
  iTk=t;
  return false;
}

bool exprAndPrim(Ret *r){
    Token *t = iTk;
  if(consume(AND)){
    Ret right;
    if(exprEq(&right)){
      Type tDst;
      if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for &&");
      *r=(Ret){{TB_INT,NULL,-1},false,true};
      if(exprAndPrim(r))
	    return true;
    } tkerr("lipsa id dupa AND");
  }
  iTk=t;
  return true;
}

bool exprAnd(Ret *r){
    Token *t = iTk;
  if(exprEq(r)){
    if(exprAndPrim(r)) return true;
  }
  iTk=t;
  return false;
}

bool exprEqPrim(Ret *r){
    Token *t = iTk;
  if(consume(EQUAL) || consume(NOTEQ)){
    Ret right;
    if(exprRel(&right)){
      Type tDst;
      if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for == or !=");
      *r=(Ret){{TB_INT,NULL,-1},false,true};
      if(exprEqPrim(r))
	    return true;
    } tkerr("lipsa id dupa comparator");
  }
  iTk=t;
  return true;
}

bool exprEq(Ret *r){
    Token *t = iTk;
  if(exprRel(r)){
    if(exprEqPrim(r)) return true;
  }
  iTk=t;
  return false;
}

bool exprRelPrim(Ret *r){
    Token *t = iTk;
  if(consume(LESS) || consume(LESSEQ) || consume(GREATER) || consume(GREATEREQ)){
    Ret right;
    if(exprAdd(&right)){
      Type tDst;
      if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for <, <=, >, >=");
      *r=(Ret){{TB_INT,NULL,-1},false,true};
      if(exprRelPrim(r))
	      return true;
    } else tkerr("lipsa operand drept");
  }
  iTk=t;
  return true;
}

bool exprRel(Ret *r){
    Token *t = iTk;
  if(exprAdd(r)){
    if(exprRelPrim(r)) return true;
  }
  iTk=t;
  return false;
}

bool exprAddPrim(Ret *r){
    Token *t = iTk;
  if(consume(ADD) || consume(SUB)){
    Ret right;
    if(exprMul(&right)){
      Type tDst;
      if(!arithTypeTo(&r->type,&right.type,&tDst)){
        tkerr("invalid operand type for + or -");
      }
      *r=(Ret){tDst,false,true};
      if(exprAddPrim(r))
	      return true;
    } tkerr("lipsa expresie");
  }
  iTk=t;
  return true;
}

bool exprAdd(Ret *r){
    Token *t = iTk;
  if(exprMul(r)){
    if(exprAddPrim(r)) return true;
  }
  iTk=t;
  return false;
}

bool exprMulPrim(Ret *r){
    Token *t = iTk;
  if(consume(MUL) || consume(DIV)){
    Ret right;
    if(exprCast(&right)){
      Type tDst;
      if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for * or /");
      *r=(Ret){tDst,false,true};
      if(exprMulPrim(r))
	return true;
    } tkerr("lipsa id dupa operand");
  }
  iTk=t;
  return true;
}

bool exprMul(Ret *r){
    Token *t = iTk;
  if(exprCast(r)){
    if(exprMulPrim(r)) return true;
  }
  iTk=t;
  return false;
}

bool exprCast(Ret *r){
    Token *start = iTk;
  if(consume(LPAR)){
    Type t;
    Ret op;
    if(typeBase(&t)){
      if(arrayDecl(&t)){}
      if(consume(RPAR)){
	    if(exprCast(&op)){
        if(t.tb==TB_STRUCT){
          tkerr("cannot convert to a struct type");
        }
        if(op.type.tb==TB_STRUCT){
          tkerr("cannot convert a struct");
        }
        if(op.type.n>=0&&t.n<0){
          tkerr("an array can be converted only to another array");
        }
        if(op.type.n<0&&t.n>=0){
          tkerr("a scalar can be converted only to another scalar");
        }
        *r=(Ret){t,false,true};
        return true;
      }
      }
      else{
        tkerr("Lipsa )");
      }
    }
  }
  else if(exprUnary(r)) return true;
  iTk=start;
  return false;
}

bool exprUnary(Ret *r){
    Token *t = iTk;
  if(consume(SUB) || consume(NOT)){
    if(exprUnary(r)){
      if(!canBeScalar(r)){
        tkerr("unary - or ! must have a scalar operand");
      }
      r->lval=false;
      r->ct=true;
      return true;
    }
  }
  else if (exprPostfix(r)) return true;
  iTk=t;
  return false;
}

bool exprPostfixPrim(Ret *r){
    Token *t = iTk;
  if(consume(LBRACKET)){
    Ret idx;
    if(expr(&idx)){
      if(consume(RBRACKET)){
        if(r->type.n<0){
          tkerr("only an array can be indexed");
        }
        Type tInt={TB_INT,NULL,-1};
        if(!convTo(&idx.type,&tInt)){
          tkerr("the index is not convertible to int");
        }
        r->type.n=-1;
        r->lval=true;
        r->ct=false;
	    if(exprPostfixPrim(r)){
        return true;
      }
      else{
        tkerr("Expresie invalida dupa ]");
      }
      }
      else{
        tkerr("Lipsa ]");
      }
    }
  }
  else if(consume(DOT)){
            if(consume(ID)){
                Token *tkName = consumedTk;
                if(r->type.tb!=TB_STRUCT){
                  tkerr("a field can only be selected from a struct");
                }
                Symbol *s=findSymbolInList(r->type.s->structMembers,tkName->text);
                if(!s){
                  tkerr("the structure %s does not have a field %s",r->type.s->name,tkName->text);
                }
                *r=(Ret){s->type,true,s->type.n>=0};
                if(exprPostfixPrim(r)){
                  return true;
                } 
                else{
                  tkerr("Lipsa expresie");
                }
    }
    else{
      tkerr("Lipsa nume camp");
    }
  }
  iTk=t;
  return true;
}

bool exprPostfix(Ret *r){
    Token *t = iTk;
  if(exprPrimary(r)){
    if(exprPostfixPrim(r)) return true;
  }
  iTk=t;
  return false;
}


bool exprPrimary(Ret *r) {
    Token *t = iTk;
  if(consume(INT)){
    *r = (Ret){{TB_INT,NULL,-1},false,true};
    return true;
  }
  if(consume(DOUBLE)){
    *r = (Ret){{TB_DOUBLE,NULL,-1},false,true};
    return true;
  }
  if(consume(CHAR)){
    *r = (Ret){{TB_CHAR,NULL,-1},false,true};
    return true;
  }
  if(consume(STRING)){
    *r = (Ret){{TB_CHAR,NULL,0},false,true};
    return true;
  }
  if(consume(LPAR)){
      if(expr(r)){
          if(consume(RPAR)) return true;
          else{
            tkerr("Lipseste )");
          }
      }
  }
  if(consume(ID)){
      Token *tkName = consumedTk;
      Symbol *s=findSymbol(tkName->text);
      if(!s){
        tkerr("undefined id: %s",tkName->text);
      }
      if(consume(LPAR)){
          if(s->kind!=SK_FN){
            tkerr("only a function can be called");
          }
          Ret rArg;
          Symbol *param=s->fn.params;
          if(expr(&rArg)){
              if(!param){
                tkerr("too many arguments in function call");
              }
              if(!convTo(&rArg.type,&param->type)){
                tkerr("in call, cannot convert the argument type to the parameter type");
              }
              param=param->next;
              for(;;){
                  if(consume(COMMA)){
                      if(expr(&rArg)){
                        if(!param)tkerr("too many arguments in function call");
                        if(!convTo(&rArg.type,&param->type))tkerr("in call, cannot convert the argument type to the parameter type");
                        param=param->next;
                      } else {
                          tkerr("argument invalid dupa virgula");
                         } 
                  }
                  else break;
              }
          }
          if(consume(RPAR)){
            if(param){
              tkerr("too few arguments in function call");
            }
            *r=(Ret){s->type,false,true};
          }
          else{
            if(s->kind==SK_FN){
              tkerr("a function can only be called");
            }
            *r=(Ret){s->type,true,s->type.n>=0};
            tkerr("Lipsa )");
          }
      }
      else{
        if(s->kind==SK_FN) {
				tkerr("a function can only be called"); 
			  }
			  *r = (Ret){s->type,true,s->type.n>=0};
      }
    return true;
  }
  iTk=t;
  return false;
}

void parse(Token *tokens){
  iTk=tokens;
  if(!unit())tkerr("syntax error");
}
