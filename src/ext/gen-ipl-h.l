
%Start yS yR

D       [0-9]
A       [A-Za-z_$,:]
B       [A-Za-z0-9_$,?:]
W       [ \t\r\n]

%%

        BEGIN   yS;

<yS>\/\/.*$		|
<yS>\/\*.*\*\/		;	/* eat comments */

<yS>{W}			;	/* eat white */

<yS>const		;	/* ignore const decls */	

<yS>[tT]itle		{ return T_TITLE; }

<yS>[vV]oid		{ return T_V; }
<yS>[fF]lt		|
<yS>[fF]loat		{ return T_F; }
<yS>double		|
<yS>[dD]bl		{ return T_D; }
<yS>u_char		|
<yS>[iI]nt		{ return T_I; }
<yS>[bB]ool		{ return T_B; }
<yS>[sS]tr		|
<yS>char\*		{ return T_S; }
<yS>[pP]t		|
<yS>[pP]oint		{ return T_P; }
<yS>[iI]mg		|
<yS>[iI]mage		|
<yS>Bimage\*		{ return T_BR; }
<yS>Bimage\&		{ return T_BA; }
<yS>[cC]mplx		|
<yS>[cC]omplex		{ return T_C; }
<yS>[nN]ew		{ return T_NEW; }

<yS>errno		{ return T_ERR; }
<yS>errn0		{ return T_ERR0; }

<yS>operator		{ return T_OPTR; }

<yS>\,			{ return T_COMMA; }
<yS>\;			{ return T_SEMI; }
<yS>\=			{ return T_EQUAL; }

<yS>\(			{ return T_OPEN; }
<yS>\)			{ return T_CLOSE; }

<yS>-?(\.?{D}+|{D}+\.{D}*)	{
				value = atof( yytext );
				return T_VALUE;
			}
<yS>[^ \t\r\n(),;\"]*	{
				strcpy(text, yytext);
				return T_NAME;
			}

<yS>^DEBUG$		{
				yydebug = 1;
				return T_EOL;
			}

<yS>\"          { BEGIN yR; txtlen = 0; }
<yR>\\.         { text[txtlen++] = yytext[1]; }
<yR>\"          { BEGIN yS; text[txtlen] = 0; return T_STRING; }
<yR>.           { text[txtlen++] = *yytext; }


%%


 
