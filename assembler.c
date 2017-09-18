#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

#define MAX_LINE_LENGTH 255



struct Symbols {
	char *labels[256];
	int addresses[256];
	int length;
}

typedef struct Symbols SymbolTable;

struct OpList {
	const char *codes[31];
}

typdef struct OpList Opcodes;

Opcodes Opcompare = { "ADD", "AND", "JMP", "JSR", "JSRR", "LDB", "LDW", "LEA", "NOP", "NOT", "RET", "LSHF", "RSHF", "RSHFA", "RTI", "STB", "STW", "XOR", "TRAP", "HALT", "BRn", "BRz", "BRp", "BR", "BRzp", "BRnp", "BRnz", "BRnzp", ".ORIG", ".END", ".FILL" }

int isOpcode (char *str) {
	int i, cmp;
	for( i=0; i<31; i++) {
		cmp = strcmp(Opcompare.codes[i], str);
		if(cmp == 1) {
			return 0;
		}
	}
	return -1;
}

enum {
	DONE, OK, EMPTY_LINE
};



int toNum( char * pStr )
{
	char * t_ptr;
	char * orig_pStr;
	int t_length,k;
	int lNum, lNeg = 0;
	long int lNumLong;

	orig_pStr = pStr;
	if( *pStr == '#' )                                /* decimal */
  	{ 
    		pStr++;
    		if( *pStr == '-' )                                /* dec is negative */
    		{
      			lNeg = 1;
      			pStr++;
    		}
    		t_ptr = pStr;
    		t_length = strlen(t_ptr);
    		for(k=0;k < t_length;k++)
    		{
      			if (!isdigit(*t_ptr))
      			{
         			printf("Error: invalid decimal operand, %s\n",orig_pStr);
         			exit(4);
      			}
      			t_ptr++;
    		}
    		lNum = atoi(pStr);
    		if (lNeg)
      			lNum = -lNum;

    		return lNum;
  	}
  	else if( *pStr == 'x' )        /* hex     */
  	{
    		pStr++;
    		if( *pStr == '-' )                                /* hex is negative */
    		{
    			lNeg = 1;
      			pStr++;
    		}
    		t_ptr = pStr;
    		t_length = strlen(t_ptr);
    		for(k=0;k < t_length;k++)
    		{
      			if (!isxdigit(*t_ptr))
      			{
         			printf("Error: invalid hex operand, %s\n",orig_pStr);
         			exit(4);
      			}
      			t_ptr++;
    		}
    		lNumLong = strtol(pStr, NULL, 16);    /* convert hex string into integer */
    		lNum = (lNumLong > INT_MAX)? INT_MAX : lNumLong;
    		if( lNeg )
      			lNum = -lNum;
    
		return lNum;
  	}
  	else
  	{
        	printf( "Error: invalid operand, %s\n", orig_pStr);
        	exit(4);  /* This has been changed from error code 3 to error code 4, see clarification 12 */
  	}
}



int readAndParse( FILE * pInfile, char * pLine, char ** pLabel, char** pOpcode, char ** pArg1, char ** pArg2, char ** pArg3, char ** pArg4 )
        {
           char * lRet, * lPtr;
           int i;
           if( !fgets( pLine, MAX_LINE_LENGTH, pInfile ) )
                return( DONE );
           for( i = 0; i < strlen( pLine ); i++ )
                pLine[i] = tolower( pLine[i] );
           
          /* convert entire line to lowercase */
           *pLabel = *pOpcode = *pArg1 = *pArg2 = *pArg3 = *pArg4 = pLine + strlen(pLine);

           /* ignore the comments */
           lPtr = pLine;

           while( *lPtr != ';' && *lPtr != '\0' && *lPtr != '\n' ) 
                lPtr++;

           *lPtr = '\0';
           if( !(lPtr = strtok( pLine, "\t\n ," ) ) ) 
                return( EMPTY_LINE );

           if( isOpcode( lPtr ) == -1 && lPtr[0] != '.' ) /* found a label */
           {
                *pLabel = lPtr;
                if( !( lPtr = strtok( NULL, "\t\n ," ) ) ) return( OK );
           }
           
          *pOpcode = lPtr;

           if( !( lPtr = strtok( NULL, "\t\n ," ) ) ) return( OK );
           
          *pArg1 = lPtr;
           
          if( !( lPtr = strtok( NULL, "\t\n ," ) ) ) return( OK );

           *pArg2 = lPtr;
           if( !( lPtr = strtok( NULL, "\t\n ," ) ) ) return( OK );

           *pArg3 = lPtr;

           if( !( lPtr = strtok( NULL, "\t\n ," ) ) ) return( OK );

           *pArg4 = lPtr;

           return( OK );
        }


void validLabel(char *label){
	int i = 0;
	if(strlen(label) > 20){
		printf("Error code 4: Label exceeds 20 characters");
		exit(4);
	}
	if(*label < 97 || *label > 122 || *label == 120){
		printf("Error code 4: Label does not begin with a letter");
		exit(4);
	} 
	for( i=0; i < strlen(label); i++ ){
		if( !( isalnum(label[i]) ) ){
			printf("Error code 4: Label contains an invalid character");
			exit(4);
		}
	}
	i=0;
	while(i < symbols.length){
		if( strcmp(symbols.labels[i], label) == 0){
			printf("Error code 4: Label already exists");
			exit(4);
		}
		i++;
	}
	if( *label == "r" && (*(label+1) > -1 && *(label+1) < 8) ){
			printf("Error code 4: Label is a register name");
			exit(4);
	}
}

//create a string of the last 3 bits
//for(i = 0; i < 3; i++
//mask first bit of the arg
//place bit into out array
//right shift
void RegConv(char *arg, char *out){
	arg = arg+1;
	int i = 0;
	for( i=0; i<3; i++) {
		out[2-i] = *arg & 0x01;
		*arg = (*arg) >> 1;
	}
}


void Translate( char *opcode, char *arg1, char *arg2, char *arg3, char *arg4) {
	char *op;
	char *operand;
	char *instruction;
	if( (strcmp(opcode, "add") == 0 || strcmp(opcode, "and") == 0 || strcmp(opcode, "xor") == 0) && *arg3 == "r"){
		if( opcode[1] == "d"){
			op = strcpy("0001");
		}
		else if( opcode[1] == "n" ){
			op = strcpy("0101");
		}
		else {
			op = strcpy("1001");	
		}
		RegConv(arg1,operand);
		instruction = strcat(op, operand);
		RegConv(arg2,operand);
		instruction = strcat(instruction, operand);
		instruction = strcat(instruction, "000");
		RegConv(arg3, operand);
		instruction = strcat(instruction, operand);
	}
}





int start, current;

FILE *infile = NULL;
FILE *outfile = NULL;

void main (int argc, char *argv[]) {
	char lLine[MAX_LINE_LENGTH + 1], *lLabel, *lOpcode, *lArg1, *lArg2, *lArg3, *lArg4;

	SymbolTable symbols;
	symbols.length = 0;
	
	int lRet;
	start=-1;
	int nl = 0;

	
	//open the source file
	infile = fopen(argv[1], "r");
	outfile = fopen(argv[2], "w");

	if( !infile) {
		printf( "Error: Cannot open file %s\n", argv[1]);
		exit(4);
	}
	if( !outfile ) {
		printf( "Error: Cannot open file %s\n", argv[2]);
		exit(4);
	}
	//remove if doesn't open correctly



	FILE * lInfile;

	lInfile = fopen( "data.in", "r" );

	//1st Pass - Create Symbol Table
	do
 	{
		lRet = readAndParse( lInfile, lLine, &lLabel, &lOpcode, &lArg1, &lArg2, &lArg3, &lArg4);
		if( lRet != DONE && lRet != EMPTY_LINE ){
			if(nl == 0 && start == -1 && (strcmp(".ORIG", lOpcode) != 0)){
				printf("Error code 4: Does not start with .ORIG");
				exit(4);
			}
			nl = nl+2;
			if(strcmp(".ORIG", lOpcode) == 0) {
				start = toNum(lArg1); 		//get starting address from .ORIG
				if( (start % 2) != 0) {
					printf("Error code 3: Starting address is not aligned")
					exit(2);
				}
				nl = -2;			//nl = offset from starting address
			}
			if(*lLabel != NULL){			//stores label in symbol table
				validLable(lLabel);
				strcpy(symbols.labels[symbols.length], lLabel);
				symbols.addresses[symbols.length] = start + nl;
				symbols.length++;
			}
		
		}
	} while (lRet != DONE );
	
	rewind(lInfile);
	
	//2nd Pass - Perform Translation of Assembly to Machine Language
	current = start;
	do
	{
		lRet = readAndParse( lInfile, lLine, &lLabel, &lOpcode, &lArg1, &lArg2, &lArg3, &lArg4);
		if( lRet != DONE && lRet != EMPTY_LINE ){
			
			Translate(lOpcode, lArg1, lArg2, lArg3, lArg4);


		}
	} while (lRet != DONE );


	fclose(infile);
	fclose(outfile);


}
