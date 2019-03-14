#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <string.h>

#define HASH_TABLE_SIZE 100
#define MAX_SCOPE 10

int table_index = 0;
int current_scope = 0;

struct entry_s
{
	char* lexeme;
	double value;
	int dtype;
	int* parameter_list; 
	int array_dimension;
	int is_constant;
	int num_params;
	struct entry_s* successor;
};

typedef struct entry_s entry_type;

struct table_s
{
	entry_type** symbol_table;
	int parent;
};

typedef struct table_s table_t;

extern table_t stable[MAX_SCOPE];

entry_type** create_table()
{
	entry_type** hash_table_ptr = NULL; 
	if( ( hash_table_ptr = malloc( sizeof( entry_type* ) * HASH_TABLE_SIZE ) ) == NULL )
    	return NULL;

	int i;

	
    for( i = 0; i < HASH_TABLE_SIZE; i++ )
	{
		hash_table_ptr[i] = NULL;
	}

	return hash_table_ptr;
}

int create_new_scope()
{
	table_index++;

	stable[table_index].symbol_table = create_table();
	stable[table_index].parent = current_scope;

	return table_index;
}

int exit_scope()
{
	return stable[current_scope].parent;
}

uint32_t hash( char *lexeme )
{
	size_t i;
	uint32_t hash;

	for ( hash = i = 0; i < strlen(lexeme); ++i ) {
        hash += lexeme[i];
        hash += ( hash << 10 );
        hash ^= ( hash >> 6 );
    }
	hash += ( hash << 3 );
	hash ^= ( hash >> 11 );
    hash += ( hash << 15 );

	return hash % HASH_TABLE_SIZE; 
}

entry_type *create_entry( char *lexeme, int value, int dtype )
{
	entry_type *new_entry;

	if( ( new_entry = malloc( sizeof( entry_type ) ) ) == NULL ) {
		return NULL;
	}
	if( ( new_entry->lexeme = strdup( lexeme ) ) == NULL ) {
		return NULL;
	}

	new_entry->value = value;
	new_entry->successor = NULL;
	new_entry->parameter_list = NULL;
	new_entry->array_dimension = -1;
	new_entry->is_constant = 0;
	new_entry->num_params = 0;
	new_entry->dtype = dtype;

	return new_entry;
}

entry_type* search(entry_type** hash_table_ptr, char* lexeme)
{
	uint32_t idx = 0;
	entry_type* myentry;

	idx = hash( lexeme );

	myentry = hash_table_ptr[idx];

	while( myentry != NULL && strcmp( lexeme, myentry->lexeme ) != 0 )
	{
		myentry = myentry->successor;
	}

	if(myentry == NULL)
		return NULL;

	else return myentry;

}

entry_type* search_recursive(char* lexeme)
{
	int idx = current_scope;
	entry_type* finder = NULL;

	while(idx != -1)
	{
		finder = search(stable[idx].symbol_table, lexeme);

		if(finder != NULL)
			return finder;

		idx = stable[idx].parent;
	}

	return finder;
}
entry_type* insert( entry_type** hash_table_ptr, char* lexeme, int value, int dtype)
{
	entry_type* finder = search( hash_table_ptr, lexeme );
	if( finder != NULL) 
	{
		if(finder->is_constant)
			return finder;
		return NULL;
	}

	uint32_t idx;
	entry_type* new_entry = NULL;
	entry_type* head = NULL;

	idx = hash( lexeme ); 
	new_entry = create_entry( lexeme, value, dtype ); 

	if(new_entry == NULL) 
	{
		printf("Insert failed. New entry could not be created.");
		exit(1);
	}

	head = hash_table_ptr[idx]; 

	if(head == NULL) 
	{
		hash_table_ptr[idx] = new_entry;
	}
	else 
	{
		new_entry->successor = hash_table_ptr[idx];
		hash_table_ptr[idx] = new_entry;
	}
	return hash_table_ptr[idx];
}


int verify_arg_list(entry_type* entry, int* list, int m)
{
	int* parameter_list = entry->parameter_list;

	if(m != entry->num_params)
	{
		yyerror("Number of parameters and arguments do not match");
	}

	int i;
	for(i=0; i<m; i++)
	{
		if(list[i] != parameter_list[i])
		yyerror("Parameter and argument types do not match");
	}

	return 1;
}

void fill_parameter_list(entry_type* entry, int* list, int n)
{
	entry->parameter_list = (int *)malloc(n*sizeof(int));

	int i;
	for(i=0; i<n; i++)
	{
		entry->parameter_list[i] = list[i];
	}
	entry->num_params = n;
}


void print_dashes(int n)
{
  printf("\n");

	int i;
	for(i=0; i< n; i++)
	printf("-");
	printf("\n");
}

void display_symbol_table(entry_type** hash_table_ptr)
{
	int i;
	entry_type* traverser;

	print_dashes(100);

  printf(" %-15s %-15s %-15s %-15s %-15s\n","lexeme","data_type","array_dim","num_args","parameter_list");

	print_dashes(100);

	for( i=0; i < HASH_TABLE_SIZE; i++)
	{
		traverser = hash_table_ptr[i];
		while( traverser != NULL)
		{
			printf(" %-15s %-15d %-15d ", traverser->lexeme, traverser->dtype, traverser->array_dimension);

			printf(" %-15d", traverser->num_params);

			int j;
			for(j=0; j < traverser->num_params; j++)
			printf(" %d",traverser->parameter_list[j]);
			printf("\n");

			traverser = traverser->successor;
		}
	}

	print_dashes(100);

}

void ctable_disp(entry_type** hash_table_ptr)
{
	int i;
	entry_type* traverser;

	print_dashes(25);

	printf(" %-10s %-10s \n","lexeme","data-type");

	print_dashes(25);

	for( i=0; i < HASH_TABLE_SIZE; i++)
	{
		traverser = hash_table_ptr[i];
		while( traverser != NULL)
		{
			printf(" %-10s %-10d \n", traverser->lexeme, traverser->dtype);
			traverser = traverser->successor;
		}
	}

	print_dashes(25);
}

void stable_disp()
{
		int i;
		for(i=0; i<=table_index; i++)
		{
			printf("Scope: %d\n",i);
			display_symbol_table(stable[i].symbol_table);
			printf("\n\n");
		}
}
