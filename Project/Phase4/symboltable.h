#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <string.h>
// #include <vector>
// #include <string>

int yyerror(const char *msg);

using namespace std;

#define HASH_TABLE_SIZE 100
#define MAX_SCOPE 10

int table_index = 0;
int current_scope = 0;

/* struct to hold each node */
struct entry_type
{
	char* lexeme;
	int value;
	int dtype;
	int* parameter_list; // for functions
	int array_dimension;
	int is_constant;
	int num_params;
	struct entry_type* successor;
};

typedef struct entry_type entry_type;

/* Wrapper for symbol table with pointer to symbol table of parent scope */
struct table_t
{
	entry_type** symbol_table;
	int parent;
};

//typedef struct table_s table_t;

struct icg_temp
{
	vector<int> follow_if_correct;
	vector<int> follow_if_wrong;
	vector<int> follow_l;
	vector<int> follow_br;
	vector<int> follow_cont;
	string addr;
	string code;

	entry_type* node;
	int dtype;
};

//typedef struct content_s icg_temp;

extern table_t stable[MAX_SCOPE];

vector<int> append_icg(vector<int>& v1, vector<int>& v2)
{
	vector<int> concat;
	concat.reserve(v1.size() + v2.size());
	concat.insert(concat.end(), v1.begin(), v1.end());
	concat.insert(concat.end(), v2.begin(), v2.end());

	return concat;
}



void display_symbol_table(entry_type** hash_table_ptr);
/* Create a new hash_table. */
entry_type** create_table()
{
	entry_type** hash_table_ptr = NULL; // declare a pointer

	/* Allocate memory for a hashtable array of size HASH_TABLE_SIZE */
	if( ( hash_table_ptr = (entry_type**) malloc( sizeof( entry_type* ) * HASH_TABLE_SIZE ) ) == NULL )
    	return NULL;

	int i;

	// Intitialise all entries as NUscopeLL
    for( i = 0; i < HASH_TABLE_SIZE; i++ )
	{
		hash_table_ptr[i] = NULL;
	}

	return hash_table_ptr;
}

int create_new_scope()
{
	table_index++;
	// printf("Table index:%d\n",table_index);
	stable[table_index].symbol_table = create_table();
	stable[table_index].parent = current_scope;

	return table_index;
}

int exit_scope()
{
	return stable[current_scope].parent;
}
/* Generate myhash from a string. Then generate an index in [0, HASH_TABLE_SIZE) */
int myhash( char *lexeme )
{
	size_t i;
	int hashvalue;

	/* Apply jenkin's myhash function
	* https://en.wikipedia.org/wiki/Jenkins_hash_function#one-at-a-time
	*/

	for ( hashvalue = i = 0; i < strlen(lexeme); ++i ) {
        hashvalue += lexeme[i];
        hashvalue += ( hashvalue << 10 );
        hashvalue ^= ( hashvalue >> 6 );
    }
	hashvalue += ( hashvalue << 3 );
	hashvalue ^= ( hashvalue >> 11 );
    hashvalue += ( hashvalue << 15 );
	//cout<<(hashvalue% HASH_TABLE_SIZE + HASH_TABLE_SIZE) % HASH_TABLE_SIZE<<endl;
	return (hashvalue% HASH_TABLE_SIZE + HASH_TABLE_SIZE) % HASH_TABLE_SIZE; // return an index in [0, HASH_TABLE_SIZE)
}

/* Create an node for a lexeme, token pair. This will be called from the insert function */
entry_type *create_entry( char *lexeme, int value, int dtype )
{
	entry_type *new_entry;

	/* Allocate space for new_entry */
	if( ( new_entry = (entry_type*) malloc( sizeof( entry_type ) ) ) == NULL ) {
		return NULL;
	}
	/* Copy lexeme to new_entry location using strdup (string-duplicate). Return NULL if it fails */
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

/* Search for an node given a lexeme. Return a pointer to the node of the lexeme exists, else return NULL */
entry_type* search(entry_type** hash_table_ptr, char* lexeme)
{
	int idx = 0;
	entry_type* myentry;

    // get the index of this lexeme as per the myhash function
	idx = myhash( lexeme );

	/* Traverse the linked list at this idx and see if lexeme exists */
	myentry = hash_table_ptr[idx];

	while( myentry != NULL && strcmp( lexeme, myentry->lexeme ) != 0 )
	{
		myentry = myentry->successor;
	}

	if(myentry == NULL) // lexeme is not found
		return NULL;

	else // lexeme found
		return myentry;

}

// Search recursively in every parent scope for lexeme
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
/* Insert an node into a myhash table. */
entry_type* insert( entry_type** hash_table_ptr, char* lexeme, int value, int dtype)
{
	// Make sure you pass the current scope symbol table here
	entry_type* finder = search( hash_table_ptr, lexeme );
	if( finder != NULL) // If lexeme already exists, don't insert, return NULL
	{
		if(finder->is_constant)
			return finder;
		return NULL; //capture this is callee code and do necessary error handling
	}

	int idx;
	entry_type* new_entry = NULL;
	entry_type* head = NULL;

	idx = myhash( lexeme ); // Get the index for this lexeme based on the myhash function
	new_entry = create_entry( lexeme, value, dtype ); // Create an node using the <lexeme, token> pair

	if(new_entry == NULL) // In case there was some error while executing create_entry()
	{
		printf("Insert failed. New node could not be created.");
		exit(1);
	}

	head = hash_table_ptr[idx];
	// printf("Index: %d\n",idx);// get the head node at this index

	if(head == NULL) // This is the first lexeme that matches this myhash index
	{
		hash_table_ptr[idx] = new_entry;
	}
	else // if not, add this node to the head
	{
		new_entry->successor = hash_table_ptr[idx];
		hash_table_ptr[idx] = new_entry;
	}

	// printf("in insert! Symbol table :%p, node: %p, text: %s\n",hash_table_ptr, hash_table_ptr[idx], lexeme);
	// display_symbol_table(hash_table_ptr);
	return hash_table_ptr[idx];
}

// This is called after a function call to check if param list match
int check_parameter_list(entry_type* node, int* list, int m)
{
	int* parameter_list = node->parameter_list;

	if(m != node->num_params)
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

void fill_parameter_list(entry_type* node, int* list, int n)
{
	node->parameter_list = (int *)malloc(n*sizeof(int));

	int i;
	for(i=0; i<n; i++)
	{
		node->parameter_list[i] = list[i];
	}
	node->num_params = n;
}


void print_dashes(int n)
{
  printf("\n");

	int i;
	for(i=0; i< n; i++)
	printf("=");
	printf("\n");
}

// Traverse the myhash table and print all the entries
void display_symbol_table(entry_type** hash_table_ptr)
{
	int i;
	entry_type* traverser;

	print_dashes(100);

  printf(" %-20s %-20s %-20s %-20s %-20s\n","lexeme","data-type","array_dimension","num_params","arg_list");

	print_dashes(100);

	for( i=0; i < HASH_TABLE_SIZE; i++)
	{
		// printf("In loop\n");
		traverser = hash_table_ptr[i];
		while( traverser != NULL)
		{
			printf(" %-20s %-20d %-20d ", traverser->lexeme, traverser->dtype, traverser->array_dimension);

			printf(" %-20d", traverser->num_params);

			int j;
			for(j=0; j < traverser->num_params; j++)
			printf(" %d",traverser->parameter_list[j]);
			printf("\n");

			traverser = traverser->successor;
		}
	}

	print_dashes(100);

}

void display_constant_table(entry_type** hash_table_ptr)
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

void display_all()
{
		int i;
		for(i=0; i<=table_index; i++)
		{
			printf("Scope: %d\n",i);
			display_symbol_table(stable[i].symbol_table);
			printf("\n\n");
		}

		// display_symbol_table(stable[0].symbol_table);
		// display_symbol_table(stable[1].symbol_table);
}
