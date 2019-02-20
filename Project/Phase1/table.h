#include <stdio.h>

#define TABLE_SIZE 500

/* struct to hold each entry */
struct entry_into_state
{
	char* lexeme; 
	int token_name;
	struct entry_into_state* successor;
};

typedef struct entry_into_state entry_into_table;


// Traverse the hash table and print all the entries
void display(entry_into_table** hash_table_ptr)
{
	int i;
	entry_into_table* traverser;
    printf("\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n");
    printf("\t < lexeme , token >\n");
    printf("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n");

	for( i=0; i < TABLE_SIZE; i++)
	{
		traverser = hash_table_ptr[i];

		while( traverser != NULL)
		{
			printf("< %-20s, %3d >\n", traverser->lexeme, traverser->token_name);
			traverser = traverser->successor;
		}
	}
   
}


/* Create a new hash_table. */
entry_into_table** table_create()
{
	entry_into_table** hash_table_ptr = NULL; // declare a pointer

	/* Allocate memroy for a hashtable array of size TABLE_SIZE */
	if( ( hash_table_ptr = ( entry_into_table** ) malloc( sizeof( entry_into_table* ) * TABLE_SIZE ) ) == NULL )
    	return NULL;

	int i;
	
	// Intitialise all entries as NULL
    for( i = 0; i < TABLE_SIZE; i++ )
	{
		hash_table_ptr[i] = NULL;
	}

	return hash_table_ptr;
}

/* Generate hash from a string. Then generate an index in [0, TABLE_SIZE) */
uint32_t hash( char *lexeme )
{
	size_t i;
	uint32_t hash;

	/* Apply jenkin's hash function
	* https://en.wikipedia.org/wiki/Jenkins_hash_function#one-at-a-time
	*/
	for ( hash = i = 0; i < strlen(lexeme); ++i ) {
        hash += lexeme[i];
        hash += ( hash << 10 );
        hash ^= ( hash >> 6 );
    }
	hash += ( hash << 3 );
	hash ^= ( hash >> 11 );
    hash += ( hash << 15 );

	return hash % TABLE_SIZE; // return an index in [0, TABLE_SIZE)
}

/* Create an entry for a lexeme, token pair. This will be called from the insert function */
entry_into_table *create( char *lexeme, int token_name )
{
	entry_into_table *newentry;

	/* Allocate space for newentry */
	if( ( newentry = ( entry_into_table* )malloc( sizeof( entry_into_table ) ) ) == NULL ) {
		return NULL;
	}
	/* Copy lexeme to newentry location using strdup (string-duplicate). Return NULL if it fails */
	if( ( newentry->lexeme = strdup( lexeme ) ) == NULL ) {
		return NULL;
	}

	newentry->token_name = token_name;
	newentry->successor = NULL;

	return newentry;
}

/* Search for an entry given a lexeme. Return a pointer to the entry of the lexeme exists, else return NULL */
entry_into_table* search( entry_into_table** hash_table_ptr, char* lexeme )
{
	uint32_t idx = 0;
	entry_into_table* myentry;
    
    // get the index of this lexeme as per the hash function
	idx = hash( lexeme );

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

/* Insert an entry into a hash table. */
void insert( entry_into_table** ptr, char* lexeme, int token_name )
{
	if( search( ptr, lexeme ) != NULL) // If lexeme already exists int the table, then don't insert
	    return;

	uint32_t idx;
	entry_into_table* newentry = NULL;
	entry_into_table* head = NULL;

	idx = hash( lexeme ); // Retrieving the index for this lexeme using the hash function
	newentry = create( lexeme, token_name ); // Create new entry using the <lexeme, token> pair

	if(newentry == NULL) // handling any error due to insufficient memory or other errors
	{
		printf("Insert failed. New entry could not be created.");
		exit(1);
	}

	head = ptr[idx]; // finding head entry at idx

	if(head == NULL) // The first lexeme that matches the hash index value
	{
		ptr[idx] = newentry;
	}
	else // normal entry in the table
	{
		newentry->successor = ptr[idx];
		ptr[idx] = newentry;
	}
}


