// gcc -Wall -Wextra -pedantic -I/usr/include/libxml2 -o xml_valid_example xml_valid_example.c -lxml2

#include <string.h>
#include <libxml/xmlschemas.h>


xmlSchemaPtr parseSchemaDoc( xmlDocPtr schemaDoc )
{
  xmlSchemaParserCtxtPtr schemaParserCtxt = xmlSchemaNewDocParserCtxt( schemaDoc );
  if( NULL == schemaParserCtxt )
	{
	  return NULL;
	}

  xmlSchemaPtr schema = xmlSchemaParse( schemaParserCtxt );

  xmlSchemaFreeParserCtxt( schemaParserCtxt );

  return schema;
}

int validateSchemaPtrSubjDoc( xmlSchemaPtr schemaPtr, xmlDocPtr subjDoc )
{

  // create an XML Schemas validation context based on the given schema
  xmlSchemaValidCtxtPtr schemaValidCtxt = xmlSchemaNewValidCtxt( schemaPtr );
  if( NULL == schemaValidCtxt )
	{
	  return -1;
	}

  // validate a document tree in memory
  int result = xmlSchemaValidateDoc( schemaValidCtxt, subjDoc );

  // free the resources associated to the schema validation context
  xmlSchemaFreeValidCtxt( schemaValidCtxt );
  
  return result;
}


int validateSchemaDocSubjDoc( xmlDocPtr schemaDoc, xmlDocPtr subjDoc )
{
  xmlSchemaPtr schemaPtr = parseSchemaDoc( schemaDoc );
  if( NULL == schemaPtr )
	{
	  return -1;
	}

  int result = validateSchemaPtrSubjDoc( schemaPtr, subjDoc );

  xmlSchemaFree( schemaPtr );

  return result;
}

int validateSchemaDocSubjString( xmlDocPtr schemaDoc, char const* subjString )
{
  xmlDocPtr subjDoc = xmlReadMemory( subjString, strlen( subjString ), "doc.xml", NULL, 0 );

  int result = validateSchemaDocSubjDoc( schemaDoc, subjDoc );

  xmlFreeDoc( subjDoc );

  return result;
}

int validateSchemaStringSubjString( char const* schemaString, char const* subjString )
{

  xmlDocPtr schemaDoc = xmlReadMemory( schemaString, strlen( schemaString ), "schema.xml", NULL, 0 );

  int result = validateSchemaDocSubjString( schemaDoc, subjString );

  xmlFreeDoc( schemaDoc );

  return result;
}

int main( void )
{

  char const* docString = "<doc/>";
  char const* schemaDocString = "";

  int result = validateSchemaStringSubjString( schemaDocString, docString );

  printf( "%d\n", result );
  
  return result;
}
