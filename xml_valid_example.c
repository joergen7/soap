// gcc -Wall -Wextra -pedantic -I/usr/include/libxml2 -o xml_valid_example xml_valid_example.c -lxml2

#include <string.h>
#include <libxml/xmlschemas.h>

int parseSchemaDoc( xmlDocPtr schemaDoc, xmlSchemaPtr *schema )
{
  xmlSchemaParserCtxtPtr schemaParserCtxt = xmlSchemaNewDocParserCtxt( schemaDoc );
  if( NULL == schemaParserCtxt )
	{
	  return -1;
	}

  *schema = xmlSchemaParse( schemaParserCtxt );

  xmlSchemaFreeParserCtxt( schemaParserCtxt );

  if( NULL == *schema )
	{
	  return -1;
	}

  return 0;
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
  xmlSchemaPtr schemaPtr;

  int result = parseSchemaDoc( schemaDoc, &schemaPtr );
  if( 0 != result )
	{
	  return result;
	}

  result = validateSchemaPtrSubjDoc( schemaPtr, subjDoc );

  xmlSchemaFree( schemaPtr );

  return result;
}

int validateSchemaDocSubjString( xmlDocPtr schemaDoc, char const* subjString )
{
  xmlDocPtr subjDoc = xmlReadMemory( subjString, strlen( subjString ), "doc.xml", NULL, 0 );
  if( NULL == subjDoc )
	{
	  return -1;
	}

  int result = validateSchemaDocSubjDoc( schemaDoc, subjDoc );

  xmlFreeDoc( subjDoc );

  return result;
}

int validateSchemaStringSubjString( char const* schemaString, char const* subjString )
{

  xmlDocPtr schemaDoc = xmlReadMemory( schemaString, strlen( schemaString ), "schema.xml", NULL, 0 );
  if( NULL == schemaDoc )
	{
	  return -1;
	}

  int result = validateSchemaDocSubjString( schemaDoc, subjString );

  xmlFreeDoc( schemaDoc );

  return result;
}

int main( void )
{

  char const* docString = "<note/>";
  char const* schemaDocString = "<?xml version=\"1.0\"?><xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\"><xs:element name=\"note\"></xs:element></xs:schema>";

  int result = validateSchemaStringSubjString( schemaDocString, docString );
  if( 0 == result )
	{
	  printf( "ok\n" );
	}
  else
	{
	  printf( "Error: %d\n", result );
	}
  
  return result;
}
