#ifndef __STRUCT_METADATA_H_
#define __STRUCT_METADATA_H_

#include <ruby.h>

struct _field_metadata;
typedef struct _field_metadata field_metadata;

struct _struct_metadata;
typedef struct _struct_metadata struct_metadata;

struct _field_metadata
{
	int id;
	int type;
	//int optional; not used currently -- TODO
	char* name;
	ID name_id;
	struct_metadata* klass_md;
	VALUE klass_v;

	field_metadata* element;
	field_metadata* key;
	field_metadata* value;
};

struct _struct_metadata
{
	field_metadata* fields;
	field_metadata** index;
	int size;
	int maxid;
};

field_metadata* getFieldMetadataByID(struct_metadata* md, int id);
field_metadata* getFieldMetadataByName(struct_metadata* md, char* name);
field_metadata* getFieldMetadataByIndex(struct_metadata* md, int idx);

struct_metadata* getStructMetadataEx(VALUE klass, int autocompile);
struct_metadata* getStructMetadata(VALUE klass);

int getMetadataFieldCount(struct_metadata* md);

void Init_struct_metadata();

#endif