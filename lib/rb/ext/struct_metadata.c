#include "ruby.h"
#include "constants.h"
#include "struct_metadata.h"
#include "ruby_ptr.h"


#define DEBUG 0



#include "debug.h"

#define STRUCT_FIELDS(obj) rb_const_get(CLASS_OF(obj), fields_const_id)

static ID metadata_id;
static ID fields_id;

static field_metadata* createAndCompileField(int id, VALUE field_info);
static void compileField(int id, VALUE field_info, field_metadata* fmd);


static VALUE get_name_id(char* field_name) {
  DEBUG_FUNCTION_ENTRY();

  char name_buf[strlen(field_name) + 2];

  name_buf[0] = '@';
  strcpy(&name_buf[1], field_name);

  VALUE v = rb_intern(name_buf);

  DEBUG_FUNCTION_EXIT();

  return v;
}


field_metadata* getFieldMetadataByID(struct_metadata* md, int id)
{
  DEBUG_FUNCTION_ENTRY();
	if (md->maxid < id) rb_raise(rb_eRuntimeError, "Unknown entry %d for metadata object. maximum ID is %d", id, md->maxid);
	if (md->index[id] == NULL) rb_raise(rb_eRuntimeError, "Entry %d for metadata object not present.", id);

  DEBUG_FUNCTION_EXIT();
	return md->index[id];
}

// SLOOOOOOOW!!!
field_metadata* getFieldMetadataByName(struct_metadata* md, char* name)
{
	printf("SLOW EXEC\n");


	DEBUG_FUNCTION_ENTRY();
	int i;

	for(i=0;i<md->size;i++)
		if (strcmp(md->fields[i].name, name) == 0)
		{
			DEBUG_FUNCTION_EXIT();
			return &md->fields[i];
		}

	rb_raise(rb_eRuntimeError, "Entry '%s' for metadata object not present.", name);

	DEBUG_FUNCTION_EXIT();
	return NULL;
}

field_metadata* getFieldMetadataByIndex(struct_metadata* md, int idx)
{
	DEBUG_FUNCTION_ENTRY();
	if (md->size <= idx) rb_raise(rb_eRuntimeError, "Index %d out of range for metadata object. Maximum is %d", idx, md->size - 1);
	

	DEBUG_FUNCTION_EXIT();
	return &md->fields[idx];
}

int getMetadataFieldCount(struct_metadata* md)
{
	DEBUG_FUNCTION_ENTRY();
  	DEBUG_FUNCTION_EXIT();
	return md->size;
}

static void compileField(int id, VALUE field_info, field_metadata* fmd)
{
  	DEBUG_FUNCTION_ENTRY();

	fmd->id = id;

	DEBUG_FUNCTION_PROGRESS();
	VALUE v_type = rb_hash_aref(field_info, type_sym);
	fmd->type = v_type == Qnil ? 0 : NUM2INT(v_type);

	//fmd->optional = NUM2INT(rb_hash_aref(field_info, optional_sym));  <-- not used currently. TODO

	DEBUG_FUNCTION_PROGRESS();
	VALUE v_name = rb_hash_aref(field_info, name_sym);
	fmd->name = v_name == Qnil ? 0 : strdup(RSTRING_PTR(v_name));
	fmd->name_id = v_name == Qnil ? Qnil : get_name_id(fmd->name);

	DEBUG_FUNCTION_PROGRESS();
  	DEBUGF("id=%d type=%d, name=%s", fmd->id, fmd->type, fmd->name);

  	VALUE v_klass = rb_hash_aref(field_info, class_sym);

  	
	fmd->klass_md = v_klass == Qnil ? 0 : getStructMetadataEx(v_klass, 1);
	fmd->klass_v = v_klass;
	if (v_klass != Qnil) rb_const_set(v_klass, rb_intern("do_not_garbage_collect_me"), v_klass);

	DEBUG_FUNCTION_PROGRESS();

	VALUE v_element = rb_hash_aref(field_info, element_sym);
	fmd->element = v_element == Qnil ? 0 : createAndCompileField(-1, v_element);
	DEBUG_FUNCTION_PROGRESS();

	VALUE v_key = rb_hash_aref(field_info, key_sym);
	fmd->key = v_key == Qnil ? 0 : createAndCompileField(-1, v_key);

	DEBUG_FUNCTION_PROGRESS();
	VALUE v_value = rb_hash_aref(field_info, value_sym);
	fmd->value = v_value == Qnil ? 0 : createAndCompileField(-1, v_value);

	DEBUG_FUNCTION_PROGRESS();
	DEBUG_FUNCTION_EXIT();
}

static field_metadata* createAndCompileField(int id, VALUE field_info)
{
  	DEBUG_FUNCTION_ENTRY();
	
	if (field_info == Qnil)
	{
		DEBUG_FUNCTION_EXIT();
		return NULL;
	}

	field_metadata* fmd = malloc(sizeof(field_metadata));

	compileField(id, field_info, fmd);

	DEBUG_FUNCTION_EXIT();
	return fmd;
}


static struct_metadata* compileMetadata(VALUE klass)
{  	
	printf("Compile Class=%s\n", RSTRING_PTR(rb_class_name(klass)));

  	DEBUG_FUNCTION_ENTRY();

	int i;
	struct_metadata* met = malloc(sizeof(struct_metadata));

	VALUE struct_fields = rb_const_get(klass, fields_const_id); 
	if (struct_fields == Qnil) rb_raise(rb_eRuntimeError, "Could not find class constant FIELDS in supplied klass parameter when compiling metadata!");
	VALUE field_ids = rb_funcall(struct_fields, rb_intern("keys"), 0); 
	if (field_ids == Qnil) rb_raise(rb_eRuntimeError, "Could not get FIELDS keys from supplied klass parameter when compiling metadata!");

	met->size = RARRAY_LEN(field_ids);
	met->maxid = -1;

	DEBUGF("size=%d", met->size);

	met->fields = calloc(sizeof(field_metadata), met->size);

	for(i=0;i<met->size;i++)
	{
		VALUE field_id = rb_ary_entry(field_ids, i);
		VALUE field_info = rb_hash_aref(struct_fields, field_id);

		int id = NUM2INT(field_id);
		compileField(id, field_info, &met->fields[i]);

		if (id > met->maxid) met->maxid = id;
	}

	DEBUGF("maxid=%d", met->maxid);
	//Initialize index
	met->index = calloc(sizeof(field_metadata*), met->maxid + 1);
	for(i=0;i<met->size;i++)
		met->index[met->fields[i].id] = &met->fields[i];
	
	DEBUG_FUNCTION_EXIT();
	return met;
}

struct_metadata* getStructMetadataEx(VALUE klass, int autocompile)
{
	DEBUG_FUNCTION_ENTRY();

	if (klass == Qnil) 
	{
		DEBUG_FUNCTION_EXIT();
		return NULL;
	}

	DEBUGF("Class=%s", RSTRING_PTR(rb_class_name(klass)));

	if (rb_const_defined(klass, metadata_id))
	{
		DEBUG_FUNCTION_EXIT();
		return NUM2PTR(rb_const_get(klass, metadata_id));
	}

	DEBUG_FUNCTION_PROGRESS();

	if (autocompile)
	{
		struct_metadata *md = compileMetadata(klass);
		rb_const_set(klass, metadata_id, PTR2NUM(md));
		DEBUG_FUNCTION_EXIT();
		return md;
	}

	rb_raise(rb_eRuntimeError, "No compiled metadata found, and autocompilation disabled!");
	DEBUG_FUNCTION_EXIT();
	return NULL;
}

struct_metadata* getStructMetadata(VALUE klass)
{
  DEBUG_FUNCTION_ENTRY();
  struct_metadata* md = getStructMetadataEx(klass, 1);
  DEBUG_FUNCTION_EXIT();
  
  return md;
}



void Init_struct_metadata()
{
	DEBUG_FUNCTION_ENTRY();
	metadata_id = rb_intern("metadata_ptr");
	fields_id = rb_intern("FIELDS");
	DEBUG_FUNCTION_EXIT();
}