#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <libxml/xmlreader.h>

#include "euc.h"

static	char	*
Strndup(
	char	*s,
	size_t	len)
{
	char	*str;

	if (s != NULL){
		str = malloc(len+1);
		strncpy(str,s,len);
		str[len] = '\0';
	} else {
		str = NULL;
	}
	return	(str);
}

static int
CheckEuc213(
	char *p,
	int size)
{
	int i;
	int advanced_bytes;
	int count;
	int is213;
	int rc;
	char *buf;
	char *in;
	char out[4];

	rc = 1;
	in = buf = Strndup(p,size);
#if 0
printf("size:%d data:",size);
for(i=0;i<size;i++) {
printf("%02X",buf[i] & 0xFF);
}
printf("\n");
#endif
	count = 0;
	while(*in) {
		advanced_bytes = jis213_count(in, out, &is213);
		if (is213 == -1) {
			*(p+count) = ' ';
			*(buf+count) = ' ';
			rc = 0;
		}
		in += advanced_bytes;
		count += advanced_bytes;
	}
#if 0
printf("CheckEuc213 buf[%s]\n",buf);
#endif
	free(buf);
	return rc;
}

static xmlChar *
GetAttributeValue(
	xmlTextReaderPtr reader,
	const xmlChar *name)
{
	if (!xmlTextReaderMoveToAttribute(reader,name)) {
		return NULL;
	}	
	return xmlTextReaderValue(reader);
}

int
EmbedData(xmlTextReaderPtr reader)
{
	char *name;
	char *value;
	int size;
	int isString;
	
	size = 0;
	isString = 0;
	if ((name = xmlTextReaderName(reader)) != NULL) {
		if (!xmlStrcmp(name, "embed")) {
			if ((value = GetAttributeValue(reader, "object")) != NULL) {
				if (!xmlStrcmp(value, "string")) {
					isString = 1;
				}
				if (!xmlStrcmp(value, "image")) {
					size = 1024;
				}
				xmlFree(value);
			}
			if (isString) {
				if ((value = GetAttributeValue(reader, "length")) != NULL) {
					size = atoi(value);
					xmlFree(value);
				}
			}
		}
		xmlFree(name);
	}
	return size;
}

int
Element(xmlTextReaderPtr reader)
{
	char *name;
	char *value;
	int occurs;
	xmlElementType node_type;
	
	occurs = 0;
	
	node_type = xmlTextReaderNodeType(reader);
	if ((name = xmlTextReaderName(reader)) != NULL) {
		if (node_type == XML_READER_TYPE_ELEMENT ) {
		if (!xmlStrcmp(name, "element")) {
			occurs = 1;
			if ((value = GetAttributeValue(reader, "name")) != NULL) {
#if 0
printf("element name[%s]\n",value);
#endif
				xmlFree(value);
			}
			if ((value = GetAttributeValue(reader, "occurs")) != NULL) {
#if 0
printf("element occurs[%s]\n",value);
#endif
				occurs = atoi(value);
				xmlFree(value);
			}
		}
		}
		xmlFree(name);
	}
	return occurs;
}

int
Element_close(xmlTextReaderPtr reader)
{
#if 0
  printf("Enter Element_close\n");
#endif
	char *name;
	int closing;
	xmlElementType node_type;
	
	closing = 0;
	node_type = xmlTextReaderNodeType(reader);
	if ((name = xmlTextReaderName(reader)) != NULL) { 
		if (node_type == XML_READER_TYPE_END_ELEMENT ) {
		if (!xmlStrcmp(name, "element")) {
			closing = 1;
		}
		}
		xmlFree(name);
	}
	return closing;
}

typedef struct _EmbedStruct {
	struct _EmbedStruct *parent;
	struct _EmbedStruct **child;
	int child_num;
	int occurs;
	int block_size;
} EmbedStruct;

#define MAX_CHILD_NUM 128

EmbedStruct *
NewEmbedStruct(void)
{
	EmbedStruct *ret;
	ret = malloc(sizeof(EmbedStruct));
	ret->parent = NULL;
	ret->child = (EmbedStruct **)malloc(sizeof(EmbedStruct *)*MAX_CHILD_NUM);
	ret->child_num = 0;
	ret->occurs = 0;
	ret->block_size = 0;
	return ret;
}

void
FreeEmbedStruct(EmbedStruct *embed)
{
	int i;
	for (i = 0; i < embed->child_num; i++) {
		FreeEmbedStruct(embed->child[i]);
	}
    free(embed->child);
	free(embed);
}

int
EmbedStructSize(EmbedStruct *embed)
{
	int i,j;
	int size;

	size = 0;
	for (i = 0; i < embed->occurs; i++) {
		for (j = 0; j < embed->child_num; j++) {
			size += EmbedStructSize(embed->child[j]);
		}
		if (embed->block_size > 0) {
			size += embed->block_size;
		}
	}
	return size;
}

int
EvalEmbedStruct(EmbedStruct *embed, char *p, int *rc)
{
	int i,j;
	int size;
	
	size = 0;
	for (i = 0; i < embed->occurs; i++) {
		for (j = 0; j < embed->child_num; j++) {
#if 0
printf("size:%d p:%p\n",size,p + size);
#endif
			size += EvalEmbedStruct(embed->child[j],p + size,rc);
		}
		if (embed->block_size > 0) {
			if(!CheckEuc213(p + size,embed->block_size)) {
				*rc = 1;
			}
			size += embed->block_size;
		}
	}
	return size;
}

/*
	for OpenCOBOL 
	arg {
		int path_size;
		int data_size;
		int result
		char path[path_size],
		char data[data_size],
	}
*/
extern void
fixreddata (char *args)
{
	int path_size,data_size;
	int block_size,occurs;
	int done_size,process_size;
	int *rc;
	int i,j;
	char *path,*data;
	int status;
	char *p;
	xmlTextReaderPtr reader;
	EmbedStruct *embed,*newembed;

	p = args;

	path_size = *(size_t *)(p);
    p += sizeof(int);

	data_size = *(size_t *)(p);
    p += sizeof(int);

	rc = (int *)(p);
	*rc = 0;
    p += sizeof(int);

	path = Strndup(p,path_size);
	StringCobol2C(path, path_size);
	p += path_size;

	data = malloc(data_size+1);
	memcpy(data,p,data_size);
	data[data_size] = 0;
	p += data_size;

#if 0
printf("in fixreddata\n");
printf("path_size[%d] data_size[%d] rc[%d]\n",path_size,data_size,*rc);
printf("path[%s]\n",path);
printf("data[%s]\n",data);
#endif

	reader = xmlNewTextReaderFilename(path);
	embed = NULL;
	done_size = 0;
	if (reader != NULL) {
		p = data;
		status = xmlTextReaderRead(reader);
		while(status == 1) {
			status = xmlTextReaderRead(reader);
			if ((occurs = Element(reader)) > 0) {
				newembed = NewEmbedStruct();
				newembed->occurs = occurs;
				if (embed != NULL) {
					embed->child[embed->child_num] = newembed;
					newembed->parent = embed;
					embed->child_num++;
				}
				embed = newembed;
			}
			if ((block_size = EmbedData(reader)) > 0) {
				embed->block_size = block_size;
			}
			if ((j = Element_close(reader)) > 0) {
				if (embed->parent == NULL) {
					process_size = EmbedStructSize(embed);
					if (process_size + done_size > data_size) {
						printf("size of data does not suffice. need size[%d],but data size[%d]",process_size + done_size, data_size);
						*rc = 3;
						break;
					}
					process_size = EvalEmbedStruct(embed,p,rc);
					p += process_size;
					done_size += process_size;
					FreeEmbedStruct(embed);
					embed = NULL;
				} else {
					embed = embed->parent;
				}
			}
		}
		xmlFreeTextReader(reader);
	} else {
		printf("Unable to open %s\n",path);
		*rc = 2;
	}
#if 0
printf("%s\n",data);
printf("%d\n",*rc);
#endif
	memcpy(args + sizeof(int) * 3 + path_size ,data, data_size);
	free(path);
	free(data);
}

int
main(
	int argc,
	char *argv[])
{
	struct _prefix{
		int path_size;
		int data_size;
		int rc;
	};

	struct _prefix pre;
	struct _prefix *p;
	char *arg;
	char *path;
	char *data;

	if (argc < 3) {
		printf("./%s <red> <data>\n",argv[0]);
		exit(1);
	}

	pre.path_size = strlen(argv[1]);
	pre.data_size = strlen(argv[2]);
	pre.rc = 0;
	
	arg = malloc(sizeof(struct _prefix) + pre.path_size + pre.data_size);
	memcpy(arg, &pre, sizeof(struct _prefix));
	memcpy(arg + sizeof(struct _prefix), argv[1], pre.path_size);
	memcpy(arg + sizeof(struct _prefix) + pre.path_size, argv[2], pre.data_size);

	fixreddata(arg);
	p = (struct _prefix *)arg;
	path = Strndup(arg + sizeof(struct _prefix),p->path_size);
	data = Strndup(arg + sizeof(struct _prefix) + p->path_size,p->data_size);
printf("%s\n",data);
printf("%d\n",p->rc);

    free(path);
    free(data);
    free(arg);

	return 0;
}
