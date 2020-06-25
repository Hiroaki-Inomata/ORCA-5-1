--
-- 病名入力セット
--
--
create  table   tbl_byomeiinputset (
   HOSPNUM smallint DEFAULT 0 NOT NULL,
   BYOMEIINPUTSET     character varying(10) NOT NULL,
   BYOMEIINPUTSETNAME character varying(40),
   BYOMEIINPUTCD1  character varying(20),
   BYOMEIINPUTCD2  character varying(20),
   BYOMEIINPUTCD3  character varying(20),
   BYOMEIINPUTCD4  character varying(20),
   BYOMEIINPUTCD5  character varying(20),
   BYOMEIINPUTCD6  character varying(20),
   TERMID          character varying(64),
   OPID            character varying(16),
   CREYMD          character(8),
   UPYMD           character(8),
   UPHMS           character(6)
);


ALTER TABLE ONLY tbl_byomeiinputset
    ADD CONSTRAINT tbl_byomeiinputset_primary_key PRIMARY KEY (HOSPNUM,BYOMEIINPUTSET);
