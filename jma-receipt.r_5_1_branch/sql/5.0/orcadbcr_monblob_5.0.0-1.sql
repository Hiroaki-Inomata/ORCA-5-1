--                            --
-- monblob                    --
--                            --
-- Create Date : 2016/11/30   --
--                            --



CREATE TABLE monblob (
    id character varying(37) NOT NULL,
    importtime timestamp with time zone,
    lifetype integer,
    filename character varying(4096),
    file_data bytea
);

ALTER TABLE ONLY monblob
    ADD CONSTRAINT monblob_pkey PRIMARY KEY (id);


