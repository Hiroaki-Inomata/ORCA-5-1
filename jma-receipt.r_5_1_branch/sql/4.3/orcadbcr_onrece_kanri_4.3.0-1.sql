create	table	tbl_onrece_kanri	(
	HOSPNUM	numeric(2,0),
	TEISYUTUSAKI	char(1),
	SRYYM	char(6),
	UCNT	numeric(3,0),
	SCNT	numeric(2,0),
	RENNUM	numeric(3,0),
	SYORIYMD	char(8),
	UKEYMD		char(8),
	SKYYMD		char(8),
	RECV_FLG	numeric(1,0),
	AMOUNT_FLG	numeric(1,0),
	SEND_PATIENT_CNT	numeric(5,0),
	NOSEND_PATIENT_CNT	numeric(5,0),
	SEND_TOTAL_CNT	numeric(5,0),
        TERMID  varchar(64),
        OPID    varchar(16),
        CREYMD  char(8),
        UPYMD   char(8),
        UPHMS   char(6),
	primary	key(
		HOSPNUM,
		TEISYUTUSAKI,
		SRYYM,
		UCNT,
		SCNT,
		RENNUM
	)
);
