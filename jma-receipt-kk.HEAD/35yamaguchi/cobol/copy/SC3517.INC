      *******************************************************************
      * Project code name "ORCA"
      * Æü°åÉ¸½à¥ì¥»¥×¥È¥½¥Õ¥È¡ÊJMA standard receipt software¡Ë
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
        01  SC17.
      *°õºþÇ¯·îÆü
            03  SC17-PRTYY                 PIC X(02).
            03  SC17-PRTMM                 PIC X(02).
            03  SC17-PRTDD                 PIC X(02).
      *»ÔÄ®Â¼Ì¾
            03  SC17-CITYNAME              PIC X(10).
      *ÉéÃ´¼ÔÈÖ¹æ
            03  SC17-FTNJANUM.
                05  SC17-FTNJANUM1         PIC X(01).
                05  SC17-FTNJANUM2         PIC X(01).
                05  SC17-FTNJANUM3         PIC X(01).
                05  SC17-FTNJANUM4         PIC X(01).
                05  SC17-FTNJANUM5         PIC X(01).
                05  SC17-FTNJANUM6         PIC X(01).
                05  SC17-FTNJANUM7         PIC X(01).
                05  SC17-FTNJANUM8         PIC X(01).
      *°åÎ¸µ¡´Ø¥³¡¼¥É
            03  SC17-HOSPCD.
                05  SC17-HOSPCD1           PIC X(01).
                05  SC17-HOSPCD2           PIC X(01).
                05  SC17-HOSPCD3           PIC X(01).
                05  SC17-HOSPCD4           PIC X(01).
                05  SC17-HOSPCD5           PIC X(01).
                05  SC17-HOSPCD6           PIC X(01).
                05  SC17-HOSPCD7           PIC X(01).
      *½»½ê
            03  SC17-ADRS                  PIC X(100).
      *°åÎÅµ¡´ØÌ¾
            03  SC17-HOSPNAME              PIC X(100).
      *³«Àß¼ÔÌ¾
            03  SC17-KAISETU               PIC X(40).
      *¿ÇÎÅÇ¯·î
            03  SC17-SRYYM-TBL          OCCURS  5 TIMES.
                05  SC17-SRYYM.
                    07  SC17-SRYYM1        PIC X(01).
                    07  SC17-SRYYM2        PIC X(01).
                    07  SC17-SRYYM3        PIC X(01).
                    07  SC17-SRYYM4        PIC X(01).
      *Æþ³°¶èÊ¬
            03  SC17-NYUGAIKBN-TBL      OCCURS  5 TIMES.
                05  SC17-NYUGAIKBN         PIC X(01).
      *¼õµë¼ÔÈÖ¹æ
            03  SC17-KIGO-TBL           OCCURS  5 TIMES.
                05  SC17-KIGO.
                    07  SC17-KIGO1         PIC X(01).
                    07  SC17-KIGO2         PIC X(01).
                    07  SC17-KIGO3         PIC X(01).
            03  SC17-BANGO-TBL          OCCURS  5 TIMES.
                05  SC17-BANGO.
                    07  SC17-BANGO1        PIC X(01).
                    07  SC17-BANGO2        PIC X(01).
                    07  SC17-BANGO3        PIC X(01).
                    07  SC17-BANGO4        PIC X(01).
                    07  SC17-BANGO5        PIC X(01).
      *Æþ±¡Ç¯·îÆü
            03  SC17-NYUINYMD-TBL       OCCURS  5 TIMES.
                05  SC17-NYUINGYMD.
                    07  SC17-NYUINGYMD1    PIC X(01).
                    07  SC17-NYUINGYMD2    PIC X(01).
                    07  SC17-NYUINGYMD3    PIC X(01).
                    07  SC17-NYUINGYMD4    PIC X(01).
                    07  SC17-NYUINGYMD5    PIC X(01).
                    07  SC17-NYUINGYMD6    PIC X(01).
                    07  SC17-NYUINGYMD7    PIC X(01).
      *¹ñ¼Ò¶èÊ¬
            03  SC17-HKN-TBL            OCCURS  5 TIMES.
                05  SC17-HKN               PIC X(01).
      *¿ÇÎÅ²Ê
            03  SC17-SRKA-TBL           OCCURS  5 TIMES.
                05  SC17-SRYKA.
                    07  SC17-SRYKA1        PIC X(01).
                    07  SC17-SRYKA2        PIC X(01).
      *Æü¿ô
            03  SC17-NISSU-TBL          OCCURS  5 TIMES.
                05  SC17-NISSU.
                    07  SC17-NISSU1        PIC X(01).
                    07  SC17-NISSU2        PIC X(01).
      *Ê¡»ã°åÎÅÈñÀÁµá³Û
            03  SC17-SEIKYUGK-TBL       OCCURS  5 TIMES.
                05  SC17-SEIKYUGK.
                    07  SC17-SEIKYUGK1     PIC X(01).
                    07  SC17-SEIKYUGK2     PIC X(01).
                    07  SC17-SEIKYUGK3     PIC X(01).
                    07  SC17-SEIKYUGK4     PIC X(01).
                    07  SC17-SEIKYUGK5     PIC X(01).
                    07  SC17-SEIKYUGK6     PIC X(01).
      *¿©»öÆü¿ô
            03  SC17-SNISSU-TBL         OCCURS  5 TIMES.
                05  SC17-SNISSU.
                    07  SC17-SNISSU1       PIC X(01).
                    07  SC17-SNISSU2       PIC X(01).
      *¿©»ö½õÀ®³Û
            03  SC17-JOSEIGK-TBL        OCCURS  5 TIMES.
                05  SC17-JOSEIGK.
                    07  SC17-JOSEIGK1      PIC X(01).
                    07  SC17-JOSEIGK2      PIC X(01).
                    07  SC17-JOSEIGK3      PIC X(01).
                    07  SC17-JOSEIGK4      PIC X(01).
                    07  SC17-JOSEIGK5      PIC X(01).
                    07  SC17-JOSEIGK6      PIC X(01).
      *ÌôºÞ°ìÉôÉéÃ´¶â
            03  SC17-YKZFTN-TBL         OCCURS  5 TIMES.
                05  SC17-YKZFTN.
                    07  SC17-YKZFTN1       PIC X(01).
                    07  SC17-YKZFTN2       PIC X(01).
                    07  SC17-YKZFTN3       PIC X(01).
                    07  SC17-YKZFTN4       PIC X(01).
                    07  SC17-YKZFTN5       PIC X(01).
                    07  SC17-YKZFTN6       PIC X(01).
                    07  SC17-YKZFTN7       PIC X(01).
