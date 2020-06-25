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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCR0040.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¥ì¥»¥×¥È
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¥ì¥»¥×¥È¥³¥á¥ó¥ÈÊÔ½¸
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  00/04/23    MCC-¾®Æ¦Âô    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  05.00.01    NACL-Æ£¸¶    18/06/11  ¥Õ¥¡¥¤¥ë¤Î¥¨¥é¡¼½èÍýÄÉ²Ã
      *  05.00.02    NACL-ÌçÏÆ    19/03/28  ÅÅÏÃºÆ¿Ç¥³¥á¥ó¥ÈÆâÍÆ½¤Àµ
      *  05.00.03    NACL-ÌçÏÆ    19/04/03  ¼«Æ°µ­ºÜ¥³¥á¥ó¥È²þ¸µÂÐ±þ
      *  05.00.04    NACL-ÌçÏÆ    19/06/24  ¥ê¥Ï¥Ó¥êÈ¯¾ÉÆü¥ì¥»¥×¥Èµ­ºÜÂÐ±þ
      *                                     ¡Êµ¡Ç½ÄÉ²Ã¡Ë
      *  05.00.05    NACL-ÌçÏÆ    19/08/01  ¥ê¥Ï¥Ó¥êÈ¯¾ÉÆü¥ì¥»¥×¥Èµ­ºÜÂÐ±þ
      *                                     ¡Êµ¡Ç½ÄÉ²Ã¡Ë½¤Àµ
      *  05.00.06    NACL-ÌçÏÆ    20/03/30  ÎáÏÂ£²Ç¯£´·î²þÀµÂÐ±þ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ¥ì¥»¥×¥È£°£²¥Ç¡¼¥¿
           SELECT  RECE02-FILE     ASSIGN  RECE01PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE02-KEY
                                   FILE    STATUS  IS  STS-RECE02.
      *    ¥ì¥»¥×¥È£°£´¥Ç¡¼¥¿¡Ê£É£Î¡Ë
           SELECT  RECE04-FILE     ASSIGN  RECE04PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE04-KEY
                                   FILE    STATUS  IS  STS-RECE04.
      *    ¥ì¥»¥×¥È£°£µ¥Ç¡¼¥¿¡Ê£É£Î¡Ë
           SELECT  RECE05-FILE     ASSIGN  RECE05PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE05-KEY
                                   FILE    STATUS  IS  STS-RECE05.
      *    ¥ì¥»¥×¥È£°£´¥Ç¡¼¥¿¡Ê£Ï£Õ£Ô¡Ë
           SELECT  RECE041-FILE    ASSIGN  RECE041PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE041-KEY
                                   FILE    STATUS  IS  STS-RECE041.
      *    ¥ì¥»¥×¥È£°£µ¥Ç¡¼¥¿¡Ê£Ï£Õ£Ô¡Ë
           SELECT  RECE051-FILE    ASSIGN  RECE051PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE051-KEY
                                   FILE    STATUS  IS  STS-RECE051.
      *    ¥ì¥»¥×¥È£°£´¥Ç¡¼¥¿¡Ê£Ï£Õ£Ô¡Ë¡Ê¥ï¡¼¥¯¡Ë
           SELECT  RECE049-FILE    ASSIGN  RECE049PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE049-KEY
                                   FILE    STATUS  IS  STS-RECE049.
      *    ¥ì¥»¥×¥È£°£µ¥Ç¡¼¥¿¡Ê£Ï£Õ£Ô¡Ë¡Ê¥ï¡¼¥¯¡Ë
           SELECT  RECE059-FILE    ASSIGN  RECE059PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE059-KEY
                                   FILE    STATUS  IS  STS-RECE059.
      *
           SELECT  RECE055-FILE    ASSIGN  RECE055PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE055-KEY
                                   FILE    STATUS  IS  STS-RECE055.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    ¥ì¥»¥×¥È£°£²¥Ç¡¼¥¿
       FD  RECE02-FILE.
       01  RECE02-REC.
           COPY    "CPRCF002.INC".
      *
      *    ¥ì¥»¥×¥È£°£´¥Ç¡¼¥¿
       FD  RECE04-FILE.
       01  RECE04-REC.
           COPY    "CPRCF004.INC".
      *
      *    ¥ì¥»¥×¥È£°£µ¥Ç¡¼¥¿
       FD  RECE05-FILE.
       01  RECE05-REC.
           COPY    "CPRCF005.INC".
      *
       FD  RECE041-FILE.
       01  RECE041-REC. 
           COPY    "CPRCF004.INC"  REPLACING   //RECE04//
                                   BY          //RECE041//.
      *
       FD  RECE051-FILE.
       01  RECE051-REC. 
           COPY    "CPRCF005.INC"  REPLACING   //RECE05//
                                   BY          //RECE051//.
      *¥ï¡¼¥¯
       FD  RECE049-FILE.
       01  RECE049-REC. 
           COPY    "CPRCF004.INC"  REPLACING   //RECE04//
                                   BY          //RECE049//.
      *
       FD  RECE059-FILE.
       01  RECE059-REC. 
           COPY    "CPRCF005.INC"  REPLACING   //RECE05//
                                   BY          //RECE059//.
      *
       FD  RECE055-FILE.
       01  RECE055-REC. 
           COPY    "CPRCF0055.INC".
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     ¥ì¥»¥×¥ÈÌÀºÙ¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE01PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE04PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE05PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE041PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE051PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE049PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE059PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE055PARA//..
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECEERR//.
      *
      *    ¥Õ¥¡¥¤¥ëÌ¾¼èÆÀ
           COPY    "CPRECEDAT1.INC".
           COPY    "CPTEMPFL.INC".
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  STS-AREA.
           03  STS-RECE02          PIC X(02).
           03  STS-RECE04          PIC X(02).
           03  STS-RECE041         PIC X(02).
           03  STS-RECE05          PIC X(02).
           03  STS-RECE051         PIC X(02).
           03  STS-RECE049         PIC X(02).
           03  STS-RECE059         PIC X(02).
           03  STS-RECE055         PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE02          PIC 9(01).
           03  FLG-RECE04          PIC 9(01).
           03  FLG-RECE049         PIC 9(01).
           03  FLG-RECE041         PIC 9(01).
           03  FLG-RECE05          PIC 9(01).
           03  FLG-RECE059         PIC 9(01).
           03  FLG-RECE051         PIC 9(01).
           03  FLG-WRKRECE05       PIC 9(01).
           03  FLG-SHIDOU-COMMENT  PIC 9(01).
           03  FLG-HINKAI-COMMENT  PIC 9(01).
           03  FLG-KYUSEI-COMMENT  PIC 9(01).
           03  FLG-KYUSEI-COMMENT2 PIC 9(01).
           03  FLG-COMMENT         PIC 9(02).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-FIRST           PIC 9(01).
           03  FLG-YAKUTEI         PIC 9(01).
           03  FLG-YAKUTEI2        PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-KYUSEI-OK       PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-NIKOCHINTOU-COMMENT
                                   PIC 9(01).
           03  FLG-RIHA            PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-OSIN-HENSYU     PIC 9(01).
           03  FLG-COMOKIKAE       PIC 9(01).
           03  FLG-COMOKIKAE2      PIC 9(01).
           03  FLG-HENSYUKIN       PIC 9(01).
           03  FLG-SYOUNI-COMMENT  PIC 9(01).
           03  FLG-COMARI          PIC 9(01).
           03  FLG-COMARI2         PIC 9(01).
           03  FLG-SANTEIPLUS      PIC 9(01).
           03  FLG-SETCOM          PIC 9(01).
           03  FLG-MEISKY-OKIKAE   PIC 9(01).
           03  FLG-SINPESU-COMMENT PIC 9(01).
           03  FLG-SINPESUR-COMMENT
                                   PIC 9(01).
           03  FLG-SANTEIPLUS-HANTEI
                                   PIC 9(01).
           03  FLG-TAKYO-HENSYU    PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-DAISAN1         PIC 9(01).
           03  FLG-DAISAN2         PIC 9(01).
           03  FLG-ZAIKYO-COMMENT  PIC 9(01).
           03  FLG-BOKOHO-COMMENT  PIC 9(01).
           03  FLG-TUINZAI-COMMENT PIC 9(01).
           03  FLG-CHECK-MAE       PIC 9(01).
           03  FLG-CHECK-ATO       PIC 9(01).
           03  FLG-RETRY           PIC 9(01).
           03  FLG-ZAIKYO-OK       PIC 9(01).
           03  FLG-ZAIKYOCOMARI    PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-SIPPU-OK        PIC 9(01).
           03  FLG-GET             PIC 9(01).
           03  FLG-RKISAI          PIC 9(01).
           03  FLG-ETC             PIC 9(01).
           03  FLG-SENTAKU-OKIKAE  PIC 9(01).
           03  FLG-CHECK-OK        PIC 9(01).
           03  FLG-CHECK-OK2       PIC 9(01).
           03  FLG-JINNAI-COMMENT  PIC 9(01).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-RECE02          PIC 9(06).
           03  CNT-RECE04          PIC 9(06).
           03  CNT-RECE05          PIC 9(06).
           03  CNT-RECE049         PIC 9(06).
           03  CNT-RECE059         PIC 9(06).
           03  CNT-RECE055         PIC 9(06).
           03  CNT-RECE04-RE       PIC 9(06).
           03  CNT-RECE05-RE       PIC 9(06).
           03  CNT-JINZOU          PIC 9(06).
           03  CNT-RIHA            PIC 9(06).
           03  CNT-JUSHIN          PIC 9(06).
           03  CNT-SANTEIKA        PIC 9(06).
           03  CNT-IN-COMMIT       PIC 9(06).
           03  CNT-IN-CANCEL       PIC 9(06).
           03  CNT-KENSA           PIC 9(06).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(02).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDX6                PIC 9(04).
           03  IDX7                PIC 9(04).
           03  IDX8                PIC 9(04).
           03  IDX9                PIC 9(04).
           03  IDX10               PIC 9(04).
           03  IDX11               PIC 9(04).
           03  IDX12               PIC 9(04).
           03  IDX13               PIC 9(04).
           03  IDX14               PIC 9(04).
           03  IDX15               PIC 9(01).
           03  IDX16               PIC 9(02).
           03  IDX17               PIC 9(04).
           03  IDX18               PIC 9(04).
           03  IDX19               PIC 9(01).
           03  IDX22               PIC 9(04).
           03  IDX26               PIC 9(02).
           03  IDX27               PIC 9(02).
           03  IDX28               PIC 9(02).
           03  IDX29               PIC 9(02).
           03  IDX30               PIC 9(02).
           03  IDX31               PIC 9(02).
           03  IDX32               PIC 9(02).
           03  IDX33               PIC 9(03).
           03  IDX34               PIC 9(03).
           03  IDX35               PIC 9(03).
           03  IDX36               PIC 9(03).
           03  IDX37               PIC 9(03).
           03  IDX38               PIC 9(02).
           03  IDX39               PIC 9(02).
           03  IDX40               PIC 9(02).
           03  IDX41               PIC 9(02).
           03  IDX42               PIC 9(02).
           03  IDX43               PIC 9(02).
           03  IDX45               PIC 9(03).
           03  IDX46               PIC 9(03).
           03  IDX47               PIC 9(03).
           03  IDX48               PIC 9(02).
           03  IDX49               PIC 9(02).
           03  DAY-IDX             PIC 9(04).
           03  DAY-IDX2            PIC 9(02).
           03  IDY                 PIC 9(04).
           03  IDY11               PIC 9(03).
           03  IDY12               PIC 9(03).
           03  IDY13               PIC 9(03).
           03  IDZ                 PIC 9(04).
           03  IDX-RECE05          PIC 9(04).
           03  IDV                 PIC 9(04).
           03  IDW                 PIC 9(04).
           03  IDA                 PIC 9(04).
           03  IDB                 PIC 9(04).
           03  IDC1                PIC 9(02).
           03  IDC2                PIC 9(02).
           03  IDC3                PIC 9(02).
           03  IDC4                PIC 9(02).
           03  RIHA-IDX            PIC 9(02).
           03  RIHA-IDX2           PIC 9(02).
           03  RIHA-ST             PIC 9(02).
           03  RIHA-ST2            PIC 9(02).
           03  RIHA-ED             PIC 9(02).
           03  RIHA-ED2            PIC 9(02).
      *     
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM              PIC  X(06).
               05  WRK-PARA-TERMID             PIC  X(16).
               05  WRK-PARA-SYSYMD             PIC  X(08).
               05  WRK-PARA-JOBID              PIC  9(07).
               05  WRK-PARA-SHELLID            PIC  X(08).
           03  WRK-PARA-HOSPNUM                PIC 9(02).
      *
           03  WRK-JYUNBAN         PIC 9(04).
           03  WRK-GYOSUU          PIC 9(04).
           03  WRK-GYOSUU2         PIC 9(04).
           03  WRK-RENNUM          PIC 9(02).
           03  WRK-CNT-MEISAI      PIC 9(04).
           03  WRK-KETA            PIC 9(04).
           03  WRK-KETA-TAIHI      PIC 9(04).
           03  WRK-KETA2           PIC 9(04).
           03  WRK-KOHBUNRI        PIC 9(01).
           03  WRK-KOHBUNRI-JIDO   PIC 9(01).
           03  WRK-KOHBUNRI-JIDO2  PIC 9(01).
           03  WRK-HKNCOMBI-JIDO   PIC 9(04).
           03  WRK-HKNCOMBI-JIDO2  PIC 9(04).
           03  WRK-ZAINUM          PIC 9(08).
      *
           03  WRK-FSANTEIYMD      PIC 9(08).
           03  WRK-AFTER2          PIC 9(08).
           03  WRK-AFTER180        PIC 9(08).
           03  WRK-TOSEKIYMD       PIC 9(08).
           03  WRK-KYUSEIYMD       PIC 9(08).
           03  WRK-HINKAIYMD       PIC 9(08).
           03  WRK-DAY             PIC 9(02).
      *
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-SRYCD           PIC X(09).
           03  WRK-SRYCD2          PIC X(09).
           03  WRK-NIKOCHINTOU-YMD PIC X(08).
      *
           03  WRK-HENSYU-KAISU    PIC X(02).
           03  WRK-HENSYU-KAISU2   PIC X(04).
      *
           03  WRK-RUIKEITOTAL     PIC 9(13)V9(05).
           03  WRK-RUIKEITOTAL2    PIC 9(14).
      *
           03  TBL-SANTEI-DAY.
               05  TB-SANTEI-DAY     PIC 9(02)   OCCURS  31.
      *
      *    ³ºÅö¿ÇÎÅ·îÊ¬
           03  TBL-RIHASANTEI-DAY.
               05  TB-RIHASANTEI-DAY PIC 9(01)   OCCURS  31.
      *
           03  TBL-RIHASANTEI-DAY-END.
               05  TB-RIHASANTEI-DAY-END
                                     PIC 9(01)   OCCURS  31.
           03  TBL-RIHASANTEI-DAY-END2.
               05  TB-RIHASANTEI-DAY-END2
                                     PIC 9(01)   OCCURS  31.
           03  TBL-RIHASANTEI-DAY-END3.
               05  TB-RIHASANTEI-DAY-END3
                                     PIC 9(01)   OCCURS  31.
      *
      *    Ä¾¶áÊ¬
           03  WRK-RIHASANTEI-YMD    PIC X(08).
      *
      *    Ä¾¶áÊ¬ÂàÈòÍÑ
           03  WRK-RIHASANTEI-YMD2   PIC X(08).
           03  WRK-RIHASANTEI-YMD3   PIC X(08).
      *
      *    Á°·î°ÊÁ°¤ÎÄ¾¶á½ªÎ»Æü
           03  WRK-RIHASANTEI-END    PIC X(08).
      *
      *    £²£°£°£¶Ç¯£´·î£±Æü¤Ë»»ÄêÆü¤¬¤¢¤ë¤«
           03  WRK-RIHASANTEI41-FLG  PIC 9(01).
      *
      *    ¥ê¥Ï¥Ó¥ê³«»ÏÆü»ØÄê¥Á¥§¥Ã¥¯ÍÑ
           03  WRK-CHECK-RIHASANTEI-AREA.
               05  WRK-CHECK-RIHASANTEI-YMD     PIC X(08).
               05  WRK-CHECK-RIHASANTEI-YMD3    PIC X(08).
               05  WRK-CHECK-RIHASANTEI-DAY-OCC.
                 07  WRK-CHECK-RIHASANTEI-DAY   PIC 9(01)  OCCURS  31.
      *
           03  WRK-HKNCOMBINUMX      PIC X(04).
           03  WRK-HKNCOMBINUM9      REDEFINES  WRK-HKNCOMBINUMX
                                     PIC 9(04).
      *
           03  WRK-SRYYMDX.
               05  WRK-SRYYM         PIC 9(06).
               05  WRK-SRYDD         PIC 9(02).
      *
           03  WRK-KEISAN            PIC 9(09).
           03  WRK-KEISAN2           PIC 9(03).
           03  WRK-KEISAN3           PIC 9(09).
           03  WRK-KEISAN4           PIC 9(09).
           03  WRK-KEISAN-TAIHI      PIC X(09).
      *
           03  WRK-KYUSEI-NYUGAIKBN  PIC X(01).
           03  WRK-KYUSEI-SRYKA      PIC X(02).
           03  WRK-KYUSEI-ZAINUM     PIC 9(08).
      *
           03  WRK-ZAIKYO-NYUGAIKBN  PIC X(01).
           03  WRK-ZAIKYO-SRYKA      PIC X(02).
           03  WRK-ZAIKYO-ZAINUM     PIC 9(08).
      *
           03  WRK-HENYMD            PIC X(08).
           03  WRK-TAIHI-XX          PIC X(02).
      *
           03  WRK-HENSYU-KINGAKU    PIC X(09).
           03  WRK-HENSYU-KINGAKU2   PIC X(18).
           03  WRK-HENSYU-KINGAKU3   PIC X(18).
      *
           03  WRK-RIHAIGAKU-SRYCD           PIC X(09).
           03  WRK-RIHAIGAKU-SRYCD2          PIC X(09).
           03  WRK-RIHAIGAKU-FSANTEIYMD      PIC X(08).
           03  WRK-RIHAIGAKU-SANTEI-AREA.
               05  WRK-RIHAIGAKU-SANTEI-FLG  PIC X(01).
               05  WRK-RIHAIGAKU-SANTEI-DAY  PIC 9(03)
                                             OCCURS 31.
      *
           03  WRK-SYOUNI-FSANTEIYMD         PIC X(08).
      *
           03  WRK-HEN-RENNUM                PIC 9(02).
           03  WRK-HEN-SHORI-RENNUM          PIC 9(01).
           03  WRK-HEN-SEQNO                 PIC 9(02).
      *
           03  WRK-HEN-RENNUM-X              PIC 9(02).
           03  WRK-HEN-SHORI-RENNUM-X        PIC 9(01).
           03  WRK-HEN-SEQNO-X               PIC 9(02).
      *
           03  WRK-HEN-RENNUM-J              PIC 9(02).
           03  WRK-HEN-SHORI-RENNUM-J        PIC 9(01).
           03  WRK-HEN-SEQNO-J               PIC 9(02).
      *
           03  WRK-HEN-RENNUM-J2             PIC 9(02).
           03  WRK-HEN-SHORI-RENNUM-J2       PIC 9(01).
           03  WRK-HEN-SEQNO-J2              PIC 9(02).
      *
           03  WRK-HEN-RENNUM-J3             PIC 9(02).
           03  WRK-HEN-SHORI-RENNUM-J3       PIC 9(01).
           03  WRK-HEN-SEQNO-J3              PIC 9(02).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-SINPESU-SRYCD             PIC X(09).
           03  WRK-SINPESU-YMD               PIC X(08).
           03  WRK-SINPESUR-YMD              PIC X(08).
           03  WRK-ZAIKYO-YMD                PIC X(08).
           03  WRK-ZAIKYO-YMD2               PIC X(08).
           03  WRK-BOKOHO-YMD                PIC X(08).
           03  WRK-TUINZAI-YMD               PIC X(08).
           03  WRK-NINRYO-FSANTEIYMD         PIC X(08).
      *
           03  WRK-BUNKAKUKETA               PIC 9(02).
           03  WRK-BUNKAKU-N                 PIC X(10).
           03  WRK-SRYKAISU-X                PIC ZZ9.
           03  WRK-CHAR-HENKAN-AREA.
               05  WRK-CHAR-X                PIC X(03).
               05  WRK-CHAR-N                PIC X(06).
               05  WRK-CHAR-N-CNT            PIC 9(01).
               05  WRK-CHAR-IDX              PIC 9(01).
      *
           03  WRK-RIHA-HKNCOMBI             PIC 9(04).
      *
           03  WRK-GANSIDO-YMD-AREA.
               05  WRK-GANSIDO-YMD           PIC X(08)
                                             OCCURS 10.
      *
           03  WRK-BOKOHO-YMD9               PIC X(08).
           03  WRK-BOKOHO-YMD2-AREA.
               05  WRK-BOKOHO-YMD2           PIC X(08)
                                             OCCURS 30.
      *
           03  WRK-ZAIKYO-YMD9               PIC X(08).
           03  WRK-ZAIKYO-KAISU              PIC 9(02).
      *
           03  WRK-SEIKENTEN                 PIC ZZZZZZ9.
      *
           03  WRK-SIPPU-ZAICNT              PIC 9(08).
      *
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1              PIC ZZZZZ.
               05  WRK-SURYO-Z2              PIC X(01).
               05  WRK-SURYO-Z3              PIC ZZZZZ.
           03  WRK-SURYOST                   PIC 9(02).
           03  WRK-SURYOKETA                 PIC 9(02).
           03  WRK-SURYOKETA1                PIC 9(02).
           03  WRK-SURYOKETA2                PIC 9(02).
           03  WRK-SURYO-N.
               05  WRK-SURYO-N1              PIC X(10).
               05  WRK-SURYO-N2              PIC X(02).
               05  WRK-SURYO-N3              PIC X(10).
           03  WRK-SRYSURYOW                 PIC X(24).
           03  WRK-SURYO                     PIC 9(05)V9(05).
           03  WRK-SURYO-R  REDEFINES  WRK-SURYO.
               05  WRK-SURYO-S1              PIC 9(05).
               05  WRK-SURYO-S2              PIC 9(05).
           03  WRK-CHAR2-HENKAN-AREA.
               05  WRK-CHAR2-X               PIC X(10).
               05  WRK-CHAR2-N               PIC X(20).
               05  WRK-CHAR2-CNT             PIC 9(02).
               05  WRK-CHAR2-N-CNT           PIC 9(02).
           03  WRK-CHAR2-IDX                 PIC 9(02).
      *
           03  WRK-INGAISIPPU-NAME           PIC X(300).
      *
           03  WRK-GENBAKU-TOTALKIN          PIC 9(07).
           03  WRK-GENBAKU-FTNKIN            PIC 9(07)V9(01).
           03  WRK-GENBAKU-FTNRATE.
               05  WRK-GENBAKU-FTNRATE1      PIC 9(02).
               05  WRK-GENBAKU-FTNRATE2      PIC 9(01).
      *
           03  WRK-CHECK-SRYCD               PIC X(09).
      *
           03  WRK-SJIYMD                    PIC X(08).
      *
           03  WRK-ONLINE-COMCD              PIC X(09).
           03  WRK-ONLINE-FSANTEIYM          PIC X(08).
      *
           03  WRK-TENSU-SRYCD               PIC X(09).
           03  WRK-TENSU-SRYYM               PIC X(06).
      *
           03  WRK-JINNAI-YMD                PIC X(08).
      *
           03  WRK-GAIJIR-FSANTEIYM          PIC X(08).
           03  WRK-GAIJIR-KAISU              PIC 9(02).
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-ERRMSG                          PIC X(300).
           03  WRK-MCP-RC                          PIC 9(9).
           03  WRK-ERR-FILE-STS                    PIC X(02).
           03  WRK-ERR-FILE-MSG                    PIC X(100).
           03  WRK-ERR-FILE-MSG1                   PIC X(17).
           03  WRK-ERR-FLG                         PIC 9(01).
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ëÌ¾¾ÎÎÎ°è 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *    FULLNAMEÆâ¤ÎBASENAME³«»Ï°ÌÃÖ
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *
       01  WRK-RECE04-REC. 
           COPY    "CPRCF004.INC"  REPLACING   //RECE04//
                                   BY          //WRK-RECE04//.
      *
       01  WRK-RECE05-REC. 
           COPY    "CPRCF005.INC"  REPLACING   //RECE05//
                                   BY          //WRK-RECE05//.
      *
       01  WRK-RECE05XX-REC.
         02 WRK-RECE05XX-TBL       OCCURS 100.
           COPY    "CPRCF005.INC"  REPLACING   //RECE05//
                                   BY          //WRK-RECE05XX//.
      *
       01  WRK-RECE05ZZ-REC.
         02 WRK-RECE05ZZ-TBL       OCCURS 100.
           COPY    "CPRCF005.INC"  REPLACING   //RECE05//
                                   BY          //WRK-RECE05ZZ//.
      *
       01  WRK-FREE-AREA.
           03  WRK-FREEINF                     OCCURS 49.         
               05  WK-FREE-YUKOKETA          PIC  9(03).
               05  WK-FREE-NAME              PIC  X(300).
      *
       01  WRK-FREE-AREA2.                                              
           03  WRK-FREEINF2                    OCCURS 50.               
               05  WK-FREE-YUKOKETA2         PIC  9(03).                
               05  WK-FREE-NAME2             PIC  X(300).               
               05  WK-FREE-MEISKYFLG2        PIC  X(01).
               05  WK-FREE-SETCOMATFLG2      PIC  9(03).
      *
       01  WRK-FREE-AREA3.                                              
           03  WRK-FREEINF3.
               05  WK-FREE-NAME3             PIC  X(1000).              
               05  WK-FREE-NAME3-TAIHI       PIC  X(1000).              
      *
       01  WRK-JIDO-SETCOM-AREA.                                              
           03  WRK-JIDO-SETCOMATFLG          PIC  9(03).
           03  WRK-JIDO-SETCOMFLG            PIC  X(01).
           03  WRK-JIDO-JYOUGEFLG            PIC  9(01).
      *
      *¿Í¹©¿ÕÂ¡¼Â»Ü¥«¥ì¥ó¥À¡¼
       01  WRK-JINZOU-AREA.
           03  WRK-JINZOU-T                    OCCURS   5.
             05  WRK-JINZOU                    PIC  X(01)  OCCURS 31.   
             05  WRK-JINZOU-HKNCOMBI           PIC  9(04).
      *
      *ºÆ¿Ç£´²óÌÜ°Ê¹ß¼Â»Ü¥«¥ì¥ó¥À¡¼
       01  WRK-SAISHIN4-AREA.
           03  WRK-SAISHIN4-T                  OCCURS   5.
             05  WRK-SAISHIN4                  PIC  X(01)  OCCURS 31.   
      *
      *    ÆüÉÕÁ´³ÑÊÑ´¹ÎÎ°è
       01  TBL-DAY-AREA.
           03  FILLER              PIC X(04)  VALUE "¡¡£±".
           03  FILLER              PIC X(04)  VALUE "¡¡£²".
           03  FILLER              PIC X(04)  VALUE "¡¡£³".
           03  FILLER              PIC X(04)  VALUE "¡¡£´".
           03  FILLER              PIC X(04)  VALUE "¡¡£µ".
           03  FILLER              PIC X(04)  VALUE "¡¡£¶".
           03  FILLER              PIC X(04)  VALUE "¡¡£·".
           03  FILLER              PIC X(04)  VALUE "¡¡£¸".
           03  FILLER              PIC X(04)  VALUE "¡¡£¹".
           03  FILLER              PIC X(04)  VALUE "£±£°".
           03  FILLER              PIC X(04)  VALUE "£±£±".
           03  FILLER              PIC X(04)  VALUE "£±£²".
           03  FILLER              PIC X(04)  VALUE "£±£³".
           03  FILLER              PIC X(04)  VALUE "£±£´".
           03  FILLER              PIC X(04)  VALUE "£±£µ".
           03  FILLER              PIC X(04)  VALUE "£±£¶".
           03  FILLER              PIC X(04)  VALUE "£±£·".
           03  FILLER              PIC X(04)  VALUE "£±£¸".
           03  FILLER              PIC X(04)  VALUE "£±£¹".
           03  FILLER              PIC X(04)  VALUE "£²£°".
           03  FILLER              PIC X(04)  VALUE "£²£±".
           03  FILLER              PIC X(04)  VALUE "£²£²".
           03  FILLER              PIC X(04)  VALUE "£²£³".
           03  FILLER              PIC X(04)  VALUE "£²£´".
           03  FILLER              PIC X(04)  VALUE "£²£µ".
           03  FILLER              PIC X(04)  VALUE "£²£¶".
           03  FILLER              PIC X(04)  VALUE "£²£·".
           03  FILLER              PIC X(04)  VALUE "£²£¸".
           03  FILLER              PIC X(04)  VALUE "£²£¹".
           03  FILLER              PIC X(04)  VALUE "£³£°".
           03  FILLER              PIC X(04)  VALUE "£³£±".
       01  TBL-DAY-AREA-R          REDEFINES   TBL-DAY-AREA.
           03  TBL-DAY             PIC X(04)   OCCURS   31.
      *
       01  MAX-MEISAISU            PIC 9(07).
      *
      *    ¸ÇÄêÃÍ
       01  CONST-AREA.
      *    ËýÀ­¤È¤¦ÄË¼À´µ
           03  CONS-MANSEI          PIC X(09) VALUE "113006510".
      *    ºßÂð·ì±ÕÆ©ÀÏ»ØÆ³´ÉÍýÎÁ
           03  CONS-TOSEKI          PIC X(09) VALUE "114009310".
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü
           03  CONS-KYUSEI          PIC X(09) VALUE "099800101".
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾É½ªÎ»Æü
           03  CONS-KYUSEI-END      PIC X(09) VALUE "099800102".
      *    ¥Ë¥³¥Á¥ó°ÍÂ¸¾É´ÉÍýÎÁ¡Ê½é²ó¡Ë
           03  CONS-NIKOCHIN        PIC X(09) VALUE "113008310".
      *    ½ÅÅÙÓÃÂ©´µ¼Ô¼£ÎÅ´ÉÍý²Ã»»¡Ê£±·îÌÜ¡Ë
           03  CONS-ZENSOKU         PIC X(09) VALUE "113008070".
      *    ¿´Âç¥ê¥Ï³«»ÏÆü
           03  CONS-SINDAI          PIC X(09) VALUE "099800111".
      *    ¸ÆµÛ´ï¥ê¥Ï³«»ÏÆü
           03  CONS-KOKYUKI         PIC X(09) VALUE "099800141".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï³«»ÏÆü
           03  CONS-NOUKE           PIC X(09) VALUE "099800121".
      *    ±¿Æ°´ï¥ê¥Ï³«»ÏÆü
           03  CONS-UNDOKI          PIC X(09) VALUE "099800131".
      *    ÆñÉÂ´µ¼Ô¥ê¥Ï³«»ÏÆü
           03  CONS-NANBYO          PIC X(09) VALUE "099800161".
      *    ¾ã³²»ù¡Ê¼Ô¡Ë¥ê¥Ï³«»ÏÆü
           03  CONS-SYOGAI          PIC X(09) VALUE "099800171".
      *    ÀÝ¿©ÎÅË¡³«»ÏÆü
           03  CONS-SESSYOKU        PIC X(09) VALUE "099800151".
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï³«»ÏÆü
           03  CONS-HAIYO           PIC X(09) VALUE "099800191".
      *
      *    ¿´Âç¥ê¥Ï½ªÎ»Æü
           03  CONS-SINDAI-E        PIC X(09) VALUE "099800112".
      *    ¸ÆµÛ´ï¥ê¥Ï½ªÎ»Æü
           03  CONS-KOKYUKI-E       PIC X(09) VALUE "099800142".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï½ªÎ»Æü
           03  CONS-NOUKE-E         PIC X(09) VALUE "099800122".
      *    ±¿Æ°´ï¥ê¥Ï½ªÎ»Æü
           03  CONS-UNDOKI-E        PIC X(09) VALUE "099800132".
      *    ÆñÉÂ´µ¼Ô¥ê¥Ï½ªÎ»Æü
           03  CONS-NANBYO-E        PIC X(09) VALUE "099800162".
      *    ¾ã³²»ù¡Ê¼Ô¡Ë¥ê¥Ï½ªÎ»Æü
           03  CONS-SYOGAI-E        PIC X(09) VALUE "099800172".
      *    ÀÝ¿©ÎÅË¡½ªÎ»Æü
           03  CONS-SESSYOKU-E      PIC X(09) VALUE "099800152".
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï³«»ÏÆü
           03  CONS-HAIYO-E         PIC X(09) VALUE "099800192".
      *
      *    ¿´Âç¥ê¥Ï²Ã»»³«»ÏÆü
           03  CONS-SINDAI-K        PIC X(09) VALUE "099800211".
      *    ¸ÆµÛ´ï¥ê¥Ï²Ã»»³«»ÏÆü
           03  CONS-KOKYUKI-K       PIC X(09) VALUE "099800241".
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï²Ã»»³«»ÏÆü
           03  CONS-HAIYO-K         PIC X(09) VALUE "099800291".
      *
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£±¡Ë
           03  CONS-SINDAI-IGAKU1   PIC X(09) VALUE "180029910".
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£²¡Ë
           03  CONS-SINDAI-IGAKU2   PIC X(09) VALUE "180030010".
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£±¡Ë
           03  CONS-KOKYUKI-IGAKU1  PIC X(09) VALUE "180030510".
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£²¡Ë
           03  CONS-KOKYUKI-IGAKU2  PIC X(09) VALUE "180030610".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£±¡Ë
           03  CONS-NOUKE-IGAKU1    PIC X(09) VALUE "180030110".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£²¡Ë
           03  CONS-NOUKE-IGAKU2    PIC X(09) VALUE "180030210".
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£±¡Ë
           03  CONS-UNDOKI-IGAKU1   PIC X(09) VALUE "180030310".
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£²¡Ë
           03  CONS-UNDOKI-IGAKU2   PIC X(09) VALUE "180030410".
      *
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¼Â»Ü
           03  CONS-SINDAI-DUMMY    PIC X(09) VALUE "099800311".
      *    ¸ÆµÛ´ï¥ê¥Ï¼Â»Ü
           03  CONS-KOKYUKI-DUMMY   PIC X(09) VALUE "099800341".
      *    Ç¾·ì´É¼À´µ¥ê¥Ï¼Â»Ü
           03  CONS-NOUKE-DUMMY     PIC X(09) VALUE "099800321".
      *    ±¿Æ°´ï¥ê¥Ï¼Â»Ü
           03  CONS-UNDOKI-DUMMY    PIC X(09) VALUE "099800331".
      *
      *    ¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ¡Ê£±²óÌÜ¡Ë
           03  CONS-SYOUNI          PIC X(09) VALUE "113000810".
      *
      *    ¿´Â¡¥Ú¡¼¥¹¥á¡¼¥«¡¼»ØÆ³´ÉÍýÎÁ¡Ê±ó³Ö¥â¥Ë¥¿¥ê¥ó¥°¡Ë
           03  CONS-SINPESU         PIC X(09) VALUE "113001510".
      *    ±ó³Ö¥â¥Ë¥¿¥ê¥ó¥°²Ã»»
           03  CONS-SINPESU2        PIC X(09) VALUE "113018070".
      *
      *    Ç§ÃÎÎÅË¡¡¦Ç§ÃÎ¹ÔÆ°ÎÅË¡
           03  CONS-NINRYO          PIC X(09) VALUE "180033210".
      *    Ç§ÃÎÎÅË¡¡¦Ç§ÃÎ¹ÔÆ°ÎÅË¡¡ÊÀº¿À²ÊµßµÞÂÎÀ©¤ÎÀº¿ÀÊÝ·ò»ØÄê°å¡Ë
           03  CONS-NINRYO2         PIC X(09) VALUE "180035910".
      *    °åÎÅ´Ñ»¡Ç§ÃÎÎÅË¡¡¦Ç§ÃÎ¹ÔÆ°ÎÅË¡¡Ê¥¤°Ê³°¤Î¾ì¹ç¡Ë
           03  CONS-NINRYO3         PIC X(09) VALUE "188006210".
      *    °åÎÅ´Ñ»¡Ç§ÃÎÎÅË¡¡¦Ç§ÃÎ¹ÔÆ°ÎÅË¡¡ÊÀº¿À²ÊµßµÞÂÎÀ©¤ÎÀº¿ÀÊÝ·ò»ØÄê°å¡Ë
           03  CONS-NINRYO4         PIC X(09) VALUE "188007010".
      *    Ç§ÃÎÎÅË¡¡¦Ç§ÃÎ¹ÔÆ°ÎÅË¡¡ÊÀº¿ÀÊÝ·ò»ØÄê°å¤È´Ç¸î»Õ¤¬¶¦Æ±¤Ç¹Ô¤¦¾ì¹ç¡Ë
           03  CONS-NINRYO5         PIC X(09) VALUE "180047810".
      *    °åÎÅ´Ñ»¡Ç§ÃÎÎÅË¡¡¦Ç§ÃÎ¹ÔÆ°ÎÅË¡¡ÊÀº¿ÀÊÝ·ò»ØÄê°å¤È´Ç¸î»Õ¤¬¶¦Æ±¡Ë
           03  CONS-NINRYO7         PIC X(09) VALUE "188010610".
      *    Ç§ÃÎÎÅË¡¡¦Ç§ÃÎ¹ÔÆ°ÎÅË¡¡Ê°å»Õ¡Ë
           03  CONS-NINRYO8         PIC X(09) VALUE "180056410".
      *
      *    Ç§ÃÎ¾ÉÀìÌç¿ÇÃÇ´ÉÍýÎÁ£²
           03  CONS-NINSEN          PIC X(09) VALUE "113014610".
      *    ¾®»ù±¿Æ°´ï¼À´µ»ØÆ³´ÉÍýÎÁ
           03  CONS-SYOUNDO         PIC X(09) VALUE "113024310".
      *    ÎÅÍÜ¡¦½¢Ï«Î¾Î©»Ù±ç»ØÆ³ÎÁ
           03  CONS-RYOSYU          PIC X(09) VALUE "113027910".
      *    Ç§ÃÎ¾É¥µ¥Ý¡¼¥È»ØÆ³ÎÁ
           03  CONS-NINSAPO         PIC X(09) VALUE "113028310".
      *
      *    ²ð¸î¿¦°÷ÅùÓ½áâµÛ°úÅù»Ø¼¨ÎÁ
           03  CONS-KAISYOKU        PIC X(09) VALUE "114020910".
      *
      *    ¿´Â¡¥Ú¡¼¥¹¥á¡¼¥«¡¼»ØÆ³´ÉÍýÎÁ¡ÊÃåÍÑ·¿¼«Æ°½üºÙÆ°´ï¡Ë
           03  CONS-SINPESUR1       PIC X(09) VALUE "113015010".
      *    ¿´Â¡¥Ú¡¼¥¹¥á¡¼¥«¡¼»ØÆ³´ÉÍýÎÁ¡Ê¥í¡¦¿¢¹þ·¿½üºÙÆ°´ï°Ü¹Ô´ü²Ã»»´Þ¤à¡Ë
           03  CONS-SINPESUR2       PIC X(09) VALUE "113015130".
      *    ¿´Â¡¥Ú¡¼¥¹¥á¡¼¥«¡¼»ØÆ³´ÉÍýÎÁ¡Ê¥¤°Ê³°¡Ë
           03  CONS-SINPESUR3       PIC X(09) VALUE "113001610".
      *    ¿´Â¡¥Ú¡¼¥¹¥á¡¼¥«¡¼»ØÆ³´ÉÍýÎÁ¡Ê¥Ú¡¼¥¹¥á¡¼¥«¡¼¡Ë
           03  CONS-SINPESUR4       PIC X(09) VALUE "113030210".
      *    ¿´Â¡¥Ú¡¼¥¹¥á¡¼¥«¡¼»ØÆ³´ÉÍýÎÁ¡Ê¿¢¹þ·¿½üºÙÆ°´ïÅù¡Ë
           03  CONS-SINPESUR5       PIC X(09) VALUE "113030310".
      *
      *    ¤¬¤ó´µ¼Ô»ØÆ³´ÉÍýÎÁ¡Ê°å»Õ¡¦´Ç¸î»Õ¤¬¿´ÍýÅªÉÔ°Â·Ú¸º¤Î¤¿¤áÌÌÀÜ¡Ë
           03  CONS-GANSIDO1        PIC X(09) VALUE "113015310".
      *    ¤¬¤ó´µ¼Ô»ØÆ³´ÉÍýÎÁ¡Ê°å»ÕÅù¤¬¹£°­À­¼ðáçºÞ¤ÎÉ¬Í×À­Åù¤òÊ¸½ñÀâÌÀ¡Ë
           03  CONS-GANSIDO2        PIC X(09) VALUE "113015410".
      *
      *    ºßÂð´µ¼Ô¶¦Æ±¿ÇÎÅÎÁ³«»ÏÆü
           03  CONS-ZAIKYO          PIC X(09) VALUE "099140004".
      *
      *    ¼§µ¤¤Ë¤è¤ëç¯æùÅù»É·ãË¡³«»ÏÆü
           03  CONS-BOKOHO1         PIC X(09) VALUE "099400001".
      *    ¼§µ¤¤Ë¤è¤ëç¯æùÅù»É·ãË¡
           03  CONS-BOKOHO2         PIC X(09) VALUE "140055110".
      *
      *    ÄÌ±¡¡¦ºßÂðÀº¿ÀÎÅË¡³«»ÏÆü
           03  CONS-TUINZAI         PIC X(09) VALUE "099830102".
      *
      *    °ÍÂ¸¾É½¸ÃÄÎÅË¡³«»ÏÆü¡ÊÌôÊª°ÍÂ¸¾É¡Ë
           03  CONS-IZONSYO         PIC X(09) VALUE "099830103".
      *    °ÍÂ¸¾É½¸ÃÄÎÅË¡³«»ÏÆü¡Ê¥®¥ã¥ó¥Ö¥ë°ÍÂ¸¾É¡Ë
           03  CONS-IZONSYO2        PIC X(09) VALUE "099830105".
      *
      *    Àº¿À²Ê£Ó£Ã¼À´µÊÌÅùÀìÌç¥×¥í¥°¥é¥à²Ã»»³«»ÏÆü
           03  CONS-SCPRO           PIC X(09) VALUE "099830104".
      *
      *    ·ÐÆ¬³¸¼§µ¤»É·ãÎÅË¡³«»ÏÆü
           03  CONS-JIKSGK          PIC X(09) VALUE "099830106".
      *    ·ÐÆ¬³¸¼§µ¤»É·ãÎÅË¡½ªÎ»Æü
           03  CONS-JIKSGK-E        PIC X(09) VALUE "099830107".
      *
      *    Àº¿À²Ê½Å¾É´µ¼ÔÁá´ü½¸Ãæ»Ù±ç´ÉÍýÎÁ£±¡ÊÃ±°ì·úÊª£±¿Í¡Ë
           03  CONS-SEIJYU          PIC X(09) VALUE "180042110".
      *    Àº¿À²Ê½Å¾É´µ¼ÔÁá´ü½¸Ãæ»Ù±ç´ÉÍýÎÁ£²¡ÊÃ±°ì·úÊª£±¿Í¡Ë
           03  CONS-SEIJYU2         PIC X(09) VALUE "180042410".
      *    Àº¿À²Ê½Å¾É´µ¼ÔÁá´ü½¸Ãæ»Ù±ç´ÉÍýÎÁ£±¡ÊÃ±°ì·úÊª£²¿Í°Ê¾å¡Ë
           03  CONS-SEIJYU3         PIC X(09) VALUE "180048710".
      *    Àº¿À²Ê½Å¾É´µ¼ÔÁá´ü½¸Ãæ»Ù±ç´ÉÍýÎÁ£²¡ÊÃ±°ì·úÊª£²¿Í°Ê¾å¡Ë
           03  CONS-SEIJYU4         PIC X(09) VALUE "180048810".
      *
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£±¡Ê½¸ÃæÅª»Ù±çÉ¬Í×¡Ë¡ÊÃ±°ì·úÊª£±¿Í¡Ë
           03  CONS-SEIZAI          PIC X(09) VALUE "180057210".
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£±¡Ê½¸ÃæÅª»Ù±çÉ¬Í×¡Ë¡ÊÃ±°ì·úÊª£²¿Í°Ê¾å¡Ë
           03  CONS-SEIZAI2         PIC X(09) VALUE "180057310".
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£±¡Ê½ÅÅÙÀº¿À¾ã³²¼Ô¡Ë¡ÊÃ±°ì·úÊª£±¿Í¡Ë
           03  CONS-SEIZAI3         PIC X(09) VALUE "180057410".
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£±¡Ê½ÅÅÙÀº¿À¾ã³²¼Ô¡Ë¡ÊÃ±°ì·úÊª£²¿Í°Ê¾å¡Ë
           03  CONS-SEIZAI4         PIC X(09) VALUE "180057510".
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£±¡Ê¥¤µÚ¤Ó¥í°Ê³°¡Ë¡ÊÃ±°ì·úÊª£±¿Í¡Ë
           03  CONS-SEIZAI5         PIC X(09) VALUE "180057610".
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£±¡Ê¥¤µÚ¤Ó¥í°Ê³°¡Ë¡ÊÃ±°ì·úÊª£²¿Í°Ê¾å¡Ë
           03  CONS-SEIZAI6         PIC X(09) VALUE "180057710".
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£²¡Ê½¸ÃæÅª»Ù±çÉ¬Í×¡Ë¡ÊÃ±°ì·úÊª£±¿Í¡Ë
           03  CONS-SEIZAI7         PIC X(09) VALUE "180057810".
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£²¡Ê½¸ÃæÅª»Ù±çÉ¬Í×¡Ë¡ÊÃ±°ì·úÊª£²¿Í°Ê¾å¡Ë
           03  CONS-SEIZAI8         PIC X(09) VALUE "180057910".
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£²¡Ê½ÅÅÙÀº¿À¾ã³²¼Ô¡Ë¡ÊÃ±°ì·úÊª£±¿Í¡Ë
           03  CONS-SEIZAI9         PIC X(09) VALUE "180058010".
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£²¡Ê½ÅÅÙÀº¿À¾ã³²¼Ô¡Ë¡ÊÃ±°ì·úÊª£²¿Í°Ê¾å¡Ë
           03  CONS-SEIZAI10        PIC X(09) VALUE "180058110".
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£³¡ÊÃ±°ì·úÊª£±¿Í¡Ë
           03  CONS-SEIZAI11        PIC X(09) VALUE "180064010".
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£³¡ÊÃ±°ì·úÊª£²¿Í°Ê¾å¡Ë
           03  CONS-SEIZAI12        PIC X(09) VALUE "180064110".
      *
      *    ºßÂðÈ¾¸Ç·Á±ÉÍÜ·Ð´É±ÉÍÜË¡»ØÆ³´ÉÍýÎÁ
           03  CONS-ZAIHANKO        PIC X(09) VALUE "114045510".
      *
      *    ÆýÁ£±ê½Å¾É²½Í½ËÉ¥±¥¢¡¦»ØÆ³ÎÁ¡Ê½é²ó¡Ë
           03  CONS-NYUSEN          PIC X(09) VALUE "113024410".
      *    ÆýÁ£±ê½Å¾É²½Í½ËÉ¥±¥¢¡¦»ØÆ³ÎÁ¡Ê£²²óÌÜ¤«¤é£´²óÌÜ¤Þ¤Ç¡Ë
           03  CONS-NYUSEN2         PIC X(09) VALUE "113024510".
      *
      *    ºßÂð»ÀÁÇÎÅË¡»ØÆ³´ÉÍýÎÁ¡Ê¤½¤ÎÂ¾¡Ë
           03  CONS-ZAISAN          PIC X(09) VALUE "114003710".
      *    ºßÂð»ýÂ³ÍÛ°µ¸ÆµÛÎÅË¡»ØÆ³´ÉÍýÎÁ£²
           03  CONS-YOUATU2         PIC X(09) VALUE "114040810".
      *
      *    ¹âµ¤°µ»ÀÁÇ¼£ÎÅ¡Ê¸º°µ¾ÉËô¤Ï¶õµ¤ºÉÀò¡Ë
           03  CONS-KIATU           PIC X(09) VALUE "140057510".
      *    ¹âµ¤°µ»ÀÁÇ¼£ÎÅ¡Ê¤½¤ÎÂ¾¤Î¤â¤Î¡Ë
           03  CONS-KIATU2          PIC X(09) VALUE "140057610".
      *    ¹âµ¤°µ»ÀÁÇ¼£ÎÅ³«»ÏÆü
           03  CONS-KIATU3          PIC X(09) VALUE "099400002".
      *
      *    ¿Í¹©Æâ¼ªµ¡´ïÄ´À°²Ã»»¡Ê£¶ºÐÌ¤Ëþ¡Ë
           03  CONS-JINNAI1         PIC X(09) VALUE "113030670".
      *    ¿Í¹©Æâ¼ªµ¡´ïÄ´À°²Ã»»¡Ê£¶ºÐ°Ê¾å¡Ë
           03  CONS-JINNAI2         PIC X(09) VALUE "113030770".
      *
      *    ºßÂð´µ¼Ô¶¦Æ±¿ÇÎÅÎÁ¥Æ¡¼¥Ö¥ë
       01  TBL-ZAIKYO-CHECK-AREA.
           03  FILLER          PIC X(09)   VALUE    "114027610".
           03  FILLER          PIC X(09)   VALUE    "114027710".
           03  FILLER          PIC X(09)   VALUE    "114027810".
           03  FILLER          PIC X(09)   VALUE    "114027910".
      *
       01  TBL-ZAIKYO-CHECK-R  REDEFINES   TBL-ZAIKYO-CHECK-AREA.
           03  TBL-ZAIKYO-CHECK     PIC X(09)   OCCURS    4.
      *
       01  TBL-ZAIKYO-CHECK-MAX     PIC 9(02)   VALUE     4.
      *
      *    Ê£¿ô¿ÇÎÅ²Ê¼õ¿Ç¤ÎÂàÈò¥Æ¡¼¥Ö¥ë
       01  TBL-JUSHIN-INF.
           03  TBL-JUSHIN-INF2      OCCURS   100.
               05  TBL-JUSHIN-SRYKA       PIC X(02).
               05  TBL-JUSHIN-SRYKANAME   PIC X(80).
               05  TBL-JUSHIN-ARI         PIC X(01).
               05  TBL-JUSHIN-DAY         PIC X(01)   OCCURS  31.
      *
       01  TBL-COUNT-INF.
           03  TBL-COUNT-INF2       OCCURS     5.
               05  CNT-DENWA              PIC 9(06).
               05  CNT-DOUJITU            PIC 9(06).
               05  CNT-DOUDEN             PIC 9(06).
               05  CNT-DENWA2             PIC 9(06).
               05  CNT-DOUDEN2            PIC 9(06).
               05  TBL-COUNT-HKNCOMBI     PIC 9(04).
      *
       01  TBL-HASSYO-INF.
           03  TBL-HASSYO-KAITATU-FLG           PIC 9(01).
           03  TBL-HASSYO-SINDAI-FLG            PIC 9(01).
           03  TBL-HASSYO-KOKYUKI-FLG           PIC 9(01).
           03  TBL-HASSYO-NOUKE-FLG             PIC 9(01).
           03  TBL-HASSYO-UNDOKI-FLG            PIC 9(01).
           03  TBL-HASSYO-NANBYO-FLG            PIC 9(01).
           03  TBL-HASSYO-SYOGAI-FLG            PIC 9(01).
           03  TBL-HASSYO-SESSYOKU-FLG          PIC 9(01).
           03  TBL-HASSYO-HAIYO-FLG             PIC 9(01).
           03  TBL-HASSYO-INF2                  OCCURS  5.
               05  TBL-HASSYO-KAITATU-COMMENT   PIC 9(01).
               05  TBL-HASSYO-SINDAI-COMMENT    PIC 9(01).
               05  TBL-HASSYO-KOKYUKI-COMMENT   PIC 9(01).
               05  TBL-HASSYO-NOUKE-COMMENT     PIC 9(01).
               05  TBL-HASSYO-UNDOKI-COMMENT    PIC 9(01).
               05  TBL-HASSYO-NANBYO-COMMENT    PIC 9(01).
               05  TBL-HASSYO-SYOGAI-COMMENT    PIC 9(01).
               05  TBL-HASSYO-SESSYOKU-COMMENT  PIC 9(01).
               05  TBL-HASSYO-HAIYO-COMMENT     PIC 9(01).
               05  TBL-HASSYO-HKNCOMBI          PIC 9(04).
           03  TBL-HASSYO-INF3                  OCCURS 10.
               05  TBL-HASSYO-SINDAI2-COMMENT   PIC 9(01).
               05  TBL-HASSYO-KOKYUKI2-COMMENT  PIC 9(01).
               05  TBL-HASSYO-NOUKE2-COMMENT    PIC 9(01).
               05  TBL-HASSYO-UNDOKI2-COMMENT   PIC 9(01).
               05  TBL-HASSYO-NANBYO2-COMMENT   PIC 9(01).
               05  TBL-HASSYO-SYOGAI2-COMMENT   PIC 9(01).
               05  TBL-HASSYO-SESSYOKU2-COMMENT PIC 9(01).
               05  TBL-HASSYO-HAIYO2-COMMENT    PIC 9(01).
               05  TBL-HASSYO-HKNCOMBI2         PIC 9(04).
      *
           03  TBL-HASSYO-SINDAI-DAY-T.
             04  TBL-HASSYO-SINDAI-DAY-T2         OCCURS   5.
               05  TBL-HASSYO-SINDAI-DAY-OCC      OCCURS  31.
                   07  TBL-HASSYO-SINDAI-SANTEI   PIC 9(01).
           03  TBL-HASSYO-KOKYUKI-DAY-T.
             04  TBL-HASSYO-KOKYUKI-DAY-T2        OCCURS   5.
               05  TBL-HASSYO-KOKYUKI-DAY-OCC     OCCURS  31.
                   07  TBL-HASSYO-KOKYUKI-SANTEI  PIC 9(01).
           03  TBL-HASSYO-NOUKE-DAY-T.
             04  TBL-HASSYO-NOUKE-DAY-T2          OCCURS   5.
               05  TBL-HASSYO-NOUKE-DAY-OCC       OCCURS  31.
                   07  TBL-HASSYO-NOUKE-SANTEI    PIC 9(01).
           03  TBL-HASSYO-UNDOKI-DAY-T.
             04  TBL-HASSYO-UNDOKI-DAY-T2         OCCURS   5.
               05  TBL-HASSYO-UNDOKI-DAY-OCC      OCCURS  31.
                   07  TBL-HASSYO-UNDOKI-SANTEI   PIC 9(01).
           03  TBL-HASSYO-NANBYO-DAY-T.
             04  TBL-HASSYO-NANBYO-DAY-T2         OCCURS   5.
               05  TBL-HASSYO-NANBYO-DAY-OCC      OCCURS  31.
                   07  TBL-HASSYO-NANBYO-SANTEI   PIC 9(01).
           03  TBL-HASSYO-SYOGAI-DAY-T.
             04  TBL-HASSYO-SYOGAI-DAY-T2         OCCURS   5.
               05  TBL-HASSYO-SYOGAI-DAY-OCC      OCCURS  31.
                   07  TBL-HASSYO-SYOGAI-SANTEI   PIC 9(01).
           03  TBL-HASSYO-SESSYOKU-DAY-T.
             04  TBL-HASSYO-SESSYOKU-DAY-T2       OCCURS   5.
               05  TBL-HASSYO-SESSYOKU-DAY-OCC    OCCURS  31.
                   07  TBL-HASSYO-SESSYOKU-SANTEI PIC 9(01).
           03  TBL-HASSYO-HAIYO-DAY-T.
             04  TBL-HASSYO-HAIYO-DAY-T2          OCCURS   5.
               05  TBL-HASSYO-HAIYO-DAY-OCC       OCCURS  31.
                   07  TBL-HASSYO-HAIYO-SANTEI    PIC 9(01).
           03  TBL-HASSYO-TAIHI-DAY-T.
               05  TBL-HASSYO-TAIHI-DAY-OCC       OCCURS  31.
                   07  TBL-HASSYO-TAIHI-SANTEI    PIC 9(01).
      *****
           03  TBL-HASSYO-SINYMD1-DAY-T.
             04  TBL-HASSYO-SINYMD1-DAY-T2        OCCURS   5.
               05  TBL-HASSYO-SINYMD1-FLG         PIC 9(01).
               05  TBL-HASSYO-SINYMD1-DAY-OCC1.
                   07  TBL-HASSYO-SINYMD11        PIC X(08) OCCURS  2.
               05  TBL-HASSYO-SINYMD1-DAY-OCC2.
                   07  TBL-HASSYO-SINYMD12        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-KOKYMD1-DAY-T.
             04  TBL-HASSYO-KOKYMD1-DAY-T2        OCCURS   5.
               05  TBL-HASSYO-KOKYMD1-FLG         PIC 9(01).
               05  TBL-HASSYO-KOKYMD1-DAY-OCC1.
                   07  TBL-HASSYO-KOKYMD11        PIC X(08) OCCURS  2.
               05  TBL-HASSYO-KOKYMD1-DAY-OCC2.
                   07  TBL-HASSYO-KOKYMD12        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-NOUYMD1-DAY-T.
             04  TBL-HASSYO-NOUYMD1-DAY-T2        OCCURS   5.
               05  TBL-HASSYO-NOUYMD1-FLG         PIC 9(01).
               05  TBL-HASSYO-NOUYMD1-DAY-OCC1.
                   07  TBL-HASSYO-NOUYMD11        PIC X(08) OCCURS  2.
               05  TBL-HASSYO-NOUYMD1-DAY-OCC2.
                   07  TBL-HASSYO-NOUYMD12        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-UNDYMD1-DAY-T.
             04  TBL-HASSYO-UNDYMD1-DAY-T2        OCCURS   5.
               05  TBL-HASSYO-UNDYMD1-FLG         PIC 9(01).
               05  TBL-HASSYO-UNDYMD1-DAY-OCC1.
                   07  TBL-HASSYO-UNDYMD11        PIC X(08) OCCURS  2.
               05  TBL-HASSYO-UNDYMD1-DAY-OCC2.
                   07  TBL-HASSYO-UNDYMD12        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-NANYMD1-DAY-T.
             04  TBL-HASSYO-NANYMD1-DAY-T2        OCCURS   5.
               05  TBL-HASSYO-NANYMD1-FLG         PIC 9(01).
               05  TBL-HASSYO-NANYMD1-DAY-OCC1.
                   07  TBL-HASSYO-NANYMD11        PIC X(08) OCCURS  2.
               05  TBL-HASSYO-NANYMD1-DAY-OCC2.
                   07  TBL-HASSYO-NANYMD12        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-SYOYMD1-DAY-T.
             04  TBL-HASSYO-SYOYMD1-DAY-T2        OCCURS   5.
               05  TBL-HASSYO-SYOYMD1-FLG         PIC 9(01).
               05  TBL-HASSYO-SYOYMD1-DAY-OCC1.
                   07  TBL-HASSYO-SYOYMD11        PIC X(08) OCCURS  2.
               05  TBL-HASSYO-SYOYMD1-DAY-OCC2.
                   07  TBL-HASSYO-SYOYMD12        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-SESYMD1-DAY-T.
             04  TBL-HASSYO-SESYMD1-DAY-T2        OCCURS   5.
               05  TBL-HASSYO-SESYMD1-FLG         PIC 9(01).
               05  TBL-HASSYO-SESYMD1-DAY-OCC1.
                   07  TBL-HASSYO-SESYMD11        PIC X(08) OCCURS  2.
               05  TBL-HASSYO-SESYMD1-DAY-OCC2.
                   07  TBL-HASSYO-SESYMD12        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-HAIYMD1-DAY-T.
             04  TBL-HASSYO-HAIYMD1-DAY-T2        OCCURS   5.
               05  TBL-HASSYO-HAIYMD1-FLG         PIC 9(01).
               05  TBL-HASSYO-HAIYMD1-DAY-OCC1.
                   07  TBL-HASSYO-HAIYMD11        PIC X(08) OCCURS  2.
               05  TBL-HASSYO-HAIYMD1-DAY-OCC2.
                   07  TBL-HASSYO-HAIYMD12        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-TAIHIYMD1-DAY-T.
               05  TBL-HASSYO-TAIHIYMD1-FLG       PIC 9(01).
               05  TBL-HASSYO-TAIHIYMD1-DAY-OCC1.
                   07  TBL-HASSYO-TAIHIYMD11      PIC X(08) OCCURS  2.
               05  TBL-HASSYO-TAIHIYMD1-DAY-OCC2.
                   07  TBL-HASSYO-TAIHIYMD12      PIC 9(01) OCCURS 31.
      *
           03  TBL-HASSYO-SINDAI2-DAY-T.
             04  TBL-HASSYO-SINDAI2-DAY-OCC         OCCURS  10.
               05  TBL-HASSYO-SINDAI2-DAY-OCC2      OCCURS  31.
                   07  TBL-HASSYO-SINDAI2-SANTEI    PIC 9(01).
           03  TBL-HASSYO-KOKYUKI2-DAY-T.
             04  TBL-HASSYO-KOKYUKI2-DAY-OCC        OCCURS  10.
               05  TBL-HASSYO-KOKYUKI2-DAY-OCC2     OCCURS  31.
                   07  TBL-HASSYO-KOKYUKI2-SANTEI   PIC 9(01).
           03  TBL-HASSYO-NOUKE2-DAY-T.
             04  TBL-HASSYO-NOUKE2-DAY-OCC          OCCURS  10.
               05  TBL-HASSYO-NOUKE2-DAY-OCC2       OCCURS  31.
                   07  TBL-HASSYO-NOUKE2-SANTEI     PIC 9(01).
           03  TBL-HASSYO-UNDOKI2-DAY-T.
             04  TBL-HASSYO-UNDOKI2-DAY-OCC         OCCURS  10.
               05  TBL-HASSYO-UNDOKI2-DAY-OCC2      OCCURS  31.
                   07  TBL-HASSYO-UNDOKI2-SANTEI    PIC 9(01).
           03  TBL-HASSYO-NANBYO2-DAY-T.
             04  TBL-HASSYO-NANBYO2-DAY-OCC         OCCURS  10. 
               05  TBL-HASSYO-NANBYO2-DAY-OCC2      OCCURS  31.
                   07  TBL-HASSYO-NANBYO2-SANTEI    PIC 9(01).
           03  TBL-HASSYO-SYOGAI2-DAY-T.
             04  TBL-HASSYO-SYOGAI2-DAY-OCC         OCCURS  10.
               05  TBL-HASSYO-SYOGAI2-DAY-OCC2      OCCURS  31.
                   07  TBL-HASSYO-SYOGAI2-SANTEI    PIC 9(01).
           03  TBL-HASSYO-SESSYOKU2-DAY-T.
             04  TBL-HASSYO-SESSYOKU2-DAY-OCC       OCCURS  10.
               05  TBL-HASSYO-SESSYOKU2-DAY-OCC2    OCCURS  31.
                   07  TBL-HASSYO-SESSYOKU2-SANTEI  PIC 9(01).
           03  TBL-HASSYO-HAIYO2-DAY-T.
             04  TBL-HASSYO-HAIYO2-DAY-OCC          OCCURS  10.
               05  TBL-HASSYO-HAIYO2-DAY-OCC2       OCCURS  31.
                   07  TBL-HASSYO-HAIYO2-SANTEI     PIC 9(01).
      *****
           03  TBL-HASSYO-SINYMD2-DAY-T.
             04  TBL-HASSYO-SINYMD2-DAY-T2          OCCURS  10.
               05  TBL-HASSYO-SINYMD2-FLG           PIC 9(01).
               05  TBL-HASSYO-SINYMD2-DAY-OCC1.
                   07  TBL-HASSYO-SINYMD21          PIC X(08) OCCURS  2.
               05  TBL-HASSYO-SINYMD2-DAY-OCC2.
                   07  TBL-HASSYO-SINYMD22          PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-KOKYMD2-DAY-T.
             04  TBL-HASSYO-KOKYMD2-DAY-T2          OCCURS  10.
               05  TBL-HASSYO-KOKYMD2-FLG           PIC 9(01).
               05  TBL-HASSYO-KOKYMD2-DAY-OCC1.
                   07  TBL-HASSYO-KOKYMD21          PIC X(08) OCCURS  2.
               05  TBL-HASSYO-KOKYMD2-DAY-OCC2.
                   07  TBL-HASSYO-KOKYMD22          PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-NOUYMD2-DAY-T.
             04  TBL-HASSYO-NOUYMD2-DAY-T2          OCCURS  10.
               05  TBL-HASSYO-NOUYMD2-FLG           PIC 9(01).
               05  TBL-HASSYO-NOUYMD2-DAY-OCC1.
                   07  TBL-HASSYO-NOUYMD21          PIC X(08) OCCURS  2.
               05  TBL-HASSYO-NOUYMD2-DAY-OCC2.
                   07  TBL-HASSYO-NOUYMD22          PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-UNDYMD2-DAY-T.
             04  TBL-HASSYO-UNDYMD2-DAY-T2          OCCURS  10.
               05  TBL-HASSYO-UNDYMD2-FLG           PIC 9(01).
               05  TBL-HASSYO-UNDYMD2-DAY-OCC1.
                   07  TBL-HASSYO-UNDYMD21          PIC X(08) OCCURS  2.
               05  TBL-HASSYO-UNDYMD2-DAY-OCC2.
                   07  TBL-HASSYO-UNDYMD22          PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-NANYMD2-DAY-T.
             04  TBL-HASSYO-NANYMD2-DAY-T2          OCCURS  10.
               05  TBL-HASSYO-NANYMD2-FLG           PIC 9(01).
               05  TBL-HASSYO-NANYMD2-DAY-OCC1.
                   07  TBL-HASSYO-NANYMD21          PIC X(08) OCCURS  2.
               05  TBL-HASSYO-NANYMD2-DAY-OCC2.
                   07  TBL-HASSYO-NANYMD22          PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-SYOYMD2-DAY-T.
             04  TBL-HASSYO-SYOYMD2-DAY-T2          OCCURS  10.
               05  TBL-HASSYO-SYOYMD2-FLG           PIC 9(01).
               05  TBL-HASSYO-SYOYMD2-DAY-OCC1.
                   07  TBL-HASSYO-SYOYMD21          PIC X(08) OCCURS  2.
               05  TBL-HASSYO-SYOYMD2-DAY-OCC2.
                   07  TBL-HASSYO-SYOYMD22          PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-SESYMD2-DAY-T.
             04  TBL-HASSYO-SESYMD2-DAY-T2          OCCURS  10.
               05  TBL-HASSYO-SESYMD2-FLG           PIC 9(01).
               05  TBL-HASSYO-SESYMD2-DAY-OCC1.
                   07  TBL-HASSYO-SESYMD21          PIC X(08) OCCURS  2.
               05  TBL-HASSYO-SESYMD2-DAY-OCC2.
                   07  TBL-HASSYO-SESYMD22          PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-HAIYMD2-DAY-T.
             04  TBL-HASSYO-HAIYMD2-DAY-T2          OCCURS  10.
               05  TBL-HASSYO-HAIYMD2-FLG           PIC 9(01).
               05  TBL-HASSYO-HAIYMD2-DAY-OCC1.
                   07  TBL-HASSYO-HAIYMD21          PIC X(08) OCCURS  2.
               05  TBL-HASSYO-HAIYMD2-DAY-OCC2.
                   07  TBL-HASSYO-HAIYMD22          PIC 9(01) OCCURS 31.
      *
           03  TBL-HASSYO-KYUSEIYMD-T.
             04  TBL-HASSYO-KYUSEIYMD-OCC           OCCURS  50.
               05  TBL-HASSYO-SINDAI-KYUSEIYMD      PIC X(08).
               05  TBL-HASSYO-KOKYUKI-KYUSEIYMD     PIC X(08).
               05  TBL-HASSYO-NOUKE-KYUSEIYMD       PIC X(08).
               05  TBL-HASSYO-UNDOKI-KYUSEIYMD      PIC X(08).
               05  TBL-HASSYO-NANBYO-KYUSEIYMD      PIC X(08).
               05  TBL-HASSYO-SYOGAI-KYUSEIYMD      PIC X(08).
               05  TBL-HASSYO-SESSYOKU-KYUSEIYMD    PIC X(08).
               05  TBL-HASSYO-HAIYO-KYUSEIYMD       PIC X(08).
      *
      *****¡Ê¥ê¥Ï²Ã»»ÍÑ¡Ë
       01  TBL-HASSYO9-INF.
           03  TBL-HASSYO9-SINDAI-FLG            PIC 9(01).
           03  TBL-HASSYO9-KOKYUKI-FLG           PIC 9(01).
           03  TBL-HASSYO9-HAIYO-FLG             PIC 9(01).
           03  TBL-HASSYO9-INF2                  OCCURS  5.
               05  TBL-HASSYO9-SINDAI-COMMENT    PIC 9(01).
               05  TBL-HASSYO9-KOKYUKI-COMMENT   PIC 9(01).
               05  TBL-HASSYO9-HAIYO-COMMENT     PIC 9(01).
               05  TBL-HASSYO9-HKNCOMBI          PIC 9(04).
           03  TBL-HASSYO9-INF3                  OCCURS 10.
               05  TBL-HASSYO9-SINDAI2-COMMENT   PIC 9(01).
               05  TBL-HASSYO9-KOKYUKI2-COMMENT  PIC 9(01).
               05  TBL-HASSYO9-HAIYO2-COMMENT    PIC 9(01).
               05  TBL-HASSYO9-HKNCOMBI2         PIC 9(04).
      *
           03  TBL-HASSYO9-SINDAI-DAY-T.
             04  TBL-HASSYO9-SINDAI-DAY-T2         OCCURS   5.
               05  TBL-HASSYO9-SINDAI-DAY-OCC      OCCURS  31.
                   07  TBL-HASSYO9-SINDAI-SANTEI   PIC 9(01).
           03  TBL-HASSYO9-KOKYUKI-DAY-T.
             04  TBL-HASSYO9-KOKYUKI-DAY-T2        OCCURS   5.
               05  TBL-HASSYO9-KOKYUKI-DAY-OCC     OCCURS  31.
                   07  TBL-HASSYO9-KOKYUKI-SANTEI  PIC 9(01).
           03  TBL-HASSYO9-HAIYO-DAY-T.
             04  TBL-HASSYO9-HAIYO-DAY-T2          OCCURS   5.
               05  TBL-HASSYO9-HAIYO-DAY-OCC       OCCURS  31.
                   07  TBL-HASSYO9-HAIYO-SANTEI    PIC 9(01).
      *****
           03  TBL-HASSYO9-SINYMD1-DAY-T.
             04  TBL-HASSYO9-SINYMD1-DAY-T2        OCCURS   5.
               05  TBL-HASSYO9-SINYMD1-FLG         PIC 9(01).
               05  TBL-HASSYO9-SINYMD1-DAY-OCC1.
                   07  TBL-HASSYO9-SINYMD11        PIC X(08) OCCURS  2.
               05  TBL-HASSYO9-SINYMD1-DAY-OCC2.
                   07  TBL-HASSYO9-SINYMD12        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO9-KOKYMD1-DAY-T.
             04  TBL-HASSYO9-KOKYMD1-DAY-T2        OCCURS   5.
               05  TBL-HASSYO9-KOKYMD1-FLG         PIC 9(01).
               05  TBL-HASSYO9-KOKYMD1-DAY-OCC1.
                   07  TBL-HASSYO9-KOKYMD11        PIC X(08) OCCURS  2.
               05  TBL-HASSYO9-KOKYMD1-DAY-OCC2.
                   07  TBL-HASSYO9-KOKYMD12        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO9-HAIYMD1-DAY-T.
             04  TBL-HASSYO9-HAIYMD1-DAY-T2        OCCURS   5.
               05  TBL-HASSYO9-HAIYMD1-FLG         PIC 9(01).
               05  TBL-HASSYO9-HAIYMD1-DAY-OCC1.
                   07  TBL-HASSYO9-HAIYMD11        PIC X(08) OCCURS  2.
               05  TBL-HASSYO9-HAIYMD1-DAY-OCC2.
                   07  TBL-HASSYO9-HAIYMD12        PIC 9(01) OCCURS 31.
      *
           03  TBL-HASSYO9-SINDAI2-DAY-T.
             04  TBL-HASSYO9-SINDAI2-DAY-OCC         OCCURS  10.
               05  TBL-HASSYO9-SINDAI2-DAY-OCC2      OCCURS  31.
                   07  TBL-HASSYO9-SINDAI2-SANTEI    PIC 9(01).
           03  TBL-HASSYO9-KOKYUKI2-DAY-T.
             04  TBL-HASSYO9-KOKYUKI2-DAY-OCC        OCCURS  10.
               05  TBL-HASSYO9-KOKYUKI2-DAY-OCC2     OCCURS  31.
                   07  TBL-HASSYO9-KOKYUKI2-SANTEI   PIC 9(01).
           03  TBL-HASSYO9-HAIYO2-DAY-T.
             04  TBL-HASSYO9-HAIYO2-DAY-OCC          OCCURS  10.
               05  TBL-HASSYO9-HAIYO2-DAY-OCC2       OCCURS  31.
                   07  TBL-HASSYO9-HAIYO2-SANTEI     PIC 9(01).
      *****
           03  TBL-HASSYO9-SINYMD2-DAY-T.
             04  TBL-HASSYO9-SINYMD2-DAY-T2        OCCURS  10.
               05  TBL-HASSYO9-SINYMD2-FLG         PIC 9(01).
               05  TBL-HASSYO9-SINYMD2-DAY-OCC1.
                   07  TBL-HASSYO9-SINYMD21        PIC X(08) OCCURS  2.
               05  TBL-HASSYO9-SINYMD2-DAY-OCC2.
                   07  TBL-HASSYO9-SINYMD22        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO9-KOKYMD2-DAY-T.
             04  TBL-HASSYO9-KOKYMD2-DAY-T2        OCCURS  10.
               05  TBL-HASSYO9-KOKYMD2-FLG         PIC 9(01).
               05  TBL-HASSYO9-KOKYMD2-DAY-OCC1.
                   07  TBL-HASSYO9-KOKYMD21        PIC X(08) OCCURS  2.
               05  TBL-HASSYO9-KOKYMD2-DAY-OCC2.
                   07  TBL-HASSYO9-KOKYMD22        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO9-HAIYMD2-DAY-T.
             04  TBL-HASSYO9-HAIYMD2-DAY-T2        OCCURS  10.
               05  TBL-HASSYO9-HAIYMD2-FLG         PIC 9(01).
               05  TBL-HASSYO9-HAIYMD2-DAY-OCC1.
                   07  TBL-HASSYO9-HAIYMD21        PIC X(08) OCCURS  2.
               05  TBL-HASSYO9-HAIYMD2-DAY-OCC2.
                   07  TBL-HASSYO9-HAIYMD22        PIC 9(01) OCCURS 31.
      *
           03  TBL-HASSYO9-KYUSEIYMD-T.
             04  TBL-HASSYO9-KYUSEIYMD-OCC           OCCURS  50.
               05  TBL-HASSYO9-SINDAI-KYUSEIYMD      PIC X(08).
               05  TBL-HASSYO9-KOKYUKI-KYUSEIYMD     PIC X(08).
               05  TBL-HASSYO9-HAIYO-KYUSEIYMD       PIC X(08).
      *****
      *
       01  TBL-ZAISIEN-INF.
           03  TBL-ZAISIEN-INF2                 OCCURS  5.
               05  TBL-ZAISIEN-COMMENT          PIC 9(01).
               05  TBL-ZAISIEN-HKNCOMBI         PIC 9(04).
      *
       01  TBL-HOMONCOM-NINZU-INF.
           03  TBL-HOMONCOM-SETFLG              PIC 9(01).
           03  TBL-HOMONCOM-NINZU               PIC 9(03)  OCCURS  31.
      *
       01  TBL-GENBAKU-INF.
           03  TBL-GENBAKU-INF2                 OCCURS  5.
               05  TBL-GENBAKU-COMMENT          PIC 9(01).
               05  TBL-GENBAKU-HKNCOMBI         PIC 9(04).
               05  TBL-GENBAKU-FTNRATE          PIC 9(03).
      *
      *    »ÀÁÇ¡Ê¶â³Û¡¦¿ôÎÌ¡Ë¾ðÊó
       01  TBL-SANSO-INF.
           03  TBL-SANSO-INF2       OCCURS    10.
               05  TBL-SANSO-TEN          PIC 9(09)V9(02).
               05  TBL-SANSO-SRYSURYO     PIC 9(05)V9(05).
           03  TBL-SANSO-INF3.
               05  TBL-KASAN-SRYSURYO     PIC 9(05)V9(05).
      *
      *    ÌôºÞÄþ¸º¾ðÊó
       01  TBL-YAKUTEI-INF.
           03  TBL-YT1-OCC                      OCCURS 100.
             05  TBL-YT1-KEY.
               07  TBL-YT1-KOHBUNRI             PIC 9(01).
               07  TBL-YT1-SRYKBN               PIC X(02).
               07  TBL-YT1-JIHOKBN              PIC X(01).
      ***      07  TBL-YT1-HKNCOMBI             PIC 9(04).
      ***      07  TBL-YT1-SRYKA                PIC X(02).
             05  TBL-YT1-RECE04-REC             PIC X(2000).
             05  TBL-YT1-HIKAKU.
               07  TBL-YT1-ZAITEN               PIC 9(08).
               07  TBL-YT1-SRYCDTOTAL           PIC 9(14).
               07  TBL-YT1-SURYOUTOTAL          PIC 9(07)V9(05).
               07  TBL-YT1-RUIKEITOTAL          PIC 9(13)V9(05).
               07  TBL-YT1-RUIKEITOTAL2         PIC 9(13)V9(05).
               07  TBL-YT1-RUIKEITOTAL3         PIC 9(13)V9(05).
               07  TBL-YT1-RUIKEITOTAL4         PIC 9(14).
               07  TBL-YT1-MEISAISU             PIC 9(04).
             05  TBL-YT1-RECE04-DAY-AREA.
               07  TBL-YT1-OCC2                 OCCURS  31.
                 09  TBL-YT1-OCC3               OCCURS   9.
                   11  TBL-YT1-KAISU            PIC 9(03).
             05  TBL-YT1-FLG                    PIC 9(01).
      *
      *    ÊÂ¤ÓÂØ¤¨ÍÑ
      *01  WRK-YAKUTEI-DATA                     PIC X(2934).
       01  WRK-YAKUTEI-DATA                     PIC X(2948).
      *
       01  WRK-TAIHI-YT1-KEY                    PIC X(04).
       01  WRK-YAKUTEI-ZAINUM                   PIC 9(08).
       01  WRK-YAKUTEI-ZAINUM2                  PIC 9(08).
      *
      *    ÌôºÞÄþ¸º¾ðÊó¡Ê¥­¡¼Ã±°Ì¤ÎÊÔ½¸¥Æ¡¼¥Ö¥ë¡Ë
       01  TBL-YAKUTEI-INF2.
           03  TBL-YT2-DAY                    OCCURS      31.
             05  TBL-YT2X-KAIT                  OCCURS     9.
               06  TBL-YT2X1-OCC                  OCCURS  25.
                 07  TBL-YT2X1-RECE04-REC         PIC X(2000).
                 07  TBL-YT2X1-KAISU              PIC 9(03).
                 07  TBL-YT2X1-DAY-AREA.
                   09  TBL-YT2X1-OCC2               OCCURS  31.
                       11  TBL-YT2X1-DAY            PIC 9(03).
               06  TBL-YT2X2-OCC                  OCCURS   3.
                 07  TBL-YT2X2-RECE04-REC         PIC X(2000).
                 07  TBL-YT2X2-KAISU              PIC 9(03).
                 07  TBL-YT2X2-DAY-AREA.
                   09  TBL-YT2X2-OCC2               OCCURS  31.
                       11  TBL-YT2X2-DAY            PIC 9(03).
             05  TBL-YT2Y-KAIT                  OCCURS     9.
               06  TBL-YT2Y1-OCC                  OCCURS  26.
                 07  TBL-YT2Y1-HIKAKU.
                   09  TBL-YT2Y1-ZAITEN           PIC 9(08).
                   09  TBL-YT2Y1-SRYCDTOTAL       PIC 9(14).
                   09  TBL-YT2Y1-SURYOUTOTAL      PIC 9(07)V9(05).
                   09  TBL-YT2Y1-RUIKEITOTAL      PIC 9(13)V9(05).
                   09  TBL-YT2Y1-RUIKEITOTAL2     PIC 9(13)V9(05).
                   09  TBL-YT2Y1-RUIKEITOTAL3     PIC 9(13)V9(05).
                   09  TBL-YT2Y1-RUIKEITOTAL4     PIC 9(14).
                   09  TBL-YT2Y1-MEISAISU         PIC 9(04).
               06  TBL-YT2Y2-OCC                  OCCURS   4.
                 07  TBL-YT2Y2-HIKAKU.
                   09  TBL-YT2Y2-ZAITEN           PIC 9(08).
                   09  TBL-YT2Y2-SRYCDTOTAL       PIC 9(14).
                   09  TBL-YT2Y2-SURYOUTOTAL      PIC 9(07)V9(05).
                   09  TBL-YT2Y2-RUIKEITOTAL      PIC 9(13)V9(05).
                   09  TBL-YT2Y2-RUIKEITOTAL2     PIC 9(13)V9(05).
                   09  TBL-YT2Y2-RUIKEITOTAL3     PIC 9(13)V9(05).
                   09  TBL-YT2Y2-RUIKEITOTAL4     PIC 9(14).
                   09  TBL-YT2Y2-MEISAISU         PIC 9(04).
      *
      *    ÌôºÞÄþ¸º¾ðÊó¡Ê¤Þ¤È¤áÍÑ¤ÎÊÔ½¸¥Æ¡¼¥Ö¥ë¡Ë
       01  TBL-YAKUTEI-INF3.
           03  TBL-YT3-XXX                    OCCURS     100.
             05  TBL-YT3X-KAIT.
               06  TBL-YT3X1-OCC                  OCCURS  25.
                 07  TBL-YT3X1-RECE04-REC         PIC X(2000).
                 07  TBL-YT3X1-KAISU              PIC 9(03).
                 07  TBL-YT3X1-DAY-AREA.
                   09  TBL-YT3X1-OCC2               OCCURS  31.
                       11  TBL-YT3X1-DAY            PIC 9(03).
               06  TBL-YT3X2-OCC                  OCCURS   3.
                 07  TBL-YT3X2-RECE04-REC         PIC X(2000).
                 07  TBL-YT3X2-KAISU              PIC 9(03).
                 07  TBL-YT3X2-DAY-AREA.
                   09  TBL-YT3X2-OCC2               OCCURS  31.
                       11  TBL-YT3X2-DAY            PIC 9(03).
             05  TBL-YT3Y-KAIT.
               06  TBL-YT3Y1-OCC                  OCCURS  26.
                 07  TBL-YT3Y1-HIKAKU.
                   09  TBL-YT3Y1-ZAITEN           PIC 9(08).
                   09  TBL-YT3Y1-SRYCDTOTAL       PIC 9(14).
                   09  TBL-YT3Y1-SURYOUTOTAL      PIC 9(07)V9(05).
                   09  TBL-YT3Y1-RUIKEITOTAL      PIC 9(13)V9(05).
                   09  TBL-YT3Y1-RUIKEITOTAL2     PIC 9(13)V9(05).
                   09  TBL-YT3Y1-RUIKEITOTAL3     PIC 9(13)V9(05).
                   09  TBL-YT3Y1-RUIKEITOTAL4     PIC 9(14).
                   09  TBL-YT3Y1-MEISAISU         PIC 9(04).
               06  TBL-YT3Y2-OCC                  OCCURS   4.
                 07  TBL-YT3Y2-HIKAKU.
                   09  TBL-YT3Y2-ZAITEN           PIC 9(08).
                   09  TBL-YT3Y2-SRYCDTOTAL       PIC 9(14).
                   09  TBL-YT3Y2-SURYOUTOTAL      PIC 9(07)V9(05).
                   09  TBL-YT3Y2-RUIKEITOTAL      PIC 9(13)V9(05).
                   09  TBL-YT3Y2-RUIKEITOTAL2     PIC 9(13)V9(05).
                   09  TBL-YT3Y2-RUIKEITOTAL3     PIC 9(13)V9(05).
                   09  TBL-YT3Y2-RUIKEITOTAL4     PIC 9(14).
                   09  TBL-YT3Y2-MEISAISU         PIC 9(04).
      *
      *    ¥Ñ¥¹Ì¾ÎÎ°è
       01  WRK-PATH                        PIC X(64).
      *
      *    »ÀÁÇ 
           COPY    "CMSANSOTBL.INC".
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥¸¥ç¥Ö´ÉÍý¥Þ¥¹¥¿
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *
      *    ¿ÇÎÅ²ÊÌÜ¾ðÊó
           COPY    "CPSK1005.INC".
      *
      *    ¿ÇÎÅ¹Ô°Ù
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    »»ÄêÍúÎò
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    ¼õ¿ÇÍúÎò
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    ¿ÇÎÅ²ñ·×
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ´µ¼Ô¥³¥á¥ó¥È
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    ÅÀ¿ô
           COPY    "CPTENSU.INC".
      *
      *    »»ÄêÍúÎòÉÕ²Ã
       01  SANTEIPLUS-REC.
           COPY    "CPSANTEIPLUS.INC".
      *
      *    ¥ì¥»¥×¥Èµ­ºÜ
       01  RKISAI-REC.
           COPY    "CPRECEKISAI.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑÎÎ°è
      *****************************************************************
      *   ¥¸¥ç¥Ö´ÉÍý£Ä£ÂÀ©¸æ¥µ¥Ö
           COPY    "CPORCSJOBKANRI.INC".
      *     
      *    ¿ôÃÍÊÑ´¹¥µ¥Ö
           COPY    "CPORCSNUM.INC".
      *
      *    ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    Á´³Ñ¥Á¥§¥Ã¥¯¥Ñ¥é¥á¥¿
           COPY    "CPORCSKANACHK.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ¥ª¥ó¥é¥¤¥ó¿ÇÎÅÎÁÅù¤Ë´Ø¤ï¤ë»»ÄêÍúÎò¥Á¥§¥Ã¥¯¥µ¥Ö
           COPY    "CPORCSRECESRCHK.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL  FLG-END  =   1
      *
           PERFORM 300-END-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
           .
      *****************************************************************
      *    ½é´ü¡¡½èÍý
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
                                       SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        £Ä£Â¥ª¡¼¥×¥ó½èÍý
               PERFORM 100-DBOPEN-SEC
      *
      *        ¥¹¥Æ¥Ã¥×´ÉÍý³«»Ï½èÍý
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0040"      TO  JOB-PGID
               MOVE    "¥³¥á¥ó¥ÈÊÔ½¸"    TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        ½èÍýÃæ»ßÀßÄê½èÍý
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
      *    ¥Õ¥¡¥¤¥ë£Ï£Ð£Å£Î½èÍý
           PERFORM 110-FILE-OPEN-SEC
      *
           PERFORM 990-RECE02-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë£Ï£Ð£Å£Î½èÍý
      *****************************************************************
       110-FILE-OPEN-SEC          SECTION.
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    "RECE020"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (2)
           MOVE    "RECE04W"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (3)
           MOVE    "RECE050"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (4)
           MOVE    "RECE041"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (5)
           MOVE    "RECE051"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (6)
           MOVE    "RECE04Y"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (7)
           MOVE    "RECE05Y"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (8)
           MOVE    "RECE055"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (9)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE01PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE04PARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE05PARA
           MOVE    SGETTEMP-FULLNAMES (5)
                                       TO  RECE041PARA
           MOVE    SGETTEMP-FULLNAMES (6)
                                       TO  RECE051PARA
           MOVE    SGETTEMP-FULLNAMES (7)
                                       TO  RECE049PARA
           MOVE    SGETTEMP-FULLNAMES (8)
                                       TO  RECE059PARA
           MOVE    SGETTEMP-FULLNAMES (9)
                                       TO  RECE055PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           OPEN    INPUT               RECE02-FILE
           IF      STS-RECE02          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE01PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE02          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE04-FILE
           IF      STS-RECE04          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE04PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE04          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE05-FILE
           IF      STS-RECE05          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE05PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE05          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    OUTPUT              RECE041-FILE
           CLOSE                       RECE041-FILE
      *
           OPEN    OUTPUT              RECE051-FILE
           CLOSE                       RECE051-FILE
      *
           OPEN    OUTPUT              RECE049-FILE
           CLOSE                       RECE049-FILE
      *
           OPEN    OUTPUT              RECE059-FILE
           CLOSE                       RECE059-FILE
      *
           OPEN    OUTPUT              RECE055-FILE
           CLOSE                       RECE055-FILE
      *
           OPEN    I-O                 RECE041-FILE
           OPEN    I-O                 RECE049-FILE
      *
           OPEN    I-O                 RECE051-FILE
           OPEN    I-O                 RECE059-FILE
      *
           OPEN    OUTPUT              RECE055-FILE
      *
           .
       110-FILE-OPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE       ZERO             TO  CNT-SANTEIKA
           INITIALIZE                      TBL-COUNT-INF
           INITIALIZE                      WRK-SAISHIN4-AREA
           INITIALIZE                      WRK-JINZOU-AREA
      *
           INITIALIZE                      TBL-HASSYO-INF
           INITIALIZE                      TBL-HASSYO9-INF
           INITIALIZE                      TBL-ZAISIEN-INF
           INITIALIZE                      TBL-GENBAKU-INF
           INITIALIZE                      TBL-HOMONCOM-NINZU-INF
      *
           INITIALIZE                      TBL-YAKUTEI-INF
      *
           INITIALIZE                      WRK-JIDO-SETCOM-AREA
      *
           MOVE       ZERO             TO  FLG-OSIN-HENSYU
           MOVE       ZERO             TO  FLG-TAKYO-HENSYU
      *
           MOVE       ZERO             TO  IDX6
      *
           MOVE       ZERO             TO  IDX33
           MOVE       ZERO             TO  IDX34
      *
           MOVE  RECE02-KOHBUNRI-MIN   TO  WRK-KOHBUNRI-JIDO
           MOVE  RECE02-KOHBUNRI-MAX   TO  WRK-KOHBUNRI-JIDO2
           MOVE  ZERO                  TO  WRK-HKNCOMBI-JIDO
           MOVE  ZERO                  TO  WRK-HKNCOMBI-JIDO2
      *
           MOVE  ZERO        TO    CNT-KENSA
           IF   RECE02-KENSA-BNPFLG     =   1
                 ADD    1    TO    CNT-KENSA
           END-IF
           IF   RECE02-KENSA-NTPROFLG   =   1
                 ADD    1    TO    CNT-KENSA
           END-IF
           IF   RECE02-KENSA-ANPFLG     =   1
                 ADD    1    TO    CNT-KENSA
           END-IF
      *
           IF   RECE02-COMCNTFLG   =   "1"
      *
             PERFORM 800-RECE02-SET04-SEC
             MOVE    ZERO                TO  RECE04-ZAINUM
      *
             START   RECE04-FILE
                 KEY IS    >=   RECE04-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE04
                 NOT INVALID
                     PERFORM 990-RECE04-NEXT-SEC
             END-START
             PERFORM
               UNTIL  ( FLG-RECE04           =   1           )
               OR     ( RECE02-KEY       NOT =   RECE04-KEY1 )
      *
               EVALUATE      RECE04-SRYKBN
                   WHEN      "11"
      *            ¡Ê½é¿ÇÎÁ»»Äê²Ê¡§¤Î¥«¥¦¥ó¥È¡Ë
                      IF  RECE04-SYOSANTEIKA-COMMENT  =  "1"
                             ADD   RECE04-ZAIKAISU   TO   CNT-SANTEIKA
                      END-IF
                   WHEN      "12"
      *            ¡ÊºÆ¿ÇÎÁ»»Äê²Ê¡§¤Î¥«¥¦¥ó¥È¡Ë
                      IF  RECE04-SAISANTEIKA-COMMENT  =  "1"
                             ADD   RECE04-ZAIKAISU   TO   CNT-SANTEIKA
                      END-IF
      *
      *            ¡ÊÅÅÏÃºÆ¿Ç¤Î¥«¥¦¥ó¥È¡Ë
                      IF  RECE04-DENWASAI-COMMENT     =  "1"
                        COMPUTE  WRK-KOHBUNRI 
                                   =    RECE04-KOHBUNRI  +  1
                        ADD   RECE04-ZAIKAISU  
                                   TO   CNT-DENWA(WRK-KOHBUNRI)
                        MOVE  RECE04-HKNCOMBI
                                   TO   TBL-COUNT-HKNCOMBI(WRK-KOHBUNRI)
                      END-IF
      *            ¡ÊÆ±ÆüºÆ¿Ç¤Î¥«¥¦¥ó¥È¡Ë
                      IF  RECE04-DOUJITUSAI-COMMENT   =  "1"
                        COMPUTE  WRK-KOHBUNRI 
                                   =    RECE04-KOHBUNRI  +  1
                        ADD   RECE04-ZAIKAISU  
                                   TO   CNT-DOUJITU(WRK-KOHBUNRI)
                        MOVE  RECE04-HKNCOMBI
                                   TO   TBL-COUNT-HKNCOMBI(WRK-KOHBUNRI)
                      END-IF
      *            ¡ÊÆ±ÆüÅÅÏÃºÆ¿Ç¤Î¥«¥¦¥ó¥È¡Ë
                      IF  RECE04-DOUDENSAI-COMMENT    =  "1"
                        COMPUTE  WRK-KOHBUNRI 
                                   =    RECE04-KOHBUNRI  +  1
                        ADD   RECE04-ZAIKAISU  
                                   TO   CNT-DOUDEN(WRK-KOHBUNRI)
                        MOVE  RECE04-HKNCOMBI
                                   TO   TBL-COUNT-HKNCOMBI(WRK-KOHBUNRI)
                      END-IF
      *            ¡ÊÅÅÏÃºÆ¿Ç¡Ê£³£°Ç¯£³·î°ÊÁ°·ÑÂ³¡Ë¤Î¥«¥¦¥ó¥È¡Ë
                      IF  RECE04-DENWASAI2-COMMENT    =  "1"
                        COMPUTE  WRK-KOHBUNRI 
                                   =    RECE04-KOHBUNRI  +  1
                        ADD   RECE04-ZAIKAISU  
                                   TO   CNT-DENWA2(WRK-KOHBUNRI)
                        MOVE  RECE04-HKNCOMBI
                                   TO   TBL-COUNT-HKNCOMBI(WRK-KOHBUNRI)
                      END-IF
      *            ¡ÊÆ±ÆüÅÅÏÃºÆ¿Ç¡Ê£³£°Ç¯£³·î°ÊÁ°·ÑÂ³¡Ë¤Î¥«¥¦¥ó¥È¡Ë
                      IF  RECE04-DOUDENSAI2-COMMENT   =  "1"
                        COMPUTE  WRK-KOHBUNRI 
                                   =    RECE04-KOHBUNRI  +  1
                        ADD   RECE04-ZAIKAISU  
                                   TO   CNT-DOUDEN2(WRK-KOHBUNRI)
                        MOVE  RECE04-HKNCOMBI
                                   TO   TBL-COUNT-HKNCOMBI(WRK-KOHBUNRI)
                      END-IF
      *
      *            ¡ÊºÆ¿Ç£´²óÌÜ¤Î¥«¥¦¥ó¥È¡Ë
                      IF  RECE04-SAIJINKO-COMMENT     =  "1"
                        COMPUTE  WRK-KOHBUNRI 
                                   =    RECE04-KOHBUNRI  +  1
                        PERFORM  VARYING   IDX1  FROM  1  BY  1
                                 UNTIL     IDX1  >   31
                            IF   RECE04-KAISU(1 IDX1)  NOT = ZERO
                                MOVE  "1"  TO  
                                      WRK-SAISHIN4(WRK-KOHBUNRI IDX1)
                            END-IF
                        END-PERFORM
                      END-IF
                   WHEN      "13"
      *            ¡ÊÂà±¡»þ¶¦Æ±»ØÆ³ÎÁÆÃÊÌ´ÉÍý»ØÆ³²Ã»»¤Ë´Ø¤¹¤ëÊÔ½¸¡Ë
                      IF  RECE04-FLG-TAKYO-COMMENT      =  "1"
                        MOVE  1  TO  FLG-TAKYO-HENSYU
                      END-IF
                   WHEN      "14"
      *            ¡Êºß»Ù±ç¤Ë´Ø¤¹¤ë¿ÇÎÅ¤Î¥«¥¦¥ó¥È¡Ë
                      IF  RECE04-FLG-ZAISIEN-COMMENT   =  "1" OR "2"
                        MOVE    RECE04-REC    TO   WRK-RECE04-REC
                        COMPUTE WRK-KOHBUNRI
                                   =    RECE04-KOHBUNRI  +  1
                        EVALUATE  RECE04-FLG-ZAISIEN-COMMENT
                         WHEN      "1"
                          MOVE 1 TO TBL-ZAISIEN-COMMENT(WRK-KOHBUNRI)
                         WHEN      "2"
                          MOVE 2 TO TBL-ZAISIEN-COMMENT(WRK-KOHBUNRI)
                        END-EVALUATE
                        MOVE  RECE04-HKNCOMBI  TO
                                     TBL-ZAISIEN-HKNCOMBI(WRK-KOHBUNRI)
                      END-IF
      *            ¡Ê±ý¿Ç¤Ë´Ø¤¹¤ëÊÔ½¸¡Ë
                      IF  RECE04-FLG-OSIN-COMMENT      =  "2"
                        MOVE  1  TO  FLG-OSIN-HENSYU
                      END-IF
                   WHEN      "40"
      *            ¡Ê¿Í¹©¿ÕÂ¡¤Î¥«¥¦¥ó¥È¡Ë
                      IF  RECE04-JINKOJIN-COMMENT     =  "1"
                        COMPUTE  WRK-KOHBUNRI 
                                   =    RECE04-KOHBUNRI  +  1
                        PERFORM  VARYING   IDX1  FROM  1  BY  1
                                 UNTIL     IDX1  >   31
                            IF   RECE04-KAISU(1 IDX1)  NOT = ZERO
                                 MOVE  "1"  TO  
                                       WRK-JINZOU(WRK-KOHBUNRI IDX1)
                            END-IF
                        END-PERFORM
                        MOVE   RECE04-HKNCOMBI  TO
                                       WRK-JINZOU-HKNCOMBI(WRK-KOHBUNRI)
                      END-IF
      *            ¡Ê²ðÃ£¸£°ú¤¢¤ê¡Ë
                      IF  RECE04-KAITATU-COMMENT    =  "1"
                        MOVE    1    TO    TBL-HASSYO-KAITATU-FLG
                        COMPUTE  WRK-KOHBUNRI
                                   =    RECE04-KOHBUNRI  +  1
                        MOVE    1    TO
                               TBL-HASSYO-KAITATU-COMMENT(WRK-KOHBUNRI)
                        MOVE   RECE04-HKNCOMBI  TO
                                      TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                      END-IF
                   WHEN      "80"
      *            ¡Ê¥ê¥Ï¥Ó¥ê¤¢¤ê¡Ë
                      IF  RECE04-SINDAI-COMMENT     =  "1"
                        MOVE    9    TO    TBL-HASSYO-SINDAI-FLG
                      END-IF
                      IF  RECE04-KOKYUKI-COMMENT    =  "1"
                        MOVE    9    TO    TBL-HASSYO-KOKYUKI-FLG
                      END-IF
                      IF  RECE04-NOUKE-COMMENT      =  "1"
                        MOVE    9    TO    TBL-HASSYO-NOUKE-FLG
                      END-IF
                      IF  RECE04-UNDOKI-COMMENT     =  "1"
                        MOVE    9    TO    TBL-HASSYO-UNDOKI-FLG
                      END-IF
                      IF  RECE04-NANBYO-COMMENT     =  "1"
                        MOVE    9    TO    TBL-HASSYO-NANBYO-FLG
                      END-IF
                      IF  RECE04-SYOGAI-COMMENT     =  "1"
                        MOVE    9    TO    TBL-HASSYO-SYOGAI-FLG
                      END-IF
                      IF  RECE04-SESSYOKU-COMMENT   =  "1"
                        MOVE    9    TO    TBL-HASSYO-SESSYOKU-FLG
                      END-IF
                      IF  RECE04-HAIYO-COMMENT      =  "1"
                        MOVE    9    TO    TBL-HASSYO-HAIYO-FLG
                      END-IF
      *
                      IF  RECE04-SINDAI9-COMMENT    =  "1"
                        MOVE    9    TO    TBL-HASSYO9-SINDAI-FLG
                      END-IF
                      IF  RECE04-KOKYUKI9-COMMENT   =  "1"
                        MOVE    9    TO    TBL-HASSYO9-KOKYUKI-FLG
                      END-IF
                      IF  RECE04-HAIYO9-COMMENT     =  "1"
                        MOVE    9    TO    TBL-HASSYO9-HAIYO-FLG
                      END-IF
                   WHEN      "99"
      *            ¡ÊË¬Ìä¿ÇÎÅ¤Ë¤«¤«¤ëµ­Ï¿½ñ¥³¥á¥ó¥È¤Ë´Ø¤¹¤ëÊÔ½¸¡Ë
                      IF  RECE04-HOMONCOMFLG           =  "2"
                        PERFORM  VARYING   IDX1  FROM  1  BY  1
                                  UNTIL    IDX1  >   31
                            IF   RECE04-KAISU(1 IDX1)  NOT = ZERO
                              MOVE  1  TO  TBL-HOMONCOM-SETFLG
                              ADD  RECE04-KAISU(1 IDX1)  TO
                                           TBL-HOMONCOM-NINZU(IDX1)
                            END-IF
                        END-PERFORM
                      END-IF
               END-EVALUATE
      *
               PERFORM 990-RECE04-NEXT-SEC
      *
             END-PERFORM
      *
           END-IF
      *
      *    ¥Ñ¥é¥á¡¼¥¿£°£´¥Õ¥¡¥¤¥ë¤è¤êÊÔ½¸
           PERFORM 800-RECE02-SET04-SEC
           MOVE    ZERO                TO  RECE04-ZAINUM
      *
           START   RECE04-FILE
               KEY IS    >=   RECE04-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE04
               NOT INVALID
                   PERFORM 990-RECE04-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE04           =   1                   )
               OR     ( RECE02-KEY       NOT =   RECE04-KEY1         )
      *
               INITIALIZE    WRK-JIDO-SETCOM-AREA
      *
               IF   WRK-HKNCOMBI-JIDO   =   ZERO
                 IF    WRK-KOHBUNRI-JIDO   =   RECE04-KOHBUNRI
                       MOVE   RECE04-HKNCOMBI   TO   WRK-HKNCOMBI-JIDO
                 END-IF
               END-IF
      *
               IF   WRK-HKNCOMBI-JIDO2  =   ZERO
                 IF    WRK-KOHBUNRI-JIDO2  =   RECE04-KOHBUNRI
                       MOVE   RECE04-HKNCOMBI   TO   WRK-HKNCOMBI-JIDO2
                 END-IF
               END-IF
      *
               IF   RECE04-JIHOKBN  NOT =  SPACE
                 INITIALIZE  WRK-JIDO-SETCOM-AREA
                 PERFORM     20011-HOKATU-COMMENT-SEC
               ELSE
                 EVALUATE      RECE04-SRYKBN
                   WHEN      "11"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYODOU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE04-TENSOU-SEC
                      END-IF
                   WHEN      "12"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SAIDOU-HANTEI-SEC
                      END-IF
      *               ¿Ç¶è£±£²¡¦£±£³¡¦£±£´¡¦£¸£°¶¦ÍÑÊ¬
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ONLINE-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE04-TENSOU-SEC
                      END-IF
                   WHEN      "13"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                         PERFORM   2001-SHIDOU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYOBYO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-RYOYO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NIKOCHINTOU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYOUNI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SINPESU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYUEIYO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NINSEN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-TAKYOKSN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-GAIRIHA-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-JINNAI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-GAIJIR-HANTEI-SEC
                      END-IF
      *               ¿Ç¶è£±£³¡¦£±£´¶¦ÍÑÊ¬
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SINPESUR-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-GANSIDO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NYUSEN-HANTEI-SEC
                      END-IF
      *               ¿Ç¶è£±£²¡¦£±£³¡¦£±£´¡¦£¸£°¶¦ÍÑÊ¬
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ONLINE-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE04-TENSOU-SEC
                      END-IF
                   WHEN      "14"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HOUSIJI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HINKAI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-OSIN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ZAIHOU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KAISYOKU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-YOUATU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ZAIKYO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ZAIRIHA-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ZAIYAK-HANTEI-SEC
                      END-IF
      *               ¿Ç¶è£±£³¡¦£±£´¶¦ÍÑÊ¬
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SINPESUR-HANTEI-SEC
                      END-IF
      *               ¿Ç¶è£±£´¡¦£¸£°¤È¶¦ÍÑÊ¬
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NINRYO-HANTEI-SEC
                      END-IF
      *               ¿Ç¶è£±£²¡¦£±£³¡¦£±£´¡¦£¸£°¶¦ÍÑÊ¬
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ONLINE-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE04-TENSOU-SEC
                      END-IF
                   WHEN      "21"
                   WHEN      "22"
                   WHEN      "23"
                      IF   RECE04-COMCDFLG   =   "2"
                        PERFORM    2001-YAKUTEI-HENSYU-SEC
                      ELSE
                        IF  RECE04-SIPPUFLG  =   "1"
                           MOVE    ZERO   TO   FLG-SIPPU-OK
                           PERFORM 2001-INGAISIPPU-CHECK-SEC
                           IF  FLG-SIPPU-OK  =  1
                             PERFORM 2001-INGAISIPPU-COMMENT-SEC
                           END-IF
                        END-IF
                        PERFORM    2099-RECE04-TENSOU-SEC
                      END-IF
                   WHEN      "33"
                      PERFORM      2001-HOUTEN-HANTEI-SEC
                   WHEN      "40"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-JINZOUETC-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KYOKUIN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-BOKOHO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KIATU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE04-TENSOU-SEC
                      END-IF
                   WHEN      "50"
                      PERFORM      2001-SHUJUTSU-HANTEI-SEC
                   WHEN      "54"
                      PERFORM      2001-MASUI-HANTEI-SEC
                   WHEN      "60"
                   WHEN      "64"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KENSEN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KENSA-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE04-TENSOU-SEC
                      END-IF
                   WHEN      "70"
                      PERFORM      2001-CTMRI-HANTEI-SEC
                   WHEN      "80"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        IF    RECE04-RIHABIRI-COMMENT  NOT = SPACE
                          IF  RECE04-RIHABIRI-COMMENT      =  "1"
                            MOVE      99   TO   FLG-COMMENT
                            PERFORM   2001-RIHA-COMMENT-SEC
                          ELSE
                            MOVE      99   TO   FLG-COMMENT
                            PERFORM   2001-RIHAIGAKU-COMMENT-SEC
                          END-IF
                        END-IF
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-RIHASEN-HANTEI-SEC
                      END-IF
      *               ¿Ç¶è£±£´¡¦£¸£°¤È¶¦ÍÑÊ¬
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NINRYO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-TUINZAI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-IZONSYO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SIPPUCOM-HANTEI-SEC
                      END-IF
      *               ¿Ç¶è£±£²¡¦£±£³¡¦£±£´¡¦£¸£°¶¦ÍÑÊ¬
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ONLINE-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE04-TENSOU-SEC
                      END-IF
                   WHEN      "99"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HOMONCOM-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE04-TENSOU-SEC
                      END-IF
                   WHEN   OTHER
                      PERFORM      2099-RECE04-TENSOU-SEC
                 END-EVALUATE
               END-IF
      *
               PERFORM 990-RECE04-NEXT-SEC
      *
           END-PERFORM
      *
           INITIALIZE    WRK-JIDO-SETCOM-AREA
      *
      *    ÆýÍÄ»ù¡¦ÍÄ»ù¤Î¥³¥á¥ó¥È
      *
           IF   RECE02-NYUYOJIFLG   NOT =   SPACE
      *      ÃÂÀ¸·î¤Ç¡¢ÃÂÀ¸Æü¤¬£±Æü°Ê³°¤ÎÆü¤Ç¤¢¤ë¾ì¹ç
             IF   RECE02-SRYYM  >= "200604"
               IF  (RECE02-BIRTHDAY(5:2)      =  RECE02-SRYYM(5:2))
               AND (RECE02-BIRTHDAY(7:2)  NOT =  "01"             )
               AND (RECE02-AGE                =   5               )
                   PERFORM   2001-NYUYOJI-COMMENT-SEC
               END-IF
             ELSE
                   PERFORM   2001-NYUYOJI-COMMENT-SEC
             END-IF
           END-IF
      *
      *    ¸øÈñ¼õ¿ÇÆü¤Î¥³¥á¥ó¥È
      *
           IF   RECE02-KOHJNISSUTBLINF    =   ALL " "
              CONTINUE
           ELSE
              PERFORM   2001-KOHJYUSIN-COMMENT-SEC
           END-IF
      *
      *    £³ºÐ»ù»ë³ÐÄ°³ÐÀºÌ©·ò¹¯¿Çºº¡ÊÃÏ°è¸øÈñ¡Ë¤ÎÅÀ¿ô¥³¥á¥ó¥È
      *
           IF   RECE02-SEIKENFLG      =  1
           AND  RECE02-SEIKENTEN  NOT = ZERO
              PERFORM   2001-KOHTEN-COMMENT-SEC
           END-IF
      *
      *    Îà»öÏ·¿ÍÁë¸ýÄ§¼ý³Û¤Î¥³¥á¥ó¥È
      *
           IF   RECE02-KOH-SAIKEIFLG    =   1
              PERFORM   2001-KOHSAIKEI-COMMENT-SEC
           END-IF
      *
      *    È¯¾ÉÆü¤Î¥³¥á¥ó¥È
           IF   RECE02-SRYYM      >=    "200604"
               PERFORM   2001-HASSYO-HANTEI-SEC
           ELSE
               MOVE      ZERO            TO   FLG-COMOKIKAE
               MOVE      CONS-KYUSEI     TO   WRK-SRYCD
               PERFORM   2001-KYUSEIHASSYO-HANTEI-SEC
           END-IF
      *
      *    Ê£¿ô¿ÇÎÅ²Ê¼õ¿Ç
      *
           IF   CNT-SANTEIKA   >  ZERO
      *
             MOVE       ZERO             TO  CNT-JUSHIN
             INITIALIZE                      TBL-JUSHIN-INF
      *      ¿ÇÎÅ²Ê¤Î¼èÆÀ
             INITIALIZE                      SYS-1005-REC
             MOVE    "1005"              TO  SYS-1005-KANRICD
             MOVE    RECE02-SRYYM        TO  SYS-1005-STYUKYMD(1:6)
             MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
             MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
grpsys       MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
             MOVE    SYS-1005-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key2"              TO  MCP-PATHNAME
grpsys       PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM   900-SYSKANRI-READ-SEC
               PERFORM   VARYING   IDX    FROM   1   BY   1
                          UNTIL   (IDX             >     100)
                            OR    (FLG-SYSKANRI    =       1)
                   MOVE    SYS-1005-KBNCD  TO TBL-JUSHIN-SRYKA(IDX)
                   MOVE    SYS-1005-SRYKANAME   
                                           TO TBL-JUSHIN-SRYKANAME(IDX)
      *            ¼õ¿ÇÍúÎò¥Æ¡¼¥Ö¥ë¤«¤é¿ÇÎÅ²ÊËè¤Î¼õ¿ÇÆü¤ò¸¡º÷¥»¥Ã¥È
                   PERFORM   2001-JUSHIN-KENSAKU-SEC
      *
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM   900-SYSKANRI-READ-SEC
               END-PERFORM
             END-IF
      *
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key2"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
      *      Ê£¿ô¤Î¿ÇÎÅ²Ê¼õ¿Ç¤¬¤¢¤ë¤«È½Äê
             MOVE       ZERO       TO   IDZ  
             INITIALIZE            WRK-FREE-AREA
      *
             PERFORM   VARYING   IDX    FROM   1   BY   1
                        UNTIL    IDX      >  100
               IF   TBL-JUSHIN-ARI (IDX)  =  "1"
                   ADD       1     TO   CNT-JUSHIN
                   PERFORM  2001-FREE-COMMENT-SEC
               END-IF
             END-PERFORM
      *      Ê£¿ô¿ÇÎÅ²Ê¼õ¿Ç¤Î¥³¥á¥ó¥ÈÊÔ½¸
             IF    CNT-JUSHIN  >   1
                    PERFORM   2001-JUSHIN-COMMENT-SEC
             END-IF
      *
           END-IF
      *
      *    ÅÅÏÃºÆ¿Ç¡¦Æ±ÆüÊ£¿ôºÆ¿Ç
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                          UNTIL   (IDX             >      5)
      *
              IF    CNT-DENWA(IDX)     >   ZERO
              OR    CNT-DOUJITU(IDX)   >   ZERO
              OR    CNT-DOUDEN(IDX)    >   ZERO
              OR    CNT-DENWA2(IDX)    >   ZERO
              OR    CNT-DOUDEN2(IDX)   >   ZERO
                  PERFORM    2001-DENWA-DOUJITU-COMMENT-SEC
              END-IF
      *
           END-PERFORM
      *
      *    ¿Í¹©¿ÕÂ¡¤ÎÈ½Äê
      *
           PERFORM    2001-JINZOU-HANTEI-SEC
      *
      *    ºß»Ù±ç¥³¥á¥ó¥È
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                      UNTIL    IDX      >    5
              IF     TBL-ZAISIEN-COMMENT(IDX)   NOT =  ZERO
                  PERFORM    2001-ZAISIEN-COMMENT-SEC
              END-IF
           END-PERFORM
      *
      *    ¾ã³²¤Î¥³¥á¥ó¥È¡Ê£·£µºÐÅþÃ£·î¤ÎÆÃÎã¡Ë
      *
           IF   RECE02-RJNGAIKOHFTN(3)  =   1
              PERFORM   2001-KOUREI-TOKUREI-COMMENT-SEC
           END-IF
      *
      *    ÆÃµ­»ö¹à£°£¹»Ü¤¬¤¢¤ë¾ì¹ç¤Î¡ÚÇÛ¡Û²ó¿ô¥³¥á¥ó¥È
      *
           IF   RECE02-TOKKI09FLG      =   1
           AND  RECE02-HAICOMFLG       =  ZERO
           AND  RECE02-DUMMYKAISU  NOT =  ZERO
              PERFORM   2001-HAIKAISU-COMMENT-SEC
           END-IF
      *
      *    £±£¹¸¶Çú°ìÈÌÆÃ¼ì·×»»¤ÎÉéÃ´³ä¹ç¥³¥á¥ó¥È
      *
           IF   RECE02-GENBAKU-TOKFLG   =   1
              PERFORM   2001-GENBAKU-COMMENT-SEC
           END-IF
      *
      *    Ç¥ÉØ¤Î¥³¥á¥ó¥È
      *
           IF   RECE02-NINPUFLG   =   1
              PERFORM   2001-NINPU-COMMENT-SEC
           END-IF
      *
      *****ÆâÉþÌôºÞÎÁÄþ¸º¤Ë´Ø¤¹¤ëÊÔ½¸£²****
           PERFORM     240-YAKUTEI-HENSYU2-SEC
      *
      *****º¹¹þÊÔ½¸£²**********************
           PERFORM     250-SASHIKOMI2-SEC
      *
      *****ÁªÂò¼°¥³¥á¥ó¥ÈÊÔ½¸**************
           IF   RECE02-SRYYM  >=  "201810"
             PERFORM   270-SENTAKUCOM-HENSYU-SEC
           END-IF
      *
      *****´Ø·¸¥³¥á¥ó¥ÈÊÔ½¸****************
           PERFORM     260-SETCOM-HENSYU-SEC
      *
           PERFORM 990-RECE02-READ-SEC
      *
           IF  CNT-IN-COMMIT   =    300
                 MOVE  ZERO          TO      CNT-IN-COMMIT
                 PERFORM 900-DBCOMMIT-SEC
                 PERFORM 900-DBSTART-SEC
           END-IF
      *
      *    ½èÍýÃæ»ßÀßÄê½èÍý
           IF      CNT-IN-CANCEL   =   50
               MOVE    ZERO            TO  CNT-IN-CANCEL
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼õ¿ÇÍúÎò¥Æ¡¼¥Ö¥ë¤«¤é¿ÇÎÅ²ÊËè¤Î¼õ¿ÇÆü¤ò¸¡º÷¥»¥Ã¥È
      *****************************************************************
       2001-JUSHIN-KENSAKU-SEC             SECTION.
      *
      *    ¼õ¿ÇÎò¤Î¸¡º÷
           INITIALIZE                      JYURRK-REC
grpsys     MOVE    RECE02-HOSPNUM      TO  JYURRK-HOSPNUM
           MOVE    RECE02-PTID         TO  JYURRK-PTID
           MOVE    RECE02-NYUGAIKBN    TO  JYURRK-NYUGAIKBN
           MOVE    TBL-JUSHIN-SRYKA(IDX)
                                       TO  JYURRK-SRYKA
           MOVE    RECE02-SRYYM        TO  JYURRK-SRYYMD(1:6)
           MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
           MOVE    RECE02-SRYYM        TO  JYURRK-UPSRYYMD(1:6)
           MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM   900-JYURRK-READ-SEC
               PERFORM   UNTIL   FLG-JYURRK   =    1
                   IF    JYURRK-HKNKBN    =    "0" OR "7"
                      MOVE   JYURRK-HKNCOMBINUM
                                       TO  WRK-HKNCOMBINUMX
                      IF  WRK-HKNCOMBINUM9  =  RECE02-HKNCOMBI(1) OR
                                               RECE02-HKNCOMBI(2) OR
                                               RECE02-HKNCOMBI(3) OR
                                               RECE02-HKNCOMBI(4) OR
                                               RECE02-HKNCOMBI(5) OR
                                               RECE02-HKNCOMBI(6) OR
                                               RECE02-HKNCOMBI(7) OR
                                               RECE02-HKNCOMBI(8) OR
                                               RECE02-HKNCOMBI(9) OR
                                               RECE02-HKNCOMBI(10)
                            MOVE   JYURRK-SRYYMD   TO   WRK-SRYYMDX
                            IF   RECE02-JNISSUTBL(WRK-SRYDD) = SPACE
                               CONTINUE
                            ELSE
                               MOVE   "1"     TO    
                                        TBL-JUSHIN-ARI (IDX)
                               MOVE   "1"     TO    
                                        TBL-JUSHIN-DAY (IDX WRK-SRYDD)
                            END-IF
                      END-IF      
                   END-IF
      *
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key24"             TO  MCP-PATHNAME
                   PERFORM   900-JYURRK-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2001-JUSHIN-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ê£¿ô¿ÇÎÅ²Ê¼õ¿Ç¤Î²Ê¡¦ÆüÉÕ¤ÎÊÔ½¸
      *****************************************************************
       2001-FREE-COMMENT-SEC          SECTION.
      *
           ADD      1                      TO  IDZ
           MOVE     ZERO                   TO  WRK-KETA
                                               WRK-KETA-TAIHI
                                               WRK-KETA2
      *
           IF  TBL-JUSHIN-SRYKANAME(IDX)  NOT =  SPACE
             INITIALIZE                      ORCSKANACHKAREA
             MOVE    "2"                     TO  KANACHK-SYORI 
             MOVE  TBL-JUSHIN-SRYKANAME(IDX) TO  KANACHK-MAE-INPUT
             CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
             IF      KANACHK-RC      =   ZERO
                 COMPUTE WRK-KETA          =   KANACHK-MAX
                 COMPUTE WRK-KETA-TAIHI    =   KANACHK-MAX
                 COMPUTE WRK-KETA2         =   KANACHK-MAX  /  2
                 MOVE    WRK-KETA2   TO  WK-FREE-YUKOKETA(IDZ)
                 MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                   TO  WK-FREE-NAME(IDZ)(1:KANACHK-MAX)
             END-IF    
           END-IF
      *
           MOVE      ZERO          TO  FLG-FIRST
      *
           PERFORM   VARYING   IDY    FROM   1   BY   1
                          UNTIL   (IDY             >      31)
      *
             IF  TBL-JUSHIN-DAY(IDX IDY) =  "1"
                 IF  FLG-FIRST  =  ZERO
                    IF  IDY     <  10 
                       ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                       MOVE    "¡¡"
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                       ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                       MOVE    TBL-DAY(IDY)(3:2)     
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                       ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                       MOVE    "Æü"
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                    ELSE
                       ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                       MOVE    "¡¡"
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                       ADD   2  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                       MOVE    TBL-DAY(IDY)     
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:4)
      *
                       ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                       MOVE    "Æü"
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                    END-IF
                    MOVE      1              TO  FLG-FIRST
                 ELSE
                    IF  IDY     <  10 
                       IF    WK-FREE-YUKOKETA(IDZ)  =  25
                          PERFORM  20011-FREE-COMMENT-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    "¡¢"
                                    TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "¡¢"
                                    TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                       END-IF
      *
                       IF    WK-FREE-YUKOKETA(IDZ)  =  24 OR 25
                          PERFORM  20011-FREE-COMMENT-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    TBL-DAY(IDY)(3:2)     
                                    TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "Æü"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2) 
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)(3:2)     
                                    TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "Æü"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2) 
                       END-IF
                    ELSE
                       IF    WK-FREE-YUKOKETA(IDZ)  =  25
                          PERFORM  20011-FREE-COMMENT-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    "¡¢"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "¡¢"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                       END-IF
      *
                       IF    WK-FREE-YUKOKETA(IDZ)  =  23 OR 24 OR 25
                          PERFORM  20011-FREE-COMMENT-KAIGYO-SEC
                          ADD   2  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    TBL-DAY(IDY)     
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:4)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "Æü"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   2  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)     
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:4)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "Æü"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                       END-IF
                    END-IF
                 END-IF
             END-IF
      *
           END-PERFORM
           .
       2001-FREE-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ê£¿ô¿ÇÎÅ²Ê¼õ¿Ç¤Î²Ê¡¦ÆüÉÕ¤ÎÊÔ½¸¡Ê²þ¹Ô¡Ë
      *****************************************************************
       20011-FREE-COMMENT-KAIGYO-SEC          SECTION.
      *
           ADD      1              TO  IDZ
      *
           MOVE    WRK-KETA-TAIHI  TO  WRK-KETA
           MOVE    WRK-KETA2       TO  WK-FREE-YUKOKETA(IDZ)
           MOVE    ALL "¡¡"        TO  WK-FREE-NAME(IDZ)(1:WRK-KETA)
      *
           .
       20011-FREE-COMMENT-KAIGYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ê£¿ô¿ÇÎÅ²Ê¼õ¿Ç¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-JUSHIN-COMMENT-SEC          SECTION.
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                TO  IDX-RECE05
           MOVE    SPACE               TO  WRK-RECE05XX-REC
           INITIALIZE                      WRK-RECE05XX-REC
      *
           ADD     1           TO  IDX-RECE05
           MOVE    "810000001" TO  WRK-RECE05XX-SRYCD(IDX-RECE05 1)
           MOVE    ZERO        TO  WRK-RECE05XX-SRYSURYO(IDX-RECE05 1)
           MOVE    7           TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
           MOVE    "Ê£¿ô¿ÇÎÅ²Ê¼õ¿Ç"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   49    )
                         OR      ( WK-FREE-NAME(IDX) =   SPACE )
                ADD    1       TO  IDX-RECE05
                MOVE   "810000001"
                               TO  WRK-RECE05XX-SRYCD(IDX-RECE05 1)
                MOVE   ZERO    TO  WRK-RECE05XX-SRYSURYO(IDX-RECE05 1)
                MOVE   WK-FREE-YUKOKETA(IDX)
                               TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                MOVE   WK-FREE-NAME(IDX)
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
           END-PERFORM 
      *
      *    ²Ê¡¦ÆüÉÕ¤ÎÊÔ½¸¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE05
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE05
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE05  =    1
                PERFORM 800-RECE02-SET059-SEC
      *         MOVE    ZERO                 TO  RECE059-ZAINUM
                MOVE    99999999             TO  RECE059-ZAINUM
                MOVE    WRK-KOHBUNRI-JIDO    TO  RECE059-KOHBUNRI
                MOVE    "99"                 TO  RECE059-SRYKBN
                ADD     1                    TO  WRK-RENNUM
                MOVE    WRK-RENNUM           TO  RECE059-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                         UNTIL   (IDX       >    5)   OR
                                 (FLG-WRKRECE05   =   1)
                   ADD         1             TO    IDX-RECE05
                   IF (WRK-RECE05XX-SRYCD(IDX-RECE05 1) = SPACE)
                   OR (IDX-RECE05                       >  100 )
                       MOVE    1             TO    FLG-WRKRECE05
                   ELSE
                       MOVE  WRK-RECE05XX-SRYACT-TBL (IDX-RECE05 1)
                                        TO   RECE059-SRYACT-TBL (IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF  
                END-PERFORM
                IF   RECE059-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE059-REC
                   ADD     1            TO   CNT-RECE059
                END-IF
           END-PERFORM     
      *
      *
           PERFORM 800-RECE02-SET049-SEC
      *    MOVE    ZERO                TO  RECE049-ZAINUM
           MOVE    99999999            TO  RECE049-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE049-KOHBUNRI
           MOVE    "99"                TO  RECE049-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE049-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE049-MEISAISU
      *
           WRITE   RECE049-REC
           ADD     1                   TO  CNT-RECE049
      *
           .
       2001-JUSHIN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÅÏÃºÆ¿Ç¡¦Æ±ÆüÊ£¿ôºÆ¿Ç¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-DENWA-DOUJITU-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE02-SET049-SEC
           COMPUTE WRK-ZAINUM    =  99999990  +  IDX
           MOVE    WRK-ZAINUM          TO  RECE049-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  IDX  -  1
           MOVE    WRK-KOHBUNRI        TO  RECE049-KOHBUNRI
           MOVE    "99"                TO  RECE049-SRYKBN
      *
           IF    CNT-DENWA(IDX)     >   ZERO
              ADD     1                TO  RECE049-MEISAISU
           END-IF
           IF    CNT-DOUJITU(IDX)   >   ZERO
              ADD     1                TO  RECE049-MEISAISU
           END-IF
           IF    CNT-DOUDEN(IDX)    >   ZERO
              ADD     1                TO  RECE049-MEISAISU
           END-IF
           IF    CNT-DENWA2(IDX)    >   ZERO
              ADD     1                TO  RECE049-MEISAISU
           END-IF
           IF    CNT-DOUDEN2(IDX)   >   ZERO
              ADD     1                TO  RECE049-MEISAISU
           END-IF
      *
           MOVE    TBL-COUNT-HKNCOMBI(IDX)
                                       TO  RECE049-HKNCOMBI
      *
           WRITE   RECE049-REC
           ADD     1                   TO  CNT-RECE049
      ***
           PERFORM 800-RECE02-SET059-SEC
           COMPUTE WRK-ZAINUM    =  99999990  +  IDX
           MOVE    WRK-ZAINUM          TO  RECE059-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  IDX  -  1
           MOVE    WRK-KOHBUNRI        TO  RECE059-KOHBUNRI
           MOVE    "99"                TO  RECE059-SRYKBN
      *
           MOVE    SPACE               TO  WRK-RECE05XX-REC
           INITIALIZE                      WRK-RECE05XX-REC
           MOVE    ZERO                TO  IDX40
      *    ÅÅÏÃºÆ¿Ç¤Î¥³¥á¥ó¥È¤ÎÁÞÆþ
           IF    CNT-DENWA(IDX)     >   ZERO
             ADD     1                 TO  IDX40
             MOVE    "810000001"       TO  WRK-RECE05XX-SRYCD(1 1)
             MOVE    ZERO              TO  WRK-RECE05XX-SRYSURYO(1 1)
             MOVE    CNT-DENWA(IDX)    TO  DAY-IDX2
             PERFORM    20011-KAISU-HENSYU-SEC
             IF   RECE02-SRYYM  >=  "201904"
               MOVE  10                TO  WRK-RECE05XX-YUKOKETA(1 1)
               MOVE  "ÅÅÏÃÅùºÆ¿Ç¡Ê¡¡¡¡²ó¡Ë"
                                       TO  WRK-RECE05XX-NAME(1 1)
               MOVE  WRK-HENSYU-KAISU2 TO  WRK-RECE05XX-NAME(1 1)(13:4)
             ELSE
               MOVE  9                 TO  WRK-RECE05XX-YUKOKETA(1 1)
               MOVE  "ÅÅÏÃºÆ¿Ç¡Ê¡¡¡¡²ó¡Ë"
                                       TO  WRK-RECE05XX-NAME(1 1)
               MOVE  WRK-HENSYU-KAISU2 TO  WRK-RECE05XX-NAME(1 1)(11:4)
             END-IF
             MOVE    WRK-RECE05XX-SRYACT-TBL (1 1)
                                       TO  RECE059-SRYACT-TBL(IDX40)
           END-IF
      *    Æ±ÆüÊ£¿ôºÆ¿Ç¤Î¥³¥á¥ó¥È¤ÎÁÞÆþ
           IF    CNT-DOUJITU(IDX)   >   ZERO
             ADD     1                 TO  IDX40
             IF   RECE02-SRYYM      >=    "201004"
               MOVE  "810000001"       TO  WRK-RECE05XX-SRYCD(2 1)
               MOVE  ZERO              TO  WRK-RECE05XX-SRYSURYO(2 1)
               MOVE  9                 TO  WRK-RECE05XX-YUKOKETA(2 1)
               MOVE  "Æ±ÆüºÆ¿Ç¡Ê¡¡¡¡²ó¡Ë"
                                       TO  WRK-RECE05XX-NAME(2 1)
      *
               MOVE  CNT-DOUJITU(IDX)  TO  DAY-IDX2
               PERFORM    20011-KAISU-HENSYU-SEC
               MOVE  WRK-HENSYU-KAISU2 TO  WRK-RECE05XX-NAME(2 1)(11:4)
      *
               MOVE  WRK-RECE05XX-SRYACT-TBL(2 1)
                                       TO  RECE059-SRYACT-TBL(IDX40)
             ELSE
               MOVE  "810000001"       TO  WRK-RECE05XX-SRYCD(2 1)
               MOVE  ZERO              TO  WRK-RECE05XX-SRYSURYO(2 1)
               MOVE  11                TO  WRK-RECE05XX-YUKOKETA(2 1)
               MOVE  "Æ±ÆüÊ£¿ôºÆ¿Ç¡Ê¡¡¡¡²ó¡Ë"
                                       TO  WRK-RECE05XX-NAME(2 1)
      *
               MOVE  CNT-DOUJITU(IDX)  TO  DAY-IDX2
               PERFORM    20011-KAISU-HENSYU-SEC
               MOVE  WRK-HENSYU-KAISU2 TO  WRK-RECE05XX-NAME(2 1)(15:4)
      *
               MOVE  WRK-RECE05XX-SRYACT-TBL(2 1)
                                       TO  RECE059-SRYACT-TBL(IDX40)
             END-IF
           END-IF
      *    Æ±ÆüÅÅÏÃºÆ¿Ç¤Î¥³¥á¥ó¥È¤ÎÁÞÆþ
           IF    CNT-DOUDEN(IDX)    >   ZERO
             ADD     1                 TO  IDX40
             MOVE    "810000001"       TO  WRK-RECE05XX-SRYCD(3 1)
             MOVE    ZERO              TO  WRK-RECE05XX-SRYSURYO(3 1)
             MOVE    CNT-DOUDEN(IDX)   TO  DAY-IDX2
             PERFORM    20011-KAISU-HENSYU-SEC
             IF   RECE02-SRYYM  >=  "201904"
               MOVE  12                TO  WRK-RECE05XX-YUKOKETA(3 1)
               MOVE  "Æ±ÆüÅÅÏÃÅùºÆ¿Ç¡Ê¡¡¡¡²ó¡Ë"
                                       TO  WRK-RECE05XX-NAME(3 1)
               MOVE  WRK-HENSYU-KAISU2 TO  WRK-RECE05XX-NAME(3 1)(17:4)
             ELSE
               MOVE  11                TO  WRK-RECE05XX-YUKOKETA(3 1)
               MOVE  "Æ±ÆüÅÅÏÃºÆ¿Ç¡Ê¡¡¡¡²ó¡Ë"
                                       TO  WRK-RECE05XX-NAME(3 1)
               MOVE  WRK-HENSYU-KAISU2 TO  WRK-RECE05XX-NAME(3 1)(15:4)
             END-IF
             MOVE    WRK-RECE05XX-SRYACT-TBL(3 1)
                                       TO  RECE059-SRYACT-TBL(IDX40)
           END-IF
      *    ÅÅÏÃºÆ¿Ç¡Ê£³£°Ç¯£³·î°ÊÁ°·ÑÂ³¡Ë¤Î¥³¥á¥ó¥È¤ÎÁÞÆþ
           IF    CNT-DENWA2(IDX)    >   ZERO
             ADD     1                 TO  IDX40
             MOVE    "810000001"       TO  WRK-RECE05XX-SRYCD(4 1)
             MOVE    ZERO              TO  WRK-RECE05XX-SRYSURYO(4 1)
             MOVE    CNT-DENWA2(IDX)   TO  DAY-IDX2
             PERFORM    20011-KAISU-HENSYU-SEC
             IF   RECE02-SRYYM  >=  "201904"
               MOVE  21                TO  WRK-RECE05XX-YUKOKETA(4 1)
               MOVE  "ÅÅÏÃÅùºÆ¿Ç¡Ê£³£°Ç¯£³·î°ÊÁ°·ÑÂ³¡Ë¡Ê¡¡¡¡²ó¡Ë"
                                       TO  WRK-RECE05XX-NAME(4 1)
               MOVE  WRK-HENSYU-KAISU2 TO  WRK-RECE05XX-NAME(4 1)(35:4)
             ELSE
               MOVE  20                TO  WRK-RECE05XX-YUKOKETA(4 1)
               MOVE  "ÅÅÏÃºÆ¿Ç¡Ê£³£°Ç¯£³·î°ÊÁ°·ÑÂ³¡Ë¡Ê¡¡¡¡²ó¡Ë"
                                       TO  WRK-RECE05XX-NAME(4 1)
               MOVE  WRK-HENSYU-KAISU2 TO  WRK-RECE05XX-NAME(4 1)(33:4)
             END-IF
             MOVE    WRK-RECE05XX-SRYACT-TBL (4 1)
                                       TO  RECE059-SRYACT-TBL(IDX40)
           END-IF
      *    Æ±ÆüÅÅÏÃºÆ¿Ç¡Ê£³£°Ç¯£³·î°ÊÁ°·ÑÂ³¡Ë¤Î¥³¥á¥ó¥È¤ÎÁÞÆþ
           IF    CNT-DOUDEN2(IDX)   >   ZERO
             ADD     1                 TO  IDX40
             MOVE    "810000001"       TO  WRK-RECE05XX-SRYCD(5 1)
             MOVE    ZERO              TO  WRK-RECE05XX-SRYSURYO(5 1)
             MOVE    CNT-DOUDEN2(IDX)  TO  DAY-IDX2
             PERFORM    20011-KAISU-HENSYU-SEC
             IF   RECE02-SRYYM  >=  "201904"
               MOVE  23                TO  WRK-RECE05XX-YUKOKETA(5 1)
               MOVE  "Æ±ÆüÅÅÏÃÅùºÆ¿Ç¡Ê£³£°Ç¯£³·î°ÊÁ°·ÑÂ³¡Ë¡Ê¡¡¡¡²ó¡Ë"
                                       TO  WRK-RECE05XX-NAME(5 1)
               MOVE  WRK-HENSYU-KAISU2 TO  WRK-RECE05XX-NAME(5 1)(39:4)
             ELSE
               MOVE  22                TO  WRK-RECE05XX-YUKOKETA(5 1)
               MOVE  "Æ±ÆüÅÅÏÃºÆ¿Ç¡Ê£³£°Ç¯£³·î°ÊÁ°·ÑÂ³¡Ë¡Ê¡¡¡¡²ó¡Ë"
                                       TO  WRK-RECE05XX-NAME(5 1)
               MOVE  WRK-HENSYU-KAISU2 TO  WRK-RECE05XX-NAME(5 1)(37:4)
             END-IF
             MOVE    WRK-RECE05XX-SRYACT-TBL(5 1)
                                       TO  RECE059-SRYACT-TBL(IDX40)
           END-IF
      *
           WRITE   RECE059-REC
           ADD     1                    TO  CNT-RECE059
      *
           .
       2001-DENWA-DOUJITU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ²ó¿ôÊÔ½¸
      *****************************************************************
       20011-KAISU-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE                TO  WRK-HENSYU-KAISU
           MOVE    SPACE                TO  WRK-HENSYU-KAISU2
      *
           MOVE    DAY-IDX2             TO  WRK-HENSYU-KAISU
           IF      WRK-HENSYU-KAISU(1:1) =  "0"
               MOVE      SPACE          TO  WRK-HENSYU-KAISU(1:1)
           END-IF
      *
           INITIALIZE                       ORCSKANACHKAREA
           MOVE    "2"                  TO  KANACHK-SYORI
           MOVE    WRK-HENSYU-KAISU     TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
           IF      KANACHK-RC      =   ZERO
                MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WRK-HENSYU-KAISU2
           END-IF
      *
           .
       20011-KAISU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿Í¹©¿ÕÂ¡¤ÎÈ½Äê
      *****************************************************************
       2001-JINZOU-HANTEI-SEC          SECTION.
      *
      *    ¿Í¹©¿ÕÂ¡¤ÈºÆ¿Ç¤ÎÆÍ¤­¹ç¤ï¤»
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL   ( IDX           >   5  )
      *
              MOVE       ZERO             TO     CNT-JINZOU
              PERFORM    VARYING    IDY   FROM   1   BY   1
                      UNTIL      IDY   >    31
                  IF   (WRK-JINZOU(IDX IDY)    =   "1")  AND
                       (WRK-SAISHIN4(IDX IDY)  =   "1")
                       ADD     1           TO   CNT-JINZOU
                  END-IF
              END-PERFORM
      *
              IF      CNT-JINZOU        NOT =      ZERO
                  PERFORM    20011-JINZOU-COMMENT-SEC
              END-IF
      *
           END-PERFORM
      *
           .
       2001-JINZOU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿Í¹©¿ÕÂ¡¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       20011-JINZOU-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE02-SET049-SEC
           COMPUTE WRK-ZAINUM    =  99999980  +  IDX
           MOVE    WRK-ZAINUM          TO  RECE049-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  IDX  -  1
           MOVE    WRK-KOHBUNRI        TO  RECE049-KOHBUNRI
           MOVE    "40"                TO  RECE049-SRYKBN
      *
           MOVE    1                   TO  RECE049-MEISAISU
      *
           MOVE    WRK-JINZOU-HKNCOMBI(IDX)
                                       TO  RECE049-HKNCOMBI
      *
           WRITE   RECE049-REC
           ADD     1                   TO  CNT-RECE049
      *
           PERFORM 800-RECE02-SET059-SEC
           COMPUTE WRK-ZAINUM    =  99999980  +  IDX
           MOVE    WRK-ZAINUM          TO  RECE059-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  IDX  -  1
           MOVE    WRK-KOHBUNRI        TO  RECE059-KOHBUNRI
           MOVE    "40"                TO  RECE059-SRYKBN
      *    ¿Í¹©¿ÕÂ¡¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE                TO  WRK-RECE05XX-REC
           INITIALIZE                       WRK-RECE05XX-REC
           MOVE    "810000001"          TO  WRK-RECE05XX-SRYCD(1 1)
           MOVE    ZERO                 TO  WRK-RECE05XX-SRYSURYO(1 1)
           MOVE    14                   TO  WRK-RECE05XX-YUKOKETA(1 1)
           MOVE    "ºÆ¿Ç£´²óÌÜ°Ê¹ß¼Â»Ü²ó¿ô¡¡¡¡²ó"
                                        TO  WRK-RECE05XX-NAME(1 1)
           MOVE    CNT-JINZOU           TO  DAY-IDX
           MOVE    TBL-DAY(DAY-IDX)     TO  WRK-RECE05XX-NAME(1 1)(23:4)
           MOVE    WRK-RECE05XX-SRYACT-TBL(1 1)
                                        TO  RECE059-SRYACT-TBL(1)
           WRITE   RECE059-REC
           ADD     1                    TO  CNT-RECE059
      *
           .
       20011-JINZOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºß»Ù±ç¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-ZAISIEN-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE02-SET049-SEC
           COMPUTE WRK-ZAINUM    =  99999964  +  IDX
           MOVE    WRK-ZAINUM          TO  RECE049-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  IDX  -  1
           MOVE    WRK-KOHBUNRI        TO  RECE049-KOHBUNRI
           MOVE    WRK-RECE04-SRYKBN   TO  RECE049-SRYKBN
      *
           MOVE    1                   TO  RECE049-MEISAISU
      *
           MOVE    TBL-ZAISIEN-HKNCOMBI(IDX)
                                       TO  RECE049-HKNCOMBI
      *
           WRITE   RECE049-REC
           ADD     1                   TO  CNT-RECE049
      *
           PERFORM 800-RECE02-SET059-SEC
           COMPUTE WRK-ZAINUM    =  99999964  +  IDX
           MOVE    WRK-ZAINUM          TO  RECE059-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  IDX  -  1
           MOVE    WRK-KOHBUNRI        TO  RECE059-KOHBUNRI
           MOVE    WRK-RECE04-SRYKBN   TO  RECE059-SRYKBN
      *    ºß»Ù±ç¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE                TO  WRK-RECE05XX-REC
           INITIALIZE                       WRK-RECE05XX-REC
           MOVE    "810000001"          TO  WRK-RECE05XX-SRYCD(1 1)
           MOVE    ZERO                 TO  WRK-RECE05XX-SRYSURYO(1 1)
           MOVE    3                    TO  WRK-RECE05XX-YUKOKETA(1 1)
           EVALUATE  TBL-ZAISIEN-COMMENT(IDX)
             WHEN      1
               MOVE    "ºß»Ù±ç"         TO  WRK-RECE05XX-NAME(1 1)
             WHEN      2
               MOVE    "ºß»ÙÉÂ"         TO  WRK-RECE05XX-NAME(1 1)
           END-EVALUATE
           MOVE    WRK-RECE05XX-SRYACT-TBL(1 1)
                                        TO  RECE059-SRYACT-TBL(1)
           WRITE   RECE059-REC
           ADD     1                    TO  CNT-RECE059
      *
           .
       2001-ZAISIEN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆýÍÄ»ù¡¦ÍÄ»ù¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-NYUYOJI-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE02-SET049-SEC
           MOVE    99999990            TO  RECE049-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE049-KOHBUNRI
           MOVE    "99"                TO  RECE049-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE049-HKNCOMBI
      *
           MOVE    1                   TO  RECE049-MEISAISU
      *
           WRITE   RECE049-REC
           ADD     1                   TO  CNT-RECE049
      *
           PERFORM 800-RECE02-SET059-SEC
           MOVE    99999990            TO  RECE059-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE059-KOHBUNRI
           MOVE    "99"                TO  RECE059-SRYKBN
      *    ¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE               TO  WRK-RECE05XX-REC
           INITIALIZE                      WRK-RECE05XX-REC
           IF   RECE02-SRYYM  >= "201810"
      *      ¡Ö·î¤ÎÅÓÃæ¤Þ¤ÇÆýÍÄ»ù¡×
             MOVE  "820100005"      TO  WRK-TENSU-SRYCD
             MOVE  RECE02-SRYYM     TO  WRK-TENSU-SRYYM
             PERFORM 900-TENSU-TBL-SEC
      *
             MOVE  TNS-SRYCD        TO  WRK-RECE05XX-SRYCD(1 1)
             MOVE  ZERO             TO  WRK-RECE05XX-SRYSURYO(1 1)
             MOVE  TNS-YUKOKETA     TO  WRK-RECE05XX-YUKOKETA(1 1)
             MOVE  TNS-NAME         TO  WRK-RECE05XX-NAME(1 1)
                                        WRK-RECE05XX-INPUTCOMENT(1 1)
           ELSE
             EVALUATE  RECE02-NYUYOJIFLG
               WHEN      "1"
                 MOVE  "810000001"  TO  WRK-RECE05XX-SRYCD(1 1)
                 MOVE   ZERO        TO  WRK-RECE05XX-SRYSURYO(1 1)
                 MOVE   11          TO  WRK-RECE05XX-YUKOKETA(1 1)
                 MOVE  "¡¡¡¡·î¡¡¡¡Æü¤Þ¤ÇÆýÍÄ»ù"
                                    TO  WRK-RECE05XX-NAME(1 1)
               WHEN      "2"
                 MOVE  "810000001"  TO  WRK-RECE05XX-SRYCD(1 1)
                 MOVE   ZERO        TO  WRK-RECE05XX-SRYSURYO(1 1)
                 MOVE   10          TO  WRK-RECE05XX-YUKOKETA(1 1)
                 MOVE  "¡¡¡¡·î¡¡¡¡Æü¤Þ¤ÇÍÄ»ù"
                                    TO  WRK-RECE05XX-NAME(1 1)
             END-EVALUATE
      *
             MOVE  RECE02-BIRTHDAY(5:2) TO  DAY-IDX2
             MOVE  TBL-DAY(DAY-IDX2)    TO  WRK-RECE05XX-NAME(1 1)(1:4)
             MOVE  RECE02-BIRTHDAY(7:2) TO  DAY-IDX2
             COMPUTE   DAY-IDX2  =  DAY-IDX2  -  1
             IF    DAY-IDX2  NOT =  ZERO
               MOVE  TBL-DAY(DAY-IDX2)  TO  WRK-RECE05XX-NAME(1 1)(7:4)
             END-IF
           END-IF
      *
           MOVE    WRK-RECE05XX-SRYACT-TBL(1 1)
                                        TO  RECE059-SRYACT-TBL(1)
           WRITE   RECE059-REC
           ADD     1                    TO  CNT-RECE059
      *
           .
       2001-NYUYOJI-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¾ã³²¤Î¥³¥á¥ó¥È¡Ê£·£µºÐÅþÃ£·î¤ÎÆÃÎã¡Ë
      *****************************************************************
       2001-KOUREI-TOKUREI-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE02-SET049-SEC
           MOVE    99999998            TO  RECE049-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE049-KOHBUNRI
           MOVE    "99"                TO  RECE049-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE049-HKNCOMBI
      *
           MOVE    1                   TO  RECE049-MEISAISU
      *
           WRITE   RECE049-REC
           ADD     1                   TO  CNT-RECE049
      *
           PERFORM 800-RECE02-SET059-SEC
           MOVE    99999998            TO  RECE059-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE059-KOHBUNRI
           MOVE    "99"                TO  RECE059-SRYKBN
      *    ¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE               TO  WRK-RECE05XX-REC
           INITIALIZE                      WRK-RECE05XX-REC
           MOVE    "820000143"         TO  WRK-RECE05XX-SRYCD(1 1)
           MOVE     ZERO               TO  WRK-RECE05XX-SRYSURYO(1 1)
           MOVE     4                  TO  WRK-RECE05XX-YUKOKETA(1 1)
           MOVE    "¡Ê¾ã³²¡Ë"          TO  WRK-RECE05XX-NAME(1 1)
      *
           MOVE    WRK-RECE05XX-SRYACT-TBL(1 1)
                                        TO  RECE059-SRYACT-TBL(1)
           WRITE   RECE059-REC
           ADD     1                    TO  CNT-RECE059
      *
           .
       2001-KOUREI-TOKUREI-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆÃµ­»ö¹à£°£¹»Ü¤¬¤¢¤ë¾ì¹ç¤Î¡ÚÇÛ¡Û²ó¿ô¥³¥á¥ó¥È
      *****************************************************************
       2001-HAIKAISU-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE02-SET049-SEC
           MOVE    99999988            TO  RECE049-ZAINUM
      *****MOVE    WRK-KOHBUNRI-JIDO   TO  RECE049-KOHBUNRI
           MOVE    WRK-KOHBUNRI-JIDO2  TO  RECE049-KOHBUNRI
           MOVE    "99"                TO  RECE049-SRYKBN
           MOVE    "991"               TO  RECE049-SRYSYUKBN
      *****MOVE    WRK-HKNCOMBI-JIDO   TO  RECE049-HKNCOMBI
           MOVE    WRK-HKNCOMBI-JIDO2  TO  RECE049-HKNCOMBI
      *
           MOVE    1                   TO  RECE049-MEISAISU
      *
           WRITE   RECE049-REC
           ADD     1                   TO  CNT-RECE049
      *
           PERFORM 800-RECE02-SET059-SEC
           MOVE    99999988            TO  RECE059-ZAINUM
      *****MOVE    WRK-KOHBUNRI-JIDO   TO  RECE059-KOHBUNRI
           MOVE    WRK-KOHBUNRI-JIDO2  TO  RECE059-KOHBUNRI
           MOVE    "99"                TO  RECE059-SRYKBN
           MOVE    "991"               TO  RECE059-SRYSYUKBN
      *    ¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE               TO  WRK-RECE05XX-REC
           INITIALIZE                      WRK-RECE05XX-REC
      *
           MOVE    "810000001"         TO  WRK-RECE05XX-SRYCD(1 1)
           MOVE     ZERO               TO  WRK-RECE05XX-SRYSURYO(1 1)
      *    Á´³ÑÊÑ´¹
           INITIALIZE                      WRK-CHAR-HENKAN-AREA
           MOVE     RECE02-DUMMYKAISU  TO  WRK-SRYKAISU-X
           MOVE     WRK-SRYKAISU-X     TO  WRK-CHAR-X
           PERFORM  2502111-NUM-ZENKAKU-HENKAN-SEC
           EVALUATE WRK-CHAR-N-CNT
             WHEN       2
               MOVE "¡ÚÇÛ¡Û¡¡²ó"       TO  WRK-RECE05XX-NAME(1 1)
               MOVE WRK-CHAR-N(1:2)    TO  WRK-RECE05XX-NAME(1 1)(7:2)
               MOVE 5                  TO  WRK-RECE05XX-YUKOKETA(1 1)
             WHEN       4
               MOVE "¡ÚÇÛ¡Û¡¡¡¡²ó"     TO  WRK-RECE05XX-NAME(1 1)
               MOVE WRK-CHAR-N(1:4)    TO  WRK-RECE05XX-NAME(1 1)(7:4)
               MOVE 6                  TO  WRK-RECE05XX-YUKOKETA(1 1)
             WHEN       6
               MOVE "¡ÚÇÛ¡Û¡¡¡¡¡¡²ó"   TO  WRK-RECE05XX-NAME(1 1)
               MOVE WRK-CHAR-N(1:6)    TO  WRK-RECE05XX-NAME(1 1)(7:6)
               MOVE 7                  TO  WRK-RECE05XX-YUKOKETA(1 1)
           END-EVALUATE
      *
           MOVE    WRK-RECE05XX-SRYACT-TBL(1 1)
                                       TO  RECE059-SRYACT-TBL(1)
           WRITE   RECE059-REC
           ADD     1                   TO  CNT-RECE059
      *
           .
       2001-HAIKAISU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £±£¹¸¶Çú°ìÈÌÆÃ¼ì·×»»¤ÎÉéÃ´³ä¹ç¥³¥á¥ó¥È
      *****************************************************************
       2001-GENBAKU-COMMENT-SEC          SECTION.
      *
           PERFORM  VARYING  IDX38  FROM  1  BY  1
                     UNTIL  (IDX38   >   10                )
                      OR    (RECE02-HKNCOMBI(IDX38) =  ZERO)
             COMPUTE  WRK-KOHBUNRI = RECE02-RIHA-KOHBUNRI(IDX38)
                                   + 1
             IF   RECE02-GENBAKU-FTNKIN(IDX38)  NOT =  ZERO
               MOVE  1   TO  TBL-GENBAKU-COMMENT(WRK-KOHBUNRI)
               MOVE  RECE02-HKNCOMBI(IDX38)
                         TO  TBL-GENBAKU-HKNCOMBI(WRK-KOHBUNRI)
      *
               MOVE  RECE02-GENBAKU-IDX(IDX38)  TO  IDX39
               COMPUTE WRK-GENBAKU-TOTALKIN = RECE02-TOTALTEN(IDX39)
                                            * 10
               MOVE  RECE02-GENBAKU-FTNKIN(IDX38)
                                                TO  WRK-GENBAKU-FTNKIN
               COMPUTE TBL-GENBAKU-FTNRATE(WRK-KOHBUNRI) =
                       (WRK-GENBAKU-FTNKIN / WRK-GENBAKU-TOTALKIN)
                                           * 100
             END-IF
           END-PERFORM
      *
           PERFORM  VARYING  IDX38  FROM  1  BY  1
                     UNTIL   IDX38   >    5
      *
             IF  TBL-GENBAKU-COMMENT(IDX38)  =  1
      *
               PERFORM 800-RECE02-SET049-SEC
               COMPUTE WRK-ZAINUM    =  99999954  +  IDX38
               MOVE    WRK-ZAINUM    TO  RECE049-ZAINUM
               COMPUTE WRK-KOHBUNRI  =  IDX38  -  1
               MOVE    WRK-KOHBUNRI  TO  RECE049-KOHBUNRI
               MOVE    "99"          TO  RECE049-SRYKBN
               MOVE    "990"         TO  RECE049-SRYSYUKBN
               MOVE    TBL-GENBAKU-HKNCOMBI(IDX38)
                                     TO  RECE049-HKNCOMBI
               MOVE    1             TO  RECE049-MEISAISU
               WRITE   RECE049-REC
               ADD     1             TO  CNT-RECE049
      *
               PERFORM 800-RECE02-SET059-SEC
               COMPUTE WRK-ZAINUM    =  99999954  +  IDX38
               MOVE    WRK-ZAINUM    TO  RECE059-ZAINUM
               COMPUTE WRK-KOHBUNRI  =  IDX38  -  1
               MOVE    WRK-KOHBUNRI  TO  RECE059-KOHBUNRI
               MOVE    "99"          TO  RECE059-SRYKBN
               MOVE    "990"         TO  RECE059-SRYSYUKBN
      *        ¥³¥á¥ó¥È¤ÎÁÞÆþ
               MOVE    SPACE         TO  WRK-RECE05XX-REC
               INITIALIZE                WRK-RECE05XX-REC
               MOVE    "810000001"   TO  WRK-RECE05XX-SRYCD(1 1)
               MOVE    ZERO          TO  WRK-RECE05XX-SRYSURYO(1 1)
      *
               MOVE    TBL-GENBAKU-FTNRATE(IDX38)
                                     TO  WRK-GENBAKU-FTNRATE
               IF   WRK-GENBAKU-FTNRATE2  =  ZERO
                 MOVE  7             TO  WRK-RECE05XX-YUKOKETA(1 1)
                 MOVE  "¼«¸ÊÉéÃ´¡¡¡¡³ä"
                                     TO  WRK-RECE05XX-NAME(1 1)
                 MOVE  WRK-GENBAKU-FTNRATE1
                                     TO  DAY-IDX2
                 IF    DAY-IDX2 = ZERO
                   MOVE  "¡¡£°"      TO  WRK-RECE05XX-NAME(1 1)(9:4)
                 ELSE
                   MOVE  TBL-DAY(DAY-IDX2)
                                     TO  WRK-RECE05XX-NAME(1 1)(9:4)
                 END-IF
               ELSE
                 MOVE  9             TO  WRK-RECE05XX-YUKOKETA(1 1)
                 MOVE  "¼«¸ÊÉéÃ´¡¡¡¡³ä¡¡Ê¬"
                                     TO  WRK-RECE05XX-NAME(1 1)
                 MOVE  WRK-GENBAKU-FTNRATE1
                                     TO  DAY-IDX2
                 IF    DAY-IDX2 = ZERO
                   MOVE  "¡¡£°"      TO  WRK-RECE05XX-NAME(1 1)(9:4)
                 ELSE
                   MOVE  TBL-DAY(DAY-IDX2)
                                     TO  WRK-RECE05XX-NAME(1 1)(9:4)
                 END-IF
                 MOVE  WRK-GENBAKU-FTNRATE2
                                     TO  DAY-IDX2
                 MOVE  TBL-DAY(DAY-IDX2)(3:2)
                                     TO  WRK-RECE05XX-NAME(1 1)(15:2)
               END-IF
      *
               MOVE    WRK-RECE05XX-SRYACT-TBL(1 1)
                                     TO  RECE059-SRYACT-TBL(1)
               WRITE   RECE059-REC
               ADD     1             TO  CNT-RECE059
      *
             END-IF
      *
           END-PERFORM
      *
           .
       2001-GENBAKU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñ¼õ¿ÇÆü¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-KOHJYUSIN-COMMENT-SEC          SECTION.
      *
           INITIALIZE                TBL-SANTEI-DAY
      *
           MOVE    ZERO          TO  DAY-IDX
           PERFORM    VARYING    IDX  FROM   1   BY   1
                      UNTIL     (IDX      >   31)  OR
                                (DAY-IDX  >   31)
                IF   RECE02-KOHJNISSUTBL(IDX)  NOT = SPACE
                      ADD   1     TO   DAY-IDX
                      MOVE  IDX   TO   TB-SANTEI-DAY(DAY-IDX)
                END-IF
           END-PERFORM
      *
           MOVE       ZERO       TO   IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           MOVE       5          TO   FLG-COMMENT
      *
           PERFORM  20011-FREE-COMMENT2-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                TO  IDX-RECE05
           MOVE    SPACE               TO  WRK-RECE05XX-REC
           INITIALIZE                      WRK-RECE05XX-REC
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                ADD    1     TO  IDX-RECE05
                MOVE   "810000001"
                             TO  WRK-RECE05XX-SRYCD(IDX-RECE05 1)
                MOVE   ZERO  TO  WRK-RECE05XX-SRYSURYO(IDX-RECE05 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
           END-PERFORM 
      *
      *    ¼õ¿ÇÆü¤ÎÊÔ½¸¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE05
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE05
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE05  =    1
                PERFORM 800-RECE02-SET059-SEC
                MOVE    99999980             TO  RECE059-ZAINUM
                MOVE    WRK-KOHBUNRI-JIDO    TO  RECE059-KOHBUNRI
                MOVE    "99"                 TO  RECE059-SRYKBN
                ADD     1                    TO  WRK-RENNUM
                MOVE    WRK-RENNUM           TO  RECE059-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                         UNTIL   (IDX       >    5)   OR
                                 (FLG-WRKRECE05   =   1)
                   ADD         1             TO    IDX-RECE05
                   IF (WRK-RECE05XX-SRYCD(IDX-RECE05 1) = SPACE)
                   OR (IDX-RECE05                       >  100 )
                       MOVE    1             TO    FLG-WRKRECE05
                   ELSE
                       MOVE  WRK-RECE05XX-SRYACT-TBL(IDX-RECE05 1)
                                        TO   RECE059-SRYACT-TBL(IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF  
                END-PERFORM
                IF   RECE059-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE059-REC
                   ADD     1            TO   CNT-RECE059
                END-IF
           END-PERFORM
      *
      *
           PERFORM 800-RECE02-SET049-SEC
           MOVE    99999980            TO  RECE049-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE049-KOHBUNRI
           MOVE    "99"                TO  RECE049-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE049-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE049-MEISAISU
      *
           WRITE   RECE049-REC
           ADD     1                   TO  CNT-RECE049
      *
           .
       2001-KOHJYUSIN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £³ºÐ»ù»ë³ÐÄ°³ÐÀºÌ©·ò¹¯¿Çºº¡ÊÃÏ°è¸øÈñ¡Ë¤ÎÅÀ¿ô¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-KOHTEN-COMMENT-SEC          SECTION.
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    SPACE               TO  WRK-RECE05XX-REC
           INITIALIZE                      WRK-RECE05XX-REC
      *
           MOVE    "810000001"         TO  WRK-RECE05XX-SRYCD(1 1)
           MOVE    ZERO                TO  WRK-RECE05XX-SRYSURYO(1 1)
      *
           MOVE    ZERO                TO  IDX31
           MOVE    RECE02-SEIKENTEN    TO  WRK-SEIKENTEN
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-SEIKENTEN       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF    ( KANACHK-RC      =   ZERO )
           AND   ( KANACHK-MAX     >   ZERO )
             PERFORM  VARYING   IDX   FROM   1   BY   2
                       UNTIL    IDX     >    KANACHK-MAX
               IF  KANACHK-OUT-INPUT(IDX:2)  NOT =  "¡¡"
                   MOVE   IDX          TO  IDX31
                   MOVE   KANACHK-MAX  TO  IDX
               END-IF
             END-PERFORM
             IF    IDX31  NOT = ZERO
               COMPUTE  IDX32  =  KANACHK-MAX - IDX31 + 1
               STRING  "£³ºÐ»ùÀºÌ©·ò¿Ç¡¡"  DELIMITED  BY  SIZE
                        KANACHK-OUT-INPUT(IDX31:IDX32)
                                           DELIMITED  BY  SIZE
                       "ÅÀ"                DELIMITED  BY  SIZE
                                           INTO  WRK-RECE05XX-NAME(1 1)
               END-STRING
             END-IF
           END-IF
           COMPUTE  WRK-RECE05XX-YUKOKETA(1 1) = 9 + (IDX32 / 2)
      *
      *    ¼õ¿ÇÆü¤ÎÊÔ½¸¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE05
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE05
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE05  =    1
                PERFORM 800-RECE02-SET059-SEC
                MOVE    99999970             TO  RECE059-ZAINUM
                MOVE    WRK-KOHBUNRI-JIDO    TO  RECE059-KOHBUNRI
                MOVE    "99"                 TO  RECE059-SRYKBN
                ADD     1                    TO  WRK-RENNUM
                MOVE    WRK-RENNUM           TO  RECE059-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                         UNTIL   (IDX       >    5)   OR
                                 (FLG-WRKRECE05   =   1)
                   ADD         1             TO    IDX-RECE05
                   IF (WRK-RECE05XX-SRYCD(IDX-RECE05 1) = SPACE)
                   OR (IDX-RECE05                       >  100 )
                       MOVE    1             TO    FLG-WRKRECE05
                   ELSE
                       MOVE  WRK-RECE05XX-SRYACT-TBL(IDX-RECE05 1)
                                        TO   RECE059-SRYACT-TBL(IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF  
                END-PERFORM
                IF   RECE059-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE059-REC
                   ADD     1            TO   CNT-RECE059
                END-IF
           END-PERFORM
      *
      *
           PERFORM 800-RECE02-SET049-SEC
           MOVE    99999970            TO  RECE049-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE049-KOHBUNRI
           MOVE    "99"                TO  RECE049-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE049-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE049-MEISAISU
      *
           WRITE   RECE049-REC
           ADD     1                   TO  CNT-RECE049
      *
           .
       2001-KOHTEN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Îà»öÏ·¿ÍÁë¸ýÄ§¼ý³Û¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-KOHSAIKEI-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE02-SET049-SEC
           MOVE    99999997            TO  RECE049-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE049-KOHBUNRI
           MOVE    "99"                TO  RECE049-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE049-HKNCOMBI
      *
           MOVE    1                   TO  RECE049-MEISAISU
      *
           WRITE   RECE049-REC
           ADD     1                   TO  CNT-RECE049
      *
           PERFORM 800-RECE02-SET059-SEC
           MOVE    99999997            TO  RECE059-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE059-KOHBUNRI
           MOVE    "99"                TO  RECE059-SRYKBN
      *    ¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE               TO  WRK-RECE05XX-REC
           INITIALIZE                      WRK-RECE05XX-REC
           MOVE    "810000001"         TO  WRK-RECE05XX-SRYCD(1 1)
           MOVE     ZERO               TO  WRK-RECE05XX-SRYSURYO(1 1)
           MOVE     10                 TO  WRK-RECE05XX-YUKOKETA(1 1)
           MOVE    "Îà»öÏ·¿ÍÁë¸ýÄ§¼ý³Û¡¡"
                                       TO  WRK-RECE05XX-NAME(1 1)
      *
           MOVE     ZERO               TO  IDA
           MOVE     ZERO               TO  IDB
           MOVE     ZERO               TO  FLG-HENSYUKIN
           MOVE     RECE02-KOH-SAIKEI  TO  WRK-HENSYU-KINGAKU
           MOVE     SPACE              TO  WRK-HENSYU-KINGAKU2
           MOVE     SPACE              TO  WRK-HENSYU-KINGAKU3
      *
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-HENSYU-KINGAKU  TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      KANACHK-RC      =   ZERO
              MOVE  KANACHK-OUT-INPUT(1:18)
                                       TO  WRK-HENSYU-KINGAKU2
           END-IF
           PERFORM   VARYING   IDA   FROM   1   BY   2
                      UNTIL    IDA    >    18
              IF   WRK-HENSYU-KINGAKU2(IDA:2)  =  "£°"
                    IF  (FLG-HENSYUKIN = ZERO)
                    AND (IDA       NOT =  17 )
                       CONTINUE
                    ELSE
                       MOVE  1   TO  FLG-HENSYUKIN
                       ADD   1   TO  IDB
                       MOVE  WRK-HENSYU-KINGAKU2(IDA:2)  TO
                                 WRK-HENSYU-KINGAKU3(IDB:2)
                       ADD   1   TO  IDB
                    END-IF
              ELSE
                    MOVE  1   TO  FLG-HENSYUKIN
                    ADD   1   TO  IDB
                    MOVE  WRK-HENSYU-KINGAKU2(IDA:2)  TO
                              WRK-HENSYU-KINGAKU3(IDB:2)
                    ADD   1   TO  IDB
              END-IF
           END-PERFORM
      *
           COMPUTE  IDA  =  ( IDB  /  2 )  +  1
           COMPUTE  WRK-RECE05XX-YUKOKETA(1 1)  =
                    WRK-RECE05XX-YUKOKETA(1 1)  +  IDA
      *
           MOVE     WRK-HENSYU-KINGAKU3(1:IDB)
                                   TO  WRK-RECE05XX-NAME(1 1)(21:IDB)
           COMPUTE  IDA  =  21  +  IDB
           MOVE    "±ß"            TO  WRK-RECE05XX-NAME(1 1)(IDA:2)
      *
           MOVE    "1"             TO  WRK-RECE05XX-RECEPTFLG(1 1)
      *
           MOVE    WRK-RECE05XX-SRYACT-TBL(1 1)
                                   TO  RECE059-SRYACT-TBL(1)
           WRITE   RECE059-REC
           ADD     1               TO  CNT-RECE059
      *
           .
       2001-KOHSAIKEI-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ç¥ÉØ¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-NINPU-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE02-SET049-SEC
           MOVE    99999960            TO  RECE049-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE049-KOHBUNRI
           MOVE    "99"                TO  RECE049-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE049-HKNCOMBI
           MOVE    1                   TO  RECE049-MEISAISU
           WRITE   RECE049-REC
           ADD     1                   TO  CNT-RECE049
      *
           PERFORM 800-RECE02-SET059-SEC
           MOVE    99999960            TO  RECE059-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE059-KOHBUNRI
           MOVE    "99"                TO  RECE059-SRYKBN
      *    ¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE               TO  WRK-RECE05XX-REC
           INITIALIZE                      WRK-RECE05XX-REC
      *    ¡ÖÇ¥ÉØ¡×
           MOVE    "820100348"         TO  WRK-TENSU-SRYCD
           MOVE    RECE02-SRYYM        TO  WRK-TENSU-SRYYM
           PERFORM 900-TENSU-TBL-SEC
      *
           MOVE    TNS-SRYCD           TO  WRK-RECE05XX-SRYCD(1 1)
           MOVE    ZERO                TO  WRK-RECE05XX-SRYSURYO(1 1)
           MOVE    TNS-YUKOKETA        TO  WRK-RECE05XX-YUKOKETA(1 1)
           MOVE    TNS-NAME            TO  WRK-RECE05XX-NAME(1 1)
                                           WRK-RECE05XX-INPUTCOMENT(1 1)
           MOVE    WRK-RECE05XX-SRYACT-TBL(1 1)
                                        TO  RECE059-SRYACT-TBL(1)
           WRITE   RECE059-REC
           ADD     1                    TO  CNT-RECE059
      *
           .
       2001-NINPU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    È¯¾ÉÆü¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-HASSYO-HANTEI-SEC          SECTION.
      *
           PERFORM     2002-RIHA-HENSYU-SEC
      *
           IF   TBL-HASSYO-KAITATU-FLG   =   1
             MOVE      ZERO            TO   FLG-COMOKIKAE
             MOVE      CONS-KYUSEI     TO   WRK-SRYCD
             PERFORM   2001-KYUSEIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-SINDAI-FLG   =   1 OR 3
             PERFORM   VARYING   IDX14    FROM   1   BY   1
                        UNTIL    IDX14      >    5
               IF   TBL-HASSYO-SINDAI-COMMENT(IDX14)   =  1
                 MOVE      ZERO            TO   FLG-COMOKIKAE
                 MOVE      CONS-SINDAI     TO   WRK-SRYCD
                 MOVE      CONS-SINDAI-E   TO   WRK-SRYCD2
                 MOVE      TBL-HASSYO-SINDAI-DAY-T2(IDX14)
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
                 MOVE      TBL-HASSYO-SINYMD1-DAY-T2(IDX14)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2001X-HASSYO-HENSYU-SEC
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      ***
           IF   TBL-HASSYO-KOKYUKI-FLG   =   1 OR 3
             PERFORM   VARYING   IDX14    FROM   1   BY   1
                        UNTIL    IDX14      >    5
               IF   TBL-HASSYO-KOKYUKI-COMMENT(IDX14)   =  1
                 MOVE      ZERO            TO   FLG-COMOKIKAE
                 MOVE      CONS-KOKYUKI    TO   WRK-SRYCD
                 MOVE      CONS-KOKYUKI-E  TO   WRK-SRYCD2
                 MOVE      TBL-HASSYO-KOKYUKI-DAY-T2(IDX14)
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
                 MOVE      TBL-HASSYO-KOKYMD1-DAY-T2(IDX14)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2001X-HASSYO-HENSYU-SEC
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      ***
           IF   TBL-HASSYO-NOUKE-FLG   =   1 OR 3
             PERFORM   VARYING   IDX14    FROM   1   BY   1
                        UNTIL    IDX14      >    5
               IF   TBL-HASSYO-NOUKE-COMMENT(IDX14)   =  1
                 MOVE      1               TO   FLG-COMOKIKAE
                 MOVE      CONS-NOUKE      TO   WRK-SRYCD
                 MOVE      CONS-NOUKE-E    TO   WRK-SRYCD2
                 MOVE      TBL-HASSYO-NOUKE-DAY-T2(IDX14)
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
                 MOVE      TBL-HASSYO-NOUYMD1-DAY-T2(IDX14)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2001X-HASSYO-HENSYU-SEC
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      ***
           IF   TBL-HASSYO-UNDOKI-FLG   =   1 OR 3
             PERFORM   VARYING   IDX14    FROM   1   BY   1
                        UNTIL    IDX14      >    5
               IF   TBL-HASSYO-UNDOKI-COMMENT(IDX14)   =  1
                 MOVE      1               TO   FLG-COMOKIKAE
                 MOVE      CONS-UNDOKI     TO   WRK-SRYCD
                 MOVE      CONS-UNDOKI-E   TO   WRK-SRYCD2
                 MOVE      TBL-HASSYO-UNDOKI-DAY-T2(IDX14)
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
                 MOVE      TBL-HASSYO-UNDYMD1-DAY-T2(IDX14)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2001X-HASSYO-HENSYU-SEC
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      ***
           IF   TBL-HASSYO-NANBYO-FLG   =   1 OR 3
             PERFORM   VARYING   IDX14    FROM   1   BY   1
                        UNTIL    IDX14      >    5
               IF   TBL-HASSYO-NANBYO-COMMENT(IDX14)   =  1
                 MOVE      1               TO   FLG-COMOKIKAE
                 MOVE      CONS-NANBYO     TO   WRK-SRYCD
                 MOVE      CONS-NANBYO-E   TO   WRK-SRYCD2
                 MOVE      TBL-HASSYO-NANBYO-DAY-T2(IDX14)
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
                 MOVE      TBL-HASSYO-NANYMD1-DAY-T2(IDX14)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2001X-HASSYO-HENSYU-SEC
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      ***
           IF   TBL-HASSYO-SYOGAI-FLG   =   1 OR 3
             PERFORM   VARYING   IDX14    FROM   1   BY   1
                        UNTIL    IDX14      >    5
               IF   TBL-HASSYO-SYOGAI-COMMENT(IDX14)   =  1
                 MOVE      ZERO            TO   FLG-COMOKIKAE
                 MOVE      CONS-SYOGAI     TO   WRK-SRYCD
                 MOVE      CONS-SYOGAI-E   TO   WRK-SRYCD2
                 MOVE      TBL-HASSYO-SYOGAI-DAY-T2(IDX14)
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
                 MOVE      TBL-HASSYO-SYOYMD1-DAY-T2(IDX14)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2001X-HASSYO-HENSYU-SEC
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      ***
           IF   TBL-HASSYO-SESSYOKU-FLG   =   1 OR 3
             PERFORM   VARYING   IDX14    FROM   1   BY   1
                        UNTIL    IDX14      >    5
               IF   TBL-HASSYO-SESSYOKU-COMMENT(IDX14)   =  1
                 MOVE      ZERO            TO   FLG-COMOKIKAE
                 MOVE      CONS-SESSYOKU   TO   WRK-SRYCD
                 MOVE      CONS-SESSYOKU-E TO   WRK-SRYCD2
                 MOVE      TBL-HASSYO-SESSYOKU-DAY-T2(IDX14)
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
                 MOVE      TBL-HASSYO-SESYMD1-DAY-T2(IDX14)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2001X-HASSYO-HENSYU-SEC
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      ***
           IF   TBL-HASSYO-HAIYO-FLG   =   1 OR 3
             PERFORM   VARYING   IDX14    FROM   1   BY   1
                        UNTIL    IDX14      >    5
               IF   TBL-HASSYO-HAIYO-COMMENT(IDX14)   =  1
                 MOVE      1               TO   FLG-COMOKIKAE
                 MOVE      CONS-HAIYO      TO   WRK-SRYCD
                 MOVE      CONS-HAIYO-E    TO   WRK-SRYCD2
                 MOVE      TBL-HASSYO-HAIYO-DAY-T2(IDX14)
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
                 MOVE      TBL-HASSYO-HAIYMD1-DAY-T2(IDX14)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2001X-HASSYO-HENSYU-SEC
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      *####
           IF   TBL-HASSYO9-SINDAI-FLG   =   1 OR 3
             PERFORM   VARYING   IDX14    FROM   1   BY   1
                        UNTIL    IDX14      >    5
               IF   TBL-HASSYO9-SINDAI-COMMENT(IDX14)   =  1
                 MOVE      1               TO   FLG-COMOKIKAE
                 MOVE      CONS-SINDAI-K   TO   WRK-SRYCD
                 MOVE      "999999999"     TO   WRK-SRYCD2
                 MOVE      TBL-HASSYO9-SINDAI-DAY-T2(IDX14)
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
                 MOVE      TBL-HASSYO9-SINYMD1-DAY-T2(IDX14)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2001X-HASSYO-HENSYU-SEC
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      ***
           IF   TBL-HASSYO9-KOKYUKI-FLG   =   1 OR 3
             PERFORM   VARYING   IDX14    FROM   1   BY   1
                        UNTIL    IDX14      >    5
               IF   TBL-HASSYO9-KOKYUKI-COMMENT(IDX14)   =  1
                 MOVE      1               TO   FLG-COMOKIKAE
                 MOVE      CONS-KOKYUKI-K  TO   WRK-SRYCD
                 MOVE      "999999999"     TO   WRK-SRYCD2
                 MOVE      TBL-HASSYO9-KOKYUKI-DAY-T2(IDX14)
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
                 MOVE      TBL-HASSYO9-KOKYMD1-DAY-T2(IDX14)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2001X-HASSYO-HENSYU-SEC
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      ***
           IF   TBL-HASSYO9-HAIYO-FLG   =   1 OR 3
             PERFORM   VARYING   IDX14    FROM   1   BY   1
                        UNTIL    IDX14      >    5
               IF   TBL-HASSYO9-HAIYO-COMMENT(IDX14)   =  1
                 MOVE      1               TO   FLG-COMOKIKAE
                 MOVE      CONS-HAIYO-K    TO   WRK-SRYCD
                 MOVE      "999999999"     TO   WRK-SRYCD2
                 MOVE      TBL-HASSYO9-HAIYO-DAY-T2(IDX14)
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
                 MOVE      TBL-HASSYO9-HAIYMD1-DAY-T2(IDX14)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2001X-HASSYO-HENSYU-SEC
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      *
           PERFORM  2002-DAISAN-HASSYO-HANTEI-SEC
      *
           .
       2001-HASSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ê¥Ï¥Ó¥êÊÔ½¸
      *****************************************************************
       2002-RIHA-HENSYU-SEC             SECTION.
      *
           IF   TBL-HASSYO-SINDAI-FLG   =   9
             MOVE    ZERO       TO  FLG-DAISAN1
             MOVE    ZERO       TO  FLG-DAISAN2
             PERFORM  VARYING  IDX22  FROM  1  BY  1
                       UNTIL  (IDX22   >   10                )
                        OR    (RECE02-HKNCOMBI(IDX22) =  ZERO)
               IF  RECE02-DAISANFLG(IDX22) =  1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE02-RIHA-SINDAI(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-SINDAI2-SANTEI
                                                 (IDX22 IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-SINDAI2-COMMENT
                                                      (IDX22)
                   MOVE  RECE02-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI2(IDX22)
                   MOVE    1  TO  FLG-DAISAN1
                 END-IF
                 IF  RECE02-RIHA2-SINDAI(IDX22)    =  1
                 AND TBL-HASSYO-SINYMD2-FLG(IDX22) = ZERO
                   MOVE  RECE02-RIHA2-SINDAI(IDX22)
                              TO  TBL-HASSYO-SINYMD2-FLG(IDX22)
                   MOVE  RECE02-RIHA2-SINYMD-OCC1(IDX22)
                              TO  TBL-HASSYO-SINYMD2-DAY-OCC1(IDX22)
                   MOVE  RECE02-RIHA2-SINYMD-OCC2(IDX22)
                              TO  TBL-HASSYO-SINYMD2-DAY-OCC2(IDX22)
                 END-IF
               ELSE
                 COMPUTE WRK-KOHBUNRI = RECE02-RIHA-KOHBUNRI(IDX22)
                                      + 1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE02-RIHA-SINDAI(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-SINDAI-SANTEI
                                                (WRK-KOHBUNRI IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-SINDAI-COMMENT
                                                     (WRK-KOHBUNRI)
                   MOVE  RECE02-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                   MOVE    1  TO  FLG-DAISAN2
                 END-IF
                 IF  RECE02-RIHA2-SINDAI(IDX22)           =  1
                 AND TBL-HASSYO-SINYMD1-FLG(WRK-KOHBUNRI) = ZERO
                   MOVE  RECE02-RIHA2-SINDAI(IDX22)
                           TO  TBL-HASSYO-SINYMD1-FLG(WRK-KOHBUNRI)
                   MOVE  RECE02-RIHA2-SINYMD-OCC1(IDX22)
                           TO  TBL-HASSYO-SINYMD1-DAY-OCC1(WRK-KOHBUNRI)
                   MOVE  RECE02-RIHA2-SINYMD-OCC2(IDX22)
                           TO  TBL-HASSYO-SINYMD1-DAY-OCC2(WRK-KOHBUNRI)
                 END-IF
               END-IF
             END-PERFORM
             EVALUATE   FLG-DAISAN1   ALSO   FLG-DAISAN2
               WHEN         1         ALSO       1
                 MOVE    3    TO    TBL-HASSYO-SINDAI-FLG
               WHEN         1         ALSO       0
                 MOVE    2    TO    TBL-HASSYO-SINDAI-FLG
               WHEN         0         ALSO       1
                 MOVE    1    TO    TBL-HASSYO-SINDAI-FLG
             END-EVALUATE
           END-IF
      *
           IF   TBL-HASSYO-KOKYUKI-FLG   =   9
             MOVE    ZERO       TO  FLG-DAISAN1
             MOVE    ZERO       TO  FLG-DAISAN2
             PERFORM  VARYING  IDX22  FROM  1  BY  1
                       UNTIL  (IDX22   >   10                )
                        OR    (RECE02-HKNCOMBI(IDX22) =  ZERO)
               IF  RECE02-DAISANFLG(IDX22) =  1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE02-RIHA-KOKYUKI(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-KOKYUKI2-SANTEI
                                                 (IDX22 IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-KOKYUKI2-COMMENT
                                                      (IDX22)
                   MOVE  RECE02-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI2(IDX22)
                   MOVE    1  TO  FLG-DAISAN1
                 END-IF
                 IF  RECE02-RIHA2-KOKYUKI(IDX22)   =  1
                 AND TBL-HASSYO-KOKYMD2-FLG(IDX22) = ZERO
                   MOVE  RECE02-RIHA2-KOKYUKI(IDX22)
                              TO  TBL-HASSYO-KOKYMD2-FLG(IDX22)
                   MOVE  RECE02-RIHA2-KOKYMD-OCC1(IDX22)
                              TO  TBL-HASSYO-KOKYMD2-DAY-OCC1(IDX22)
                   MOVE  RECE02-RIHA2-KOKYMD-OCC2(IDX22)
                              TO  TBL-HASSYO-KOKYMD2-DAY-OCC2(IDX22)
                 END-IF
               ELSE
                 COMPUTE WRK-KOHBUNRI = RECE02-RIHA-KOHBUNRI(IDX22)
                                      + 1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE02-RIHA-KOKYUKI(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-KOKYUKI-SANTEI
                                                (WRK-KOHBUNRI IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-KOKYUKI-COMMENT
                                                     (WRK-KOHBUNRI)
                   MOVE  RECE02-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                   MOVE    1  TO  FLG-DAISAN2
                 END-IF
                 IF  RECE02-RIHA2-KOKYUKI(IDX22)          =  1
                 AND TBL-HASSYO-KOKYMD1-FLG(WRK-KOHBUNRI) = ZERO
                   MOVE  RECE02-RIHA2-KOKYUKI(IDX22)
                           TO  TBL-HASSYO-KOKYMD1-FLG(WRK-KOHBUNRI)
                   MOVE  RECE02-RIHA2-KOKYMD-OCC1(IDX22)
                           TO  TBL-HASSYO-KOKYMD1-DAY-OCC1(WRK-KOHBUNRI)
                   MOVE  RECE02-RIHA2-KOKYMD-OCC2(IDX22)
                           TO  TBL-HASSYO-KOKYMD1-DAY-OCC2(WRK-KOHBUNRI)
                 END-IF
               END-IF
             END-PERFORM
             EVALUATE   FLG-DAISAN1   ALSO   FLG-DAISAN2
               WHEN         1         ALSO       1
                 MOVE    3    TO    TBL-HASSYO-KOKYUKI-FLG
               WHEN         1         ALSO       0
                 MOVE    2    TO    TBL-HASSYO-KOKYUKI-FLG
               WHEN         0         ALSO       1
                 MOVE    1    TO    TBL-HASSYO-KOKYUKI-FLG
             END-EVALUATE
           END-IF
      *
           IF   TBL-HASSYO-NOUKE-FLG   =   9
             MOVE    ZERO       TO  FLG-DAISAN1
             MOVE    ZERO       TO  FLG-DAISAN2
             PERFORM  VARYING  IDX22  FROM  1  BY  1
                       UNTIL  (IDX22   >   10                )
                        OR    (RECE02-HKNCOMBI(IDX22) =  ZERO)
               IF  RECE02-DAISANFLG(IDX22) =  1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE02-RIHA-NOUKE(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-NOUKE2-SANTEI
                                                 (IDX22 IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-NOUKE2-COMMENT
                                                      (IDX22)
                   MOVE  RECE02-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI2(IDX22)
                   MOVE    1  TO  FLG-DAISAN1
                 END-IF
                 IF  RECE02-RIHA2-NOUKE(IDX22)     =  1
                 AND TBL-HASSYO-NOUYMD2-FLG(IDX22) = ZERO
                   MOVE  RECE02-RIHA2-NOUKE(IDX22)
                              TO  TBL-HASSYO-NOUYMD2-FLG(IDX22)
                   MOVE  RECE02-RIHA2-NOUYMD-OCC1(IDX22)
                              TO  TBL-HASSYO-NOUYMD2-DAY-OCC1(IDX22)
                   MOVE  RECE02-RIHA2-NOUYMD-OCC2(IDX22)
                              TO  TBL-HASSYO-NOUYMD2-DAY-OCC2(IDX22)
                 END-IF
               ELSE
                 COMPUTE WRK-KOHBUNRI = RECE02-RIHA-KOHBUNRI(IDX22)
                                      + 1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE02-RIHA-NOUKE(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-NOUKE-SANTEI
                                                (WRK-KOHBUNRI IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-NOUKE-COMMENT
                                                     (WRK-KOHBUNRI)
                   MOVE  RECE02-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                   MOVE    1  TO  FLG-DAISAN2
                 END-IF
                 IF  RECE02-RIHA2-NOUKE(IDX22)            =  1
                 AND TBL-HASSYO-NOUYMD1-FLG(WRK-KOHBUNRI) = ZERO
                   MOVE  RECE02-RIHA2-NOUKE(IDX22)
                           TO  TBL-HASSYO-NOUYMD1-FLG(WRK-KOHBUNRI)
                   MOVE  RECE02-RIHA2-NOUYMD-OCC1(IDX22)
                           TO  TBL-HASSYO-NOUYMD1-DAY-OCC1(WRK-KOHBUNRI)
                   MOVE  RECE02-RIHA2-NOUYMD-OCC2(IDX22)
                           TO  TBL-HASSYO-NOUYMD1-DAY-OCC2(WRK-KOHBUNRI)
                 END-IF
               END-IF
             END-PERFORM
             EVALUATE   FLG-DAISAN1   ALSO   FLG-DAISAN2
               WHEN         1         ALSO       1
                 MOVE    3    TO    TBL-HASSYO-NOUKE-FLG
               WHEN         1         ALSO       0
                 MOVE    2    TO    TBL-HASSYO-NOUKE-FLG
               WHEN         0         ALSO       1
                 MOVE    1    TO    TBL-HASSYO-NOUKE-FLG
             END-EVALUATE
           END-IF
      *
           IF   TBL-HASSYO-UNDOKI-FLG   =   9
             MOVE    ZERO       TO  FLG-DAISAN1
             MOVE    ZERO       TO  FLG-DAISAN2
             PERFORM  VARYING  IDX22  FROM  1  BY  1
                       UNTIL  (IDX22   >   10                )
                        OR    (RECE02-HKNCOMBI(IDX22) =  ZERO)
               IF  RECE02-DAISANFLG(IDX22) =  1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE02-RIHA-UNDOKI(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-UNDOKI2-SANTEI
                                                 (IDX22 IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-UNDOKI2-COMMENT
                                                      (IDX22)
                   MOVE  RECE02-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI2(IDX22)
                   MOVE    1  TO  FLG-DAISAN1
                 END-IF
                 IF  RECE02-RIHA2-UNDOKI(IDX22)    =  1
                 AND TBL-HASSYO-UNDYMD2-FLG(IDX22) = ZERO
                   MOVE  RECE02-RIHA2-UNDOKI(IDX22)
                              TO  TBL-HASSYO-UNDYMD2-FLG(IDX22)
                   MOVE  RECE02-RIHA2-UNDYMD-OCC1(IDX22)
                              TO  TBL-HASSYO-UNDYMD2-DAY-OCC1(IDX22)
                   MOVE  RECE02-RIHA2-UNDYMD-OCC2(IDX22)
                              TO  TBL-HASSYO-UNDYMD2-DAY-OCC2(IDX22)
                 END-IF
               ELSE
                 COMPUTE WRK-KOHBUNRI = RECE02-RIHA-KOHBUNRI(IDX22)
                                      + 1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE02-RIHA-UNDOKI(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-UNDOKI-SANTEI
                                                (WRK-KOHBUNRI IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-UNDOKI-COMMENT
                                                     (WRK-KOHBUNRI)
                   MOVE  RECE02-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                   MOVE    1  TO  FLG-DAISAN2
                 END-IF
                 IF  RECE02-RIHA2-UNDOKI(IDX22)           =  1
                 AND TBL-HASSYO-UNDYMD1-FLG(WRK-KOHBUNRI) = ZERO
                   MOVE  RECE02-RIHA2-UNDOKI(IDX22)
                           TO  TBL-HASSYO-UNDYMD1-FLG(WRK-KOHBUNRI)
                   MOVE  RECE02-RIHA2-UNDYMD-OCC1(IDX22)
                           TO  TBL-HASSYO-UNDYMD1-DAY-OCC1(WRK-KOHBUNRI)
                   MOVE  RECE02-RIHA2-UNDYMD-OCC2(IDX22)
                           TO  TBL-HASSYO-UNDYMD1-DAY-OCC2(WRK-KOHBUNRI)
                 END-IF
               END-IF
             END-PERFORM
             EVALUATE   FLG-DAISAN1   ALSO   FLG-DAISAN2
               WHEN         1         ALSO       1
                 MOVE    3    TO    TBL-HASSYO-UNDOKI-FLG
               WHEN         1         ALSO       0
                 MOVE    2    TO    TBL-HASSYO-UNDOKI-FLG
               WHEN         0         ALSO       1
                 MOVE    1    TO    TBL-HASSYO-UNDOKI-FLG
             END-EVALUATE
           END-IF
      *
           IF   TBL-HASSYO-NANBYO-FLG   =   9
             MOVE    ZERO       TO  FLG-DAISAN1
             MOVE    ZERO       TO  FLG-DAISAN2
             PERFORM  VARYING  IDX22  FROM  1  BY  1
                       UNTIL  (IDX22   >   10                )
                        OR    (RECE02-HKNCOMBI(IDX22) =  ZERO)
               IF  RECE02-DAISANFLG(IDX22) =  1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE02-RIHA-NANBYO(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-NANBYO2-SANTEI
                                                 (IDX22 IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-NANBYO2-COMMENT
                                                      (IDX22)
                   MOVE  RECE02-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI2(IDX22)
                   MOVE    1  TO  FLG-DAISAN1
                 END-IF
                 IF  RECE02-RIHA2-NANBYO(IDX22)    =  1
                 AND TBL-HASSYO-NANYMD2-FLG(IDX22) = ZERO
                   MOVE  RECE02-RIHA2-NANBYO(IDX22)
                              TO  TBL-HASSYO-NANYMD2-FLG(IDX22)
                   MOVE  RECE02-RIHA2-NANYMD-OCC1(IDX22)
                              TO  TBL-HASSYO-NANYMD2-DAY-OCC1(IDX22)
                   MOVE  RECE02-RIHA2-NANYMD-OCC2(IDX22)
                              TO  TBL-HASSYO-NANYMD2-DAY-OCC2(IDX22)
                 END-IF
               ELSE
                 COMPUTE WRK-KOHBUNRI = RECE02-RIHA-KOHBUNRI(IDX22)
                                      + 1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE02-RIHA-NANBYO(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-NANBYO-SANTEI
                                                (WRK-KOHBUNRI IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-NANBYO-COMMENT
                                                     (WRK-KOHBUNRI)
                   MOVE  RECE02-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                   MOVE    1  TO  FLG-DAISAN2
                 END-IF
                 IF  RECE02-RIHA2-NANBYO(IDX22)           =  1
                 AND TBL-HASSYO-NANYMD1-FLG(WRK-KOHBUNRI) = ZERO
                   MOVE  RECE02-RIHA2-NANBYO(IDX22)
                           TO  TBL-HASSYO-NANYMD1-FLG(WRK-KOHBUNRI)
                   MOVE  RECE02-RIHA2-NANYMD-OCC1(IDX22)
                           TO  TBL-HASSYO-NANYMD1-DAY-OCC1(WRK-KOHBUNRI)
                   MOVE  RECE02-RIHA2-NANYMD-OCC2(IDX22)
                           TO  TBL-HASSYO-NANYMD1-DAY-OCC2(WRK-KOHBUNRI)
                 END-IF
               END-IF
             END-PERFORM
             EVALUATE   FLG-DAISAN1   ALSO   FLG-DAISAN2
               WHEN         1         ALSO       1
                 MOVE    3    TO    TBL-HASSYO-NANBYO-FLG
               WHEN         1         ALSO       0
                 MOVE    2    TO    TBL-HASSYO-NANBYO-FLG
               WHEN         0         ALSO       1
                 MOVE    1    TO    TBL-HASSYO-NANBYO-FLG
             END-EVALUATE
           END-IF
      *
           IF   TBL-HASSYO-SYOGAI-FLG   =   9
             MOVE    ZERO       TO  FLG-DAISAN1
             MOVE    ZERO       TO  FLG-DAISAN2
             PERFORM  VARYING  IDX22  FROM  1  BY  1
                       UNTIL  (IDX22   >   10                )
                        OR    (RECE02-HKNCOMBI(IDX22) =  ZERO)
               IF  RECE02-DAISANFLG(IDX22) =  1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE02-RIHA-SYOGAI(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-SYOGAI2-SANTEI
                                                 (IDX22 IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-SYOGAI2-COMMENT
                                                      (IDX22)
                   MOVE  RECE02-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI2(IDX22)
                   MOVE    1  TO  FLG-DAISAN1
                 END-IF
                 IF  RECE02-RIHA2-SYOGAI(IDX22)    =  1
                 AND TBL-HASSYO-SYOYMD2-FLG(IDX22) = ZERO
                   MOVE  RECE02-RIHA2-SYOGAI(IDX22)
                              TO  TBL-HASSYO-SYOYMD2-FLG(IDX22)
                   MOVE  RECE02-RIHA2-SYOYMD-OCC1(IDX22)
                              TO  TBL-HASSYO-SYOYMD2-DAY-OCC1(IDX22)
                   MOVE  RECE02-RIHA2-SYOYMD-OCC2(IDX22)
                              TO  TBL-HASSYO-SYOYMD2-DAY-OCC2(IDX22)
                 END-IF
               ELSE
                 COMPUTE WRK-KOHBUNRI = RECE02-RIHA-KOHBUNRI(IDX22)
                                      + 1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE02-RIHA-SYOGAI(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-SYOGAI-SANTEI
                                                (WRK-KOHBUNRI IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-SYOGAI-COMMENT
                                                     (WRK-KOHBUNRI)
                   MOVE  RECE02-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                   MOVE    1  TO  FLG-DAISAN2
                 END-IF
                 IF  RECE02-RIHA2-SYOGAI(IDX22)           =  1
                 AND TBL-HASSYO-SYOYMD1-FLG(WRK-KOHBUNRI) = ZERO
                   MOVE  RECE02-RIHA2-SYOGAI(IDX22)
                           TO  TBL-HASSYO-SYOYMD1-FLG(WRK-KOHBUNRI)
                   MOVE  RECE02-RIHA2-SYOYMD-OCC1(IDX22)
                           TO  TBL-HASSYO-SYOYMD1-DAY-OCC1(WRK-KOHBUNRI)
                   MOVE  RECE02-RIHA2-SYOYMD-OCC2(IDX22)
                           TO  TBL-HASSYO-SYOYMD1-DAY-OCC2(WRK-KOHBUNRI)
                 END-IF
               END-IF
             END-PERFORM
             EVALUATE   FLG-DAISAN1   ALSO   FLG-DAISAN2
               WHEN         1         ALSO       1
                 MOVE    3    TO    TBL-HASSYO-SYOGAI-FLG
               WHEN         1         ALSO       0
                 MOVE    2    TO    TBL-HASSYO-SYOGAI-FLG
               WHEN         0         ALSO       1
                 MOVE    1    TO    TBL-HASSYO-SYOGAI-FLG
             END-EVALUATE
           END-IF
      *
           IF   TBL-HASSYO-SESSYOKU-FLG   =   9
             MOVE    ZERO       TO  FLG-DAISAN1
             MOVE    ZERO       TO  FLG-DAISAN2
             PERFORM  VARYING  IDX22  FROM  1  BY  1
                       UNTIL  (IDX22   >   10                )
                        OR    (RECE02-HKNCOMBI(IDX22) =  ZERO)
               IF  RECE02-DAISANFLG(IDX22) =  1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE02-RIHA-SESSYOKU(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-SESSYOKU2-SANTEI
                                                 (IDX22 IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-SESSYOKU2-COMMENT
                                                      (IDX22)
                   MOVE  RECE02-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI2(IDX22)
                   MOVE    1  TO  FLG-DAISAN1
                 END-IF
                 IF  RECE02-RIHA2-SESSYOKU(IDX22)  =  1
                 AND TBL-HASSYO-SESYMD2-FLG(IDX22) = ZERO
                   MOVE  RECE02-RIHA2-SESSYOKU(IDX22)
                              TO  TBL-HASSYO-SESYMD2-FLG(IDX22)
                   MOVE  RECE02-RIHA2-SESYMD-OCC1(IDX22)
                              TO  TBL-HASSYO-SESYMD2-DAY-OCC1(IDX22)
                   MOVE  RECE02-RIHA2-SESYMD-OCC2(IDX22)
                              TO  TBL-HASSYO-SESYMD2-DAY-OCC2(IDX22)
                 END-IF
               ELSE
                 COMPUTE WRK-KOHBUNRI = RECE02-RIHA-KOHBUNRI(IDX22)
                                      + 1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE02-RIHA-SESSYOKU(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-SESSYOKU-SANTEI
                                                (WRK-KOHBUNRI IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-SESSYOKU-COMMENT
                                                     (WRK-KOHBUNRI)
                   MOVE  RECE02-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                   MOVE    1  TO  FLG-DAISAN2
                 END-IF
                 IF  RECE02-RIHA2-SESSYOKU(IDX22)         =  1
                 AND TBL-HASSYO-SESYMD1-FLG(WRK-KOHBUNRI) = ZERO
                   MOVE  RECE02-RIHA2-SESSYOKU(IDX22)
                           TO  TBL-HASSYO-SESYMD1-FLG(WRK-KOHBUNRI)
                   MOVE  RECE02-RIHA2-SESYMD-OCC1(IDX22)
                           TO  TBL-HASSYO-SESYMD1-DAY-OCC1(WRK-KOHBUNRI)
                   MOVE  RECE02-RIHA2-SESYMD-OCC2(IDX22)
                           TO  TBL-HASSYO-SESYMD1-DAY-OCC2(WRK-KOHBUNRI)
                 END-IF
               END-IF
             END-PERFORM
             EVALUATE   FLG-DAISAN1   ALSO   FLG-DAISAN2
               WHEN         1         ALSO       1
                 MOVE    3    TO    TBL-HASSYO-SESSYOKU-FLG
               WHEN         1         ALSO       0
                 MOVE    2    TO    TBL-HASSYO-SESSYOKU-FLG
               WHEN         0         ALSO       1
                 MOVE    1    TO    TBL-HASSYO-SESSYOKU-FLG
             END-EVALUATE
           END-IF
      *
           IF   TBL-HASSYO-HAIYO-FLG   =   9
             MOVE    ZERO       TO  FLG-DAISAN1
             MOVE    ZERO       TO  FLG-DAISAN2
             PERFORM  VARYING  IDX22  FROM  1  BY  1
                       UNTIL  (IDX22   >   10                )
                        OR    (RECE02-HKNCOMBI(IDX22) =  ZERO)
               IF  RECE02-DAISANFLG(IDX22) =  1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE02-RIHA-HAIYO(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-HAIYO2-SANTEI
                                                 (IDX22 IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-HAIYO2-COMMENT
                                                      (IDX22)
                   MOVE  RECE02-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI2(IDX22)
                   MOVE    1  TO  FLG-DAISAN1
                 END-IF
                 IF  RECE02-RIHA2-HAIYO(IDX22)     =  1
                 AND TBL-HASSYO-HAIYMD2-FLG(IDX22) = ZERO
                   MOVE  RECE02-RIHA2-HAIYO(IDX22)
                              TO  TBL-HASSYO-HAIYMD2-FLG(IDX22)
                   MOVE  RECE02-RIHA2-HAIYMD-OCC1(IDX22)
                              TO  TBL-HASSYO-HAIYMD2-DAY-OCC1(IDX22)
                   MOVE  RECE02-RIHA2-HAIYMD-OCC2(IDX22)
                              TO  TBL-HASSYO-HAIYMD2-DAY-OCC2(IDX22)
                 END-IF
               ELSE
                 COMPUTE WRK-KOHBUNRI = RECE02-RIHA-KOHBUNRI(IDX22)
                                      + 1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE02-RIHA-HAIYO(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-HAIYO-SANTEI
                                                (WRK-KOHBUNRI IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-HAIYO-COMMENT
                                                     (WRK-KOHBUNRI)
                   MOVE  RECE02-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                   MOVE    1  TO  FLG-DAISAN2
                 END-IF
                 IF  RECE02-RIHA2-HAIYO(IDX22)            =  1
                 AND TBL-HASSYO-HAIYMD1-FLG(WRK-KOHBUNRI) = ZERO
                   MOVE  RECE02-RIHA2-HAIYO(IDX22)
                           TO  TBL-HASSYO-HAIYMD1-FLG(WRK-KOHBUNRI)
                   MOVE  RECE02-RIHA2-HAIYMD-OCC1(IDX22)
                           TO  TBL-HASSYO-HAIYMD1-DAY-OCC1(WRK-KOHBUNRI)
                   MOVE  RECE02-RIHA2-HAIYMD-OCC2(IDX22)
                           TO  TBL-HASSYO-HAIYMD1-DAY-OCC2(WRK-KOHBUNRI)
                 END-IF
               END-IF
             END-PERFORM
             EVALUATE   FLG-DAISAN1   ALSO   FLG-DAISAN2
               WHEN         1         ALSO       1
                 MOVE    3    TO    TBL-HASSYO-HAIYO-FLG
               WHEN         1         ALSO       0
                 MOVE    2    TO    TBL-HASSYO-HAIYO-FLG
               WHEN         0         ALSO       1
                 MOVE    1    TO    TBL-HASSYO-HAIYO-FLG
             END-EVALUATE
           END-IF
      *
      *****
      *
           IF   TBL-HASSYO9-SINDAI-FLG   =   9
             MOVE    ZERO       TO  FLG-DAISAN1
             MOVE    ZERO       TO  FLG-DAISAN2
             PERFORM  VARYING  IDX22  FROM  1  BY  1
                       UNTIL  (IDX22   >   10                )
                        OR    (RECE02-HKNCOMBI(IDX22) =  ZERO)
               IF  RECE02-DAISANFLG(IDX22) =  1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE02-RIHA-SINDAI9(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO9-SINDAI2-SANTEI
                                                 (IDX22 IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO9-SINDAI2-COMMENT
                                                      (IDX22)
                   MOVE  RECE02-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO9-HKNCOMBI2(IDX22)
                   MOVE    1  TO  FLG-DAISAN1
                 END-IF
                 IF  RECE02-RIHA2-SINDAI9(IDX22)    =  1
                 AND TBL-HASSYO9-SINYMD2-FLG(IDX22) = ZERO
                   MOVE  RECE02-RIHA2-SINDAI9(IDX22)
                              TO  TBL-HASSYO9-SINYMD2-FLG(IDX22)
                   MOVE  RECE02-RIHA2-SIN9YMD-OCC1(IDX22)
                              TO  TBL-HASSYO9-SINYMD2-DAY-OCC1(IDX22)
                   MOVE  RECE02-RIHA2-SIN9YMD-OCC2(IDX22)
                              TO  TBL-HASSYO9-SINYMD2-DAY-OCC2(IDX22)
                 END-IF
               ELSE
                 COMPUTE WRK-KOHBUNRI = RECE02-RIHA-KOHBUNRI(IDX22)
                                      + 1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE02-RIHA-SINDAI9(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO9-SINDAI-SANTEI
                                                (WRK-KOHBUNRI IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO9-SINDAI-COMMENT
                                                     (WRK-KOHBUNRI)
                   MOVE  RECE02-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO9-HKNCOMBI(WRK-KOHBUNRI)
                   MOVE    1  TO  FLG-DAISAN2
                 END-IF
                 IF  RECE02-RIHA2-SINDAI9(IDX22)           =  1
                 AND TBL-HASSYO9-SINYMD1-FLG(WRK-KOHBUNRI) = ZERO
                   MOVE  RECE02-RIHA2-SINDAI9(IDX22)
                          TO  TBL-HASSYO9-SINYMD1-FLG(WRK-KOHBUNRI)
                   MOVE  RECE02-RIHA2-SIN9YMD-OCC1(IDX22)
                          TO  TBL-HASSYO9-SINYMD1-DAY-OCC1(WRK-KOHBUNRI)
                   MOVE  RECE02-RIHA2-SIN9YMD-OCC2(IDX22)
                          TO  TBL-HASSYO9-SINYMD1-DAY-OCC2(WRK-KOHBUNRI)
                 END-IF
               END-IF
             END-PERFORM
             EVALUATE   FLG-DAISAN1   ALSO   FLG-DAISAN2
               WHEN         1         ALSO       1
                 MOVE    3    TO    TBL-HASSYO9-SINDAI-FLG
               WHEN         1         ALSO       0
                 MOVE    2    TO    TBL-HASSYO9-SINDAI-FLG
               WHEN         0         ALSO       1
                 MOVE    1    TO    TBL-HASSYO9-SINDAI-FLG
             END-EVALUATE
           END-IF
      *
           IF   TBL-HASSYO9-KOKYUKI-FLG   =   9
             MOVE    ZERO       TO  FLG-DAISAN1
             MOVE    ZERO       TO  FLG-DAISAN2
             PERFORM  VARYING  IDX22  FROM  1  BY  1
                       UNTIL  (IDX22   >   10                )
                        OR    (RECE02-HKNCOMBI(IDX22) =  ZERO)
               IF  RECE02-DAISANFLG(IDX22) =  1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE02-RIHA-KOKYUKI9(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO9-KOKYUKI2-SANTEI
                                                 (IDX22 IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO9-KOKYUKI2-COMMENT
                                                      (IDX22)
                   MOVE  RECE02-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO9-HKNCOMBI2(IDX22)
                   MOVE    1  TO  FLG-DAISAN1
                 END-IF
                 IF  RECE02-RIHA2-KOKYUKI9(IDX22)   =  1
                 AND TBL-HASSYO9-KOKYMD2-FLG(IDX22) = ZERO
                   MOVE  RECE02-RIHA2-KOKYUKI9(IDX22)
                              TO  TBL-HASSYO9-KOKYMD2-FLG(IDX22)
                   MOVE  RECE02-RIHA2-KOK9YMD-OCC1(IDX22)
                              TO  TBL-HASSYO9-KOKYMD2-DAY-OCC1(IDX22)
                   MOVE  RECE02-RIHA2-KOK9YMD-OCC2(IDX22)
                              TO  TBL-HASSYO9-KOKYMD2-DAY-OCC2(IDX22)
                 END-IF
               ELSE
                 COMPUTE WRK-KOHBUNRI = RECE02-RIHA-KOHBUNRI(IDX22)
                                      + 1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE02-RIHA-KOKYUKI9(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO9-KOKYUKI-SANTEI
                                                (WRK-KOHBUNRI IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO9-KOKYUKI-COMMENT
                                                     (WRK-KOHBUNRI)
                   MOVE  RECE02-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO9-HKNCOMBI(WRK-KOHBUNRI)
                   MOVE    1  TO  FLG-DAISAN2
                 END-IF
                 IF  RECE02-RIHA2-KOKYUKI9(IDX22)          =  1
                 AND TBL-HASSYO9-KOKYMD1-FLG(WRK-KOHBUNRI) = ZERO
                   MOVE  RECE02-RIHA2-KOKYUKI9(IDX22)
                          TO  TBL-HASSYO9-KOKYMD1-FLG(WRK-KOHBUNRI)
                   MOVE  RECE02-RIHA2-KOK9YMD-OCC1(IDX22)
                          TO  TBL-HASSYO9-KOKYMD1-DAY-OCC1(WRK-KOHBUNRI)
                   MOVE  RECE02-RIHA2-KOK9YMD-OCC2(IDX22)
                          TO  TBL-HASSYO9-KOKYMD1-DAY-OCC2(WRK-KOHBUNRI)
                 END-IF
               END-IF
             END-PERFORM
             EVALUATE   FLG-DAISAN1   ALSO   FLG-DAISAN2
               WHEN         1         ALSO       1
                 MOVE    3    TO    TBL-HASSYO9-KOKYUKI-FLG
               WHEN         1         ALSO       0
                 MOVE    2    TO    TBL-HASSYO9-KOKYUKI-FLG
               WHEN         0         ALSO       1
                 MOVE    1    TO    TBL-HASSYO9-KOKYUKI-FLG
             END-EVALUATE
           END-IF
      *
           IF   TBL-HASSYO9-HAIYO-FLG   =   9
             MOVE    ZERO       TO  FLG-DAISAN1
             MOVE    ZERO       TO  FLG-DAISAN2
             PERFORM  VARYING  IDX22  FROM  1  BY  1
                       UNTIL  (IDX22   >   10                )
                        OR    (RECE02-HKNCOMBI(IDX22) =  ZERO)
               IF  RECE02-DAISANFLG(IDX22) =  1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE02-RIHA-HAIYO9(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO9-HAIYO2-SANTEI
                                                 (IDX22 IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO9-HAIYO2-COMMENT
                                                      (IDX22)
                   MOVE  RECE02-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO9-HKNCOMBI2(IDX22)
                   MOVE    1  TO  FLG-DAISAN1
                 END-IF
                 IF  RECE02-RIHA2-HAIYO9(IDX22)     =  1
                 AND TBL-HASSYO9-HAIYMD2-FLG(IDX22) = ZERO
                   MOVE  RECE02-RIHA2-HAIYO9(IDX22)
                              TO  TBL-HASSYO9-HAIYMD2-FLG(IDX22)
                   MOVE  RECE02-RIHA2-HAI9YMD-OCC1(IDX22)
                              TO  TBL-HASSYO9-HAIYMD2-DAY-OCC1(IDX22)
                   MOVE  RECE02-RIHA2-HAI9YMD-OCC2(IDX22)
                              TO  TBL-HASSYO9-HAIYMD2-DAY-OCC2(IDX22)
                 END-IF
               ELSE
                 COMPUTE WRK-KOHBUNRI = RECE02-RIHA-KOHBUNRI(IDX22)
                                      + 1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE02-RIHA-HAIYO9(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO9-HAIYO-SANTEI
                                                (WRK-KOHBUNRI IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO9-HAIYO-COMMENT
                                                     (WRK-KOHBUNRI)
                   MOVE  RECE02-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO9-HKNCOMBI(WRK-KOHBUNRI)
                   MOVE    1  TO  FLG-DAISAN2
                 END-IF
                 IF  RECE02-RIHA2-HAIYO9(IDX22)            =  1
                 AND TBL-HASSYO9-HAIYMD1-FLG(WRK-KOHBUNRI) = ZERO
                   MOVE  RECE02-RIHA2-HAIYO9(IDX22)
                          TO  TBL-HASSYO9-HAIYMD1-FLG(WRK-KOHBUNRI)
                   MOVE  RECE02-RIHA2-HAI9YMD-OCC1(IDX22)
                          TO  TBL-HASSYO9-HAIYMD1-DAY-OCC1(WRK-KOHBUNRI)
                   MOVE  RECE02-RIHA2-HAI9YMD-OCC2(IDX22)
                          TO  TBL-HASSYO9-HAIYMD1-DAY-OCC2(WRK-KOHBUNRI)
                 END-IF
               END-IF
             END-PERFORM
             EVALUATE   FLG-DAISAN1   ALSO   FLG-DAISAN2
               WHEN         1         ALSO       1
                 MOVE    3    TO    TBL-HASSYO9-HAIYO-FLG
               WHEN         1         ALSO       0
                 MOVE    2    TO    TBL-HASSYO9-HAIYO-FLG
               WHEN         0         ALSO       1
                 MOVE    1    TO    TBL-HASSYO9-HAIYO-FLG
             END-EVALUATE
           END-IF
      *
           .
       2002-RIHA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    È¯¾ÉÆüÊÔ½¸
      *****************************************************************
       2001X-HASSYO-HENSYU-SEC          SECTION.
      *
           IF  WRK-RIHASANTEI-YMD3 NOT = SPACE
             MOVE     ZERO   TO  FLG-RIHA
             MOVE     WRK-RIHASANTEI-YMD3 TO  WRK-KYUSEIYMD
             PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
             IF   RECE02-SRYYM >= "201905"
               PERFORM  20011-HASSYO-COM-201905-SEC
             ELSE
               PERFORM  20011-HASSYO-COMMENT-SEC
             END-IF
           END-IF
      *
           IF  WRK-RIHASANTEI-YMD  NOT = SPACE
             MOVE     1      TO  FLG-RIHA
             MOVE     WRK-RIHASANTEI-YMD  TO  WRK-KYUSEIYMD
             PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
             IF   RECE02-SRYYM >= "201905"
               PERFORM  20011-HASSYO-COM-201905-SEC
             ELSE
               PERFORM  20011-HASSYO-COMMENT-SEC
             END-IF
           END-IF
           PERFORM   VARYING   IDX16    FROM   1   BY   1
                      UNTIL    IDX16      >   31
             IF  TB-RIHASANTEI-DAY(IDX16)  NOT = ZERO
               MOVE   2      TO  FLG-RIHA
               MOVE   RECE02-SRYYM        TO  WRK-KYUSEIYMD(1:6)
               MOVE   IDX16               TO  WRK-KYUSEIYMD(7:2)
               PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
               IF   RECE02-SRYYM >= "201905"
                 PERFORM  20011-HASSYO-COM-201905-SEC
               ELSE
                 PERFORM  20011-HASSYO-COMMENT-SEC
               END-IF
             END-IF
           END-PERFORM
      *
           .
       2001X-HASSYO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    È¯¾ÉÆü¤¬¤¢¤ë¤«È½Äê¡ÊÂè»°¼Ô¹Ô°Ù¡Ë
      *****************************************************************
       2002-DAISAN-HASSYO-HANTEI-SEC          SECTION.
      *
           PERFORM  VARYING  IDX22  FROM  1  BY  1
                     UNTIL   IDX22   >   10
      *
             IF   TBL-HASSYO-SINDAI-FLG   =   2 OR 3
               IF  TBL-HASSYO-SINDAI2-COMMENT(IDX22)  =  1
                 MOVE      ZERO            TO   FLG-COMOKIKAE
                 MOVE      CONS-SINDAI     TO   WRK-SRYCD
                 MOVE      CONS-SINDAI-E   TO   WRK-SRYCD2
                 MOVE  TBL-HASSYO-SINDAI2-DAY-OCC(IDX22)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE  TBL-HASSYO-HKNCOMBI2(IDX22)
                                       TO   WRK-RIHA-HKNCOMBI
                 MOVE  TBL-HASSYO-SINYMD2-DAY-T2(IDX22)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2002X-HASSYO-HENSYU2-SEC
                 END-IF
               END-IF
             END-IF
      *
             IF   TBL-HASSYO-KOKYUKI-FLG   =   2 OR 3
               IF  TBL-HASSYO-KOKYUKI2-COMMENT(IDX22)  =  1
                 MOVE      ZERO            TO   FLG-COMOKIKAE
                 MOVE      CONS-KOKYUKI    TO   WRK-SRYCD
                 MOVE      CONS-KOKYUKI-E  TO   WRK-SRYCD2
                 MOVE  TBL-HASSYO-KOKYUKI2-DAY-OCC(IDX22)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE  TBL-HASSYO-HKNCOMBI2(IDX22)
                                       TO   WRK-RIHA-HKNCOMBI
                 MOVE  TBL-HASSYO-KOKYMD2-DAY-T2(IDX22)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2002X-HASSYO-HENSYU2-SEC
                 END-IF
               END-IF
             END-IF
      *
             IF   TBL-HASSYO-NOUKE-FLG   =   2 OR 3
               IF  TBL-HASSYO-NOUKE2-COMMENT(IDX22)  =  1
                 MOVE      1               TO   FLG-COMOKIKAE
                 MOVE      CONS-NOUKE      TO   WRK-SRYCD
                 MOVE      CONS-NOUKE-E    TO   WRK-SRYCD2
                 MOVE  TBL-HASSYO-NOUKE2-DAY-OCC(IDX22)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE  TBL-HASSYO-HKNCOMBI2(IDX22)
                                       TO   WRK-RIHA-HKNCOMBI
                 MOVE  TBL-HASSYO-NOUYMD2-DAY-T2(IDX22)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2002X-HASSYO-HENSYU2-SEC
                 END-IF
               END-IF
             END-IF
      *
             IF   TBL-HASSYO-UNDOKI-FLG   =   2 OR 3
               IF  TBL-HASSYO-UNDOKI2-COMMENT(IDX22)  =  1
                 MOVE      1                TO   FLG-COMOKIKAE
                 MOVE      CONS-UNDOKI      TO   WRK-SRYCD
                 MOVE      CONS-UNDOKI-E    TO   WRK-SRYCD2
                 MOVE  TBL-HASSYO-UNDOKI2-DAY-OCC(IDX22)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE  TBL-HASSYO-HKNCOMBI2(IDX22)
                                       TO   WRK-RIHA-HKNCOMBI
                 MOVE  TBL-HASSYO-UNDYMD2-DAY-T2(IDX22)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2002X-HASSYO-HENSYU2-SEC
                 END-IF
               END-IF
             END-IF
      *
             IF   TBL-HASSYO-NANBYO-FLG   =   2 OR 3
               IF  TBL-HASSYO-NANBYO2-COMMENT(IDX22)  =  1
                 MOVE      1                TO   FLG-COMOKIKAE
                 MOVE      CONS-NANBYO      TO   WRK-SRYCD
                 MOVE      CONS-NANBYO-E    TO   WRK-SRYCD2
                 MOVE  TBL-HASSYO-NANBYO2-DAY-OCC(IDX22)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE  TBL-HASSYO-HKNCOMBI2(IDX22)
                                       TO   WRK-RIHA-HKNCOMBI
                 MOVE  TBL-HASSYO-NANYMD2-DAY-T2(IDX22)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2002X-HASSYO-HENSYU2-SEC
                 END-IF
               END-IF
             END-IF
      *
             IF   TBL-HASSYO-SYOGAI-FLG   =   2 OR 3
               IF  TBL-HASSYO-SYOGAI2-COMMENT(IDX22)  =  1
                 MOVE      ZERO             TO   FLG-COMOKIKAE
                 MOVE      CONS-SYOGAI      TO   WRK-SRYCD
                 MOVE      CONS-SYOGAI-E    TO   WRK-SRYCD2
                 MOVE  TBL-HASSYO-SYOGAI2-DAY-OCC(IDX22)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE  TBL-HASSYO-HKNCOMBI2(IDX22)
                                       TO   WRK-RIHA-HKNCOMBI
                 MOVE  TBL-HASSYO-SYOYMD2-DAY-T2(IDX22)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2002X-HASSYO-HENSYU2-SEC
                 END-IF
               END-IF
             END-IF
      *
             IF   TBL-HASSYO-SESSYOKU-FLG   =   2 OR 3
               IF  TBL-HASSYO-SESSYOKU2-COMMENT(IDX22)  =  1
                 MOVE      ZERO               TO   FLG-COMOKIKAE
                 MOVE      CONS-SESSYOKU      TO   WRK-SRYCD
                 MOVE      CONS-SESSYOKU-E    TO   WRK-SRYCD2
                 MOVE  TBL-HASSYO-SESSYOKU2-DAY-OCC(IDX22)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE  TBL-HASSYO-HKNCOMBI2(IDX22)
                                       TO   WRK-RIHA-HKNCOMBI
                 MOVE  TBL-HASSYO-SESYMD2-DAY-T2(IDX22)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2002X-HASSYO-HENSYU2-SEC
                 END-IF
               END-IF
             END-IF
      *
             IF   TBL-HASSYO-HAIYO-FLG   =   2 OR 3
               IF  TBL-HASSYO-HAIYO2-COMMENT(IDX22)  =  1
                 MOVE      1                TO   FLG-COMOKIKAE
                 MOVE      CONS-HAIYO       TO   WRK-SRYCD
                 MOVE      CONS-HAIYO-E     TO   WRK-SRYCD2
                 MOVE  TBL-HASSYO-HAIYO2-DAY-OCC(IDX22)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE  TBL-HASSYO-HKNCOMBI2(IDX22)
                                       TO   WRK-RIHA-HKNCOMBI
                 MOVE  TBL-HASSYO-HAIYMD2-DAY-T2(IDX22)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2002X-HASSYO-HENSYU2-SEC
                 END-IF
               END-IF
             END-IF
      *
      *****
      *
             IF   TBL-HASSYO9-SINDAI-FLG   =   2 OR 3
               IF  TBL-HASSYO9-SINDAI2-COMMENT(IDX22)  =  1
                 MOVE      1               TO   FLG-COMOKIKAE
                 MOVE      CONS-SINDAI-K   TO   WRK-SRYCD
                 MOVE      "999999999"     TO   WRK-SRYCD2
                 MOVE  TBL-HASSYO9-SINDAI2-DAY-OCC(IDX22)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE  TBL-HASSYO9-HKNCOMBI2(IDX22)
                                       TO   WRK-RIHA-HKNCOMBI
                 MOVE  TBL-HASSYO9-SINYMD2-DAY-T2(IDX22)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2002X-HASSYO-HENSYU2-SEC
                 END-IF
               END-IF
             END-IF
      *
             IF   TBL-HASSYO9-KOKYUKI-FLG   =   2 OR 3
               IF  TBL-HASSYO9-KOKYUKI2-COMMENT(IDX22)  =  1
                 MOVE      1               TO   FLG-COMOKIKAE
                 MOVE      CONS-KOKYUKI-K  TO   WRK-SRYCD
                 MOVE      "999999999"     TO   WRK-SRYCD2
                 MOVE  TBL-HASSYO9-KOKYUKI2-DAY-OCC(IDX22)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE  TBL-HASSYO9-HKNCOMBI2(IDX22)
                                       TO   WRK-RIHA-HKNCOMBI
                 MOVE  TBL-HASSYO9-KOKYMD2-DAY-T2(IDX22)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2002X-HASSYO-HENSYU2-SEC
                 END-IF
               END-IF
             END-IF
      *
             IF   TBL-HASSYO9-HAIYO-FLG   =   2 OR 3
               IF  TBL-HASSYO9-HAIYO2-COMMENT(IDX22)  =  1
                 MOVE      1                TO   FLG-COMOKIKAE
                 MOVE      CONS-HAIYO-K     TO   WRK-SRYCD
                 MOVE      "999999999"      TO   WRK-SRYCD2
                 MOVE  TBL-HASSYO9-HAIYO2-DAY-OCC(IDX22)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE  TBL-HASSYO9-HKNCOMBI2(IDX22)
                                       TO   WRK-RIHA-HKNCOMBI
                 MOVE  TBL-HASSYO9-HAIYMD2-DAY-T2(IDX22)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2002X-HASSYO-HENSYU2-SEC
                 END-IF
               END-IF
             END-IF
      *
           END-PERFORM
      *
           .
       2002-DAISAN-HASSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    È¯¾ÉÆüÊÔ½¸¡ÊÂè»°¼Ô¹Ô°Ù¡Ë
      *****************************************************************
       2002X-HASSYO-HENSYU2-SEC          SECTION.
      *
           IF  WRK-RIHASANTEI-YMD3 NOT = SPACE
             MOVE     ZERO   TO  FLG-RIHA
             MOVE     WRK-RIHASANTEI-YMD3 TO  WRK-KYUSEIYMD
             MOVE     ZERO   TO  FLG-CHECK-OK
             PERFORM      20011-KYUSEIYMD-CHECK-SEC
             IF   FLG-CHECK-OK = 1
               PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
               IF   RECE02-SRYYM >= "201905"
                 PERFORM  20011-HASSYO-COM2-201905-SEC
               ELSE
                 PERFORM  20011-HASSYO-COMMENT2-SEC
               END-IF
             END-IF
           END-IF
      *
           IF  WRK-RIHASANTEI-YMD  NOT = SPACE
             MOVE     1      TO  FLG-RIHA
             MOVE     WRK-RIHASANTEI-YMD  TO  WRK-KYUSEIYMD
             MOVE     ZERO   TO  FLG-CHECK-OK
             PERFORM      20011-KYUSEIYMD-CHECK-SEC
             IF   FLG-CHECK-OK = 1
               PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
               IF   RECE02-SRYYM >= "201905"
                 PERFORM  20011-HASSYO-COM2-201905-SEC
               ELSE
                 PERFORM  20011-HASSYO-COMMENT2-SEC
               END-IF
             END-IF
           END-IF
           PERFORM   VARYING   IDX16    FROM   1   BY   1
                      UNTIL    IDX16      >   31
             IF  TB-RIHASANTEI-DAY(IDX16)  NOT = ZERO
               MOVE   2      TO  FLG-RIHA
               MOVE   RECE02-SRYYM        TO  WRK-KYUSEIYMD(1:6)
               MOVE   IDX16               TO  WRK-KYUSEIYMD(7:2)
               MOVE   ZERO   TO  FLG-CHECK-OK
               PERFORM    20011-KYUSEIYMD-CHECK-SEC
               IF   FLG-CHECK-OK = 1
                 PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
                 IF   RECE02-SRYYM >= "201905"
                   PERFORM  20011-HASSYO-COM2-201905-SEC
                 ELSE
                   PERFORM  20011-HASSYO-COMMENT2-SEC
                 END-IF
               END-IF
             END-IF
           END-PERFORM
      *
           .
       2002X-HASSYO-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü¤¬¤¢¤ë¤«È½Äê
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü¤Î£±£¸£°Æü°ÊÆâ¤«È½Äê
      *****************************************************************
       2001-KYUSEIHASSYO-HANTEI-SEC          SECTION.
      *
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT2
           MOVE     ZERO     TO     WRK-KYUSEIYMD
      *
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü
      *
      *    »»ÄêÍúÎò¸¡º÷
           INITIALIZE                  SANTEI-REC
grpsys     MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE02-PTID         TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
                MOVE    "tbl_santei"        TO  MCP-TABLE
                MOVE    "key3"              TO  MCP-PATHNAME
                PERFORM        900-SANTEI-READ-SEC
      *
                PERFORM   UNTIL  ( FLG-SANTEI         =   1 ) OR
                                 ( FLG-KYUSEI-COMMENT =   1 )
      *
                   PERFORM  VARYING   IDX3   FROM  31  BY  -1
                             UNTIL   ( IDX3               <    1 )
                               OR    ( FLG-KYUSEI-COMMENT =    1 )
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
                         MOVE   SANTEI-SRYYM   TO  WRK-FSANTEIYMD(1:6)
                         MOVE   IDX3           TO  WRK-FSANTEIYMD(7:2)
      *
                         INITIALIZE                  STS-AREA-DAY
                         INITIALIZE                  LNK-DAY6-AREA
                         MOVE    "61"            TO  LNK-DAY6-IRAI
                         MOVE    WRK-FSANTEIYMD  TO  LNK-DAY6-KIJUN
                         MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                         MOVE     179            TO  LNK-DAY6-ZOGEN
                         CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                         LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN TO  WRK-AFTER180
      *
                         IF   WRK-FSANTEIYMD(1:6) <=  RECE02-SRYYM
                         AND  RECE02-SRYYM        <=  WRK-AFTER180(1:6)
                               MOVE    1    TO   FLG-KYUSEI-COMMENT
                               MOVE  WRK-FSANTEIYMD
                                            TO   WRK-KYUSEIYMD
                         END-IF
      *
                     END-IF
      *
                   END-PERFORM
      *
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM        900-SANTEI-READ-SEC
      *
                END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾É½ªÎ»Æü
      *
           IF      FLG-KYUSEI-COMMENT    =      1
      *       »»ÄêÍúÎò¸¡º÷
              INITIALIZE                  SANTEI-REC
grpsys        MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
              MOVE    RECE02-PTID         TO  SANTEI-PTID
              MOVE    CONS-KYUSEI-END     TO  SANTEI-SRYCD
              MOVE    ZERO                TO  SANTEI-NYUGAIKBN
              MOVE    ZERO                TO  SANTEI-SRYKA
              MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
              MOVE    SANTEI-REC          TO  MCPDATA-REC
              MOVE    "tbl_santei"        TO  MCP-TABLE
              MOVE    "key3"              TO  MCP-PATHNAME
grpsys        PERFORM 900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM        900-SANTEI-READ-SEC
      *
                   PERFORM   UNTIL ( FLG-SANTEI          =   1         )
                              OR   ( SANTEI-SRYYM  < WRK-KYUSEIYMD(1:6))
                              OR   ( FLG-KYUSEI-COMMENT2 =   1         )
      *
                      PERFORM  VARYING   IDX3   FROM  31  BY  -1
                             UNTIL ( IDX3                  <    1      )
                              OR   ( FLG-KYUSEI-COMMENT2   =    1      )
      *
                        IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
                           MOVE   SANTEI-SRYYM   TO  WRK-FSANTEIYMD(1:6)
                           MOVE   IDX3           TO  WRK-FSANTEIYMD(7:2)
      *
                           IF   WRK-KYUSEIYMD        <  WRK-FSANTEIYMD
                           AND  WRK-FSANTEIYMD(1:6)  <  RECE02-SRYYM
                                  MOVE    1    TO   FLG-KYUSEI-COMMENT2
                           END-IF
      *
                        END-IF
      *
                      END-PERFORM
      *
                      MOVE    "tbl_santei"        TO  MCP-TABLE
                      MOVE    "key3"              TO  MCP-PATHNAME
                      PERFORM        900-SANTEI-READ-SEC
      *
                   END-PERFORM
      *
              END-IF
      *
              MOVE    "tbl_santei"        TO  MCP-TABLE
              MOVE    "key3"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
           END-IF
      *
           IF      FLG-KYUSEI-COMMENT    =      1
           AND     FLG-KYUSEI-COMMENT2   =      ZERO
               MOVE       ZERO            TO  WRK-RIHA-HKNCOMBI
               PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
               IF   RECE02-SRYYM      >=    "200604"
                   PERFORM   VARYING   IDX14    FROM   1   BY   1
                              UNTIL    IDX14      >    5
                      IF   TBL-HASSYO-KAITATU-COMMENT(IDX14)   =  1
                          MOVE       WRK-KYUSEIYMD(7:2)  TO  IDX16
                          IF   RECE02-SRYYM >= "201905"
                            PERFORM  20011-HASSYO-COM-201905-SEC
                          ELSE
                            PERFORM  20011-HASSYO-COMMENT-SEC
                          END-IF
                      END-IF
                   END-PERFORM
               ELSE
                   PERFORM    20011-KYUSEIHASSYO-COMMENT-SEC
               END-IF
           END-IF
      *
           .
       2001-KYUSEIHASSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆüÅù¤Î¸¡º÷
      *****************************************************************
       20011-KYUSEIHASSYO-KENSAKU-SEC          SECTION.
      *
           MOVE    ZERO                  TO  FLG-KYUSEI-OK
           MOVE    SPACE                 TO  WRK-KYUSEI-NYUGAIKBN
           MOVE    SPACE                 TO  WRK-KYUSEI-SRYKA
           MOVE    ZERO                  TO  WRK-KYUSEI-ZAINUM
           MOVE    ZERO                  TO  FLG-COMOKIKAE2
           MOVE    ZERO                  TO  FLG-COMARI
           MOVE    ZERO                  TO  FLG-COMARI2
      *
           MOVE    ZERO                  TO  IDZ
           INITIALIZE                        WRK-FREE-AREA
      *++++
           PERFORM   VARYING   IDX15    FROM   2   BY  -1
                      UNTIL   (IDX15          <    1)
                       OR     (FLG-KYUSEI-OK  =    1)
      *
           INITIALIZE                        SRYACT-REC
grpsys     MOVE    RECE02-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    IDX15                 TO  SRY-NYUGAIKBN
           MOVE    RECE02-PTID           TO  SRY-PTID
           MOVE    WRK-KYUSEIYMD(1:6)    TO  SRY-SRYYM
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (1)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (2)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (3)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (4)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (5)
           MOVE    SRYACT-REC            TO  MCPDATA-REC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key7"               TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"         TO  MCP-TABLE
               MOVE    "key7"               TO  MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL   FLG-SRYACT     =   1
                         OR     FLG-KYUSEI-OK  =   1
                 INITIALIZE                     SRYACCT-REC
grpsys           MOVE    SRY-HOSPNUM      TO    ACCT-HOSPNUM
                 MOVE    SRY-NYUGAIKBN    TO    ACCT-NYUGAIKBN
                 MOVE    SRY-PTID         TO    ACCT-PTID
                 MOVE    SRY-SRYKA        TO    ACCT-SRYKA
                 MOVE    SRY-SRYYM        TO    ACCT-SRYYM
                 MOVE    SRY-ZAINUM       TO    ACCT-ZAINUM
                 MOVE    SRYACCT-REC      TO    MCPDATA-REC
                 MOVE    "tbl_sryacct"       TO  MCP-TABLE
                 MOVE    "key31"             TO  MCP-PATHNAME
grpsys           PERFORM 900-DBSELECT-SEC
                 IF      MCP-RC              =   ZERO
                    MOVE    "tbl_sryacct"       TO   MCP-TABLE
                    MOVE    "key31"             TO   MCP-PATHNAME
grpsys              PERFORM 900-DBFETCH-SEC
                    IF      MCP-RC        =       ZERO
                        MOVE    MCPDATA-REC  TO  SRYACCT-REC
      *
                        MOVE    WRK-KYUSEIYMD(7:2)  TO  DAY-IDX2
                        IF   ACCT-DAY(1 DAY-IDX2)  NOT =  ZERO
      *                   ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¤Î¸¡º÷
                          INITIALIZE                 HKNCOMBI-REC
grpsys                    MOVE    ACCT-HOSPNUM   TO  COMB-HOSPNUM
                          MOVE    ACCT-PTID      TO  COMB-PTID
                          MOVE    ACCT-HKNCOMBI  TO  COMB-HKNCOMBINUM
                          MOVE    HKNCOMBI-REC   TO  MCPDATA-REC
                          MOVE    "tbl_hkncombi" TO  MCP-TABLE
                          MOVE    "key"          TO  MCP-PATHNAME
grpsys                    PERFORM 900-DBSELECT-SEC
                          IF      MCP-RC         =   ZERO
                             MOVE    "tbl_hkncombi"  TO  MCP-TABLE
                             MOVE    "key"           TO  MCP-PATHNAME
grpsys                       PERFORM 900-DBFETCH-SEC
                             IF      MCP-RC        =       ZERO
                               MOVE   MCPDATA-REC  TO   HKNCOMBI-REC
                               IF   WRK-RIHA-HKNCOMBI = ZERO
                                 IF  (COMB-HKNNUM = "971" OR "973"
                                                          OR "975")
                                 OR  (COMB-KOH1HKNNUM = "970"     )
                                     CONTINUE
                                 ELSE
                                   MOVE   1   TO  FLG-KYUSEI-OK
                                   MOVE   ACCT-NYUGAIKBN
                                              TO  WRK-KYUSEI-NYUGAIKBN
                                   MOVE   ACCT-SRYKA
                                              TO  WRK-KYUSEI-SRYKA
                                   MOVE   ACCT-ZAINUM
                                              TO  WRK-KYUSEI-ZAINUM
                                 END-IF
                               ELSE
                                 IF WRK-RIHA-HKNCOMBI =
                                                    COMB-HKNCOMBINUM
                                   MOVE   1   TO  FLG-KYUSEI-OK
                                   MOVE   ACCT-NYUGAIKBN
                                              TO  WRK-KYUSEI-NYUGAIKBN
                                   MOVE   ACCT-SRYKA
                                              TO  WRK-KYUSEI-SRYKA
                                   MOVE   ACCT-ZAINUM
                                              TO  WRK-KYUSEI-ZAINUM
                                 END-IF
                               END-IF
                             END-IF
                          END-IF
                          MOVE    "tbl_hkncombi"      TO  MCP-TABLE
                          MOVE    "key"               TO  MCP-PATHNAME
                          PERFORM 900-CLOSE-SEC
      *
                        END-IF
                    END-IF
                 END-IF
                 MOVE    "tbl_sryacct"       TO   MCP-TABLE
                 MOVE    "key31"             TO   MCP-PATHNAME
                 PERFORM   900-CLOSE-SEC
      *
                 MOVE    "tbl_sryact"         TO  MCP-TABLE
                 MOVE    "key7"               TO  MCP-PATHNAME
                 PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "key7"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           END-PERFORM
      *++++
           IF      WRK-KYUSEI-ZAINUM    NOT =    ZERO
              INITIALIZE                           SRYACT-REC
grpsys        MOVE    RECE02-HOSPNUM         TO    SRY-HOSPNUM
              MOVE    WRK-KYUSEI-NYUGAIKBN   TO    SRY-NYUGAIKBN
              MOVE    RECE02-PTID            TO    SRY-PTID
              MOVE    WRK-KYUSEI-SRYKA       TO    SRY-SRYKA
              MOVE    WRK-KYUSEIYMD(1:6)     TO    SRY-SRYYM
              MOVE    WRK-KYUSEI-ZAINUM      TO    SRY-ZAINUM
              MOVE    SRYACT-REC             TO    MCPDATA-REC
              MOVE    "tbl_sryact"         TO      MCP-TABLE
              MOVE    "key2"               TO      MCP-PATHNAME
grpsys        PERFORM 900-DBSELECT-SEC
              IF      MCP-RC               =       ZERO
                 MOVE    "tbl_sryact"         TO  MCP-TABLE
                 MOVE    "key2"               TO  MCP-PATHNAME
                 PERFORM  900-SRYACT-READ-SEC
                 PERFORM  UNTIL   FLG-SRYACT     =   1
                   PERFORM  VARYING  IDX12  FROM  1  BY  1
                             UNTIL  (IDX12             >    5   )  OR
                                    (SRY-SRYCD(IDX12)  =   SPACE)
                      IF  ((SRY-SRYCD(IDX12)(1:1)     =   "8"  )  OR
                           (SRY-SRYCD(IDX12)(1:3)     =   "008"))
                      AND ( SRY-SRYCD(IDX12)(1:4) NOT =
                                              "0085" AND "0086" )
                       IF   SRY-INPUTNUM(IDX12)   NOT =   ZERO
                         INITIALIZE                      PTCOM-REC
grpsys                   MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
                         MOVE    SRY-PTID            TO  PTCOM-PTID
                         MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
                         MOVE    SRY-SRYCD(IDX12)    TO  PTCOM-SRYCD
                         MOVE    SRY-INPUTNUM(IDX12) TO  PTCOM-RENNUM
                         PERFORM 900-PTCOM-READ-SEC
                         ADD     1     TO    IDZ
                         MOVE    PTCOM-INPUTCOMENT
                                                TO  WK-FREE-NAME(IDZ)
                         PERFORM   VARYING   IDX13   FROM   300   BY  -1
                                    UNTIL    IDX13    <    1
                           IF  WK-FREE-NAME(IDZ)(IDX13:1)  NOT =  SPACE
                             COMPUTE  WK-FREE-YUKOKETA(IDZ)  =
                                      IDX13    /    2
                             MOVE     1     TO      IDX13
                           END-IF
                         END-PERFORM
                       ELSE
                         MOVE    SRY-SRYCD(IDX12)
                                             TO  WRK-TENSU-SRYCD
                         MOVE    SRY-SRYYM   TO  WRK-TENSU-SRYYM
                         PERFORM      900-TENSU-TBL-SEC
                         ADD     1           TO  IDZ
                         MOVE    TNS-NAME    TO  WK-FREE-NAME(IDZ)
                         PERFORM   VARYING   IDX13   FROM   300   BY  -1
                                    UNTIL    IDX13    <    1
                           IF  WK-FREE-NAME(IDZ)(IDX13:1)  NOT =  SPACE
                             COMPUTE  WK-FREE-YUKOKETA(IDZ)  =
                                      IDX13    /    2
                             MOVE     1     TO      IDX13
                           END-IF
                         END-PERFORM
                       END-IF
                       IF   FLG-COMOKIKAE  =   1
                          EVALUATE  SRY-SRYCD(IDX12)
                            WHEN     "840000042"
                              IF   FLG-COMOKIKAE2 = ZERO
                                 MOVE    1    TO  FLG-COMOKIKAE2
                                 MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                                 MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                                 COMPUTE  IDZ  =  IDZ - 1
                              END-IF
                            WHEN     "840000101"
                              IF   FLG-COMOKIKAE2 = ZERO
                                 MOVE    2    TO  FLG-COMOKIKAE2
                                 MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                                 MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                                 COMPUTE  IDZ  =  IDZ - 1
                              END-IF
                            WHEN     "840000058"
                              IF   FLG-COMOKIKAE2 = ZERO
                                 MOVE    3    TO  FLG-COMOKIKAE2
                                 MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                                 MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                                 COMPUTE  IDZ  =  IDZ - 1
                              END-IF
                          END-EVALUATE
                       END-IF
                       IF   FLG-COMOKIKAE  =   1
                          IF  SRY-SRYCD(IDX12) = "840000042" OR
                                                 "840000101" OR
                                                 "840000058"
                            MOVE    1    TO   FLG-COMARI2
                          ELSE
                            MOVE    1    TO   FLG-COMARI
                          END-IF
                       ELSE
                            MOVE    1    TO   FLG-COMARI
                       END-IF
                      END-IF
                   END-PERFORM
                   MOVE    "tbl_sryact"         TO  MCP-TABLE
                   MOVE    "key2"               TO  MCP-PATHNAME
                   PERFORM   900-SRYACT-READ-SEC
                 END-PERFORM
              END-IF
              MOVE    "tbl_sryact"         TO  MCP-TABLE
              MOVE    "key2"               TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
           END-IF
      *++++
           IF      FLG-COMARI    =    ZERO
              INITIALIZE                      SANTEIPLUS-REC
              MOVE    RECE02-HOSPNUM      TO  SANTEIPLUS-HOSPNUM
              MOVE    RECE02-PTID         TO  SANTEIPLUS-PTID
              MOVE    WRK-KYUSEIYMD(1:6)  TO  SANTEIPLUS-SRYYM
              MOVE    WRK-SRYCD           TO  SANTEIPLUS-SRYCD
              MOVE    ZERO                TO  SANTEIPLUS-NYUGAIKBN
              MOVE    ZERO                TO  SANTEIPLUS-SRYKA
              MOVE    WRK-RIHA-HKNCOMBI   TO  SANTEIPLUS-HKNCOMBINUM
              MOVE    WRK-KYUSEIYMD(7:2)  TO  SANTEIPLUS-DAYKEY
              MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
              MOVE    "tbl_santeiplus"    TO  MCP-TABLE
              MOVE    "key2"              TO  MCP-PATHNAME
              PERFORM 900-DBSELECT-SEC
              IF      MCP-RC               =       ZERO
                 MOVE    "tbl_santeiplus"     TO  MCP-TABLE
                 MOVE    "key2"               TO  MCP-PATHNAME
                 PERFORM  900-SANTEIPLUS-READ-SEC
                 PERFORM  UNTIL   FLG-SANTEIPLUS     =   1
      *            ¥ê¥Ï¥³¥á¥ó¥È¤Î½ªÎ»Æü¤È¿ÇÎÅ·î¤ò¥Á¥§¥Ã¥¯
      *            ¡ÊÍúÎòÈÖ¹æ¡ä£°¤Î¥Ç¡¼¥¿¡Ë
                   IF     SANTEIPLUS-RENNUM   NOT =  ZERO
                    IF  ( SANTEIPLUS-ENDYMD       =  SPACE        )
                    OR  ((SANTEIPLUS-ENDYMD   NOT =  SPACE       ) AND
                         (SANTEIPLUS-ENDYMD(1:6) >=  RECE02-SRYYM))
                     IF  SANTEIPLUS-COMMENT  NOT =  SPACE
                      ADD     1                 TO  IDZ
                      MOVE  SANTEIPLUS-COMMENT  TO  WK-FREE-NAME(IDZ)
                      PERFORM   VARYING   IDX13   FROM   300   BY  -1
                                 UNTIL    IDX13    <     1
                        IF  WK-FREE-NAME(IDZ)(IDX13:1)  NOT =  SPACE
                          COMPUTE  WK-FREE-YUKOKETA(IDZ)  =
                                   IDX13    /    2
                          MOVE     1     TO      IDX13
                        END-IF
                      END-PERFORM
                     END-IF
                     IF   FLG-COMOKIKAE  =    1
                     AND  FLG-COMARI2    =   ZERO
                      IF   SANTEIPLUS-COMMENT(1:8)  = "¼ê½Ñ¡¡Æü"
                        IF   FLG-COMOKIKAE2 = ZERO
                           MOVE    1    TO  FLG-COMOKIKAE2
                           MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                           MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                           COMPUTE  IDZ  =  IDZ - 1
                        END-IF
                      END-IF
                      IF   SANTEIPLUS-COMMENT(1:12) = "µÞÀ­Áý°­¡¡Æü"
                        IF   FLG-COMOKIKAE2 = ZERO
                           MOVE    2    TO  FLG-COMOKIKAE2
                           MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                           MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                           COMPUTE  IDZ  =  IDZ - 1
                        END-IF
                      END-IF
                      IF   SANTEIPLUS-COMMENT(1:8) = "¿ÇÃÇ¡¡Æü"
                        IF   FLG-COMOKIKAE2 = ZERO
                           MOVE    3    TO  FLG-COMOKIKAE2
                           MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                           MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                           COMPUTE  IDZ  =  IDZ - 1
                        END-IF
                      END-IF
                     END-IF
                    END-IF
                   END-IF
                   MOVE    "tbl_santeiplus"     TO  MCP-TABLE
                   MOVE    "key2"               TO  MCP-PATHNAME
                   PERFORM  900-SANTEIPLUS-READ-SEC
                 END-PERFORM
              END-IF
              MOVE    "tbl_santeiplus"     TO  MCP-TABLE
              MOVE    "key2"               TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
       20011-KYUSEIHASSYO-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       20011-KYUSEIHASSYO-COMMENT-SEC          SECTION.
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO        TO  IDX-RECE05
           MOVE    SPACE       TO  WRK-RECE05XX-REC
           INITIALIZE              WRK-RECE05XX-REC
      *
           ADD     1           TO  IDX-RECE05
           MOVE    "810000001" TO  WRK-RECE05XX-SRYCD(IDX-RECE05 1)
           MOVE    ZERO        TO  WRK-RECE05XX-SRYSURYO(IDX-RECE05 1)
           MOVE    9           TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
           MOVE    "È¯¾É¡¡¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
           MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(7:4)
           MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(13:4)
      *
           PERFORM   VARYING    IDX12   FROM    1   BY  1
                      UNTIL   ( IDX12               >   49    )
                       OR     ( WK-FREE-NAME(IDX12) =   SPACE )
                ADD    1       TO  IDX-RECE05
                MOVE   "810000001"
                               TO  WRK-RECE05XX-SRYCD(IDX-RECE05 1)
                MOVE   ZERO    TO  WRK-RECE05XX-SRYSURYO(IDX-RECE05 1)
                MOVE   WK-FREE-YUKOKETA(IDX12)
                               TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                MOVE   WK-FREE-NAME(IDX12)
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
           END-PERFORM
      *
      *    ²Ê¡¦ÆüÉÕ¤ÎÊÔ½¸¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE05
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE05
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE05  =    1
                PERFORM 800-RECE02-SET059-SEC
                MOVE    99999996             TO  RECE059-ZAINUM
                MOVE    WRK-KOHBUNRI-JIDO    TO  RECE059-KOHBUNRI
                MOVE    "99"                 TO  RECE059-SRYKBN
                ADD     1                    TO  WRK-RENNUM
                MOVE    WRK-RENNUM           TO  RECE059-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                          UNTIL   (IDX             >    5)   OR
                                  (FLG-WRKRECE05   =    1)
                   ADD         1             TO    IDX-RECE05
                   IF (WRK-RECE05XX-SRYCD(IDX-RECE05 1) = SPACE)
                   OR (IDX-RECE05                       >  100 )
                       MOVE    1             TO    FLG-WRKRECE05
                   ELSE
                       MOVE  WRK-RECE05XX-SRYACT-TBL (IDX-RECE05 1)
                                        TO   RECE059-SRYACT-TBL (IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                END-PERFORM
                IF   RECE059-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE059-REC
                   ADD     1            TO   CNT-RECE059
                END-IF
           END-PERFORM
      *
      *
           PERFORM 800-RECE02-SET049-SEC
           MOVE    99999996            TO  RECE049-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE049-KOHBUNRI
           MOVE    "99"                TO  RECE049-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE049-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE049-MEISAISU
      *
           WRITE   RECE049-REC
           ADD     1                   TO  CNT-RECE049
      *
           .
       20011-KYUSEIHASSYO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    È¯¾ÉÆü¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       20011-HASSYO-COMMENT-SEC          SECTION.
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO        TO  IDX-RECE05
           MOVE    SPACE       TO  WRK-RECE05XX-REC
           INITIALIZE              WRK-RECE05XX-REC
      *
           ADD     1           TO  IDX-RECE05
           MOVE    "810000001" TO  WRK-RECE05XX-SRYCD(IDX-RECE05 1)
           MOVE    ZERO        TO  WRK-RECE05XX-SRYSURYO(IDX-RECE05 1)
           EVALUATE      WRK-SRYCD
             WHEN       CONS-KYUSEI
               MOVE    19   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
               MOVE    "Ç¾·ì´É¼À´µµÞÀ­È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
               MOVE    21   TO  IDC1
               MOVE    27   TO  IDC2
               MOVE    33   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               COMPUTE WRK-ZAINUM    =  99980000
                                     + (IDX16 + (IDX14 * 31))
               MOVE    "40" TO  WRK-SRYKBN
             WHEN       CONS-SINDAI
               MOVE    23   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
               MOVE    "¿´Âç·ì´É¼À´µ¥ê¥Ï¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
               MOVE    29   TO  IDC1
               MOVE    35   TO  IDC2
               MOVE    41   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980200 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980200 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980200
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SINDAI-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    22  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE  "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    22  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE  "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§¼ê½Ñ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    24  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
               MOVE  "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980400 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980400 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980400
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-NOUKE
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   MOVE    21  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE    "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    21  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE    "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    23  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE
                       "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    29  TO  IDC1
                   MOVE    35  TO  IDC2
                   MOVE    41  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       3
                   MOVE    21  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE    "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§¿ÇÃÇ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980600 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980600 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980600
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-HAIYO
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       1
                 WHEN       3
                   MOVE    20  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE    "ÇÑÍÑ¾É¸õ·²¥ê¥Ï¡§¿ÇÃÇ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    23  TO  IDC1
                   MOVE    29  TO  IDC2
                   MOVE    35  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    22  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE
                       "ÇÑÍÑ¾É¸õ·²¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980800 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980800 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980800
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-HAIYO-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    22  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE  "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    22  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE  "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§¼ê½Ñ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    24  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
               MOVE  "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981000 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981000 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981000
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-UNDOKI
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   MOVE    18  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE    "±¿Æ°´ï¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    19  TO  IDC1
                   MOVE    25  TO  IDC2
                   MOVE    31  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    18  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE    "±¿Æ°´ï¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    19  TO  IDC1
                   MOVE    25  TO  IDC2
                   MOVE    31  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    20  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE    "±¿Æ°´ï¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    23  TO  IDC1
                   MOVE    29  TO  IDC2
                   MOVE    35  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       3
                   MOVE    18  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE    "±¿Æ°´ï¥ê¥Ï¡§¿ÇÃÇ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    19  TO  IDC1
                   MOVE    25  TO  IDC2
                   MOVE    31  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981200 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981200 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981200
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-KOKYUKI
               MOVE    20   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
               MOVE    "¸ÆµÛ´ï¥ê¥Ï¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
               MOVE    23   TO  IDC1
               MOVE    29   TO  IDC2
               MOVE    35   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981400 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981400 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981400
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-KOKYUKI-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    22  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE  "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    22  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE  "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§¼ê½Ñ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    24  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
               MOVE  "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981600 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981600 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981600
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SESSYOKU
               MOVE    21   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
               MOVE    "ÀÝ¿©µ¡Ç½ÎÅË¡¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
               MOVE    25   TO  IDC1
               MOVE    31   TO  IDC2
               MOVE    37   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981800 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981800 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981800
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-NANBYO
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    19  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE    "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    21  TO  IDC1
                   MOVE    27  TO  IDC2
                   MOVE    33  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    19  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE    "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    21  TO  IDC1
                   MOVE    27  TO  IDC2
                   MOVE    33  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    21  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE    "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99982000 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99982000 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99982000
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SYOGAI
               MOVE    21   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
               MOVE    "¾ã³²»ù¡Ê¼Ô¡Ë¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
               MOVE    25   TO  IDC1
               MOVE    31   TO  IDC2
               MOVE    37   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99982200 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99982200 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99982200
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
           END-EVALUATE
      *
           PERFORM   VARYING    IDX12   FROM    1   BY  1
                      UNTIL   ( IDX12               >   49    )
                       OR     ( WK-FREE-NAME(IDX12) =   SPACE )
                ADD    1       TO  IDX-RECE05
                MOVE   "810000001"
                               TO  WRK-RECE05XX-SRYCD(IDX-RECE05 1)
                MOVE   ZERO    TO  WRK-RECE05XX-SRYSURYO(IDX-RECE05 1)
                MOVE   WK-FREE-YUKOKETA(IDX12)
                               TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                MOVE   WK-FREE-NAME(IDX12)
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
           END-PERFORM
      *
      *    ²Ê¡¦ÆüÉÕ¤ÎÊÔ½¸¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE05
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE05
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE05  =    1
                PERFORM 800-RECE02-SET059-SEC
                MOVE    WRK-ZAINUM           TO  RECE059-ZAINUM
                COMPUTE WRK-KOHBUNRI  =  IDX14  -  1
                MOVE    WRK-KOHBUNRI         TO  RECE059-KOHBUNRI
                MOVE    WRK-SRYKBN           TO  RECE059-SRYKBN
                ADD     1                    TO  WRK-RENNUM
                MOVE    WRK-RENNUM           TO  RECE059-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                          UNTIL   (IDX             >    5)   OR
                                  (FLG-WRKRECE05   =    1)
                   ADD         1             TO    IDX-RECE05
                   IF (WRK-RECE05XX-SRYCD(IDX-RECE05 1) = SPACE)
                   OR (IDX-RECE05                       >  100 )
                       MOVE    1             TO    FLG-WRKRECE05
                   ELSE
                       MOVE  WRK-RECE05XX-SRYACT-TBL (IDX-RECE05 1)
                                        TO   RECE059-SRYACT-TBL (IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                END-PERFORM
                IF   RECE059-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE059-REC
                   ADD     1            TO   CNT-RECE059
                END-IF
           END-PERFORM
      *
      *
           PERFORM 800-RECE02-SET049-SEC
           MOVE    WRK-ZAINUM          TO  RECE049-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  IDX14  -  1
           MOVE    WRK-KOHBUNRI        TO  RECE049-KOHBUNRI
           MOVE    WRK-SRYKBN          TO  RECE049-SRYKBN
           MOVE    TBL-HASSYO-HKNCOMBI(IDX14)
                                       TO  RECE049-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE049-MEISAISU
      *
           WRITE   RECE049-REC
           ADD     1                   TO  CNT-RECE049
      *
           .
       20011-HASSYO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    È¯¾ÉÆü¤Î¥³¥á¥ó¥ÈÊÔ½¸¡ÊÂè»°¼Ô¹Ô°Ù¡Ë
      *****************************************************************
       20011-HASSYO-COMMENT2-SEC          SECTION.
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO        TO  IDX-RECE05
           MOVE    SPACE       TO  WRK-RECE05XX-REC
           INITIALIZE              WRK-RECE05XX-REC
      *
           ADD     1           TO  IDX-RECE05
           MOVE    "810000001" TO  WRK-RECE05XX-SRYCD(IDX-RECE05 1)
           MOVE    ZERO        TO  WRK-RECE05XX-SRYSURYO(IDX-RECE05 1)
           EVALUATE      WRK-SRYCD
             WHEN       CONS-SINDAI
               IF  RECE02-DAISAN-TOKKI  =  ZERO
                 MOVE  30   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE  "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                 MOVE  "¿´Âç·ì´É¼À´µ¥ê¥Ï¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                 MOVE  43   TO  IDC1
                 MOVE  49   TO  IDC2
                 MOVE  55   TO  IDC3
               ELSE
                 MOVE  23   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE  "¿´Âç·ì´É¼À´µ¥ê¥Ï¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                 MOVE  29   TO  IDC1
                 MOVE  35   TO  IDC2
                 MOVE  41   TO  IDC3
               END-IF
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99983000 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99983000 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99983000
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SINDAI-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  29  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  41  TO  IDC1
                     MOVE  47  TO  IDC2
                     MOVE  53  TO  IDC3
                   ELSE
                     MOVE  22  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  27  TO  IDC1
                     MOVE  33  TO  IDC2
                     MOVE  39  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  29  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§¼ê½Ñ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  41  TO  IDC1
                     MOVE  47  TO  IDC2
                     MOVE  53  TO  IDC3
                   ELSE
                     MOVE  22  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§¼ê½Ñ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  27  TO  IDC1
                     MOVE  33  TO  IDC2
                     MOVE  39  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  31  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                 MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  45  TO  IDC1
                     MOVE  51  TO  IDC2
                     MOVE  57  TO  IDC3
                   ELSE
                     MOVE  24  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  31  TO  IDC1
                     MOVE  37  TO  IDC2
                     MOVE  43  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99983400 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99983400 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99983400
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-NOUKE
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  28  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE  "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  39  TO  IDC1
                     MOVE  45  TO  IDC2
                     MOVE  51  TO  IDC3
                   ELSE
                     MOVE  21  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  25  TO  IDC1
                     MOVE  31  TO  IDC2
                     MOVE  37  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  28  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE  "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  39  TO  IDC1
                     MOVE  45  TO  IDC2
                     MOVE  51  TO  IDC3
                   ELSE
                     MOVE  21  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  25  TO  IDC1
                     MOVE  31  TO  IDC2
                     MOVE  37  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  30  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE
                       "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  43  TO  IDC1
                     MOVE  49  TO  IDC2
                     MOVE  55  TO  IDC3
                   ELSE
                     MOVE  23  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE
                       "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  29  TO  IDC1
                     MOVE  35  TO  IDC2
                     MOVE  41  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       3
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  28  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE  "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§¿ÇÃÇ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  39  TO  IDC1
                     MOVE  45  TO  IDC2
                     MOVE  51  TO  IDC3
                   ELSE
                     MOVE  21  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§¿ÇÃÇ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  25  TO  IDC1
                     MOVE  31  TO  IDC2
                     MOVE  37  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99983800 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99983800 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99983800
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-HAIYO
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       1
                 WHEN       3
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  27  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE  "ÇÑÍÑ¾É¸õ·²¥ê¥Ï¡§¿ÇÃÇ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  37  TO  IDC1
                     MOVE  43  TO  IDC2
                     MOVE  49  TO  IDC3
                   ELSE
                     MOVE  20  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "ÇÑÍÑ¾É¸õ·²¥ê¥Ï¡§¿ÇÃÇ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  23  TO  IDC1
                     MOVE  29  TO  IDC2
                     MOVE  35  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  29  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE
                       "ÇÑÍÑ¾É¸õ·²¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  41  TO  IDC1
                     MOVE  47  TO  IDC2
                     MOVE  53  TO  IDC3
                   ELSE
                     MOVE  22  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE
                       "ÇÑÍÑ¾É¸õ·²¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  27  TO  IDC1
                     MOVE  33  TO  IDC2
                     MOVE  39  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99984200 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99984200 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99984200
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-HAIYO-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  29  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  41  TO  IDC1
                     MOVE  47  TO  IDC2
                     MOVE  53  TO  IDC3
                   ELSE
                     MOVE  22  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  27  TO  IDC1
                     MOVE  33  TO  IDC2
                     MOVE  39  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  29  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§¼ê½Ñ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  41  TO  IDC1
                     MOVE  47  TO  IDC2
                     MOVE  53  TO  IDC3
                   ELSE
                     MOVE  22  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§¼ê½Ñ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  27  TO  IDC1
                     MOVE  33  TO  IDC2
                     MOVE  39  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  31  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                 MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  45  TO  IDC1
                     MOVE  51  TO  IDC2
                     MOVE  57  TO  IDC3
                   ELSE
                     MOVE  24  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  31  TO  IDC1
                     MOVE  37  TO  IDC2
                     MOVE  43  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99984600 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99984600 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99984600
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-UNDOKI
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  25  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE  "±¿Æ°´ï¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  33  TO  IDC1
                     MOVE  39  TO  IDC2
                     MOVE  45  TO  IDC3
                   ELSE
                     MOVE  18  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "±¿Æ°´ï¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  19  TO  IDC1
                     MOVE  25  TO  IDC2
                     MOVE  31  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  25  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE  "±¿Æ°´ï¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  33  TO  IDC1
                     MOVE  39  TO  IDC2
                     MOVE  45  TO  IDC3
                   ELSE
                     MOVE  18  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "±¿Æ°´ï¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  19  TO  IDC1
                     MOVE  25  TO  IDC2
                     MOVE  31  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  27  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE  "±¿Æ°´ï¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  37  TO  IDC1
                     MOVE  43  TO  IDC2
                     MOVE  49  TO  IDC3
                   ELSE
                     MOVE  20  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "±¿Æ°´ï¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  23  TO  IDC1
                     MOVE  29  TO  IDC2
                     MOVE  35  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       3
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  25  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE  "±¿Æ°´ï¥ê¥Ï¡§¿ÇÃÇ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  33  TO  IDC1
                     MOVE  39  TO  IDC2
                     MOVE  45  TO  IDC3
                   ELSE
                     MOVE  18  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "±¿Æ°´ï¥ê¥Ï¡§¿ÇÃÇ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  19  TO  IDC1
                     MOVE  25  TO  IDC2
                     MOVE  31  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99985000 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99985000 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99985000
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-KOKYUKI
               IF  RECE02-DAISAN-TOKKI  =  ZERO
                 MOVE  27   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE  "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                 MOVE  "¸ÆµÛ´ï¥ê¥Ï¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                 MOVE  37   TO  IDC1
                 MOVE  43   TO  IDC2
                 MOVE  49   TO  IDC3
               ELSE
                 MOVE  20   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE  "¸ÆµÛ´ï¥ê¥Ï¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                 MOVE  23   TO  IDC1
                 MOVE  29   TO  IDC2
                 MOVE  35   TO  IDC3
               END-IF
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99985400 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99985400 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99985400
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-KOKYUKI-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  29  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  41  TO  IDC1
                     MOVE  47  TO  IDC2
                     MOVE  53  TO  IDC3
                   ELSE
                     MOVE  22  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  27  TO  IDC1
                     MOVE  33  TO  IDC2
                     MOVE  39  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  29  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§¼ê½Ñ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  41  TO  IDC1
                     MOVE  47  TO  IDC2
                     MOVE  53  TO  IDC3
                   ELSE
                     MOVE  22  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§¼ê½Ñ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  27  TO  IDC1
                     MOVE  33  TO  IDC2
                     MOVE  39  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  31  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                 MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  45  TO  IDC1
                     MOVE  51  TO  IDC2
                     MOVE  57  TO  IDC3
                   ELSE
                     MOVE  24  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  31  TO  IDC1
                     MOVE  37  TO  IDC2
                     MOVE  43  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99985800 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99985800 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99985800
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SESSYOKU
               IF  RECE02-DAISAN-TOKKI  =  ZERO
                 MOVE  28   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE  "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                 MOVE  "ÀÝ¿©µ¡Ç½ÎÅË¡¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                 MOVE  39   TO  IDC1
                 MOVE  45   TO  IDC2
                 MOVE  51   TO  IDC3
               ELSE
                 MOVE  21   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE  "ÀÝ¿©µ¡Ç½ÎÅË¡¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                 MOVE  25   TO  IDC1
                 MOVE  31   TO  IDC2
                 MOVE  37   TO  IDC3
               END-IF
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99986200 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99986200 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99986200
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-NANBYO
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  26  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE  "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  35  TO  IDC1
                     MOVE  41  TO  IDC2
                     MOVE  47  TO  IDC3
                   ELSE
                     MOVE  19  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  21  TO  IDC1
                     MOVE  27  TO  IDC2
                     MOVE  33  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  26  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE  "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  35  TO  IDC1
                     MOVE  41  TO  IDC2
                     MOVE  47  TO  IDC3
                   ELSE
                     MOVE  19  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  21  TO  IDC1
                     MOVE  27  TO  IDC2
                     MOVE  33  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  28  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE  "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  39  TO  IDC1
                     MOVE  45  TO  IDC2
                     MOVE  51  TO  IDC3
                   ELSE
                     MOVE  21  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE  "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  25  TO  IDC1
                     MOVE  31  TO  IDC2
                     MOVE  37  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99986600 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99986600 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99986600
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SYOGAI
               IF  RECE02-DAISAN-TOKKI  =  ZERO
                 MOVE  28   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE  "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                 MOVE  "¾ã³²»ù¡Ê¼Ô¡Ë¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                 MOVE  39   TO  IDC1
                 MOVE  45   TO  IDC2
                 MOVE  51   TO  IDC3
               ELSE
                 MOVE  21   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE  "¾ã³²»ù¡Ê¼Ô¡Ë¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                 MOVE  25   TO  IDC1
                 MOVE  31   TO  IDC2
                 MOVE  37   TO  IDC3
               END-IF
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99987000 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99987000 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99987000
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
           END-EVALUATE
      *
           PERFORM   VARYING    IDX12   FROM    1   BY  1
                      UNTIL   ( IDX12               >   49    )
                       OR     ( WK-FREE-NAME(IDX12) =   SPACE )
                ADD    1       TO  IDX-RECE05
                MOVE   "810000001"
                               TO  WRK-RECE05XX-SRYCD(IDX-RECE05 1)
                MOVE   ZERO    TO  WRK-RECE05XX-SRYSURYO(IDX-RECE05 1)
                MOVE   WK-FREE-YUKOKETA(IDX12)
                               TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                MOVE   WK-FREE-NAME(IDX12)
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
           END-PERFORM
      *
      *    ²Ê¡¦ÆüÉÕ¤ÎÊÔ½¸¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE05
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE05
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE05  =    1
                PERFORM 800-RECE02-SET059-SEC
                MOVE    WRK-ZAINUM           TO  RECE059-ZAINUM
                COMPUTE WRK-KOHBUNRI  =  WRK-KOHBUNRI-JIDO
                MOVE    WRK-KOHBUNRI         TO  RECE059-KOHBUNRI
                MOVE    WRK-SRYKBN           TO  RECE059-SRYKBN
                ADD     1                    TO  WRK-RENNUM
                MOVE    WRK-RENNUM           TO  RECE059-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                          UNTIL   (IDX             >    5)   OR
                                  (FLG-WRKRECE05   =    1)
                   ADD         1             TO    IDX-RECE05
                   IF (WRK-RECE05XX-SRYCD(IDX-RECE05 1) = SPACE)
                   OR (IDX-RECE05                       >  100 )
                       MOVE    1             TO    FLG-WRKRECE05
                   ELSE
                       MOVE  WRK-RECE05XX-SRYACT-TBL (IDX-RECE05 1)
                                        TO   RECE059-SRYACT-TBL (IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                END-PERFORM
                IF   RECE059-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE059-REC
                   ADD     1            TO   CNT-RECE059
                END-IF
           END-PERFORM
      *
      *
           PERFORM 800-RECE02-SET049-SEC
           MOVE    WRK-ZAINUM          TO  RECE049-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  WRK-KOHBUNRI-JIDO
           MOVE    WRK-KOHBUNRI        TO  RECE049-KOHBUNRI
           MOVE    WRK-SRYKBN          TO  RECE049-SRYKBN
           MOVE    TBL-HASSYO-HKNCOMBI2(IDX22)
                                       TO  RECE049-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE049-MEISAISU
      *
           WRITE   RECE049-REC
           ADD     1                   TO  CNT-RECE049
      *
           .
       20011-HASSYO-COMMENT2-EXT.
           EXIT.
      *
      *****************************************************************
      *    È¯¾ÉÆü¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       20011-HASSYO-COM-201905-SEC          SECTION.
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO        TO  IDX-RECE05
           MOVE    SPACE       TO  WRK-RECE05XX-REC
           INITIALIZE              WRK-RECE05XX-REC
      *
           ADD     1           TO  IDX-RECE05
           MOVE    "810000001" TO  WRK-RECE05XX-SRYCD(IDX-RECE05 1)
           MOVE    ZERO        TO  WRK-RECE05XX-SRYSURYO(IDX-RECE05 1)
           EVALUATE      WRK-SRYCD
             WHEN       CONS-KYUSEI
               MOVE    21   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
               MOVE "Ç¾·ì´É¼À´µµÞÀ­È¯¾É¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
               MOVE    25   TO  IDC1
               MOVE    31   TO  IDC2
               MOVE    37   TO  IDC3
               MOVE    21   TO  IDC4
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               COMPUTE WRK-ZAINUM    =  99980000
                                     + (IDX16 + (IDX14 * 31))
               MOVE    "40" TO  WRK-SRYKBN
             WHEN       CONS-SINDAI
               MOVE    25   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
               MOVE "¿´Âç·ì´É¼À´µ¥ê¥Ï¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
               MOVE    33   TO  IDC1
               MOVE    39   TO  IDC2
               MOVE    45   TO  IDC3
               MOVE    29   TO  IDC4
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980200 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980200 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980200
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SINDAI-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    24  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§È¯¾É¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   MOVE    27  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    24  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§¼ê½Ñ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   MOVE    27  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    26  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
             MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§µÞÀ­Áý°­¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    35  TO  IDC1
                   MOVE    41  TO  IDC2
                   MOVE    47  TO  IDC3
                   MOVE    31  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980400 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980400 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980400
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-NOUKE
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   MOVE    23  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    29  TO  IDC1
                   MOVE    35  TO  IDC2
                   MOVE    41  TO  IDC3
                   MOVE    25  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    23  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    29  TO  IDC1
                   MOVE    35  TO  IDC2
                   MOVE    41  TO  IDC3
                   MOVE    25  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    25  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
               MOVE "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    33  TO  IDC1
                   MOVE    39  TO  IDC2
                   MOVE    45  TO  IDC3
                   MOVE    29  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       3
                   MOVE    23  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§¿ÇÃÇ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    29  TO  IDC1
                   MOVE    35  TO  IDC2
                   MOVE    41  TO  IDC3
                   MOVE    25  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980600 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980600 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980600
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-HAIYO
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       1
                 WHEN       3
                   MOVE    22  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE "ÇÑÍÑ¾É¸õ·²¥ê¥Ï¡§¿ÇÃÇ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   MOVE    23  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    24  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
               MOVE "ÇÑÍÑ¾É¸õ·²¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   MOVE    27  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980800 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980800 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980800
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-HAIYO-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    24  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
               MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§È¯¾É¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   MOVE    27  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    24  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
               MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§¼ê½Ñ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   MOVE    27  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    26  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
           MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§µÞÀ­Áý°­¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    35  TO  IDC1
                   MOVE    41  TO  IDC2
                   MOVE    47  TO  IDC3
                   MOVE    31  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981000 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981000 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981000
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-UNDOKI
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   MOVE    20  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE "±¿Æ°´ï¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    23  TO  IDC1
                   MOVE    29  TO  IDC2
                   MOVE    35  TO  IDC3
                   MOVE    19  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    20  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE "±¿Æ°´ï¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    23  TO  IDC1
                   MOVE    29  TO  IDC2
                   MOVE    35  TO  IDC3
                   MOVE    19  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    22  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE "±¿Æ°´ï¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   MOVE    23  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       3
                   MOVE    20  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE "±¿Æ°´ï¥ê¥Ï¡§¿ÇÃÇ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    23  TO  IDC1
                   MOVE    29  TO  IDC2
                   MOVE    35  TO  IDC3
                   MOVE    19  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981200 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981200 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981200
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-KOKYUKI
               MOVE    22   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
               MOVE "¸ÆµÛ´ï¥ê¥Ï¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
               MOVE    27   TO  IDC1
               MOVE    33   TO  IDC2
               MOVE    39   TO  IDC3
               MOVE    23   TO  IDC4
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981400 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981400 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981400
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-KOKYUKI-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    24  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§È¯¾É¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   MOVE    27  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    24  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§¼ê½Ñ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   MOVE    27  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    26  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
             MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§µÞÀ­Áý°­¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    35  TO  IDC1
                   MOVE    41  TO  IDC2
                   MOVE    47  TO  IDC3
                   MOVE    31  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981600 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981600 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981600
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SESSYOKU
               MOVE    23   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
               MOVE "ÀÝ¿©µ¡Ç½ÎÅË¡¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
               MOVE    29   TO  IDC1
               MOVE    35   TO  IDC2
               MOVE    41   TO  IDC3
               MOVE    25   TO  IDC4
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981800 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981800 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981800
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-NANBYO
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    21  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   MOVE    21  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    21  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   MOVE    21  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    23  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                   MOVE    29  TO  IDC1
                   MOVE    35  TO  IDC2
                   MOVE    41  TO  IDC3
                   MOVE    25  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99982000 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99982000 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99982000
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SYOGAI
               MOVE    23   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
               MOVE "¾ã³²»ù¡Ê¼Ô¡Ë¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
               MOVE    29   TO  IDC1
               MOVE    35   TO  IDC2
               MOVE    41   TO  IDC3
               MOVE    25   TO  IDC4
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99982200 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99982200 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99982200
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
           END-EVALUATE
      *
           PERFORM   VARYING    IDX12   FROM    1   BY  1
                      UNTIL   ( IDX12               >   49    )
                       OR     ( WK-FREE-NAME(IDX12) =   SPACE )
                ADD    1       TO  IDX-RECE05
                MOVE   "810000001"
                               TO  WRK-RECE05XX-SRYCD(IDX-RECE05 1)
                MOVE   ZERO    TO  WRK-RECE05XX-SRYSURYO(IDX-RECE05 1)
                MOVE   WK-FREE-YUKOKETA(IDX12)
                               TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                MOVE   WK-FREE-NAME(IDX12)
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
           END-PERFORM
      *
      *    ²Ê¡¦ÆüÉÕ¤ÎÊÔ½¸¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE05
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE05
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE05  =    1
                PERFORM 800-RECE02-SET059-SEC
                MOVE    WRK-ZAINUM           TO  RECE059-ZAINUM
                COMPUTE WRK-KOHBUNRI  =  IDX14  -  1
                MOVE    WRK-KOHBUNRI         TO  RECE059-KOHBUNRI
                MOVE    WRK-SRYKBN           TO  RECE059-SRYKBN
                ADD     1                    TO  WRK-RENNUM
                MOVE    WRK-RENNUM           TO  RECE059-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                          UNTIL   (IDX             >    5)   OR
                                  (FLG-WRKRECE05   =    1)
                   ADD         1             TO    IDX-RECE05
                   IF (WRK-RECE05XX-SRYCD(IDX-RECE05 1) = SPACE)
                   OR (IDX-RECE05                       >  100 )
                       MOVE    1             TO    FLG-WRKRECE05
                   ELSE
                       MOVE  WRK-RECE05XX-SRYACT-TBL (IDX-RECE05 1)
                                        TO   RECE059-SRYACT-TBL (IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                END-PERFORM
                IF   RECE059-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE059-REC
                   ADD     1            TO   CNT-RECE059
                END-IF
           END-PERFORM
      *
      *
           PERFORM 800-RECE02-SET049-SEC
           MOVE    WRK-ZAINUM          TO  RECE049-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  IDX14  -  1
           MOVE    WRK-KOHBUNRI        TO  RECE049-KOHBUNRI
           MOVE    WRK-SRYKBN          TO  RECE049-SRYKBN
           MOVE    TBL-HASSYO-HKNCOMBI(IDX14)
                                       TO  RECE049-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE049-MEISAISU
      *
           WRITE   RECE049-REC
           ADD     1                   TO  CNT-RECE049
      *
           .
       20011-HASSYO-COM-201905-EXT.
           EXIT.
      *
      *****************************************************************
      *    È¯¾ÉÆü¤Î¥³¥á¥ó¥ÈÊÔ½¸¡ÊÂè»°¼Ô¹Ô°Ù¡Ë
      *****************************************************************
       20011-HASSYO-COM2-201905-SEC          SECTION.
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO        TO  IDX-RECE05
           MOVE    SPACE       TO  WRK-RECE05XX-REC
           INITIALIZE              WRK-RECE05XX-REC
      *
           ADD     1           TO  IDX-RECE05
           MOVE    "810000001" TO  WRK-RECE05XX-SRYCD(IDX-RECE05 1)
           MOVE    ZERO        TO  WRK-RECE05XX-SRYSURYO(IDX-RECE05 1)
           EVALUATE      WRK-SRYCD
             WHEN       CONS-SINDAI
               IF  RECE02-DAISAN-TOKKI  =  ZERO
                 MOVE  32   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
               MOVE "¿´Âç·ì´É¼À´µ¥ê¥Ï¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                 MOVE  47   TO  IDC1
                 MOVE  53   TO  IDC2
                 MOVE  59   TO  IDC3
                 MOVE  43   TO  IDC4
               ELSE
                 MOVE  25   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
               MOVE "¿´Âç·ì´É¼À´µ¥ê¥Ï¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                 MOVE  33   TO  IDC1
                 MOVE  39   TO  IDC2
                 MOVE  45   TO  IDC3
                 MOVE  29   TO  IDC4
               END-IF
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99983000 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99983000 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99983000
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SINDAI-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  31  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                 MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§È¯¾É¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  45  TO  IDC1
                     MOVE  51  TO  IDC2
                     MOVE  57  TO  IDC3
                     MOVE  41  TO  IDC4
                   ELSE
                     MOVE  24  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§È¯¾É¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  31  TO  IDC1
                     MOVE  37  TO  IDC2
                     MOVE  43  TO  IDC3
                     MOVE  27  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  31  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                 MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§¼ê½Ñ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  45  TO  IDC1
                     MOVE  51  TO  IDC2
                     MOVE  57  TO  IDC3
                     MOVE  41  TO  IDC4
                   ELSE
                     MOVE  24  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§¼ê½Ñ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  31  TO  IDC1
                     MOVE  37  TO  IDC2
                     MOVE  43  TO  IDC3
                     MOVE  27  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  33  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
             MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§µÞÀ­Áý°­¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  49  TO  IDC1
                     MOVE  55  TO  IDC2
                     MOVE  61  TO  IDC3
                     MOVE  45  TO  IDC4
                   ELSE
                     MOVE  26  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
             MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§µÞÀ­Áý°­¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  35  TO  IDC1
                     MOVE  41  TO  IDC2
                     MOVE  47  TO  IDC3
                     MOVE  31  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99983400 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99983400 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99983400
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-NOUKE
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  30  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                   MOVE "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  43  TO  IDC1
                     MOVE  49  TO  IDC2
                     MOVE  55  TO  IDC3
                     MOVE  39  TO  IDC4
                   ELSE
                     MOVE  23  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  29  TO  IDC1
                     MOVE  35  TO  IDC2
                     MOVE  41  TO  IDC3
                     MOVE  25  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  30  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                   MOVE "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  43  TO  IDC1
                     MOVE  49  TO  IDC2
                     MOVE  55  TO  IDC3
                     MOVE  39  TO  IDC4
                   ELSE
                     MOVE  23  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  29  TO  IDC1
                     MOVE  35  TO  IDC2
                     MOVE  41  TO  IDC3
                     MOVE  25  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  32  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
               MOVE "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  47  TO  IDC1
                     MOVE  53  TO  IDC2
                     MOVE  59  TO  IDC3
                     MOVE  43  TO  IDC4
                   ELSE
                     MOVE  25  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
               MOVE "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  33  TO  IDC1
                     MOVE  39  TO  IDC2
                     MOVE  45  TO  IDC3
                     MOVE  29  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       3
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  30  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                   MOVE "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§¿ÇÃÇ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  43  TO  IDC1
                     MOVE  49  TO  IDC2
                     MOVE  55  TO  IDC3
                     MOVE  39  TO  IDC4
                   ELSE
                     MOVE  23  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§¿ÇÃÇ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  29  TO  IDC1
                     MOVE  35  TO  IDC2
                     MOVE  41  TO  IDC3
                     MOVE  25  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99983800 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99983800 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99983800
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-HAIYO
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       1
                 WHEN       3
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  29  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE "ÇÑÍÑ¾É¸õ·²¥ê¥Ï¡§¿ÇÃÇ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  41  TO  IDC1
                     MOVE  47  TO  IDC2
                     MOVE  53  TO  IDC3
                     MOVE  37  TO  IDC4
                   ELSE
                     MOVE  22  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "ÇÑÍÑ¾É¸õ·²¥ê¥Ï¡§¿ÇÃÇ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  27  TO  IDC1
                     MOVE  33  TO  IDC2
                     MOVE  39  TO  IDC3
                     MOVE  23  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  31  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                 MOVE "ÇÑÍÑ¾É¸õ·²¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  45  TO  IDC1
                     MOVE  51  TO  IDC2
                     MOVE  57  TO  IDC3
                     MOVE  41  TO  IDC4
                   ELSE
                     MOVE  24  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE "ÇÑÍÑ¾É¸õ·²¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  31  TO  IDC1
                     MOVE  37  TO  IDC2
                     MOVE  43  TO  IDC3
                     MOVE  27  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99984200 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99984200 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99984200
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-HAIYO-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  31  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                 MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§È¯¾É¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  45  TO  IDC1
                     MOVE  51  TO  IDC2
                     MOVE  57  TO  IDC3
                     MOVE  41  TO  IDC4
                   ELSE
                     MOVE  24  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§È¯¾É¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  31  TO  IDC1
                     MOVE  37  TO  IDC2
                     MOVE  43  TO  IDC3
                     MOVE  27  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  31  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                 MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§¼ê½Ñ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  45  TO  IDC1
                     MOVE  51  TO  IDC2
                     MOVE  57  TO  IDC3
                     MOVE  41  TO  IDC4
                   ELSE
                     MOVE  24  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§¼ê½Ñ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  31  TO  IDC1
                     MOVE  37  TO  IDC2
                     MOVE  43  TO  IDC3
                     MOVE  27  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  33  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
             MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§µÞÀ­Áý°­¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  49  TO  IDC1
                     MOVE  55  TO  IDC2
                     MOVE  61  TO  IDC3
                     MOVE  45  TO  IDC4
                   ELSE
                     MOVE  26  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
             MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§µÞÀ­Áý°­¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  35  TO  IDC1
                     MOVE  41  TO  IDC2
                     MOVE  47  TO  IDC3
                     MOVE  31  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99984600 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99984600 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99984600
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-UNDOKI
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  27  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE "±¿Æ°´ï¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  37  TO  IDC1
                     MOVE  43  TO  IDC2
                     MOVE  49  TO  IDC3
                     MOVE  33  TO  IDC4
                   ELSE
                     MOVE  20  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "±¿Æ°´ï¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  23  TO  IDC1
                     MOVE  29  TO  IDC2
                     MOVE  35  TO  IDC3
                     MOVE  19  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  27  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE "±¿Æ°´ï¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  37  TO  IDC1
                     MOVE  43  TO  IDC2
                     MOVE  49  TO  IDC3
                     MOVE  33  TO  IDC4
                   ELSE
                     MOVE  20  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "±¿Æ°´ï¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  23  TO  IDC1
                     MOVE  29  TO  IDC2
                     MOVE  35  TO  IDC3
                     MOVE  19  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  29  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE "±¿Æ°´ï¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  41  TO  IDC1
                     MOVE  47  TO  IDC2
                     MOVE  53  TO  IDC3
                     MOVE  37  TO  IDC4
                   ELSE
                     MOVE  22  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "±¿Æ°´ï¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  27  TO  IDC1
                     MOVE  33  TO  IDC2
                     MOVE  39  TO  IDC3
                     MOVE  23  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       3
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  27  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE "±¿Æ°´ï¥ê¥Ï¡§¿ÇÃÇ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  37  TO  IDC1
                     MOVE  43  TO  IDC2
                     MOVE  49  TO  IDC3
                     MOVE  33  TO  IDC4
                   ELSE
                     MOVE  20  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "±¿Æ°´ï¥ê¥Ï¡§¿ÇÃÇ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  23  TO  IDC1
                     MOVE  29  TO  IDC2
                     MOVE  35  TO  IDC3
                     MOVE  19  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99985000 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99985000 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99985000
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-KOKYUKI
               IF  RECE02-DAISAN-TOKKI  =  ZERO
                 MOVE  29   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                 MOVE "¸ÆµÛ´ï¥ê¥Ï¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                 MOVE  41   TO  IDC1
                 MOVE  47   TO  IDC2
                 MOVE  53   TO  IDC3
                 MOVE  37   TO  IDC4
               ELSE
                 MOVE  22   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE "¸ÆµÛ´ï¥ê¥Ï¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                 MOVE  27   TO  IDC1
                 MOVE  33   TO  IDC2
                 MOVE  39   TO  IDC3
                 MOVE  23   TO  IDC4
               END-IF
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99985400 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99985400 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99985400
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-KOKYUKI-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  31  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                 MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§È¯¾É¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  45  TO  IDC1
                     MOVE  51  TO  IDC2
                     MOVE  57  TO  IDC3
                     MOVE  41  TO  IDC4
                   ELSE
                     MOVE  24  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§È¯¾É¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  31  TO  IDC1
                     MOVE  37  TO  IDC2
                     MOVE  43  TO  IDC3
                     MOVE  27  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  31  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                 MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§¼ê½Ñ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  45  TO  IDC1
                     MOVE  51  TO  IDC2
                     MOVE  57  TO  IDC3
                     MOVE  41  TO  IDC4
                   ELSE
                     MOVE  24  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§¼ê½Ñ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  31  TO  IDC1
                     MOVE  37  TO  IDC2
                     MOVE  43  TO  IDC3
                     MOVE  27  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  33  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
             MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§µÞÀ­Áý°­¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  49  TO  IDC1
                     MOVE  55  TO  IDC2
                     MOVE  61  TO  IDC3
                     MOVE  45  TO  IDC4
                   ELSE
                     MOVE  26  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
             MOVE "Áá´ü¡¦½é´ü¥ê¥Ï²Ã»»¡§µÞÀ­Áý°­¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  35  TO  IDC1
                     MOVE  41  TO  IDC2
                     MOVE  47  TO  IDC3
                     MOVE  31  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99985800 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99985800 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99985800
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SESSYOKU
               IF  RECE02-DAISAN-TOKKI  =  ZERO
                 MOVE  30   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                 MOVE "ÀÝ¿©µ¡Ç½ÎÅË¡¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                 MOVE  43   TO  IDC1
                 MOVE  49   TO  IDC2
                 MOVE  55   TO  IDC3
                 MOVE  39   TO  IDC4
               ELSE
                 MOVE  23   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE "ÀÝ¿©µ¡Ç½ÎÅË¡¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                 MOVE  29   TO  IDC1
                 MOVE  35   TO  IDC2
                 MOVE  41   TO  IDC3
                 MOVE  25   TO  IDC4
               END-IF
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99986200 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99986200 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99986200
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-NANBYO
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  28  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  39  TO  IDC1
                     MOVE  45  TO  IDC2
                     MOVE  51  TO  IDC3
                     MOVE  35  TO  IDC4
                   ELSE
                     MOVE  21  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  25  TO  IDC1
                     MOVE  31  TO  IDC2
                     MOVE  37  TO  IDC3
                     MOVE  21  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  28  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                     MOVE "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  39  TO  IDC1
                     MOVE  45  TO  IDC2
                     MOVE  51  TO  IDC3
                     MOVE  35  TO  IDC4
                   ELSE
                     MOVE  21  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  25  TO  IDC1
                     MOVE  31  TO  IDC2
                     MOVE  37  TO  IDC3
                     MOVE  21  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE02-DAISAN-TOKKI  =  ZERO
                     MOVE  30  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                     MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                   MOVE "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                     MOVE  43  TO  IDC1
                     MOVE  49  TO  IDC2
                     MOVE  55  TO  IDC3
                     MOVE  39  TO  IDC4
                   ELSE
                     MOVE  23  TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                   MOVE "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                     MOVE  29  TO  IDC1
                     MOVE  35  TO  IDC2
                     MOVE  41  TO  IDC3
                     MOVE  25  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99986600 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99986600 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99986600
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SYOGAI
               IF  RECE02-DAISAN-TOKKI  =  ZERO
                 MOVE  30   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE "¡ÊÂè»°¼Ô¹Ô°Ù¡Ë"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:14)
                 MOVE "¾ã³²»ù¡Ê¼Ô¡Ë¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(15:)
                 MOVE  43   TO  IDC1
                 MOVE  49   TO  IDC2
                 MOVE  55   TO  IDC3
                 MOVE  39   TO  IDC4
               ELSE
                 MOVE  23   TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                 MOVE "¾ã³²»ù¡Ê¼Ô¡Ë¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(1:)
                 MOVE  29   TO  IDC1
                 MOVE  35   TO  IDC2
                 MOVE  41   TO  IDC3
                 MOVE  25   TO  IDC4
               END-IF
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99987000 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99987000 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99987000
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
           END-EVALUATE
      *
           PERFORM   VARYING    IDX12   FROM    1   BY  1
                      UNTIL   ( IDX12               >   49    )
                       OR     ( WK-FREE-NAME(IDX12) =   SPACE )
                ADD    1       TO  IDX-RECE05
                MOVE   "810000001"
                               TO  WRK-RECE05XX-SRYCD(IDX-RECE05 1)
                MOVE   ZERO    TO  WRK-RECE05XX-SRYSURYO(IDX-RECE05 1)
                MOVE   WK-FREE-YUKOKETA(IDX12)
                               TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                MOVE   WK-FREE-NAME(IDX12)
                               TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
           END-PERFORM
      *
      *    ²Ê¡¦ÆüÉÕ¤ÎÊÔ½¸¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE05
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE05
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE05  =    1
                PERFORM 800-RECE02-SET059-SEC
                MOVE    WRK-ZAINUM           TO  RECE059-ZAINUM
                COMPUTE WRK-KOHBUNRI  =  WRK-KOHBUNRI-JIDO
                MOVE    WRK-KOHBUNRI         TO  RECE059-KOHBUNRI
                MOVE    WRK-SRYKBN           TO  RECE059-SRYKBN
                ADD     1                    TO  WRK-RENNUM
                MOVE    WRK-RENNUM           TO  RECE059-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                          UNTIL   (IDX             >    5)   OR
                                  (FLG-WRKRECE05   =    1)
                   ADD         1             TO    IDX-RECE05
                   IF (WRK-RECE05XX-SRYCD(IDX-RECE05 1) = SPACE)
                   OR (IDX-RECE05                       >  100 )
                       MOVE    1             TO    FLG-WRKRECE05
                   ELSE
                       MOVE  WRK-RECE05XX-SRYACT-TBL (IDX-RECE05 1)
                                        TO   RECE059-SRYACT-TBL (IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                END-PERFORM
                IF   RECE059-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE059-REC
                   ADD     1            TO   CNT-RECE059
                END-IF
           END-PERFORM
      *
      *
           PERFORM 800-RECE02-SET049-SEC
           MOVE    WRK-ZAINUM          TO  RECE049-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  WRK-KOHBUNRI-JIDO
           MOVE    WRK-KOHBUNRI        TO  RECE049-KOHBUNRI
           MOVE    WRK-SRYKBN          TO  RECE049-SRYKBN
           MOVE    TBL-HASSYO-HKNCOMBI2(IDX22)
                                       TO  RECE049-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE049-MEISAISU
      *
           WRITE   RECE049-REC
           ADD     1                   TO  CNT-RECE049
      *
           .
       20011-HASSYO-COM2-201905-EXT.
           EXIT.
      *
      *****************************************************************
      *    È¯¾ÉÆü¤Î¥³¥á¥ó¥ÈÊÔ½¸¡ÊÇ¯·îÆü¡Ë
      *****************************************************************
       200111-HASSYO-COMMENT-DATE-SEC          SECTION.
      *
           INITIALIZE                         STS-AREA-DAY
           INITIALIZE                         LNK-DAY2-AREA
           MOVE    "21"                   TO  LNK-DAY2-IRAI
           MOVE    WRK-KYUSEIYMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                          LNK-DAY2-AREA
           IF      STS-DAY-RC1            =   ZERO
              MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                       TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(IDC1:4)
              END-IF
      *
              MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                       TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(IDC2:4)
              END-IF
      *
              MOVE    LNK-DAY2-EDTYMD1-D   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                       TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(IDC3:4)
              END-IF
              IF   RECE02-SRYYM >= "201905"
                 MOVE    LNK-DAY2-EDTYMD2-G
                       TO  WRK-RECE05XX-NAME(IDX-RECE05 1)(IDC4:4)
              END-IF
           END-IF
      *
           .
       200111-HASSYO-COMMENT-DATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ê¥Ï¥Ó¥êÈ¯¾ÉÆü¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       20011-RIHABIRIHASSYO-HANTEI-SEC          SECTION.
      *
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT2
           MOVE     SPACE    TO     WRK-RIHASANTEI-END
           INITIALIZE               TBL-RIHASANTEI-DAY-END
           INITIALIZE               TBL-RIHASANTEI-DAY-END2
           INITIALIZE               TBL-RIHASANTEI-DAY-END3
      *
      *    ¥ê¥Ï½ªÎ»Æü
      *
      *    »»ÄêÍúÎò¸¡º÷
           INITIALIZE                  SANTEI-REC
grpsys     MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE02-PTID         TO  SANTEI-PTID
           MOVE    WRK-SRYCD2          TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    WRK-RIHA-HKNCOMBI   TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
                MOVE    "tbl_santei"        TO  MCP-TABLE
                MOVE    "key3"              TO  MCP-PATHNAME
                PERFORM        900-SANTEI-READ-SEC
      *
                PERFORM   UNTIL  ( FLG-SANTEI          =  1 ) OR
                                 ( FLG-KYUSEI-COMMENT2 =  1 )
      *
                 IF   SANTEI-SRYYM  <=  RECE02-SRYYM
                  IF   SANTEI-SRYYM  =  RECE02-SRYYM
                   PERFORM   VARYING   IDX3   FROM   1  BY   1
                              UNTIL    IDX3    >    31
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
                        MOVE    1   TO   TB-RIHASANTEI-DAY-END(IDX3)
      *
                     END-IF
      *
                   END-PERFORM
                  ELSE
                   PERFORM   VARYING   IDX3   FROM  31  BY  -1
                              UNTIL  ( IDX3                <   1 )
                                OR   ( FLG-KYUSEI-COMMENT2 =   1 )
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
                        MOVE  SANTEI-SRYYM  TO  WRK-RIHASANTEI-END(1:6)
                        MOVE  IDX3          TO  WRK-RIHASANTEI-END(7:2)
      *
                        MOVE  1             TO  FLG-KYUSEI-COMMENT2
      *
                     END-IF
      *
                   END-PERFORM
                  END-IF
                 END-IF
      *
                 MOVE    "tbl_santei"        TO  MCP-TABLE
                 MOVE    "key3"              TO  MCP-PATHNAME
                 PERFORM        900-SANTEI-READ-SEC
      *
                END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      ********
      *
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT2
           MOVE     ZERO     TO     WRK-KYUSEIYMD
           INITIALIZE               TBL-RIHASANTEI-DAY
           MOVE     SPACE    TO     WRK-RIHASANTEI-YMD
           MOVE     SPACE    TO     WRK-RIHASANTEI-YMD2
           MOVE     SPACE    TO     WRK-RIHASANTEI-YMD3
           MOVE     ZERO     TO     WRK-RIHASANTEI41-FLG
      *
      *    ¥ê¥Ï³«»ÏÆü
      *
      *    Åö·î¤ÎÀèÆ¬¤Î¥ê¥Ï¥Ó¥ê»»ÄêÆü
           MOVE     ZERO           TO  RIHA-ST2
           PERFORM  VARYING  RIHA-IDX  FROM   1  BY   1
                     UNTIL   RIHA-IDX    >   31
             IF     TBL-HASSYO-TAIHI-SANTEI(RIHA-IDX) NOT = ZERO
               MOVE     RIHA-IDX   TO  RIHA-ST2
               MOVE     31         TO  RIHA-IDX
             END-IF
           END-PERFORM
      *
      *    »»ÄêÍúÎò¸¡º÷
           INITIALIZE                  SANTEI-REC
grpsys     MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE02-PTID         TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    WRK-RIHA-HKNCOMBI   TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
                MOVE    "tbl_santei"        TO  MCP-TABLE
                MOVE    "key3"              TO  MCP-PATHNAME
                PERFORM        900-SANTEI-READ-SEC
      *
                PERFORM   UNTIL  ( FLG-SANTEI          =  1 ) OR
                                 ( FLG-KYUSEI-COMMENT2 =  1 )
      *
                 IF   SANTEI-SRYYM  <=  RECE02-SRYYM
                  IF   SANTEI-SRYYM  =  RECE02-SRYYM
                   PERFORM  VARYING   IDX3   FROM   1  BY  1
                             UNTIL    IDX3    >    31
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
                         MOVE   1     TO  TB-RIHASANTEI-DAY(IDX3)
      *
                         MOVE   1     TO  FLG-KYUSEI-COMMENT
      *
                         IF    (SANTEI-SRYYM = "200604")
                         AND   (IDX3         =  1      )
                            MOVE    1    TO   WRK-RIHASANTEI41-FLG
                         END-IF
      **>                IF    (SANTEI-SRYYM  NOT = "200604")
      **>                AND   (IDX3              =  1      )
      **>                   MOVE   1     TO   FLG-KYUSEI-COMMENT2
      **>                END-IF
      *
                     END-IF
      *
                   END-PERFORM
                  ELSE
                   PERFORM  VARYING   IDX3   FROM  31  BY  -1
                             UNTIL   ( IDX3                <   1 )
                               OR    ( FLG-KYUSEI-COMMENT2 =   1 )
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
                       PERFORM  200111-RIHA-SANTEIPLUS-SEC
                     END-IF
      *
                     IF  (SANTEI-DAY(IDX3)  NOT =  ZERO)
                     AND (FLG-SANTEIPLUS-HANTEI =  ZERO)
                       MOVE  SANTEI-SRYYM    TO WRK-RIHASANTEI-YMD2(1:6)
                       MOVE  IDX3            TO WRK-RIHASANTEI-YMD2(7:2)
                       IF   (WRK-RIHASANTEI-YMD   = SPACE     )
                       OR   (WRK-RIHASANTEI-YMD   = "20060401")
                         IF  (WRK-RIHASANTEI-END  = SPACE              )
                         OR  (WRK-RIHASANTEI-END  < WRK-RIHASANTEI-YMD2)
                           MOVE  WRK-RIHASANTEI-YMD2
                                             TO WRK-RIHASANTEI-YMD
                           MOVE  1           TO FLG-KYUSEI-COMMENT
                           IF  (SANTEIPLUS-ENDYMD   NOT =  SPACE       )
                           AND (SANTEIPLUS-ENDYMD(1:6)  =  RECE02-SRYYM)
                           AND (IDX43                  >=  RIHA-ST2    )
                             MOVE   1   TO TB-RIHASANTEI-DAY-END3(IDX43)
                           END-IF
                         END-IF
                         IF  (WRK-RIHASANTEI-END  = SPACE              )
                         OR  (WRK-RIHASANTEI-END  < WRK-RIHASANTEI-YMD2)
                           IF   (WRK-RIHASANTEI41-FLG =  1        )
                           AND  (WRK-RIHASANTEI-YMD2  < "20060401")
                             MOVE  ZERO      TO TB-RIHASANTEI-DAY(1)
                           END-IF
                         END-IF
                       ELSE
                         IF  (WRK-RIHASANTEI-END  = SPACE              )
                         OR  (WRK-RIHASANTEI-END  < WRK-RIHASANTEI-YMD2)
                           MOVE  WRK-RIHASANTEI-YMD2
                                             TO WRK-RIHASANTEI-YMD3
                         END-IF
                         MOVE     1          TO FLG-KYUSEI-COMMENT2
                         IF  (WRK-RIHASANTEI-END  = SPACE              )
                         OR  (WRK-RIHASANTEI-END  < WRK-RIHASANTEI-YMD2)
                           IF    WRK-RIHASANTEI-YMD2  = "20060401"
                             MOVE  ZERO      TO FLG-KYUSEI-COMMENT2
                           END-IF
                         END-IF
                       END-IF
                     END-IF
      *
                   END-PERFORM
                  END-IF
                 END-IF
      *
                 MOVE    "tbl_santei"        TO  MCP-TABLE
                 MOVE    "key3"              TO  MCP-PATHNAME
                 PERFORM        900-SANTEI-READ-SEC
      *
                END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      ***
      *
      *    Åö·îÀèÆ¬¤Î¥ê¥Ï¥Ó¥ê»»ÄêÆü¤è¤êÁ°¤ËÅö·î¤Î¥ê¥Ï¥Ó¥ê½ªÎ»Æü¤¬¤¢¤ì¤Ð
      *    Á°·î°ÊÁ°¤ÎÄ¾¶á¤Î¥ê¥Ï¥Ó¥ê³«»ÏÆü¤ò¥¯¥ê¥¢¤¹¤ë
      *    µÚ¤Ó¡¢
      *    Åö·î¤Î¥ê¥Ï¥Ó¥ê½ªÎ»Æü¤è¤êÁ°¤ÎÅö·î¤Î¥ê¥Ï¥Ó¥ê³«»ÏÆü¤ò¥¯¥ê¥¢¤¹¤ë
           IF    RIHA-ST2            NOT = ZERO
             PERFORM  VARYING  RIHA-IDX  FROM   1  BY   1
                       UNTIL   RIHA-IDX    >   31
               IF    (TB-RIHASANTEI-DAY-END(RIHA-IDX) NOT = ZERO)
               AND   (RIHA-ST2  >  RIHA-IDX                     )
                 MOVE   SPACE      TO  WRK-RIHASANTEI-YMD
                 MOVE   SPACE      TO  WRK-RIHASANTEI-YMD3
                 PERFORM  VARYING  RIHA-IDX2  FROM  1  BY  1
                           UNTIL   RIHA-IDX2    >  31
                   IF    (TB-RIHASANTEI-DAY(RIHA-IDX2)  NOT = ZERO)
                   AND   (RIHA-IDX  >= RIHA-IDX2                  )
                     MOVE  ZERO    TO  TB-RIHASANTEI-DAY(RIHA-IDX2)
                   END-IF
                 END-PERFORM
               END-IF
             END-PERFORM
           END-IF
      *
      *    ¥ê¥Ï¥Ó¥ê³«»ÏÆü¤òÂàÈò
           INITIALIZE                     WRK-CHECK-RIHASANTEI-AREA
           MOVE  TBL-RIHASANTEI-DAY   TO  WRK-CHECK-RIHASANTEI-DAY-OCC
           MOVE  WRK-RIHASANTEI-YMD   TO  WRK-CHECK-RIHASANTEI-YMD
           MOVE  WRK-RIHASANTEI-YMD3  TO  WRK-CHECK-RIHASANTEI-YMD3
      *
      *    Åö·î¤Î¥ê¥Ï¥Ó¥ê³«»ÏÆü¤è¤ê¸å¤ËÅö·î¤Î¥ê¥Ï¥Ó¥ê»»ÄêÆü¤¬¤Ê¤±¤ì¤Ð
      *    Åö·î¤Î¥ê¥Ï¥Ó¥ê³«»ÏÆü¤ò¥¯¥ê¥¢¤¹¤ë
           MOVE     ZERO       TO  FLG-OK
           PERFORM  VARYING  RIHA-IDX  FROM  31  BY  -1
                     UNTIL   RIHA-IDX    <    1
             IF     TB-RIHASANTEI-DAY(RIHA-IDX)  NOT = ZERO
               PERFORM   200111-RIHA-SANTEIPLUS2-SEC
               MOVE      ZERO  TO  FLG-OK
               PERFORM  VARYING  RIHA-IDX2  FROM  1  BY  1
                         UNTIL   RIHA-IDX2    >  31
                 IF    (TBL-HASSYO-TAIHI-SANTEI(RIHA-IDX2) NOT = ZERO)
                 AND   (RIHA-IDX  <=  RIHA-IDX2                      )
                 AND   (IDX43     >=  RIHA-IDX2                      )
                   MOVE  ZERO  TO  TBL-HASSYO-TAIHI-SANTEI(RIHA-IDX2)
                   MOVE  1     TO  FLG-OK
                 END-IF
               END-PERFORM
               IF   FLG-OK  =  ZERO
                 MOVE    ZERO  TO  TB-RIHASANTEI-DAY(RIHA-IDX)
               ELSE
                 IF  IDX43  NOT =  99
                   MOVE  1     TO  TB-RIHASANTEI-DAY-END2(IDX43)
                 END-IF
               END-IF
             END-IF
           END-PERFORM
      *
      *    Åö·î¤ÎÀèÆ¬¤Î¥ê¥Ï¥Ó¥ê³«»ÏÆü
           MOVE     ZERO           TO  RIHA-ST
           PERFORM  VARYING  RIHA-IDX  FROM   1  BY   1
                     UNTIL   RIHA-IDX    >   31
             IF     TB-RIHASANTEI-DAY(RIHA-IDX)  NOT = ZERO
               MOVE   RIHA-IDX     TO  RIHA-ST
               MOVE   31           TO  RIHA-IDX
             END-IF
           END-PERFORM
      *
      *    Åö·î¤Î¥ê¥Ï¥Ó¥ê³«»ÏÆü¤ËÅö·î¤Î½ªÎ»Æü¤ÎÆþÎÏ¡Ê»»ÄêÍúÎò¤ÇÆþÎÏ¡Ë
      *    ¤¬¤¢¤ê¡¢¤½¤ÎÃæ¤Ç°ìÈÖ·îËö¤Ë¶á¤¤Æü
           MOVE     99             TO  RIHA-ED
           PERFORM  VARYING  RIHA-IDX  FROM  31  BY  -1
                     UNTIL   RIHA-IDX    <    1
             IF     TB-RIHASANTEI-DAY-END2(RIHA-IDX)  NOT = ZERO
               MOVE   RIHA-IDX     TO  RIHA-ED
               MOVE   1            TO  RIHA-IDX
             END-IF
           END-PERFORM
      *
      *    Ä¾¶á¤Î¥ê¥Ï¥Ó¥ê³«»ÏÆü¤ËÆþÎÏ¤·¤¿Åö·î¤Î½ªÎ»Æü¡Ê»»ÄêÍúÎò¤ÇÆþÎÏ¡Ë
           MOVE     99             TO  RIHA-ED2
           PERFORM  VARYING  RIHA-IDX  FROM  31  BY  -1
                     UNTIL   RIHA-IDX    <    1
             IF     TB-RIHASANTEI-DAY-END3(RIHA-IDX)  NOT = ZERO
               MOVE   RIHA-IDX     TO  RIHA-ED2
               MOVE   1            TO  RIHA-IDX
             END-IF
           END-PERFORM
      *
      *    Åö·îÀèÆ¬¤Î¥ê¥Ï¥Ó¥ê³«»ÏÆü¤è¤êÁ°¤ËÅö·î¤Î¥ê¥Ï¥Ó¥ê»»ÄêÆü¤¬¤Ê¤±¤ì¤Ð
      *    Á°·î°ÊÁ°¤ÎÄ¾¶á¤Î¥ê¥Ï¥Ó¥ê³«»ÏÆü¤ò¥¯¥ê¥¢¤¹¤ë
           MOVE     ZERO           TO  FLG-OK
           IF    WRK-RIHASANTEI-YMD  NOT = SPACE
             IF  RIHA-ST             NOT = ZERO
               PERFORM  VARYING  RIHA-IDX  FROM  1  BY  1
                         UNTIL   RIHA-IDX    >  31
                 IF ((TBL-HASSYO-TAIHI-SANTEI(RIHA-IDX) NOT = ZERO) AND
                     (RIHA-ST   >  RIHA-IDX                       ))
                 OR ((TBL-HASSYO-TAIHI-SANTEI(RIHA-IDX) NOT = ZERO) AND
                     (RIHA-ED   <  RIHA-IDX                       ))
                   MOVE   1        TO  FLG-OK
                 END-IF
               END-PERFORM
               IF    FLG-OK  =  ZERO
                 MOVE     SPACE    TO  WRK-RIHASANTEI-YMD
                 MOVE     SPACE    TO  WRK-RIHASANTEI-YMD3
               ELSE
                 MOVE     ZERO     TO  FLG-OK
                 PERFORM  VARYING  RIHA-IDX  FROM  1  BY  1
                           UNTIL   RIHA-IDX    >  31
                   IF  (TBL-HASSYO-TAIHI-SANTEI(RIHA-IDX) NOT = ZERO)
                   AND (RIHA-ED2  <  RIHA-IDX                       )
                     MOVE   1      TO  FLG-OK
                   END-IF
                 END-PERFORM
                 IF    FLG-OK  =  ZERO
                   MOVE     SPACE  TO  WRK-RIHASANTEI-YMD3
                 END-IF
               END-IF
             ELSE
               PERFORM  VARYING  RIHA-IDX  FROM  1  BY  1
                         UNTIL   RIHA-IDX    >  31
                 IF  (TBL-HASSYO-TAIHI-SANTEI(RIHA-IDX) NOT = ZERO)
                 AND (RIHA-ED2  <  RIHA-IDX                       )
                   MOVE   1        TO  FLG-OK
                 END-IF
               END-PERFORM
               IF    FLG-OK  =  ZERO
                 MOVE     SPACE    TO  WRK-RIHASANTEI-YMD3
               END-IF
             END-IF
           END-IF
      *
           IF   (TBL-HASSYO-TAIHIYMD1-FLG  =  1)
           AND  (FLG-KYUSEI-COMMENT        =  1)
             INITIALIZE                     TBL-RIHASANTEI-DAY
             MOVE     SPACE            TO   WRK-RIHASANTEI-YMD
             MOVE     SPACE            TO   WRK-RIHASANTEI-YMD3
      *
             IF  (TBL-HASSYO-TAIHIYMD11(1) NOT = SPACE                )
             AND (TBL-HASSYO-TAIHIYMD11(1) = WRK-CHECK-RIHASANTEI-YMD
                                          OR WRK-CHECK-RIHASANTEI-YMD3)
                 CONTINUE
             ELSE
                 MOVE  SPACE  TO  TBL-HASSYO-TAIHIYMD11(1)
             END-IF
             IF  (TBL-HASSYO-TAIHIYMD11(2) NOT = SPACE                )
             AND (TBL-HASSYO-TAIHIYMD11(2) = WRK-CHECK-RIHASANTEI-YMD
                                          OR WRK-CHECK-RIHASANTEI-YMD3)
                 CONTINUE
             ELSE
                 MOVE  SPACE  TO  TBL-HASSYO-TAIHIYMD11(2)
             END-IF
      *
             IF  (TBL-HASSYO-TAIHIYMD11(1) NOT = SPACE)
             AND (TBL-HASSYO-TAIHIYMD11(2) NOT = SPACE)
               IF TBL-HASSYO-TAIHIYMD11(2) > TBL-HASSYO-TAIHIYMD11(1)
                 MOVE TBL-HASSYO-TAIHIYMD11(1) TO WRK-RIHASANTEI-YMD3
                 MOVE TBL-HASSYO-TAIHIYMD11(2) TO WRK-RIHASANTEI-YMD
               ELSE
                 MOVE TBL-HASSYO-TAIHIYMD11(2) TO WRK-RIHASANTEI-YMD3
                 MOVE TBL-HASSYO-TAIHIYMD11(1) TO WRK-RIHASANTEI-YMD
               END-IF
             ELSE
               IF TBL-HASSYO-TAIHIYMD11(1) NOT = SPACE
                 MOVE TBL-HASSYO-TAIHIYMD11(1) TO WRK-RIHASANTEI-YMD
               END-IF
               IF TBL-HASSYO-TAIHIYMD11(2) NOT = SPACE
                 MOVE TBL-HASSYO-TAIHIYMD11(2) TO WRK-RIHASANTEI-YMD
               END-IF
             END-IF
      *
             PERFORM  VARYING  RIHA-IDX  FROM  1  BY  1
                       UNTIL   RIHA-IDX    >  31
               IF    (TBL-HASSYO-TAIHIYMD12(RIHA-IDX)  NOT = ZERO)
               AND   (WRK-CHECK-RIHASANTEI-DAY(RIHA-IDX)   = ZERO)
                 MOVE  ZERO  TO  TBL-HASSYO-TAIHIYMD12(RIHA-IDX)
               END-IF
             END-PERFORM
             MOVE  TBL-HASSYO-TAIHIYMD1-DAY-OCC2
                             TO  TBL-RIHASANTEI-DAY
           END-IF
      *
           .
       20011-RIHABIRIHASSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ê¥Ï¥Ó¥ê¡¦»»ÄêÍúÎòÉÕ²ÃÈ½Äê
      *    ¡Ê¥ê¥Ï³«»ÏÆü¤Î½ªÎ»Æü¤È¿ÇÎÅ·î¤ò¥Á¥§¥Ã¥¯¡Ë¡ÊÍúÎòÈÖ¹æ¡á£°¤Î¥Ç¡¼¥¿¡Ë
      *****************************************************************
       200111-RIHA-SANTEIPLUS-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SANTEIPLUS-HANTEI
           MOVE    ZERO                TO  IDX43
      *
           INITIALIZE                      SANTEIPLUS-REC
           MOVE    RECE02-HOSPNUM      TO  SANTEIPLUS-HOSPNUM
           MOVE    RECE02-PTID         TO  SANTEIPLUS-PTID
           MOVE    SANTEI-SRYYM        TO  SANTEIPLUS-SRYYM
           MOVE    WRK-SRYCD           TO  SANTEIPLUS-SRYCD
           MOVE    ZERO                TO  SANTEIPLUS-NYUGAIKBN
           MOVE    ZERO                TO  SANTEIPLUS-SRYKA
           MOVE    WRK-RIHA-HKNCOMBI   TO  SANTEIPLUS-HKNCOMBINUM
           MOVE    IDX3                TO  SANTEIPLUS-DAYKEY
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC           =  ZERO
             MOVE  "tbl_santeiplus"    TO  MCP-TABLE
             MOVE  "key4"              TO  MCP-PATHNAME
             PERFORM  900-SANTEIPLUS-READ-SEC
             IF    FLG-SANTEIPLUS   =  ZERO
               MOVE  SANTEIPLUS-ENDYMD(7:2)  TO  IDX43
               IF  ((SANTEIPLUS-ENDYMD   NOT =  SPACE       ) AND
                    (SANTEIPLUS-ENDYMD(1:6)  <  RECE02-SRYYM))
               OR  ((SANTEIPLUS-ENDYMD   NOT =  SPACE       ) AND
                    (SANTEIPLUS-ENDYMD(1:6)  =  RECE02-SRYYM) AND
                    (IDX43                   <  RIHA-ST2    ))
                  MOVE     1           TO  FLG-SANTEIPLUS-HANTEI
               END-IF
             END-IF
           END-IF
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       200111-RIHA-SANTEIPLUS-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ê¥Ï¥Ó¥ê¡¦»»ÄêÍúÎòÉÕ²ÃÈ½Äê
      *    ¡Ê¥ê¥Ï³«»ÏÆü¤Î½ªÎ»Æü¤È¿ÇÎÅ·î¤ò¥Á¥§¥Ã¥¯¡Ë¡ÊÍúÎòÈÖ¹æ¡á£°¤Î¥Ç¡¼¥¿¡Ë
      *****************************************************************
       200111-RIHA-SANTEIPLUS2-SEC          SECTION.
      *
           MOVE    99                  TO  IDX43
      *
           INITIALIZE                      SANTEIPLUS-REC
           MOVE    RECE02-HOSPNUM      TO  SANTEIPLUS-HOSPNUM
           MOVE    RECE02-PTID         TO  SANTEIPLUS-PTID
           MOVE    RECE02-SRYYM        TO  SANTEIPLUS-SRYYM
           MOVE    WRK-SRYCD           TO  SANTEIPLUS-SRYCD
           MOVE    ZERO                TO  SANTEIPLUS-NYUGAIKBN
           MOVE    ZERO                TO  SANTEIPLUS-SRYKA
           MOVE    WRK-RIHA-HKNCOMBI   TO  SANTEIPLUS-HKNCOMBINUM
           MOVE    RIHA-IDX            TO  SANTEIPLUS-DAYKEY
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC           =  ZERO
             MOVE  "tbl_santeiplus"    TO  MCP-TABLE
             MOVE  "key4"              TO  MCP-PATHNAME
             PERFORM  900-SANTEIPLUS-READ-SEC
             IF    FLG-SANTEIPLUS   =  ZERO
               IF   (SANTEIPLUS-ENDYMD   NOT =  SPACE       )
               AND  (SANTEIPLUS-ENDYMD(1:6)  =  RECE02-SRYYM)
                 MOVE  SANTEIPLUS-ENDYMD(7:2)  TO  IDX43
               END-IF
             END-IF
           END-IF
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       200111-RIHA-SANTEIPLUS2-EXT.
           EXIT.
      *
      *****************************************************************
      *    È¯¾ÉÆü¥Á¥§¥Ã¥¯¡ÊÂè»°¼Ô¹Ô°Ù¡Ë
      *****************************************************************
       20011-KYUSEIYMD-CHECK-SEC          SECTION.
      *
           IF   TBL-HASSYO-SINDAI-FLG   =   2 OR 3
             MOVE     ZERO   TO    FLG-CHECK-OK2
             PERFORM  VARYING  IDX47  FROM  1  BY  1
                       UNTIL  (IDX47             >  50   )
                        OR    (TBL-HASSYO-SINDAI-KYUSEIYMD(IDX47)
                                                 =  SPACE)
                        OR    (FLG-CHECK-OK2     =  1    )
               IF  TBL-HASSYO-SINDAI-KYUSEIYMD(IDX47) = WRK-KYUSEIYMD
                 MOVE   1    TO    FLG-CHECK-OK2
               END-IF
             END-PERFORM
             IF    FLG-CHECK-OK2 = ZERO
               MOVE   WRK-KYUSEIYMD
                             TO    TBL-HASSYO-SINDAI-KYUSEIYMD(IDX47)
               MOVE     1    TO    FLG-CHECK-OK
             END-IF
           END-IF
      *
           IF   TBL-HASSYO-KOKYUKI-FLG  =   2 OR 3
             MOVE     ZERO   TO    FLG-CHECK-OK2
             PERFORM  VARYING  IDX47  FROM  1  BY  1
                       UNTIL  (IDX47             >  50   )
                        OR    (TBL-HASSYO-KOKYUKI-KYUSEIYMD(IDX47)
                                                 =  SPACE)
                        OR    (FLG-CHECK-OK2     =  1    )
               IF  TBL-HASSYO-KOKYUKI-KYUSEIYMD(IDX47) = WRK-KYUSEIYMD
                 MOVE   1    TO    FLG-CHECK-OK2
               END-IF
             END-PERFORM
             IF    FLG-CHECK-OK2 = ZERO
               MOVE   WRK-KYUSEIYMD
                             TO    TBL-HASSYO-KOKYUKI-KYUSEIYMD(IDX47)
               MOVE     1    TO    FLG-CHECK-OK
             END-IF
           END-IF
      *
           IF   TBL-HASSYO-NOUKE-FLG    =   2 OR 3
             MOVE     ZERO   TO    FLG-CHECK-OK2
             PERFORM  VARYING  IDX47  FROM  1  BY  1
                       UNTIL  (IDX47             >  50   )
                        OR    (TBL-HASSYO-NOUKE-KYUSEIYMD(IDX47)
                                                 =  SPACE)
                        OR    (FLG-CHECK-OK2     =  1    )
               IF  TBL-HASSYO-NOUKE-KYUSEIYMD(IDX47) = WRK-KYUSEIYMD
                 MOVE   1    TO    FLG-CHECK-OK2
               END-IF
             END-PERFORM
             IF    FLG-CHECK-OK2 = ZERO
               MOVE   WRK-KYUSEIYMD
                             TO    TBL-HASSYO-NOUKE-KYUSEIYMD(IDX47)
               MOVE     1    TO    FLG-CHECK-OK
             END-IF
           END-IF
      *
           IF   TBL-HASSYO-UNDOKI-FLG   =   2 OR 3
             MOVE     ZERO   TO    FLG-CHECK-OK2
             PERFORM  VARYING  IDX47  FROM  1  BY  1
                       UNTIL  (IDX47             >  50   )
                        OR    (TBL-HASSYO-UNDOKI-KYUSEIYMD(IDX47)
                                                 =  SPACE)
                        OR    (FLG-CHECK-OK2     =  1    )
               IF  TBL-HASSYO-UNDOKI-KYUSEIYMD(IDX47) = WRK-KYUSEIYMD
                 MOVE   1    TO    FLG-CHECK-OK2
               END-IF
             END-PERFORM
             IF    FLG-CHECK-OK2 = ZERO
               MOVE   WRK-KYUSEIYMD
                             TO    TBL-HASSYO-UNDOKI-KYUSEIYMD(IDX47)
               MOVE     1    TO    FLG-CHECK-OK
             END-IF
           END-IF
      *
           IF   TBL-HASSYO-NANBYO-FLG   =   2 OR 3
             MOVE     ZERO   TO    FLG-CHECK-OK2
             PERFORM  VARYING  IDX47  FROM  1  BY  1
                       UNTIL  (IDX47             >  50   )
                        OR    (TBL-HASSYO-NANBYO-KYUSEIYMD(IDX47)
                                                 =  SPACE)
                        OR    (FLG-CHECK-OK2     =  1    )
               IF  TBL-HASSYO-NANBYO-KYUSEIYMD(IDX47) = WRK-KYUSEIYMD
                 MOVE   1    TO    FLG-CHECK-OK2
               END-IF
             END-PERFORM
             IF    FLG-CHECK-OK2 = ZERO
               MOVE   WRK-KYUSEIYMD
                             TO    TBL-HASSYO-NANBYO-KYUSEIYMD(IDX47)
               MOVE     1    TO    FLG-CHECK-OK
             END-IF
           END-IF
      *
           IF   TBL-HASSYO-SYOGAI-FLG   =   2 OR 3
             MOVE     ZERO   TO    FLG-CHECK-OK2
             PERFORM  VARYING  IDX47  FROM  1  BY  1
                       UNTIL  (IDX47             >  50   )
                        OR    (TBL-HASSYO-SYOGAI-KYUSEIYMD(IDX47)
                                                 =  SPACE)
                        OR    (FLG-CHECK-OK2     =  1    )
               IF  TBL-HASSYO-SYOGAI-KYUSEIYMD(IDX47) = WRK-KYUSEIYMD
                 MOVE   1    TO    FLG-CHECK-OK2
               END-IF
             END-PERFORM
             IF    FLG-CHECK-OK2 = ZERO
               MOVE   WRK-KYUSEIYMD
                             TO    TBL-HASSYO-SYOGAI-KYUSEIYMD(IDX47)
               MOVE     1    TO    FLG-CHECK-OK
             END-IF
           END-IF
      *
           IF   TBL-HASSYO-SESSYOKU-FLG =   2 OR 3
             MOVE     ZERO   TO    FLG-CHECK-OK2
             PERFORM  VARYING  IDX47  FROM  1  BY  1
                       UNTIL  (IDX47             >  50   )
                        OR    (TBL-HASSYO-SESSYOKU-KYUSEIYMD(IDX47)
                                                 =  SPACE)
                        OR    (FLG-CHECK-OK2     =  1    )
               IF  TBL-HASSYO-SESSYOKU-KYUSEIYMD(IDX47) = WRK-KYUSEIYMD
                 MOVE   1    TO    FLG-CHECK-OK2
               END-IF
             END-PERFORM
             IF    FLG-CHECK-OK2 = ZERO
               MOVE   WRK-KYUSEIYMD
                             TO    TBL-HASSYO-SESSYOKU-KYUSEIYMD(IDX47)
               MOVE     1    TO    FLG-CHECK-OK
             END-IF
           END-IF
      *
           IF   TBL-HASSYO-HAIYO-FLG    =   2 OR 3
             MOVE     ZERO   TO    FLG-CHECK-OK2
             PERFORM  VARYING  IDX47  FROM  1  BY  1
                       UNTIL  (IDX47             >  50   )
                        OR    (TBL-HASSYO-HAIYO-KYUSEIYMD(IDX47)
                                                 =  SPACE)
                        OR    (FLG-CHECK-OK2     =  1    )
               IF  TBL-HASSYO-HAIYO-KYUSEIYMD(IDX47) = WRK-KYUSEIYMD
                 MOVE   1    TO    FLG-CHECK-OK2
               END-IF
             END-PERFORM
             IF    FLG-CHECK-OK2 = ZERO
               MOVE   WRK-KYUSEIYMD
                             TO    TBL-HASSYO-HAIYO-KYUSEIYMD(IDX47)
               MOVE     1    TO    FLG-CHECK-OK
             END-IF
           END-IF
      *
      *****
      *
           IF   TBL-HASSYO9-SINDAI-FLG  =   2 OR 3
             MOVE     ZERO   TO    FLG-CHECK-OK2
             PERFORM  VARYING  IDX47  FROM  1  BY  1
                       UNTIL  (IDX47             >  50   )
                        OR    (TBL-HASSYO9-SINDAI-KYUSEIYMD(IDX47)
                                                 =  SPACE)
                        OR    (FLG-CHECK-OK2     =  1    )
               IF  TBL-HASSYO9-SINDAI-KYUSEIYMD(IDX47) = WRK-KYUSEIYMD
                 MOVE   1    TO    FLG-CHECK-OK2
               END-IF
             END-PERFORM
             IF    FLG-CHECK-OK2 = ZERO
               MOVE   WRK-KYUSEIYMD
                             TO    TBL-HASSYO9-SINDAI-KYUSEIYMD(IDX47)
               MOVE     1    TO    FLG-CHECK-OK
             END-IF
           END-IF
      *
           IF   TBL-HASSYO9-HAIYO-FLG   =   2 OR 3
             MOVE     ZERO   TO    FLG-CHECK-OK2
             PERFORM  VARYING  IDX47  FROM  1  BY  1
                       UNTIL  (IDX47             >  50   )
                        OR    (TBL-HASSYO9-HAIYO-KYUSEIYMD(IDX47)
                                                 =  SPACE)
                        OR    (FLG-CHECK-OK2     =  1    )
               IF  TBL-HASSYO9-HAIYO-KYUSEIYMD(IDX47) = WRK-KYUSEIYMD
                 MOVE   1    TO    FLG-CHECK-OK2
               END-IF
             END-PERFORM
             IF    FLG-CHECK-OK2 = ZERO
               MOVE   WRK-KYUSEIYMD
                             TO    TBL-HASSYO9-HAIYO-KYUSEIYMD(IDX47)
               MOVE     1    TO    FLG-CHECK-OK
             END-IF
           END-IF
      *
           IF   TBL-HASSYO9-KOKYUKI-FLG =   2 OR 3
             MOVE     ZERO   TO    FLG-CHECK-OK2
             PERFORM  VARYING  IDX47  FROM  1  BY  1
                       UNTIL  (IDX47             >  50   )
                        OR    (TBL-HASSYO9-KOKYUKI-KYUSEIYMD(IDX47)
                                                 =  SPACE)
                        OR    (FLG-CHECK-OK2     =  1    )
               IF  TBL-HASSYO9-KOKYUKI-KYUSEIYMD(IDX47) = WRK-KYUSEIYMD
                 MOVE   1    TO    FLG-CHECK-OK2
               END-IF
             END-PERFORM
             IF    FLG-CHECK-OK2 = ZERO
               MOVE   WRK-KYUSEIYMD
                             TO    TBL-HASSYO9-KOKYUKI-KYUSEIYMD(IDX47)
               MOVE     1    TO    FLG-CHECK-OK
             END-IF
           END-IF
      *
           .
       20011-KYUSEIYMD-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½é¿Ç¡ÊÆ±ÆüÊ£¿ô¿ÇÎÅ²Ê¡Ë¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-SYODOU-HANTEI-SEC          SECTION.
      *
           IF      RECE04-FLG-SYODOU-COMMENT   =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE      "1"       TO     WRK-JIDO-SETCOMFLG
                MOVE       4        TO     WRK-JIDO-SETCOMATFLG
                MOVE       99       TO     FLG-COMMENT
                PERFORM    20011-SYODOU-COMMENT-SEC
           END-IF
      *
           .
       2001-SYODOU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½é¿Ç¡ÊÆ±ÆüÊ£¿ô¿ÇÎÅ²Ê¡Ë¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       20011-SYODOU-COMMENT-SEC          SECTION.
      *
           MOVE       ZERO       TO   IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                      UNTIL   (IDX                       >   10  )
                       OR     (RECE04-SYODOU-SRYKA(IDX)  =  SPACE)
      *
             ADD      1                  TO  IDZ
             MOVE    "Ê£½é»»Äê¿ÇÎÅ²Ê¡§"  TO  WK-FREE-NAME2(IDZ)(1:16)
      *      ¿ÇÎÅ²Ê¤Î¼èÆÀ
             INITIALIZE                      SYS-1005-REC
             MOVE    "1005"              TO  SYS-1005-KANRICD
             MOVE    RECE04-SYODOU-SRYKA(IDX)
                                         TO  SYS-1005-KBNCD
             MOVE    RECE02-SRYYM        TO  SYS-1005-STYUKYMD(1:6)
             MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
             MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
grpsys       MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
             MOVE    SYS-1005-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
grpsys       PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI      =   ZERO
                  MOVE  SYS-1005-SRYKANAME(1:40)
                                         TO  WK-FREE-NAME2(IDZ)(17:40)
               END-IF
             END-IF
      *
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
               MOVE  "2"                   TO  WK-FREE-MEISKYFLG2(IDZ)
               MOVE  WRK-JIDO-SETCOMATFLG  TO  WK-FREE-SETCOMATFLG2(IDZ)
             END-IF
      *
             PERFORM   VARYING   IDW   FROM   300   BY  -1
                        UNTIL    IDW   <   1
                IF   WK-FREE-NAME2(IDZ)(IDW:1)  NOT =  SPACE
                     COMPUTE WK-FREE-YUKOKETA2(IDZ) =  IDW  /   2
                     MOVE    1   TO    IDW
                END-IF
             END-PERFORM
      *
           END-PERFORM
      *
           PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           .
       20011-SYODOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºÆ¿Ç¡ÊÆ±°ìÆü£²²ÊÌÜ¡Ë¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-SAIDOU-HANTEI-SEC          SECTION.
      *
           IF      RECE04-FLG-SAIDOU-COMMENT   =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE      "1"       TO     WRK-JIDO-SETCOMFLG
                MOVE       25       TO     WRK-JIDO-SETCOMATFLG
                MOVE       99       TO     FLG-COMMENT
                PERFORM    20011-SAIDOU-COMMENT-SEC
           END-IF
      *
           .
       2001-SAIDOU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºÆ¿Ç¡ÊÆ±°ìÆü£²²ÊÌÜ¡Ë¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       20011-SAIDOU-COMMENT-SEC          SECTION.
      *
           MOVE       ZERO       TO   IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                      UNTIL   (IDX                       >   10  )
                       OR     (RECE04-SAIDOU-SRYKA(IDX)  =  SPACE)
      *
             ADD      1                  TO  IDZ
             MOVE    "Ê£ºÆ»»Äê¿ÇÎÅ²Ê¡§"  TO  WK-FREE-NAME2(IDZ)(1:16)
      *      ¿ÇÎÅ²Ê¤Î¼èÆÀ
             INITIALIZE                      SYS-1005-REC
             MOVE    "1005"              TO  SYS-1005-KANRICD
             MOVE    RECE04-SAIDOU-SRYKA(IDX)
                                         TO  SYS-1005-KBNCD
             MOVE    RECE02-SRYYM        TO  SYS-1005-STYUKYMD(1:6)
             MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
             MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
grpsys       MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
             MOVE    SYS-1005-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
grpsys       PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI      =   ZERO
                  MOVE  SYS-1005-SRYKANAME(1:40)
                                         TO  WK-FREE-NAME2(IDZ)(17:40)
               END-IF
             END-IF
      *
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
               MOVE  "2"                   TO  WK-FREE-MEISKYFLG2(IDZ)
               MOVE  WRK-JIDO-SETCOMATFLG  TO  WK-FREE-SETCOMATFLG2(IDZ)
             END-IF
      *
             PERFORM   VARYING   IDW   FROM   300   BY  -1
                        UNTIL    IDW   <   1
                IF   WK-FREE-NAME2(IDZ)(IDW:1)  NOT =  SPACE
                     COMPUTE WK-FREE-YUKOKETA2(IDZ) =  IDW  /   2
                     MOVE    1   TO    IDW
                END-IF
             END-PERFORM
      *
           END-PERFORM
      *
           PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           .
       20011-SAIDOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ª¥ó¥é¥¤¥ó¿ÇÎÅÎÁ¤¬¤¢¤ë¤«È½Äê
      *    ¥ª¥ó¥é¥¤¥ó°å³Ø´ÉÍýÎÁ¤¬¤¢¤ë¤«È½Äê
      *    ¥ª¥ó¥é¥¤¥óºßÂð´ÉÍýÎÁ¡Êºß°åÁí´É¡Ê·î£±²óË¬Ìä¿ÇÎÅ¡Ë¡Ë¤¬¤¢¤ë¤«È½Äê
      *    Àº¿À²Ê¥ª¥ó¥é¥¤¥óºßÂð´ÉÍýÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-ONLINE-HANTEI-SEC          SECTION.
      *
           IF      RECE04-FLG-ONLINE-COMMENT   NOT =  SPACE
             MOVE      ZERO         TO     IDZ
             INITIALIZE                    WRK-FREE-AREA2
             IF  (RECE04-FLG-NINRYO-COMMENT   =
                 "A" OR "B" OR "G" OR "H" OR "C" OR "D" OR
                 "E" OR "F" OR "I" OR "J" OR "P" OR "Q"       )
             AND (RECE04-FLG-ONLINE-COMMENT   =  "4"          )
               PERFORM   2001-NINRYO-HENSYU-SEC
               IF   WRK-NINRYO-FSANTEIYMD   NOT =  SPACE
                 INITIALIZE                WRK-JIDO-SETCOM-AREA
                 MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                 MOVE       19      TO     WRK-JIDO-SETCOMATFLG
                 MOVE       99      TO     FLG-COMMENT
      *
                 ADD        1       TO     IDZ
      *
                 MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                 MOVE   "½é²ó»»Äê¡¡¡¡¡¡·î¡¡¡¡Æü"
                                    TO     WK-FREE-NAME2(IDZ)
                 MOVE   WRK-NINRYO-FSANTEIYMD(5:2)   TO  DAY-IDX2
                 MOVE   TBL-DAY(DAY-IDX2)
                                    TO     WK-FREE-NAME2(IDZ)(11:4)
                 MOVE   WRK-NINRYO-FSANTEIYMD(7:2)   TO  DAY-IDX2
                 MOVE   TBL-DAY(DAY-IDX2)
                                    TO     WK-FREE-NAME2(IDZ)(17:4)
      *
                 IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                   MOVE  "2"        TO  WK-FREE-MEISKYFLG2(IDZ)
                   MOVE  WRK-JIDO-SETCOMATFLG
                                    TO  WK-FREE-SETCOMATFLG2(IDZ)
                 END-IF
               END-IF
             END-IF
      *
             MOVE  SPACE            TO  WRK-ONLINE-COMCD
             MOVE  SPACE            TO  WRK-ONLINE-FSANTEIYM
             INITIALIZE                 ORCSRECESRCHKAREA
             MOVE  RECE02-PTID      TO  LNK-RECESRCHK-PTID
             MOVE  RECE02-SRYYM     TO  LNK-RECESRCHK-SRYYM
             MOVE  RECE04-FLG-ONLINE-COMMENT
                                    TO  LNK-RECESRCHK-CHKKBN
             IF    RECE04-FLG-ONLINE-COMMENT       =  "1"
             AND   RECE04-FLG-ONLINE2-COMMENT  NOT =  ZERO
                MOVE  RECE04-FLG-ONLINE2-COMMENT
                                    TO  LNK-RECESRCHK-CHKNO
             END-IF
             CALL  "ORCSRECESRCHK"  USING  SPA-AREA
                                           ORCSRECESRCHKAREA
             IF    LNK-RECESRCHK-RC  =  ZERO
                MOVE  LNK-RECESRCHK-COMCD
                                    TO  WRK-ONLINE-COMCD
                MOVE  LNK-RECESRCHK-FSANTEIYMD
                                    TO  WRK-ONLINE-FSANTEIYM
             END-IF
      *
             IF   WRK-ONLINE-COMCD   NOT =  SPACE
               IF   RECE04-FLG-ONLINE-COMMENT   =   "1"
               AND  RECE04-FLG-ONLINE2-COMMENT  =   ZERO
      *
                  MOVE  WRK-ONLINE-COMCD
                                       TO  WRK-TENSU-SRYCD
                  MOVE  RECE02-SRYYM   TO  WRK-TENSU-SRYYM
                  PERFORM      900-TENSU-TBL-SEC
      *
                  INITIALIZE               WRK-JIDO-SETCOM-AREA
                  MOVE      "1"        TO  WRK-JIDO-SETCOMFLG
                  MOVE       38        TO  WRK-JIDO-SETCOMATFLG
                  MOVE       99        TO  FLG-COMMENT
      *
                  ADD        1         TO  IDZ
      *
                  MOVE  TNS-YUKOKETA   TO  WK-FREE-YUKOKETA2(IDZ)
                  MOVE  TNS-NAME       TO  WK-FREE-NAME2(IDZ)
      *
                  IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                    MOVE  "2"          TO  WK-FREE-MEISKYFLG2(IDZ)
                    MOVE  WRK-JIDO-SETCOMATFLG
                                       TO  WK-FREE-SETCOMATFLG2(IDZ)
                  END-IF
      *
               END-IF
      *
               IF   RECE04-FLG-ONLINE3-COMMENT  =   SPACE
      *
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                EVALUATE  RECE04-FLG-ONLINE-COMMENT
                  WHEN      "1"
                  WHEN      "2"
                  WHEN      "3"
                    MOVE    38     TO     WRK-JIDO-SETCOMATFLG
                  WHEN      "4"
                    MOVE    39     TO     WRK-JIDO-SETCOMATFLG
                END-EVALUATE
                MOVE       99      TO     FLG-COMMENT
      *
                ADD        1       TO     IDZ
      *
                EVALUATE  RECE04-FLG-ONLINE-COMMENT
                  WHEN      "1"
                  WHEN      "2"
                    IF   RECE02-SRYYM >= "201905"
                      MOVE       19      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "´ÉÍýÎÁÅù»»Äê³«»ÏÇ¯·î¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î"
                                         TO     WK-FREE-NAME2(IDZ)
                      MOVE       27      TO     IDX41
                      MOVE       33      TO     IDX42
                      MOVE       23      TO     IDX49
                    ELSE
                      MOVE       17      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "´ÉÍýÎÁÅù»»Äê³«»ÏÇ¯·î¡¡¡¡¡¡Ç¯¡¡¡¡·î"
                                         TO     WK-FREE-NAME2(IDZ)
                      MOVE       23      TO     IDX41
                      MOVE       29      TO     IDX42
                    END-IF
                  WHEN      "3"
                  WHEN      "4"
                    IF   RECE02-SRYYM >= "201905"
                      MOVE       18      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "´ÉÍýÎÁ»»Äê³«»ÏÇ¯·î¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î"
                                         TO     WK-FREE-NAME2(IDZ)
                      MOVE       25      TO     IDX41
                      MOVE       31      TO     IDX42
                      MOVE       21      TO     IDX49
                    ELSE
                      MOVE       16      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "´ÉÍýÎÁ»»Äê³«»ÏÇ¯·î¡¡¡¡¡¡Ç¯¡¡¡¡·î"
                                         TO     WK-FREE-NAME2(IDZ)
                      MOVE       21      TO     IDX41
                      MOVE       27      TO     IDX42
                    END-IF
                END-EVALUATE
      *
                INITIALIZE                          STS-AREA-DAY
                INITIALIZE                          LNK-DAY2-AREA
                MOVE    "21"                    TO  LNK-DAY2-IRAI
                MOVE    WRK-ONLINE-FSANTEIYM    TO  LNK-DAY2-YMD
                CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                               LNK-DAY2-AREA
                IF      STS-DAY-RC1            =   ZERO
                   MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                   END-IF
      *
                   MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                   END-IF
      *
                   IF   RECE02-SRYYM >= "201905"
                      MOVE    LNK-DAY2-EDTYMD2-G
                                        TO  WK-FREE-NAME2(IDZ)(IDX49:4)
                   END-IF
                END-IF
      *
                IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                  MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                  MOVE  WRK-JIDO-SETCOMATFLG
                                   TO     WK-FREE-SETCOMATFLG2(IDZ)
                END-IF
      *
               END-IF
             END-IF
      *
             IF   IDZ    NOT =  ZERO
                PERFORM  200111-RECE05-TAIHI-SEC
      *
      *         £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
                MOVE    ZERO                 TO  IDX-RECE05
                MOVE    SPACE                TO  WRK-RECE05XX-REC
                INITIALIZE                       WRK-RECE05XX-REC
      *
                MOVE    RECE04-MEISAISU      TO  MAX-MEISAISU
      *
                PERFORM   VARYING   IDX   FROM    1   BY  1
                           UNTIL  ( IDX                >   50    )
                            OR    ( WK-FREE-NAME2(IDX) =   SPACE )
                  COMPUTE  MAX-MEISAISU  =   MAX-MEISAISU  +  1
                  IF   (IDX                         =    1  )
                  AND  (WRK-ONLINE-COMCD        NOT =  SPACE)
                  AND  (RECE04-FLG-ONLINE-COMMENT   =   "1" )
                  AND  (RECE04-FLG-ONLINE2-COMMENT  =   ZERO)
                    MOVE  WRK-ONLINE-COMCD
                          TO  WRK-RECE05XX-SRYCD(MAX-MEISAISU 1)
                    MOVE  WK-FREE-NAME2(IDX)
                          TO  WRK-RECE05XX-INPUTCOMENT(MAX-MEISAISU 1)
                  ELSE
                    MOVE  "810000001"
                          TO  WRK-RECE05XX-SRYCD(MAX-MEISAISU 1)
                  END-IF
                  MOVE  ZERO
                          TO  WRK-RECE05XX-SRYSURYO(MAX-MEISAISU 1)
                  MOVE  WK-FREE-YUKOKETA2(IDX)
                          TO  WRK-RECE05XX-YUKOKETA(MAX-MEISAISU 1)
                  MOVE  WK-FREE-NAME2(IDX)
                          TO  WRK-RECE05XX-NAME(MAX-MEISAISU 1)
                  MOVE  WK-FREE-MEISKYFLG2(IDX)
                          TO  WRK-RECE05XX-MEISKYFLG(MAX-MEISAISU 1)
                  MOVE  WK-FREE-SETCOMATFLG2(IDX)
                          TO  WRK-RECE05XX-SETCOMATFLG(MAX-MEISAISU 1)
                END-PERFORM
      *
                PERFORM    200111-SRYACTINF-HENSYU-SEC
             END-IF
           END-IF
      *
           .
       2001-ONLINE-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ËýÀ­áÖÄË¼À´µ´ÉÍýÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-SHIDOU-HANTEI-SEC          SECTION.
      *
           IF   RECE04-FLG-SHIDOU-COMMENT   =    "1"
      *
             MOVE     ZERO     TO     FLG-SHIDOU-COMMENT
      *
      *      »»ÄêÍúÎò¸¡º÷
             INITIALIZE                      SANTEI-REC
             MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE02-PTID         TO  SANTEI-PTID
             MOVE    RECE02-SRYYM        TO  SANTEI-SRYYM
             MOVE    CONS-MANSEI         TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  WRK-MCP-TABLE
             MOVE    "key"               TO  WRK-MCP-PATHNAME
             PERFORM  900-SANTEI-INV-SEC
             IF       FLG-SANTEI        =   ZERO
               IF     SANTEI-FSANTEIYMD(1:6)  =  RECE02-SRYYM
                  MOVE    1    TO    FLG-SHIDOU-COMMENT
               END-IF
             END-IF
      *
             IF      FLG-SHIDOU-COMMENT    =      1
                 MOVE       99      TO     FLG-COMMENT
                 PERFORM    20011-SHIDOU-COMMENT-SEC
             END-IF         
      *
           END-IF
      *
           .
       2001-SHIDOU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ËýÀ­áÖÄË¼À´µ´ÉÍýÎÁ¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       20011-SHIDOU-COMMENT-SEC          SECTION.
      *
           PERFORM   200111-SANTEI-DAY-SEC
      *
           MOVE       RECE04-REC    TO   RECE049-REC
           ADD     1                TO   RECE049-MEISAISU
           MOVE   "1"               TO   RECE049-SETCOMFLG
           WRITE      RECE049-REC
           ADD     1                TO   CNT-RECE049
      *
           PERFORM 800-RECE04-SET05-SEC
           MOVE    RECE04-ZAINUM       TO  RECE05-ZAINUM
           START   RECE05-FILE
                 KEY IS    >=   RECE05-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE05
                 NOT INVALID
                     PERFORM 990-RECE05-NEXT-SEC
           END-START
      *
           MOVE    RECE05-REC           TO  RECE059-REC
           MOVE    RECE05-SRYACT-TBL(1)  TO  RECE059-SRYACT-TBL(1)
           MOVE    RECE05-SRYACT-TBL(2)  TO  RECE059-SRYACT-TBL(3)
           MOVE    RECE05-SRYACT-TBL(3)  TO  RECE059-SRYACT-TBL(4)
           MOVE    RECE05-SRYACT-TBL(4)  TO  RECE059-SRYACT-TBL(5)
      *
           MOVE    SPACE                TO  RECE059-SRYACT-TBL(2)
           INITIALIZE                       RECE059-SRYACT-TBL(2)
           MOVE    "810000001"          TO  RECE059-SRYCD(2)
           MOVE    ZERO                 TO  RECE059-SRYSURYO(2)
           MOVE    15                   TO  RECE059-YUKOKETA(2)
           MOVE    "ËýÀ­áÖÄË¼À´µ´ÉÍý»»ÄêÆü¡¡¡¡¡¡Æü"
                                        TO  RECE059-NAME(2)
           MOVE     TB-SANTEI-DAY(1)    TO  DAY-IDX
           MOVE     TBL-DAY(DAY-IDX)    TO  RECE059-NAME(2)(25:4)
           MOVE    "2"                  TO  RECE059-MEISKYFLG(2)
           MOVE     5                   TO  RECE059-SETCOMATFLG(2)
           WRITE    RECE059-REC
           ADD      1                    TO  CNT-RECE059
      *
           .
       20011-SHIDOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ýÉÂ¼êÅö¶â°Õ¸«½ñ¸òÉÕÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-SYOBYO-HANTEI-SEC          SECTION.
      *
           IF      RECE04-FLG-SYOBYO-COMMENT   =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE       6       TO     WRK-JIDO-SETCOMATFLG
                MOVE       4       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-SYOBYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÎÅÍÜÈñÆ±°Õ½ñ¸òÉÕÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-RYOYO-HANTEI-SEC          SECTION.
      *
           IF      RECE04-FLG-RYOYO-COMMENT    =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE       7       TO     WRK-JIDO-SETCOMATFLG
                MOVE       4       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-RYOYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ë¥³¥Á¥ó°ÍÂ¸¾É´ÉÍýÎÁ¡¦½ÅÅÙÓÃÂ©´µ¼Ô¼£ÎÅ´ÉÍý²Ã»»¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-NIKOCHINTOU-HANTEI-SEC          SECTION.
      *
           IF      RECE04-FLG-NIKOCHIN-COMMENT   =    "1"
           OR      RECE04-FLG-ZENSOKU-COMMENT    =    "1"
      *
             IF    RECE04-FLG-NIKOCHIN-COMMENT   =    "1"
                 MOVE   CONS-NIKOCHIN  TO  WRK-SRYCD
             END-IF
             IF    RECE04-FLG-ZENSOKU-COMMENT    =    "1"
                 MOVE   CONS-ZENSOKU   TO  WRK-SRYCD
             END-IF
      *
             MOVE     ZERO     TO     FLG-NIKOCHINTOU-COMMENT
             MOVE     SPACE    TO     WRK-NIKOCHINTOU-YMD
      *
      *      »»ÄêÍúÎò¸¡º÷
             INITIALIZE                  SANTEI-REC
grpsys       MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE02-PTID         TO  SANTEI-PTID
             MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
grpsys       PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"        TO  MCP-TABLE
                 MOVE    "key3"              TO  MCP-PATHNAME
                 PERFORM        900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI              =   1 )
                             OR   ( FLG-NIKOCHINTOU-COMMENT =   1 )
      *
                   IF   SANTEI-SRYYM  <=  RECE02-SRYYM
                    MOVE  SANTEI-SRYYM     TO  WRK-NIKOCHINTOU-YMD(1:6)
                    MOVE  SANTEI-MSANTEID  TO  WRK-NIKOCHINTOU-YMD(7:2)
                    MOVE    1              TO  FLG-NIKOCHINTOU-COMMENT
                   END-IF
      *
                   MOVE    "tbl_santei"    TO  MCP-TABLE
                   MOVE    "key3"          TO  MCP-PATHNAME
                   PERFORM        900-SANTEI-READ-SEC
                 END-PERFORM
             END-IF
      *
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             IF      FLG-NIKOCHINTOU-COMMENT    =      1
                 INITIALIZE            WRK-JIDO-SETCOM-AREA
                 MOVE      "1"       TO     WRK-JIDO-SETCOMFLG
                 IF    RECE04-FLG-NIKOCHIN-COMMENT   =    "1"
                   MOVE     8        TO     WRK-JIDO-SETCOMATFLG
                 END-IF
                 IF    RECE04-FLG-ZENSOKU-COMMENT    =    "1"
                   MOVE     9        TO     WRK-JIDO-SETCOMATFLG
                 END-IF
                 MOVE       99       TO     FLG-COMMENT
                 PERFORM    20011-NIKOCHINTOU-COMMENT-SEC
             END-IF
      *
           END-IF
      *
           .
       2001-NIKOCHINTOU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ë¥³¥Á¥ó°ÍÂ¸¾É´ÉÍýÎÁ¡¦½ÅÅÙÓÃÂ©´µ¼Ô¼£ÎÅ´ÉÍý²Ã»»¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       20011-NIKOCHINTOU-COMMENT-SEC          SECTION.
      *
           MOVE       ZERO       TO   IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           ADD      1            TO  IDZ
           IF    RECE04-FLG-NIKOCHIN-COMMENT   =    "1"
                 MOVE    "½é²ó»»Äê¡¡¡¡¡¡·î¡¡¡¡Æü"
                                 TO  WK-FREE-NAME2(IDZ)(1:22)
                 MOVE    WRK-NIKOCHINTOU-YMD(5:2)   TO  DAY-IDX2
                 MOVE    TBL-DAY(DAY-IDX2)
                                 TO  WK-FREE-NAME2(IDZ)(11:4)
                 MOVE    WRK-NIKOCHINTOU-YMD(7:2)   TO  DAY-IDX2
                 MOVE    TBL-DAY(DAY-IDX2)
                                 TO  WK-FREE-NAME2(IDZ)(17:4)
                 MOVE    11      TO  WK-FREE-YUKOKETA2(IDZ)
           END-IF
           IF    RECE04-FLG-ZENSOKU-COMMENT    =    "1"
                 MOVE    "Âè£±²óÌÜ¼£ÎÅ´ÉÍý¡¡¡¡¡¡·î¡¡¡¡Æü"
                                 TO  WK-FREE-NAME2(IDZ)(1:30)
                 MOVE    WRK-NIKOCHINTOU-YMD(5:2)   TO  DAY-IDX2
                 MOVE    TBL-DAY(DAY-IDX2)
                                 TO  WK-FREE-NAME2(IDZ)(19:4)
                 MOVE    WRK-NIKOCHINTOU-YMD(7:2)   TO  DAY-IDX2
                 MOVE    TBL-DAY(DAY-IDX2)
                                 TO  WK-FREE-NAME2(IDZ)(25:4)
                 MOVE    15      TO  WK-FREE-YUKOKETA2(IDZ)
           END-IF
      *
           IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
             MOVE  "2"                   TO  WK-FREE-MEISKYFLG2(IDZ)
             MOVE  WRK-JIDO-SETCOMATFLG  TO  WK-FREE-SETCOMATFLG2(IDZ)
           END-IF
      *
           PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           .
       20011-NIKOCHINTOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ¡Ê£±²óÌÜ¡Ë¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-SYOUNI-HANTEI-SEC          SECTION.
      *
           IF  ( RECE04-FLG-SYOUNI-COMMENT   =    "1"   )
           OR  ((RECE04-FLG-SYOUNI-COMMENT   =    "2"  ) AND
                (RECE02-SYOUNIFLG            =    SPACE))
      *
             MOVE        ZERO        TO      FLG-SYOUNI-COMMENT
      *
      *      »»ÄêÍúÎò¸¡º÷
             INITIALIZE                      SANTEI-REC
             MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE02-PTID         TO  SANTEI-PTID
             MOVE    CONS-SYOUNI         TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"      TO  MCP-TABLE
               MOVE    "key3"            TO  MCP-PATHNAME
               PERFORM  900-SANTEI-READ-SEC
               PERFORM  UNTIL (FLG-SANTEI         =  1)
                         OR   (FLG-SYOUNI-COMMENT =  1)
                 IF   SANTEI-SRYYM  <=  RECE02-SRYYM
                   MOVE  SANTEI-FSANTEIYMD
                                    TO  WRK-SYOUNI-FSANTEIYMD
                   MOVE     1       TO  FLG-SYOUNI-COMMENT
                 END-IF
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
               END-PERFORM
             END-IF
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM  900-CLOSE-SEC
      *
             IF      FLG-SYOUNI-COMMENT    =      1
                 INITIALIZE            WRK-JIDO-SETCOM-AREA
                 MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                 MOVE       10      TO     WRK-JIDO-SETCOMATFLG
                 MOVE       99      TO     FLG-COMMENT
                 PERFORM    20011-SYOUNI-COMMENT-SEC
             END-IF
      *
           END-IF
      *
           .
       2001-SYOUNI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ½é²ó»»ÄêÆüÊÔ½¸
      *****************************************************************
       20011-SYOUNI-COMMENT-SEC          SECTION.
      *
           MOVE       ZERO       TO  IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           ADD      1            TO  IDZ
      *
           IF   RECE02-SRYYM >= "201905"
             MOVE       23      TO     WK-FREE-YUKOKETA2(IDZ)
             MOVE   "Âè£±²óÌÜ¥«¥¦¥ó¥»¥ê¥ó¥°¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                                TO     WK-FREE-NAME2(IDZ)
             MOVE       29      TO     IDX41
             MOVE       35      TO     IDX42
             MOVE       41      TO     IDX48
             MOVE       25      TO     IDX49
           ELSE
             MOVE       21      TO     WK-FREE-YUKOKETA2(IDZ)
             MOVE   "Âè£±²óÌÜ¥«¥¦¥ó¥»¥ê¥ó¥°¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                                TO     WK-FREE-NAME2(IDZ)
             MOVE       25      TO     IDX41
             MOVE       31      TO     IDX42
             MOVE       37      TO     IDX48
           END-IF
      *
           INITIALIZE                         STS-AREA-DAY
           INITIALIZE                         LNK-DAY2-AREA
           MOVE    "21"                   TO  LNK-DAY2-IRAI
           MOVE    WRK-SYOUNI-FSANTEIYMD  TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                          LNK-DAY2-AREA
           IF      STS-DAY-RC1            =   ZERO
              MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX41:4)
              END-IF
      *
              MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX42:4)
              END-IF
      *
              MOVE    LNK-DAY2-EDTYMD1-D   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX48:4)
              END-IF
      *
              IF   RECE02-SRYYM >= "201905"
                 MOVE    LNK-DAY2-EDTYMD2-G
                                   TO  WK-FREE-NAME2(IDZ)(IDX49:4)
              END-IF
           END-IF
      *
           IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
             MOVE  "2"                   TO  WK-FREE-MEISKYFLG2(IDZ)
             MOVE  WRK-JIDO-SETCOMATFLG  TO  WK-FREE-SETCOMATFLG2(IDZ)
           END-IF
      *
           PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           .
       20011-SYOUNI-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿´Â¡¥Ú¡¼¥¹¥á¡¼¥«¡¼»ØÆ³´ÉÍýÎÁ¡Ê±ó³Ö¥â¥Ë¥¿¥ê¥ó¥°¡Ë¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-SINPESU-HANTEI-SEC          SECTION.
      *
           IF   ((RECE02-SRYYM                 <  "201604") AND
                 (RECE04-FLG-SINPESU-COMMENT   =  "1"     ))
           OR   ((RECE02-SRYYM                >=  "201604") AND
                 (RECE02-SRYYM                 <  "201804") AND
                 (RECE04-FLG-SINPESU-COMMENT   =  "1"     ) AND
                 (RECE04-FLG-SINPESUR-COMMENT  =  "3"     ))
            IF    RECE02-SRYYM  >=  "201604"
             MOVE    ZERO                TO  FLG-SINPESU-COMMENT
             MOVE    SPACE               TO  WRK-SINPESU-YMD
             MOVE    CONS-SINPESU2       TO  WRK-SINPESU-SRYCD
             PERFORM   20011-SINPESU-CHECK-SEC
            ELSE
             MOVE    ZERO                TO  FLG-SINPESU-COMMENT
             MOVE    SPACE               TO  WRK-SINPESU-YMD
             INITIALIZE                      SANTEI-REC
             MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE02-PTID         TO  SANTEI-PTID
             MOVE    CONS-SINPESU        TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI          =   1 )
                             OR   ( FLG-SINPESU-COMMENT =   1 )
      *
                   IF   SANTEI-SRYYM  <  RECE02-SRYYM
                     MOVE  SANTEI-SRYYM     TO  WRK-SINPESU-YMD(1:6)
                     MOVE  SANTEI-MSANTEID  TO  WRK-SINPESU-YMD(7:2)
                     MOVE    1              TO  FLG-SINPESU-COMMENT
                   END-IF
      *
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
             END-IF
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
            END-IF
      *
            IF    WRK-SINPESU-YMD   NOT =  SPACE
      *
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE       17      TO     WRK-JIDO-SETCOMATFLG
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                ADD        1       TO     IDZ
      *
                MOVE       13      TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "Á°²ó»»ÄêÇ¯·î¡¡¡¡¡¡Ç¯¡¡¡¡·î"
                                   TO     WK-FREE-NAME2(IDZ)
                MOVE       15      TO     IDX41
                MOVE       21      TO     IDX42
      *
                INITIALIZE                         STS-AREA-DAY
                INITIALIZE                         LNK-DAY2-AREA
                MOVE    "21"                   TO  LNK-DAY2-IRAI
                MOVE    WRK-SINPESU-YMD        TO  LNK-DAY2-YMD
                CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                               LNK-DAY2-AREA
                IF      STS-DAY-RC1            =   ZERO
                   MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                   END-IF
      *
                   MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                   END-IF
                END-IF
      *
                IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                  MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                  MOVE  WRK-JIDO-SETCOMATFLG
                                   TO     WK-FREE-SETCOMATFLG2(IDZ)
                END-IF
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
            END-IF
           END-IF
      *
           .
       2001-SINPESU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ±ó³Ö¥â¥Ë¥¿¥ê¥ó¥°²Ã»»¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       20011-SINPESU-CHECK-SEC          SECTION.
      *
           INITIALIZE                        SRYACT-REC
           MOVE    RECE02-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    RECE02-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    RECE02-PTID           TO  SRY-PTID
           MOVE    RECE02-SRYYM          TO  SRY-SRYYM
           MOVE    WRK-SINPESU-SRYCD     TO  SRY-SRYCD(1)
           MOVE    WRK-SINPESU-SRYCD     TO  SRY-SRYCD(2)
           MOVE    WRK-SINPESU-SRYCD     TO  SRY-SRYCD(3)
           MOVE    WRK-SINPESU-SRYCD     TO  SRY-SRYCD(4)
           MOVE    WRK-SINPESU-SRYCD     TO  SRY-SRYCD(5)
           MOVE    SRYACT-REC            TO  MCPDATA-REC
           MOVE    "tbl_sryact"          TO  MCP-TABLE
           MOVE    "key23"               TO  MCP-PATHNAME
           PERFORM  900-DBSELECT-SEC
           IF       MCP-RC  =  ZERO
             MOVE      "tbl_sryact"      TO  MCP-TABLE
             MOVE      "key23"           TO  MCP-PATHNAME
             PERFORM    900-SRYACT-READ-SEC
             PERFORM  UNTIL   FLG-SRYACT           =   1
                       OR     FLG-SINPESU-COMMENT  =   1
      *
               MOVE     SRY-SRYYM        TO  WRK-SINPESU-YMD(1:6)
               MOVE     "01"             TO  WRK-SINPESU-YMD(7:2)
               MOVE       1              TO  FLG-SINPESU-COMMENT
      *
               MOVE    "tbl_sryact"      TO  MCP-TABLE
               MOVE    "key23"           TO  MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
             END-PERFORM
           END-IF
           MOVE    "tbl_sryact"          TO  MCP-TABLE
           MOVE    "key23"               TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       20011-SINPESU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿´Â¡¥Ú¡¼¥¹¥á¡¼¥«¡¼»ØÆ³´ÉÍýÎÁ¡Ê¥í¡Ë¤¬¤¢¤ë¤«È½Äê
      *    ±ó³Ö¥â¥Ë¥¿¥ê¥ó¥°²Ã»»¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-SINPESUR-HANTEI-SEC          SECTION.
      *
           IF   ((RECE02-SRYYM                  <    "201604"  ) AND
                 (RECE04-FLG-SINPESUR-COMMENT   =    "1" OR "2"))
           OR   ((RECE02-SRYYM                 >=    "201604"  ) AND
                 (RECE04-FLG-SINPESUR-COMMENT   =    "1" OR "2") AND
                 (RECE04-FLG-SINPESU-COMMENT    =     SPACE    ))
           OR   ((RECE02-SRYYM                 >=    "201804"  ) AND
                 (RECE02-SRYYM                 <=    "202003"  ) AND
                 (RECE04-FLG-SINPESU-COMMENT    =    "1"       ) AND
                 (RECE04-FLG-SINPESUR-COMMENT   =    "3"       ))
           OR   ((RECE02-SRYYM                 >=    "202004"  ) AND
                 (RECE04-FLG-SINPESU-COMMENT    =    "4"       ) AND
                 (RECE04-FLG-SINPESUR-COMMENT   =    "4"       ))
           OR   ((RECE02-SRYYM                 >=    "202004"  ) AND
                 (RECE04-FLG-SINPESU-COMMENT    =    "5"       ) AND
                 (RECE04-FLG-SINPESUR-COMMENT   =    "5"       ))
           OR   ((RECE02-SRYYM                 >=    "201804"  ) AND
                 (RECE04-FLG-SINPESU-COMMENT    =    "A"       ) AND
                 (RECE04-FLG-SINPESUR-COMMENT   =    "A"       ))
           OR   ((RECE02-SRYYM                 >=    "201804"  ) AND
                 (RECE04-FLG-SINPESU-COMMENT    =    "B"       ) AND
                 (RECE04-FLG-SINPESUR-COMMENT   =    "B"       ))
             MOVE    ZERO                TO  FLG-SINPESUR-COMMENT
             MOVE    SPACE               TO  WRK-SINPESUR-YMD
             INITIALIZE                      SANTEI-REC
             MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE02-PTID         TO  SANTEI-PTID
             EVALUATE  RECE04-FLG-SINPESUR-COMMENT
               WHEN     "1"
                 MOVE    CONS-SINPESUR1  TO  SANTEI-SRYCD
               WHEN     "2"
                 MOVE    CONS-SINPESUR2  TO  SANTEI-SRYCD
               WHEN     "3"
                 MOVE    CONS-SINPESUR3  TO  SANTEI-SRYCD
               WHEN     "4"
                 MOVE    CONS-SINPESUR4  TO  SANTEI-SRYCD
               WHEN     "5"
                 MOVE    CONS-SINPESUR5  TO  SANTEI-SRYCD
               WHEN     "A"
                 MOVE    CONS-ZAISAN     TO  SANTEI-SRYCD
               WHEN     "B"
                 MOVE    CONS-YOUATU2    TO  SANTEI-SRYCD
             END-EVALUATE
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI           =   1 )
                             OR   ( FLG-SINPESUR-COMMENT =   1 )
      *
                   IF   SANTEI-SRYYM  <  RECE02-SRYYM
                     MOVE  SANTEI-SRYYM     TO  WRK-SINPESUR-YMD(1:6)
                     MOVE  SANTEI-MSANTEID  TO  WRK-SINPESUR-YMD(7:2)
                     MOVE    1              TO  FLG-SINPESUR-COMMENT
                   END-IF
      *
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
             END-IF
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             IF   WRK-SINPESUR-YMD   NOT =  SPACE
      *
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE       27      TO     WRK-JIDO-SETCOMATFLG
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                ADD        1       TO     IDZ
      *
                EVALUATE  RECE04-FLG-SINPESUR-COMMENT
                  WHEN     "1"
                  WHEN     "2"
                     IF   RECE02-SRYYM >= "201905"
                       MOVE    15    TO     WK-FREE-YUKOKETA2(IDZ)
                       MOVE   "Á°²ó»»ÄêÇ¯·î¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î"
                                     TO     WK-FREE-NAME2(IDZ)
                       MOVE    19    TO     IDX41
                       MOVE    25    TO     IDX42
                       MOVE    15    TO     IDX49
                     ELSE
                       MOVE    13    TO     WK-FREE-YUKOKETA2(IDZ)
                       MOVE   "Á°²ó»»ÄêÇ¯·î¡¡¡¡¡¡Ç¯¡¡¡¡·î"
                                     TO     WK-FREE-NAME2(IDZ)
                       MOVE    15    TO     IDX41
                       MOVE    21    TO     IDX42
                     END-IF
                  WHEN     "3"
                  WHEN     "4"
                  WHEN     "5"
                  WHEN     "A"
                  WHEN     "B"
                     IF   RECE02-SRYYM >= "201905"
                       MOVE    20    TO     WK-FREE-YUKOKETA2(IDZ)
                       MOVE   "»ØÆ³´ÉÍýÎÁÁ°²ó»»ÄêÇ¯·î¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î"
                                     TO     WK-FREE-NAME2(IDZ)
                       MOVE    29    TO     IDX41
                       MOVE    35    TO     IDX42
                       MOVE    25    TO     IDX49
                     ELSE
                       MOVE    18    TO     WK-FREE-YUKOKETA2(IDZ)
                       MOVE   "»ØÆ³´ÉÍýÎÁÁ°²ó»»ÄêÇ¯·î¡¡¡¡¡¡Ç¯¡¡¡¡·î"
                                     TO     WK-FREE-NAME2(IDZ)
                       MOVE    25    TO     IDX41
                       MOVE    31    TO     IDX42
                     END-IF
                END-EVALUATE
      *
                INITIALIZE                         STS-AREA-DAY
                INITIALIZE                         LNK-DAY2-AREA
                MOVE    "21"                   TO  LNK-DAY2-IRAI
                MOVE    WRK-SINPESUR-YMD       TO  LNK-DAY2-YMD
                CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                               LNK-DAY2-AREA
                IF      STS-DAY-RC1            =   ZERO
                   MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                   END-IF
      *
                   MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                   END-IF
      *
                   IF   RECE02-SRYYM >= "201905"
                      MOVE    LNK-DAY2-EDTYMD2-G
                                        TO  WK-FREE-NAME2(IDZ)(IDX49:4)
                   END-IF
                END-IF
      *
                IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                  MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                  MOVE  WRK-JIDO-SETCOMATFLG
                                   TO     WK-FREE-SETCOMATFLG2(IDZ)
                END-IF
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-SINPESUR-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¤¬¤ó´µ¼Ô»ØÆ³´ÉÍýÎÁ£²¡¦£³¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-GANSIDO-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-GANSIDO-COMMENT   =    "1" OR "2"
             MOVE    ZERO                TO  IDX27
             INITIALIZE                      WRK-GANSIDO-YMD-AREA
             INITIALIZE                      SANTEI-REC
             MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE02-PTID         TO  SANTEI-PTID
             EVALUATE  RECE04-FLG-GANSIDO-COMMENT
               WHEN     "1"
                 MOVE    CONS-GANSIDO1   TO  SANTEI-SRYCD
               WHEN     "2"
                 MOVE    CONS-GANSIDO2   TO  SANTEI-SRYCD
             END-EVALUATE
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL   FLG-SANTEI  =   1
      *
      ***>         IF   SANTEI-SRYYM  <= RECE02-SRYYM
                   IF   SANTEI-SRYYM  <  RECE02-SRYYM
                     PERFORM  VARYING  IDX26  FROM  31  BY  -1
                               UNTIL   IDX26   <     1
                       IF  (SANTEI-DAY(IDX26)  NOT =  ZERO)
                       AND (IDX27              <    10    )
                         ADD   1      TO  IDX27
                         MOVE  SANTEI-SRYYM
                                      TO  WRK-GANSIDO-YMD(IDX27)(1:6)
                         MOVE  IDX26  TO  WRK-GANSIDO-YMD(IDX27)(7:2)
                       END-IF
                     END-PERFORM
                   END-IF
      *
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
             END-IF
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             IF   WRK-GANSIDO-YMD(1) NOT =  SPACE
      *
                INITIALIZE                WRK-JIDO-SETCOM-AREA
      ***       MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
      ***       MOVE       28      TO     WRK-JIDO-SETCOMATFLG
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                PERFORM  VARYING  IDX26  FROM  10  BY  -1
                          UNTIL   IDX26   <     1
                 IF  WRK-GANSIDO-YMD(IDX26)  NOT =  SPACE
                  ADD        1       TO     IDZ
      *
                  IF   WRK-GANSIDO-YMD(IDX26)(1:6) = RECE02-SRYYM
                    IF   RECE02-SRYYM >= "201905"
                      MOVE     17      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "Åö·î»»ÄêÆü¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                                       TO     WK-FREE-NAME2(IDZ)
                      MOVE     17      TO     IDX41
                      MOVE     23      TO     IDX42
                      MOVE     29      TO     IDX48
                      MOVE     13      TO     IDX49
                    ELSE
                      MOVE     15      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "Åö·î»»ÄêÆü¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                                       TO     WK-FREE-NAME2(IDZ)
                      MOVE     13      TO     IDX41
                      MOVE     19      TO     IDX42
                      MOVE     25      TO     IDX48
                    END-IF
                  ELSE
                    IF   RECE02-SRYYM >= "201905"
                      MOVE     17      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "²áµî»»ÄêÆü¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                                       TO     WK-FREE-NAME2(IDZ)
                      MOVE     17      TO     IDX41
                      MOVE     23      TO     IDX42
                      MOVE     29      TO     IDX48
                      MOVE     13      TO     IDX49
                    ELSE
                      MOVE     15      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "²áµî»»ÄêÆü¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                                       TO     WK-FREE-NAME2(IDZ)
                      MOVE     13      TO     IDX41
                      MOVE     19      TO     IDX42
                      MOVE     25      TO     IDX48
                    END-IF
                  END-IF
      *
                  INITIALIZE                         STS-AREA-DAY
                  INITIALIZE                         LNK-DAY2-AREA
                  MOVE    "21"                   TO  LNK-DAY2-IRAI
                  MOVE    WRK-GANSIDO-YMD(IDX26) TO  LNK-DAY2-YMD
                  CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                                 LNK-DAY2-AREA
                  IF      STS-DAY-RC1            =   ZERO
                     MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
                     INITIALIZE                       ORCSKANACHKAREA
                     MOVE    "2"                  TO  KANACHK-SYORI
                     MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                     CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                     IF      KANACHK-RC      =   ZERO
                        MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                     END-IF
      *
                     MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
                     INITIALIZE                       ORCSKANACHKAREA
                     MOVE    "2"                  TO  KANACHK-SYORI
                     MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                     CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                     IF      KANACHK-RC      =   ZERO
                        MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                     END-IF
      *
                     MOVE    LNK-DAY2-EDTYMD1-D   TO  WRK-TAIHI-XX
                     INITIALIZE                       ORCSKANACHKAREA
                     MOVE    "2"                  TO  KANACHK-SYORI
                     MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                     CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                     IF      KANACHK-RC      =   ZERO
                        MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX48:4)
                     END-IF
      *
                     IF   RECE02-SRYYM >= "201905"
                        MOVE    LNK-DAY2-EDTYMD2-G
                                   TO  WK-FREE-NAME2(IDZ)(IDX49:4)
                     END-IF
                  END-IF
                  IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                    MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                    MOVE  WRK-JIDO-SETCOMATFLG
                                     TO     WK-FREE-SETCOMATFLG2(IDZ)
                  END-IF
                 END-IF
                END-PERFORM
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-GANSIDO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆýÁ£±ê½Å¾É²½Í½ËÉ¥±¥¢¡¦»ØÆ³ÎÁ¡Ê½é²ó¡Ë¤¬¤¢¤ë¤«È½Äê
      *    ÆýÁ£±ê½Å¾É²½Í½ËÉ¥±¥¢¡¦»ØÆ³ÎÁ¡Ê£²²óÌÜ¤«¤é£´²óÌÜ¤Þ¤Ç¡Ë¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-NYUSEN-HANTEI-SEC          SECTION.
      *
           IF   ((RECE04-FLG-NYUSEN-COMMENT   =    "1"  ) AND
                 (RECE02-NYUSENFLG            =    SPACE))
           OR   ( RECE04-FLG-NYUSEN-COMMENT   =    "2"   )
             MOVE   CONS-NYUSEN   TO   WRK-CHECK-SRYCD
             PERFORM       20011-ZAIKYO-CHECK-SEC
      *
             MOVE       ZERO                TO  WRK-ZAIKYO-KAISU
             IF   WRK-ZAIKYO-YMD   NOT =  SPACE
               MOVE      1                  TO  WRK-ZAIKYO-KAISU
               IF   RECE04-FLG-NYUSEN-COMMENT  =  "2"
                INITIALIZE                      SANTEI-REC
                MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
                MOVE    RECE02-PTID         TO  SANTEI-PTID
                MOVE    CONS-NYUSEN2        TO  SANTEI-SRYCD
                MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                MOVE    ZERO                TO  SANTEI-SRYKA
                MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
                MOVE    SANTEI-REC          TO  MCPDATA-REC
                MOVE    "tbl_santei"        TO  MCP-TABLE
                MOVE    "key3"              TO  MCP-PATHNAME
                PERFORM 900-DBSELECT-SEC
                IF      MCP-RC              =   ZERO
                  MOVE    "tbl_santei"      TO  MCP-TABLE
                  MOVE    "key3"            TO  MCP-PATHNAME
                  PERFORM  900-SANTEI-READ-SEC
                  PERFORM   UNTIL   FLG-SANTEI  =   1
      *
                    IF   SANTEI-SRYYM  <=  RECE02-SRYYM
                    AND  SANTEI-SRYYM  >=  WRK-ZAIKYO-YMD(1:6)
                      MOVE  WRK-ZAIKYO-YMD(7:2)  TO  IDX28
                      MOVE  WRK-ZAIKYO-YMD9(7:2) TO  IDX29
                      PERFORM  VARYING  IDX26  FROM  31  BY  -1
                                UNTIL   IDX26   <     1
                        IF   SANTEI-DAY(IDX26)  NOT =  ZERO
                          IF ((SANTEI-SRYYM =
                                      WRK-ZAIKYO-YMD(1:6) ) AND
                              (IDX26        < IDX28       ))
                          OR ((SANTEI-SRYYM =
                                      WRK-ZAIKYO-YMD9(1:6)) AND
                              (IDX26       >= IDX29       ))
                            CONTINUE
                          ELSE
                            ADD    1    TO   WRK-ZAIKYO-KAISU
                          END-IF
                        END-IF
                      END-PERFORM
                    END-IF
      *
                    MOVE    "tbl_santei"    TO  MCP-TABLE
                    MOVE    "key3"          TO  MCP-PATHNAME
                    PERFORM  900-SANTEI-READ-SEC
                  END-PERFORM
                END-IF
                MOVE    "tbl_santei"        TO  MCP-TABLE
                MOVE    "key3"              TO  MCP-PATHNAME
                PERFORM 900-CLOSE-SEC
               END-IF
             END-IF
      *
             IF   WRK-ZAIKYO-KAISU  NOT =  ZERO
             AND  WRK-ZAIKYO-KAISU     <=   31
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE       37      TO     WRK-JIDO-SETCOMATFLG
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                ADD        1       TO     IDZ
      *
                MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE     "ÄÌ»»¼Â»Ü²ó¿ô¡Ê¡¡¡¡²ó¡Ë"
                                   TO     WK-FREE-NAME2(IDZ)
                MOVE      WRK-ZAIKYO-KAISU TO  DAY-IDX2
                MOVE      TBL-DAY(DAY-IDX2)
                                   TO     WK-FREE-NAME2(IDZ)(15:4)
      *
                IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                  MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                  MOVE  WRK-JIDO-SETCOMATFLG
                                   TO     WK-FREE-SETCOMATFLG2(IDZ)
                END-IF
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-NYUSEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½¸ÃÄ±ÉÍÜ¿©»ö»ØÆ³ÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-SYUEIYO-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-SYUEIYO-COMMENT   =    "1"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       18      TO     WRK-JIDO-SETCOMATFLG
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-SYUEIYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ë¬Ìä´Ç¸î»Ø¼¨ÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-HOUSIJI-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-HOUSIJI-COMMENT   =    "1"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       11      TO     WRK-JIDO-SETCOMATFLG
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-HOUSIJI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºßÂð·ì±ÕÆ©ÀÏÉÑ²ó»ØÆ³´ÉÍýÎÁ¤¬¤¢¤ë¤«È½Äê
      *    ºßÂð·ì±ÕÆ©ÀÏ»ØÆ³´ÉÍýÎÁ»»ÄêÆü¤ÎÎò·î¤Ç£²·î°ÊÆâ¤«È½Äê
      *****************************************************************
       2001-HINKAI-HANTEI-SEC          SECTION.
      *
           IF   RECE04-FLG-HINKAI-COMMENT   =    "1"
      *
             MOVE    ZERO    TO   WRK-DAY
             PERFORM  VARYING   IDX3   FROM  1  BY  1
                       UNTIL    IDX3  >   31
                 IF   RECE04-KAISU(1 IDX3)  NOT =  ZERO
                        MOVE  IDX3  TO  WRK-DAY
                        MOVE  31    TO  IDX3
                 END-IF
             END-PERFORM
      *
             MOVE     ZERO     TO     FLG-HINKAI-COMMENT
             MOVE     ZERO     TO     WRK-TOSEKIYMD
      *
      *      »»ÄêÍúÎò¸¡º÷
             INITIALIZE                  SANTEI-REC
grpsys       MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE02-PTID         TO  SANTEI-PTID
             MOVE    CONS-TOSEKI         TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
grpsys       PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                     MOVE    "tbl_santei"        TO  MCP-TABLE
                     MOVE    "key3"              TO  MCP-PATHNAME
                    PERFORM        900-SANTEI-READ-SEC
                    PERFORM   UNTIL  ( FLG-SANTEI         =   1 ) OR
                                     ( FLG-HINKAI-COMMENT =   1 )
      *
                      IF   SANTEI-SRYYM  <=  RECE02-SRYYM
      ***              MOVE   SANTEI-SRYYM     TO  WRK-FSANTEIYMD(1:6)
      ***              MOVE   SANTEI-MSANTEID  TO  WRK-FSANTEIYMD(7:2)
                       MOVE   SANTEI-FSANTEIYMD
                                               TO  WRK-FSANTEIYMD
      *
                       INITIALIZE                  STS-AREA-DAY
                       INITIALIZE                  LNK-DAY6-AREA
                       MOVE    "61"            TO  LNK-DAY6-IRAI
                       MOVE    WRK-FSANTEIYMD  TO  LNK-DAY6-KIJUN
                       MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
                       MOVE     2              TO  LNK-DAY6-ZOGEN
                       CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN TO  WRK-AFTER2
      *
                       MOVE    RECE02-SRYYM    TO  WRK-HINKAIYMD(1:6)
                       MOVE    WRK-DAY         TO  WRK-HINKAIYMD(7:2)
      *
                       IF     WRK-DAY      NOT  =  ZERO
                       AND    WRK-FSANTEIYMD   <=  WRK-HINKAIYMD
                         IF ((WRK-HINKAIYMD    <   WRK-AFTER2) AND
                             (RECE02-SRYYM     <= "201003"   ))
                         OR ( RECE02-SRYYM     >= "201004"    )
                             MOVE    1    TO   FLG-HINKAI-COMMENT
                             MOVE  WRK-FSANTEIYMD
                                          TO   WRK-TOSEKIYMD
                         END-IF
                       END-IF
                      END-IF
      * 
                      MOVE    "tbl_santei"     TO  MCP-TABLE
                      MOVE    "key3"           TO  MCP-PATHNAME
                      PERFORM        900-SANTEI-READ-SEC
                    END-PERFORM
             END-IF
      *
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             IF      FLG-HINKAI-COMMENT    =      1
                 INITIALIZE            WRK-JIDO-SETCOM-AREA
                 MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                 MOVE       12      TO     WRK-JIDO-SETCOMATFLG
                 MOVE       99      TO     FLG-COMMENT
                 PERFORM    20011-HINKAI-COMMENT-SEC
             END-IF
      *
           END-IF
      *
           .
       2001-HINKAI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºßÂð·ì±ÕÆ©ÀÏÉÑ²ó»ØÆ³´ÉÍýÎÁ¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       20011-HINKAI-COMMENT-SEC          SECTION.
      *
             PERFORM    200111-RECE05-TAIHI-SEC
      *
      *      £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
             MOVE    ZERO                 TO  IDX-RECE05
             MOVE    SPACE                TO  WRK-RECE05XX-REC
             INITIALIZE                       WRK-RECE05XX-REC
      *      ¥³¥á¥ó¥È¤ÎÁÞÆþ
             COMPUTE  MAX-MEISAISU        =   RECE04-MEISAISU  +  1
      *
             MOVE    "810000001"
                            TO  WRK-RECE05XX-SRYCD(MAX-MEISAISU 1)
             MOVE    ZERO   TO  WRK-RECE05XX-SRYSURYO(MAX-MEISAISU 1)
             MOVE    16     TO  WRK-RECE05XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE    "½é²ó»ØÆ³´ÉÍý»»ÄêÆü¡¡¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE05XX-NAME(MAX-MEISAISU 1)
             MOVE    WRK-TOSEKIYMD(5:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE05XX-NAME(MAX-MEISAISU 1)(21:4)
             MOVE    WRK-TOSEKIYMD(7:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE05XX-NAME(MAX-MEISAISU 1)(27:4)
      *
             IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
               MOVE  "2"    TO  WRK-RECE05XX-MEISKYFLG(MAX-MEISAISU 1)
               MOVE  WRK-JIDO-SETCOMATFLG
                            TO  WRK-RECE05XX-SETCOMATFLG(MAX-MEISAISU 1)
             END-IF
      *
             PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-HINKAI-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ç§ÃÎ¾ÉÀìÌç¿ÇÃÇ´ÉÍýÎÁ£²¤¬¤¢¤ë¤«È½Äê
      *    ¾®»ù±¿Æ°´ï¼À´µ»ØÆ³´ÉÍýÎÁ¤¬¤¢¤ë¤«È½Äê
      *    ÎÅÍÜ¡¦½¢Ï«Î¾Î©»Ù±ç»ØÆ³ÎÁ¤¬¤¢¤ë¤«È½Äê
      *    Ç§ÃÎ¾É¥µ¥Ý¡¼¥È»ØÆ³ÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-NINSEN-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-NINSEN-COMMENT   NOT =   SPACE
             MOVE    ZERO                TO  FLG-SINPESU-COMMENT
             MOVE    SPACE               TO  WRK-SINPESU-YMD
             INITIALIZE                      SANTEI-REC
             MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE02-PTID         TO  SANTEI-PTID
             EVALUATE  RECE04-FLG-NINSEN-COMMENT
               WHEN      "1"
                 MOVE  CONS-NINSEN       TO  SANTEI-SRYCD
               WHEN      "2"
                 MOVE  CONS-SYOUNDO      TO  SANTEI-SRYCD
               WHEN      "3"
                 MOVE  CONS-RYOSYU       TO  SANTEI-SRYCD
               WHEN      "4"
                 MOVE  CONS-NINSAPO      TO  SANTEI-SRYCD
             END-EVALUATE
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI          =   1 )
                             OR   ( FLG-SINPESU-COMMENT =   1 )
      *
                   IF   SANTEI-SRYYM  <  RECE02-SRYYM
                     MOVE  SANTEI-SRYYM     TO  WRK-SINPESU-YMD(1:6)
                     MOVE  SANTEI-MSANTEID  TO  WRK-SINPESU-YMD(7:2)
                     MOVE    1              TO  FLG-SINPESU-COMMENT
                   END-IF
      *
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
             END-IF
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             INITIALIZE                WRK-JIDO-SETCOM-AREA
             MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
             MOVE       21      TO     WRK-JIDO-SETCOMATFLG
             MOVE       99      TO     FLG-COMMENT
      *
             IF   WRK-SINPESU-YMD   NOT =  SPACE
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                ADD        1       TO     IDZ
      *
                EVALUATE  RECE04-FLG-NINSEN-COMMENT
                  WHEN    "1"
                    IF   RECE02-SRYYM >= "201905"
                      MOVE       19      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "Á°²ó»»ÄêÇ¯·îÆü¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                                         TO     WK-FREE-NAME2(IDZ)
                      MOVE       21      TO     IDX41
                      MOVE       27      TO     IDX42
                      MOVE       33      TO     IDX48
                      MOVE       17      TO     IDX49
                    ELSE
                      MOVE       17      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "Á°²ó»»ÄêÇ¯·îÆü¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                                         TO     WK-FREE-NAME2(IDZ)
                      MOVE       17      TO     IDX41
                      MOVE       23      TO     IDX42
                      MOVE       29      TO     IDX48
                    END-IF
      *
                    INITIALIZE                      STS-AREA-DAY
                    INITIALIZE                      LNK-DAY2-AREA
                    MOVE    "21"                TO  LNK-DAY2-IRAI
                    MOVE    WRK-SINPESU-YMD     TO  LNK-DAY2-YMD
                    CALL    "ORCSDAY"    USING      STS-AREA-DAY
                                                    LNK-DAY2-AREA
                    IF      STS-DAY-RC1  =  ZERO
                      MOVE  LNK-DAY2-EDTYMD1-Y  TO  WRK-TAIHI-XX
                      INITIALIZE                    ORCSKANACHKAREA
                      MOVE  "2"                 TO  KANACHK-SYORI
                      MOVE  WRK-TAIHI-XX        TO  KANACHK-MAE-INPUT
                      CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                      IF    KANACHK-RC  =  ZERO
                        MOVE  KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                      END-IF
      *
                      MOVE  LNK-DAY2-EDTYMD1-M  TO  WRK-TAIHI-XX
                      INITIALIZE                    ORCSKANACHKAREA
                      MOVE  "2"                 TO  KANACHK-SYORI
                      MOVE  WRK-TAIHI-XX        TO  KANACHK-MAE-INPUT
                      CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                      IF    KANACHK-RC  =  ZERO
                        MOVE  KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                      END-IF
      *
                      MOVE  LNK-DAY2-EDTYMD1-D  TO  WRK-TAIHI-XX
                      INITIALIZE                    ORCSKANACHKAREA
                      MOVE  "2"                 TO  KANACHK-SYORI
                      MOVE  WRK-TAIHI-XX        TO  KANACHK-MAE-INPUT
                      CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                      IF    KANACHK-RC  =  ZERO
                        MOVE  KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX48:4)
                      END-IF
      *
                      IF   RECE02-SRYYM >= "201905"
                        MOVE  LNK-DAY2-EDTYMD2-G
                                   TO  WK-FREE-NAME2(IDZ)(IDX49:4)
                      END-IF
                    END-IF
                  WHEN    "2"
                  WHEN    "3"
                  WHEN    "4"
                    IF   RECE02-SRYYM >= "201905"
                      MOVE       15      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "Á°²ó»»ÄêÇ¯·î¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î"
                                         TO     WK-FREE-NAME2(IDZ)
                      MOVE       19      TO     IDX41
                      MOVE       25      TO     IDX42
                      MOVE       15      TO     IDX49
                    ELSE
                      MOVE       13      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "Á°²ó»»ÄêÇ¯·î¡¡¡¡¡¡Ç¯¡¡¡¡·î"
                                         TO     WK-FREE-NAME2(IDZ)
                      MOVE       15      TO     IDX41
                      MOVE       21      TO     IDX42
                    END-IF
      *
                    INITIALIZE                      STS-AREA-DAY
                    INITIALIZE                      LNK-DAY2-AREA
                    MOVE    "21"                TO  LNK-DAY2-IRAI
                    MOVE    WRK-SINPESU-YMD     TO  LNK-DAY2-YMD
                    CALL    "ORCSDAY"    USING      STS-AREA-DAY
                                                    LNK-DAY2-AREA
                    IF      STS-DAY-RC1  =  ZERO
                      MOVE  LNK-DAY2-EDTYMD1-Y  TO  WRK-TAIHI-XX
                      INITIALIZE                    ORCSKANACHKAREA
                      MOVE  "2"                 TO  KANACHK-SYORI
                      MOVE  WRK-TAIHI-XX        TO  KANACHK-MAE-INPUT
                      CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                      IF    KANACHK-RC  =  ZERO
                        MOVE  KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                      END-IF
      *
                      MOVE  LNK-DAY2-EDTYMD1-M  TO  WRK-TAIHI-XX
                      INITIALIZE                    ORCSKANACHKAREA
                      MOVE  "2"                 TO  KANACHK-SYORI
                      MOVE  WRK-TAIHI-XX        TO  KANACHK-MAE-INPUT
                      CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                      IF    KANACHK-RC  =  ZERO
                        MOVE  KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                      END-IF
      *
                      IF   RECE02-SRYYM >= "201905"
                        MOVE  LNK-DAY2-EDTYMD2-G
                                   TO  WK-FREE-NAME2(IDZ)(IDX49:4)
                      END-IF
                    END-IF
                END-EVALUATE
             ELSE
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                ADD        1       TO     IDZ
      *
                MOVE       6       TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "¡Ê½é²ó»»Äê¡Ë"  TO     WK-FREE-NAME2(IDZ)
             END-IF
      *
             IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
               MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
               MOVE  WRK-JIDO-SETCOMATFLG
                                TO     WK-FREE-SETCOMATFLG2(IDZ)
             END-IF
      *
             PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           END-IF
      *
           .
       2001-NINSEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Âà±¡»þ¶¦Æ±»ØÆ³ÎÁÆÃÊÌ´ÉÍý»ØÆ³²Ã»»¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-TAKYOKSN-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-TAKYOKSN-COMMENT   =    "1"
           AND    FLG-TAKYO-HENSYU              =     1 
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       22      TO     WRK-JIDO-SETCOMATFLG
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-TAKYOKSN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³°Íè¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó¿ÇÎÅÎÁ¡¦³°ÍèÊü¼ÍÀþ¾È¼Í¿ÇÎÅÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-GAIRIHA-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-GAIRIHA-COMMENT   =    "1"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       23      TO     WRK-JIDO-SETCOMATFLG
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-GAIRIHA-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿Í¹©Æâ¼ªµ¡´ïÄ´À°²Ã»»¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-JINNAI-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-JINNAI-COMMENT   NOT =   SPACE
             MOVE      ZERO                TO  FLG-JINNAI-COMMENT
             MOVE      SPACE               TO  WRK-JINNAI-YMD
             PERFORM   VARYING   IDX   FROM    1   BY  1
                        UNTIL   (IDX                 >  2)
                         OR     (FLG-JINNAI-COMMENT  =  1)
               INITIALIZE                      SANTEI-REC
               MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
               MOVE    RECE02-PTID         TO  SANTEI-PTID
               EVALUATE  RECE04-FLG-JINNAI-COMMENT
                 WHEN      "1"
                       MOVE  CONS-JINNAI1  TO  SANTEI-SRYCD
                 WHEN      "2"
                   EVALUATE  IDX
                     WHEN     1
                       MOVE  CONS-JINNAI2  TO  SANTEI-SRYCD
                     WHEN     2
                       MOVE  CONS-JINNAI1  TO  SANTEI-SRYCD
                   END-EVALUATE
               END-EVALUATE
               MOVE    ZERO                TO  SANTEI-NYUGAIKBN
               MOVE    ZERO                TO  SANTEI-SRYKA
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"      TO  MCP-TABLE
                 MOVE    "key3"            TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI          =   1 )
                             OR   ( FLG-JINNAI-COMMENT  =   1 )
                   IF   SANTEI-SRYYM  <  RECE02-SRYYM
                     MOVE  SANTEI-SRYYM     TO  WRK-JINNAI-YMD(1:6)
                     MOVE  SANTEI-MSANTEID  TO  WRK-JINNAI-YMD(7:2)
                     MOVE    1              TO  FLG-JINNAI-COMMENT
                   END-IF
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
               END-IF
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               EVALUATE  RECE04-FLG-JINNAI-COMMENT
                 WHEN      "1"
                   MOVE     2              TO  IDX
               END-EVALUATE
             END-PERFORM
      *
             INITIALIZE                WRK-JIDO-SETCOM-AREA
             MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
             MOVE       45      TO     WRK-JIDO-SETCOMATFLG
             MOVE       99      TO     FLG-COMMENT
      *
             IF   WRK-JINNAI-YMD   NOT =  SPACE
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                ADD        1       TO     IDZ
      *
                MOVE       19      TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "Á°²ó»»ÄêÇ¯·îÆü¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                                   TO     WK-FREE-NAME2(IDZ)
                MOVE       21      TO     IDX41
                MOVE       27      TO     IDX42
                MOVE       33      TO     IDX48
                MOVE       17      TO     IDX49
      *
                INITIALIZE                      STS-AREA-DAY
                INITIALIZE                      LNK-DAY2-AREA
                MOVE    "21"                TO  LNK-DAY2-IRAI
                MOVE    WRK-JINNAI-YMD      TO  LNK-DAY2-YMD
                CALL    "ORCSDAY"    USING      STS-AREA-DAY
                                                LNK-DAY2-AREA
                IF      STS-DAY-RC1  =  ZERO
                  MOVE  LNK-DAY2-EDTYMD1-Y  TO  WRK-TAIHI-XX
                  INITIALIZE                    ORCSKANACHKAREA
                  MOVE  "2"                 TO  KANACHK-SYORI
                  MOVE  WRK-TAIHI-XX        TO  KANACHK-MAE-INPUT
                  CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                  IF    KANACHK-RC  =  ZERO
                    MOVE  KANACHK-OUT-INPUT(1:4)
                               TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                  END-IF
      *
                  MOVE  LNK-DAY2-EDTYMD1-M  TO  WRK-TAIHI-XX
                  INITIALIZE                    ORCSKANACHKAREA
                  MOVE  "2"                 TO  KANACHK-SYORI
                  MOVE  WRK-TAIHI-XX        TO  KANACHK-MAE-INPUT
                  CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                  IF    KANACHK-RC  =  ZERO
                    MOVE  KANACHK-OUT-INPUT(1:4)
                               TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                  END-IF
      *
                  MOVE  LNK-DAY2-EDTYMD1-D  TO  WRK-TAIHI-XX
                  INITIALIZE                    ORCSKANACHKAREA
                  MOVE  "2"                 TO  KANACHK-SYORI
                  MOVE  WRK-TAIHI-XX        TO  KANACHK-MAE-INPUT
                  CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                  IF    KANACHK-RC  =  ZERO
                    MOVE  KANACHK-OUT-INPUT(1:4)
                               TO  WK-FREE-NAME2(IDZ)(IDX48:4)
                  END-IF
      *
                  MOVE  LNK-DAY2-EDTYMD2-G
                               TO  WK-FREE-NAME2(IDZ)(IDX49:4)
                END-IF
             ELSE
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                ADD        1       TO     IDZ
      *
                MOVE       6       TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "¡Ê½é²ó»»Äê¡Ë"  TO     WK-FREE-NAME2(IDZ)
             END-IF
      *
             IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
               MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
               MOVE  WRK-JIDO-SETCOMATFLG
                                TO     WK-FREE-SETCOMATFLG2(IDZ)
             END-IF
      *
             PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           END-IF
      *
           .
       2001-JINNAI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³°ÍèÇÓÇ¢¼«Î©»ØÆ³ÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-GAIJIR-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-GAIJIR-COMMENT   =    "1"
      *
             MOVE  SPACE             TO  WRK-GAIJIR-FSANTEIYM
             MOVE  ZERO              TO  WRK-GAIJIR-KAISU
      *
             INITIALIZE                  ORCSRECESRCHKAREA
             MOVE  "1"               TO  LNK-RECESRCHK-SYORI
             MOVE  RECE02-PTID       TO  LNK-RECESRCHK-PTID
             MOVE  RECE02-SRYYM      TO  LNK-RECESRCHK-SRYYM
             MOVE  RECE02-NYUGAIKBN  TO  LNK-RECESRCHK-NYUGAIKBN
             MOVE  RECE02-GAIJIRYMD  TO  LNK-RECESRCHK-SRYYMD(1)
             CALL  "ORCSRECESRCHK"   USING  SPA-AREA
                                            ORCSRECESRCHKAREA
             IF    LNK-RECESRCHK-RC  =  ZERO
                MOVE  LNK-RECESRCHK-FSANTEIYMD
                                           TO  WRK-GAIJIR-FSANTEIYM
                MOVE  LNK-RECESRCHK-KAISU  TO  WRK-GAIJIR-KAISU
             END-IF
      *
             IF   WRK-GAIJIR-FSANTEIYM  NOT = SPACE
             AND  WRK-GAIJIR-KAISU      NOT = ZERO
      *
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE       46      TO     WRK-JIDO-SETCOMATFLG
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                ADD        1       TO     IDZ
      *
                MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "½é²ó»»Äê¡¡¡¡¡¡·î¡¡¡¡Æü"
                                   TO     WK-FREE-NAME2(IDZ)
      *
                MOVE    WRK-GAIJIR-FSANTEIYM(5:2)  TO  DAY-IDX2
                MOVE    TBL-DAY(DAY-IDX2)
                                   TO  WK-FREE-NAME2(IDZ)(11:4)
                MOVE    WRK-GAIJIR-FSANTEIYM(7:2)  TO  DAY-IDX2
                MOVE    TBL-DAY(DAY-IDX2)
                                   TO  WK-FREE-NAME2(IDZ)(17:4)
      *
                IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                  MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                  MOVE  WRK-JIDO-SETCOMATFLG
                                   TO     WK-FREE-SETCOMATFLG2(IDZ)
                END-IF
      *
                IF    WRK-GAIJIR-KAISU  <=  31
                  ADD      1       TO     IDZ
      *
                  MOVE     11      TO     WK-FREE-YUKOKETA2(IDZ)
                  MOVE   "ÄÌ»»¼Â»Ü²ó¿ô¡Ê¡¡¡¡²ó¡Ë"
                                   TO     WK-FREE-NAME2(IDZ)
                  MOVE    WRK-GAIJIR-KAISU  TO  DAY-IDX2
                  MOVE    TBL-DAY(DAY-IDX2)
                                   TO     WK-FREE-NAME2(IDZ)(15:4)
      *
                  IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                    MOVE  "2"      TO     WK-FREE-MEISKYFLG2(IDZ)
                    MOVE  WRK-JIDO-SETCOMATFLG
                                   TO     WK-FREE-SETCOMATFLG2(IDZ)
                  END-IF
                END-IF
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
      *
           END-IF
      *
           .
       2001-GAIJIR-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ±ý¿Ç¡¦ÆÃÊÌ±ý¿Ç¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-OSIN-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-OSIN-COMMENT   =    "1"
           AND    FLG-OSIN-HENSYU           =     1 
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       13      TO     WRK-JIDO-SETCOMATFLG
               MOVE       6       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-OSIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-ZAIHOU-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-ZAIHOU-COMMENT   =    "1"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       14      TO     WRK-JIDO-SETCOMATFLG
               MOVE       6       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-ZAIHOU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºßÂð´µ¼ÔË¬ÌäÅÀÅ©Ãí¼Í´ÉÍý»ØÆ³ÎÁ»»Äê»þ¤ÎÅÀÅ©Ãí¼Í¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-HOUTEN-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-HOUTEN-COMMENT   =    "1"
               PERFORM    20011-HOUTEN-COMMENT-SEC
           ELSE
               PERFORM    2099-RECE04-TENSOU-SEC
           END-IF
      *
           .
       2001-HOUTEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºßÂð´µ¼ÔË¬ÌäÅÀÅ©Ãí¼Í´ÉÍý»ØÆ³ÎÁ»»Äê»þ¤ÎÅÀÅ©Ãí¼Í¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       20011-HOUTEN-COMMENT-SEC          SECTION.
      *
             INITIALIZE            WRK-JIDO-SETCOM-AREA
      *
             PERFORM    200111-RECE05-TAIHI-SEC
      *
      *      £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
             MOVE    ZERO                 TO  IDX-RECE05
             MOVE    SPACE                TO  WRK-RECE05XX-REC
             INITIALIZE                       WRK-RECE05XX-REC
      *      ¥³¥á¥ó¥È¤ÎÁÞÆþ
             ADD     1                    TO  IDX-RECE05
      *
             MOVE    "810000001"
                            TO  WRK-RECE05XX-SRYCD(IDX-RECE05 1)
             MOVE    ZERO   TO  WRK-RECE05XX-SRYSURYO(IDX-RECE05 1)
             MOVE    4      TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
             MOVE    "¡ÊË¬ÅÀ¡Ë"
                            TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
      *
             PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-HOUTEN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ²ð¸î¿¦°÷ÅùÓ½áâµÛ°úÅù»Ø¼¨ÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-KAISYOKU-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-KAISYOKU-COMMENT   =    "1"
             MOVE    ZERO                TO  FLG-SINPESU-COMMENT
             MOVE    SPACE               TO  WRK-SINPESU-YMD
             INITIALIZE                      SANTEI-REC
             MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE02-PTID         TO  SANTEI-PTID
             MOVE    CONS-KAISYOKU       TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI          =   1 )
                             OR   ( FLG-SINPESU-COMMENT =   1 )
      *
                   IF   SANTEI-SRYYM  <  RECE02-SRYYM
                     MOVE  SANTEI-SRYYM     TO  WRK-SINPESU-YMD(1:6)
                     MOVE  SANTEI-MSANTEID  TO  WRK-SINPESU-YMD(7:2)
                     MOVE    1              TO  FLG-SINPESU-COMMENT
                   END-IF
      *
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
             END-IF
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             INITIALIZE                WRK-JIDO-SETCOM-AREA
             MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
             MOVE       24      TO     WRK-JIDO-SETCOMATFLG
             MOVE       99      TO     FLG-COMMENT
      *
             IF   WRK-SINPESU-YMD   NOT =  SPACE
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                ADD        1       TO     IDZ
      *
                IF   RECE02-SRYYM >= "201905"
                  MOVE       19      TO     WK-FREE-YUKOKETA2(IDZ)
                  MOVE   "Á°²ó¸òÉÕÇ¯·îÆü¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                                     TO     WK-FREE-NAME2(IDZ)
                  MOVE       21      TO     IDX41
                  MOVE       27      TO     IDX42
                  MOVE       33      TO     IDX48
                  MOVE       17      TO     IDX49
                ELSE
                  MOVE       17      TO     WK-FREE-YUKOKETA2(IDZ)
                  MOVE   "Á°²ó¸òÉÕÇ¯·îÆü¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                                     TO     WK-FREE-NAME2(IDZ)
                  MOVE       17      TO     IDX41
                  MOVE       23      TO     IDX42
                  MOVE       29      TO     IDX48
                END-IF
      *
                INITIALIZE                         STS-AREA-DAY
                INITIALIZE                         LNK-DAY2-AREA
                MOVE    "21"                   TO  LNK-DAY2-IRAI
                MOVE    WRK-SINPESU-YMD        TO  LNK-DAY2-YMD
                CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                               LNK-DAY2-AREA
                IF      STS-DAY-RC1            =   ZERO
                   MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                   END-IF
      *
                   MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                   END-IF
      *
                   MOVE    LNK-DAY2-EDTYMD1-D   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX48:4)
                   END-IF
      *
                   IF   RECE02-SRYYM >= "201905"
                      MOVE    LNK-DAY2-EDTYMD2-G
                                   TO  WK-FREE-NAME2(IDZ)(IDX49:4)
                   END-IF
                END-IF
             ELSE
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                ADD        1       TO     IDZ
      *
                MOVE       6       TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "¡Ê½é²ó¸òÉÕ¡Ë"  TO     WK-FREE-NAME2(IDZ)
             END-IF
      *
             IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
               MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
               MOVE  WRK-JIDO-SETCOMATFLG
                                TO     WK-FREE-SETCOMATFLG2(IDZ)
             END-IF
      *
             PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           END-IF
      *
           .
       2001-KAISYOKU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºßÂð»ýÂ³ÍÛ°µ¸ÆµÛÎÅË¡»ØÆ³´ÉÍýÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-YOUATU-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-YOUATU-COMMENT   =    "1"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       26      TO     WRK-JIDO-SETCOMATFLG
               MOVE       99      TO     FLG-COMMENT
               PERFORM    20011-YOUATU-COMMENT-SEC
           END-IF
      *
           .
       2001-YOUATU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºßÂð»ýÂ³ÍÛ°µ¸ÆµÛÎÅË¡»ØÆ³´ÉÍýÎÁ¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       20011-YOUATU-COMMENT-SEC          SECTION.
      *
           PERFORM   200111-SANTEI-DAY-SEC
      *
           MOVE     ZERO         TO  IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           ADD      1            TO  IDZ
           MOVE    "¼Â»ÜÆü¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                                 TO  WK-FREE-NAME2(IDZ)(1:26)
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    RECE02-SRYYM        TO  WRK-HENYMD(1:6)
           MOVE    "01"                TO  WRK-HENYMD(7:2)
           MOVE    WRK-HENYMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF  STS-DAY-RC1             =   ZERO
             MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
             INITIALIZE                       ORCSKANACHKAREA
             MOVE    "2"                  TO  KANACHK-SYORI
             MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
             CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
             IF      KANACHK-RC      =   ZERO
                MOVE    KANACHK-OUT-INPUT(1:4)
                                  TO  WK-FREE-NAME2(IDZ)(9:4)
             END-IF
      *
             MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
             INITIALIZE                       ORCSKANACHKAREA
             MOVE    "2"                  TO  KANACHK-SYORI
             MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
             CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
             IF      KANACHK-RC      =   ZERO
                MOVE    KANACHK-OUT-INPUT(1:4)
                                  TO  WK-FREE-NAME2(IDZ)(15:4)
             END-IF
           END-IF
      *
           MOVE    TB-SANTEI-DAY(1)       TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)      TO  WK-FREE-NAME2(IDZ)(21:4)
      *
           MOVE    13                     TO  WK-FREE-YUKOKETA2(IDZ)
      *
           IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
             MOVE  "2"                    TO  WK-FREE-MEISKYFLG2(IDZ)
             MOVE  WRK-JIDO-SETCOMATFLG   TO  WK-FREE-SETCOMATFLG2(IDZ)
           END-IF
      *
           PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           .
       20011-YOUATU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºßÂð´µ¼Ô¶¦Æ±¿ÇÎÅÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-ZAIKYO-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-ZAIKYO-COMMENT   =    "1"
             MOVE   CONS-ZAIKYO   TO   WRK-CHECK-SRYCD
             PERFORM       20011-ZAIKYO-CHECK-SEC
      *
             IF   WRK-ZAIKYO-YMD   NOT =  SPACE
      *
      *         ºßÂð´µ¼Ô¶¦Æ±¿ÇÎÅÎÁ³«»ÏÆü¤ÎºÞ¸¡º÷
                MOVE       ZERO                 TO  WRK-ZAIKYO-KAISU
                MOVE       ZERO                 TO  FLG-ZAIKYOCOMARI
                PERFORM    20011-ZAIKYO-KENSAKU-SEC
                IF    FLG-ZAIKYOCOMARI  =  1
                  PERFORM  VARYING  IDX30  FROM  1  BY  1
                            UNTIL   IDX30   >   TBL-ZAIKYO-CHECK-MAX
                    INITIALIZE                      SANTEI-REC
                    MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
                    MOVE    RECE02-PTID         TO  SANTEI-PTID
                    MOVE    TBL-ZAIKYO-CHECK(IDX30)
                                                TO  SANTEI-SRYCD
                    MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                    MOVE    ZERO                TO  SANTEI-SRYKA
                    MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
                    MOVE    SANTEI-REC          TO  MCPDATA-REC
                    MOVE    "tbl_santei"        TO  MCP-TABLE
                    MOVE    "key3"              TO  MCP-PATHNAME
                    PERFORM 900-DBSELECT-SEC
                    IF      MCP-RC              =   ZERO
                      MOVE    "tbl_santei"      TO  MCP-TABLE
                      MOVE    "key3"            TO  MCP-PATHNAME
                      PERFORM  900-SANTEI-READ-SEC
                      PERFORM   UNTIL   FLG-SANTEI  =   1
      *
                        IF   SANTEI-SRYYM  <=  RECE02-SRYYM
                        AND  SANTEI-SRYYM  >=  WRK-ZAIKYO-YMD(1:6)
                          MOVE  WRK-ZAIKYO-YMD(7:2)  TO  IDX28
                          MOVE  WRK-ZAIKYO-YMD9(7:2) TO  IDX29
                          PERFORM  VARYING  IDX26  FROM  31  BY  -1
                                    UNTIL   IDX26   <     1
                            IF   SANTEI-DAY(IDX26)  NOT =  ZERO
                              IF ((SANTEI-SRYYM =
                                          WRK-ZAIKYO-YMD(1:6) ) AND
                                  (IDX26        < IDX28       ))
                              OR ((SANTEI-SRYYM =
                                          WRK-ZAIKYO-YMD9(1:6)) AND
                                  (IDX26       >= IDX29       ))
                                CONTINUE
                              ELSE
                                ADD    1    TO   WRK-ZAIKYO-KAISU
                              END-IF
                            END-IF
                          END-PERFORM
                        END-IF
      *
                        MOVE    "tbl_santei"    TO  MCP-TABLE
                        MOVE    "key3"          TO  MCP-PATHNAME
                        PERFORM  900-SANTEI-READ-SEC
                      END-PERFORM
                    END-IF
                    MOVE    "tbl_santei"        TO  MCP-TABLE
                    MOVE    "key3"              TO  MCP-PATHNAME
                    PERFORM 900-CLOSE-SEC
                  END-PERFORM
                END-IF
      *
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE       29      TO     WRK-JIDO-SETCOMATFLG
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                ADD        1       TO     IDZ
      *
                MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "½é²ó»»Äê¡¡¡¡¡¡·î¡¡¡¡Æü"
                                   TO     WK-FREE-NAME2(IDZ)
      *
                MOVE    WRK-ZAIKYO-YMD(5:2)   TO  DAY-IDX2
                MOVE    TBL-DAY(DAY-IDX2)
                                   TO  WK-FREE-NAME2(IDZ)(11:4)
                MOVE    WRK-ZAIKYO-YMD(7:2)   TO  DAY-IDX2
                MOVE    TBL-DAY(DAY-IDX2)
                                   TO  WK-FREE-NAME2(IDZ)(17:4)
      *
                IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                  MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                  MOVE  WRK-JIDO-SETCOMATFLG
                                   TO     WK-FREE-SETCOMATFLG2(IDZ)
                END-IF
      *
                IF   WRK-ZAIKYO-KAISU  NOT =  ZERO
                AND  WRK-ZAIKYO-KAISU     <=   31
                  ADD        1       TO     IDZ
      *
                  MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                  MOVE     "ÄÌ»»¼Â»Ü²ó¿ô¡Ê¡¡¡¡²ó¡Ë"
                                     TO     WK-FREE-NAME2(IDZ)
                  MOVE      WRK-ZAIKYO-KAISU TO  DAY-IDX2
                  MOVE      TBL-DAY(DAY-IDX2)
                                     TO     WK-FREE-NAME2(IDZ)(15:4)
      *
                  IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                    MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                    MOVE  WRK-JIDO-SETCOMATFLG
                                     TO     WK-FREE-SETCOMATFLG2(IDZ)
                  END-IF
                END-IF
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-ZAIKYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    
      *****************************************************************
       20011-ZAIKYO-CHECK-SEC          SECTION.
      *
           MOVE    RECE02-SRYYM        TO  WRK-ZAIKYO-YMD9(1:6)
           MOVE    "99"                TO  WRK-ZAIKYO-YMD9(7:2)
      *
           MOVE    ZERO                TO  FLG-ZAIKYO-COMMENT
           MOVE    SPACE               TO  WRK-ZAIKYO-YMD
           MOVE    ZERO                TO  FLG-RETRY
           INITIALIZE                      SANTEI-REC
           MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE02-PTID         TO  SANTEI-PTID
           MOVE    WRK-CHECK-SRYCD     TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"    TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM  900-SANTEI-READ-SEC
               PERFORM   UNTIL  ( FLG-SANTEI          =   1 )
                           OR   ( FLG-ZAIKYO-COMMENT  =   1 )
      *
                 IF   SANTEI-SRYYM  <=  RECE02-SRYYM
                   MOVE  SANTEI-SRYYM     TO  WRK-ZAIKYO-YMD(1:6)
                   MOVE  SANTEI-MSANTEID  TO  WRK-ZAIKYO-YMD(7:2)
                   MOVE    1              TO  FLG-ZAIKYO-COMMENT
      *
                   IF   SANTEI-SRYYM  =  RECE02-SRYYM
                     MOVE   ZERO  TO  FLG-CHECK-MAE
                     MOVE   ZERO  TO  FLG-CHECK-ATO
                     MOVE  WRK-ZAIKYO-YMD(7:2)  TO  IDX28
                     PERFORM  VARYING  IDX26  FROM   1  BY  1
                               UNTIL   IDX26   >    31
                       IF  RECE04-KAISU(1 IDX26)  NOT =  ZERO
                         IF    IDX26  <  IDX28
                           MOVE    1   TO  FLG-CHECK-MAE
                         ELSE
                           MOVE    1   TO  FLG-CHECK-ATO
                         END-IF
                       END-IF
                     END-PERFORM
                     IF  ((FLG-CHECK-MAE = 1   ) AND
                          (FLG-CHECK-ATO = ZERO))
                     OR  ((FLG-CHECK-MAE = 1   ) AND
                          (FLG-CHECK-ATO = 1   ))
                       MOVE    WRK-ZAIKYO-YMD
                                       TO  WRK-ZAIKYO-YMD9
                       MOVE    SPACE   TO  WRK-ZAIKYO-YMD
                       MOVE    1       TO  FLG-RETRY
                     END-IF
                   END-IF
                 END-IF
      *
                 MOVE    "tbl_santei"  TO  MCP-TABLE
                 MOVE    "key3"        TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF   FLG-RETRY =  1
             MOVE    ZERO                TO  FLG-ZAIKYO-COMMENT
             MOVE    SPACE               TO  WRK-ZAIKYO-YMD
             MOVE    ZERO                TO  FLG-RETRY
             INITIALIZE                      SANTEI-REC
             MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE02-PTID         TO  SANTEI-PTID
             MOVE    WRK-CHECK-SRYCD     TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"    TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM  900-SANTEI-READ-SEC
               PERFORM   UNTIL  ( FLG-SANTEI          =   1 )
                           OR   ( FLG-ZAIKYO-COMMENT  =   1 )
      *
                 IF   SANTEI-SRYYM  <   RECE02-SRYYM
                   MOVE  SANTEI-SRYYM     TO  WRK-ZAIKYO-YMD(1:6)
                   MOVE  SANTEI-MSANTEID  TO  WRK-ZAIKYO-YMD(7:2)
                   MOVE    1              TO  FLG-ZAIKYO-COMMENT
                 END-IF
      *
                 MOVE    "tbl_santei"  TO  MCP-TABLE
                 MOVE    "key3"        TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
               END-PERFORM
             END-IF
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
       20011-ZAIKYO-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    
      *****************************************************************
       20011-ZAIKYO-CHECK2-SEC          SECTION.
      *
      *    »»ÄêÍúÎò¸¡º÷
           MOVE    SPACE               TO  WRK-ZAIKYO-YMD2
           INITIALIZE                      SANTEI-REC
           MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE02-PTID         TO  SANTEI-PTID
           MOVE    RECE02-SRYYM        TO  SANTEI-SRYYM
           MOVE    WRK-CHECK-SRYCD     TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           PERFORM  900-SANTEI-INV-SEC
           IF       FLG-SANTEI        =   ZERO
             MOVE   SANTEI-SRYYM       TO  WRK-ZAIKYO-YMD2(1:6)
             MOVE   SANTEI-MSANTEID    TO  WRK-ZAIKYO-YMD2(7:2)
             IF     WRK-ZAIKYO-YMD     <=  WRK-ZAIKYO-YMD2
               MOVE   ZERO  TO  FLG-CHECK-MAE
               MOVE   ZERO  TO  FLG-CHECK-ATO
               MOVE   WRK-ZAIKYO-YMD2(7:2)  TO  IDX28
               PERFORM  VARYING  IDX26  FROM   1  BY  1
                         UNTIL   IDX26   >    31
                 IF  RECE04-KAISU(1 IDX26)  NOT =  ZERO
                   IF    IDX26  <=  IDX28
                     MOVE    1   TO  FLG-CHECK-MAE
                   ELSE
                     MOVE    1   TO  FLG-CHECK-ATO
                   END-IF
                 END-IF
               END-PERFORM
               IF    FLG-CHECK-MAE = ZERO
                 MOVE    SPACE   TO  WRK-ZAIKYO-YMD
                 MOVE    SPACE   TO  WRK-ZAIKYO-YMD2
               END-IF
             ELSE
                 MOVE    SPACE   TO  WRK-ZAIKYO-YMD2
             END-IF
           END-IF
      *
           .
       20011-ZAIKYO-CHECK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºßÂð´µ¼Ô¶¦Æ±¿ÇÎÅÎÁ³«»ÏÆü¤ÎºÞ¸¡º÷
      *****************************************************************
       20011-ZAIKYO-KENSAKU-SEC          SECTION.
      *
           MOVE    ZERO                  TO  FLG-ZAIKYO-OK
           MOVE    SPACE                 TO  WRK-ZAIKYO-NYUGAIKBN
           MOVE    SPACE                 TO  WRK-ZAIKYO-SRYKA
           MOVE    ZERO                  TO  WRK-ZAIKYO-ZAINUM
      *
           PERFORM   VARYING   IDX15    FROM   2   BY  -1
                      UNTIL   (IDX15          <    1)
                       OR     (FLG-ZAIKYO-OK  =    1)
             INITIALIZE                      SRYACT-REC
             MOVE    RECE02-HOSPNUM      TO  SRY-HOSPNUM
             MOVE    IDX15               TO  SRY-NYUGAIKBN
             MOVE    RECE02-PTID         TO  SRY-PTID
             MOVE    WRK-ZAIKYO-YMD(1:6) TO  SRY-SRYYM
             MOVE    CONS-ZAIKYO         TO  SRY-SRYCD (1)
             MOVE    CONS-ZAIKYO         TO  SRY-SRYCD (2)
             MOVE    CONS-ZAIKYO         TO  SRY-SRYCD (3)
             MOVE    CONS-ZAIKYO         TO  SRY-SRYCD (4)
             MOVE    CONS-ZAIKYO         TO  SRY-SRYCD (5)
             MOVE    SRYACT-REC          TO  MCPDATA-REC
             MOVE    "tbl_sryact"        TO  MCP-TABLE
             MOVE    "key7"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"      TO  MCP-TABLE
               MOVE    "key7"            TO  MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL   FLG-SRYACT     =   1
                         OR     FLG-ZAIKYO-OK  =   1
                 INITIALIZE                  SRYACCT-REC
                 MOVE    SRY-HOSPNUM     TO  ACCT-HOSPNUM
                 MOVE    SRY-NYUGAIKBN   TO  ACCT-NYUGAIKBN
                 MOVE    SRY-PTID        TO  ACCT-PTID
                 MOVE    SRY-SRYKA       TO  ACCT-SRYKA
                 MOVE    SRY-SRYYM       TO  ACCT-SRYYM
                 MOVE    SRY-ZAINUM      TO  ACCT-ZAINUM
                 MOVE    SRYACCT-REC     TO  MCPDATA-REC
                 MOVE    "tbl_sryacct"   TO  MCP-TABLE
                 MOVE    "key31"         TO  MCP-PATHNAME
                 PERFORM 900-DBSELECT-SEC
                 IF      MCP-RC          =   ZERO
                    MOVE "tbl_sryacct"   TO  MCP-TABLE
                    MOVE "key31"         TO  MCP-PATHNAME
                    PERFORM 900-DBFETCH-SEC
                    IF      MCP-RC       =   ZERO
                      MOVE  MCPDATA-REC  TO  SRYACCT-REC
      *
                      MOVE  WRK-ZAIKYO-YMD(7:2)
                                         TO  DAY-IDX2
                      IF   ACCT-DAY(1 DAY-IDX2)  NOT =  ZERO
                        MOVE   1   TO  FLG-ZAIKYO-OK
                        MOVE   ACCT-NYUGAIKBN
                                   TO  WRK-ZAIKYO-NYUGAIKBN
                        MOVE   ACCT-SRYKA
                                   TO  WRK-ZAIKYO-SRYKA
                        MOVE   ACCT-ZAINUM
                                   TO  WRK-ZAIKYO-ZAINUM
                      END-IF
                    END-IF
                 END-IF
                 MOVE    "tbl_sryacct"   TO  MCP-TABLE
                 MOVE    "key31"         TO  MCP-PATHNAME
                 PERFORM   900-CLOSE-SEC
      *
                 MOVE    "tbl_sryact"    TO  MCP-TABLE
                 MOVE    "key7"          TO  MCP-PATHNAME
                 PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
             END-IF
             MOVE    "tbl_sryact"        TO  MCP-TABLE
             MOVE    "key7"              TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
           END-PERFORM
      *
      *++++
      *
           IF      WRK-ZAIKYO-ZAINUM    NOT =    ZERO
              INITIALIZE                         SRYACT-REC
              MOVE    RECE02-HOSPNUM         TO  SRY-HOSPNUM
              MOVE    WRK-ZAIKYO-NYUGAIKBN   TO  SRY-NYUGAIKBN
              MOVE    RECE02-PTID            TO  SRY-PTID
              MOVE    WRK-ZAIKYO-SRYKA       TO  SRY-SRYKA
              MOVE    WRK-ZAIKYO-YMD(1:6)    TO  SRY-SRYYM
              MOVE    WRK-ZAIKYO-ZAINUM      TO  SRY-ZAINUM
              MOVE    SRYACT-REC             TO  MCPDATA-REC
              MOVE    "tbl_sryact"           TO  MCP-TABLE
              MOVE    "key2"                 TO  MCP-PATHNAME
              PERFORM 900-DBSELECT-SEC
              IF      MCP-RC                 =   ZERO
                 MOVE    "tbl_sryact"        TO  MCP-TABLE
                 MOVE    "key2"              TO  MCP-PATHNAME
                 PERFORM  900-SRYACT-READ-SEC
                 PERFORM  UNTIL  (FLG-SRYACT       =  1)
                           OR    (FLG-ZAIKYOCOMARI =  1)
                   PERFORM  VARYING  IDX12  FROM  1  BY  1
                             UNTIL  (IDX12             >    5   )
                              OR    (SRY-SRYCD(IDX12)  =   SPACE)
                              OR    (FLG-ZAIKYOCOMARI  =    1   )
                      IF  SRY-SRYCD(IDX12)  =   "840000086"
                         MOVE    1    TO   FLG-ZAIKYOCOMARI
                      END-IF
                   END-PERFORM
                   MOVE    "tbl_sryact"      TO  MCP-TABLE
                   MOVE    "key2"            TO  MCP-PATHNAME
                   PERFORM   900-SRYACT-READ-SEC
                 END-PERFORM
              END-IF
              MOVE    "tbl_sryact"           TO  MCP-TABLE
              MOVE    "key2"                 TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
       20011-ZAIKYO-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºßÂð´µ¼ÔË¬Ìä¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó»ØÆ³´ÉÍýÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-ZAIRIHA-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-ZAIRIHA-COMMENT   =    "1"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       32      TO     WRK-JIDO-SETCOMATFLG
               MOVE       9       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-ZAIRIHA-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºßÂð´µ¼ÔË¬ÌäÌôºÞ´ÉÍý»ØÆ³ÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-ZAIYAK-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-ZAIYAK-COMMENT   =    "1"
           AND    RECE04-ZAIKAISU            >=     2
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       36      TO     WRK-JIDO-SETCOMATFLG
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-ZAIYAK-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ë¬Ìä¿ÇÎÅ¤Ë¤«¤«¤ëµ­Ï¿½ñ¥³¥á¥ó¥È¡ÊÆü¡¦¿Í¿ô¡Ë¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-HOMONCOM-HANTEI-SEC          SECTION.
      *
           IF     RECE04-HOMONCOMFLG    =  "1"
           AND    TBL-HOMONCOM-SETFLG   =   1
      *
             INITIALIZE            WRK-JIDO-SETCOM-AREA
      *
             MOVE       99         TO   FLG-COMMENT
      *
             MOVE       ZERO       TO   IDZ
             INITIALIZE            WRK-FREE-AREA2
      *
             PERFORM   VARYING   IDX    FROM   1   BY   1
                        UNTIL    IDX      >   31
      *
               IF    TBL-HOMONCOM-NINZU(IDX)  NOT = ZERO
                 ADD      1                TO  IDZ
                 MOVE     "¡¡¡¡¡¡¡¡¡¡¡¡¡¡Æü¡Ê¡¡¡¡¡¡¿Í¡Ë"
                                           TO  WK-FREE-NAME2(IDZ)
                 IF    IDZ  =  1
                   MOVE   "¿ÇÎÅ¿Í¿ô¡§"     TO  WK-FREE-NAME2(IDZ)(1:10)
                 END-IF
                 MOVE     TBL-DAY(IDX)     TO  WK-FREE-NAME2(IDZ)(11:4)
                 INITIALIZE                    WRK-CHAR-HENKAN-AREA
                 MOVE     TBL-HOMONCOM-NINZU(IDX)
                                           TO  WRK-SRYKAISU-X
                 MOVE     WRK-SRYKAISU-X   TO  WRK-CHAR-X
                 PERFORM  2502111-NUM-ZENKAKU-HENKAN-SEC
                 EVALUATE WRK-CHAR-N-CNT
                   WHEN       2
                     MOVE WRK-CHAR-N(1:2)  TO  WK-FREE-NAME2(IDZ)(23:2)
                   WHEN       4
                     MOVE WRK-CHAR-N(1:4)  TO  WK-FREE-NAME2(IDZ)(21:4)
                   WHEN       6
                     MOVE WRK-CHAR-N(1:6)  TO  WK-FREE-NAME2(IDZ)(19:6)
                 END-EVALUATE
                 MOVE     14               TO  WK-FREE-YUKOKETA2(IDZ)
               END-IF
      *
             END-PERFORM
      *
             PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           END-IF
      *
           .
       2001-HOMONCOM-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼§µ¤¤Ë¤è¤ëç¯æùÅù»É·ãË¡¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-BOKOHO-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-BOKOHO-COMMENT   =    "1"
             MOVE    RECE02-SRYYM        TO  WRK-BOKOHO-YMD9(1:6)
             MOVE    "99"                TO  WRK-BOKOHO-YMD9(7:2)
      *
             MOVE    ZERO                TO  FLG-BOKOHO-COMMENT
             MOVE    SPACE               TO  WRK-BOKOHO-YMD
             MOVE    ZERO                TO  FLG-RETRY
             INITIALIZE                      SANTEI-REC
             MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE02-PTID         TO  SANTEI-PTID
             MOVE    CONS-BOKOHO1        TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI          =   1 )
                             OR   ( FLG-BOKOHO-COMMENT  =   1 )
      *
                   IF   SANTEI-SRYYM  <=  RECE02-SRYYM
                     MOVE  SANTEI-SRYYM     TO  WRK-BOKOHO-YMD(1:6)
                     MOVE  SANTEI-MSANTEID  TO  WRK-BOKOHO-YMD(7:2)
                     MOVE    1              TO  FLG-BOKOHO-COMMENT
      *
                     IF   SANTEI-SRYYM  =  RECE02-SRYYM
                       MOVE   ZERO  TO  FLG-CHECK-MAE
                       MOVE   ZERO  TO  FLG-CHECK-ATO
                       MOVE  WRK-BOKOHO-YMD(7:2)  TO  IDX28
                       PERFORM  VARYING  IDX26  FROM   1  BY  1
                                 UNTIL   IDX26   >    31
                         IF  RECE04-KAISU(1 IDX26)  NOT =  ZERO
                           IF    IDX26  <  IDX28
                             MOVE    1   TO  FLG-CHECK-MAE
                           ELSE
                             MOVE    1   TO  FLG-CHECK-ATO
                           END-IF
                         END-IF
                       END-PERFORM
                       IF  ((FLG-CHECK-MAE = 1   ) AND
                            (FLG-CHECK-ATO = ZERO))
                       OR  ((FLG-CHECK-MAE = 1   ) AND
                            (FLG-CHECK-ATO = 1   ))
                         MOVE    WRK-BOKOHO-YMD
                                         TO  WRK-BOKOHO-YMD9
                         MOVE    SPACE   TO  WRK-BOKOHO-YMD
                         MOVE    1       TO  FLG-RETRY
                       END-IF
                     END-IF
                   END-IF
      *
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
             END-IF
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             IF   FLG-RETRY =  1
               MOVE    ZERO                TO  FLG-BOKOHO-COMMENT
               MOVE    SPACE               TO  WRK-BOKOHO-YMD
               MOVE    ZERO                TO  FLG-RETRY
               INITIALIZE                      SANTEI-REC
               MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
               MOVE    RECE02-PTID         TO  SANTEI-PTID
               MOVE    CONS-BOKOHO1        TO  SANTEI-SRYCD
               MOVE    ZERO                TO  SANTEI-NYUGAIKBN
               MOVE    ZERO                TO  SANTEI-SRYKA
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI          =   1 )
                             OR   ( FLG-BOKOHO-COMMENT  =   1 )
      *
                   IF   SANTEI-SRYYM  <   RECE02-SRYYM
                     MOVE  SANTEI-SRYYM     TO  WRK-BOKOHO-YMD(1:6)
                     MOVE  SANTEI-MSANTEID  TO  WRK-BOKOHO-YMD(7:2)
                     MOVE    1              TO  FLG-BOKOHO-COMMENT
                   END-IF
      *
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
               END-IF
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
             END-IF
      *
             IF   WRK-BOKOHO-YMD   NOT =  SPACE
               MOVE    ZERO                TO  IDX27
               INITIALIZE                      WRK-BOKOHO-YMD2-AREA
               INITIALIZE                      SANTEI-REC
               MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
               MOVE    RECE02-PTID         TO  SANTEI-PTID
               MOVE    CONS-BOKOHO2        TO  SANTEI-SRYCD
               MOVE    ZERO                TO  SANTEI-NYUGAIKBN
               MOVE    ZERO                TO  SANTEI-SRYKA
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL   FLG-SANTEI  =   1
      *
                   IF   SANTEI-SRYYM  <=  RECE02-SRYYM
                   AND  SANTEI-SRYYM  >=  WRK-BOKOHO-YMD(1:6)
                     MOVE  WRK-BOKOHO-YMD(7:2)  TO  IDX28
                     MOVE  WRK-BOKOHO-YMD9(7:2) TO  IDX29
                     PERFORM  VARYING  IDX26  FROM  31  BY  -1
                               UNTIL   IDX26   <     1
                       IF  (SANTEI-DAY(IDX26)  NOT =  ZERO)
                       AND (IDX27              <    30    )
                         IF ((SANTEI-SRYYM = WRK-BOKOHO-YMD(1:6) ) AND
                             (IDX26        < IDX28               ))
                         OR ((SANTEI-SRYYM = WRK-BOKOHO-YMD9(1:6)) AND
                             (IDX26       >= IDX29               ))
      *                  ½é²ó¼Â»ÜÆü¤ÏÄÌ»»¼Â»ÜÆü¤È¤·¤Æµ­ºÜ¤·¤Ê¤¤
                         OR ((SANTEI-SRYYM = WRK-BOKOHO-YMD(1:6) ) AND
                             (IDX26        = IDX28               ))
                           CONTINUE
                         ELSE
                           ADD   1      TO  IDX27
                           MOVE  SANTEI-SRYYM
                                        TO  WRK-BOKOHO-YMD2(IDX27)(1:6)
                           MOVE  IDX26  TO  WRK-BOKOHO-YMD2(IDX27)(7:2)
                         END-IF
                       END-IF
                     END-PERFORM
                   END-IF
      *
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
               END-IF
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               INITIALIZE                WRK-JIDO-SETCOM-AREA
      ***      MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
      ***      MOVE       31      TO     WRK-JIDO-SETCOMATFLG
               MOVE       99      TO     FLG-COMMENT
      *
               MOVE      ZERO     TO     IDZ
               INITIALIZE                WRK-FREE-AREA2
      *
               ADD        1       TO     IDZ
      *
               MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
               MOVE   "½é²ó¼Â»Ü¡¡¡¡¡¡·î¡¡¡¡Æü"
                                  TO     WK-FREE-NAME2(IDZ)
      *
               MOVE    WRK-BOKOHO-YMD(5:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                                  TO     WK-FREE-NAME2(IDZ)(11:4)
               MOVE    WRK-BOKOHO-YMD(7:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                                  TO     WK-FREE-NAME2(IDZ)(17:4)
      *
               IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                 MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                 MOVE  WRK-JIDO-SETCOMATFLG
                                  TO     WK-FREE-SETCOMATFLG2(IDZ)
               END-IF
      *
               IF   WRK-BOKOHO-YMD2(1) NOT =  SPACE
                 MOVE         1       TO     IDX27
                 PERFORM  VARYING  IDX26  FROM  30  BY  -1
                           UNTIL   IDX26   <     1
                  IF  WRK-BOKOHO-YMD2(IDX26)  NOT =  SPACE
                   ADD        1       TO     IDZ
      *
                   ADD        1       TO     IDX27
                   IF    IDX27   <    10
                     MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                     MOVE     "¡¡²óÌÜ¡¡¡¡¡¡¡¡·î¡¡¡¡Æü"
                                        TO     WK-FREE-NAME2(IDZ)
                     MOVE      IDX27    TO     DAY-IDX2
                     MOVE      TBL-DAY(DAY-IDX2)(3:2)
                                        TO     WK-FREE-NAME2(IDZ)(1:2)
                   ELSE
                     MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                     MOVE     "¡¡¡¡²óÌÜ¡¡¡¡¡¡·î¡¡¡¡Æü"
                                        TO     WK-FREE-NAME2(IDZ)
                     MOVE      IDX27    TO     DAY-IDX2
                     MOVE      TBL-DAY(DAY-IDX2)
                                        TO     WK-FREE-NAME2(IDZ)(1:4)
                   END-IF
                   MOVE      WRK-BOKOHO-YMD2(IDX26)(5:2) TO  DAY-IDX2
                   MOVE      TBL-DAY(DAY-IDX2)
                                        TO     WK-FREE-NAME2(IDZ)(11:4)
                   MOVE      WRK-BOKOHO-YMD2(IDX26)(7:2) TO  DAY-IDX2
                   MOVE      TBL-DAY(DAY-IDX2)
                                        TO     WK-FREE-NAME2(IDZ)(17:4)
      *
                   IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                     MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                     MOVE  WRK-JIDO-SETCOMATFLG
                                      TO     WK-FREE-SETCOMATFLG2(IDZ)
                   END-IF
                  END-IF
                 END-PERFORM
               END-IF
      *
               PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-BOKOHO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¹âµ¤°µ»ÀÁÇ¼£ÎÅ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-KIATU-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-KIATU-COMMENT    NOT =  SPACE
             MOVE   CONS-KIATU3   TO   WRK-CHECK-SRYCD
             PERFORM       20011-ZAIKYO-CHECK-SEC
      *
             MOVE       SPACE               TO  WRK-SJIYMD
             MOVE       ZERO                TO  WRK-ZAIKYO-KAISU
             IF   WRK-ZAIKYO-YMD   NOT =  SPACE
                INITIALIZE                      SANTEI-REC
                MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
                MOVE    RECE02-PTID         TO  SANTEI-PTID
                EVALUATE  RECE04-FLG-KIATU-COMMENT
                  WHEN      "1"
                    MOVE    CONS-KIATU      TO  SANTEI-SRYCD
                  WHEN      "2"
                    MOVE    CONS-KIATU2     TO  SANTEI-SRYCD
                END-EVALUATE
                MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                MOVE    ZERO                TO  SANTEI-SRYKA
                MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
                MOVE    SANTEI-REC          TO  MCPDATA-REC
                MOVE    "tbl_santei"        TO  MCP-TABLE
                MOVE    "key3"              TO  MCP-PATHNAME
                PERFORM 900-DBSELECT-SEC
                IF      MCP-RC              =   ZERO
                  MOVE    "tbl_santei"      TO  MCP-TABLE
                  MOVE    "key3"            TO  MCP-PATHNAME
                  PERFORM  900-SANTEI-READ-SEC
                  PERFORM   UNTIL   FLG-SANTEI  =   1
      *
                    IF   SANTEI-SRYYM  <=  RECE02-SRYYM
                    AND  SANTEI-SRYYM  >=  WRK-ZAIKYO-YMD(1:6)
                      MOVE  WRK-ZAIKYO-YMD(7:2)  TO  IDX28
                      MOVE  WRK-ZAIKYO-YMD9(7:2) TO  IDX29
                      PERFORM  VARYING  IDX26  FROM  31  BY  -1
                                UNTIL   IDX26   <     1
                        IF   SANTEI-DAY(IDX26)  NOT =  ZERO
                          IF ((SANTEI-SRYYM =
                                      WRK-ZAIKYO-YMD(1:6) ) AND
                              (IDX26        < IDX28       ))
                          OR ((SANTEI-SRYYM =
                                      WRK-ZAIKYO-YMD9(1:6)) AND
                              (IDX26       >= IDX29       ))
                            CONTINUE
                          ELSE
                            ADD   1       TO  WRK-ZAIKYO-KAISU
                            MOVE  SANTEI-SRYYM
                                          TO  WRK-SJIYMD(1:6)
                            MOVE  IDX26   TO  WRK-SJIYMD(7:2)
                          END-IF
                        END-IF
                      END-PERFORM
                    END-IF
      *
                    MOVE    "tbl_santei"    TO  MCP-TABLE
                    MOVE    "key3"          TO  MCP-PATHNAME
                    PERFORM  900-SANTEI-READ-SEC
                  END-PERFORM
                END-IF
                MOVE    "tbl_santei"        TO  MCP-TABLE
                MOVE    "key3"              TO  MCP-PATHNAME
                PERFORM 900-CLOSE-SEC
             END-IF
      *
             IF   WRK-ZAIKYO-KAISU  NOT =  ZERO
             AND  WRK-ZAIKYO-KAISU     <=   31
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE       35      TO     WRK-JIDO-SETCOMATFLG
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                IF    RECE04-FLG-KIATU-COMMENT  =  "1"
      *
                  ADD        1       TO     IDZ
      *
                  MOVE       9       TO     WK-FREE-YUKOKETA2(IDZ)
                  MOVE   "È¯¾É¡¡¡¡¡¡·î¡¡¡¡Æü"
                                     TO     WK-FREE-NAME2(IDZ)
      *
                  MOVE    WRK-ZAIKYO-YMD(5:2)  TO  DAY-IDX2
                  MOVE    TBL-DAY(DAY-IDX2)
                                     TO     WK-FREE-NAME2(IDZ)(7:4)
                  MOVE    WRK-ZAIKYO-YMD(7:2)  TO  DAY-IDX2
                  MOVE    TBL-DAY(DAY-IDX2)
                                     TO     WK-FREE-NAME2(IDZ)(13:4)
      *
                  IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                    MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                    MOVE  WRK-JIDO-SETCOMATFLG
                                     TO     WK-FREE-SETCOMATFLG2(IDZ)
                  END-IF
      *
                END-IF
      *
                ADD        1       TO     IDZ
      *
                MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "½é²ó¼Â»Ü¡¡¡¡¡¡·î¡¡¡¡Æü"
                                   TO     WK-FREE-NAME2(IDZ)
      *
                MOVE    WRK-SJIYMD(5:2)   TO  DAY-IDX2
                MOVE    TBL-DAY(DAY-IDX2)
                                   TO     WK-FREE-NAME2(IDZ)(11:4)
                MOVE    WRK-SJIYMD(7:2)   TO  DAY-IDX2
                MOVE    TBL-DAY(DAY-IDX2)
                                   TO     WK-FREE-NAME2(IDZ)(17:4)
      *
                IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                  MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                  MOVE  WRK-JIDO-SETCOMATFLG
                                   TO     WK-FREE-SETCOMATFLG2(IDZ)
                END-IF
      *
                ADD        1       TO     IDZ
      *
                MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "ÄÌ»»¼Â»Ü²ó¿ô¡Ê¡¡¡¡²ó¡Ë"
                                   TO     WK-FREE-NAME2(IDZ)
                MOVE    WRK-ZAIKYO-KAISU  TO  DAY-IDX2
                MOVE    TBL-DAY(DAY-IDX2)
                                   TO     WK-FREE-NAME2(IDZ)(15:4)
      *
                IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                  MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                  MOVE  WRK-JIDO-SETCOMATFLG
                                   TO     WK-FREE-SETCOMATFLG2(IDZ)
                END-IF
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-KIATU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿Í¹©¿ÕÂ¡Åù¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-JINZOUETC-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-JINZOU-COMMENT   =    "1"
             IF   RECE02-SRYYM  >= "201810"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       41      TO     WRK-JIDO-SETCOMATFLG
               MOVE       1       TO     WRK-JIDO-JYOUGEFLG
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
             ELSE
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
             END-IF
           END-IF
      *
           .
       2001-JINZOUETC-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¶É½ê±¢°µÊÄº¿½èÃÖ¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-KYOKUIN-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-KYOKUIN-COMMENT   =    "1"
             IF   RECE02-SRYYM  >= "201810"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       42      TO     WRK-JIDO-SETCOMATFLG
               MOVE       1       TO     WRK-JIDO-JYOUGEFLG
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
             ELSE
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
             END-IF
           END-IF
      *
           .
       2001-KYOKUIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ê½ÑÆü¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-SHUJUTSU-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-SHUJUTU-COMMENT   =    SPACE
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE       1       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
               PERFORM    2099-RECE04-TENSOU-SEC
           END-IF
      *
           .
       2001-SHUJUTSU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ëã¿ìÆü¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-MASUI-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-MASUI-COMMENT   =    SPACE
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE       8       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
               PERFORM    2099-RECE04-TENSOU-SEC
           END-IF
      *
           .
       2001-MASUI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¡Ê¸¡Áª¡Ë¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-KENSEN-HANTEI-SEC          SECTION.
      *
           IF     RECE04-SAFLG   =    "2"
               MOVE       99      TO     FLG-COMMENT
               PERFORM    20011-KENSENTOU-COMMENT-SEC
           END-IF
      *
           .
       2001-KENSEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¡Ê¸¡Áª¡ËÅù¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       20011-KENSENTOU-COMMENT-SEC          SECTION.
      *
           PERFORM  200111-RECE05-TAIHI-SEC
      *
           MOVE    ZERO                 TO  IDX-RECE05
           MOVE    SPACE                TO  WRK-RECE05XX-REC
           INITIALIZE                       WRK-RECE05XX-REC
      *
           PERFORM
               UNTIL  ( FLG-RECE05           =   1                   )
               OR     ( RECE04-KEY1      NOT =   RECE05-KEY1         )
               OR     ( RECE04-ZAINUM    NOT =   RECE05-ZAINUM       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   5     )
                         OR      ( RECE05-SRYCD(IDX) =   SPACE )
                    ADD    1             TO      IDX-RECE05
                    MOVE   RECE05-SRYACT-TBL(IDX)
                             TO   WRK-RECE05XX-SRYACT-TBL(IDX-RECE05 1)
      *++++++
                    IF    IDX-RECE05  =  RECE04-MEISAISU
      *
                      IF  WRK-RECE05XX-SRYCD(IDX-RECE05 1) = "810000001"
                      OR  WRK-RECE05XX-SRYCD(IDX-RECE05 1)(1:3)  = "008"
      *
                        COMPUTE   IDV  =
                         (WRK-RECE05XX-YUKOKETA(IDX-RECE05 1) * 2) + 1
      *
                        IF   WRK-RECE05XX-YUKOKETA(IDX-RECE05 1) <=  36
                          MOVE    "¡¡¡ß"   TO
                           WRK-RECE05XX-INPUTCOMENT(IDX-RECE05 1)(IDV:4)
                        END-IF
                        IF   WRK-RECE05XX-YUKOKETA(IDX-RECE05 1) <=  96
                          MOVE    "¡¡¡ß"   TO
                           WRK-RECE05XX-NAME(IDX-RECE05 1)(IDV:4)
                        END-IF
      *
                        COMPUTE   IDV  =  IDV  +  4
      *
                        MOVE    RECE04-ZAIKAISU   TO   DAY-IDX2
                        PERFORM 20011-KAISU-HENSYU-SEC
                        IF   WRK-RECE05XX-YUKOKETA(IDX-RECE05 1) <=  36
                          MOVE    WRK-HENSYU-KAISU2  TO
                           WRK-RECE05XX-INPUTCOMENT(IDX-RECE05 1)(IDV:4)
                        END-IF
                        IF   WRK-RECE05XX-YUKOKETA(IDX-RECE05 1) <=  96
                          MOVE    WRK-HENSYU-KAISU2  TO
                           WRK-RECE05XX-NAME(IDX-RECE05 1)(IDV:4)
                          COMPUTE WRK-RECE05XX-YUKOKETA(IDX-RECE05 1) =
                                  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1) +
                                  4
                        END-IF
      *
                      END-IF
      *
                    END-IF
      *++++++
               END-PERFORM 
      *
               PERFORM 990-RECE05-NEXT-SEC
      *
           END-PERFORM
      *    ¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE05
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE05
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE05  =    1
                    INITIALIZE                        RECE059-REC
                    MOVE    WRK-RECE05-KEY       TO   RECE059-KEY
                    ADD     1                    TO   WRK-RENNUM
                    MOVE    WRK-RENNUM           TO   RECE059-RENNUM
                    MOVE    WRK-RECE05-KOHBUNRI  TO   RECE059-KOHBUNRI
                    MOVE    WRK-RECE05-PTNUM     TO   RECE059-PTNUM
                    MOVE    WRK-RECE05-SRYKA     TO   RECE059-SRYKA
                    MOVE    WRK-RECE05-SRYKBN    TO   RECE059-SRYKBN
                    MOVE    WRK-RECE05-SRYSYUKBN TO   RECE059-SRYSYUKBN
                    MOVE    WRK-RECE05-JIHIMONEY TO   RECE059-JIHIMONEY
                    PERFORM  VARYING   IDX   FROM   1   BY   1
                             UNTIL   (IDX       >    5)   OR
                                     (FLG-WRKRECE05   =   1)
                       ADD         1             TO    IDX-RECE05
                       IF (WRK-RECE05XX-SRYCD(IDX-RECE05 1) = SPACE)
                       OR (IDX-RECE05                       >  100 )
                           MOVE    1             TO    FLG-WRKRECE05
                       ELSE
                           MOVE  WRK-RECE05XX-SRYACT-TBL(IDX-RECE05 1)
                                            TO   RECE059-SRYACT-TBL(IDX)
                           ADD   1          TO   WRK-CNT-MEISAI
                       END-IF  
                    END-PERFORM
                    IF   RECE059-SRYCD(1)  NOT =  SPACE
                       WRITE   RECE059-REC
                       ADD     1            TO   CNT-RECE059
                    END-IF
           END-PERFORM
      *
           MOVE    RECE04-REC        TO      RECE049-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE049-MEISAISU
           WRITE   RECE049-REC
           ADD     1                 TO      CNT-RECE049
      *
           .
       20011-KENSENTOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸¡ºº¡Ê£Â£Î£Ð¡¢£Î£Ô¡Ý£ð£ò£ï£Â£Î£Ð¡¢£È£Á£Î£Ð¡Ë¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-KENSA-HANTEI-SEC          SECTION.
      *
           IF  ((RECE02-SRYYM  >= "201804"       ) AND
                (RECE04-FLG-KENSA-COMMENT  =  "1") AND
                (CNT-KENSA                >=   2 ))
           OR  ((RECE02-SRYYM  <= "201803"       ) AND
                (RECE04-FLG-KENSA-COMMENT  =  "1"))
               INITIALIZE            WRK-JIDO-SETCOM-AREA
      ***      MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
      ***      MOVE       15      TO     WRK-JIDO-SETCOMATFLG
               MOVE       7       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-KENSA-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ã£Ô¡¦£Í£Ò£É¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-CTMRI-HANTEI-SEC          SECTION.
      *
           IF      RECE04-FLG-CTMRI-COMMENT   =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE       2       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
                PERFORM    2099-RECE04-TENSOU-SEC
           END-IF
      *
           .
       2001-CTMRI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ê½ÑÆüÅù¤Î¥³¥á¥ó¥ÈÊÔ½¸¡Êº¹¹þ¡§¾å¡Ë
      *****************************************************************
       20011-SHUJUTSU-ETC-COMMENT-SEC          SECTION.
      *
           PERFORM   200111-SANTEI-DAY-SEC
      *
           MOVE       ZERO       TO   IDZ                               
           INITIALIZE            WRK-FREE-AREA2                         
      *
           PERFORM  20011-FREE-COMMENT2-SEC                              
      *
           PERFORM  200111-RECE05-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE05
           MOVE    SPACE                TO  WRK-RECE05XX-REC
           INITIALIZE                       WRK-RECE05XX-REC
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                ADD    1                  TO  IDX-RECE05
                MOVE   "810000001"
                              TO  WRK-RECE05XX-SRYCD(IDX-RECE05 1)
                MOVE   ZERO   TO  WRK-RECE05XX-SRYSURYO(IDX-RECE05 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                              TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                MOVE   WK-FREE-NAME2(IDX)
                              TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                IF     WRK-JIDO-JYOUGEFLG  =  1
                  MOVE  "1"   TO  WRK-RECE05XX-RECEDENFLG(IDX-RECE05 1)
                END-IF
           END-PERFORM 
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-SHUJUTSU-ETC-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ê½ÑÆüÅù¤Î¥³¥á¥ó¥ÈÊÔ½¸£²¡Êº¹¹þ¡§²¼¡Ë
      *****************************************************************
       20011-SHUJUTSU-ETC2-COMMENT-SEC          SECTION.
      *
           PERFORM   200111-SANTEI-DAY-SEC
      *
           MOVE       ZERO       TO   IDZ                               
           INITIALIZE            WRK-FREE-AREA2                         
      *
           PERFORM  20011-FREE-COMMENT2-SEC                              
      *
           PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           .
       20011-SHUJUTSU-ETC2-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³ç¿ÇÎÅÊ¬¤Î¥³¥á¥ó¥ÈÊÔ½¸¡Êº¹¹þ¡§¾å¡Ë
      *****************************************************************
       20011-HOKATU-COMMENT-SEC          SECTION.
      *
           PERFORM  200111-RECE05-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE   ZERO          TO  IDX-RECE05
           MOVE   SPACE         TO  WRK-RECE05XX-REC
           INITIALIZE               WRK-RECE05XX-REC
      *
           ADD    1             TO  IDX-RECE05
           MOVE  "810000001"    TO  WRK-RECE05XX-SRYCD(IDX-RECE05 1)
           MOVE   ZERO          TO  WRK-RECE05XX-SRYSURYO(IDX-RECE05 1)
           MOVE   6             TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
           MOVE  "¡ÚÊñ³ç¿ÇÎÅ¡Û" TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
      *
           PERFORM  200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-HOKATU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    »»ÄêÆü¤Î³ÎÄê
      *****************************************************************
       200111-SANTEI-DAY-SEC          SECTION.
      *
           INITIALIZE                TBL-SANTEI-DAY
      *
           MOVE    ZERO          TO  DAY-IDX
           PERFORM    VARYING    IDX  FROM   1   BY   1
                      UNTIL     (IDX      >   31)  OR
                                (DAY-IDX  >   31)
                IF   RECE04-KAISU(1 IDX)  NOT = ZERO
                      ADD   1     TO   DAY-IDX
                      MOVE  IDX   TO   TB-SANTEI-DAY(DAY-IDX)
                END-IF
           END-PERFORM
      *
           .
       200111-SANTEI-DAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ê½ÑÆüÅù¤ÎÆüÉÕ¤ÎÊÔ½¸
      *****************************************************************
       20011-FREE-COMMENT2-SEC          SECTION.                         
      *
           ADD      1          TO    IDZ
      *
           EVALUATE   FLG-COMMENT
               WHEN    1
                 MOVE     4          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "¼ê½Ñ"  TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
               WHEN    2
                 MOVE     4          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "¼Â»Ü"  TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
               WHEN    3
                 MOVE     4          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "»»Äê"  TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
               WHEN    4
                 IF   RECE02-SRYYM >= "201905"
                   MOVE     22         TO    WRK-KETA
                                             WRK-KETA-TAIHI
                   MOVE     11         TO    WRK-KETA2
                   MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                   MOVE    "¸òÉÕ¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î"
                                 TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
                   MOVE     11         TO     IDX41
                   MOVE     17         TO     IDX42
                   MOVE      7         TO     IDX49
                 ELSE
                   MOVE     18         TO    WRK-KETA
                                             WRK-KETA-TAIHI
                   MOVE     9          TO    WRK-KETA2
                   MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                   MOVE    "¸òÉÕ¡¡¡¡¡¡Ç¯¡¡¡¡·î"
                                 TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
                   MOVE      7         TO     IDX41
                   MOVE     13         TO     IDX42
                 END-IF
      *
                 INITIALIZE                      STS-AREA-DAY
                 INITIALIZE                      LNK-DAY2-AREA
                 MOVE    "21"                TO  LNK-DAY2-IRAI
                 MOVE    RECE02-SRYYM        TO  WRK-HENYMD(1:6)
                 MOVE    "01"                TO  WRK-HENYMD(7:2)
                 MOVE    WRK-HENYMD          TO  LNK-DAY2-YMD
                 CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                             LNK-DAY2-AREA
                 IF  STS-DAY-RC1             =   ZERO
                   MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                   END-IF
      *
                   MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                   END-IF
      *
                   IF   RECE02-SRYYM >= "201905"
                      MOVE    LNK-DAY2-EDTYMD2-G
                                   TO  WK-FREE-NAME2(IDZ)(IDX49:4)
                   END-IF
                 END-IF
               WHEN    5
                 MOVE     4          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "Ê¡»ã"  TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
               WHEN    6
                 MOVE     0          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     0          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
               WHEN    7
                 MOVE     8          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     4          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "¸¡ºº¼Â»Ü"
                             TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
               WHEN    8
                 MOVE     4          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "Ëã¿ì"  TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
               WHEN    9
                 MOVE     8          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     4          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "Ë¬Ìä»ØÆ³"
                             TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
           END-EVALUATE
      *
           IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
             MOVE  "2"                   TO  WK-FREE-MEISKYFLG2(IDZ)
             MOVE  WRK-JIDO-SETCOMATFLG  TO  WK-FREE-SETCOMATFLG2(IDZ)
           END-IF
      *
           MOVE      ZERO          TO  FLG-FIRST
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                          UNTIL   (IDX                 >    31)
                           OR     (TB-SANTEI-DAY(IDX)  =  ZERO)
      *
                 IF  FLG-FIRST  =  ZERO
                    IF  TB-SANTEI-DAY(IDX)     <  10 
      *
                       MOVE   TB-SANTEI-DAY(IDX)  TO  IDY
      *
                       ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                       MOVE    "¡¡"
                           TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                       ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                       MOVE    TBL-DAY(IDY)(3:2)     
                           TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                       ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                       MOVE    "Æü"
                           TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                    ELSE
      *
                       MOVE   TB-SANTEI-DAY(IDX)  TO  IDY
      *
                       IF   FLG-COMMENT   =   4 OR 6
                         ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                         MOVE    TBL-DAY(IDY)     
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
                       ELSE
                         ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                         MOVE    "¡¡"
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                         ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                         MOVE    TBL-DAY(IDY)     
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
                       END-IF
                       ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                       MOVE    "Æü"
                           TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                    END-IF
                    MOVE      1              TO  FLG-FIRST
                 ELSE
                    IF  TB-SANTEI-DAY(IDX)     <  10 
      *
                       MOVE   TB-SANTEI-DAY(IDX)  TO  IDY
      *
                       IF    WK-FREE-YUKOKETA2(IDZ)  =  25
                          PERFORM  200111-FREE-COMMENT2-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    "¡¢"
                                    TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "¡¢"
                                    TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                       END-IF
      *
                       IF    WK-FREE-YUKOKETA2(IDZ)  =  24 OR 25
                          PERFORM  200111-FREE-COMMENT2-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    TBL-DAY(IDY)(3:2)     
                                    TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "Æü"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2) 
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)(3:2)     
                                    TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "Æü"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2) 
                       END-IF
                    ELSE
      *
                       MOVE   TB-SANTEI-DAY(IDX)  TO  IDY
      *
                       IF    WK-FREE-YUKOKETA2(IDZ)  =  25
                          PERFORM  200111-FREE-COMMENT2-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    "¡¢"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "¡¢"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       END-IF
      *
                       IF    WK-FREE-YUKOKETA2(IDZ)  =  23 OR 24 OR 25
                          PERFORM  200111-FREE-COMMENT2-KAIGYO-SEC
                          ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    TBL-DAY(IDY)     
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "Æü"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)     
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "Æü"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       END-IF
                    END-IF
                 END-IF
      *
           END-PERFORM
           .
       20011-FREE-COMMENT2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ê½ÑÆüÅù¤ÎÆüÉÕ¤ÎÊÔ½¸¡Ê²þ¹Ô¡Ë
      *****************************************************************
       200111-FREE-COMMENT2-KAIGYO-SEC          SECTION.                  
      *
           ADD      1              TO  IDZ
      *
           MOVE    WRK-KETA-TAIHI  TO  WRK-KETA
           MOVE    WRK-KETA2       TO  WK-FREE-YUKOKETA2(IDZ)
           MOVE    ALL "¡¡"        TO  WK-FREE-NAME2(IDZ)(1:WRK-KETA)
      *
           IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
             MOVE  "2"                   TO  WK-FREE-MEISKYFLG2(IDZ)
             MOVE  WRK-JIDO-SETCOMATFLG  TO  WK-FREE-SETCOMATFLG2(IDZ)
           END-IF
      *
           .
       200111-FREE-COMMENT2-KAIGYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ê¥Ï¥Ó¥ê¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-RIHA-COMMENT-SEC          SECTION.
      *
             INITIALIZE            WRK-JIDO-SETCOM-AREA
             MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
             MOVE       16      TO     WRK-JIDO-SETCOMATFLG
      *
             PERFORM  200111-RECE05-TAIHI-SEC
      *
      *      £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
             MOVE    ZERO                 TO  IDX-RECE05
             MOVE    SPACE                TO  WRK-RECE05XX-REC
             INITIALIZE                       WRK-RECE05XX-REC
      *      ¥ê¥Ï¥Ó¥ê¥³¥á¥ó¥È¤ÎÁÞÆþ
             COMPUTE  MAX-MEISAISU        =   RECE04-MEISAISU  +  1
      *
             MOVE    "810000001"
                           TO  WRK-RECE05XX-SRYCD(MAX-MEISAISU 1)
             MOVE    ZERO  TO  WRK-RECE05XX-SRYSURYO(MAX-MEISAISU 1)
             MOVE    7     TO  WRK-RECE05XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE    "¼Â»ÜÆü¿ô¡¡¡¡Æü"
                           TO  WRK-RECE05XX-NAME(MAX-MEISAISU 1)
             MOVE    ZERO                 TO  CNT-RIHA
             PERFORM  VARYING   IDX  FROM  1  BY  1
                       UNTIL    IDX   >   31
                 IF   RECE04-KAISU(1 IDX)  NOT = ZERO
                        ADD      1        TO  CNT-RIHA
                 END-IF
             END-PERFORM
             IF      CNT-RIHA             NOT =    ZERO
               MOVE    CNT-RIHA             TO  DAY-IDX
               MOVE    TBL-DAY(DAY-IDX)
                           TO  WRK-RECE05XX-NAME(MAX-MEISAISU 1)(9:4)
             END-IF
      *
             IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
               MOVE  "2"   TO  WRK-RECE05XX-MEISKYFLG(MAX-MEISAISU 1)
               MOVE  WRK-JIDO-SETCOMATFLG
                           TO  WRK-RECE05XX-SETCOMATFLG(MAX-MEISAISU 1)
             END-IF
      *
             PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       2001-RIHA-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ê¥Ï¥Ó¥ê°å³Ø´ÉÍýÎÁ¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-RIHAIGAKU-COMMENT-SEC          SECTION.
      *
           INITIALIZE            WRK-JIDO-SETCOM-AREA
      *
           EVALUATE      RECE04-RIHABIRI-COMMENT
              WHEN           "2"
                  MOVE   CONS-SINDAI-IGAKU1   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-SINDAI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "3"
                  MOVE   CONS-SINDAI-IGAKU2   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-SINDAI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "4"
                  MOVE   CONS-KOKYUKI-IGAKU1  TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-KOKYUKI-DUMMY   TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "5"
                  MOVE   CONS-KOKYUKI-IGAKU2  TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-KOKYUKI-DUMMY   TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "6"
                  MOVE   CONS-NOUKE-IGAKU1    TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-NOUKE-DUMMY     TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "7"
                  MOVE   CONS-NOUKE-IGAKU2    TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-NOUKE-DUMMY     TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "8"
                  MOVE   CONS-UNDOKI-IGAKU1   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-UNDOKI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "9"
                  MOVE   CONS-UNDOKI-IGAKU2   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-UNDOKI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
           END-EVALUATE
      ***
           MOVE          SPACE          TO  WRK-RIHAIGAKU-FSANTEIYMD
      *    °å³Ø´ÉÍý¡§»»ÄêÍúÎò¸¡º÷
           INITIALIZE                       SANTEI-REC
grpsys     MOVE    RECE02-HOSPNUM       TO  SANTEI-HOSPNUM
           MOVE    RECE02-PTID          TO  SANTEI-PTID
           MOVE    RECE02-SRYYM         TO  SANTEI-SRYYM
           MOVE    WRK-RIHAIGAKU-SRYCD  TO  SANTEI-SRYCD
           MOVE    ZERO                 TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                 TO  SANTEI-SRYKA
           MOVE    ZERO                 TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC           TO  MCPDATA-REC
           MOVE    "tbl_santei"         TO  WRK-MCP-TABLE
           MOVE    "key"                TO  WRK-MCP-PATHNAME
           PERFORM  900-SANTEI-INV-SEC
           IF       FLG-SANTEI        =   ZERO
                MOVE  SANTEI-FSANTEIYMD TO  WRK-RIHAIGAKU-FSANTEIYMD
           END-IF
      ***
           INITIALIZE      WRK-RIHAIGAKU-SANTEI-AREA
      *    ¥ê¥Ï¼Â»Ü¥À¥ß¡¼¡§»»ÄêÍúÎò¸¡º÷
           INITIALIZE                       SANTEI-REC
grpsys     MOVE    RECE02-HOSPNUM       TO  SANTEI-HOSPNUM
           MOVE    RECE02-PTID          TO  SANTEI-PTID
           MOVE    RECE02-SRYYM         TO  SANTEI-SRYYM
           MOVE    WRK-RIHAIGAKU-SRYCD2 TO  SANTEI-SRYCD
           MOVE    ZERO                 TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                 TO  SANTEI-SRYKA
           MOVE    ZERO                 TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC           TO  MCPDATA-REC
           MOVE    "tbl_santei"         TO  WRK-MCP-TABLE
           MOVE    "key"                TO  WRK-MCP-PATHNAME
           PERFORM  900-SANTEI-INV-SEC
           IF       FLG-SANTEI        =   ZERO
               PERFORM  VARYING  IDX  FROM  1  BY  1
                         UNTIL   IDX    >  31
                 IF   SANTEI-DAY(IDX)   NOT =  ZERO
                    MOVE  "1"  TO  WRK-RIHAIGAKU-SANTEI-FLG
                    MOVE  SANTEI-DAY(IDX)
                               TO  WRK-RIHAIGAKU-SANTEI-DAY(IDX)
                 END-IF
               END-PERFORM
           END-IF
      *
      ***
      *
           PERFORM  200111-RECE05-TAIHI-SEC
      *
           MOVE    ZERO                 TO  IDX-RECE05
           MOVE    SPACE                TO  WRK-RECE05XX-REC
           INITIALIZE                       WRK-RECE05XX-REC
      *
           COMPUTE  MAX-MEISAISU        =   RECE04-MEISAISU  +  1
      *
           MOVE    "810000001"
                         TO  WRK-RECE05XX-SRYCD(MAX-MEISAISU 1)
           MOVE    ZERO  TO  WRK-RECE05XX-SRYSURYO(MAX-MEISAISU 1)
           MOVE    9     TO  WRK-RECE05XX-YUKOKETA(MAX-MEISAISU 1)
           MOVE    "³«»Ï¡¡¡¡¡¡·î¡¡¡¡Æü"
                         TO  WRK-RECE05XX-NAME(MAX-MEISAISU 1)
           IF      WRK-RIHAIGAKU-FSANTEIYMD  NOT = SPACE
             MOVE    WRK-RIHAIGAKU-FSANTEIYMD(5:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                         TO  WRK-RECE05XX-NAME(MAX-MEISAISU 1)(7:4)
             MOVE    WRK-RIHAIGAKU-FSANTEIYMD(7:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                         TO  WRK-RECE05XX-NAME(MAX-MEISAISU 1)(13:4)
           END-IF
      *
           IF      WRK-RIHAIGAKU-SANTEI-FLG  NOT = SPACE
             PERFORM    20011-RIHAIGAKU-DUMMY-COM-SEC
           ELSE
             PERFORM    200111-SRYACTINF-HENSYU-SEC
           END-IF
      *
           .
       2001-RIHAIGAKU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ê¥Ï¥Ó¥ê°å³Ø´ÉÍýÎÁ¤Î¥³¥á¥ó¥ÈÊÔ½¸¡Ê¥ê¥Ï¼Â»ÜÆü¡Ë
      *****************************************************************
       20011-RIHAIGAKU-DUMMY-COM-SEC          SECTION.
      *
           INITIALIZE             TBL-SANTEI-DAY
      *
           MOVE     ZERO      TO   DAY-IDX
           PERFORM  VARYING  IDX  FROM   1   BY   1
                     UNTIL   IDX    >   31
             IF  WRK-RIHAIGAKU-SANTEI-DAY(IDX)  NOT = ZERO
                 ADD    1     TO   DAY-IDX
                 MOVE   IDX   TO   TB-SANTEI-DAY(DAY-IDX)
             END-IF
           END-PERFORM
      *
           MOVE     ZERO      TO   IDZ
           INITIALIZE         WRK-FREE-AREA2
      *
           MOVE     2         TO   FLG-COMMENT
           PERFORM  20011-FREE-COMMENT2-SEC
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                             TO  WRK-RECE05XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE05XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE05XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE05XX-NAME(MAX-MEISAISU 1)
           END-PERFORM
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-RIHAIGAKU-DUMMY-COM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¡Ê¥ê¥ÏÁª¡Ë¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-RIHASEN-HANTEI-SEC          SECTION.
      *
           IF     RECE04-SAFLG   =    "2"
               MOVE       99      TO     FLG-COMMENT
               PERFORM    20011-KENSENTOU-COMMENT-SEC
           END-IF
      *
           .
       2001-RIHASEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ç§ÃÎÎÅË¡¡¦Ç§ÃÎ¹ÔÆ°ÎÅË¡¤¬¤¢¤ë¤«È½Äê
      *    Àº¿À²Ê½Å¾É´µ¼ÔÁá´ü½¸Ãæ»Ù±ç´ÉÍýÎÁ¤¬¤¢¤ë¤«È½Äê
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ¤¬¤¢¤ë¤«È½Äê
      *    ºßÂðÈ¾¸Ç·Á±ÉÍÜ·Ð´É±ÉÍÜË¡»ØÆ³´ÉÍýÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-NINRYO-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-NINRYO-COMMENT   NOT =  SPACE
             IF  (RECE04-FLG-NINRYO-COMMENT   =
                 "A" OR "B" OR "G" OR "H" OR "C" OR "D" OR
                 "E" OR "F" OR "I" OR "J" OR "P" OR "Q"       )
             AND (RECE04-FLG-ONLINE-COMMENT   =  "4"          )
               CONTINUE
             ELSE
               PERFORM   2001-NINRYO-HENSYU-SEC
               IF   WRK-NINRYO-FSANTEIYMD   NOT =  SPACE
      *
                  INITIALIZE                WRK-JIDO-SETCOM-AREA
                  MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                  MOVE       19      TO     WRK-JIDO-SETCOMATFLG
                  MOVE       99      TO     FLG-COMMENT
      *
                  MOVE      ZERO     TO     IDZ
                  INITIALIZE                WRK-FREE-AREA2
      *
                  ADD        1       TO     IDZ
      *
                  MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                  MOVE   "½é²ó»»Äê¡¡¡¡¡¡·î¡¡¡¡Æü"
                                     TO     WK-FREE-NAME2(IDZ)
                  MOVE   WRK-NINRYO-FSANTEIYMD(5:2)   TO  DAY-IDX2
                  MOVE   TBL-DAY(DAY-IDX2)
                                     TO     WK-FREE-NAME2(IDZ)(11:4)
                  MOVE   WRK-NINRYO-FSANTEIYMD(7:2)   TO  DAY-IDX2
                  MOVE   TBL-DAY(DAY-IDX2)
                                     TO     WK-FREE-NAME2(IDZ)(17:4)
      *
                  IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                    MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                    MOVE  WRK-JIDO-SETCOMATFLG
                                     TO     WK-FREE-SETCOMATFLG2(IDZ)
                  END-IF
      *
                  PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
               END-IF
             END-IF
           END-IF
      *
           .
       2001-NINRYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    
      *****************************************************************
       2001-NINRYO-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE                TO  WRK-NINRYO-FSANTEIYMD
           INITIALIZE                       SANTEI-REC
           MOVE    RECE02-HOSPNUM       TO  SANTEI-HOSPNUM
           MOVE    RECE02-PTID          TO  SANTEI-PTID
           MOVE    RECE02-SRYYM         TO  SANTEI-SRYYM
           EVALUATE  RECE04-FLG-NINRYO-COMMENT
             WHEN      "1"
               MOVE  CONS-NINRYO        TO  SANTEI-SRYCD
             WHEN      "2"
               MOVE  CONS-NINRYO2       TO  SANTEI-SRYCD
             WHEN      "3"
               MOVE  CONS-NINRYO3       TO  SANTEI-SRYCD
             WHEN      "4"
               MOVE  CONS-NINRYO4       TO  SANTEI-SRYCD
             WHEN      "5"
               MOVE  CONS-NINRYO5       TO  SANTEI-SRYCD
             WHEN      "7"
               MOVE  CONS-NINRYO7       TO  SANTEI-SRYCD
             WHEN      "8"
               MOVE  CONS-NINRYO8       TO  SANTEI-SRYCD
      *
             WHEN      "9"
               MOVE  CONS-ZAIHANKO      TO  SANTEI-SRYCD
      *
             WHEN      "A"
               MOVE  CONS-SEIZAI        TO  SANTEI-SRYCD
             WHEN      "B"
               MOVE  CONS-SEIZAI2       TO  SANTEI-SRYCD
             WHEN      "C"
               MOVE  CONS-SEIZAI3       TO  SANTEI-SRYCD
             WHEN      "D"
               MOVE  CONS-SEIZAI4       TO  SANTEI-SRYCD
             WHEN      "E"
               MOVE  CONS-SEIZAI5       TO  SANTEI-SRYCD
             WHEN      "F"
               MOVE  CONS-SEIZAI6       TO  SANTEI-SRYCD
             WHEN      "G"
               MOVE  CONS-SEIZAI7       TO  SANTEI-SRYCD
             WHEN      "H"
               MOVE  CONS-SEIZAI8       TO  SANTEI-SRYCD
             WHEN      "I"
               MOVE  CONS-SEIZAI9       TO  SANTEI-SRYCD
             WHEN      "J"
               MOVE  CONS-SEIZAI10      TO  SANTEI-SRYCD
             WHEN      "P"
               MOVE  CONS-SEIZAI11      TO  SANTEI-SRYCD
             WHEN      "Q"
               MOVE  CONS-SEIZAI12      TO  SANTEI-SRYCD
      *
             WHEN      "K"
               MOVE  CONS-SEIJYU        TO  SANTEI-SRYCD
             WHEN      "L"
               MOVE  CONS-SEIJYU2       TO  SANTEI-SRYCD
             WHEN      "M"
               MOVE  CONS-SEIJYU3       TO  SANTEI-SRYCD
             WHEN      "N"
               MOVE  CONS-SEIJYU4       TO  SANTEI-SRYCD
           END-EVALUATE
           MOVE    ZERO                 TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                 TO  SANTEI-SRYKA
           MOVE    ZERO                 TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC           TO  MCPDATA-REC
           MOVE    "tbl_santei"         TO  WRK-MCP-TABLE
           MOVE    "key"                TO  WRK-MCP-PATHNAME
           PERFORM  900-SANTEI-INV-SEC
           IF       FLG-SANTEI        =   ZERO
              MOVE  SANTEI-FSANTEIYMD   TO  WRK-NINRYO-FSANTEIYMD
           END-IF
      *
           .
       2001-NINRYO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÄÌ±¡¡¦ºßÂðÀº¿ÀÎÅË¡¡Ê£²£°ºÐÌ¤Ëþ¡Ë²Ã»»¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-TUINZAI-HANTEI-SEC          SECTION.
      *
           IF  ((RECE02-SRYYM                >=  "201604") AND
                (RECE04-FLG-TUINZAI-COMMENT   =  "1"     ) AND
                (RECE04-FLG-TUINZAI2-COMMENT  =  "1"     ))
           OR  ((RECE02-SRYYM                <=  "201603") AND
                (RECE04-FLG-TUINZAI-COMMENT   =  "1"     ))
             MOVE    ZERO                TO  FLG-TUINZAI-COMMENT
             MOVE    SPACE               TO  WRK-TUINZAI-YMD
             INITIALIZE                      SANTEI-REC
             MOVE    RECE02-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE02-PTID         TO  SANTEI-PTID
             MOVE    CONS-TUINZAI        TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI          =   1 )
                             OR   ( FLG-TUINZAI-COMMENT =   1 )
      *
                   IF   SANTEI-SRYYM  <=  RECE02-SRYYM
                     MOVE  SANTEI-SRYYM     TO  WRK-TUINZAI-YMD(1:6)
                     MOVE  SANTEI-MSANTEID  TO  WRK-TUINZAI-YMD(7:2)
                     MOVE    1              TO  FLG-TUINZAI-COMMENT
      *
                     IF   SANTEI-SRYYM  =  RECE02-SRYYM
                       MOVE   ZERO  TO  FLG-CHECK-MAE
                       MOVE   ZERO  TO  FLG-CHECK-ATO
                       MOVE  WRK-TUINZAI-YMD(7:2)  TO  IDX28
                       PERFORM  VARYING  IDX26  FROM   1  BY  1
                                 UNTIL   IDX26   >    31
                         IF  RECE04-KAISU(1 IDX26)  NOT =  ZERO
                           IF    IDX26  <  IDX28
                             MOVE    1   TO  FLG-CHECK-MAE
                           ELSE
                             MOVE    1   TO  FLG-CHECK-ATO
                           END-IF
                         END-IF
                       END-PERFORM
                       IF  ((FLG-CHECK-MAE = 1   ) AND
                            (FLG-CHECK-ATO = ZERO))
                       OR  ((FLG-CHECK-MAE = 1   ) AND
                            (FLG-CHECK-ATO = 1   ))
                         MOVE    SPACE   TO  WRK-TUINZAI-YMD
                       END-IF
                     END-IF
                   END-IF
      *
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
             END-IF
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             IF   WRK-TUINZAI-YMD   NOT =  SPACE
      *
                INITIALIZE                WRK-JIDO-SETCOM-AREA
      ***       MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
      ***       MOVE       30      TO     WRK-JIDO-SETCOMATFLG
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                ADD        1       TO     IDZ
      *
                IF   RECE02-SRYYM >= "201905"
                 MOVE     24      TO     WK-FREE-YUKOKETA2(IDZ)
                 MOVE "Àº¿À²Ê¤ò½é¤á¤Æ¼õ¿Ç¤·¤¿Æü¡¡¡¡¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                                  TO     WK-FREE-NAME2(IDZ)
                 MOVE     31      TO     IDX41
                 MOVE     37      TO     IDX42
                 MOVE     43      TO     IDX48
                 MOVE     27      TO     IDX49
                ELSE
                 MOVE     22      TO     WK-FREE-YUKOKETA2(IDZ)
                 MOVE "Àº¿À²Ê¤ò½é¤á¤Æ¼õ¿Ç¤·¤¿Æü¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                                  TO     WK-FREE-NAME2(IDZ)
                 MOVE     27      TO     IDX41
                 MOVE     33      TO     IDX42
                 MOVE     39      TO     IDX48
                END-IF
      *
                INITIALIZE                         STS-AREA-DAY
                INITIALIZE                         LNK-DAY2-AREA
                MOVE    "21"                   TO  LNK-DAY2-IRAI
                MOVE    WRK-TUINZAI-YMD        TO  LNK-DAY2-YMD
                CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                               LNK-DAY2-AREA
                IF      STS-DAY-RC1            =   ZERO
                   MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                   END-IF
      *
                   MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                   END-IF
      *
                   MOVE    LNK-DAY2-EDTYMD1-D   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX48:4)
                   END-IF
      *
                   IF   RECE02-SRYYM >= "201905"
                      MOVE    LNK-DAY2-EDTYMD2-G
                                   TO  WK-FREE-NAME2(IDZ)(IDX49:4)
                   END-IF
                END-IF
      *
                IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                  MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                  MOVE  WRK-JIDO-SETCOMATFLG
                                   TO     WK-FREE-SETCOMATFLG2(IDZ)
                END-IF
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-TUINZAI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    °ÍÂ¸¾É½¸ÃÄÎÅË¡¤¬¤¢¤ë¤«È½Äê
      *    ¼À´µÊÌÅùÀìÌç¥×¥í¥°¥é¥à²Ã»»¤¬¤¢¤ë¤«È½Äê
      *    ·ÐÆ¬³¸¼§µ¤»É·ãÎÅË¡¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-IZONSYO-HANTEI-SEC          SECTION.
      *
           IF     RECE04-FLG-IZONSYO-COMMENT   NOT =  SPACE
             EVALUATE  RECE04-FLG-IZONSYO-COMMENT
               WHEN      "1"
                 MOVE  CONS-IZONSYO  TO  WRK-CHECK-SRYCD
               WHEN      "2"
                 MOVE  CONS-SCPRO    TO  WRK-CHECK-SRYCD
               WHEN      "3"
                 MOVE  CONS-IZONSYO2 TO  WRK-CHECK-SRYCD
               WHEN      "4"
                 MOVE  CONS-JIKSGK   TO  WRK-CHECK-SRYCD
             END-EVALUATE
             PERFORM       20011-ZAIKYO-CHECK-SEC
      *
             IF   RECE04-FLG-IZONSYO-COMMENT  =  "4"
             AND  WRK-ZAIKYO-YMD          NOT =  SPACE
               MOVE    CONS-JIKSGK-E TO  WRK-CHECK-SRYCD
               PERFORM     20011-ZAIKYO-CHECK2-SEC
             END-IF
      *
             IF   WRK-ZAIKYO-YMD   NOT =  SPACE
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                EVALUATE  RECE04-FLG-IZONSYO-COMMENT
                  WHEN      "1"
                  WHEN      "3"
                    MOVE   33      TO     WRK-JIDO-SETCOMATFLG
                  WHEN      "2"
                    MOVE   40      TO     WRK-JIDO-SETCOMATFLG
                  WHEN      "4"
                    MOVE   47      TO     WRK-JIDO-SETCOMATFLG
                END-EVALUATE
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                ADD        1       TO     IDZ
      *
                MOVE       12      TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "¼£ÎÅ³«»ÏÆü¡¡¡¡¡¡·î¡¡¡¡Æü"
                                   TO     WK-FREE-NAME2(IDZ)
      *
                MOVE    WRK-ZAIKYO-YMD(5:2)   TO  DAY-IDX2
                MOVE    TBL-DAY(DAY-IDX2)
                                   TO     WK-FREE-NAME2(IDZ)(13:4)
                MOVE    WRK-ZAIKYO-YMD(7:2)   TO  DAY-IDX2
                MOVE    TBL-DAY(DAY-IDX2)
                                   TO     WK-FREE-NAME2(IDZ)(19:4)
      *
                IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                  MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                  MOVE  WRK-JIDO-SETCOMATFLG
                                   TO     WK-FREE-SETCOMATFLG2(IDZ)
                END-IF
      *
                IF   WRK-ZAIKYO-YMD2   NOT =  SPACE
      *
                  ADD        1       TO     IDZ
      *
                  MOVE       12      TO     WK-FREE-YUKOKETA2(IDZ)
                  MOVE   "¼£ÎÅ½ªÎ»Æü¡¡¡¡¡¡·î¡¡¡¡Æü"
                                     TO     WK-FREE-NAME2(IDZ)
      *
                  MOVE    WRK-ZAIKYO-YMD2(5:2)  TO  DAY-IDX2
                  MOVE    TBL-DAY(DAY-IDX2)
                                     TO     WK-FREE-NAME2(IDZ)(13:4)
                  MOVE    WRK-ZAIKYO-YMD2(7:2)  TO  DAY-IDX2
                  MOVE    TBL-DAY(DAY-IDX2)
                                     TO     WK-FREE-NAME2(IDZ)(19:4)
      *
                  IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                    MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                    MOVE  WRK-JIDO-SETCOMATFLG
                                     TO     WK-FREE-SETCOMATFLG2(IDZ)
                  END-IF
      *
                END-IF
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-IZONSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ±¡³°¼¾ÉÛÌô£·£°ËçÄ¶ÍýÍ³¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-SIPPUCOM-HANTEI-SEC          SECTION.
      *
           IF     RECE04-SIPPUCOMFLG  =  "1"
      *
             MOVE    99                   TO  FLG-COMMENT
      *
             ADD     1                    TO  IDX34
      *
             MOVE    ZERO                 TO  IDX-RECE05
             MOVE    SPACE                TO  WRK-RECE05XX-REC
             INITIALIZE                       WRK-RECE05XX-REC
      *
             PERFORM  200111-RECE05-TAIHI-SEC
      *
             PERFORM  UNTIL  (FLG-RECE05         =   1            )
                       OR    (RECE04-KEY1    NOT =   RECE05-KEY1  )
                       OR    (RECE04-ZAINUM  NOT =   RECE05-ZAINUM)
               PERFORM  VARYING  IDX  FROM  1  BY  1
                         UNTIL  (IDX               >   5    )
                          OR    (RECE05-SRYCD(IDX) =   SPACE)
                 IF  ((RECE05-SRYCD(IDX)(1:1)  =  "8"  ) OR
                      (RECE05-SRYCD(IDX)(1:3)  =  "008"))
                 AND ( RECE05-NAME(IDX)        =  SPACE )
                   CONTINUE
                 ELSE
                   IF  (RECE05-SRYCD(IDX)(1:1)  =  "8"  )
                   OR  (RECE05-SRYCD(IDX)(1:3)  =  "008")
                     MOVE  "810000001"  TO   RECE05-SRYCD(IDX)
                   END-IF
                   ADD    1   TO   IDX-RECE05
                   MOVE   RECE05-SRYACT-TBL(IDX)
                              TO   WRK-RECE05XX-SRYACT-TBL(IDX-RECE05 1)
                 END-IF
               END-PERFORM
               PERFORM  990-RECE05-NEXT-SEC
             END-PERFORM
      *
             MOVE      ZERO                 TO   FLG-WRKRECE05
             MOVE      ZERO                 TO   IDX
             MOVE      ZERO                 TO   IDX-RECE05
             MOVE      ZERO                 TO   WRK-RENNUM
             MOVE      ZERO                 TO   WRK-CNT-MEISAI
             PERFORM  UNTIL  FLG-WRKRECE05  =  1
               INITIALIZE                        RECE059-REC
               MOVE    WRK-RECE05-KEY       TO   RECE059-KEY
               COMPUTE WRK-SIPPU-ZAICNT     =    99991900  +  IDX34
               MOVE    WRK-SIPPU-ZAICNT     TO   RECE059-ZAINUM
               ADD     1                    TO   WRK-RENNUM
               MOVE    WRK-RENNUM           TO   RECE059-RENNUM
               MOVE    WRK-RECE05-KOHBUNRI  TO   RECE059-KOHBUNRI
               MOVE    WRK-RECE05-PTNUM     TO   RECE059-PTNUM
               MOVE    WRK-RECE05-SRYKA     TO   RECE059-SRYKA
               MOVE    WRK-RECE05-SRYKBN    TO   RECE059-SRYKBN
               MOVE    WRK-RECE05-SRYSYUKBN TO   RECE059-SRYSYUKBN
               MOVE    WRK-RECE05-JIHIMONEY TO   RECE059-JIHIMONEY
               PERFORM  VARYING  IDX  FROM  1  BY  1
                         UNTIL  (IDX            >   5)
                          OR    (FLG-WRKRECE05  =   1)
                 ADD        1     TO   IDX-RECE05
                 IF (WRK-RECE05XX-SRYCD(IDX-RECE05 1) = SPACE)
                 OR (IDX-RECE05                       >  100 )
                    MOVE    1     TO   FLG-WRKRECE05
                 ELSE
                    MOVE  WRK-RECE05XX-SRYACT-TBL(IDX-RECE05 1)
                                  TO   RECE059-SRYACT-TBL(IDX)
                    ADD     1     TO   WRK-CNT-MEISAI
                 END-IF
               END-PERFORM
               IF   RECE059-SRYCD(1)  NOT =  SPACE
                    WRITE   RECE059-REC
                    ADD     1     TO    CNT-RECE059
               END-IF
             END-PERFORM
      *
             MOVE     RECE04-REC        TO   RECE049-REC
             MOVE     WRK-CNT-MEISAI    TO   RECE049-MEISAISU
             COMPUTE  WRK-SIPPU-ZAICNT  =    99991900  +  IDX34
             MOVE     WRK-SIPPU-ZAICNT  TO   RECE049-ZAINUM
             WRITE    RECE049-REC
             ADD      1                 TO   CNT-RECE049
      *
           END-IF
      *
           .
       2001-SIPPUCOM-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆâÉþÌôºÞÎÁÄþ¸º¤Ë´Ø¤¹¤ëÊÔ½¸
      *****************************************************************
       2001-YAKUTEI-HENSYU-SEC          SECTION.
      *
           ADD       1        TO         IDX6
      *
           IF    IDX6    <=    100
             MOVE    RECE04-KOHBUNRI     TO   TBL-YT1-KOHBUNRI(IDX6)
             MOVE    RECE04-SRYKBN       TO   TBL-YT1-SRYKBN(IDX6)
             MOVE    RECE04-JIHOKBN      TO   TBL-YT1-JIHOKBN(IDX6)
      ***    MOVE    RECE04-HKNCOMBI     TO   TBL-YT1-HKNCOMBI(IDX6)
      ***    MOVE    RECE04-SRYKA        TO   TBL-YT1-SRYKA(IDX6)
      *
             MOVE    RECE04-REC          TO   TBL-YT1-RECE04-REC(IDX6)
      *
             MOVE    RECE04-ZAITEN       TO   TBL-YT1-ZAITEN(IDX6)
             MOVE    RECE04-SRYCDTOTAL   TO   TBL-YT1-SRYCDTOTAL(IDX6)
             MOVE    RECE04-SURYOUTOTAL  TO   TBL-YT1-SURYOUTOTAL(IDX6)
             MOVE    RECE04-RUIKEITOTAL  TO   TBL-YT1-RUIKEITOTAL(IDX6)
             MOVE    ZERO                TO   TBL-YT1-RUIKEITOTAL2(IDX6)
             MOVE    ZERO                TO   TBL-YT1-RUIKEITOTAL3(IDX6)
             MOVE    ZERO                TO   TBL-YT1-RUIKEITOTAL4(IDX6)
             MOVE    RECE04-MEISAISU     TO   TBL-YT1-MEISAISU(IDX6)
      *
             PERFORM   VARYING   IDX7    FROM   2   BY   1
                        UNTIL   (IDX7      >   10)
      *
               COMPUTE   IDX9    =   IDX7  -   1
               PERFORM   VARYING   IDX8    FROM   1   BY   1
                          UNTIL   (IDX8      >    31)
      *
                  MOVE    RECE04-KAISU(IDX7 IDX8)
                            TO    TBL-YT1-KAISU(IDX6 IDX8 IDX9)
      *
               END-PERFORM
      *
             END-PERFORM
      *
             IF      RECE04-SRYCDTOTAL  =   630010002  OR  630010005
                                        OR  630010006  OR  630010007
                  MOVE      2      TO     TBL-YT1-FLG(IDX6)
             ELSE
                  MOVE      1      TO     TBL-YT1-FLG(IDX6)
             END-IF
           ELSE
      *
             DISPLAY "TABLE OVER1"
      *
           END-IF
      *
           .
       2001-YAKUTEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ±¡³°¼¾ÉÛÌô¤Î¥Á¥§¥Ã¥¯
      *****************************************************************
       2001-INGAISIPPU-CHECK-SEC          SECTION.
      *
           IF    RECE02-SIPPUCHKFLG  =  1
             PERFORM  VARYING  IDX36  FROM  2  BY  1
                       UNTIL   IDX36   >   10
               PERFORM  VARYING  IDX37  FROM  1  BY  1
                         UNTIL   IDX37   >   31
                 IF   RECE04-KAISU(IDX36 IDX37)     NOT =  ZERO
                 AND  RECE02-SIPPUCHK(IDX36 IDX37)  NOT =  ZERO
                    MOVE     1     TO    FLG-SIPPU-OK
                 END-IF
               END-PERFORM
             END-PERFORM
           END-IF
      *
           .
       2001-INGAISIPPU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ±¡³°¼¾ÉÛÌô¤Î¥³¥á¥ó¥ÈÊÑ´¹
      *****************************************************************
       2001-INGAISIPPU-COMMENT-SEC          SECTION.
      *
           ADD     1                    TO  IDX33
      *
           MOVE    ZERO                 TO  IDX-RECE05
           MOVE    SPACE                TO  WRK-RECE05XX-REC
           INITIALIZE                       WRK-RECE05XX-REC
      *
           PERFORM  200111-RECE05-TAIHI-SEC
      *
           PERFORM  UNTIL  (FLG-RECE05         =   1            )
                     OR    (RECE04-KEY1    NOT =   RECE05-KEY1  )
                     OR    (RECE04-ZAINUM  NOT =   RECE05-ZAINUM)
             PERFORM  VARYING  IDX  FROM  1  BY  1
                       UNTIL  (IDX               >   5    )
                        OR    (RECE05-SRYCD(IDX) =   SPACE)
               IF  (RECE05-SIPPUFLG(IDX)    =  "1"  )
      ***      OR  (RECE05-SRYCD(IDX)(1:1)  =  "8"  )
      ***      OR  (RECE05-SRYCD(IDX)(1:3)  =  "008")
                 ADD    1   TO   IDX-RECE05
                 IF   RECE05-SIPPUFLG(IDX)  =  "1"
                    IF  RECE05-INGAIFLG-SIPPU(IDX)  =  "1"
                       MOVE  RECE05-GENERICNAME(IDX)
                                    TO   RECE05-NAME(IDX)
                       MOVE  RECE05-GENERICYUKOKETA(IDX)
                                    TO   RECE05-YUKOKETA(IDX)
                    END-IF
                    PERFORM  20011-INGAISIPPU-SURYO-SEC
                 END-IF
                 MOVE  "810000001"  TO   RECE05-SRYCD(IDX)
                 MOVE   ZERO        TO   RECE05-SRYSURYO(IDX)
                                         RECE05-TANICD(IDX)
                                         RECE05-TANIMOJISU(IDX)
                 MOVE   SPACE       TO   RECE05-TANINAME(IDX)
                 MOVE   SPACE       TO   RECE05-INGAIFLG(IDX)
                                         RECE05-INGAIFLG-RECE(IDX)
                 MOVE   RECE05-SRYACT-TBL(IDX)
                            TO   WRK-RECE05XX-SRYACT-TBL(IDX-RECE05 1)
               END-IF
             END-PERFORM
             PERFORM  990-RECE05-NEXT-SEC
           END-PERFORM
      *
           MOVE      ZERO                 TO   FLG-WRKRECE05
           MOVE      ZERO                 TO   IDX
           MOVE      ZERO                 TO   IDX-RECE05
           MOVE      ZERO                 TO   WRK-RENNUM
           MOVE      ZERO                 TO   WRK-CNT-MEISAI
           PERFORM  UNTIL  FLG-WRKRECE05  =  1
             INITIALIZE                        RECE059-REC
             MOVE    WRK-RECE05-KEY       TO   RECE059-KEY
             COMPUTE WRK-SIPPU-ZAICNT     =    99991000  +  IDX33
             MOVE    WRK-SIPPU-ZAICNT     TO   RECE059-ZAINUM
             ADD     1                    TO   WRK-RENNUM
             MOVE    WRK-RENNUM           TO   RECE059-RENNUM
             MOVE    WRK-RECE05-KOHBUNRI  TO   RECE059-KOHBUNRI
             MOVE    WRK-RECE05-PTNUM     TO   RECE059-PTNUM
             MOVE    WRK-RECE05-SRYKA     TO   RECE059-SRYKA
             MOVE    "80"                 TO   RECE059-SRYKBN
             MOVE    "800"                TO   RECE059-SRYSYUKBN
             MOVE    WRK-RECE05-JIHIMONEY TO   RECE059-JIHIMONEY
             PERFORM  VARYING  IDX  FROM  1  BY  1
                       UNTIL  (IDX            >   5)
                        OR    (FLG-WRKRECE05  =   1)
               ADD        1     TO   IDX-RECE05
               IF (WRK-RECE05XX-SRYCD(IDX-RECE05 1) = SPACE)
               OR (IDX-RECE05                       >  100 )
                  MOVE    1     TO   FLG-WRKRECE05
               ELSE
                  MOVE  WRK-RECE05XX-SRYACT-TBL(IDX-RECE05 1)
                                TO   RECE059-SRYACT-TBL(IDX)
                  ADD     1     TO   WRK-CNT-MEISAI
               END-IF
             END-PERFORM
             IF   RECE059-SRYCD(1)  NOT =  SPACE
                  WRITE   RECE059-REC
                  ADD     1     TO    CNT-RECE059
             END-IF
           END-PERFORM
      *
           MOVE     RECE04-REC        TO   RECE049-REC
           MOVE     WRK-CNT-MEISAI    TO   RECE049-MEISAISU
           COMPUTE  WRK-SIPPU-ZAICNT  =    99991000  +  IDX33
           MOVE     WRK-SIPPU-ZAICNT  TO   RECE049-ZAINUM
           MOVE     "80"              TO   RECE049-SRYKBN
           MOVE     "800"             TO   RECE049-SRYSYUKBN
           MOVE     SPACE             TO   RECE049-TOYAKUFLG
           MOVE     SPACE             TO   RECE049-SETCOMFLG
           WRITE    RECE049-REC
           ADD      1                 TO   CNT-RECE049
      *
           .
       2001-INGAISIPPU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ±¡³°¼¾ÉÛÌô¤Î¿ôÎÌÊÔ½¸
      *****************************************************************
       20011-INGAISIPPU-SURYO-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-SURYO-X
           MOVE    ZERO            TO  WRK-SURYOST
           MOVE    ZERO            TO  WRK-SURYOKETA
           MOVE    ZERO            TO  WRK-SURYOKETA1
           MOVE    ZERO            TO  WRK-SURYOKETA2
           MOVE    SPACE           TO  WRK-SURYO-N
           MOVE    SPACE           TO  WRK-SRYSURYOW
      * 
      *    ¿ô»úÉ½¼¨ÍÑÊÔ½¸¡ÊÈ¾³Ñ¡Ë
           MOVE    RECE05-SRYSURYO(IDX)
                                   TO  WRK-SURYO
           MOVE    WRK-SURYO-S1    TO  WRK-SURYO-Z1
      *
           MOVE    ZERO            TO  FLG-SP
           PERFORM  VARYING  IDX35  FROM  5  BY  -1
                     UNTIL   IDX35  =   ZERO
             IF  WRK-SURYO-S2(IDX35:1)  NOT =  ZERO
                 MOVE    1         TO  FLG-SP
             END-IF
             IF  FLG-SP  =  1
                 MOVE    WRK-SURYO-S2(IDX35:1)
                                   TO  WRK-SURYO-Z3(IDX35:1)
             END-IF
           END-PERFORM
           IF    WRK-SURYO-Z3  NOT =  SPACE
                 MOVE    "."       TO  WRK-SURYO-Z2
           END-IF
      *    É½¼¨Ê¸»ú¿ô¥«¥¦¥ó¥È
           PERFORM  VARYING  IDX35  FROM  1  BY  1
                     UNTIL   IDX35   >   11
             IF  WRK-SURYO-X(IDX35:1)  NOT =  SPACE
                 IF  WRK-SURYOST  =  ZERO
                     MOVE    IDX35    TO   WRK-SURYOST
                 END-IF
      *          ¿ôÎÌ·å¿ô¥«¥¦¥ó¥È(£¶·åÌÜ¡§¥Ô¥ê¥ª¥É)
                 ADD     1            TO   WRK-SURYOKETA
                 IF  IDX35  <  6
                     ADD     1        TO   WRK-SURYOKETA1
                 END-IF
                 IF  IDX35  >  6
                     ADD     1        TO   WRK-SURYOKETA2
                 END-IF
             END-IF
           END-PERFORM
           IF  WRK-SURYOST  =  6
               MOVE    "0"             TO  WRK-SURYO-Z1(5:1)
               MOVE    5               TO  WRK-SURYOST
               MOVE    1               TO  WRK-SURYOKETA1
               ADD     1               TO  WRK-SURYOKETA
           END-IF
      *    Á´³ÑÊÑ´¹
           IF  WRK-SURYOKETA1          >   ZERO
               INITIALIZE              WRK-CHAR2-HENKAN-AREA
               MOVE    WRK-SURYO-Z1 (WRK-SURYOST:WRK-SURYOKETA1)
                                       TO  WRK-CHAR2-X
               MOVE    WRK-SURYOKETA1  TO  WRK-CHAR2-CNT
               PERFORM 200111-INGAISIPPU-HENKAN-SEC
               MOVE    WRK-CHAR2-N     TO  WRK-SURYO-N1
           END-IF
           IF  WRK-SURYOKETA2          >   ZERO
               MOVE    "¡¥"            TO  WRK-SURYO-N2
               INITIALIZE              WRK-CHAR2-HENKAN-AREA
               MOVE    WRK-SURYO-Z3 (1:WRK-SURYOKETA2)
                                       TO  WRK-CHAR2-X
               MOVE    WRK-SURYOKETA2  TO  WRK-CHAR2-CNT
               PERFORM 200111-INGAISIPPU-HENKAN-SEC
               MOVE    WRK-CHAR2-N     TO  WRK-SURYO-N3
           END-IF
           STRING      WRK-SURYO-N1    DELIMITED   BY  " "
                       WRK-SURYO-N2    DELIMITED   BY  " "
                       WRK-SURYO-N3    DELIMITED   BY  " "
                       INTO            WRK-SRYSURYOW
           END-STRING
      *
           MOVE        RECE05-NAME(IDX)  TO  WRK-INGAISIPPU-NAME
           MOVE        SPACE             TO  RECE05-NAME(IDX)
           STRING  WRK-INGAISIPPU-NAME   DELIMITED  BY  " "
                   "¡¡"                  DELIMITED  BY  SIZE
                   WRK-SRYSURYOW         DELIMITED  BY  " "
                   RECE05-TANINAME(IDX)  DELIMITED  BY  " "
                   INTO                  RECE05-NAME(IDX)
           END-STRING
      *
           PERFORM  VARYING  IDX35  FROM  300  BY  -1
                     UNTIL   IDX35   <    1
              IF  RECE05-NAME(IDX)(IDX35:1)  NOT =  SPACE
                  COMPUTE  RECE05-YUKOKETA(IDX)  =
                           IDX35   /    2
                  MOVE     1     TO      IDX35
              END-IF
           END-PERFORM
      *
           .
       20011-INGAISIPPU-SURYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ôÃÍÈ¾³Ñ¢ªÁ´³ÑÊÑ´¹½èÍý
      *****************************************************************
       200111-INGAISIPPU-HENKAN-SEC    SECTION.
      *
           MOVE    1           TO  WRK-CHAR2-IDX
           MOVE    ZERO        TO  WRK-CHAR2-N-CNT
      *
           PERFORM  VARYING  IDX35  FROM  1  BY  1
                     UNTIL   IDX35   >   WRK-CHAR2-CNT
             IF     WRK-CHAR2-X(IDX35:1)  IS  NUMERIC
               IF      WRK-CHAR2-X(IDX35:1)  =  "0"
                 MOVE    "£°"  TO  WRK-CHAR2-N(WRK-CHAR2-IDX:)
               ELSE
                 MOVE  WRK-CHAR2-X(IDX35:1)
                               TO  IDX19
                 MOVE  TBL-DAY(IDX19)(3:2)
                               TO  WRK-CHAR2-N(WRK-CHAR2-IDX:)
               END-IF
             ELSE
                 MOVE  "  "    TO  WRK-CHAR2-N(WRK-CHAR2-IDX:)
             END-IF
             ADD     2         TO  WRK-CHAR2-IDX
                                   WRK-CHAR2-N-CNT
           END-PERFORM
      *
           .
       200111-INGAISIPPU-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£´Å¾Á÷½èÍý
      *****************************************************************
       2099-RECE04-TENSOU-SEC     SECTION.
      *
           MOVE       RECE04-REC    TO   RECE049-REC
           WRITE      RECE049-REC
           ADD        1             TO   CNT-RECE049
           PERFORM    20991-RECE05-TENSOU-SEC
      *
           .
       2099-RECE04-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£µÅ¾Á÷½èÍý
      *****************************************************************
       20991-RECE05-TENSOU-SEC     SECTION.
      *
           PERFORM 800-RECE04-SET05-SEC
           MOVE    RECE04-ZAINUM       TO  RECE05-ZAINUM
           START   RECE05-FILE
               KEY IS    >=   RECE05-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE05
               NOT INVALID
                   PERFORM 990-RECE05-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE05           =   1                   )
               OR     ( RECE04-KEY1      NOT =   RECE05-KEY1         )
               OR     ( RECE04-ZAINUM    NOT =   RECE05-ZAINUM       )
      *
      *
               MOVE          RECE05-REC       TO      RECE059-REC
               WRITE         RECE059-REC
               ADD     1                 TO  CNT-RECE059
      *
               PERFORM 990-RECE05-NEXT-SEC
      *
           END-PERFORM
      *
           .
       20991-RECE05-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    £°£µ¡¡ÂàÈò½èÍý
      *****************************************************************
       200111-RECE05-TAIHI-SEC     SECTION.
      *
           PERFORM 800-RECE04-SET05-SEC
           MOVE    RECE04-ZAINUM       TO  RECE05-ZAINUM
           START   RECE05-FILE
                 KEY IS    >=   RECE05-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE05
                 NOT INVALID
                     PERFORM 990-RECE05-NEXT-SEC
                     MOVE    RECE05-REC  TO  WRK-RECE05-REC
           END-START
      *
           .
       200111-RECE05-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    £°£µ¡¡£Ó£Ò£Ù£Á£Ã£Ô£É£Î£ÆÊÔ½¸½èÍý
      *****************************************************************
       200111-SRYACTINF-HENSYU-SEC     SECTION.
      *
           PERFORM
               UNTIL  ( FLG-RECE05           =   1                   )
               OR     ( RECE04-KEY1      NOT =   RECE05-KEY1         )
               OR     ( RECE04-ZAINUM    NOT =   RECE05-ZAINUM       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   5     )
                         OR      ( RECE05-SRYCD(IDX) =   SPACE )
                    ADD    1             TO      IDX-RECE05
                    MOVE   RECE05-SRYACT-TBL(IDX)
                             TO   WRK-RECE05XX-SRYACT-TBL(IDX-RECE05 1)
               END-PERFORM 
      *
               PERFORM 990-RECE05-NEXT-SEC
      *
           END-PERFORM
      *
           IF     WRK-JIDO-JYOUGEFLG  =  1
             PERFORM   VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX                >   50    )
                       OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                ADD    1                  TO  IDX-RECE05
                MOVE   "810000001"
                              TO  WRK-RECE05XX-SRYCD(IDX-RECE05 1)
                MOVE   ZERO   TO  WRK-RECE05XX-SRYSURYO(IDX-RECE05 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                              TO  WRK-RECE05XX-YUKOKETA(IDX-RECE05 1)
                MOVE   WK-FREE-NAME2(IDX)
                              TO  WRK-RECE05XX-NAME(IDX-RECE05 1)
                MOVE   WK-FREE-MEISKYFLG2(IDX)
                          TO  WRK-RECE05XX-MEISKYFLG(IDX-RECE05 1)
                MOVE   WK-FREE-SETCOMATFLG2(IDX)
                          TO  WRK-RECE05XX-SETCOMATFLG(IDX-RECE05 1)
                MOVE   "1"    TO  WRK-RECE05XX-RECEPTFLG(IDX-RECE05 1)
             END-PERFORM
           END-IF
      *
      *    ¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE05
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE05
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE05  =    1
                    INITIALIZE                        RECE059-REC
                    MOVE    WRK-RECE05-KEY       TO   RECE059-KEY
                    ADD     1                    TO   WRK-RENNUM
                    MOVE    WRK-RENNUM           TO   RECE059-RENNUM
                    MOVE    WRK-RECE05-KOHBUNRI  TO   RECE059-KOHBUNRI
                    MOVE    WRK-RECE05-PTNUM     TO   RECE059-PTNUM
                    MOVE    WRK-RECE05-SRYKA     TO   RECE059-SRYKA
                    MOVE    WRK-RECE05-SRYKBN    TO   RECE059-SRYKBN
                    MOVE    WRK-RECE05-SRYSYUKBN TO   RECE059-SRYSYUKBN
                    MOVE    WRK-RECE05-JIHIMONEY TO   RECE059-JIHIMONEY
                    PERFORM  VARYING   IDX   FROM   1   BY   1
                             UNTIL   (IDX       >    5)   OR
                                     (FLG-WRKRECE05   =   1)
                       ADD         1             TO    IDX-RECE05
                       IF (WRK-RECE05XX-SRYCD(IDX-RECE05 1) = SPACE)
                       OR (IDX-RECE05                       >  100 )
                           MOVE    1             TO    FLG-WRKRECE05
                       ELSE
                           MOVE  WRK-RECE05XX-SRYACT-TBL(IDX-RECE05 1)
                                            TO   RECE059-SRYACT-TBL(IDX)
                           ADD   1          TO   WRK-CNT-MEISAI
                       END-IF  
                    END-PERFORM
                    IF   RECE059-SRYCD(1)  NOT =  SPACE
                       WRITE   RECE059-REC
                       ADD     1            TO   CNT-RECE059
                    END-IF
           END-PERFORM
      *
           MOVE    RECE04-REC        TO      RECE049-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE049-MEISAISU
           IF   WRK-JIDO-SETCOMFLG   =  "1"
             MOVE  WRK-JIDO-SETCOMFLG
                                     TO      RECE049-SETCOMFLG
           END-IF
           WRITE   RECE049-REC
           ADD     1                 TO      CNT-RECE049
      *
           .
       200111-SRYACTINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    £°£µ¡¡£Ó£Ò£Ù£Á£Ã£Ô£É£Î£ÆÊÔ½¸½èÍý¡¡¤Þ¤È¤á
      *****************************************************************
       200111-SRYACTINF-HENSYU-MAT-SEC     SECTION.
      *
           PERFORM    200111-RECE05-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE05
           MOVE    SPACE                TO  WRK-RECE05XX-REC
           INITIALIZE                       WRK-RECE05XX-REC
      *
           MOVE    RECE04-MEISAISU      TO  MAX-MEISAISU
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                      UNTIL   (IDX                >   50   )
                        OR    (WK-FREE-NAME2(IDX) =   SPACE)
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                             TO  WRK-RECE05XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE05XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE05XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE05XX-NAME(MAX-MEISAISU 1)
                MOVE   WK-FREE-MEISKYFLG2(IDX)
                          TO  WRK-RECE05XX-MEISKYFLG(MAX-MEISAISU 1)
                MOVE   WK-FREE-SETCOMATFLG2(IDX)
                          TO  WRK-RECE05XX-SETCOMATFLG(MAX-MEISAISU 1)
           END-PERFORM
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       200111-SRYACTINF-HENSYU-MAT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆâÉþÌôºÞÎÁÄþ¸º¤Ë´Ø¤¹¤ëÊÔ½¸£²
      *****************************************************************
       240-YAKUTEI-HENSYU2-SEC          SECTION.
      *
      *    ÊÂ¤ÓÂØ¤¨
           PERFORM    VARYING   IDX6    FROM   1   BY   1
                       UNTIL   (IDX6                     >   100 )
                         OR    (TBL-YT1-RECE04-REC(IDX6) =  SPACE)
      *
              COMPUTE    IDX7  =  IDX6  +  1
      *
              PERFORM    VARYING   IDX8   FROM   IDX7   BY   1
                          UNTIL   (IDX8                     >   100 )
                            OR    (TBL-YT1-RECE04-REC(IDX8) =  SPACE)
      *
                IF       TBL-YT1-KEY(IDX6)  >   TBL-YT1-KEY(IDX8)
                   MOVE  TBL-YT1-OCC(IDX6)  TO  WRK-YAKUTEI-DATA
                   MOVE  TBL-YT1-OCC(IDX8)  TO  TBL-YT1-OCC(IDX6)
                   MOVE  WRK-YAKUTEI-DATA   TO  TBL-YT1-OCC(IDX8)
                END-IF
      *
              END-PERFORM
      *
           END-PERFORM
      *
      *    ½ÐÎÏ½èÍý
           INITIALIZE                   TBL-YAKUTEI-INF2
           INITIALIZE                   TBL-YAKUTEI-INF3
           MOVE       SPACE     TO      WRK-TAIHI-YT1-KEY
      *
           MOVE       ZERO      TO      IDX11
      *
           PERFORM    VARYING   IDX6    FROM   1   BY   1
                       UNTIL   (IDX6                     >   100 )
                         OR    (TBL-YT1-RECE04-REC(IDX6) =  SPACE)
      *
              IF     TBL-YT1-KEY(IDX6)   NOT =  WRK-TAIHI-YT1-KEY
      *
                 IF   WRK-TAIHI-YT1-KEY  NOT =  SPACE
                   PERFORM   2401-YAKUTEI-MATOME-SEC
                   PERFORM   2402-YAKUTEI-WRITE-SEC
                   INITIALIZE                      TBL-YAKUTEI-INF3
                 END-IF
      *
                 INITIALIZE                        TBL-YAKUTEI-INF2
                 MOVE   TBL-YT1-KEY(IDX6)      TO  WRK-TAIHI-YT1-KEY
      *
              END-IF
      *
              IF     TBL-YT1-FLG(IDX6)   =    1
      *
                   PERFORM   VARYING   IDX7    FROM   1   BY   1
                              UNTIL   (IDX7      >    31)
      *
                     PERFORM   VARYING   IDX8    FROM   1   BY   1
                                UNTIL   (IDX8      >    9)
      *
                        IF   TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                                                        NOT =  ZERO
                           PERFORM   VARYING   IDX9    FROM   1   BY   1
                            UNTIL (IDX9      >    25)
                              OR  (TBL-YT2X1-RECE04-REC(IDX7 IDX8 IDX9)
                                                              =  SPACE)
                                 CONTINUE
                           END-PERFORM
                           IF    IDX9   >   25
                             DISPLAY "TABLE OVER2"
                           ELSE
                             MOVE   TBL-YT1-RECE04-REC(IDX6)
                                TO  TBL-YT2X1-RECE04-REC(IDX7 IDX8 IDX9)
                             MOVE   TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                                TO  TBL-YT2X1-KAISU(IDX7 IDX8 IDX9)
                             MOVE   TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                                TO  TBL-YT2X1-DAY(IDX7 IDX8 IDX9 IDX7)
                             MOVE   TBL-YT1-HIKAKU(IDX6)
                                TO  TBL-YT2Y1-HIKAKU(IDX7 IDX8 IDX9)
      *
                             COMPUTE WRK-RUIKEITOTAL  =
                                     TBL-YT1-MEISAISU(IDX6)        *
                                     TBL-YT1-KAISU(IDX6 IDX7 IDX8) *
                                     TBL-YT1-SURYOUTOTAL(IDX6)
                             MOVE    WRK-RUIKEITOTAL
                                TO  TBL-YT2Y1-RUIKEITOTAL2
                                                       (IDX7 IDX8 IDX9)
                             COMPUTE WRK-RUIKEITOTAL  =
                                     TBL-YT1-SRYCDTOTAL(IDX6)      *
                                     TBL-YT1-SURYOUTOTAL(IDX6)
                             MOVE    WRK-RUIKEITOTAL
                                TO  TBL-YT2Y1-RUIKEITOTAL3
                                                       (IDX7 IDX8 IDX9)
                             COMPUTE WRK-RUIKEITOTAL2 =
                                     TBL-YT1-SRYCDTOTAL(IDX6)      *
                                     TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                             MOVE    WRK-RUIKEITOTAL2
                                TO  TBL-YT2Y1-RUIKEITOTAL4
                                                       (IDX7 IDX8 IDX9)
      *
                             ADD    TBL-YT1-ZAITEN(IDX6)
                                TO  TBL-YT2Y1-ZAITEN(IDX7 IDX8 26)
                             ADD    TBL-YT1-SRYCDTOTAL(IDX6)
                                TO  TBL-YT2Y1-SRYCDTOTAL(IDX7 IDX8 26)
                             ADD    TBL-YT1-SURYOUTOTAL(IDX6)
                                TO  TBL-YT2Y1-SURYOUTOTAL(IDX7 IDX8 26)
                             ADD    TBL-YT1-RUIKEITOTAL(IDX6)
                                TO  TBL-YT2Y1-RUIKEITOTAL(IDX7 IDX8 26)
                             COMPUTE WRK-RUIKEITOTAL  =
                                     TBL-YT1-MEISAISU(IDX6)        *
                                     TBL-YT1-KAISU(IDX6 IDX7 IDX8) *
                                     TBL-YT1-SURYOUTOTAL(IDX6)
                             ADD     WRK-RUIKEITOTAL
                                TO  TBL-YT2Y1-RUIKEITOTAL2(IDX7 IDX8 26)
                             COMPUTE WRK-RUIKEITOTAL  =
                                     TBL-YT1-SRYCDTOTAL(IDX6)      *
                                     TBL-YT1-SURYOUTOTAL(IDX6)
                             ADD     WRK-RUIKEITOTAL
                                TO  TBL-YT2Y1-RUIKEITOTAL3(IDX7 IDX8 26)
                             COMPUTE WRK-RUIKEITOTAL2 =
                                     TBL-YT1-SRYCDTOTAL(IDX6)      *
                                     TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                             ADD     WRK-RUIKEITOTAL2
                                TO  TBL-YT2Y1-RUIKEITOTAL4(IDX7 IDX8 26)
                             ADD    TBL-YT1-MEISAISU(IDX6)
                                TO  TBL-YT2Y1-MEISAISU(IDX7 IDX8 26)
      *
                           END-IF
                        END-IF
      *
                     END-PERFORM
      *
                   END-PERFORM
      *
              ELSE
      *
                   PERFORM   VARYING   IDX7    FROM   1   BY   1
                              UNTIL   (IDX7      >    31)
      *
                     PERFORM   VARYING   IDX8    FROM   1   BY   1
                                UNTIL   (IDX8      >    9)
      *
                        IF   TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                                                        NOT =  ZERO
                           PERFORM   VARYING   IDX9    FROM   1   BY   1
                            UNTIL (IDX9      >    3)
                              OR  (TBL-YT2X2-RECE04-REC(IDX7 IDX8 IDX9)
                                                              =  SPACE)
                                 CONTINUE
                           END-PERFORM
                           IF    IDX9   >   3
                             DISPLAY "TABLE OVER3"
                           ELSE
                             MOVE   TBL-YT1-RECE04-REC(IDX6)
                                TO  TBL-YT2X2-RECE04-REC(IDX7 IDX8 IDX9)
                             MOVE   TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                                TO  TBL-YT2X2-KAISU(IDX7 IDX8 IDX9)
                             MOVE   TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                                TO  TBL-YT2X2-DAY(IDX7 IDX8 IDX9 IDX7)
                             MOVE   TBL-YT1-HIKAKU(IDX6)
                                TO  TBL-YT2Y2-HIKAKU(IDX7 IDX8 IDX9)
      *
                             COMPUTE WRK-RUIKEITOTAL  =
                                     TBL-YT1-MEISAISU(IDX6)        *
                                     TBL-YT1-KAISU(IDX6 IDX7 IDX8) *
                                     TBL-YT1-SURYOUTOTAL(IDX6)
                             MOVE    WRK-RUIKEITOTAL
                                TO  TBL-YT2Y2-RUIKEITOTAL2
                                                       (IDX7 IDX8 IDX9)
                             COMPUTE WRK-RUIKEITOTAL  =
                                     TBL-YT1-SRYCDTOTAL(IDX6)      *
                                     TBL-YT1-SURYOUTOTAL(IDX6)
                             MOVE    WRK-RUIKEITOTAL
                                TO  TBL-YT2Y2-RUIKEITOTAL3
                                                       (IDX7 IDX8 IDX9)
                             COMPUTE WRK-RUIKEITOTAL2 =
                                     TBL-YT1-SRYCDTOTAL(IDX6)      *
                                     TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                             MOVE    WRK-RUIKEITOTAL2
                                TO  TBL-YT2Y2-RUIKEITOTAL4
                                                       (IDX7 IDX8 IDX9)
      *
                             ADD    TBL-YT1-ZAITEN(IDX6)
                                TO  TBL-YT2Y2-ZAITEN(IDX7 IDX8 4)
                             ADD    TBL-YT1-SRYCDTOTAL(IDX6)
                                TO  TBL-YT2Y2-SRYCDTOTAL(IDX7 IDX8 4)
                             ADD    TBL-YT1-SURYOUTOTAL(IDX6)
                                TO  TBL-YT2Y2-SURYOUTOTAL(IDX7 IDX8 4)
                             ADD    TBL-YT1-RUIKEITOTAL(IDX6)
                                TO  TBL-YT2Y2-RUIKEITOTAL(IDX7 IDX8 4)
                             COMPUTE WRK-RUIKEITOTAL  =
                                     TBL-YT1-MEISAISU(IDX6)        *
                                     TBL-YT1-KAISU(IDX6 IDX7 IDX8) *
                                     TBL-YT1-SURYOUTOTAL(IDX6)
                             ADD     WRK-RUIKEITOTAL
                                TO  TBL-YT2Y2-RUIKEITOTAL2(IDX7 IDX8 4)
                             COMPUTE WRK-RUIKEITOTAL  =
                                     TBL-YT1-SRYCDTOTAL(IDX6)      *
                                     TBL-YT1-SURYOUTOTAL(IDX6)
                             ADD     WRK-RUIKEITOTAL
                                TO  TBL-YT2Y2-RUIKEITOTAL3(IDX7 IDX8 4)
                             COMPUTE WRK-RUIKEITOTAL2 =
                                     TBL-YT1-SRYCDTOTAL(IDX6)      *
                                     TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                             ADD     WRK-RUIKEITOTAL2
                                TO  TBL-YT2Y2-RUIKEITOTAL4(IDX7 IDX8 4)
                             ADD    TBL-YT1-MEISAISU(IDX6)
                                TO  TBL-YT2Y2-MEISAISU(IDX7 IDX8 4)
      *
                           END-IF
                        END-IF
      *
                     END-PERFORM
      *
                   END-PERFORM
      *
              END-IF
      *
           END-PERFORM
      *
           IF   WRK-TAIHI-YT1-KEY  NOT =  SPACE
              PERFORM   2401-YAKUTEI-MATOME-SEC
              PERFORM   2402-YAKUTEI-WRITE-SEC
           END-IF
      *
           .
       240-YAKUTEI-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆâÉþÌôºÞÎÁÄþ¸º¤Ë´Ø¤¹¤ë¥Õ¥¡¥¤¥ë¤Þ¤È¤á½èÍý
      *****************************************************************
       2401-YAKUTEI-MATOME-SEC          SECTION.
      *
           PERFORM   VARYING   IDX7    FROM   1   BY   1
                      UNTIL   (IDX7      >    31)
      *
              PERFORM   VARYING   IDX8    FROM   1   BY   1
                         UNTIL   (IDX8      >    9)
      *
               IF  (TBL-YT2Y1-HIKAKU(IDX7 IDX8 26)  NOT = ZERO)
               OR  (TBL-YT2Y2-HIKAKU(IDX7 IDX8  4)  NOT = ZERO)
                 MOVE     ZERO     TO    FLG-YAKUTEI
                 PERFORM    VARYING   IDX9    FROM   1   BY   1
                       UNTIL   (IDX9                          >   100 )
                         OR    (TBL-YT3X1-RECE04-REC(IDX9 1)  =  SPACE)
                         OR    (FLG-YAKUTEI                   =   1   )
      *
                    IF   (TBL-YT2Y1-HIKAKU(IDX7 IDX8 26) =
                                              TBL-YT3Y1-HIKAKU(IDX9 26))
                    AND  (TBL-YT2Y2-HIKAKU(IDX7 IDX8  4) =
                                              TBL-YT3Y2-HIKAKU(IDX9  4))
      *
                     MOVE    ZERO    TO   FLG-YAKUTEI2
                     PERFORM      VARYING   IDX17  FROM   1   BY   1
                         UNTIL (IDX17                         >    25 )
                           OR  (TBL-YT3X1-RECE04-REC(IDX9 IDX17)
                                                              =  SPACE)
                       MOVE    ZERO    TO   FLG-YAKUTEI2
                       PERFORM    VARYING   IDX10  FROM   1   BY   1
                         UNTIL (IDX10                         >    25 )
                           OR  (TBL-YT2X1-RECE04-REC(IDX7 IDX8 IDX10)
                                                              =  SPACE)
                           OR  (FLG-YAKUTEI2                  =   1   )
                        IF  TBL-YT3Y1-HIKAKU(IDX9 IDX17)      =
                            TBL-YT2Y1-HIKAKU(IDX7 IDX8 IDX10)
                         ADD    TBL-YT2X1-KAISU(IDX7 IDX8 IDX10)
                                    TO  TBL-YT3X1-KAISU(IDX9 IDX17)
                         ADD    TBL-YT2X1-DAY(IDX7 IDX8 IDX10 IDX7)
                                    TO  TBL-YT3X1-DAY(IDX9 IDX17 IDX7)
                         MOVE   1   TO  FLG-YAKUTEI2
                        END-IF
                       END-PERFORM
                     END-PERFORM
      *
                     MOVE    ZERO    TO   FLG-YAKUTEI2
                     PERFORM      VARYING   IDX17  FROM   1   BY   1
                         UNTIL (IDX17                         >     3 )
                           OR  (TBL-YT3X2-RECE04-REC(IDX9 IDX17)
                                                              =  SPACE)
                       MOVE    ZERO    TO   FLG-YAKUTEI2
                       PERFORM    VARYING   IDX10  FROM   1   BY   1
                         UNTIL (IDX10                         >     3 )
                           OR  (TBL-YT2X2-RECE04-REC(IDX7 IDX8 IDX10)
                                                              =  SPACE)
                           OR  (FLG-YAKUTEI2                  =   1   )
                        IF  TBL-YT3Y2-HIKAKU(IDX9 IDX17)      =
                            TBL-YT2Y2-HIKAKU(IDX7 IDX8 IDX10)
                         ADD    TBL-YT2X2-KAISU(IDX7 IDX8 IDX10)
                                    TO  TBL-YT3X2-KAISU(IDX9 IDX17)
                         ADD    TBL-YT2X2-DAY(IDX7 IDX8 IDX10 IDX7)
                                    TO  TBL-YT3X2-DAY(IDX9 IDX17 IDX7)
                         MOVE   1   TO  FLG-YAKUTEI2
                        END-IF
                       END-PERFORM
                     END-PERFORM
      *
                     MOVE    1    TO    FLG-YAKUTEI
                    END-IF
      *
                 END-PERFORM
                 IF  FLG-YAKUTEI  =  ZERO
                    IF   IDX9    >    100
                       DISPLAY   "TABLE OVER4"
                    ELSE
                       MOVE   TBL-YT2X-KAIT(IDX7 IDX8)
                                      TO  TBL-YT3X-KAIT(IDX9)
                       MOVE   TBL-YT2Y-KAIT(IDX7 IDX8)
                                      TO  TBL-YT3Y-KAIT(IDX9)
                    END-IF
                 END-IF
               END-IF
      *
              END-PERFORM
      *
           END-PERFORM
      *
           .
       2401-YAKUTEI-MATOME-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆâÉþÌôºÞÎÁÄþ¸º¤Ë´Ø¤¹¤ë¥Õ¥¡¥¤¥ë½ÐÎÏ
      *****************************************************************
       2402-YAKUTEI-WRITE-SEC          SECTION.
      *
           PERFORM   VARYING   IDX7    FROM   1   BY   1
                      UNTIL   (IDX7                          >   100 )
                        OR    (TBL-YT3X1-RECE04-REC(IDX7 1)  =  SPACE)
      *
                 PERFORM    VARYING   IDX8    FROM   1   BY   1
                             UNTIL   (IDX8                     >   25  )
                               OR    (TBL-YT3X1-RECE04-REC(IDX7 IDX8)
                                                               =  SPACE)
      *
                      INITIALIZE       RECE049-REC
                      MOVE   TBL-YT3X1-RECE04-REC(IDX7 IDX8)
                                  TO   RECE049-REC
                      MOVE   RECE049-ZAINUM  TO  WRK-YAKUTEI-ZAINUM
                      MOVE   TBL-YT3X1-KAISU(IDX7 IDX8)
                                  TO   RECE049-ZAIKAISU
                      INITIALIZE       RECE049-DAY-AREA
                      PERFORM VARYING IDX17 FROM 1 BY 1
                               UNTIL  IDX17   > 31
                        MOVE   TBL-YT3X1-DAY(IDX7 IDX8 IDX17)
                                  TO   RECE049-KAISU(1 IDX17)
                      END-PERFORM
                      ADD       1             TO  IDX11
                      COMPUTE   WRK-ZAINUM    =  99990000  +  IDX11
                      MOVE      WRK-ZAINUM    TO  RECE049-ZAINUM
                      MOVE      RECE049-ZAINUM  TO  WRK-YAKUTEI-ZAINUM2
      *
                      WRITE     RECE049-REC
                      ADD       1            TO  CNT-RECE049
                      PERFORM   24021-YAKUTEI-RECE05-TENSOU-SEC
      *
                 END-PERFORM
      *
                 PERFORM    VARYING   IDX8    FROM   1   BY   1
                              UNTIL  (IDX8                     >    3  )
                                OR   (TBL-YT3X2-RECE04-REC(IDX7 IDX8)
                                                               =  SPACE)
      *
                      INITIALIZE       RECE049-REC
                      MOVE   TBL-YT3X2-RECE04-REC(IDX7 IDX8)
                                  TO   RECE049-REC
                      MOVE   RECE049-ZAINUM  TO  WRK-YAKUTEI-ZAINUM
                      MOVE   TBL-YT3X2-KAISU(IDX7 IDX8)
                                  TO   RECE049-ZAIKAISU
                      INITIALIZE       RECE049-DAY-AREA
                      PERFORM VARYING IDX17 FROM 1 BY 1
                               UNTIL  IDX17   > 31
                        MOVE   TBL-YT3X2-DAY(IDX7 IDX8 IDX17)
                                  TO   RECE049-KAISU(1 IDX17)
                      END-PERFORM
                      ADD       1             TO  IDX11
                      COMPUTE   WRK-ZAINUM    =  99990000  +  IDX11
                      MOVE      WRK-ZAINUM    TO  RECE049-ZAINUM
                      MOVE      RECE049-ZAINUM  TO  WRK-YAKUTEI-ZAINUM2
      *
                      WRITE     RECE049-REC
                      ADD       1            TO  CNT-RECE049
                      PERFORM   24021-YAKUTEI-RECE05-TENSOU-SEC
      *
                 END-PERFORM
      *
           END-PERFORM
      *
           .
       2402-YAKUTEI-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆâÉþÌôºÞÎÁÄþ¸º¤Ë´Ø¤¹¤ë¥Õ¥¡¥¤¥ë½ÐÎÏ¡Ê£°£µ¥Õ¥¡¥¤¥ë¡Ë
      *****************************************************************
       24021-YAKUTEI-RECE05-TENSOU-SEC          SECTION.
      *
           PERFORM 800-RECE049-SET05-SEC
           MOVE    WRK-YAKUTEI-ZAINUM   TO  RECE05-ZAINUM
           START   RECE05-FILE
               KEY IS    >=   RECE05-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE05
               NOT INVALID
                   PERFORM 990-RECE05-NEXT-SEC
           END-START
           PERFORM
               UNTIL ( FLG-RECE05              =   1             )
               OR    ( RECE049-KEY1        NOT =   RECE05-KEY1   )
               OR    ( WRK-YAKUTEI-ZAINUM  NOT =   RECE05-ZAINUM )
      *
               MOVE     RECE05-REC            TO      RECE059-REC
               MOVE     WRK-YAKUTEI-ZAINUM2   TO      RECE059-ZAINUM
               WRITE    RECE059-REC
               ADD      1                     TO      CNT-RECE059
      *
               PERFORM 990-RECE05-NEXT-SEC
      *
           END-PERFORM
      *
           .
       24021-YAKUTEI-RECE05-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    º¹¹þÊÔ½¸£²
      *****************************************************************
       250-SASHIKOMI2-SEC     SECTION.
      *
      *    ¥Ñ¥é¥á¡¼¥¿£°£´£¹¥Õ¥¡¥¤¥ë¤è¤êÊÔ½¸
           PERFORM 800-RECE02-SET049-SEC
           MOVE    ZERO                TO  RECE049-ZAINUM
      *
           START   RECE049-FILE
               KEY IS    >=   RECE049-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE049
               NOT INVALID
                   PERFORM 990-RECE049-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE049          =   1            )
               OR     ( RECE02-KEY       NOT =   RECE049-KEY1 )
      *
               MOVE   ZERO   TO  FLG-COMMENT
      *        ¡Ê»ÀÁÇ·×»»¼°¤Î¥³¥á¥ó¥È¤Î¥»¥Ã¥È¡Ë
               IF   FLG-COMMENT  =  ZERO
                 PERFORM    2501-SANSO-HANTEI-SEC
               END-IF
      *        ¡Ê¥Õ¥¤¥ë¥à¤Î¥³¥á¥ó¥È¤Î¥»¥Ã¥È¡Ë
               IF   FLG-COMMENT  =  ZERO
                 PERFORM    2502-FILM-HANTEI-SEC
               END-IF
               IF   FLG-COMMENT  =  ZERO
                 PERFORM    2599-RECE049-TENSOU-SEC
               END-IF
      *
               PERFORM 990-RECE049-NEXT-SEC
      *
           END-PERFORM
      *
           .
       250-SASHIKOMI2-EXT.
           EXIT.
      *
      *****************************************************************
      *    »ÀÁÇ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2501-SANSO-HANTEI-SEC          SECTION.
      *
           IF     RECE049-FLG-SANSO-COMMENT   =    "1"
               MOVE       99      TO     FLG-COMMENT
      *
               PERFORM    25011-SANSO-TAIHI-SEC
      *
               PERFORM    25011-SANSO-COMMENT-SEC
      *
           END-IF
      *
           .
       2501-SANSO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    »ÀÁÇ¤Î¿ôÎÌ¡¦Ã±²ÁÂàÈò
      *****************************************************************
       25011-SANSO-TAIHI-SEC          SECTION.
      *
           MOVE       ZERO    TO     IDX4
           INITIALIZE                TBL-SANSO-INF
      *
           PERFORM 800-RECE049-SET059-SEC
           MOVE    RECE049-ZAINUM       TO  RECE059-ZAINUM
           START   RECE059-FILE
               KEY IS    >=   RECE059-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE059
               NOT INVALID
                   PERFORM 990-RECE059-NEXT-SEC
           END-START
           PERFORM
               UNTIL ( FLG-RECE059           =  1              )
               OR    ( RECE049-KEY1      NOT =  RECE059-KEY1   )
               OR    ( RECE049-ZAINUM    NOT =  RECE059-ZAINUM )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                          UNTIL  ( IDX                >   5     )
                           OR    ( RECE059-SRYCD(IDX) =   SPACE )
      *
                  SET     TBL-SANSO    TO     1
                  SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                       AT  END
                           MOVE    ZERO    TO   FLG-SANSO-OK
                       WHEN   RECE059-SRYCD(IDX) = TBL-SANSO-SRYCD
                                                       (TBL-SANSO)
                           MOVE    1       TO   FLG-SANSO-OK
                  END-SEARCH
                  IF      FLG-SANSO-OK        =    1
                     ADD      1             TO  IDX4
                     MOVE  RECE059-TEN(IDX) TO  TBL-SANSO-TEN(IDX4)
                     MOVE  RECE059-SRYSURYO(IDX)
                                            TO  TBL-SANSO-SRYSURYO
                                                             (IDX4)
                  END-IF
                  IF    RECE059-SRYCD(IDX) = "770030070"
                     IF   TBL-KASAN-SRYSURYO  =  ZERO
                        MOVE   RECE059-SRYSURYO(IDX)
                                     TO  TBL-KASAN-SRYSURYO
                     ELSE
                        COMPUTE   TBL-KASAN-SRYSURYO  =  
                                  TBL-KASAN-SRYSURYO  *  
                                  RECE059-SRYSURYO(IDX)  
                     END-IF
                  END-IF
      *
               END-PERFORM 
      *
               PERFORM 990-RECE059-NEXT-SEC
      *
           END-PERFORM
      *
           .
       25011-SANSO-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    »ÀÁÇ¤Î¥³¥á¥ó¥ÈÊÔ½¸¡Êº¹¹þ¡§²¼¡Ë
      *****************************************************************
       25011-SANSO-COMMENT-SEC          SECTION.
      *
           MOVE       ZERO       TO   IDZ                               
           INITIALIZE            WRK-FREE-AREA2                         
           INITIALIZE            WRK-FREE-AREA3
      *
           PERFORM  250111-SANSO-FREE-COMMENT-SEC
      *
           PERFORM  250111-RECE059-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ£¹¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE05
           MOVE    SPACE                TO  WRK-RECE05XX-REC
           INITIALIZE                       WRK-RECE05XX-REC
           MOVE    ZERO                 TO  WRK-JYUNBAN
                                            WRK-GYOSUU
                                            WRK-GYOSUU2
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
      *
                ADD       1      TO     WRK-GYOSUU
      *
           END-PERFORM
      *
           PERFORM    250111-SRYACTINF2-HENSYU-SEC
      *
           .
       25011-SANSO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    »ÀÁÇ·×»»¼°ÊÔ½¸
      *****************************************************************
       250111-SANSO-FREE-COMMENT-SEC          SECTION.                      
      *
           MOVE     1          TO    WRK-KETA
           MOVE    ZERO        TO    WRK-KEISAN4
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                      UNTIL   (IDX                 >    10)
                       OR     (TBL-SANSO-TEN(IDX)  =  ZERO)
      *
             IF   IDX  >  1
                MOVE    "¡Ü"   TO  WK-FREE-NAME3(WRK-KETA:2)
                COMPUTE  WRK-KETA   =  WRK-KETA   +  2
             END-IF
      *
             MOVE    "¡Ê"   TO  WK-FREE-NAME3(WRK-KETA:2)
             COMPUTE  WRK-KETA   =  WRK-KETA   +  2
      *Ã±²Á
             MOVE     TBL-SANSO-TEN(IDX)   TO   WRK-KEISAN
             COMPUTE  WRK-KEISAN2  = (TBL-SANSO-TEN(IDX) -  WRK-KEISAN)
                                   *   1000
      *
             PERFORM  2501111-SANSO-FREE-COM-NUM-SEC
      *
             MOVE    "±ß¡ß" TO  WK-FREE-NAME3(WRK-KETA:4)
             COMPUTE  WRK-KETA   =  WRK-KETA   +  4
      *¿ôÎÌ
             MOVE     TBL-SANSO-SRYSURYO(IDX)  TO  WRK-KEISAN
             COMPUTE  WRK-KEISAN2  
                             = (TBL-SANSO-SRYSURYO(IDX) -  WRK-KEISAN)
                             *   1000
      *
             PERFORM  2501111-SANSO-FREE-COM-NUM-SEC
      *
             IF    TBL-KASAN-SRYSURYO  =  ZERO
                MOVE    "£Ì¡ß£±¡¥£³¡Ë" TO  WK-FREE-NAME3(WRK-KETA:12)
                COMPUTE  WRK-KETA   =  WRK-KETA   +  12
      *
      ***       COMPUTE  WRK-KEISAN3   =   WRK-KEISAN3  +
      ***         (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3)
                COMPUTE  WRK-KEISAN3   =
                  (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3)
                                                                + 0.5
                COMPUTE  WRK-KEISAN4  = WRK-KEISAN4 + WRK-KEISAN3
             ELSE
                MOVE    "£Ì¡ß£±¡¥£³¡ß" TO  WK-FREE-NAME3(WRK-KETA:12)
                COMPUTE  WRK-KETA   =  WRK-KETA   +  12
      *
                MOVE     TBL-KASAN-SRYSURYO  TO  WRK-KEISAN
                COMPUTE  WRK-KEISAN2  
                             = (TBL-KASAN-SRYSURYO -  WRK-KEISAN)
                             *   1000
      *
                PERFORM  2501111-SANSO-FREE-COM-NUM-SEC
      *
                MOVE    "¡Ë" TO  WK-FREE-NAME3(WRK-KETA:2)
                COMPUTE  WRK-KETA   =  WRK-KETA   +  2
      *
      ***       COMPUTE  WRK-KEISAN3   =   WRK-KEISAN3  +
      ***         (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3
      ***                             * TBL-KASAN-SRYSURYO)
                COMPUTE  WRK-KEISAN3   =
                  (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3
                                      * TBL-KASAN-SRYSURYO)     + 0.5
                COMPUTE  WRK-KEISAN4  = WRK-KEISAN4 + WRK-KEISAN3
             END-IF
      *
           END-PERFORM
      *
           IF   IDX  =  2
              MOVE    "¡à£±£°¡á"  TO  WK-FREE-NAME3(WRK-KETA:8)
              COMPUTE  WRK-KETA   =  WRK-KETA   +  8
      *
      ***     COMPUTE  WRK-KEISAN =   WRK-KEISAN3  +  0.5
      ***     COMPUTE  WRK-KEISAN =  (WRK-KEISAN   /  10)  +  0.5
              COMPUTE  WRK-KEISAN =  (WRK-KEISAN4  /  10)  +  0.5
              MOVE     ZERO       TO   WRK-KEISAN2
      *
              PERFORM  2501111-SANSO-FREE-COM-NUM-SEC
      *
              MOVE    "ÅÀ"   TO  WK-FREE-NAME3(WRK-KETA:2)
              COMPUTE  WRK-KETA   =  WRK-KETA   +  2
           ELSE
              IF   IDX  >  2
                  MOVE   WK-FREE-NAME3  TO  WK-FREE-NAME3-TAIHI
                  MOVE    "¡Ê"    TO   WK-FREE-NAME3(1:2)
                  MOVE   WK-FREE-NAME3-TAIHI
                                  TO   WK-FREE-NAME3(3:)
                  COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                  MOVE    "¡Ë¡à£±£°¡á"  TO  WK-FREE-NAME3(WRK-KETA:10)
                  COMPUTE  WRK-KETA   =  WRK-KETA   +  10
      *
      ***         COMPUTE  WRK-KEISAN =   WRK-KEISAN3  +  0.5
      ***         COMPUTE  WRK-KEISAN =  (WRK-KEISAN   /  10)  +  0.5
                  COMPUTE  WRK-KEISAN =  (WRK-KEISAN4  /  10)  +  0.5
                  MOVE     ZERO       TO   WRK-KEISAN2
      *
                  PERFORM  2501111-SANSO-FREE-COM-NUM-SEC
      *
                  MOVE    "ÅÀ"   TO  WK-FREE-NAME3(WRK-KETA:2)
                  COMPUTE  WRK-KETA   =  WRK-KETA   +  2
              END-IF
           END-IF
      *
           IF     WRK-KETA    >   1
               PERFORM   VARYING   IDX4    FROM   1   BY   50
                          UNTIL   (IDX4     >    1000)
                            OR    (WK-FREE-NAME3(IDX4:1) = SPACE)
                   ADD     1      TO    IDZ
                   MOVE    ZERO   TO    WRK-KETA2
                   PERFORM   VARYING   IDX5    FROM   1   BY   2
                              UNTIL   (IDX5     >    50)
                      IF   WK-FREE-NAME3(IDX5:2)    = SPACE
                          MOVE   IDX5   TO   WRK-KETA2
                          MOVE   50     TO   IDX5
                      END-IF
                   END-PERFORM
                   IF   WRK-KETA2 =  ZERO
                        MOVE   25   TO  WK-FREE-YUKOKETA2(IDZ)
                   ELSE
                        COMPUTE   WK-FREE-YUKOKETA2(IDZ)  =
                                       ( WRK-KETA2 - 1 )  /  2
                   END-IF
                   MOVE   WK-FREE-NAME3(IDX4:50)
                                   TO    WK-FREE-NAME2(IDZ)
               END-PERFORM
           END-IF
      *
           .
       250111-SANSO-FREE-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    »ÀÁÇ·×»»¼°¡¦¿ôÃÍÊÔ½¸
      *****************************************************************
       2501111-SANSO-FREE-COM-NUM-SEC          SECTION.
      *
           IF    WRK-KEISAN  NOT  =  ZERO
               PERFORM   VARYING   IDX4    FROM   1   BY   1
                          UNTIL   (IDX4               >    9)
                     IF   WRK-KEISAN(IDX4:1) =   "0"
                        CONTINUE
                     ELSE
                        MOVE  IDX4   TO  IDX5
                        MOVE   9     TO  IDX4
                     END-IF
               END-PERFORM
               MOVE  SPACE             TO  WRK-KEISAN-TAIHI
               MOVE  WRK-KEISAN(IDX5:) TO  WRK-KEISAN-TAIHI
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI
               MOVE  WRK-KEISAN-TAIHI  TO  KANACHK-MAE-INPUT
               CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   ZERO
                   MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                               TO  WK-FREE-NAME3(WRK-KETA:KANACHK-MAX)
                   COMPUTE  WRK-KETA   =  WRK-KETA   +  KANACHK-MAX
               END-IF
           END-IF
      *
           IF    WRK-KEISAN2  NOT  =  ZERO
                PERFORM   VARYING   IDX4    FROM   3   BY  -1
                           UNTIL   (IDX4               <    1)
                      IF   WRK-KEISAN2(IDX4:1) =   "0"
                         CONTINUE
                      ELSE
                         MOVE  IDX4   TO  IDX5
                         MOVE   1     TO  IDX4
                      END-IF
                END-PERFORM
                MOVE  SPACE               TO  WRK-KEISAN-TAIHI
                MOVE  WRK-KEISAN2(1:IDX5) TO  WRK-KEISAN-TAIHI
      *
                IF    WRK-KEISAN  NOT  =  ZERO
                   MOVE    "¡¥"     TO  WK-FREE-NAME3(WRK-KETA:2)
                   COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                ELSE
                   MOVE    "£°¡¥"   TO  WK-FREE-NAME3(WRK-KETA:4)
                   COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                END-IF
      *
                INITIALIZE                  ORCSKANACHKAREA
                MOVE    "2"             TO  KANACHK-SYORI
                MOVE  WRK-KEISAN-TAIHI  TO  KANACHK-MAE-INPUT
                CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                IF      KANACHK-RC      =   ZERO
                    MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                TO  WK-FREE-NAME3(WRK-KETA:KANACHK-MAX)
                    COMPUTE  WRK-KETA   =  WRK-KETA   +  KANACHK-MAX
                END-IF
           END-IF
      *
           IF    WRK-KEISAN    =  ZERO
           AND   WRK-KEISAN2   =  ZERO
               MOVE    "£°"   TO  WK-FREE-NAME3(WRK-KETA:2)
               COMPUTE  WRK-KETA   =  WRK-KETA   +  2
           END-IF
      *
           .
       2501111-SANSO-FREE-COM-NUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    £°£µ£¹¡¡ÂàÈò½èÍý
      *****************************************************************
       250111-RECE059-TAIHI-SEC     SECTION.
      *
           PERFORM 800-RECE049-SET059-SEC
           MOVE    RECE049-ZAINUM       TO  RECE059-ZAINUM
           START   RECE059-FILE
                 KEY IS    >=   RECE059-KEY
                 INVALID
                     MOVE    1          TO  FLG-RECE059
                 NOT INVALID
                     PERFORM 990-RECE059-NEXT-SEC
                     MOVE   RECE059-REC TO  WRK-RECE05-REC
           END-START
      *
           .
       250111-RECE059-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    £°£µ£¹¡¡£Ó£Ò£Ù£Á£Ã£Ô£É£Î£ÆÊÔ½¸½èÍý
      *****************************************************************
       250111-SRYACTINF2-HENSYU-SEC     SECTION.
      *
           PERFORM
               UNTIL  ( FLG-RECE059           =   1                    )
               OR     ( RECE049-KEY1      NOT =   RECE059-KEY1         )
               OR     ( RECE049-ZAINUM    NOT =   RECE059-ZAINUM       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   5     )
                         OR      ( RECE059-SRYCD(IDX) =   SPACE )
                    ADD    1             TO      IDX-RECE05
                    MOVE   RECE059-SRYACT-TBL(IDX)
                             TO   WRK-RECE05XX-SRYACT-TBL(IDX-RECE05 1)
      *
                    IF   RECE059-SRYCD(IDX) = "770020070" OR "770030070"
                         MOVE   IDX-RECE05   TO   WRK-JYUNBAN
                    END-IF
      *
               END-PERFORM 
      *
               PERFORM 990-RECE059-NEXT-SEC
      *
           END-PERFORM
      *
           IF     WRK-GYOSUU   NOT =  ZERO
      *
      *      ÊäÀµÎ¨Ëô¤Ï¹âµ¤°µ»ÀÁÇ²Ã»»°Ê¹ß¤ÎÌÀºÙ¤ò¤º¤é¤¹
             IF   WRK-JYUNBAN  NOT =  ZERO
      *
               PERFORM   VARYING   IDX   FROM   IDX-RECE05   BY  -1
                          UNTIL  ( IDX    <    1           )
                           OR    ( IDX    <=   WRK-JYUNBAN )
      *
                   COMPUTE   WRK-GYOSUU2  =  IDX  +  WRK-GYOSUU
                   MOVE   WRK-RECE05XX-SRYACT-TBL(IDX 1)  TO
                          WRK-RECE05XX-SRYACT-TBL(WRK-GYOSUU2 1)
                   INITIALIZE   WRK-RECE05XX-SRYACT-TBL(IDX 1)
      *
               END-PERFORM
      *
             END-IF
      *
      *      ·×»»¼°¤Îº¹¤·¹þ¤ß
             IF   WRK-JYUNBAN  NOT =  ZERO
                 MOVE      WRK-JYUNBAN    TO     MAX-MEISAISU
             ELSE
                 MOVE      IDX-RECE05     TO     MAX-MEISAISU
             END-IF
             PERFORM   VARYING   IDX   FROM    1   BY  1
                        UNTIL  ( IDX                >   50    )
                         OR    ( WK-FREE-NAME2(IDX) =   SPACE )
      *
                ADD    1     TO  MAX-MEISAISU
                MOVE   "810000001"
                             TO  WRK-RECE05XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE05XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE05XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE05XX-NAME(MAX-MEISAISU 1)
                MOVE   "1"   TO  WRK-RECE05XX-RECEDENFLG(MAX-MEISAISU 1)
      *
             END-PERFORM
      *
           END-IF
      *
      *    ¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE05
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE05
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE05  =    1
                    INITIALIZE                        RECE051-REC
                    MOVE    WRK-RECE05-KEY       TO   RECE051-KEY
                    ADD     1                    TO   WRK-RENNUM
                    MOVE    WRK-RENNUM           TO   RECE051-RENNUM
                    MOVE    WRK-RECE05-KOHBUNRI  TO   RECE051-KOHBUNRI
                    MOVE    WRK-RECE05-PTNUM     TO   RECE051-PTNUM
                    MOVE    WRK-RECE05-SRYKA     TO   RECE051-SRYKA
                    MOVE    WRK-RECE05-SRYKBN    TO   RECE051-SRYKBN
                    MOVE    WRK-RECE05-SRYSYUKBN TO   RECE051-SRYSYUKBN
                    MOVE    WRK-RECE05-JIHIMONEY TO   RECE051-JIHIMONEY
                    PERFORM  VARYING   IDX   FROM   1   BY   1
                             UNTIL   (IDX       >    5)   OR
                                     (FLG-WRKRECE05   =   1)
                       ADD         1             TO    IDX-RECE05
                       IF (WRK-RECE05XX-SRYCD(IDX-RECE05 1) = SPACE)
                       OR (IDX-RECE05                       >  100 )
                           MOVE    1             TO    FLG-WRKRECE05
                       ELSE
                           MOVE  WRK-RECE05XX-SRYACT-TBL(IDX-RECE05 1)
                                            TO   RECE051-SRYACT-TBL(IDX)
                           ADD   1          TO   WRK-CNT-MEISAI
                       END-IF  
                    END-PERFORM
                    IF   RECE051-SRYCD(1)  NOT =  SPACE
                       WRITE   RECE051-REC
                       ADD     1            TO   CNT-RECE05
                    END-IF
           END-PERFORM
      *
           MOVE    RECE049-REC       TO      RECE041-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE041-MEISAISU
           WRITE   RECE041-REC
           ADD     1                 TO      CNT-RECE04
      *
           .
       250111-SRYACTINF2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥¤¥ë¥à¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2502-FILM-HANTEI-SEC          SECTION.
      *
           IF     RECE049-FLG-FILM-COMMENT   =    "1"
               MOVE       99      TO     FLG-COMMENT
               PERFORM    25021-FILM-COMMENT-SEC
           END-IF
      *
           .
       2502-FILM-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥¤¥ë¥à¤Î¥³¥á¥ó¥ÈÊÔ½¸¡Êº¹¹þ¡§²¼¡Ë
      *****************************************************************
       25021-FILM-COMMENT-SEC          SECTION.
      *
           INITIALIZE                WRK-JIDO-SETCOM-AREA
           MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
           MOVE       20      TO     WRK-JIDO-SETCOMATFLG
      *
           PERFORM  250111-RECE059-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ£¹¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO       TO     IDX-RECE05
           MOVE    SPACE      TO     WRK-RECE05XX-REC
           INITIALIZE                WRK-RECE05XX-REC
           MOVE    ZERO       TO     WRK-JYUNBAN
      *
           PERFORM  250211-SRYACTINF2-HENSYU-SEC
      *
           .
       25021-FILM-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £°£µ£¹¡¡£Ó£Ò£Ù£Á£Ã£Ô£É£Î£ÆÊÔ½¸½èÍý
      *****************************************************************
       250211-SRYACTINF2-HENSYU-SEC     SECTION.
      *
           PERFORM
               UNTIL  ( FLG-RECE059           =   1                    )
               OR     ( RECE049-KEY1      NOT =   RECE059-KEY1         )
               OR     ( RECE049-ZAINUM    NOT =   RECE059-ZAINUM       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   5     )
                         OR      ( RECE059-SRYCD(IDX) =   SPACE )
                    ADD    1             TO      IDX-RECE05
                    MOVE   RECE059-SRYACT-TBL(IDX)
                             TO   WRK-RECE05XX-SRYACT-TBL(IDX-RECE05 1)
      *
                    IF   RECE059-SRYCD(IDX) =
                         "700010000" OR "700030000" OR "700050000" OR
                         "700080000" OR "700110000" OR "700140000" OR
                         "700290000" OR "700310000" OR "700330000" OR
                         "700710000" OR "700730000" OR "700750000" OR
                         "700780000" OR "700810000" OR "700840000" OR
                         "700870000" OR "700160000" OR "700180000" OR
                         "700190000" OR "700200000" OR "700210000" OR
                         "700220000" OR "700230000" OR "700240000" OR
                         "700250000" OR "700260000" OR "700270000" OR
                         "700280000" OR "700590000" OR "700600000" OR
                         "700630000" OR "700640000" OR "700650000" OR
                         "700680000" OR "729780000"
                         MOVE   IDX-RECE05   TO   WRK-JYUNBAN
                    END-IF
      *
               END-PERFORM
      *
               PERFORM 990-RECE059-NEXT-SEC
      *
           END-PERFORM
      *
      *    ¥Õ¥£¥ë¥à°Ê¹ß¤ÎÌÀºÙ¤ò¤º¤é¤¹
           IF   WRK-JYUNBAN  NOT =  ZERO
      *
               PERFORM   VARYING   IDX   FROM   IDX-RECE05   BY  -1
                          UNTIL  ( IDX    <    1           )
                           OR    ( IDX    <=   WRK-JYUNBAN )
      *
                   COMPUTE   WRK-GYOSUU2  =  IDX  +  1
                   MOVE   WRK-RECE05XX-SRYACT-TBL(IDX 1)  TO
                          WRK-RECE05XX-SRYACT-TBL(WRK-GYOSUU2 1)
                   INITIALIZE   WRK-RECE05XX-SRYACT-TBL(IDX 1)
      *
               END-PERFORM
      *
      *        Ê¬²è¿ô¤Îº¹¤·¹þ¤ß
               MOVE   WRK-JYUNBAN     TO  MAX-MEISAISU
      *
               ADD    1               TO  MAX-MEISAISU
               MOVE  "810000001"
                              TO  WRK-RECE05XX-SRYCD(MAX-MEISAISU 1)
               MOVE   ZERO    TO  WRK-RECE05XX-SRYSURYO(MAX-MEISAISU 1)
      *
               MOVE   WRK-RECE05XX-SRYKAISU(WRK-JYUNBAN 1)
                                      TO  WRK-SRYKAISU-X
      *        Á´³ÑÊÑ´¹
               INITIALIZE                     WRK-CHAR-HENKAN-AREA
               MOVE     WRK-SRYKAISU-X  TO    WRK-CHAR-X
               PERFORM  2502111-NUM-ZENKAKU-HENKAN-SEC
               STRING   WRK-CHAR-N      DELIMITED  BY  SPACE
                        "Ê¬²è"          DELIMITED  BY  SIZE
                                        INTO  WRK-BUNKAKU-N
               END-STRING
               COMPUTE WRK-BUNKAKUKETA   =   WRK-CHAR-N-CNT  +  4
      *
               COMPUTE WRK-RECE05XX-YUKOKETA(MAX-MEISAISU 1) =
                       WRK-BUNKAKUKETA   /   2
               MOVE    WRK-BUNKAKU-N
                           TO  WRK-RECE05XX-NAME(MAX-MEISAISU 1)
               MOVE    "1" TO  WRK-RECE05XX-RECEPTFLG(MAX-MEISAISU 1)
      *
               IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                 MOVE  "2" TO  WRK-RECE05XX-MEISKYFLG(MAX-MEISAISU 1)
                 MOVE  WRK-JIDO-SETCOMATFLG
                           TO  WRK-RECE05XX-SETCOMATFLG(MAX-MEISAISU 1)
               END-IF
      *
           END-IF
      *
      *    ¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE05
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE05
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE05  =    1
                    INITIALIZE                        RECE051-REC
                    MOVE    WRK-RECE05-KEY       TO   RECE051-KEY
                    ADD     1                    TO   WRK-RENNUM
                    MOVE    WRK-RENNUM           TO   RECE051-RENNUM
                    MOVE    WRK-RECE05-KOHBUNRI  TO   RECE051-KOHBUNRI
                    MOVE    WRK-RECE05-PTNUM     TO   RECE051-PTNUM
                    MOVE    WRK-RECE05-SRYKA     TO   RECE051-SRYKA
                    MOVE    WRK-RECE05-SRYKBN    TO   RECE051-SRYKBN
                    MOVE    WRK-RECE05-SRYSYUKBN TO   RECE051-SRYSYUKBN
                    MOVE    WRK-RECE05-JIHIMONEY TO   RECE051-JIHIMONEY
                    PERFORM  VARYING   IDX   FROM   1   BY   1
                             UNTIL   (IDX       >    5)   OR
                                     (FLG-WRKRECE05   =   1)
                       ADD         1             TO    IDX-RECE05
                       IF (WRK-RECE05XX-SRYCD(IDX-RECE05 1) = SPACE)
                       OR (IDX-RECE05                       >  100 )
                           MOVE    1             TO    FLG-WRKRECE05
                       ELSE
                           MOVE  WRK-RECE05XX-SRYACT-TBL(IDX-RECE05 1)
                                            TO   RECE051-SRYACT-TBL(IDX)
                           ADD   1          TO   WRK-CNT-MEISAI
                       END-IF  
                    END-PERFORM
                    IF   RECE051-SRYCD(1)  NOT =  SPACE
                       WRITE   RECE051-REC
                       ADD     1            TO   CNT-RECE05
                    END-IF
           END-PERFORM
      *
           MOVE    RECE049-REC          TO   RECE041-REC
           MOVE    WRK-CNT-MEISAI       TO   RECE041-MEISAISU
           IF   WRK-JIDO-SETCOMFLG   =  "1"
             MOVE  WRK-JIDO-SETCOMFLG   TO   RECE041-SETCOMFLG
           END-IF
           WRITE   RECE041-REC
           ADD     1                    TO   CNT-RECE04
      *
           .
       250211-SRYACTINF2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ôÃÍÈ¾³Ñ¢ªÁ´³ÑÊÑ´¹½èÍý
      *****************************************************************
       2502111-NUM-ZENKAKU-HENKAN-SEC    SECTION.
      *
           MOVE     1       TO   WRK-CHAR-IDX
           MOVE    ZERO     TO   WRK-CHAR-N-CNT
      *
           PERFORM   VARYING   IDX18   FROM    1   BY  1
                      UNTIL    IDX18     >     3
              IF  WRK-CHAR-X(IDX18:1)  IS   NUMERIC
                 IF   WRK-CHAR-X(IDX18:1) = "0"
                   MOVE   "£°"      TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                 ELSE
                   MOVE   WRK-CHAR-X(IDX18:1)
                                    TO  IDX19
                   MOVE   TBL-DAY(IDX19)(3:2)
                                    TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                 END-IF
                 ADD    2           TO  WRK-CHAR-IDX
                                        WRK-CHAR-N-CNT
              END-IF
           END-PERFORM
      *
           .
       2502111-NUM-ZENKAKU-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£´£¹Å¾Á÷½èÍý
      *****************************************************************
       2599-RECE049-TENSOU-SEC     SECTION.
      *
           MOVE       RECE049-REC    TO   RECE041-REC
           WRITE      RECE041-REC
           ADD        1              TO   CNT-RECE04
           PERFORM    25991-RECE059-TENSOU-SEC
      *
           .
       2599-RECE049-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£µ£¹Å¾Á÷½èÍý
      *****************************************************************
       25991-RECE059-TENSOU-SEC     SECTION.
      *
           PERFORM  250111-RECE059-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ£¹¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE05
           MOVE    SPACE                TO  WRK-RECE05XX-REC
           INITIALIZE                       WRK-RECE05XX-REC
      *
           PERFORM
               UNTIL  ( FLG-RECE059           =   1                    )
               OR     ( RECE049-KEY1      NOT =   RECE059-KEY1         )
               OR     ( RECE049-ZAINUM    NOT =   RECE059-ZAINUM       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   5     )
                         OR      ( RECE059-SRYCD(IDX) =   SPACE )
                    ADD    1             TO      IDX-RECE05
                    MOVE   RECE059-SRYACT-TBL(IDX)
                             TO   WRK-RECE05XX-SRYACT-TBL(IDX-RECE05 1)
               END-PERFORM
      *
               PERFORM 990-RECE059-NEXT-SEC
      *
           END-PERFORM
      *
      *    ¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE05
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE05
           MOVE       ZERO             TO      WRK-RENNUM
           PERFORM    UNTIL     FLG-WRKRECE05  =    1
                    INITIALIZE                        RECE051-REC
                    MOVE    WRK-RECE05-KEY       TO   RECE051-KEY
                    ADD     1                    TO   WRK-RENNUM
                    MOVE    WRK-RENNUM           TO   RECE051-RENNUM
                    MOVE    WRK-RECE05-KOHBUNRI  TO   RECE051-KOHBUNRI
                    MOVE    WRK-RECE05-PTNUM     TO   RECE051-PTNUM
                    MOVE    WRK-RECE05-SRYKA     TO   RECE051-SRYKA
                    MOVE    WRK-RECE05-SRYKBN    TO   RECE051-SRYKBN
                    MOVE    WRK-RECE05-SRYSYUKBN TO   RECE051-SRYSYUKBN
                    MOVE    WRK-RECE05-JIHIMONEY TO   RECE051-JIHIMONEY
                    PERFORM  VARYING   IDX   FROM   1   BY   1
                             UNTIL   (IDX       >    5)   OR
                                     (FLG-WRKRECE05   =   1)
                       ADD         1             TO    IDX-RECE05
                       IF (WRK-RECE05XX-SRYCD(IDX-RECE05 1) = SPACE)
                       OR (IDX-RECE05                       >  100 )
                           MOVE    1             TO    FLG-WRKRECE05
                       ELSE
                           MOVE  WRK-RECE05XX-SRYACT-TBL(IDX-RECE05 1)
                                            TO   RECE051-SRYACT-TBL(IDX)
                       END-IF
                    END-PERFORM
                    IF   RECE051-SRYCD(1)  NOT =  SPACE
                       WRITE   RECE051-REC
                       ADD     1            TO   CNT-RECE05
                    END-IF
           END-PERFORM
      *
           .
       25991-RECE059-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÁªÂò¼°¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       270-SENTAKUCOM-HENSYU-SEC     SECTION.
      *
      *    ¥Ñ¥é¥á¡¼¥¿£°£´£±¥Õ¥¡¥¤¥ë¤è¤êÊÔ½¸
           PERFORM 800-RECE02-SET041-SEC
           MOVE    ZERO                TO  RECE041-ZAINUM
      *
           START   RECE041-FILE
               KEY IS    >=   RECE041-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE041
               NOT INVALID
                   PERFORM 990-RECE041-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE041          =   1            )
               OR     ( RECE02-KEY       NOT =   RECE041-KEY1 )
      *
               IF     RECE041-SENTAKUCOMFLG  =  "1"
                  MOVE    ZERO    TO    FLG-SENTAKU-OKIKAE
                  PERFORM   2701-SENTAKUCOM-HENSYU2-SEC
                  IF  FLG-SENTAKU-OKIKAE     =   1
                    PERFORM 2702-SENTAKUCOM-HENSYU4-SEC
                  END-IF
               END-IF
      *
               PERFORM 990-RECE041-NEXT-SEC
      *
           END-PERFORM
      *
           .
       270-SENTAKUCOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÁªÂò¼°¥³¥á¥ó¥ÈÊÔ½¸£²
      *****************************************************************
       2701-SENTAKUCOM-HENSYU2-SEC     SECTION.
      *
           MOVE    ZERO                TO  IDX45
           MOVE    SPACE               TO  WRK-RECE05XX-REC
           INITIALIZE                      WRK-RECE05XX-REC
      *
           PERFORM 800-RECE041-SET051-SEC
           MOVE    RECE041-ZAINUM      TO  RECE051-ZAINUM
           START   RECE051-FILE
               KEY IS    >=   RECE051-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE051
               NOT INVALID
                   PERFORM 990-RECE051-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE051           =   1              )
               OR     ( RECE041-KEY1      NOT =   RECE051-KEY1   )
               OR     ( RECE041-ZAINUM    NOT =   RECE051-ZAINUM )
      *
               PERFORM  VARYING  IDX  FROM  1  BY  1
                         UNTIL  (IDX                >   5  )
                          OR    (RECE051-SRYCD(IDX) = SPACE)
                 ADD    1   TO   IDX45
                 MOVE   RECE051-SRYACT-TBL(IDX)
                            TO   WRK-RECE05XX-SRYACT-TBL(IDX45 1)
      *
                 IF   (IDX45                                >   1   )
                 AND  (WRK-RECE05XX-SRYCD(IDX45 1)(1:4)     = "8201")
                 AND  (WRK-RECE05XX-MEISKYFLG(IDX45 1)  NOT = "2"   )
                   MOVE      ZERO    TO   FLG-GET
                   MOVE      ZERO    TO   FLG-ETC
                   COMPUTE   IDY12   =    IDX45  -  1
                   PERFORM  VARYING  IDY11  FROM  IDY12  BY  -1
                             UNTIL  (IDY11    <  1)
                              OR    (FLG-GET  =  1)
                     IF (WRK-RECE05XX-SRYCD(IDY11 1)(1:1) = "6" OR "7")
                     OR (WRK-RECE05XX-SRYCD(IDY11 1)(1:3) = "059"     )
                       MOVE    1     TO   FLG-ETC
                       MOVE  IDY11   TO   IDY13
                     ELSE
                       IF  WRK-RECE05XX-SRYCD(IDY11 1)(1:1) = "1"
                         PERFORM 27011-SENTAKUCOM-HENSYU3-SEC
                         IF   FLG-GET = ZERO
                           MOVE    1     TO   FLG-ETC
                           MOVE  IDY11   TO   IDY13
                         END-IF
                       END-IF
                     END-IF
                   END-PERFORM
                   IF     FLG-GET  =   1
                     MOVE      1    TO   FLG-SENTAKU-OKIKAE
                     MOVE     "2"   TO   WRK-RECE05XX-MEISKYFLG(IDX45 1)
                     IF   FLG-ETC  =   1
                       MOVE    ZERO      TO  IDX46
                       MOVE    SPACE     TO  WRK-RECE05ZZ-REC
                       INITIALIZE            WRK-RECE05ZZ-REC
                       PERFORM  VARYING  IDY11  FROM  1  BY  1
                                 UNTIL   IDY11    >   IDY12
                         IF   IDY11 = IDY13
                           ADD   1   TO   IDX46
                           MOVE  WRK-RECE05XX-SRYACT-TBL(IDX45 1)
                                 TO   WRK-RECE05ZZ-SRYACT-TBL(IDX46 1)
                         END-IF
                         ADD     1   TO   IDX46
                         MOVE    WRK-RECE05XX-SRYACT-TBL(IDY11 1)
                                 TO   WRK-RECE05ZZ-SRYACT-TBL(IDX46 1)
                       END-PERFORM
                       PERFORM  VARYING  IDY11  FROM  1  BY  1
                                 UNTIL   IDY11    >   IDX45
                         MOVE   WRK-RECE05ZZ-SRYACT-TBL(IDY11 1)
                                 TO   WRK-RECE05XX-SRYACT-TBL(IDY11 1)
                       END-PERFORM
                     END-IF
                   END-IF
                 END-IF
               END-PERFORM
      *
               PERFORM 990-RECE051-NEXT-SEC
      *
           END-PERFORM
      *
           .
       2701-SENTAKUCOM-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÁªÂò¼°¥³¥á¥ó¥ÈÊÔ½¸£³
      *****************************************************************
       27011-SENTAKUCOM-HENSYU3-SEC     SECTION.
      *
           INITIALIZE                      RKISAI-REC
           MOVE  WRK-RECE05XX-SRYCD(IDY11 1)
                                       TO  RKISAI-SRYCD
           MOVE  RECE02-SRYYM          TO  RKISAI-STYM
           MOVE  RECE02-SRYYM          TO  RKISAI-EDYM
           MOVE  RKISAI-REC            TO  MCPDATA-REC
           MOVE  "tbl_recekisai"       TO  MCP-TABLE
           MOVE  "key5"                TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC  =  ZERO
             MOVE  "tbl_recekisai"     TO  MCP-TABLE
             MOVE  "key5"              TO  MCP-PATHNAME
             PERFORM  900-RKISAI-READ-SEC
             PERFORM  UNTIL (FLG-RKISAI  =  1)
                       OR   (FLG-GET     =  1)
               IF   RKISAI-C-CD  =  WRK-RECE05XX-SRYCD(IDX45 1)
                 MOVE    1    TO    FLG-GET
               END-IF
               MOVE  "tbl_recekisai"   TO  MCP-TABLE
               MOVE  "key5"            TO  MCP-PATHNAME
               PERFORM  900-RKISAI-READ-SEC
             END-PERFORM
           END-IF
           MOVE  "tbl_recekisai"       TO  MCP-TABLE
           MOVE  "key5"                TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       27011-SENTAKUCOM-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÁªÂò¼°¥³¥á¥ó¥ÈÊÔ½¸£´
      *****************************************************************
       2702-SENTAKUCOM-HENSYU4-SEC     SECTION.
      *
           MOVE    ZERO                TO  IDX45
      *
           PERFORM 800-RECE041-SET051-SEC
           MOVE    RECE041-ZAINUM      TO  RECE051-ZAINUM
           START   RECE051-FILE
               KEY IS    >=   RECE051-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE051
               NOT INVALID
                   PERFORM 990-RECE051-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE051           =   1              )
               OR     ( RECE041-KEY1      NOT =   RECE051-KEY1   )
               OR     ( RECE041-ZAINUM    NOT =   RECE051-ZAINUM )
      *
               PERFORM  VARYING  IDX  FROM  1  BY  1
                         UNTIL  (IDX                >   5  )
                          OR    (RECE051-SRYCD(IDX) = SPACE)
                 ADD    1   TO   IDX45
                 MOVE   WRK-RECE05XX-SRYACT-TBL(IDX45 1)
                            TO   RECE051-SRYACT-TBL(IDX)
               END-PERFORM
      *
               REWRITE   RECE051-REC
               ADD       1        TO   CNT-RECE05-RE
      *
               PERFORM 990-RECE051-NEXT-SEC
      *
           END-PERFORM
      *
           MOVE   "1"     TO   RECE041-SETCOMFLG
           REWRITE   RECE041-REC
           ADD     1      TO   CNT-RECE04-RE
      *
           .
       2702-SENTAKUCOM-HENSYU4-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´Ø·¸¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       260-SETCOM-HENSYU-SEC     SECTION.
      *
      *    ¥Ñ¥é¥á¡¼¥¿£°£´£±¥Õ¥¡¥¤¥ë¤è¤êÊÔ½¸
           PERFORM 800-RECE02-SET041-SEC
           MOVE    ZERO                TO  RECE041-ZAINUM
      *
           START   RECE041-FILE
               KEY IS    >=   RECE041-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE041
               NOT INVALID
                   PERFORM 990-RECE041-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE041          =   1            )
               OR     ( RECE02-KEY       NOT =   RECE041-KEY1 )
      *
               IF     RECE041-SETCOMFLG   =    "1"
                  PERFORM 2601-SETCOM-HENSYU2-SEC
                  IF  RECE041-SRYYM   >=  "200804"
                    CONTINUE
                  ELSE
                    MOVE   SPACE   TO   RECE041-SETCOMFLG
                    REWRITE   RECE041-REC
                    ADD     1      TO   CNT-RECE04-RE
                  END-IF
               END-IF
      *
               PERFORM 990-RECE041-NEXT-SEC
      *
           END-PERFORM
      *
           .
       260-SETCOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´Ø·¸¥³¥á¥ó¥ÈÊÔ½¸£²
      *****************************************************************
       2601-SETCOM-HENSYU2-SEC     SECTION.
      *
           MOVE    ZERO    TO    FLG-SETCOM
      *
           MOVE    ZERO    TO    WRK-HEN-RENNUM-X
                                 WRK-HEN-SHORI-RENNUM-X
                                 WRK-HEN-SEQNO-X
           MOVE    ZERO    TO    WRK-HEN-RENNUM-J
                                 WRK-HEN-SHORI-RENNUM-J
                                 WRK-HEN-SEQNO-J
           MOVE    ZERO    TO    WRK-HEN-RENNUM-J2
                                 WRK-HEN-SHORI-RENNUM-J2
                                 WRK-HEN-SEQNO-J2
           MOVE    ZERO    TO    WRK-HEN-RENNUM-J3
                                 WRK-HEN-SHORI-RENNUM-J3
                                 WRK-HEN-SEQNO-J3
      *
           PERFORM 800-RECE041-SET051-SEC
           MOVE    RECE041-ZAINUM      TO  RECE051-ZAINUM
           START   RECE051-FILE
               KEY IS    >=   RECE051-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE051
               NOT INVALID
                   PERFORM 990-RECE051-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE051           =   1              )
               OR     ( RECE041-KEY1      NOT =   RECE051-KEY1   )
               OR     ( RECE041-ZAINUM    NOT =   RECE051-ZAINUM )
      *
               MOVE      ZERO      TO    FLG-MEISKY-OKIKAE
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   5     )
                         OR      ( RECE051-SRYCD(IDX) =   SPACE )
                 IF   RECE051-MEISKYFLG(IDX)  =  "2"
                   IF      RECE051-SRYYM  >=  "200804"
                    IF     RECE051-SETCOMATFLG(IDX) = ZERO
                      IF   (FLG-SETCOM              =  1   )
                      AND  (RECE051-RECEDENFLG(IDX) = SPACE)
                        ADD    1                 TO  WRK-HEN-SEQNO-X
                        IF   WRK-HEN-SEQNO-X  >  3
                          MOVE   SPACE   TO  RECE051-MEISKYFLG(IDX)
                          MOVE   1       TO  FLG-MEISKY-OKIKAE
                        ELSE
                          MOVE   WRK-HEN-RENNUM-X  TO  WRK-HEN-RENNUM
                          MOVE   WRK-HEN-SHORI-RENNUM-X
                                         TO  WRK-HEN-SHORI-RENNUM
                          MOVE   WRK-HEN-SEQNO-X   TO  WRK-HEN-SEQNO
                          PERFORM  26011-SETCOM-HENSYU3-SEC
                        END-IF
                        IF  (WRK-HEN-RENNUM-X = WRK-HEN-RENNUM-J )
                        AND (WRK-HEN-SHORI-RENNUM-X
                                        = WRK-HEN-SHORI-RENNUM-J )
                          ADD  1                 TO  WRK-HEN-SEQNO-J
                        END-IF
                        IF  (WRK-HEN-RENNUM-X = WRK-HEN-RENNUM-J2)
                        AND (WRK-HEN-SHORI-RENNUM-X
                                        = WRK-HEN-SHORI-RENNUM-J2)
                          ADD  1                 TO  WRK-HEN-SEQNO-J2
                        END-IF
                      END-IF
                    ELSE
                      EVALUATE   RECE051-SETCOMATFLG(IDX)
                        WHEN    1
                        WHEN    3  THRU   38
                        WHEN   41
                        WHEN   43  THRU   47
                        WHEN  100  THRU  110
                          ADD  1                 TO  WRK-HEN-SEQNO-J
                          IF   WRK-HEN-SEQNO-J  >  3
                            MOVE   SPACE   TO  RECE051-MEISKYFLG(IDX)
                            MOVE   1       TO  FLG-MEISKY-OKIKAE
                          ELSE
                            MOVE WRK-HEN-RENNUM-J  TO  WRK-HEN-RENNUM
                            MOVE WRK-HEN-SHORI-RENNUM-J
                                         TO  WRK-HEN-SHORI-RENNUM
                            MOVE WRK-HEN-SEQNO-J   TO  WRK-HEN-SEQNO
                            PERFORM  26011-SETCOM-HENSYU3-SEC
                          END-IF
                          IF  (WRK-HEN-RENNUM-X = WRK-HEN-RENNUM-J )
                          AND (WRK-HEN-SHORI-RENNUM-X
                                          = WRK-HEN-SHORI-RENNUM-J )
                            ADD    1             TO  WRK-HEN-SEQNO-X
                          END-IF
                        WHEN    2
                        WHEN   39  THRU   40
                        WHEN   42
                          ADD  1                 TO  WRK-HEN-SEQNO-J2
                          IF   WRK-HEN-SEQNO-J2 >  3
                            MOVE   SPACE   TO  RECE051-MEISKYFLG(IDX)
                            MOVE   1       TO  FLG-MEISKY-OKIKAE
                          ELSE
                            MOVE WRK-HEN-RENNUM-J2 TO  WRK-HEN-RENNUM
                            MOVE WRK-HEN-SHORI-RENNUM-J2
                                         TO  WRK-HEN-SHORI-RENNUM
                            MOVE WRK-HEN-SEQNO-J2  TO  WRK-HEN-SEQNO
                            PERFORM  26011-SETCOM-HENSYU3-SEC
                          END-IF
                          IF  (WRK-HEN-RENNUM-X = WRK-HEN-RENNUM-J2)
                          AND (WRK-HEN-SHORI-RENNUM-X
                                          = WRK-HEN-SHORI-RENNUM-J2)
                            ADD    1             TO  WRK-HEN-SEQNO-X
                          END-IF
                      END-EVALUATE
                    END-IF
                   ELSE
                    MOVE   SPACE   TO  RECE051-MEISKYFLG(IDX)
                    MOVE   1       TO  FLG-MEISKY-OKIKAE
                   END-IF
                 ELSE
                    IF   (RECE051-SRYCD(IDX)(1:1) = "1" OR "6" OR "7")
                    OR   (RECE051-SRYCD(IDX)(1:3) = "059"            )
                      MOVE   1               TO  FLG-SETCOM
                      MOVE   RECE051-RENNUM  TO  WRK-HEN-RENNUM-X
                      MOVE   IDX             TO  WRK-HEN-SHORI-RENNUM-X
                      MOVE   ZERO            TO  WRK-HEN-SEQNO-X
                    END-IF
      *
                    IF    RECE051-SRYCD(IDX) =
      *1
                          "113000410" OR "113000510" OR "180000110" OR
                          "180000210" OR "113023910" OR
      *3
                          "113001210" OR "113001310" OR "113002110" OR
      *4
                          "111011810" OR "111012610" OR "111012810" OR
      *5
                          "113006510" OR
      *6
                          "180000710" OR
      *7
                          "113004310" OR
      *8
                          "113008310" OR "113008410" OR "113008510" OR
                          "113031610" OR
      *9
                          "113008070" OR "113008170" OR
      *10
                          "113000810" OR "113009910" OR "113029410" OR
      *11
                          "114008010" OR
      *12
                          "114009410" OR
      *13
                          "114000110" OR "114001610" OR
      *14
                          "114001110" OR "114012910" OR "114018010" OR
                          "114030310" OR "114042110" OR "114042210" OR
                          "114042810" OR "114046310" OR
      *16
                          "180027410" OR "180027510" OR "180028010" OR
                          "180028110" OR "180027610" OR "180027710" OR
                          "180030810" OR "180027810" OR "180027910" OR
                          "180028210" OR "180028310" OR "180028410" OR
                          "180032410" OR "180032510" OR "180032610" OR
                          "180032710" OR "180033110" OR
                          "180033910" OR "180034010" OR "180034110" OR
                          "180034210" OR "180034310" OR "180034410" OR
                          "180034510" OR "180034610" OR "180034710" OR
                          "180044610" OR "180044710" OR "180044810" OR
                          "180045010" OR "180045210" OR "180045410" OR
                          "180045530" OR "180045630" OR "180045730" OR
                          "180043430" OR "180043530" OR "180043630" OR
                          "180043730" OR "180043830" OR "180043930" OR
                          "180044030" OR "180044130" OR "180044230" OR
                          "180050330" OR "180050430" OR "180050530" OR
                          "180050730" OR "180050930" OR "180051130" OR
                          "180051230" OR "180051330" OR "180051430" OR
                          "180052730" OR "180052830" OR "180052930" OR
                          "180053130" OR "180053330" OR "180053530" OR
                          "180053630" OR "180053730" OR "180053830" OR
                          "180051530" OR "180051630" OR "180051730" OR
                          "180051930" OR "180052130" OR "180052330" OR
                          "180052430" OR "180052530" OR "180052630" OR
      *17
                          "113001510" OR
      *18
                          "113003410" OR
      *19
                          "180033210" OR "180035910" OR "188006210" OR
                          "188007010" OR "180047810" OR "188010610" OR
                          "180056410" OR
                          "180042110" OR "180042410" OR "180048710" OR
                          "180048810" OR "180057210" OR "180057310" OR
                          "180057410" OR "180057510" OR "180057610" OR
                          "180057710" OR
                          "180057810" OR "180057910" OR
                          "180058010" OR "180058110" OR
                          "180064010" OR "180064110" OR
                          "114045510" OR
      *20
                          "700010000" OR "700030000" OR "700050000" OR
                          "700080000" OR "700110000" OR "700140000" OR
                          "700290000" OR "700310000" OR "700330000" OR
                          "700710000" OR "700730000" OR "700750000" OR
                          "700780000" OR "700810000" OR "700840000" OR
                          "700870000" OR "700160000" OR "700180000" OR
                          "700190000" OR "700200000" OR "700210000" OR
                          "700220000" OR "700230000" OR "700240000" OR
                          "700250000" OR "700260000" OR "700270000" OR
                          "700280000" OR "700590000" OR "700600000" OR
                          "700630000" OR "700640000" OR "700650000" OR
                          "700680000" OR "729780000" OR
      *21
                          "113014610" OR "113024310" OR "113027910" OR
                          "113028310" OR
      *22
                          "113014370" OR
      *23
                          "113013910" OR "113014010" OR "113014110" OR
      *24
                          "114020910" OR
      *25
                          "112015810" OR "112015950" OR "112016210" OR
                          "112016410" OR "112017010" OR "112017150" OR
                          "112017610" OR "112023550" OR "112023850" OR
      *26
                          "114009710" OR
      *27
                          "113015010" OR "113015130" OR "113018070" OR
                          "114045470" OR "114045670" OR "113030470" OR
                          "113030570" OR
      *29
                          "114027610" OR "114027710" OR "114027810" OR
                          "114027910" OR
      *32
                          "114006410" OR "114015010" OR
      *33
                          "180047910" OR "188010710" OR "180058710" OR
                          "188011810" OR
      *35
                          "140057510" OR "140057610" OR
      *36
                          "114044710" OR "114044810" OR "114044910" OR
      *37
                          "113024410" OR "113024510" OR
      *38
                          "112023210" OR "113023890" OR "114043970" OR
      *41
                          "140007710" OR "140008510" OR "140029850" OR
                          "140008210" OR "140039210" OR "140057810" OR
                          "140057910" OR "140058010" OR "140058110" OR
                          "140058210" OR "140058310" OR "140058410" OR
                          "140058510" OR "140058610" OR "140059310" OR
                          "140059410" OR "140059510" OR
                          "140060210" OR "140060310" OR "140060410" OR
                          "140060510" OR "140060610" OR "140060710" OR
                          "140060810" OR "140060910" OR "140061010" OR
      *43
                          "160000210" OR
      *44
                          "170016010" OR
      *45
                          "113030670" OR "113030770" OR
      *46
                          "113032810" OR
      *47
                          "180058650"
      *
                       MOVE   RECE051-RENNUM  TO  WRK-HEN-RENNUM-J
                       MOVE   IDX       TO  WRK-HEN-SHORI-RENNUM-J
                       MOVE   ZERO            TO  WRK-HEN-SEQNO-J
                    END-IF
      *
                    IF    RECE051-SRYCD(IDX) =
      *2
                          "113000670" OR "113000770" OR "113029170" OR
                          "113029270" OR "113029370" OR
      *39
                          "180058270" OR
      *40
                          "180056570" OR "188011270" OR
      *42
                          "140054270" OR "140054370" OR "140054470"
      *
                       MOVE   RECE051-RENNUM  TO  WRK-HEN-RENNUM-J2
                       MOVE   IDX       TO  WRK-HEN-SHORI-RENNUM-J2
                       MOVE   ZERO            TO  WRK-HEN-SEQNO-J2
                    END-IF
      *
      *             °åÎÅ¾ðÊóÏ¢·ÈÊÔ½¸
                    IF    RECE051-INGAIFLG(IDX)  =   "1"
                       MOVE   RECE051-RENNUM  TO  WRK-HEN-RENNUM-J3
                       MOVE   IDX       TO  WRK-HEN-SHORI-RENNUM-J3
                       MOVE   ZERO            TO  WRK-HEN-SEQNO-J3
      *
                       ADD    1         TO  WRK-HEN-SEQNO-J3
                       MOVE   WRK-HEN-RENNUM-J3
                                        TO  WRK-HEN-RENNUM
                       MOVE   WRK-HEN-SHORI-RENNUM-J3
                                        TO  WRK-HEN-SHORI-RENNUM
                       MOVE   WRK-HEN-SEQNO-J3
                                        TO  WRK-HEN-SEQNO
                       PERFORM  26011-SETCOM-HENSYU3-SEC
                       IF  (WRK-HEN-RENNUM-J3 = WRK-HEN-RENNUM-X)
                       AND (WRK-HEN-SHORI-RENNUM-J3
                                        = WRK-HEN-SHORI-RENNUM-X)
                         ADD  1         TO  WRK-HEN-SEQNO-X
                       END-IF
                    END-IF
                 END-IF
               END-PERFORM
      *
               IF    FLG-MEISKY-OKIKAE  =  1
                  REWRITE   RECE051-REC
                  ADD       1        TO   CNT-RECE05-RE
               END-IF
      *
               PERFORM 990-RECE051-NEXT-SEC
      *
           END-PERFORM
      *
           .
       2601-SETCOM-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´Ø·¸¥³¥á¥ó¥ÈÊÔ½¸£³
      *****************************************************************
       26011-SETCOM-HENSYU3-SEC     SECTION.
      *
           MOVE    SPACE                 TO  RECE055-REC
           INITIALIZE                        RECE055-REC
      *
           MOVE    RECE051-KEY           TO  RECE055-KEY
      *
           MOVE    WRK-HEN-RENNUM        TO  RECE055-RENNUM
           MOVE    WRK-HEN-SHORI-RENNUM  TO  RECE055-SHORI-RENNUM
           MOVE    WRK-HEN-SEQNO         TO  RECE055-SEQNO
      *
           IF    RECE051-INGAIFLG(IDX)  =   "1"
             IF   RECE051-GENERIC-KBN(IDX)  =  "1"
               MOVE  RECE051-YAKKAKJNCD(IDX)(1:9)
                                         TO  RECE055-SRYCD
             END-IF
      *      ¸å¤í¤ÎÁ´³Ñ¶õÇò¤òºï½ü¤¹¤ë
             INITIALIZE                      ORCSKANACHKAREA
             MOVE    "1"                 TO  KANACHK-SYORI
             MOVE    "1"                 TO  KANACHK-SYORIKBN
             MOVE    RECE051-GENERICNAME (IDX)
                                         TO  KANACHK-MAE-INPUT
             CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
             IF    ( KANACHK-RC          =   ZERO )
             AND   ( KANACHK-MAX         >   ZERO )
                 MOVE    SPACE           TO  RECE051-GENERICNAME (IDX)
                 MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                         TO  RECE051-GENERICNAME (IDX)
             END-IF
             MOVE    RECE051-GENERICNAME(IDX)
                                         TO  RECE055-NAME
                                             RECE055-INPUTCOMENT
           ELSE
             MOVE    RECE051-SRYCD(IDX)  TO  RECE055-SRYCD
      *      ¸å¤í¤ÎÁ´³Ñ¶õÇò¤òºï½ü¤¹¤ë
             INITIALIZE                      ORCSKANACHKAREA
             MOVE    "1"                 TO  KANACHK-SYORI
             MOVE    "1"                 TO  KANACHK-SYORIKBN
             MOVE    RECE051-NAME (IDX)  TO  KANACHK-MAE-INPUT
             CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
             IF    ( KANACHK-RC          =   ZERO )
             AND   ( KANACHK-MAX         >   ZERO )
                 MOVE    SPACE           TO  RECE051-NAME (IDX)
                 MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                         TO  RECE051-NAME (IDX)
             END-IF
             MOVE    RECE051-NAME(IDX)   TO  RECE055-NAME
      *      ¸å¤í¤ÎÁ´³Ñ¶õÇò¤òºï½ü¤¹¤ë
             INITIALIZE                      ORCSKANACHKAREA
             MOVE    "1"                 TO  KANACHK-SYORI
             MOVE    "1"                 TO  KANACHK-SYORIKBN
             MOVE    RECE051-INPUTCOMENT (IDX)
                                         TO  KANACHK-MAE-INPUT
             CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
             IF    ( KANACHK-RC          =   ZERO )
             AND   ( KANACHK-MAX         >   ZERO )
                 MOVE    SPACE           TO  RECE051-INPUTCOMENT (IDX)
                 MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                         TO  RECE051-INPUTCOMENT (IDX)
             END-IF
             MOVE    RECE051-INPUTCOMENT(IDX)
                                         TO  RECE055-INPUTCOMENT
           END-IF
      *
           WRITE   RECE055-REC
           ADD     1                     TO  CNT-RECE055
      *
           .
       26011-SETCOM-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¨¥é¡¼½ÐÎÏ½èÍý
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
               IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            ¥¸¥ç¥Ö´ÉÍý½ªÎ»½èÍý
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¨¥é¡¼»þ½ªÎ»½èÍý
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥¨¥é¡¼½èÍý
      *****************************************************************
       500-FILE-ERR-ABORT-SEC           SECTION.
      *
           EVALUATE    WRK-ERR-FLG
               WHEN    1
                   MOVE    "¥Õ¥¡¥¤¥ë£Ï£Ð£Å£Î¥¨¥é¡¼¡¡FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file open err" TO  WRK-ERR-FILE-MSG1
               WHEN    2
                   MOVE    "¥Õ¥¡¥¤¥ëÆÉ¤ß¹þ¤ß¥¨¥é¡¼¡¡FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file read err" TO  WRK-ERR-FILE-MSG1
               WHEN    3
                   MOVE    "¥Õ¥¡¥¤¥ë½ñ¤­¹þ¤ß¥¨¥é¡¼¡¡FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file write err"
                                           TO  WRK-ERR-FILE-MSG1
               WHEN    4
                   MOVE    "¥Õ¥¡¥¤¥ë¹¹¿·¥¨¥é¡¼¡¡FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file rewrite err"
                                           TO  WRK-ERR-FILE-MSG1
           END-EVALUATE
      *
           STRING  WRK-ERR-FILE-MSG        DELIMITED   BY  SPACE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   " STS="                 DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
           END-STRING
           PERFORM 500-ERR-HENSYU-SEC
      *
           STRING  "ORCR0040 "             DELIMITED   BY  SIZE
                   WRK-ERR-FILE-MSG1       DELIMITED   BY  SIZE
                   " FILE="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   "  STS="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                   LOW-VALUE               DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
           END-STRING
           PERFORM 500-COBABORT-SEC
      *
           .
       500-FILE-ERR-ABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½èÍýÃæ»ß½èÍý
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     =   1    )
                   MOVE    "½èÍý¤¬Ãæ»ß¤µ¤ì¤Þ¤·¤¿"
                                           TO  WRK-RECEERR
      *
                   OPEN    INPUT   RECEERR-FILE
                   IF      STS-RECEERR         =   ZERO
                       CLOSE   RECEERR-FILE
                   ELSE
                       OPEN    OUTPUT              RECEERR-FILE
      *    
                       MOVE    WRK-RECEERR         TO  RECEERR-REC
                       WRITE   RECEERR-REC
                       CLOSE   RECEERR-FILE
      *         
      *                ¥¸¥ç¥Ö´ÉÍý½ªÎ»½èÍý
                       MOVE    "CAN"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
grpsys                 PERFORM 900-CALL-ORCSJOB-SEC
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ªÎ»¡¡¡¡½èÍý
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ¥Õ¥¡¥¤¥ë£Ã£Ì£Ï£Ó£Å½èÍý
           PERFORM 310-FILE-CLOSE-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ¥¹¥Æ¥Ã¥×´ÉÍý½ªÎ»½èÍý
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE02  CNT-IN  :" CNT-RECE02 
           DISPLAY "RECE049 CNT-WK  :" CNT-RECE049
           DISPLAY "RECE059 CNT-WK  :" CNT-RECE059
           DISPLAY "RECE04  CNT-OUT :" CNT-RECE04 
           DISPLAY "RECE05  CNT-OUT :" CNT-RECE05 
           DISPLAY "RECE04  REWRITE :" CNT-RECE04-RE
           DISPLAY "RECE05  REWRITE :" CNT-RECE05-RE
           DISPLAY "RECE055 CNT-OUT :" CNT-RECE055
           DISPLAY "*** ORCR0040 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë£Ã£Ì£Ï£Ó£Å½èÍý
      *****************************************************************
       310-FILE-CLOSE-SEC              SECTION.
      *
           CLOSE                       RECE02-FILE
      *
           CLOSE                       RECE04-FILE
      *
           CLOSE                       RECE05-FILE
      *
           CLOSE                       RECE041-FILE     
           CLOSE                       RECE051-FILE     
      *
           CLOSE                       RECE049-FILE
           CLOSE                       RECE059-FILE
      *
           CLOSE                       RECE055-FILE
      *
           .
       310-FILE-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    °ì»þ¥Õ¥¡¥¤¥ëÌ¾¼èÆÀ½èÍý
      *****************************************************************
       800-FILENM-SET-SEC        SECTION.
      *
           MOVE    WRK-PARA-TERMID     TO  RECEDAT1PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEDAT1PARA-HOSPNUM
           .
       800-FILENM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£²¥Ç¡¼¥¿ÊÔ½¸½èÍý¡Ê£°£´¡Ë
      *****************************************************************
       800-RECE02-SET04-SEC           SECTION.
      *
           INITIALIZE                      RECE04-REC
           MOVE    RECE02-KEY          TO  RECE04-KEY1
      *
           .
       800-RECE02-SET04-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£²¥Ç¡¼¥¿ÊÔ½¸½èÍý¡Ê£°£´£¹¡Ë
      *****************************************************************
       800-RECE02-SET049-SEC           SECTION.
      *
           INITIALIZE                      RECE049-REC
           MOVE    RECE02-KEY          TO  RECE049-KEY1
      *
           .
       800-RECE02-SET049-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£²¥Ç¡¼¥¿ÊÔ½¸½èÍý¡Ê£°£´£±¡Ë
      *****************************************************************
       800-RECE02-SET041-SEC           SECTION.
      *
           INITIALIZE                      RECE041-REC
           MOVE    RECE02-KEY          TO  RECE041-KEY1
      *
           .
       800-RECE02-SET041-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£²¥Ç¡¼¥¿ÊÔ½¸½èÍý¡Ê£°£µ£¹¡Ë
      *****************************************************************
       800-RECE02-SET059-SEC           SECTION.
      *
           INITIALIZE                       RECE059-REC
           MOVE    RECE02-KEY           TO  RECE059-KEY1
      *
           .
       800-RECE02-SET059-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£´¥Ç¡¼¥¿ÊÔ½¸½èÍý¡Ê£°£µ¡Ë
      *****************************************************************
       800-RECE04-SET05-SEC           SECTION.
      *
           INITIALIZE                      RECE05-REC
           MOVE    RECE04-KEY1         TO  RECE05-KEY1
      *
           .
       800-RECE04-SET05-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£´£¹¥Ç¡¼¥¿ÊÔ½¸½èÍý¡Ê£°£µ¡Ë
      *****************************************************************
       800-RECE049-SET05-SEC           SECTION.
      *
           INITIALIZE                       RECE05-REC
           MOVE    RECE049-KEY1         TO  RECE05-KEY1
      *
           .
       800-RECE049-SET05-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£´£¹¥Ç¡¼¥¿ÊÔ½¸½èÍý¡Ê£°£µ£¹¡Ë
      *****************************************************************
       800-RECE049-SET059-SEC           SECTION.
      *
           INITIALIZE                       RECE059-REC
           MOVE    RECE049-KEY1         TO  RECE059-KEY1
      *
           .
       800-RECE049-SET059-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£´£±¥Ç¡¼¥¿ÊÔ½¸½èÍý¡Ê£°£µ£±¡Ë
      *****************************************************************
       800-RECE041-SET051-SEC           SECTION.
      *
           INITIALIZE                       RECE051-REC
           MOVE    RECE041-KEY1         TO  RECE051-KEY1
      *
           .
       800-RECE041-SET051-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£²¥Ç¡¼¥¿½ç¸¡º÷½èÍý
      *****************************************************************
       990-RECE02-READ-SEC           SECTION.
      *
           READ    RECE02-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
               NOT AT  END
                   ADD     1           TO  CNT-RECE02
                   ADD     1           TO  CNT-IN-COMMIT
                                           CNT-IN-CANCEL
      *
                   DISPLAY "RECE02  PTNUM=" RECE02-PTNUM
           END-READ
      *
           .
       990-RECE02-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£°£´¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE04-NEXT-SEC           SECTION.
      *
           READ    RECE04-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE04
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE04
           END-READ
      *
           .
       990-RECE04-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£°£´£¹¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE049-NEXT-SEC           SECTION.
      *
           READ    RECE049-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE049
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE049
           END-READ
      *
           .
       990-RECE049-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£°£´£±¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE041-NEXT-SEC           SECTION.
      *
           READ    RECE041-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE041
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE041
           END-READ
      *
           .
       990-RECE041-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£°£µ¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE05-NEXT-SEC           SECTION.
      *
           READ    RECE05-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE05
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE05
           END-READ
      *
           .
       990-RECE05-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£°£µ£¹¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE059-NEXT-SEC           SECTION.
      *
           READ    RECE059-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE059
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE059
           END-READ
      *
           .
       990-RECE059-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£°£µ£±¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE051-NEXT-SEC           SECTION.
      *
           READ    RECE051-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE051
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE051
           END-READ
      *
           .
       990-RECE051-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    »»ÄêÍúÎò¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       900-SANTEI-INV-SEC             SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SANTEI-REC
                   MOVE    ZERO                TO  FLG-SANTEI
               ELSE
                   MOVE    1                   TO  FLG-SANTEI
                   INITIALIZE                      SANTEI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SANTEI
               INITIALIZE                      SANTEI-REC
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SANTEI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    »»ÄêÎò¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       900-SANTEI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       900-SANTEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼õ¿ÇÍúÎòÆÉ¹þ
      *****************************************************************
       900-JYURRK-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       900-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°ÙÆÉ¹þ
      *****************************************************************
       900-SRYACT-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    »»ÄêÍúÎòÉÕ²ÃÆÉ¹þ
      *****************************************************************
       900-SANTEIPLUS-READ-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SANTEIPLUS-REC
               MOVE    ZERO            TO  FLG-SANTEIPLUS
           ELSE
               INITIALIZE                  SANTEIPLUS-REC
               MOVE    1               TO  FLG-SANTEIPLUS
           END-IF
      *
           .
       900-SANTEIPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥Èµ­ºÜ
      *****************************************************************
       900-RKISAI-READ-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  RKISAI-REC
               MOVE    ZERO            TO  FLG-RKISAI
           ELSE
               INITIALIZE                  RKISAI-REC
               MOVE    1               TO  FLG-RKISAI
           END-IF
      *
           .
       900-RKISAI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥³¥á¥ó¥È¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       900-PTCOM-READ-SEC           SECTION.
      *
           MOVE     PTCOM-REC          TO  MCPDATA-REC
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   MOVE    1                   TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Æ¡¼¥Ö¥ë¤Î¸¡º÷
      *****************************************************************
       900-TENSU-TBL-SEC               SECTION.
      *
           INITIALIZE                           TNS-TENSU-REC
           MOVE    WRK-TENSU-SRYCD      TO      TNS-SRYCD
           MOVE    WRK-TENSU-SRYYM      TO      TNS-YUKOSTYMD(1:6)
           MOVE    "01"                 TO      TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD        TO      TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM          TO      TNS-HOSPNUM
           MOVE    TNS-TENSU-REC        TO      MCPDATA-REC
           MOVE    "tbl_tensu"          TO      MCP-TABLE
           MOVE    "key"                TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_tensu"      TO      MCP-TABLE
               MOVE    "key"            TO      MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC           =       ZERO
                   MOVE    MCPDATA-REC  TO      TNS-TENSU-REC
                   MOVE    ZERO         TO      FLG-TENSU
               ELSE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE    1            TO      FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                       TNS-TENSU-REC
               MOVE    1                TO      FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"          TO      MCP-TABLE
           MOVE    "key"                TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       900-TENSU-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¸¥ç¥Ö´ÉÍý£Ä£ÂÀ©¸æ½èÍý
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â£Ó£Å£Ì£Å£Ã£Ô½èÍý
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â£Æ£Å£Ô£Ã£È½èÍý
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¥¢¥¯¥»¥¹½èÍý
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
      *****************************************************************
      *    £Ä£Â¥ª¡¼¥×¥ó½èÍý
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¡¡£Ã£Ï£Í£Í£É£Ô½èÍý
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *      
      *****************************************************************
      *    £Ä£Â¡¡£Ó£Ô£Á£Ò£Ô½èÍý
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
           EXIT.
      *
