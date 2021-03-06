      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
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
       PROGRAM-ID.             ORCBI202.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �������ԾȲ�
      *  ����ݡ��ͥ��̾  : ���ﴵ�Կ�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  06/10/31    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����       ����
      *  03.05.00    NACL-����    07.05.23 ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    ���������ΰ�
           COPY    "CPSHELLTBL.INC".
      *
      *    ����ǥå����ΰ�
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXB                    PIC 9(05).
           03  IDXC                    PIC 9(05).
           03  IDXD                    PIC 9(05).
           03  IDXT                    PIC 9(05).
           03  IDXZ                    PIC 9(05).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
       01  SEL-AREA.
           03  SEL-UPYMD               PIC X(08).
           03  SEL-UPHMS               PIC X(06).
           03  SEL-STSRYYMD            PIC X(08).
           03  SEL-EDSRYYMD            PIC X(08).
      *
       01  WRK-AREA.
           03  WRK-BTUNUM-G.
               05  WRK-BTUNUM          PIC 9(02).
      *
       01  TBTUINF-AREA.
           03  TBTUINF-OCC             OCCURS 2.
               05  TBTUINF-BTUNUM      PIC 9(02).
               05  TBTUINF-STS-ST      PIC 9(01).
               05  TBTUINF-STS-NYUIN   PIC 9(01).
               05  TBTUINF-STS-TENNYU  PIC 9(01).
               05  TBTUINF-STS-TENSTU  PIC 9(01).
               05  TBTUINF-STS-TAIIN   PIC 9(01).
               05  TBTUINF-STS-ED      PIC 9(01).
               05  TBTUINF-STS-COUNT   PIC 9(01).
      *
       01  BTUSUM-AREA.
         02  BTUSUM-OCC                OCCURS  100.
           COPY    "CPBTUSUM.INC".
      *
       01  COSNT-AREA.
           03  CONST-BTUSUM-MAX        PIC 9(05) VALUE 99.
           03  CONST-BTUSUM-TTL        PIC 9(05) VALUE 100.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ����
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK5001.INC".
      *
      *    ���ԾȲ�
       01  PTSRH-REC.
           COPY    "CPPTSRH.INC".
      *
      *    ���Ծ���
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    �����ֹ�
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    ��������
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ���������Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ������ñ�������������
           COPY    "CPORCSNDAYINF.INC".
      *
      *    �ģ¸���
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      *
      ******************************************************************
       PROCEDURE                       DIVISION
                                       USING     COMMAND-PARAM.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           MOVE    ZERO            TO  RETURN-CODE
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       WRK-AREA
                                       SEL-AREA
                                       SPA-AREA
                                       BTUSUM-AREA
      *
           PERFORM 800-MCNDATE-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    SPA-TERMID
                                               SPA-HOSPNUM
                                               SEL-UPYMD
                                               SEL-UPHMS
                                               SEL-STSRYYMD
                                               SEL-EDSRYYMD
           END-UNSTRING
      *
      *    �ģ¥����ץ�
           PERFORM 120-DBOPEN-SEC
      *
           PERFORM 1001-INIT-SEC
      *
           PERFORM 900-PTSRH-DEL2-SEC
           PERFORM 910-DBCOMMIT-SEC
           PERFORM 910-DBSTART-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       1001-INIT-SEC               SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5001"              TO  SYS-KANRICD
           MOVE    SEL-EDSRYYMD        TO  SYS-STYUKYMD
           MOVE    SEL-STSRYYMD        TO  SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    SYS-KBNCD       TO  WRK-BTUNUM-G
               PERFORM 2002-BTUNUM-CHK-SEC
               IF    ( WRK-BTUNUM      NOT =   ZERO )
                   MOVE    WRK-BTUNUM  TO  BTUSUM-BTUNUM (WRK-BTUNUM)
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1001-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 900-PTSRH-DEL3-SEC
      *
           PERFORM 2001-BTUSUM-EDIT-SEC
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    "b"             TO  PTSRH-DATAKBN
           MOVE    SEL-UPYMD       TO  PTSRH-CREYMD
                                       PTSRH-UPYMD
           MOVE    SEL-UPHMS       TO  PTSRH-UPHMS
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   CONST-BTUSUM-MAX )
      *
               IF    ( BTUSUM-BTUNUM (IDXA)    NOT =   ZERO )
      *
                   COMPUTE PTSRH-TOTALTEN
                       =   PTSRH-TOTALTEN  +   1
      *
                   MOVE    BTUSUM-OCC (IDXA)
                                           TO  PTSRH-MEMO
      *
                   MOVE    PTSRH-REC       TO  MCPDATA-REC
                   MOVE    "tbl_ptsrh"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
      *
                   PERFORM 2001-TOTAL-EDIT-SEC
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( PTSRH-TOTALTEN  >   1 )
      *
               COMPUTE PTSRH-TOTALTEN
                   =   PTSRH-TOTALTEN  +   1
      *
               MOVE    BTUSUM-OCC (CONST-BTUSUM-TTL)
                                   TO  PTSRH-MEMO
      *
               MOVE    PTSRH-REC   TO  MCPDATA-REC
               MOVE    "tbl_ptsrh" TO  MCP-TABLE
               MOVE    "key"       TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
           END-IF
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    "a"             TO  PTSRH-DATAKBN
           MOVE    SEL-UPYMD       TO  PTSRH-CREYMD
                                       PTSRH-UPYMD
           MOVE    SEL-UPHMS       TO  PTSRH-UPHMS
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ﴵ�Կ����׽���
      *****************************************************************
       2001-BTUSUM-EDIT-SEC          SECTION.
      *
           DISPLAY "CHECK:,BTUNUM,PTNUM,RRKNUM,SRYYMD,"
                   "START,NYUIN,TENNYU,TENSTU,TAIIN,END,COUNT"
      *
           PERFORM 900-PTNYUINRRK-KEY26-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
      *
               PERFORM 900-PTINF-KEY-SEL-SEC
               IF    ( FLG-PTINF           =   ZERO )
                   IF    ( PTINF-TSTPTNUMKBN   <>  "1" )
                       PERFORM 20011-BTUSUM-EDIT-SEC
                   END-IF
               END-IF
      *
               PERFORM 900-PTNYUINRRK-KEY26-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-BTUSUM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ���ﴵ�Կ����׽���
      *****************************************************************
       20011-BTUSUM-EDIT-SEC             SECTION.
      *
           PERFORM 900-PTNUM-KEY-SEL-SEC
      *
           INITIALIZE              SNDAYINF-AREA
           MOVE    SEL-STSRYYMD
                               TO  SNDAYINF-STSRYYMD
           MOVE    SEL-EDSRYYMD
                               TO  SNDAYINF-EDSRYYMD
           CALL    "ORCSNDAYINF"   USING
                                   SNDAYINF-AREA
                                   PTNYUINRRK-REC
                                   SPA-AREA
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   SNDAYINF-DAYS-MAX )
      *
               IF    ( SNDAYINF-ZAIINKBN (IDXD)    =   "1" )
                   PERFORM 200111-BTUSUM-EDIT-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       20011-BTUSUM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ���ﴵ�Կ����׽���
      *****************************************************************
       200111-BTUSUM-EDIT-SEC            SECTION.
      *
           MOVE    ZERO            TO  IDXA
                                       IDXB
                                       IDXZ
           INITIALIZE                  TBTUINF-AREA
      *
           MOVE    SNDAYINF-BTUNUM (IDXD)  TO  WRK-BTUNUM-G
           PERFORM 2002-BTUNUM-CHK-SEC
           MOVE    WRK-BTUNUM      TO  IDXA
      *
           MOVE    SNDAYINF-BTUNUM-SAKI (IDXD)
                                           TO  WRK-BTUNUM-G
           PERFORM 2002-BTUNUM-CHK-SEC
           MOVE    WRK-BTUNUM      TO  IDXB
      *
           IF    ( IDXA        NOT =   ZERO )
      *
               MOVE    IDXA        TO  TBTUINF-BTUNUM (1)
      *
               MOVE    1           TO  IDXZ
      *
               IF    ( SNDAYINF-NYUINKBN (IDXD)    NOT =   SPACE )
                   MOVE    1       TO  TBTUINF-STS-NYUIN (1)
               ELSE
                   IF    ( IDXD        =   1 )
                       MOVE    1   TO  TBTUINF-STS-ST (1)
                   END-IF
               END-IF
      *
               IF    ( IDXB    NOT =   ZERO )
      *
                   MOVE    IDXB        TO  TBTUINF-BTUNUM (2)
      *
                   MOVE    2           TO  IDXZ
      *
                   MOVE    1           TO  TBTUINF-STS-TENNYU (2)
                   MOVE    1           TO  TBTUINF-STS-TENSTU (1)
               END-IF
      *
               MOVE    1           TO  TBTUINF-STS-COUNT (IDXZ)
      *
               IF    ( SNDAYINF-TAIINKBN (IDXD)    NOT =   SPACE )
                       MOVE    1       TO  TBTUINF-STS-TAIIN (IDXZ)
               ELSE
                   IF    ( IDXD        =   SNDAYINF-DAYS-MAX )
                       MOVE    1       TO  TBTUINF-STS-ED (IDXZ)
                   END-IF
               END-IF
           END-IF
      *
           IF    ( TBTUINF-BTUNUM (1)  NOT =   ZERO )
               MOVE    1               TO  IDXZ
               PERFORM 2001111-BTUSUM-EDIT-SEC
           END-IF
      *
           IF    ( TBTUINF-BTUNUM (2)  NOT =   ZERO )
               MOVE    2               TO  IDXZ
               PERFORM 2001111-BTUSUM-EDIT-SEC
           END-IF
      *
           .
       200111-BTUSUM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ���ﴵ�Կ����׽���
      *****************************************************************
       2001111-BTUSUM-EDIT-SEC         SECTION.
      *
           DISPLAY "CHECK:,"
                   TBTUINF-BTUNUM (IDXZ)    ","
                   PTNUM-PTNUM              ","
                   PTNYUINRRK-RRKNUM        ","
                   SNDAYINF-SRYYMD   (IDXD) ","
                   TBTUINF-STS-ST    (IDXZ) ","
                   TBTUINF-STS-NYUIN (IDXZ) ","
                   TBTUINF-STS-TENNYU(IDXZ) ","
                   TBTUINF-STS-TENSTU(IDXZ) ","
                   TBTUINF-STS-TAIIN (IDXZ) ","
                   TBTUINF-STS-ED    (IDXZ) ","
                   TBTUINF-STS-COUNT (IDXZ)
      *
           MOVE    TBTUINF-BTUNUM (IDXZ)   TO  IDXC
           MOVE    IDXC                    TO  BTUSUM-BTUNUM (IDXC)
      *
           IF    ( TBTUINF-STS-ST    (IDXZ)    =   1 )
               COMPUTE BTUSUM-ST (IDXC)
                   =   BTUSUM-ST (IDXC)  +   1
           END-IF
      *
           IF    ( TBTUINF-STS-NYUIN (IDXZ)    =   1 )
               COMPUTE BTUSUM-NYUIN (IDXC)
                   =   BTUSUM-NYUIN (IDXC)  +   1
           END-IF
      *
           IF    ( TBTUINF-STS-TENNYU (IDXZ)    =   1 )
               COMPUTE BTUSUM-TENNYU (IDXC)
                   =   BTUSUM-TENNYU (IDXC)  +   1
           END-IF
      *
           IF    ( TBTUINF-STS-TENSTU (IDXZ)    =   1 )
               COMPUTE BTUSUM-TENSTU (IDXC)
                   =   BTUSUM-TENSTU (IDXC)  +   1
           END-IF
      *
           IF    ( TBTUINF-STS-TAIIN (IDXZ)  =   1 )
               COMPUTE BTUSUM-TAIIN (IDXC)
                   =   BTUSUM-TAIIN (IDXC) +   1
           END-IF
      *
           IF    ( TBTUINF-STS-ED (IDXZ)  =   1 )
               COMPUTE BTUSUM-ED (IDXC)
                   =   BTUSUM-ED (IDXC) +   1
           END-IF
      *
           IF    ( TBTUINF-STS-COUNT (IDXZ)  =   1 )
               COMPUTE BTUSUM-TOTAL (IDXC)
                   =   BTUSUM-TOTAL (IDXC) +   1
           END-IF
      *
           .
       2001111-BTUSUM-EDIT-EXT.
           EXIT.






      *****************************************************************
      *    ���ﴵ�Կ����׽���
      *****************************************************************
       200112-BTUSUM-EDIT-SEC            SECTION.
      *
           MOVE    ZERO            TO  IDXA
                                       IDXB
                                       IDXZ
      *
           MOVE    SNDAYINF-BTUNUM (IDXD)  TO  WRK-BTUNUM-G
           PERFORM 2002-BTUNUM-CHK-SEC
           MOVE    WRK-BTUNUM      TO  IDXA
      *
           MOVE    SNDAYINF-BTUNUM-SAKI (IDXD)
                                           TO  WRK-BTUNUM-G
           PERFORM 2002-BTUNUM-CHK-SEC
           MOVE    WRK-BTUNUM      TO  IDXB
      *
           IF    ( IDXA        NOT =   ZERO )
      *
               MOVE    IDXA        TO  BTUSUM-BTUNUM (IDXA)
      *
               MOVE    IDXA        TO  IDXZ
      *
               IF    ( SNDAYINF-NYUINKBN (IDXD)    NOT =   SPACE )
                   COMPUTE BTUSUM-NYUIN (IDXA)
                       =   BTUSUM-NYUIN (IDXA)   +   1
               ELSE
                   IF    ( IDXD        =   1 )
                       COMPUTE BTUSUM-ST (IDXA)
                           =   BTUSUM-ST (IDXA)  +   1
                   END-IF
               END-IF
      *
               IF    ( IDXB    NOT =   ZERO )
      *
                   MOVE    IDXB    TO  IDXZ
      *
                   COMPUTE BTUSUM-TENNYU (IDXB)
                       =   BTUSUM-TENNYU (IDXB)  +   1
                   COMPUTE BTUSUM-TENSTU (IDXA)
                       =   BTUSUM-TENSTU (IDXA)  +   1
               END-IF
      *
               COMPUTE BTUSUM-TOTAL (IDXZ)
                   =   BTUSUM-TOTAL (IDXZ)   +   1
      *
               IF    ( SNDAYINF-TAIINKBN (IDXD)    NOT =   SPACE )
                   COMPUTE BTUSUM-TAIIN (IDXZ)
                       =   BTUSUM-TAIIN (IDXZ)   +   1
               ELSE
                   IF    ( IDXD        =   SNDAYINF-DAYS-MAX )
                       COMPUTE BTUSUM-ED (IDXZ)
                           =   BTUSUM-ED (IDXZ)  +   1
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       200112-BTUSUM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    �����ֹ�����å�����
      *****************************************************************
       2002-BTUNUM-CHK-SEC             SECTION.
      *
           IF    ( WRK-BTUNUM-G    IS    NUMERIC )
               IF   (  WRK-BTUNUM  >=  1 )
                AND (  WRK-BTUNUM  <=  CONST-BTUSUM-MAX )
                   CONTINUE
               ELSE
                   MOVE    ZERO    TO    WRK-BTUNUM
               END-IF
           ELSE
                   MOVE    ZERO    TO    WRK-BTUNUM
           END-IF
      *
           .
       2002-BTUNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ���Կ���׽��׽���
      *****************************************************************
       2001-TOTAL-EDIT-SEC             SECTION.
      *
           MOVE    CONST-BTUSUM-TTL    TO  IDXT
      *
           MOVE    "1"                 TO  BTUSUM-TTLKBN (IDXT)
      *
           COMPUTE BTUSUM-ST (IDXT)
               =   BTUSUM-ST (IDXT)
               +   BTUSUM-ST (IDXA)
      *
           COMPUTE BTUSUM-NYUIN (IDXT)
               =   BTUSUM-NYUIN (IDXT)
               +   BTUSUM-NYUIN (IDXA)
      *
           COMPUTE BTUSUM-TENNYU (IDXT)
               =   BTUSUM-TENNYU (IDXT)
               +   BTUSUM-TENNYU (IDXA)
      *
           COMPUTE BTUSUM-TENSTU (IDXT)
               =   BTUSUM-TENSTU (IDXT)
               +   BTUSUM-TENSTU (IDXA)
      *
           COMPUTE BTUSUM-TAIIN (IDXT)
               =   BTUSUM-TAIIN (IDXT)
               +   BTUSUM-TAIIN (IDXA)
      *
           COMPUTE BTUSUM-ED (IDXT)
               =   BTUSUM-ED (IDXT)
               +   BTUSUM-ED (IDXA)
      *
           COMPUTE BTUSUM-TOTAL (IDXT)
               =   BTUSUM-TOTAL (IDXT)
               +   BTUSUM-TOTAL (IDXA)
      *
           .
       2001-TOTAL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ��λ��������
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ޥ������ռ�������
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    ���ԾȲ�������
      *****************************************************************
       900-PTSRH-DEL3-SEC              SECTION.
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    "a"             TO  PTSRH-VSTR(1)
           MOVE    "b"             TO  PTSRH-VSTR(2)
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "del5"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-PTSRH-DEL3-EXT.
           EXIT.
      *****************************************************************
      *    ���ԾȲ�������
      *****************************************************************
       900-PTSRH-DEL2-SEC              SECTION.
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM         TO  PTSRH-HOSPNUM
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SEL-UPYMD           TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -3                  TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN TO  PTSRH-CREYMD
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "del2"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-PTSRH-DEL2-EXT.
           EXIT.
      *****************************************************************
      *    �������򸡺�����
      *****************************************************************
       900-PTNYUINRRK-KEY26-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE                      PTNYUINRRK-REC
      *
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SEL-EDSRYYMD        TO  PTNYUINRRK-NYUINYMD
           MOVE    SEL-STSRYYMD        TO  PTNYUINRRK-TAIINYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           .
       900-PTNYUINRRK-KEY26-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ��������FETCH����
      *****************************************************************
       900-PTNYUINRRK-KEY26-FET-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           .
       900-PTNYUINRRK-KEY26-FET-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ������������
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ����FETCH����
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    ���Ծ����������
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
           INITIALIZE                      PTINF-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO  PTINF-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �����ֹ��������
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
           INITIALIZE                      PTNUM-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO  PTNUM-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               MOVE    1               TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¡���³����
      *****************************************************************
       120-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       120-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¡����ǽ���
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¸���������FHETCH��Ԥ�)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸���������FHETCH�ϹԤ�ʤ�)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£ģţ̣ţԣŽ���
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£ɣΣӣţңԽ���
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£գУģ��ԣŽ���
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£ãϣͣͣɣԽ���
      *****************************************************************
       910-DBCOMMIT-SEC                SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCOMMIT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£ӣԣ��ңԽ���
      *****************************************************************
       910-DBSTART-SEC                SECTION.
      *
           MOVE    "DBSTART"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBSTART-EXT.
           EXIT.
