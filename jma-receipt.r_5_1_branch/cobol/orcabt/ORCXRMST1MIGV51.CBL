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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCXRMST1MIGV51.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �ޥ�����������
      *  ����ݡ��ͥ��̾  : �����ե������������
      *                    : 
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  02/02/22    MCC-¿��Ǽ    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-¿��Ǽ  02/09/18  �С������Ƚ���ѹ�
      *  01.00.02    NACL-��ƣ    05/04/05  �ޥ���ñ�̤��ѹ�
      *  01.00.03    NACL-��ƣ    06/05/26  MONFUNC�б�
      *  05.01.00    NACL-��ƣ    17/08/28  V510�ޥ�����ʬΥ�б�������
      *                                     �оݥޥ���
      *                                       00:����
      *                                       04:�����å�
      *                                       05:����Ϸ���ִ�
      *                                       10:����
      *                                     �оݥơ��֥�
      *                                       tbl_mstkanri_org(�׻�)
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ���ϡ��ѥ�᡼���ե�����
           SELECT  IN-FILE         ASSIGN  ASS-IN
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-IN.
      *    ���ϡ������ե������ɸ���
           SELECT  OUT1-FILE       ASSIGN  ASS-OUT1
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-OUT1.
      *    ���ϡ������ե�����ʰܹ�����
           SELECT  OUT2-FILE       ASSIGN  ASS-OUT2
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-OUT2.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ���ϥե�����
       FD  IN-FILE.
       01  IN-REC.
           03  IN-MASTER-KBN           PIC X(02).
           03  IN-MASTER-DELI-1        PIC X(01).
           03  IN-DBRVERVION           PIC X(21).
           03  IN-DBRNAME              PIC X(60).
           03  IN-UPFILE               PIC X(100).
           03  IN-UPPG                 PIC X(10).
           03  IN-MASTER-DELI-2        PIC X(01).
           03  IN-SCHEMA               PIC X(01).
      *
      *    �����ե�����
       FD  OUT1-FILE.
       01  OUT1-REC                    PIC X(200).
      *
       FD  OUT2-FILE.
       01  OUT2-REC                    PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "COMMON-SPA".
      *
      *    �ե���������ΰ�
       01  ASS-AREA.
           03  ASS-IN                  PIC X(256).
           03  ASS-OUT1                PIC X(256).
           03  ASS-OUT2                PIC X(256).
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-IN                  PIC X(02).
           03  STS-OUT1                PIC X(02).
           03  STS-OUT2                PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-IN                  PIC 9(01).
           03  FLG-DBKANRI             PIC 9(01).
           03  FLG-MSTKANRI            PIC 9(01).
           03  FLG-OUTFILE             PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-OUT1                PIC 9(06).
           03  CNT-OUT2                PIC 9(06).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
      *
       01  OUT-REC.
           03  OUT-MASTER-KBN          PIC X(02).
           03  OUT-MASTER-DELI         PIC X(01).
           03  OUT-DBRVERVION          PIC X(21).
           03  OUT-UPPG                PIC X(10).
           03  OUT-UPFILE              PIC X(100).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-DBRVERSION          PIC X(64).
           03  WRK-DBRVERSION-ORG      PIC X(64).
      *
           03  WRK-DBKANRI-VERSION     PIC X(64).
      *
           03  WRK-IN                  PIC X(01). 
      *
           03  WRK-TBL-DBRVERSION   OCCURS 100   INDEXED DBR-IDX.
               05  WRK-TBL-MASTER-KBN  PIC X(02).
               05  WRK-TBL-DBRVERSION1 PIC X(64).
               05  WRK-TBL-UPDATE-FLG  PIC X(01).
      *
           03  WRK-ORG-DBRVERSION   OCCURS 100   INDEXED ORG-IDX.
               05  WRK-ORG-MASTER-KBN  PIC X(02).
               05  WRK-ORG-DBRVERSION1 PIC X(64).
               05  WRK-ORG-UPDATE-FLG  PIC X(01).
      *
           03  WRK-HOMEDIR             PIC X(128).
      *
       01  CONST-AREA.
           03  CONST-HOMEDIR           PIC X(28)  VALUE
               "/var/lib/jma-receipt/master/".
      *
           COPY    "ENUM-VALUE".
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *****************************************************************
      *    �ե��������
      *****************************************************************
      *
      *    �ģ´�������
       01  DBKANRI-REC.
           COPY    "CPDBKANRI.INC".
      *
       01  MSTKANRI-REC.
           COPY    "CPMSTKANRI.INC".
      *
      *    ���̥ѥ�᥿
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
       01  PARA-REC.
           03  PARA-HOSPNUM            PIC 9(02).
           03  PARA-SUPERVISOR         PIC X(01).
           03  PARA-DBFILE             PIC X(13).
           03  PARA-SCHEMA             PIC X(01).
      *
       PROCEDURE                       DIVISION
           USING
           PARA-REC.
      ****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-IN      =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
      *
           .
      *
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           MOVE    ZERO            TO  FLG-AREA
           MOVE    ZERO            TO  CNT-AREA
           MOVE    ZERO            TO  IDX-AREA
           INITIALIZE                  WRK-AREA
                                       SPA-AREA
      *
           MOVE    PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE    PARA-SUPERVISOR TO  SPA-SUPERVISOR
      *
           INITIALIZE                      SGETTEMP-AREA
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
      *
      *    DB OPEN
           PERFORM 100-DBOPEN-SEC
           IF      MCP-RC      NOT =   ZERO
               DISPLAY "DBOPEN ERR"
           END-IF.
      *
      *    �ե��������
           PERFORM 101-ASSIN-GET-SEC
      *
      *    ���ϡ��������
           OPEN    INPUT           IN-FILE
           IF      STS-IN          NOT =   ZERO
               MOVE    1           TO  FLG-IN
               DISPLAY "*(ORCXRMST1MIGV51)* IN-FILE OPN STS["
                                       STS-IN "]"
           END-IF
      *
      *    ���ϥե����롡�������
           OPEN    OUTPUT          OUT1-FILE
           IF      STS-OUT1        NOT =   ZERO
               DISPLAY "*(ORCXRMST1MIGV51)* OUT1-FILE OPN STS["
                                       STS-OUT1 "]"
           END-IF
           OPEN    OUTPUT          OUT2-FILE
           IF      STS-OUT2        NOT =   ZERO
               DISPLAY "*(ORCXRMST1MIGV51)* OUT2-FILE OPN STS["
                                       STS-OUT2 "]"
           END-IF
      *
           PERFORM 101-VERSION-GET-SEC
      *    MSTKANRI �쥳���ɥС������Υơ��֥�ί�����
           PERFORM 101-DBR-TBL-SET-SEC
           PERFORM 101-ORG-TBL-SET-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ե�����������
      *****************************************************************
       101-ASSIN-GET-SEC           SECTION.
      *
           IF      SGETTEMP-MW         =   SPA-PANDA
               MOVE    CONST-HOMEDIR       TO  WRK-HOMEDIR
           ELSE
               MOVE    SGETTEMP-TEMPDIR    TO  WRK-HOMEDIR
           END-IF
      *
      *    ���ϥե�����
           MOVE    SPACE               TO  ASS-IN
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   PARA-HOSPNUM        DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   PARA-DBFILE         DELIMITED  BY  SPACE
      ************ "/ORCADBR.DAT"      DELIMITED  BY  SIZE
      * 2.5.0del * "/ORCADBR-1.DAT"    DELIMITED  BY  SIZE
      * 4.1.0del * "/ORCADBR-2.DAT"    DELIMITED  BY  SIZE
      * 5.1.0del * "/ORCADBR-3.DAT"    DELIMITED  BY  SIZE
                   "/ORCADBR-4.DAT"    DELIMITED  BY  SIZE
                                       INTO    ASS-IN
           END-STRING
      *
      *    ���ϡ��ե�����
           MOVE    SPACE               TO  ASS-OUT1
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   PARA-HOSPNUM        DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   PARA-DBFILE         DELIMITED  BY  SPACE
                   "/ORCADBR.OUT1"     DELIMITED  BY  SIZE
                                       INTO    ASS-OUT1
           END-STRING
           MOVE    SPACE               TO  ASS-OUT2
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   PARA-HOSPNUM        DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   PARA-DBFILE         DELIMITED  BY  SPACE
                   "/ORCADBR.OUT2"     DELIMITED  BY  SIZE
                                       INTO    ASS-OUT2
           END-STRING
      *
           .
       101-ASSIN-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    DBKANRI �С�������������
      *****************************************************************
       101-VERSION-GET-SEC             SECTION.
      *
      *    �ģ´����ޥ�������
           INITIALIZE                  DBKANRI-REC
           MOVE    "ORCADB00"          TO  DBKANRI-KANRICD
           MOVE    DBKANRI-REC         TO  MCPDATA-REC
           MOVE    "tbl_dbkanri"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_dbkanri"   TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBKANRI-READ-SEC
           ELSE
               INITIALIZE                      DBKANRI-REC
               MOVE    1                   TO  FLG-DBKANRI
           END-IF
      *
           MOVE    SPACE               TO  WRK-DBKANRI-VERSION
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDZ
           IF      DBKANRI-KANRICD(1:8) =  "ORCADB00"
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL       IDZ     >   64
                   IF   (DBKANRI-VERSION(IDZ:1)   NOT =   SPACE)
                         ADD   1       TO  IDY
                         MOVE  DBKANRI-VERSION(IDZ:1)
                                       TO  WRK-DBKANRI-VERSION(IDY:1)
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_dbkanri"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-CLOSE-SEC
      *
           .
       101-VERSION-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    MSTKANRI �쥳���ɥС������Υơ��֥�ί����߽���
      *****************************************************************
       101-DBR-TBL-SET-SEC              SECTION.
      *
      *    �ޥ��������ޥ�������
           INITIALIZE                  MSTKANRI-REC
           MOVE    SPA-HOSPNUM         TO  MSTKANRI-HOSPNUM
           MOVE    MSTKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_mstkanri"      TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_mstkanri"  TO  MCP-TABLE
               MOVE    "all"           TO  MCP-PATHNAME
               PERFORM 910-MSTKANRI-READ-SEC
           ELSE
               INITIALIZE                      MSTKANRI-REC
               MOVE    1                   TO  FLG-MSTKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL   FLG-MSTKANRI    =   1
               IF      MSTKANRI-KANRICD(7:2)    NUMERIC
                   ADD   1             TO  IDX
                   MOVE MSTKANRI-KANRICD(7:2)
                                       TO  WRK-TBL-MASTER-KBN (IDX)
                   MOVE MSTKANRI-DBRVERSION1
                                       TO  WRK-TBL-DBRVERSION1(IDX)
                   MOVE SPACE          TO  WRK-TBL-UPDATE-FLG (IDX)
               END-IF
               MOVE    "tbl_mstkanri"  TO  MCP-TABLE
               MOVE    "all"           TO  MCP-PATHNAME
               PERFORM 910-MSTKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_mstkanri"      TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-CLOSE-SEC
      *
           .
       101-DBR-TBL-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    MSTKANRI �쥳���ɥС������Υơ��֥�ί����߽���
      *****************************************************************
       101-ORG-TBL-SET-SEC             SECTION.
           INITIALIZE                  MSTKANRI-REC
           MOVE    SPA-HOSPNUM         TO  MSTKANRI-HOSPNUM
           MOVE    MSTKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_mstkanri_org"  TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_mstkanri_org"  TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 910-MSTKANRI-READ-SEC
           ELSE
               INITIALIZE                      MSTKANRI-REC
               MOVE    1                   TO  FLG-MSTKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL   FLG-MSTKANRI    =   1
               IF      MSTKANRI-KANRICD(7:2)    NUMERIC
                   ADD   1             TO  IDX
                   MOVE MSTKANRI-KANRICD(7:2)
                                       TO  WRK-ORG-MASTER-KBN (IDX)
                   MOVE MSTKANRI-DBRVERSION1
                                       TO  WRK-ORG-DBRVERSION1(IDX)
                   MOVE SPACE          TO  WRK-ORG-UPDATE-FLG (IDX)
               END-IF
               MOVE    "tbl_mstkanri_org"  TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 910-MSTKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_mstkanri_org"  TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-CLOSE-SEC
           .
       101-ORG-TBL-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ᥤ�����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *    �쥳���ɴ����ե������ɹ���
           PERFORM 900-IN-READ-SEC
           PERFORM UNTIL   FLG-IN      =   1
               MOVE     SPACE              TO  WRK-DBRVERSION
      *        �����ޥ����Υ쥳���ɥС���������
               SET     DBR-IDX             TO  1
               SEARCH  WRK-TBL-DBRVERSION  VARYING DBR-IDX
                   AT END
                       MOVE    SPACE       TO  WRK-DBRVERSION
                   WHEN    IN-MASTER-KBN   =   WRK-TBL-MASTER-KBN
                                                              (DBR-IDX)
                       MOVE    WRK-TBL-DBRVERSION1(DBR-IDX)
                                           TO  WRK-DBRVERSION
               END-SEARCH
               IF      WRK-DBRVERSION  NOT =   SPACE
                   SET     ORG-IDX             TO  1
                   SEARCH  WRK-ORG-DBRVERSION  VARYING ORG-IDX
                       AT END
                           MOVE    SPACE       TO  WRK-DBRVERSION-ORG
                       WHEN    IN-MASTER-KBN   =   WRK-ORG-MASTER-KBN
                                                              (ORG-IDX)
                           MOVE    WRK-ORG-DBRVERSION1(ORG-IDX)
                                               TO  WRK-DBRVERSION-ORG
                   END-SEARCH
                   EVALUATE   TRUE
                       WHEN   WRK-DBRVERSION-ORG   >   WRK-DBRVERSION
                           IF    IN-DBRVERVION     >   WRK-DBRVERSION
                             IF    IN-DBRVERVION(3:IDY)  >
                                           WRK-DBKANRI-VERSION(1:IDY)
                                 CONTINUE
                             ELSE
                                 MOVE    1           TO  FLG-OUTFILE
                                 PERFORM 210-OUTFILE-SEC
                                 MOVE    IN-DBRVERVION       TO
                                           WRK-TBL-DBRVERSION1(DBR-IDX)
                                 MOVE    "1"                 TO
                                           WRK-TBL-UPDATE-FLG(DBR-IDX)
                             END-IF
                           END-IF
                       WHEN   WRK-DBRVERSION-ORG   <   WRK-DBRVERSION
                           IF    IN-DBRVERVION     >
                                                   WRK-DBRVERSION-ORG
                             IF    IN-DBRVERVION(3:IDY)  >
                                           WRK-DBKANRI-VERSION(1:IDY)
                                 CONTINUE
                             ELSE
                                 MOVE    2           TO  FLG-OUTFILE
                                 PERFORM 210-OUTFILE-SEC
                                 MOVE    IN-DBRVERVION       TO
                                           WRK-ORG-DBRVERSION1(ORG-IDX)
                                 MOVE    "1"                 TO
                                           WRK-ORG-UPDATE-FLG(ORG-IDX)
                             END-IF
                           END-IF
                       WHEN   WRK-DBRVERSION-ORG   =   WRK-DBRVERSION
                           CONTINUE
                   END-EVALUATE
               END-IF
      *        �쥳���ɴ����ե������ɹ���
               PERFORM 900-IN-READ-SEC
           END-PERFORM
      *
      *    �ޥ��������ޥ�������
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         =   100 OR
                           WRK-TBL-MASTER-KBN(IDX) =   SPACE
               IF      WRK-TBL-UPDATE-FLG(IDX) =   "1"
                   INITIALIZE              MSTKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  MSTKANRI-HOSPNUM
                   MOVE    "ORCADB"        TO  MSTKANRI-KANRICD
                   MOVE    WRK-TBL-MASTER-KBN(IDX)
                                           TO  MSTKANRI-KANRICD(7:2)
                   MOVE    MSTKANRI-REC    TO  MCPDATA-REC
                   MOVE    "tbl_mstkanri"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC          =   ZERO
                       PERFORM 910-MSTKANRI-READ-SEC
                   ELSE
                       INITIALIZE              MSTKANRI-REC
                       MOVE    1               TO  FLG-MSTKANRI
                   END-IF
      *
                   IF      FLG-MSTKANRI        =   ZERO
                       MOVE    WRK-TBL-DBRVERSION1(IDX)    TO
                                                   MSTKANRI-DBRVERSION2
                       MOVE    MSTKANRI-REC        TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       IF      MSTKANRI-KANRICD (7:2)  =   "01" OR "02"
                                                        OR "15" OR "16"
                           MOVE    "tbl_mstkanri_public"
                                                       TO  MCP-TABLE
                       ELSE
                           MOVE    "tbl_mstkanri_master"
                                                       TO  MCP-TABLE
                       END-IF
                       MOVE    "key"               TO  MCP-PATHNAME
                       PERFORM 900-ORCDBMAIN-SEC
                       IF      MCP-RC          NOT =   ZERO
                           DISPLAY "ORCXRMST1MIGV51 MSTKANRI UPD ERR:"
                                   MCP-RC
                       END-IF
                   END-IF
                   MOVE    "tbl_mstkanri"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-CLOSE-SEC
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         =   100 OR
                           WRK-ORG-MASTER-KBN(IDX) =   SPACE
               IF      WRK-ORG-UPDATE-FLG(IDX) =   "1"
                   INITIALIZE              MSTKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  MSTKANRI-HOSPNUM
                   MOVE    "ORCADB"        TO  MSTKANRI-KANRICD
                   MOVE    WRK-ORG-MASTER-KBN(IDX)
                                           TO  MSTKANRI-KANRICD(7:2)
                   MOVE    MSTKANRI-REC    TO  MCPDATA-REC
                   MOVE    "tbl_mstkanri_org"
                                           TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC          =   ZERO
                       PERFORM 910-MSTKANRI-READ-SEC
                   ELSE
                       INITIALIZE              MSTKANRI-REC
                       MOVE    1               TO  FLG-MSTKANRI
                   END-IF
      *
                   IF      FLG-MSTKANRI        =   ZERO
                       MOVE    WRK-ORG-DBRVERSION1(IDX)    TO
                                                   MSTKANRI-DBRVERSION2
                       MOVE    MSTKANRI-REC        TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_mstkanri_org"  TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       PERFORM 900-ORCDBMAIN-SEC
                       IF      MCP-RC          NOT =   ZERO
                           DISPLAY "ORCXRMST1MIGV51 MSTKANRI UPD ERR:"
                                   MCP-RC
                       END-IF
                   END-IF
                   MOVE    "tbl_mstkanri_org"  TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-CLOSE-SEC
               END-IF
           END-PERFORM
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ե��������  ����
      *****************************************************************
       210-OUTFILE-SEC               SECTION.
      *
           INITIALIZE                  OUT-REC
           MOVE    IN-MASTER-KBN       TO  OUT-MASTER-KBN
           MOVE    ":"                 TO  OUT-MASTER-DELI
           MOVE    IN-DBRVERVION       TO  OUT-DBRVERVION
      *
           MOVE    IN-UPFILE           TO  OUT-UPFILE
           MOVE    IN-UPPG             TO  OUT-UPPG
           IF      FLG-OUTFILE         =   1
               MOVE    OUT-REC             TO  OUT1-REC
               WRITE   OUT1-REC
               ADD     1                   TO  CNT-OUT1
           ELSE
               MOVE    OUT-REC             TO  OUT2-REC
               WRITE   OUT2-REC
               ADD     1                   TO  CNT-OUT2
           END-IF
      *
           .
       210-OUTFILE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *    DB DISCONNECT
           PERFORM 900-DBDISCONNECT-SEC
      *
      *    ���ϥե����롡��λ����
           CLOSE                       IN-FILE
           CLOSE                       OUT1-FILE
           CLOSE                       OUT2-FILE
      *
           DISPLAY "*(ORCXRMST1MIGV51)* INPUT  /I   CNT[" CNT-IN   "]"
           DISPLAY "*(ORCXRMST1MIGV51)* OUT1   /O   CNT[" CNT-OUT1 "]"
           DISPLAY "*(ORCXRMST1MIGV51)* OUT2   /O   CNT[" CNT-OUT2 "]"
      *****ACCEPT  WRK-IN
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϥե������ɹ��߽���
      *****************************************************************
       900-IN-READ-SEC          SECTION.
      *
       900-IN-READ-10.
           READ    IN-FILE
               AT  END
                   MOVE    1                   TO  FLG-IN
               NOT AT  END
                   MOVE    ZERO                TO  FLG-IN
                   ADD     1                   TO  CNT-IN
                   IF      IN-MASTER-KBN       =   "00"  OR  "04"
                                               OR  "05"  OR  "10"
                       CONTINUE
                   ELSE
                       GO  TO  900-IN-READ-10
                   END-IF
           END-READ
      *
           .
       900-IN-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ޥ��������ե������ɹ�
      *****************************************************************
       910-MSTKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  MSTKANRI-REC
               MOVE    ZERO            TO  FLG-MSTKANRI
           ELSE
               INITIALIZE                  MSTKANRI-REC
               MOVE    1               TO  FLG-MSTKANRI
           END-IF
      *
           .
       910-MSTKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ´����ե������ɹ�
      *****************************************************************
       910-DBKANRI-READ-SEC            SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  DBKANRI-REC
               MOVE    ZERO            TO  FLG-DBKANRI
           ELSE
               INITIALIZE                  DBKANRI-REC
               MOVE    1               TO  FLG-DBKANRI
           END-IF
      *
           .
       910-DBKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    ZERO                TO  MCP-RC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������륯����������
      *****************************************************************
       910-CLOSE-SEC                   SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
      *
       910-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡������ץ����
      *****************************************************************
       100-DBOPEN-SEC                  SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡�������������
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륢����������
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      