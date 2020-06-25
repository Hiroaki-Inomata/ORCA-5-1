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
       PROGRAM-ID.                 ORCXRMST1.
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
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ���ϡ��ѥ�᡼���ե�����
           SELECT  IN-FILE       ASSIGN
                                   ASS-IN
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-IN.
      *    ���ϡ������ե�����
           SELECT  OUT-FILE        ASSIGN
                                   ASS-OUT
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-OUT.
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
       FD  OUT-FILE.
       01  OUT-REC.
           03  OUT-MASTER-KBN          PIC X(02).
           03  OUT-MASTER-DELI         PIC X(01).
           03  OUT-DBRVERVION          PIC X(21).
           03  OUT-UPPG                PIC X(10).
           03  OUT-UPFILE              PIC X(100).
      *
       WORKING-STORAGE             SECTION.
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
      *    �ե���������ΰ�
       01  ASS-AREA.
           03  ASS-IN                  PIC X(256).
           03  ASS-OUT                 PIC X(256).
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-IN                  PIC X(02).
           03  STS-OUT                 PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-IN                  PIC 9(01).
           03  FLG-DBKANRI             PIC 9(01).
           03  FLG-MSTKANRI            PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-OUT                 PIC 9(06).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-DBRVERSION          PIC X(64).
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
               DISPLAY "*(ORCXRMST1)* IN-FILE OPN STS["
                                       STS-IN "]"
           END-IF
      *
      *    ���ϥե����롡�������
           OPEN    OUTPUT          OUT-FILE
           IF      STS-OUT         NOT =   ZERO
               DISPLAY "*(ORCXRMST1)* OUT-FILE OPN STS["
                                       STS-OUT "]"
           END-IF
      *
           PERFORM 101-VERSION-GET-SEC
      *    MSTKANRI �쥳���ɥС������Υơ��֥�ί�����
           PERFORM 101-DBR-TBL-SET-SEC
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
           MOVE    SPACE               TO  ASS-OUT
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   PARA-HOSPNUM        DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   PARA-DBFILE         DELIMITED  BY  SPACE
                   "/ORCADBR.OUT"      DELIMITED  BY  SIZE
                                       INTO    ASS-OUT
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
grpsys     MOVE    SPA-HOSPNUM         TO  MSTKANRI-HOSPNUM
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
      *    �ᥤ�����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *    �쥳���ɴ����ե������ɹ���
           PERFORM 900-IN-READ-SEC
           PERFORM
               UNTIL    FLG-IN         =   1
               MOVE     SPACE          TO  WRK-DBRVERSION
      *        �����ޥ����Υ쥳���ɥС���������
               SET      DBR-IDX        TO  1
               SEARCH   WRK-TBL-DBRVERSION   VARYING   DBR-IDX
                     AT END
                        MOVE  SPACE    TO  WRK-DBRVERSION
                     WHEN     IN-MASTER-KBN
                                       =   WRK-TBL-MASTER-KBN(DBR-IDX)
                        MOVE  WRK-TBL-DBRVERSION1(DBR-IDX)
                                       TO  WRK-DBRVERSION
               END-SEARCH
               IF      WRK-DBRVERSION  NOT =   SPACE
      *        �ģ¥쥳���ɥС������ޤǽ��Ϥ���
                 IF    IN-DBRVERVION       >   WRK-DBRVERSION
                   MOVE    1               TO  FLG-SYORI
                   IF      IN-DBRVERVION(3:IDY)
                                           >   WRK-DBKANRI-VERSION
                                                       (1:IDY)
                       CONTINUE
                   ELSE
                       PERFORM 210-OUTFILE-SEC
                       MOVE    IN-DBRVERVION
                                       TO  WRK-TBL-DBRVERSION1(DBR-IDX)
                       MOVE    "1"     TO  WRK-TBL-UPDATE-FLG(DBR-IDX)
                   END-IF
                 END-IF
               END-IF
      *
      *        �쥳���ɴ����ե������ɹ���
               PERFORM 900-IN-READ-SEC
           END-PERFORM
      *
      *    �ޥ��������ޥ�������
           PERFORM     VARYING  IDX
                       FROM     1     BY   1
                       UNTIL    IDX   =    100   OR
                                WRK-TBL-MASTER-KBN(IDX) = SPACE
             IF      WRK-TBL-UPDATE-FLG(IDX)  =  "1"
               INITIALIZE              MSTKANRI-REC
grpsys         MOVE    SPA-HOSPNUM     TO  MSTKANRI-HOSPNUM
               MOVE    "ORCADB"        TO  MSTKANRI-KANRICD
               MOVE    WRK-TBL-MASTER-KBN(IDX)
                                       TO  MSTKANRI-KANRICD(7:2)
               MOVE    MSTKANRI-REC    TO  MCPDATA-REC
               MOVE    "tbl_mstkanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC           =  ZERO
                   PERFORM 910-MSTKANRI-READ-SEC
               ELSE
                   INITIALIZE              MSTKANRI-REC
                   MOVE    1           TO  FLG-MSTKANRI
               END-IF
      *
               IF      FLG-MSTKANRI     =  ZERO
                   MOVE    WRK-TBL-DBRVERSION1(IDX)
                                       TO  MSTKANRI-DBRVERSION2
                   MOVE    MSTKANRI-REC TO  MCPDATA-REC
                   MOVE    "DBUPDATE"  TO  MCP-FUNC
                   IF      MSTKANRI-KANRICD (7:2)  =           "01"
                                    OR "02" OR "15" OR "16"
                       MOVE    "tbl_mstkanri_public"
                                       TO  MCP-TABLE
                   ELSE
                       MOVE    "tbl_mstkanri_master"
                                       TO  MCP-TABLE
                   END-IF
                   MOVE    "key"       TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC      NOT =  ZERO
                     DISPLAY "ORCXRMST1 MSTKANRI UPD ERR:" MCP-RC
                   END-IF
               END-IF
               MOVE    "tbl_mstkanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
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
           WRITE                       OUT-REC
           ADD     1                   TO  CNT-OUT
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
           CLOSE                       OUT-FILE
      *
           DISPLAY "*(ORCXRMST1)* INPUT  /I   CNT[" CNT-IN  "]"
           DISPLAY "*(ORCXRMST1)* OUTPUT /O   CNT[" CNT-OUT "]"
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
                  MOVE    1               TO  FLG-IN
              NOT AT  END
                  ADD     1               TO  CNT-IN
                  IF    ((PARA-SCHEMA     =   "P") AND
                         (IN-SCHEMA   NOT =   "P"))    OR   
                        ((PARA-SCHEMA     =   "M") AND
                         (IN-SCHEMA   NOT =   "M"))
                      GO  TO  900-IN-READ-10
                  END-IF   
                  MOVE    ZERO            TO  FLG-IN
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
      *    �ģ¡�������������
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
       900-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륢����������
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