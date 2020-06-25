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
       PROGRAM-ID.                 SYS3004.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �ģº�������
      *  ����ݡ��ͥ��̾  : �����ƥ������������
      *                    : 
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  14/10/02    uchi    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ���ϡ������ƥ���������ե�����
           SELECT  IN-FILE         ASSIGN
                                   ASS-IN
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-IN.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ���ϡ������ƥ���������ե�����
       FD  IN-FILE.
       01  IN-3004-REC.
      *������ID
           03  IN-3004-SHELLID         PIC  X(020).
      *����̾��
           03  IN-3004-SHORIMSG        PIC  X(040).
      *����ե饰
           03  IN-3004-CHK.
               05  IN-3004-CHKFLG      PIC  X(01)
                                        OCCURS   4.
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "COMMON-SPA".
      *
      *    �ե���������ΰ�
       01  ASS-AREA.
           03  ASS-IN                  PIC X(256).
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-IN                  PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-PARA                PIC 9(01).
           03  FLG-IN                  PIC 9(01).
      *
           03  FLG-SYS                 PIC 9(01).
           03  FLG-CHECK               PIC 9(01).
           03  FLG-DATA                PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDX-LIKE                PIC 9(04).
      *
      *
       01  SYS-DATE.
           03  SYS-YY                  PIC 9(02).
           03  SYS-MM                  PIC 9(02).
           03  SYS-DD                  PIC 9(02).
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYMD2.
               05  WRK-SYY2            PIC 9(02).
               05  WRK-SMM2            PIC 9(02).
               05  WRK-SDD2            PIC 9(02).
           03  WRK-LAST-STYUKYMD       PIC X(08).
           03  WRK-LAST-EDYUKYMD       PIC X(08).
      *
      *�����祭�������ѹ���
           03  WRK-SYSUPD-KEY.
               05  WRK-SYSUP-HOSPNUM   PIC 9(02).
               05  WRK-SYSUP-KANRICD   PIC X(04).
               05  WRK-SYSUP-KBNCD     PIC X(08).
               05  WRK-SYSUP-STYUKYMD  PIC X(08).
               05  WRK-SYSUP-EDYUKYMD  PIC X(08).
      *
           03  WRK-KBNCD               PIC X(08).
      *
       01  LNK-AREA.
           03  LNK-HOSPNUM             PIC  X(02).
           03  LNK-YMD                 PIC  X(08).
           03  LNK-HMS                 PIC  X(06).
           03  LNK-DIR                 PIC  X(100).
           03  LNK-FILENAME            PIC  X(30).
      *
      *****************************************************************
      *    �ե��������
      *****************************************************************
      *
      *    �����Ѵ��ΰ�
           COPY    "CPORCSNUM.INC".
      *
      *****************************************************************
      *    �ե��������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *
      *    ����ִ�������
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ���̥ѥ�᥿
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER  PIC X(256).
      *
       PROCEDURE                       DIVISION
           USING
           COMMAND-PARAM.
      *
      ****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF  FLG-IN  =  ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
      *****STOP    RUN
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           MOVE    ZERO            TO  FLG-AREA
           MOVE    ZERO            TO  IDX-AREA
           INITIALIZE                  WRK-AREA
                                       SPA-AREA
      *    ����
           UNSTRING  COMMAND-PARAM  DELIMITED  BY  ","
                                    INTO  LNK-HOSPNUM
                                          LNK-YMD
                                          LNK-HMS
                                          LNK-DIR
                                          LNK-FILENAME
           END-UNSTRING
      *
           DISPLAY "START"
      *
           MOVE    LNK-HOSPNUM     TO  SPA-HOSPNUM
      *
      *    DB OPEN
           PERFORM 100-DBOPEN-SEC
      *
      *    �ե��������
           PERFORM 101-ASSIN-GET-SEC
      *
      *    ���ϡ��������
           OPEN    INPUT           IN-FILE
           IF  STS-IN          NOT =   ZERO
               MOVE    1           TO  FLG-IN
               DISPLAY "*(SYS3004)* IN-FILE OPN STS["
                                       STS-IN "]"
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ե�����������
      *****************************************************************
       101-ASSIN-GET-SEC                SECTION.
      *
      *    ���ϥե�����
           MOVE    SPACE               TO  ASS-IN
           STRING  LNK-DIR             DELIMITED  BY  SPACE
                   "/"                 DELIMITED  BY  SIZE
                   LNK-FILENAME        DELIMITED  BY  SPACE
                                       INTO    ASS-IN
           END-STRING
      *
           .
       101-ASSIN-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ᥤ�����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *    �����ɡ�3004�ץǡ�����¸�ߥ����å�
           PERFORM  2001-DATA-CHECK-SEC
           IF  FLG-CHECK  =  0
      *        �ǡ����ɲ�
               PERFORM  201-DATA-INSERT-MAIN-SEC
                   UNTIL ( FLG-IN      =   1 )
           ELSE
      *        �ǡ�������
               PERFORM  202-DATA-UPDATE-MAIN-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ɡ�3004�ץǡ���¸�ߥ����å�  ����
      *****************************************************************
       2001-DATA-CHECK-SEC               SECTION.
      *
      *    ���٤⻲�Ȥ��Ƥ��ʤ����Τߥǡ�����¸�ߤ��ʤ�
           INITIALIZE                  SYSKANRI-REC
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "3004"              TO  SYS-KANRICD
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               
               IF  FLG-SYS  =  0
                   MOVE      1             TO  FLG-CHECK
               END-IF
           END-IF
      *
           MOVE  "tbl_syskanri"        TO  MCP-TABLE
           MOVE  "key5"                TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2001-DATA-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ǡ����ɲåᥤ��  ����
      *****************************************************************
       201-DATA-INSERT-MAIN-SEC               SECTION.
      *
      *    �ե������ɤ߹���
           PERFORM  900-DATA-READ-SEC
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >    25
                    OR       FLG-IN  =  1
      *        �ǡ�����Ͽ���ɲá�
               PERFORM  2010-DATA-INSERT-SEC
      *
      *        �ե������ɤ߹���
               PERFORM  900-DATA-READ-SEC
           END-PERFORM
      *
           .
       201-DATA-INSERT-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ǡ����ɲ�  ����
      *****************************************************************
       2010-DATA-INSERT-SEC               SECTION.
      *
      *    �ǡ����ɲý���
           INITIALIZE                  SYSKANRI-REC
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "3004"              TO  SYS-KANRICD
           MOVE    IDX                 TO  SYS-KBNCD
           MOVE    "00000000"          TO  SYS-STYUKYMD
           MOVE    "99999999"          TO  SYS-EDYUKYMD
           MOVE    "     0"            TO  SYS-UPHMS
           MOVE    IN-3004-REC         TO  SYS-KANRITBL
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               DISPLAY  "DATA INSERT!"
           END-IF
      *
           .
       2010-DATA-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ǡ��������ᥤ��  ����
      *****************************************************************
       202-DATA-UPDATE-MAIN-SEC               SECTION.
      *
      *    �ǿ���ͭ�����¼���
           PERFORM  2020-KIGEN-GET-SEC
      *
      *    �ե������ɤ߹���
           PERFORM  900-DATA-READ-SEC
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >    25
                    OR       FLG-DATA   =  1
                    OR       FLG-IN     =  1
      *        shellid�ν�ʣ�����å�
               PERFORM  2021-TYOFUKU-CHECK-SEC
      *
               IF  FLG-CHECK  =  0
      *            ��Ͽ�������å��ʶ���Ϥ��뤫��
                   PERFORM  2022-SPACE-CHECK-SEC
      *
      *            ���󤬤ʤ��ʤ�ȴ����ʼ�ư��Ͽ������λ��
                   IF  FLG-CHECK  =  1
      *                �ǡ�����Ͽ�ʹ�����
                       PERFORM  2023-DATA-UPDATE-SEC
                   ELSE
                       MOVE  1  TO  FLG-DATA
                   END-IF
               END-IF
      *
      *        �ե������ɤ߹���
               PERFORM  900-DATA-READ-SEC
      *        �ɤ߹�����ǡ���������ʤ鼫ư��Ͽ������λ
               IF  IN-3004-SHELLID(1:20)  =  SPACE
                   MOVE  1  TO  FLG-DATA
               END-IF
           END-PERFORM
      *
           .
       202-DATA-UPDATE-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ǿ�ͭ�����¼���  ����
      *****************************************************************
       2020-KIGEN-GET-SEC               SECTION.
      *
      *    �ǿ�����
           INITIALIZE                  SYSKANRI-REC
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "3004"              TO  SYS-KANRICD
           MOVE    "0001"              TO  SYS-KBNCD
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               
               IF  FLG-SYS  =  ZERO
                   MOVE  SYS-STYUKYMD  TO  WRK-LAST-STYUKYMD
                   MOVE  SYS-EDYUKYMD  TO  WRK-LAST-EDYUKYMD
               ELSE
                   MOVE  1  TO  FLG-DATA
               END-IF
           ELSE
               MOVE  1  TO  FLG-DATA
           END-IF
      *
           MOVE  "tbl_syskanri"        TO  MCP-TABLE
           MOVE  "key8"                TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2020-KIGEN-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    shellid�ν�ʣ�����å�  ����
      *****************************************************************
       2021-TYOFUKU-CHECK-SEC               SECTION.
      *
      *    ��ʣ�����å�
           INITIALIZE                  SYSKANRI-REC
                                       FLG-CHECK
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "3004"              TO  SYS-KANRICD
           MOVE    WRK-LAST-STYUKYMD   TO  SYS-STYUKYMD
           MOVE    WRK-LAST-EDYUKYMD   TO  SYS-EDYUKYMD
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
      *
               PERFORM  UNTIL  FLG-SYS    =  1
                        OR     FLG-CHECK  =  1
      *            Ʊ��Shellid����Ͽ����Ƥ��뤫
                   IF  SYS-KANRITBL(1:20)  =  IN-3004-SHELLID
                       MOVE  1  TO  FLG-CHECK
                   ELSE
                       PERFORM  910-SYSKANRI-READ-SEC
                   END-IF
               END-PERFORM
           ELSE
               MOVE  1  TO  FLG-DATA
           END-IF
      *
           MOVE  "tbl_syskanri"        TO  MCP-TABLE
           MOVE  "key2"                TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2021-TYOFUKU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��Ͽ�������å�  ����
      *****************************************************************
       2022-SPACE-CHECK-SEC               SECTION.
      *
      *    ����Ϥ��뤫
           INITIALIZE                  SYSKANRI-REC
                                       FLG-CHECK
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "3004"              TO  SYS-KANRICD
           MOVE    WRK-LAST-STYUKYMD   TO  SYS-STYUKYMD
           MOVE    WRK-LAST-EDYUKYMD   TO  SYS-EDYUKYMD
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               
               PERFORM  UNTIL  FLG-SYS    =  1
                        OR     FLG-CHECK  =  1
      *            �����Ƥ��뤫
                   IF  SYS-KANRITBL(1:60)  =  SPACE
                       MOVE  SYS-KBNCD  TO  WRK-KBNCD
                       MOVE  1          TO  FLG-CHECK
                   ELSE
                       PERFORM  910-SYSKANRI-READ-SEC
                   END-IF
               END-PERFORM
           ELSE
               MOVE  1  TO  FLG-DATA
           END-IF
      *
           MOVE  "tbl_syskanri"        TO  MCP-TABLE
           MOVE  "key2"                TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2022-SPACE-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ǡ�����Ͽ�ʹ�����  ����
      *****************************************************************
       2023-DATA-UPDATE-SEC               SECTION.
      *
      *    ����Ϥ��뤫
           INITIALIZE                  SYSKANRI-REC
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "3004"              TO  SYS-KANRICD
           MOVE    WRK-KBNCD           TO  SYS-KBNCD
           MOVE    WRK-LAST-STYUKYMD   TO  SYS-STYUKYMD
           MOVE    WRK-LAST-EDYUKYMD   TO  SYS-EDYUKYMD
           MOVE    "     0"            TO  SYS-UPHMS
           MOVE    IN-3004-REC         TO  SYS-KANRITBL
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               DISPLAY  "DATA UPDATE!"
           END-IF
      *
           .
       2023-DATA-UPDATE-EXT.
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
           DISPLAY "END"
      *    ���ϥե����롡��λ����
           CLOSE                       IN-FILE
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƥ���������ե������ɹ��߽���
      *****************************************************************
       900-DATA-READ-SEC          SECTION.
      *
          READ    IN-FILE
              AT  END
                  MOVE    1               TO  FLG-IN
              NOT AT  END
                  MOVE    ZERO            TO  FLG-IN
          END-READ
      *
          .
       900-DATA-READ-EXT.
          EXIT.
      *
      *****************************************************************
      *    �����ƥ�����ޥ����ɹ�
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
               MOVE    ZERO            TO  FLG-SYS
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1               TO  FLG-SYS
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������륯����������
      *****************************************************************
       990-DBCLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
      *
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡������ץ����
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
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
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
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