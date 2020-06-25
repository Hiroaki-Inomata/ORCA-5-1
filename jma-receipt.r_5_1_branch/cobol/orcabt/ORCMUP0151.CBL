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
       PROGRAM-ID.                 ORCMUP0151.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �ģº�������
      *  ����ݡ��ͥ��̾  : ���ץ�⹹������
      *                    : 
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  07.11.15    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 04.03.00     NACL-����    08/05/28  ���顼���������б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ���ϡ����ץ�⹹���ǡ���
           SELECT  IN-FILE         ASSIGN
                                   ASS-IN
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-IN.
      *    ���ϡ����顼�ե�����
           SELECT  ERR-FILE        ASSIGN
                                   ASS-ERR
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-ERR.
      *    ���ϡ��ץ롼�եե�����
           SELECT  PLF-FILE        ASSIGN
                                   ASS-PLF
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PLF.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ���ϡ����ץ�⹹���ǡ���
       FD  IN-FILE.
       01  TOUKEIMEMOWK-REC.
           03  TKIMEMOWK-SYORIKBN             PIC  X(01).
           COPY    "CPTOUKEIMEMO.INC"  REPLACING   //TKIMEMO-//
                                              BY   //TKIMEMOWK-//.
      *
      *    ���ϡ����顼�ե�����
       FD  ERR-FILE.
       01  ERR-TOUKEIMEMOWK-REC.
           03  ERR-TOUKEIMEMOWK-SYORIKBN      PIC  X(01).
           COPY    "CPTOUKEIMEMO.INC"  REPLACING   //TKIMEMO-//
                                              BY   //ERR-TKIMEMOWK-//.
      *
      *    ���ϡ��ץ롼�եե�����
       FD  PLF-FILE.
       01  PLF-REC                     PIC X(1000).
      *
       WORKING-STORAGE             SECTION.
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
      *    �ե���������ΰ�
       01  ASS-AREA.
           03  ASS-IN                  PIC X(256).
           03  ASS-ERR                 PIC X(256).
           03  ASS-PLF                 PIC X(256).
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-IN                  PIC X(02).
           03  STS-ERR                 PIC X(02).
           03  STS-PLF                 PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-IN                  PIC 9(01).
           03  FLG-TOUKEIMEMO          PIC 9(01).
           03  FLG-LF                  PIC 9(01).
      *
      *    ����ǥå����ΰ�
       01  IDX-AREA.
           03  IDX-PNT1                PIC 9(05).
           03  IDX-PNT2                PIC 9(05).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-INS                 PIC 9(06).
           03  CNT-UPD                 PIC 9(06).
           03  CNT-DEL                 PIC 9(06).
           03  CNT-ERR                 PIC 9(06).
           03  CNT-PLF                 PIC 9(06).
      *
       01  SYS-DATE.
           03  SYS-YY                  PIC 9(02).
           03  SYS-MM                  PIC 9(02).
           03  SYS-DD                  PIC 9(02).
      *
       01  SYS-TIME                    PIC 9(08).
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
           03  WRK-ERRCD               PIC 9(01).
      *
           03  WRK-IN                  PIC X(01).
           03  WRK-MEMO                PIC X(10000).
           03  WRK-MEMO-LEN            PIC 9(08).
           03  WRK-HOMEDIR             PIC X(128).
      *
      *    ���ϥե������ΰ�
       01  MES-AREA.
           03  MES-TITLE1.
               05  FILLER              PIC X(26) VALUE
                                       "��PGID:ORCMUP0151.CBL��".
               05  MES-TITLE1-YY       PIC X(04) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-MM       PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-DD       PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(02) VALUE SPACE.
               05  MES-TITLE1-HOSPNUM  PIC 9(02).
      *
       01  WRK-MSG-AREA.
           03  WRK-MSG-01              PIC X(30).
           03  FILLER                  PIC X(01)   VALUE   SPACE.
           03  WRK-SYORIKBN            PIC X(01).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-PGID                PIC X(20).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-KANRICD             PIC  X(04).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-KBNCD               PIC  X(08).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-STYUKYMD            PIC  X(08).
           03  FILLER                  PIC X(01)   VALUE   "-".
           03  WRK-EDYUKYMD            PIC  X(08).
      *
       01  CONST-AREA.
           03  CONST-HOMEDIR           PIC X(28)  VALUE
               "/var/lib/jma-receipt/master/".
      *****************************************************************
      *    �ե��������
      *****************************************************************
      *
           COPY    "ENUM-VALUE".
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ���ץ��
       01  TOUKEIMEMO-REC.
           COPY    "CPTOUKEIMEMO.INC".
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
      *
           COPY    "CPMUPLINK.INC".
      *
       PROCEDURE                       DIVISION
           USING
           LNK-AREA.
      *
      ****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL ( FLG-IN      =   1 )
                    OR   ( LNK-RC  NOT =   ZERO )
      *
           PERFORM 300-END-SEC
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-AREA
           MOVE    ZERO                TO  CNT-AREA
           INITIALIZE                      WRK-AREA
                                           SPA-AREA
                                           IDX-AREA
      *
           MOVE    LNK-HOSPNUM         TO  SPA-HOSPNUM
      *
           ACCEPT  SYS-DATE            FROM    DATE
           COMPUTE WRK-SYY             =   SYS-YY    +   2000
           MOVE    SYS-MM              TO  WRK-SMM
           MOVE    SYS-DD              TO  WRK-SDD
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    DB OPEN
      *    PERFORM 100-DBOPEN-SEC
      *    IF      MCP-RC      NOT =   ZERO
      *        DISPLAY "DBOPEN ERR"
      *    END-IF.
      *
           PERFORM 101-ASSIN-GET-SEC
      *
      *    ���ϡ��������
           OPEN    INPUT               IN-FILE
           IF  STS-IN              NOT =   ZERO
               MOVE    1               TO  FLG-IN
               DISPLAY "*(ORCMUP0151)* IN-FILE OPN STS["
                                       STS-IN "]"
           END-IF
      *
      *    ���ϥե����롡�������
           PERFORM 2700-OUTLST-INIT-SEC
      *
      *    ���ץ�⹹���ǡ����ɹ��߽���
           IF      FLG-IN              =   ZERO
               PERFORM 900-IN-KENSAKU-SEC
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �ե�����������
      *****************************************************************
       101-ASSIN-GET-SEC           SECTION.
      *
           INITIALIZE                      SGETTEMP-AREA
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
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
                   SPA-HOSPNUM         DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   LNK-DBFILE          DELIMITED  BY  SPACE
                   "/"                 DELIMITED  BY  SIZE
                   LNK-UPFILE          DELIMITED  BY  SPACE
                                       INTO    ASS-IN
           END-STRING
      *
      *    ���顼�ե�����
           MOVE    SPACE               TO  ASS-ERR
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   SPA-HOSPNUM         DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   "orca-mstlog/"      DELIMITED  BY  SIZE
                   LNK-UPFILE          DELIMITED  BY  SPACE
                   ".ERR"              DELIMITED  BY  SIZE
                                       INTO    ASS-ERR
           END-STRING
      *
      *    �ץ롼�եե�����
           MOVE    SPACE               TO  ASS-PLF
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   SPA-HOSPNUM         DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   "orca-mstlog/"      DELIMITED  BY  SIZE
                   LNK-UPFILE          DELIMITED  BY  SPACE
                   ".PRF"              DELIMITED  BY  SIZE
                                       INTO    ASS-PLF
           END-STRING
           .
       101-ASSIN-GET-EXT.
           EXIT.
      *****************************************************************
      *    �ᥤ�����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO                TO  WRK-ERRCD
      *
           EVALUATE    TKIMEMOWK-SYORIKBN
           WHEN    "I"
           WHEN    "U"
      *
               INITIALIZE                      TOUKEIMEMO-REC
               MOVE    TKIMEMOWK-KEY       TO  TKIMEMO-KEY
               MOVE    TOUKEIMEMO-REC      TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "tbl_toukeimemo"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_toukeimemo"
                                           TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM  910-TOUKEIMEMO-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-TOUKEIMEMO
               END-IF
      *
      *        ���ץ�⥯������
               MOVE    "tbl_toukeimemo"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TBL-CLOSE-SEC
      *
               IF    ( FLG-TOUKEIMEMO      =   ZERO )
                   PERFORM 2020-UPDATE-SYORI-SEC
               ELSE
                   PERFORM 2010-INSERT-SYORI-SEC
               END-IF
      *
      *    ���
           WHEN    "D"
               PERFORM 2030-DELETE-SYORI-SEC
           END-EVALUATE
      *
      *    ���ץ�⹹���ǡ����ɹ��߽���
           PERFORM 900-IN-KENSAKU-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ɲ�  ����
      *****************************************************************
       2010-INSERT-SYORI-SEC       SECTION.
      *
      *    �ģ��ɲ�
           PERFORM 20101-TOUKEIMEMO-INS-SEC
           IF      WRK-ERRCD       =   ZERO
               ADD     1               TO  CNT-INS
           END-IF
           .
       2010-INSERT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¹���  ����
      *****************************************************************
       2020-UPDATE-SYORI-SEC           SECTION.
      *
      *    �ģ¹���
           PERFORM 20101-TOUKEIMEMO-UPD-SEC
           IF      WRK-ERRCD           =   ZERO
               ADD     1               TO  CNT-UPD
           END-IF
           .
       2020-UPDATE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���  ����
      *****************************************************************
       2030-DELETE-SYORI-SEC       SECTION.
      *
      *    �ģº��
           PERFORM 20101-TOUKEIMEMO-DEL-SEC
           IF      WRK-ERRCD           =   ZERO
               ADD     1               TO  CNT-DEL
           END-IF
           .
       2030-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ��ɲ�  ����
      *****************************************************************
       20101-TOUKEIMEMO-INS-SEC  SECTION.
      *
           PERFORM 220-TOUKEIMEMO-EDIT-SEC
      *
           MOVE    LNK-MNTYMD          TO  TKIMEMO-CREYMD
                                           TKIMEMO-UPYMD
           MOVE    LNK-MNTHMS          TO  TKIMEMO-UPHMS
           MOVE    TOUKEIMEMO-REC      TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_toukeimemo"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    1               TO  LNK-RC
               MOVE    2               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           .
       20101-TOUKEIMEMO-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¹���  ����
      *****************************************************************
       20101-TOUKEIMEMO-UPD-SEC  SECTION.
      *
           PERFORM 220-TOUKEIMEMO-EDIT-SEC
      *
           MOVE    LNK-MNTYMD          TO  TKIMEMO-UPYMD
           MOVE    LNK-MNTHMS          TO  TKIMEMO-UPHMS
           MOVE    TOUKEIMEMO-REC      TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_toukeimemo"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    2               TO  LNK-RC
               MOVE    3               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           .
       20101-TOUKEIMEMO-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģº��  ����
      *****************************************************************
       20101-TOUKEIMEMO-DEL-SEC  SECTION.
      *
           INITIALIZE                  TOUKEIMEMO-REC
           MOVE    TOUKEIMEMOWK-REC(2:)
                                       TO  TOUKEIMEMO-REC
           MOVE    TOUKEIMEMO-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_toukeimemo"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    3               TO  LNK-RC
               MOVE    4               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           .
       20101-TOUKEIMEMO-DEL-EXT.
           EXIT.
      *****************************************************************
      *    ���ץ���Խ�����
      *****************************************************************
       220-TOUKEIMEMO-EDIT-SEC         SECTION.
      *
           INITIALIZE                  TOUKEIMEMO-REC
           MOVE    TOUKEIMEMOWK-REC(2:)
                                       TO  TOUKEIMEMO-REC
      *
           MOVE    SPACE               TO  WRK-MEMO
           MOVE    1                   TO  WRK-MEMO-LEN
           STRING  TKIMEMO-MEMO        DELIMITED   BY  SIZE
           INTO    WRK-MEMO
           WITH    POINTER             WRK-MEMO-LEN
           END-STRING
      *
           COMPUTE WRK-MEMO-LEN    =   WRK-MEMO-LEN    -   1
      *
           MOVE    1                   TO  IDX-PNT1
           MOVE    1                   TO  IDX-PNT2
           MOVE    SPACE               TO  WRK-MEMO
      *
           PERFORM UNTIL ( IDX-PNT1    >   WRK-MEMO-LEN )
                    OR   ( TKIMEMO-MEMO (IDX-PNT1 : ) 
                                       =   SPACE )
      *
               MOVE    ZERO            TO  FLG-LF
      *
               IF    ( IDX-PNT1        <   WRK-MEMO-LEN )
                   IF    ( TKIMEMO-MEMO (IDX-PNT1 : 2 )
                                       =   "\n" )
                       MOVE    1       TO  FLG-LF
                   END-IF
               END-IF
      *
               IF  ( FLG-LF  =  ZERO )
                   MOVE    TKIMEMO-MEMO (IDX-PNT1 : 1)
                                        TO  WRK-MEMO (IDX-PNT2 : 1)
                   COMPUTE IDX-PNT2 =   IDX-PNT2    +   1
               ELSE
                   MOVE    X"0A"        TO  WRK-MEMO (IDX-PNT2 : 1)
                   COMPUTE IDX-PNT2 =   IDX-PNT2   +   1
                   COMPUTE IDX-PNT1 =   IDX-PNT1   +   1
               END-IF
      *
                   COMPUTE IDX-PNT1 =   IDX-PNT1   +   1
      *
           END-PERFORM
      *
           MOVE    WRK-MEMO            TO  TKIMEMO-MEMO
      *
           .
       220-TOUKEIMEMO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ���顼����
      *****************************************************************
       2040-ERR-SYORI-SEC          SECTION.
      *
      *    ���顼����
           MOVE    TOUKEIMEMOWK-REC     TO  ERR-TOUKEIMEMOWK-REC
           WRITE   ERR-TOUKEIMEMOWK-REC
           ADD     1                   TO  CNT-ERR
      *
      *    ���顼��å���������
           PERFORM 20401-PLF-OUT-SEC
           .
       2040-ERR-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å��������Ͻ���
      *****************************************************************
       20401-PLF-OUT-SEC           SECTION.
      *
           MOVE    TKIMEMOWK-SYORIKBN  TO  WRK-SYORIKBN
           MOVE    TKIMEMOWK-PGID      TO  WRK-PGID
           MOVE    TKIMEMOWK-KANRICD   TO  WRK-KANRICD
           MOVE    TKIMEMOWK-KBNCD     TO  WRK-KBNCD
           MOVE    TKIMEMOWK-STYUKYMD  TO  WRK-STYUKYMD
           MOVE    TKIMEMOWK-EDYUKYMD  TO  WRK-EDYUKYMD
           EVALUATE    WRK-ERRCD
               WHEN    1
                   MOVE  "���ץ����Ͽ�Ѥ�"
                                       TO  WRK-MSG-01
               WHEN    2
                   MOVE  "�ɲå��顼"  TO  WRK-MSG-01
               WHEN    3
                   MOVE  "�������顼"  TO  WRK-MSG-01
               WHEN    4
                   MOVE  "������顼"  TO  WRK-MSG-01
               WHEN    5
                   MOVE  "�����оݤʤ�"  TO  WRK-MSG-01
               WHEN    6
                   MOVE  "����оݤʤ�"  TO  WRK-MSG-01
           END-EVALUATE
      *
           MOVE    WRK-MSG-AREA        TO  PLF-REC
           WRITE                           PLF-REC
           ADD     1                   TO  CNT-PLF
           .
       20401-PLF-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϥե����롡�������
      *****************************************************************
       2700-OUTLST-INIT-SEC        SECTION.
      *
           OPEN    OUTPUT              ERR-FILE
           IF  STS-ERR             NOT =   ZERO
               DISPLAY "*(ORCMUP0151)* OUTFILE OPN STS[" STS-ERR "]"
               GO      TO              2700-OUTLST-INIT-EXT
           END-IF
      *
      *    ���顼�쥳���ɽ��ϥե����롡�����ץ�
           OPEN    OUTPUT              PLF-FILE
           IF  STS-PLF             NOT =   ZERO
               DISPLAY "*(ORCMUP0151)* PLFFILE OPN STS[" STS-PLF "]"
               GO      TO              2700-OUTLST-INIT-EXT
           END-IF
           MOVE    WRK-SYY             TO  MES-TITLE1-YY
           MOVE    SYS-MM              TO  MES-TITLE1-MM
           MOVE    SYS-DD              TO  MES-TITLE1-DD
           MOVE    SPA-HOSPNUM         TO  MES-TITLE1-HOSPNUM
           MOVE    MES-TITLE1          TO  PLF-REC
           WRITE   PLF-REC
           .
       2700-OUTLST-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *    DB DISCONNECT
      *    PERFORM 900-DBDISCONNECT-SEC
      *
      *    ���ϥե����롡��λ����
           CLOSE                       IN-FILE
           CLOSE                       ERR-FILE
           CLOSE                       PLF-FILE
      *
           IF    ( LNK-RC              =   ZERO )
               IF    ( CNT-IN      NOT =   LNK-COUNT )
      *
                   MOVE    99              TO  LNK-RC
      *
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    1               TO  JOB-HOSPNUM
                   MOVE    "tbl_jobkanri"  TO  MCP-TABLE
                   MOVE    "abort"         TO  MCP-PATHNAME
                   MOVE    JOBKANRI-REC    TO  MCPDATA-REC
                   MOVE    "DBSELECT"      TO  MCP-FUNC
                   PERFORM 900-ORCDBMAIN-SEC
               END-IF
      *
           END-IF
      *
           DISPLAY "*(ORCMUP0151)* INPUT  /I   CNT[" CNT-IN  "]"
           DISPLAY "*(ORCMUP0151)*  INSERT     CNT[" CNT-INS "]"
           DISPLAY "*(ORCMUP0151)*  UPDATE     CNT[" CNT-UPD "]"
           DISPLAY "*(ORCMUP0151)*  DELETE     CNT[" CNT-DEL "]"
           DISPLAY "*(ORCMUP0151)* ERR    /O   CNT[" CNT-ERR "]"
           DISPLAY "*(ORCMUP0151)* PLF    /O   CNT[" CNT-PLF "]"
           DISPLAY "*(ORCMUP0151)* RC             [" LNK-RC "]"
      **** ACCEPT  WRK-IN
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *     ���ץ���ѹ��ǡ����ɹ��߽���
      *****************************************************************
       900-IN-KENSAKU-SEC          SECTION.
      *
          READ    IN-FILE
              AT  END
                  MOVE    1               TO  FLG-IN
              NOT AT  END
                  MOVE    ZERO            TO  FLG-IN
                  ADD     1               TO  CNT-IN
                  MOVE    SPA-HOSPNUM     TO  TKIMEMOWK-HOSPNUM
          END-READ
          .
       900-IN-KENSAKU-EXT.
          EXIT.
      *
      *****************************************************************
      *    ���ץ���ɹ�
      *****************************************************************
       910-TOUKEIMEMO-READ-SEC   SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TOUKEIMEMO-REC
               MOVE    ZERO                TO  FLG-TOUKEIMEMO
           ELSE
               INITIALIZE                      TOUKEIMEMO-REC
               MOVE    1                   TO  FLG-TOUKEIMEMO
           END-IF
           .
       910-TOUKEIMEMO-READ-EXT.
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
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
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륯����������
      *****************************************************************
       900-TBL-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-TBL-CLOSE-EXT.
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