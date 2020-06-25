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
       PROGRAM-ID.                 ORCGW981.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �����ƥ������������
      *  ����ݡ��ͥ��̾  : ��¾����������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  07/01/23    NACL��¿��Ǽ  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.00    NACL-¿��Ǽ  07/05/02  ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    �����ƥ�������������Ѷ��̥ѥ�᥿
           COPY    "W01COMMON-SPA".
      *
      *    �����ѣӣУ�
           COPY    "W98SCR-SPA".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-LDMAP           PIC 9(01).
           03  FLG-LOCK            PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-SELNUM          PIC 9(01).
           03  FLG-ERR             PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-WIDGET              PIC X(64).
           03  WRK-WIDMSG              PIC X(80).
           03  WRK-SDATE               PIC X(06).
           03  WRK-STIME.
               05  WRK-SHH              PIC 9(02).
               05  WRK-SMM              PIC 9(02).
               05  WRK-SSS              PIC 9(02).
      *
           03  WRK-SELNUM              PIC 9(03).
           03  WRK-ZZ9                 PIC ZZ9.
      *
      *    �����ꥹ�Ȱ���
           03  WRK-W981-ROW            PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
      *
       01  WRK-HYUOJI-AREA.
           03  WRK-DATE.
               05  WRK-YY              PIC X(02).
               05  WRK-D1              PIC X(01)   VALUE   "/".
               05  WRK-MM              PIC X(02).
               05  WRK-D2              PIC X(01)   VALUE   "/".
               05  WRK-DD              PIC X(02).
           03  WRK-TIME.
               05  WRK-THH             PIC X(02).
               05  WRK-T1              PIC X(01)   VALUE   ":".
               05  WRK-TMM             PIC X(02).
               05  WRK-T2              PIC X(01)   VALUE   ":".
               05  WRK-TSS             PIC X(02).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY  "CPSYSKANRI.INC".
      *    ��¾�������
           COPY  "CPSK9800.INC".
      *    ��¾�������ʾܺ١�
           COPY  "CPLDMAP.INC".
      *    ��¾��������ʾܺ١�
           COPY  "CPTBLLOCK.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    �����Ѵ��ΰ�
           COPY    "CPORCSNUM.INC".
      *   �����ֹ��Ѵ�����
           COPY    "CPORCSPTID.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W98
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    ��������
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    ����
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    �������ܤ��ʤ���������ɽ����
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W98         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        ��������Խ�
               PERFORM 300-SCREEN-SEC
               IF      SPA-WIDCD       NOT =   SPACE
                   PERFORM 520-WIDSET-SEC
               END-IF
      *
      *        �����Խ�
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
               MOVE   "NEW"                TO  MCP-PUTTYPE
               MOVE   "W981"               TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    ¾�Σ̣Ĥ��
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    ��ǧ���̡ʣãɣģ��ˣϣ˽���
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
      *            �����
                   MOVE    SPACE           TO  SPA-WIDCD
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 2401-ALL-DELETE-SYORI-SEC
                   ELSE
                       MOVE    1           TO  SPA-W981-CUR
                   END-IF
               WHEN    "1002"
      *            �������ǧ��
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 2402-ALL-DELETE-SEC
                   ELSE
                       MOVE    1           TO  SPA-W981-CUR
                   END-IF
                   MOVE    SPACE           TO  SPA-WIDCD
               WHEN    "1003"
      *            ���
                   MOVE    SPACE           TO  SPA-WIDCD
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 2301-DELETE-SYORI-SEC
                   ELSE
                       MOVE    1           TO  SPA-W981-CUR
                   END-IF
               WHEN    "1004"
      *            ���
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 2301-DELETE-SYORI-SEC
                   ELSE
                       MOVE    1           TO  SPA-W981-CUR
                   END-IF
                   MOVE    SPACE           TO  SPA-WIDCD
           END-EVALUATE
           MOVE    SPACE           TO  SPA-WID1-FLG
      *
           .
       330-WID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-W981-AREA
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
           INITIALIZE                  SPA-W981-LIST-G
           MOVE    SPACE               TO  TBL-LOCK
           INITIALIZE                      TBL-LOCK
           MOVE    SPA-HOSPNUM         TO  LOCK-HOSPNUM
           MOVE    TBL-LOCK            TO  MCPDATA-REC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_lock"          TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 910-LOCK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-LOCK
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL  (FLG-LOCK        =   1 )  OR
                          (IDX             >=  100 )
               PERFORM 3101-LOCK-HEN-SEC
      *
               MOVE    "tbl_lock"          TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 910-LOCK-READ-SEC
           END-PERFORM
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SPA-W981-LIST-MAX        =   ZERO
               MOVE    3                   TO  SPA-W981-CUR
           ELSE
               MOVE    1                   TO  SPA-W981-CUR
           END-IF
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       3101-LOCK-HEN-SEC                  SECTION.
      *
           ADD     1                   TO  IDX
           MOVE    IDX                 TO  SPA-W981-LIST-MAX
           MOVE    IDX                 TO  SPA-W981-RENNUM (IDX)
           MOVE    LOCK-TERMID         TO  SPA-W981-TERMID (IDX)
           MOVE    LOCK-OPID           TO  SPA-W981-OPID (IDX)
      *
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    LOCK-PTID (1)       TO  SPTID-PTID
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               MOVE    SPACE           TO  SPA-W981-PTNUM  (IDX)
           ELSE
               MOVE    SPTID-PTNUM     TO  SPA-W981-PTNUM  (IDX)
           END-IF
      *
           MOVE    LOCK-LDNAME (1)     TO  SPA-W981-LDNAME (IDX)
           MOVE    LOCK-STARTDATE (1)(1:2)
                                       TO  WRK-YY
           MOVE    LOCK-STARTDATE (1)(3:2)
                                       TO  WRK-MM
           MOVE    LOCK-STARTDATE (1)(5:2)
                                       TO  WRK-DD
           MOVE    WRK-DATE            TO  SPA-W981-DATE (IDX)
           MOVE    "-"                 TO  SPA-W981-F1   (IDX)
           MOVE    LOCK-STARTTIME (1)(1:2) TO  WRK-THH
           MOVE    LOCK-STARTTIME (1)(3:2) TO  WRK-TMM
           MOVE    LOCK-STARTTIME (1)(5:2) TO  WRK-TSS
           MOVE    WRK-TIME            TO  SPA-W981-TIME (IDX)
           .
       3101-LOCK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *    �����ꥹ�Ȱ�����¸
           IF      FLG-GMN-INIT        =   1
               MOVE    ZERO                TO  WRK-W981-ROW
           ELSE
               MOVE    W981-MEILIST-ROW    TO  WRK-W981-ROW
           END-IF
      *
           MOVE    SPACE               TO  W981
           INITIALIZE                      W981
      *    �����ꥹ�Ȥΰ��ֻ���
           MOVE    ZERO                TO  W981-MEILIST-ROW
           IF      WRK-W981-ROW    NOT =   ZERO
               MOVE    WRK-W981-ROW    TO  W981-MEILIST-ROW 
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-W981-LIST-MAX
               MOVE    SPA-W981-RENNUM (IDX)  TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  W981-RENNUM (IDX)
               MOVE    SPA-W981-OPID  (IDX)
                                           TO  W981-OPID (IDX)
               MOVE    SPA-W981-PTNUM (IDX)
                                           TO  W981-PTNUM (IDX)
               MOVE    SPA-W981-LDNAME (IDX)
                                           TO  W981-LDNAME (IDX)
               MOVE    SPA-W981-SETTIME (IDX)
                                           TO  W981-TIME (IDX)
               IF      SPA-W981-SELNUM     =   SPA-W981-RENNUM (IDX)
                   MOVE    "T"             TO  W981-MEILIST-SELECT
                                                               (IDX)
                   MOVE    IDX             TO  W981-MEILIST-ROW
                   MOVE    2               TO  W981-MEILIST-ROWATTR
               ELSE
                   MOVE    "F"             TO  W981-MEILIST-SELECT
                                                               (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-W981-LIST-MAX    TO  W981-MEILIST-COUNT
      *
           MOVE    SPA-W981-SELNUM      TO  W981-SELNUM
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̥������륻�åȽ���
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-W981-CUR        =   ZERO
               EVALUATE    WRK-WIDGET
                   WHEN    "SELNUM"
                       MOVE    2                   TO  SPA-W981-CUR
               END-EVALUATE
           END-IF
      *
           EVALUATE    SPA-W981-CUR
               WHEN    1
                   MOVE    "SELNUM"            TO  MCP-WIDGET
               WHEN    2
                   MOVE    "B11"               TO  MCP-WIDGET
               WHEN    3
                   MOVE    "B01"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        ���
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        ���ꥢ
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 220-CLERA-SYORI-SEC
      *        ���
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 230-DELETE-SEC
      *        �����
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 240-DELETE-ALL-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ���á�����
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    ���ϸĽ�Υ��å�
           PERFORM 210-GMN-INPUT-SEC
      *
           EVALUATE    WRK-WIDGET
               WHEN    "MEILIST"
                   PERFORM 2001-MEILIST-SEC
               WHEN    OTHER
      *            ���ϥ����å�����
                   PERFORM 220-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϸĽ�Υ��åȽ���
      *****************************************************************
       210-GMN-INPUT-SEC               SECTION.
      *
           MOVE    ZERO                TO  SPA-W981-CUR
      *    ���Ϲ��ܤη���
           MOVE    MCP-WIDGET          TO  WRK-WIDGET
      *
           .
       210-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϥ����å�����
      *****************************************************************
       220-INPUT-CHK-SEC               SECTION.
      *
      *    ���̤�ӣУ����Խ�
           PERFORM 2201-GMNSPA-SET-SEC
      *
           PERFORM 2002-SELNUM-SEC
      *
           .
       220-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ꥹ���������
      *****************************************************************
       2001-MEILIST-SEC               SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-W981-LIST-MAX
              IF      W981-MEILIST-SELECT (IDX)    =  "T"
                  IF       IDX                 =   SPA-W981-SELNUM
                      CONTINUE
                  ELSE
                      MOVE    IDX              TO  WRK-SELNUM
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-W981-SELNUM
               MOVE    1                   TO  FLG-SELNUM
               PERFORM 2002-SELNUM-SEC
           END-IF
      *
           .
       2001-HKNCOMBI-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̹��ܢ��ӣУ��Խ�����
      *****************************************************************
       2201-GMNSPA-SET-SEC               SECTION.
      *
           MOVE    W981-SELNUM          TO  SPA-W981-SELNUM
           .
       2201-GMNSPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ֹ����
      *****************************************************************
       2002-SELNUM-SEC               SECTION.
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-W981-SELNUM     TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-W981-CUR
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-W981-SELNUM
           END-IF
      *
           IF      (SPA-W981-SELNUM    >   ZERO)  AND
                   (SPA-ERRCD          =   SPACE) 
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-W981-LIST-MAX ) OR
                                  (FLG-CHK =   1    )
                   IF      SPA-W981-RENNUM (IDX)   =   SPA-W981-SELNUM
                       MOVE    1                   TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   ZERO
                   MOVE    "0001"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-W981-CUR
               END-IF
           END-IF
      *
           .
       2002-SELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ꥢ����
      *****************************************************************
       220-CLERA-SYORI-SEC                   SECTION.
      *
           MOVE    ZERO                TO  SPA-W981-SELNUM
      *
           IF      SPA-W981-LIST-MAX   =   ZERO
               MOVE    3                   TO  SPA-W981-CUR
           ELSE
               MOVE    1                   TO  SPA-W981-CUR
           END-IF
      *
           .
       220-CLERA-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ���ꥢ����
      *****************************************************************
       230-DELETE-SEC                   SECTION.
      *
           PERFORM 2002-SELNUM-SEC
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-W981-SELNUM     >   ZERO
                   MOVE    "1003"          TO  SPA-WIDCD
               ELSE
                   MOVE    "0001"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-W981-CUR
               END-IF
           END-IF
      *
           .
       230-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��������ʸ��̡�
      *****************************************************************
       2301-DELETE-SYORI-SEC                   SECTION.
      *
           MOVE    SPACE               TO  TBL-LOCK
           INITIALIZE                      TBL-LOCK
           MOVE    SPA-W981-TERMID (SPA-W981-SELNUM)
                                       TO  LOCK-TERMID
           MOVE    SPA-HOSPNUM         TO  LOCK-HOSPNUM
           MOVE    TBL-LOCK            TO  MCPDATA-REC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_lock"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-LOCK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-LOCK
           END-IF
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  SPA-WIDCD
           IF      FLG-LOCK            =   ZERO
               PERFORM 2311-TIME-CHK-SEC
               IF      FLG-ERR             =   1
                   MOVE    "1004"          TO  SPA-WIDCD
                   GO      TO       2301-DELETE-SYORI-EXT
               END-IF
           END-IF
      *
           IF      FLG-LOCK            =   ZERO
               MOVE    SPA-HOSPNUM         TO  LOCK-HOSPNUM
               MOVE    TBL-LOCK            TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_lock"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO 
                   MOVE    "9001"              TO  SPA-ERRCD
                   DISPLAY "W981 DEL LOCK-KEY =>" LOCK-TERMID
                           ",MCP-RC:" MCP-RC 
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 310-SPASET-SEC
           END-IF
           .
       2301-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����оݥ����å�����
      *****************************************************************
       2311-TIME-CHK-SEC                   SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
           ACCEPT  WRK-SDATE           FROM  DATE
           ACCEPT  WRK-STIME           FROM  TIME
      *    ���ߤλ��֤��飱ʬ���Ȥ���
           IF      WRK-SMM             >=  1
               COMPUTE WRK-SMM             =   WRK-SMM    -  1
           ELSE
               MOVE    59                  TO  WRK-SMM
               COMPUTE WRK-SHH             =   WRK-SHH    -  1
           END-IF
           IF      WRK-SDATE           =   LOCK-STARTDATE(01)
               IF      WRK-STIME       <   LOCK-STARTTIME(01)
                   MOVE    1               TO  FLG-ERR
               END-IF
           END-IF
           .
       2311-TIME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����������
      *****************************************************************
       240-DELETE-ALL-SEC                   SECTION.
      *
           IF      SPA-W981-LIST-MAX   >   ZERO
               MOVE    "1001"          TO  SPA-WIDCD
           END-IF
      *
           .
       240-DELETE-ALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       2401-ALL-DELETE-SYORI-SEC      SECTION.
      *
           MOVE    SPACE               TO  TBL-LOCK
           INITIALIZE                      TBL-LOCK
           MOVE    SPA-HOSPNUM         TO  LOCK-HOSPNUM
           MOVE    TBL-LOCK            TO  MCPDATA-REC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_lock"          TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 910-LOCK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-LOCK
           END-IF
           PERFORM UNTIL  (FLG-LOCK        =   1 )  OR
                          (FLG-ERR         =   1 )
               PERFORM 2311-TIME-CHK-SEC
      *
               MOVE    "tbl_lock"          TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 910-LOCK-READ-SEC
           END-PERFORM
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-ERR             =   1
               MOVE    "1002"          TO  SPA-WIDCD
           ELSE
               PERFORM 2402-ALL-DELETE-SEC
           END-IF
           .
       2401-ALL-DELETE-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ���������
      *****************************************************************
       2402-ALL-DELETE-SEC          SECTION.
      *
           MOVE    SPACE               TO  TBL-LOCK
           INITIALIZE                      TBL-LOCK
           MOVE    SPA-HOSPNUM         TO  LOCK-HOSPNUM
           MOVE    TBL-LOCK            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "9001"              TO  SPA-ERRCD
               DISPLAY "W981 DEL LOCK-KEY =>" LOCK-TERMID
                           ",MCP-RC:" MCP-RC 
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 310-SPASET-SEC
           END-IF
           .
       2402-ALL-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W98"               TO  SPA-SAKIPG
           MOVE    "W981"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "W98"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������Խ�����
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
            PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W981"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "�����ֹ椬����ޤ���"
                                       TO  SPA-ERRMSG
               WHEN    "9001"
                   MOVE    "�������顼�Ǥ���"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W981"               TO  SPA-MOTOPG
           MOVE    "WERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "WERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    �����ɥ�å�����ɽ��
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   STRING  "��¾����򤹤٤ƺ�����ޤ���"
                           "ü����Ÿ����Ǥʤ����Ȥ��ǧ���Ʋ�������"
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-WIDMSG
                   END-STRING
               WHEN    "1002"
                   STRING  "ü��Ÿ����Ȼפ�����¾���֤�����ޤ���"
                           "ü����Ÿ����Ǥʤ����Ȥ��ǧ���Ʋ�����"
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-WIDMSG
                   END-STRING
               WHEN    "1003"
                   STRING  "���򤵤줿��¾����������ޤ���"
                           "ü����Ÿ����Ǥʤ����Ȥ��ǧ���Ʋ�������"
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-WIDMSG
                   END-STRING
               WHEN    "1004"
                   STRING  "ü��Ÿ����Ȼפ�����¾���֤Ǥ���"
                           "ü����Ÿ����Ǥʤ����Ȥ��ǧ���Ʋ�����"
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-WIDMSG
                   END-STRING
          END-EVALUATE
      *
           MOVE    SPACE               TO  WID1
           INITIALIZE                      WID1
           MOVE    SPA-WIDCD           TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG          TO  WID1-ID1MSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W981"              TO  SPA-MOTOPG
           MOVE    "WID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *****************************************************************
      *    �����ޥ������ɹ�����
      *****************************************************************
       910-LOCK-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-LOCK
               MOVE    MCPDATA-REC         TO  TBL-LOCK
           ELSE
               MOVE    1                   TO  FLG-LOCK
               INITIALIZE                      TBL-LOCK
           END-IF
      *
           .
       910-LOCK-READ-EXT.
           EXIT.
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *