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
       PROGRAM-ID.             ORCGT91.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����ɽ�����������
      *  ����ݡ��ͥ��̾  : ����Ĵ���ǡ�����ǧ�ʣԣ�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  07/06/18    NACL�ݰ�ƣ    ��������
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
      *    ����Ĵ�������ǡ���
           SELECT  COLLECT-FILE    ASSIGN  COLLECT
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-COLLECT.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ����Ĵ�������ǡ���
       FD  COLLECT-FILE.
       01  COLLECT-REC             PIC X(3000). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    ����Ĵ�������ǡ��� 
       01  COLLECT.
           03  FILLER              PIC X(08)  VALUE "/var/lib".
           03  FILLER              PIC X(12)  VALUE "/jma-receipt".
           03  FILLER              PIC X(05)  VALUE "/das/".
           03  COLLECT-HOSPNUM     PIC 9(02).
           03  FILLER              PIC X(05)  VALUE "/tmp/".
           03  FILLER              PIC X(07)  VALUE "collect".
           03  COLLECT-GN          PIC X(01).
           03  FILLER              PIC X(04)  VALUE ".csv".
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
           COPY    "ENUM-VALUE".
      *
      *    �쥻�ץȺ����ؼ����̥ѥ�᥿
           COPY    "T01COMMON-SPA".
      *
      *    �����ѣӣУ�
       01  SPA-T91.
           03  SPA-NAI-MOTOPG        PIC X(08).
           03  SPA-T91-AREA.
               05  SPA-GMN-CUR       PIC 9(02).
               05  SPA-GMN-DATAVIEW  PIC X(63000).
               05  SPA-GMN-CNTHEN    PIC X(50).
               05  SPA-GMN-SYORI     PIC X(01).
      *
               05  SPA-NAI-CNT       PIC 9(06).
               05  SPA-NAI-NEXT      PIC 9(01).
               05  SPA-NAI-PAGE      PIC 9(06).
               05  SPA-NAI-TOTALPAGE PIC 9(06).
               05  SPA-NAI-PAGECNT   OCCURS 200
                                     PIC S9(09) BINARY.
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-COLLECT         PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-COLLECT         PIC 9(01).
           03  FLG-OVR             PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(06).
           03  IDX-2               PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX2-1              PIC 9(05).
           03  IDX-1               PIC 9(05).
           03  IDX-1U              PIC 9(05).
           03  IDX-3               PIC 9(05).
           03  IDY                 PIC 9(05).
           03  IDZ                 PIC 9(05).
           03  IDX-CNT             PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-TIDMSG          PIC X(80).
      *
           03  WRK-DATA-MAX        PIC 9(06).
           03  WRK-DATAVIEW-G      PIC X(63000).
           03  WRK-DATAVIEW        PIC X(63000). 
           03  WRK-CNT             PIC 9(05).
           03  WRK-CNT-U           PIC 9(05).
      *
           03  WRK-Z1              PIC ZZZ,ZZ9.
           03  WRK-Z2              PIC ZZZ,ZZ9.
      *
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMDG3        PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *
      *    �ե�����ǥ��쥯�ȥ���ֻ��ꥵ��
           COPY  "CPMKPASS.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *                             
       01  WRK-SHELL-NAME         PIC X(50).
      *     
       01  WRK-CONS-AREA.
      *    ����ִ����ģ¤Υ����͡ʥǡ���������  
           03  WRK-CONS-JOB-SHELLID-COLLECT
                                   PIC X(08)   VALUE   "COLLECT".
           03  WRK-CONS-JOB-JOBID-COLLECT
                                   PIC 9(07)   VALUE   1.
      *
           03  WRK-CONS-MAX-LEN    PIC 9(05)   VALUE   63000.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1031.INC".
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    Ⱦ�ѥ����å�����
           COPY    "CPORCSKANACHK.INC".
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   �����Ѵ�����
           COPY    "CPORCSGDAY.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *   ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *   �ե��������ѥ�᥿
           COPY    "CPORCSFDEL.INC".
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA44SCRAREA.INC".
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
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-T91-AREA
           MOVE    SPA-WORK        TO  SPA-T01KYOUTU
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
           MOVE    SPA-T01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-T91-AREA    TO  SPA-FREE
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "TERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        ������̥��å�
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *
               IF      FLG-END             =   ZERO
      *            �����Խ�
                   PERFORM 500-GMNHEN-SEC
               END-IF
               IF      SPA-TIDCD       NOT =   SPACE
                   EVALUATE    SPA-TIDCD
                       WHEN    "2008"
                           PERFORM 520-TIDSET-SEC
                           GO  TO  100-INIT-EXT
                   END-EVALUATE
               END-IF
           END-IF
      * 
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      *
               MOVE    SPACE               TO  MCP-PUTTYPE
               MOVE    "T91    "           TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    ¾�Σ̣Ĥ��
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  SPA-NAI-MOTOPG
               INITIALIZE              SPA-T01KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "TID1"
                   PERFORM 330-TID1-SET-SEC
               WHEN    "TID2"
                   PERFORM 340-TID2-SET-SEC
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ѽ���Խ�����
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-T91-AREA
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           MOVE    "2"             TO  SPA-GMN-SYORI
           PERFORM 310-DATA-HENSYU-SEC
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ǡ����Խ�����
      *****************************************************************
       310-DATA-HENSYU-SEC            SECTION.
      *
           IF      SPA-GMN-SYORI       =   "2"
               MOVE    SPA-HOSPNUM     TO  COLLECT-HOSPNUM
               MOVE    "g"             TO  COLLECT-GN
           ELSE
               MOVE    SPA-HOSPNUM     TO  COLLECT-HOSPNUM
               MOVE    "n"             TO  COLLECT-GN
           END-IF
      *
           MOVE    SPACE               TO  SPA-GMN-DATAVIEW
           MOVE    SPA-NAI-CNT         TO  WRK-DATA-MAX
           MOVE    ZERO                TO  SPA-NAI-NEXT
           MOVE    ZERO                TO  SPA-NAI-CNT
           MOVE    ZERO                TO  IDX-CNT
           MOVE    ZERO                TO  IDX
           COMPUTE SPA-NAI-PAGE        =   SPA-NAI-PAGE  +  1
           IF      SPA-NAI-PAGE        >   200
               MOVE    1                   TO  SPA-NAI-PAGE
               MOVE    ZERO                TO  WRK-DATA-MAX
           END-IF
           MOVE    WRK-DATA-MAX      TO  SPA-NAI-PAGECNT (SPA-NAI-PAGE)
      *
      *#   IF      SPA-NAI-TOTALPAGE   =   0
      *#           PERFORM 3100-DATA-HENSYU-SEC
      *#       DISPLAY "ORCGT91 : SPA-NAI-TOTALPAGE = "
      *#               SPA-NAI-TOTALPAGE
      *#   END-IF
      *
           OPEN    INPUT               COLLECT-FILE
           MOVE    ZERO                TO  FLG-COLLECT
           IF      STS-COLLECT     NOT =   ZERO
      *        �ǡ����ʤ�
               MOVE   1                TO  FLG-COLLECT
               DISPLAY " STS-COLLECT:"  STS-COLLECT
           END-IF
      *
           MOVE   SPACE            TO  WRK-DATAVIEW
           MOVE   ZERO             TO  WRK-CNT
           MOVE   ZERO             TO  WRK-CNT-U
           PERFORM UNTIL       FLG-COLLECT  =   1
               READ    COLLECT-FILE
                   AT  END
                       MOVE   1                TO  FLG-COLLECT
                   NOT AT  END
                       ADD    1                TO  IDX
                       IF     WRK-DATA-MAX     <   IDX
                           ADD     1               TO  IDX-CNT
                           PERFORM 3101-DATA-HEN-SEC
                       END-IF
               END-READ
           END-PERFORM
      *
           CLOSE                       COLLECT-FILE
      *
           IF      IDX-CNT             =   ZERO
      *        �оݤʤ�
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               MOVE    WRK-DATAVIEW        TO  SPA-GMN-DATAVIEW
      *        ����
               MOVE    SPACE               TO  SPA-GMN-CNTHEN
               ADD     1                   TO  WRK-DATA-MAX
               MOVE    WRK-DATA-MAX        TO  WRK-Z1
               IF      SPA-NAI-NEXT    =   1
                   MOVE    SPA-NAI-CNT     TO  WRK-Z2
               ELSE
                   MOVE    IDX             TO  WRK-Z2
               END-IF
               STRING  "��"                DELIMITED  BY  SIZE
                       WRK-Z1              DELIMITED  BY  SIZE
                       "��"                DELIMITED  BY  SIZE
                       WRK-Z2              DELIMITED  BY  SIZE
                       "��"                DELIMITED  BY  SIZE
                                       INTO  SPA-GMN-CNTHEN
               END-STRING
               IF      SPA-NAI-NEXT    =   1
                   MOVE    2               TO  SPA-GMN-CUR
               ELSE
                   MOVE    9               TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       310-DATA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ǡ����Խ����������ڡ��������С�
      *    �����աˤȤꤢ����̤���ѤǤ�
      *****************************************************************
       3100-DATA-HENSYU-SEC           SECTION.
      *
           MOVE    ZERO                TO  IDX-CNT
           MOVE    ZERO                TO  IDX
      *
           OPEN    INPUT               COLLECT-FILE
           MOVE    ZERO                TO  FLG-COLLECT
           IF      STS-COLLECT     NOT =   ZERO
      *        �ǡ����ʤ�
               MOVE   1                TO  FLG-COLLECT
               DISPLAY " STS-COLLECT:"  STS-COLLECT
           END-IF
      *
           MOVE    ZERO                TO  WRK-CNT
           PERFORM UNTIL       FLG-COLLECT  =   1
               READ    COLLECT-FILE
                   AT  END
                       MOVE   1                TO  FLG-COLLECT
                   NOT AT  END
                       PERFORM 31001-DATA-HEN-SEC
               END-READ
           END-PERFORM
      *
           CLOSE                       COLLECT-FILE
           .
       3100-DATA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ǡ����Խ����������ڡ��������С�
      *    �����աˤȤꤢ����̤���ѤǤ�
      *****************************************************************
       31001-DATA-HEN-SEC           SECTION.
      *
           MOVE   ZERO             TO  FLG-OVR
           MOVE   ZERO             TO  IDX-1
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   3000 )  OR
                              (FLG-OVR =   1    )
               COMPUTE IDX2-1          =   IDX2  +  1
               IF     (COLLECT-REC(IDX2:2)     =   SPACE )
                   MOVE    1           TO  FLG-OVR
               ELSE
                   IF     (COLLECT-REC(IDX2:1)     >=  X"A1" )
                       ADD     2           TO  IDX-1
                   ELSE
                       ADD     1           TO  IDX-1
                   END-IF
               END-IF
           END-PERFORM
      *
           IF     (IDX-1   +   WRK-CNT )   >=  WRK-CONS-MAX-LEN
               COMPUTE SPA-NAI-TOTALPAGE   =   SPA-NAI-TOTALPAGE  +  1
               MOVE   ZERO             TO  WRK-CNT
           ELSE
               COMPUTE WRK-CNT         =   WRK-CNT  +  IDX-1
               COMPUTE WRK-CNT         =   WRK-CNT  +  1
           END-IF
           .
       31001-DATA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ǧ���̡ʣãɣģ��ˣϣ˽���
      *****************************************************************
       330-TID1-SET-SEC            SECTION.
      *
           IF      SPA-TID1-FLG    =   "OK"
               EVALUATE    SPA-TIDCD
                   WHEN    "2006"
      *                ����Ĵ�� �ե���������
                       PERFORM 470-COLLECT-SYORI-SEC
                       MOVE    "JOIN"          TO  MCP-PUTTYPE
                       PERFORM 210-BACK
                   WHEN    "2008"
      *                ����Ĵ�� �ǡ������
                       PERFORM 470-COLLECT-DELETE-SEC
                       MOVE    "JOIN"          TO  MCP-PUTTYPE
                       PERFORM 210-BACK
               END-EVALUATE
           ELSE        
               EVALUATE    SPA-TIDCD
                   WHEN    "2006"
                       MOVE    "2008"          TO  SPA-TIDCD
                       GO  TO  330-TID1-SET-EXT
                   WHEN    "2008"
                       MOVE    "JOIN"          TO  MCP-PUTTYPE
                       PERFORM 210-BACK
               END-EVALUATE
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
           .
       330-TID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ǧ���̡ʣãɣģ��ˣϣ˽���
      *****************************************************************
       3101-DATA-HEN-SEC            SECTION.
      *
           MOVE   ZERO             TO  FLG-OVR
           MOVE   ZERO             TO  IDX-1
           MOVE   ZERO             TO  IDX-1U
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   3000 )  OR
                              (FLG-OVR =   1    )
               COMPUTE IDX2-1          =   IDX2  +  1
               IF     (COLLECT-REC(IDX2:2)     =   SPACE )
                   MOVE    1           TO  FLG-OVR
               ELSE
                   ADD     1           TO  IDX-1
                   IF     (COLLECT-REC(IDX2:1)     >=  X"A1" )
                       ADD     2           TO  IDX-1U
                   ELSE
                       ADD     1           TO  IDX-1U
                   END-IF
               END-IF
           END-PERFORM
      *
           IF     (IDX-1U +  WRK-CNT-U )   >=  WRK-CONS-MAX-LEN
               MOVE    1               TO  FLG-COLLECT
               MOVE    1               TO  SPA-NAI-NEXT
               COMPUTE SPA-NAI-CNT     =   IDX    -   1
           ELSE
               IF      WRK-DATAVIEW    =   SPACE
                   MOVE    COLLECT-REC(1:IDX-1)   TO  WRK-DATAVIEW
                   MOVE    IDX-1           TO  WRK-CNT
                   MOVE    IDX-1U          TO  WRK-CNT-U
               ELSE
                   COMPUTE IDX-3           =   WRK-CNT   +  1
                   MOVE    COLLECT-REC(1:IDX-1)   TO  WRK-DATAVIEW
                                                          (IDX-3:IDX-1)
                   COMPUTE WRK-CNT         =   WRK-CNT  +  IDX-1
                   COMPUTE WRK-CNT-U       =   WRK-CNT-U  +  IDX-1U
               END-IF
               COMPUTE WRK-CNT         =   WRK-CNT  +  1
               COMPUTE WRK-CNT-U       =   WRK-CNT-U  +  1
               IF      WRK-CNT-U       <=  WRK-CONS-MAX-LEN
                   MOVE    X"0A"           TO  WRK-DATAVIEW(WRK-CNT:1)
               END-IF
           END-IF
           .
       3101-DATA-HEN-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ǧ���̡ʣԣɣģ��ˣϣ˽���
      *****************************************************************
       340-TID2-SET-SEC            SECTION.
      *
           MOVE    1               TO  SPA-GMN-CUR
           .
       340-TID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *     ���
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    "CHANGE"            TO  MCP-PUTTYPE
                   MOVE    "2006"              TO  SPA-TIDCD
      *****        PERFORM 210-BACK
      *     ��
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 470-PREV-SYORI-SEC
      *     ��
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 470-NEXT-SYORI-SEC
      *     ����
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 410-NYUIN-SYORI-SEC
      *     ����
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 411-GAIRAI-SYORI-SEC
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        ���ʬ
               WHEN    "ACTIVATE"       ALSO    ANY
                   PERFORM 400-ENTRY-SYORI-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ʬ���Ͻ���
      *****************************************************************
       400-ENTRY-SYORI-SEC             SECTION.
      *
      *    MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *    MOVE    ZERO                TO  SPA-GMN-CUR
      *
      *    ���ϥ����å�����
      *    MOVE    ZERO                TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           .
       400-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϥ����å�����
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̤�ӣУ����åȽ���
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƽ���
      *****************************************************************
       470-PREV-SYORI-SEC           SECTION.
      *
           IF      SPA-NAI-PAGE        >   1
               COMPUTE SPA-NAI-PAGE    =   SPA-NAI-PAGE  -  1
               MOVE    SPA-NAI-PAGECNT (SPA-NAI-PAGE)
                                           TO  SPA-NAI-CNT
               COMPUTE SPA-NAI-PAGE    =   SPA-NAI-PAGE  -  1
               PERFORM 310-DATA-HENSYU-SEC
           ELSE
               MOVE    "0003"          TO  SPA-ERRCD
           END-IF
      *
           .
       470-PREV-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƽ���
      *****************************************************************
       470-NEXT-SYORI-SEC           SECTION.
      *
           IF      SPA-NAI-NEXT        =   1
               PERFORM 310-DATA-HENSYU-SEC
           ELSE
               MOVE    "0002"          TO  SPA-ERRCD
           END-IF
      *
           .
       470-NEXT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��������
      *****************************************************************
       410-NYUIN-SYORI-SEC           SECTION.
      *
           MOVE    "1"                 TO  SPA-GMN-SYORI
           MOVE    ZERO                TO  SPA-NAI-NEXT
           MOVE    ZERO                TO  SPA-NAI-CNT
           MOVE    ZERO                TO  SPA-NAI-PAGE
           MOVE    ZERO                TO  SPA-NAI-TOTALPAGE
           PERFORM 310-DATA-HENSYU-SEC
      *
           .
       410-NYUIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������
      *****************************************************************
       411-GAIRAI-SYORI-SEC           SECTION.
      *
           MOVE    "2"                 TO  SPA-GMN-SYORI
           MOVE    ZERO                TO  SPA-NAI-NEXT
           MOVE    ZERO                TO  SPA-NAI-CNT
           MOVE    ZERO                TO  SPA-NAI-PAGE
           MOVE    ZERO                TO  SPA-NAI-TOTALPAGE
           PERFORM 310-DATA-HENSYU-SEC
      *
           .
       411-GAIRAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ǡ�����������
      *****************************************************************
       470-COLLECT-SYORI-SEC      SECTION.
      *
      *    ����ֳ��ϥ��åȽ���
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID-COLLECT
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID-COLLECT
                                   TO  JOB-SHELLID
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID-COLLECT
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID-COLLECT
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           MOVE    SPACE           TO  SHELLTBL
           MOVE    "collectdata1.sh"   TO  SHELLTBL-NAME
           MOVE    SPA-TERMID      TO  SHELLTBL-ARG1
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG2
           MOVE    SPA-PREFNUM     TO  SHELLTBL-ARG3
      *****MOVE    SPA-NAI-SKYYM   TO  SHELLTBL-ARG4
           MOVE    SPA-BEDFLG      TO  SHELLTBL-ARG5
           MOVE    WRK-CONS-JOB-JOBID-COLLECT
                                   TO  SHELLTBL-ARG6
           MOVE    WRK-CONS-JOB-SHELLID-COLLECT
                                   TO  SHELLTBL-ARG7
           MOVE    "2"             TO  SHELLTBL-ARG8
           MOVE    SPA-HOSPID      TO  SHELLTBL-ARG9
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "monthly"       TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       470-COLLECT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ǡ�����������
      *****************************************************************
       470-COLLECT-DELETE-SEC     SECTION.
      *
           MOVE    ZERO                TO  IF-RESULT
           MOVE    SPA-HOSPNUM         TO  COLLECT-HOSPNUM
           MOVE    "g"                 TO  COLLECT-GN
           MOVE    COLLECT             TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                           ORCSFDELAREA
      *
           MOVE    ZERO                TO  IF-RESULT
           MOVE    SPA-HOSPNUM         TO  COLLECT-HOSPNUM
           MOVE    "n"                 TO  COLLECT-GN
           MOVE    COLLECT             TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                           ORCSFDELAREA
           .
       470-COLLECT-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           IF      SPA-NAI-MOTOPG      =   "M01"  OR  "M01N"
               MOVE    "M00"           TO  SPA-SAKIPG
           ELSE
             IF    SPA-NAI-MOTOPG      =   "T01"
               MOVE    SPA-MOTOPG2     TO  SPA-SAKIPG
             ELSE
               MOVE    SPA-NAI-MOTOPG  TO  SPA-SAKIPG
             END-IF
           END-IF
           MOVE    "T91"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  SPA-WORK
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������Ѵ��Խ�����
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG3
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
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
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-TIDCD       NOT =   SPACE
               PERFORM 520-TIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "T91    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE                      T91
      *
           MOVE    SPA-GMN-DATAVIEW    TO  T91-DATAVIEW
           MOVE    SPA-GMN-CNTHEN      TO  T91-CNTHEN
      *
           IF      SPA-GMN-SYORI       =   "2"
               MOVE    "�ڳ����"          TO  T91-MIDASI1
           ELSE
               MOVE    "��������"          TO  T91-MIDASI1
           END-IF
           IF      SPA-NAI-NEXT        =   1
               MOVE    "�Լ��ڡ��������"  TO  T91-MIDASI2
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̥������륻�åȽ���
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE)
           AND   ( SPA-GMN-CUR         =   ZERO )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
         EVALUATE    SPA-GMN-CUR
             WHEN    1
                 MOVE    "B10"       TO  MCP-WIDGET   
             WHEN    2
                 MOVE    "B07"       TO  MCP-WIDGET   
             WHEN    3
                 MOVE    "B11"       TO  MCP-WIDGET   
             WHEN    9
                 MOVE    "B01"       TO  MCP-WIDGET   
         END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϸĽ�Υ��åȽ���
      *****************************************************************
       50011-INPUT-CUR-SEC             SECTION.
      *
           MOVE    1               TO  SPA-GMN-CUR
           .
      *
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å�����ɽ������
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "ɽ������ǡ����Ϥ���ޤ���" 
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "���ǤϤ���ޤ���" 
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "���ǤϤ���ޤ���" 
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  TERR
           MOVE    SPA-ERRCD            TO  TERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  TERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "T91"                TO  SPA-MOTOPG
           MOVE    "TERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "TERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ɥ�å�����ɽ������
      *****************************************************************
       520-TIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-TIDMSG
      *
           EVALUATE    SPA-TIDCD
               WHEN    "2006"
                 MOVE  "����Ĵ�����������Фإ��åץ����ɤ��ޤ�����"
                                       TO  WRK-TIDMSG
               WHEN    "2008"
                 MOVE  "����Ĵ�������ǡ����������ޤ�����"
                                       TO  WRK-TIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  TID1
           INITIALIZE                       TID1
           MOVE    SPA-TIDCD            TO  TID1-ID1CODE
           MOVE    WRK-TIDMSG           TO  TID1-ID1MSG
      *
           MOVE    "������"             TO  TID1-B01-LABEL
           MOVE    "��  ��"             TO  TID1-B12-LABEL
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "T91"                TO  SPA-MOTOPG
           MOVE    "TID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "TID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
           .
       520-TIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ִ����ģ��������
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
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
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *