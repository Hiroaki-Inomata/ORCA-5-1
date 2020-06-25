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
       PROGRAM-ID.             ORCSDRCDGET.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : ��ե����ɼ�������
      *  ������            : 
      *  08/06/23    NACL��ƣ��    ��������
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
       DATA                        DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
           03  FLG-READ            PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-IN              PIC 9(06).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
       01  WRK-PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"
                                   REPLACING   //PTNYUINRRK//
                                   BY          //WRK-PTNYUINRRK//.
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "COMMON-SPA".
       01  ORCSDRCDGETAREA.
           COPY    "CPORCSDRCDGET.INC".
      *****************************************************************
     *
       PROCEDURE                    DIVISION    USING
                                    SPA-AREA
                                    ORCSDRCDGETAREA.
     *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1
      *
           PERFORM 300-END-SEC
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *     
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
                                       WRK-PTNYUINRRK-REC
      *
           .
       100-INIT-EXT.
           EXIT
           .
      *     
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC               SECTION.
      *
           IF    LNK-DRCDGET-I-NYUGAIKBN   =  "2"
               INITIALIZE                      JYURRK-REC
grpsys         MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
               MOVE    LNK-DRCDGET-I-SRYYM TO  JYURRK-SRYYMD(1:6)
               MOVE    "01"                TO  JYURRK-SRYYMD(7:)
               MOVE    LNK-DRCDGET-I-SRYYM TO  JYURRK-UPSRYYMD(1:6)
               MOVE    "31"                TO  JYURRK-UPSRYYMD(7:)
               MOVE    LNK-DRCDGET-I-NYUGAIKBN
                                           TO  JYURRK-NYUGAIKBN
               MOVE    LNK-DRCDGET-I-PTID  TO  JYURRK-PTID
               MOVE    JYURRK-REC          TO  MCPDATA-REC
               MOVE    "tbl_jyurrk"        TO  WRK-MCP-TABLE
               MOVE    "key43"             TO  WRK-MCP-PATHNAME
           ELSE
               INITIALIZE                      PTNYUINRRK-REC
grpsys         MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
               MOVE    LNK-DRCDGET-I-SRYYM TO  PTNYUINRRK-NYUINYMD (1:6)
               MOVE    "31"                TO  PTNYUINRRK-NYUINYMD (7:2)
               MOVE    LNK-DRCDGET-I-SRYYM TO  PTNYUINRRK-TAIINYMD (1:6)
               MOVE    "01"                TO  PTNYUINRRK-TAIINYMD (7:2)
               MOVE    LNK-DRCDGET-I-PTID  TO  PTNYUINRRK-PTID
               MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
               MOVE    "tbl_ptnyuinrrk"    TO  WRK-MCP-TABLE
               MOVE    "key23"             TO  WRK-MCP-PATHNAME
           END-IF    
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               MOVE    FLG-READ            TO  FLG-END
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
      *
           PERFORM UNTIL       FLG-END     =   1
               PERFORM 20011-DRCD-HENSYU-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    1               TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *            
      *****************************************************************
      *    ��ե����ɼ�������
      *****************************************************************
       20011-DRCD-HENSYU-SEC              SECTION.
      *
           IF    LNK-DRCDGET-I-NYUGAIKBN   =  "2"
               MOVE    MCPDATA-REC         TO  JYURRK-REC
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       10
                   IF      JYURRK-HKNCOMBINUM  =
                                           LNK-DRCDGET-I-HKNCOMBI (IDX)
                       MOVE    JYURRK-DRCD TO  LNK-DRCDGET-O-DRCD
                       MOVE    JYURRK-SRYKA
                                           TO  LNK-DRCDGET-O-DRCD-SRYKA
                       MOVE    10          TO  IDX
                       MOVE    1           TO  FLG-END
                   END-IF
               END-PERFORM
           ELSE            
               MOVE    MCPDATA-REC         TO  PTNYUINRRK-REC
               IF    ( PTNYUINRRK-TENNYUYMD (1:6)
                                           >   LNK-DRCDGET-I-SRYYM     )
               OR    (   ( PTNYUINRRK-TENNYUYMD
                                           >   LNK-DRCDGET-I-TEKEDYMD )
                      AND( LNK-DRCDGET-I-TEKEDYMD
                                       NOT =   SPACE                  ))
                   CONTINUE
               ELSE
      *            �ǿ�����������������
                   IF    ( PTNYUINRRK-NYUINYMD >=
                                               LNK-DRCDGET-I-NYUINYMD )
                   AND   ( WRK-PTNYUINRRK-NYUINYMD
                                               =   SPACE              )
                       MOVE    PTNYUINRRK-REC  TO  WRK-PTNYUINRRK-REC
                   END-IF
      *
      *???
      **                 DISPLAY "RRK="  PTNYUINRRK-HKNCOMBINUM  "#"
      **                 DISPLAY "LNK="  LNK-DRCDGET-I-HKNCOMBI-OCC  "#"
      *???
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       10
                       IF      PTNYUINRRK-HKNCOMBINUM  =
                                           LNK-DRCDGET-I-HKNCOMBI (IDX)
                           IF      PTNYUINRRK-DRCD1    NOT =   SPACE
                               MOVE    PTNYUINRRK-DRCD1
                                               TO  LNK-DRCDGET-O-DRCD
                           ELSE
                               IF      PTNYUINRRK-DRCD2    NOT =   SPACE
                                   MOVE    PTNYUINRRK-DRCD2
                                               TO  LNK-DRCDGET-O-DRCD
                               ELSE
                                   MOVE    PTNYUINRRK-DRCD3
                                               TO  LNK-DRCDGET-O-DRCD
                               END-IF
                           END-IF
                           MOVE    PTNYUINRRK-NYUINKA
                                           TO  LNK-DRCDGET-O-DRCD-SRYKA
                           MOVE    10      TO  IDX
                           MOVE    1       TO  FLG-END
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               MOVE    FLG-READ            TO  FLG-END
           END-IF
      *
      *    �������ݸ���������Ͽ���Ƥ��ʤ��Ȥ�
           IF    ( FLG-END             =   1     )
           AND   ( LNK-DRCDGET-I-NYUGAIKBN
                                       =   "1"   )
           AND   ( LNK-DRCDGET-O-DRCD-SRYKA
                                       =   SPACE )
      *???
      **         DISPLAY "WRK=" WRK-PTNYUINRRK-NYUINKA "#"
      *???
               IF      WRK-PTNYUINRRK-DRCD1    NOT =   SPACE
                   MOVE    WRK-PTNYUINRRK-DRCD1
                                               TO  LNK-DRCDGET-O-DRCD
               ELSE
                   IF      WRK-PTNYUINRRK-DRCD2    NOT =   SPACE
                       MOVE    WRK-PTNYUINRRK-DRCD2
                                               TO  LNK-DRCDGET-O-DRCD
                   ELSE
                       MOVE    WRK-PTNYUINRRK-DRCD3
                                               TO  LNK-DRCDGET-O-DRCD
                   END-IF
               END-IF
               MOVE    WRK-PTNYUINRRK-NYUINKA
                                           TO  LNK-DRCDGET-O-DRCD-SRYKA
           END-IF
           .
       20011-DRCD-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "DRCD="  LNK-DRCDGET-O-DRCD
                            "#"  LNK-DRCDGET-O-DRCD-SRYKA
      *
           .
       300-END-EXT.
           EXIT. 
      *
      *****************************************************************
      *    �ģ£ӣţ̣ţãԽ���
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ£ƣţԣãȽ���
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-READ
           ELSE
               MOVE    1                   TO  FLG-READ
           END-IF
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륯����������
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
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