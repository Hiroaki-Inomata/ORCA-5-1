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
       PROGRAM-ID.                 ORCSAKYUSEI.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : ���������������Ŵ�����Ƚ�ꥵ��
      *      ʿ������ǯ������������˰��������������Ŵ�����������
      *      ���Ƥ�����֤�Ƚ�ꤹ�륵�֥ץ�
      *                      
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  12/03/17    NACL�ݾ�Ʀ��  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
           03  IDX2                        PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
      *
      *
      *    ���Ŷ�ʬ�������¤��ؤ���
           03  WRK-IDX                     PIC 9(04).
           03  WRK-RYOCOM-IRYO-SRYCD       PIC X(09).
           03  WRK-RYOCOM-IRYO-ZAINUM      PIC 9(08).
      *
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ������������
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    ���ԥޥ�����
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *    ���ܥ쥻�����ȼ����ѥ�᥿
           COPY    "CPORCSAKYUSEI.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSAKYUSEI-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           MOVE    ZERO                TO  LNK-AKYUSEI-RC
      *
      *    �ѥ�᡼�������å�
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-AKYUSEI-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    �����
           IF      LNK-AKYUSEI-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    �ѥ�᡼�������å�
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    ���ԥޥ���������
           INITIALIZE                       PTINF-REC
           MOVE    LNK-AKYUSEI-HOSPNUM  TO  PTINF-HOSPNUM
           MOVE    LNK-AKYUSEI-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    9               TO  LNK-AKYUSEI-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *    �������鴵����������򸡺�����ʿ������ǯ�������������
      *    ���������������Ŵ������λ���̵ͭ���������
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    LNK-AKYUSEI-HOSPNUM TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-AKYUSEI-PTID    TO  PTNYUINRRK-PTID
           MOVE    LNK-AKYUSEI-RRKNUM  TO  PTNYUINRRK-RRKNUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM  910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK      =   ZERO
               MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               PERFORM  910-PTNYUINRRK-READ-SEC
               IF    FLG-PTNYUINRRK    =   ZERO
                    PERFORM   UNTIL  FLG-PTNYUINRRK    =   1
                        IF  (PTNYUINRRK-TENNYUYMD   <=  "20120331") AND
                            (PTNYUINRRK-TENSTUYMD   >   "20120331")
                          IF   (PTNYUINRRK-TOKUTEI-KBN    =  "3")  AND
                               (PTNYUINRRK-TOKUTEI-NYUIN  =  "18")
                             MOVE    1     TO   LNK-AKYUSEI-RC
                          END-IF
                          IF   (PTNYUINRRK-TOKUTEI-KBN    =  "3")  AND
                               (PTNYUINRRK-TOKUTEI-NYUIN  =  "54")
                             MOVE    2     TO   LNK-AKYUSEI-RC
                          END-IF
                        END-IF
                        MOVE    "tbl_ptnyuinrrk"  TO  MCP-TABLE
                        MOVE    "key4"            TO  MCP-PATHNAME
                        PERFORM 910-PTNYUINRRK-READ-SEC
                    END-PERFORM
               ELSE
                   MOVE     9      TO  LNK-AKYUSEI-RC
               END-IF
           END-IF
      *
      *    �����������򥯥�����
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ԥޥ������ɹ�
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"   USING MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL "ORCDBMAIN" USING MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTINF-REC
                   MOVE    ZERO        TO  FLG-PTINF
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE    1           TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������������ޥ���������
      *****************************************************************
       910-PTNYUINRRK-SELECT-SEC        SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"   USING MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������������ޥ������ɹ�
      *****************************************************************
       910-PTNYUINRRK-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"   USING MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTNYUINRRK-REC
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                     PTNYUINRRK-REC
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"    USING MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.