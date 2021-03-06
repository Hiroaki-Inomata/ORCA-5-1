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
       PROGRAM-ID.             ORCGI0SUB03.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���ౡ��Ͽ
      *  ����ݡ��ͥ��̾  : ʬ��������ư������å�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *****************************************************************
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
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-BUNBEN-NYUIN    PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
      *
      *    ����ΰ�
      *01  WRK-AREA.
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ��������
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    �����ƥ����
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1005.INC".
      *
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    �����ΰ�
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                         SECTION.
      *
       01  LNKPTNYUINRRK-REC.
           COPY  "CPPTNYUINRRK.INC"   REPLACING  //PTNYUINRRK-//
                                      BY         //LNKPTNYUINRRK-//.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                       DIVISION    USING
           LNKPTNYUINRRK-REC
           SPA-AREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *    INITIALIZE                  WRK-AREA
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    ZERO            TO  FLG-BUNBEN-NYUIN
      *
           PERFORM 900-PTNYUINRRK-KEY5-SEL-SEC
           DISPLAY "FLG-PTNYUINRRK =" FLG-PTNYUINRRK
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK      NOT =   ZERO )
                    OR   ( FLG-BUNBEN-NYUIN    NOT =   ZERO )
      *
               INITIALIZE                  SYSKANRI-REC
               MOVE    "1005"          TO  SYS-KANRICD
               MOVE    PTNYUINRRK-NYUINKA
                                       TO  SYS-KBNCD
               MOVE    PTNYUINRRK-NYUINYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-1005-REC
               DISPLAY "SYS-1005-RECESRYKA =" SYS-1005-RECESRYKA  
               IF    ( SYS-1005-RECESRYKA  =   "23" OR "24" )
                   MOVE    PTNYUINRRK-REC
                                       TO  LNKPTNYUINRRK-REC
                   MOVE    1           TO  FLG-BUNBEN-NYUIN
               END-IF
      *
               PERFORM 900-PTNYUINRRK-KEY5-FET-SEC
           DISPLAY "FLG-PTNYUINRRK =" FLG-PTNYUINRRK
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-BUNBEN-NYUIN    =   ZERO )
               INITIALIZE              LNKPTNYUINRRK-REC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                    SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������򸡺�����
      *****************************************************************
       900-PTNYUINRRK-KEY5-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    LNKPTNYUINRRK-REC   TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       900-PTNYUINRRK-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ��������FETCH����
      *****************************************************************
       900-PTNYUINRRK-KEY5-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       900-PTNYUINRRK-KEY5-FET-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ������������
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC  SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
      *    �ģ¸�������
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
