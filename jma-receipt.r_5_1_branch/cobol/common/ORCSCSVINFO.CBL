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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSCSVINFO.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �ãӣִ���
      *  ����ݡ��ͥ��̾  : �ãӣ֡��ɣΣƣ�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  07/08/01    NaCl��ƣ��    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-CSV-INFO            PIC 9(01). 
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
       01  CSV-INFO-REC.
           COPY    "CPCSV-INFO.INC".          
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *     
      ******************************************************************
       LINKAGE                 SECTION.
      *
      *   �����ģ����極��
           COPY    "CPORCSCSVINFO.INC".
      *
           COPY    "COMMON-SPA".
      *
      ******************************************************************
       PROCEDURE           DIVISION
               USING
               ORCSCSVINFOAREA
               SPA-AREA
               .
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *    �����⡼�ɤΥ����å�
           EVALUATE    SCSVINFO-MODE
               WHEN    "INS"
                       PERFORM 110-INS-SEC
               WHEN    "DEL"
                       PERFORM 140-DEL-SEC
               WHEN    OTHER
                       MOVE  10            TO  SCSVINFO-RETURN  
           END-EVALUATE
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *****************************************************************
      *    �ãӣ֡��ɣΣƣ��ɲý���
      *****************************************************************
       110-INS-SEC                 SECTION.
      *
           PERFORM 210-CSV-INFO-HENSYU-SEC
      *
           IF      MCP-RC          =   ZERO
               CONTINUE
           ELSE
               MOVE  MCP-RC            TO  SCSVINFO-RETURN
           END-IF
      *
           .
       110-INS-EXT.
           EXIT.
      * 
      *****************************************************************
      *    �������
      *****************************************************************
       140-DEL-SEC                 SECTION.
      *
           MOVE    SPACE               TO  CSV-INFO-REC
           INITIALIZE                      CSV-INFO-REC
           MOVE    SPA-HOSPNUM         TO  CSV-INFO-HOSPNUM
           MOVE    SCSVINFO-TBL-KEY    TO  CSV-INFO-TBL-KEY
           MOVE    SCSVINFO-SHELLID    TO  CSV-INFO-SHELLID
           MOVE    SCSVINFO-SRYYM      TO  CSV-INFO-SRYYM
      *
           MOVE    CSV-INFO-REC        TO  MCPDATA-REC
           MOVE    "tbl_csv_info"      TO  MCP-TABLE
           IF    ( SCSVINFO-SHELLID    =   SPACE )
           AND   ( SCSVINFO-SRYYM      =   SPACE )
               MOVE    "del1"               TO  MCP-PATHNAME
           ELSE
               IF    ( SCSVINFO-SHELLID    =   SPACE )
                   MOVE    "del2"               TO  MCP-PATHNAME
               ELSE
                   MOVE    "del3"               TO  MCP-PATHNAME
               END-IF 
           END-IF 
           MOVE    "DBDELETE"           TO  MCP-FUNC
            PERFORM 900-ORCDBMAIN-SEC
      *                                      
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SCSVINFO-RETURN
           ELSE
               DISPLAY "CSV-INFO DELETE ERR KEY = ["
                                  MCP-RC  " " CSV-INFO-KEY  "]"   
               MOVE  MCP-RC            TO  SCSVINFO-RETURN
           END-IF
      *
      *
           .
       140-DEL-EXT.
           EXIT.
      * 
      *****************************************************************
      *    �ãӣ֡��ɣΣƣϹ�������
      *****************************************************************
       210-CSV-INFO-HENSYU-SEC          SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      * 
           MOVE    SPACE               TO  CSV-INFO-REC
           INITIALIZE                      CSV-INFO-REC
           MOVE    SPA-HOSPNUM         TO  CSV-INFO-HOSPNUM
           MOVE    SCSVINFO-TBL-KEY    TO  CSV-INFO-TBL-KEY
           MOVE    SCSVINFO-SHELLID    TO  CSV-INFO-SHELLID
           MOVE    SCSVINFO-SHORI-RENNUM
                                       TO  CSV-INFO-SHORI-RENNUM
           MOVE    SCSVINFO-RENNUM     TO  CSV-INFO-RENNUM
           MOVE    SCSVINFO-SEQ-NO     TO  CSV-INFO-SEQ-NO
           MOVE    SCSVINFO-NYUGAIKBN  TO  CSV-INFO-NYUGAIKBN
           MOVE    SCSVINFO-SRYYM      TO  CSV-INFO-SRYYM
           MOVE    SCSVINFO-SKYYMD     TO  CSV-INFO-SKYYMD
           MOVE    SCSVINFO-TEISYUTUSAKI
                                       TO  CSV-INFO-TEISYUTUSAKI
           MOVE    SCSVINFO-PTID       TO  CSV-INFO-PTID 
           MOVE    SCSVINFO-CSVDATA    TO  CSV-INFO-CSVDATA
           MOVE    SMCNDATE-YMD        TO  CSV-INFO-CREYMD
           MOVE    SMCNDATE-HMS        TO  CSV-INFO-CREHMS 
      *
           MOVE    CSV-INFO-REC         TO  MCPDATA-REC
           MOVE    "tbl_csv_info"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBINSERT"          TO  MCP-FUNC
            PERFORM 900-ORCDBMAIN-SEC
      *                                      
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SCSVINFO-RETURN
           ELSE
               DISPLAY "CSV-INFO INSERT ERR KEY = ["
                                  MCP-RC  " " CSV-INFO-KEY  "]"   
               MOVE  MCP-RC            TO  SCSVINFO-RETURN
           END-IF
      *
           .
       210-CSV-INFO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ£ӣţ̣ţãԽ���
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
