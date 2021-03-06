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
       PROGRAM-ID.             ORCSPRT.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : Ģɼ����
      *  ����ݡ��ͥ��̾  : Ģɼ���������������ģ�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  02/04/16    MCC��ƣ����   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-ƣ��    02/10/10  �����ȶ�ʬ�ɲ�
      *  01.00.02    NACL-ƣ��    03/03/03  �����ѥե饰�ɲ�
      *  01.01.02    NACL-ƣ��    03/03/04  �����ģ¤ؤΥץ��̾�ɲäˤ��
      *                                     ����
      *  01.03.01    NACL-ƣ��    03/07/07  �ץ�ӥ塼��ǽ�ɲ�
      *                                     ���ˤ��ɲ�
      *
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *
      *  04.01.01    NACL-ƣ��    07/11/22  �����쥻�ץ��б�
      *
      *  04.07.01    NACL-ƣ��    13/09/20  �軰�Թ԰��б�
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
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PRTKANRI            PIC 9(01).
           03  FLG-PRTDATA             PIC 9(01). 
      *
           COPY    "ENUM-VALUE".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ���������ģ�
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    �����ģ�
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    ����ǥ��쥯�ȥ��������
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    �����Ѵ��ΰ�
           COPY    "CPORCSNUM.INC".
      *   �����ֹ��Ѵ�����
           COPY    "CPORCSPTID.INC".
      *
      *    �ե�����ǥ��쥯�ȥ���ֻ��ꥵ��
           COPY    "CPMKPASS.INC".
      *
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *     
      *****************************************************************
      *
       LINKAGE                 SECTION.
      *   �����ģ����極��
           COPY    "CPORCSPRT.INC".
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
      *****************************************************************
       PROCEDURE           DIVISION
               USING
               ORCSPRTAREA
               SPA-AREA
               .
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *    SPA-MW���ͤ����
           INITIALIZE                  SGETTEMP-AREA
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE     SGETTEMP-MW        TO  SPA-MW
      *
           IF      SPRT-GYOUMU-CD  NOT NUMERIC
                   MOVE  99            TO  SPRT-GYOUMU-CD
           END-IF
      *    �����⡼�ɤΥ����å�
           EVALUATE    SPRT-MODE
               WHEN    "INS"
                       PERFORM 110-INS-SEC
               WHEN    "DEL"
                       PERFORM 140-DEL-SEC
               WHEN    OTHER
                       MOVE  10            TO  SPRT-RETURN  
           END-EVALUATE
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *****************************************************************
      *    �����ģ��ɲý���
      *****************************************************************
       110-INS-SEC                 SECTION.
      *
           PERFORM 200-PRTKANRI-HENSYU-SEC
      *
           IF      MCP-RC          =   ZERO
               PERFORM 210-PRTDATA-HENSYU-SEC
           END-IF         
      *
           IF      MCP-RC          =   ZERO
               CONTINUE
           ELSE
               MOVE  MCP-RC            TO  SPRT-RETURN
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
      *
           .
       140-DEL-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ���������ģ¹�������
      *****************************************************************
       200-PRTKANRI-HENSYU-SEC         SECTION.
      *
      *
           MOVE    SPACE               TO  PRTKANRI-REC
           INITIALIZE                      PRTKANRI-REC
           MOVE    SPRT-TBL-KEY        TO  PRTKANRI-TBL-KEY
           MOVE    SPRT-RENNUM         TO  PRTKANRI-RENNUM
           MOVE    SPRT-TBL-GROUP      TO  PRTKANRI-TBL-GROUP
           MOVE    SPRT-TBL-UUID       TO  PRTKANRI-TBL-UUID
           MOVE    SPRT-GYOUMU-CD      TO  PRTKANRI-GYOUMU-CD
           MOVE    SPRT-SHORI-RENNUM   TO  PRTKANRI-SHORI-RENNUM
grpsys     MOVE    SPA-HOSPNUM         TO  PRTKANRI-HOSPNUM
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           PERFORM 910-PRTKANRI-INV-SEC
           IF      FLG-PRTKANRI        =  ZERO
               ADD     1                   TO  PRTKANRI-PAGE
           ELSE
               MOVE    SPACE               TO  PRTKANRI-REC
               INITIALIZE                      PRTKANRI-REC
               MOVE    SPRT-TBL-KEY        TO  PRTKANRI-TBL-KEY
               MOVE    SPRT-RENNUM         TO  PRTKANRI-RENNUM
               MOVE    SPRT-TBL-GROUP      TO  PRTKANRI-TBL-GROUP
               MOVE    SPRT-TBL-UUID       TO  PRTKANRI-TBL-UUID
               MOVE    SPRT-GYOUMU-CD      TO  PRTKANRI-GYOUMU-CD
               MOVE    SPRT-SHELLID        TO  PRTKANRI-SHELLID
               MOVE    SPRT-SHORI-RENNUM   TO  PRTKANRI-SHORI-RENNUM
grpsys         MOVE    SPA-HOSPNUM         TO  PRTKANRI-HOSPNUM
               MOVE    SPRT-SKYYMD         TO  PRTKANRI-SKYYMD
      * �� �ץ�ӥ塼�б�
      *        MOVE    SPRT-SRYYM          TO  PRTKANRI-SRYYM
      *        MOVE    SPRT-PRIORITY       TO  PRTKANRI-PRIORITY
               EVALUATE    SPRT-PRIORITY
                   WHEN    11
                   WHEN    21
                   WHEN    31
                   WHEN    41
                   WHEN    51
                   WHEN    61
                   WHEN    71
                   WHEN    81
                   WHEN    91
                   WHEN    101
                           COMPUTE PRTKANRI-PRIORITY  =   SPRT-PRIORITY 
                                                      /   10
                           MOVE    SPRT-SRYYM         TO  PRTKANRI-SRYYM
                   WHEN    12
                   WHEN    22
                   WHEN    32
                   WHEN    42
                   WHEN    52
                   WHEN    62
                   WHEN    72
                   WHEN    82
                   WHEN    92
                   WHEN    102
                           COMPUTE PRTKANRI-PRIORITY  =   SPRT-PRIORITY 
                                                      /   10
                           MOVE    ZERO               TO  PRTKANRI-SRYYM
                   WHEN    OTHER
                           MOVE    SPRT-SRYYM         TO  PRTKANRI-SRYYM
                           MOVE    SPRT-PRIORITY      TO  
                                                      PRTKANRI-PRIORITY
               END-EVALUATE
      * �� end
               MOVE    1                   TO  PRTKANRI-PAGE
               IF      SPRT-PRTFLG         =   "1" OR "D"
                   MOVE    SPRT-PRTFLG         TO  PRTKANRI-PRTFLG
               ELSE
                   MOVE    SPACE               TO  PRTKANRI-PRTFLG
               END-IF
               MOVE    SPRT-TITLE          TO  PRTKANRI-TITLE
               MOVE    SPRT-TERMID         TO  PRTKANRI-TERMID
               MOVE    SPRT-OPID           TO  PRTKANRI-OPID
               MOVE    SPRT-PRTNM          TO  PRTKANRI-PRTNM
               MOVE    SPRT-API-ID         TO  PRTKANRI-API-ID
           END-IF
      *
           MOVE    SPRT-TBL-GROUP(1:8) TO  PRTKANRI-CREYMD
      *
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-PRTKANRI        =   ZERO     
               MOVE    "DBUPDATE"          TO  MCP-FUNC
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC     =   ZERO
                   MOVE  MCP-RC            TO  SPRT-RETURN
               ELSE
                   DISPLAY "PRTKANRI UPDATE ERR KEY = ["
                                      MCP-RC  " " PRTKANRI-KEY  "]"   
                   MOVE  MCP-RC            TO  SPRT-RETURN
               END-IF
           ELSE
               MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC     =   ZERO
                   MOVE  MCP-RC            TO  SPRT-RETURN
               ELSE
                   DISPLAY "PRTKANRI INSERT ERR KEY = ["
                                      MCP-RC  " " PRTKANRI-KEY  "]"   
                   MOVE  MCP-RC            TO  SPRT-RETURN
               END-IF
           END-IF    
      *
      *
           .
       200-PRTKANRI-HENSYU-EXT.
           EXIT.
      *
      * 
      *****************************************************************
      *    �����ģ¹�������
      *****************************************************************
       210-PRTDATA-HENSYU-SEC          SECTION.
      * 
           MOVE    SPACE               TO  PRTDATA-REC
           INITIALIZE                      PRTDATA-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PRTDATA-HOSPNUM
           MOVE    SPRT-TBL-KEY        TO  PRTDATA-TBL-KEY
           MOVE    SPRT-RENNUM         TO  PRTDATA-RENNUM
           MOVE    SPRT-TBL-GROUP      TO  PRTDATA-TBL-GROUP
           MOVE    SPRT-TBL-UUID       TO  PRTDATA-TBL-UUID
           MOVE    SPRT-GYOUMU-CD      TO  PRTDATA-GYOUMU-CD
           MOVE    SPRT-SHELLID        TO  PRTDATA-SHELLID
           MOVE    SPRT-PRTID          TO  PRTDATA-PRTID
           MOVE    SPRT-SHORI-RENNUM   TO  PRTDATA-SHORI-RENNUM
           MOVE    SPRT-SKYYMD         TO  PRTDATA-SKYYMD
           MOVE    SPRT-PTID           TO  PRTDATA-PTID 
      * �� �ץ�ӥ塼�б�
      *    MOVE    SPRT-SRYYM          TO  PRTDATA-SRYYM
      *    MOVE    SPRT-PRIORITY       TO  PRTDATA-PRIORITY
               EVALUATE    SPRT-PRIORITY
                   WHEN    11
                   WHEN    21
                   WHEN    31
                   WHEN    41
                   WHEN    51
                   WHEN    61
                   WHEN    71
                   WHEN    81
                   WHEN    91
                   WHEN    101
                           COMPUTE PRTDATA-PRIORITY   =   SPRT-PRIORITY 
                                                      /   10
                           MOVE    SPRT-SRYYM         TO  PRTDATA-SRYYM
                   WHEN    12
                   WHEN    22
                   WHEN    32
                   WHEN    42
                   WHEN    52
                   WHEN    62
                   WHEN    72
                   WHEN    82
                   WHEN    92
                   WHEN    102
                           COMPUTE PRTDATA-PRIORITY   =   SPRT-PRIORITY 
                                                      /   10
                           MOVE    SPRT-SRYYM         TO  PRTDATA-SRYYM
                   WHEN    OTHER
                           MOVE    SPRT-SRYYM         TO  PRTDATA-SRYYM
                           MOVE    SPRT-PRIORITY      TO  
                                                      PRTDATA-PRIORITY
               END-EVALUATE
      * �� end
           MOVE    PRTKANRI-PAGE       TO  PRTDATA-PAGE
           IF      SPRT-PRTFLG         =   "1" OR "D"
               MOVE    SPRT-PRTFLG         TO  PRTDATA-PRTFLG
           ELSE
               MOVE    SPACE               TO  PRTDATA-PRTFLG
           END-IF
           IF      SPRT-SITEKBN        =   SPACE
               MOVE    "0"                 TO  PRTDATA-SITEKBN
           ELSE    
               MOVE    SPRT-SITEKBN        TO  PRTDATA-SITEKBN
           END-IF    
           MOVE    SPRT-PRTNM          TO  PRTDATA-PRTNM
           MOVE    FUNCTION LOWER-CASE (SPRT-DATA-TYPE)
                                       TO  PRTDATA-DATA-TYPE
      *
           IF    ( SPA-MW              =   SPA-GINBEE )
            AND  ( SPRT-SITEKBN        =   "2" )
               MOVE    "pdf"           TO  PRTDATA-DATA-TYPE
           END-IF
      *
           MOVE    SPRT-API-ID         TO  PRTDATA-API-ID
      *
           IF    ( PRTDATA-DATA-TYPE   =   "pdf" )
               CALL    "ORCSPDF"       USING
                                       ORCSPRTAREA
                                       SPA-AREA
               MOVE    SPRT-PDF        TO  PRTDATA-PRTDATA
           ELSE
               MOVE    SPRT-PRTDATA    TO  PRTDATA-PRTDATA
           END-IF
      *
           MOVE    PRTDATA-REC         TO  MCPDATA-REC
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *                                      
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SPRT-RETURN
           ELSE
               DISPLAY "PRTDATA INSERT ERR KEY = ["
                                  MCP-RC  " " PRTDATA-KEY  "]"   
               MOVE  MCP-RC            TO  SPRT-RETURN
           END-IF
      *
           .
       210-PRTDATA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������ޥ����ɹ�
      *****************************************************************
       910-PRTKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_prtkanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PRTKANRI-REC
                   MOVE    ZERO                TO  FLG-PRTKANRI
               ELSE
                   MOVE    1                   TO  FLG-PRTKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PRTKANRI
           END-IF
      *
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-PRTKANRI-INV-EXT.
           EXIT.
           .
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
