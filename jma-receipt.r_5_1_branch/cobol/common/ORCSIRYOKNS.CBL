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
       PROGRAM-ID.                 ORCSIRYOKNS.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : ���Ŵѻ������������ɼ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  10/08/24    NACL�ݾ�Ʀ��  ��������
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
           03  FLG-SYSKANRI                PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK5001.INC".
      *
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
      *    ���Ŵѻ������������ɼ����ѥ�᥿
           COPY    "CPORCSIRYOKNS.INC".
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSIRYOKNS-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *
           PERFORM    200-EDIT-SEC
           .
      *
       000-PROC-EXT.
           EXIT   PROGRAM
           .
      *
      *****************************************************************
      *    ���Ŵѻ������������ɥ��å�
      *****************************************************************
       200-EDIT-SEC                    SECTION.
      *
           INITIALIZE                  LNK-IRYOKNS-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-IRYOKNS-RC
           MOVE    ZERO                TO  IDX
      *
           IF  LNK-IRYOKNS-KBN      =      "4"
      *
      *(8801-0)������ش������Υ��å�
             EVALUATE    LNK-IRYOKNS-NYUIN
      *        �����������оݼ�������ش�����
               WHEN    "01"
               WHEN    "02"
               WHEN    "03"
                       ADD     1            TO  IDX
                       MOVE   "188000110"   TO  LNK-IRYOKNS-SRYCD(IDX)
      *        �����������оݼ�������ش�����
               WHEN    "04"
               WHEN    "11"
               WHEN    "18"
                       ADD     1            TO  IDX
                       MOVE   "188000510"   TO  LNK-IRYOKNS-SRYCD(IDX)
      *        �Ҳ������������оݼ�������ش�����
               WHEN    "05"
               WHEN    "06"
               WHEN    "07"
               WHEN    "08"
               WHEN    "09"
               WHEN    "10"
               WHEN    "12"
               WHEN    "13"
               WHEN    "14"
               WHEN    "15"
               WHEN    "16"
               WHEN    "17"
                       ADD     1            TO  IDX
                       MOVE   "188000610"   TO  LNK-IRYOKNS-SRYCD(IDX)
             END-EVALUATE
      *
      *
      *(8801-1)̤��ฺ��������˷�������������ʤ����ˤΥ��å�
             INITIALIZE                   SYSKANRI-REC
             MOVE    LNK-IRYOKNS-HOSPNUM
                                      TO  SYS-HOSPNUM
             MOVE    "5001"           TO  SYS-KANRICD
             MOVE    LNK-IRYOKNS-BTUNUM
                                      TO  SYS-KBNCD
             MOVE    LNK-IRYOKNS-SANTEIYM
                                      TO  SYS-STYUKYMD(1:6)
             MOVE    "01"             TO  SYS-STYUKYMD(7:2)
             MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
             MOVE    "tbl_syskanri"   TO  MCP-TABLE
             MOVE    "key10"          TO  MCP-PATHNAME
             PERFORM  910-SYSKANRI-SELECT-SEC
             IF      FLG-SYSKANRI     =   ZERO
               MOVE    "tbl_syskanri" TO  MCP-TABLE
               MOVE    "key10"        TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF     FLG-SYSKANRI     =   ZERO
                   PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL (IDX1     >   14)   OR
                            (SYS-5001-KSNCD(IDX1)  =   SPACE)
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188000270") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188000270"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
                   END-PERFORM
               END-IF
             END-IF
      *
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
      *
      *
      *(8801-2)�����������оݼ�������ش�������̤Ŭ�縺���Υ��å�
             EVALUATE    LNK-IRYOKNS-NYUIN
               WHEN    "02"
      *                ̤Ŭ�縺�����������������飹�����ʾ壱ǯ�����
                       ADD     1            TO  IDX
                       MOVE   "188000370"   TO  LNK-IRYOKNS-SRYCD(IDX)
               WHEN    "03"
      *                ̤Ŭ�縺�����������������飱ǯĶ��
                       ADD     1            TO  IDX
                       MOVE   "188000470"   TO  LNK-IRYOKNS-SRYCD(IDX)
             END-EVALUATE
      *
      *
      *(8801-2)�ѻ������������Υ��åȡʻ��곫�������飲�������ʾ��
             EVALUATE    LNK-IRYOKNS-NYUIN
               WHEN    "11"
                       ADD     1            TO  IDX
                       MOVE   "188006570"   TO  LNK-IRYOKNS-SRYCD(IDX)
             END-EVALUATE
      *
      *
      *(8801-2)�ѻ������������Υ��åȡʻ��곫�������飱ǯ������Ķ��
             EVALUATE    LNK-IRYOKNS-NYUIN
               WHEN    "18"
                       ADD     1            TO  IDX
                       MOVE   "188010170"   TO  LNK-IRYOKNS-SRYCD(IDX)
             END-EVALUATE
      *
      *
      *(8801-2)�ѻ��Ҳ������������Υ��å�
             EVALUATE    LNK-IRYOKNS-NYUIN
               WHEN    "06"
               WHEN    "15"
      *                �ѻ��Ҳ������������ʻ��곫�������飱�������ʾ�
      *                                    ��ǯ�����
                       ADD     1            TO  IDX
                       MOVE   "188000770"   TO  LNK-IRYOKNS-SRYCD(IDX)
               WHEN    "07"
      *                �ѻ��Ҳ������������ʻ��곫�������飱ǯĶ
      *                                    ��ǯ�������������
                       ADD     1            TO  IDX
                       MOVE   "188000870"   TO  LNK-IRYOKNS-SRYCD(IDX)
               WHEN    "08"
      *                �ѻ��Ҳ������������ʻ��곫�������飱ǯ��������Ķ��
                       ADD     1            TO  IDX
                       MOVE   "188005870"   TO  LNK-IRYOKNS-SRYCD(IDX)
               WHEN    "16"
      *                �ѻ��Ҳ������������ʻ��곫�������飱ǯĶ
      *                                    ��ǯ�����������⡦����ϲû���
                       ADD     1            TO  IDX
                       MOVE   "188009170"   TO  LNK-IRYOKNS-SRYCD(IDX)
               WHEN    "17"
      *                �ѻ��Ҳ������������ʻ��곫�������飱ǯ��������Ķ
      *                                    ������ϲû���
                       ADD     1            TO  IDX
                       MOVE   "188009270"   TO  LNK-IRYOKNS-SRYCD(IDX)
             END-EVALUATE
      *
      *
      *(8801-2)̤�вḺ���Υ��å�
             EVALUATE    LNK-IRYOKNS-NYUIN
               WHEN    "09"
      *                ̤�вḺ���ʻ��곫������ǯĶ���ౡ���Ĥο�Ω��
      *                            �������������
                       ADD     1            TO  IDX
                       MOVE   "188000970"   TO  LNK-IRYOKNS-SRYCD(IDX)
               WHEN    "10"
      *                ̤�вḺ���ʻ��곫������ǯ��������Ķ���ౡ���Ĥο�Ω��
      *                        �������������
                       ADD     1            TO  IDX
                       MOVE   "188005970"   TO  LNK-IRYOKNS-SRYCD(IDX)
             END-EVALUATE
      *
      *
      *(8801-3)�Ҳ��������ܹԲû��Υ��å�
             EVALUATE    LNK-IRYOKNS-NYUIN
               WHEN    "12"
               WHEN    "14"
                       ADD     1            TO  IDX
                       MOVE   "188006670"   TO  LNK-IRYOKNS-SRYCD(IDX)
             END-EVALUATE
      *
      *
      *(8801-4)����ϲû��Υ��å�
             EVALUATE    LNK-IRYOKNS-NYUIN
               WHEN    "13"
               WHEN    "14"
               WHEN    "15"
                       ADD     1            TO  IDX
                       MOVE   "188009070"   TO  LNK-IRYOKNS-SRYCD(IDX)
             END-EVALUATE
      *
      *
      *(8801-5)�����������¾����û��Υ��å�
             INITIALIZE                   SYSKANRI-REC
             MOVE    LNK-IRYOKNS-HOSPNUM
                                      TO  SYS-HOSPNUM
             MOVE    "5001"           TO  SYS-KANRICD
             MOVE    LNK-IRYOKNS-BTUNUM
                                      TO  SYS-KBNCD
             MOVE    LNK-IRYOKNS-SANTEIYM
                                      TO  SYS-STYUKYMD(1:6)
             MOVE    "01"             TO  SYS-STYUKYMD(7:2)
             MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
             MOVE    "tbl_syskanri"   TO  MCP-TABLE
             MOVE    "key10"          TO  MCP-PATHNAME
             PERFORM  910-SYSKANRI-SELECT-SEC
             IF      FLG-SYSKANRI     =   ZERO
               MOVE    "tbl_syskanri" TO  MCP-TABLE
               MOVE    "key10"        TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF     FLG-SYSKANRI     =   ZERO
                   PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL (IDX1     >   14)   OR
                            (SYS-5001-KSNCD(IDX1)  =   SPACE)
      *                 �����������¾����û��ʣ�������
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188001070") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188001070"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 �����������¾����û��ʣ�������
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188001170") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188001170"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 �����������¾����û��ʣ�������
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188001270") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188001270"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 �����������¾����û��ʣ�������
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188001370") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188001370"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 �����������¾����û��ʣ�������
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188001470") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188001470"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 �����������¾����û��ʣ�������
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188001570") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188001570"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 �����������¾����û��ʣ�������
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188001670") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188001670"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 �����������¾����û��ʣ�������
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188001770") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188001770"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 �����������¾����û��ʣ�������
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188001870") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188001870"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 �����������¾����û��ʣ�������
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188001970") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188001970"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 �����������¾����û��ʣ�������
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188002070") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188002070"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 �����������¾����û��ʣ�������
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188002170") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188002170"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 �����������¾����û��ʣ�������
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188002270") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188002270"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 �����������¾����û��ʣ�������
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188002370") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188002370"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 �����������¾����û��ʣ�������
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188002470") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188002470"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
                   END-PERFORM
               END-IF
             END-IF
      *
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
      *
           END-IF
      *
      *
           IF    LNK-IRYOKNS-SRYCD(1)    =   SPACE
               MOVE    1                 TO  LNK-IRYOKNS-RC
           END-IF
      *
           .
       200-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƥ�����ޥ���������
      *****************************************************************
       910-SYSKANRI-SELECT-SEC        SECTION.
      *
           MOVE      SYSKANRI-REC         TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC                 =   ZERO
               MOVE    ZERO               TO  FLG-SYSKANRI
           ELSE
               MOVE    1                  TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƥ�����ޥ������ɹ�
      *****************************************************************
       910-SYSKANRI-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  SYS-5001-REC
               MOVE    ZERO               TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                     SYS-5001-REC
               MOVE    1                  TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ£ӣţ̣ţãԽ���
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
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
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륢����������
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
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