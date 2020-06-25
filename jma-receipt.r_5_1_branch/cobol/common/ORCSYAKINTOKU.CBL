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
       PROGRAM-ID.                 ORCSYAKINTOKU.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : ��л����������������������ɼ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  16/03/25    NACL�ݾ�Ʀ��  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *ʿ������ǯ��������б�
      *  00.00.00    NACL-��Ʀ��  18/03/01  �����������������ο���
      *                                     �ϰ�����������ο���
      *                                     ����������������������ʿ��
      *                                     ����ǯ����ޤ�
      *  00.00.00    NACL-��Ʀ��  18/03/02  �������ʽžɴ��Գ����
      *                                     �����������������ο��ߡ�
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
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ����
           COPY    "CPSK5001.INC".
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
      *    ��л����������������������ɼ����ѥ�᥿
           COPY    "CPORCSYAKINTOKU.INC".
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSYAKINTOKU-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           IF  LNK-YAKINTOKU-SANTEIYM       >=   "201604"
      *        �����ƥ������������л������������������ζ�ʬ������
               MOVE    SPACE               TO  SYS-5001-REC
               INITIALIZE                      SYS-5001-REC
               MOVE    "5001"              TO  SYS-5001-KANRICD
               MOVE    LNK-YAKINTOKU-BTU-NUM
                                           TO  SYS-5001-KBNCD
               MOVE    LNK-YAKINTOKU-SANTEIYM
                                           TO  SYS-5001-STYUKYMD(1:6)
               MOVE    "01"                TO  SYS-5001-STYUKYMD(7:2)
               MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
               MOVE    SYS-5001-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key10"             TO  MCP-PATHNAME
                   PERFORM 920-DBFETCH-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    ZERO                TO  FLG-SYSKANRI
                       MOVE    MCPDATA-REC         TO  SYS-5001-REC
                   ELSE
                       MOVE    1                   TO  FLG-SYSKANRI
                   END-IF
               ELSE
                   MOVE    SPACE               TO  SYS-5001-REC
                   INITIALIZE                  SYS-5001-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
      *        �������֤���������л�������������������
               IF    SYS-5001-YKNTOKUKBN   =   "1"
                   IF    LNK-YAKINTOKU-SRYCD    NOT  =  SPACE
                       IF  LNK-YAKINTOKU-SANTEIYM       >=   "201804"
                           PERFORM    201-CDCHG-SEC
                       ELSE
                           PERFORM    200-CDCHG-SEC
                       END-IF
                   END-IF
               END-IF
      *
      *        �������֤������ʷ������νžɴ��Գ����������������������
               IF    SYS-5001-JUUSHOUKNGKBN    =   "1"
                   IF    LNK-YAKINTOKU-SRYCD    NOT  =  SPACE
                       IF  LNK-YAKINTOKU-SANTEIYM       >=   "201804"
                           PERFORM    210-CDCHG-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    LNK-YAKINTOKU-CHGSRYCD   =     SPACE
               MOVE    1                 TO     LNK-YAKINTOKU-RC
           END-IF
      *
           .
      *
       000-PROC-EXT.
           EXIT   PROGRAM
           .
      *
      *****************************************************************
      *    �������֤���������л�������������������
      *    ʿ������ǯ�����ʿ������ǯ����ޤ�
      *****************************************************************
       200-CDCHG-SEC                    SECTION.
      *
           EVALUATE    LNK-YAKINTOKU-SRYCD
      *        ��������У�����������
               WHEN    "190117710"
                  MOVE "190183310" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ����������У�����������
               WHEN    "190077410"
                  MOVE "190183410" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ����������У�����������
               WHEN    "190077510"
                  MOVE "190183510" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ����������У�����������
               WHEN    "190077610"
                  MOVE "190183610" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ������������������������������������
               WHEN    "190132410"
                  MOVE "190185510" TO  LNK-YAKINTOKU-CHGSRYCD
      *        �����������������������������������ˡ��������ܡ�
               WHEN    "190132510"
                  MOVE "190185610" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������������������������������¡�
               WHEN    "190132610"
                  MOVE "190185710" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������������������������������¡ˡ��������ܡ�
               WHEN    "190132710"
                  MOVE "190185810" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������������������������������á�
               WHEN    "190132810"
                  MOVE "190185910" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������������������������������áˡ��������ܡ�
               WHEN    "190132910"
                  MOVE "190186010" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������������������������������ġ�
               WHEN    "190133010"
                  MOVE "190186110" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������������������������������ġˡ��������ܡ�
               WHEN    "190133110"
                  MOVE "190186210" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������������������������������š�
               WHEN    "190133210"
                  MOVE "190186310" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������������������������������šˡ��������ܡ�
               WHEN    "190133310"
                  MOVE "190186410" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������������������������������ơ�
               WHEN    "190133410"
                  MOVE "190186510" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������������������������������ơˡ��������ܡ�
               WHEN    "190133510"
                  MOVE "190186610" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������������������������������ǡ�
               WHEN    "190133610"
                  MOVE "190186710" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������������������������������ǡˡ��������ܡ�
               WHEN    "190133710"
                  MOVE "190186810" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������������������������������ȡ�
               WHEN    "190133810"
                  MOVE "190186910" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������������������������������ȡˡ��������ܡ�
               WHEN    "190133910"
                  MOVE "190187010" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������������������������������ɡ�
               WHEN    "190134010"
                  MOVE "190187110" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������������������������������ɡˡ��������ܡ�
               WHEN    "190134110"
                  MOVE "190187210" TO  LNK-YAKINTOKU-CHGSRYCD
      *        �������У�����������
               WHEN    "190117910"
                  MOVE "190187710" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������У�����������
               WHEN    "190081710"
                  MOVE "190187810" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������У�����������
               WHEN    "190081810"
                  MOVE "190187910" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������У�����������
               WHEN    "190081910"
                  MOVE "190188110" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������У�����������
               WHEN    "190082010"
                  MOVE "190188210" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������У�����������
               WHEN    "190082110"
                  MOVE "190188010" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ����������У�����������
               WHEN    "190083610"
                  MOVE "190188310" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ����������У�����������
               WHEN    "190134510"
                  MOVE "190188410" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ����������У�����������
               WHEN    "190083810"
                  MOVE "190188510" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ����������У�����������
               WHEN    "190083910"
                  MOVE "190188610" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ����������У�����������
               WHEN    "190084010"
                  MOVE "190188710" TO  LNK-YAKINTOKU-CHGSRYCD
               WHEN    OTHER
                       CONTINUE
           END-EVALUATE
      *
           .
      *
       200-CDCHG-EXT.
           .
      *
      *****************************************************************
      *    �������֤���������л�������������������
      *    ʿ������ǯ�����
      *****************************************************************
       201-CDCHG-SEC                    SECTION.
      *
           EVALUATE    LNK-YAKINTOKU-SRYCD
      *        ������������������
               WHEN    "190117710"
                  MOVE "190183310" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ������������������
               WHEN    "190199710"
                  MOVE "190201010" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ������������������
               WHEN    "190199810"
                  MOVE "190201110" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ������������������
               WHEN    "190199910"
                  MOVE "190201210" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ������������������
               WHEN    "190200010"
                  MOVE "190201310" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ������������������
               WHEN    "190200110"
                  MOVE "190201410" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ������������������
               WHEN    "190077410"
                  MOVE "190183410" TO  LNK-YAKINTOKU-CHGSRYCD
      *        �ϰ������������
               WHEN    "190200210"
                  MOVE "190201510" TO  LNK-YAKINTOKU-CHGSRYCD
      *        �ϰ������������
               WHEN    "190077510"
                  MOVE "190183510" TO  LNK-YAKINTOKU-CHGSRYCD
      *        �ϰ������������
               WHEN    "190077610"
                  MOVE "190183610" TO  LNK-YAKINTOKU-CHGSRYCD
      *        �������У�����������
               WHEN    "190117910"
                  MOVE "190187710" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������У�����������
               WHEN    "190081710"
                  MOVE "190187810" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������У�����������
               WHEN    "190081810"
                  MOVE "190187910" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������У�����������
               WHEN    "190081910"
                  MOVE "190188110" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������У�����������
               WHEN    "190082010"
                  MOVE "190188210" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ���������У�����������
               WHEN    "190082110"
                  MOVE "190188010" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ����������У�����������
               WHEN    "190083610"
                  MOVE "190188310" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ����������У�����������
               WHEN    "190134510"
                  MOVE "190188410" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ����������У�����������
               WHEN    "190083810"
                  MOVE "190188510" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ����������У�����������
               WHEN    "190083910"
                  MOVE "190188610" TO  LNK-YAKINTOKU-CHGSRYCD
      *        ����������У�����������
               WHEN    "190084010"
                  MOVE "190188710" TO  LNK-YAKINTOKU-CHGSRYCD
               WHEN    OTHER
                       CONTINUE
           END-EVALUATE
      *
           .
      *
       201-CDCHG-EXT.
           .
      *
      *****************************************************************
      *    �������֤������ʷ������νžɴ��Գ����������������������
      *    ʿ������ǯ�����
      *****************************************************************
       210-CDCHG-SEC                    SECTION.
      *
           EVALUATE    LNK-YAKINTOKU-SRYCD
      *        �������У�����������
               WHEN    "190117910"
                  MOVE "190204610" TO  LNK-YAKINTOKU-CHGSRYCD
               WHEN    OTHER
                       CONTINUE
           END-EVALUATE
      *
           .
      *
       210-CDCHG-EXT.
           .
      *
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