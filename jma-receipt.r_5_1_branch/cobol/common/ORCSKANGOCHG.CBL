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
       PROGRAM-ID.                 ORCSKANGOCHG.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : �Ǹ���������������������ɼ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  16/03/02    NACL�ݾ�Ʀ��  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *ʿ������ǯ��������б�
      *  05.00.00    NACL-��Ʀ��  18/03/02  ���������������������ηв�
      *                                     �����ɲáʴǸ���Ψ�����У���
      *  05.00.00    NACL-��Ʀ��  18/03/09  �����������������ʣɡ�
      *                                     �����ɲã�����§���б�
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
      *
           COPY    "CPSK5000.INC".
      *
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
      *    �Ǹ���������������������ɼ����ѥ�᥿
           COPY    "CPORCSKANGOCHG.INC".
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSKANGOCHG-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           IF  LNK-KANGOCHG-SANTEIYM       >=   "201604"
      *        �����ƥ���������ʴǸ�������в����֤ζ�ʬ������
               MOVE    SPACE               TO  SYS-5001-REC
               INITIALIZE                      SYS-5001-REC
               MOVE    "5001"              TO  SYS-5001-KANRICD
               MOVE    LNK-KANGOCHG-BTU-NUM
                                           TO  SYS-5001-KBNCD
               MOVE    LNK-KANGOCHG-SANTEIYM
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
      *        ���ܴ��������������곺������§���ˤ��оݳ�ǧ
               IF  LNK-KANGOCHG-SANTEIYM       >=   "201804"
      *           �����ƥ��������
                   MOVE    SPACE           TO  SYS-5000-REC
                   INITIALIZE                  SYS-5000-REC
                   MOVE    "5000"          TO  SYS-5000-KANRICD
                   MOVE    "*"             TO  SYS-5000-KBNCD
                   MOVE    LNK-KANGOCHG-SANTEIYM
                                           TO  SYS-5000-STYUKYMD(1:6)
                   MOVE    "01"            TO  SYS-5000-STYUKYMD(7:2)
                   MOVE    SYS-5000-STYUKYMD
                                           TO  SYS-5000-EDYUKYMD
                   MOVE    SPA-HOSPNUM     TO  SYS-5000-HOSPNUM
                   MOVE    SYS-5000-REC    TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key10"         TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key10"             TO  MCP-PATHNAME
                       PERFORM 920-DBFETCH-SEC
                       IF      MCP-RC              =   ZERO
                           MOVE    ZERO                TO  FLG-SYSKANRI
                           MOVE    MCPDATA-REC         TO  SYS-5000-REC
                       ELSE
                           MOVE    SPACE           TO  SYS-5000-REC
                           INITIALIZE                  SYS-5000-REC
                           MOVE    1                   TO  FLG-SYSKANRI
                       END-IF
                   ELSE
                       MOVE    SPACE           TO  SYS-5000-REC
                       INITIALIZE                  SYS-5000-REC
                       MOVE    1               TO  FLG-SYSKANRI
                   END-IF
      *
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key10"             TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
               END-IF
      *
      *        ʿ������ǯ�������ʴǸ�������в�����������������
               IF  LNK-KANGOCHG-SANTEIYM       <=   "201803"
                   IF    SYS-5001-SYOKUINKBN  =   "1"
                       IF    LNK-KANGOCHG-SRYCD    NOT  =  SPACE
                           PERFORM    200-CDCHG-SEC
                       END-IF
                   END-IF
               END-IF
      *
      *        ʿ������ǯ�������ʴǸ�������в������������������в����֣���
               IF  LNK-KANGOCHG-SANTEIYM       >=   "201804"
                   IF    SYS-5001-SYOKUINKBN  =   "1"
                       IF    LNK-KANGOCHG-SRYCD    NOT  =  SPACE
                           PERFORM    201-CDCHG-SEC
                       END-IF
                   END-IF
               END-IF
      *
      *        ʿ������ǯ�������ʴǸ���Ψ�����У����в����֡��в����֣���
               IF  LNK-KANGOCHG-SANTEIYM       >=   "201804"
                   IF    SYS-5001-SYOKUINKBN2  =   "1"
                       IF    LNK-KANGOCHG-SRYCD    NOT  =  SPACE
                           PERFORM    202-CDCHG-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    LNK-KANGOCHG-CHGSRYCD   =     SPACE
               MOVE    1                 TO     LNK-KANGOCHG-RC
           END-IF
      *
           .
      *
       000-PROC-EXT.
           EXIT   PROGRAM
           .
      *
      *****************************************************************
      *    �������֤������ʴǸ�������в�����������������
      *    ʿ������ǯ�����ʿ������ǯ����ޤ�
      *****************************************************************
       200-CDCHG-SEC                    SECTION.
      *
           EVALUATE    LNK-KANGOCHG-SRYCD
      *        ������������������������������������
               WHEN    "190132410"
                  MOVE "190183710" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������������������������ˡ��������ܡ�
               WHEN    "190132510"
                  MOVE "190183810" TO  LNK-KANGOCHG-CHGSRYCD
      *        ���������������������������������¡�
               WHEN    "190132610"
                  MOVE "190183910" TO  LNK-KANGOCHG-CHGSRYCD
      *        ���������������������������������¡ˡ��������ܡ�
               WHEN    "190132710"
                  MOVE "190184010" TO  LNK-KANGOCHG-CHGSRYCD
      *        ���������������������������������á�
               WHEN    "190132810"
                  MOVE "190184110" TO  LNK-KANGOCHG-CHGSRYCD
      *        ���������������������������������áˡ��������ܡ�
               WHEN    "190132910"
                  MOVE "190184210" TO  LNK-KANGOCHG-CHGSRYCD
      *        ���������������������������������ġ�
               WHEN    "190133010"
                  MOVE "190184310" TO  LNK-KANGOCHG-CHGSRYCD
      *        ���������������������������������ġˡ��������ܡ�
               WHEN    "190133110"
                  MOVE "190184410" TO  LNK-KANGOCHG-CHGSRYCD
      *        ���������������������������������š�
               WHEN    "190133210"
                  MOVE "190184510" TO  LNK-KANGOCHG-CHGSRYCD
      *        ���������������������������������šˡ��������ܡ�
               WHEN    "190133310"
                  MOVE "190184610" TO  LNK-KANGOCHG-CHGSRYCD
      *        ���������������������������������ơ�
               WHEN    "190133410"
                  MOVE "190184710" TO  LNK-KANGOCHG-CHGSRYCD
      *        ���������������������������������ơˡ��������ܡ�
               WHEN    "190133510"
                  MOVE "190184810" TO  LNK-KANGOCHG-CHGSRYCD
      *        ���������������������������������ǡ�
               WHEN    "190133610"
                  MOVE "190184910" TO  LNK-KANGOCHG-CHGSRYCD
      *        ���������������������������������ǡˡ��������ܡ�
               WHEN    "190133710"
                  MOVE "190185010" TO  LNK-KANGOCHG-CHGSRYCD
      *        ���������������������������������ȡ�
               WHEN    "190133810"
                  MOVE "190185110" TO  LNK-KANGOCHG-CHGSRYCD
      *        ���������������������������������ȡˡ��������ܡ�
               WHEN    "190133910"
                  MOVE "190185210" TO  LNK-KANGOCHG-CHGSRYCD
      *        ���������������������������������ɡ�
               WHEN    "190134010"
                  MOVE "190185310" TO  LNK-KANGOCHG-CHGSRYCD
      *        ���������������������������������ɡˡ��������ܡ�
               WHEN    "190134110"
                  MOVE "190185410" TO  LNK-KANGOCHG-CHGSRYCD
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
      *    �������֤������ʴǸ�������в������������������в����֣���
      *    ʿ������ǯ�����
      *****************************************************************
       201-CDCHG-SEC                    SECTION.
      *
           EVALUATE    LNK-KANGOCHG-SRYCD
      *        �����������������ʣ��ˡʷв����֣���
               WHEN    "190132410"
                  MOVE "190215110" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣ��ˡ��������ܡˡʷв����֣���
               WHEN    "190132510"
                  MOVE "190215210" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣ¡ˡʷв����֣���
               WHEN    "190132610"
                  MOVE "190215310" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣ¡ˡ��������ܡˡʷв����֣���
               WHEN    "190132710"
                  MOVE "190215410" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣáˡʷв����֣���
               WHEN    "190132810"
                  MOVE "190215510" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣáˡ��������ܡˡʷв����֣���
               WHEN    "190132910"
                  MOVE "190215610" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣġˡʷв����֣���
               WHEN    "190133010"
                  MOVE "190215710" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣġˡ��������ܡˡʷв����֣���
               WHEN    "190133110"
                  MOVE "190215810" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣšˡʷв����֣���
               WHEN    "190133210"
                  MOVE "190215910" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣšˡ��������ܡˡʷв����֣���
               WHEN    "190133310"
                  MOVE "190216010" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣơˡʷв����֣���
               WHEN    "190133410"
                  MOVE "190216110" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣơˡ��������ܡˡʷв����֣���
               WHEN    "190133510"
                  MOVE "190216210" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣǡˡʷв����֣���
               WHEN    "190133610"
                  MOVE "190216310" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣǡˡ��������ܡˡʷв����֣���
               WHEN    "190133710"
                  MOVE "190216410" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣȡˡʷв����֣���
               WHEN    "190133810"
                  MOVE "190216510" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣȡˡ��������ܡˡʷв����֣���
               WHEN    "190133910"
                  MOVE "190216610" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣɡˡʷв����֣���
               WHEN    "190134010"
                  MOVE "190216710" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣɡˡ��������ܡˡʷв����֣���
               WHEN    "190134110"
                  MOVE "190216810" TO  LNK-KANGOCHG-CHGSRYCD
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
      *    �������֤������ʴǸ���Ψ�����У����в������������������в����֣���
      *    ʿ������ǯ�����
      *****************************************************************
       202-CDCHG-SEC                    SECTION.
      *
           EVALUATE    LNK-KANGOCHG-SRYCD
      *        �����������������ʣ��ˡʷв����֣���
               WHEN    "190132410"
                  MOVE "190202510" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣ��ˡ��������ܡˡʷв����֣���
               WHEN    "190132510"
                  MOVE "190202610" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣ¡ˡʷв����֣���
               WHEN    "190132610"
                  MOVE "190202710" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣ¡ˡ��������ܡˡʷв����֣���
               WHEN    "190132710"
                  MOVE "190202810" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣáˡʷв����֣���
               WHEN    "190132810"
                  MOVE "190202910" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣáˡ��������ܡˡʷв����֣���
               WHEN    "190132910"
                  MOVE "190203010" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣġˡʷв����֣���
               WHEN    "190133010"
                  MOVE "190203110" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣġˡ��������ܡˡʷв����֣���
               WHEN    "190133110"
                  MOVE "190203210" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣšˡʷв����֣���
               WHEN    "190133210"
                  MOVE "190203310" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣšˡ��������ܡˡʷв����֣���
               WHEN    "190133310"
                  MOVE "190203410" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣơˡʷв����֣���
               WHEN    "190133410"
                  MOVE "190203510" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣơˡ��������ܡˡʷв����֣���
               WHEN    "190133510"
                  MOVE "190203610" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣǡˡʷв����֣���
               WHEN    "190133610"
                  MOVE "190203710" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣǡˡ��������ܡˡʷв����֣���
               WHEN    "190133710"
                  MOVE "190203810" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣȡˡʷв����֣���
               WHEN    "190133810"
                  MOVE "190203910" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣȡˡ��������ܡˡʷв����֣���
               WHEN    "190133910"
                  MOVE "190204010" TO  LNK-KANGOCHG-CHGSRYCD
      *        �����������������ʣɡˡʷв����֣���
               WHEN    "190134010"
                  MOVE "190204110" TO  LNK-KANGOCHG-CHGSRYCD
      *           ��§���λ��������֤�����
                  IF    SYS-5000-EIYOUKSNKBN   =    "1"
                       MOVE "190204310" TO  LNK-KANGOCHG-CHGSRYCD
                  END-IF
      *        �����������������ʣɡˡ��������ܡˡʷв����֣���
               WHEN    "190134110"
                  MOVE "190204210" TO  LNK-KANGOCHG-CHGSRYCD
      *           ��§���λ��������֤�����
                  IF    SYS-5000-EIYOUKSNKBN   =    "1"
                       MOVE "190204410" TO  LNK-KANGOCHG-CHGSRYCD
                  END-IF
               WHEN    OTHER
                       CONTINUE
           END-EVALUATE
      *
           .
      *
       202-CDCHG-EXT.
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
