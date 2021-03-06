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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCSS015.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : �����ǧ�����ƥ�������ܼ���
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  17/04/28    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *
       WORKING-STORAGE                 SECTION.
      *
       01  IDX-AREA.
           03  IDX1                            PIC 9(05).
      *
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
      *
       01  WRK-AREA.
           03  WRK-TEXT                        PIC X(01).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1013.INC".
           COPY    "CPSK1041.INC".
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                         SECTION.
           COPY    "CPORCSS015.INC".
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       PROCEDURE                       DIVISION
           USING
           SS015-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           PERFORM 990-EXIT-PROGRAM-SEC
      *
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  IDX-AREA
                                       FLG-AREA
                                       WRK-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 2001-SRYITEMLST-EDIT-SEC
      *
           PERFORM 2001-JIHINAME-EDIT-SEC
      *
           PERFORM 2001-NYKN-HOHO-EDIT-SEC
      *
           PERFORM 2001-SMEIJOTAI-EDIT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    �������ƶ�ʬ�Խ�����
      *****************************************************************
       2001-SRYITEMLST-EDIT-SEC        SECTION.
      *
           INITIALIZE                  SS015-SRYITEMLST-G
      *
           MOVE    16              TO  SS015-SRYITEMLST-MAX
      *
           MOVE    "A00"           TO  SS015-SRYITEML (1)
           MOVE    "�顦�ƿ���"    TO  SS015-SRYITEMMEIL (1)
      *
           MOVE    "B00"           TO  SS015-SRYITEML (2)
           MOVE    "��ش�����"    TO  SS015-SRYITEMMEIL (2)
      *
           MOVE    "C00"           TO  SS015-SRYITEML (3)
           MOVE    "��������"      TO  SS015-SRYITEMMEIL (3)
      *
           MOVE    "F00"           TO  SS015-SRYITEML (4)
           MOVE    "����"          TO  SS015-SRYITEMMEIL (4)
      *
           MOVE    "G00"           TO  SS015-SRYITEML (5)
           MOVE    "����"          TO  SS015-SRYITEMMEIL (5)
      *
           MOVE    "J00"           TO  SS015-SRYITEML (6)
           MOVE    "����"          TO  SS015-SRYITEMMEIL (6)
      *
           MOVE    "K00"           TO  SS015-SRYITEML (7)
           MOVE    "���"          TO  SS015-SRYITEMMEIL (7)
      *
           MOVE    "L00"           TO  SS015-SRYITEML (8)
           MOVE    "���"          TO  SS015-SRYITEMMEIL (8)
      *
           MOVE    "D00"           TO  SS015-SRYITEML (9)
           MOVE    "����"          TO  SS015-SRYITEMMEIL (9)
      *
           MOVE    "E00"           TO  SS015-SRYITEML (10)
           MOVE    "��������"      TO  SS015-SRYITEMMEIL (10)
      *
           MOVE    "H00"           TO  SS015-SRYITEML (11)
           MOVE    "��ϥӥ�"      TO  SS015-SRYITEMMEIL (11)
      *
           MOVE    "I00"           TO  SS015-SRYITEML (12)
           MOVE    "����������"    TO  SS015-SRYITEMMEIL (12)
      *
           MOVE    "M00"           TO  SS015-SRYITEML (13)
           MOVE    "����������"    TO  SS015-SRYITEMMEIL (13)
      *
           MOVE    "N00"           TO  SS015-SRYITEML (14)
           MOVE    "��������"      TO  SS015-SRYITEMMEIL (14)
      *
           MOVE    "A10"           TO  SS015-SRYITEML (15)
           MOVE    "������"        TO  SS015-SRYITEMMEIL (15)
      *
           MOVE    "001"           TO  SS015-SRYITEML (16)
           MOVE    "����ô������"  TO  SS015-SRYITEMMEIL (16)
      *
           .
      *
       2001-SRYITEMLST-EXT.
           EXIT.
      *****************************************************************
      *    ����̾���Խ�����
      *****************************************************************
       2001-JIHINAME-EDIT-SEC          SECTION.
      *
           INITIALIZE                  SS015-JIHINAME-G
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1013"          TO  SYS-KANRICD
           MOVE    SS015-SRYYMD    TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    SYSKANRI-REC    TO  SYS-1013-REC
      *
               IF    ( SYS-1013-KBNCD(2:1) IS NUMERIC )
                   MOVE    FUNCTION NUMVAL(SYS-1013-KBNCD(2:1))
                                       TO  IDX1
                   IF    ( SYS-1013-KBNCD(2:1) =   "0" )
                       MOVE    10      TO  IDX1
                   END-IF
      *
                   IF    ( SYS-1013-KBNCD(1:1) =   "1" )
                       MOVE    SYS-1013-JIHINAME
                                       TO  SS015-JIHINAME (1 IDX1)
                   ELSE
                       MOVE    SYS-1013-JIHINAME
                                       TO  SS015-JIHINAME (2 IDX1)
                   END-IF
      *
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-JIHINAME-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ������ˡ�Խ�����
      *****************************************************************
       2001-NYKN-HOHO-EDIT-SEC         SECTION.
      *
           INITIALIZE                  SS015-NYKN-HOHOLST-G
      *
           MOVE    ZERO                TO  IDX1
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1041"              TO  SYS-KANRICD
           MOVE    SS015-SRYYMD        TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( IDX1    >=  50 )
      *
               MOVE    SYSKANRI-REC    TO  SYS-1041-REC
      *
               COMPUTE IDX1    =   IDX1    +   1
      *
               MOVE    IDX1            TO  SS015-NYKN-HOHOLST-MAX
               MOVE    SYS-1041-KBNCD  TO  SS015-NYKN-HOHOL (IDX1)
               MOVE    SYS-1041-NYKN-TANNAME
                                       TO  SS015-NYKN-HOHOMEIL (IDX1)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-JIHINAME-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ��Ǽ���پ����Խ�����
      *****************************************************************
       2001-SMEIJOTAI-EDIT-SEC         SECTION.
      *
           INITIALIZE                  SS015-SMEIJOTAILST-G
      *
           MOVE   10               TO  SS015-SMEIJOTAILST-MAX
      *
           MOVE   SPACE            TO  SS015-SMEIJOTAIL   (1)
           MOVE   "����ۤʤ�"     TO  SS015-SMEIJOTAIMEIL (1)
      *
           MOVE   "1"              TO  SS015-SMEIJOTAIL   (2)
           MOVE   "����"           TO  SS015-SMEIJOTAIMEIL (2)
      *
           MOVE   "2"              TO  SS015-SMEIJOTAIL   (3)
           MOVE   "����"           TO  SS015-SMEIJOTAIMEIL (3)
      *
           MOVE   "3"              TO  SS015-SMEIJOTAIL   (4)
           MOVE   "�Ʒ׻�"         TO  SS015-SMEIJOTAIMEIL (4)
      *
           MOVE   "4"              TO  SS015-SMEIJOTAIL   (5)
           MOVE   "�ֶ�"           TO  SS015-SMEIJOTAIMEIL (5)
      *
           MOVE   "5"              TO  SS015-SMEIJOTAIL   (6)
           MOVE   "������"       TO  SS015-SMEIJOTAIMEIL (6)
      *
           MOVE   "6"              TO  SS015-SMEIJOTAIL   (7)
           MOVE   "������"       TO  SS015-SMEIJOTAIMEIL (7)
      *
           MOVE   "7"              TO  SS015-SMEIJOTAIL   (8)
           MOVE   "��³"           TO  SS015-SMEIJOTAIMEIL (8)
      *
           MOVE   "8"              TO  SS015-SMEIJOTAIL   (9)
           MOVE   "��������"       TO  SS015-SMEIJOTAIMEIL (9)
      *
           MOVE   "9"              TO  SS015-SMEIJOTAIL   (10)
           MOVE   "����"           TO  SS015-SMEIJOTAIMEIL (10)
      *
           .
       2001-SMEIJOTAI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ��λ������
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ������������(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ����FETCH����(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸���������FHETCH��Ԥ�)
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
      *    �ģ¸���������FHETCH�ϹԤ�ʤ�)
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
       910-DBCLOSECURSOR-SEC                 SECTION.
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
      *****************************************************************
      *    EXIT PROGRAM
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
