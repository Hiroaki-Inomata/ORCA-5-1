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
       PROGRAM-ID.                     ORCGI2SUB03.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �������ԾȲ�
      *  ����ݡ��ͥ��̾  : �������ԾȲ񥵥֡ʥ���ܥܥå������������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  16/10/04    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX0                            PIC 9(05).
           03  IDXL                            PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-CD99-G.
               05  WRK-CD99                    PIC 9(02).
           03  WRK-KANRICD                     PIC X(04).
           03  WRK-STYMD                       PIC X(08).
           03  WRK-EDYMD                       PIC X(08).
      *
       01  CONST-AREA.
           03  CONST-SRYKALST-MAX              PIC 9(05)   VALUE 99.
           03  CONST-BTUNUMLST-MAX             PIC 9(05)   VALUE 50.
           03  CONST-TAIINCDLST-MAX            PIC 9(05)   VALUE 50.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ���Ծ��֥�����
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1005.INC".
           COPY    "CPSK5001.INC".
           COPY    "CPSK5013.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPI2SUB03.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           I2SUB03-AREA
           SPA-AREA.
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE              FLG-AREA
                                   IDX-AREA
                                   WRK-AREA
                                   I2SUB03-ITEM-LIST-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    ���Ųʥ���ܥܥå����������
           PERFORM 2001-SRYKALST-SEC
      *
      *    �����ֹ楳��ܥܥå����������
           PERFORM 2001-BTUNUMLST-SEC
      *
      *    ���֥���ܥܥå����������
           PERFORM 2001-JOTAILST-SEC
      *
      *    ������֥���ܥܥå����������
           PERFORM 2001-SEIKYUJOTAILST-SEC
      *
      *    ��˴��ʬ����ܥܥå����������
           PERFORM 2001-DEATHKBNLST-SEC
      *
      *    �ƥ��ȴ��ԥ���ܥܥå����������
           PERFORM 2001-TSTPTLST-SEC
      *
      *    �ౡ��ͳ����ܥܥå����������
           PERFORM 2001-TAIINCDLST-SEC
      *
      *    �������ܥ���ܥܥå����������
           PERFORM 2001-SORTLST-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ���Ųʥ���ܥܥå����������
      *****************************************************************
       2001-SRYKALST-SEC                   SECTION.
      *
           MOVE    ZERO                TO  IDXL
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1005"              TO  SYS-KANRICD
           MOVE    I2SUB03-BASEYMD     TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( IDXL        >=  CONST-SRYKALST-MAX )
      *
               MOVE    SYSKANRI-REC    TO  SYS-1005-REC
      *
               COMPUTE IDXL    =   IDXL    +   1
      *
               MOVE    IDXL            TO  I2SUB03-SRYKALST-MAX
      *
               MOVE    SYS-1005-KBNCD (1:2)
                                       TO  I2SUB03-SRYKAL (IDXL)
               MOVE    SYS-1005-SRYKANAME
                                       TO  I2SUB03-SRYKAMEIL (IDXL)
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
       2001-SRYKALST-EXT.
           EXIT.
      *****************************************************************
      *    �����ֹ楳��ܥܥå����������
      *****************************************************************
       2001-BTUNUMLST-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDXL
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5001"              TO  SYS-KANRICD
           MOVE    I2SUB03-BASEYMD     TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( IDXL        >=  CONST-BTUNUMLST-MAX )
      *
               MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
               COMPUTE IDXL    =   IDXL    +   1
      *
               MOVE    IDXL            TO  I2SUB03-BTUNUMLST-MAX
      *
               MOVE    SYS-5001-KBNCD (1:2)
                                       TO  I2SUB03-BTUNUML (IDXL)
               MOVE    SYS-5001-BTU-NAME
                                       TO  I2SUB03-BTUNUMMEIL (IDXL)
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
       2001-BTUNUMLST-EXT.
           EXIT.
      *****************************************************************
      *    ���֥���ܥܥå����������
      *****************************************************************
       2001-JOTAILST-SEC                   SECTION.
      *
           MOVE    4                   TO  I2SUB03-JOTAILST-MAX
      *
           MOVE    "1"                 TO  I2SUB03-JOTAIL (1)
           MOVE    "������"            TO  I2SUB03-JOTAIMEIL (1)
      *
           MOVE    "2"                 TO  I2SUB03-JOTAIL (2)
           MOVE    "�ౡ��"            TO  I2SUB03-JOTAIMEIL (2)
      *
           MOVE    "3"                 TO  I2SUB03-JOTAIL (3)
           MOVE    "����߱�"          TO  I2SUB03-JOTAIMEIL (3)
      *
           MOVE    "4"                 TO  I2SUB03-JOTAIL (4)
           MOVE    "�����߱�"          TO  I2SUB03-JOTAIMEIL (4)
      *
           IF    ( I2SUB03-FROM        =   "API" )
      *
               COMPUTE I2SUB03-JOTAILST-MAX
                   =   I2SUB03-JOTAILST-MAX    +   1
      *
               MOVE    "5"             TO  I2SUB03-JOTAIL (5)
               MOVE    "���ݸ��ȹ礻��Ͽ"
                                       TO  I2SUB03-JOTAIMEIL (5)
           END-IF
      *
           .
       2001-JOTAILST-EXT.
           EXIT.
      *****************************************************************
      *    ������֥���ܥܥå����������
      *****************************************************************
       2001-SEIKYUJOTAILST-SEC             SECTION.
      *
           MOVE    2                   TO  I2SUB03-SEIKYUJOTAILST-MAX
      *
           MOVE    "1"                 TO  I2SUB03-SEIKYUJOTAIL (1)
           MOVE    "�ౡ�����"        TO  I2SUB03-SEIKYUJOTAIMEIL (1)
      *
           MOVE    "2"                 TO  I2SUB03-SEIKYUJOTAIL (2)
           MOVE    "ľ�����������"  TO  I2SUB03-SEIKYUJOTAIMEIL (2)
      *
           .
       2001-SEIKYUJOTAILST-EXT.
           EXIT.
      *****************************************************************
      *    ��˴����ܽ������
      *****************************************************************
       2001-DEATHKBNLST-SEC                   SECTION.
      *
           MOVE    2                   TO  I2SUB03-DEATHKBNLST-MAX
      *
           MOVE    "0"                 TO  I2SUB03-DEATHKBNL    (1)
           MOVE    "̤����"            TO  I2SUB03-DEATHKBNMEIL (1)
      *
           MOVE    "1"                 TO  I2SUB03-DEATHKBNL    (2)
           MOVE    "��˴"              TO  I2SUB03-DEATHKBNMEIL (2)
      *
           .
       2001-DEATHKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    �ƥ��ȴ��ԥ���ܽ������
      *****************************************************************
       2001-TSTPTLST-SEC                   SECTION.
      *
           MOVE    3                   TO  I2SUB03-TSTPTLST-MAX
      *
           MOVE    "a"                 TO  I2SUB03-TSTPTL    (1)
           MOVE    "�ƥ��ȴ��Ԥ����"  TO  I2SUB03-TSTPTMEIL (1)
      *
           MOVE    "b"                 TO  I2SUB03-TSTPTL    (2)
           MOVE    "�ƥ��ȴ��Ԥ�ޤ��" TO I2SUB03-TSTPTMEIL (2)
      *
           MOVE    "1"                 TO  I2SUB03-TSTPTL    (3)
           MOVE    "�ƥ��ȴ��ԤΤ�"    TO  I2SUB03-TSTPTMEIL (3)
      *
           .
       2001-TSTPTLST-EXT.
           EXIT.
      *****************************************************************
      *    �����ֹ楳��ܥܥå����������
      *****************************************************************
       2001-TAIINCDLST-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDXL
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5013"              TO  SYS-KANRICD
           MOVE    I2SUB03-BASEYMD     TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( IDXL        >=  CONST-TAIINCDLST-MAX )
      *
               MOVE    SYSKANRI-REC    TO  SYS-5013-REC
      *
               COMPUTE IDXL    =   IDXL    +   1
      *
               MOVE    IDXL            TO  I2SUB03-TAIINCDLST-MAX
      *
               MOVE    SYS-5013-KBNCD (1:2)
                                       TO  I2SUB03-TAIINCDL (IDXL)
               MOVE    SYS-5013-NAME
                                       TO  I2SUB03-TAIINCDMEIL (IDXL)
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
       2001-TAIINCDLST-EXT.
           EXIT.
      *****************************************************************
      *    �������ܥ���ܥܥå����������
      *****************************************************************
       2001-SORTLST-SEC                    SECTION.
      *
           MOVE    7                   TO  I2SUB03-SORTLST-MAX
      *
           MOVE    "1"                 TO  I2SUB03-SORTL (1)
           MOVE    "����¼���"      TO  I2SUB03-SORTMEIL (1)
      *
           MOVE    "2"                 TO  I2SUB03-SORTL (2)
           MOVE    "�����ֹ��"        TO  I2SUB03-SORTMEIL (2)
      *
           MOVE    "3"                 TO  I2SUB03-SORTL (3)
           MOVE    "���ʻ�̾��"        TO  I2SUB03-SORTMEIL (3)
      *
           MOVE    "4"                 TO  I2SUB03-SORTL (4)
           MOVE    "�������ο�������"  TO  I2SUB03-SORTMEIL (4)
      *
           MOVE    "5"                 TO  I2SUB03-SORTL (5)
           MOVE    "�������θŤ���"    TO  I2SUB03-SORTMEIL (5)
      *
           MOVE    "6"                 TO  I2SUB03-SORTL (6)
           MOVE    "�ౡ���ο�������"  TO  I2SUB03-SORTMEIL (6)
      *
           MOVE    "7"                 TO  I2SUB03-SORTL (7)
           MOVE    "�ౡ���θŤ���"    TO  I2SUB03-SORTMEIL (7)
      *
           .
       2001-SORTLST-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ�����ޥ�����������(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ�����ޥ���FETCH����(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           ELSE
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO      MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.