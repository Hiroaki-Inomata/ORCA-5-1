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
       PROGRAM-ID.                 ORCSHOSPCHG.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ��������
      *  ����ݡ��ͥ��̾  : ���ŵ��إ��������ؤ�Ƚ�ꥵ��
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  11/12/21    NACL�ݾ�Ʀ��  ��������
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
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
           03  IDX2                        PIC 9(04).
           03  IDX3                        PIC 9(04).
           03  IDX4                        PIC 9(04).
           03  IDX5                        PIC 9(04).
           03  IDX6                        PIC 9(04).
           03  IDX7                        PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
      *
           03  WRK-KSNKBN                  PIC 9(03).
      *
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1001.INC".
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
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *    ���ŵ��إ��������ؤ�Ƚ��ѥ�᥿
           COPY    "CPORCSHOSPCHG.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSHOSPCHG-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-HOSPCHG-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-HOSPCHG-RC
      *
      *    �ѥ�᡼�������å�
           PERFORM 100-PRM-CHK-SEC
      *
      *    �����
           IF      LNK-HOSPCHG-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
           DISPLAY "=====  ORCSHOSPCHG  ===="
           DISPLAY "LNK-HOSPCHG-CHGKBN = " LNK-HOSPCHG-CHGKBN
           DISPLAY "=="
           DISPLAY "LNK-HOSPCHG-HOSPCD(1)  = " LNK-HOSPCHG-HOSPCD(1)
           DISPLAY "LNK-HOSPCHG-HOSPCDN(1) = " LNK-HOSPCHG-HOSPCDN(1)
           DISPLAY "LNK-HOSPCHG-HOSPSBT(1) = " LNK-HOSPCHG-HOSPSBT(1)
           DISPLAY "LNK-HOSPCHG-STYUKYMD(1)= " LNK-HOSPCHG-STYUKYMD(1)
           DISPLAY "LNK-HOSPCHG-EDYUKYMD(1)= " LNK-HOSPCHG-EDYUKYMD(1)
           DISPLAY "=="
           DISPLAY "LNK-HOSPCHG-HOSPCD(2)  = " LNK-HOSPCHG-HOSPCD(2)
           DISPLAY "LNK-HOSPCHG-HOSPCDN(2) = " LNK-HOSPCHG-HOSPCDN(2)
           DISPLAY "LNK-HOSPCHG-HOSPSBT(2) = " LNK-HOSPCHG-HOSPSBT(2)
           DISPLAY "LNK-HOSPCHG-STYUKYMD(2)= " LNK-HOSPCHG-STYUKYMD(2)
           DISPLAY "LNK-HOSPCHG-EDYUKYMD(2)= " LNK-HOSPCHG-EDYUKYMD(2)
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
      *    Ƚ��ǯ������å�
           IF    ( LNK-HOSPCHG-HANTEIYM    =   SPACE )
               MOVE    1               TO  LNK-HOSPCHG-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    Ƚ���������ΰ��ŵ��ؾ���μ���
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1001"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-HOSPCHG-HANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    LNK-HOSPCHG-HOSPNUM
                                       TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF       FLG-SYSKANRI   =   0
                   MOVE   SYS-1001-HOSPCD
                                       TO  LNK-HOSPCHG-HOSPCD(1)
                   MOVE   SYS-1001-HOSPCDN
                                       TO  LNK-HOSPCHG-HOSPCDN(1)
                   MOVE   SYS-1001-HOSPSBT
                                       TO  LNK-HOSPCHG-HOSPSBT(1)
                   MOVE   SYS-1001-STYUKYMD
                                       TO  LNK-HOSPCHG-STYUKYMD(1)
                   MOVE   SYS-1001-EDYUKYMD
                                       TO  LNK-HOSPCHG-EDYUKYMD(1)
               END-IF
           END-IF
      *    �����ƥ������������
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *
           PERFORM   210-SYS-1001-CHK-SEC
      *
           IF  LNK-HOSPCHG-HOSPCD(2)  NOT =  ZERO
               MOVE    1              TO     LNK-HOSPCHG-CHGKBN
           ELSE
               MOVE    ZERO           TO     LNK-HOSPCHG-CHGKBN
               INITIALIZE                    LNK-HOSPCHG-HENKYAKU-AREA
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      ******************************************************************
      *    ����ΰ��Ŵ��־������                                      *
      ******************************************************************
       210-SYS-1001-CHK-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1001"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-HOSPCHG-HANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "__"                TO  SYS-STYUKYMD(7:2)
           MOVE    LNK-HOSPCHG-HOSPNUM
                                       TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"    TO  MCP-TABLE
               MOVE    "key11"           TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               PERFORM  UNTIL   (FLG-SYSKANRI   =   1)  OR
                                (LNK-HOSPCHG-HOSPCD(2)  NOT =  ZERO)
      *
                 IF    SYS-1001-STYUKYMD(7:2)    =   "01"
                    CONTINUE
                 ELSE
                    IF   SYS-1001-HOSPCD  NOT = LNK-HOSPCHG-HOSPCD(1)
                       MOVE   SYS-1001-HOSPCD
                                       TO  LNK-HOSPCHG-HOSPCD(2)
                       MOVE   SYS-1001-HOSPCDN
                                       TO  LNK-HOSPCHG-HOSPCDN(2)
                       MOVE   SYS-1001-HOSPSBT
                                       TO  LNK-HOSPCHG-HOSPSBT(2)
                       MOVE   SYS-1001-STYUKYMD
                                       TO  LNK-HOSPCHG-STYUKYMD(2)
                       MOVE   "99999999"
                                       TO  LNK-HOSPCHG-EDYUKYMD(2)
                    ELSE
      *                ���ŵ��إ����ɤ��ѹ����ʤ�����ͭ�����֤�
      *                ��λ���������֤�������
                       MOVE   SYS-1001-EDYUKYMD
                                       TO  LNK-HOSPCHG-EDYUKYMD(1)
                    END-IF
                 END-IF
      *
                 MOVE    "tbl_syskanri"    TO  MCP-TABLE
                 MOVE    "key11"           TO  MCP-PATHNAME
                 PERFORM  910-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
      *
       210-SYS-1001-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƥ�����ޥ������ɹ�
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                      SYS-1001-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ԥޥ���������
      *****************************************************************
       910-PTINF-SELECT-SEC            SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       910-PTINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ԥޥ������ɹ�
      *****************************************************************
       910-PTINF-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTINF-REC
               MOVE    ZERO               TO  FLG-PTINF
           ELSE
               INITIALIZE                     PTINF-REC
               MOVE    1                  TO  FLG-PTINF
           END-IF
      *
           .
       910-PTINF-READ-EXT.
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