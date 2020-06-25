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
       PROGRAM-ID.                 ORCSKOHPLUS.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���̸����б�
      *  ����ݡ��ͥ��̾  : �����ղþ����������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  03/07/11    NACL�ݾ�Ʀ��  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL�ݾ�Ʀ�� 03/10/07  �����˥쥻�ץ������ʬ���ɲ�
      *  01.00.02    NACL�ݾ�Ʀ�� 06/12/15  MONFUNC�б�
      *
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *  04.02.01    NACL-����    08/02/29  ʿ������ǯ��������б�
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
           03  FLG-PTINF           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-RJNHKN          PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-KOHPLUS         PIC 9(01).
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  KOH-IDX             PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
      *
           03  WRK-GAIHAKU-SRYCD           PIC X(09).
           03  WRK-MEISAISU                PIC 9(03).
           03  WRK-RYAKU-SRYCD             PIC X(09).
           03  WRK-RYAKUSHO                PIC X(14).
           03  WRK-SRYKBN                  PIC X(02).
           03  WRK-KSNKBN                  PIC 9(03).
           03  WRK-ZAISKBKBN               PIC X(01).
      *
           03  WRK-GENSAN2-MIJISI          PIC 9(01).
           03  WRK-GENSAN3-MIJISI          PIC 9(01).
           03  WRK-GENSAN4-MIJISI          PIC 9(01).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK5000.INC".
      *
      *    ���Ծ���
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    �ݸ��Ȥ߹�碌
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ���Ը������
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    �����ղþ���
       01  KOHPLUS-REC.
           COPY    "CPKOHPLUS.INC".
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
      *    �����ղþ�������ѥѥ�᥿
           COPY    "CPORCSKOHPLUS.INC".
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSKOHPLUS-AREA
grpsys     SPA-AREA
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
           INITIALIZE                  LNK-SKOHPLUS-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-SKOHPLUS-RC
      *
      *    �ѥ�᡼�������å�
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-SKOHPLUS-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    �����
           IF      LNK-SKOHPLUS-RC      =    ZERO
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
      ***  DISPLAY "KOHPLUS PRM-CHK-SEC"
           IF    ( LNK-SKOHPLUS-SRYYMD    =   SPACE )
               MOVE    1               TO  LNK-SKOHPLUS-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ���ԥޥ���������
           INITIALIZE                      PTINF-REC
grpsys     MOVE    LNK-SKOHPLUS-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNK-SKOHPLUS-PTID      TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    2               TO  LNK-SKOHPLUS-RC
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
      ***  DISPLAY "KOHPLUS MAIN-SEC"
      *�ݸ��ȹ礻�������
           MOVE    ZERO                TO   FLG-HKNCOMBI
           INITIALIZE                       HKNCOMBI-REC
grpsys     MOVE    LNK-SKOHPLUS-HOSPNUM
                                       TO   COMB-HOSPNUM
           MOVE    LNK-SKOHPLUS-PTID   TO   COMB-PTID
           MOVE    LNK-SKOHPLUS-HKNCOMBI
                                       TO   COMB-HKNCOMBINUM
           MOVE    "tbl_hkncombi"      TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           PERFORM 910-HKNCOMBI-SELECT-SEC
           IF      FLG-HKNCOMBI        =    ZERO
               MOVE    "tbl_hkncombi"  TO   MCP-TABLE
               MOVE    "key"           TO   MCP-PATHNAME
               PERFORM   910-HKNCOMBI-READ-SEC
           END-IF
      *    �ݸ��ȹ礻��������
           MOVE    "tbl_hkncombi"      TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *Ϸ���ݷ����Ԥ���Ƚ��
           IF     COMB-KOHHKNNUM(1)    =    "027"
           OR     COMB-HKNNUM          =    "039"
                  MOVE     1           TO   FLG-RJNHKN
           END-IF
      *
      *��������γ�Ǽ��������
           PERFORM  VARYING    IDX   FROM   1   BY  1
                    UNTIL     (COMB-KOHHKNNUM(IDX)  =   SPACE)  OR
                              (IDX   >   4)
                 IF   (COMB-KOHHKNNUM(IDX)  >=    "100")   AND
                      (COMB-KOHHKNNUM(IDX)  <=    "899")
                      MOVE   IDX      TO    KOH-IDX
                 END-IF
           END-PERFORM
      *
           IF       KOH-IDX   =   ZERO
                MOVE    3              TO  LNK-SKOHPLUS-RC
                GO   TO    200-MAIN-EXT
           END-IF
      *��Į¼�ֹ�μ����ʸ�����ô���ֹ�Σ����ܤ���Ĺ�������
           INITIALIZE                         PTKOHINF-REC
grpsys     MOVE    LNK-SKOHPLUS-HOSPNUM   TO  PTKOH-HOSPNUM
           MOVE    LNK-SKOHPLUS-PTID      TO  PTKOH-PTID
           MOVE    COMB-KOHID(KOH-IDX)    TO  PTKOH-KOHID
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           PERFORM 910-PTKOHINF-SELECT-SEC
           IF      FLG-PTKOHINF           =   ZERO
               MOVE    "tbl_ptkohinf"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               PERFORM   910-PTKOHINF-READ-SEC
           END-IF
      *    ���Ը�����󥯥�����
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *�����ղþ���ơ��֥���������
           INITIALIZE                         KOHPLUS-REC
grpsys     MOVE    LNK-SKOHPLUS-HOSPNUM   TO  KOHPLUS-HOSPNUM
           MOVE    LNK-SKOHPLUS-PREFNUM   TO  KOHPLUS-PREFNUM
           MOVE    PTKOH-FTNJANUM(5:3)    TO  KOHPLUS-CITYNUM
           MOVE    COMB-KOHHKNNUM(KOH-IDX) TO KOHPLUS-KOHNUM
           MOVE    "0"                    TO  KOHPLUS-PTKBN
           MOVE    LNK-SKOHPLUS-SRYYMD    TO  KOHPLUS-YUKOSTYMD
           MOVE    LNK-SKOHPLUS-SRYYMD    TO  KOHPLUS-YUKOEDYMD
           MOVE    "tbl_kohplus"          TO  MCP-TABLE
           MOVE    "key2"                 TO  MCP-PATHNAME
           PERFORM 910-KOHPLUS-SELECT-SEC
           IF      FLG-KOHPLUS            =   ZERO
               MOVE    "tbl_kohplus"      TO  MCP-TABLE
               MOVE    "key2"             TO  MCP-PATHNAME
               PERFORM   910-KOHPLUS-READ-SEC
               IF    FLG-KOHPLUS          =   ZERO
      ***          DISPLAY "KOHPLUS FETCH COMPLETE"
      *            �ֵ��ΰ�Υ��å�
                   MOVE   KOHPLUS-FTNKBN  TO LNK-SKOHPLUS-FTNKBN
                   MOVE   KOHPLUS-NYUSKJRYYKBN
                                          TO  LNK-SKOHPLUS-NYUSKJRYYKBN
                   MOVE   KOHPLUS-RECESKYKBN
                                          TO  LNK-SKOHPLUS-RECESKYKBN
               ELSE
      *            �����ղþ��󥯥�����
                   MOVE    "tbl_kohplus"          TO  MCP-TABLE
                   MOVE    "key2"                 TO  MCP-PATHNAME
                   PERFORM   910-DB-CLOSE-SEC
                   INITIALIZE                         KOHPLUS-REC
grpsys             MOVE    LNK-SKOHPLUS-HOSPNUM   TO  KOHPLUS-HOSPNUM
                   MOVE    LNK-SKOHPLUS-PREFNUM   TO  KOHPLUS-PREFNUM
                   MOVE    PTKOH-FTNJANUM(5:3)    TO  KOHPLUS-CITYNUM
                   MOVE    COMB-KOHHKNNUM(KOH-IDX) TO KOHPLUS-KOHNUM
                   IF      FLG-RJNHKN             =   ZERO
                       MOVE    "1"                TO  KOHPLUS-PTKBN
                   ELSE
                       MOVE    "2"                TO  KOHPLUS-PTKBN
                   END-IF
                   MOVE    LNK-SKOHPLUS-SRYYMD    TO  KOHPLUS-YUKOSTYMD
                   MOVE    LNK-SKOHPLUS-SRYYMD    TO  KOHPLUS-YUKOEDYMD
                   MOVE    "tbl_kohplus"          TO  MCP-TABLE
                   MOVE    "key2"                 TO  MCP-PATHNAME
                   PERFORM 910-KOHPLUS-SELECT-SEC
                   IF      FLG-KOHPLUS            =   ZERO
                       MOVE    "tbl_kohplus"      TO  MCP-TABLE
                       MOVE    "key2"             TO  MCP-PATHNAME
                       PERFORM   910-KOHPLUS-READ-SEC
                       IF    FLG-KOHPLUS          =   ZERO
      ***                  DISPLAY "KOHPLUS FETCH COMPLETE"
      *                    �ֵ��ΰ�Υ��å�
                           MOVE   KOHPLUS-FTNKBN TO LNK-SKOHPLUS-FTNKBN
                           MOVE   KOHPLUS-NYUSKJRYYKBN
                                          TO  LNK-SKOHPLUS-NYUSKJRYYKBN
                           MOVE   KOHPLUS-RECESKYKBN
                                          TO  LNK-SKOHPLUS-RECESKYKBN
                       ELSE
      ***                  DISPLAY "KOHPLUS FETCH ERR "
                           MOVE    3              TO  LNK-SKOHPLUS-RC
                       END-IF
                   ELSE
                       DISPLAY "KOHPLUS SELECT ERR "
                       MOVE    3              TO  LNK-SKOHPLUS-RC
                   END-IF
               END-IF
           ELSE
               DISPLAY "KOHPLUS SELECT ERR "
               MOVE    3              TO  LNK-SKOHPLUS-RC
           END-IF
      *    �����ղþ��󥯥�����
           MOVE    "tbl_kohplus"          TO  MCP-TABLE
           MOVE    "key2"                 TO  MCP-PATHNAME
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
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTINF-REC
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   INITIALIZE                  PTINF-REC
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                 PTINF-REC
               MOVE    1              TO  FLG-PTINF
           END-IF
      *
           MOVE     "tbl_ptinf"       TO  MCP-TABLE
           MOVE     "key"             TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ը�����󸡺�
      *****************************************************************
       910-PTKOHINF-SELECT-SEC        SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       910-PTKOHINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ը�������ɹ�
      *****************************************************************
       910-PTKOHINF-READ-SEC        SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC               =   ZERO
               MOVE    MCPDATA-REC      TO  PTKOHINF-REC
               MOVE    ZERO             TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                   PTKOHINF-REC
               MOVE    1                TO  FLG-PTKOHINF
           END-IF
      *
           .
       910-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ղþ���ޥ���������
      *****************************************************************
       910-KOHPLUS-SELECT-SEC        SECTION.
      *
           MOVE    KOHPLUS-REC         TO  MCPDATA-REC
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-KOHPLUS
           ELSE
               MOVE    1               TO  FLG-KOHPLUS
           END-IF
      *
           .
       910-KOHPLUS-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ղþ���ޥ������ɹ�
      *****************************************************************
       910-KOHPLUS-READ-SEC        SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC               =   ZERO
               MOVE    MCPDATA-REC      TO  KOHPLUS-REC
               MOVE    ZERO             TO  FLG-KOHPLUS
           ELSE
               INITIALIZE                   KOHPLUS-REC
               MOVE    1                TO  FLG-KOHPLUS
           END-IF
      *
           .
       910-KOHPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ݸ��ȹ礻�ޥ���������
      *****************************************************************
       910-HKNCOMBI-SELECT-SEC        SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ݸ��ȹ礻�ޥ������ɹ�
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
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
      *    �ģ¥�����������
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
      *
       910-DB-CLOSE-EXT.
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