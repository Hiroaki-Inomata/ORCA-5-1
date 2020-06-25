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
       PROGRAM-ID.                 ORCSTOTALACCT.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : ���Ų����������������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  06/02/02    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.00    NACL-����    07/05/09  ���롼�׿����б�
      *
      *  04.05.01    NACL-ƣ��    11/09/16  ����ñ���б�
      *                                     �������磵�����ѹ���ʻ����
      *                                     �ݸ��ȹ礻���ѹ�
      *
      *  04.07.01    NACL-����    13/09/19  �����ա��軰�Թ԰١��б�
      *  04.07.02    NACL-����    14/09/01  ʿ������ǯ����������б�
      *  04.07.03    NACL-����    15/03/16  ʿ������ǯ��������б�
      *  04.08.01    NACL-����    16/03/22  ʿ������ǯ��������б�
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
           03  FLG-SRYACCT                 PIC 9(01).
           03  FLG-JYURRK                  PIC 9(01).
           03  FLG-SRYACCTPLUS             PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-ARI                     PIC 9(01).
           03  FLG-ARI2                    PIC 9(01).
           03  FLG-NG                      PIC 9(01).
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                         PIC 9(03).
           03  IDX2                        PIC 9(03).
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-HKNCOMBIINF-TABLE.
             05  WRK-HKNCOMBIINF-OCC       OCCURS  50.
               07  WRK-HKNCOMBI-DAISANFLG  PIC 9(01).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ���ԥޥ�����
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ���Ų��
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    ��������
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    ���Ų���ղþ���ޥ�����
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *    �ݸ��Ȥ߹�碌
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
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
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    ������׺����ѥѥ�᥿
           COPY    "CPORCSTOTALACCT.INC".
      *
grpsys     COPY    "COMMON-SPA".

      *
       PROCEDURE                   DIVISION    USING
            ORCSTOTALACCT-AREA
            SPA-AREA
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
           INITIALIZE                  LNK-ORCSTOTALACCT-OUT-AREA
           MOVE    ZERO          TO    LNK-ORCSTOTALACCT-RC
      *
      *    �ѥ�᡼�������å�
           PERFORM 100-PRM-CHK-SEC
           IF      LNK-ORCSTOTALACCT-RC  NOT =   ZERO
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    �����
           IF      LNK-ORCSTOTALACCT-RC      =    ZERO
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
      *    ������ʬ�����å�
           IF     LNK-ORCSTOTALACCT-NYUGAIKBN   =   "1"  OR  "2"
               CONTINUE
           ELSE
               MOVE    1               TO  LNK-ORCSTOTALACCT-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ���ԥޥ���������
           INITIALIZE                             PTINF-REC
           MOVE    LNK-ORCSTOTALACCT-HOSPNUM  TO  PTINF-HOSPNUM
           MOVE    LNK-ORCSTOTALACCT-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    2               TO  LNK-ORCSTOTALACCT-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ����ǯ������å�
           IF     LNK-ORCSTOTALACCT-SANTEIYM    =   SPACE
               MOVE    3               TO  LNK-ORCSTOTALACCT-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ������ʬ�����å�
           EVALUATE   LNK-ORCSTOTALACCT-NYUGAIKBN
               WHEN    "1"
                  CONTINUE
               WHEN    "2"
                  IF  LNK-ORCSTOTALACCT-SHORIKBN = SPACE OR "1" OR "2"
                      CONTINUE
                  ELSE
                      MOVE    4        TO  LNK-ORCSTOTALACCT-RC
                      GO  TO  100-PRM-CHK-EXT
                  END-IF
           END-EVALUATE
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
           EVALUATE   LNK-ORCSTOTALACCT-NYUGAIKBN
               WHEN    "1"
                  PERFORM     2001-TENSU-HENSYU-SEC
               WHEN    "2"
                  EVALUATE   LNK-ORCSTOTALACCT-SHORIKBN
                    WHEN    SPACE
                      PERFORM 2001-TENSU-HENSYU-SEC
                      PERFORM 2002-KAISU-HENSYU-SEC
                    WHEN    "1"
                      PERFORM 2001-TENSU-HENSYU-SEC
                    WHEN    "2"
                      PERFORM 2002-KAISU-HENSYU-SEC
                  END-EVALUATE
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����������
      *****************************************************************
       2001-TENSU-HENSYU-SEC            SECTION.
      *
           INITIALIZE                               SRYACCT-REC
           MOVE    LNK-ORCSTOTALACCT-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    LNK-ORCSTOTALACCT-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE    LNK-ORCSTOTALACCT-PTID       TO  ACCT-PTID
           MOVE    LNK-ORCSTOTALACCT-SANTEIYM   TO  ACCT-SRYYM
           MOVE    "tbl_sryacct"                TO  MCP-TABLE
           MOVE    "key44"                      TO  MCP-PATHNAME
           MOVE    SRYACCT-REC                  TO  MCPDATA-REC
           MOVE    "DBSELECT"                   TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"                  USING MCPAREA
                                                      MCPDATA-REC
                                                      SPA-AREA
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"            TO  MCP-TABLE
               MOVE    "key44"                  TO  MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM   UNTIL   FLG-SRYACCT  =  1
                   MOVE     ZERO     TO     FLG-ARI
                   PERFORM VARYING IDX    FROM    1   BY  1
                           UNTIL  (IDX        >    50)
                             OR   (LNK-ORCSTOTALACCT-HKNCOMBINUM(IDX)
                                                             =    ZERO)
                             OR   (FLG-ARI    =     1)
                      IF  LNK-ORCSTOTALACCT-HKNCOMBINUM(IDX)
                                                       = ACCT-HKNCOMBI
                          PERFORM 20011-SRYKA-HENSYU-SEC
                          MOVE     1     TO     FLG-ARI
                      END-IF
                   END-PERFORM
                   IF   FLG-ARI   =   ZERO
                     IF  IDX     <=    50
                        MOVE   ACCT-HKNCOMBI TO
                                     LNK-ORCSTOTALACCT-HKNCOMBINUM(IDX)
                        PERFORM   20012-HKNCOMBI-HENSYU-SEC
                        PERFORM   20011-SRYKA-HENSYU-SEC
                     END-IF
                   END-IF
                   MOVE    "tbl_sryacct"            TO  MCP-TABLE
                   MOVE    "key44"                  TO  MCP-PATHNAME
                   PERFORM  900-SRYACCT-READ-SEC
               END-PERFORM
           ELSE
               MOVE    5    TO     LNK-ORCSTOTALACCT-RC
           END-IF
      *
           MOVE    "tbl_sryacct"                TO  MCP-TABLE
           MOVE    "key44"                      TO  MCP-PATHNAME
           PERFORM 910-DB-CLOSE-SEC
      *
           .
       2001-TENSU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ų��Խ����������ѡ�
      *****************************************************************
       20011-SRYKA-HENSYU-SEC            SECTION.
      *
           MOVE    ZERO    TO   FLG-NG
           IF  (WRK-HKNCOMBI-DAISANFLG(IDX)  =   1  )
           AND (ACCT-SRYKBN                  =  "80")
           AND (ACCT-ZAIREQFLG               =  "1" )
               PERFORM 200112-SRYACCTPLUS-KENSAKU-SEC
           END-IF
      *
           IF   FLG-NG  =  ZERO
             MOVE     ZERO     TO     FLG-ARI2
             PERFORM VARYING IDX2   FROM    1   BY  1
                    UNTIL (IDX2       >    50)
                      OR  (LNK-ORCSTOTALACCT-SRYKA(IDX IDX2) =  SPACE)
                      OR  (FLG-ARI2   =     1)
                IF  LNK-ORCSTOTALACCT-SRYKA(IDX IDX2) = ACCT-SRYKA
                       PERFORM 200111-TENSU-KEISAN-SEC
                       MOVE     1     TO     FLG-ARI2
                END-IF
             END-PERFORM
             IF   FLG-ARI2  =   ZERO
               IF  IDX2  <=    50
                   MOVE   ACCT-SRYKA  TO
                                    LNK-ORCSTOTALACCT-SRYKA(IDX IDX2)
                   PERFORM 200111-TENSU-KEISAN-SEC
               END-IF
             END-IF
           END-IF
      *
           .
       20011-SRYKA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������׻�
      *****************************************************************
       200111-TENSU-KEISAN-SEC            SECTION.
      *
           IF   ACCT-SRYCDTOTAL   =   630010002
                                 OR   630010005
                                 OR   630010006
                                 OR   630010007
                                 OR   190080470
                                 OR   190799670
                                 OR   190080570
                                 OR   190799770
                                 OR   190111690
                                 OR   190809090
                                 OR   190111790
                                 OR   190809190
                COMPUTE  LNK-ORCSTOTALACCT-TENSU(IDX IDX2)  =
                         LNK-ORCSTOTALACCT-TENSU(IDX IDX2)  -
                         (ACCT-ZAITEN  *  ACCT-ZAIKAISU)
           ELSE
                COMPUTE  LNK-ORCSTOTALACCT-TENSU(IDX IDX2)  =
                         LNK-ORCSTOTALACCT-TENSU(IDX IDX2)  +
                         (ACCT-ZAITEN  *  ACCT-ZAIKAISU)
           END-IF
      *
           .
       200111-TENSU-KEISAN-EXT.
           EXIT.
      *
      **************************************************************
      *    ���Ų���ղþ��󸡺�
      **************************************************************
       200112-SRYACCTPLUS-KENSAKU-SEC           SECTION.
      *
           INITIALIZE                                SRYACCTPLUS-REC
           MOVE    LNK-ORCSTOTALACCT-HOSPNUM    TO   ACCTP-HOSPNUM
           MOVE    LNK-ORCSTOTALACCT-NYUGAIKBN  TO   ACCTP-NYUGAIKBN
           MOVE    LNK-ORCSTOTALACCT-PTID       TO   ACCTP-PTID
           MOVE    ACCT-SRYKA                   TO   ACCTP-SRYKA
           MOVE    LNK-ORCSTOTALACCT-SANTEIYM   TO   ACCTP-SRYYM
           MOVE    ACCT-ZAINUM                  TO   ACCTP-ZAINUM
           MOVE    1                            TO   ACCTP-RENNUM
           MOVE    SRYACCTPLUS-REC              TO   MCPDATA-REC
           MOVE    "tbl_sryacctplus"            TO   MCP-TABLE
           MOVE    "key"                        TO   MCP-PATHNAME
           MOVE    "DBSELECT"                   TO   MCP-FUNC
           CALL    "ORCDBMAIN"                  USING  MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
               MOVE    "key"              TO   MCP-PATHNAME
               PERFORM  900-SRYACCTPLUS-READ-SEC
               IF    FLG-SRYACCTPLUS   =   ZERO
                 IF   ACCTP-RSIKINGAKU  NOT = ZERO
                    MOVE    1    TO    FLG-NG
                 END-IF
               END-IF
           END-IF
           MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
           MOVE    "key"              TO   MCP-PATHNAME
           PERFORM 910-DB-CLOSE-SEC
      *
           .
      *
       200112-SRYACCTPLUS-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ݸ��Ȥ߹�碌�����Խ�
      *****************************************************************
       20012-HKNCOMBI-HENSYU-SEC            SECTION.
      *
           INITIALIZE                                HKNCOMBI-REC
           MOVE    LNK-ORCSTOTALACCT-HOSPNUM    TO   COMB-HOSPNUM
           MOVE    LNK-ORCSTOTALACCT-PTID       TO   COMB-PTID
           MOVE    ACCT-HKNCOMBI                TO   COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC                 TO   MCPDATA-REC
           MOVE    "tbl_hkncombi"               TO   MCP-TABLE
           MOVE    "key"                        TO   MCP-PATHNAME
           MOVE    "DBSELECT"                   TO   MCP-FUNC
           CALL    "ORCDBMAIN"                  USING  MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_hkncombi"   TO      MCP-TABLE
               MOVE    "key"            TO      MCP-PATHNAME
               PERFORM  900-HKNCOMBI-READ-SEC
               IF    FLG-HKNCOMBI   =   ZERO
                 IF   COMB-KOH1HKNNUM = "970"
                    MOVE    1    TO    WRK-HKNCOMBI-DAISANFLG(IDX)
                 END-IF
               END-IF
           END-IF
           MOVE    "tbl_hkncombi"       TO       MCP-TABLE
           MOVE    "key"                TO       MCP-PATHNAME
           PERFORM 910-DB-CLOSE-SEC
      *
           .
       20012-HKNCOMBI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ǲ������
      *****************************************************************
       2002-KAISU-HENSYU-SEC            SECTION.
      *
           INITIALIZE                              JYURRK-REC
           MOVE    LNK-ORCSTOTALACCT-HOSPNUM   TO  JYURRK-HOSPNUM
           MOVE    LNK-ORCSTOTALACCT-PTID      TO  JYURRK-PTID
           MOVE    LNK-ORCSTOTALACCT-NYUGAIKBN TO  JYURRK-NYUGAIKBN
           MOVE    LNK-ORCSTOTALACCT-SANTEIYM  TO  JYURRK-SRYYMD(1:6)
           MOVE    "01"                        TO  JYURRK-SRYYMD(7:2)
           MOVE    LNK-ORCSTOTALACCT-SANTEIYM  TO  JYURRK-UPSRYYMD(1:6)
           MOVE    "31"                        TO  JYURRK-UPSRYYMD(7:2)
           MOVE    "tbl_jyurrk"                TO  MCP-TABLE
           MOVE    "key33"                     TO  MCP-PATHNAME
           MOVE    JYURRK-REC                  TO  MCPDATA-REC
           MOVE    "DBSELECT"                  TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"                 USING  MCPAREA
                                                      MCPDATA-REC
                                                      SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"            TO  MCP-TABLE
               MOVE    "key33"                 TO  MCP-PATHNAME
               PERFORM   900-JYURRK-READ-SEC
               PERFORM   UNTIL   FLG-JYURRK   =    1
                   MOVE     ZERO     TO     FLG-ARI
                   PERFORM VARYING IDX    FROM    1   BY  1
                           UNTIL  (IDX        >    50)
                             OR   (LNK-ORCSTOTALACCT-HKNCOMBINUM(IDX)
                                                             =    ZERO)
                             OR   (FLG-ARI    =     1)
                      IF  LNK-ORCSTOTALACCT-HKNCOMBINUM(IDX)
                                                 = JYURRK-HKNCOMBINUM
                          PERFORM 20021-SRYKA-HENSYU2-SEC
                          MOVE     1     TO     FLG-ARI
                      END-IF
                   END-PERFORM
                   IF   FLG-ARI   =   ZERO
                     IF  IDX     <=    50
                        MOVE   JYURRK-HKNCOMBINUM  TO
                                     LNK-ORCSTOTALACCT-HKNCOMBINUM(IDX)
                        PERFORM   20021-SRYKA-HENSYU2-SEC
                     END-IF
                   END-IF
                   MOVE    "tbl_jyurrk"                TO  MCP-TABLE
                   MOVE    "key33"                     TO  MCP-PATHNAME
                   PERFORM   900-JYURRK-READ-SEC
               END-PERFORM
           ELSE
               MOVE    6    TO     LNK-ORCSTOTALACCT-RC
           END-IF
      *
           MOVE    "tbl_jyurrk"                TO  MCP-TABLE
           MOVE    "key33"                     TO  MCP-PATHNAME
           PERFORM 910-DB-CLOSE-SEC
      *
           .
       2002-KAISU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ų��Խ��ʼ��ǲ���ѡ�
      *****************************************************************
       20021-SRYKA-HENSYU2-SEC            SECTION.
      *
           MOVE     ZERO     TO     FLG-ARI2
           PERFORM VARYING IDX2   FROM    1   BY  1
                    UNTIL (IDX2       >    50)
                      OR  (LNK-ORCSTOTALACCT-SRYKA(IDX IDX2) =  SPACE)
                      OR  (FLG-ARI2   =     1)
                IF  LNK-ORCSTOTALACCT-SRYKA(IDX IDX2) = JYURRK-SRYKA
                       PERFORM 200211-KAISU-KEISAN-SEC
                       MOVE     1     TO     FLG-ARI2
                END-IF
           END-PERFORM
           IF   FLG-ARI2  =   ZERO
               IF  IDX2  <=    50
                   MOVE   JYURRK-SRYKA  TO
                                    LNK-ORCSTOTALACCT-SRYKA(IDX IDX2)
                   PERFORM 200211-KAISU-KEISAN-SEC
               END-IF
           END-IF
      *
           .
       20021-SRYKA-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ǲ���׻�
      *****************************************************************
       200211-KAISU-KEISAN-SEC            SECTION.
      *
           COMPUTE  LNK-ORCSTOTALACCT-KAISU(IDX IDX2)  =
                    LNK-ORCSTOTALACCT-KAISU(IDX IDX2)  +   1
      *
           .
       200211-KAISU-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ԥޥ������ɹ�
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING  MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys         CALL    "ORCDBMAIN"         USING  MCPAREA
                                                  MCPDATA-REC
                                                  SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTINF-REC
                   MOVE    ZERO        TO  FLG-PTINF
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE    1           TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DB-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ų�ץޥ������ɹ�
      *****************************************************************
       900-SRYACCT-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"          USING  MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  SRYACCT-REC
               MOVE    ZERO               TO  FLG-SRYACCT
           ELSE
               INITIALIZE                     SRYACCT-REC
               MOVE    1                  TO  FLG-SRYACCT
           END-IF
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������ɹ�
      *****************************************************************
       900-JYURRK-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING  MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       900-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ų���ղþ����ɹ�
      *****************************************************************
       900-SRYACCTPLUS-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING  MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCTPLUS-REC
               MOVE    ZERO            TO  FLG-SRYACCTPLUS
           ELSE
               INITIALIZE                  SRYACCTPLUS-REC
               MOVE    1               TO  FLG-SRYACCTPLUS
           END-IF
      *
           .
       900-SRYACCTPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ݸ��Ȥ߹�碌�����ɹ�
      *****************************************************************
       900-HKNCOMBI-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING  MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING  MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.