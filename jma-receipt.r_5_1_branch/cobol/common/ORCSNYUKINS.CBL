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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSNYUKINS.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : �ޤȤ����⥵�֡�̤���ޤȤ���������ѡ�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  03.05.00    NACL-¿��Ǽ  07/04/XX  ���롼�׿����б�
      *****************************************************************
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
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SYUNOU-DBPATH  PIC X(20).
           03  WRK-NYUKIN-ZAN     PIC 9(08).
           03  WRK-MISYU          PIC 9(07).
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ���󥭡���������
           COPY "CPORCSDAILYKEY.INC".
      *
      *    �����ΰ�
           COPY    "MCPAREA".
      *
      *    ���Ծ���
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ��Ǽ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    ��Ǽ����
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSNYUKINS.INC".
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
           ORCSNYUKINAREAS
           SPA-AREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
      *    �ޥ������ռ���
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    ZERO                TO  LNK-SCNYUKINS-RC
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    �ѥ�᡼�������å�
           PERFORM   100-PRM-CHK-SEC
           IF       (LNK-SCNYUKINS-RC   NOT =   ZERO)
               GO   TO   000-PROC-EXT
           END-IF
      *
           IF    LNK-SCNYUKINS-RC      =   ZERO
               PERFORM   200-MAIN-SEC
           END-IF
           .
       000-PROC-EXT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    �ѥ�᡼�������å�
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    ���ԥޥ���������
           INITIALIZE                      PTINF-REC
           MOVE    LNK-SCNYUKINS-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNK-SCNYUKINS-PTID      TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    10              TO  LNK-SCNYUKINS-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
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
      *    ��Ǽ����
           PERFORM    VARYING  IDX   FROM  1  BY  1
             UNTIL    (IDX        >   LNK-SCNYUKINS-MAX )   OR
                      (IDX        >   500)                     OR
                      (LNK-SCNYUKINS-RC   NOT =    ZERO)
                       PERFORM   300-SYUNOU-UPD-SEC
                       PERFORM   310-SYUMEI-INS-SEC
           END-PERFORM
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��Ǽ��������
      *****************************************************************
       300-SYUNOU-UPD-SEC                SECTION.
      *
           INITIALIZE                          SYUNOU-REC
           MOVE    LNK-SCNYUKINS-HOSPNUM   TO  SYU-HOSPNUM
           MOVE    LNK-SCNYUKINS-PTID      TO  SYU-PTID
           MOVE    LNK-SCNYUKINS-NYUGAIKBN(IDX)
                                           TO  SYU-NYUGAIKBN
           MOVE    LNK-SCNYUKINS-DENPNUM(IDX)
                                           TO  SYU-DENPNUM
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-SYUNOU-SELECT-SEC
           IF      FLG-SYUNOU              NOT =   ZERO
               MOVE    11                  TO  LNK-SCNYUKINS-RC
           ELSE
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-SYUNOU-READ-SEC
               IF   FLG-SYUNOU             NOT =   ZERO
                   MOVE   11                  TO  LNK-SCNYUKINS-RC
               ELSE
                   ADD     LNK-SCNYUKINS-NYUKIN-MONEY(IDX)
                                              TO  SYU-NYUKIN-TOTAL
                   IF      LNK-SCNYUKINS-NYUKIN-MONEY(IDX)  >  ZERO
                       ADD     1              TO  SYU-NYUKIN-KAISU
                   END-IF
                   IF      SYU-DENPJTIKBN   NOT =   "3"
                     EVALUATE      TRUE
                     WHEN  ( SYU-SKYMONEY    =   ZERO )
                         MOVE    SPACE       TO  SYU-DENPJTIKBN
                     WHEN  ( SYU-SKYMONEY    >   SYU-NYUKIN-TOTAL )
                         MOVE   "2"          TO  SYU-DENPJTIKBN
                     WHEN  OTHER
                         MOVE   "1"          TO  SYU-DENPJTIKBN
                     END-EVALUATE
                  END-IF
      *
                   MOVE    SMCNDATE-YMD    TO  SYU-UPYMD
                   MOVE    SMCNDATE-HMS    TO  SYU-UPHMS
                   MOVE    SPA-OPID        TO  SYU-OPID
      *
                   MOVE    SYU-SKYMONEY
                                   TO  LNK-SCNYUKINS-SKYMONEY (IDX)
                   MOVE    SYU-NYUKIN-TOTAL
                                   TO  LNK-SCNYUKINS-NYUKIN-TOTAL (IDX)
                   MOVE    SYU-DENPJTIKBN
                                   TO  LNK-SCNYUKINS-DENPJTIKBN (IDX)
                   MOVE    SYU-CREATEKBN
                                   TO  LNK-SCNYUKINS-CREATEKBN (IDX)
      *
      *           ��Ǽ����
                  MOVE    SYUNOU-REC          TO  MCPDATA-REC
                  MOVE    "DBUPDATE"          TO  MCP-FUNC
                  MOVE    "tbl_syunou"        TO  MCP-TABLE
                  MOVE    "key"               TO  MCP-PATHNAME
grpsys            CALL    "ORCDBMAIN"         USING   MCPAREA
                                                      MCPDATA-REC
                                                      SPA-AREA
                   IF      MCP-RC              NOT =   ZERO
                       DISPLAY "ORCSNYUKINS SYUNOU UPD ERR:"
                                MCP-RC  "."
                       MOVE    11          TO  LNK-SCNYUKINS-RC
                   END-IF
               END-IF
           END-IF
      *
      *    ��Ǽ��������
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM    910-DB-CLOSE-SEC
      *
           .
       300-SYUNOU-UPD-EXT.
           EXIT.
      *
      *************************************************************
      *    ��Ǽ���ٺ���
      *************************************************************
       310-SYUMEI-INS-SEC           SECTION.
      *
      *    ���������μ�Ǽ���٥ޥ�����ɬ�פ���ɼ�ֹ���ּ���
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM         TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =    ZERO
                 MOVE    "tbl_syumei"  TO  MCP-TABLE
                 MOVE    "key2"        TO  MCP-PATHNAME
                 PERFORM 910-SYUMEI-READ-SEC
           ELSE
                 MOVE    1                    TO  FLG-SYUMEI
           END-IF
           IF      FLG-SYUMEI    =     1
                 CONTINUE
           ELSE
                 PERFORM   VARYING  IDX1  FROM   1  BY   1
                     UNTIL (FLG-SYUMEI   =      1 )
                            MOVE    "tbl_syumei"   TO  MCP-TABLE
                            MOVE    "key2"         TO  MCP-PATHNAME
                            PERFORM   910-SYUMEI-READ-SEC
                 END-PERFORM
           END-IF
      *    ��Ǽ���٥�������
           MOVE    "tbl_syumei"   TO  MCP-TABLE
           MOVE    "key2"         TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           MOVE    IDX1                TO  LNK-SCNYUKINS-DENPEDANUM(IDX)
      *
      *��Ǽ���ٺ�������
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM         TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
      *��ɼ�ֹ�
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *��ɼ�ֹ����
           MOVE    IDX1                TO  SMEI-DENPEDANUM
      *����Ϣ��
           MOVE    ZERO                TO  SMEI-NYUKINRENNUM
      *���Ų�
           MOVE    SYU-SRYKA           TO  SMEI-SRYKA
      *��ɼ���ֶ�ʬ
           MOVE    SPACE               TO  SMEI-MEISAIJOUTAIKBN
      *����ȯ������ȯ�����������
           MOVE    SPACE               TO  SMEI-SKYPRINTYMD
      *�����ȯ����
           MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *�μ���ȯ����
           MOVE    SPACE               TO  SMEI-RYOSYUPRINTYMD
      *�μ����ȯ����
           MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *������
           MOVE    ZERO                TO  SMEI-SKYMONEY
      *���ֶ��ʬ
           IF  LNK-SCNYUKINS-NYUKIN-MONEY(IDX)  >=  ZERO
               MOVE    "1"             TO  SMEI-NYUHEN-KBN
           ELSE
               MOVE    "2"             TO  SMEI-NYUHEN-KBN
           END-IF
      *���ֶ��
           MOVE    LNK-SCNYUKINS-NYUKIN-MONEY(IDX)
                                       TO  SMEI-NYUHEN-MONEY
      *���ֶ���
           IF    ( LNK-SCNYUKINS-NYUKIN-YMD2 (IDX)
                                       NOT =   SPACE )
               MOVE    LNK-SCNYUKINS-NYUKIN-YMD2 (IDX)
                                       TO  SMEI-NYUHEN-YMD
           ELSE
               MOVE    LNK-SCNYUKINS-NYUKIN-YMD
                                       TO  SMEI-NYUHEN-YMD
           END-IF
      *������ˡ
           IF  LNK-SCNYUKINS-NYUKIN-MONEY(IDX)  >  ZERO
               IF    ( LNK-SCNYUKINS-NYUKIN-HOHO2 (IDX)
                                       NOT =   SPACE )
                   MOVE    LNK-SCNYUKINS-NYUKIN-HOHO2 (IDX)
                                       TO  SMEI-NYUKIN-HOHO
               ELSE
                   MOVE    LNK-SCNYUKINS-NYUKIN-HOHO
                                       TO  SMEI-NYUKIN-HOHO
               END-IF
           ELSE
               MOVE    "00"            TO  SMEI-NYUKIN-HOHO
           END-IF
      *���ֶ�ʬ
           IF  LNK-SCNYUKINS-NYUKIN-MONEY(IDX)  >  ZERO
               MOVE    "2"             TO  SMEI-JOUTAIKBN
           ELSE
               MOVE    "4"             TO  SMEI-JOUTAIKBN
           END-IF
      *
           MOVE    SMCNDATE-YMD        TO  SMEI-CREYMD
                                           SMEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SMEI-CREHMS
                                           SMEI-UPHMS
      *
           MOVE    SMEI-CREYMD         TO  SDAILYKEY-CREYMD
           MOVE    SMEI-CREHMS         TO  SDAILYKEY-CREHMS
           MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
      *
           IF    ( LNK-SCNYUKINS-NYUKIN-HMS2 (IDX)
                                       NOT =   SPACE )
               MOVE    SMEI-NYUHEN-YMD TO  SDAILYKEY-CREYMD
               MOVE    LNK-SCNYUKINS-NYUKIN-HMS2 (IDX)
                                       TO  SDAILYKEY-CREHMS
               MOVE    "FORCE"         TO  SDAILYKEY-BASEYMD
           END-IF
      *
           PERFORM 800-DAILYKEY-SEC
           MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
      *
           MOVE    SPA-OPID            TO  SMEI-OPID
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              NOT =   ZERO
                   DISPLAY "ORCSNYUKINS SYUMEI INS ERR:"  MCP-RC  "."
                   MOVE    12          TO  LNK-SCNYUKINS-RC
           END-IF
           .
       310-SYUMEI-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���󥭡���������
      *****************************************************************
       800-DAILYKEY-SEC         SECTION.
      *
           CALL    "ORCSDAILYKEY"   USING   SDAILYKEYAREA
                                            SPA-AREA
      *
           .
      *
       800-DAILYKEY-EXT.
           EXIT.
      *****************************************************************
      *    ���ԥޥ������ɹ�
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC        TO  PTINF-REC
                   MOVE    ZERO               TO  FLG-PTINF
               ELSE
                   INITIALIZE                     PTINF-REC
                   MOVE    1                  TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                 PTINF-REC
               MOVE    1              TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��Ǽ�ޥ���������
      *****************************************************************
       910-SYUNOU-SELECT-SEC        SECTION.
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SYUNOU
           ELSE
               MOVE    1                  TO  FLG-SYUNOU
           END-IF
      *
           .
       910-SYUNOU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��Ǽ�ޥ������ɹ�
      *****************************************************************
       910-SYUNOU-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYUNOU-REC
               MOVE    ZERO                TO  FLG-SYUNOU
           ELSE
               INITIALIZE                      SYUNOU-REC
               MOVE    1                   TO  FLG-SYUNOU
           END-IF
      *
           .
       910-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��Ǽ���٥ޥ������ɹ�
      *****************************************************************
       910-SYUMEI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYUMEI-REC
               MOVE    ZERO                TO  FLG-SYUMEI
           ELSE
               INITIALIZE                      SYUMEI-REC
               MOVE    1                   TO  FLG-SYUMEI
           END-IF
      *
           .
       910-SYUMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.