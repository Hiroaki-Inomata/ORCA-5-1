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
       PROGRAM-ID.             ORCSNYUKIN.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : �ޤȤ����⥵��
      *  ������            : 
      *  ��������    ��ȼ�        ����
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
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-PATHNAME        PIC X(20).
           03  WRK-NYUKIN-ZAN      PIC 9(08).
           03  WRK-MISYU           PIC 9(07).
           03  WRK-HENKIN-ZAN      PIC 9(08).
           03  WRK-DENPEDANUM      PIC 9(02).
           03  WRK-HENKIN          PIC S9(10).
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
      *    �����ƥ����
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1001.INC".
      *
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                         SECTION.
      *
           COPY    "CPORCSNYUKIN.INC".
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                       DIVISION    USING
           ORCSNYUKINAREA
           SPA-AREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  LNK-SCNYUKIN-OUT-AREA
           MOVE    ZERO            TO  LNK-SCNYUKIN-RC
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    �ѥ�᡼�������å�
           PERFORM 100-PRM-CHK-SEC
      *
           IF    ( LNK-SCNYUKIN-RC   = ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
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
           IF    ( LNK-SCNYUKIN-SYORIKBN   =   "1" OR "2" )
               CONTINUE
           ELSE
               MOVE    10              TO  LNK-SCNYUKIN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
           IF    ( LNK-SCNYUKIN-KOUSHINKBN =   "1" OR "2" )
               CONTINUE
           ELSE
               MOVE    10              TO  LNK-SCNYUKIN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
           IF    ( LNK-SCNYUKIN-NYKNKNRKBN =   "1" OR "2" OR "3" OR
                                               "4" OR "5" )
               CONTINUE
           ELSE
               MOVE    10              TO  LNK-SCNYUKIN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    IF    ( LNK-SCNYUKIN-NYUKIN-MONEY   =   ZERO )
      *        MOVE    10              TO  LNK-SCNYUKIN-RC
      *        GO  TO  100-PRM-CHK-EXT
      *    END-IF
      *
      *    ���ԥޥ���������
           INITIALIZE                      PTINF-REC
           MOVE    LNK-SCNYUKIN-HOSPNUM TO  PTINF-HOSPNUM
           MOVE    LNK-SCNYUKIN-PTID   TO  PTINF-PTID
           PERFORM 900-PTINF-KEY-SEL-SEC
           IF    ( FLG-PTINF           NOT =   ZERO )
               MOVE    11              TO  LNK-SCNYUKIN-RC
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
      *    �ޥ������ռ���
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    ��Ǽ�ɤ߹����ѣˣţ٤λ���
           MOVE    SPACE               TO  WRK-PATHNAME
           EVALUATE    LNK-SCNYUKIN-NYKNKNRKBN
           WHEN    "2"
           WHEN    "4"
               MOVE   "key22"          TO  WRK-PATHNAME
           WHEN    "3"
           WHEN    "5"
               MOVE   "key23"          TO  WRK-PATHNAME
           END-EVALUATE
      *
      *
      *    ��Ǽ�ޥ���������̤�������μ���
           INITIALIZE                      SYUNOU-REC
           MOVE    LNK-SCNYUKIN-HOSPNUM TO  SYU-HOSPNUM
           MOVE    LNK-SCNYUKIN-PTID   TO  SYU-PTID
           MOVE    LNK-SCNYUKIN-NYUGAIKBN
                                       TO  SYU-NYUGAIKBN
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 900-SYUNOU-SELECT-SEC
      *
           MOVE    LNK-SCNYUKIN-NYUKIN-MONEY
                                       TO  WRK-NYUKIN-ZAN
           IF    ( LNK-SCNYUKIN-HENKINKBN  =   "1" )
               MOVE    ZERO            TO  WRK-HENKIN-ZAN
           ELSE
               MOVE    LNK-SCNYUKIN-HENKIN-MONEY
                                       TO  WRK-HENKIN-ZAN
           END-IF
      *
           MOVE    ZERO                TO  IDX
      *
           PERFORM UNTIL ( FLG-SYUNOU      =   1)
                    OR   ((WRK-NYUKIN-ZAN  =  ZERO)
                       AND(WRK-HENKIN-ZAN  =  ZERO))
      *
               MOVE    ZERO            TO  WRK-MISYU
               ADD     1               TO  IDX
               COMPUTE WRK-MISYU
                   =   SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL
               MOVE    SYU-DENPNUM     TO  LNK-SCNYUKIN-DENPNUM (IDX)
               MOVE    SYU-SKYMONEY    TO  LNK-SCNYUKIN-SKYMONEY(IDX)
      *
               IF    ( WRK-HENKIN-ZAN  >   ZERO )
                   IF    ( WRK-HENKIN-ZAN >=   WRK-MISYU )
                       MOVE   WRK-MISYU
                                       TO  LNK-SCNYUKIN-FURIWAKEMONEY
                                                             ( IDX 1)
                       COMPUTE WRK-HENKIN-ZAN
                           =   WRK-HENKIN-ZAN  -  WRK-MISYU
                       MOVE    ZERO    TO  WRK-MISYU
                   ELSE
                       COMPUTE WRK-MISYU
                           =   WRK-MISYU       -  WRK-HENKIN-ZAN
                       MOVE    WRK-HENKIN-ZAN
                                       TO  LNK-SCNYUKIN-FURIWAKEMONEY
                                                             (IDX 1)
                       MOVE   ZERO     TO  WRK-HENKIN-ZAN
                   END-IF
               END-IF
      *
               IF    ( WRK-HENKIN-ZAN <=   ZERO )
                AND  ( WRK-MISYU       >   ZERO )
                   IF    ( WRK-NYUKIN-ZAN  >=  WRK-MISYU )
      *
                       MOVE   WRK-MISYU
                                       TO  LNK-SCNYUKIN-FURIWAKEMONEY
                                                             (IDX 2)
                       COMPUTE WRK-NYUKIN-ZAN
                           =   WRK-NYUKIN-ZAN  -   WRK-MISYU
                   ELSE
                       MOVE    WRK-NYUKIN-ZAN
                                       TO  LNK-SCNYUKIN-FURIWAKEMONEY
                                                             (IDX 2)
                       MOVE    ZERO    TO   WRK-NYUKIN-ZAN
                   END-IF
               END-IF
      *
               IF    ( LNK-SCNYUKIN-KOUSHINKBN     =   "2" )
                   PERFORM   300-SYUNOU-UPD-SEC
                   PERFORM   310-SYUMEI-INS-SEC
               END-IF
      *
               MOVE    "tbl_syunou"    TO  MCP-TABLE
               MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SYUNOU-FETCH-SEC
      *
           END-PERFORM
      *
      *    ��Ǽ��������
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( IDX                 =   ZERO )
               IF    ( LNK-SCNYUKIN-HENKIN-MONEY
                                       =   ZERO )
                AND  ( LNK-SCNYUKIN-HENKINKBN  =   SPACE )
                   MOVE    11          TO  LNK-SCNYUKIN-RC
                   GO  TO  200-MAIN-EXT
               END-IF
           END-IF
      *
      *    �ֶ⿶��ʬ���ĳۤ�����
           IF    ( LNK-SCNYUKIN-HENKINKBN  =   "1" )
               MOVE    LNK-SCNYUKIN-HENKIN-MONEY
                                           TO  LNK-SCNYUKIN-HENKINZAN
           ELSE
               IF    ( WRK-HENKIN-ZAN      NOT =   ZERO )
                   MOVE  WRK-HENKIN-ZAN    TO  LNK-SCNYUKIN-HENKINZAN
               END-IF
           END-IF
      *
      *    ���⿶��ʬ���ĳۤ�����
           IF    ( WRK-NYUKIN-ZAN      NOT =   ZERO )
               MOVE  WRK-NYUKIN-ZAN    TO  LNK-SCNYUKIN-FURIWAKEZAN
           END-IF
      *
           IF    ( LNK-SCNYUKIN-KOUSHINKBN =   "2" )
            AND (( LNK-SCNYUKIN-HENKINKBN  =   "1" )
             OR  ( LNK-SCNYUKIN-HENKIN-MONEY   >   ZERO ))
               PERFORM 210-SYUNOU-HENKIN-SEC
               IF    ( LNK-SCNYUKIN-RC NOT =   ZERO )
                   GO  TO  200-MAIN-EXT
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��Ǽ�ֶ����
      *****************************************************************
       210-SYUNOU-HENKIN-SEC           SECTION.
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    LNK-SCNYUKIN-HOSPNUM TO  SYU-HOSPNUM
           MOVE    LNK-SCNYUKIN-PTID   TO  SYU-PTID
           MOVE    LNK-SCNYUKIN-NYUGAIKBN
                                       TO  SYU-NYUGAIKBN
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 900-SYUNOU-SELECT-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU  =   1 )
      *
               COMPUTE WRK-HENKIN
                   =   SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL
      *
               IF    ( WRK-HENKIN      <   ZERO )
                   PERFORM 2101-SYUMEI-HENKIN-SEC
      *
                   PERFORM 2101-SYUNOU-HENKIN-SEC
               END-IF
      *
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 900-SYUNOU-FETCH-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       210-SYUNOU-HENKIN-EXT.
           EXIT.
      *****************************************************************
      *    ��Ǽ�����ֶ����
      *****************************************************************
       2101-SYUMEI-HENKIN-SEC          SECTION.
      *
           PERFORM 800-GET-DENPEDANUM-SEC
      *
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
           MOVE    WRK-DENPEDANUM      TO  SMEI-DENPEDANUM
           MOVE    ZERO                TO  SMEI-NYUKINRENNUM
           MOVE    SYU-SRYKA           TO  SMEI-SRYKA
           MOVE    SPACE               TO  SMEI-MEISAIJOUTAIKBN
           MOVE    SPACE               TO  SMEI-SKYPRINTYMD
           MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
           MOVE    SPACE               TO  SMEI-RYOSYUPRINTYMD
           MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
           MOVE    ZERO                TO  SMEI-SKYMONEY
           MOVE    "2"                 TO  SMEI-NYUHEN-KBN
           MOVE    WRK-HENKIN          TO  SMEI-NYUHEN-MONEY
           MOVE    SPA-SYSYMD          TO  SMEI-NYUHEN-YMD
           MOVE    "00"                TO  SMEI-NYUKIN-HOHO
           MOVE    "4"                 TO  SMEI-JOUTAIKBN
      *
           MOVE    SMCNDATE-YMD        TO  SMEI-CREYMD
                                           SMEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SMEI-CREHMS
                                           SMEI-UPHMS
      *
           MOVE    SMEI-CREYMD         TO  SDAILYKEY-CREYMD
           MOVE    SMEI-CREHMS         TO  SDAILYKEY-CREHMS
           MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
           PERFORM 800-DAILYKEY-SEC
           MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
      *
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "1"         TO  LNK-SCNYUKIN-MATOME-HENKIN-KBN
           ELSE
               DISPLAY "SYUMEI INSERT ERR " MCP-RC
               MOVE    15              TO  LNK-SCNYUKIN-RC
           END-IF
      *
           .
       2101-SYUMEI-HENKIN-EXT.
           EXIT.
      *****************************************************************
      *    ��Ǽ�ֶ����
      *****************************************************************
       2101-SYUNOU-HENKIN-SEC          SECTION.
      *
           MOVE    SYU-SKYMONEY        TO  SYU-NYUKIN-TOTAL
      *
           MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
           MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               DISPLAY "SYUNOU INSERT ERR " MCP-RC
               MOVE    15              TO  LNK-SCNYUKIN-RC
           END-IF
      *
           .
       2101-SYUNOU-HENKIN-EXT.
           EXIT.
      *****************************************************************
      *    ��Ǽ��������
      *****************************************************************
       300-SYUNOU-UPD-SEC                SECTION.
      *
      *    ���⿶��ʬ���ۥ��å�
           ADD     LNK-SCNYUKIN-FURIWAKEMONEY(IDX 1)
                                       TO  SYU-NYUKIN-TOTAL
           ADD     LNK-SCNYUKIN-FURIWAKEMONEY(IDX 2)
                                       TO  SYU-NYUKIN-TOTAL
           ADD     1                   TO  SYU-NYUKIN-KAISU
           IF    ( SYU-SKYMONEY        >   SYU-NYUKIN-TOTAL )
                   MOVE   "2"          TO  SYU-DENPJTIKBN
           ELSE
                   MOVE   "1"          TO  SYU-DENPJTIKBN
           END-IF
      *
           MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
           MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
      *
      *    ��Ǽ����
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC              NOT =   ZERO )
                   DISPLAY "ORCSNYUKIN SYUNOU UPD ERR:"  MCP-RC  "."
                   MOVE    13          TO  LNK-SCNYUKIN-RC
           END-IF
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
           PERFORM 800-GET-DENPEDANUM-SEC
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   2 )
      *
               IF    ( LNK-SCNYUKIN-FURIWAKEMONEY(IDX IDX2)
                                   >   ZERO )
      *
                   PERFORM 3101-SYUMEI-INS-SEC
      *
                   COMPUTE WRK-DENPEDANUM
                       =   WRK-DENPEDANUM  +   1
               END-IF
      *
           END-PERFORM
           .
       310-SYUMEI-INS-EXT.
           EXIT.
      *
      *************************************************************
      *    ��Ǽ���ٺ���
      *************************************************************
       3101-SYUMEI-INS-SEC             SECTION.
      *
      *��Ǽ���ٺ�������
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
      *��ɼ�ֹ�
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *��ɼ�ֹ����
           MOVE    WRK-DENPEDANUM      TO  SMEI-DENPEDANUM
      *����Ϣ��
           MOVE    ZERO                TO  SMEI-NYUKINRENNUM
      *���Ų�
           MOVE    SYU-SRYKA           TO  SMEI-SRYKA
      *��ɼ���ֶ�ʬ
           IF    ( IDX2                =   1 )
               MOVE    "1"             TO  SMEI-MEISAIJOUTAIKBN
           END-IF
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
           MOVE    "1"                 TO  SMEI-NYUHEN-KBN
      *���ֶ��
           MOVE    LNK-SCNYUKIN-FURIWAKEMONEY(IDX IDX2)
                                       TO  SMEI-NYUHEN-MONEY
      *���ֶ���
           MOVE    LNK-SCNYUKIN-NYUKIN-YMD
                                       TO  SMEI-NYUHEN-YMD
      *������ˡ
           IF    ( IDX2                =   1 )
                   MOVE    "01"        TO  SMEI-NYUKIN-HOHO
           ELSE
               IF    ( LNK-SCNYUKIN-NYUKIN-HOHO    =   SPACE )
                   MOVE    "01"        TO  SMEI-NYUKIN-HOHO
               ELSE
                   MOVE    LNK-SCNYUKIN-NYUKIN-HOHO
                                       TO  SMEI-NYUKIN-HOHO
               END-IF
           END-IF
      *���ֶ�ʬ
           MOVE    "2"                 TO  SMEI-JOUTAIKBN
      *
           MOVE    SMCNDATE-YMD        TO  SMEI-CREYMD
                                           SMEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SMEI-CREHMS
                                           SMEI-UPHMS
      *
           MOVE    SMEI-CREYMD         TO  SDAILYKEY-CREYMD
           MOVE    SMEI-CREHMS         TO  SDAILYKEY-CREHMS
           MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
           PERFORM 800-DAILYKEY-SEC
           MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
      *
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "1"         TO  LNK-SCNYUKIN-MATOME-NYUKIN-KBN
           ELSE
               DISPLAY "ORCSNYUKIN SYUMEI INS ERR:"  MCP-RC  "."
               MOVE    14              TO  LNK-SCNYUKIN-RC
           END-IF
      *
           .
       3101-SYUMEI-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ɼ�ֹ���Ǽ�������
      *****************************************************************
       800-GET-DENPEDANUM-SEC          SECTION.
      *
           PERFORM 900-SYUMEI-KEY14-SEL-SEC
      *
           COMPUTE WRK-DENPEDANUM  =   SMEI-DENPEDANUM +   1
      *
           .
       800-GET-DENPEDANUM-EXT.
           EXIT.
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
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��Ǽ�ޥ���������
      *****************************************************************
       900-SYUNOU-SELECT-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��Ǽ�ޥ������ɹ�
      *****************************************************************
       900-SYUNOU-FETCH-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��Ǽ���ٸ������� (KEY14)
      *****************************************************************
       900-SYUMEI-KEY14-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
      *
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUMEI-KEY14-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¥��󥵡��Ƚ���
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¹�������
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    �ģº������
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.