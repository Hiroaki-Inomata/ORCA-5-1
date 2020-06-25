      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
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
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : まとめ入金サブ
      *  管理者            : 
      *  作成日付    作業者        記述
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
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATHNAME        PIC X(20).
           03  WRK-NYUKIN-ZAN      PIC 9(08).
           03  WRK-MISYU           PIC 9(07).
           03  WRK-HENKIN-ZAN      PIC 9(08).
           03  WRK-DENPEDANUM      PIC 9(02).
           03  WRK-HENKIN          PIC S9(10).
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日報キー取得サブ
           COPY "CPORCSDAILYKEY.INC".
      *
      *    共通領域
           COPY    "MCPAREA".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1001.INC".
      *
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                         SECTION.
      *
           COPY    "CPORCSNYUKIN.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                       DIVISION    USING
           ORCSNYUKINAREA
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
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
      *    パラメータチェック
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
      *    パラメータチェック
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
      *    患者マスター検索
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
      *    主取得
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *    マシン日付取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    収納読み込み用ＫＥＹの指定
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
      *    収納マスターから未収金情報の取得
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
      *    収納クローズ
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
      *    返金振り分け残額の設定
           IF    ( LNK-SCNYUKIN-HENKINKBN  =   "1" )
               MOVE    LNK-SCNYUKIN-HENKIN-MONEY
                                           TO  LNK-SCNYUKIN-HENKINZAN
           ELSE
               IF    ( WRK-HENKIN-ZAN      NOT =   ZERO )
                   MOVE  WRK-HENKIN-ZAN    TO  LNK-SCNYUKIN-HENKINZAN
               END-IF
           END-IF
      *
      *    入金振り分け残額の設定
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
      *    収納返金処理
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
      *    収納明細返金処理
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
      *    収納返金処理
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
      *    収納更新処理
      *****************************************************************
       300-SYUNOU-UPD-SEC                SECTION.
      *
      *    入金振り分け額セット
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
      *    収納更新
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
      *    収納明細作成
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
      *    収納明細作成
      *************************************************************
       3101-SYUMEI-INS-SEC             SECTION.
      *
      *収納明細作成処理
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
      *伝票番号
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *伝票番号枝番
           MOVE    WRK-DENPEDANUM      TO  SMEI-DENPEDANUM
      *入金連番
           MOVE    ZERO                TO  SMEI-NYUKINRENNUM
      *診療科
           MOVE    SYU-SRYKA           TO  SMEI-SRYKA
      *伝票状態区分
           IF    ( IDX2                =   1 )
               MOVE    "1"             TO  SMEI-MEISAIJOUTAIKBN
           END-IF
      *請求発行日（発行日の西暦）
           MOVE    SPACE               TO  SMEI-SKYPRINTYMD
      *請求再発行日
           MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *領収書発行日
           MOVE    SPACE               TO  SMEI-RYOSYUPRINTYMD
      *領収書再発行日
           MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *請求金額
           MOVE    ZERO                TO  SMEI-SKYMONEY
      *入返金区分
           MOVE    "1"                 TO  SMEI-NYUHEN-KBN
      *入返金額
           MOVE    LNK-SCNYUKIN-FURIWAKEMONEY(IDX IDX2)
                                       TO  SMEI-NYUHEN-MONEY
      *入返金日
           MOVE    LNK-SCNYUKIN-NYUKIN-YMD
                                       TO  SMEI-NYUHEN-YMD
      *入金方法
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
      *状態区分
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
      *    伝票番号枝版取得処理
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
      *    日報キー取得サブ
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
      *    患者マスター読込
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
      *    収納マスター検索
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
      *    収納マスター読込
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
      *    収納明細検索処理 (KEY14)
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
      *    ＤＢ検索処理
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
      *    ＤＢ読み込み処理
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
      *    ＤＢクローズ処理
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
      *    ＤＢインサート処理
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
      *    ＤＢ更新処理
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
      *    ＤＢ削除処理
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
