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
       PROGRAM-ID.             ORCHC503.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為
      *  コンポーネント名  : 入院指示箋　印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/02/15    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  06/01/25  MONFUNCへ変更
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  03.05.00    NACL-多々納  07/05/22  再印刷処理追加
      *  04.01.00    NACL-多々納  07/11/22  器材商品名コード対応
      *  04.05.00    NACL-多々納  09/06/XX  数量小数５桁対応
      *  04.08.00    NACL-多々納  14/08/01  一時ディレクトリ対応
      *  05.00.00    NACL-多々納  19/05/XX  API(GINBEE）対応
      *  05.00.00    NACL-太田    17/10/11  API(GINBEE）対応
      *  05.01.00    NACL-多々納  20/02/XX  ユーザー単位対応
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
      **** SELECT  PRT-FILE        ASSIGN  HC01PARA
           SELECT  PRT-FILE        ASSIGN  ASGNPARA
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
           COPY    "HC503.INC".
      *
       WORKING-STORAGE             SECTION.
      *
      *     印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
      *v4.8
      *    一時デイレクトリ領域 
           COPY    "CPASGNPARA.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-ORDERPRT        PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *R02.2
           03  FLG-TENSUPLUS       PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-KIZAI           PIC 9(01).
           03  FLG-KETA5           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-SMAX            PIC 9(04).
           03  IDX-H               PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDX-B               PIC 9(04).
           03  IDX-P               PIC 9(04).
           03  IDX-P2              PIC 9(04).
      *
           03  IDX-PAGE            PIC 9(04).
           03  IDX-LINE            PIC 9(04).
      *
           03  IDXM                PIC 9(02).
           03  IDXN                PIC 9(02).
      *
           03  KETA                PIC 9(04).
      *API
           03  IDX-AP              PIC 9(04).
           03  IDX-AP1             PIC 9(04).
           03  IDX-AP2             PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-PAGE            PIC 9(03).
      *
           03  WRK-PRTKBN          PIC  X(01).
           03  WRK-DRCD            PIC  X(05).
      *
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-SRYCD           PIC X(09).
           03  WRK-SRYYMD          PIC X(08).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WYY         PIC X(03).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-SRYYMD-HEN.
               05  WRK-SRYYY-H     PIC X(10).
               05  WRK-SRYMMDD-H   PIC X(12).
      *
           03  WRK-AGE-ZZ9X.
               05  WRK-AGE-ZZ9             PIC ZZ9.
           03  WRK-AGE-Z9X.
               05  WRK-AGE-Z9              PIC Z9.
           03  WRK-AGE-HEN.
               05  WRK-AGE-N               PIC X(04).
               05  WRK-AGE-MEIN            PIC X(04).
           03  WRK-AGE-HEN-R               REDEFINES   WRK-AGE-HEN.
               05  WRK-AGE-N2              PIC X(06).
               05  WRK-AGE-MEIN2           PIC X(02).
      *
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
      *
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZZZ.
           03  WRK-SURYO-N.
               05  WRK-SURYO-N-22      PIC X(04).
               05  WRK-SURYO-N-18      PIC X(18).
      *
           03  WRK-DAY                 PIC 9(02).
           03  WRK-KAISU               PIC 9(03).
           03  WRK-DAY-X.
               05  WRK-DAY-Z9          PIC Z9.
           03  WRK-KAISU-X.
               05  WRK-KAISU-Z9        PIC Z9.
           03  WRK-KAISU-HENSYU.
               05  WRK-KAISU-N         PIC X(04).
               05  WRK-KAISU-H2        PIC X(02).
      *
           03  WRK-TANINAME            PIC X(04).
           03  WRK-ZZ                  PIC ZZ.
           03  WRK-TNS-NAME            PIC X(200).
      *
      *    文字変換
           03  WRK-ZENKAKU-G           PIC X(100).
           03  WRK-MOJI                PIC X(100).
           03  WRK-MOJISU              PIC 9(04).
      *
      *    麻薬編集
           03  WRK-BIKOH-AREA.
               05  WRK-BIKOH           PIC X(110)   OCCURS  3.
      *
      *    帳票用
           03  WRK-SEX                 PIC X(02).
           03  WRK-BIRTHDAY            PIC X(22).
           03  WRK-BTUNAME             PIC X(20).
           03  WRK-ROOMNO              PIC X(06).
           03  WRK-SYSYMD              PIC X(22).
           03  WRK-TIME-G.
               05  WRK-TIME-HH         PIC X(02).
               05  WRK-TIME-P          PIC X(01).
               05  WRK-TIME-SS         PIC X(02).
               05  WRK-SIJISEN-HEAD    PIC X(30).
               05  WRK-SIJISEN-PRT     PIC 9(02).
               05  WRK-SRYKBNMEI       PIC X(30).
      *
           03  WRK-BRMNUM              PIC 9(06).
           03  WRK-Z6                  PIC ZZZZZ9.
      *
      *    商品名単位名称
           03  WRK-SYO-TANINAME        PIC X(12).
      *
      *    見出し名称
       01  WRK-SIJISEN-AREA.
           03  WRK-SIJISEN-HEAD1-G     OCCURS   4.
               05  WRK-SIJISEN-HEAD1       PIC X(30).
               05  WRK-SIJISEN-PRT1        PIC 9(02).
      *    剤毎の内容
       01  WRK-NAIYOU-AREA.
           03  WRK-NAIYOU-TBL          OCCURS   100.
               05  WRK-NAIYOU-Z            PIC X(112).
               05  WRK-NAIYOU-R1           REDEFINES   WRK-NAIYOU-Z.
                   07  WRK-FIL10           PIC X(02).
                   07  WRK-MEISYOU1        PIC X(70).
                   07  WRK-SURYOU          PIC X(22).
                   07  WRK-TANI            PIC X(12).
                   07  WRK-KAISUU          PIC X(06).
     ********      07  WRK-FIL11           PIC X(02).
               05  WRK-NAIYOU-R3           REDEFINES   WRK-NAIYOU-Z.
                   07  WRK-FIL20           PIC X(02).
                   07  WRK-MEISYOU3        PIC X(100).
                   07  WRK-FIL21           PIC X(10).
      *
      *    処方内容
       01  WRK-SYOHOU-AREA.
           03  WRK-SYOHOU-DAY          OCCURS   31.
               05  WRK-SYOHOU-PAGE         OCCURS   10.
                   07  WRK-SYOHOU-TBL          OCCURS   35.
                       09  WRK-NAIYOU          PIC X(112).
      *
               05  WRK-PAGE-MAX                PIC 9(03).
               05  WRK-LINE-MAX                PIC 9(03).
      *
      *    処方内容最大行
       01  WRK-IDX-MAX                 PIC 9(03)  VALUE   35.
      *
      *H25.5
      *API(gingee)
       01  WRK-API-AREA.
      *    日付編集
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *    時間編集
           03  WRK-THMS.
               05  WRK-THH             PIC X(02).
               05  WRK-TMM             PIC X(02).
               05  WRK-TSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *    病棟番号
           03  WRK-BTUNUM              PIC X(02).
      *    日付
           03  WRK-API-SYSYMD          PIC X(10).
           03  WRK-API-SYSTIME         PIC X(05).
           03  WRK-API-SRYYMD          PIC X(10).
           03  WRK-API-JISIYMD         PIC X(10).
      *
      *    剤内容
       01  WRK-AIP-ZAI-AREA.
           03  WRK-API-ZAI-SYOHOU-G        OCCURS   50.
               05  WRK-ZAI-YKZNAME         PIC X(200).
               05  WRK-ZAI-SURYO           PIC X(12).
               05  WRK-ZAI-TANINAME        PIC X(12).
               05  WRK-ZAI-SRYCD           PIC X(09).
      *    処方内容
       01  WRK-API-SYOHOU-AREA.
           03  WRK-API-SYOHOU-DAY-G        OCCURS   31.
               05  WRK-API-ZAI-MAX         PIC 9(03).
               05  WRK-API-SYOHOU-Z-G      OCCURS   50.
                   07  WRK-API-KAISU           PIC X(3).
                   07  WRK-API-KAISU-MEI       PIC X(6).
                   07  WRK-API-SYOHOU-G2       OCCURS   50.
                       09  WRK-API-YKZNAME         PIC X(200).
                       09  WRK-API-SURYO           PIC X(12).
                       09  WRK-API-TANINAME        PIC X(12).
                       09  WRK-API-SRYCD           PIC X(09).
      *
      *H29.5
      *    プログラムオプションパラメタ
       01  PARA-AREA.
           03  PARA-API-DATA           PIC X(01).
           03  PARA-API-CUSTOM-ID      PIC X(30).
      *
           03  WRK-PARA-NAME           PIC X(25).
           03  WRK-PARA                PIC X(50).
           03  WRK-PARA-MAX            PIC 9(03).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    医療機関情報−基本情報 
      ***  COPY    "CPSK1001.INC".
      *    医療機関情報−所在地、連絡先 
      ***  COPY    "CPSK1002.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1010.INC".
      *    処方箋ＧＥ表示
      **** COPY    "CPSK1030.INC".
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1034.INC".
      *    入院指示せん設定情報
           COPY  "CPSK5007.INC".
      *    （病棟情報）
           COPY    "CPSK5001.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *R02.2
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    入院帳票オーダ
       01  ORDERPRT-REC.
           COPY    "CPORDERPRT.INC".
      *
       01  LNK-ORDERPRT-AREA.
           02  LNK-ORDERPRT-REC      OCCURS   50.
           COPY    "CPORDERPRT.INC"  REPLACING
                                     //ORDERPRT-// BY //LNK-ORDERPRT-//.
       01  LNK-ORDERPRT-MAX           PIC 9(04).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    保険年齢パラメタ
           COPY    "CPORCSAGECHK.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *v4.8
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *H29.5
      *
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
      * API対応
      *    入院指示せん情報構造体
           COPY    "CPFMSHIJI.INC".
      *
           COPY    "CPORCRPMAIN.INC".
      *
      *
           COPY    "MCPAREA".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *****COPY    "ORCA-DBPATH".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    処方箋共通パラメタ
           COPY    "CPORCHC503.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCHC503AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *H29.5
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *
      *    各ＤＢ読込
           PERFORM 100-SYORI-SEC
      *
           EVALUATE    ORCHC503-SYORIKBN
               WHEN    ZERO
      *            処方内容編集
                   PERFORM 200-MAIN-SYORI-SEC
               WHEN    1
      *            頭書印刷
                   PERFORM 200-HEAD-PRINT-SEC
           END-EVALUATE
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *H29.5
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           INITIALIZE                  PARA-AREA
           MOVE    ZERO                TO  WRK-PARA-MAX
      *    ＡＰＩ返却データ（1:作成する 帳票印刷なし）
           MOVE    "API_DATA"          TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA(1:1)       =   "1" )
               MOVE    WRK-PARA(1:1)       TO  PARA-API-DATA
           END-IF
      *
           IF     (PARA-API-DATA       =   "1"       )
               CONTINUE
           ELSE
      *        API送信なし
               MOVE    "0"             TO  PARA-API-DATA
           END-IF
      *
      *    HAORI対応
           IF     ( SPA-API-FLG    NOT =   ZERO )
               IF    ( SPA-PRTCONF     =   "2" OR "3" )
                   MOVE    ZERO        TO  PARA-API-DATA
               ELSE
                   MOVE    "1"         TO  PARA-API-DATA
               END-IF
           END-IF
      *
      *    ＡＰＩカスタムＩＤ
           MOVE    "API_CUSTOM_ID"     TO  WRK-PARA-NAME
           MOVE    30                  TO  WRK-PARA-MAX
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA            TO  PARA-API-CUSTOM-ID
           .
       100-OPTION-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプションパラメタ取得処理
      *****************************************************************
       1001-OPTION-CALL-SEC             SECTION.
      *
           INITIALIZE                  ORCSPRGOPTIONAREA
           MOVE    "ORCHC503"          TO  ORCSPRGOPTION-PRGID
           MOVE    WRK-PARA-NAME       TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
           MOVE    SPACE               TO  WRK-PARA
           IF      ORCSPRGOPTION-OT-TYPE   =   1
      *        半角で１文字目を値とする
               MOVE    ORCSPRGOPTION-OT-PARA(1:1)  TO  WRK-PARA(1:1)
               IF      WRK-PARA-MAX        >   ZERO
                   MOVE    ORCSPRGOPTION-OT-PARA(1:WRK-PARA-MAX)
                                               TO  WRK-PARA
               END-IF
           ELSE
               IF      WRK-PARA-MAX        >   ZERO
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI
                   MOVE    ORCSPRGOPTION-OT-PARA(1:WRK-PARA-MAX)
                                               TO  KANACHK-MAE-INPUT
                   MOVE    WRK-PARA-MAX        TO  KANACHK-MAX-CNT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   MOVE    KANACHK-MAE-INPUT   TO  WRK-PARA
               END-IF
           END-IF
           .
       1001-OPTION-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       100-SYORI-SEC                SECTION.
      *
      *    患者情報
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               INITIALIZE              PTINF-REC
           END-IF
      *
      *    入院履歴判定
           MOVE    SPACE               TO  PTNYUINRRK-REC
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key36"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-PTNYUINRRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
           MOVE    ZERO                TO  FLG-OK
           PERFORM
                   UNTIL      (FLG-PTNYUINRRK      =   1 )  OR
                              (FLG-OK              =   1 )
      *        転入日
               IF      PTNYUINRRK-TENNYUYMD    <=  ORCHC503-SYSYMD
      *            転出日
                   IF     (PTNYUINRRK-TENSTUYMD    =   SPACE )  OR
                          (PTNYUINRRK-TENSTUYMD    NOT =   SPACE AND
                           PTNYUINRRK-TENSTUYMD    >=  ORCHC503-SYSYMD)
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-IF
      *
               IF      FLG-OK              =   ZERO
                   MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
                   MOVE    "key36"             TO  MCP-PATHNAME
                   PERFORM 900-PTNYUINRRK-READ-SEC
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key36"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    病棟
           MOVE    SPACE               TO  SYS-5001-REC
           INITIALIZE                  SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    PTNYUINRRK-BTUNUM   TO  SYS-5001-KBNCD
           MOVE    SYS-5001-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-5001-REC
           ELSE
               INITIALIZE                      SYS-5001-REC
           END-IF
      *
           MOVE    SYS-5001-BTU-NAME   TO  WRK-BTUNAME
      *API
      *    病棟番号
           MOVE    PTNYUINRRK-BTUNUM   TO  WRK-BTUNUM
      *
      *    病室番号
           IF      PTNYUINRRK-BRMNUM (3:6) IS  NUMERIC
               MOVE    PTNYUINRRK-BRMNUM (3:6)
                                           TO   WRK-BRMNUM
               MOVE    WRK-BRMNUM          TO   WRK-Z6
               MOVE    WRK-Z6              TO   WRK-ROOMNO
           ELSE
               MOVE    PTNYUINRRK-BRMNUM (3:6)
                                           TO  WRK-ROOMNO
           END-IF
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1034-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    ORCHC503-SYSYMD     TO  ORCSPRTNM-SRYYMD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1034-REC
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    ORCHC503-SRYKA      TO  SYS-1005-KBNCD
           MOVE    SYS-1005-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
           ELSE
               INITIALIZE                      SYS-1005-REC
           END-IF
      *
      *    処方箋見出し名称
           INITIALIZE  WRK-SIJISEN-AREA
           MOVE    SPACE               TO  SYS-5007-REC
           INITIALIZE                      SYS-5007-REC
           MOVE    "5007"              TO  SYS-5007-KANRICD
           MOVE    ORCHC503-SRYYMD     TO  SYS-5007-STYUKYMD
           MOVE    ORCHC503-SRYYMD     TO  SYS-5007-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-5007-HOSPNUM
           MOVE    SYS-5007-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
           END-IF
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-5007-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
               INITIALIZE                      SYS-5007-REC
           END-IF
           PERFORM
                   UNTIL       FLG-SYSKANRI    =   1
               EVALUATE    SYS-5007-KBNCD
                   WHEN    "05"
      *                指示箋
                       PERFORM VARYING IDX     FROM    1   BY  1
                               UNTIL   IDX     >   4
                           MOVE    SYS-5007-SIJISEN-HEAD(IDX)
                                           TO  WRK-SIJISEN-HEAD1 (IDX)
                           MOVE    SYS-5007-SIJISEN-PRT (IDX)
                                           TO  WRK-SIJISEN-PRT1 (IDX)
                       END-PERFORM
               END-EVALUATE
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-5007-REC
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       100-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SYORI-SEC                   SECTION.
      *
      *    見出し共通編集
           PERFORM 2001-HEAD-HENSYU-SEC
           MOVE    SPACE               TO  WRK-DRCD
      *
      *    処置
           MOVE    "40"                TO  WRK-SRYKBN
      *    明細編集処理
           PERFORM 2001-HENSYU-SYORI-SEC
      *
      *    リハビリ
           MOVE    "80"                TO  WRK-SRYKBN
      *    明細編集処理
           PERFORM 2001-HENSYU-SYORI-SEC
      *
      *    ＤＢ削除処理
           PERFORM 500-DELETE-SEC
      *
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    見出し共通編集処理
      *****************************************************************
       2001-HEAD-HENSYU-SEC          SECTION.
      *
      *    性別
           EVALUATE   PTINF-SEX
              WHEN    1
                  MOVE    "男"             TO  WRK-SEX
              WHEN    2
                  MOVE    "女"             TO  WRK-SEX
           END-EVALUATE
      *    生年月日
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 510-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-BIRTHDAY
      *    年齢
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    ORCHC503-SYSYMD     TO  AGECHK-SRYYMD
           MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           IF      AGECHK-NENREI2-YY   =   ZERO
               MOVE    AGECHK-NENREI2-MM   TO  WRK-AGE-Z9
               MOVE    WRK-AGE-Z9X         TO  WRK-MOJI
               PERFORM 500-HENKAN-SEC
               MOVE    WRK-ZENKAKU-G(1:4)  TO  WRK-AGE-N
               MOVE    "ヶ月"              TO  WRK-AGE-MEIN
           ELSE
               MOVE    AGECHK-NENREI2-YY   TO  WRK-AGE-ZZ9
               MOVE    WRK-AGE-ZZ9X        TO  WRK-MOJI
               PERFORM 500-HENKAN-SEC
               MOVE    WRK-ZENKAKU-G(1:6)  TO  WRK-AGE-N2
               MOVE    "才"                TO  WRK-AGE-MEIN2
           END-IF
      *
      *    処方日編集
           MOVE    ORCHC503-SYSYMD     TO  WRK-SYMD
           PERFORM 510-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-SYSYMD
      *    処方時間
           MOVE    ORCHC503-PRINTHMS(1:2)
                                        TO  WRK-TIME-HH
           MOVE    ORCHC503-PRINTHMS(3:2)
                                        TO  WRK-TIME-SS
           MOVE    ":"                  TO  WRK-TIME-P
      *
      *H29.5
      *API GINGEE対応
           IF      PARA-API-DATA        =   "1"
      *        処理日
               MOVE    ORCHC503-PRINTYMD   TO  WRK-SYMD
               MOVE    ORCHC503-PRINTHMS   TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  WRK-API-SYSYMD
               MOVE    WRK-HEN-TIME        TO  WRK-API-SYSTIME
      *        処理日
               MOVE    ORCHC503-SYSYMD     TO  WRK-SYMD
               MOVE    SPACE               TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  WRK-API-SRYYMD
           END-IF
      *
           .
       2001-HEAD-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    見出しドクター編集処理
      *****************************************************************
       2002-DRCD-HENSYU-SEC          SECTION.
      *
      *    職員情報（ドクター）
           IF      WRK-DRCD            =   SPACE
               INITIALIZE                      SYS-1010-REC
           ELSE
               MOVE    SPACE               TO  SYS-1010-REC
               MOVE    "1010"              TO  SYS-1010-KANRICD
               MOVE    WRK-DRCD            TO  SYS-1010-KBNCD
               MOVE    SYS-1010-REC        TO  SYSKANRI-REC
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    SYSKANRI-REC        TO  SYS-1010-REC
               ELSE
                   INITIALIZE                      SYS-1010-REC
               END-IF
           END-IF
           .
       2002-DRCD-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       2001-HENSYU-SYORI-SEC                   SECTION.
      *
           INITIALIZE                  WRK-SYOHOU-AREA
      *API GINGEE対応
           INITIALIZE                  WRK-API-SYOHOU-AREA
           INITIALIZE                  WRK-AIP-ZAI-AREA
      *
      *    入院帳票オーダー検索
           INITIALIZE                  ORDERPRT-REC
           MOVE    SPA-HOSPNUM         TO  ORDERPRT-HOSPNUM
           MOVE    SPA-PTID            TO  ORDERPRT-PTID
           MOVE    ORCHC503-PTNUM      TO  ORDERPRT-PTNUM
           MOVE    SPA-NYUGAIKBN       TO  ORDERPRT-NYUGAIKBN
           MOVE    ORCHC503-SRYKA      TO  ORDERPRT-SRYKA
           MOVE    ORCHC503-HKNCOMBI   TO  ORDERPRT-HKNCOMBI
      *    システム日付
           MOVE    ORCHC503-SYSYMD     TO  ORDERPRT-SYSYMD
      *    発行日
           MOVE    ORCHC503-PRINTYMD   TO  ORDERPRT-PRINTYMD
      *    発行時間
           MOVE    ORCHC503-PRINTHMS   TO  ORDERPRT-PRINTHMS
      *    帳票区分
           MOVE    "6"                 TO  ORDERPRT-PRTKBN
      *    診療区分
           MOVE    WRK-SRYKBN          TO  ORDERPRT-SRYKBN
      *
           MOVE    ORDERPRT-REC        TO  MCPDATA-REC
           MOVE    "tbl_orderprt"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-ORDERPRT-READ-SEC
           END-IF
           INITIALIZE                  LNK-ORDERPRT-AREA
           INITIALIZE                  LNK-ORDERPRT-MAX
      *
           PERFORM UNTIL    FLG-ORDERPRT   =   1
      *        剤毎に編集する
               IF     (LNK-ORDERPRT-MAX    >   ZERO  )  AND
                      (ORDERPRT-RENNUM     =   1     )
                   PERFORM 220-NAIYOU-HENSYU-SEC
               END-IF
               ADD     1               TO  LNK-ORDERPRT-MAX
               MOVE    ORDERPRT-REC    TO  LNK-ORDERPRT-REC
                                              (LNK-ORDERPRT-MAX)
      *        ドクターはすべて同じとする
               IF      WRK-DRCD        =   SPACE
                   MOVE    ORDERPRT-DRCD   TO  WRK-DRCD
                   PERFORM 2002-DRCD-HENSYU-SEC
               END-IF
      *
               MOVE    "tbl_orderprt"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-ORDERPRT-READ-SEC
           END-PERFORM
           MOVE    "tbl_orderprt"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    剤毎に編集する
           IF      LNK-ORDERPRT-MAX    >   ZERO
               PERFORM 220-NAIYOU-HENSYU-SEC
           END-IF
      *
      *H29.5
      *API GINGEE対応
           IF      PARA-API-DATA        =   "1"
      *        ＡＰＩ明細処理
               PERFORM 700-API-SYORI-SEC
               GO      TO      2001-HENSYU-SYORI-EXT
           END-IF
      *
      *    帳票印刷
           MOVE    ORCHC503-SRYYMD     TO  WRK-SRYYMD
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   31
               IF      WRK-PAGE-MAX (IDX2) >   ZERO
                   MOVE    IDX2            TO  WRK-SRYYMD(7:2)
                   PERFORM 300-PRINT-SYORI-SEC
               END-IF
           END-PERFORM
      *
           .
       2001-HENSYU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       220-NAIYOU-HENSYU-SEC                   SECTION.
      *
      *    最初の日で点数マスタを検索する
           MOVE    ORCHC503-SRYYMD     TO  WRK-SRYYMD
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   31
               IF      LNK-ORDERPRT-DAY (1 IDX2)  >   ZERO
                   MOVE    IDX2            TO  WRK-SRYYMD(7:2)
                   MOVE    31              TO  IDX2
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  WRK-NAIYOU-AREA
           MOVE    ZERO                TO  IDX-S
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   LNK-ORDERPRT-MAX )  OR
                              (IDX-S   >=  100  )
               MOVE    LNK-ORDERPRT-SRYCD (IDX)    TO  WRK-SRYCD
               PERFORM 2001-ORDERPRT-HEN-SEC
           END-PERFORM
      *
      *H29.5
      *API GINGEE対応
           IF      PARA-API-DATA        =   "1"
      *        ＡＰＩ明細処理
               PERFORM 7002-API-MEISAISYORI-SEC
               INITIALIZE                  LNK-ORDERPRT-AREA
               INITIALIZE                  LNK-ORDERPRT-MAX
               GO      TO      220-NAIYOU-HENSYU-EXT
           END-IF
      *
           MOVE    IDX-S               TO  IDX-SMAX
      *
      *    日毎に編集
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   31
               IF      LNK-ORDERPRT-DAY (1 IDX2)   >   ZERO
      *            帳票毎のテーブルへ編集
                   MOVE    IDX-SMAX            TO  IDX-S
                   PERFORM 2209-PRINT-HENSYU-SEC
               END-IF
           END-PERFORM
      *
           INITIALIZE                  LNK-ORDERPRT-AREA
           INITIALIZE                  LNK-ORDERPRT-MAX
      *
           .
       220-NAIYOU-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    行明細編集処理
      *****************************************************************
       2001-ORDERPRT-HEN-SEC                   SECTION.
      *
           ADD     1                   TO  IDX-S
      *
      *    点数検索
           MOVE    LNK-ORDERPRT-SRYCD(IDX) TO  WRK-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *
           IF      LNK-ORDERPRT-INPUTCOMENT (IDX)  =   SPACE
               MOVE    TNS-NAME            TO  WRK-TNS-NAME
           ELSE
               MOVE    LNK-ORDERPRT-INPUTCOMENT(IDX)
                                           TO  WRK-TNS-NAME
           END-IF
      *    器材商品名コード
           IF      WRK-SRYCD (1:3)     =   "058"
               MOVE   TNS-FORMALNAME       TO  WRK-TNS-NAME
      *ver.4.7 単位名称
               MOVE    TNS-TANINAME        TO  WRK-SYO-TANINAME
               MOVE    SPACE               TO  TNS-TANINAME
           END-IF
      *!   器材商品名の器材
           IF     (WRK-SRYCD (1:1)     =   "7"  )  AND
                  (FLG-KIZAI           =   1    )
               MOVE    SPACE           TO  WRK-TNS-NAME
               STRING  "　〔"
                       TNS-NAME        DELIMITED  BY  SPACE
                       "〕"            DELIMITED  BY  SIZE
                                       INTO    WRK-TNS-NAME
               END-STRING
      *ver.4.7
      *        商品名コードの単位へ
               IF      WRK-SYO-TANINAME    NOT =   SPACE
                   MOVE    WRK-SYO-TANINAME    TO  TNS-TANINAME
                   MOVE    SPACE               TO  WRK-SYO-TANINAME
               END-IF
           END-IF
           IF      WRK-SRYCD (1:3)     =   "058"
               MOVE    1               TO  FLG-KIZAI
           ELSE
               MOVE    ZERO            TO  FLG-KIZAI
           END-IF
      *!
      *    名称編集
           PERFORM 20011-MEISYOU-HEN-SEC
      *
           INSPECT WRK-MEISYOU1 (IDX-S)    REPLACING  ALL "  "  BY  "　"
      *
           IF     (WRK-SRYCD (1:1)   =   "6"  OR  "7" )  OR
                  (WRK-SRYCD (1:3)   =   "059"        )
      *H29.5
      *        リハビリ単位
              OR ((WRK-SRYCD (1:1)   =   "1"   )  AND
                  (TNS-TANICD    NOT =   ZERO  ))
      *
      *        数量
               MOVE    LNK-ORDERPRT-SRYSURYO (IDX) TO  WRK-SURYO
               IF      WRK-SURYO           =   ZERO
                   MOVE    SPACE               TO  WRK-SURYO-N
               ELSE
                   PERFORM 20012-SURYO-HEN-SEC
               END-IF
               MOVE    WRK-SURYO-N             TO  WRK-SURYOU (IDX-S)
      *        数量単位
               MOVE    TNS-TANINAME            TO  WRK-TANI   (IDX-S)
           END-IF
      *
           INSPECT WRK-FIL10  (IDX-S)    REPLACING  ALL "  "  BY  "　"
           INSPECT WRK-SURYOU (IDX-S)    REPLACING  ALL "  "  BY  "　"
           INSPECT WRK-TANI   (IDX-S)    REPLACING  ALL "  "  BY  "　"
      *
           .
       2001-ORDERPRT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     名称編集処理
      *****************************************************************
       20011-MEISYOU-HEN-SEC           SECTION.
      *
           MOVE    WRK-TNS-NAME        TO  WRK-MEISYOU1   (IDX-S)
           IF      WRK-TNS-NAME(71:)   NOT =   SPACE
               MOVE    WRK-TNS-NAME        TO  WRK-MEISYOU3   (IDX-S)
               IF    ((WRK-SRYCD (1:1)     =   "6"  OR  "7" )  OR
                      (WRK-SRYCD (1:3)     =   "059"  ))
                 AND  (WRK-TNS-NAME(101:)  =   SPACE   )
                   ADD     1                   TO  IDX-S
               END-IF
           END-IF
           IF      WRK-TNS-NAME(101:)   NOT =   SPACE
               ADD     1                   TO  IDX-S
               MOVE    WRK-TNS-NAME(101:)  TO  WRK-MEISYOU3 (IDX-S)
               IF     (WRK-SRYCD (1:1)     =   "6"  OR  "7" )  OR
                      (WRK-SRYCD (1:3)     =   "059"  )
                   ADD     1                   TO  IDX-S
               END-IF
           END-IF
      *
           .
       20011-MEISYOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
       20012-SURYO-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           MOVE    ZERO                TO  FLG-KETA5
           PERFORM VARYING     IDXM   FROM    5  BY  -1
                   UNTIL       IDXM    <   1
               IF      WRK-SURYO-S2(IDXM:1) NOT =   ZERO
                   MOVE    1               TO  FLG-SP
                   IF      IDXM            >   3
                       MOVE    1               TO  FLG-KETA5
                   END-IF
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDXM:1) TO  WRK-SURYO-Z3(IDXM:1)
               END-IF
           END-PERFORM
      *
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
           END-IF
      *
           MOVE    SPACE               TO  WRK-SURYO-N
           IF      FLG-KETA5           =   ZERO
               MOVE    WRK-SURYO-X(1:9)    TO  WRK-MOJI
               PERFORM 500-HENKAN-SEC
               MOVE    WRK-ZENKAKU-G(1:18) TO  WRK-SURYO-N-18
           ELSE
               MOVE    WRK-SURYO-X         TO  WRK-MOJI
               PERFORM 500-HENKAN-SEC
               MOVE    WRK-ZENKAKU-G(1:22) TO  WRK-SURYO-N
           END-IF
      *
           .
       20012-SURYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票振り分け編集処理
      *****************************************************************
       2209-PRINT-HENSYU-SEC          SECTION.
      *
           MOVE    LNK-ORDERPRT-DAY (1 IDX2)  TO  WRK-KAISU
      *    回数編集
           IF      WRK-KAISU           >   1
               MOVE    WRK-KAISU           TO  WRK-KAISU-Z9
               MOVE    WRK-KAISU-X         TO  WRK-MOJI
               PERFORM 500-HENKAN-SEC
               MOVE    WRK-ZENKAKU-G(1:4)  TO  WRK-KAISU-N
               MOVE    "回"                TO  WRK-KAISU-H2
               MOVE    WRK-KAISU-HENSYU    TO  WRK-KAISUU(IDX-SMAX)
           ELSE
               MOVE    SPACE               TO  WRK-KAISU-HENSYU
               MOVE    WRK-KAISU-HENSYU    TO  WRK-KAISUU(IDX-SMAX)
           END-IF
      *
           MOVE    WRK-PAGE-MAX (IDX2) TO  IDX-PAGE
           MOVE    WRK-LINE-MAX (IDX2) TO  IDX-LINE
      *
           IF      IDX-PAGE            =   ZERO
               ADD     1                   TO  IDX-PAGE
      *        １頁目の１行目に実施日
               MOVE    2                   TO  IDX-LINE
           ELSE
      *        帳票改ページ編集処理
               MOVE    WRK-IDX-MAX         TO  IDX-MAX
               PERFORM 20022-PRINT-HEN-SEC
           END-IF
      *    最大行数−１のセット
           COMPUTE IDX-MAX             =   WRK-IDX-MAX  -  1
      *
           IF      IDX-SMAX            >   ZERO
               IF      IDX-LINE        >=   IDX-MAX
                   PERFORM 20022-PRINT-HEN-SEC
               END-IF
           END-IF
      *
           MOVE    IDX-SMAX            TO  IDX-S
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL   IDX         >   IDX-SMAX
               ADD     1                   TO  IDX-LINE
               MOVE    WRK-NAIYOU-Z (IDX)  TO  WRK-NAIYOU
                                              (IDX2 IDX-PAGE IDX-LINE)
               COMPUTE IDX-S           =   IDX-S   -  1
               IF      IDX-LINE        =   IDX-MAX
                   PERFORM 20022-PRINT-HEN-SEC
               END-IF
           END-PERFORM
           ADD     1                   TO  IDX-LINE
           IF      IDX-LINE            <   WRK-IDX-MAX
               MOVE    ALL "-"             TO  WRK-NAIYOU
                                              (IDX2 IDX-PAGE IDX-LINE)
           END-IF
           IF      IDX-LINE            >   WRK-IDX-MAX
               MOVE    WRK-IDX-MAX         TO  IDX-LINE
           END-IF
      *
           MOVE    IDX-PAGE            TO  WRK-PAGE-MAX (IDX2)
           MOVE    IDX-LINE            TO  WRK-LINE-MAX (IDX2)
      *
           .
       2209-PRINT-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    帳票用編集処理
      *****************************************************************
       20022-PRINT-HEN-SEC         SECTION.
      *
           IF     (IDX-S               =   1   )  AND
                  (IDX-LINE            =   IDX-MAX)
               CONTINUE
           ELSE
               IF     (IDX-S   +   IDX-LINE)   >   IDX-MAX
                   MOVE    "次ページへ続く"    TO  WRK-NAIYOU
                                     (IDX2 IDX-PAGE WRK-IDX-MAX)(51:)
                   IF      IDX-LINE         >   ZERO
      *            改ページ
                       ADD     1                   TO  IDX-PAGE
                       MOVE    ZERO                TO  IDX-LINE
                   END-IF
               END-IF
           END-IF
      *
           .
       20022-PRINT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       300-PRINT-SYORI-SEC          SECTION.
      *
           MOVE    WRK-PAGE-MAX (IDX2) TO  IDX-PAGE
           MOVE    WRK-LINE-MAX (IDX2) TO  IDX-LINE
      *
      *    実施日編集
           MOVE    WRK-SRYYMD          TO  WRK-SYMD
           PERFORM 510-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-SRYYMD-HEN
           STRING  "実施日："          DELIMITED  BY  SIZE
                   WRK-SRYYMD-HEN      DELIMITED  BY  SIZE
                              INTO  WRK-NAIYOU  (IDX2 1 1)
           END-STRING
      *
      *    最終行の設定
           PERFORM 3002-LAST-HENSYU-SEC
      *    印刷処理
           PERFORM 3003-PRINT-SYORI-SEC
           .
       300-PRINT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    最終行のメッセージ編集処理
      *****************************************************************
       3002-LAST-HENSYU-SEC          SECTION.
      *
           IF      IDX-LINE            <=  WRK-IDX-MAX
               IF     (WRK-NAIYOU (IDX2 IDX-PAGE IDX-LINE)
                                           =   SPACE ) AND
                      (IDX-LINE            =   WRK-IDX-MAX  )
                   MOVE    "　　　　　　以下余白"
                                           TO  WRK-NAIYOU
                                           (IDX2 IDX-PAGE IDX-LINE)
               END-IF
               ADD     1                   TO  IDX-LINE
           END-IF
           IF      IDX-LINE            <=  WRK-IDX-MAX
               MOVE    "　　　　　　以下余白"
                                           TO  WRK-NAIYOU
                                           (IDX2 IDX-PAGE IDX-LINE)
           END-IF
           .
       3002-LAST-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       3003-PRINT-SYORI-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-BIKOH-AREA
      *
      *    発行区分毎に印刷
           PERFORM VARYING     IDX-P   FROM    1   BY  1
                   UNTIL       IDX-P   >   4
               IF      ORCHC503-CHKPRT (IDX-P) =   1
                   EVALUATE    WRK-SRYKBN
                       WHEN    "40"
                           MOVE    "処置"          TO  WRK-SRYKBNMEI
                       WHEN    "80"
                           MOVE    "リハビリ・その他"
                                                   TO  WRK-SRYKBNMEI
                   END-EVALUATE
                   MOVE    SPACE               TO  WRK-SIJISEN-HEAD
                   STRING  WRK-SRYKBNMEI       DELIMITED  BY  SPACE
                           WRK-SIJISEN-HEAD1(IDX-P)
                                               DELIMITED  BY  SPACE
                                           INTO    WRK-SIJISEN-HEAD
                   END-STRING
                   MOVE    WRK-SIJISEN-PRT1 (IDX-P)
                                               TO  WRK-SIJISEN-PRT
                   PERFORM VARYING     IDX-P2  FROM    1   BY  1
                           UNTIL       IDX-P2  >   WRK-SIJISEN-PRT
                       PERFORM 3003-HC503-SYORI-SEC
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       3003-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     帳票発行処理
      *****************************************************************
       3003-HC503-SYORI-SEC      SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-PAGE
      *        帳票印刷開始処理
               PERFORM 400-SYOKI-PRINT-SEC
      *        帳票編集処理
               PERFORM 300-HC503-HEN-SEC
      *        帳票印刷処理
               PERFORM 400-PRINT-SEC
           END-PERFORM
           .
       3003-HC503-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       300-HC503-HEN-SEC                SECTION.
      *
           INITIALIZE                  HC503
      *
           MOVE    WRK-SIJISEN-HEAD    TO  HC503-TITLE
      *    発行日
           MOVE    ORCHC503-PRINTYMD   TO  HC503-SYSYMD
      *    発行時間
           MOVE    WRK-TIME-G          TO  HC503-TIME
      *    診療科
           MOVE    SYS-1005-SRYKANAME1 TO  HC503-SRYKA
      *    患者番号
           MOVE    ORCHC503-PTNUM      TO  HC503-PTNUM
      *    患者名称カナ
           MOVE    PTINF-KANANAME      TO  HC503-KANANAME
      *    患者名称
           MOVE    PTINF-NAME          TO  HC503-NAME
      *
           MOVE    WRK-SEX             TO  HC503-SEX
      *    生年月日
           MOVE    WRK-BIRTHDAY        TO  HC503-BIRTH
           MOVE    WRK-AGE-HEN         TO  HC503-AGE
      *    病棟名称
           MOVE    WRK-BTUNAME         TO  HC503-BYOTO
      *    病室番号
           MOVE    WRK-ROOMNO          TO  HC503-ROOMNO
      *    処方日
           MOVE    WRK-SYSYMD          TO  HC503-SRYYMD
      *
      *    ドクター
           MOVE    SYS-1010-NAME       TO  HC503-DRNAME
      *
      *    処方内容
           PERFORM VARYING     IDX3    FROM    1   BY  1
                   UNTIL       IDX3    >   WRK-IDX-MAX
               MOVE    WRK-NAIYOU (IDX2 IDX IDX3)
                                           TO  HC503-NAIYO (IDX3)
           END-PERFORM
      *
      *    備考欄
           PERFORM VARYING     IDX-B   FROM    1   BY  1
                   UNTIL       IDX-B   >   3
               IF      WRK-BIKOH  (IDX-B)  NOT =   SPACE
                   MOVE    WRK-BIKOH  (IDX-B)  TO  HC503-BIKOH (IDX-B)
               END-IF
           END-PERFORM
      *
           MOVE    SPA-OPID            TO  HC503-OPID
      *
           .
       300-HC503-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    頭書印刷処理
      *****************************************************************
       200-HEAD-PRINT-SEC             SECTION.
      *
           INITIALIZE                  WRK-SYOHOU-AREA
           MOVE    ZERO                TO  IDX-LINE
           MOVE    1                   TO  IDX-PAGE
           MOVE    1                   TO  IDX2
      *
      *    見出し共通編集
           PERFORM 2001-HEAD-HENSYU-SEC
      *    ドクター
           MOVE    ORCHC503-DRCD       TO  WRK-DRCD
           PERFORM 2002-DRCD-HENSYU-SEC
      *    処方日・時間はクリア
           MOVE    SPACE               TO  WRK-SYSYMD
           MOVE    SPACE               TO  WRK-TIME-G
      *    年齢
           MOVE    SPACE               TO  WRK-AGE-HEN
      *H29.5
      *API GINGEE対応
           IF      PARA-API-DATA       =   "1"
               PERFORM 7005-API-HEADSYORI-SEC
               GO      TO      200-HEAD-PRINT-EXT
           END-IF
      *API
      *    印刷処理
           PERFORM 3003-PRINT-SYORI-SEC
      *
           .
       200-HEAD-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷開始処理
      *****************************************************************
       400-SYOKI-PRINT-SEC             SECTION.
      *
      *    ファイルＯＰＥＮ
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    "HC53"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
      *    変更★★★★★★
           ADD     1                   TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC01PARA-CNT
      *v4.8
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
      *
           OPEN    OUTPUT               PRT-FILE
      *
           .
       400-SYOKI-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       400-PRINT-SEC                SECTION.
      *
      *　　印刷用データ出力
            WRITE  HC503
      *
            CLOSE                       PRT-FILE
      *
            INITIALIZE                 ORCSMKPRTAREA
            MOVE   "HC01.sh"           TO  MKPRT-ID
            MOVE   "HC503.red"         TO  MKPRT-DIA
            MOVE   SPACE               TO  MKPRT-DEF
      ***** MOVE   HC01PARA            TO  MKPRT-DAT
            MOVE   ASGNPARA            TO  MKPRT-DAT
            MOVE   SYS-1034-PRTNM(8)   TO  MKPRT-PRTNM
            MOVE   "入院指示箋"        TO  MKPRT-INFO
            CALL   "ORCSMKPRT1"        USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       400-PRINT-EXT.
           EXIT.
      *
      *H29.5
      *API GINGEE対応
      *****************************************************************
      *    ＡＰＩ送信編集処理
      *****************************************************************
       700-API-SYORI-SEC           SECTION.
      *
      *    帳票印刷
           MOVE    ORCHC503-SRYYMD     TO  WRK-SRYYMD
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   31
               IF      WRK-API-ZAI-MAX (IDX2)  >   ZERO
                   MOVE    IDX2            TO  WRK-SRYYMD(7:2)
                   PERFORM 70021-API-DAYCALL-SEC
               END-IF
           END-PERFORM
           .
       700-API-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＡＰＩ明細編集処理
      *****************************************************************
       7002-API-MEISAISYORI-SEC           SECTION.
      *
           INITIALIZE                  WRK-AIP-ZAI-AREA
           MOVE    ZERO                TO  IDX-AP
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   LNK-ORDERPRT-MAX )  OR
                              (IDX-AP  >=  50  )
               MOVE    LNK-ORDERPRT-SRYCD (IDX)    TO  WRK-SRYCD
               PERFORM 70021-API-ORDERPRT-HEN-SEC
           END-PERFORM
      *
      *    日毎に編集
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   31
               IF      LNK-ORDERPRT-DAY (1 IDX2)   >   ZERO
      *            日毎のテーブルへ編集
                   PERFORM 70022-DAY-TBLHEN-SEC
               END-IF
           END-PERFORM
           .
       7002-API-MEISAISYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＡＰＩ明細内容編集処理
      *****************************************************************
       70021-API-ORDERPRT-HEN-SEC      SECTION.
      *
           ADD     1                   TO  IDX-AP
      *
      *    点数検索
           MOVE    LNK-ORDERPRT-SRYCD(IDX) TO  WRK-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *
           IF      LNK-ORDERPRT-INPUTCOMENT(IDX)   =   SPACE
               MOVE    TNS-NAME                TO  WRK-TNS-NAME
           ELSE
               MOVE    LNK-ORDERPRT-INPUTCOMENT(IDX)
                                               TO  WRK-TNS-NAME
           END-IF
      *    器材商品名コード
           IF      WRK-SRYCD (1:3)     =   "058"
               MOVE    TNS-FORMALNAME      TO  WRK-TNS-NAME
      *        単位名称
               MOVE    TNS-TANINAME        TO  WRK-SYO-TANINAME
               MOVE    SPACE               TO  TNS-TANINAME
           END-IF
      *!   器材商品名の器材
           IF     (WRK-SRYCD (1:1)     =   "7"  )  AND
                  (FLG-KIZAI           =   1    )
               MOVE    SPACE           TO  WRK-TNS-NAME
               STRING  "　〔"
                       TNS-NAME        DELIMITED  BY  SPACE
                       "〕"            DELIMITED  BY  SIZE
                                       INTO    WRK-TNS-NAME
               END-STRING
      *
      *        商品名コードの単位へ
               IF      WRK-SYO-TANINAME    NOT =   SPACE
                   MOVE    WRK-SYO-TANINAME    TO  TNS-TANINAME
                   MOVE    SPACE               TO  WRK-SYO-TANINAME
               END-IF
           END-IF
           IF      WRK-SRYCD (1:3)     =   "058"
               MOVE    1               TO  FLG-KIZAI
           ELSE
               MOVE    ZERO            TO  FLG-KIZAI
           END-IF
      *
      *    編集名
           MOVE    WRK-TNS-NAME        TO  WRK-ZAI-YKZNAME
                                                      (IDX-AP)
           MOVE    WRK-SRYCD           TO  WRK-ZAI-SRYCD
                                                      (IDX-AP)
      *
           IF    ((WRK-SRYCD (1:1)     =   "6"  OR  "7" )  OR
                  (WRK-SRYCD (1:3)     =   "059"        ))
      *        リハビリ単位
             OR  ((WRK-SRYCD (1:1)     =   "1"      )  AND
                  (TNS-TANICD      NOT =   ZERO     ))
      *        数量
               MOVE    LNK-ORDERPRT-SRYSURYO (IDX) TO  WRK-SURYO
               IF      WRK-SURYO           =   ZERO
                   MOVE    SPACE               TO  WRK-SURYO-X
               ELSE
                   PERFORM 20012-SURYO-HEN-SEC
               END-IF
               MOVE    WRK-SURYO-X         TO  WRK-ZAI-SURYO
                                                      (IDX-AP)
      *        数量単位
               MOVE    TNS-TANINAME        TO  WRK-ZAI-TANINAME
                                                      (IDX-AP)
           END-IF
           .
       70042-API-ORDERPRT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＡＰＩ日毎編集処理
      *****************************************************************
       70022-DAY-TBLHEN-SEC      SECTION.
      *
           MOVE    LNK-ORDERPRT-DAY (1 IDX2)  TO  WRK-KAISU
      *    回数編集
           MOVE    WRK-KAISU           TO  WRK-KAISU-Z9
           MOVE    WRK-KAISU-X         TO  WRK-MOJI
           PERFORM 500-HENKAN-SEC
           MOVE    WRK-ZENKAKU-G(1:4)  TO  WRK-KAISU-N
           MOVE    "回"                TO  WRK-KAISU-H2
      *
      *    最終件数
           MOVE    WRK-API-ZAI-MAX (IDX2)  TO  IDX-AP1
      *
           ADD     1                   TO  IDX-AP1
           IF      IDX-AP1             >   50
               MOVE    50                  TO  IDX-AP1
               GO      TO    70022-DAY-TBLHEN-EXT
           END-IF
           MOVE    IDX-AP1             TO  WRK-API-ZAI-MAX (IDX2)
           MOVE    WRK-KAISU-HENSYU    TO  WRK-API-KAISU-MEI
                                                  (IDX2 IDX-AP1)
           MOVE    WRK-KAISU-X         TO  WRK-API-KAISU
                                                  (IDX2 IDX-AP1)
      *    明細
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL   IDX         >   IDX-AP
               MOVE    WRK-API-ZAI-SYOHOU-G (IDX)
                                      TO  WRK-API-SYOHOU-G2
                                              (IDX2 IDX-AP1 IDX)
           END-PERFORM
      *
           .
       70022-DAY-TBLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＡＰＩ送信内容編集処理
      *****************************************************************
       70021-API-DAYCALL-SEC          SECTION.
      *
      *    実施日編集
           MOVE    WRK-SRYYMD          TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  WRK-API-JISIYMD
           MOVE    SPACE               TO  WRK-BIKOH-AREA
      *
           INITIALIZE                  FMSHIJI-REC
      *
           MOVE    ZERO                TO  IDXN
      *    発行区分毎に印刷
           PERFORM VARYING     IDX-P   FROM    1   BY  1
                   UNTIL       IDX-P   >   4
               IF      ORCHC503-CHKPRT (IDX-P) =   1
      *
                   COMPUTE IDXN    =   IDXN    +   1
      *
                   EVALUATE    WRK-SRYKBN
                       WHEN    "40"
                           MOVE    "処置"          TO  WRK-SRYKBNMEI
                       WHEN    "80"
                           MOVE    "リハビリ・その他"
                                                   TO  WRK-SRYKBNMEI
                   END-EVALUATE
                   MOVE    SPACE               TO  WRK-SIJISEN-HEAD
                   STRING  WRK-SRYKBNMEI       DELIMITED  BY  SPACE
                           WRK-SIJISEN-HEAD1(IDX-P)
                                               DELIMITED  BY  SPACE
                                           INTO    FMSHIJI-TITLE (IDXN)
                   END-STRING
                   MOVE    WRK-SIJISEN-PRT1 (IDX-P)
                                               TO  FMSHIJI-MAISU (IDXN)
               END-IF
      *
           END-PERFORM
      *
           IF    ( IDXN    >   ZERO )
      *        ＡＰＩ送信内容編集処理
               PERFORM 7004-APIMEI-HENSYU-SEC
      *        ＡＰＩ送信処理
               PERFORM 7009-API-CALLSYORI-SEC
           END-IF
      *
           .
       70021-API-DAYCALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＡＰＩ送信内容編集処理
      *****************************************************************
       7004-APIMEI-HENSYU-SEC          SECTION.
      *
      *
           MOVE    "shijisen_n"        TO  FMSHIJI-FORM-ID
      *    プリンタ名
           MOVE    SYS-1034-PRTNM(8)   TO  FMSHIJI-PRINTER
      *
           IF      ORCHC503-SYORIKBN   =   1
               MOVE    "2"             TO  FMSHIJI-HAKKOKBN
           ELSE
               MOVE    "1"             TO  FMSHIJI-HAKKOKBN
           END-IF
      *
      *    処理日
           MOVE    WRK-API-SYSYMD      TO  FMSHIJI-SYSYMD
           MOVE    WRK-API-SYSTIME     TO  FMSHIJI-SYSTIME
      *    指示日
           MOVE    WRK-API-SRYYMD      TO  FMSHIJI-SRYYMD
      *
      *    診療科
           MOVE    ORCHC503-SRYKA      TO  FMSHIJI-SRYKA
           MOVE    SYS-1005-SRYKANAME1 TO  FMSHIJI-SRYKANAME
      *    ドクターコード
           MOVE    WRK-DRCD            TO  FMSHIJI-DR-CODE 
      *    医師情報
           MOVE    SYS-1010-KANANAME   TO  FMSHIJI-DR-KANANAME
           MOVE    SYS-1010-NAME       TO  FMSHIJI-DR-NAME
           MOVE    SYS-1010-MAYAKULICENSE
                                       TO  FMSHIJI-DR-MAYAKULICENSE
      *
      *    患者情報
           MOVE    ORCHC503-PTNUM      TO  FMSHIJI-PTNUM
           MOVE    PTINF-NAME          TO  FMSHIJI-NAME
           MOVE    PTINF-KANANAME      TO  FMSHIJI-KANANAME
           MOVE    PTINF-SEX           TO  FMSHIJI-SEX
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  FMSHIJI-BIRTHDAY
      *    年齢
      *    MOVE    WRK-AGE-HEN         TO  FMSHIJI-NENREI
      *
      *    病棟番号
           MOVE    WRK-BTUNUM          TO  FMSHIJI-BTUNUM
      *    病棟名称
           MOVE    WRK-BTUNAME         TO  FMSHIJI-BTUNAME
      *    病室番号
           MOVE    WRK-ROOMNO          TO  FMSHIJI-BRMNUM
      *    実施日
           MOVE    WRK-API-JISIYMD     TO  FMSHIJI-JISIYMD
      *    診療区分
      *
           MOVE    WRK-API-ZAI-MAX (IDX2)  TO  IDX-MAX
           PERFORM VARYING     IDX-AP1     FROM    1   BY  1
                   UNTIL      (IDX-AP1     >   50  )
                           OR (IDX-AP1     >   IDX-MAX )
      *
               MOVE    WRK-SRYKBN          TO  FMSHIJI-SRYKBN
                                                          (IDX-AP1)
      *        回数
               MOVE    WRK-API-KAISU (IDX2 IDX-AP1)
                                           TO  FMSHIJI-KAISU
                                                          (IDX-AP1)
      *        MOVE    WRK-API-KAISU-MEI (IDX2 IDX-AP1)
      *                                    TO  FMSHIJI-KAISU-MEI
      *                                                   (IDX-AP1)
      *        処方内容
               PERFORM VARYING     IDX-AP2     FROM    1   BY  1
                       UNTIL       IDX-AP2     >   50
                   MOVE    WRK-API-YKZNAME (IDX2 IDX-AP1 IDX-AP2)
                                       TO  FMSHIJI-YKZNAME
                                                    (IDX-AP1 IDX-AP2)
                   MOVE    WRK-API-SURYO (IDX2 IDX-AP1 IDX-AP2)
                                       TO  FMSHIJI-SURYO
                                                    (IDX-AP1 IDX-AP2)
                   MOVE    WRK-API-TANINAME (IDX2 IDX-AP1 IDX-AP2)
                                       TO  FMSHIJI-TANINAME
                                                    (IDX-AP1 IDX-AP2)
                   MOVE    WRK-API-SRYCD (IDX2 IDX-AP1 IDX-AP2)
                                       TO  FMSHIJI-SRYCD
                                                    (IDX-AP1 IDX-AP2)
               END-PERFORM
           END-PERFORM
      *
      *    備考欄
      *    PERFORM VARYING     IDX-B   FROM    1   BY  1
      *            UNTIL       IDX-B   >   3
      *        IF      WRK-BIKOH  (IDX-B)  NOT =   SPACE
      *            MOVE    WRK-BIKOH  (IDX-B)  TO  FMSHIJI-MEMO(IDX-B)
      *        END-IF
      *    END-PERFORM
      *
           MOVE    SPA-OPID            TO  FMSHIJI-OPID
      *
           .
       7004-APIMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＡＰＩ送信処理
      *****************************************************************
       7009-API-CALLSYORI-SEC         SECTION.
      *
      *    カスタムＩＤ
           MOVE    PARA-API-CUSTOM-ID  TO  FMSHIJI-CUSTOM-ID
      *
           INITIALIZE                      RPMAIN-AREA
           MOVE    "shijisen_n"        TO  RPMAIN-ID
           MOVE    PARA-API-CUSTOM-ID  TO  RPMAIN-CUSTOM-ID
           MOVE    "入院指示せん"      TO  RPMAIN-TITLE
           MOVE    "fm_order_direction"
                                       TO  MCP-TABLE
           MOVE    FMSHIJI-DATA        TO  RPMAIN-DATA
           CALL    "ORCRPMAIN"         USING
                                       MCPAREA
                                       RPMAIN-AREA
                                       SPA-AREA
      *
           .
       7009-API-CALLSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＡＰＩ頭書き処理
      *****************************************************************
       7005-API-HEADSYORI-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYYMD
           INITIALIZE                  WRK-API-SYOHOU-AREA
           MOVE    SPACE               TO  WRK-API-SYSYMD
           MOVE    SPACE               TO  WRK-API-SYSTIME
           MOVE    SPACE               TO  WRK-API-SRYYMD
           MOVE    SPACE               TO  WRK-SRYKBN
           PERFORM 70021-API-DAYCALL-SEC
           .
       7005-API-HEADSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理(API）
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
           ELSE
               INITIALIZE                  WRK-HEN-DATE
               MOVE    WRK-SYY             TO  WRK-HEN-YY
               MOVE    WRK-SMM             TO  WRK-HEN-MM
               MOVE    WRK-SDD             TO  WRK-HEN-DD
               MOVE    "-"                 TO  WRK-HEN-H1
               MOVE    "-"                 TO  WRK-HEN-H2
               IF      WRK-SYMD            =   "99999999"
                   MOVE    "12"                TO  WRK-HEN-MM
                   MOVE    "31"                TO  WRK-HEN-DD
               END-IF
           END-IF
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       500-DELETE-SEC                   SECTION.
      *
      *    入院帳票オーダー
           INITIALIZE                  ORDERPRT-REC
           MOVE    SPA-HOSPNUM         TO  ORDERPRT-HOSPNUM
           MOVE    SPA-PTID            TO  ORDERPRT-PTID
           MOVE    ORCHC503-PTNUM      TO  ORDERPRT-PTNUM
           MOVE    SPA-NYUGAIKBN       TO  ORDERPRT-NYUGAIKBN
           MOVE    ORCHC503-SRYKA      TO  ORDERPRT-SRYKA
           MOVE    ORCHC503-HKNCOMBI   TO  ORDERPRT-HKNCOMBI
           MOVE    ORCHC503-SYSYMD     TO  ORDERPRT-SYSYMD
           MOVE    ORCHC503-PRINTYMD   TO  ORDERPRT-PRINTYMD
           MOVE    ORCHC503-PRINTHMS   TO  ORDERPRT-PRINTHMS
           MOVE    "6"                 TO  ORDERPRT-PRTKBN
      *
           MOVE    ORDERPRT-REC        TO  MCPDATA-REC
           MOVE    "tbl_orderprt"      TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           MOVE    "DBDELETE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ORCHC503 ORDERPRT DEL ERR:"  MCP-RC
                       ",KEY:" ORDERPRT-KEY
               MOVE    1               TO  ORCHC503-RC
           END-IF
           .
       500-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       500-HENKAN-SEC                 SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MOJI            TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC          =   ZERO )  AND
                  (KANACHK-MAX         >   ZERO )
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-ZENKAKU-G
           ELSE
               MOVE    WRK-MOJI            TO  WRK-ZENKAKU-G
           END-IF
      *
           .
       500-HENKAN-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       510-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           .
       510-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    システム管理主キー読込み
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
      **** MOVE    ZERO                TO  WRK-MOJISU
      *    PERFORM VARYING     IDXS    FROM    8   BY  -1
      *            UNTIL       IDXS    <       1
      *        IF      SYS-KBNCD(IDXS:1)   NOT =   SPACE
      *            MOVE    IDXS            TO  WRK-MOJISU 
      *            MOVE    1               TO  IDXS
      *        END-IF
      *    END-PERFORM
      *    IF      WRK-MOJISU          <   8
      *        COMPUTE IDXS            =   WRK-MOJISU  +  1
      *        MOVE    "%"             TO  SYS-KBNCD(IDXS:1)
      **** END-IF
      *
           MOVE    ORCHC503-SRYYMD     TO  SYS-STYUKYMD
           MOVE    ORCHC503-SRYYMD     TO  SYS-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
           END-IF
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYSKANRI-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                      SYSKANRI-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
           END-IF
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTINF-REC
               MOVE    ZERO                TO  FLG-PTINF
           ELSE
               INITIALIZE                      PTINF-REC
               MOVE    1                   TO  FLG-PTINF
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
           END-IF
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSU
           ELSE
               INITIALIZE                      TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *R02.2
      *    ユーザー単位編集
           IF     (TNS-SRYCD(1:1)      =   "6"
                                       OR  "7" )
               PERFORM 9101-TENSUPLUS-READ-SEC
           END-IF
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *R02.2
      *****************************************************************
      *    点数マスタ付加情報検索処理
      *****************************************************************
       9101-TENSUPLUS-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    WRK-SRYYMD          TO  TNSP-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  TNSP-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
                   MOVE    ZERO                TO  FLG-TENSUPLUS
               ELSE
                   INITIALIZE                  TENSUPLUS-REC
                   MOVE    1                   TO  FLG-TENSUPLUS
               END-IF
           ELSE
               INITIALIZE                      TENSUPLUS-REC
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    ユーザー単位設定
           IF     (FLG-TENSUPLUS       =   ZERO  )
              AND (TNSP-USERTANICD NOT =   SPACE )
                MOVE    TNSP-USERTANICD    TO  TNS-TANICD
                MOVE    TNSP-USERTANINAME  TO  TNS-TANINAME
           END-IF
           .
       9101-TENSUPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院帳票オーダ読込処理
      *****************************************************************
       900-ORDERPRT-READ-SEC                SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ORDERPRT-REC
               MOVE    ZERO                TO  FLG-ORDERPRT
           ELSE
               INITIALIZE                      ORDERPRT-REC
               MOVE    1                   TO  FLG-ORDERPRT
           END-IF
           .
       900-ORDERPRT-READ-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴読込処理
      *****************************************************************
       900-PTNYUINRRK-READ-SEC                SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTNYUINRRK-REC
               MOVE    ZERO                TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                      PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
           .
       900-PTNYUINRRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
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
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
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
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *
