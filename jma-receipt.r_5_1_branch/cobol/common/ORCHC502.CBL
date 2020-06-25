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
       PROGRAM-ID.             ORCHC502.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為
      *  コンポーネント名  : 注射処方箋　印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/02/21    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  06/01/25  MONFUNCへ変更
      * 03.05.00     NACL-多々納  07/04/XX  グループ診療対応
      * 03.05.00     NACL-多々納  07/05/22  再印刷処理追加
      * 04.01.00     NACL-多々納  07/11/22  器材商品名コード対応
      * 04.05.00     NACL-多々納  09/06/XX  数量小数５桁対応
      * 04.08.00     NACL-多々納  14/08/01  一時ディレクトリ対応
      * 05.00.00     NACL-多々納  17/05/XX  API(GINBEE）対応
      * 05.00.00     NACL-太田    17/10/11  API(GINBEE）対応
      * 05.01.00     NACL-多々納  20/02/XX  ユーザー単位対応
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
           COPY    "HC502.INC".
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
           03  FLG-DAY             PIC 9(01).
           03  FLG-FUKU            PIC 9(01).
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
           03  IDX-KAI             PIC 9(04).
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
           03  WRK-SRYSYUKBN       PIC X(03).
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
           03  WRK-KAISU-X.
               05  WRK-KAISU-Z9        PIC Z9.
           03  WRK-KAISU-HENSYU.
               05  WRK-KAISU-N         PIC X(04).
               05  WRK-DAY-H2          PIC X(04).
      *
           03  WRK-TANINAME            PIC X(04).
           03  WRK-ZZ                  PIC ZZ.
           03  WRK-ZZZ                 PIC ZZZ.
      *
           03  WRK-TNS-NAME            PIC X(200).
           03  WRK-CHUSYAMEI           PIC X(200).
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
           03  WRK-PRINTYMD            PIC X(08).
           03  WRK-TIME-G.
               05  WRK-TIME-HH         PIC X(02).
               05  WRK-TIME-P          PIC X(01).
               05  WRK-TIME-SS         PIC X(02).
               05  WRK-SIJISEN-HEAD    PIC X(30).
               05  WRK-SIJISEN-PRT     PIC 9(02).
      *
           03  WRK-BRMNUM              PIC 9(06).
           03  WRK-Z6                  PIC ZZZZZ9.
      *
      *    商品名単位名称
           03  WRK-SYO-TANINAME        PIC X(12).
      *
      *    処方箋見出し名称
       01  WRK-SIJISEN-AREA.
           03  WRK-SIJISEN-HEAD1-G     OCCURS   4.
               05  WRK-SIJISEN-HEAD1       PIC X(30).
               05  WRK-SIJISEN-PRT1        PIC 9(02).
           03  WRK-SIJISEN-HEAD2-G     OCCURS   4.
               05  WRK-SIJISEN-HEAD2       PIC X(30).
               05  WRK-SIJISEN-PRT2        PIC 9(02).
      *
      *    剤毎の内容
       01  WRK-NAIYOU-AREA.
           03  WRK-NAIYOU-TBL          OCCURS   100.
               05  WRK-NAIYOU-Z            PIC X(106).
               05  WRK-NAIYOU-R1           REDEFINES   WRK-NAIYOU-Z.
                   07  WRK-MEISYOU1        PIC X(70).
                   07  WRK-SURYOU          PIC X(22).
                   07  WRK-TANI            PIC X(12).
                   07  WRK-FIL11           PIC X(02).
               05  WRK-NAIYOU-R3           REDEFINES   WRK-NAIYOU-Z.
                   07  WRK-MEISYOU3        PIC X(100).
                   07  WRK-FIL12           PIC X(06).
      *
      *    処方内容
       01  WRK-SYOHOU-AREA.
           03  WRK-SYOHOU-PAGE         OCCURS   10.
               05  WRK-SYOHOU-TBL          OCCURS   34.
                   07  WRK-NAIYOU          PIC X(106).
      *
      *    処方内容最大行
       01  WRK-IDX-MAX                 PIC 9(03)  VALUE   34.
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
           03  WRK-API-SRYYMD          PIC X(10).
           03  WRK-API-SYSTIME         PIC X(05).
           03  WRK-API-CHUSYACD        PIC X(09).
      *
           03  WRK-API-KAISU-X.
               05  WRK-API-KAISU-9     PIC 9(02).
      *
      *    処方内容
       01  WRK-API-SYOHOU-AREA.
           03  WRK-API-SYOHOU-G        OCCURS   50.
               05  WRK-API-YKZNAME         PIC X(200).
               05  WRK-API-SURYO           PIC X(12).
               05  WRK-API-TANINAME        PIC X(12).
               05  WRK-API-SRYCD           PIC X(09).
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
      **   COPY    "CPSK1001.INC".
      *    医療機関情報−所在地、連絡先 
      **** COPY    "CPSK1002.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1010.INC".
      *    処方箋ＧＥ表示
      ***  COPY    "CPSK1030.INC".
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
      *
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
           COPY    "MCPAREA".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *H29.5
      * API対応
      *    注射処方せん情報構造体
           COPY    "CPFMCHUSYA.INC".
      *
           COPY    "CPORCRPMAIN.INC".
      *
      *****COPY    "CPORCMCP.INC".
      **** COPY    "ORCA-DBPATH".
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
           COPY    "CPORCHC502.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCHC502AREA
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
           INITIALIZE                  WRK-API-AREA
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *H29.5
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *
      *    各ＤＢ読込
           PERFORM 100-SYORI-SEC
      *
           EVALUATE    ORCHC502-SYORIKBN
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
           MOVE    "ORCHC502"          TO  ORCSPRGOPTION-PRGID
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
           IF    ( FLG-PTINF       NOT =   ZERO )
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
               IF      PTNYUINRRK-TENNYUYMD    <=  ORCHC502-SYSYMD
      *            転出日
                   IF     (PTNYUINRRK-TENSTUYMD    =   SPACE )  OR
                          (PTNYUINRRK-TENSTUYMD    NOT =   SPACE AND
                           PTNYUINRRK-TENSTUYMD    >=  ORCHC502-SYSYMD)
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
           MOVE    ORCHC502-SYSYMD     TO  ORCSPRTNM-SRYYMD
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
           MOVE    ORCHC502-SRYKA      TO  SYS-1005-KBNCD
           MOVE    SYS-1005-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
           ELSE
               INITIALIZE                      SYS-1005-REC
           END-IF
      *
      *    注射処方箋見出し名称
           INITIALIZE  WRK-SIJISEN-AREA
           MOVE    SPACE               TO  SYS-5007-REC
           INITIALIZE                      SYS-5007-REC
           MOVE    "5007"              TO  SYS-5007-KANRICD
           MOVE    ORCHC502-SRYYMD     TO  SYS-5007-STYUKYMD
           MOVE    ORCHC502-SRYYMD     TO  SYS-5007-EDYUKYMD
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
                   WHEN    "03"
      *                注射処方箋
                       PERFORM VARYING IDX     FROM    1   BY  1
                               UNTIL   IDX     >   4
                           MOVE    SYS-5007-SIJISEN-HEAD(IDX)
                                           TO  WRK-SIJISEN-HEAD1 (IDX)
                           MOVE    SYS-5007-SIJISEN-PRT (IDX)
                                           TO  WRK-SIJISEN-PRT1 (IDX)
                       END-PERFORM
                   WHEN    "04"
      *                注射麻薬処方箋
                       PERFORM VARYING IDX     FROM    1   BY  1
                               UNTIL   IDX     >   4
                           MOVE    SYS-5007-SIJISEN-HEAD(IDX)
                                           TO  WRK-SIJISEN-HEAD2 (IDX)
                           MOVE    SYS-5007-SIJISEN-PRT (IDX)
                                           TO  WRK-SIJISEN-PRT2 (IDX)
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
      *
           MOVE    SPACE               TO  WRK-DRCD
           PERFORM VARYING     IDX-H   FROM    1   BY  1
                   UNTIL       IDX-H   >   2
               EVALUATE    IDX-H
                   WHEN    1
      *                注射処方箋
                       MOVE    "4"             TO  WRK-PRTKBN
                   WHEN    2
      *                注射麻薬処方箋
                       MOVE    "5"             TO  WRK-PRTKBN
               END-EVALUATE
               PERFORM 2001-HENSYU-SYORI-SEC
      *        ＤＢ削除処理
               PERFORM 500-DELETE-SEC
           END-PERFORM
      *
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *
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
           MOVE    ORCHC502-SYSYMD     TO  AGECHK-SRYYMD
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
      *    処方時間
           MOVE    ORCHC502-PRINTYMD    TO  WRK-PRINTYMD
           MOVE    ORCHC502-PRINTHMS(1:2)
                                        TO  WRK-TIME-HH
           MOVE    ORCHC502-PRINTHMS(3:2)
                                        TO  WRK-TIME-SS
           MOVE    ":"                  TO  WRK-TIME-P
      *
      *H29.5
      *API GINGEE対応
      *    処理日
           MOVE    ORCHC502-PRINTYMD   TO  WRK-SYMD
           MOVE    ORCHC502-PRINTHMS   TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  WRK-API-SYSYMD
           MOVE    WRK-HEN-TIME        TO  WRK-API-SYSTIME
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
      *    印刷主処理
      *****************************************************************
       2001-HENSYU-SYORI-SEC                   SECTION.
      *
      *    入院帳票オーダー検索
           INITIALIZE                  ORDERPRT-REC
           MOVE    SPA-HOSPNUM         TO  ORDERPRT-HOSPNUM
           MOVE    SPA-PTID            TO  ORDERPRT-PTID
           MOVE    ORCHC502-PTNUM      TO  ORDERPRT-PTNUM
           MOVE    SPA-NYUGAIKBN       TO  ORDERPRT-NYUGAIKBN
           MOVE    ORCHC502-SRYKA      TO  ORDERPRT-SRYKA
           MOVE    ORCHC502-HKNCOMBI   TO  ORDERPRT-HKNCOMBI
      *    システム日付
           MOVE    ORCHC502-SYSYMD     TO  ORDERPRT-SYSYMD
      *    発行日
           MOVE    ORCHC502-PRINTYMD   TO  ORDERPRT-PRINTYMD
      *    発行時間
           MOVE    ORCHC502-PRINTHMS   TO  ORDERPRT-PRINTHMS
      *    帳票区分
           MOVE    WRK-PRTKBN          TO  ORDERPRT-PRTKBN
      *
           MOVE    ORDERPRT-REC        TO  MCPDATA-REC
           MOVE    "tbl_orderprt"      TO  MCP-TABLE
           MOVE    "key1"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-ORDERPRT-READ-SEC
           END-IF
           INITIALIZE                  LNK-ORDERPRT-AREA
           INITIALIZE                  LNK-ORDERPRT-MAX
           INITIALIZE                  WRK-SYOHOU-AREA
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
               MOVE    "key1"              TO  MCP-PATHNAME
               PERFORM 900-ORDERPRT-READ-SEC
           END-PERFORM
           MOVE    "tbl_orderprt"      TO  MCP-TABLE
           MOVE    "key1"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    剤毎に編集する
           IF      LNK-ORDERPRT-MAX    >   ZERO
               PERFORM 220-NAIYOU-HENSYU-SEC
           END-IF
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
           MOVE    ZERO                TO  FLG-FUKU
           MOVE    ZERO                TO  FLG-DAY
           MOVE    ORCHC502-SRYYMD     TO  WRK-SRYYMD
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   31
               IF      LNK-ORDERPRT-DAY (1 IDX2)  >   ZERO
                   IF      FLG-DAY         =   ZERO
                       MOVE    IDX2            TO  WRK-SRYYMD(7:2)
                       MOVE    1               TO  FLG-DAY
                   END-IF
               END-IF
               IF      LNK-ORDERPRT-DAY (1 IDX2)  >   1
                   MOVE    1               TO  FLG-FUKU
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  WRK-NAIYOU-AREA
           MOVE    ZERO                TO  IDX-S
           MOVE    SPACE               TO  WRK-CHUSYAMEI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   LNK-ORDERPRT-MAX )  OR
                              (IDX-S   >=  100  )
               IF      LNK-ORDERPRT-SRYCD (IDX)(1:1)   =   "1"
                   PERFORM 2002-CHUSYAMEI-HEN-SEC
               ELSE
                   MOVE    LNK-ORDERPRT-SRYCD (IDX)    TO  WRK-SRYCD
                   PERFORM 2001-ORDERPRT-HEN-SEC
               END-IF
           END-PERFORM
      *
      *H29.5
      *API GINGEE対応
           IF      PARA-API-DATA        =   "1"
      *        ＡＰＩ明細編集処理
               PERFORM 700-API-SYORI-SEC
               INITIALIZE                  LNK-ORDERPRT-AREA
               INITIALIZE                  LNK-ORDERPRT-MAX
               INITIALIZE                  WRK-SYOHOU-AREA
               GO      TO      220-NAIYOU-HENSYU-EXT
           END-IF
      *
      *    帳票のテーブルへ編集
           PERFORM 2209-PRINT-HENSYU-SEC
      *
      *
      *    日数分印刷する
           MOVE    ZERO                TO  WRK-KAISU
           MOVE    ORCHC502-SRYYMD     TO  WRK-SRYYMD
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   31
               IF      LNK-ORDERPRT-DAY (1 IDX2)   >   ZERO
                   MOVE    IDX2                    TO  WRK-SRYYMD(7:2)
                   MOVE    LNK-ORDERPRT-DAY (1 IDX2)   TO  WRK-KAISU
      *            帳票印刷
                   PERFORM 300-PRINT-SYORI-SEC
               END-IF
           END-PERFORM
      *
           INITIALIZE                  LNK-ORDERPRT-AREA
           INITIALIZE                  LNK-ORDERPRT-MAX
           INITIALIZE                  WRK-SYOHOU-AREA
      *
           .
       220-NAIYOU-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    注射名称編集処理
      *****************************************************************
       2002-CHUSYAMEI-HEN-SEC                   SECTION.
      *
           MOVE    SPACE               TO  WRK-CHUSYAMEI
      *
           MOVE    LNK-ORDERPRT-SRYCD(IDX) TO  WRK-SRYCD
           PERFORM 910-TENSU-READ-SEC
           MOVE    TNS-NAME            TO  WRK-CHUSYAMEI
      *
           .
       2002-CHUSYAMEI-HEN-EXT.
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
           IF      LNK-ORDERPRT-INPUTCOMENT(IDX)   =   SPACE
               MOVE    TNS-NAME                TO  WRK-TNS-NAME
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
           IF      WRK-TNS-NAME(101:)  NOT =   SPACE
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
      *        小数３桁以下
               MOVE    WRK-SURYO-X(1:9)    TO  WRK-MOJI
               PERFORM 500-HENKAN-SEC
               MOVE    WRK-ZENKAKU-G(1:18) TO  WRK-SURYO-N-18
           ELSE
      *        小数４桁以上
               MOVE    WRK-SURYO-X         TO  WRK-MOJI
               PERFORM 500-HENKAN-SEC
               MOVE    WRK-ZENKAKU-G(1:22) TO  WRK-SURYO-N
           END-IF
      *
           .
       20012-SURYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       2209-PRINT-HENSYU-SEC          SECTION.
      *
      *    注射種類
           IF      WRK-CHUSYAMEI       =   SPACE
               PERFORM 22093-CHUSYAMEI-SET-SEC
           END-IF
      *
           MOVE    1                   TO  IDX-PAGE
           MOVE    ZERO                TO  IDX-LINE
           MOVE    IDX-S               TO  IDX-SMAX
      *
      *    最大行数−１のセット
           COMPUTE IDX-MAX             =   WRK-IDX-MAX  -  1
      *
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL   IDX         >   IDX-SMAX
               ADD     1                   TO  IDX-LINE
               MOVE    WRK-NAIYOU-Z (IDX)  TO  WRK-NAIYOU
                                              (IDX-PAGE IDX-LINE)
               COMPUTE IDX-S           =   IDX-S   -  1
               IF      IDX-LINE        >=  IDX-MAX
                   PERFORM 22091-PRINT-HEN-SEC
               END-IF
           END-PERFORM
      *
           ADD     1                   TO  IDX-LINE
      *
      *    最終行の設定
           IF      IDX-LINE            <=  WRK-IDX-MAX
               PERFORM 22092-LAST-LINE-SEC
           END-IF
      *
           .
       2209-PRINT-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    帳票用編集処理
      *****************************************************************
       22091-PRINT-HEN-SEC         SECTION.
      *
           IF     (IDX-S   +   IDX-LINE)   >   WRK-IDX-MAX
               MOVE    "次ページへ続く"    TO  WRK-NAIYOU
                                     (IDX-PAGE WRK-IDX-MAX)(51:)
               IF      IDX-LINE         >   ZERO
      *            改ページ
                   ADD     1                   TO  IDX-PAGE
                   MOVE    ZERO                TO  IDX-LINE
               END-IF
           END-IF
      *
           .
       22091-PRINT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    最終行の設定処理
      *****************************************************************
       22092-LAST-LINE-SEC          SECTION.
      *
           IF      IDX-LINE            <=  WRK-IDX-MAX
               IF     (WRK-NAIYOU (IDX-PAGE IDX-LINE)
                                           =   SPACE ) AND
                      (IDX-LINE            =   WRK-IDX-MAX  )
                   MOVE    "　　　　　　以下余白"
                                           TO  WRK-NAIYOU
                                               (IDX-PAGE IDX-LINE)
               END-IF
               ADD     1                   TO  IDX-LINE
           END-IF
           IF      IDX-LINE            <=  WRK-IDX-MAX
               MOVE    "　　　　　　以下余白"
                                           TO  WRK-NAIYOU
                                               (IDX-PAGE IDX-LINE)
           END-IF
           .
       22092-LAST-LINE-EXT.
           EXIT.
      *
      *****************************************************************
      *    注射種類編集処理
      *****************************************************************
       22093-CHUSYAMEI-SET-SEC          SECTION.
      *
           EVALUATE    LNK-ORDERPRT-SRYKBN(1)
               WHEN    "31"
                   MOVE    "皮下筋肉注射"          TO  WRK-CHUSYAMEI
               WHEN    "32"
                   MOVE    "静脈注射"              TO  WRK-CHUSYAMEI
               WHEN    "33"
                   EVALUATE    LNK-ORDERPRT-SRYSYUKBN(1)
                       WHEN    "330"
      *H29.5
                       WHEN    "331"
                       WHEN    "332"
                           MOVE    "点滴注射"      TO  WRK-CHUSYAMEI
                       WHEN    "350"
                       WHEN    "352"
                           MOVE    "中心静脈注射"  TO  WRK-CHUSYAMEI
                   END-EVALUATE
           END-EVALUATE
      *
           .
       22093-CHUSYAMEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       300-PRINT-SYORI-SEC          SECTION.
      *
      *    処方日
           MOVE    WRK-SRYYMD          TO  WRK-SYMD
           PERFORM 510-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-SYSYMD
      *
           MOVE    SPACE               TO  WRK-KAISU-HENSYU
           IF      IDX-PAGE            >   ZERO
      *        回数分印刷する
               IF      WRK-KAISU           >   1
                   PERFORM VARYING     IDX-KAI FROM    1   BY  1
                           UNTIL       IDX-KAI >   WRK-KAISU
                       PERFORM 3002-KAISU-HENSYU-SEC
      *                印刷処理
                       PERFORM 3003-PRINT-SYORI-SEC
                   END-PERFORM
               ELSE
      *            印刷処理
                   PERFORM 3003-PRINT-SYORI-SEC
               END-IF
           END-IF
           .
       300-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    回数編集処理
      *****************************************************************
       3002-KAISU-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-KAISU-HENSYU
      *
           MOVE    IDX-KAI             TO  WRK-KAISU-Z9
           MOVE    WRK-KAISU-X         TO  WRK-MOJI
           PERFORM 500-HENKAN-SEC
           MOVE    WRK-ZENKAKU-G(1:6)  TO  WRK-KAISU-N
           MOVE    "回目"              TO  WRK-DAY-H2
      *
           .
       3002-KAISU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       3003-PRINT-SYORI-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-BIKOH-AREA
           IF      IDX-H               =   2
      *       麻薬編集
              PERFORM 2003-MAYAKU-HEN-SEC
           END-IF
      *
      *    発行区分毎に印刷
           PERFORM VARYING     IDX-P   FROM    1   BY  1
                   UNTIL       IDX-P   >   4
               IF      ORCHC502-CHKPRT (IDX-P) =   1
                   IF      IDX-H           =   2
                       MOVE    WRK-SIJISEN-HEAD2(IDX-P)
                                               TO  WRK-SIJISEN-HEAD
                       MOVE    WRK-SIJISEN-PRT2 (IDX-P)
                                               TO  WRK-SIJISEN-PRT
                   ELSE
                       MOVE    WRK-SIJISEN-HEAD1(IDX-P)
                                               TO  WRK-SIJISEN-HEAD
                       MOVE    WRK-SIJISEN-PRT1 (IDX-P)
                                               TO  WRK-SIJISEN-PRT
                   END-IF
                   PERFORM VARYING     IDX-P2  FROM    1   BY  1
                           UNTIL       IDX-P2  >   WRK-SIJISEN-PRT
                       PERFORM 3003-HC502-SYORI-SEC
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
       3003-HC502-SYORI-SEC      SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-PAGE
      *        帳票印刷開始処理
               PERFORM 400-SYOKI-PRINT-SEC
      *        帳票編集処理
               PERFORM 300-HC502-HEN-SEC
      *        帳票印刷処理
               PERFORM 400-PRINT-SEC
           END-PERFORM
           .
       3003-HC502-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *        麻薬編集編集処理
      *****************************************************************
       2003-MAYAKU-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  IDX-B
      *
      *    麻薬施用者免許
           ADD     1                   TO  IDX-B
           STRING  "麻薬施用者免許証番号："
                                       DELIMITED   BY  SIZE
                   SYS-1010-MAYAKULICENSE
                                       DELIMITED   BY  SPACE
                                       INTO    WRK-BIKOH  (IDX-B)
           END-STRING
      *
           .
       2003-MAYAKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       300-HC502-HEN-SEC                SECTION.
      *
           INITIALIZE                  HC502
      *
           MOVE    WRK-SIJISEN-HEAD    TO  HC502-TITLE
      *    麻薬
           IF      IDX-H               =   2
               MOVE    "麻"                TO  HC502-MAYAKU
           END-IF
      *    発行日
           MOVE    WRK-PRINTYMD        TO  HC502-SYSYMD
      *    発行時間
           MOVE    WRK-TIME-G          TO  HC502-TIME
      *    診療科
           MOVE    SYS-1005-SRYKANAME1 TO  HC502-SRYKA
      *    患者番号
           MOVE    ORCHC502-PTNUM      TO  HC502-PTNUM
      *    患者名称カナ
           MOVE    PTINF-KANANAME      TO  HC502-KANANAME
      *    患者名称
           MOVE    PTINF-NAME          TO  HC502-NAME
      *
           MOVE    WRK-SEX             TO  HC502-SEX
      *    生年月日
           MOVE    WRK-BIRTHDAY        TO  HC502-BIRTH
      *    年齢
           MOVE    WRK-AGE-HEN         TO  HC502-AGE
      *    病棟名称
           MOVE    WRK-BTUNAME         TO  HC502-BYOTO
      *    病室番号
           MOVE    WRK-ROOMNO          TO  HC502-ROOMNO
      *    処方日
           MOVE    WRK-SYSYMD          TO  HC502-SRYYMD
      *
           MOVE    WRK-KAISU-HENSYU    TO  HC502-KAISU
      *    ドクター
           MOVE    SYS-1010-NAME       TO  HC502-DRNAME
      *
      *    注射内容
           MOVE    WRK-CHUSYAMEI       TO  HC502-NAIYO1
      *    処方内容
           PERFORM VARYING     IDX3    FROM    1   BY  1
                   UNTIL       IDX3    >   WRK-IDX-MAX
               MOVE    WRK-NAIYOU (IDX IDX3)
                                           TO  HC502-NAIYO (IDX3)
           END-PERFORM
      *
      *    備考欄
           PERFORM VARYING     IDX-B   FROM    1   BY  1
                   UNTIL       IDX-B   >   3
               IF      WRK-BIKOH  (IDX-B)  NOT =   SPACE
                   MOVE    WRK-BIKOH  (IDX-B)  TO  HC502-BIKOH (IDX-B)
               END-IF
           END-PERFORM
      *
           MOVE    SPA-OPID            TO  HC502-OPID
      *
           .
       300-HC502-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    頭書印刷処理
      *****************************************************************
       200-HEAD-PRINT-SEC             SECTION.
      *
           INITIALIZE                  WRK-SYOHOU-AREA
           MOVE    1                   TO  IDX-H
           MOVE    ZERO                TO  IDX-LINE
           MOVE    1                   TO  IDX-PAGE
      *
      *    見出し共通編集
           PERFORM 2001-HEAD-HENSYU-SEC
      *    ドクター編集
           MOVE    ORCHC502-DRCD       TO  WRK-DRCD
           PERFORM 2002-DRCD-HENSYU-SEC
      *    処方日・時間はクリア
           MOVE    SPACE               TO  WRK-SYSYMD
           MOVE    SPACE               TO  WRK-PRINTYMD
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
      *
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
           MOVE    "HC52"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
      *    変更★★★★★★
           ADD     1                   TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC01PARA-CNT
      *
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
            WRITE  HC502
      *
            CLOSE                       PRT-FILE
      *
            INITIALIZE                 ORCSMKPRTAREA
            MOVE   "HC01.sh"           TO  MKPRT-ID
            MOVE   "HC502.red"         TO  MKPRT-DIA
            MOVE   SPACE               TO  MKPRT-DEF
      ***** MOVE   HC01PARA            TO  MKPRT-DAT
            MOVE   ASGNPARA            TO  MKPRT-DAT
            MOVE   SYS-1034-PRTNM(7)   TO  MKPRT-PRTNM
            MOVE   "注射処方箋"        TO  MKPRT-INFO
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
           INITIALIZE                  WRK-API-SYOHOU-AREA
           MOVE    ZERO                TO  IDX-AP
      *
           MOVE    SPACE               TO  WRK-CHUSYAMEI
           MOVE    SPACE               TO  WRK-API-CHUSYACD
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   LNK-ORDERPRT-MAX )  OR
                              (IDX-AP  >=  50    )
               IF      LNK-ORDERPRT-SRYCD (IDX)(1:1)   =   "1"
                   PERFORM 2002-CHUSYAMEI-HEN-SEC
                   MOVE    LNK-ORDERPRT-SRYCD(IDX)
                                               TO  WRK-API-CHUSYACD
               ELSE
                   MOVE    LNK-ORDERPRT-SRYCD(IDX) TO  WRK-SRYCD
      *            ＡＰＩ明細内容編集処理
                   PERFORM 70042-API-ORDERPRT-HEN-SEC
               END-IF
           END-PERFORM
      *    注射種類
           IF      WRK-CHUSYAMEI       =   SPACE
               PERFORM 22093-CHUSYAMEI-SET-SEC
           END-IF
      *
      *    日数分印刷する
           MOVE    ZERO                TO  WRK-KAISU
           MOVE    ORCHC502-SRYYMD     TO  WRK-SRYYMD
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   31
               IF      LNK-ORDERPRT-DAY (1 IDX2)   >   ZERO
                   MOVE    IDX2                    TO  WRK-SRYYMD(7:2)
                   MOVE    LNK-ORDERPRT-DAY (1 IDX2)   TO  WRK-KAISU
      *            ＡＰＩ日毎送信処理
                   PERFORM 7002-API-DAYSYORI-SEC
               END-IF
           END-PERFORM
      *
           .
       700-API-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＡＰＩ処方内容編集処理
      *****************************************************************
       7002-API-DAYSYORI-SEC           SECTION.
      *
      *    処方日
           MOVE    WRK-SRYYMD          TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  WRK-API-SRYYMD
      *
           MOVE    SPACE               TO  WRK-KAISU-HENSYU
           MOVE    SPACE               TO  WRK-API-KAISU-X
      *
      *    回数
           MOVE    WRK-KAISU           TO  WRK-API-KAISU-9
           PERFORM 3002-KAISU-HENSYU-SEC
      *
           PERFORM 70021-API-DAYCALL-SEC
      *
           .
       7002-API-DAYSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＡＰＩ処方内容編集処理
      *****************************************************************
       70021-API-DAYCALL-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-BIKOH-AREA
           IF      IDX-H               =   2
      *       麻薬編集
              PERFORM 2003-MAYAKU-HEN-SEC
           END-IF
      *
           INITIALIZE                  FMCHUSYA-REC
      *
      *    発行区分毎に印刷
           MOVE    ZERO                TO  IDXN
           PERFORM VARYING     IDX-P   FROM    1   BY  1
                   UNTIL       IDX-P   >   4
               IF      ORCHC502-CHKPRT (IDX-P) =   1
                   COMPUTE IDXN    =   IDXN    +   1
                   IF      IDX-H           =   2
                       MOVE    WRK-SIJISEN-HEAD2(IDX-P)
                                               TO  FMCHUSYA-TITLE (IDXN)
                       MOVE    WRK-SIJISEN-PRT2 (IDX-P)
                                               TO  FMCHUSYA-MAISU (IDXN)
                   ELSE
                       MOVE    WRK-SIJISEN-HEAD1(IDX-P)
                                               TO  FMCHUSYA-TITLE (IDXN)
                       MOVE    WRK-SIJISEN-PRT1 (IDX-P)
                                               TO  FMCHUSYA-MAISU (IDXN)
                   END-IF
               END-IF
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
       7004-APIMEI-HENSYU-SEC      SECTION.
      *
           MOVE    "chushasen_n"       TO  FMCHUSYA-FORM-ID
      *    プリンタ名
           MOVE    SYS-1034-PRTNM(7)   TO  FMCHUSYA-PRINTER
      *
           IF      ORCHC502-SYORIKBN   =   "1"
               MOVE    "2"             TO  FMCHUSYA-HAKKOKBN
           ELSE
               MOVE    "1"             TO  FMCHUSYA-HAKKOKBN
           END-IF
      *
      *    処理日
           MOVE    WRK-API-SYSYMD      TO  FMCHUSYA-SYSYMD
           MOVE    WRK-API-SYSTIME     TO  FMCHUSYA-SYSTIME
      *    指示日
           MOVE    WRK-API-SRYYMD      TO  FMCHUSYA-SRYYMD
      *
      *    診療科
           MOVE    ORCHC502-SRYKA      TO  FMCHUSYA-SRYKA
           MOVE    SYS-1005-SRYKANAME1 TO  FMCHUSYA-SRYKANAME
      *    ドクターコード
           MOVE    WRK-DRCD            TO  FMCHUSYA-DR-CODE 
      *    医師情報
           MOVE    SYS-1010-KANANAME   TO  FMCHUSYA-DR-KANANAME
           MOVE    SYS-1010-NAME       TO  FMCHUSYA-DR-NAME
           MOVE    SYS-1010-MAYAKULICENSE
                                       TO  FMCHUSYA-DR-MAYAKULICENSE
      *
      *    患者情報
           MOVE    ORCHC502-PTNUM      TO  FMCHUSYA-PTNUM
           MOVE    PTINF-NAME          TO  FMCHUSYA-NAME
           MOVE    PTINF-KANANAME      TO  FMCHUSYA-KANANAME
           MOVE    PTINF-SEX           TO  FMCHUSYA-SEX
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  FMCHUSYA-BIRTHDAY
      *    年齢
      *    MOVE    WRK-AGE-HEN         TO  FMCHUSYA-NENREI
      *
      *    病棟番号
           MOVE    WRK-BTUNUM          TO  FMCHUSYA-BTUNUM
      *    病棟名称
           MOVE    WRK-BTUNAME         TO  FMCHUSYA-BTUNAME
      *    病室番号
           MOVE    WRK-ROOMNO          TO  FMCHUSYA-BRMNUM
      *    麻薬
           IF      IDX-H               =   2
               MOVE    "1"             TO  FMCHUSYA-MAYAKU
           END-IF
      *
      *    回数
           MOVE    WRK-API-KAISU-X     TO  FMCHUSYA-KAI-RENNUM
      *
      *    注射内容
           MOVE    WRK-CHUSYAMEI       TO  FMCHUSYA-CHUSYA-NAME
           MOVE    WRK-API-CHUSYACD    TO  FMCHUSYA-CHUSYA-SRYCD
      *    診療区分
           MOVE    LNK-ORDERPRT-SRYKBN (1)
                                       TO  FMCHUSYA-SRYKBN
           MOVE    LNK-ORDERPRT-SRYSYUKBN (1)
                                       TO  FMCHUSYA-SRYSYUKBN
      *    処方内容
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-AP
               MOVE    WRK-API-YKZNAME (IDX)
                                       TO  FMCHUSYA-YKZNAME (IDX)
               MOVE    WRK-API-SURYO (IDX)
                                       TO  FMCHUSYA-SURYO (IDX)
               MOVE    WRK-API-TANINAME (IDX)
                                       TO  FMCHUSYA-TANINAME(IDX)
               MOVE    WRK-API-SRYCD (IDX)
                                       TO  FMCHUSYA-SRYCD (IDX)
           END-PERFORM
      *
      *    備考欄
           PERFORM VARYING     IDX-B   FROM    1   BY  1
                   UNTIL       IDX-B   >   3
               IF      WRK-BIKOH  (IDX-B)  NOT =   SPACE
                   MOVE    WRK-BIKOH  (IDX-B)  TO  FMCHUSYA-MEMO(IDX-B)
               END-IF
           END-PERFORM
      *
           MOVE    SPA-OPID            TO  FMCHUSYA-OPID
      *
           .
       7004-APIMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＡＰＩ明細内容編集処理
      *****************************************************************
       70042-API-ORDERPRT-HEN-SEC      SECTION.
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
               MOVE   TNS-FORMALNAME       TO  WRK-TNS-NAME
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
           MOVE    WRK-TNS-NAME        TO  WRK-API-YKZNAME
                                                      (IDX-AP)
           MOVE    WRK-SRYCD           TO  WRK-API-SRYCD
                                                      (IDX-AP)
      *
           IF     (WRK-SRYCD (1:1)   =   "6"  OR  "7" )  OR
                  (WRK-SRYCD (1:3)   =   "059"        )
      *        数量
               MOVE    LNK-ORDERPRT-SRYSURYO (IDX)
                                           TO  WRK-SURYO
               PERFORM 20012-SURYO-HEN-SEC
               MOVE    WRK-SURYO-X         TO  WRK-API-SURYO
                                                      (IDX-AP)
      *        数量単位
               MOVE    TNS-TANINAME        TO  WRK-API-TANINAME
                                                      (IDX-AP)
           END-IF
           .
       70042-API-ORDERPRT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＡＰＩ送信処理
      *****************************************************************
       7009-API-CALLSYORI-SEC         SECTION.
      *
      *    カスタムＩＤ
           MOVE    PARA-API-CUSTOM-ID  TO  FMCHUSYA-CUSTOM-ID
      *
           INITIALIZE                      RPMAIN-AREA
           MOVE    "chushasen_n"       TO  RPMAIN-ID
           MOVE    PARA-API-CUSTOM-ID  TO  RPMAIN-CUSTOM-ID
           MOVE    "注射処方せん"      TO  RPMAIN-TITLE
           MOVE    "fm_order_injection"
                                       TO  MCP-TABLE
           MOVE    FMCHUSYA-DATA       TO  RPMAIN-DATA
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
           INITIALIZE                  LNK-ORDERPRT-AREA
           INITIALIZE                  LNK-ORDERPRT-MAX
           MOVE    SPACE               TO  WRK-KAISU-HENSYU
           MOVE    SPACE               TO  WRK-KAISU-X
           MOVE    1                   TO  WRK-API-KAISU-9
           MOVE    SPACE               TO  WRK-SRYYMD
           INITIALIZE                  WRK-API-SYOHOU-AREA
           MOVE    ZERO                TO  IDX-AP
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
      *    入院帳票オーダー検索
           INITIALIZE                  ORDERPRT-REC
           MOVE    SPA-HOSPNUM         TO  ORDERPRT-HOSPNUM
           MOVE    SPA-PTID            TO  ORDERPRT-PTID
           MOVE    ORCHC502-PTNUM      TO  ORDERPRT-PTNUM
           MOVE    SPA-NYUGAIKBN       TO  ORDERPRT-NYUGAIKBN
           MOVE    ORCHC502-SRYKA      TO  ORDERPRT-SRYKA
           MOVE    ORCHC502-HKNCOMBI   TO  ORDERPRT-HKNCOMBI
           MOVE    ORCHC502-SYSYMD     TO  ORDERPRT-SYSYMD
           MOVE    ORCHC502-PRINTYMD   TO  ORDERPRT-PRINTYMD
           MOVE    ORCHC502-PRINTHMS   TO  ORDERPRT-PRINTHMS
           MOVE    WRK-PRTKBN          TO  ORDERPRT-PRTKBN
      *
           MOVE    ORDERPRT-REC        TO  MCPDATA-REC
           MOVE    "tbl_orderprt"      TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           MOVE    "DBDELETE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ORCHC502 ORDERPRT DEL ERR:"  MCP-RC
                       ",KEY:" ORDERPRT-KEY
               MOVE    1               TO  ORCHC502-RC
           END-IF
           .
       500-DELETE-EXT.
           EXIT.
      *
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
      *    MOVE    ZERO                TO  WRK-MOJISU
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
           MOVE    ORCHC502-SRYYMD     TO  SYS-STYUKYMD
           MOVE    ORCHC502-SRYYMD     TO  SYS-EDYUKYMD
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
