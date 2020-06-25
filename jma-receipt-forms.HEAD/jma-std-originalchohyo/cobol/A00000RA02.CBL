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
       PROGRAM-ID.             A00000RA02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為
      *  コンポーネント名  : 処方控え
      *  管理者            : 
      *  作成日付    作業者    記述
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *****************************************************************
      *  作成日付    作業者    記述
      *  16/08/08    萩野      処方控え新規
      *  16/08/26    萩野      フッター改ページ不具合修正
      *  16/09/06    萩野      フッターの「診療日」と日付の間にスペースを入れる
      *                        投薬の内容を「●内服」「●頓服」「●外用」の３グループに分ける
      *  16/09/26    萩野      院内分、院外分の印字をプログラムオプションで選択できるようにする
      *  17/05/19    萩野      一般名処方加算対象追加対応
      *                        後発区分04対応
      *  20/02/20    萩野      ユーザー単位対応
      *  20/03/23    萩野      コメント桁数変更
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  ASGNPARA
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
       01  PRT-REC.
           03  PRT-PRTDATA         PIC X(20000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
           COPY    "CPASGNPARA.INC".
      *
      *    帳票領域
           COPY    "A00000RA02.INC".
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TNSP            PIC 9(01).
      *
           03  FLG-GENERICNAME     PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-HIKAKU          PIC 9(01).
           03  FLG-IPNMEI          PIC 9(01).
      *
           03  FLG-SP              PIC 9(01).
           03  FLG-SOURYO          PIC 9(01).
           03  FLG-DRCD            PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-GROUP           PIC 9(01).
           03  FLG-SRYKBN2         PIC 9(01).
           03  FLG-SRYKBN3         PIC 9(01).
           03  FLG-SRYKBN6         PIC 9(01).
           03  FLG-SRYKBN7         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(03).
           03  CNT-LINE            PIC 9(02).
           03  CNT-ZAI             PIC 9(03).
           03  CNT-ZAI2            PIC 9(03).
           03  CNT-SRY             PIC 9(02).
           03  CNT-YKZ             PIC 9(03).
           03  CNT-YKZ2            PIC 9(03).
           03  CNT-YKZALL          PIC 9(03).
           03  CNT-HC62            PIC 9(03).
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDXF                PIC 9(04).
           03  IDXF2               PIC 9(04).
           03  IDX-STR             PIC 9(04).
           03  IDX-STR1            PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
           03  IDX-ZAI             PIC 9(03).
           03  IDX-SRY             PIC 9(02).
      *
           03  KETA                PIC 9(04).
           03  IDX-ST              PIC 9(02).
           03  IDX-PTNUM           PIC 9(02).
           03  IDX-PRINT           PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MSG             PIC X(500).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-HENSURYO        PIC X(11).
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZZ.
               05  WRK-SURYO-Z1-9  REDEFINES   WRK-SURYO-Z1
                                   PIC ZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZZZ.
      *
           03  WRK-ZZ9             PIC ZZ9.
           03  WRK-Z9              PIC Z9.
      *
           03  WRK-SRYCD           PIC X(09).
           03  WRK-TBL-SRYCD       PIC X(09)  OCCURS 50.
      *
           03  WRK-HEN-AREA.
               05  WRK-HENLINE1    PIC X(03).
               05  WRK-HENLINE2    PIC X(50).
               05  WRK-HENNAIYO    PIC X(24).
               05  WRK-YKZNAME     PIC X(140).
               05  WRK-YOUHOU      PIC X(140).
               05  WRK-SURYOU      PIC X(11).
               05  WRK-TANI        PIC X(12).
               05  WRK-KAISUU      PIC X(03).
      *
           03  WRK-HENSYUDATA      PIC X(140).
      *
      *ver.4.7
           03  WRK-SHO-NAME        PIC X(140).
           03  WRK-SHO-TANINAME    PIC X(20).
      *
           03  WRK-TNS-NAME-G      PIC X(210).
           03  WRK-NAME            PIC X(210).
           03  WRK-PRTNAME         PIC X(10).
      *
      *    文字変換
           03  WRK-MAE-INPUT       PIC X(200).
           03  WRK-OUT-INPUT       PIC X(200).
      *
      *    右詰め変換用
           03  WRK-HENKAN          PIC  X(20).
           03  WRK-LENGTH          PIC  9(02).
           03  WRK-HENSYU          PIC  X(20).
      *
      *    患者番号全角変換用
           03  WRK-HENKAN-PTNUM    PIC  X(40).
      *
           03  WRK-DRCD            PIC X(05).
           03  WRK-DENPNUM         PIC 9(07).
      *
           03  WRK-CHARCNT         PIC 9(02).
           03  WRK-CHARFLG         PIC 9(01).
      *--- 2016/09/06 START ------------------------------------------
           03  WRK-GROUPNAME       PIC X(20).
      *--- 2016/09/06 END   ------------------------------------------
      *
      *    医師コード 一時領域
       01  H016-DR-AREA.
           03  H016-DRCD             PIC X(05).
      *
       01  H016-DR2-AREA.
           03  H016-DR-TBL         OCCURS  5.
             05  H016-DRCD1          PIC X(05).
             05  H016-DRCD2          PIC X(05).
      *
      *    伝票番号 一時領域
       01  H016-AREA.
           03  H016-GRP-TBL        OCCURS  5.
             05  H016-DENPNUM        PIC 9(07).
      *
      *    薬剤名編集
       01  WRK-MEISAI-AREA.
           03  WRK-M-NAME              PIC X(228).
           03  WRK-M-NAME-RR           REDEFINES    WRK-M-NAME.
      *        １行名称の最大
               05  WRK-M-NAME-R1           PIC X(76).
               05  WRK-M-NAME-R2           PIC X(76).
               05  WRK-M-NAME-R3           PIC X(76).
           03  WRK-M-NAME-RR2          REDEFINES    WRK-M-NAME.
               05  WRK-M-NAME-R            PIC X(76)
                                           OCCURS   3.
      *
           03  WRK-M-KETA              PIC 9(03).
           03  WRK-M-KETACHK           PIC 9(03).
           03  WRK-FUKA-MEI-AREA.
      *        変更不可コード編集
               05  WRK-FUKA-TBL            OCCURS   3.
                   07  WRK-KOU-SRYCD           PIC X(09).
                   07  WRK-KOU-NAME            PIC X(100).
                   07  WRK-KOU-TNSNAME         PIC X(100).
                   07  WRK-KOU-KETA            PIC 9(03).
      *        後発変更不可
               05  WRK-HEN-FUKA        PIC X(02).
      *        一般名記載
               05  WRK-IPNMEI          PIC 9(01).
           03  WRK-HEN-NAME            PIC X(100).
           03  WRK-HEN-NAME2           PIC X(100).
           03  WRK-HEN-KETA            PIC 9(03).
      *
      *    剤内容領域
       01  WRK-SRY-AREA.
           03  WRK-ZAIKAIKEI       PIC 9(03).
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-SRYSYUKBN       PIC X(03).
           03  WRK-SRY-TBL         OCCURS  50.
               05  WRK-SRYSURYO    PIC 9(05)V9(05).
               05  WRK-SRYKAISU    PIC 9(03).
               05  WRK-INPUTCOMENT PIC X(140).
               05  WRK-YOURYOU-TBL.
                   07  WRK-YOURYOU     PIC 9(05)V9(05)
                                       OCCURS   5.
      *
      *    印刷編集領域
       01  WRK-LINE-AREA.
           03  WRK-LINE-1.
               05  WRK-PTNAME      PIC X(54).
           03  WRK-LINE-2.
               05  FILLER          PIC X(08)   VALUE   "処方日：".
               05  WRK-SRYYMDG     PIC X(04).
               05  WRK-SRYYMDY     PIC X(02).
               05  FILLER          PIC X(02)   VALUE   "年".
               05  WRK-SRYYMDM     PIC X(02).
               05  FILLER          PIC X(02)   VALUE   "月".
               05  WRK-SRYYMDD     PIC X(02).
               05  FILLER          PIC X(14)   VALUE   "日　保険医名：".
               05  WRK-DRNAME      PIC X(18).
           03  WRK-LINE-3.
               05  WRK-HOSPNAME    PIC X(54).
           03  WRK-LINE-4.
               05  FILLER          PIC X(08)   VALUE   "ＴＥＬ：".
               05  WRK-HOSPTEL     PIC X(46).
           03  WRK-LINE-5.
               05  FILLER          PIC X(54)   VALUE
               "＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿".
      *
      *    プログラムオプションパラメタ
       01  PARA-AREA.
           03  PARA-TYUSYA             PIC X(01).
           03  PARA-KENSA              PIC X(01).
           03  PARA-GAZOU              PIC X(01).
      *--- 2016/09/26 START ------------------------------------------
           03  PARA-SHOHOU             PIC X(01).
      *--- 2016/09/26 END   ------------------------------------------
      *
           03  WRK-PARA-NAME           PIC X(25).
           03  WRK-PARA                PIC X(01).
      *
      *    パラメタ領域
       01  WRK-PARA-AREA.
           03  WRK-PARA-LDNAME       PIC X(10).
           03  WRK-PARA-SYORIKBN     PIC 9(01).
           03  WRK-PARA-SYSYMD       PIC X(08).
           03  WRK-PARA-SRYYMD       PIC X(08).
           03  WRK-PARA-PTID         PIC 9(10).
           03  WRK-PARA-PTNUM        PIC X(20).
           03  WRK-PARA-SRYKA        PIC 9(02).
           03  WRK-PARA-DRCD         PIC X(05).
           03  WRK-PARA-HKNCOMBINUM  PIC 9(04).
           03  WRK-PARA-SHORINAME    PIC X(20).
           03  WRK-PARA-PRINTNAME    PIC X(08).
           03  WRK-PARA-HOSPNUM      PIC 9(02).
           03  WRK-PARA-DENPNUM      PIC 9(07).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    医療機関情報−基本情報
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY    "CPSK1002.INC".
      *
      *    職員情報
           COPY    "CPSK1010.INC".
      *
      *    処方箋ＧＥ表示
           COPY    "CPSK1030.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ワーク診療行為マスタ
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    点数付加情報マスタ
       01  TNSP-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    一般名テーブル
       01  GENERICNAME-REC.
           COPY    "CPGENERICNAME.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *--- 2016/01/05 START ------------------------------------------
      *    ＳＰＡ共通設定
           COPY    "CPORCSCOMMON.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *--- 2016/01/05 END   ------------------------------------------
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角変換サブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    お薬手帳発行サブパラメタ
           COPY    "COMMON-SPA".
           COPY    "CPORCHC62.INC".
           COPY    "CPORCSCH62.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      *
      ******************************************************************
       PROCEDURE               DIVISION
      *
                                   USING    COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  SYS-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
                                       H016-AREA
                                       H016-DR-AREA
                                       H016-DR2-AREA
                                       WRK-PARA-AREA
                                       SPA-AREA
      *
           PERFORM 900-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    WRK-PARA-LDNAME
                                               WRK-PARA-SYORIKBN
                                               WRK-PARA-SYSYMD
                                               WRK-PARA-SRYYMD
                                               WRK-PARA-PTID
                                               WRK-PARA-PTNUM
                                               WRK-PARA-SRYKA
                                               WRK-PARA-DRCD
                                               WRK-PARA-HKNCOMBINUM
                                               WRK-PARA-SHORINAME
                                               WRK-PARA-PRINTNAME
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-DENPNUM
           END-UNSTRING
      *
           DISPLAY "--- COMMAND-PARAM ---"
           DISPLAY "PARA LDNAME       ="WRK-PARA-LDNAME
           DISPLAY "PARA SYORIKBN     ="WRK-PARA-SYORIKBN
           DISPLAY "PARA SYSYMD       ="WRK-PARA-SYSYMD
           DISPLAY "PARA SRYYMD       ="WRK-PARA-SRYYMD
           DISPLAY "PARA PTID         ="WRK-PARA-PTID
           DISPLAY "PARA PTNUM        ="WRK-PARA-PTNUM
           DISPLAY "PARA SRYKA        ="WRK-PARA-SRYKA
           DISPLAY "PARA DRCD         ="WRK-PARA-DRCD
           DISPLAY "PARA HKNCOMBINUM  ="WRK-PARA-HKNCOMBINUM
           DISPLAY "PARA SHORINAME    ="WRK-PARA-SHORINAME
           DISPLAY "PARA PRINTNAME    ="WRK-PARA-PRINTNAME
           DISPLAY "PARA HOSPNUM      ="WRK-PARA-HOSPNUM
           DISPLAY "PARA DENPNUM      ="WRK-PARA-DENPNUM
      *
           MOVE    WRK-PARA-LDNAME        TO  SPA-SAKIPG
           MOVE    MCP-USER               TO  SPA-OPID
           MOVE    WRK-PARA-HOSPNUM       TO  SPA-HOSPNUM
           MOVE    WRK-PARA-SRYYMD        TO  SPA-SYSYMD
           MOVE    WRK-PARA-SRYYMD        TO  SPA-SRYYMD
           MOVE    WRK-PARA-HKNCOMBINUM   TO  SPA-HKNCOMBINUM
           MOVE    WRK-PARA-PTID          TO  SPA-PTID
           MOVE    WRK-PARA-PTNUM         TO  SPA-PTNUM
           MOVE    WRK-PARA-SRYKA         TO  SPA-SRYKA 
           MOVE    "2"                    TO  SPA-NYUGAIKBN
           MOVE    WRK-PARA-DRCD          TO  SPA-DRCD
      *
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
      *
           ACCEPT  SYS-TIME    FROM    TIME
      *
      *    出力先プリンタ名割り当て情報
           PERFORM 900-1031-READ-SEC
      *
      *    医療機関情報−基本情報
           PERFORM 900-1001-READ-SEC
      *
      *    医療機関情報−所在地、連絡先
           PERFORM 900-1002-READ-SEC
      *
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
      *    職員情報
           PERFORM 900-1010-READ-SEC
      *
      *    帳票編集区分情報
           PERFORM 900-1030-READ-SEC
      *
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *
           CALL    "ORCSONPRTSET"  USING   SPA-AREA
           IF    ( SPA-CLIENT-PRT-FLG  =   2 )
            OR   ( SPA-PRTCONF         =   "1" )
               INITIALIZE                  ORCSPRTCTRLAREA
               CALL    "ORCSPRTCTRL"       USING
                                           SPA-AREA
                                           ORCSPRTCTRLAREA
                                           MCPAREA
           END-IF
           MOVE    "01"                TO  SPA-PRT-FLG
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           DISPLAY "--- OPTION-SELECT ---"
      *
           INITIALIZE                    PARA-AREA
      *
      *    注射
           MOVE    "TYUSYA"              TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)         TO  PARA-TYUSYA
           IF  WRK-PARA(1:1)           =   SPACE
               MOVE    "0"               TO  PARA-TYUSYA
           END-IF
      *
      *    検査
           MOVE    "KENSA"               TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)         TO  PARA-KENSA
           IF  WRK-PARA(1:1)           =   SPACE
               MOVE    "0"               TO  PARA-KENSA
           END-IF
      *
      *    画像
           MOVE    "GAZOU"               TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)         TO  PARA-GAZOU
           IF  WRK-PARA(1:1)           =   SPACE
               MOVE    "0"               TO  PARA-GAZOU
           END-IF
      *
      *--- 2016/09/26 START ------------------------------------------
      *    院内院外
           MOVE    "SHOHOU"              TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)         TO  PARA-SHOHOU
           IF  WRK-PARA(1:1)           =   SPACE
               MOVE    "1"               TO  PARA-SHOHOU
           END-IF
      *--- 2016/09/26 END   ------------------------------------------
      *
           DISPLAY "TYUSYA            ="PARA-TYUSYA
           DISPLAY "KENSA             ="PARA-KENSA
           DISPLAY "GAZOU             ="PARA-GAZOU
           DISPLAY "SHOHOU            ="PARA-SHOHOU
      *
           .
       100-OPTION-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプションパラメタ取得処理
      *****************************************************************
       1001-OPTION-CALL-SEC             SECTION.
      *
           INITIALIZE                  ORCSPRGOPTIONAREA
      *
           MOVE    "A00000RA02"        TO  ORCSPRGOPTION-PRGID
           MOVE    WRK-PARA-NAME       TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
      *
           MOVE    SPACE                   TO  WRK-PARA
           MOVE    ORCSPRGOPTION-OT-PARA   TO  WRK-PARA
      *
           .
       1001-OPTION-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           EVALUATE  WRK-PARA-LDNAME(1:3)
           WHEN  "K02"
      *      中途終了処理
             PERFORM  210-MAIN-SEC
           WHEN  "K03"
      *      請求確認（K03）
             PERFORM  220-MAIN-SEC
           WHEN  OTHER
             GO TO  200-MAIN-EXT
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    中途終了  主処理
      *****************************************************************
       210-MAIN-SEC                SECTION.
      *
           DISPLAY "--- 210-MAIN-SEC ---"
      *
           MOVE    ZERO              TO  CNT-HC62
      *
      *    印刷対象取得処理
           PERFORM  210-SUB-SEC
      *
           PERFORM  VARYING  IDX-ST  FROM  1  BY  1
                    UNTIL  ( IDX-ST              >  5
                    OR       H016-DRCD1(IDX-ST)  =  SPACE )
      *
               COMPUTE CNT-HC62    =   CNT-HC62    +   1
      *
               INITIALIZE  ORCHC62AREA
               INITIALIZE  ORCSCH62AREA
      *
      *--- 2016/09/26 START ------------------------------------------
      *         MOVE    "3"                     TO  ORCSCH62-KBN
               IF    PARA-SHOHOU          =   "1"
                   MOVE    "3"                 TO  ORCSCH62-KBN
               ELSE
                   MOVE    "7"                 TO  ORCSCH62-KBN
               END-IF
      *--- 2016/09/26 END   ------------------------------------------
               MOVE    H016-DRCD1(IDX-ST)      TO  SPA-DRCD
               MOVE    H016-DRCD2(IDX-ST)      TO  H016-DRCD
      *
               CALL    "A00000RA022"        USING
                                             SPA-AREA
                                             ORCSCH62AREA
                                             ORCHC62AREA
                                             H016-DR-AREA
      *
               DISPLAY "IDX-ST              ="IDX-ST
               DISPLAY "H016-DRCD1          ="H016-DRCD1(IDX-ST)
               DISPLAY "H016-DRCD2          ="H016-DRCD2(IDX-ST)
               DISPLAY "ORCHC62-SRYYMD      ="ORCHC62-SRYYMD
               DISPLAY "ORCHC62-CNT         ="ORCHC62-CNT
               DISPLAY "ORCHC62-DRCD        ="ORCHC62-DRCD
               DISPLAY "ORCHC62-DENPNUM     ="ORCHC62-DENPNUM
               DISPLAY "ORCHC62-ZAI-MAX     ="ORCHC62-ZAI-MAX
      *
      *        印字データ  初期化
               INITIALIZE                  G016
               MOVE    ZERO            TO  CNT-LINE
      *
      *--- 2016/09/06 START ------------------------------------------
      *         COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *         MOVE    "●内服・頓服・外用"
      *                                       TO  G016-NAIYO11(CNT-LINE)
      *--- 2016/09/06 END   ------------------------------------------
      *
      *        データがなくなるまで
               MOVE    ZERO            TO  CNT-ZAI
               MOVE    ZERO            TO  CNT-ZAI2
               MOVE    ZERO            TO  FLG-GROUP
               MOVE    ZERO            TO  FLG-SRYKBN2
               MOVE    ZERO            TO  FLG-SRYKBN3
               MOVE    ZERO            TO  FLG-SRYKBN6
               MOVE    ZERO            TO  FLG-SRYKBN7
      *
               PERFORM VARYING IDX-ZAI     FROM    1   BY  1
                       UNTIL ( IDX-ZAI     >   ORCHC62-ZAI-MAX  )
      *            帳票ＢＯＤＹ情報編集処理
                   PERFORM 220-BODY-HENSYU-SEC
               END-PERFORM
      *
      *      データがあれば印刷処理を行う
             IF  FLG-GROUP       NOT =  ZERO
      *
      *        帳票ＦＯＯＴ情報編集処理
               PERFORM 430-FOOT-SET-SEC
      *
      *        帳票印刷処理
               PERFORM 400-PRT-WRITE-SEC
      *
             END-IF
      *
           END-PERFORM
      *
           .
       210-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    中途終了  サブ処理
      *****************************************************************
       210-SUB-SEC                SECTION.
      *
      *    ワーク診療行為から医師情報を取得する
           PERFORM  900-WKSRYACT-SELECT-SEC
           PERFORM  UNTIL  FLG-WKSRYACT  =  1
      *
             INITIALIZE  WRK-DRCD
      *
             MOVE    WKSRY-DRCD             TO  WRK-DRCD
      *
      *      ドクターの編集
             IF    WKSRY-INPUTCD(1)(1:1)  =  "$"
               IF  WKSRY-INPUTCD(1)(5:4)  NUMERIC
                 MOVE    "1"                    TO  WRK-DRCD(1:1)
                 MOVE    WKSRY-INPUTCD(1)(5:4)  TO  WRK-DRCD(2:4)
               END-IF
             END-IF
      *
             MOVE    ZERO             TO  FLG-DRCD
             PERFORM  VARYING  IDX-ST  FROM  1  BY  1
                      UNTIL  ( IDX-ST     >  5
                      OR       FLG-DRCD  =  1 )
      *
               IF  H016-DRCD1(IDX-ST)  =  SPACE
                 MOVE    1                TO  FLG-DRCD
                 MOVE    WKSRY-DRCD       TO  H016-DRCD1(IDX-ST)
                 MOVE    WRK-DRCD         TO  H016-DRCD2(IDX-ST)
               ELSE
                 IF    ( WKSRY-DRCD  =  H016-DRCD1(IDX-ST)
                   AND   WRK-DRCD    =  H016-DRCD2(IDX-ST) )
                   MOVE    1                TO  FLG-DRCD
                 END-IF
               END-IF
      *
             END-PERFORM
      *
             PERFORM  900-WKSRYACT-FETCH-SEC
           END-PERFORM
           PERFORM  900-WKSRYACT-CLOSE-SEC
      *
           .
       210-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求確認  主処理
      *****************************************************************
       220-MAIN-SEC                SECTION.
      *
           DISPLAY "--- 220-MAIN-SEC ---"
      *
           MOVE    ZERO                TO  CNT-HC62
           MOVE    9999999             TO  ORCHC62-DENPNUM
      *
           PERFORM  VARYING  IDX-ST  FROM  1  BY  1
                    UNTIL    IDX-ST  >     10
                    OR      ORCHC62-DENPNUM     =   ZERO
      *
               INITIALIZE  ORCHC62AREA
               INITIALIZE  ORCSCH62AREA
      *
      *--- 2016/09/26 START ------------------------------------------
      *         MOVE    "1"                     TO  ORCSCH62-KBN
               IF    PARA-SHOHOU          =   "1"
                   MOVE    "1"                 TO  ORCSCH62-KBN
               ELSE
                   MOVE    "6"                 TO  ORCSCH62-KBN
               END-IF
      *--- 2016/09/26 END   ------------------------------------------
               MOVE    WRK-PARA-DENPNUM        TO  ORCSCH62-DENPNUM
               MOVE    IDX-ST                  TO  ORCHC62-CNT
      *
               CALL    "A00000RA022"        USING
                                             SPA-AREA
                                             ORCSCH62AREA
                                             ORCHC62AREA
      *
               DISPLAY "IDX-ST              ="IDX-ST
               DISPLAY "ORCHC62-SRYYMD      ="ORCHC62-SRYYMD
               DISPLAY "ORCHC62-CNT         ="ORCHC62-CNT
               DISPLAY "ORCHC62-DRCD        ="ORCHC62-DRCD
               DISPLAY "ORCHC62-DENPNUM     ="ORCHC62-DENPNUM
               DISPLAY "ORCHC62-DENPNUMG1   ="ORCHC62-GRP-DENPNUM(1)
               DISPLAY "ORCHC62-DENPNUMG2   ="ORCHC62-GRP-DENPNUM(2)
               DISPLAY "ORCHC62-DENPNUMG3   ="ORCHC62-GRP-DENPNUM(3)
               DISPLAY "ORCHC62-ZAI-MAX     ="ORCHC62-ZAI-MAX
      *
               COMPUTE CNT-HC62    =   CNT-HC62    +   1
      *
      *        印字データ  初期化
               INITIALIZE                  G016
               MOVE    ZERO            TO  CNT-LINE
      *
      *--- 2016/09/06 START ------------------------------------------
      *         COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *         MOVE    "●内服・頓服・外用"
      *                                     TO  G016-NAIYO11(CNT-LINE)
      *--- 2016/09/06 END   ------------------------------------------
      *
      *        データがなくなるまで
               MOVE    ZERO            TO  CNT-ZAI
               MOVE    ZERO            TO  CNT-ZAI2
               MOVE    ZERO            TO  FLG-GROUP
               MOVE    ZERO            TO  FLG-SRYKBN2
               MOVE    ZERO            TO  FLG-SRYKBN3
               MOVE    ZERO            TO  FLG-SRYKBN6
               MOVE    ZERO            TO  FLG-SRYKBN7
      *
               PERFORM VARYING IDX-ZAI     FROM    1   BY  1
                       UNTIL ( IDX-ZAI     >   ORCHC62-ZAI-MAX  )
      *            帳票ＢＯＤＹ情報編集処理
                   PERFORM 220-BODY-HENSYU-SEC
               END-PERFORM
      *
      *        データがあれば印刷処理を行う
               IF  FLG-GROUP       NOT =  ZERO
      *
      *          帳票ＦＯＯＴ情報編集処理
                 PERFORM 430-FOOT-SET-SEC
      *
      *          帳票印刷処理
                 PERFORM 400-PRT-WRITE-SEC
      *
               END-IF
      *
           END-PERFORM
      *
           .
       220-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＢＯＤＹ情報編集処理
      *****************************************************************
       220-BODY-HENSYU-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SOURYO
      *
      *    コメントは対象外
      *     IF      ORCHC62-SRYKBN(IDX-ZAI) =   "98"
      *         GO  TO  220-BODY-HENSYU-EXT
      *     END-IF
      *
      *    注射
           IF      ORCHC62-SRYKBN(IDX-ZAI)         =   "31"
                                                  OR   "32"
                                                  OR   "33"
               IF  PARA-TYUSYA           =   "0"
                   GO  TO  220-BODY-HENSYU-EXT
               END-IF
           END-IF
      *
      *    検査
           IF      ORCHC62-SRYKBN(IDX-ZAI)         =   "60"
               IF  PARA-KENSA            =   "0"
                   GO  TO  220-BODY-HENSYU-EXT
               END-IF
           END-IF
      *
      *    画像
           IF      ORCHC62-SRYKBN(IDX-ZAI)         =   "70"
               IF  PARA-GAZOU            =   "0"
                   GO  TO  220-BODY-HENSYU-EXT
               END-IF
           END-IF
      *
           IF      ORCHC62-SRYKBN(IDX-ZAI)(1:1)   
                            NOT  =   ORCHC62-SRYKBN(IDX-ZAI - 1)(1:1)
               MOVE    ZERO                TO  CNT-ZAI2
           END-IF
      *
      *--- 2016/09/06 START ------------------------------------------
           IF  ORCHC62-SRYKBN(IDX-ZAI)(1:1)  =  "2"
               IF  ORCHC62-SRYKBN(IDX-ZAI) 
                                NOT = ORCHC62-SRYKBN(IDX-ZAI - 1)
                   MOVE    ZERO            TO  CNT-ZAI2
               END-IF
           END-IF
      *--- 2016/09/06 END   ------------------------------------------
      *
           COMPUTE CNT-ZAI     =   CNT-ZAI     +   1
      *
      *    剤情報編集領域にセット
           INITIALIZE                  WRK-SRY-AREA
      *
           MOVE    ORCHC62-ZAIKAIKEI(IDX-ZAI)
                                   TO  WRK-ZAIKAIKEI
           MOVE    ORCHC62-SRYSYUKBN(IDX-ZAI)
                                   TO  WRK-SRYSYUKBN
           MOVE    ORCHC62-SRYKBN(IDX-ZAI)
                                   TO  WRK-SRYKBN
      *
           MOVE    ZERO            TO  CNT-SRY
                                       CNT-YKZ
           PERFORM VARYING IDX-SRY     FROM    1   BY  1
                   UNTIL ( IDX-SRY     >   50  )
                   OR    ( ORCHC62-SRYCD(IDX-ZAI IDX-SRY)  =   SPACE )
      *        明細編集
               PERFORM 230-NAIYO-HENSYU-SEC
      *
           END-PERFORM
      *
      *    剤情報があれば処理する
           IF      CNT-SRY         =   ZERO
               GO  TO  220-BODY-HENSYU-EXT
           END-IF
      *
      *    回数編集
           PERFORM 240-KAISUU-HENSYU-SEC
      *
           MOVE    WRK-LINE-5      TO  G016-SEN(CNT-LINE)
      *
           .
       220-BODY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    内容編集処理
      *****************************************************************
       230-NAIYO-HENSYU-SEC        SECTION.
      *
      *ver.4.7
      *    商品名コード
           MOVE    SPACE               TO  WRK-SHO-TANINAME
           MOVE    SPACE               TO  WRK-SHO-NAME

           MOVE    ORCHC62-SRYCD(IDX-ZAI IDX-SRY)
                                   TO  WRK-SRYCD
      *
      *    薬剤、その他材料、用法以外は処理抜ける
      *     IF    ( WRK-SRYCD(1:1)  NOT =   "6"   ) AND
      *           ( WRK-SRYCD(1:3)  NOT =   "059" ) AND
      *           ( WRK-SRYCD(1:3)  NOT =   "001" )
      *         GO  TO  230-NAIYO-HENSYU-EXT
      *     END-IF
      *    減の文字は対象外
           IF      WRK-SRYCD   =   "820000047"
               GO  TO  230-NAIYO-HENSYU-EXT
           END-IF
      *
           INITIALIZE                     WRK-MEISAI-AREA
                                          FLG-IPNMEI
      *
           IF      WRK-SRYCD(1:1)  =   "6"
      *
      *        何番目の薬剤かをカウント
               ADD    1             TO  CNT-YKZ
                                        CNT-YKZALL
      *
      *        剤がゼロの場合、中途処理と判定
               IF  ORCHC62-ZAINUM(IDX-ZAI)  =  ZERO
      *            ワーク診療行為から一般名記載コード検索
                   PERFORM 2301-WKSRYACT-SYORI-SEC
               ELSE
      *            診療行為から一般名記載コード検索
                   PERFORM 2301-SRYACT-SYORI-SEC
               END-IF
           END-IF
      *
      *    点数検索
           PERFORM 900-TENSU-READ-SEC
      *
      *    点数なければ処理抜ける
           IF      FLG-TENSU       =   1
               GO  TO  230-NAIYO-HENSYU-EXT
           END-IF
      *
      *    診療行為名称抽出
           EVALUATE  ORCHC62-SRYKBN(IDX-ZAI)
               WHEN  "31"
               WHEN  "32"
               WHEN  "33"
      *              注射
                     IF  WRK-SRYCD(1:1)  =   "6"
                         CONTINUE
                     ELSE
                         IF    WRK-SRYCD(1:4)  =   "0086"
                               GO  TO  230-NAIYO-HENSYU-EXT
                         END-IF
                     END-IF
               WHEN  "60"
               WHEN  "70"
      *              検査  画像診断
                     IF  WRK-SRYCD(1:1)  =   "6"
                         GO  TO  230-NAIYO-HENSYU-EXT
                     ELSE
                         IF  WRK-SRYCD(1:2)  =   "16"  OR  "17"
      *                      検査、画像診断は
      *                      診療行為名の「〜加算」「〜判断料」などは印字しない
                             PERFORM 231-TENSUMEI-SYORI-SEC
                             IF  WRK-CHARFLG          =   1
                                 GO  TO  230-NAIYO-HENSYU-EXT
                             END-IF
                         ELSE
                             IF    WRK-SRYCD(1:4)  =   "0086"
                                   GO  TO  230-NAIYO-HENSYU-EXT
                             END-IF
                         END-IF
                     END-IF
               WHEN  OTHER
                     IF    WRK-SRYCD(1:4)  =   "0086"
                           GO  TO  230-NAIYO-HENSYU-EXT
                     END-IF
           END-EVALUATE
      *
      *--- 2016/09/06 START ------------------------------------------
      *    診療行為グループ名称
      *     PERFORM 231-GROUPMEI-SYORI-SEC
      *--- 2016/09/06 END   ------------------------------------------
      *
      *ver.4.7
      *    商品名コード
           IF      ORCHC62-SHO-SRYCD(IDX-ZAI IDX-SRY)  NOT =   SPACE
      *        名称と単位は商品名コードの内容とする
               IF      WRK-SHO-TANINAME    NOT =   SPACE
                   MOVE    WRK-SHO-TANINAME    TO  TNS-TANINAME
               END-IF
               IF      WRK-SHO-NAME        NOT =   SPACE
                   MOVE    WRK-SHO-NAME        TO  TNS-NAME
               END-IF
           END-IF
      *
           COMPUTE CNT-SRY     =   CNT-SRY     +   1
      *
           MOVE    ORCHC62-SRYSURYO(IDX-ZAI IDX-SRY)
                                   TO  WRK-SRYSURYO(CNT-SRY)
           MOVE    ORCHC62-SRYKAISU(IDX-ZAI IDX-SRY)
                                   TO  WRK-SRYKAISU(CNT-SRY)
           MOVE    ORCHC62-INPUTCOMENT(IDX-ZAI IDX-SRY)
                                   TO  WRK-INPUTCOMENT(CNT-SRY)
           MOVE    ORCHC62-YOURYOU-TBL(IDX-ZAI IDX-SRY)
                                   TO  WRK-YOURYOU-TBL(CNT-SRY)
      *
           INITIALIZE                  WRK-HEN-AREA
      *
      *    １件目だけ番号表示
           IF      CNT-SRY         =   1
      *--- 2016/09/06 START ------------------------------------------
      *         IF      ORCHC62-SRYKBN(IDX-ZAI)  =  "31"
      *                                          OR "32"
      *                                          OR "33"
      *                                          OR "60"
      *                                          OR "70"
      *                 MOVE    CNT-ZAI2        TO  WRK-Z9
      *         ELSE
      *                 MOVE    CNT-ZAI         TO  WRK-Z9
      *         END-IF
               COMPUTE CNT-ZAI2         =  CNT-ZAI2    +   1
               MOVE    CNT-ZAI2        TO  WRK-Z9
      *--- 2016/09/06 END   ------------------------------------------
               MOVE    WRK-Z9          TO  WRK-HENLINE1(1:2)
               MOVE    ")"             TO  WRK-HENLINE1(3:1)
      *--- 2016/09/06 START ------------------------------------------
                   PERFORM 231-GROUPMEI-SYORI2-SEC
      *--- 2016/09/06 END   ------------------------------------------
           END-IF
      *
           IF      TNS-SRYCD(1:3)  =   "001"
      *        用法編集
               PERFORM 240-YOUHOU-HENSYU-SEC
           ELSE
      *        名称編集
               PERFORM 240-YKZNAME-HENSYU-SEC
      *        数量編集
               PERFORM 240-SURYOU-HENSYU-SEC
           END-IF
      *
           .
       230-NAIYO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       2301-SRYACT-SYORI-SEC                   SECTION.
      *
      *    診療行為検索
           INITIALIZE                      SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    SPA-SRYYMD(1:6)     TO  SRY-SRYYM
           MOVE    SPA-SRYKA           TO  SRY-SRYKA
           MOVE    ORCHC62-ZAINUM(IDX-ZAI)
                                       TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-SRYACT-READ-SEC
      *
               MOVE    ZERO                      TO  FLG-HIKAKU
                                                     CNT-YKZ2
               PERFORM  UNTIL  FLG-SRYACT  =  1
                           OR  FLG-IPNMEI  =  1
      *
                 PERFORM VARYING     IDXF    FROM    1   BY  1
                         UNTIL       IDXF    >       5
                         OR          FLG-IPNMEI  =  1
      *
                   IF  FLG-HIKAKU  =  1
                     EVALUATE    SRY-SRYCD(IDXF)
                         WHEN    "099209903"
      *                      後発医薬品へ変更不可
                             MOVE    1           TO  WRK-IPNMEI
                         WHEN    "099209907"
      *                      銘柄名記載
                             MOVE    1           TO  WRK-IPNMEI
                         WHEN    "099209908"
      *                      一般名記載
                             MOVE    2           TO  WRK-IPNMEI
                     END-EVALUATE
      *
                     MOVE    1                   TO  FLG-IPNMEI
                   END-IF
      *
                   IF  SRY-SRYCD(IDXF)(1:1)  =  "6"
      *              診療行為TBL内で何番目の薬剤かカウント
                     ADD    1                    TO  CNT-YKZ2
      *
      *              次データの診療コードを比較するためにフラグオン
                     IF  WRK-SRYCD  =  SRY-SRYCD(IDXF)
                     AND CNT-YKZ    =  CNT-YKZ2
                       MOVE    1                 TO  FLG-HIKAKU
                     ELSE
                       MOVE    ZERO              TO  FLG-HIKAKU
                     END-IF
                   END-IF
      *
                 END-PERFORM
      *
                 PERFORM 900-SRYACT-READ-SEC
               END-PERFORM
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       2301-SRACT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為検索処理
      *****************************************************************
       2301-WKSRYACT-SYORI-SEC                   SECTION.
      *
      *    ワーク診療行為検索
           INITIALIZE                      WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    SPA-SRYKA           TO  WKSRY-SRYKA
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
           MOVE    SPA-HKNCOMBINUM     TO  WKSRY-HKNCOMBI
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-WKSRYACT-READ-SEC
      *
               MOVE    ZERO                      TO  FLG-HIKAKU
                                                     CNT-YKZ2
               PERFORM  UNTIL  FLG-WKSRYACT  =  1
                           OR  FLG-IPNMEI    =  1
      *
                 PERFORM VARYING     IDXF    FROM    1   BY  1
                         UNTIL       IDXF    >       5
                         OR          FLG-IPNMEI  =  1
      *
                   IF  FLG-HIKAKU  =  1
                     EVALUATE    WKSRY-SRYCD(IDXF)
                         WHEN    "099209903"
      *                      後発医薬品へ変更不可
                             MOVE    1           TO  WRK-IPNMEI
                         WHEN    "099209907"
      *                      銘柄名記載
                             MOVE    1           TO  WRK-IPNMEI
                         WHEN    "099209908"
      *                      一般名記載
                             MOVE    2           TO  WRK-IPNMEI
                     END-EVALUATE
      *
                     MOVE    1                   TO  FLG-IPNMEI
                   END-IF
      *
                   IF  WKSRY-SRYCD(IDXF)(1:1)  =  "6"
      *              診療行為TBL内で何番目の薬剤かカウント
                     ADD    1                    TO  CNT-YKZ2
      *
      *              次データの診療コードを比較するためにフラグオン
                     IF  WRK-SRYCD  =  WKSRY-SRYCD(IDXF)
                     AND CNT-YKZALL    =  CNT-YKZ2
                       MOVE    1                 TO  FLG-HIKAKU
                     ELSE
                       MOVE    ZERO              TO  FLG-HIKAKU
                     END-IF
                   END-IF
      *
                 END-PERFORM
      *
                 PERFORM 900-WKSRYACT-READ-SEC
               END-PERFORM
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
      *
           MOVE    "tbl_wksryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       2301-WKSRACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為グループ名称
      *****************************************************************
       231-GROUPMEI-SYORI-SEC                   SECTION.
      *
      *    内服・頓服・外用
           IF      ORCHC62-SRYKBN(IDX-ZAI)         =   "21"
                                                  OR   "22"
                                                  OR   "23"
                                                  OR   "14"
               MOVE    1               TO  FLG-GROUP
           END-IF
      *
      *    注射
           IF      ORCHC62-SRYKBN(IDX-ZAI)         =   "31"
                                                  OR   "32"
                                                  OR   "33"
               IF  FLG-SRYKBN3         =   ZERO
                   PERFORM 250-PAGE-CHK-SEC
                   MOVE    "●注射・点滴"
                                           TO  G016-NAIYO11(CNT-LINE)
                   MOVE    1               TO  FLG-GROUP
                   MOVE    1               TO  FLG-SRYKBN3
               END-IF
               IF      IDX-SRY         =   1
                   COMPUTE CNT-ZAI2    =   CNT-ZAI2    +   1
               END-IF
           END-IF
      *
      *    検査
           IF      ORCHC62-SRYKBN(IDX-ZAI)         =   "60"
               IF  FLG-SRYKBN6         =   ZERO
                   PERFORM 250-PAGE-CHK-SEC
                   MOVE    "●検査"
                                           TO  G016-NAIYO11(CNT-LINE)
                   MOVE    1               TO  FLG-GROUP
                   MOVE    1               TO  FLG-SRYKBN6
               END-IF
               IF      IDX-SRY         =   1
                   COMPUTE CNT-ZAI2    =   CNT-ZAI2    +   1
               END-IF
           END-IF
      *
      *    画像
           IF      ORCHC62-SRYKBN(IDX-ZAI)         =   "70"
               IF  FLG-SRYKBN7         =   ZERO
                   PERFORM 250-PAGE-CHK-SEC
                   MOVE    "●画像"
                                           TO  G016-NAIYO11(CNT-LINE)
                   MOVE    1               TO  FLG-GROUP
                   MOVE    1               TO  FLG-SRYKBN7
               END-IF
               IF      IDX-SRY         =   1
                   COMPUTE CNT-ZAI2    =   CNT-ZAI2    +   1
               END-IF
           END-IF
      *
      *
           .
       231-GROUPMEI-SYORI-EXT.
           EXIT.
      *
      *--- 2016/09/06 START ------------------------------------------
      *****************************************************************
      *    診療行為グループ名称２
      *****************************************************************
       231-GROUPMEI-SYORI2-SEC                   SECTION.
      *
           MOVE    SPACE                   TO  WRK-GROUPNAME
      *
           EVALUATE  ORCHC62-SRYKBN(IDX-ZAI)
               WHEN  "14"
                     MOVE  "●在宅"        TO  WRK-GROUPNAME
               WHEN  "21"
                     MOVE  "●内服"        TO  WRK-GROUPNAME
               WHEN  "22"
                     MOVE  "●頓服"        TO  WRK-GROUPNAME
               WHEN  "23"
                     MOVE  "●外用"        TO  WRK-GROUPNAME
               WHEN  "31"
               WHEN  "32"
               WHEN  "33"
                     MOVE  "●注射・点滴"  TO  WRK-GROUPNAME
               WHEN  "60"
                     MOVE  "●検査"        TO  WRK-GROUPNAME
               WHEN  "70"
                     MOVE  "●画像診断"    TO  WRK-GROUPNAME
           END-EVALUATE
      *
           IF  CNT-ZAI2                =  1
               PERFORM 250-PAGE-CHK-SEC
               MOVE    WRK-GROUPNAME
                                     TO  G016-NAIYO11(CNT-LINE)
               MOVE    1             TO  FLG-GROUP
           END-IF
      *
           .
       231-GROUPMEI-SYORI2-EXT.
           EXIT.
      *--- 2016/09/06 END   ------------------------------------------
      *
      *****************************************************************
      *    診療行為　名称抽出
      *****************************************************************
       231-TENSUMEI-SYORI-SEC                   SECTION.
      *
           IF    ( WRK-SRYCD(1:2)  =   "16"  OR  "17" )
      *
               IF    ( WRK-SRYCD(1:2)  =   "17" )
      *         「〜剤注入」は印字しない
                 IF    ( TNS-SRYSYUKBN   =   "711"  OR   "732" )
                     MOVE    1                  TO  WRK-CHARFLG
                     GO  TO  231-TENSUMEI-SYORI-EXT
                 ELSE
                     MOVE    ZERO               TO  WRK-CHARFLG
                 END-IF
               END-IF
      *
      *        診療行為名に「〜加算」「〜判断料」とある場合は印字しない
               IF  TNS-DATAKBN   =   2
                   MOVE    1                  TO  WRK-CHARFLG
                   GO  TO  231-TENSUMEI-SYORI-EXT
               ELSE
                   MOVE    ZERO               TO  WRK-CHARFLG
               END-IF
      *
               IF  TNS-KNSJISKBN   =   2
                   MOVE    1                  TO  WRK-CHARFLG
                   GO  TO  231-TENSUMEI-SYORI-EXT
               ELSE
                   MOVE    ZERO               TO  WRK-CHARFLG
               END-IF
      *
               MOVE    ZERO                   TO  WRK-CHARCNT
               INSPECT  TNS-NAME    TALLYING
                        WRK-CHARCNT  FOR  ALL  "加算"
               IF  WRK-CHARCNT  >=  1
                   MOVE    1                  TO  WRK-CHARFLG
                   GO  TO  231-TENSUMEI-SYORI-EXT
               ELSE
                   MOVE    ZERO               TO  WRK-CHARFLG
               END-IF
      *
               MOVE    ZERO                   TO  WRK-CHARCNT
               INSPECT  TNS-NAME    TALLYING
                        WRK-CHARCNT  FOR  ALL  "減算"
               IF  WRK-CHARCNT  >=  1
                   MOVE    1                  TO  WRK-CHARFLG
                   GO  TO  231-TENSUMEI-SYORI-EXT
               ELSE
                   MOVE    ZERO               TO  WRK-CHARFLG
               END-IF
      *
               MOVE    ZERO                   TO  WRK-CHARCNT
               INSPECT  TNS-NAME    TALLYING
                        WRK-CHARCNT  FOR  ALL  "判断料"
               IF  WRK-CHARCNT  >=  1
                   MOVE    1                  TO  WRK-CHARFLG
                   GO  TO  231-TENSUMEI-SYORI-EXT
               ELSE
                   MOVE    ZERO               TO  WRK-CHARFLG
               END-IF
      *
               MOVE    ZERO                   TO  WRK-CHARCNT
               INSPECT  TNS-NAME    TALLYING
                        WRK-CHARCNT  FOR  ALL  "検査料"
               IF  WRK-CHARCNT  >=  1
                   MOVE    1                  TO  WRK-CHARFLG
                   GO  TO  231-TENSUMEI-SYORI-EXT
               ELSE
                   MOVE    ZERO               TO  WRK-CHARFLG
               END-IF
      *
               MOVE    ZERO                   TO  WRK-CHARCNT
               INSPECT  TNS-NAME    TALLYING
                        WRK-CHARCNT  FOR  ALL  "実施料"
               IF  WRK-CHARCNT  >=  1
                   MOVE    1                  TO  WRK-CHARFLG
                   GO  TO  231-TENSUMEI-SYORI-EXT
               ELSE
                   MOVE    ZERO               TO  WRK-CHARFLG
               END-IF
      *
               MOVE    ZERO                   TO  WRK-CHARCNT
               INSPECT  TNS-NAME    TALLYING
                        WRK-CHARCNT  FOR  ALL  "診断料"
               IF  WRK-CHARCNT  >=  1
                   MOVE    1                  TO  WRK-CHARFLG
                   GO  TO  231-TENSUMEI-SYORI-EXT
               ELSE
                   MOVE    ZERO               TO  WRK-CHARFLG
               END-IF
      *
               MOVE    ZERO                   TO  WRK-CHARCNT
               INSPECT  TNS-NAME    TALLYING
                        WRK-CHARCNT  FOR  ALL  "手技料"
               IF  WRK-CHARCNT  >=  1
                   MOVE    1                  TO  WRK-CHARFLG
                   GO  TO  231-TENSUMEI-SYORI-EXT
               ELSE
                   MOVE    ZERO               TO  WRK-CHARFLG
               END-IF
      *
           END-IF
      *
           .
       231-TENSUMEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    用法編集処理
      *****************************************************************
       240-YOUHOU-HENSYU-SEC       SECTION.
      *
      *    コメント名称判定
           IF      WRK-INPUTCOMENT(CNT-SRY)    =   SPACE
               STRING  "【"            DELIMITED   BY  SIZE
                       TNS-NAME        DELIMITED   BY  SPACE
                       "】"            DELIMITED   BY  SIZE
                       INTO            WRK-YOUHOU
               END-STRING
           ELSE
               STRING  "【"            DELIMITED   BY  SIZE
                       WRK-INPUTCOMENT(CNT-SRY)
                                       DELIMITED   BY  SPACE
                       "】"            DELIMITED   BY  SIZE
                       INTO            WRK-YOUHOU
               END-STRING
           END-IF
      *
           STRING  "　"            DELIMITED   BY  SIZE
                   WRK-YOUHOU      DELIMITED   BY  SIZE
                   INTO            WRK-HENSYUDATA
           END-STRING
      *
           PERFORM 250-HENSYU-SYORI-SEC
      *
           .
       240-YOUHOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    名称編集処理
      *****************************************************************
       240-YKZNAME-HENSYU-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-YKZNAME
      *
      *    薬剤編集
           IF      TNS-SRYCD(1:1)  =   "6"
      *
      *        点数マスタなしは、一般名記載なし
               IF      FLG-TENSU           =   1
                   MOVE    ZERO                TO  WRK-IPNMEI
               END-IF
      *
               IF      TNSP-IPN-KISAIKBN   =   1
                                           OR  3
      *            一般名記載なし
                   IF     (WRK-IPNMEI          =   1 )
                     OR   (WRK-HEN-FUKA    NOT =   SPACE)
                       MOVE    ZERO            TO  TNSP-IPN-KISAIKBN
                   END-IF
               ELSE
      *           一般名記載あり
                   IF      WRK-IPNMEI          =   2
                       MOVE    1               TO  TNSP-IPN-KISAIKBN
                   END-IF
               END-IF
      *
      *        院内は一般名記載なしとする
      *         IF     (ORCSCH62-KBN        =   2  )  AND
      *                (TNSP-IPN-KISAIKBN   =   1
      *                                     OR  3  )
      *             MOVE    ZERO                TO  TNSP-IPN-KISAIKBN
      *         END-IF
      *
      *        一般名・処方名称記載
               MOVE    TNS-NAME            TO  WRK-TNS-NAME-G
               IF      TNSP-IPN-KISAIKBN   =   ZERO
                   MOVE    WRK-TNS-NAME-G      TO  WRK-YKZNAME
               ELSE
                   PERFORM 2401-GENERIC-HEN-SEC
                   MOVE    WRK-TNS-NAME-G      TO  WRK-YKZNAME
               END-IF
      *        ＧＥ付き表示の時、表示
               MOVE    WRK-TNS-NAME-G      TO  WRK-YKZNAME
               IF      SYS-1030-SYOHOGEPRTFLG  =   1
                   IF     (TNS-SYUSAISKB       =   8   )  AND
                          (TNS-SYOMEIKANREN    NOT =   SPACE AND
                                                       ZERO )
                       COMPUTE KETA            =  (TNS-YUKOKETA  * 2)
                                               +   1
                       MOVE    "（ＧＥ）"      TO  WRK-YKZNAME(KETA:)
                   END-IF
               END-IF
      *        後発医薬品の時、表示（優先）
               IF      SYS-1030-SYOHODSPNAME   NOT =   SPACE
      *            後発医薬品区分
                   IF     (TNS-KOUHATUKBN          =   1
      *--- 2017/05/19 START ------------------------------------------
                                                   OR  3
      *--- 2017/05/19 END   ------------------------------------------
                                                   OR  7    )
      *              一般名記載以外
                     AND  (TNSP-IPN-KISAIKBN       =   ZERO
                                                   OR  2     )
                       MOVE    WRK-TNS-NAME-G      TO  WRK-YKZNAME
                       COMPUTE KETA            =  (TNS-YUKOKETA  * 2)
                                               +   1
                       MOVE    SYS-1030-SYOHODSPNAME
                                               TO  WRK-YKZNAME(KETA:)
                   END-IF
               END-IF
      *        内用薬の総量編集の判定
               IF      TNSP-SOURYOHENKBN   =   1
                   MOVE    1               TO  FLG-SOURYO
               END-IF
           ELSE
      *        コメントあればセット、なければ名称セット
               IF      WRK-INPUTCOMENT(CNT-SRY)    =   SPACE
                   MOVE    TNS-NAME        TO  WRK-YKZNAME
               ELSE
                   MOVE    WRK-INPUTCOMENT(CNT-SRY)
                                           TO  WRK-YKZNAME
               END-IF
           END-IF
      *
           MOVE    WRK-YKZNAME     TO  WRK-HENSYUDATA
           PERFORM 250-HENSYU-SYORI-SEC
      *
           .
       240-YKZNAME-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *     一般名・処方名称記載処理
      *****************************************************************
       2401-GENERIC-HEN-SEC           SECTION.
      *
           EVALUATE    TNSP-IPN-KISAIKBN
      *        一般名記載
               WHEN    1
                   MOVE    SPACE               TO  WRK-TNS-NAME-G
                   STRING  "【般】"                DELIMITED  BY  SIZE
                           GENERIC-GENERICNAME     DELIMITED  BY  SPACE
                                               INTO    WRK-TNS-NAME-G
                   END-STRING
                   COMPUTE TNS-YUKOKETA        =   GENERIC-YUKOKETA
                                               +   3
                   IF      GENERIC-TANINAME    NOT =   SPACE
                       MOVE    GENERIC-TANICD      TO  TNS-TANICD
                       MOVE    GENERIC-TANINAME    TO  TNS-TANINAME
                   END-IF
      *        処方名称記載
               WHEN    2
                   MOVE    TNSP-SHONAME        TO  WRK-TNS-NAME-G
                   MOVE    TNSP-SHOYUKOKETA    TO  TNS-YUKOKETA
      *
      *        処方名称を一般名称とする
               WHEN    3
                   MOVE    SPACE               TO  WRK-TNS-NAME-G
                   STRING  "【般】"                DELIMITED  BY  SIZE
                           TNSP-SHONAME            DELIMITED  BY  SPACE
                                               INTO    WRK-TNS-NAME-G
                   END-STRING
                   COMPUTE TNS-YUKOKETA        =   TNSP-SHOYUKOKETA
                                               +   3
           END-EVALUATE
      *
           .
       2401-GENERIC-HEN-EXT.
           EXIT.
      *****************************************************************
      *    数量編集処理
      *****************************************************************
       240-SURYOU-HENSYU-SEC       SECTION.
      *
      *    薬剤、その他材料、用法以外は処理抜ける
      *     IF    ( WRK-SRYCD(1:1)  NOT =   "6"   ) AND
      *           ( WRK-SRYCD(1:3)  NOT =   "059" ) AND
      *           ( WRK-SRYCD(1:3)  NOT =   "001" )
      *         GO  TO  240-SURYOU-HENSYU-EXT
      *     END-IF
      *
      *    薬剤、器材のみ
           IF    ( WRK-SRYCD(1:1)      =   "6"   )  OR
                 ( WRK-SRYCD(1:1)      =   "7"   )  OR
                 ( WRK-SRYCD(1:3)      =   "059" )
               CONTINUE
           ELSE
               GO  TO  240-SURYOU-HENSYU-EXT
           END-IF
      *
           MOVE    SPACE           TO  WRK-HENNAIYO
      *
           MOVE    WRK-SRYSURYO(CNT-SRY)   TO  WRK-SURYO
           IF      WRK-SURYO       >   0
               PERFORM 250-ZHEN-SURYO-SET-SEC
               MOVE    WRK-HENSURYO        TO  WRK-SURYOU
           END-IF
      *
           STRING  " "             DELIMITED  BY  SIZE
                   WRK-SURYOU      DELIMITED  BY  SPACE
                   " "             DELIMITED  BY  SIZE
                   TNS-TANINAME    DELIMITED  BY  SPACE
                   INTO            WRK-HENNAIYO
           END-STRING
      *
           PERFORM 250-MHEN-SYORI-SET-SEC
      *
           .
       240-SURYOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    回数編集処理
      *****************************************************************
       240-KAISUU-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE           TO  WRK-HENNAIYO
      *
      *    内服で総量表示の薬剤のみの場合、外用と同様の表示とする
           IF    ( WRK-SRYKBN      =   "21" )  AND
                 ( FLG-SOURYO      =   1    )
               MOVE    "23"            TO  WRK-SRYKBN
           END-IF
      *
      *    内服か頓服でなければ処理抜ける
           IF    ( WRK-SRYKBN  NOT =   "21" )  AND
                 ( WRK-SRYKBN  NOT =   "22" )
               GO  TO  240-KAISUU-HENSYU-EXT
           END-IF
      *
           MOVE    WRK-ZAIKAIKEI   TO  WRK-SURYO
           IF      WRK-SURYO       >   0
               PERFORM 250-ZHEN-SURYO-SET-SEC
               MOVE    WRK-HENSURYO    TO  WRK-KAISUU
           END-IF
      *
           EVALUATE    WRK-SRYKBN
      *        内服
               WHEN    "21"
                   STRING  " "             DELIMITED  BY  SIZE
                           WRK-KAISUU      DELIMITED  BY  SPACE
                           " "             DELIMITED  BY  SIZE
                           "日分"          DELIMITED  BY  SPACE
                           INTO            WRK-HENNAIYO
                   END-STRING
      *        頓服
               WHEN    "22"
                   STRING  " "             DELIMITED  BY  SIZE
                           WRK-KAISUU      DELIMITED  BY  SPACE
                           " "             DELIMITED  BY  SIZE
                           "回分"          DELIMITED  BY  SPACE
                           INTO            WRK-HENNAIYO
                   END-STRING
           END-EVALUATE
      *
           PERFORM 250-MHEN-SYORI-SET-SEC
      *
           .
       240-KAISUU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁チェック　処理
      *****************************************************************
       250-PAGE-CHK-SEC            SECTION.
      *
      *    ページ件数を超えていたら印刷
      *    1ページの最終行(34行目)になると印刷処理を行う。
           IF      CNT-LINE    =   34
               PERFORM 400-PRT-WRITE-SEC
               INITIALIZE                  G016
               MOVE    ZERO            TO  CNT-LINE
               COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
           END-IF
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           .
       250-PAGE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票Ｚ編集処理
      *****************************************************************
       250-ZHEN-SURYO-SET-SEC      SECTION.
      *
      *    数量Ｚ編集
           MOVE    SPACE           TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1    TO  WRK-SURYO-Z1
      *
      *    数量小数点以下編集
           MOVE    ZERO            TO  FLG-SP
           PERFORM VARYING IDX-STR     FROM    5   BY  -1
                   UNTIL   IDX-STR     <   1
               IF      WRK-SURYO-S2(IDX-STR:1) NOT =   ZERO
                   MOVE    1           TO  FLG-SP
               END-IF
               IF      FLG-SP      =   1
                   MOVE    WRK-SURYO-S2(IDX-STR:1)
                                        TO  WRK-SURYO-Z3(IDX-STR:1)
               END-IF
           END-PERFORM
      *
           IF      WRK-SURYO-Z3    NOT =   SPACE
               MOVE    "."             TO  WRK-SURYO-Z2
               IF  WRK-SURYO-X(1:5)    =   SPACE
                   MOVE    WRK-SURYO-S1    TO  WRK-SURYO-Z1-9
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  WRK-HENSURYO
           MOVE    ZERO            TO  IDX-STR1
           PERFORM VARYING IDX-STR     FROM    1   BY  1
                   UNTIL   IDX-STR     >   11
               IF      WRK-SURYO-X(IDX-STR:1)  NOT =   SPACE
                   COMPUTE IDX-STR1    =   IDX-STR1    +   1
                   MOVE  WRK-SURYO-X(IDX-STR:1)
                                           TO  WRK-HENSURYO(IDX-STR1:1)
               END-IF
           END-PERFORM
      *
           .
       250-ZHEN-SURYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票文字編集処理
      *****************************************************************
       250-MHEN-SYORI-SET-SEC      SECTION.
      *
      *    初期値セット
           MOVE    1              TO  IDX-STR
           MOVE    0              TO  IDX-STR1
      *
      *    最大文字を超えるか編集データがなくなるまで繰り返す
           PERFORM UNTIL  (IDX-STR     >   23 )
                   OR     (WRK-HENNAIYO(IDX-STR:)    =   SPACE)
               ADD     1               TO  IDX-STR
               ADD     1               TO  IDX-STR1
           END-PERFORM
      *
           COMPUTE IDX-STR1     =   IDX-STR1  +  3
           COMPUTE IDX-STR     =   44  -   IDX-STR1
      *
           IF  ( G016-NAIYO2(CNT-LINE)           NOT =  SPACE )
           OR  ( G016-NAIYO12(CNT-LINE)(IDX-STR:) NOT =  SPACE )
               PERFORM 250-PAGE-CHK-SEC
               MOVE    WRK-HENNAIYO    TO  G016-NAIYO2(CNT-LINE)
           ELSE
               MOVE    WRK-HENNAIYO    TO  G016-NAIYO2(CNT-LINE)
           END-IF
      *
           .
       250-MHEN-SYORI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票文字編集処理
      *****************************************************************
       250-HENSYU-SYORI-SEC        SECTION.
      *
      *    初期値セット
           MOVE    1              TO  IDX-STR
           MOVE    1              TO  IDX-STR1
      *
      *    編集データがなくなるまで繰り返す
           PERFORM UNTIL  (IDX-STR     >   4000 )
                   OR     (WRK-HENSYUDATA(IDX-STR:)    =   SPACE)
      *        全角セット
               MOVE    WRK-HENSYUDATA(IDX-STR:2)
                                       TO  WRK-HENLINE2(IDX-STR1:2)
               ADD     2               TO  IDX-STR
               ADD     2               TO  IDX-STR1
      *
      *        最大字で改行
               IF         (IDX-STR1        >=  44)
                   OR    ((IDX-STR         <=  4000)
                   AND    (WRK-HENSYUDATA(IDX-STR:) =   SPACE))
                   PERFORM 250-PAGE-CHK-SEC
                 MOVE    WRK-HENLINE1  TO  G016-NAIYO11(CNT-LINE)
                 MOVE    WRK-HENLINE2  TO  G016-NAIYO12(CNT-LINE)
                 MOVE    SPACE         TO  WRK-HENLINE1
                                           WRK-HENLINE2
                 MOVE    1             TO  IDX-STR1
               END-IF
           END-PERFORM
      *
           .
       250-HENSYU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       400-PRT-WRITE-SEC           SECTION.
      *
           ADD     1                   TO  IDX-PRINT
      *
      *    印刷用ファイル作成
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    "HC62"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
           MOVE    CNT-HC62            TO  HC01PARA-CNT(1:3)
           MOVE    CNT-PAGE            TO  HC01PARA-CNT(4:3)
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"           USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
           OPEN    OUTPUT                  PRT-FILE
           MOVE    SPACE               TO  PRT-REC
      *
      *    印刷用データ出力
           MOVE    G016                TO  PRT-PRTDATA
           WRITE                           PRT-REC
      *
           CLOSE                           PRT-FILE
      *
      *    帳票印刷処理
           INITIALIZE                      ORCSMKPRTAREA
           MOVE    "HC01.sh"           TO  MKPRT-ID
           MOVE    "A00000RA02.red"    TO  MKPRT-DIA
           MOVE    SPACE               TO  MKPRT-DEF
           MOVE    ASGNPARA            TO  MKPRT-DAT
           MOVE    WRK-PARA-PRINTNAME  TO  MKPRT-PRTNM
           MOVE    "処方せん（控え）"  TO  MKPRT-INFO
           CALL    "ORCSMKPRTSITE1"        USING
                                           ORCSMKPRTAREA
                                           SPA-AREA
      *
           .
       400-PRT-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＦＯＯＴセット処理
      *****************************************************************
       430-FOOT-SET-SEC            SECTION.
      *
           PERFORM 250-PAGE-CHK-SEC
      *
           INITIALIZE                       PTINF-REC
           MOVE    WRK-PARA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    WRK-PARA-PTID        TO  PTINF-PTID
           MOVE    PTINF-REC            TO  MCPDATA-REC
           PERFORM 900-PTINF-READ-SEC
      *
      *    患者番号
           MOVE    WRK-PARA-PTNUM       TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT        TO  WRK-HENKAN-PTNUM
           MOVE    WRK-HENKAN-PTNUM     TO  G016-PTNUM-NAME(CNT-LINE)
      *
      *    患者名
      *--- 2016/08/26 START ------------------------------------------
           PERFORM 250-PAGE-CHK-SEC
      *--- 2016/08/26 END   ------------------------------------------
           MOVE    SPACE                TO  WRK-PTNAME
           IF  PTINF-NAME  NOT =  SPACE
             STRING  PTINF-NAME         DELIMITED  BY  SPACE
                     "様"               DELIMITED  BY  SIZE
                     INTO               WRK-PTNAME
             END-STRING
           ELSE
             STRING  PTINF-KANANAME     DELIMITED  BY  SPACE
                     "様"               DELIMITED  BY  SIZE
                     INTO               WRK-PTNAME
             END-STRING
           END-IF
           INSPECT    WRK-PTNAME        REPLACING ALL "  " BY "　"
      *--- 2016/08/26 START ------------------------------------------
      *     COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *--- 2016/08/26 END   ------------------------------------------
           MOVE    WRK-PTNAME           TO  G016-PTNUM-NAME(CNT-LINE)
      *
      *    診療日
      *--- 2016/08/26 START ------------------------------------------
           PERFORM 250-PAGE-CHK-SEC
      *--- 2016/08/26 END   ------------------------------------------
           MOVE    ORCHC62-SRYYMD       TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           INSPECT    LNK-DAY2-EDTYMD3  REPLACING ALL "  " BY "　"
      *--- 2016/08/26 START ------------------------------------------
      *     COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *--- 2016/08/26 END   ------------------------------------------
      *--- 2016/09/06 START ------------------------------------------
      *     STRING  "診療日"             DELIMITED  BY  SPACE
           STRING  "診療日　"           DELIMITED  BY  SIZE
      *--- 2016/09/06 END   ------------------------------------------
                   LNK-DAY2-EDTYMD3     DELIMITED  BY  SIZE
                   INTO                 G016-PTNUM-NAME(CNT-LINE)
           END-STRING
      *
           .
       430-FOOT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    右づめ編集処理
      *****************************************************************
       435-RIGHT-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE                  TO  WRK-HENSYU
           MOVE    5                      TO  IDZ
      *
           PERFORM  VARYING  IDY  FROM  20  BY  -1
                    UNTIL    IDY  <     1
             IF  WRK-HENKAN(IDY:1)  NOT =  SPACE
               MOVE  WRK-HENKAN(IDY:1)   TO  WRK-HENSYU(IDZ:1)
               COMPUTE  IDZ  =  IDZ  -  1
      *
               IF  IDZ  =  0
                 MOVE    1               TO  IDY
               END-IF
             END-IF
           END-PERFORM
      *
           .
       435-RIGHT-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       2009-HENKAN-SEC                 SECTION.
      *
           INITIALIZE                  WRK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT
                                           TO  WRK-OUT-INPUT
           ELSE
               MOVE    WRK-MAE-INPUT       TO  WRK-OUT-INPUT
           END-IF
      *
           .
       2009-HENKAN-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      (SPA-CLIENT-PRT-FLG =   1  )
      *        印刷ダミー処理
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為  情報読込
      *****************************************************************
       900-WKSRYACT-SELECT-SEC          SECTION.
      *
           INITIALIZE                 WKSRYACT-REC
           MOVE    SPA-HOSPNUM        TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN      TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID           TO  WKSRY-PTID
           MOVE    SPA-SRYKA          TO  WKSRY-SRYKA
           MOVE    SPA-SRYYMD         TO  WKSRY-SRYYMD
           MOVE    SPA-HKNCOMBINUM    TO  WKSRY-HKNCOMBI
      *
           MOVE    WKSRYACT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"         TO  MCP-FUNC
           MOVE    "tbl_wksryact"     TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             PERFORM  900-WKSRYACT-FETCH-SEC
           ELSE
             MOVE     1                 TO  FLG-WKSRYACT
           END-IF
      *
           .
       900-WKSRYACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為  情報検索
      *****************************************************************
       900-WKSRYACT-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"          TO  MCP-FUNC
           MOVE    "tbl_wksryact"     TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    ZERO               TO  FLG-WKSRYACT
             MOVE    MCPDATA-REC        TO  WKSRYACT-REC
      *
           ELSE
             MOVE    1                  TO  FLG-WKSRYACT
           END-IF
      *
           .
       900-WKSRYACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為  クローズ処理
      *****************************************************************
       900-WKSRYACT-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"    TO  MCP-FUNC
           MOVE    "tbl_wksryact"     TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       900-WKSRYACT-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTINF-REC
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読み込み
      *****************************************************************
       900-TENSU-READ-SEC          SECTION.
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-SRYCD       TO  TNS-SRYCD
           MOVE    ORCHC62-SRYYMD  TO  TNS-YUKOSTYMD
           MOVE    ORCHC62-SRYYMD  TO  TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-TENSU
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               ELSE
                   MOVE    1               TO  FLG-TENSU
               END-IF
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
      *    点数附加情報検索
           IF    ( TNS-SRYCD(1:1)      =   "6"
                                       OR  "7"  )  AND
                 ( FLG-TENSU           =   ZERO )
               PERFORM 900-TNSP-READ-SEC
           ELSE
               INITIALIZE                      TNSP-REC
           END-IF
      *
           IF     (TNS-SRYCD(1:1)      =   "6" ) AND
                  (FLG-TENSU           =   ZERO)
      *        システム管理の変更可によって一般名記載とする
               IF     (TNSP-IPN-KISAIKBN       =   ZERO    )
                  AND (SYS-1030-KOUHATUKA      =   "1"     )
                  AND (TNS-KOUHATUKBN          =   1  OR  2
      *--- 2017/05/19 START ------------------------------------------
                                               OR  3
                                               OR  4
      *--- 2017/05/19 END   ------------------------------------------
                                               OR  7       )
                   MOVE    1               TO  TNSP-IPN-KISAIKBN
               END-IF
      *
      *        一般名記載あり
               IF     (TNSP-IPN-KISAIKBN   =   1  )
                 OR   (WRK-IPNMEI          =   2  )
                   IF      TNSP-IPN-KISAIKBN   =   3
      *                処方名称を一般名称
                       CONTINUE
                   ELSE
                       PERFORM 900-GENERICNAME-READ-SEC
      *                一般名マスタなし
                       IF      FLG-GENERICNAME     =   1
                           MOVE    ZERO            TO  TNSP-IPN-KISAIKBN
                           MOVE    ZERO            TO  WRK-IPNMEI
                       END-IF
                   END-IF
               END-IF
      *
           ELSE
      *        一般名マスタなし
               INITIALIZE                      GENERICNAME-REC
           END-IF
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数付加情報読み込み
      *****************************************************************
       900-TNSP-READ-SEC           SECTION.
      *
           INITIALIZE                  TNSP-REC
           MOVE    WRK-SRYCD       TO  TNSP-SRYCD
           MOVE    ORCHC62-SRYYMD  TO  TNSP-YUKOSTYMD
           MOVE    ORCHC62-SRYYMD  TO  TNSP-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNSP-HOSPNUM
           MOVE    TNSP-REC        TO  MCPDATA-REC
           MOVE    "tbl_tensuplus" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_tensuplus" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-TNSP
                   MOVE    MCPDATA-REC     TO  TNSP-REC
               ELSE
                   MOVE    1               TO  FLG-TNSP
               END-IF
           ELSE
               MOVE    1               TO  FLG-TNSP
           END-IF
      *
           MOVE    "tbl_tensuplus" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
      *R02.2
      *    ユーザー単位編集
           IF     (TNS-SRYCD(1:1)      =   "6"
                                       OR  "7" )
              AND (FLG-TNSP            =   ZERO  )
              AND (TNSP-USERTANICD NOT =   SPACE )
                MOVE    TNSP-USERTANICD    TO  TNS-TANICD
                MOVE    TNSP-USERTANINAME  TO  TNS-TANINAME
           END-IF
      *
           .
       900-TNSP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ付加情報検索処理
      *****************************************************************
       900-GENERICNAME-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  GENERICNAME-REC
           INITIALIZE                      GENERICNAME-REC
           MOVE    TNS-YAKKAKJNCD      TO  GENERIC-YAKKAKJNCD
      *
           MOVE    GENERICNAME-REC     TO  MCPDATA-REC
           MOVE    "tbl_genericname"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_genericname"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  GENERICNAME-REC
                   MOVE    ZERO                TO  FLG-GENERICNAME
               ELSE
                   INITIALIZE                  GENERICNAME-REC
                   MOVE    1                   TO  FLG-GENERICNAME
               END-IF
           ELSE
               INITIALIZE                      GENERICNAME-REC
               MOVE    1                   TO  FLG-GENERICNAME
           END-IF
           MOVE    "tbl_genericname"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
           .
       900-GENERICNAME-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       900-SRYACT-READ-SEC         SECTION.
      *
           MOVE    "tbl_sryact"            TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為マスター読込
      *****************************************************************
       900-WKSRYACT-READ-SEC         SECTION.
      *
           MOVE    "tbl_wksryact"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  WKSRYACT-REC
               MOVE    ZERO                TO  FLG-WKSRYACT
           ELSE
               INITIALIZE                      WKSRYACT-REC
               MOVE    1                   TO  FLG-WKSRYACT
           END-IF
      *
           .
       900-WKSRYACT-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報−基本情報読み込み
      *****************************************************************
       900-1001-READ-SEC           SECTION.
      *
      *    医療機関情報−基本情報読み込み
           MOVE    SPACE           TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1001-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
           ELSE
               INITIALIZE                  SYS-1001-REC
           END-IF
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
      *
           .
       900-1001-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報−所在地、連絡先読み込み
      *****************************************************************
       900-1002-READ-SEC           SECTION.
      *
      *    医療機関情報−所在地、連絡先読み込み
           MOVE    SPACE           TO  SYS-1002-REC
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"          TO  SYS-1002-KANRICD
           MOVE    "*"             TO  SYS-1002-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1002-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1002-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1002-REC
           ELSE
               INITIALIZE                  SYS-1002-REC
           END-IF
      *
           MOVE    SYS-1002-TEL    TO  WRK-HOSPTEL
      *
           .
       900-1002-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    職員情報読み込み
      *****************************************************************
       900-1010-READ-SEC           SECTION.
      *
      *    職員情報読み込み
           MOVE    SPACE           TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"          TO  SYS-1010-KANRICD
           MOVE    ORCHC62-DRCD    TO  SYS-1010-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1010-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1010-REC
           ELSE
               INITIALIZE                  SYS-1010-REC
           END-IF
      *
           MOVE    SYS-1010-NAME   TO  WRK-DRNAME
      *
           .
       900-1010-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集区分情報読み込み
      *****************************************************************
       900-1030-READ-SEC           SECTION.
      *
      *    処方せんＧＥ出力情報編集
           INITIALIZE                      SYS-1030-REC
           MOVE    "1030"              TO  SYS-1030-KANRICD
           MOVE    "*"                 TO  SYS-1030-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1030-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1030-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1030-HOSPNUM
           MOVE    SYS-1030-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1030-REC
           ELSE
               INITIALIZE                  SYS-1030-REC
           END-IF
      *
           .
       900-10301-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力先プリンタ名割り当て情報読み込み
      *****************************************************************
       900-1031-READ-SEC           SECTION.
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                  SYS-1031-REC
           INITIALIZE                  ORCSPRTNMAREA
           MOVE    "2"             TO  ORCSPRTNM-KBN
           MOVE    ORCHC62-SRYYMD  TO  ORCSPRTNM-SRYYMD
           MOVE    "2"             TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1031-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               INITIALIZE                  SYS-1031-REC
           END-IF
      *
           .
       900-1031-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関編集情報読み込み
      *****************************************************************
       900-1900-READ-SEC           SECTION.
      *
      *    医療機関編集情報読み込み
           MOVE    SPACE           TO  SYS-1900-REC
           INITIALIZE                  SYS-1900-REC
           MOVE    "1900"          TO  SYS-1900-KANRICD
           MOVE    "*"             TO  SYS-1900-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1900-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1900-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(18) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
               END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
      *
           .
       900-1900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報読み込み
      *****************************************************************
       900-1901-READ-SEC           SECTION.
      *
      *    医療機関名称編集情報読み込み
           MOVE    SPACE           TO  SYS-1901-REC
           INITIALIZE                  SYS-1901-REC
           MOVE    "1901"          TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(18)
                                   TO  SYS-1901-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1901-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1
                                       TO  WRK-HOSPNAME
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
      *
           .
       900-1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1902-READ-SEC           SECTION.
      *
      *    医療機関住所編集情報読み込み
           MOVE    SPACE           TO  SYS-1902-REC
           INITIALIZE                  SYS-1902-REC
           MOVE    "1902"          TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(18)
                                   TO  SYS-1902-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1902-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1902-REC
               MOVE    SYS-1902-TEL    TO  WRK-HOSPTEL
           ELSE
               INITIALIZE                  SYS-1902-REC
           END-IF
      *
           .
       900-1902-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読込開始処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読込検索処理
      *****************************************************************
       920-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読込終了処理
      *****************************************************************
       930-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       930-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       900-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       900-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
