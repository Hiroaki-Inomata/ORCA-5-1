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
       PROGRAM-ID.             A00000RA03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : お薬手帳印刷
      *  コンポーネント名  : お薬手帳印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *****************************************************************
      *  作成日付    作業者    記述
      *  16/08/08    萩野      薬袋新規
      *  16/09/26    萩野      院内分、院外分の印字をプログラムオプションで
      *                        選択できるようにする
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
      *    SORT用ファイル
           SELECT  SORT-FILE         ASSIGN   SORTPARA
                                     ORGANIZATION    IS  INDEXED
                                     ACCESS  MODE    IS  DYNAMIC
                                     RECORD  KEY     IS  SORT-KEY
                                     FILE    STATUS  IS  STS-SORT.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
       01  PRT-REC.
           03  PRT-PRTDATA         PIC X(20000).
      *
      *    SORT用ファイル
       FD  SORT-FILE.
       01  SORT-REC. 
           COPY  "A00000RA03SORT.INC".
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
           COPY    "CPASGNPARA.INC".
      *
      *    帳票領域
           COPY    "A00000RA03.INC" REPLACING    //A00000RA03// 
                                    BY           //RA03//.
      *
       01  SORTPARA.
           03  FILLER                PIC X(05) VALUE "/tmp/".
           03  SORTPARA-FILENAME     PIC X(13) VALUE "A00000RA03-IN".
           03  FILLER                PIC X(04)  VALUE ".TMP".
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-SORT              PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TNSP            PIC 9(01).
     *
           03  FLG-SP              PIC 9(01).
           03  FLG-SOURYO          PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-NAIYO           PIC 9(01).
      *
           03  FLG-INVALID           PIC 9(01).
           03  FLG-PRINT             PIC 9(01).
           03  FLG-SYUNOU            PIC 9(01).
      *
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-DRCD            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(03).
           03  CNT-GAI-LINE        PIC 9(02).
           03  CNT-NAI-LINE        PIC 9(02).
           03  CNT-TON-LINE        PIC 9(02).
           03  CNT-TAKU-LINE       PIC 9(02).
           03  CNT-TYU-LINE        PIC 9(02).
           03  CNT-LINE            PIC 9(02).
           03  CNT-LINE1           PIC 9(02).
           03  CNT-LINE2           PIC 9(02).
           03  CNT-LINE3           PIC 9(02).
           03  CNT-LINE4           PIC 9(02).
           03  CNT-LINE5           PIC 9(02).
           03  CNT-LINE6           PIC 9(02).
           03  CNT-ZAI.
               05  CNT-NAI-ZAI     PIC 9(03).
               05  CNT-GAI-ZAI     PIC 9(03).
               05  CNT-TON-ZAI     PIC 9(03).
               05  CNT-TAKU-ZAI    PIC 9(03).
               05  CNT-TYU-ZAI     PIC 9(03).
           03  CNT-SRY             PIC 9(02).
           03  CNT-PER             PIC 9(02).
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDX-STR             PIC 9(04).
           03  IDX-STR1            PIC 9(04).
      *
           03  IDX-ST              PIC 9(02).
           03  IDX-PRINT           PIC 9(02).
      *
           03  IDX-ZAI             PIC 9(03).
           03  IDX-SRY             PIC 9(02).
           03  IDX-PARA            PIC 9(02).
      *
           03  IDX1                  PIC 9(02).
           03  IDX2                  PIC 9(02).
           03  IDX3                  PIC 9(02).
           03  IDX5                  PIC 9(02).
      *
           03  IDX                   PIC 9(02).
           03  IDX-YOU               PIC 9(02).
      *
           03  IDX-NAI               PIC 9(02).
           03  IDX-TON               PIC 9(02).
           03  IDX-GAI               PIC 9(02).
           03  IDX-TAKU              PIC 9(02).
           03  IDX-TYU               PIC 9(02).
      *
           03  IDX-NAI-MAX           PIC 9(02).
           03  IDX-TON-MAX           PIC 9(02).
           03  IDX-GAI-MAX           PIC 9(02).
           03  IDX-TAKU-MAX          PIC 9(02).
           03  IDX-TYU-MAX           PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MSG               PIC X(800).
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
           03  WRK-SRYSURYO        PIC 9(05).
      *
           03  WRK-ZZ9             PIC ZZ9.
           03  WRK-Z9              PIC Z9.
      *
           03  WRK-SRYCD           PIC X(09).
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-HOSPNAME        PIC X(22).
           03  WRK-PTNAME          PIC X(64).
      *
           03  WRK-HEN-AREA.
               05  WRK-HENLINE     PIC X(54).
               05  WRK-HENNAIYO    PIC X(24).
               05  WRK-YKTNAME     PIC X(140).
               05  WRK-YOUHOU      PIC X(140).
               05  WRK-SURYOU      PIC X(11).
               05  WRK-TANI        PIC X(12).
               05  WRK-KAISUU      PIC X(03).
      *
           03  WRK-HENSYUDATA      PIC X(140).
      *
           03  WRK-SYUNOU-SORT       OCCURS 10.
               05  WRK-SYU-SORT-KEY.
                 07  WRK-SYU-UPYMD   PIC X(08).
                 07  WRK-SYU-UPHMS   PIC X(08).
               05  WRK-SYU-PTID      PIC X(10).
               05  WRK-SYU-DENPNUM   PIC X(07).
           03  WRK-SORT-DATA         PIC X(33).
           03  WRK-FUKUYOSU          PIC 9(01).
           03  WRK-AMARI              PIC 9(05)V9(05).
      *
           03  WRK-DRCD            PIC X(05).
           03  WRK-DENPNUM         PIC 9(07).
      *
           03  WRK-YOUCOM-NAME     PIC X(200).
      *
      *    医師コード 一時領域
       01  H016-DR-AREA.
           03  H016-DRCD             PIC X(05).
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
      *    剤内容領域
       01  WRK-SRY-AREA.
           03  WRK-ZAIKAIKEI       PIC 9(03).
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-SRYKBNBACK      PIC X(02).
           03  WRK-SRYSYUKBN       PIC X(03).
           03  WRK-ZAIGATAKBN      PIC X(03).
      *
      *    内服編集領域
       01  WRK-NAIHUKU.
           03  WRK-NAI-NAIYO-TBL.
             05  WRK-NAI-KBN         OCCURS 10.
               07  WRK-NAI-YOHO      PIC X(50)  OCCURS 1000.
               07  WRK-NAI-YOHO-SURYOFLG    PIC 9(01)  OCCURS 1000.
               07  WRK-NAI-NAIYO     PIC X(50)  OCCURS 1000.
               07  WRK-NAI-NAIYO2    PIC X(50)  OCCURS 1000.
               07  WRK-NAI-SURYOFLG  PIC 9(01)  OCCURS 1000.
               07  WRK-CNT-NAI       PIC 9(02).
      *
      **    頓服編集領域
       01  WRK-TONPUKU.
           03  WRK-TON-NAIYO-TBL.
             05  WRK-TON-KBN         OCCURS 10.
               07  WRK-TON-YOHO      PIC X(50)  OCCURS 1000.
               07  WRK-TON-YOHO-SURYOFLG    PIC 9(01)  OCCURS 1000.
               07  WRK-TON-NAIYO     PIC X(50)  OCCURS 1000.
               07  WRK-TON-NAIYO2    PIC X(50)  OCCURS 1000.
               07  WRK-TON-SURYOFLG  PIC 9(01)  OCCURS 1000.
               07  WRK-CNT-TON       PIC 9(02).
      *
      **    外用編集領域
       01  WRK-GAIYOU.
           03  WRK-GAI-NAIYO-TBL.
             05  WRK-GAI-KBN         OCCURS 10.
               07  WRK-GAI-YOHO      PIC X(50)  OCCURS 1000.
               07  WRK-GAI-YOHO-SURYOFLG    PIC 9(01)  OCCURS 1000.
               07  WRK-GAI-NAIYO     PIC X(50)  OCCURS 1000.
               07  WRK-GAI-NAIYO2    PIC X(50)  OCCURS 1000.
               07  WRK-GAI-SURYOFLG  PIC 9(01)  OCCURS 1000.
               07  WRK-CNT-GAI       PIC 9(02).
      *
      *    注射編集領域(在宅-薬剤・材料 )
       01  WRK-ZAITAKU.
           03  WRK-TAKU-NAIYO-TBL.
             05  WRK-TAKU-KBN         OCCURS 10.
               07  WRK-TAKU-YOHO      PIC X(50)  OCCURS 1000.
               07  WRK-TAKU-YOHO-SURYOFLG    PIC 9(01)  OCCURS 1000.
               07  WRK-TAKU-NAIYO     PIC X(50)  OCCURS 1000.
               07  WRK-TAKU-NAIYO2    PIC X(50)  OCCURS 1000.
               07  WRK-TAKU-SURYOFLG  PIC 9(01)  OCCURS 1000.
               07  WRK-CNT-TAKU       PIC 9(02).
      *
      *    注射編集領域(注射)
       01  WRK-TYUSYA.
           03  WRK-TYU-NAIYO-TBL.
             05  WRK-TYU-KBN         OCCURS 10.
               07  WRK-TYU-YOHO      PIC X(50)  OCCURS 1000.
               07  WRK-TYU-YOHO-SURYOFLG    PIC 9(01)  OCCURS 1000.
               07  WRK-TYU-NAIYO     PIC X(50)  OCCURS 1000.
               07  WRK-TYU-NAIYO2    PIC X(50)  OCCURS 1000.
               07  WRK-TYU-SURYOFLG  PIC 9(01)  OCCURS 1000.
               07  WRK-CNT-TYU       PIC 9(02).
      *
      *    帳票編集領域
       01  WRK-NAIYOU.
           03  WRK-NAIYO-TBL.
             05  WRK-NAIYO           PIC X(40)  OCCURS 1000.
             05  WRK-NAIYO2          PIC X(40)  OCCURS 1000.
             05  WRK-SURYOFLG        PIC 9(01)  OCCURS 1000.
      *
       01  WRK-HEN-NAIYOU.
           03  WRK-HEN-NAIYO         PIC X(36)  OCCURS  1000.
           03  WRK-HEN-NAIYO2        PIC X(36)  OCCURS  1000.
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
      *    プログラムオプションパラメタ
       01  PARA-AREA.
           03  PARA-TYUSYA             PIC X(01).
           03  PARA-YOUCOM             PIC X(09).
      *--- 2016/09/26 START ------------------------------------------
           03  PARA-SHOHOU             PIC X(01).
      *--- 2016/09/26 END   ------------------------------------------
      *
           03  WRK-PARA-NAME           PIC X(20).
           03  WRK-PARA                PIC X(100).
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
      *    点数付加情報マスタ
       01  TNSP-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    ワーク診療行為マスタ
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ＳＰＡ共通設定
           COPY    "CPORCSCOMMON.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
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
                                   USING
                                   COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           DISPLAY "START"
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
      *
           INITIALIZE                  WRK-NAIHUKU
           INITIALIZE                  RA03
           INITIALIZE                  WRK-PARA-AREA
                                       H016-AREA
                                       H016-DR-AREA
                                       H016-DR2-AREA
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
           MOVE    WRK-PARA-LDNAME         TO  SPA-SAKIPG
           MOVE    MCP-USER                TO  SPA-OPID
           MOVE    WRK-PARA-HOSPNUM        TO  SPA-HOSPNUM
           MOVE    WRK-PARA-SRYYMD         TO  SPA-SYSYMD
           MOVE    WRK-PARA-SRYYMD         TO  SPA-SRYYMD
           MOVE    WRK-PARA-HKNCOMBINUM    TO  SPA-HKNCOMBINUM
           MOVE    WRK-PARA-PTID           TO  SPA-PTID
           MOVE    WRK-PARA-PTNUM          TO  SPA-PTNUM
           MOVE    WRK-PARA-SRYKA          TO  SPA-SRYKA 
           MOVE    "2"                     TO  SPA-NYUGAIKBN
           MOVE    WRK-PARA-DRCD           TO  SPA-DRCD
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
      *    出力先プリンタ名割り当て情報
           PERFORM 900-1031-READ-SEC
      *
      *    医療機関情報−基本情報
           PERFORM 900-1001-READ-SEC
      *
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
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
           MOVE    "0"                   TO  PARA-TYUSYA
      *
      *    用法コメントコード
           MOVE    "YOUCOM"              TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA              TO  PARA-YOUCOM
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
           DISPLAY "YOUCOM            ="PARA-YOUCOM
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
           MOVE    "A00000RA03"        TO  ORCSPRGOPTION-PRGID
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
      *    薬剤情報取得
      *****************************************************************
       1300-ORCSCH62-REC-SEC       SECTION.
      *
           MOVE    1          TO  IDX3
           INITIALIZE                     SYUNOU-REC
           MOVE    SPA-HOSPNUM            TO  SYU-HOSPNUM
           MOVE    SPA-SRYYMD             TO  SYU-SRYYMD
           MOVE    SPA-PTID               TO  SYU-PTID
           MOVE    SPA-SRYKA              TO  SYU-SRYKA
           MOVE    SPA-HKNCOMBINUM        TO  SYU-HKNCOMBINUM
           MOVE    "2"                    TO  SYU-NYUGAIKBN
           MOVE    SYUNOU-REC             TO  MCPDATA-REC
           PERFORM  910-SYUNOU-SELECT-SEC
           PERFORM  UNTIL  FLG-SYUNOU  =  1
             MOVE    SYU-PTID         TO  WRK-SYU-PTID(IDX3)
             MOVE    SYU-DENPNUM      TO  WRK-SYU-DENPNUM(IDX3)
             MOVE    SYU-UPYMD        TO  WRK-SYU-UPYMD(IDX3)
             MOVE    SYU-UPHMS        TO  WRK-SYU-UPHMS(IDX3)
             ADD   1                  TO  IDX3
             PERFORM   910-SYUNOU-FETCH-SEC
           END-PERFORM
           PERFORM  910-SYUNOU-CLOSE-SEC
      *
      *    順番入れ替え
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL   WRK-SYU-SORT-KEY(IDX3)  =  SPACE
                    OR  IDX3   >  10
             IF   WRK-SYU-SORT-KEY(IDX3)  <  WRK-SYU-SORT-KEY(IDX3 + 1)
               MOVE   WRK-SYUNOU-SORT(IDX3)  TO  WRK-SORT-DATA
               MOVE   WRK-SYUNOU-SORT(IDX3 + 1)
                                             TO  WRK-SYUNOU-SORT(IDX3)
               MOVE   WRK-SORT-DATA    TO  WRK-SYUNOU-SORT(IDX3 + 1)
             END-IF
           END-PERFORM
      *
           MOVE    WRK-PARA-SRYYMD      TO  WRK-SORT-DATA
           .
       1300-ORCSCH62-REC-EXT.
           EXIT.
      
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           EVALUATE  WRK-PARA-LDNAME(1:3)
               WHEN  "K02"
      *               中途終了処理
                      PERFORM  210-MAIN-SEC
               WHEN  "K03"
      *               請求確認（K03）
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
      *    印刷対象取得処理
           PERFORM  210-SUB-SEC
      *
           PERFORM  VARYING  IDX-ST  FROM  1  BY  1
                    UNTIL  ( IDX-ST              >  5
                    OR       H016-DRCD1(IDX-ST)  =  SPACE )
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
               CALL    "A00000RA032"           USING
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
               OPEN    OUTPUT  SORT-FILE
               CLOSE           SORT-FILE
               OPEN    I-O     SORT-FILE
      *
      *        データがなくなるまで
               MOVE    ZERO            TO  CNT-ZAI
               PERFORM VARYING IDX-ZAI     FROM    1   BY  1
                       UNTIL ( IDX-ZAI     >   ORCHC62-ZAI-MAX  )
      *            帳票ＢＯＤＹ情報編集処理
                   PERFORM 220-BODY-HENSYU-SEC
               END-PERFORM
      *
               DISPLAY "CNT-ZAI             ="CNT-ZAI
               DISPLAY "CNT-NAI-ZAI         ="CNT-NAI-ZAI
               DISPLAY "CNT-GAI-ZAI         ="CNT-GAI-ZAI
               DISPLAY "CNT-TON-ZAI         ="CNT-TON-ZAI
               DISPLAY "CNT-TAKU-ZAI        ="CNT-TAKU-ZAI
               DISPLAY "CNT-TYU-ZAI         ="CNT-TYU-ZAI
      *
               CLOSE           SORT-FILE
      *
      *        一時ファイル編集
               OPEN    INPUT   SORT-FILE
               PERFORM  240-SORTFILE-HEN-SEC
               CLOSE           SORT-FILE
      *
      *        データがあれば印刷処理を行う
               IF  CNT-ZAI  NOT =  ZERO
      *          帳票印刷処理
                 PERFORM 400-PRT-EDIT-SEC
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
           MOVE     9999999            TO  ORCHC62-DENPNUM
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
               CALL    "A00000RA032"           USING
                                               SPA-AREA
                                               ORCSCH62AREA
                                               ORCHC62AREA
                                               H016-DR-AREA
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
               OPEN    OUTPUT  SORT-FILE
               CLOSE           SORT-FILE
               OPEN    I-O     SORT-FILE
      *
      *        データがなくなるまで
               MOVE    ZERO            TO  CNT-ZAI
               PERFORM VARYING IDX-ZAI     FROM    1   BY  1
                       UNTIL ( IDX-ZAI     >   ORCHC62-ZAI-MAX  )
      *            帳票ＢＯＤＹ情報編集処理
                   PERFORM 220-BODY-HENSYU-SEC
               END-PERFORM
      *
               DISPLAY "CNT-ZAI             ="CNT-ZAI
               DISPLAY "CNT-NAI-ZAI         ="CNT-NAI-ZAI
               DISPLAY "CNT-GAI-ZAI         ="CNT-GAI-ZAI
               DISPLAY "CNT-TON-ZAI         ="CNT-TON-ZAI
               DISPLAY "CNT-TAKU-ZAI        ="CNT-TAKU-ZAI
               DISPLAY "CNT-TYU-ZAI         ="CNT-TYU-ZAI
      *
               CLOSE           SORT-FILE
      *
      *        一時ファイル編集
               OPEN    INPUT   SORT-FILE
               PERFORM  240-SORTFILE-HEN-SEC
               CLOSE           SORT-FILE
      *
      *        データがあれば印刷処理を行う
               IF  CNT-ZAI  NOT =  ZERO
      *          帳票印刷処理
                 PERFORM 400-PRT-EDIT-SEC
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
      *    剤情報編集領域にセット
           INITIALIZE                  WRK-SRY-AREA
           INITIALIZE                  SORT-REC
      *
           MOVE    ORCHC62-ZAIKAIKEI(IDX-ZAI)  TO  WRK-ZAIKAIKEI
           MOVE    ORCHC62-SRYSYUKBN(IDX-ZAI)  TO  WRK-SRYSYUKBN
           MOVE    ORCHC62-SRYKBN(IDX-ZAI)     TO  WRK-SRYKBN
                                                   WRK-SRYKBNBACK
      *
           EVALUATE  WRK-SRYKBNBACK
               WHEN  "21"
                   MOVE    "210"           TO  WRK-SRYSYUKBN
                   COMPUTE CNT-NAI-ZAI     =   CNT-NAI-ZAI     +   1
               WHEN  "22"
                   MOVE    "220"           TO  WRK-SRYSYUKBN
                   COMPUTE  CNT-TON-ZAI    =   CNT-TON-ZAI     +   1
               WHEN  "23"
                   MOVE    "230"           TO  WRK-SRYSYUKBN
                   COMPUTE CNT-GAI-ZAI     =   CNT-GAI-ZAI     +   1
               WHEN  "14"
                   IF  PARA-TYUSYA           =   "1"
                     EVALUATE  WRK-SRYSYUKBN
                         WHEN  "141"
                         WHEN  "142"
                              MOVE    WRK-SRYSYUKBN  TO  WRK-SRYSYUKBN
                              COMPUTE CNT-TAKU-ZAI = CNT-TAKU-ZAI + 1
                     END-EVALUATE
                   END-IF
               WHEN  "31"
               WHEN  "32"
               WHEN  "33"
               WHEN  "34"
               WHEN  "35"
                     IF  PARA-TYUSYA           =   "1"
                       MOVE    WRK-SRYSYUKBN   TO  WRK-SRYSYUKBN
                       COMPUTE CNT-TYU-ZAI     =   CNT-TYU-ZAI + 1
                     END-IF
               WHEN  OTHER
                   GO  TO  220-BODY-HENSYU-EXT
           END-EVALUATE
      *
      *    KEY情報設定
           MOVE    ZERO            TO  CNT-SRY
           MOVE    ZERO            TO  WRK-FUKUYOSU
           MOVE    1               TO  IDX2
           MOVE    0               TO  SORT-ZAIGATAKBN(IDX2)
           MOVE    99              TO  IDX-YOU
      *
           PERFORM  VARYING  IDX-SRY  FROM  1  BY  1
                    UNTIL  ( IDX-SRY  >  50 )
                    OR  ( ORCHC62-SRYCD(IDX-ZAI IDX-SRY)  =  SPACE )
      *    点数検索
             MOVE    ORCHC62-SRYCD(IDX-ZAI IDX-SRY)
                                         TO  WRK-SRYCD
             PERFORM  900-TENSU-READ-SEC
             MOVE    WRK-SRYKBNBACK      TO  SORT-SRYKBN
             MOVE    WRK-SRYSYUKBN       TO  SORT-SRYSYUKBN
      *
             IF  TNS-SRYCD(1:3)  =  "001"
               MOVE    TNS-SRYCD         TO  SORT-YOHOCD(IDX2)
               MOVE    IDX-SRY           TO  IDX-YOU
               IF  ORCHC62-INPUTCOMENT(IDX-ZAI IDX-SRY)    =   SPACE
                 STRING  "【"            DELIMITED   BY  SIZE
                         TNS-NAME        DELIMITED   BY  SPACE
                         "】"            DELIMITED   BY  SIZE
                         INTO            SORT-YOHONAME(IDX2)
                 END-STRING
               ELSE
                   STRING  "【"            DELIMITED   BY  SIZE
                           ORCHC62-INPUTCOMENT(IDX-ZAI IDX-SRY)
                                           DELIMITED   BY  SPACE
                           "】"            DELIMITED   BY  SIZE
                           INTO            SORT-YOHONAME(IDX2)
                   END-STRING
               END-IF
               ADD    1                  TO  IDX2
      *        服用時点
               PERFORM  VARYING  IDX5  FROM  1  BY  1
                        UNTIL  IDX5  >  5
                 IF   TNS-SSTKIJUNCD(IDX5)  =  1
                   ADD    1        TO  WRK-FUKUYOSU
                 END-IF
               END-PERFORM
             ELSE
               IF  TNS-ZAIGATAKBN  =  "3"
      *            液剤の場合
                   MOVE    3                 TO  SORT-ZAIGATAKBN(IDX2)
               END-IF
             END-IF
      *
             IF  TNS-SRYCD(1:3)  =  "008"
               IF  IDX-SRY  >  IDX-YOU
      *
                 IF    TNS-SRYCD            =  PARA-YOUCOM
                   MOVE    TNS-SRYCD             TO  SORT-YOHOCD(IDX2)
                   IF  ORCHC62-INPUTCOMENT(IDX-ZAI IDX-SRY)   =   SPACE
                     MOVE    TNS-NAME            TO  WRK-YOUCOM-NAME
                   ELSE
                     MOVE    ORCHC62-INPUTCOMENT(IDX-ZAI IDX-SRY)
                                                 TO  WRK-YOUCOM-NAME
                   END-IF
                   MOVE   WRK-YOUCOM-NAME        TO  SORT-YOHONAME(IDX2)
                   ADD    1                      TO  IDX2
                 END-IF
               END-IF
             END-IF
      *
           END-PERFORM
      *
      *    剤回数(日数等)
           MOVE    "999999999"                 TO  SORT-YOHOCD(IDX2)
           MOVE    ORCHC62-ZAIKAIKEI(IDX-ZAI)  TO  SORT-YOHONAME(IDX2)
           MOVE    ORCHC62-ZAIKAIKEI(IDX-ZAI)  TO  SORT-ZAIKAIKEI
      *
      *    TMPファイル読み込み
           READ  SORT-FILE
             INVALID    KEY
               MOVE    1           TO  FLG-INVALID
             NOT  INVALID  KEY
               MOVE    ZERO        TO  FLG-INVALID
           END-READ
      *
           MOVE    1               TO  IDX1
           MOVE    ZERO            TO  CNT-SRY
           PERFORM VARYING IDX-SRY     FROM    1   BY  1
                   UNTIL ( IDX-SRY     >   50  )
                   OR    ( ORCHC62-SRYCD(IDX-ZAI IDX-SRY)  =   SPACE )
      *        明細編集
               PERFORM 230-NAIYO-HENSYU-SEC
           END-PERFORM
      *
      *    剤情報があれば処理する
           IF      CNT-SRY         =   ZERO
               GO  TO  220-BODY-HENSYU-EXT
           END-IF
      *
           IF  FLG-INVALID  =  1
             WRITE  SORT-REC
           ELSE
             REWRITE  SORT-REC
           END-IF
      *
           .
       220-BODY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    SORTファイル編集処理
      *****************************************************************
       240-SORTFILE-HEN-SEC       SECTION.
      *
           INITIALIZE           SORT-REC
      *
           MOVE    ZERO         TO  IDX-NAI
           MOVE    ZERO         TO  IDX-TON
           MOVE    ZERO         TO  IDX-GAI
           MOVE    ZERO         TO  IDX-TAKU
           MOVE    ZERO         TO  IDX-TYU
      *
           MOVE    ZERO         TO  IDX-NAI-MAX
           MOVE    ZERO         TO  IDX-TON-MAX
           MOVE    ZERO         TO  IDX-GAI-MAX
           MOVE    ZERO         TO  IDX-TAKU-MAX
           MOVE    ZERO         TO  IDX-TYU-MAX
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL  FLG-PRINT  =  1
      *
             EVALUATE  SORT-SRYKBN
               WHEN  "21"
                 ADD     1              TO  IDX-NAI
                 MOVE    IDX-NAI        TO  IDX-NAI-MAX
               WHEN  "22"
                 ADD     1              TO  IDX-TON
                 MOVE    IDX-TON        TO  IDX-TON-MAX
               WHEN  "23"
                 ADD     1              TO  IDX-GAI
                 MOVE    IDX-GAI        TO  IDX-GAI-MAX
               WHEN  "14"
                   IF  PARA-TYUSYA           =   "1"
                     EVALUATE  SORT-SRYSYUKBN
                         WHEN  "141"
                         WHEN  "142"
                               ADD     1            TO  IDX-TAKU
                               MOVE    IDX-TAKU     TO  IDX-TAKU-MAX
                     END-EVALUATE
                   END-IF
               WHEN  "31"
               WHEN  "32"
               WHEN  "33"
               WHEN  "34"
               WHEN  "35"
                     IF  PARA-TYUSYA           =   "1"
                         ADD     1              TO  IDX-TYU
                         MOVE    IDX-TYU        TO  IDX-TYU-MAX
                     END-IF
             END-EVALUATE
      *
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL  IDX2  >  10
                      OR  ( SORT-YOHOCD(IDX2)  =  SPACE
                      AND  SORT-YOHONAME(IDX2)  =  SPACE )
      *        用法編集
               PERFORM  240-YOUHOU-HENSYU-SEC
             END-PERFORM
      *
      *      一行スペース
             EVALUATE  SORT-SRYKBN
                 WHEN  "21"
                     PERFORM 250-PAGE-CHK-NAI-SEC
                     MOVE    SPACE
                         TO  WRK-NAI-NAIYO(IDX-NAI CNT-NAI-LINE)
                     MOVE    ZERO
                         TO  WRK-NAI-SURYOFLG(IDX-NAI CNT-NAI-LINE)
                     MOVE    CNT-NAI-LINE TO  WRK-CNT-NAI(IDX-NAI)
                 WHEN  "22"
                     PERFORM 250-PAGE-CHK-TON-SEC
                     MOVE    SPACE
                         TO  WRK-TON-NAIYO(IDX-TON CNT-TON-LINE)
                     MOVE    ZERO
                         TO  WRK-TON-SURYOFLG(IDX-TON CNT-TON-LINE)
                     MOVE    CNT-TON-LINE TO  WRK-CNT-TON(IDX-TON)
                 WHEN  "23"
                     PERFORM 250-PAGE-CHK-GAI-SEC
                     MOVE    SPACE
                         TO  WRK-GAI-NAIYO(IDX-GAI CNT-GAI-LINE)
                     MOVE    ZERO
                         TO  WRK-GAI-SURYOFLG(IDX-GAI CNT-GAI-LINE)
                     MOVE    CNT-GAI-LINE TO  WRK-CNT-GAI(IDX-GAI)
                 WHEN  "14"
                   IF  PARA-TYUSYA           =   "1"
                     EVALUATE  SORT-SRYSYUKBN
                     WHEN  "141"
                     WHEN  "142"
                       PERFORM 250-PAGE-CHK-TAKU-SEC
                       MOVE  SPACE
                         TO WRK-TAKU-NAIYO(IDX-TAKU CNT-TAKU-LINE)
                       MOVE  ZERO
                         TO WRK-TAKU-SURYOFLG(IDX-TAKU CNT-TAKU-LINE)
                       MOVE  CNT-TAKU-LINE TO  WRK-CNT-TAKU(IDX-TAKU)
                     END-EVALUATE
                   END-IF
                 WHEN  "31"
                 WHEN  "32"
                 WHEN  "33"
                 WHEN  "34"
                 WHEN  "35"
                     IF  PARA-TYUSYA           =   "1"
                         PERFORM 250-PAGE-CHK-TYU-SEC
                         MOVE  SPACE
                           TO WRK-TYU-NAIYO(IDX-TYU CNT-TYU-LINE)
                         MOVE  ZERO
                           TO WRK-TYU-SURYOFLG(IDX-TYU CNT-TYU-LINE)
                         MOVE  CNT-TYU-LINE TO  WRK-CNT-TYU(IDX-TYU)
                     END-IF
             END-EVALUATE
      *
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL  IDX1  >  50
                      OR  SORT-SRYNAME(IDX1)  =  SPACE
               IF  SORT-SRYCD(IDX1)(1:3)  NOT =  "001"
      *          名称編集
                 PERFORM  240-YKTNAME-HENSYU-SEC
      *          数量編集
                 PERFORM  240-SURYOU-HENSYU-SEC
               END-IF
             END-PERFORM
      *
             MOVE    ZERO           TO  CNT-NAI-LINE
             MOVE    ZERO           TO  CNT-TON-LINE
             MOVE    ZERO           TO  CNT-GAI-LINE
             MOVE    ZERO           TO  CNT-TAKU-LINE
             MOVE    ZERO           TO  CNT-TYU-LINE
      *
      *    一時ファイル読込
             PERFORM  900-TMP-READ-SEC
           END-PERFORM
           .
       240-SORTFILE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    内容編集処理
      *****************************************************************
       230-NAIYO-HENSYU-SEC        SECTION.
      *
           MOVE    ORCHC62-SRYCD(IDX-ZAI IDX-SRY)
                                   TO  WRK-SRYCD
      *
      *    点数検索
           PERFORM 900-TENSU-READ-SEC
      *
      *    点数なければ処理抜ける
           IF      FLG-TENSU       =   1
               GO  TO  230-NAIYO-HENSYU-EXT
           END-IF
      *
           COMPUTE CNT-SRY     =   CNT-SRY     +   1
      *
           INITIALIZE                  WRK-HEN-AREA
      *
           IF   TNS-SRYCD(1:3)  NOT =  "001"
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL  IDX1  >  50
               IF   SORT-SRYNAME(IDX1)  =   SPACE
                 IF   TNS-SRYCD(1:3)  =  "008"
                   IF   TNS-SRYCD  =  PARA-YOUCOM
                     IF   IDX-SRY  <  IDX-YOU
                        MOVE    TNS-SRYCD      TO  SORT-SRYCD(IDX1)
                        MOVE    ORCHC62-INPUTCOMENT(IDX-ZAI IDX-SRY)
                                               TO  SORT-SRYNAME(IDX1)
                     END-IF
                   END-IF
                 ELSE
                   MOVE    TNS-SRYCD           TO  SORT-SRYCD(IDX1)
                   MOVE    TNS-NAME            TO  SORT-SRYNAME(IDX1)
                 END-IF
      *
      *          服用時点の有無 AND 剤単位チェック
                 IF  WRK-FUKUYOSU  NOT =  ZERO
                   DIVIDE  ORCHC62-SRYSURYO(IDX-ZAI IDX-SRY)
                                               BY  WRK-FUKUYOSU
                                               GIVING  WRK-SRYSURYO
                                               REMAINDER  WRK-AMARI
                   IF  WRK-AMARI     >   ZERO
                       MOVE    ZERO            TO  SORT-SRYSURYO(IDX1)
                   ELSE
                       MOVE    WRK-SRYSURYO    TO  SORT-SRYSURYO(IDX1)
                   END-IF
                 ELSE
                   MOVE    ORCHC62-SRYSURYO(IDX-ZAI IDX-SRY)
                                             TO  WRK-SRYSURYO
                   MOVE    WRK-SRYSURYO      TO  SORT-SRYSURYO(IDX1)
                 END-IF
                 MOVE    ORCHC62-INPUTCOMENT(IDX-ZAI IDX-SRY)
                                             TO  SORT-INPUTCOMENT(IDX1)
                 MOVE    99                  TO  IDX1
               END-IF
      *
             END-PERFORM
           END-IF
      *
           .
       230-NAIYO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    用法編集処理
      *****************************************************************
       240-YOUHOU-HENSYU-SEC       SECTION.
      *
      *    回数編集
           PERFORM 240-KAISUU-HENSYU-SEC
      *
           .
       240-YOUHOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    名称編集処理
      *****************************************************************
       240-YKTNAME-HENSYU-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-YKTNAME
           MOVE    SORT-SRYCD(IDX1)   TO  WRK-SRYCD
      *
      *    点数検索
           PERFORM  900-TENSU-READ-SEC
      *
      *    薬剤編集
           IF      TNS-SRYCD(1:1)  =   "6"
      *        内用薬の総量編集の判定
               IF      TNSP-SOURYOHENKBN   =   1
                   MOVE    1               TO  FLG-SOURYO
               END-IF
               MOVE     SORT-SRYNAME(IDX1)  TO  WRK-YKTNAME
           ELSE
      *        コメントあればセット、なければ名称セット
               IF      SORT-INPUTCOMENT(IDX1)    =   SPACE
                    MOVE    SORT-SRYNAME(IDX1)  TO  WRK-YKTNAME
               ELSE
                   MOVE    SORT-INPUTCOMENT(IDX1)   TO  WRK-YKTNAME
               END-IF
           END-IF
      *
           MOVE    WRK-YKTNAME     TO  WRK-HENSYUDATA
      *
           PERFORM 250-HENSYU-SYORI-SEC
      *
           .
       240-YKTNAME-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量編集処理
      *****************************************************************
       240-SURYOU-HENSYU-SEC       SECTION.
      *
      *    薬剤、器材のみ
           IF    ( SORT-SRYCD(IDX1)(1:1)      =   "6"   )  OR
                 ( SORT-SRYCD(IDX1)(1:1)      =   "7"   )  OR
                 ( SORT-SRYCD(IDX1)(1:3)      =   "059" )
               CONTINUE
           ELSE
               GO  TO  240-SURYOU-HENSYU-EXT
           END-IF
      *
           MOVE    SPACE           TO  WRK-HENNAIYO
           MOVE    SPACE           TO  WRK-SURYOU
      *
           MOVE    SORT-SRYSURYO(IDX1)   TO  WRK-SURYO
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
           INITIALIZE     WRK-YOUHOU
           INITIALIZE     WRK-HENSYUDATA
      *
           IF   SORT-YOHOCD(IDX2)  =  "999999999"
      *    回数の場合
             MOVE    SPACE           TO  WRK-HENNAIYO
      *
      *    内服で総量表示の薬剤のみの場合、外用と同様の表示とする
             IF    ( SORT-SRYKBN      =   "21" )  AND
                   ( FLG-SOURYO      =   1    )
                 MOVE    "23"            TO  WRK-SRYKBN
             END-IF
      *
      *    内服か頓服でなければ処理抜ける
             IF    ( SORT-SRYKBN  NOT =   "21" )  AND
                   ( SORT-SRYKBN  NOT =   "22" )
                 GO  TO  240-KAISUU-HENSYU-EXT
             END-IF
      *
             MOVE    SORT-ZAIKAIKEI   TO  WRK-SURYO
             IF      WRK-SURYO       >   0
                 PERFORM 250-ZHEN-SURYO-SET-SEC
                 MOVE    WRK-HENSURYO    TO  WRK-KAISUU
             END-IF
      *
             EVALUATE    SORT-SRYKBN
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
             PERFORM 250-MHEN-SYORI-SET-SEC
      *
           ELSE
      *    用法の場合
               MOVE    SORT-YOHONAME(IDX2)  TO  WRK-YOUHOU
               MOVE    WRK-YOUHOU           TO  WRK-HENSYUDATA
             PERFORM 250-HENSYU-SYORI-SEC
      *
           END-IF
      *
      *
           .
       240-KAISUU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁チェック　処理（内服）
      *****************************************************************
       250-PAGE-CHK-NAI-SEC            SECTION.
      *
           COMPUTE CNT-NAI-LINE    =   CNT-NAI-LINE    +   1
           MOVE    SPACE      TO  WRK-NAI-NAIYO(IDX-NAI CNT-NAI-LINE)
           MOVE    SPACE      TO  WRK-NAI-NAIYO2(IDX-NAI CNT-NAI-LINE)
      *
           .
       250-PAGE-NAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁チェック　処理（頓服）
      *****************************************************************
       250-PAGE-CHK-TON-SEC            SECTION.
      *
           COMPUTE CNT-TON-LINE    =   CNT-TON-LINE    +   1
           MOVE    SPACE       TO  WRK-TON-NAIYO(IDX-TON CNT-TON-LINE)
           MOVE    SPACE       TO  WRK-TON-NAIYO2(IDX-TON CNT-TON-LINE)
      *
           .
       250-PAGE-TON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁チェック　処理（外用）
      *****************************************************************
       250-PAGE-CHK-GAI-SEC            SECTION.
      *
           COMPUTE CNT-GAI-LINE    =   CNT-GAI-LINE    +   1
           MOVE    SPACE      TO  WRK-GAI-NAIYO(IDX-GAI CNT-GAI-LINE)
           MOVE    SPACE      TO  WRK-GAI-NAIYO2(IDX-GAI CNT-GAI-LINE)
      *
           .
       250-PAGE-GAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁チェック　処理  在宅(薬剤・材料)
      *****************************************************************
       250-PAGE-CHK-TAKU-SEC            SECTION.
      *
           COMPUTE CNT-TAKU-LINE    =   CNT-TAKU-LINE    +   1
           MOVE    SPACE   TO  WRK-TAKU-NAIYO(IDX-TAKU CNT-TAKU-LINE)
           MOVE    SPACE   TO  WRK-TAKU-NAIYO2(IDX-TAKU CNT-TAKU-LINE)
      *
           .
       250-PAGE-TAKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁チェック　処理  (注射)
      *****************************************************************
       250-PAGE-CHK-TYU-SEC            SECTION.
      *
           COMPUTE CNT-TYU-LINE    =   CNT-TYU-LINE    +   1
           MOVE    SPACE   TO  WRK-TYU-NAIYO(IDX-TYU CNT-TYU-LINE)
           MOVE    SPACE   TO  WRK-TYU-NAIYO2(IDX-TYU CNT-TYU-LINE)
      *
           .
       250-PAGE-TYU-CHK-EXT.
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
           COMPUTE IDX-STR     =   33  -   IDX-STR1
      *
      *    文字が重なる場合は１行下に表示する
           EVALUATE  SORT-SRYKBN
               WHEN  "21"
      *        内服
                   IF   ( WRK-NAI-NAIYO2(IDX-NAI CNT-NAI-LINE)
                                                        NOT =   SPACE )
                   OR   ( WRK-NAI-NAIYO(IDX-NAI CNT-NAI-LINE)(IDX-STR:)
                                                        NOT =   SPACE )
                       PERFORM 250-PAGE-CHK-NAI-SEC
                       MOVE    WRK-HENNAIYO  
                                TO  WRK-NAI-NAIYO2(IDX-NAI CNT-NAI-LINE)
                       MOVE    IDX-NAI      TO  WRK-CNT-NAI(IDX-NAI)
                   ELSE
                       MOVE    WRK-HENNAIYO
                                TO  WRK-NAI-NAIYO2(IDX-NAI CNT-NAI-LINE)
                   END-IF
                       MOVE   1  
                              TO  WRK-NAI-SURYOFLG(IDX-NAI CNT-NAI-LINE)
                       MOVE   CNT-NAI-LINE      TO  WRK-CNT-NAI(IDX-NAI)
      *
               WHEN  "22"
      *        頓服
                   IF    ( WRK-TON-NAIYO2(IDX-TON CNT-TON-LINE)
                                                        NOT =   SPACE )
                   OR   ( WRK-TON-NAIYO(IDX-TON CNT-TON-LINE)(IDX-STR:)
                                                        NOT =   SPACE )
                       PERFORM 250-PAGE-CHK-TON-SEC
                       MOVE    WRK-HENNAIYO  
                               TO  WRK-TON-NAIYO2(IDX-TON CNT-TON-LINE)
                       MOVE    CNT-TON-LINE TO  WRK-CNT-TON(IDX-TON)
                   ELSE
                       MOVE    WRK-HENNAIYO
                               TO  WRK-TON-NAIYO2(IDX-TON CNT-TON-LINE)
                   END-IF
               WHEN  "23"
      *        外用
                   IF    ( WRK-GAI-NAIYO2(IDX-GAI CNT-GAI-LINE)
                                                        NOT =   SPACE )
                   OR   ( WRK-GAI-NAIYO(IDX-GAI CNT-GAI-LINE)(IDX-STR:)
                                                        NOT =   SPACE )
                       PERFORM 250-PAGE-CHK-GAI-SEC
                       MOVE    WRK-HENNAIYO
                               TO  WRK-GAI-NAIYO2(IDX-GAI CNT-GAI-LINE)
                       MOVE    CNT-GAI-LINE TO  WRK-CNT-GAI(IDX-GAI)
                   ELSE
                       MOVE    WRK-HENNAIYO
                               TO  WRK-GAI-NAIYO2(IDX-GAI CNT-GAI-LINE)
                   END-IF
               WHEN  "14"
      *        在宅(薬剤・材料)
               IF  PARA-TYUSYA           =   "1"
                 EVALUATE  SORT-SRYSYUKBN
                 WHEN  "141"
                 WHEN  "142"
                   IF ( WRK-TAKU-NAIYO2(IDX-TAKU CNT-TAKU-LINE)
                                                       NOT =   SPACE )
                   OR ( WRK-TAKU-NAIYO(IDX-TAKU CNT-TAKU-LINE)(IDX-STR:)
                                                       NOT =   SPACE )
                       PERFORM 250-PAGE-CHK-TAKU-SEC
                       MOVE  WRK-HENNAIYO
                         TO  WRK-TAKU-NAIYO2(IDX-TAKU CNT-TAKU-LINE)
                       MOVE  CNT-TAKU-LINE TO  WRK-CNT-TAKU(IDX-TAKU)
                   ELSE
                       MOVE  WRK-HENNAIYO
                         TO  WRK-TAKU-NAIYO2(IDX-TAKU CNT-TAKU-LINE)
                   END-IF
                 END-EVALUATE
               END-IF
               WHEN  "31"
               WHEN  "32"
               WHEN  "33"
               WHEN  "34"
               WHEN  "35"
      *        注射
                   IF  PARA-TYUSYA           =   "1"
                     IF ( WRK-TYU-NAIYO2(IDX-TYU CNT-TYU-LINE)
                                                       NOT =   SPACE )
                     OR
                     ( WRK-TYU-NAIYO(IDX-TYU CNT-TYU-LINE)(IDX-STR:)
                                                       NOT =   SPACE )
                         PERFORM 250-PAGE-CHK-TYU-SEC
                         MOVE  WRK-HENNAIYO
                           TO  WRK-TYU-NAIYO2(IDX-TYU CNT-TYU-LINE)
                     ELSE
                         MOVE  WRK-HENNAIYO
                           TO  WRK-TYU-NAIYO2(IDX-TYU CNT-TYU-LINE)
                     END-IF
                   END-IF
               WHEN  OTHER
      *        その他
                   GO  TO      240-SURYOU-HENSYU-EXT
           END-EVALUATE
      *
           .
       250-MHEN-SYORI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票文字編集処理
      *****************************************************************
       250-HENSYU-YOHO-SEC        SECTION.
      *
      *    初期値セット
           MOVE    1              TO  IDX-STR
           MOVE    2              TO  IDX-STR1
      *
      *    編集データがなくなるまで繰り返す
           PERFORM  UNTIL  ( IDX-STR  >  100 )
                    OR  ( WRK-HENSYUDATA(IDX-STR:)  =  SPACE )
      *    全角セット
             MOVE    WRK-HENSYUDATA(IDX-STR:2)
                                  TO  WRK-HENLINE(IDX-STR1:2)
             ADD    2             TO  IDX-STR
             ADD    2             TO  IDX-STR1
      *    最大字で改行
             IF  IDX-STR1  >=  38 
                 OR  ( IDX-STR  <=  100 
                 AND  (WRK-HENSYUDATA(IDX-STR:)  =  SPACE 
                 OR   WRK-HENSYUDATA(IDX-STR:2)  =  "　" ))
               EVALUATE  SORT-SRYKBN
                 WHEN  "21"
                   MOVE    WRK-HENLINE 
                                  TO  WRK-NAI-YOHO(IDX-NAI IDX2)
                   IF   SORT-YOHOCD(IDX2)  =  "999999999"
                     MOVE    1   TO  WRK-NAI-YOHO-SURYOFLG(IDX-NAI IDX2)
                   END-IF
                 WHEN  "22"
                   MOVE    WRK-HENLINE
                                  TO  WRK-TON-YOHO(IDX-TON IDX2)
                   IF   SORT-YOHOCD(IDX2)  =  "999999999"
                     MOVE    1   TO  WRK-TON-YOHO-SURYOFLG(IDX-TON IDX2)
                   END-IF
                 WHEN  "23"
                   MOVE    WRK-HENLINE
                                  TO  WRK-GAI-YOHO(IDX-GAI IDX2)
                   IF   SORT-YOHOCD(IDX2)  =  "999999999"
                     MOVE    1   TO  WRK-GAI-YOHO-SURYOFLG(IDX-GAI IDX2)
                   END-IF
                 WHEN  "14"
      *              在宅(薬剤・材料) 
                   IF  PARA-TYUSYA           =   "1"
                     EVALUATE  SORT-SRYSYUKBN
                         WHEN  "141"
                         WHEN  "142"
                           MOVE    WRK-HENLINE 
                                   TO  WRK-TAKU-YOHO(IDX-TAKU IDX2)
                           IF   SORT-YOHOCD(IDX2)  =  "999999999"
                             MOVE    1
                               TO WRK-TAKU-YOHO-SURYOFLG(IDX-TAKU IDX2)
                           END-IF
                     END-EVALUATE
                   END-IF
                 WHEN  "31"
                 WHEN  "32"
                 WHEN  "33"
                 WHEN  "34"
                 WHEN  "35"
      *              注射
                     IF  PARA-TYUSYA           =   "1"
                       MOVE    WRK-HENLINE 
                               TO  WRK-TYU-YOHO(IDX-TYU IDX2)
                       IF   SORT-YOHOCD(IDX2)  =  "999999999"
                         MOVE    1
                           TO WRK-TYU-YOHO-SURYOFLG(IDX-TYU IDX2)
                       END-IF
                     END-IF
                 WHEN  OTHER
                     GO    TO  250-HENSYU-YOHO-EXT
               END-EVALUATE
               MOVE    SPACE      TO  WRK-HENLINE
               MOVE    2          TO  IDX-STR1
             END-IF
           END-PERFORM
      *
           .
       250-HENSYU-YOHO-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票文字編集処理
      *****************************************************************
       250-HENSYU-SYORI-SEC        SECTION.
      *
      *    初期値セット
           MOVE    1              TO  IDX-STR
           MOVE    2              TO  IDX-STR1
      *
      *    編集データがなくなるまで繰り返す
           PERFORM UNTIL  (IDX-STR     >   4000 )
                   OR     (WRK-HENSYUDATA(IDX-STR:)    =   SPACE)
      *        全角セット
               MOVE    WRK-HENSYUDATA(IDX-STR:2)
                                       TO  WRK-HENLINE(IDX-STR1:2)
               ADD     2               TO  IDX-STR
               ADD     2               TO  IDX-STR1
      *        最大字で改行
               IF         (IDX-STR1        >=  38)
                   OR    ((IDX-STR         <=  4000)
                   AND    (WRK-HENSYUDATA(IDX-STR:) =   SPACE))
      *
                   EVALUATE  SORT-SRYKBN
                     WHEN  "21"
                         PERFORM 250-PAGE-CHK-NAI-SEC
                           MOVE    WRK-HENLINE
                                TO  WRK-NAI-NAIYO(IDX-NAI CNT-NAI-LINE)
                   MOVE    ZERO    
                             TO  WRK-NAI-SURYOFLG(IDX-NAI CNT-NAI-LINE)
                   MOVE    CNT-NAI-LINE TO  WRK-CNT-NAI(IDX-NAI)
                     WHEN  "22"
                         PERFORM 250-PAGE-CHK-TON-SEC
                         MOVE    WRK-HENLINE
                              TO  WRK-TON-NAIYO(IDX-TON CNT-TON-LINE)
                         MOVE    ZERO    
                            TO  WRK-TON-SURYOFLG(IDX-TON CNT-TON-LINE)
                         MOVE    CNT-TON-LINE TO  WRK-CNT-TON(IDX-TON)
                     WHEN  "23"
                         PERFORM 250-PAGE-CHK-GAI-SEC
                         MOVE    WRK-HENLINE
                              TO  WRK-GAI-NAIYO(IDX-GAI CNT-GAI-LINE)
                         MOVE    ZERO    
                            TO  WRK-GAI-SURYOFLG(IDX-GAI CNT-GAI-LINE)
                         MOVE    CNT-GAI-LINE TO  WRK-CNT-GAI(IDX-GAI)
                     WHEN  "14"
      *                在宅(薬剤・材料) 
                     IF  PARA-TYUSYA           =   "1"
                       EVALUATE  SORT-SRYSYUKBN
                       WHEN  "141"
                       WHEN  "142"
                         PERFORM 250-PAGE-CHK-TAKU-SEC
                         MOVE    WRK-HENLINE
                          TO WRK-TAKU-NAIYO(IDX-TAKU CNT-TAKU-LINE)
                         MOVE    ZERO
                          TO WRK-TAKU-SURYOFLG(IDX-TAKU CNT-TAKU-LINE)
                         MOVE   CNT-TAKU-LINE
                          TO  WRK-CNT-TAKU(IDX-TAKU)
                       END-EVALUATE
                     END-IF
                     WHEN  "31"
                     WHEN  "32"
                     WHEN  "33"
                     WHEN  "34"
                     WHEN  "35"
      *                注射
                       IF  PARA-TYUSYA           =   "1"
                         PERFORM 250-PAGE-CHK-TYU-SEC
                         MOVE    WRK-HENLINE
                          TO WRK-TYU-NAIYO(IDX-TYU CNT-TYU-LINE)
                         MOVE    ZERO
                          TO WRK-TYU-SURYOFLG(IDX-TYU CNT-TYU-LINE)
                         MOVE   CNT-TYU-LINE
                          TO  WRK-CNT-TYU(IDX-TYU)
                       END-IF
                     WHEN  OTHER
                         GO  TO  240-YOUHOU-HENSYU-EXT
                   END-EVALUATE
                   MOVE    SPACE   TO  WRK-HENLINE
                   MOVE    2       TO  IDX-STR1
               END-IF
           END-PERFORM
      *
           .
       250-HENSYU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力編集処理
      *****************************************************************
       400-PRT-EDIT-SEC           SECTION.
      *
      *    患者情報取得
           INITIALIZE                PTINF-REC
           MOVE    SPA-HOSPNUM       TO  PTINF-HOSPNUM
           MOVE    SPA-PTID          TO  PTINF-PTID
           MOVE    PTINF-REC          TO  MCPDATA-REC
           PERFORM  900-PTINF-READ-SEC
           MOVE    PTINF-NAME        TO  SPA-NAME
      *
      *    帳票印刷処理
      *    内服
           PERFORM  VARYING  IDX-NAI  FROM  1  BY  1
                    UNTIL  IDX-NAI  >  IDX-NAI-MAX
             INITIALIZE RA03
             PERFORM 410-HEAD-SET-SEC
             PERFORM 400-PRT-EDIT-NAI-SEC
             IF   WRK-NAIYO-TBL  NOT  =  SPACE
                  PERFORM 400-PRT-NAI-SEC
             END-IF
           END-PERFORM
      *
      *    頓服
           PERFORM  VARYING  IDX-TON  FROM  1  BY  1
                    UNTIL  IDX-TON  >  IDX-TON-MAX
             INITIALIZE RA03
             PERFORM 410-HEAD-SET-SEC
             PERFORM 400-PRT-EDIT-TON-SEC
             IF   WRK-NAIYO-TBL  NOT  =  SPACE
                  PERFORM 400-PRT-TON-SEC
             END-IF
           END-PERFORM
      *
      *    外用
           PERFORM  VARYING  IDX-GAI  FROM  1  BY  1
                    UNTIL  IDX-GAI  >  IDX-GAI-MAX
             INITIALIZE RA03
             PERFORM 410-HEAD-SET-SEC
             PERFORM 400-PRT-EDIT-GAI-SEC
             IF   WRK-NAIYO-TBL  NOT  =  SPACE
                  PERFORM 400-PRT-GAI-SEC
             END-IF
           END-PERFORM
      *
      *    在宅(薬剤・材料)
           PERFORM  VARYING  IDX-TAKU  FROM  1  BY  1
                    UNTIL  IDX-TAKU  >  IDX-TAKU-MAX
             INITIALIZE RA03
             PERFORM 410-HEAD-SET-SEC
             PERFORM 400-PRT-EDIT-TAKU-SEC
             IF   WRK-NAIYO-TBL  NOT  =  SPACE
                  PERFORM 400-PRT-TAKU-SEC
             END-IF
           END-PERFORM
      *
      *    注射
           PERFORM  VARYING  IDX-TYU  FROM  1  BY  1
                    UNTIL  IDX-TYU  >  IDX-TYU-MAX
             INITIALIZE RA03
             PERFORM 410-HEAD-SET-SEC
             PERFORM 400-PRT-EDIT-TYU-SEC
             IF   WRK-NAIYO-TBL  NOT  =  SPACE
                  PERFORM 400-PRT-TYU-SEC
             END-IF
           END-PERFORM
      *
           .
       400-PRT-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力編集処理（内服）
      *****************************************************************
       400-PRT-EDIT-NAI-SEC                SECTION.
      *
           INITIALIZE                       CNT-LINE
           INITIALIZE                       CNT-LINE2
           INITIALIZE                       CNT-LINE3
           INITIALIZE                       CNT-LINE4
           MOVE              SPACE     TO   WRK-NAIYOU
           MOVE              SPACE     TO   WRK-HEN-NAIYOU
      *
           PERFORM VARYING   CNT-LINE     FROM  1   BY  1
                   UNTIL     CNT-LINE  >  WRK-CNT-NAI(IDX-NAI)
      *
             ADD       1            TO    CNT-LINE2
             IF        CNT-LINE2    >     22
                 IF  WRK-NAI-NAIYO(IDX-NAI CNT-LINE)(1:3) NOT = SPACE
                     PERFORM  400-PRT-NAI-SEC
                     INITIALIZE                   RA03
                     MOVE     SPACE         TO    WRK-NAIYOU
                     INITIALIZE                   CNT-LINE2
                     PERFORM  410-HEAD-SET-SEC
                 ELSE
                     INITIALIZE       FLG-NAIYO
                     INITIALIZE       CNT-LINE3
                     MOVE     CNT-LINE2          TO    CNT-LINE3
                     PERFORM  UNTIL WRK-NAIYO(CNT-LINE3)(1:3)
                                                 NOT   =   SPACE
                              OR       CNT-LINE3       =   1
                              COMPUTE  CNT-LINE3 = CNT-LINE3 - 1
                     END-PERFORM
                     IF  CNT-LINE3                     =   1
                         MOVE  0            TO    FLG-NAIYO
                     ELSE
                         MOVE  1            TO    FLG-NAIYO
                     END-IF
                     IF  FLG-NAIYO     =    0
                         PERFORM  400-PRT-NAI-SEC
                         INITIALIZE         RA03
                         MOVE     SPACE     TO    WRK-NAIYOU
                         INITIALIZE         CNT-LINE2
                         PERFORM  410-HEAD-SET-SEC
                         MOVE     22        TO  CNT-LINE3
                     ELSE
                         MOVE     SPACE     TO  WRK-HEN-NAIYOU
                         PERFORM  VARYING CNT-LINE2  FROM  22  BY  -1
                              UNTIL  WRK-NAIYO(CNT-LINE2)(1:3)
                                                  NOT   =   SPACE
                              IF      WRK-NAIYO(CNT-LINE2)  =  SPACE
                                   MOVE    SPACE
                                        TO  WRK-NAIYO(CNT-LINE2)
                              ELSE
                                   MOVE  WRK-NAIYO(CNT-LINE2)
                                      TO WRK-HEN-NAIYO(CNT-LINE2)
                                   MOVE    SPACE
                                        TO  WRK-NAIYO(CNT-LINE2)
                              END-IF
                              IF      WRK-NAIYO2(CNT-LINE2)
                                                         =  SPACE
                                   MOVE    SPACE
                                        TO  WRK-NAIYO2(CNT-LINE2)
                              ELSE
                                   MOVE  WRK-NAIYO2(CNT-LINE2)
                                     TO WRK-HEN-NAIYO2(CNT-LINE2)
                                   MOVE    SPACE
                                        TO  WRK-NAIYO2(CNT-LINE2)
                              END-IF
                         END-PERFORM
                         IF      WRK-NAIYO(CNT-LINE2)    =  SPACE
                              MOVE    SPACE
                                        TO  WRK-NAIYO(CNT-LINE2)
                         ELSE
                              MOVE  WRK-NAIYO(CNT-LINE2)
                                      TO WRK-HEN-NAIYO(CNT-LINE2)
                              MOVE    SPACE
                                        TO  WRK-NAIYO(CNT-LINE2)
                         END-IF
                         IF      WRK-NAIYO2(CNT-LINE2)   =  SPACE
                             MOVE    SPACE
                                       TO  WRK-NAIYO2(CNT-LINE2)
                         ELSE
                             MOVE  WRK-NAIYO2(CNT-LINE2)
                                     TO WRK-HEN-NAIYO2(CNT-LINE2)
                             MOVE    SPACE
                                        TO  WRK-NAIYO2(CNT-LINE2)
                         END-IF
                         PERFORM  400-PRT-NAI-SEC
                         INITIALIZE           RA03
                         MOVE     SPACE     TO WRK-NAIYOU
                         INITIALIZE           CNT-LINE3
                         PERFORM 410-HEAD-SET-SEC
                         MOVE     CNT-LINE2 TO  CNT-LINE3
                         INITIALIZE           CNT-LINE2
                         PERFORM  UNTIL   CNT-LINE3  >  22
                              ADD     1     TO   CNT-LINE2
                              IF      WRK-HEN-NAIYO(CNT-LINE3)
                                                         =  SPACE
                                   MOVE  SPACE
                                          TO WRK-NAIYO(CNT-LINE2)
                              ELSE
                                   MOVE  WRK-HEN-NAIYO(CNT-LINE3)
                                          TO WRK-NAIYO(CNT-LINE2)
                              END-IF
                              IF  WRK-HEN-NAIYO2(CNT-LINE3)
                                                         =  SPACE
                                  MOVE  SPACE
                                         TO WRK-NAIYO2(CNT-LINE2)
                              ELSE
                                  MOVE  WRK-HEN-NAIYO2(CNT-LINE3)
                                         TO WRK-NAIYO2(CNT-LINE2)
                              END-IF
                              ADD     1     TO   CNT-LINE3
                         END-PERFORM
                     END-IF
                 END-IF
                 ADD          1       TO         CNT-LINE2
                 IF  WRK-NAI-NAIYO(IDX-NAI CNT-LINE)  =  SPACE
                     CONTINUE
                 ELSE
                     MOVE     WRK-NAI-NAIYO(IDX-NAI CNT-LINE)
                                 TO  WRK-NAIYO(CNT-LINE2)
                     MOVE    WRK-NAI-SURYOFLG(IDX-NAI CNT-LINE)
                                  TO  WRK-SURYOFLG(CNT-LINE2)
                 END-IF
                 IF  WRK-NAI-NAIYO2(IDX-NAI CNT-LINE)    =  SPACE
                     CONTINUE
                 ELSE
                     MOVE     WRK-NAI-NAIYO2(IDX-NAI CNT-LINE)
                                 TO  WRK-NAIYO2(CNT-LINE2)
                 END-IF
             ELSE
                 IF  WRK-NAI-NAIYO(IDX-NAI CNT-LINE)     =  SPACE
                     CONTINUE
                 ELSE
                     MOVE    WRK-NAI-NAIYO(IDX-NAI CNT-LINE)
                                 TO  WRK-NAIYO(CNT-LINE2)
                     MOVE    WRK-NAI-SURYOFLG(IDX-NAI CNT-LINE)
                                  TO  WRK-SURYOFLG(CNT-LINE2)
                 END-IF
                 IF  WRK-NAI-NAIYO2(IDX-NAI CNT-LINE)    =  SPACE
                     CONTINUE
                 ELSE
                     MOVE    WRK-NAI-NAIYO2(IDX-NAI CNT-LINE)
                                 TO  WRK-NAIYO2(CNT-LINE2)
                 END-IF
             END-IF
           END-PERFORM
           .
       400-PRT-EDIT-NAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力編集処理２（内服）
      *****************************************************************
       400-PRT-NAI-SEC                SECTION.
      *
           MOVE    "内服"               TO  RA03-HUKUYO
      *
      *    内容
           INITIALIZE CNT-LINE5
           PERFORM VARYING CNT-LINE5     FROM    1   BY  1
                   UNTIL   CNT-LINE5     >   CNT-LINE2
                   IF      WRK-NAIYO(CNT-LINE5)    =  SPACE
                           CONTINUE
                   ELSE
                           MOVE    WRK-NAIYO(CNT-LINE5)
                                        TO  RA03-NAIYO(CNT-LINE5)
                   END-IF
                   IF      WRK-NAIYO2(CNT-LINE5)    =  SPACE
                           CONTINUE
                   ELSE
                           MOVE    WRK-NAIYO2(CNT-LINE5)
                                       TO  RA03-NAIYO2(CNT-LINE5)
                   END-IF
           END-PERFORM
           PERFORM  400-PRT-WRITE-SEC
           .
       400-PRT-NAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力編集処理（頓服）
      *****************************************************************
       400-PRT-EDIT-TON-SEC                SECTION.
      *
           INITIALIZE                       CNT-LINE
           INITIALIZE                       CNT-LINE2
           INITIALIZE                       CNT-LINE3
           INITIALIZE                       CNT-LINE4
           MOVE              SPACE     TO   WRK-NAIYOU
           MOVE              SPACE     TO   WRK-HEN-NAIYOU
      *
           PERFORM VARYING   CNT-LINE     FROM  1   BY  1
                   UNTIL     CNT-LINE     >     WRK-CNT-TON(IDX-TON)
                   ADD       1            TO    CNT-LINE2
                   IF        CNT-LINE2    >     22
                  IF  WRK-TON-NAIYO(IDX-TON CNT-LINE)(1:3) NOT = SPACE
                           PERFORM  400-PRT-TON-SEC
                           INITIALIZE                   RA03
                           MOVE     SPACE         TO    WRK-NAIYOU
                           INITIALIZE                   CNT-LINE2
                           PERFORM  410-HEAD-SET-SEC
                       ELSE
                           INITIALIZE       FLG-NAIYO
                           INITIALIZE       CNT-LINE3
                           MOVE     CNT-LINE2          TO    CNT-LINE3
                           PERFORM
                                UNTIL WRK-NAIYO(CNT-LINE3)(1:3)
                                                       NOT   =   SPACE
                                    OR       CNT-LINE3       =   1
                                    COMPUTE  CNT-LINE3 = CNT-LINE3 - 1
                           END-PERFORM
                           IF  CNT-LINE3                     =   1
                               MOVE  0            TO    FLG-NAIYO
                           ELSE
                               MOVE  1            TO    FLG-NAIYO
                           END-IF
                           IF  FLG-NAIYO     =    0
                               PERFORM  400-PRT-TON-SEC
                               INITIALIZE         RA03
                               MOVE     SPACE     TO    WRK-NAIYOU
                               INITIALIZE         CNT-LINE2
                               PERFORM  410-HEAD-SET-SEC
                               MOVE     22        TO  CNT-LINE3
                           ELSE
                               MOVE     SPACE     TO  WRK-HEN-NAIYOU
                               PERFORM  VARYING CNT-LINE2  FROM  
                                                             22  BY  -1
                                    UNTIL 
                                       WRK-NAIYO(CNT-LINE2)(1:3)
                                                        NOT   =   SPACE
                                    IF      WRK-NAIYO(CNT-LINE2)
                                                               =  SPACE
                                         MOVE    SPACE
                                              TO  WRK-NAIYO(CNT-LINE2)
                                    ELSE
                                         MOVE  WRK-NAIYO(CNT-LINE2)
                                            TO WRK-HEN-NAIYO(CNT-LINE2)
                                         MOVE    SPACE
                                              TO  WRK-NAIYO(CNT-LINE2)
                                    END-IF
                                    IF      WRK-NAIYO2(CNT-LINE2)
                                                               =  SPACE
                                         MOVE    SPACE
                                              TO  WRK-NAIYO2(CNT-LINE2)
                                    ELSE
                                         MOVE  WRK-NAIYO2(CNT-LINE2)
                                           TO WRK-HEN-NAIYO2(CNT-LINE2)
                                         MOVE    SPACE
                                              TO  WRK-NAIYO2(CNT-LINE2)
                                    END-IF
                               END-PERFORM
                               IF      WRK-NAIYO(CNT-LINE2)    =  SPACE
                                    MOVE    SPACE
                                              TO  WRK-NAIYO(CNT-LINE2)
                               ELSE
                                    MOVE  WRK-NAIYO(CNT-LINE2)
                                            TO WRK-HEN-NAIYO(CNT-LINE2)
                                    MOVE    SPACE
                                              TO  WRK-NAIYO(CNT-LINE2)
                               END-IF
                               IF      WRK-NAIYO2(CNT-LINE2)   =  SPACE
                                   MOVE    SPACE
                                             TO  WRK-NAIYO2(CNT-LINE2)
                               ELSE
                                   MOVE  WRK-NAIYO2(CNT-LINE2)
                                           TO WRK-HEN-NAIYO2(CNT-LINE2)
                                   MOVE    SPACE
                                              TO  WRK-NAIYO2(CNT-LINE2)
                               END-IF
                               PERFORM  400-PRT-TON-SEC
                               INITIALIZE           RA03
                               MOVE     SPACE     TO WRK-NAIYOU
                               INITIALIZE           CNT-LINE3
                               PERFORM 410-HEAD-SET-SEC
                               MOVE     CNT-LINE2 TO  CNT-LINE3
                               INITIALIZE           CNT-LINE2
                               PERFORM  UNTIL   CNT-LINE3  >  22
                                    ADD     1     TO   CNT-LINE2
                                    IF      WRK-HEN-NAIYO(CNT-LINE3)
                                                               =  SPACE
                                         MOVE  SPACE
                                                TO WRK-NAIYO(CNT-LINE2)
                                    ELSE
                                         MOVE  WRK-HEN-NAIYO(CNT-LINE3)
                                                TO WRK-NAIYO(CNT-LINE2)
                                    END-IF
                                    IF  WRK-HEN-NAIYO2(CNT-LINE3)
                                                               =  SPACE
                                        MOVE  SPACE
                                               TO WRK-NAIYO2(CNT-LINE2)
                                    ELSE
                                        MOVE  WRK-HEN-NAIYO2(CNT-LINE3)
                                               TO WRK-NAIYO2(CNT-LINE2)
                                    END-IF
                                    ADD     1     TO   CNT-LINE3
                               END-PERFORM
                           END-IF
                       END-IF
                       ADD          1       TO         CNT-LINE2
                       IF  WRK-TON-NAIYO(IDX-TON CNT-LINE)  =  SPACE
                           CONTINUE
                       ELSE
                           MOVE     WRK-TON-NAIYO(IDX-TON CNT-LINE)
                                       TO  WRK-NAIYO(CNT-LINE2)
                       END-IF
                       IF  WRK-TON-NAIYO2(IDX-TON CNT-LINE)  =  SPACE
                           CONTINUE
                       ELSE
                           MOVE     WRK-TON-NAIYO2(IDX-TON CNT-LINE)
                                       TO  WRK-NAIYO2(CNT-LINE2)
                       END-IF
                   ELSE
                       IF  WRK-TON-NAIYO(IDX-TON CNT-LINE)  =  SPACE
                           CONTINUE
                       ELSE
                           MOVE    WRK-TON-NAIYO(IDX-TON CNT-LINE)
                                       TO  WRK-NAIYO(CNT-LINE2)
                       END-IF
                       IF  WRK-TON-NAIYO2(IDX-TON CNT-LINE)  =  SPACE
                           CONTINUE
                       ELSE
                           MOVE    WRK-TON-NAIYO2(IDX-TON CNT-LINE)
                                       TO  WRK-NAIYO2(CNT-LINE2)
                       END-IF
                   END-IF
           END-PERFORM
           .
       400-PRT-EDIT-TON-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力編集処理２（頓服）
      *****************************************************************
       400-PRT-TON-SEC                SECTION.
      *
           MOVE    "頓服"               TO  RA03-HUKUYO
      *
           INITIALIZE CNT-LINE5
      *
           PERFORM VARYING CNT-LINE5     FROM    1   BY  1
                   UNTIL   CNT-LINE5     >   CNT-LINE2
                   IF      WRK-NAIYO(CNT-LINE5)    =  SPACE
                           CONTINUE
                   ELSE
                           MOVE    WRK-NAIYO(CNT-LINE5)
                                        TO  RA03-NAIYO(CNT-LINE5)
                   END-IF
      *
                   IF      WRK-NAIYO2(CNT-LINE5)    =  SPACE
                           CONTINUE
                   ELSE
                           MOVE    WRK-NAIYO2(CNT-LINE5)
                                       TO  RA03-NAIYO2(CNT-LINE5)
                   END-IF
      *
           END-PERFORM
      *
           PERFORM  400-PRT-WRITE-SEC
           .
       400-PRT-TON-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力編集処理（外用）
      *****************************************************************
       400-PRT-EDIT-GAI-SEC                SECTION.
      *
           INITIALIZE                       CNT-LINE
           INITIALIZE                       CNT-LINE2
           INITIALIZE                       CNT-LINE3
           INITIALIZE                       CNT-LINE4
           MOVE              SPACE     TO   WRK-NAIYOU
           MOVE              SPACE     TO   WRK-HEN-NAIYOU
      *
           PERFORM VARYING   CNT-LINE     FROM  1   BY  1
                   UNTIL     CNT-LINE     >     WRK-CNT-GAI(IDX-GAI)
                   ADD       1            TO    CNT-LINE2
                   IF        CNT-LINE2    >     22
                 IF  WRK-GAI-NAIYO(IDX-GAI CNT-LINE)(1:3) NOT  =  SPACE
                           PERFORM  400-PRT-GAI-SEC
                           INITIALIZE                   RA03
                           MOVE     SPACE         TO    WRK-NAIYOU
                           INITIALIZE                   CNT-LINE2
                           PERFORM  410-HEAD-SET-SEC
                       ELSE
                           INITIALIZE       FLG-NAIYO
                           INITIALIZE       CNT-LINE3
                           MOVE     CNT-LINE2          TO    CNT-LINE3
                           PERFORM
                                UNTIL WRK-NAIYO(CNT-LINE3)(1:3)
                                                       NOT   =   SPACE
                                    OR       CNT-LINE3       =   1
                                    COMPUTE  CNT-LINE3 = CNT-LINE3 - 1
                           END-PERFORM
                           IF  CNT-LINE3                     =   1
                               MOVE  0            TO    FLG-NAIYO
                           ELSE
                               MOVE  1            TO    FLG-NAIYO
                           END-IF
                           IF  FLG-NAIYO     =    0
                               PERFORM  400-PRT-GAI-SEC
                               INITIALIZE         RA03
                               MOVE     SPACE     TO    WRK-NAIYOU
                               INITIALIZE         CNT-LINE2
                               PERFORM  410-HEAD-SET-SEC
                               MOVE     22        TO  CNT-LINE3
                           ELSE
                               MOVE     SPACE     TO  WRK-HEN-NAIYOU
                               PERFORM  VARYING CNT-LINE2  FROM  
                                                             22  BY  -1
                                    UNTIL 
                                       WRK-NAIYO(CNT-LINE2)(1:3)
                                                        NOT   =   SPACE
                                    IF      WRK-NAIYO(CNT-LINE2)
                                                               =  SPACE
                                         MOVE    SPACE
                                              TO  WRK-NAIYO(CNT-LINE2)
                                    ELSE
                                         MOVE  WRK-NAIYO(CNT-LINE2)
                                            TO WRK-HEN-NAIYO(CNT-LINE2)
                                         MOVE    SPACE
                                              TO  WRK-NAIYO(CNT-LINE2)
                                    END-IF
                                    IF      WRK-NAIYO2(CNT-LINE2)
                                                               =  SPACE
                                         MOVE    SPACE
                                              TO  WRK-NAIYO2(CNT-LINE2)
                                    ELSE
                                         MOVE  WRK-NAIYO2(CNT-LINE2)
                                           TO WRK-HEN-NAIYO2(CNT-LINE2)
                                         MOVE    SPACE
                                              TO  WRK-NAIYO2(CNT-LINE2)
                                    END-IF
                               END-PERFORM
                               IF      WRK-NAIYO(CNT-LINE2)    =  SPACE
                                    MOVE    SPACE
                                              TO  WRK-NAIYO(CNT-LINE2)
                               ELSE
                                    MOVE  WRK-NAIYO(CNT-LINE2)
                                            TO WRK-HEN-NAIYO(CNT-LINE2)
                                    MOVE    SPACE
                                              TO  WRK-NAIYO(CNT-LINE2)
                               END-IF
                               IF      WRK-NAIYO2(CNT-LINE2)   =  SPACE
                                   MOVE    SPACE
                                             TO  WRK-NAIYO2(CNT-LINE2)
                               ELSE
                                   MOVE  WRK-NAIYO2(CNT-LINE2)
                                           TO WRK-HEN-NAIYO2(CNT-LINE2)
                                   MOVE    SPACE
                                              TO  WRK-NAIYO2(CNT-LINE2)
                               END-IF
                               PERFORM  400-PRT-GAI-SEC
                               INITIALIZE           RA03
                               MOVE     SPACE     TO WRK-NAIYOU
                               INITIALIZE           CNT-LINE3
                               PERFORM 410-HEAD-SET-SEC
                               MOVE     CNT-LINE2 TO  CNT-LINE3
                               INITIALIZE           CNT-LINE2
                               PERFORM  UNTIL   CNT-LINE3  >  22
                                    ADD     1     TO   CNT-LINE2
                                    IF      WRK-HEN-NAIYO(CNT-LINE3)
                                                               =  SPACE
                                         MOVE  SPACE
                                                TO WRK-NAIYO(CNT-LINE2)
                                    ELSE
                                         MOVE  WRK-HEN-NAIYO(CNT-LINE3)
                                                TO WRK-NAIYO(CNT-LINE2)
                                    END-IF
                                    IF  WRK-HEN-NAIYO2(CNT-LINE3)
                                                               =  SPACE
                                        MOVE  SPACE
                                               TO WRK-NAIYO2(CNT-LINE2)
                                    ELSE
                                        MOVE  WRK-HEN-NAIYO2(CNT-LINE3)
                                               TO WRK-NAIYO2(CNT-LINE2)
                                    END-IF
                                    ADD     1     TO   CNT-LINE3
                               END-PERFORM
                           END-IF
                       END-IF
                       ADD          1       TO         CNT-LINE2
                       IF  WRK-GAI-NAIYO(IDX-GAI CNT-LINE)  =  SPACE
                           CONTINUE
                       ELSE
                           MOVE     WRK-GAI-NAIYO(IDX-GAI CNT-LINE)
                                       TO  WRK-NAIYO(CNT-LINE2)
                       END-IF
                       IF  WRK-GAI-NAIYO2(IDX-GAI CNT-LINE)  =  SPACE
                           CONTINUE
                       ELSE
                           MOVE     WRK-GAI-NAIYO2(IDX-GAI CNT-LINE)
                                       TO  WRK-NAIYO2(CNT-LINE2)
                       END-IF
                   ELSE
                       IF  WRK-GAI-NAIYO(IDX-GAI CNT-LINE)  =  SPACE
                           CONTINUE
                       ELSE
                           MOVE    WRK-GAI-NAIYO(IDX-GAI CNT-LINE)
                                       TO  WRK-NAIYO(CNT-LINE2)
                       END-IF
                       IF  WRK-GAI-NAIYO2(IDX-GAI CNT-LINE)  =  SPACE
                           CONTINUE
                       ELSE
                           MOVE    WRK-GAI-NAIYO2(IDX-GAI CNT-LINE)
                                       TO  WRK-NAIYO2(CNT-LINE2)
                       END-IF
                   END-IF
           END-PERFORM
           .
       400-PRT-EDIT-GAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力編集処理２（外用）
      *****************************************************************
       400-PRT-GAI-SEC                SECTION.
      *
           MOVE    "外用"               TO  RA03-HUKUYO
      *
           INITIALIZE CNT-LINE5
      *
           PERFORM VARYING CNT-LINE5     FROM    1   BY  1
                   UNTIL   CNT-LINE5     >   CNT-LINE2
                   IF      WRK-NAIYO(CNT-LINE5)    =  SPACE
                           CONTINUE
                   ELSE
                           MOVE    WRK-NAIYO(CNT-LINE5)
                                        TO  RA03-NAIYO(CNT-LINE5)
                   END-IF
      *
                   IF      WRK-NAIYO2(CNT-LINE5)    =  SPACE
                           CONTINUE
                   ELSE
                           MOVE    WRK-NAIYO2(CNT-LINE5)
                                       TO  RA03-NAIYO2(CNT-LINE5)
                   END-IF
      *
           END-PERFORM
      *
           PERFORM  400-PRT-WRITE-SEC
           .
       400-PRT-GAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力編集処理（在宅-薬剤・材料）
      *****************************************************************
       400-PRT-EDIT-TAKU-SEC                SECTION.
      *
           INITIALIZE                       CNT-LINE
           INITIALIZE                       CNT-LINE2
           INITIALIZE                       CNT-LINE3
           INITIALIZE                       CNT-LINE4
           MOVE              SPACE     TO   WRK-NAIYOU
           MOVE              SPACE     TO   WRK-HEN-NAIYOU
      *
           PERFORM VARYING   CNT-LINE     FROM  1   BY  1
                   UNTIL     CNT-LINE     >     WRK-CNT-TAKU(IDX-TAKU)
                   ADD       1            TO    CNT-LINE2
                   IF        CNT-LINE2    >     22
                 IF  WRK-TAKU-NAIYO(IDX-TAKU CNT-LINE)(1:3) NOT = SPACE
                           PERFORM  400-PRT-TAKU-SEC
                           INITIALIZE                   RA03
                           MOVE     SPACE         TO    WRK-NAIYOU
                           INITIALIZE                   CNT-LINE2
                           PERFORM  410-HEAD-SET-SEC
                       ELSE
                           INITIALIZE       FLG-NAIYO
                           INITIALIZE       CNT-LINE3
                           MOVE     CNT-LINE2          TO    CNT-LINE3
                           PERFORM
                                UNTIL WRK-NAIYO(CNT-LINE3)(1:3)
                                                       NOT   =   SPACE
                                    OR       CNT-LINE3       =   1
                                    COMPUTE  CNT-LINE3 = CNT-LINE3 - 1
                           END-PERFORM
                           IF  CNT-LINE3                     =   1
                               MOVE  0            TO    FLG-NAIYO
                           ELSE
                               MOVE  1            TO    FLG-NAIYO
                           END-IF
                           IF  FLG-NAIYO     =    0
                               PERFORM  400-PRT-TAKU-SEC
                               INITIALIZE         RA03
                               MOVE     SPACE     TO    WRK-NAIYOU
                               INITIALIZE         CNT-LINE2
                               PERFORM  410-HEAD-SET-SEC
                               MOVE     22        TO  CNT-LINE3
                           ELSE
                               MOVE     SPACE     TO  WRK-HEN-NAIYOU
                               PERFORM  VARYING CNT-LINE2  FROM  
                                                             22  BY  -1
                                    UNTIL 
                                       WRK-NAIYO(CNT-LINE2)(1:3)
                                                        NOT   =   SPACE
                                    IF      WRK-NAIYO(CNT-LINE2)
                                                               =  SPACE
                                         MOVE    SPACE
                                              TO  WRK-NAIYO(CNT-LINE2)
                                    ELSE
                                         MOVE  WRK-NAIYO(CNT-LINE2)
                                            TO WRK-HEN-NAIYO(CNT-LINE2)
                                         MOVE    SPACE
                                              TO  WRK-NAIYO(CNT-LINE2)
                                    END-IF
                                    IF      WRK-NAIYO2(CNT-LINE2)
                                                               =  SPACE
                                         MOVE    SPACE
                                              TO  WRK-NAIYO2(CNT-LINE2)
                                    ELSE
                                         MOVE  WRK-NAIYO2(CNT-LINE2)
                                           TO WRK-HEN-NAIYO2(CNT-LINE2)
                                         MOVE    SPACE
                                              TO  WRK-NAIYO2(CNT-LINE2)
                                    END-IF
                               END-PERFORM
                               IF      WRK-NAIYO(CNT-LINE2)    =  SPACE
                                    MOVE    SPACE
                                              TO  WRK-NAIYO(CNT-LINE2)
                               ELSE
                                    MOVE  WRK-NAIYO(CNT-LINE2)
                                            TO WRK-HEN-NAIYO(CNT-LINE2)
                                    MOVE    SPACE
                                              TO  WRK-NAIYO(CNT-LINE2)
                               END-IF
                               IF      WRK-NAIYO2(CNT-LINE2)   =  SPACE
                                   MOVE    SPACE
                                             TO  WRK-NAIYO2(CNT-LINE2)
                               ELSE
                                   MOVE  WRK-NAIYO2(CNT-LINE2)
                                           TO WRK-HEN-NAIYO2(CNT-LINE2)
                                   MOVE    SPACE
                                              TO  WRK-NAIYO2(CNT-LINE2)
                               END-IF
                               PERFORM  400-PRT-TAKU-SEC
                               INITIALIZE           RA03
                               MOVE     SPACE     TO WRK-NAIYOU
                               INITIALIZE           CNT-LINE3
                               PERFORM 410-HEAD-SET-SEC
                               MOVE     CNT-LINE2 TO  CNT-LINE3
                               INITIALIZE           CNT-LINE2
                               PERFORM  UNTIL   CNT-LINE3  >  22
                                    ADD     1     TO   CNT-LINE2
                                    IF      WRK-HEN-NAIYO(CNT-LINE3)
                                                               =  SPACE
                                         MOVE  SPACE
                                                TO WRK-NAIYO(CNT-LINE2)
                                    ELSE
                                         MOVE  WRK-HEN-NAIYO(CNT-LINE3)
                                                TO WRK-NAIYO(CNT-LINE2)
                                    END-IF
                                    IF  WRK-HEN-NAIYO2(CNT-LINE3)
                                                               =  SPACE
                                        MOVE  SPACE
                                               TO WRK-NAIYO2(CNT-LINE2)
                                    ELSE
                                        MOVE  WRK-HEN-NAIYO2(CNT-LINE3)
                                               TO WRK-NAIYO2(CNT-LINE2)
                                    END-IF
                                    ADD     1     TO   CNT-LINE3
                               END-PERFORM
                           END-IF
                       END-IF
                       ADD          1       TO         CNT-LINE2
                       IF  WRK-TAKU-NAIYO(IDX-TAKU CNT-LINE)  =  SPACE
                           CONTINUE
                       ELSE
                           MOVE     WRK-TAKU-NAIYO(IDX-TAKU CNT-LINE)
                                       TO  WRK-NAIYO(CNT-LINE2)
                       END-IF
                       IF  WRK-TAKU-NAIYO2(IDX-TAKU CNT-LINE)  =  SPACE
                           CONTINUE
                       ELSE
                           MOVE     WRK-TAKU-NAIYO2(IDX-TAKU CNT-LINE)
                                       TO  WRK-NAIYO2(CNT-LINE2)
                       END-IF
                   ELSE
                       IF  WRK-TAKU-NAIYO(IDX-TAKU CNT-LINE)  =  SPACE
                           CONTINUE
                       ELSE
                           MOVE    WRK-TAKU-NAIYO(IDX-TAKU CNT-LINE)
                                       TO  WRK-NAIYO(CNT-LINE2)
                       END-IF
                       IF  WRK-TAKU-NAIYO2(IDX-TAKU CNT-LINE)  =  SPACE
                           CONTINUE
                       ELSE
                           MOVE    WRK-TAKU-NAIYO2(IDX-TAKU CNT-LINE)
                                       TO  WRK-NAIYO2(CNT-LINE2)
                       END-IF
                   END-IF
           END-PERFORM
           .
       400-PRT-EDIT-TAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力編集処理２（在宅-薬剤・材料）
      *****************************************************************
       400-PRT-TAKU-SEC                SECTION.
      *
           MOVE    "注射"               TO  RA03-HUKUYO
      *
           INITIALIZE CNT-LINE5
      *
           PERFORM VARYING CNT-LINE5     FROM    1   BY  1
                   UNTIL   CNT-LINE5     >   CNT-LINE2
                   IF      WRK-NAIYO(CNT-LINE5)    =  SPACE
                           CONTINUE
                   ELSE
                           MOVE    WRK-NAIYO(CNT-LINE5)
                                        TO  RA03-NAIYO(CNT-LINE5)
                   END-IF
      *
                   IF      WRK-NAIYO2(CNT-LINE5)    =  SPACE
                           CONTINUE
                   ELSE
                           MOVE    WRK-NAIYO2(CNT-LINE5)
                                       TO  RA03-NAIYO2(CNT-LINE5)
                   END-IF
      *
           END-PERFORM
      *
           PERFORM  400-PRT-WRITE-SEC
           .
       400-PRT-TAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力編集処理（注射）
      *****************************************************************
       400-PRT-EDIT-TYU-SEC                SECTION.
      *
           INITIALIZE                       CNT-LINE
           INITIALIZE                       CNT-LINE2
           INITIALIZE                       CNT-LINE3
           INITIALIZE                       CNT-LINE4
           MOVE              SPACE     TO   WRK-NAIYOU
           MOVE              SPACE     TO   WRK-HEN-NAIYOU
      *
           PERFORM VARYING   CNT-LINE     FROM  1   BY  1
                   UNTIL     CNT-LINE     >     WRK-CNT-TYU(IDX-TYU)
                   ADD       1            TO    CNT-LINE2
                   IF        CNT-LINE2    >     22
                 IF  WRK-TYU-NAIYO(IDX-TYU CNT-LINE)(1:3) NOT = SPACE
                           PERFORM  400-PRT-TYU-SEC
                           INITIALIZE                   RA03
                           MOVE     SPACE         TO    WRK-NAIYOU
                           INITIALIZE                   CNT-LINE2
                           PERFORM  410-HEAD-SET-SEC
                       ELSE
                           INITIALIZE       FLG-NAIYO
                           INITIALIZE       CNT-LINE3
                           MOVE     CNT-LINE2          TO    CNT-LINE3
                           PERFORM
                                UNTIL WRK-NAIYO(CNT-LINE3)(1:3)
                                                       NOT   =   SPACE
                                    OR       CNT-LINE3       =   1
                                    COMPUTE  CNT-LINE3 = CNT-LINE3 - 1
                           END-PERFORM
                           IF  CNT-LINE3                     =   1
                               MOVE  0            TO    FLG-NAIYO
                           ELSE
                               MOVE  1            TO    FLG-NAIYO
                           END-IF
                           IF  FLG-NAIYO     =    0
                               PERFORM  400-PRT-TYU-SEC
                               INITIALIZE         RA03
                               MOVE     SPACE     TO    WRK-NAIYOU
                               INITIALIZE         CNT-LINE2
                               PERFORM  410-HEAD-SET-SEC
                               MOVE     22        TO  CNT-LINE3
                           ELSE
                               MOVE     SPACE     TO  WRK-HEN-NAIYOU
                               PERFORM  VARYING CNT-LINE2  FROM  
                                                             22  BY  -1
                                    UNTIL 
                                       WRK-NAIYO(CNT-LINE2)(1:3)
                                                        NOT   =   SPACE
                                    IF      WRK-NAIYO(CNT-LINE2)
                                                               =  SPACE
                                         MOVE    SPACE
                                              TO  WRK-NAIYO(CNT-LINE2)
                                    ELSE
                                         MOVE  WRK-NAIYO(CNT-LINE2)
                                            TO WRK-HEN-NAIYO(CNT-LINE2)
                                         MOVE    SPACE
                                              TO  WRK-NAIYO(CNT-LINE2)
                                    END-IF
                                    IF      WRK-NAIYO2(CNT-LINE2)
                                                               =  SPACE
                                         MOVE    SPACE
                                              TO  WRK-NAIYO2(CNT-LINE2)
                                    ELSE
                                         MOVE  WRK-NAIYO2(CNT-LINE2)
                                           TO WRK-HEN-NAIYO2(CNT-LINE2)
                                         MOVE    SPACE
                                              TO  WRK-NAIYO2(CNT-LINE2)
                                    END-IF
                               END-PERFORM
                               IF      WRK-NAIYO(CNT-LINE2)    =  SPACE
                                    MOVE    SPACE
                                              TO  WRK-NAIYO(CNT-LINE2)
                               ELSE
                                    MOVE  WRK-NAIYO(CNT-LINE2)
                                            TO WRK-HEN-NAIYO(CNT-LINE2)
                                    MOVE    SPACE
                                              TO  WRK-NAIYO(CNT-LINE2)
                               END-IF
                               IF      WRK-NAIYO2(CNT-LINE2)   =  SPACE
                                   MOVE    SPACE
                                             TO  WRK-NAIYO2(CNT-LINE2)
                               ELSE
                                   MOVE  WRK-NAIYO2(CNT-LINE2)
                                           TO WRK-HEN-NAIYO2(CNT-LINE2)
                                   MOVE    SPACE
                                              TO  WRK-NAIYO2(CNT-LINE2)
                               END-IF
                               PERFORM  400-PRT-TYU-SEC
                               INITIALIZE           RA03
                               MOVE     SPACE     TO WRK-NAIYOU
                               INITIALIZE           CNT-LINE3
                               PERFORM 410-HEAD-SET-SEC
                               MOVE     CNT-LINE2 TO  CNT-LINE3
                               INITIALIZE           CNT-LINE2
                               PERFORM  UNTIL   CNT-LINE3  >  22
                                    ADD     1     TO   CNT-LINE2
                                    IF      WRK-HEN-NAIYO(CNT-LINE3)
                                                               =  SPACE
                                         MOVE  SPACE
                                                TO WRK-NAIYO(CNT-LINE2)
                                    ELSE
                                         MOVE  WRK-HEN-NAIYO(CNT-LINE3)
                                                TO WRK-NAIYO(CNT-LINE2)
                                    END-IF
                                    IF  WRK-HEN-NAIYO2(CNT-LINE3)
                                                               =  SPACE
                                        MOVE  SPACE
                                               TO WRK-NAIYO2(CNT-LINE2)
                                    ELSE
                                        MOVE  WRK-HEN-NAIYO2(CNT-LINE3)
                                               TO WRK-NAIYO2(CNT-LINE2)
                                    END-IF
                                    ADD     1     TO   CNT-LINE3
                               END-PERFORM
                           END-IF
                       END-IF
                       ADD          1       TO         CNT-LINE2
                       IF  WRK-TYU-NAIYO(IDX-TYU CNT-LINE)  =  SPACE
                           CONTINUE
                       ELSE
                           MOVE     WRK-TYU-NAIYO(IDX-TYU CNT-LINE)
                                       TO  WRK-NAIYO(CNT-LINE2)
                       END-IF
                       IF  WRK-TYU-NAIYO2(IDX-TYU CNT-LINE)  =  SPACE
                           CONTINUE
                       ELSE
                           MOVE     WRK-TYU-NAIYO2(IDX-TYU CNT-LINE)
                                       TO  WRK-NAIYO2(CNT-LINE2)
                       END-IF
                   ELSE
                       IF  WRK-TYU-NAIYO(IDX-TYU CNT-LINE)  =  SPACE
                           CONTINUE
                       ELSE
                           MOVE    WRK-TYU-NAIYO(IDX-TYU CNT-LINE)
                                       TO  WRK-NAIYO(CNT-LINE2)
                       END-IF
                       IF  WRK-TYU-NAIYO2(IDX-TYU CNT-LINE)  =  SPACE
                           CONTINUE
                       ELSE
                           MOVE    WRK-TYU-NAIYO2(IDX-TYU CNT-LINE)
                                       TO  WRK-NAIYO2(CNT-LINE2)
                       END-IF
                   END-IF
           END-PERFORM
           .
       400-PRT-EDIT-TYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力編集処理２（注射）
      *****************************************************************
       400-PRT-TYU-SEC                SECTION.
      *
           MOVE    "注射"               TO  RA03-HUKUYO
      *
           INITIALIZE CNT-LINE5
      *
           PERFORM VARYING CNT-LINE5     FROM    1   BY  1
                   UNTIL   CNT-LINE5     >   CNT-LINE2
                   IF      WRK-NAIYO(CNT-LINE5)    =  SPACE
                           CONTINUE
                   ELSE
                           MOVE    WRK-NAIYO(CNT-LINE5)
                                        TO  RA03-NAIYO(CNT-LINE5)
                   END-IF
      *
                   IF      WRK-NAIYO2(CNT-LINE5)    =  SPACE
                           CONTINUE
                   ELSE
                           MOVE    WRK-NAIYO2(CNT-LINE5)
                                       TO  RA03-NAIYO2(CNT-LINE5)
                   END-IF
      *
           END-PERFORM
      *
           PERFORM  400-PRT-WRITE-SEC
           .
       400-PRT-TYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       400-PRT-WRITE-SEC                SECTION.
      *
           DISPLAY "--- 400-PRT-WRITE-SEC ---"
      *
           ADD     1                   TO  IDX-PRINT
      *
      *    印刷用ファイル作成
           MOVE    SPA-HOSPNUM     TO  HC01PARA-HOSPNUM
           MOVE    "HC62"          TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID      TO  HC01PARA-TERMID
           MOVE    SYS-TIME        TO  HC01PARA-SYOHMS
           MOVE    ORCHC62-CNT     TO  HC01PARA-CNT(1:3)
           MOVE    CNT-PAGE        TO  HC01PARA-CNT(4:3)
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"           USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
           OPEN    OUTPUT              PRT-FILE
           MOVE    SPACE           TO  PRT-REC
      *
      *    印刷用データ出力
           MOVE    RA03           TO  PRT-PRTDATA
           WRITE                       PRT-REC
      *
           CLOSE                       PRT-FILE
      *
           DISPLAY "ASGNPARA          ="ASGNPARA
      *
      *    帳票印刷処理
           INITIALIZE                  ORCSMKPRTAREA
           MOVE    "HC01.sh"           TO  MKPRT-ID
           MOVE    "A00000RA03.red"    TO  MKPRT-DIA
           MOVE    SPACE               TO  MKPRT-DEF
           MOVE    ASGNPARA            TO  MKPRT-DAT
           MOVE    WRK-PARA-PRINTNAME  TO  MKPRT-PRTNM
           MOVE    "薬袋シール"        TO  MKPRT-INFO
           CALL    "ORCSMKPRTSITE1"    USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       400-PRT-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＨＥＡＤセット処理
      *****************************************************************
       410-HEAD-SET-SEC            SECTION.
      *
           INITIALIZE                      RA03
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
      *    日付セット
      *     MOVE    SPA-SRYYMD          TO  WRK-SYMD
           MOVE    WRK-PARA-SRYYMD     TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  RA03-SRYYMD
      *
      *    患者番号
           MOVE    WRK-PARA-PTNUM      TO  RA03-PTNUM
      *
      *    患者氏名
           MOVE        SPACE           TO  WRK-PTNAME
           STRING      SPA-NAME            DELIMITED  BY  SPACE
                       "　様"              DELIMITED  BY  SIZE
                                 INTO      WRK-PTNAME
           END-STRING
                                
           IF  WRK-PTNAME(15:3)    =   SPACE
               MOVE    WRK-PTNAME  TO  RA03-NAME
           ELSE
               MOVE    WRK-PTNAME  TO  RA03-NAME2
           END-IF
      *
           .
       410-HEAD-SET-EXT.
           EXIT.
      *
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
           MOVE    LNK-DAY2-EDTYMD3
                                   TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG         REPLACING  ALL "  "  BY  "　"
      *
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
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC               SECTION.
      *
           MOVE    ZERO           TO  FLG-PRINT
      *
           READ  SORT-FILE  NEXT
             AT  END
               MOVE    1          TO   FLG-PRINT
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *
      ******************************************************************
      **     収納情報検索
      ******************************************************************
       910-SYUNOU-SELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"       TO  MCP-FUNC
           MOVE    "tbl_syunou"     TO  MCP-TABLE
           MOVE    "key21"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF  MCP-RC  =  ZERO
             PERFORM  910-SYUNOU-FETCH-SEC
           ELSE
             MOVE     1             TO  FLG-SYUNOU
           END-IF
           
           .
       910-SYUNOU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納情報検索
      *****************************************************************
       910-SYUNOU-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"          TO  MCP-FUNC
           MOVE    "tbl_syunou"       TO  MCP-TABLE
           MOVE    "key21"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    ZERO             TO  FLG-SYUNOU
             MOVE    MCPDATA-REC      TO  SYUNOU-REC
           ELSE
             MOVE    1                TO  FLG-SYUNOU
           END-IF  
           .
       910-SYUNOU-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       910-SYUNOU-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "key21"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       910-SYUNOU-CLOSE-EXT.
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
           MOVE    WRK-PARA-SRYYMD TO  SYS-1002-STYUKYMD
           MOVE    WRK-PARA-SRYYMD TO  SYS-1002-EDYUKYMD
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
      *    出力先プリンタ名割り当て情報読み込み
      *****************************************************************
       900-1031-READ-SEC           SECTION.
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                  SYS-1031-REC
           INITIALIZE                  ORCSPRTNMAREA
           MOVE    "2"             TO  ORCSPRTNM-KBN
           MOVE    WRK-PARA-SRYYMD TO  ORCSPRTNM-SRYYMD
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
           MOVE    WRK-PARA-SRYYMD TO  SYS-1901-STYUKYMD
           MOVE    WRK-PARA-SRYYMD TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(18) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
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
           MOVE    WRK-PARA-SRYYMD TO  SYS-1901-STYUKYMD
           MOVE    WRK-PARA-SRYYMD TO  SYS-1901-EDYUKYMD
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
