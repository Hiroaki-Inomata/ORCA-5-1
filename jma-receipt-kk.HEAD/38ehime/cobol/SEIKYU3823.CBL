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
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     SEIKYU3823.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 愛媛・福祉医療費支給申請・請求明細書
      *                      (123,194,195,223,294,295)
      *  管理者            :
      *  作成日付    作業者        記述
      *  05/04/18    山口          ORCAのバージョンアップ(2.5.0)による再作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01    有村    05/05/06  医療機関情報取得条件修正
      *  01.00.02    内田    06/11/21  帳票様式の変更に伴う修正
      *  01.00.03    内田    07/05/16  帳票様式の変更に伴う修正
      *  01.00.04    内田    07/09/02  日レセver4.0対応
      *                                システム管理マスタ「1901」の
      *                                医療機関情報を参照
      *  01.00.05    内田    08/02/10  月遅れ時の入院期間の印字修正
      *  01.00.06    吉川    08/09/12  SPA-AREA初期化
      *  01.00.07    内田    09/05/26  平成２１年５月特定疾患改正の対応
      *  01.00.08    内田    09/05/29  平成２１年５月特定疾患改正の対応
      *                                （対応条件に特疾・小児特定の使用有無
      *                                の判定追加）←後にパッチが出る
      *  01.00.09    吉川    10/01/19  長期が助成する分まで請求金額欄に
      *                                加算されてしまう不具合を修正
      *  01.00.10    吉川    10/09/28  前期高齢者９割＋長期の場合、
      *                                請求金額が正しくならないのを修正
      *  01.00.11    内田    10/11/26  前期高齢者９割該当者の
      *                                前期高齢者欄、給付割合欄の修正
      *  01.00.12    吉川    11/03/04  診療日数が無い場合は0を印字
      *  01.00.13    吉川    11/05/23  前期高齢者９割＋長期の請求金額の修正
      *  01.00.14    吉川    11/07/13  傷病名及び期間欄を印字する
      *                                公費負担額欄を印字する
      *                                在宅は70歳未満の場合でも印字する
      *                                総点数と全国公費分の点数が異なる場合
      *                                全国公費対象点数を備考欄に印字する
      *  01.00.15    吉川    11/09/22  診療日数が０の場合、INIファイルから
      *                                備考欄に理由を記載するように修正
      *  01.00.16    吉川    11/11/18  備考欄に記載するコメントを増やす
      *  01.00.17    吉川    11/12/21  全国公費と長期を併用した場合の
      *                                公費負担額の計算を修正
      *  01.00.18    吉川    12/07/05  総点数欄の桁数増加
      *  01.00.19    吉川    12/10/15  長期と高額を持っている場合、
      *                                限度額の低い方を特定疾患等欄に記載
      *  01.00.20    吉川    13/02/07  プラグイン対応
      *                                医療機関情報編集対応
      *  01.00.21    吉川    14/01/15  前期高齢者で全国公費と長期を併用
      *                                した場合の公費負担額の修正
      *                                プログラムオプション対応
      *  01.00.22    吉川    14/02/04  同月内に再入院した場合、備考欄に
      *                                それぞれの入院期間を印字する
      *  01.00.23    吉川    14/04/09  一部負担金が印字されない場合がある
      *                                現象を修正（FLG-HEIYOU2初期化漏れ）
      *  01.00.24    吉川    14/06/18  月末以降も入院中の場合
      *                                退院日が正しく印字されないのを修正
      *  01.00.25    吉川    14/10/27  ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.26    吉川    15/01/07  平成２７年１月改正対応
      *  01.00.27    吉川    15/06/05  点数はレセプトの総点数を記載する
      *  01.00.28    吉川    15/12/10  ３歳児視覚聴覚精密健康診査対応
      *                                医療機関編集情報への開設者名追加対応
      *  01.00.29    吉川    16/04/11  備考欄にコメントを印字する
      *  01.00.30    吉川    16/05/16  在宅コード変更（平成２８年４月改正）
      *  01.00.31    門間    17/05/15  対象公費はPGオプションから取得する
      *                                医療機関コード変更時、
      *                                医療機関コード別の作成処理対応
      *                                診療日数欄の記載修正
      *  01.00.32    萩野    18/03/01  SITEKBN変更（動作に変更なし）
      *  01.00.33    門間    18/09/06  不要コメントを削除
      *                                備考欄の記載修正
      *  01.00.34    門間    18/11/01  備考欄の修正
      *  01.00.35    萩野    19/04/09  「特定疾病等」欄に特記事項の
      *                                内容を記載するよう修正
      *  01.00.36    吉川    19/04/12  月途中で公費を切り替えた場合の
      *                                入院期間の印字不具合修正
      *                                同内容の公費を２度以上使用した場合
      *                                請求金額が加算されない不具合を修正
      *  01.00.37    萩野    19/06/10  改元対応
      *                                特定疾病等欄の印字修正。
      *  01.00.38    門間    19/09/09  公費の助成がない場合は印字対象外とする
      *  01.00.39    萩野    20/01/09  自己負担月額欄は全国公費併用時自己負担額がない場合は
      *                                空白ではなく「０」と印字するよう修正。
      *****************************************************************
      *
       ENVIRONMENT   DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
      *
      *    請求書用ファイル
           SELECT  MF100-FILE   ASSIGN    MF100PARA
                               ORGANIZATION    IS  INDEXED
                               ACCESS  MODE    IS  DYNAMIC
                               RECORD  KEY     IS  MF100-KEY
                               FILE    STATUS  IS  STS-MF100.
      *
006900*    エラーファイル
           SELECT  RECEERR-FILE ASSIGN  RECEERR
                                FILE    STATUS  IS  STS-RECEERR.
      *
       DATA  DIVISION.
       FILE  SECTION.
      *
      *
      *    集計ファイル
       FD  MF100-FILE.
       01  MF100-REC.
           COPY  "SEI3823.INC".
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
       WORKING-STORAGE  SECTION.
      *
      *
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF100//.
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
           COPY  "SC3823.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF100   PIC X(02).
           03  STS-RECE02  PIC X(02).
           03  STS-RECEERR  PIC X(02).
           03  STS-COMMENT PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF      PIC 9(01).
           03  FLG-END        PIC 9(01).
           03  FLG-SYUNOU     PIC 9(01).
           03  FLG-HEIYOU     PIC 9(01).
           03  FLG-MF100      PIC 9(01).
           03  FLG-TYOKI      PIC 9(01).
           03  FLG-PRINT      PIC 9(01).
           03  FLG-SYSKANRI   PIC 9(01).
           03  FLG-HKNCOMBI   PIC 9(01).
           03  FLG-PTNYUINRRK PIC 9(01).
           03  FLG-ERR        PIC 9(01).
           03  FLG-BTPARA     PIC 9(01).
           03  FLG-SYORI      PIC 9(01).
           03  FLG-TYOKIFTN   PIC 9(01).
           03  FLG-TOKUREI    PIC 9(01).
           03  FLG-PTBYOMEI   PIC 9(01).
           03  FLG-CHK        PIC 9(01).
           03  FLG-SRYACT     PIC 9(01).
           03  FLG-TENSU      PIC 9(01).
           03  FLG-COMMENT    PIC 9(01).
           03  FLG-PROC       PIC 9(01).
           03  FLG-HEIYOU2    PIC 9(01).
           03  FLG-PRGOPTION       PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-TAISYO     PIC 9(01).
           03  FLG-RECE10     PIC 9(01).
           03  FLG-PTCOM      PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX            PIC 9(05).
           03  IDXX           PIC 9(04).
           03  IDX1           PIC 9(04).
           03  IDX2           PIC 9(04).
           03  IDX3           PIC 9(04).
           03  IDX4           PIC 9(04).
           03  IDX5           PIC 9(04).
           03  IDY            PIC 9(04).
           03  IDZ            PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYORIFLG   PIC X(01).
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR           PIC  X(200).
           03  WRK-Z2                PIC ZZ.
           03  WRK-Z2-9              PIC Z9.
           03  WRK-Z7                PIC ZZZ,ZZZ.
           03  WRK-Z7-9              PIC ZZZ,ZZ9.
           03  WRK-Z9                PIC Z,ZZZ,ZZZ.
           03  WRK-Z10               PIC ZZ,ZZZ,ZZZ.
           03  WRK-SRYYMWH           PIC X(16).
           03  WRK-SEIYMDWH          PIC X(22).
           03  WRK-HENYMDG           PIC X(22).
           03  WRK-HENYMDG1          PIC X(09).
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-LASTYMD           PIC 9(08).
           03  WRK-NYMD.
               05  WRK-NYY           PIC 9(04).
               05  WRK-NMM           PIC 9(02).
               05  WRK-NDD           PIC 9(02).
           03  WRK-TYMD.
               05  WRK-TYY           PIC 9(04).
               05  WRK-TMM           PIC 9(02).
               05  WRK-TDD           PIC 9(02).
           03  TBL-MINNYMD.
               05  WRK-MINNYMD  OCCURS 5.
                   07  WRK-MINNYY    PIC 9(04).
                   07  WRK-MINNMM    PIC 9(02).
                   07  WRK-MINNDD    PIC 9(02).
           03  TBL-MINTYMD.
               05  WRK-MINTYMD  OCCURS 5.
                   07  WRK-MINTYY    PIC 9(04).
                   07  WRK-MINTMM    PIC 9(02).
                   07  WRK-MINTDD    PIC 9(02).
           03  WRK-NYUINKIKAN.
               05  WRK-NYUMM         PIC X(04).
               05  WRK-NYUDD         PIC X(02).
               05  WRK-TAIDD         PIC X(02).
           03  WRK-HENNYUTAIYMD OCCURS 5.
               05  WRK-HENNYMDG      PIC X(22).
               05  WRK-HENTYMDG      PIC X(22).
           03  WRK-HKNCOMBINUM       PIC X(04).
           03  WRK-HKNJANUM          PIC X(06).
           03  WRK-HKNJANAME         PIC X(20).
           03  WRK-DAYX.
               05  WRK-DAY9          PIC 9(02).
           03  WRK-TAIHI-AREA.
               05  WRK-KOHNUM        PIC X(03).
               05  WRK-HBTNUM        PIC X(02).
               05  WRK-KOHID         PIC 9(10).
               05  WRK-KOBETUCITYNUM PIC X(03).
               05  WRK-FTNJANUM      PIC X(08).
               05  WRK-SEIKYUGK      PIC 9(09)V9(01).
               05  WRK-SEIKYUGK-G    PIC 9(09)V9(01).
               05  WRK-SEIKYUGK-G2   PIC 9(09).
               05  WRK-JIKOFTNGK     PIC 9(09)V9(01).
               05  WRK-JIKOFTNGK-G   PIC 9(09)V9(01).
               05  WRK-JIKOFTNGK-G2  PIC 9(09).
               05  WRK-KOHFTNGK      PIC 9(09)V9(01).
               05  WRK-KOHFTNGK-G    PIC 9(09)V9(01).
               05  WRK-KOHFTNGK-G2   PIC 9(09).
               05  WRK-TAHOTEN       PIC 9(09).
               05  WRK-TAHOTEN-G     PIC 9(09).
           03  WRK-KEISAN            PIC 9(09).
      *
           03  WRK-PATH              PIC  X(50).
      *
           03  WRK-OPTION-TBL.
               05  WRK-OPTION      PIC X(100)  OCCURS 100.
           03  WRK-PRGID           PIC X(25).
           03  WRK-KBNCD           PIC X(16).
      *
           03  WRK-CHK-KOHNUM-TBL.
               05  WRK-CHK-KOHNUM    PIC X(03)  OCCURS 4.
      *    対象公費
           03  WRK-TAISYO-KOHNUM     PIC X(03)  OCCURS 100.
      *
      *    医療機関情報
           03  WRK-HOSPNAME          PIC X(100).
           03  WRK-HOSPNAME-R        REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1     PIC X(50).
               05  WRK-HOSPNAME2     PIC X(50).
           03  WRK-HOSPADRS          PIC X(100).
           03  WRK-HOSPADRS-R        REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1     PIC X(50).
               05  WRK-HOSPADRS2     PIC X(50).
           03  WRK-HOSPTEL           PIC X(15).
           03  WRK-KAISETUNAME       PIC X(80).
      *
           03  WRK-TOKUTEI           PIC 9(01).
           03  WRK-COMMENTCD         PIC X(09).
           03  WRK-COMMENT           PIC X(40).
           03  WRK-SRYCD             PIC X(09).
           03  WRK-BIKOHCD           PIC X(09).
      *
      *    文字変換
           03  WRK-MAE-INPUT         PIC X(200).
           03  WRK-OUT-INPUT         PIC X(200).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02  PIC 9(06).
           03  CNT-MF100   PIC 9(06).
           03  CNT-PAGE    PIC 9(03).
      *
       01  TABLE-AREA.
           03  TBL-PARA-OCC    OCCURS  100.
               05  TBL-PARA-HKNJANUM    PIC X(08).
               05  TBL-PARA-PTID        PIC 9(10).
      *
      *    コメントテーブル領域
       01  TBL-COMMENT-REC.
           03  TBL-COMMENTCD            PIC X(09)  OCCURS  10.
      *
       01  KEY-AREA                        VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-SRYYMD         PIC 9(08).
           03  KEY-OLD.
               05  KEY-O-SRYYMD         PIC 9(08).
      *
       01  WRK-CONS-PARA-MAX   PIC 9(03)   VALUE   100.
      *
      *    固定項目
       01  TBL-ZAITAKU-AREA.
      *    在宅総合・在宅末期（在医総）
           03  FILLER          PIC X(09)   VALUE   "114007610".
           03  FILLER          PIC X(09)   VALUE   "114007710".
           03  FILLER          PIC X(09)   VALUE   "114007510".
           03  FILLER          PIC X(09)   VALUE   "114012210".
           03  FILLER          PIC X(09)   VALUE   "114012310".
           03  FILLER          PIC X(09)   VALUE   "114012410".

       01  TBL-ZAITAKU-R      REDEFINES   TBL-ZAITAKU-AREA.
            03  TBL-ZAITAKU   PIC X(09)   OCCURS  6.
      *
       01  TBL-ZAITAKU-MAX    PIC 9(02)   VALUE   6.
      *
      *    在総診テーブル（２０１６年４月診療分〜）
       01  TBL-ZAITAKU2-AREA.
      *    在がん医総（在支診等）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114007610".
      *    在がん医総（在支診等）（処方せんなし）
           03  FILLER          PIC X(09)   VALUE   "114007710".
      *    在医総・在医総管（ＤＵＭＭＹ）
           03  FILLER          PIC X(09)   VALUE   "099140003".
      *
      *    在がん医総（機能強化した在支診等）（病床あり）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114019510".
      *    在がん医総（機能強化した在支診等）（病床あり）（処方せんなし）
           03  FILLER          PIC X(09)   VALUE   "114019610".
      *    在がん医総（機能強化した在支診等）（病床なし）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114019710".
      *    在がん医総（機能強化した在支診等）（病床なし）（処方せんなし）
           03  FILLER          PIC X(09)   VALUE   "114019810".
      *
      *    在医総管（機能強化在支診等・病床有・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114030710".
      *    在医総管（機能強化在支診等・病床有・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114030810".
      *    在医総管（機能強化在支診等・病床有・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114030910".
      *    在医総管（機能強化在支診等・病床有・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114031010".
      *    在医総管（機能強化在支診等・病床有・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114031110".
      *    在医総管（機能強化在支診等・病床有・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114031210".
      *    在医総管（機能強化在支診等・病床有・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114031310".
      *    在医総管（機能強化在支診等・病床有・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114031410".
      *    在医総管（機能強化在支診等・病床有・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114031510".
      *    在医総管（機能強化在支診等・病床無・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114031610".
      *    在医総管（機能強化在支診等・病床無・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114031710".
      *    在医総管（機能強化在支診等・病床無・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114031810".
      *    在医総管（機能強化在支診等・病床無・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114031910".
      *    在医総管（機能強化在支診等・病床無・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114032010".
      *    在医総管（機能強化在支診等・病床無・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114032110".
      *    在医総管（機能強化在支診等・病床無・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114032210".
      *    在医総管（機能強化在支診等・病床無・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114032310".
      *    在医総管（機能強化在支診等・病床無・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114032410".
      *    在医総管（在支診等・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114032510".
      *    在医総管（在支診等・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114032610".
      *    在医総管（在支診等・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114032710".
      *    在医総管（在支診等・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114032810".
      *    在医総管（在支診等・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114032910".
      *    在医総管（在支診等・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114033010".
      *    在医総管（在支診等・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114033110".
      *    在医総管（在支診等・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114033210".
      *    在医総管（在支診等・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114033310".
      *    在医総管（在支診等以外・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114033410".
      *    在医総管（在支診等以外・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114033510".
      *    在医総管（在支診等以外・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114033610".
      *    在医総管（在支診等以外・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114033710".
      *    在医総管（在支診等以外・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114033810".
      *    在医総管（在支診等以外・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114033910".
      *    在医総管（在支診等以外・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114034010".
      *    在医総管（在支診等以外・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114034110".
      *    在医総管（在支診等以外・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114034210".
      *
      *    施医総管（機能強化在支診等・病床有・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114035510".
      *    施医総管（機能強化在支診等・病床有・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114035610".
      *    施医総管（機能強化在支診等・病床有・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114035710".
      *    施医総管（機能強化在支診等・病床有・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114035810".
      *    施医総管（機能強化在支診等・病床有・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114035910".
      *    施医総管（機能強化在支診等・病床有・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114036010".
      *    施医総管（機能強化在支診等・病床有・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114036110".
      *    施医総管（機能強化在支診等・病床有・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114036210".
      *    施医総管（機能強化在支診等・病床有・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114036310".
      *    施医総管（機能強化在支診等・病床無・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114036410".
      *    施医総管（機能強化在支診等・病床無・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114036510".
      *    施医総管（機能強化在支診等・病床無・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114036610".
      *    施医総管（機能強化在支診等・病床無・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114036710".
      *    施医総管（機能強化在支診等・病床無・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114036810".
      *    施医総管（機能強化在支診等・病床無・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114036910".
      *    施医総管（機能強化在支診等・病床無・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114037010".
      *    施医総管（機能強化在支診等・病床無・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114037110".
      *    施医総管（機能強化在支診等・病床無・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114037210".
      *    施医総管（在支診等・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114037310".
      *    施医総管（在支診等・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114037410".
      *    施医総管（在支診等・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114037510".
      *    施医総管（在支診等・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114037610".
      *    施医総管（在支診等・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114037710".
      *    施医総管（在支診等・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114037810".
      *    施医総管（在支診等・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114037910".
      *    施医総管（在支診等・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114038010".
      *    施医総管（在支診等・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114038110".
      *    施医総管（在支診等以外・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114038210".
      *    施医総管（在支診等以外・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114038310".
      *    施医総管（在支診等以外・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114038410".
      *    施医総管（在支診等以外・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114038510".
      *    施医総管（在支診等以外・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114038610".
      *    施医総管（在支診等以外・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114038710".
      *    施医総管（在支診等以外・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114038810".
      *    施医総管（在支診等以外・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114038910".
      *    施医総管（在支診等以外・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114039010".
      *
       01  TBL-ZAITAKU2-R      REDEFINES   TBL-ZAITAKU2-AREA.
            03  TBL-ZAITAKU2   PIC X(09)   OCCURS  79.
      *
       01  TBL-ZAITAKU2-MAX    PIC 9(02)   VALUE   79.
      *
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-201501   PIC X(06) VALUE "201501".
           03  WRK-CONS-SRYYM-201604   PIC X(06) VALUE "201604".
           03  WRK-CONS-SRYYM-201808   PIC X(06) VALUE "201808".
      *
      *    DB参照用
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト　領域
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY  "CPSK1002.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *
      *    医療機関住所編集情報（２）
           COPY    "CPSK1903.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *    印刷管理マスタ
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *    印刷マスタ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    入院履歴情報
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *    プログラムオプション
       01  PRGOPTION-REC.
           COPY    "CPPRGOPTION.INC".
      *
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    共通パラメタ
           COPY  "MCPAREA".
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY  "MCPDATA.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE  SECTION.
       01  COMMAND-PARAM.
           02  FILLER  PIC X(256).
      *****************************************************************
      *
       PROCEDURE  DIVISION
               USING COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC  SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL  FLG-END = 1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC  SECTION.
      *
           INITIALIZE  FLG-AREA
           INITIALIZE  STS-AREA
           INITIALIZE  WRK-AREA
           INITIALIZE  CNT-AREA
           INITIALIZE  IDX-AREA
           INITIALIZE  SPA-AREA
           INITIALIZE  RECEERR
      *
           DISPLAY "START"
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID
                                               LNK-PRTKANRI-PRTNM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-SYORIFLG
                                               WRK-PARA-STSRYYM
                                               WRK-PARA-EDSRYYM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SEIKYU3823"    TO  JOB-PGID
           MOVE    "福祉医療費支給・請求明細書"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    LNK-PRTKANRI-SRYYM  TO    WRK-SYMD(1:6)
           PERFORM 31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD         TO    WRK-LASTYMD
      *
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG1
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-SRYYMWH
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    LNK-PRTKANRI-SKYYMD     TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-SEIYMDWH
           END-IF
      *
           INITIALIZE                     SGETTEMP-AREA
      *
           MOVE  WRK-PARA-HOSPNUM     TO  MF100PARA-HOSPNUM
           MOVE  "MF100ZZ"            TO  MF100PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID  TO  MF100PARA-TERMID
      *
           MOVE  MF100PARA-BASENAME   TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEERR              TO  SGETTEMP-BASENAMES (2)
           CALL  "ORCSGETTEMP"        USING
                                      SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  MF100PARA-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEERR
      *
      *    医療機関ＩＤ編集
           INITIALIZE  SYS-1001-REC
           MOVE  "1001"        TO  SYS-1001-KANRICD
           MOVE  "*"           TO  SYS-1001-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"        TO  SYS-1001-STYUKYMD(7:2)
                                   SYS-1001-EDYUKYMD(7:2)
      *
           MOVE  SPA-HOSPNUM       TO  SYS-1001-HOSPNUM
           MOVE  SYS-1001-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU3823 SYS 1001 ERR ***"
             MOVE  1            TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE  SYS-1002-REC
           MOVE  "1002"        TO  SYS-1002-KANRICD
           MOVE  "*"           TO  SYS-1002-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"        TO  SYS-1002-STYUKYMD(7:2)
                                   SYS-1002-EDYUKYMD(7:2)
      *
           MOVE  SPA-HOSPNUM       TO  SYS-1002-HOSPNUM
           MOVE  SYS-1002-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU3823 SYS 1002 ERR ***"
             MOVE  1            TO  FLG-END
           END-IF
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1001-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
      *    パラメタ編集処理
           PERFORM 1001-PARA-HENSYU-SEC
      *
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *
           PERFORM 900-KOHSKY-START-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       1001-PARA-HENSYU-SEC        SECTION.
      *
           MOVE    ZERO                    TO  IDXX
                                           FLG-SYORI
           INITIALIZE                      TABLE-AREA
      *
           IF      WRK-PARA-SYORIFLG   =   "1"
               INITIALIZE                      BTPARA-REC
               MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
               MOVE    WRK-PARA-SHELLID    TO  BTPARA-SHELLID
               MOVE    WRK-PARA-JOBID      TO  BTPARA-JOBID
               MOVE    BTPARA-REC          TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_btpara"    TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM 900-BTPARA-READ-N-SEC
               ELSE
                   INITIALIZE                  BTPARA-REC
                   MOVE    1               TO  FLG-BTPARA
               END-IF
      *
               PERFORM     UNTIL   FLG-BTPARA  =   1
                   ADD     1                   TO  IDXX
                   IF      BTPARA-INFO-KBN     =   "1"
                     MOVE    1                 TO  FLG-SYORI
                     MOVE    BTPARA-INFO-PARA
                                       TO TBL-PARA-HKNJANUM(IDXX)
                   ELSE
                     MOVE    2                 TO  FLG-SYORI
                     MOVE    BTPARA-INFO-PTID
                                       TO TBL-PARA-PTID(IDXX)
                   END-IF
                   DISPLAY "パラメタ区分=" BTPARA-INFO-KBN
                           "パラメタ=" BTPARA-INFO-PARA
                           "保険者番号="   TBL-PARA-HKNJANUM(IDXX)
                           "患者番号="     TBL-PARA-PTID(IDXX)
      *
                   MOVE    "tbl_btpara"    TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM 900-BTPARA-READ-N-SEC
               END-PERFORM
      *
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
               IF      IDXX            =   ZERO
                   MOVE    ZERO            TO  WRK-PARA-SYORIFLG
               END-IF
           END-IF
           .
       1001-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           MOVE    "SEIKYU3823"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
      *    備考欄コメントコード
           MOVE    "COMMENT"                 TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  10
             IF  WRK-OPTION(IDX)(1:2)  =  "※"
               MOVE    SPACE                 TO  TBL-COMMENTCD(IDX)
             ELSE
               MOVE    WRK-OPTION(IDX)(1:9)  TO  TBL-COMMENTCD(IDX)
               DISPLAY "コメントコード" IDX "="  TBL-COMMENTCD(IDX)
             END-IF
           END-PERFORM
      *
      *    備考のコメント設定
           MOVE    "BIKOH"                   TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           IF  WRK-OPTION(1)(1:2)  =  "※"
             MOVE    SPACE                   TO  WRK-BIKOHCD
           ELSE
             MOVE    WRK-OPTION(1)(1:9)      TO  WRK-BIKOHCD
           END-IF
           DISPLAY "備考コメントコード =" WRK-BIKOHCD
      *
      *    対象公費番号
           MOVE    "TAISYOKOHI"              TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  100
                    OR       WRK-OPTION(IDX)  =  SPACE
             MOVE    WRK-OPTION(IDX)(1:3)    TO  WRK-TAISYO-KOHNUM(IDX)
             DISPLAY "対象公費番号="  WRK-TAISYO-KOHNUM(IDX)
           END-PERFORM
      *
           .
       100-OPTION-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプションパラメタセット処理
      *****************************************************************
       1001-OPTION-SET-SEC              SECTION.
      *
           INITIALIZE                            WRK-OPTION-TBL
                                                 FLG-OK
      *    プログラムオプション情報読込
           PERFORM  900-PRGOPTION-READ-SEC
      *
      *    オプション数の初期設定
           MOVE    1                         TO  IDX1
           IF  FLG-PRGOPTION  =  ZERO
             PERFORM  VARYING  IDX  FROM  1  BY  1
                      UNTIL   (IDX     >   44000 )
                         OR   (IDX1    >  100)
                         OR   (FLG-OK  NOT =   ZERO)
      *
      *        オプションの1桁目が空白であれば処理終了
               IF   PRGOPTION-OPTION(IDX:1)  =  SPACE
               AND  IDY                      =  ZERO
                   MOVE    1                 TO  FLG-OK
               END-IF
      *
               IF  FLG-OK  =  ZERO
      *          改行地点か
                 IF  PRGOPTION-OPTION(IDX:1)  =   X"0A"
                 OR  IDY  >=  100
                   ADD    1                  TO  IDX1
                   MOVE    ZERO              TO  IDY
                 ELSE
                   ADD    1                  TO  IDY
      *            PGオプション内容を１文字ずつセット
                   MOVE    PRGOPTION-OPTION(IDX:1)
                                         TO  WRK-OPTION(IDX1)(IDY:1)
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      *
           .
       1001-OPTION-SET-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC  SECTION.
      *
           OPEN    OUTPUT  MF100-FILE
           CLOSE           MF100-FILE
           OPEN    I-O     MF100-FILE
      *
           MOVE    ZERO    TO      FLG-END
      *
           PERFORM 2001-SYUKEI-SEC
                   UNTIL   FLG-END     =   1
      *
           CLOSE   MF100-FILE
      *
           IF  CNT-RECE02  >       0
               PERFORM   2002-PRINT-SEC
           END-IF
           MOVE    1   TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       2001-SYUKEI-SEC  SECTION.
      *
      *
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       4
               INITIALIZE                            WRK-CHK-KOHNUM-TBL
               MOVE    RECE02-KOHNUM(IDZ)        TO  WRK-CHK-KOHNUM(1)
      *        対象公費番号チェック処理
               PERFORM  800-TAISYOKOHI-CHK-SEC
               IF  FLG-TAISYO  =  1
                   PERFORM 20011-SYUKEI-SEC
               END-IF
           END-PERFORM
      *
           PERFORM  900-KOHSKY-READ-SEC
      *
           .
       2001-SYUKEI-EXT.
           EXIT.
      **
      *****************************************************************
      *    集計処理２
      *****************************************************************
       20011-SYUKEI-SEC  SECTION.
      *
           INITIALIZE      WRK-TAIHI-AREA
           INITIALIZE      MF100-REC
      *
           MOVE    SPA-HOSPNUM               TO  MF100-HOSPNUM
           MOVE    RECE02-PTNUM              TO  MF100-PTNUM
           MOVE    RECE02-RECEKA             TO  MF100-SRYKA
           MOVE    RECE02-HKNJANUM-KEY       TO  MF100-HKNJANUM-KEY
           MOVE    RECE02-TEKSTYMD           TO  MF100-TEKSTYMD
           MOVE    RECE02-NYUGAIKBN          TO  MF100-NYUGAIKBN
           MOVE    RECE02-PTID               TO  MF100-PTID
           MOVE    RECE02-NAME               TO  MF100-NAME
           MOVE    RECE02-AGE                TO  MF100-AGE
           MOVE    RECE02-BIRTHDAY           TO  MF100-BIRTHDAY
           MOVE    RECE02-ZAITAKU            TO  MF100-ZAITAKU
           MOVE    RECE02-HKN                TO  MF100-HKN
           MOVE    RECE02-RJNHKN             TO  MF100-RJNHKN
           MOVE    MF100-RJNFTNJANUM         TO  MF100-CITYNUM
           MOVE    RECE02-SRYYM              TO  MF100-SRYYM
           MOVE    RECE02-SEX                TO  MF100-SEX
           MOVE    RECE02-KOHINF      (IDZ)  TO  MF100-KOHINF
           MOVE    MF100-KOHFTNJANUM         TO  MF100-FTNJANUM
           COMPUTE IDY     =       IDZ     +   1
           MOVE    RECE02-SHOKUJIINF  (IDY)  TO  MF100-SHOKUJIINF
           MOVE    RECE02-FTNMONEY    (IDY)  TO  MF100-FTNMONEY
           MOVE    RECE02-YKZFTN      (1)    TO  MF100-YKZFTN
           MOVE    RECE02-KOHFTNJANUM(IDZ)(5:3)
                                             TO  WRK-KOBETUCITYNUM
      *    ソートKEY設定
           PERFORM  20012-SORT-SEC
      *
      *    一時ファイル読み込み
           PERFORM  800-SKTTMP-READ-SEC
      *
           MOVE    RECE02-KOHINF      (IDZ)  TO  MF100-KOHINF
      *
      *    請求管理から特記事項参照
           PERFORM 900-SEIKYU-READ-SEC
           MOVE    RECE10-TOTALTEN(1)        TO  MF100-TOTALTEN
           MOVE    RECE10-JNISSU(1)          TO  MF100-JNISSU
      *
      *    収納情報より請求金額、自己負担月額を集計
           MOVE    MF100-KOHNUM    TO  WRK-KOHNUM
           MOVE    MF100-KOHID     TO  WRK-KOHID
           MOVE    ZERO            TO  FLG-TYOKI
                                       WRK-SEIKYUGK-G
                                       WRK-JIKOFTNGK-G
                                       WRK-KOHFTNGK-G
                                       WRK-TAHOTEN-G
           IF      RECE02-NYUGAIKBN    =   "1"
               MOVE    "key33"             TO  WRK-PATH
           ELSE
               MOVE    "key25"             TO  WRK-PATH
           END-IF
      *
           MOVE    1                  TO  FLG-ERR
      *
           PERFORM 920-SYUNOU-SET-SEC
      *
      *    全国公費を併用している場合、特例措置計算フラグを無効にする
           IF  FLG-HEIYOU2  =  1
             MOVE    ZERO                  TO  FLG-TOKUREI
           END-IF
      *
           MOVE  ZERO                          TO  FLG-PROC
      *
      *    保険者番号チェック処理
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
           IF    FLG-ERR               =   ZERO
      *
             COMPUTE WRK-SEIKYUGK-G2     =   WRK-SEIKYUGK-G  +   0.5
             COMPUTE WRK-JIKOFTNGK-G2    =   WRK-JIKOFTNGK-G +   0.5
             COMPUTE WRK-KOHFTNGK-G2     =   WRK-KOHFTNGK-G  +   0.5
      *
      *    保険限度額設定
             IF  RECE02-FTNMONEY(1)  NOT =   ZERO    AND
                 RECE02-FTNMONEY(1)      <   WRK-SEIKYUGK-G2
                 MOVE    RECE02-FTNMONEY(1)  TO    WRK-SEIKYUGK-G2
             END-IF
      *
             IF  FLG-TYOKI       =   1
                 MOVE    1       TO  MF100-TYOKI
                 IF  FLG-TOKUREI  =  1
                   COMPUTE  WRK-SEIKYUGK-G2  =
                            RECE02-TOTALTEN(1) * RECE02-KYURATE / 10
                 END-IF
      *    長期を併用しており、患者負担月額が１万を越えている場合は１万を設定
                 IF  WRK-JIKOFTNGK-G2    >   10000
                     MOVE    10000       TO  WRK-JIKOFTNGK-G2
                 END-IF
      *    長期を併用しており、請求金額が１万を越えている場合は１万を設定
                 IF  WRK-SEIKYUGK-G2     >  10000
                     MOVE    10000       TO  WRK-SEIKYUGK-G2
                 END-IF
                 IF  WRK-KOHFTNGK-G2     >=  10000
                   IF  WRK-SEIKYUGK-G2     >=  10000
                       MOVE    ZERO      TO  WRK-KOHFTNGK-G2
                   ELSE
                       COMPUTE  WRK-KOHFTNGK-G2  =
                                10000  -  WRK-SEIKYUGK-G2
                   END-IF
                 END-IF
             END-IF
             IF  FLG-TYOKI       =   2
                 MOVE    2       TO  MF100-TYOKI
                 IF  FLG-TOKUREI  =  1
                   COMPUTE  WRK-SEIKYUGK-G2  =
                            RECE02-TOTALTEN(1) * RECE02-KYURATE / 10
                 END-IF
      *    長期上位を併用して、患者負担月額が２万を越えている場合は２万を設定
                 IF  WRK-JIKOFTNGK-G2    >   20000
                     MOVE    20000       TO  WRK-JIKOFTNGK-G2
                 END-IF
      *    長期上位を併用して、請求金額が２万を越えている場合は２万を設定
                 IF  WRK-SEIKYUGK-G2     >   20000
                     MOVE    20000       TO  WRK-SEIKYUGK-G2
                 END-IF
                 IF  WRK-KOHFTNGK-G2     >=  20000
                   IF  WRK-SEIKYUGK-G2     >=  20000
                       MOVE    ZERO      TO  WRK-KOHFTNGK-G2
                   ELSE
                       COMPUTE  WRK-KOHFTNGK-G2  =
                                20000  -  WRK-SEIKYUGK-G2
                   END-IF
                 END-IF
             END-IF
      *
      *    請求金額：自己負担月額設定
             ADD    WRK-SEIKYUGK-G2     TO  MF100-SEIKYUGK
             ADD    WRK-JIKOFTNGK-G2    TO  MF100-JIKOFTNGK
             ADD    WRK-KOHFTNGK-G2     TO  MF100-KOHFTNGK
      *
      *    入院患者の場合、入退院日をセット
             IF  RECE02-NYUGAIKBN = "1"
                 PERFORM 20013-YMDSET-SEC
             END-IF
      *
      *    特疾・小児の使用判定（６月末のパッチで判定不要となる予定）
           IF  WRK-TOKUTEI  =  1
             IF  RECE02-FLG (4)  =  "1"
      *
               PERFORM  VARYING  IDX1  FROM  1  BY  1
                        UNTIL    IDX1  >  5
                 EVALUATE  RECE10-TOKKI(IDX1)
                   WHEN  "17"    MOVE  3            TO  MF100-TYOKI2
                   WHEN  "18"    MOVE  4            TO  MF100-TYOKI2
                   WHEN  "19"    MOVE  5            TO  MF100-TYOKI2
                   WHEN  "22"    MOVE  "「２２」"   TO  MF100-BIKO
                   WHEN  "23"    MOVE  "「２３」"   TO  MF100-BIKO
                   WHEN  "24"    MOVE  "「２４」"   TO  MF100-BIKO
                   WHEN  "31"    MOVE  "「３１」"   TO  MF100-BIKO
                   WHEN  "32"    MOVE  "「３２」"   TO  MF100-BIKO
                   WHEN  "33"    MOVE  "「３３」"   TO  MF100-BIKO
                   WHEN  "34"    MOVE  "「３４」"   TO  MF100-BIKO
                   WHEN  "35"    MOVE  "「３５」"   TO  MF100-BIKO
                 END-EVALUATE
               END-PERFORM
             END-IF
           END-IF
      *
      *    高額療養費チェック
             IF  RECE02-SRYYM  >=  WRK-CONS-SRYYM-201501
      *          高額療養費チェック(平成３０年８月から)
                 IF  (RECE02-SRYYM  >=  WRK-CONS-SRYYM-201808)
                     MOVE  ZERO                  TO  MF100-TYOKI
                     MOVE  ZERO                  TO  MF100-TYOKI2
                     PERFORM  VARYING  IDX3  FROM  1  BY  1
                              UNTIL    IDX3  >  5
                              OR       RECE10-TOKKI(IDX3)  =  SPACE
                         EVALUATE  RECE10-TOKKI(IDX3)
                           WHEN  "02"
                               MOVE    1         TO  MF100-TYOKI
                           WHEN  "16"
                               MOVE    2         TO  MF100-TYOKI
                           WHEN  "26"
                               MOVE    6         TO  MF100-TYOKI2
                           WHEN  "27"
                               MOVE    7         TO  MF100-TYOKI2
                           WHEN  "28"
                               MOVE    8         TO  MF100-TYOKI2
                           WHEN  "29"
                               MOVE    9         TO  MF100-TYOKI2
                           WHEN  "30"
                               MOVE    10        TO  MF100-TYOKI2
                           WHEN  "31"
                               MOVE  "「３１」"  TO  MF100-BIKO
                           WHEN  "32"
                               MOVE  "「３２」"  TO  MF100-BIKO
                           WHEN  "33"
                               MOVE  "「３３」"  TO  MF100-BIKO
                           WHEN  "34"
                               MOVE  "「３４」"  TO  MF100-BIKO
                           WHEN  "35"
                               MOVE  "「３５」"  TO  MF100-BIKO
                         END-EVALUATE
                     END-PERFORM
                 ELSE
                     EVALUATE  RECE02-ZAITAKU
                       WHEN  "2"    MOVE  6      TO  MF100-TYOKI2
                       WHEN  "3"    MOVE  7      TO  MF100-TYOKI2
                       WHEN  "4"    MOVE  8      TO  MF100-TYOKI2
                       WHEN  "5"    MOVE  9      TO  MF100-TYOKI2
                       WHEN  "6"    MOVE  10     TO  MF100-TYOKI2
                     END-EVALUATE
                 END-IF
             ELSE
                 EVALUATE  RECE02-ZAITAKU
                   WHEN  "2"    MOVE  3      TO  MF100-TYOKI2
                   WHEN  "3"    MOVE  4      TO  MF100-TYOKI2
                   WHEN  "4"    MOVE  5      TO  MF100-TYOKI2
                 END-EVALUATE
             END-IF
      *
      *
      *      保険助成額後の患者負担が21000円以上の場合
      *      傷病名及び期間を印字
             COMPUTE  WRK-KEISAN  =  RECE02-TOTALTEN(1)
                                  *  (RECE02-KYURATE  /  10)
             IF  WRK-KEISAN  >=  21000
               PERFORM  900-PTBYOMEI-READ-SEC
             END-IF
      *
             DISPLAY "公費点数 = " WRK-TAHOTEN-G
             DISPLAY "福祉点数 = " MF100-TOTALTEN
             IF  WRK-TAHOTEN-G  NOT =  ZERO
             AND MF100-TOTALTEN  NOT =  WRK-TAHOTEN-G
               ADD    WRK-TAHOTEN-G     TO  MF100-TAHOTEN
             END-IF
      *
      *      在総診・在医総か
             IF  RECE02-ZAITAKU  NOT =  1
               MOVE  1                   TO  FLG-PROC
               PERFORM  960-SRYACT-SET-SEC
             END-IF
      *
      *      診療日数が０の場合、INIファイルのコードを元にコメント取得
             IF  MF100-JNISSU  =  ZERO
               MOVE  2                   TO  FLG-PROC
               PERFORM  960-SRYACT-SET-SEC
               PERFORM  900-TENSU-READ-SEC
               DISPLAY "コメント名 = " TNS-NAME
               MOVE  TNS-NAME            TO  MF100-BIKO2
             END-IF
      *
      *      ３歳児健診の場合、福祉点数と３歳児健診の点数を備考欄に記載
             PERFORM  VARYING  IDX5  FROM  1  BY  1
                      UNTIL    IDX5  >  4
                 IF  RECE02-KOHNUM(IDX5)  =  "394"
                     MOVE    RECE02-TOTALTEN(IDX5 + 1)    TO  WRK-Z7
                     MOVE    RECE02-TOTALTEN(IDY)         TO  WRK-Z7-9
                     STRING  "福祉対象点数"        DELIMITED  BY  SIZE
                             WRK-Z7-9              DELIMITED  BY  SIZE
                             "点"                  DELIMITED  BY  SIZE
                             "　　"                DELIMITED  BY  SIZE
                             "３歳児健診対象点数"  DELIMITED  BY  SIZE
                             WRK-Z7                DELIMITED  BY  SIZE
                             "点"                  DELIMITED  BY  SIZE
                             INTO                  MF100-BIKO4
                     END-STRING
                     MOVE    4           TO  IDX5
                 END-IF
             END-PERFORM
      *
      *    コメントを取得
           MOVE  WRK-BIKOHCD             TO  WRK-SRYCD
           PERFORM 960-SRYACT-KEY7-SET-SEC
           IF  FLG-SRYACT  =  ZERO
             PERFORM  VARYING  IDX4  FROM  1  BY  1
                      UNTIL    IDX4  >  5
               IF  WRK-BIKOHCD  =  SRY-SRYCD(IDX4)
      *          患者コメント情報読込
                 PERFORM 950-PTCOM-READ-SEC
                 MOVE    PTCOM-INPUTCOMENT    TO  MF100-BIKO5
               END-IF
             END-PERFORM
           END-IF
      *
             IF  FLG-MF100   =   1
                 WRITE   MF100-REC
                 ADD     1                   TO  CNT-MF100
             ELSE
                 REWRITE  MF100-REC
             END-IF
             ADD     1                   TO  CNT-MF100
      *
           ELSE
             DISPLAY "助成対象外=" RECE02-PTNUM
           END-IF
      *
           .
       20011-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ソートKEY設定処理
      *****************************************************************
       20012-SORT-SEC  SECTION.
      *
           MOVE    ZERO      TO     MF100-SORT1
                                    MF100-SORT2
           EVALUATE    MF100-FTNJANUM(1:2)
               WHEN    "81"
                   MOVE    1       TO  MF100-SORT1
               WHEN    "71"
                   MOVE    2       TO  MF100-SORT1
               WHEN    "82"
                   MOVE    3       TO  MF100-SORT1
               WHEN    "72"
                   MOVE    4       TO  MF100-SORT1
               WHEN    "83"
                   MOVE    5       TO  MF100-SORT1
               WHEN    "73"
                   MOVE    6       TO  MF100-SORT1
               WHEN    OTHER
                   MOVE    9       TO  MF100-SORT1
           END-EVALUATE
      *
           IF  RECE02-TEISYUTUSAKI     =   "1"
               IF  RECE02-RJNHKNNUM  NOT =   SPACE
                   IF  RECE02-KYURATE      =   10
                       MOVE    4           TO  MF100-SORT2
                   ELSE
                       MOVE    5           TO  MF100-SORT2
                   END-IF
               ELSE
                   IF  RECE02-AGE          <   3
                       MOVE    2           TO  MF100-SORT2
                   ELSE
                       MOVE    1           TO  MF100-SORT2
                   END-IF
               END-IF
           ELSE
               IF  RECE02-RJNHKNNUM    NOT =   SPACE
                   IF  RECE02-KYURATE      =   10
                       MOVE    6           TO  MF100-SORT2
                   ELSE
                       MOVE    7           TO  MF100-SORT2
                   END-IF
               ELSE
                   MOVE        3           TO  MF100-SORT2
               END-IF
           END-IF
      *
      *
           .
       20012-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入退院日セット処理
      *****************************************************************
       20013-YMDSET-SEC  SECTION.
      *
           INITIALIZE                     TBL-MINNYMD
                                          TBL-MINTYMD
      *
      *    診療年月の最終日を取得
           MOVE    RECE02-SRYYM     TO    WRK-SYMD(1:6)
           PERFORM 31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD         TO    WRK-LASTYMD
      *    患者入院履歴情報
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    MF100-PTID          TO  PTNYUINRRK-PTID
           MOVE    WRK-LASTYMD         TO  WRK-NYMD
                                           WRK-TYMD
           MOVE    1                   TO  WRK-TDD
           MOVE    WRK-NYMD            TO  PTNYUINRRK-NYUINYMD
           MOVE    WRK-TYMD            TO  PTNYUINRRK-TAIINYMD
           PERFORM 930-PTNYUINRRK-SELECT-SEC
           PERFORM 930-PTNYUINRRK-CLOSE-SEC
      *
      *    入院日セット
           IF    WRK-TYMD  >  WRK-MINNYMD(1)
               MOVE    WRK-TYMD        TO  MF100-NYUINYMD(1)
           ELSE
               MOVE    WRK-MINNYMD(1)  TO  MF100-NYUINYMD(1)
           END-IF
           MOVE    WRK-MINNYMD(2)      TO  MF100-NYUINYMD(2)
           MOVE    WRK-MINNYMD(3)      TO  MF100-NYUINYMD(3)
           MOVE    WRK-MINNYMD(4)      TO  MF100-NYUINYMD(4)
      *
      *    退院日セット
           IF  WRK-NYMD  <  WRK-MINTYMD(4)
               MOVE    WRK-NYMD        TO  MF100-TAIINYMD(4)
           ELSE
               MOVE    WRK-MINTYMD(4)  TO  MF100-TAIINYMD(4)
           END-IF
           IF  WRK-NYMD  <  WRK-MINTYMD(3)
               MOVE    WRK-NYMD        TO  MF100-TAIINYMD(3)
           ELSE
               MOVE    WRK-MINTYMD(3)  TO  MF100-TAIINYMD(3)
           END-IF
           IF  WRK-NYMD  <  WRK-MINTYMD(2)
               MOVE    WRK-NYMD        TO  MF100-TAIINYMD(2)
           ELSE
               MOVE    WRK-MINTYMD(2)  TO  MF100-TAIINYMD(2)
           END-IF
           IF  WRK-NYMD  <  WRK-MINTYMD(1)
               MOVE    WRK-NYMD        TO  MF100-TAIINYMD(1)
           ELSE
               MOVE    WRK-MINTYMD(1)  TO  MF100-TAIINYMD(1)
           END-IF
      *
      *    備考３（複数入院期間）
           IF  MF100-NYUINYMD(2)  NOT =  ZERO
      *
             PERFORM  VARYING  IDX3  FROM  1  BY  1
                      UNTIL    IDX3  >  4
      *        入院日和暦変換
               MOVE    MF100-NYUINYMD(IDX3)  TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG           TO  WRK-HENNYMDG(IDX3)
      *        退院日和暦変換
               MOVE    MF100-TAIINYMD(IDX3)  TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG           TO  WRK-HENTYMDG(IDX3)
             END-PERFORM
      *
      *      文字結合
             IF  MF100-NYUINYMD(4)   NOT =  ZERO
                 STRING  "入院期間："          DELIMITED  BY  SIZE
                         WRK-HENNYMDG(1)       DELIMITED  BY  SIZE
                         "〜"                  DELIMITED  BY  SIZE
                         WRK-HENTYMDG(1)       DELIMITED  BY  SIZE
                         "、"                  DELIMITED  BY  SIZE
                         WRK-HENNYMDG(2)       DELIMITED  BY  SIZE
                         "〜"                  DELIMITED  BY  SIZE
                         WRK-HENTYMDG(2)       DELIMITED  BY  SIZE
                         "　　　　　"          DELIMITED  BY  SIZE
                         WRK-HENNYMDG(3)       DELIMITED  BY  SIZE
                         "〜"                  DELIMITED  BY  SIZE
                         WRK-HENTYMDG(3)       DELIMITED  BY  SIZE
                         "、"                  DELIMITED  BY  SIZE
                         WRK-HENNYMDG(4)       DELIMITED  BY  SIZE
                         "〜"                  DELIMITED  BY  SIZE
                         WRK-HENTYMDG(4)       DELIMITED  BY  SIZE
                         INTO                  MF100-BIKO3
                 END-STRING
             ELSE
               IF  MF100-NYUINYMD(3)   NOT =  ZERO
                 STRING  "入院期間："          DELIMITED  BY  SIZE
                         WRK-HENNYMDG(1)       DELIMITED  BY  SIZE
                         "〜"                  DELIMITED  BY  SIZE
                         WRK-HENTYMDG(1)       DELIMITED  BY  SIZE
                         "、"                  DELIMITED  BY  SIZE
                         WRK-HENNYMDG(2)       DELIMITED  BY  SIZE
                         "〜"                  DELIMITED  BY  SIZE
                         WRK-HENTYMDG(2)       DELIMITED  BY  SIZE
                         "　　　　　"          DELIMITED  BY  SIZE
                         WRK-HENNYMDG(3)       DELIMITED  BY  SIZE
                         "〜"                  DELIMITED  BY  SIZE
                         WRK-HENTYMDG(3)       DELIMITED  BY  SIZE
                         INTO                  MF100-BIKO3
                 END-STRING
               ELSE
                 IF  MF100-NYUINYMD(2)   NOT =  ZERO
                   STRING  "入院期間："          DELIMITED  BY  SIZE
                           WRK-HENNYMDG(1)       DELIMITED  BY  SIZE
                           "〜"                  DELIMITED  BY  SIZE
                           WRK-HENTYMDG(1)       DELIMITED  BY  SIZE
                           "、"                  DELIMITED  BY  SIZE
                           WRK-HENNYMDG(2)       DELIMITED  BY  SIZE
                           "〜"                  DELIMITED  BY  SIZE
                           WRK-HENTYMDG(2)       DELIMITED  BY  SIZE
                           INTO                  MF100-BIKO3
                   END-STRING
                 END-IF
               END-IF
             END-IF
           END-IF
      *
           .
       20013-YMDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別発行時の保険者番号(市町村番号)チェック処理
      *****************************************************************
       200111-HKNJANUM-CHK-SEC                SECTION.
      *
           INITIALIZE      WRK-HKNJANUM
           MOVE    "38"                     TO  WRK-HKNJANUM(1:2)
           MOVE    WRK-KOBETUCITYNUM(1:3)   TO  WRK-HKNJANUM(3:3)
      *
           IF  WRK-PARA-SYORIFLG       =   "1"
               AND FLG-SYORI           =    1
               AND FLG-ERR             =   ZERO
      *
               DISPLAY "チェック用保険者番号="  WRK-HKNJANUM
               MOVE    1       TO      FLG-ERR
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >   WRK-CONS-PARA-MAX
                       OR      TBL-PARA-HKNJANUM(IDXX)  =   SPACE
                   IF      WRK-HKNJANUM(1:5)
                                   =   TBL-PARA-HKNJANUM(IDXX)(1:5)
                       MOVE    ZERO                TO  FLG-ERR
                       MOVE    WRK-CONS-PARA-MAX   TO  IDXX
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       200111-HKNJANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           OPEN     INPUT       MF100-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =  1
      *
      *    帳票編集<見出し>処理
               PERFORM  310-HEAD-HEN-SEC
      *    帳票編集<明細>処理
               PERFORM  320-BODY-HEN-SEC
      *    帳票印刷処理
               PERFORM  330-PRINT-OUT-SEC
      *    一時ファイル読込
               PERFORM  900-TMP-READ-SEC
      *
           END-PERFORM
      *
           CLOSE    MF100-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
           ADD     1           TO      CNT-PAGE
           INITIALIZE                             SC23
      *
      *    診療年月
           MOVE    MF100-SRYYM             TO  WRK-SYMD (1:6)
           MOVE    "01"                    TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)       TO  SC23-PRTYM
      *
      *    請求日付
           MOVE    WRK-SEIYMDWH            TO  SC23-SEIYMD
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD         TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SC23-HOSPCD
      *
      *    住所
           MOVE  WRK-HOSPADRS          TO  SC23-ADRS
           INSPECT SC23-ADRS           REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE  WRK-HOSPNAME          TO  SC23-HOSPNAME
           INSPECT SC23-HOSPNAME       REPLACING ALL "  " BY "　"
      *
      *    開設者名
           MOVE  WRK-KAISETUNAME           TO  SC23-KAISETU
      *
      *    保険区分
           EVALUATE    MF100-KOHNUM
               WHEN "195"
               WHEN "194"
               WHEN "123"
                   MOVE    "○"            TO    SC23-HKNKBN(1)
               WHEN "295"
               WHEN "294"
               WHEN "223"
                   MOVE    "○"            TO    SC23-HKNKBN(3)
           END-EVALUATE
      *
      *    老人一般区分・前期高齢者判定
           IF  MF100-RJNHKNNUM     =    SPACE
               MOVE    "○"    TO    SC23-HKNSYUBT(1)
               IF  MF100-AGE       >=    70
                   EVALUATE MF100-KYURATE
                       WHEN    10
                       WHEN    20
                           MOVE    "○"    TO    SC23-ZNK8
                       WHEN    30
                           MOVE    "○"    TO    SC23-ZNK7
                   END-EVALUATE
               END-IF
           ELSE
               MOVE    "○"    TO    SC23-HKNSYUBT(2)
           END-IF
      *
      *    一部負担割合
           EVALUATE MF100-KYURATE
           WHEN 10
               IF  MF100-AGE  <  70
                   MOVE    "○"    TO    SC23-ITB1
               ELSE
                   MOVE    "○"    TO    SC23-ITB2
               END-IF
           WHEN 20
               MOVE    "○"    TO    SC23-ITB2
           WHEN 30
               MOVE    "○"    TO    SC23-ITB3
           END-EVALUATE
      *
      *    保険者番号
           MOVE    MF100-HKNJANUM          TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           EVALUATE    MF100-HKNNUM
               WHEN    "001"
                   MOVE    WRK-OUT-INPUT(1:8)  TO  SC23-HKNJANUM(9:8)
                   MOVE    "００００"          TO  SC23-HKNJANUM(1:8)
               WHEN    "060"
                   MOVE    WRK-OUT-INPUT(1:12) TO  SC23-HKNJANUM(5:12)
                   MOVE    "００"              TO  SC23-HKNJANUM(1:4)
               WHEN    OTHER
                   MOVE    WRK-OUT-INPUT       TO  SC23-HKNJANUM
           END-EVALUATE
      *
      *    記号・番号
           MOVE    MF100-KIGO              TO  SC23-KIGO
           MOVE    MF100-NUM               TO  SC23-BANGO
      *
      *    市町村番号(老人負担者番号)
           MOVE    MF100-CITYNUM           TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SC23-CITYNUM
      *
      *    老人受給者番号
           MOVE    MF100-RJNJKYSNUM        TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SC23-RJNJKYSNUM
      *
      *    公費負担者番号
           MOVE    MF100-KOHFTNJANUM       TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SC23-KOHFTNJANUM
      *
      *    公費受給者番号
           MOVE    MF100-KOHJKYSNUM        TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SC23-KOHJKYSNUM
      *
      *    患者氏名
           MOVE    MF100-NAME              TO  SC23-PTNAME
      *    性別
           IF  MF100-SEX           =     "1"
               MOVE    "○"        TO    SC23-SEX1
           ELSE
               MOVE    "○"        TO    SC23-SEX2
           END-IF
      *
      *    生年月日
           MOVE  MF100-BIRTHDAY    TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           EVALUATE  LNK-DAY2-EDTYMD1-G
               WHEN "M"
                   MOVE    "○"        TO  SC23-TANJOG1
               WHEN "T"
                   MOVE    "○"        TO  SC23-TANJOG2
               WHEN "S"
                   MOVE    "○"        TO  SC23-TANJOG3
               WHEN "H"
                   MOVE    "○"        TO  SC23-TANJOG4
               WHEN "R"
                   MOVE    "○"        TO  SC23-TANJOG5
           END-EVALUATE
           MOVE    LNK-DAY2-EDTYMD1-Y  TO  SC23-TANJOY
           MOVE    LNK-DAY2-EDTYMD1-M  TO  SC23-TANJOM
           MOVE    LNK-DAY2-EDTYMD1-D  TO  SC23-TANJOD

           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
           IF  MF100-NYUGAIKBN     =   "1"
               MOVE    "◯"        TO  SC23-NYUGAIKBN1
      *
      *    日数
               MOVE    MF100-JNISSU        TO  WRK-Z2-9
               MOVE    WRK-Z2-9            TO  SC23-N-SRYNISSU
      *        同月に再入院がある場合、入院日は最初の入院日を記載し、
      *        退院日は最後の退院日を記載する
               MOVE    MF100-NYUINYMD(1)           TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3(1:4)       TO  SC23-NYUING
               MOVE    LNK-DAY2-EDTYMD1(2:2)       TO  SC23-NYUINY
               MOVE    LNK-DAY2-EDTYMD1(5:2)       TO  SC23-NYUINM
               MOVE    LNK-DAY2-EDTYMD1(8:2)       TO  SC23-NYUIND
      *
               IF  MF100-NYUINYMD(1)  NOT =  "99999999"  AND  SPACE
                   IF  MF100-NYUINYMD(4)  =  ZERO
                       IF  MF100-NYUINYMD(3)  =  ZERO
                           IF  MF100-NYUINYMD(2)  =  ZERO
                               MOVE    MF100-TAIINYMD(1)   TO  WRK-SYMD
                           ELSE
                               MOVE    MF100-TAIINYMD(2)   TO  WRK-SYMD
                           END-IF
                       ELSE
                           MOVE    MF100-TAIINYMD(3)   TO  WRK-SYMD
                       END-IF
                   ELSE
                       MOVE    MF100-TAIINYMD(4)   TO  WRK-SYMD
                   END-IF
      *
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3(1:4)   TO  SC23-TAIING
                   MOVE    LNK-DAY2-EDTYMD1(2:2)   TO  SC23-TAIINY
                   MOVE    LNK-DAY2-EDTYMD1(5:2)   TO  SC23-TAIINM
                   MOVE    LNK-DAY2-EDTYMD1(8:2)   TO  SC23-TAIIND
               END-IF
           ELSE
               MOVE    "◯"        TO    SC23-NYUGAIKBN2
      *    診療日に○を付ける
               MOVE  1   TO   IDX1
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   IDX1    >       31
                   IF  MF100-SRYDAY(IDX1)  =   1
                       MOVE    "○"        TO  SC23-DAY(IDX1)
                   END-IF
               END-PERFORM
      *
      *    日数
               MOVE    MF100-JNISSU        TO  WRK-Z2-9
               MOVE    WRK-Z2-9            TO  SC23-G-SRYNISSU
           END-IF
      *
      *    傷病名及び期間
           IF  MF100-BYOMEI  NOT =  SPACE
             MOVE    MF100-BYOYMD          TO  WRK-SYMD
             PERFORM   31011-SEIWA-HEN-SEC
             STRING  LNK-DAY1-GENKIGO      DELIMITED  BY  SIZE
                     LNK-DAY1-WYMD(2:2)    DELIMITED  BY  SIZE
                     "."                   DELIMITED  BY  SIZE
                     LNK-DAY1-WYMD(4:2)    DELIMITED  BY  SIZE
                     "."                   DELIMITED  BY  SIZE
                     LNK-DAY1-WYMD(6:2)    DELIMITED  BY  SIZE
                     " "                   DELIMITED  BY  SIZE
                     MF100-BYOMEI          DELIMITED  BY  SPACE
                     INTO                  SC23-BYOMEI
             END-STRING
           END-IF
      *
      *    点数
           MOVE    MF100-TOTALTEN          TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC23-TOTALTEN
      *
      *    自己負担月額
           MOVE    MF100-JIKOFTNGK         TO  WRK-Z7
           MOVE    WRK-Z7                  TO  SC23-JIKOFTNGK
      *
      *    全国公費併用時自己負担額がない場合は空白ではなく「０」を印字する
           IF  MF100-JIKOFTNGK  =  ZERO
             PERFORM VARYING IDX2    FROM    1   BY  1
                     UNTIL   IDX2    >   6
                 IF  MF100-KOHKBN(IDX2)  =   1
                     MOVE    MF100-JIKOFTNGK         TO  WRK-Z7-9
                     MOVE    WRK-Z7-9                TO  SC23-JIKOFTNGK
                 END-IF
             END-PERFORM
           END-IF
      *
      *    公費負担額
           IF  MF100-JIKOFTNGK  NOT =  ZERO
             MOVE    MF100-KOHFTNGK        TO  WRK-Z7-9
             MOVE    WRK-Z7-9              TO  SC23-KOHFTN
           ELSE
             MOVE    MF100-KOHFTNGK        TO  WRK-Z7
             MOVE    WRK-Z7                TO  SC23-KOHFTN
           END-IF
      *
      *    請求金額
           MOVE    MF100-SEIKYUGK          TO  WRK-Z10
           MOVE    WRK-Z10                 TO  SC23-SEIKYUGK
      *
      *    公費併用チェック
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >   6
               IF  MF100-KOHKBN(IDX2)  =   1
                   MOVE  "○"    TO    SC23-KOHKBN(IDX2)
               END-IF
           END-PERFORM
      *
      *    特定疾病等
           MOVE  1                   TO  IDX2
           EVALUATE  MF100-TYOKI
             WHEN  1    MOVE  "02"   TO  SC23-TOKUTEI(IDX2)
             WHEN  2    MOVE  "16"   TO  SC23-TOKUTEI(IDX2)
           END-EVALUATE
      *
           IF  SC23-TOKUTEI(1)  NOT =  SPACE
               ADD   1                     TO  IDX2
           END-IF
           EVALUATE  MF100-TYOKI2
             WHEN  3    MOVE  "17"   TO  SC23-TOKUTEI(IDX2)
             WHEN  4    MOVE  "18"   TO  SC23-TOKUTEI(IDX2)
             WHEN  5    MOVE  "19"   TO  SC23-TOKUTEI(IDX2)
             WHEN  6    MOVE  "26"   TO  SC23-TOKUTEI(IDX2)
             WHEN  7    MOVE  "27"   TO  SC23-TOKUTEI(IDX2)
             WHEN  8    MOVE  "28"   TO  SC23-TOKUTEI(IDX2)
             WHEN  9    MOVE  "29"   TO  SC23-TOKUTEI(IDX2)
             WHEN  10   MOVE  "30"   TO  SC23-TOKUTEI(IDX2)
           END-EVALUATE
      *
      *    備考
           MOVE  MF100-BIKO    TO  SC23-BIKO
      *
      *    在宅チェック
           IF    MF100-ZAITAKU  =  "1"
               MOVE    "在"    TO  SC23-ZAI1
               MOVE    "◯"    TO  SC23-ZAI2
           END-IF
      *
      *    公費対象点数
           DISPLAY "MF100-TAHOTEN =" MF100-TAHOTEN
           IF  MF100-TAHOTEN  NOT =  ZERO
             MOVE    MF100-TAHOTEN         TO  WRK-Z7
             STRING  "公費対象点数　"      DELIMITED  BY  SIZE
                     WRK-Z7                DELIMITED  BY  SIZE
                     "点"                  DELIMITED  BY  SIZE
                     INTO                  SC23-TAHOTEN
             END-STRING
           END-IF
      *
      *    備考２
           MOVE  MF100-BIKO2   TO  SC23-BIKO2
      *
      *    備考３（複数入院期間）
           MOVE  MF100-BIKO3   TO  SC23-BIKO3
      *
      *    ３歳児健診 ※備考３に上書き
           IF  MF100-BIKO4  NOT =  SPACE
               MOVE  MF100-BIKO4   TO  SC23-BIKO3
           END-IF
      *
      *    コメント
           IF  MF100-BIKO5  NOT =  SPACE
               MOVE  MF100-BIKO5   TO  SC23-BIKO5
           END-IF
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       330-PRINT-OUT-SEC                SECTION.
      *
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE   "SC3823.red"         TO  SPRT-PRTID
           MOVE   "福祉医療費支給申請・請求明細書"
                                       TO  SPRT-TITLE
           MOVE    SC23                TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "2"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       330-PRINT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC  SECTION.
      *
           OPEN  INPUT  RECEERR-FILE
           IF  STS-RECEERR  =  ZERO
             CONTINUE
           ELSE
             OPEN  OUTPUT  RECEERR-FILE
      *
             MOVE  WRK-RECEERR  TO  RECEERR-REC
             WRITE RECEERR-REC
           END-IF
      *
           CLOSE  RECEERR-FILE
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC   SECTION.
      *
      *
           CLOSE  MF100-FILE
           MOVE  CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SEIKYU3823 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU3823 OUT "  CNT-MF100
           DISPLAY "*** SEIKYU3823 END ***"
      *
           IF  CNT-MF100  =  0
             DISPLAY "DATA NOT FOUND. PRINT JOB CANCEL!!"
           END-IF
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-PAGE   TO  JOB-UPDCNT
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計ファイル読込
      *****************************************************************
       800-SKTTMP-READ-SEC             SECTION.
      *
           READ    MF100-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-MF100
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-MF100
           END-READ
           .
       800-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象公費番号チェック処理
      *****************************************************************
       800-TAISYOKOHI-CHK-SEC                SECTION.
      *
           INITIALIZE     FLG-TAISYO
      *
           PERFORM  VARYING  IDXX  FROM  1  BY  1
                    UNTIL    IDXX  >  100
                    OR       WRK-TAISYO-KOHNUM(IDXX)  =  SPACE
               IF  WRK-CHK-KOHNUM(1)  =  WRK-TAISYO-KOHNUM(IDXX)
               OR  WRK-CHK-KOHNUM(2)  =  WRK-TAISYO-KOHNUM(IDXX)
               OR  WRK-CHK-KOHNUM(3)  =  WRK-TAISYO-KOHNUM(IDXX)
               OR  WRK-CHK-KOHNUM(4)  =  WRK-TAISYO-KOHNUM(IDXX)
                   MOVE     1                 TO  FLG-TAISYO
                   MOVE     100               TO  IDXX
               END-IF
           END-PERFORM
      *
           .
       800-TAISYOKOHI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  MF100-FILE   NEXT
               AT  END
                   MOVE    1       TO  FLG-PRINT
               NOT AT  END
                   MOVE    ZERO    TO  FLG-PRINT
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書マスタ読込
      *****************************************************************
       900-KOHSKY-START-SEC            SECTION.
      *
           INITIALIZE                  RECE02-REC
           MOVE    SPA-HOSPNUM         TO  RECE02-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECE02-SKYYM
           MOVE    RECE02-REC          TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 900-KOHSKY-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-KOHSKY-START-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書マスタＲＥＡＤ
      *****************************************************************
       900-KOHSKY-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"                  TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  RECE02-REC
      *
               INITIALIZE                          WRK-CHK-KOHNUM-TBL
               MOVE    RECE02-KOHNUM(1)        TO  WRK-CHK-KOHNUM(1)
               MOVE    RECE02-KOHNUM(2)        TO  WRK-CHK-KOHNUM(2)
               MOVE    RECE02-KOHNUM(3)        TO  WRK-CHK-KOHNUM(3)
               MOVE    RECE02-KOHNUM(4)        TO  WRK-CHK-KOHNUM(4)
      *        対象公費番号チェック処理
               PERFORM  800-TAISYOKOHI-CHK-SEC
               IF  FLG-TAISYO  =  1
      *
                   IF      WRK-PARA-STSRYYM    =   "999999"
                       CONTINUE
                   ELSE
                       IF  ( RECE02-SRYYM      <   WRK-PARA-STSRYYM )
                       OR  ( RECE02-SRYYM      >   WRK-PARA-EDSRYYM )
                           GO  TO  900-KOHSKY-READ-SEC
                       END-IF
                   END-IF
      *
      *    テスト患者のときは読み飛ばす
                   MOVE    SPACE               TO  PTINF-REC
                   INITIALIZE                      PTINF-REC
                   MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                   MOVE    RECE02-PTID         TO  PTINF-PTID
                   MOVE    PTINF-REC           TO  MCPDATA-REC
                   PERFORM 800-PTINF-READ-SEC
                   IF      FLG-PTINF           =   ZERO
                   AND     PTINF-TSTPTNUMKBN   =   "1"
                       GO  TO  900-KOHSKY-READ-SEC
                   END-IF
      *
      *        個別発行で患者番号指定の場合、対象判定を行う
                   MOVE  ZERO                TO  FLG-ERR
                   IF      WRK-PARA-SYORIFLG   =   "1"
                   AND     FLG-SYORI           =    2
                     MOVE  1                   TO  FLG-ERR
                     PERFORM VARYING IDXX    FROM    1   BY  1
                             UNTIL   IDXX    >   WRK-CONS-PARA-MAX
                             OR      TBL-PARA-PTID(IDXX) =   ZERO
                         IF      RECE02-PTID =   TBL-PARA-PTID(IDXX)
                           MOVE    ZERO        TO  FLG-ERR
                         END-IF
                     END-PERFORM
                   END-IF
                   DISPLAY "患者番号=" RECE02-PTNUM
                           "負担者番号=" RECE02-KOHFTNJANUM(1)
                           "保険者番号=" RECE02-HKNJANUM
      *
                   IF      FLG-ERR           =   1
                     GO  TO  900-KOHSKY-READ-SEC
                   END-IF
      *
                   ADD  1  TO  CNT-RECE02
               ELSE
                   GO  TO  900-KOHSKY-READ-SEC
               END-IF
           ELSE
               MOVE    1               TO  FLG-END
               PERFORM 900-KOHSKY-CLOSE-SEC
           END-IF
           .
       900-KOHSKY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書マスタクローズ
      *****************************************************************
       900-KOHSKY-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_kohsky"          TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           .
       900-KOHSKY-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *   請求管理情報読込
      *****************************************************************
       900-SEIKYU-READ-SEC  SECTION.
      *
           INITIALIZE  RECE10-REC
           MOVE  RECE02-KEY            TO  RECE10-KEY
           MOVE  RECE10-REC            TO  MCPDATA-REC
           MOVE  "DBSELECT"            TO  MCP-FUNC
           MOVE  "tbl_seikyu"          TO  MCP-TABLE
           MOVE  "key"                 TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"           USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_seikyu"      TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             MOVE    "DBFETCH"         TO  MCP-FUNC
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC  =  ZERO
                 MOVE    ZERO          TO  FLG-RECE10
                 MOVE    MCPDATA-REC   TO  RECE10-REC
             ELSE
                 MOVE    1             TO  FLG-RECE10
             END-IF
           ELSE
             DISPLAY  "SEIKYU SELECT ERR"
           END-IF
      *
           MOVE  "DBCLOSECURSOR"       TO  MCP-FUNC
           MOVE  "tbl_seikyu"          TO  MCP-TABLE
           MOVE  "key"                 TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"           USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
               MOVE    "tbl_ptinf"       TO  MCP-TABLE
               MOVE    "key"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"       USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_ptinf"            TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM                TO  COMB-HOSPNUM
           MOVE    SYU-PTID                   TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM            TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC               TO  MCPDATA-REC
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_hkncombi"         TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
               MOVE    "tbl_hkncombi"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI
               ELSE
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
            MOVE    "tbl_hkncombi"     TO  MCP-TABLE
            MOVE    "key"              TO  MCP-PATHNAME
            CALL    "ORCDBMAIN"        USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    請求額集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           MOVE    ZERO    TO      WRK-SEIKYUGK
                                   WRK-JIKOFTNGK
                                   FLG-HEIYOU
                                   WRK-TOKUTEI
                                   FLG-TYOKIFTN
                                   FLG-TOKUREI
                                   WRK-KOHFTNGK
                                   WRK-TAHOTEN
                                   FLG-HEIYOU2
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4   OR
                   SYU-KOH-HKNNUM(IDX) =   SPACE
               IF  SYU-KOH-HKNNUM(IDX)(1:1)    =   "0" AND
                   SYU-KOH-HKNNUM(IDX)     NOT =   "027"
                   MOVE    1       TO      FLG-HEIYOU
                                           FLG-HEIYOU2
      *
                   EVALUATE    SYU-KOH-HKNNUM(IDX)
                       WHEN    "020"
                       WHEN    "021"
                           MOVE    1   TO  MF100-KOHKBN(1)
                       WHEN    "010"
                       WHEN    "011"
                           MOVE    1   TO  MF100-KOHKBN(2)
                       WHEN    "015"
                           MOVE    1   TO  MF100-KOHKBN(3)
                       WHEN    "016"
                           MOVE    1   TO  MF100-KOHKBN(4)
                       WHEN    "051"
                       WHEN    "091"
                           MOVE    1   TO  MF100-KOHKBN(5)
                       WHEN    OTHER
                           MOVE    1   TO  MF100-KOHKBN(6)
                   END-EVALUATE
                   IF  SYU-KOH-HKNNUM(IDX) = "051" OR "091" OR "052"
                     MOVE  1           TO  WRK-TOKUTEI
                   END-IF
      *
                   IF  RECE02-FTNMONEY(1)  =   ZERO
                       MOVE    SYU-KOH-COMPFTN-ENTANI(IDX)
                                           TO  WRK-SEIKYUGK
                                               WRK-JIKOFTNGK
                   ELSE
                       MOVE    SYU-KOH-COMPFTN(IDX)
                                           TO  WRK-SEIKYUGK
                                               WRK-JIKOFTNGK
                   END-IF
      *
                   COMPUTE  WRK-KOHFTNGK  =  SYU-SYUCOMPFTN-ENTANI
                                          -  WRK-SEIKYUGK
      *
                   MOVE   SYU-TOTAL-HKNTEN   TO  WRK-TAHOTEN
      *
               END-IF
      *
               IF  SYU-KOH-HKNNUM(IDX)         =   "972"
                   MOVE    1       TO      FLG-TYOKI
               END-IF
               IF  SYU-KOH-HKNNUM(IDX)         =   "974"
                   MOVE    2       TO      FLG-TYOKI
               END-IF
      *
      *    現状、地方公費＋長期の場合、保険助成後の金額を地方公費が助成
      *    してしまう為、長期の計算負担金がゼロの場合は長期が助成したと判断する
               IF  (SYU-KOH-HKNNUM(IDX)  =  "972" OR "974")
               AND (SYU-KOH-COMPFTN-ENTANI(IDX)  =  ZERO)
                 MOVE      1       TO      FLG-TYOKIFTN
               END-IF
      *
      *    軽減特例措置は2割計算だが、主保険のCOMPFTNが1割計算なので
      *    計算は後に行う
               IF  (SYU-KOH-HKNNUM(IDX)  =  "972" OR "974")
               AND (RECE02-FLG(2)  =  1)
                 MOVE      1       TO      FLG-TOKUREI
               END-IF
      *
      *    公費の助成なしは印字対象外とする
               IF   SYU-KOH-HKNNUM(IDX)     =  WRK-KOHNUM
                 IF     IDX                 =  1
                   IF  SYU-SYUCOMPFTN-ENTANI
                                         > SYU-KOH-COMPFTN-ENTANI(IDX)
                     MOVE  ZERO          TO  FLG-ERR
                   END-IF
                 ELSE
                   IF  SYU-KOH-COMPFTN-ENTANI(IDX - 1)
                                        >  SYU-KOH-COMPFTN-ENTANI(IDX)
                     MOVE  ZERO          TO  FLG-ERR
                   END-IF
                 END-IF
               END-IF
      *
      *
           END-PERFORM
      *
           IF  FLG-HEIYOU      =   ZERO
      *      長期助成有りの場合、請求金額に加算しない
             IF  FLG-TYOKIFTN  =   ZERO
              IF  FLG-TOKUREI  =  ZERO
               IF  RECE02-FTNMONEY(1)  =   ZERO
                   IF  RECE02-RJNHKNNUM    NOT =   SPACE
                       MOVE    SYU-KOH-COMPFTN-ENTANI(1)
                                                   TO  WRK-SEIKYUGK
                   ELSE
                       MOVE    SYU-SYUCOMPFTN-ENTANI
                                                   TO  WRK-SEIKYUGK
                   END-IF
               ELSE
                   IF  RECE02-RJNHKNNUM    NOT =   SPACE
                       MOVE    SYU-KOH-COMPFTN(1)  TO  WRK-SEIKYUGK
                   ELSE
                       MOVE    SYU-SYUCOMPFTN      TO  WRK-SEIKYUGK
                   END-IF
               END-IF
              END-IF
             ELSE
                MOVE    ZERO                       TO  WRK-SEIKYUGK
             END-IF
           END-IF
      *
           ADD     WRK-SEIKYUGK        TO  WRK-SEIKYUGK-G
           ADD     WRK-JIKOFTNGK       TO  WRK-JIKOFTNGK-G
           ADD     WRK-KOHFTNGK        TO  WRK-KOHFTNGK-G
           ADD     WRK-TAHOTEN         TO  WRK-TAHOTEN-G
      *
      *    外来の場合、診療日を取得する
           IF  RECE02-NYUGAIKBN    =   "2"
               MOVE    SYU-SRYYMD(7:2) TO  WRK-DAYX
               MOVE    1               TO  MF100-SRYDAY(WRK-DAY9)
           END-IF
      *
           .
       800-SEIKYU-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *  収納情報セット
      *****************************************************************
       920-SYUNOU-SET-SEC  SECTION.
      *
             INITIALIZE  SYUNOU-REC
             MOVE  SPA-HOSPNUM        TO  SYU-HOSPNUM
             MOVE  RECE02-NYUGAIKBN   TO  SYU-NYUGAIKBN
             MOVE  RECE02-PTID        TO  SYU-PTID
             MOVE  RECE02-SRYYM(1:6)  TO  SYU-SRYYMD(1:6)
             MOVE  "__"               TO  SYU-SRYYMD(7:2)
             MOVE  WRK-KOHNUM         TO  SYU-KOH-HKNNUM(1)
                                          SYU-KOH-HKNNUM(2)
                                          SYU-KOH-HKNNUM(3)
                                          SYU-KOH-HKNNUM(4)
             MOVE  SYUNOU-REC         TO  MCPDATA-REC
             MOVE  "DBSELECT"         TO  MCP-FUNC
             MOVE    "tbl_syunou"     TO  MCP-TABLE
             MOVE    WRK-PATH         TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"      USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    "tbl_syunou"   TO  MCP-TABLE
               MOVE    WRK-PATH       TO  MCP-PATHNAME
               PERFORM  925-SYUNOU-READ-SEC
      *
               PERFORM  UNTIL  FLG-SYUNOU  =  1
                 IF  SYU-DENPJTIKBN   NOT = "3"
                   IF    SYU-HKNCOMBINUM  =  RECE02-HKNCOMBI(1) OR
                                             RECE02-HKNCOMBI(2) OR
                                             RECE02-HKNCOMBI(3) OR
                                             RECE02-HKNCOMBI(4) OR
                                             RECE02-HKNCOMBI(5) OR
                                             RECE02-HKNCOMBI(6) OR
                                             RECE02-HKNCOMBI(7) OR
                                             RECE02-HKNCOMBI(8) OR
                                             RECE02-HKNCOMBI(9) OR
                                             RECE02-HKNCOMBI(10)
                     PERFORM  910-HKNCOMBI-READ-SEC
                     IF  COMB-KOHID(1)    =  WRK-KOHID   OR
                         COMB-KOHID(2)    =  WRK-KOHID   OR
                         COMB-KOHID(3)    =  WRK-KOHID   OR
                         COMB-KOHID(4)    =  WRK-KOHID
                       PERFORM  800-SEIKYU-SEC
                     END-IF
                   END-IF
                 END-IF
      *
                 MOVE    "tbl_syunou"   TO  MCP-TABLE
                 MOVE    WRK-PATH       TO  MCP-PATHNAME
                 PERFORM   925-SYUNOU-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SYUNOU SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
             MOVE    "tbl_syunou"       TO  MCP-TABLE
             MOVE    WRK-PATH           TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       920-SYUNOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込
      *****************************************************************
       925-SYUNOU-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"      TO  MCP-FUNC
           CALL  "ORCDBMAIN"    USING
                                MCPAREA
                                MCPDATA-REC
                                SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  SYUNOU-REC
             MOVE  ZERO         TO  FLG-SYUNOU
           ELSE
             INITIALIZE  SYUNOU-REC
             MOVE  1  TO  FLG-SYUNOU
           END-IF
      *
           .
       925-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴情報SELECT
      *****************************************************************
       930-PTNYUINRRK-SELECT-SEC         SECTION.
      *
           MOVE    PTNYUINRRK-REC                TO  MCPDATA-REC
           MOVE    "DBSELECT"                    TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
           MOVE    "key23"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF      MCP-RC           =   ZERO
               PERFORM 930-PTNYUINRRK-FETCH-SEC
      *
      *        その月の最小入退院日を取得
               MOVE  PTNYUINRRK-NYUINYMD      TO  WRK-MINNYMD(1)
               MOVE  PTNYUINRRK-TAIINYMD      TO  WRK-MINTYMD(1)
      *
               PERFORM  UNTIL  FLG-PTNYUINRRK  =  1
      *          最小入院日が見つかったら並べ替え
                 IF  PTNYUINRRK-NYUINYMD  <  WRK-MINNYMD(1)
                   MOVE  WRK-MINNYMD(3)       TO  WRK-MINNYMD(4)
                   MOVE  WRK-MINTYMD(3)       TO  WRK-MINTYMD(4)
                   MOVE  WRK-MINNYMD(2)       TO  WRK-MINNYMD(3)
                   MOVE  WRK-MINTYMD(2)       TO  WRK-MINTYMD(3)
                   MOVE  WRK-MINNYMD(1)       TO  WRK-MINNYMD(2)
                   MOVE  WRK-MINTYMD(1)       TO  WRK-MINTYMD(2)
                   MOVE  PTNYUINRRK-NYUINYMD  TO  WRK-MINNYMD(1)
                   MOVE  PTNYUINRRK-TAIINYMD  TO  WRK-MINTYMD(1)
                 ELSE
                   IF  PTNYUINRRK-NYUINYMD  <  WRK-MINNYMD(2)
                     MOVE  WRK-MINNYMD(3)       TO  WRK-MINNYMD(4)
                     MOVE  WRK-MINTYMD(3)       TO  WRK-MINTYMD(4)
                     MOVE  WRK-MINNYMD(2)       TO  WRK-MINNYMD(3)
                     MOVE  WRK-MINTYMD(2)       TO  WRK-MINTYMD(3)
                     MOVE  PTNYUINRRK-NYUINYMD  TO  WRK-MINNYMD(2)
                     MOVE  PTNYUINRRK-TAIINYMD  TO  WRK-MINTYMD(2)
                   ELSE
                     IF  PTNYUINRRK-NYUINYMD  <  WRK-MINNYMD(3)
                       MOVE  WRK-MINNYMD(3)       TO  WRK-MINNYMD(4)
                       MOVE  WRK-MINTYMD(3)       TO  WRK-MINTYMD(4)
                       MOVE  PTNYUINRRK-NYUINYMD  TO  WRK-MINNYMD(3)
                       MOVE  PTNYUINRRK-TAIINYMD  TO  WRK-MINTYMD(3)
                     ELSE
                       IF  PTNYUINRRK-NYUINYMD  <  WRK-MINNYMD(4)
                         MOVE  PTNYUINRRK-NYUINYMD  TO  WRK-MINNYMD(4)
                         MOVE  PTNYUINRRK-TAIINYMD  TO  WRK-MINTYMD(4)
                       END-IF
                     END-IF
                   END-IF
                 END-IF
                 PERFORM 930-PTNYUINRRK-FETCH-SEC
               END-PERFORM
           ELSE
               MOVE   1               TO  FLG-PTNYUINRRK
           END-IF
           .
       930-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴情報１件読み込み
      *****************************************************************
       930-PTNYUINRRK-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"                      TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
           MOVE    "key23"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
                   MOVE    ZERO            TO  FLG-PTNYUINRRK
           ELSE
                   MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
           .
       930-PTNYUINRRK-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴情報クローズ
      *****************************************************************
       930-PTNYUINRRK-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"               TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
           MOVE    "key23"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           .
       930-PTNYUINRRK-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    患者病名情報読み込み
      *****************************************************************
       900-PTBYOMEI-READ-SEC         SECTION.
      *
      *    患者病名読み込み（患者ＩＤ、診療年月日、診療科順）
           INITIALIZE                  PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    RECE02-PTID         TO  PTBYO-PTID
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
               PERFORM 900-PTBYO-FETCH-SEC
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM 910-PTBYO-HENSYU-SEC
                   UNTIL  FLG-PTBYOMEI  =  1
      *    カーソルクロース
           INITIALIZE                  MCPAREA
           MOVE    "DBCLOSECURSOR"           TO  MCP-FUNC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PTBYOMEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者病名マスタ　ＲＥＡＤ
      *****************************************************************
       900-PTBYO-FETCH-SEC         SECTION.
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTBYOMEI
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
           ELSE
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
           .
       900-PTBYO-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名診療年月日別処理
      *****************************************************************
       910-PTBYO-HENSYU-SEC                SECTION.
      *
           MOVE    PTBYO-SRYYMD            TO  KEY-N-SRYYMD
      *
           MOVE    KEY-NEW                 TO  KEY-OLD
           DISPLAY "患者病名フラグ=" FLG-PTBYOMEI
                   "患者番号=" RECE02-PTNUM
           PERFORM  UNTIL  FLG-PTBYOMEI  =  1
                    OR     KEY-NEW  NOT =  KEY-OLD
               DISPLAY "病名チェック開始"
               PERFORM 910-PTBYO-CHK-SEC
      *
               IF  FLG-PTBYOMEI  =  ZERO
                 PERFORM 900-PTBYO-FETCH-SEC
               END-IF
           END-PERFORM
      *
           .
       910-PTBYO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名チェック処理
      *****************************************************************
       910-PTBYO-CHK-SEC         SECTION.
      *
      *    診療開始日
           IF  PTBYO-SRYYMD(1:6)  =  RECE02-SRYYM
               CONTINUE
           ELSE
               IF  PTBYO-SRYYMD(1:6)  <  RECE02-SRYYM
                   IF  PTBYO-TENKIKBN  =  SPACE
                       CONTINUE
                   ELSE
                       IF  PTBYO-TENKIYMD(1:6)  >=  RECE02-SRYYM
                           CONTINUE
                       ELSE
                            DISPLAY "診療開始日1 GO TO"
                           GO  TO  910-PTBYO-CHK-EXT
                       END-IF
                   END-IF
               ELSE
                   DISPLAY "診療開始日2 GO TO"
                   GO  TO  910-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
      *    入外区分
           IF  PTBYO-NYUGAIKBN  =  SPACE
               CONTINUE
           ELSE
               IF  PTBYO-NYUGAIKBN  NOT =   RECE02-NYUGAIKBN
                   DISPLAY "入外区分 GO TO"
                   GO  TO  910-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
      *    保険組合せ
           IF  PTBYO-HKNCOMBI  =  ZERO
               CONTINUE
           ELSE
               MOVE    ZERO            TO  IDX1
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       10
                       OR      RECE02-HKNCOMBI (IDX)   =   ZERO
                   IF  PTBYO-HKNCOMBI  =  RECE02-HKNCOMBI (IDX)
                       MOVE    IDX             TO  IDX1
                       MOVE    10              TO  IDX
                   END-IF
               END-PERFORM
               IF  IDX1  >  ZERO
                   CONTINUE
               ELSE
                   DISPLAY "保険組合せ GO TO"
                   GO  TO  910-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
           MOVE    PTBYO-BYOMEI                TO  MF100-BYOMEI
           MOVE    PTBYO-SRYYMD                TO  MF100-BYOYMD
           MOVE    1                           TO  FLG-PTBYOMEI
      *
           .
       910-PTBYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  診療情報セット
      *****************************************************************
       960-SRYACT-SET-SEC  SECTION.
      *
             INITIALIZE  SRYACT-REC
             MOVE  SPA-HOSPNUM           TO  SRY-HOSPNUM
             MOVE  RECE02-NYUGAIKBN      TO  SRY-NYUGAIKBN
             MOVE  RECE02-PTID           TO  SRY-PTID
             MOVE  RECE02-SRYYM          TO  SRY-SRYYM
             MOVE  SRYACT-REC            TO  MCPDATA-REC
             MOVE  "DBSELECT"            TO  MCP-FUNC
             MOVE  "tbl_sryact"          TO  MCP-TABLE
             MOVE  "key4"                TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  "tbl_sryact"        TO  MCP-TABLE
               MOVE  "key4"              TO  MCP-PATHNAME
               PERFORM  965-SRYACT-READ-SEC
      *    在総診の患者取得
               PERFORM  UNTIL  FLG-SRYACT  =  1
                 EVALUATE  FLG-PROC
                   WHEN  1
                     PERFORM  965-ZAISOU-CHK-SEC
                   WHEN  2
                     PERFORM  965-COMMENT-CHK-SEC
                 END-EVALUATE
      *
                 MOVE  "tbl_sryact"      TO  MCP-TABLE
                 MOVE  "key4"            TO  MCP-PATHNAME
                 PERFORM  965-SRYACT-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SRYACT SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"       TO  MCP-FUNC
             MOVE  "tbl_sryact"          TO  MCP-TABLE
             MOVE  "key4"                TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       960-SRYACT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療情報読込
      *****************************************************************
       965-SRYACT-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"      TO  MCP-FUNC
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  SRYACT-REC
             MOVE  ZERO         TO  FLG-SRYACT
           ELSE
             INITIALIZE   SRYACT-REC
             MOVE  1  TO  FLG-SRYACT
           END-IF
      *
           .
       965-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    在総診チェック
      *****************************************************************
       965-ZAISOU-CHK-SEC  SECTION.
      *
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL   (IDX4  >     5) OR
                            (SRY-SRYCD(IDX4)  =  SPACE)
             IF  SRY-SRYKBN  =  "14"
               IF  RECE02-SRYYM  >=  WRK-CONS-SRYYM-201604
                 PERFORM  VARYING  IDX5  FROM  1  BY  1
                          UNTIL   (IDX5  >  TBL-ZAITAKU2-MAX)
                          OR      (MF100-ZAITAKU  =  "1")
                   IF  SRY-SRYCD(IDX4)  =  TBL-ZAITAKU2(IDX5)
                       MOVE    "1"            TO  MF100-ZAITAKU
                   END-IF
                 END-PERFORM
               ELSE
                 PERFORM  VARYING  IDX5  FROM  1  BY  1
                          UNTIL   (IDX5  >  TBL-ZAITAKU-MAX)
                          OR      (MF100-ZAITAKU  =  "1")
                   IF  SRY-SRYCD(IDX4)  =  TBL-ZAITAKU(IDX5)
                       MOVE    "1"            TO  MF100-ZAITAKU
                   END-IF
                 END-PERFORM
               END-IF
             END-IF
           END-PERFORM
      *
           .
       965-ZAISOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    コメントチェック
      *****************************************************************
       965-COMMENT-CHK-SEC  SECTION.
      *
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL   (IDX4  >     5) OR
                            (SRY-SRYCD(IDX4)  =  SPACE)
             IF  SRY-SRYCD(IDX4)  =  TBL-COMMENTCD(1)  OR
                                     TBL-COMMENTCD(2)  OR
                                     TBL-COMMENTCD(3)  OR
                                     TBL-COMMENTCD(4)  OR
                                     TBL-COMMENTCD(5)  OR
                                     TBL-COMMENTCD(6)  OR
                                     TBL-COMMENTCD(7)  OR
                                     TBL-COMMENTCD(8)  OR
                                     TBL-COMMENTCD(9)  OR
                                     TBL-COMMENTCD(10)
               MOVE  SRY-SRYCD(IDX4)     TO  WRK-COMMENTCD
             END-IF
           END-PERFORM
      *
           .
       965-COMMENT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ読み込み
      *****************************************************************
       900-TENSU-READ-SEC         SECTION.
      *
      *    点数マスタ読み込み
           INITIALIZE                  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-COMMENTCD       TO  TNS-SRYCD
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                       TO  TNS-YUKOSTYMD(1:6)
                                           TNS-YUKOEDYMD(1:6)
           MOVE    "01"                TO  TNS-YUKOSTYMD(7:2)
                                           TNS-YUKOEDYMD(7:2)
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_tensu"       TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             MOVE    "DBFETCH"         TO  MCP-FUNC
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF      MCP-RC            =   ZERO
                 MOVE    ZERO          TO  FLG-TENSU
                 MOVE    MCPDATA-REC   TO  TNS-TENSU-REC
             ELSE
                 MOVE    1             TO  FLG-TENSU
             END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
      *    カーソルクロース
           INITIALIZE                  MCPAREA
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療情報セット
      *****************************************************************
       960-SRYACT-KEY7-SET-SEC  SECTION.
      *
             INITIALIZE                      SRYACT-REC
      *
             MOVE  SPA-HOSPNUM           TO  SRY-HOSPNUM
             MOVE  RECE02-NYUGAIKBN      TO  SRY-NYUGAIKBN
             MOVE  RECE02-PTID           TO  SRY-PTID
             MOVE  RECE02-SRYYM          TO  SRY-SRYYM
             MOVE  WRK-SRYCD             TO  SRY-SRYCD(1)
                                             SRY-SRYCD(2)
                                             SRY-SRYCD(3)
                                             SRY-SRYCD(4)
                                             SRY-SRYCD(5)
             MOVE  SRYACT-REC            TO  MCPDATA-REC
             MOVE  "DBSELECT"            TO  MCP-FUNC
             MOVE  "tbl_sryact"          TO  MCP-TABLE
             MOVE  "key7"                TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
             IF  MCP-RC  =  ZERO
               MOVE  "tbl_sryact"        TO  MCP-TABLE
               MOVE  "key7"              TO  MCP-PATHNAME
               PERFORM  965-SRYACT-READ-SEC
             ELSE
               DISPLAY  "SRYACT SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"       TO  MCP-FUNC
             MOVE  "tbl_sryact"          TO  MCP-TABLE
             MOVE  "key7"                TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       960-SRYACT-KEY7-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメント情報セット
      *****************************************************************
       950-PTCOM-READ-SEC  SECTION.
      *
             INITIALIZE                      PTCOM-REC
      *
             MOVE  SPA-HOSPNUM           TO  PTCOM-HOSPNUM
             MOVE  SRY-PTID              TO  PTCOM-PTID
             MOVE  SRY-ZAINUM            TO  PTCOM-ZAINUM
             MOVE  SRY-SRYCD(IDX4)       TO  PTCOM-SRYCD
             MOVE  SRY-INPUTNUM(IDX4)    TO  PTCOM-RENNUM
             MOVE  PTCOM-REC             TO  MCPDATA-REC
             MOVE  "DBSELECT"            TO  MCP-FUNC
             MOVE  "tbl_ptcom"           TO  MCP-TABLE
             MOVE  "key"                 TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
             IF  MCP-RC  =  ZERO
               MOVE  "tbl_ptcom"         TO  MCP-TABLE
               MOVE  "key"               TO  MCP-PATHNAME
               MOVE  "DBFETCH"           TO  MCP-FUNC
               CALL  "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
               IF  MCP-RC  =  ZERO
                 MOVE  MCPDATA-REC       TO  PTCOM-REC
                 MOVE  ZERO              TO  FLG-PTCOM
               ELSE
                 INITIALIZE                  PTCOM-REC
                 MOVE    1               TO  FLG-PTCOM
               END-IF
             ELSE
               DISPLAY  "PTCOM SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"       TO  MCP-FUNC
             MOVE  "tbl_ptcom"           TO  MCP-TABLE
             MOVE  "key"                 TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       950-PTCOM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦・和暦変換処理
      *****************************************************************
       31011-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "11"                TO  LNK-DAY1-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
           .
       31011-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    月末日算出処理
      *****************************************************************
       31012-LASTDAY-GET-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                   TO  LNK-DAY7-IRAI
           MOVE    WRK-SYMD     TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                  LNK-DAY7-AREA
           MOVE LNK-DAY7-KOYOMI  TO  WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC  SECTION.
      *
           MOVE  "DBSELECT"                TO  MCP-FUNC
           MOVE    "tbl_syskanri"         TO  MCP-TABLE
           MOVE    "key10"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  "DBFETCH"                TO  MCP-FUNC
             MOVE  "tbl_syskanri"          TO  MCP-TABLE
             MOVE  "key10"                 TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  ZERO  TO  FLG-SYSKANRI
             ELSE
               MOVE  1  TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE  1  TO  FLG-SYSKANRI
           END-IF
      *
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
           MOVE  "tbl_syskanri"          TO  MCP-TABLE
           MOVE  "key10"                 TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"             USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
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
      *    医療機関編集情報読み込み
      *****************************************************************
       900-1900-READ-SEC           SECTION.
      *
      *    医療機関編集情報読み込み
           MOVE    SPACE                   TO  SYS-1900-REC
           INITIALIZE                          SYS-1900-REC
           MOVE    "1900"                  TO  SYS-1900-KANRICD
           MOVE    "*"                     TO  SYS-1900-KBNCD
           MOVE    SPA-HOSPNUM             TO  SYS-1900-HOSPNUM
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1900-STYUKYMD(1:6)
                                               SYS-1900-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1900-STYUKYMD(1:6)
                                               SYS-1900-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                    TO  SYS-1900-STYUKYMD(7:2)
                                               SYS-1900-EDYUKYMD(7:2)
           MOVE    SYS-1900-REC            TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(8) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
      *            医療機関住所編集情報（２）
                   PERFORM 900-1903-READ-SEC
               END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
           .
      *
       900-1900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報読み込み
      *****************************************************************
       900-1901-READ-SEC           SECTION.
      *
      *    医療機関名称編集情報読み込み
           MOVE    SPACE               TO  SYS-1901-REC
           INITIALIZE                      SYS-1901-REC
           MOVE    "1901"              TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(8)  TO  SYS-1901-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1901-HOSPNUM
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-STYUKYMD(1:6)
                                               SYS-1901-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1901-STYUKYMD(1:6)
                                               SYS-1901-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1901-STYUKYMD(7:2)
                                           SYS-1901-EDYUKYMD(7:2)
           MOVE    SYS-1901-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-HOSPNAME1
               MOVE    SYS-1901-HOSPNAME2  TO  WRK-HOSPNAME2
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
           .
      *
       900-1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1902-READ-SEC           SECTION.
      *
      *    医療機関住所編集情報読み込み
           MOVE    SPACE               TO  SYS-1902-REC
           INITIALIZE                      SYS-1902-REC
           MOVE    "1902"              TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(8)  TO  SYS-1902-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1902-HOSPNUM
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-STYUKYMD(1:6)
                                               SYS-1902-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1902-STYUKYMD(1:6)
                                               SYS-1902-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1902-STYUKYMD(7:2)
                                           SYS-1902-EDYUKYMD(7:2)
           MOVE    SYS-1902-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1  TO  WRK-HOSPADRS1
               MOVE    SYS-1902-ADRS2  TO  WRK-HOSPADRS2
               MOVE    SYS-1902-TEL    TO  WRK-HOSPTEL
           ELSE
               INITIALIZE                  SYS-1902-REC
           END-IF
           .
      *
       900-1902-READ-EXT.
           EXIT.
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1903-READ-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1903-REC
           INITIALIZE                      SYS-1903-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1903-HOSPNUM
           MOVE    "1903"              TO  SYS-1903-KANRICD
           MOVE    SYS-1900-PRTKBN(8)  TO  SYS-1903-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1903-STYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1903-STYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1903-STYUKYMD(7:2)
           MOVE    SYS-1903-STYUKYMD   TO  SYS-1903-EDYUKYMD
           MOVE    SYS-1903-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1903-REC
               MOVE    SYS-1903-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           ELSE
               INITIALIZE                  SYS-1903-REC
           END-IF
           .
      *
       900-1903-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-N-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  BTPARA-REC
               MOVE    ZERO                TO  FLG-BTPARA
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           .
       900-BTPARA-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション情報読込
      *****************************************************************
       900-PRGOPTION-READ-SEC  SECTION.
      *
           INITIALIZE                      PRGOPTION-REC
           MOVE    SPA-HOSPNUM         TO  PRGOPTION-HOSPNUM
           MOVE    WRK-PRGID           TO  PRGOPTION-PRGID
           MOVE    WRK-KBNCD           TO  PRGOPTION-KBNCD
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_prgoption"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PRGOPTION-REC
                   MOVE    ZERO                TO  FLG-PRGOPTION
               ELSE
                   INITIALIZE                      PRGOPTION-REC
                   MOVE    1                   TO  FLG-PRGOPTION
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PRGOPTION
           END-IF
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PRGOPTION-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC  SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBOPEN"   TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBSTART"  TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC  SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBCOMMIT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBDISCONNECT"  TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
