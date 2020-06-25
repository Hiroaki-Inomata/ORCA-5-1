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
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGW65.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 公費付加情報
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/12/01    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    コンスタント値
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
      *
       01  SPA-W65.
           03  SPA-GMN-CUR                         PIC 9(05).
           03  SPA-GMN-PAGENO                      PIC 9(03).
           03  SPA-GMN-AREA.
               05  SPA-GMN-KJNYMD                  PIC X(10).
               05  SPA-GMN-INPUT-AREA.
                 06  SPA-GMN-SELNUM1               PIC X(50).
                 06  SPA-GMN-KOHNUM-G.
                   07  SPA-GMN-KOHNUM              PIC X(03).
                   07  SPA-GMN-KOHNUMF             PIC X(01).
                   07  SPA-GMN-KOHNUMMEI           PIC X(60).
                 06  SPA-GMN-YUKOSTYMD             PIC X(10).
                 06  SPA-GMN-YUKOEDYMD             PIC X(10).
                 06  SPA-GMN-SELNUM2               PIC S9(2).
                 06  SPA-GMN-NBOOK-AREA.
                   07  SPA-GMN-JNISSUKBN-G.
                       09  SPA-GMN-JNISSUKBN               PIC X(01).
                       09  SPA-GMN-JNISSUKBNF              PIC X(01).
                       09  SPA-GMN-JNISSUKBNMEI            PIC X(78).
                   07  SPA-GMN-JNISSUTAIKBN-G.
                       09  SPA-GMN-JNISSUTAIKBN            PIC X(01).
                       09  SPA-GMN-JNISSUTAIKBNF           PIC X(01).
                       09  SPA-GMN-JNISSUTAIKBNMEI         PIC X(38).
                   07  SPA-GMN-TOTALTENKBN-G.
                       09  SPA-GMN-TOTALTENKBN             PIC X(01).
                       09  SPA-GMN-TOTALTENKBNF            PIC X(01).
                       09  SPA-GMN-TOTALTENKBNMEI          PIC X(78).
                   07  SPA-GMN-TOTALTENTAIKBN-G.
                       09  SPA-GMN-TOTALTENTAIKBN          PIC X(01).
                       09  SPA-GMN-TOTALTENTAIKBNF         PIC X(01).
                       09  SPA-GMN-TOTALTENTAIKBNMEI       PIC X(38).
                   07  SPA-GMN-SYUKEIKBN-G.
                       09  SPA-GMN-SYUKEIKBN               PIC X(01).
                       09  SPA-GMN-SYUKEIKBNF              PIC X(01).
                       09  SPA-GMN-SYUKEIKBNMEI            PIC X(78).
                   07  SPA-GMN-SYUKEITAIKBN-G.
                       09  SPA-GMN-SYUKEITAIKBN            PIC X(01).
                       09  SPA-GMN-SYUKEITAIKBNF           PIC X(01).
                       09  SPA-GMN-SYUKEITAIKBNMEI         PIC X(38).
                   07  SPA-GMN-FTNKEIKBNG-G.
                       09  SPA-GMN-FTNKEIKBNG              PIC X(01).
                       09  SPA-GMN-FTNKEIKBNGF             PIC X(01).
                       09  SPA-GMN-FTNKEIKBNGMEI           PIC X(78).
                   07  SPA-GMN-FTNKEITAIKBNG-G.
                       09  SPA-GMN-FTNKEITAIKBNG           PIC X(01).
                       09  SPA-GMN-FTNKEITAIFKBNG          PIC X(01).
                       09  SPA-GMN-FTNKEITAIMEIKBNG        PIC X(38).
                   07  SPA-GMN-FTNKEIKBNN-G.
                       09  SPA-GMN-FTNKEIKBNN              PIC X(01).
                       09  SPA-GMN-FTNKEIKBNNF             PIC X(01).
                       09  SPA-GMN-FTNKEIKBNNMEI           PIC X(78).
                   07  SPA-GMN-FTNKEITAIKBNN-G.
                       09  SPA-GMN-FTNKEITAIKBNN           PIC X(01).
                       09  SPA-GMN-FTNKEITAIKBNNF          PIC X(01).
                       09  SPA-GMN-FTNKEITAIKBNNMEI        PIC X(38).
                   07  SPA-GMN-FTNMONEYKBNG-G.
                       09  SPA-GMN-FTNMONEYKBNG            PIC X(01).
                       09  SPA-GMN-FTNMONEYKBNGF           PIC X(01).
                       09  SPA-GMN-FTNMONEYKBNGMEI         PIC X(78).
                   07  SPA-GMN-FTNMONEYTAIKBNG-G.
                       09  SPA-GMN-FTNMONEYTAIKBNG         PIC X(01).
                       09  SPA-GMN-FTNMONEYTAIKBNGF        PIC X(01).
                       09  SPA-GMN-FTNMONEYTAIKBNGMEI      PIC X(38).
                   07  SPA-GMN-FTNMONEYKBNN-G.
                       09  SPA-GMN-FTNMONEYKBNN            PIC X(01).
                       09  SPA-GMN-FTNMONEYKBNNF           PIC X(01).
                       09  SPA-GMN-FTNMONEYKBNNMEI         PIC X(78).
                   07  SPA-GMN-FTNMONEYTAIKBNN-G.
                       09  SPA-GMN-FTNMONEYTAIKBNN         PIC X(01).
                       09  SPA-GMN-FTNMONEYTAIKBNNF        PIC X(01).
                       09  SPA-GMN-FTNMONEYTAIKBNNMEI      PIC X(38).
                   07  SPA-GMN-FTNZEROKBNG-G.
                       09  SPA-GMN-FTNZEROKBNG             PIC X(01).
                       09  SPA-GMN-FTNZEROKBNGF            PIC X(01).
                       09  SPA-GMN-FTNZEROKBNGMEI          PIC X(78).
                   07  SPA-GMN-FTNZEROTAIKBNG-G.
                       09  SPA-GMN-FTNZEROTAIKBNG          PIC X(01).
                       09  SPA-GMN-FTNZEROTAIKBNGF         PIC X(01).
                       09  SPA-GMN-FTNZEROTAIKBNGMEI       PIC X(38).
                   07  SPA-GMN-FTNZEROKBNN-G.
                       09  SPA-GMN-FTNZEROKBNN             PIC X(01).
                       09  SPA-GMN-FTNZEROKBNNF            PIC X(01).
                       09  SPA-GMN-FTNZEROKBNNMEI          PIC X(78).
                   07  SPA-GMN-FTNZEROTAIKBNN-G.
                       09  SPA-GMN-FTNZEROTAIKBNN          PIC X(01).
                       09  SPA-GMN-FTNZEROTAIKBNNF         PIC X(01).
                       09  SPA-GMN-FTNZEROTAIKBNNMEI       PIC X(38).
                   07  SPA-GMN-SKJKBN-G.
                       09  SPA-GMN-SKJKBN                  PIC X(01).
                       09  SPA-GMN-SKJKBNF                 PIC X(01).
                       09  SPA-GMN-SKJKBNMEI               PIC X(78).
                   07  SPA-GMN-SKJTAIKBN-G.
                       09  SPA-GMN-SKJTAIKBN               PIC X(01).
                       09  SPA-GMN-SKJTAIKBNF              PIC X(01).
                       09  SPA-GMN-SKJTAIKBNMEI            PIC X(38).
                   07  SPA-GMN-OCRKBN-G.
                       09  SPA-GMN-OCRKBN                  PIC X(01).
                       09  SPA-GMN-OCRKBNF                 PIC X(01).
                       09  SPA-GMN-OCRKBNMEI               PIC X(78).
                   07  SPA-GMN-OCRTAIKBN-G.
                       09  SPA-GMN-OCRTAIKBN               PIC X(01).
                       09  SPA-GMN-OCRTAIKBNF              PIC X(01).
                       09  SPA-GMN-OCRTAIKBNMEI            PIC X(38).
                   07  SPA-GMN-SAIKEIKBN-G.
                       09  SPA-GMN-SAIKEIKBN               PIC X(01).
                       09  SPA-GMN-SAIKEIKBNF              PIC X(01).
                       09  SPA-GMN-SAIKEIKBNMEI            PIC X(78).
                   07  SPA-GMN-SAIKEITAIKBN-G.
                       09  SPA-GMN-SAIKEITAIKBN            PIC X(01).
                       09  SPA-GMN-SAIKEITAIKBNF           PIC X(01).
                       09  SPA-GMN-SAIKEITAIKBNMEI         PIC X(38).
                   07  SPA-GMN-HKNFTNKBNK-G.
                       09  SPA-GMN-HKNFTNKBNK              PIC X(01).
                       09  SPA-GMN-HKNFTNKBNKF             PIC X(01).
                       09  SPA-GMN-HKNFTNKBNKMEI           PIC X(78).
                   07  SPA-GMN-HKNFTNTAIKBNK-G.
                       09  SPA-GMN-HKNFTNTAIKBNK           PIC X(01).
                       09  SPA-GMN-HKNFTNTAIKBNKF          PIC X(01).
                       09  SPA-GMN-HKNFTNTAIKBNKMEI        PIC X(38).
                   07  SPA-GMN-HKNFTNKBNY-G.
                       09  SPA-GMN-HKNFTNKBNY              PIC X(01).
                       09  SPA-GMN-HKNFTNKBNYF             PIC X(01).
                       09  SPA-GMN-HKNFTNKBNYMEI           PIC X(78).
                   07  SPA-GMN-HKNFTNTAIKBNY-G.
                       09  SPA-GMN-HKNFTNTAIKBNY           PIC X(01).
                       09  SPA-GMN-HKNFTNTAIKBNYF          PIC X(01).
                       09  SPA-GMN-HKNFTNTAIKBNYMEI        PIC X(38).
                   07  SPA-GMN-RECEKSIKBNG-G.
                       09  SPA-GMN-RECEKSIKBNG             PIC X(01).
                       09  SPA-GMN-RECEKSIKBNGF            PIC X(01).
                       09  SPA-GMN-RECEKSIKBNGMEI          PIC X(78).
                   07  SPA-GMN-RECEKSITAIKBNG-G.
                       09  SPA-GMN-RECEKSITAIKBNG          PIC X(01).
                       09  SPA-GMN-RECEKSITAIKBNGF         PIC X(01).
                       09  SPA-GMN-RECEKSITAIKBNGMEI       PIC X(38).
                   07  SPA-GMN-RECEKSIKBNN-G.
                       09  SPA-GMN-RECEKSIKBNN             PIC X(01).
                       09  SPA-GMN-RECEKSIKBNNF            PIC X(01).
                       09  SPA-GMN-RECEKSIKBNNMEI          PIC X(78).
                   07  SPA-GMN-RECEKSITAIKBNN-G.
                       09  SPA-GMN-RECEKSITAIKBNN          PIC X(01).
                       09  SPA-GMN-RECEKSITAIKBNNF         PIC X(01).
                       09  SPA-GMN-RECEKSITAIKBNNMEI       PIC X(38).
                   07  SPA-GMN-CHKJGNKBNG-G.
                       09  SPA-GMN-CHKJGNKBNG              PIC X(01).
                       09  SPA-GMN-CHKJGNKBNGF             PIC X(01).
                       09  SPA-GMN-CHKJGNKBNGMEI           PIC X(78).
                   07  SPA-GMN-CHKJGNTAIKBNG-G.
                       09  SPA-GMN-CHKJGNTAIKBNG           PIC X(01).
                       09  SPA-GMN-CHKJGNTAIKBNGF          PIC X(01).
                       09  SPA-GMN-CHKJGNTAIKBNGMEI        PIC X(38).
                   07  SPA-GMN-CHKJGNKBNN-G.
                       09  SPA-GMN-CHKJGNKBNN              PIC X(01).
                       09  SPA-GMN-CHKJGNKBNNF             PIC X(01).
                       09  SPA-GMN-CHKJGNKBNNMEI           PIC X(78).
                   07  SPA-GMN-CHKJGNTAIKBNN-G.
                       09  SPA-GMN-CHKJGNTAIKBNN           PIC X(01).
                       09  SPA-GMN-CHKJGNTAIKBNNF          PIC X(01).
                       09  SPA-GMN-CHKJGNTAIKBNNMEI        PIC X(38).
                   07  SPA-GMN-KOHSKYKBNG-G.
                       09  SPA-GMN-KOHSKYKBNG              PIC X(01).
                       09  SPA-GMN-KOHSKYKBNGF             PIC X(01).
                       09  SPA-GMN-KOHSKYKBNGMEI           PIC X(78).
                   07  SPA-GMN-KOHSKYTAIKBNG-G.
                       09  SPA-GMN-KOHSKYTAIKBNG           PIC X(01).
                       09  SPA-GMN-KOHSKYTAIKBNGF          PIC X(01).
                       09  SPA-GMN-KOHSKYTAIKBNGMEI        PIC X(38).
                   07  SPA-GMN-KOHSKYKBNN-G.
                       09  SPA-GMN-KOHSKYKBNN              PIC X(01).
                       09  SPA-GMN-KOHSKYKBNNF             PIC X(01).
                       09  SPA-GMN-KOHSKYKBNNMEI           PIC X(78).
                   07  SPA-GMN-KOHSKYTAIKBNN-G.
                       09  SPA-GMN-KOHSKYTAIKBNN           PIC X(01).
                       09  SPA-GMN-KOHSKYTAIKBNNF          PIC X(01).
                       09  SPA-GMN-KOHSKYTAIKBNNMEI        PIC X(38).
                   07  SPA-GMN-KAKKBNK-G.
                       09  SPA-GMN-KAKKBNK                 PIC X(01).
                       09  SPA-GMN-KAKKBNKF                PIC X(01).
                       09  SPA-GMN-KAKKBNKMEI              PIC X(78).
                   07  SPA-GMN-KAKTAIKBNK-G.
                       09  SPA-GMN-KAKTAIKBNK              PIC X(01).
                       09  SPA-GMN-KAKTAIKBNKF             PIC X(01).
                       09  SPA-GMN-KAKTAIKBNKMEI           PIC X(38).
                   07  SPA-GMN-KAK21KBNK-G.
                       09  SPA-GMN-KAK21KBNK                 PIC X(01).
                       09  SPA-GMN-KAK21KBNKF                PIC X(01).
                       09  SPA-GMN-KAK21KBNKMEI              PIC X(78).
                   07  SPA-GMN-KAK21TAIKBNK-G.
                       09  SPA-GMN-KAK21TAIKBNK              PIC X(01).
                       09  SPA-GMN-KAK21TAIKBNKF             PIC X(01).
                       09  SPA-GMN-KAK21TAIKBNKMEI           PIC X(38).
                   07  SPA-GMN-KAK22KBNK-G.
                       09  SPA-GMN-KAK22KBNK                 PIC X(01).
                       09  SPA-GMN-KAK22KBNKF                PIC X(01).
                       09  SPA-GMN-KAK22KBNKMEI              PIC X(78).
                   07  SPA-GMN-KAK22TAIKBNK-G.
                       09  SPA-GMN-KAK22TAIKBNK              PIC X(01).
                       09  SPA-GMN-KAK22TAIKBNKF             PIC X(01).
                       09  SPA-GMN-KAK22TAIKBNKMEI           PIC X(38).
                   07  SPA-GMN-KAK23KBNK-G.
                       09  SPA-GMN-KAK23KBNK                 PIC X(01).
                       09  SPA-GMN-KAK23KBNKF                PIC X(01).
                       09  SPA-GMN-KAK23KBNKMEI              PIC X(78).
                   07  SPA-GMN-KAK23TAIKBNK-G.
                       09  SPA-GMN-KAK23TAIKBNK              PIC X(01).
                       09  SPA-GMN-KAK23TAIKBNKF             PIC X(01).
                       09  SPA-GMN-KAK23TAIKBNKMEI           PIC X(38).
                   07  SPA-GMN-KAK31KBNK-G.
                       09  SPA-GMN-KAK31KBNK                 PIC X(01).
                       09  SPA-GMN-KAK31KBNKF                PIC X(01).
                       09  SPA-GMN-KAK31KBNKMEI              PIC X(78).
                   07  SPA-GMN-KAK31TAIKBNK-G.
                       09  SPA-GMN-KAK31TAIKBNK              PIC X(01).
                       09  SPA-GMN-KAK31TAIKBNKF             PIC X(01).
                       09  SPA-GMN-KAK31TAIKBNKMEI           PIC X(38).
                   07  SPA-GMN-KAK32KBNK-G.
                       09  SPA-GMN-KAK32KBNK                 PIC X(01).
                       09  SPA-GMN-KAK32KBNKF                PIC X(01).
                       09  SPA-GMN-KAK32KBNKMEI              PIC X(78).
                   07  SPA-GMN-KAK32TAIKBNK-G.
                       09  SPA-GMN-KAK32TAIKBNK              PIC X(01).
                       09  SPA-GMN-KAK32TAIKBNKF             PIC X(01).
                       09  SPA-GMN-KAK32TAIKBNKMEI           PIC X(38).
                   07  SPA-GMN-KAK41KBNK-G.
                       09  SPA-GMN-KAK41KBNK                 PIC X(01).
                       09  SPA-GMN-KAK41KBNKF                PIC X(01).
                       09  SPA-GMN-KAK41KBNKMEI              PIC X(78).
                   07  SPA-GMN-KAK41TAIKBNK-G.
                       09  SPA-GMN-KAK41TAIKBNK              PIC X(01).
                       09  SPA-GMN-KAK41TAIKBNKF             PIC X(01).
                       09  SPA-GMN-KAK41TAIKBNKMEI           PIC X(38).
                   07  SPA-GMN-KAK42KBNK-G.
                       09  SPA-GMN-KAK42KBNK                 PIC X(01).
                       09  SPA-GMN-KAK42KBNKF                PIC X(01).
                       09  SPA-GMN-KAK42KBNKMEI              PIC X(78).
                   07  SPA-GMN-KAK42TAIKBNK-G.
                       09  SPA-GMN-KAK42TAIKBNK              PIC X(01).
                       09  SPA-GMN-KAK42TAIKBNKF             PIC X(01).
                       09  SPA-GMN-KAK42TAIKBNKMEI           PIC X(38).
                   07  SPA-GMN-KAK43KBNK-G.
                       09  SPA-GMN-KAK43KBNK                 PIC X(01).
                       09  SPA-GMN-KAK43KBNKF                PIC X(01).
                       09  SPA-GMN-KAK43KBNKMEI              PIC X(78).
                   07  SPA-GMN-KAK43TAIKBNK-G.
                       09  SPA-GMN-KAK43TAIKBNK              PIC X(01).
                       09  SPA-GMN-KAK43TAIKBNKF             PIC X(01).
                       09  SPA-GMN-KAK43TAIKBNKMEI           PIC X(38).
                   07  SPA-GMN-KAKKBNY-G.
                       09  SPA-GMN-KAKKBNY                 PIC X(01).
                       09  SPA-GMN-KAKKBNYF                PIC X(01).
                       09  SPA-GMN-KAKKBNYMEI              PIC X(78).
                   07  SPA-GMN-KAKTAIKBNY-G.
                       09  SPA-GMN-KAKTAIKBNY              PIC X(01).
                       09  SPA-GMN-KAKTAIKBNYF             PIC X(01).
                       09  SPA-GMN-KAKTAIKBNYMEI           PIC X(38).
                   07  SPA-GMN-KAK21KBNY-G.
                       09  SPA-GMN-KAK21KBNY                 PIC X(01).
                       09  SPA-GMN-KAK21KBNYF                PIC X(01).
                       09  SPA-GMN-KAK21KBNYMEI              PIC X(78).
                   07  SPA-GMN-KAK21TAIKBNY-G.
                       09  SPA-GMN-KAK21TAIKBNY              PIC X(01).
                       09  SPA-GMN-KAK21TAIKBNYF             PIC X(01).
                       09  SPA-GMN-KAK21TAIKBNYMEI           PIC X(38).
                   07  SPA-GMN-KAK22KBNY-G.
                       09  SPA-GMN-KAK22KBNY                 PIC X(01).
                       09  SPA-GMN-KAK22KBNYF                PIC X(01).
                       09  SPA-GMN-KAK22KBNYMEI              PIC X(78).
                   07  SPA-GMN-KAK22TAIKBNY-G.
                       09  SPA-GMN-KAK22TAIKBNY              PIC X(01).
                       09  SPA-GMN-KAK22TAIKBNYF             PIC X(01).
                       09  SPA-GMN-KAK22TAIKBNYMEI           PIC X(38).
                   07  SPA-GMN-KAK23KBNY-G.
                       09  SPA-GMN-KAK23KBNY                 PIC X(01).
                       09  SPA-GMN-KAK23KBNYF                PIC X(01).
                       09  SPA-GMN-KAK23KBNYMEI              PIC X(78).
                   07  SPA-GMN-KAK23TAIKBNY-G.
                       09  SPA-GMN-KAK23TAIKBNY              PIC X(01).
                       09  SPA-GMN-KAK23TAIKBNYF             PIC X(01).
                       09  SPA-GMN-KAK23TAIKBNYMEI           PIC X(38).
                   07  SPA-GMN-KAK31KBNY-G.
                       09  SPA-GMN-KAK31KBNY                 PIC X(01).
                       09  SPA-GMN-KAK31KBNYF                PIC X(01).
                       09  SPA-GMN-KAK31KBNYMEI              PIC X(78).
                   07  SPA-GMN-KAK31TAIKBNY-G.
                       09  SPA-GMN-KAK31TAIKBNY              PIC X(01).
                       09  SPA-GMN-KAK31TAIKBNYF             PIC X(01).
                       09  SPA-GMN-KAK31TAIKBNYMEI           PIC X(38).
                   07  SPA-GMN-KAK32KBNY-G.
                       09  SPA-GMN-KAK32KBNY                 PIC X(01).
                       09  SPA-GMN-KAK32KBNYF                PIC X(01).
                       09  SPA-GMN-KAK32KBNYMEI              PIC X(78).
                   07  SPA-GMN-KAK32TAIKBNY-G.
                       09  SPA-GMN-KAK32TAIKBNY              PIC X(01).
                       09  SPA-GMN-KAK32TAIKBNYF             PIC X(01).
                       09  SPA-GMN-KAK32TAIKBNYMEI           PIC X(38).
                   07  SPA-GMN-KAK41KBNY-G.
                       09  SPA-GMN-KAK41KBNY                 PIC X(01).
                       09  SPA-GMN-KAK41KBNYF                PIC X(01).
                       09  SPA-GMN-KAK41KBNYMEI              PIC X(78).
                   07  SPA-GMN-KAK41TAIKBNY-G.
                       09  SPA-GMN-KAK41TAIKBNY              PIC X(01).
                       09  SPA-GMN-KAK41TAIKBNYF             PIC X(01).
                       09  SPA-GMN-KAK41TAIKBNYMEI           PIC X(38).
                   07  SPA-GMN-KAK42KBNY-G.
                       09  SPA-GMN-KAK42KBNY                 PIC X(01).
                       09  SPA-GMN-KAK42KBNYF                PIC X(01).
                       09  SPA-GMN-KAK42KBNYMEI              PIC X(78).
                   07  SPA-GMN-KAK42TAIKBNY-G.
                       09  SPA-GMN-KAK42TAIKBNY              PIC X(01).
                       09  SPA-GMN-KAK42TAIKBNYF             PIC X(01).
                       09  SPA-GMN-KAK42TAIKBNYMEI           PIC X(38).
                   07  SPA-GMN-KAK43KBNY-G.
                       09  SPA-GMN-KAK43KBNY                 PIC X(01).
                       09  SPA-GMN-KAK43KBNYF                PIC X(01).
                       09  SPA-GMN-KAK43KBNYMEI              PIC X(78).
                   07  SPA-GMN-KAK43TAIKBNY-G.
                       09  SPA-GMN-KAK43TAIKBNY              PIC X(01).
                       09  SPA-GMN-KAK43TAIKBNYF             PIC X(01).
                       09  SPA-GMN-KAK43TAIKBNYMEI           PIC X(38).
                   07  SPA-GMN-KSINGKBNKG-G.
                       09  SPA-GMN-KSINGKBNKG                 PIC X(01).
                       09  SPA-GMN-KSINGKBNKGF                PIC X(01).
                       09  SPA-GMN-KSINGKBNKGMEI              PIC X(78).
                   07  SPA-GMN-KSINGTAIKBNKG-G.
                       09  SPA-GMN-KSINGTAIKBNKG              PIC X(01).
                       09  SPA-GMN-KSINGTAIKBNKGF             PIC X(01).
                       09  SPA-GMN-KSINGTAIKBNKGMEI           PIC X(38).
                   07  SPA-GMN-KSINGKBNKN-G.
                       09  SPA-GMN-KSINGKBNKN                 PIC X(01).
                       09  SPA-GMN-KSINGKBNKNF                PIC X(01).
                       09  SPA-GMN-KSINGKBNKNMEI              PIC X(78).
                   07  SPA-GMN-KSINGTAIKBNKN-G.
                       09  SPA-GMN-KSINGTAIKBNKN              PIC X(01).
                       09  SPA-GMN-KSINGTAIKBNKNF             PIC X(01).
                       09  SPA-GMN-KSINGTAIKBNKNMEI           PIC X(38).
                   07  SPA-GMN-KSINGKBNYG-G.
                       09  SPA-GMN-KSINGKBNYG                 PIC X(01).
                       09  SPA-GMN-KSINGKBNYGF                PIC X(01).
                       09  SPA-GMN-KSINGKBNYGMEI              PIC X(78).
                   07  SPA-GMN-KSINGTAIKBNYG-G.
                       09  SPA-GMN-KSINGTAIKBNYG              PIC X(01).
                       09  SPA-GMN-KSINGTAIKBNYGF             PIC X(01).
                       09  SPA-GMN-KSINGTAIKBNYGMEI           PIC X(38).
                   07  SPA-GMN-KSINGKBNYN-G.
                       09  SPA-GMN-KSINGKBNYN                 PIC X(01).
                       09  SPA-GMN-KSINGKBNYNF                PIC X(01).
                       09  SPA-GMN-KSINGKBNYNMEI              PIC X(78).
                   07  SPA-GMN-KSINGTAIKBNYN-G.
                       09  SPA-GMN-KSINGTAIKBNYN              PIC X(01).
                       09  SPA-GMN-KSINGTAIKBNYNF             PIC X(01).
                       09  SPA-GMN-KSINGTAIKBNYNMEI           PIC X(38).
                   07  SPA-GMN-ENCHGKBNKG-G.
                       09  SPA-GMN-ENCHGKBNKG                 PIC X(01).
                       09  SPA-GMN-ENCHGKBNKGF                PIC X(01).
                       09  SPA-GMN-ENCHGKBNKGMEI              PIC X(78).
                   07  SPA-GMN-ENCHGTAIKBNKG-G.
                       09  SPA-GMN-ENCHGTAIKBNKG              PIC X(01).
                       09  SPA-GMN-ENCHGTAIKBNKGF             PIC X(01).
                       09  SPA-GMN-ENCHGTAIKBNKGMEI           PIC X(38).
                   07  SPA-GMN-ENCHGKBNKN-G.
                       09  SPA-GMN-ENCHGKBNKN                 PIC X(01).
                       09  SPA-GMN-ENCHGKBNKNF                PIC X(01).
                       09  SPA-GMN-ENCHGKBNKNMEI              PIC X(78).
                   07  SPA-GMN-ENCHGTAIKBNKN-G.
                       09  SPA-GMN-ENCHGTAIKBNKN              PIC X(01).
                       09  SPA-GMN-ENCHGTAIKBNKNF             PIC X(01).
                       09  SPA-GMN-ENCHGTAIKBNKNMEI           PIC X(38).
                   07  SPA-GMN-ENCHGKBNYG-G.
                       09  SPA-GMN-ENCHGKBNYG                 PIC X(01).
                       09  SPA-GMN-ENCHGKBNYGF                PIC X(01).
                       09  SPA-GMN-ENCHGKBNYGMEI              PIC X(78).
                   07  SPA-GMN-ENCHGTAIKBNYG-G.
                       09  SPA-GMN-ENCHGTAIKBNYG              PIC X(01).
                       09  SPA-GMN-ENCHGTAIKBNYGF             PIC X(01).
                       09  SPA-GMN-ENCHGTAIKBNYGMEI           PIC X(38).
                   07  SPA-GMN-ENCHGKBNYN-G.
                       09  SPA-GMN-ENCHGKBNYN                 PIC X(01).
                       09  SPA-GMN-ENCHGKBNYNF                PIC X(01).
                       09  SPA-GMN-ENCHGKBNYNMEI              PIC X(78).
                   07  SPA-GMN-ENCHGTAIKBNYN-G.
                       09  SPA-GMN-ENCHGTAIKBNYN              PIC X(01).
                       09  SPA-GMN-ENCHGTAIKBNYNF             PIC X(01).
                       09  SPA-GMN-ENCHGTAIKBNYNMEI           PIC X(38).
                   07  SPA-GMN-KOH2SKJKBN-G.
                       09  SPA-GMN-KOH2SKJKBN                 PIC X(01).
                       09  SPA-GMN-KOH2SKJKBNF                PIC X(01).
                       09  SPA-GMN-KOH2SKJKBNMEI              PIC X(78).
                   07  SPA-GMN-KOH2SKJTAIKBN-G.
                       09  SPA-GMN-KOH2SKJTAIKBN              PIC X(01).
                       09  SPA-GMN-KOH2SKJTAIKBNF             PIC X(01).
                       09  SPA-GMN-KOH2SKJTAIKBNMEI           PIC X(38).
                   07  SPA-GMN-FTNHASUKBNG-G.
                       09  SPA-GMN-FTNHASUKBNG                PIC X(01).
                       09  SPA-GMN-FTNHASUKBNGF               PIC X(01).
                       09  SPA-GMN-FTNHASUKBNGMEI             PIC X(78).
                   07  SPA-GMN-FTNHASUTAIKBNG-G.
                       09  SPA-GMN-FTNHASUTAIKBNG             PIC X(01).
                       09  SPA-GMN-FTNHASUTAIKBNGF            PIC X(01).
                       09  SPA-GMN-FTNHASUTAIKBNGMEI          PIC X(38).
                   07  SPA-GMN-FTNHASUKBNN-G.
                       09  SPA-GMN-FTNHASUKBNN                PIC X(01).
                       09  SPA-GMN-FTNHASUKBNNF               PIC X(01).
                       09  SPA-GMN-FTNHASUKBNNMEI             PIC X(78).
                   07  SPA-GMN-FTNHASUTAIKBNN-G.
                       09  SPA-GMN-FTNHASUTAIKBNN             PIC X(01).
                       09  SPA-GMN-FTNHASUTAIKBNNF            PIC X(01).
                       09  SPA-GMN-FTNHASUTAIKBNNMEI          PIC X(38).
                   07  SPA-GMN-JYUHENKBN-G.
                       09  SPA-GMN-JYUHENKBN               PIC X(01).
                       09  SPA-GMN-JYUHENKBNF              PIC X(01).
                       09  SPA-GMN-JYUHENKBNMEI            PIC X(78).
                   07  SPA-GMN-JYUHENTAIKBN-G.
                       09  SPA-GMN-JYUHENTAIKBN            PIC X(01).
                       09  SPA-GMN-JYUHENTAIKBNF           PIC X(01).
                       09  SPA-GMN-JYUHENTAIKBNMEI         PIC X(38).
                   07  SPA-GMN-JGNFTNKBN-G.
                       09  SPA-GMN-JGNFTNKBN               PIC X(01).
                       09  SPA-GMN-JGNFTNKBNF              PIC X(01).
                       09  SPA-GMN-JGNFTNKBNMEI            PIC X(78).
                   07  SPA-GMN-JGNFTNTAIKBN-G.
                       09  SPA-GMN-JGNFTNTAIKBN            PIC X(01).
                       09  SPA-GMN-JGNFTNTAIKBNF           PIC X(01).
                       09  SPA-GMN-JGNFTNTAIKBNMEI         PIC X(38).
                   07  SPA-GMN-PTJGNKBN-G.
                       09  SPA-GMN-PTJGNKBN                PIC X(01).
                       09  SPA-GMN-PTJGNKBNF               PIC X(01).
                       09  SPA-GMN-PTJGNKBNMEI             PIC X(78).
                   07  SPA-GMN-PTJGNTAIKBN-G.
                       09  SPA-GMN-PTJGNTAIKBN             PIC X(01).
                       09  SPA-GMN-PTJGNTAIKBNF            PIC X(01).
                       09  SPA-GMN-PTJGNTAIKBNMEI          PIC X(38).
                   07  SPA-GMN-IRYOKEIKBNG-G.
                       09  SPA-GMN-IRYOKEIKBNG             PIC X(01).
                       09  SPA-GMN-IRYOKEIKBNGF            PIC X(01).
                       09  SPA-GMN-IRYOKEIKBNGMEI          PIC X(78).
                   07  SPA-GMN-IRYOKEITAIKBNG-G.
                       09  SPA-GMN-IRYOKEITAIKBNG          PIC X(01).
                       09  SPA-GMN-IRYOKEITAIKBNGF         PIC X(01).
                       09  SPA-GMN-IRYOKEITAIKBNGMEI       PIC X(38).
                   07  SPA-GMN-IRYOKEIKBNN-G.
                       09  SPA-GMN-IRYOKEIKBNN             PIC X(01).
                       09  SPA-GMN-IRYOKEIKBNNF            PIC X(01).
                       09  SPA-GMN-IRYOKEIKBNNMEI          PIC X(78).
                   07  SPA-GMN-IRYOKEITAIKBNN-G.
                       09  SPA-GMN-IRYOKEITAIKBNN          PIC X(01).
                       09  SPA-GMN-IRYOKEITAIKBNNF         PIC X(01).
                       09  SPA-GMN-IRYOKEITAIKBNNMEI       PIC X(38).
                   07  SPA-GMN-SKJKEIKBN-G.
                       09  SPA-GMN-SKJKEIKBN               PIC X(01).
                       09  SPA-GMN-SKJKEIKBNF              PIC X(01).
                       09  SPA-GMN-SKJKEIKBNMEI            PIC X(78).
                   07  SPA-GMN-SKJKEITAIKBN-G.
                       09  SPA-GMN-SKJKEITAIKBN            PIC X(01).
                       09  SPA-GMN-SKJKEITAIKBNF           PIC X(01).
                       09  SPA-GMN-SKJKEITAIKBNMEI         PIC X(38).
                   07  SPA-GMN-KINJYOKBNG-G.
                       09  SPA-GMN-KINJYOKBNG               PIC X(01).
                       09  SPA-GMN-KINJYOKBNGF              PIC X(01).
                       09  SPA-GMN-KINJYOKBNGMEI            PIC X(78).
                   07  SPA-GMN-KINJYOTAIKBNG-G.
                       09  SPA-GMN-KINJYOTAIKBNG            PIC X(01).
                       09  SPA-GMN-KINJYOTAIKBNGF           PIC X(01).
                       09  SPA-GMN-KINJYOTAIKBNGMEI         PIC X(38).
                   07  SPA-GMN-KINJYOKBNN-G.
                       09  SPA-GMN-KINJYOKBNN               PIC X(01).
                       09  SPA-GMN-KINJYOKBNNF              PIC X(01).
                       09  SPA-GMN-KINJYOKBNNMEI            PIC X(78).
                   07  SPA-GMN-KINJYOTAIKBNN-G.
                       09  SPA-GMN-KINJYOTAIKBNN            PIC X(01).
                       09  SPA-GMN-KINJYOTAIKBNNF           PIC X(01).
                       09  SPA-GMN-KINJYOTAIKBNNMEI         PIC X(38).
                   07  SPA-GMN-INNAIKBN-G.
                       09  SPA-GMN-INNAIKBN                PIC X(01).
                       09  SPA-GMN-INNAIKBNF               PIC X(01).
                       09  SPA-GMN-INNAIKBNMEI             PIC X(78).
                   07  SPA-GMN-INNAITAIKBN-G.
                       09  SPA-GMN-INNAITAIKBN             PIC X(01).
                       09  SPA-GMN-INNAITAIKBNF            PIC X(01).
                       09  SPA-GMN-INNAITAIKBNMEI          PIC X(38).
                   07  SPA-GMN-JIRITUKBN-G.
                       09  SPA-GMN-JIRITUKBN                PIC X(01).
                       09  SPA-GMN-JIRITUKBNF               PIC X(01).
                       09  SPA-GMN-JIRITUKBNMEI             PIC X(78).
                   07  SPA-GMN-JIRITUTAIKBN-G.
                       09  SPA-GMN-JIRITUTAIKBN             PIC X(01).
                       09  SPA-GMN-JIRITUTAIKBNF            PIC X(01).
                       09  SPA-GMN-JIRITUTAIKBNMEI          PIC X(38).
                   07  SPA-GMN-HONTSYTNK-G.
                     08  SPA-GMN-SKJRYOKBNHON-G.
                       09  SPA-GMN-SKJRYOKBNHON            PIC X(01).
                       09  SPA-GMN-SKJRYOKBNHONF           PIC X(01).
                       09  SPA-GMN-SKJRYOKBNHONMEI         PIC X(78).
                     08  SPA-GMN-SKJRYOTAIKBNHON-G.
                       09  SPA-GMN-SKJRYOTAIKBNHON         PIC X(01).
                       09  SPA-GMN-SKJRYOTAIKBNHONF        PIC X(01).
                       09  SPA-GMN-SKJRYOTAIKBNHONMEI      PIC X(38).
                     08  SPA-GMN-SEIRYOKBNHON-G.
                       09  SPA-GMN-SEIRYOKBNHON            PIC X(01).
                       09  SPA-GMN-SEIRYOKBNHONF           PIC X(01).
                       09  SPA-GMN-SEIRYOKBNHONMEI         PIC X(78).
                     08  SPA-GMN-SEIRYOTAIKBNHON-G.
                       09  SPA-GMN-SEIRYOTAIKBNHON         PIC X(01).
                       09  SPA-GMN-SEIRYOTAIKBNHONF        PIC X(01).
                       09  SPA-GMN-SEIRYOTAIKBNHONMEI      PIC X(38).
                     08  SPA-GMN-SEIKANKBNHON-G.
                       09  SPA-GMN-SEIKANKBNHON            PIC X(01).
                       09  SPA-GMN-SEIKANKBNHONF           PIC X(01).
                       09  SPA-GMN-SEIKANKBNHONMEI         PIC X(78).
                     08  SPA-GMN-SEIKANTAIKBNHON-G.
                       09  SPA-GMN-SEIKANTAIKBNHON         PIC X(01).
                       09  SPA-GMN-SEIKANTAIKBNHONF        PIC X(01).
                       09  SPA-GMN-SEIKANTAIKBNHONMEI      PIC X(38).
                     08  SPA-GMN-SKJRYOKBNTSY-G.
                       09  SPA-GMN-SKJRYOKBNTSY            PIC X(01).
                       09  SPA-GMN-SKJRYOKBNTSYF           PIC X(01).
                       09  SPA-GMN-SKJRYOKBNTSYMEI         PIC X(78).
                     08  SPA-GMN-SKJRYOTAIKBNTSY-G.
                       09  SPA-GMN-SKJRYOTAIKBNTSY         PIC X(01).
                       09  SPA-GMN-SKJRYOTAIKBNTSYF        PIC X(01).
                       09  SPA-GMN-SKJRYOTAIKBNTSYMEI      PIC X(38).
                     08  SPA-GMN-SEIRYOKBNTSY-G.
                       09  SPA-GMN-SEIRYOKBNTSY            PIC X(01).
                       09  SPA-GMN-SEIRYOKBNTSYF           PIC X(01).
                       09  SPA-GMN-SEIRYOKBNTSYMEI         PIC X(78).
                     08  SPA-GMN-SEIRYOTAIKBNTSY-G.
                       09  SPA-GMN-SEIRYOTAIKBNTSY         PIC X(01).
                       09  SPA-GMN-SEIRYOTAIKBNTSYF        PIC X(01).
                       09  SPA-GMN-SEIRYOTAIKBNTSYMEI      PIC X(38).
                     08  SPA-GMN-SEIKANKBNTSY-G.
                       09  SPA-GMN-SEIKANKBNTSY            PIC X(01).
                       09  SPA-GMN-SEIKANKBNTSYF           PIC X(01).
                       09  SPA-GMN-SEIKANKBNTSYMEI         PIC X(78).
                     08  SPA-GMN-SEIKANTAIKBNTSY-G.
                       09  SPA-GMN-SEIKANTAIKBNTSY         PIC X(01).
                       09  SPA-GMN-SEIKANTAIKBNTSYF        PIC X(01).
                       09  SPA-GMN-SEIKANTAIKBNTSYMEI      PIC X(38).
                     08  SPA-GMN-SKJRYOKBNTNK-G.
                       09  SPA-GMN-SKJRYOKBNTNK            PIC X(01).
                       09  SPA-GMN-SKJRYOKBNTNKF           PIC X(01).
                       09  SPA-GMN-SKJRYOKBNTNKMEI         PIC X(78).
                     08  SPA-GMN-SKJRYOTAIKBNTNK-G.
                       09  SPA-GMN-SKJRYOTAIKBNTNK         PIC X(01).
                       09  SPA-GMN-SKJRYOTAIKBNTNKF        PIC X(01).
                       09  SPA-GMN-SKJRYOTAIKBNTNKMEI      PIC X(38).
                     08  SPA-GMN-SEIRYOKBNTNK-G.
                       09  SPA-GMN-SEIRYOKBNTNK            PIC X(01).
                       09  SPA-GMN-SEIRYOKBNTNKF           PIC X(01).
                       09  SPA-GMN-SEIRYOKBNTNKMEI         PIC X(78).
                     08  SPA-GMN-SEIRYOTAIKBNTNK-G.
                       09  SPA-GMN-SEIRYOTAIKBNTNK         PIC X(01).
                       09  SPA-GMN-SEIRYOTAIKBNTNKF        PIC X(01).
                       09  SPA-GMN-SEIRYOTAIKBNTNKMEI      PIC X(38).
                     08  SPA-GMN-SEIKANKBNTNK-G.
                       09  SPA-GMN-SEIKANKBNTNK            PIC X(01).
                       09  SPA-GMN-SEIKANKBNTNKF           PIC X(01).
                       09  SPA-GMN-SEIKANKBNTNKMEI         PIC X(78).
                     08  SPA-GMN-SEIKANTAIKBNTNK-G.
                       09  SPA-GMN-SEIKANTAIKBNTNK         PIC X(01).
                       09  SPA-GMN-SEIKANTAIKBNTNKF        PIC X(01).
                       09  SPA-GMN-SEIKANTAIKBNTNKMEI      PIC X(38).
                   07  SPA-GMN-HONTSYTNK-R    REDEFINES
                                              SPA-GMN-HONTSYTNK-G.
                     08  SPA-GMN-HONTSYTNK-OCC       OCCURS  3.
                       09  SPA-GMN-SKJRYOKBN-G.
                         11  SPA-GMN-SKJRYOKBN             PIC X(01).
                         11  SPA-GMN-SKJRYOKBNF            PIC X(01).
                         11  SPA-GMN-SKJRYOKBNMEI          PIC X(78).
                       09  SPA-GMN-SKJRYOTAIKBN-G.
                         11  SPA-GMN-SKJRYOTAIKBN          PIC X(01).
                         11  SPA-GMN-SKJRYOTAIKBNF         PIC X(01).
                         11  SPA-GMN-SKJRYOTAIKBNMEI       PIC X(38).
                       09  SPA-GMN-SEIRYOKBN-G. 
                         11  SPA-GMN-SEIRYOKBN             PIC X(01).
                         11  SPA-GMN-SEIRYOKBNF            PIC X(01).
                         11  SPA-GMN-SEIRYOKBNMEI          PIC X(78).
                       09  SPA-GMN-SEIRYOTAIKBN-G.
                         11  SPA-GMN-SEIRYOTAIKBN          PIC X(01).
                         11  SPA-GMN-SEIRYOTAIKBNF         PIC X(01).
                         11  SPA-GMN-SEIRYOTAIKBNMEI       PIC X(38).
                       09  SPA-GMN-SEIKANKBN-G.
                         11  SPA-GMN-SEIKANKBN             PIC X(01).
                         11  SPA-GMN-SEIKANKBNF            PIC X(01).
                         11  SPA-GMN-SEIKANKBNMEI          PIC X(78).
                       09  SPA-GMN-SEIKANTAIKBN-G.
                         11  SPA-GMN-SEIKANTAIKBN          PIC X(01).
                         11  SPA-GMN-SEIKANTAIKBNF         PIC X(01).
                         11  SPA-GMN-SEIKANTAIKBNMEI       PIC X(38).
                   07  SPA-GMN-KAICNTKBNG-G.
                       09  SPA-GMN-KAICNTKBNG              PIC X(01).
                       09  SPA-GMN-KAICNTKBNGF             PIC X(01).
                       09  SPA-GMN-KAICNTKBNGMEI           PIC X(78).
                   07  SPA-GMN-KAICNTTAIKBNG-G.
                       09  SPA-GMN-KAICNTTAIKBNG           PIC X(01).
                       09  SPA-GMN-KAICNTTAIKBNGF          PIC X(01).
                       09  SPA-GMN-KAICNTTAIKBNGMEI        PIC X(38).
                   07  SPA-GMN-CYOKEIKBNG-G.
                       09  SPA-GMN-CYOKEIKBNG              PIC X(01).
                       09  SPA-GMN-CYOKEIKBNGF             PIC X(01).
                       09  SPA-GMN-CYOKEIKBNGMEI           PIC X(78).
                   07  SPA-GMN-CYOKEITAIKBNG-G.
                       09  SPA-GMN-CYOKEITAIKBNG           PIC X(01).
                       09  SPA-GMN-CYOKEITAIKBNGF          PIC X(01).
                       09  SPA-GMN-CYOKEITAIKBNGMEI        PIC X(38).
                   07  SPA-GMN-GNBKEIKBNKG-G.
                       09  SPA-GMN-GNBKEIKBNKG             PIC X(01).
                       09  SPA-GMN-GNBKEIKBNKGF            PIC X(01).
                       09  SPA-GMN-GNBKEIKBNKGMEI          PIC X(78).
                   07  SPA-GMN-GNBKEITAIKBNKG-G.
                       09  SPA-GMN-GNBKEITAIKBNKG          PIC X(01).
                       09  SPA-GMN-GNBKEITAIKBNKGF         PIC X(01).
                       09  SPA-GMN-GNBKEITAIKBNKGMEI       PIC X(38).
                   07  SPA-GMN-ZENKEIKBNG-G.
                       09  SPA-GMN-ZENKEIKBNG              PIC X(01).
                       09  SPA-GMN-ZENKEIKBNGF             PIC X(01).
                       09  SPA-GMN-ZENKEIKBNGMEI           PIC X(78).
                   07  SPA-GMN-ZENKEITAIKBNG-G.
                       09  SPA-GMN-ZENKEITAIKBNG           PIC X(01).
                       09  SPA-GMN-ZENKEITAIKBNGF          PIC X(01).
                       09  SPA-GMN-ZENKEITAIKBNGMEI        PIC X(38).
                   07  SPA-GMN-SINKEIKBNG-G.
                       09  SPA-GMN-SINKEIKBNG              PIC X(01).
                       09  SPA-GMN-SINKEIKBNGF             PIC X(01).
                       09  SPA-GMN-SINKEIKBNGMEI           PIC X(78).
                   07  SPA-GMN-SINKEITAIKBNG-G.
                       09  SPA-GMN-SINKEITAIKBNG           PIC X(01).
                       09  SPA-GMN-SINKEITAIKBNGF          PIC X(01).
                       09  SPA-GMN-SINKEITAIKBNGMEI        PIC X(38).
                   07  SPA-GMN-SYOKEIKBNG-G.
                       09  SPA-GMN-SYOKEIKBNG              PIC X(01).
                       09  SPA-GMN-SYOKEIKBNGF             PIC X(01).
                       09  SPA-GMN-SYOKEIKBNGMEI           PIC X(78).
                   07  SPA-GMN-SYOKEITAIKBNG-G.
                       09  SPA-GMN-SYOKEITAIKBNG           PIC X(01).
                       09  SPA-GMN-SYOKEITAIKBNGF          PIC X(01).
                       09  SPA-GMN-SYOKEITAIKBNGMEI        PIC X(38).
                   07  SPA-GMN-SYOKEIKBNN-G.
                       09  SPA-GMN-SYOKEIKBNN              PIC X(01).
                       09  SPA-GMN-SYOKEIKBNNF             PIC X(01).
                       09  SPA-GMN-SYOKEIKBNNMEI           PIC X(78).
                   07  SPA-GMN-SYOKEITAIKBNN-G.
                       09  SPA-GMN-SYOKEITAIKBNN           PIC X(01).
                       09  SPA-GMN-SYOKEITAIKBNNF          PIC X(01).
                       09  SPA-GMN-SYOKEITAIKBNNMEI        PIC X(38).
                   07  SPA-GMN-NYUKEIKBNN-G.
                       09  SPA-GMN-NYUKEIKBNN              PIC X(01).
                       09  SPA-GMN-NYUKEIKBNNF             PIC X(01).
                       09  SPA-GMN-NYUKEIKBNNMEI           PIC X(78).
                   07  SPA-GMN-NYUKEITAIKBNN-G.
                       09  SPA-GMN-NYUKEITAIKBNN           PIC X(01).
                       09  SPA-GMN-NYUKEITAIKBNNF          PIC X(01).
                       09  SPA-GMN-NYUKEITAIKBNNMEI        PIC X(38).
                   07  SPA-GMN-DAYKEIKBNN-G.
                       09  SPA-GMN-DAYKEIKBNN              PIC X(01).
                       09  SPA-GMN-DAYKEIKBNNF             PIC X(01).
                       09  SPA-GMN-DAYKEIKBNNMEI           PIC X(78).
                   07  SPA-GMN-DAYKEITAIKBNN-G.
                       09  SPA-GMN-DAYKEITAIKBNN           PIC X(01).
                       09  SPA-GMN-DAYKEITAIKBNNF          PIC X(01).
                       09  SPA-GMN-DAYKEITAIKBNNMEI        PIC X(38).
                   07  SPA-GMN-NINNASIKBN-G.
                       09  SPA-GMN-NINNASIKBN              PIC X(01).
                       09  SPA-GMN-NINNASIKBNF             PIC X(01).
                       09  SPA-GMN-NINNASIKBNMEI           PIC X(78).
                   07  SPA-GMN-NINNASITAIKBN-G.
                       09  SPA-GMN-NINNASITAIKBN           PIC X(01).
                       09  SPA-GMN-NINNASITAIKBNF          PIC X(01).
                       09  SPA-GMN-NINNASITAIKBNMEI        PIC X(38).
                   07  SPA-GMN-NINJYOKBN-G.
                       09  SPA-GMN-NINJYOKBN               PIC X(01).
                       09  SPA-GMN-NINJYOKBNF              PIC X(01).
                       09  SPA-GMN-NINJYOKBNMEI            PIC X(78).
                   07  SPA-GMN-NINJYOTAIKBN-G.
                       09  SPA-GMN-NINJYOTAIKBN            PIC X(01).
                       09  SPA-GMN-NINJYOTAIKBNF           PIC X(01).
                       09  SPA-GMN-NINJYOTAIKBNMEI         PIC X(38).
                   07  SPA-GMN-KOGAKKEIKBN-G.
                       09  SPA-GMN-KOGAKKEIKBN               PIC X(01).
                       09  SPA-GMN-KOGAKKEIKBNF              PIC X(01).
                       09  SPA-GMN-KOGAKKEIKBNMEI            PIC X(78).
                   07  SPA-GMN-KOGAKKEITAIKBN-G.
                       09  SPA-GMN-KOGAKKEITAIKBN            PIC X(01).
                       09  SPA-GMN-KOGAKKEITAIKBNF           PIC X(01).
                       09  SPA-GMN-KOGAKKEITAIKBNMEI         PIC X(38).
                   07  SPA-GMN-TANKIKBN-G.
                       09  SPA-GMN-TANKIKBN               PIC X(01).
                       09  SPA-GMN-TANKIKBNF              PIC X(01).
                       09  SPA-GMN-TANKIKBNMEI            PIC X(78).
                   07  SPA-GMN-TANKITAIKBN-G.
                       09  SPA-GMN-TANKITAIKBN            PIC X(01).
                       09  SPA-GMN-TANKITAIKBNF           PIC X(01).
                       09  SPA-GMN-TANKITAIKBNMEI         PIC X(38).
                   07  SPA-GMN-KAICNTKBNN-G.
                       09  SPA-GMN-KAICNTKBNN               PIC X(01).
                       09  SPA-GMN-KAICNTKBNNF              PIC X(01).
                       09  SPA-GMN-KAICNTKBNNMEI            PIC X(78).
                   07  SPA-GMN-KAICNTTAIKBNN-G.
                       09  SPA-GMN-KAICNTTAIKBNN            PIC X(01).
                       09  SPA-GMN-KAICNTTAIKBNNF           PIC X(01).
                       09  SPA-GMN-KAICNTTAIKBNNMEI         PIC X(38).
                   07  SPA-GMN-CYOKEIKBNN-G.
                       09  SPA-GMN-CYOKEIKBNN               PIC X(01).
                       09  SPA-GMN-CYOKEIKBNNF              PIC X(01).
                       09  SPA-GMN-CYOKEIKBNNMEI            PIC X(78).
                   07  SPA-GMN-CYOKEITAIKBNN-G.
                       09  SPA-GMN-CYOKEITAIKBNN            PIC X(01).
                       09  SPA-GMN-CYOKEITAIKBNNF           PIC X(01).
                       09  SPA-GMN-CYOKEITAIKBNNMEI         PIC X(38).
                   07  SPA-GMN-ZENDAYKBNN-G.
                       09  SPA-GMN-ZENDAYKBNN               PIC X(01).
                       09  SPA-GMN-ZENDAYKBNNF              PIC X(01).
                       09  SPA-GMN-ZENDAYKBNNMEI            PIC X(78).
                   07  SPA-GMN-ZENDAYTAIKBNN-G.
                       09  SPA-GMN-ZENDAYTAIKBNN            PIC X(01).
                       09  SPA-GMN-ZENDAYTAIKBNNF           PIC X(01).
                       09  SPA-GMN-ZENDAYTAIKBNNMEI         PIC X(38).
                   07  SPA-GMN-ZENKEI2KBNG-G.
                       09  SPA-GMN-ZENKEI2KBNG              PIC X(01).
                       09  SPA-GMN-ZENKEI2KBNGF             PIC X(01).
                       09  SPA-GMN-ZENKEI2KBNGMEI           PIC X(78).
                   07  SPA-GMN-ZENKEI2TAIKBNG-G.
                       09  SPA-GMN-ZENKEI2TAIKBNG           PIC X(01).
                       09  SPA-GMN-ZENKEI2TAIKBNGF          PIC X(01).
                       09  SPA-GMN-ZENKEI2TAIKBNGMEI        PIC X(38).
                   07  SPA-GMN-ZENDAY2KBNN-G.
                       09  SPA-GMN-ZENDAY2KBNN              PIC X(01).
                       09  SPA-GMN-ZENDAY2KBNNF             PIC X(01).
                       09  SPA-GMN-ZENDAY2KBNNMEI           PIC X(78).
                   07  SPA-GMN-ZENDAY2TAIKBNN-G.
                       09  SPA-GMN-ZENDAY2TAIKBNN           PIC X(01).
                       09  SPA-GMN-ZENDAY2TAIKBNNF          PIC X(01).
                       09  SPA-GMN-ZENDAY2TAIKBNNMEI        PIC X(38).
                   07  SPA-GMN-SYOKEI2KBNG-G.
                       09  SPA-GMN-SYOKEI2KBNG              PIC X(01).
                       09  SPA-GMN-SYOKEI2KBNGF             PIC X(01).
                       09  SPA-GMN-SYOKEI2KBNGMEI           PIC X(78).
                   07  SPA-GMN-SYOKEI2TAIKBNG-G.
                       09  SPA-GMN-SYOKEI2TAIKBNG           PIC X(01).
                       09  SPA-GMN-SYOKEI2TAIKBNGF          PIC X(01).
                       09  SPA-GMN-SYOKEI2TAIKBNGMEI        PIC X(38).
                   07  SPA-GMN-SYOKEI2KBNN-G.
                       09  SPA-GMN-SYOKEI2KBNN              PIC X(01).
                       09  SPA-GMN-SYOKEI2KBNNF             PIC X(01).
                       09  SPA-GMN-SYOKEI2KBNNMEI           PIC X(78).
                   07  SPA-GMN-SYOKEI2TAIKBNN-G.
                       09  SPA-GMN-SYOKEI2TAIKBNN           PIC X(01).
                       09  SPA-GMN-SYOKEI2TAIKBNNF          PIC X(01).
                       09  SPA-GMN-SYOKEI2TAIKBNNMEI        PIC X(38).
                   07  SPA-GMN-NINNASIKBNK-G.
                       09  SPA-GMN-NINNASIKBNK              PIC X(01).
                       09  SPA-GMN-NINNASIKBNKF             PIC X(01).
                       09  SPA-GMN-NINNASIKBNKMEI           PIC X(78).
                   07  SPA-GMN-NINNASITAIKBNK-G.
                       09  SPA-GMN-NINNASITAIKBNK           PIC X(01).
                       09  SPA-GMN-NINNASITAIKBNKF          PIC X(01).
                       09  SPA-GMN-NINNASITAIKBNKMEI        PIC X(38).
                   07  SPA-GMN-CYOKIFTNKBN-G.
                       09  SPA-GMN-CYOKIFTNKBN              PIC X(01).
                       09  SPA-GMN-CYOKIFTNKBNF             PIC X(01).
                       09  SPA-GMN-CYOKIFTNKBNMEI           PIC X(78).
                   07  SPA-GMN-CYOKIFTNTAIKBN-G.
                       09  SPA-GMN-CYOKIFTNTAIKBN           PIC X(01).
                       09  SPA-GMN-CYOKIFTNTAIKBNF          PIC X(01).
                       09  SPA-GMN-CYOKIFTNTAIKBNMEI        PIC X(38).
                   07  SPA-GMN-CYOKIKSIKBN-G.
                       09  SPA-GMN-CYOKIKSIKBN              PIC X(01).
                       09  SPA-GMN-CYOKIKSIKBNF             PIC X(01).
                       09  SPA-GMN-CYOKIKSIKBNMEI           PIC X(78).
                   07  SPA-GMN-CYOKIKSITAIKBN-G.
                       09  SPA-GMN-CYOKIKSITAIKBN           PIC X(01).
                       09  SPA-GMN-CYOKIKSITAIKBNF          PIC X(01).
                       09  SPA-GMN-CYOKIKSITAIKBNMEI        PIC X(38).
               05  SPA-GMN-CMB-AREA.
                   07  SPA-GMN-KOHNUMLST-G.
                       09  SPA-GMN-KOHNUM-MAX              PIC 9(03).
                       09  SPA-GMN-KOHNUMLST               OCCURS  200.
                           11  SPA-GMN-KOHNUML             PIC X(03).
                           11  SPA-GMN-KOHNUMFL            PIC X(01).
                           11  SPA-GMN-KOHNUMMEIL          PIC X(60).
                   07  SPA-GMN-JNISSUKBNLST-G.
                       09  SPA-GMN-JNISSUKBN-MAX           PIC 9(03).
                       09  SPA-GMN-JNISSUKBNLST            OCCURS  10.
                           11  SPA-GMN-JNISSUKBNL          PIC X(01).
                           11  SPA-GMN-JNISSUKBNFL         PIC X(01).
                           11  SPA-GMN-JNISSUKBNMEIL       PIC X(78).
                   07  SPA-GMN-TOTALTENKBNLST-G.
                       09  SPA-GMN-TOTALTENKBN-MAX         PIC 9(03).
                       09  SPA-GMN-TOTALTENKBNLST          OCCURS 10.
                           11  SPA-GMN-TOTALTENKBNL        PIC X(01).
                           11  SPA-GMN-TOTALTENKBNFL       PIC X(01).
                           11  SPA-GMN-TOTALTENKBNMEIL     PIC X(78).
                   07  SPA-GMN-SYUKEIKBNLST-G.
                       09  SPA-GMN-SYUKEIKBN-MAX           PIC 9(03).
                       09  SPA-GMN-SYUKEIKBNLST            OCCURS 10.
                           11  SPA-GMN-SYUKEIKBNL          PIC X(01).
                           11  SPA-GMN-SYUKEIKBNFL         PIC X(01).
                           11  SPA-GMN-SYUKEIKBNMEIL       PIC X(78).
                   07  SPA-GMN-FTNKEIKBNLST-G.
                       09  SPA-GMN-FTNKEIKBN-MAX           PIC 9(03).
                       09  SPA-GMN-FTNKEIKBNLST            OCCURS 10.
                           11  SPA-GMN-FTNKEIKBNL          PIC X(01).
                           11  SPA-GMN-FTNKEIKBNFL         PIC X(01).
                           11  SPA-GMN-FTNKEIKBNMEIL       PIC X(78).
                   07  SPA-GMN-FTNMONEYKBNLST-G.
                       09  SPA-GMN-FTNMONEYKBN-MAX         PIC 9(03).
                       09  SPA-GMN-FTNMONEYKBNLST          OCCURS 10.
                           11  SPA-GMN-FTNMONEYKBNL        PIC X(01).
                           11  SPA-GMN-FTNMONEYKBNFL       PIC X(01).
                           11  SPA-GMN-FTNMONEYKBNMEIL     PIC X(78).
                   07  SPA-GMN-FTNZEROKBNLST-G.
                       09  SPA-GMN-FTNZEROKBN-MAX          PIC 9(03).
                       09  SPA-GMN-FTNZEROKBNLST           OCCURS 10.
                           11  SPA-GMN-FTNZEROKBNL         PIC X(01).
                           11  SPA-GMN-FTNZEROKBNFL        PIC X(01).
                           11  SPA-GMN-FTNZEROKBNMEIL      PIC X(78).
                   07  SPA-GMN-SKJKBNLST-G.
                       09  SPA-GMN-SKJKBN-MAX              PIC 9(03).
                       09  SPA-GMN-SKJKBNLST               OCCURS 10.
                           11  SPA-GMN-SKJKBNL             PIC X(01).
                           11  SPA-GMN-SKJKBNFL            PIC X(01).
                           11  SPA-GMN-SKJKBNMEIL          PIC X(78).
                   07  SPA-GMN-OCRKBNLST-G.
                       09  SPA-GMN-OCRKBN-MAX              PIC 9(03).
                       09  SPA-GMN-OCRKBNLST               OCCURS 10.
                           11  SPA-GMN-OCRKBNL             PIC X(01).
                           11  SPA-GMN-OCRKBNFL            PIC X(01).
                           11  SPA-GMN-OCRKBNMEIL          PIC X(78).
                   07  SPA-GMN-SAIKEIKBNLST-G.
                       09  SPA-GMN-SAIKEIKBN-MAX           PIC 9(03).
                       09  SPA-GMN-SAIKEIKBNLST            OCCURS 10.
                           11  SPA-GMN-SAIKEIKBNL          PIC X(01).
                           11  SPA-GMN-SAIKEIKBNFL         PIC X(01).
                           11  SPA-GMN-SAIKEIKBNMEIL       PIC X(78).
                   07  SPA-GMN-SAIKEITAIKBNLST-G.
                       09  SPA-GMN-SAIKEITAIKBN-MAX        PIC 9(03).
                       09  SPA-GMN-SAIKEITAIKBNLST         OCCURS 10.
                           11  SPA-GMN-SAIKEITAIKBNL       PIC X(01).
                           11  SPA-GMN-SAIKEITAIKBNFL      PIC X(01).
                           11  SPA-GMN-SAIKEITAIKBNMEIL    PIC X(38).
                   07  SPA-GMN-HKNFTNKBNLST-G.
                       09  SPA-GMN-HKNFTNKBN-MAX           PIC 9(03).
                       09  SPA-GMN-HKNFTNKBNLST            OCCURS 10.
                           11  SPA-GMN-HKNFTNKBNL          PIC X(01).
                           11  SPA-GMN-HKNFTNKBNFL         PIC X(01).
                           11  SPA-GMN-HKNFTNKBNMEIL       PIC X(78).
                   07  SPA-GMN-RECEKSIKBNLST-G.
                       09  SPA-GMN-RECEKSIKBN-MAX          PIC 9(03).
                       09  SPA-GMN-RECEKSIKBNLST           OCCURS 10.
                           11  SPA-GMN-RECEKSIKBNL         PIC X(01).
                           11  SPA-GMN-RECEKSIKBNFL        PIC X(01).
                           11  SPA-GMN-RECEKSIKBNMEIL      PIC X(78).
                   07  SPA-GMN-CHKJGNKBNGLST-G.
                       09  SPA-GMN-CHKJGNKBNG-MAX          PIC 9(03).
                       09  SPA-GMN-CHKJGNKBNGLST           OCCURS 10.
                           11  SPA-GMN-CHKJGNKBNGL         PIC X(01).
                           11  SPA-GMN-CHKJGNKBNGFL        PIC X(01).
                           11  SPA-GMN-CHKJGNKBNGMEIL      PIC X(78).
                   07  SPA-GMN-CHKJGNKBNNLST-G.
                       09  SPA-GMN-CHKJGNKBNN-MAX          PIC 9(03).
                       09  SPA-GMN-CHKJGNKBNNLST           OCCURS 10.
                           11  SPA-GMN-CHKJGNKBNNL         PIC X(01).
                           11  SPA-GMN-CHKJGNKBNNFL        PIC X(01).
                           11  SPA-GMN-CHKJGNKBNNMEIL      PIC X(78).
                   07  SPA-GMN-CHKJGNTAIKBNLST-G.
                       09  SPA-GMN-CHKJGNTAIKBN-MAX        PIC 9(03).
                       09  SPA-GMN-CHKJGNTAIKBNLST         OCCURS 10.
                           11  SPA-GMN-CHKJGNTAIKBNL       PIC X(01).
                           11  SPA-GMN-CHKJGNTAIKBNFL      PIC X(01).
                           11  SPA-GMN-CHKJGNTAIKBNMEIL    PIC X(38).
                   07  SPA-GMN-KOHSKYKBNLST-G.
                       09  SPA-GMN-KOHSKYKBN-MAX           PIC 9(03).
                       09  SPA-GMN-KOHSKYKBNLST            OCCURS 10.
                           11  SPA-GMN-KOHSKYKBNL          PIC X(01).
                           11  SPA-GMN-KOHSKYKBNFL         PIC X(01).
                           11  SPA-GMN-KOHSKYKBNMEIL       PIC X(78).
                   07  SPA-GMN-KAKKBNLST-G.
                       09  SPA-GMN-KAKKBN-MAX           PIC 9(03).
                       09  SPA-GMN-KAKKBNLST            OCCURS 10.
                           11  SPA-GMN-KAKKBNL          PIC X(01).
                           11  SPA-GMN-KAKKBNFL         PIC X(01).
                           11  SPA-GMN-KAKKBNMEIL       PIC X(78).
      *
                   07  SPA-GMN-KAK20KBNLST-G.
                       09  SPA-GMN-KAK20KBN-MAX           PIC 9(03).
                       09  SPA-GMN-KAK20KBNLST            OCCURS 10.
                           11  SPA-GMN-KAK20KBNL          PIC X(01).
                           11  SPA-GMN-KAK20KBNFL         PIC X(01).
                           11  SPA-GMN-KAK20KBNMEIL       PIC X(78).
      *
                   07  SPA-GMN-KAK31KBNLST-G.
                       09  SPA-GMN-KAK31KBN-MAX           PIC 9(03).
                       09  SPA-GMN-KAK31KBNLST            OCCURS 10.
                           11  SPA-GMN-KAK31KBNL          PIC X(01).
                           11  SPA-GMN-KAK31KBNFL         PIC X(01).
                           11  SPA-GMN-KAK31KBNMEIL       PIC X(78).
      *
                   07  SPA-GMN-KAK32KBNKLST-G.
                       09  SPA-GMN-KAK32KBNK-MAX           PIC 9(03).
                       09  SPA-GMN-KAK32KBNKLST            OCCURS 10.
                           11  SPA-GMN-KAK32KBNKL          PIC X(01).
                           11  SPA-GMN-KAK32KBNKFL         PIC X(01).
                           11  SPA-GMN-KAK32KBNKMEIL       PIC X(78).
      *
                   07  SPA-GMN-KAK32KBNYLST-G.
                       09  SPA-GMN-KAK32KBNY-MAX           PIC 9(03).
                       09  SPA-GMN-KAK32KBNYLST            OCCURS 10.
                           11  SPA-GMN-KAK32KBNYL          PIC X(01).
                           11  SPA-GMN-KAK32KBNYFL         PIC X(01).
                           11  SPA-GMN-KAK32KBNYMEIL       PIC X(78).
      *
                   07  SPA-GMN-KAK40KBNLST-G.
                       09  SPA-GMN-KAK40KBN-MAX           PIC 9(03).
                       09  SPA-GMN-KAK40KBNLST            OCCURS 10.
                           11  SPA-GMN-KAK40KBNL          PIC X(01).
                           11  SPA-GMN-KAK40KBNFL         PIC X(01).
                           11  SPA-GMN-KAK40KBNMEIL       PIC X(78).
      *
                   07  SPA-GMN-KSINGKBNLST-G.
                       09  SPA-GMN-KSINGKBN-MAX           PIC 9(03).
                       09  SPA-GMN-KSINGKBNLST            OCCURS 10.
                           11  SPA-GMN-KSINGKBNL          PIC X(01).
                           11  SPA-GMN-KSINGKBNFL         PIC X(01).
                           11  SPA-GMN-KSINGKBNMEIL       PIC X(78).
      *
                   07  SPA-GMN-ENCHGKBNLST-G.
                       09  SPA-GMN-ENCHGKBN-MAX           PIC 9(03).
                       09  SPA-GMN-ENCHGKBNLST            OCCURS 10.
                           11  SPA-GMN-ENCHGKBNL          PIC X(01).
                           11  SPA-GMN-ENCHGKBNFL         PIC X(01).
                           11  SPA-GMN-ENCHGKBNMEIL       PIC X(78).
      *
                   07  SPA-GMN-KOH2SKJKBNLST-G.
                       09  SPA-GMN-KOH2SKJKBN-MAX           PIC 9(03).
                       09  SPA-GMN-KOH2SKJKBNLST            OCCURS 10.
                           11  SPA-GMN-KOH2SKJKBNL          PIC X(01).
                           11  SPA-GMN-KOH2SKJKBNFL         PIC X(01).
                           11  SPA-GMN-KOH2SKJKBNMEIL       PIC X(78).
      *
                   07  SPA-GMN-FTNHASUKBNGLST-G.
                       09  SPA-GMN-FTNHASUKBNG-MAX           PIC 9(03).
                       09  SPA-GMN-FTNHASUKBNGLST            OCCURS 10.
                           11  SPA-GMN-FTNHASUKBNGL          PIC X(01).
                           11  SPA-GMN-FTNHASUKBNGFL         PIC X(01).
                           11  SPA-GMN-FTNHASUKBNGMEIL       PIC X(78).
      *
                   07  SPA-GMN-FTNHASUTAIKBNGLST-G.
                       09  SPA-GMN-FTNHASUTAIKBNG-MAX        PIC 9(03).
                       09  SPA-GMN-FTNHASUTAIKBNGLST         OCCURS 10.
                           11  SPA-GMN-FTNHASUTAIKBNGL       PIC X(01).
                           11  SPA-GMN-FTNHASUTAIKBNGFL      PIC X(01).
                           11  SPA-GMN-FTNHASUTAIKBNGMEIL    PIC X(38).
      *
                   07  SPA-GMN-FTNHASUKBNNLST-G.
                       09  SPA-GMN-FTNHASUKBNN-MAX           PIC 9(03).
                       09  SPA-GMN-FTNHASUKBNNLST            OCCURS 10.
                           11  SPA-GMN-FTNHASUKBNNL          PIC X(01).
                           11  SPA-GMN-FTNHASUKBNNFL         PIC X(01).
                           11  SPA-GMN-FTNHASUKBNNMEIL       PIC X(78).
      *
                   07  SPA-GMN-FTNHASUTAIKBNNLST-G.
                       09  SPA-GMN-FTNHASUTAIKBNN-MAX        PIC 9(03).
                       09  SPA-GMN-FTNHASUTAIKBNNLST         OCCURS 10.
                           11  SPA-GMN-FTNHASUTAIKBNNL       PIC X(01).
                           11  SPA-GMN-FTNHASUTAIKBNNFL      PIC X(01).
                           11  SPA-GMN-FTNHASUTAIKBNNMEIL    PIC X(38).
      *
                   07  SPA-GMN-JYUHENKBNLST-G.
                       09  SPA-GMN-JYUHENKBN-MAX           PIC 9(03).
                       09  SPA-GMN-JYUHENKBNLST            OCCURS 10.
                           11  SPA-GMN-JYUHENKBNL          PIC X(01).
                           11  SPA-GMN-JYUHENKBNFL         PIC X(01).
                           11  SPA-GMN-JYUHENKBNMEIL       PIC X(78).
                   07  SPA-GMN-JYUHENTAIKBNLST-G.
                       09  SPA-GMN-JYUHENTAIKBN-MAX        PIC 9(03).
                       09  SPA-GMN-JYUHENTAIKBNLST         OCCURS 15.
                           11  SPA-GMN-JYUHENTAIKBNL       PIC X(01).
                           11  SPA-GMN-JYUHENTAIKBNFL      PIC X(01).
                           11  SPA-GMN-JYUHENTAIKBNMEIL    PIC X(38).
                   07  SPA-GMN-JGNFTNKBNLST-G.
                       09  SPA-GMN-JGNFTNKBN-MAX           PIC 9(03).
                       09  SPA-GMN-JGNFTNKBNLST            OCCURS 10.
                           11  SPA-GMN-JGNFTNKBNL          PIC X(01).
                           11  SPA-GMN-JGNFTNKBNFL         PIC X(01).
                           11  SPA-GMN-JGNFTNKBNMEIL       PIC X(78).
                   07  SPA-GMN-JGNFTNTAIKBNLST-G.
                       09  SPA-GMN-JGNFTNTAIKBN-MAX        PIC 9(03).
                       09  SPA-GMN-JGNFTNTAIKBNLST         OCCURS 10.
                           11  SPA-GMN-JGNFTNTAIKBNL       PIC X(01).
                           11  SPA-GMN-JGNFTNTAIKBNFL      PIC X(01).
                           11  SPA-GMN-JGNFTNTAIKBNMEIL    PIC X(38).
                   07  SPA-GMN-PTJGNKBNLST-G.
                       09  SPA-GMN-PTJGNKBN-MAX            PIC 9(03).
                       09  SPA-GMN-PTJGNKBNLST             OCCURS 10.
                           11  SPA-GMN-PTJGNKBNL           PIC X(01).
                           11  SPA-GMN-PTJGNKBNFL          PIC X(01).
                           11  SPA-GMN-PTJGNKBNMEIL        PIC X(78).
                   07  SPA-GMN-PTJGNTAIKBNLST-G.
                       09  SPA-GMN-PTJGNTAIKBN-MAX         PIC 9(03).
                       09  SPA-GMN-PTJGNTAIKBNLST          OCCURS 10.
                           11  SPA-GMN-PTJGNTAIKBNL        PIC X(01).
                           11  SPA-GMN-PTJGNTAIKBNFL       PIC X(01).
                           11  SPA-GMN-PTJGNTAIKBNMEIL     PIC X(38).
                   07  SPA-GMN-IRYOKEIKBNLST-G.
                       09  SPA-GMN-IRYOKEIKBN-MAX          PIC 9(03).
                       09  SPA-GMN-IRYOKEIKBNLST           OCCURS 10.
                           11  SPA-GMN-IRYOKEIKBNL         PIC X(01).
                           11  SPA-GMN-IRYOKEIKBNFL        PIC X(01).
                           11  SPA-GMN-IRYOKEIKBNMEIL      PIC X(78).
                   07  SPA-GMN-IRYOKEITAIKBNLST-G.
                       09  SPA-GMN-IRYOKEITAIKBN-MAX       PIC 9(03).
                       09  SPA-GMN-IRYOKEITAIKBNLST        OCCURS 10.
                           11  SPA-GMN-IRYOKEITAIKBNL      PIC X(01).
                           11  SPA-GMN-IRYOKEITAIKBNFL     PIC X(01).
                           11  SPA-GMN-IRYOKEITAIKBNMEIL   PIC X(38).
                   07  SPA-GMN-SKJKEIKBNLST-G.
                       09  SPA-GMN-SKJKEIKBN-MAX           PIC 9(03).
                       09  SPA-GMN-SKJKEIKBNLST            OCCURS 10.
                           11  SPA-GMN-SKJKEIKBNL          PIC X(01).
                           11  SPA-GMN-SKJKEIKBNFL         PIC X(01).
                           11  SPA-GMN-SKJKEIKBNMEIL       PIC X(78).
                   07  SPA-GMN-SKJKEITAIKBNLST-G.
                       09  SPA-GMN-SKJKEITAIKBN-MAX        PIC 9(03).
                       09  SPA-GMN-SKJKEITAIKBNLST         OCCURS 10.
                           11  SPA-GMN-SKJKEITAIKBNL       PIC X(01).
                           11  SPA-GMN-SKJKEITAIKBNFL      PIC X(01).
                           11  SPA-GMN-SKJKEITAIKBNMEIL    PIC X(38).
                   07  SPA-GMN-KINJYOKBNLST-G.
                       09  SPA-GMN-KINJYOKBN-MAX           PIC 9(03).
                       09  SPA-GMN-KINJYOKBNLST            OCCURS 10.
                           11  SPA-GMN-KINJYOKBNL          PIC X(01).
                           11  SPA-GMN-KINJYOKBNFL         PIC X(01).
                           11  SPA-GMN-KINJYOKBNMEIL       PIC X(78).
                   07  SPA-GMN-KINJYOTAIKBNLST-G.
                       09  SPA-GMN-KINJYOTAIKBN-MAX        PIC 9(03).
                       09  SPA-GMN-KINJYOTAIKBNLST         OCCURS 10.
                           11  SPA-GMN-KINJYOTAIKBNL       PIC X(01).
                           11  SPA-GMN-KINJYOTAIKBNFL      PIC X(01).
                           11  SPA-GMN-KINJYOTAIKBNMEIL    PIC X(38).
                   07  SPA-GMN-INNAIKBNLST-G.
                       09  SPA-GMN-INNAIKBN-MAX           PIC 9(03).
                       09  SPA-GMN-INNAIKBNLST            OCCURS 10.
                           11  SPA-GMN-INNAIKBNL          PIC X(01).
                           11  SPA-GMN-INNAIKBNFL         PIC X(01).
                           11  SPA-GMN-INNAIKBNMEIL       PIC X(78).
                   07  SPA-GMN-INNAITAIKBNLST-G.
                       09  SPA-GMN-INNAITAIKBN-MAX        PIC 9(03).
                       09  SPA-GMN-INNAITAIKBNLST         OCCURS 10.
                           11  SPA-GMN-INNAITAIKBNL       PIC X(01).
                           11  SPA-GMN-INNAITAIKBNFL      PIC X(01).
                           11  SPA-GMN-INNAITAIKBNMEIL    PIC X(38).
                   07  SPA-GMN-JIRITUKBNLST-G.
                       09  SPA-GMN-JIRITUKBN-MAX           PIC 9(03).
                       09  SPA-GMN-JIRITUKBNLST            OCCURS 10.
                           11  SPA-GMN-JIRITUKBNL          PIC X(01).
                           11  SPA-GMN-JIRITUKBNFL         PIC X(01).
                           11  SPA-GMN-JIRITUKBNMEIL       PIC X(78).
                   07  SPA-GMN-JIRITUTAIKBNLST-G.
                       09  SPA-GMN-JIRITUTAIKBN-MAX        PIC 9(03).
                       09  SPA-GMN-JIRITUTAIKBNLST         OCCURS 10.
                           11  SPA-GMN-JIRITUTAIKBNL       PIC X(01).
                           11  SPA-GMN-JIRITUTAIKBNFL      PIC X(01).
                           11  SPA-GMN-JIRITUTAIKBNMEIL    PIC X(38).
                   07  SPA-GMN-SKJFUKAKBNLST-G.
                       09  SPA-GMN-SKJFUKAKBN-MAX          PIC 9(03).
                       09  SPA-GMN-SKJFUKAKBNLST           OCCURS 10.
                           11  SPA-GMN-SKJFUKAKBNL         PIC X(01).
                           11  SPA-GMN-SKJFUKAKBNFL        PIC X(01).
                           11  SPA-GMN-SKJFUKAKBNMEIL      PIC X(78).
                   07  SPA-GMN-SKJFUKATAIKBNHONLST-G.
                       09  SPA-GMN-SKJFUKATAIKBNHON-MAX     PIC 9(03).
                       09  SPA-GMN-SKJFUKATAIKBNHONLST      OCCURS 10.
                           11  SPA-GMN-SKJFUKATAIKBNHONL    PIC X(01).
                           11  SPA-GMN-SKJFUKATAIKBNHONFL   PIC X(01).
                           11  SPA-GMN-SKJFUKATAIKBNHONMEIL PIC X(38).
                   07  SPA-GMN-SKJFUKATAIKBNTSYLST-G.
                       09  SPA-GMN-SKJFUKATAIKBNTSY-MAX     PIC 9(03).
                       09  SPA-GMN-SKJFUKATAIKBNTSYLST      OCCURS 10.
                           11  SPA-GMN-SKJFUKATAIKBNTSYL    PIC X(01).
                           11  SPA-GMN-SKJFUKATAIKBNTSYFL   PIC X(01).
                           11  SPA-GMN-SKJFUKATAIKBNTSYMEIL PIC X(38).
                   07  SPA-GMN-SKJFUKATAIKBNTNKLST-G.
                       09  SPA-GMN-SKJFUKATAIKBNTNK-MAX     PIC 9(03).
                       09  SPA-GMN-SKJFUKATAIKBNTNKLST      OCCURS 10.
                           11  SPA-GMN-SKJFUKATAIKBNTNKL    PIC X(01).
                           11  SPA-GMN-SKJFUKATAIKBNTNKFL   PIC X(01).
                           11  SPA-GMN-SKJFUKATAIKBNTNKMEIL PIC X(38).
                   07  SPA-GMN-KAICNTKBNGLST-G.
                       09  SPA-GMN-KAICNTKBNG-MAX           PIC 9(03).
                       09  SPA-GMN-KAICNTKBNGLST            OCCURS 10.
                           11  SPA-GMN-KAICNTKBNGL          PIC X(01).
                           11  SPA-GMN-KAICNTKBNGFL         PIC X(01).
                           11  SPA-GMN-KAICNTKBNGMEIL       PIC X(78).
      *
                   07  SPA-GMN-KAICNTTAIKBNGLST-G.
                       09  SPA-GMN-KAICNTTAIKBNG-MAX        PIC 9(03).
                       09  SPA-GMN-KAICNTTAIKBNGLST         OCCURS 10.
                           11  SPA-GMN-KAICNTTAIKBNGL       PIC X(01).
                           11  SPA-GMN-KAICNTTAIKBNGFL      PIC X(01).
                           11  SPA-GMN-KAICNTTAIKBNGMEIL    PIC X(38).
      *
                   07  SPA-GMN-CYOKEIKBNGLST-G.
                       09  SPA-GMN-CYOKEIKBNG-MAX           PIC 9(03).
                       09  SPA-GMN-CYOKEIKBNGLST            OCCURS 10.
                           11  SPA-GMN-CYOKEIKBNGL          PIC X(01).
                           11  SPA-GMN-CYOKEIKBNGFL         PIC X(01).
                           11  SPA-GMN-CYOKEIKBNGMEIL       PIC X(78).
      *
                   07  SPA-GMN-CYOKEITAIKBNGLST-G.
                       09  SPA-GMN-CYOKEITAIKBNG-MAX        PIC 9(03).
                       09  SPA-GMN-CYOKEITAIKBNGLST         OCCURS 10.
                           11  SPA-GMN-CYOKEITAIKBNGL       PIC X(01).
                           11  SPA-GMN-CYOKEITAIKBNGFL      PIC X(01).
                           11  SPA-GMN-CYOKEITAIKBNGMEIL    PIC X(38).
      *
                   07  SPA-GMN-GNBKEIKBNKGLST-G.
                       09  SPA-GMN-GNBKEIKBNKG-MAX          PIC 9(03).
                       09  SPA-GMN-GNBKEIKBNKGLST           OCCURS 10.
                           11  SPA-GMN-GNBKEIKBNKGL         PIC X(01).
                           11  SPA-GMN-GNBKEIKBNKGFL        PIC X(01).
                           11  SPA-GMN-GNBKEIKBNKGMEIL      PIC X(78).
      *
                   07  SPA-GMN-GNBKEITAIKBNKGLST-G.
                       09  SPA-GMN-GNBKEITAIKBNKG-MAX       PIC 9(03).
                       09  SPA-GMN-GNBKEITAIKBNKGLST        OCCURS 10.
                           11  SPA-GMN-GNBKEITAIKBNKGL      PIC X(01).
                           11  SPA-GMN-GNBKEITAIKBNKGFL     PIC X(01).
                           11  SPA-GMN-GNBKEITAIKBNKGMEIL   PIC X(38).
      *
                   07  SPA-GMN-ZENKEIKBNGLST-G.
                       09  SPA-GMN-ZENKEIKBNG-MAX           PIC 9(03).
                       09  SPA-GMN-ZENKEIKBNGLST            OCCURS 10.
                           11  SPA-GMN-ZENKEIKBNGL          PIC X(01).
                           11  SPA-GMN-ZENKEIKBNGFL         PIC X(01).
                           11  SPA-GMN-ZENKEIKBNGMEIL       PIC X(78).
      *
                   07  SPA-GMN-ZENKEITAIKBNGLST-G.
                       09  SPA-GMN-ZENKEITAIKBNG-MAX        PIC 9(03).
                       09  SPA-GMN-ZENKEITAIKBNGLST         OCCURS 10.
                           11  SPA-GMN-ZENKEITAIKBNGL       PIC X(01).
                           11  SPA-GMN-ZENKEITAIKBNGFL      PIC X(01).
                           11  SPA-GMN-ZENKEITAIKBNGMEIL    PIC X(38).
      *
                   07  SPA-GMN-SINKEIKBNLST-G.
                       09  SPA-GMN-SINKEIKBN-MAX           PIC 9(03).
                       09  SPA-GMN-SINKEIKBNLST            OCCURS 10.
                           11  SPA-GMN-SINKEIKBNL          PIC X(01).
                           11  SPA-GMN-SINKEIKBNFL         PIC X(01).
                           11  SPA-GMN-SINKEIKBNMEIL       PIC X(78).
      *
                   07  SPA-GMN-SINKEITAIKBNLST-G.
                       09  SPA-GMN-SINKEITAIKBN-MAX         PIC 9(03).
                       09  SPA-GMN-SINKEITAIKBNLST          OCCURS 10.
                           11  SPA-GMN-SINKEITAIKBNL        PIC X(01).
                           11  SPA-GMN-SINKEITAIKBNFL       PIC X(01).
                           11  SPA-GMN-SINKEITAIKBNMEIL     PIC X(38).
      *
                   07  SPA-GMN-SYOKEIKBNLST-G.
                       09  SPA-GMN-SYOKEIKBN-MAX           PIC 9(03).
                       09  SPA-GMN-SYOKEIKBNLST            OCCURS 10.
                           11  SPA-GMN-SYOKEIKBNL          PIC X(01).
                           11  SPA-GMN-SYOKEIKBNFL         PIC X(01).
                           11  SPA-GMN-SYOKEIKBNMEIL       PIC X(78).
      *
                   07  SPA-GMN-SYOKEITAIKBNLST-G.
                       09  SPA-GMN-SYOKEITAIKBN-MAX           PIC 9(03).
                       09  SPA-GMN-SYOKEITAIKBNLST            OCCURS 10.
                           11  SPA-GMN-SYOKEITAIKBNL          PIC X(01).
                           11  SPA-GMN-SYOKEITAIKBNFL         PIC X(01).
                           11  SPA-GMN-SYOKEITAIKBNMEIL       PIC X(38).
      *
                   07  SPA-GMN-NYUKEIKBNLST-G.
                       09  SPA-GMN-NYUKEIKBN-MAX           PIC 9(03).
                       09  SPA-GMN-NYUKEIKBNLST            OCCURS 10.
                           11  SPA-GMN-NYUKEIKBNL          PIC X(01).
                           11  SPA-GMN-NYUKEIKBNFL         PIC X(01).
                           11  SPA-GMN-NYUKEIKBNMEIL       PIC X(78).
      *
                   07  SPA-GMN-NYUKEITAIKBNLST-G.
                       09  SPA-GMN-NYUKEITAIKBN-MAX           PIC 9(03).
                       09  SPA-GMN-NYUKEITAIKBNLST            OCCURS 10.
                           11  SPA-GMN-NYUKEITAIKBNL          PIC X(01).
                           11  SPA-GMN-NYUKEITAIKBNFL         PIC X(01).
                           11  SPA-GMN-NYUKEITAIKBNMEIL       PIC X(38).
      *
                   07  SPA-GMN-DAYKEIKBNLST-G.
                       09  SPA-GMN-DAYKEIKBN-MAX           PIC 9(03).
                       09  SPA-GMN-DAYKEIKBNLST            OCCURS 10.
                           11  SPA-GMN-DAYKEIKBNL          PIC X(01).
                           11  SPA-GMN-DAYKEIKBNFL         PIC X(01).
                           11  SPA-GMN-DAYKEIKBNMEIL       PIC X(78).
      *
                   07  SPA-GMN-DAYKEITAIKBNLST-G.
                       09  SPA-GMN-DAYKEITAIKBN-MAX        PIC 9(03).
                       09  SPA-GMN-DAYKEITAIKBNLST         OCCURS 10.
                           11  SPA-GMN-DAYKEITAIKBNL       PIC X(01).
                           11  SPA-GMN-DAYKEITAIKBNFL      PIC X(01).
                           11  SPA-GMN-DAYKEITAIKBNMEIL    PIC X(38).
      *
                   07  SPA-GMN-NINNASIKBNLST-G.
                       09  SPA-GMN-NINNASIKBN-MAX          PIC 9(03).
                       09  SPA-GMN-NINNASIKBNLST           OCCURS 10.
                           11  SPA-GMN-NINNASIKBNL         PIC X(01).
                           11  SPA-GMN-NINNASIKBNFL        PIC X(01).
                           11  SPA-GMN-NINNASIKBNMEIL      PIC X(78).
      *
                   07  SPA-GMN-NINNASITAIKBNLST-G.
                       09  SPA-GMN-NINNASITAIKBN-MAX       PIC 9(03).
                       09  SPA-GMN-NINNASITAIKBNLST        OCCURS 10.
                           11  SPA-GMN-NINNASITAIKBNL      PIC X(01).
                           11  SPA-GMN-NINNASITAIKBNFL     PIC X(01).
                           11  SPA-GMN-NINNASITAIKBNMEIL   PIC X(38).
      *
                   07  SPA-GMN-NINJYOKBNLST-G.
                       09  SPA-GMN-NINJYOKBN-MAX           PIC 9(03).
                       09  SPA-GMN-NINJYOKBNLST            OCCURS 10.
                           11  SPA-GMN-NINJYOKBNL          PIC X(01).
                           11  SPA-GMN-NINJYOKBNFL         PIC X(01).
                           11  SPA-GMN-NINJYOKBNMEIL       PIC X(78).
      *
                   07  SPA-GMN-NINJYOTAIKBNLST-G.
                       09  SPA-GMN-NINJYOTAIKBN-MAX        PIC 9(03).
                       09  SPA-GMN-NINJYOTAIKBNLST         OCCURS 10.
                           11  SPA-GMN-NINJYOTAIKBNL       PIC X(01).
                           11  SPA-GMN-NINJYOTAIKBNFL      PIC X(01).
                           11  SPA-GMN-NINJYOTAIKBNMEIL    PIC X(38).
      *
                   07  SPA-GMN-KOGAKKEIKBNLST-G.
                       09  SPA-GMN-KOGAKKEIKBN-MAX           PIC 9(03).
                       09  SPA-GMN-KOGAKKEIKBNLST            OCCURS 10.
                           11  SPA-GMN-KOGAKKEIKBNL          PIC X(01).
                           11  SPA-GMN-KOGAKKEIKBNFL         PIC X(01).
                           11  SPA-GMN-KOGAKKEIKBNMEIL       PIC X(78).
      *
                   07  SPA-GMN-KOGAKKEITAIKBNLST-G.
                       09  SPA-GMN-KOGAKKEITAIKBN-MAX        PIC 9(03).
                       09  SPA-GMN-KOGAKKEITAIKBNLST         OCCURS 10.
                           11  SPA-GMN-KOGAKKEITAIKBNL       PIC X(01).
                           11  SPA-GMN-KOGAKKEITAIKBNFL      PIC X(01).
                           11  SPA-GMN-KOGAKKEITAIKBNMEIL    PIC X(38).
                   07  SPA-GMN-TANKIKBNLST-G.
                       09  SPA-GMN-TANKIKBN-MAX              PIC 9(03).
                       09  SPA-GMN-TANKIKBNLST               OCCURS 10.
                           11  SPA-GMN-TANKIKBNL             PIC X(01).
                           11  SPA-GMN-TANKIKBNFL            PIC X(01).
                           11  SPA-GMN-TANKIKBNMEIL          PIC X(78).
      *
                   07  SPA-GMN-TANKITAIKBNLST-G.
                       09  SPA-GMN-TANKITAIKBN-MAX        PIC 9(03).
                       09  SPA-GMN-TANKITAIKBNLST         OCCURS 10.
                           11  SPA-GMN-TANKITAIKBNL       PIC X(01).
                           11  SPA-GMN-TANKITAIKBNFL      PIC X(01).
                           11  SPA-GMN-TANKITAIKBNMEIL    PIC X(38).
      *
                   07  SPA-GMN-KAICNTKBNNLST-G.
                       09  SPA-GMN-KAICNTKBNN-MAX          PIC 9(03).
                       09  SPA-GMN-KAICNTKBNNLST           OCCURS 10.
                           11  SPA-GMN-KAICNTKBNNL         PIC X(01).
                           11  SPA-GMN-KAICNTKBNNFL        PIC X(01).
                           11  SPA-GMN-KAICNTKBNNMEIL      PIC X(78).
      *
                   07  SPA-GMN-KAICNTTAIKBNNLST-G.
                       09  SPA-GMN-KAICNTTAIKBNN-MAX       PIC 9(03).
                       09  SPA-GMN-KAICNTTAIKBNNLST        OCCURS 10.
                           11  SPA-GMN-KAICNTTAIKBNNL      PIC X(01).
                           11  SPA-GMN-KAICNTTAIKBNNFL     PIC X(01).
                           11  SPA-GMN-KAICNTTAIKBNNMEIL   PIC X(38).
      *
                   07  SPA-GMN-CYOKEIKBNNLST-G.
                       09  SPA-GMN-CYOKEIKBNN-MAX          PIC 9(03).
                       09  SPA-GMN-CYOKEIKBNNLST           OCCURS 10.
                           11  SPA-GMN-CYOKEIKBNNL         PIC X(01).
                           11  SPA-GMN-CYOKEIKBNNFL        PIC X(01).
                           11  SPA-GMN-CYOKEIKBNNMEIL      PIC X(78).
      *
                   07  SPA-GMN-CYOKEITAIKBNNLST-G.
                       09  SPA-GMN-CYOKEITAIKBNN-MAX       PIC 9(03).
                       09  SPA-GMN-CYOKEITAIKBNNLST        OCCURS 10.
                           11  SPA-GMN-CYOKEITAIKBNNL      PIC X(01).
                           11  SPA-GMN-CYOKEITAIKBNNFL     PIC X(01).
                           11  SPA-GMN-CYOKEITAIKBNNMEIL   PIC X(38).
      *
                   07  SPA-GMN-ZENDAYKBNNLST-G.
                       09  SPA-GMN-ZENDAYKBNN-MAX          PIC 9(03).
                       09  SPA-GMN-ZENDAYKBNNLST           OCCURS 10.
                           11  SPA-GMN-ZENDAYKBNNL         PIC X(01).
                           11  SPA-GMN-ZENDAYKBNNFL        PIC X(01).
                           11  SPA-GMN-ZENDAYKBNNMEIL      PIC X(78).
      *
                   07  SPA-GMN-ZENDAYTAIKBNNLST-G.
                       09  SPA-GMN-ZENDAYTAIKBNN-MAX       PIC 9(03).
                       09  SPA-GMN-ZENDAYTAIKBNNLST        OCCURS 10.
                           11  SPA-GMN-ZENDAYTAIKBNNL      PIC X(01).
                           11  SPA-GMN-ZENDAYTAIKBNNFL     PIC X(01).
                           11  SPA-GMN-ZENDAYTAIKBNNMEIL   PIC X(38).
      *
                   07  SPA-GMN-COMMONTAIKBNLST-G.
                       09  SPA-GMN-COMMONTAIKBN-MAX        PIC 9(03).
                       09  SPA-GMN-COMMONTAIKBNLST         OCCURS 10.
                           11  SPA-GMN-COMMONTAIKBNL       PIC X(01).
                           11  SPA-GMN-COMMONTAIKBNFL      PIC X(01).
                           11  SPA-GMN-COMMONTAIKBNMEIL    PIC X(38).
      *
                   07  SPA-GMN-ZENKEI2KBNGLST-G.
                       09  SPA-GMN-ZENKEI2KBNG-MAX           PIC 9(03).
                       09  SPA-GMN-ZENKEI2KBNGLST            OCCURS 10.
                           11  SPA-GMN-ZENKEI2KBNGL          PIC X(01).
                           11  SPA-GMN-ZENKEI2KBNGFL         PIC X(01).
                           11  SPA-GMN-ZENKEI2KBNGMEIL       PIC X(78).
      *
                   07  SPA-GMN-ZENKEI2TAIKBNGLST-G.
                       09  SPA-GMN-ZENKEI2TAIKBNG-MAX        PIC 9(03).
                       09  SPA-GMN-ZENKEI2TAIKBNGLST         OCCURS 10.
                           11  SPA-GMN-ZENKEI2TAIKBNGL       PIC X(01).
                           11  SPA-GMN-ZENKEI2TAIKBNGFL      PIC X(01).
                           11  SPA-GMN-ZENKEI2TAIKBNGMEIL    PIC X(38).
      *
                   07  SPA-GMN-ZENDAY2KBNNLST-G.
                       09  SPA-GMN-ZENDAY2KBNN-MAX           PIC 9(03).
                       09  SPA-GMN-ZENDAY2KBNNLST            OCCURS 10.
                           11  SPA-GMN-ZENDAY2KBNNL          PIC X(01).
                           11  SPA-GMN-ZENDAY2KBNNFL         PIC X(01).
                           11  SPA-GMN-ZENDAY2KBNNMEIL       PIC X(78).
      *
                   07  SPA-GMN-ZENDAY2TAIKBNNLST-G.
                       09  SPA-GMN-ZENDAY2TAIKBNN-MAX        PIC 9(03).
                       09  SPA-GMN-ZENDAY2TAIKBNNLST         OCCURS 10.
                           11  SPA-GMN-ZENDAY2TAIKBNNL       PIC X(01).
                           11  SPA-GMN-ZENDAY2TAIKBNNFL      PIC X(01).
                           11  SPA-GMN-ZENDAY2TAIKBNNMEIL    PIC X(38).
      *
                   07  SPA-GMN-SYOKEI2KBNGLST-G.
                       09  SPA-GMN-SYOKEI2KBNG-MAX           PIC 9(03).
                       09  SPA-GMN-SYOKEI2KBNGLST            OCCURS 10.
                           11  SPA-GMN-SYOKEI2KBNGL          PIC X(01).
                           11  SPA-GMN-SYOKEI2KBNGFL         PIC X(01).
                           11  SPA-GMN-SYOKEI2KBNGMEIL       PIC X(78).
      *
                   07  SPA-GMN-SYOKEI2TAIKBNGLST-G.
                       09  SPA-GMN-SYOKEI2TAIKBNG-MAX        PIC 9(03).
                       09  SPA-GMN-SYOKEI2TAIKBNGLST         OCCURS 10.
                           11  SPA-GMN-SYOKEI2TAIKBNGL       PIC X(01).
                           11  SPA-GMN-SYOKEI2TAIKBNGFL      PIC X(01).
                           11  SPA-GMN-SYOKEI2TAIKBNGMEIL    PIC X(38).
                   07  SPA-GMN-SYOKEI2KBNNLST-G.
                       09  SPA-GMN-SYOKEI2KBNN-MAX           PIC 9(03).
                       09  SPA-GMN-SYOKEI2KBNNLST            OCCURS 10.
                           11  SPA-GMN-SYOKEI2KBNNL          PIC X(01).
                           11  SPA-GMN-SYOKEI2KBNNFL         PIC X(01).
                           11  SPA-GMN-SYOKEI2KBNNMEIL       PIC X(78).
      *
                   07  SPA-GMN-SYOKEI2TAIKBNNLST-G.
                       09  SPA-GMN-SYOKEI2TAIKBNN-MAX        PIC 9(03).
                       09  SPA-GMN-SYOKEI2TAIKBNNLST         OCCURS 10.
                           11  SPA-GMN-SYOKEI2TAIKBNNL       PIC X(01).
                           11  SPA-GMN-SYOKEI2TAIKBNNFL      PIC X(01).
                           11  SPA-GMN-SYOKEI2TAIKBNNMEIL    PIC X(38).
                   07  SPA-GMN-NINNASIKBNKLST-G.
                       09  SPA-GMN-NINNASIKBNK-MAX           PIC 9(03).
                       09  SPA-GMN-NINNASIKBNKLST            OCCURS 10.
                           11  SPA-GMN-NINNASIKBNKL          PIC X(01).
                           11  SPA-GMN-NINNASIKBNKFL         PIC X(01).
                           11  SPA-GMN-NINNASIKBNKMEIL       PIC X(78).
      *
                   07  SPA-GMN-NINNASITAIKBNKLST-G.
                       09  SPA-GMN-NINNASITAIKBNK-MAX        PIC 9(03).
                       09  SPA-GMN-NINNASITAIKBNKLST         OCCURS 10.
                           11  SPA-GMN-NINNASITAIKBNKL       PIC X(01).
                           11  SPA-GMN-NINNASITAIKBNKFL      PIC X(01).
                           11  SPA-GMN-NINNASITAIKBNKMEIL    PIC X(38).
      *
                   07  SPA-GMN-CYOKIFTNKBNLST-G.
                       09  SPA-GMN-CYOKIFTNKBN-MAX           PIC 9(03).
                       09  SPA-GMN-CYOKIFTNKBNLST            OCCURS 10.
                           11  SPA-GMN-CYOKIFTNKBNL          PIC X(01).
                           11  SPA-GMN-CYOKIFTNKBNFL         PIC X(01).
                           11  SPA-GMN-CYOKIFTNKBNMEIL       PIC X(78).
                   07  SPA-GMN-CYOKIFTNTAIKBNLST-G.
                       09  SPA-GMN-CYOKIFTNTAIKBN-MAX        PIC 9(03).
                       09  SPA-GMN-CYOKIFTNTAIKBNLST         OCCURS 10.
                           11  SPA-GMN-CYOKIFTNTAIKBNL       PIC X(01).
                           11  SPA-GMN-CYOKIFTNTAIKBNFL      PIC X(01).
                           11  SPA-GMN-CYOKIFTNTAIKBNMEIL    PIC X(38).
      *
                   07  SPA-GMN-CYOKIKSIKBNLST-G.
                       09  SPA-GMN-CYOKIKSIKBN-MAX           PIC 9(03).
                       09  SPA-GMN-CYOKIKSIKBNLST            OCCURS 10.
                           11  SPA-GMN-CYOKIKSIKBNL          PIC X(01).
                           11  SPA-GMN-CYOKIKSIKBNFL         PIC X(01).
                           11  SPA-GMN-CYOKIKSIKBNMEIL       PIC X(78).
                   07  SPA-GMN-CYOKIKSITAIKBNLST-G.
                       09  SPA-GMN-CYOKIKSITAIKBN-MAX        PIC 9(03).
                       09  SPA-GMN-CYOKIKSITAIKBNLST         OCCURS 10.
                           11  SPA-GMN-CYOKIKSITAIKBNL       PIC X(01).
                           11  SPA-GMN-CYOKIKSITAIKBNFL      PIC X(01).
                           11  SPA-GMN-CYOKIKSITAIKBNMEIL    PIC X(38).
               05  SPA-GMN-KOHLST-AREA.
                   07  SPA-GMN-KOHLST-MAX                  PIC 9(5).
                   07  SPA-GMN-KOHLST-SEL                  PIC 9(5).
                   07  SPA-GMN-KOHLST                      OCCURS  100.
                       09  SPA-GMN-TKKOHNUM                PIC X(3).
                       09  SPA-GMN-TKKOHNAME               PIC X(100).
                       09  SPA-GMN-TKYUKOSTYMD             PIC X(10).
                       09  SPA-GMN-TKYUKOEDYMD             PIC X(10).
               05  SPA-GMN-RIREKILST-AREA.
                   07  SPA-GMN-RIREKILST-MAX               PIC 9(5).
                   07  SPA-GMN-RIREKILST-SEL               PIC 9(5).
                   07  SPA-GMN-RIREKILST                   OCCURS   20.
                       09  SPA-GMN-TRYUKOSTYMD             PIC X(10).
                       09  SPA-GMN-TRYUKOEDYMD             PIC X(10).
               05  SPA-NAI-AREA.
                   07  SPA-NAI-KJNYMD                      PIC X(08).
                   07  SPA-NAI-INPUT-AREA.
                       09  SPA-NAI-INPUT-MODE              PIC 9(1).
                       09  SPA-NAI-SELNUM1-FLG             PIC 9(1)
                                                           OCCURS  100.
                       09  SPA-NAI-KOHNUM                  PIC X(03).
                       09  SPA-NAI-KOHNUM-ORG              PIC X(03).
                       09  SPA-NAI-SKJKBNHON               PIC X(01).
                       09  SPA-NAI-SKJKBNTSY               PIC X(01).
                       09  SPA-NAI-SKJKBNTNK               PIC X(01).
                       09  SPA-NAI-YUKOSTYMD               PIC X(08).
                       09  SPA-NAI-YUKOSTYMD-ORG           PIC X(08).
                       09  SPA-NAI-YUKOEDYMD               PIC X(08).
                       09  SPA-NAI-YUKOEDYMD-ORG           PIC X(08).
                       09  SPA-NAI-NBOOK-AREA.
                           11  SPA-NAI-NBOOK-DUMMY         PIC X(01).
               05  SPA-NAI-KOHLST-AREA.
                   07  SPA-NAI-KOHLST                      OCCURS   100.
                       09  SPA-NAI-TKYUKOSTYMD             PIC X(08).
                       09  SPA-NAI-TKYUKOEDYMD             PIC X(08).
                       09  SPA-NAI-TKSKJKBNHON             PIC X(01).
                       09  SPA-NAI-TKSKJKBNTSY             PIC X(01).
                       09  SPA-NAI-TKSKJKBNTNK             PIC X(01).
               05  SPA-NAI-RIREKILST-AREA.
                   07  SPA-NAI-RIREKILST                   OCCURS   20.
                       09  SPA-NAI-TRYUKOSTYMD             PIC X(08).
                       09  SPA-NAI-TRYUKOEDYMD             PIC X(08).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                                     PIC 9(01).
           03  FLG-SYSKANRI                                PIC 9(01).
           03  FLG-HKNNUM                                  PIC 9(01).
           03  FLG-ENTRYCHK                                PIC 9(01).
           03  FLG-HIZUKECHK                               PIC 9(01).
           03  FLG-UPD-KBN                                 PIC 9(01).
           03  FLG-KIKAN-DABURI                            PIC 9(01).
           03  FLG-ERR                                     PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                        PIC 9(05).
           03  IDX1                                        PIC 9(05).
           03  IDX-MAX                                     PIC 9(05).
           03  IDX-CMB                                     PIC 9(05).
           03  IDX-MUL                                     PIC 9(05).
           03  IDX-LST                                     PIC 9(05).
           03  IDXE                                        PIC 9(05).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SEL                                     PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET                              PIC X(64). 
           03  WRK-WIDMSG                                  PIC X(70). 
           03  WRK-MAX                                     PIC 9(05).
           03  WRK-YMD                                     PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY                                 PIC 9(04).
               05  WRK-SMM                                 PIC 9(02).
               05  WRK-SDD                                 PIC 9(02).
           03  WRK-HENYMDG                                 PIC X(09).
           03  WRK-YUKOSTYMD                               PIC X(08).
           03  WRK-YUKOEDYMD                               PIC X(08).
           03  WRK-KANACHK-INPUT                           PIC X(400).
           03  WRK-ZZ9                                     PIC ZZ9.
           03  WRK-ZZZ9                                    PIC ZZZ9.
           03  WRK-ZZZ-G.
               05  WRK-ZZZ                                 PIC ZZZ.
           03  WRK-STR-TMP                                 PIC X(100).
           03  WRK-CMB-CD                                  PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM                             PIC X(200).
               05  WRK-CMB-ITM2                            PIC X(200).
           03  WRK-ERRSTYMD                                PIC X(09).
           03  WRK-ERREDYMD                                PIC X(09).
           03  WRK-SELNUM-G.
               05  WRK-SELNUM-MUL                          PIC X(50).
               05  WRK-INPUT-MODE                          PIC 9(01).
               05  WRK-SELNUM-X                            PIC X(50).
               05  WRK-SELNUM                              PIC 9(03).
               05  WRK-BASESELNUM                          PIC 9(03).
               05  WRK-SELNUM-FLG                          PIC 9(01)
                                                           OCCURS  200.
           03  WRK-MULTI-AREA.
               05  WRK-MUL-NUM                             PIC X(10)
                                                           OCCURS 20.
               05  WRK-MUL-STNUM-X                         PIC X(10).
               05  WRK-MUL-EDNUM-X                         PIC X(10).
               05  WRK-MUL-STNUM                           PIC 9(05).
               05  WRK-MUL-EDNUM                           PIC 9(05).
           03  WRK-PAGENO                                  PIC 9(03).
           03  WRK-NAI-YUKOSTYMD                           PIC X(08).
           03  WRK-NAI-YUKOEDYMD                           PIC X(08).
           03  WRK-SKJKBN-G.
               05  WRK-SKJKBN                              PIC X(01)
                                                           OCCURS 3.
           03  WRK-TABNAME                                 PIC X(20).
      *
       01  KEY-AREA.
           03  KEY-HKNNUM                                  PIC X(03).
      *
       01  SEL-AREA.
           05  SEL-HKNNUM                                  PIC X(03).
           05  SEL-KJNYMD                                  PIC X(08).
           05  SEL-STYMD                                   PIC X(08).
           05  SEL-EDYMD                                   PIC X(08).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-KOHLST-MAX           PIC 9(03) VALUE 100.
           03  CONST-RIREKILST-MAX        PIC 9(03) VALUE 20.
      *
           03  CONST-HOKKAIDO             PIC 9(02) VALUE 01.
           03  CONST-AOMORI               PIC 9(02) VALUE 02.
           03  CONST-IWATE                PIC 9(02) VALUE 03.
           03  CONST-MIYAGI               PIC 9(02) VALUE 04.
           03  CONST-AKITA                PIC 9(02) VALUE 05.
           03  CONST-YAMAGATA             PIC 9(02) VALUE 06.
           03  CONST-FUKUSHIMA            PIC 9(02) VALUE 07.
           03  CONST-IBARAKI              PIC 9(02) VALUE 08.
           03  CONST-TOCHIGI              PIC 9(02) VALUE 09.
           03  CONST-GUNMA                PIC 9(02) VALUE 10.
           03  CONST-SAITAMA              PIC 9(02) VALUE 11.
           03  CONST-CHIBA                PIC 9(02) VALUE 12.
           03  CONST-TOKYO                PIC 9(02) VALUE 13.
           03  CONST-KANAGAWA             PIC 9(02) VALUE 14.
           03  CONST-NIIGATA              PIC 9(02) VALUE 15.
           03  CONST-TOYAMA               PIC 9(02) VALUE 16.
           03  CONST-ISHIKAWA             PIC 9(02) VALUE 17.
           03  CONST-FUKUI                PIC 9(02) VALUE 18.
           03  CONST-YAMANASHI            PIC 9(02) VALUE 19.
           03  CONST-NAGANO               PIC 9(02) VALUE 20.
           03  CONST-GIFU                 PIC 9(02) VALUE 21.
           03  CONST-SHIZUOKA             PIC 9(02) VALUE 22.
           03  CONST-AICHI                PIC 9(02) VALUE 23.
           03  CONST-MIE                  PIC 9(02) VALUE 24.
           03  CONST-SHIGA                PIC 9(02) VALUE 25.
           03  CONST-KYOTO                PIC 9(02) VALUE 26.
           03  CONST-OSAKA                PIC 9(02) VALUE 27.
           03  CONST-HYOGO                PIC 9(02) VALUE 28.
           03  CONST-NARA                 PIC 9(02) VALUE 29.
           03  CONST-WAKAYAMA             PIC 9(02) VALUE 30.
           03  CONST-TOTTORI              PIC 9(02) VALUE 31.
           03  CONST-SHIMANE              PIC 9(02) VALUE 32.
           03  CONST-OKAYAMA              PIC 9(02) VALUE 33.
           03  CONST-HIROSHIMA            PIC 9(02) VALUE 34.
           03  CONST-YAMAGUCHI            PIC 9(02) VALUE 35.
           03  CONST-TOKUSHIMA            PIC 9(02) VALUE 36.
           03  CONST-KAGAWA               PIC 9(02) VALUE 37.
           03  CONST-EHIME                PIC 9(02) VALUE 38.
           03  CONST-KOCHI                PIC 9(02) VALUE 39.
           03  CONST-FUKUOKA              PIC 9(02) VALUE 40.
           03  CONST-SAGA                 PIC 9(02) VALUE 41.
           03  CONST-NAGASAKI             PIC 9(02) VALUE 42.
           03  CONST-KUMAMOTO             PIC 9(02) VALUE 43.
           03  CONST-OITA                 PIC 9(02) VALUE 44.
           03  CONST-MIYAZAKI             PIC 9(02) VALUE 45.
           03  CONST-KAGOSHIMA            PIC 9(02) VALUE 46.
           03  CONST-OKINAWA              PIC 9(02) VALUE 47.
      *
      *    保険番号
       01  THKNNUM-REC.
           COPY    "CPHKNNUM.INC"         REPLACING     //HKN-//
                                          BY            //THKN-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    公費付加情報
           COPY  "CPSK2010.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SEL-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W65
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
           WHEN    OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W65         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG      =   "WERR" )
               MOVE    SPACE       TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF 
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "W65"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "WID1"
               PERFORM 330-WID1-SET-SEC
           WHEN    OTHER
               PERFORM 310-SPASET-SEC
               EVALUATE    TRUE
               WHEN  ( SPA-W01-KBNCD  >=   "100" )
                AND  ( SPA-W01-KBNCD  <=   "899" )
               WHEN  ( SPA-W01-KBNCD   =   "972" )
                   PERFORM 312-SPASET-SEC
               END-EVALUATE
           END-EVALUATE
      *
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *

           INITIALIZE              SPA-W65
      *
           MOVE    SPA-SYSYMD      TO  SPA-NAI-KJNYMD
           MOVE    SPA-SYSYMDWH    TO  SPA-GMN-KJNYMD
      *
           PERFORM 311-SPASET-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       311-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-GMN-RIREKILST-AREA
                                   SPA-NAI-RIREKILST-AREA
      *
           PERFORM 311-COMBO-EDIT-SEC
      *
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           PERFORM 3110-KOHLST-EDIT-SEC
      *
           MOVE    0               TO  SPA-GMN-PAGENO
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       311-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    コンボボックス編集処理
      *****************************************************************
       311-COMBO-EDIT-SEC         SECTION.
      *
      *    実日数省略記載コンボ編集処理
           PERFORM 3110-JNISSUKBNLST-SEC
      *
      *    請求点省略記載コンボ編集処理
           PERFORM 3110-TOTALTENKBNLST-SEC
      *
      *    全国公費併用分請求点集計コンボ編集処理
           PERFORM 3110-SYUKEIKBNLST-SEC
      *
      *    一部負担金金額集計コンボ編集処理
           PERFORM 3110-FTNKEIKBNLST-SEC
      *
      *    一部負担金省略記載コンボ編集処理
           PERFORM 3110-FTNMONEYKBNLST-SEC
      *
      *    一部負担金０円記載（記録）コンボ編集処理
           PERFORM 3110-FTNZEROKBNLST-SEC
      *
      *    入院食事療養省略記載コンボ編集処理
           PERFORM 3110-SKJKBNLST-SEC
      *
      *    ＯＣＲ実日数請求点省略記載コンボ編集処理
           PERFORM 3110-OCRKBNLST-SEC
      *
      *    請求書公費再掲欄記載コンボ編集処理
           PERFORM 3110-SAIKEIKBNLST-SEC
      *
      *    保険欄−一部負担金記載コンボ編集処理
           PERFORM 3110-HKNFTNKBNLST-SEC
      *
      *    設定と異なる記載設定コンボ編集処理
           PERFORM 3110-RECEKSIKBNLST-SEC
      *
      *    チェック対象上限額設定（外来）コンボ編集処理
           PERFORM 3110-CHKJGNKBNGLST-SEC
      *
      *    チェック対象上限額設定（入院）コンボ編集処理
           PERFORM 3110-CHKJGNKBNNLST-SEC
      *
      *    チェック対象上限額設定対象区分コンボ編集処理
           PERFORM 3110-CHKJGNTAIKBNLST-SEC
      *
      *    公費請求テーブル作成コンボ編集処理
           PERFORM 3110-KOHSKYKBNLST-SEC
      **
      *    保険欄の括弧再掲記載コンボ編集処理
           PERFORM 3110-KAKKBNLST-SEC
      *
      *    保険欄の括弧再掲（下）記載コンボ編集処理
           PERFORM 3110-KAK20KBNLST-SEC
      *
      *    保険欄の括弧再掲（下）金額計算コンボ編集処理（主＋全＋地）
           PERFORM 3110-KAK31KBNLST-SEC
      *
      *    保険欄の括弧再掲（下）金額計算コンボ編集処理（主＋全＋地＋長）
           PERFORM 3110-KAK32KBNKLST-SEC
      *
      *    保険欄の括弧再掲（下）金額計算コンボ編集処理（主＋全＋地＋長）
           PERFORM 3110-KAK32KBNYLST-SEC
      *
      *    保険欄の括弧再掲（下）金額加算コンボ編集処理
           PERFORM 3110-KAK40KBNLST-SEC
      *
      *    保険欄−一部負担金無記載コンボ編集処理
           PERFORM 3110-KSINGKBNLST-SEC
      *
      *    一部負担金１円→１０円単位記載コンボ編集処理
           PERFORM 3110-ENCHGKBNLST-SEC
      *
      *    入院時食事療養記載（公費２欄置き換え）コンボ編集処理
           PERFORM 3110-KOH2SKJKBNLST-SEC
      *
      *    一部負担金記載１円未満端数処理（外来）コンボ編集処理
           PERFORM 3110-FTNHASUKBNGLST-SEC
      *
      *    一部負担金記載１円未満端数処理（外来）対象区分コンボ編集処理
           PERFORM 3110-FTNHASUTAIKBNGLST-SEC
      *
      *    一部負担金記載１円未満端数処理（入院）コンボ編集処理
           PERFORM 3110-FTNHASUKBNNLST-SEC
      *
      *    一部負担金記載１円未満端数処理（入院）対象区分コンボ編集処理
           PERFORM 3110-FTNHASUTAIKBNNLST-SEC
      *
      *    月途中受給者証変更時負担計算コンボ編集処理
           PERFORM 3110-JYUHENKBNLST-SEC
      *
      *    日・月上限負担金計算コンボ編集処理
           PERFORM 3110-JGNFTNKBNLST-SEC
      *
      *    患者登録−所得者情報−月上限額入力コンボ編集処理
           PERFORM 3110-PTJGNKBNLST-SEC
      *
      *    患者登録−所得者情報−月上限額入力対象区分コンボ編集処理
           PERFORM 3110-PTJGNTAIKBNLST-SEC
      *
      *    医療費負担金計算（外来）コンボ編集処理
           PERFORM 3110-IRYOKEIKBNLST-SEC
      *
      *    医療費負担金計算対象区分コンボ編集処理
           PERFORM 3110-IRYOKEITAIKBNLST-SEC
      *
      *    食事費負担金計算（外来）コンボ編集処理
           PERFORM 3110-SKJKEIKBNLST-SEC
      *
      *    食事費負担金計算対象区分コンボ編集処理
           PERFORM 3110-SKJKEITAIKBNLST-SEC
      *
      *    設定額を助成する負担金計算コンボ編集処理
           PERFORM 3110-KINJYOKBNLST-SEC
      *
      *    設定額を助成する負担金計算対象区分コンボ編集処理
           PERFORM 3110-KINJYOTAIKBNLST-SEC
      *
      *    「月−院内−上限額」参照条件（外来）コンボ編集処理
           PERFORM 3110-INNAIKBNLST-SEC
      *
      *    「月−院内−上限額」参照条件（外来）対象区分コンボ編集処理
           PERFORM 3110-INNAITAIKBNLST-SEC
      *
      *    自立支援医療の特別計算コンボ編集処理
           PERFORM 3110-JIRITUKBNLST-SEC
      *
      *    自立支援医療の特別計算対象区分コンボ編集処理
           PERFORM 3110-JIRITUTAIKBNLST-SEC
      *
      *    食事療養付加設定コンボ編集処理
           PERFORM 3110-SKJFUKAKBNLST-SEC
      *
      *    食事療養付加設定対象区分本人コンボ編集処理
           PERFORM 3110-SKJFUKATAIKBNHONLST-SEC
      *
      *    食事療養付加設定対象区分低所得コンボ編集処理
           PERFORM 3110-SKJFUKATAIKBNTSYLST-SEC
      *
      *    食事療養付加設定対象区分低年金コンボ編集処理
           PERFORM 3110-SKJFUKATAIKBNTNKLST-SEC
      *
      *    共通対象区分コンボ編集処理
           PERFORM 3110-COMMONTAIKBNLST-SEC
      *
      *    請求書公費再掲欄記載対象区分コンボ編集処理
           PERFORM 3110-SAIKEITAIKBNLST-SEC
      *
      *    月途中受給者証変更時負担計算対象区分コンボ編集処理
           PERFORM 3110-JYUHENTAIKBNLST-SEC
      *
      **
      *    負担金未発生分の回数カウント（外来）コンボ編集処理
           PERFORM 3110-KAICNTKBNGLST-SEC
      *
      *    負担金未発生分の回数カウント（外来）対象区分コンボ編集処理
           PERFORM 3110-KAICNTTAIKBNGLST-SEC
      *
      *    長期併用時の特別計算（外来）コンボ編集処理
           PERFORM 3110-CYOKEIKBNGLST-SEC
      *
      *    長期併用時の特別計算（外来）対象区分コンボ編集処理
           PERFORM 3110-CYOKEITAIKBNGLST-SEC
      *
      *    現物給付対象者の特別計算（高齢者・外）コンボ編集処理
           PERFORM 3110-GNBKEIKBNKGLST-SEC
      *
      *    現物給付対象者の特別計算（高齢者・外）対象区分コンボ編集処理
           PERFORM 3110-GNBKEITAIKBNKGLST-SEC
      *
      *    全国公費併用時の特別計算（外来）コンボ編集処理
           PERFORM 3110-ZENKEIKBNGLST-SEC
      *
      *    全国公費併用時の特別計算（外来）対象区分コンボ編集処理
           PERFORM 3110-ZENKEITAIKBNGLST-SEC
      *
      *    診察料がない場合の負担金計算（外来）コンボ編集処理
           PERFORM 3110-SINKEIKBNLST-SEC
      *
      *    診察料がない場合の負担金計算（外来）対象区分コンボ編集処理
           PERFORM 3110-SINKEITAIKBNLST-SEC
      *
      *    初診時以外の負担金計算コンボ編集処理
           PERFORM 3110-SYOKEIKBNLST-SEC
      *
      *    初診時以外の負担金計算対象区分コンボ編集処理
           PERFORM 3110-SYOKEITAIKBNLST-SEC
      *
      *    １入院＠日限度とする特別計算（入院）コンボ編集処理
           PERFORM 3110-NYUKEIKBNLST-SEC
      *
      *    １入院＠日限度とする特別計算（入院）対象区分コンボ編集処理
           PERFORM 3110-NYUKEITAIKBNLST-SEC
      *
      *    日上限額を固定額とする特別計算（入院）コンボ編集処理
           PERFORM 3110-DAYKEIKBNLST-SEC
      *
      *    日上限額を固定額とする特別計算（入院）対象区分コンボ編集処理
           PERFORM 3110-DAYKEITAIKBNLST-SEC
      *
      *    限度額認定証提示が無い場合の特別計算コンボ編集処理
           PERFORM 3110-NINNASIKBNLST-SEC
      *
      *    限度額認定証提示が無い場合の特別計算対象区分コンボ編集処理
           PERFORM 3110-NINNASITAIKBNLST-SEC
      *
      *    限度額認定証（上位）の場合の特別計算コンボ編集処理
           PERFORM 3110-NINJYOKBNLST-SEC
      *
      *    限度額認定証（上位）の場合の特別計算対象区分コンボ編集処理
           PERFORM 3110-NINJYOTAIKBNLST-SEC
      *
      *    ７０歳未満現物給付高額療養費合算計算コンボ編集処理
           PERFORM 3110-KOGAKKEIKBNLST-SEC
      *
      *    ７０歳未満現物給付高額療養費合算計算対象区分コンボ編集処理
           PERFORM 3110-KOGAKKEITAIKBNLST-SEC
      *
      *    短期滞在手術等基本料２、３の特別計算コンボ編集処理
           PERFORM 3110-TANKIKBNLST-SEC
      *
      *    短期滞在手術等基本料２、３の特別計算対象区分コンボ編集処理
           PERFORM 3110-TANKITAIKBNLST-SEC
      *
      *    負担金未発生分の回数カウント（入院）区分コンボ編集処理
           PERFORM 3110-KAICNTKBNNLST-SEC
      *
      *    負担金未発生分の回数カウント（入院）対象区分コンボ編集処理
           PERFORM 3110-KAICNTTAIKBNNLST-SEC
      *
      *    長期併用時の特別計算（入院）区分コンボ編集処理
           PERFORM 3110-CYOKEIKBNNLST-SEC
      *
      *    長期併用時の特別計算（入院）対象区分コンボ編集処理
           PERFORM 3110-CYOKEITAIKBNNLST-SEC
      *
      *    全国公費併用時の日の負担金計算（入院）区分コンボ編集処理
           PERFORM 3110-ZENDAYKBNNLST-SEC
      *
      *    全国公費併用時の日の負担金計算（入院）対象区分コンボ編集処理
           PERFORM 3110-ZENDAYTAIKBNNLST-SEC
      *
      *    全国公費併用時の特別計算２（外来）コンボ編集処理
           PERFORM 3110-ZENKEI2KBNGLST-SEC
      *
      *    全国公費併用時の特別計算２（外来）対象区分コンボ編集処理
           PERFORM 3110-ZENKEI2TAIKBNGLST-SEC
      *
      *    全国公費併用時の日の負担金計算２（入院）コンボ編集処理
           PERFORM 3110-ZENDAY2KBNNLST-SEC
      *
      *    全国公費併用時の日の負担金計算２（入院）対象区分コンボ編集処理
           PERFORM 3110-ZENDAY2TAIKBNNLST-SEC
      *
      *    初診時の負担金計算（外来）コンボ編集処理
           PERFORM 3110-SYOKEI2KBNGLST-SEC
      *
      *    初診時の負担金計算（外来）対象区分コンボ編集処理
           PERFORM 3110-SYOKEI2TAIKBNGLST-SEC
      *
      *    初診時の負担金計算（入院）コンボ編集処理
           PERFORM 3110-SYOKEI2KBNNLST-SEC
      *
      *    初診時の負担金計算（入院）対象区分コンボ編集処理
           PERFORM 3110-SYOKEI2TAIKBNNLST-SEC
      *
      *    限度額認定証提示が無い場合の特別計算（高齢者）コンボ編集処理
           PERFORM 3110-NINNASIKBNKLST-SEC
      *
      *    限度額認定証提示が無い場合の特別計算（高齢者）対象区分コンボ編集処理
           PERFORM 3110-NINNASITAIKBNKLST-SEC
      *
      *    マル長負担金計算処理コンボ編集処理
           PERFORM 3110-CYOKIFTNKBNLST-SEC
      *
      *    マル長負担金計算処理対象区分コンボ編集処理
           PERFORM 3110-CYOKIFTNTAIKBNLST-SEC
      *
      *    レセプト保険欄一部負担金記載コンボ編集処理
           PERFORM 3110-CYOKIKSIKBNLST-SEC
      *
      *    レセプト保険欄一部負担金記載対象区分コンボ編集処理
           PERFORM 3110-CYOKIKSITAIKBNLST-SEC
      *
           .
       311-COMBO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    実日数省略記載コンボ編集処理
      *****************************************************************
       3110-JNISSUKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-JNISSUKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-JNISSUKBNL    (1)
           MOVE    "上位日数と同日数の場合、省略する"
                                   TO  SPA-GMN-JNISSUKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-JNISSUKBNL    (2)
           MOVE    "上位日数と同日数の場合、省略しない"
                                   TO  SPA-GMN-JNISSUKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-JNISSUKBNL    (3)
           MOVE    "上位日数と同日数の場合、無条件に省略する"
                                   TO  SPA-GMN-JNISSUKBNMEIL (3)
      *
           MOVE    3               TO  SPA-GMN-JNISSUKBN-MAX
      *
           .
       3110-JNISSUKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    請求点省略記載コンボ編集処理
      *****************************************************************
       3110-TOTALTENKBNLST-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-TOTALTENKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-TOTALTENKBNL    (1)
           MOVE    "上位点数と同点数の場合、省略する"
                                   TO  SPA-GMN-TOTALTENKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-TOTALTENKBNL    (2)
           MOVE    "上位点数と同点数の場合、省略しない"
                                   TO  SPA-GMN-TOTALTENKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-TOTALTENKBNL    (3)
           MOVE    "上位点数と同点数の場合、無条件に省略する"
                                   TO  SPA-GMN-TOTALTENKBNMEIL (3)
      *
           MOVE    3               TO  SPA-GMN-TOTALTENKBN-MAX
      *
           .
       3110-TOTALTENKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用分請求点集計コンボ編集処理
      *****************************************************************
       3110-SYUKEIKBNLST-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-SYUKEIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-SYUKEIKBNL    (1)
           MOVE    "集計する"      TO  SPA-GMN-SYUKEIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-SYUKEIKBNL    (2)
           MOVE    "集計しない"    TO  SPA-GMN-SYUKEIKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-SYUKEIKBNL    (3)
           MOVE    "集計しない（請求点が０点となる場合は除く）"
                                   TO  SPA-GMN-SYUKEIKBNMEIL (3)
      *
           MOVE    "3"             TO  SPA-GMN-SYUKEIKBNL    (4)
           MOVE    "集計しない【負担区分対応】"
                                   TO  SPA-GMN-SYUKEIKBNMEIL (4)
      *
           MOVE    "4"             TO  SPA-GMN-SYUKEIKBNL    (5)
           MOVE
           "集計しない（請求点が０点となる場合は除く）【負担区分対応】"
                                   TO  SPA-GMN-SYUKEIKBNMEIL (5)
      *
           MOVE    5               TO  SPA-GMN-SYUKEIKBN-MAX
      *
           .
       3110-SYUKEIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金金額集計コンボ編集処理
      *****************************************************************
       3110-FTNKEIKBNLST-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-FTNKEIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-FTNKEIKBNL    (1)
           MOVE    "集計する"      TO  SPA-GMN-FTNKEIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-FTNKEIKBNL    (2)
           MOVE    "集計しない"    TO  SPA-GMN-FTNKEIKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-FTNKEIKBNL    (3)
           MOVE    "集計する（レセ記載・レセ電記録は省略する）"
                                   TO  SPA-GMN-FTNKEIKBNMEIL (3)
      *
           MOVE    3               TO  SPA-GMN-FTNKEIKBN-MAX
      *
           .
       3110-FTNKEIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金省略記載コンボ編集処理
      *****************************************************************
       3110-FTNMONEYKBNLST-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-FTNMONEYKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-FTNMONEYKBNL    (1)
           MOVE    "上位金額と同金額の場合、省略する"
                                   TO  SPA-GMN-FTNMONEYKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-FTNMONEYKBNL    (2)
           MOVE    "上位金額と同金額の場合、省略しない"
                                   TO  SPA-GMN-FTNMONEYKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-FTNMONEYKBNL    (3)
           MOVE    "上位金額と同金額の場合、無条件に省略する"
                                   TO  SPA-GMN-FTNMONEYKBNMEIL (3)
      *
           MOVE    3               TO  SPA-GMN-FTNMONEYKBN-MAX
      *
           .
       3110-FTNMONEYKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金０円記載（記録）コンボ編集処理
      *****************************************************************
       3110-FTNZEROKBNLST-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-FTNZEROKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-FTNZEROKBNL    (1)
           MOVE    "０円の場合、省略する"
                                   TO  SPA-GMN-FTNZEROKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-FTNZEROKBNL    (2)
           MOVE    "０円の場合、省略しない"
                                   TO  SPA-GMN-FTNZEROKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-FTNZEROKBNL    (3)
           MOVE    "０円の場合、無条件に省略する"
                                   TO  SPA-GMN-FTNZEROKBNMEIL (3)
      *
           MOVE    3               TO  SPA-GMN-FTNZEROKBN-MAX
      *
           .
       3110-FTNZEROKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    入院食事療養省略記載コンボ編集処理
      *****************************************************************
       3110-SKJKBNLST-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-SKJKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-SKJKBNL    (1)
           MOVE    "上位食事療養と同内容の場合、省略する"
                                   TO  SPA-GMN-SKJKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-SKJKBNL    (2)
           MOVE    "上位食事療養と同内容の場合、省略しない"
                                   TO  SPA-GMN-SKJKBNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-SKJKBN-MAX
      *
           .
       3110-SKJKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    ＯＣＲ実日数請求点省略記載コンボ編集処理
      *****************************************************************
       3110-OCRKBNLST-SEC             SECTION.
      *
           INITIALIZE                  SPA-GMN-OCRKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-OCRKBNL    (1)
           MOVE    "上位日数点数と同日数点数の場合、０とする"
                                   TO  SPA-GMN-OCRKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-OCRKBNL    (2)
           MOVE    "上位日数点数と同日数点数の場合、０としない"
                                   TO  SPA-GMN-OCRKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-OCRKBNL    (3)
           MOVE    "上位日数点数と同日数点数の場合、無条件に０とする"
                                   TO  SPA-GMN-OCRKBNMEIL (3)
      *
           MOVE    3               TO  SPA-GMN-OCRKBN-MAX
      *
           .
       3110-OCRKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    請求書公費再掲欄記載コンボ編集処理
      *****************************************************************
       3110-SAIKEIKBNLST-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-SAIKEIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-SAIKEIKBNL    (1)
           MOVE    "記載する"      TO  SPA-GMN-SAIKEIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-SAIKEIKBNL    (2)
           MOVE    "記載しない"    TO  SPA-GMN-SAIKEIKBNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-SAIKEIKBN-MAX
      *
           .
       3110-SAIKEIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    保険欄−一部負担金記載コンボ編集処理
      *****************************************************************
       3110-HKNFTNKBNLST-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-HKNFTNKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-HKNFTNKBNL    (1)
           MOVE    "所得区分に準ずる金額記載（記録）とする" 
                                   TO  SPA-GMN-HKNFTNKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-HKNFTNKBNL    (2)
           MOVE    "一律“一般区分”での金額記載（記録）とする"
                                   TO  SPA-GMN-HKNFTNKBNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-HKNFTNKBN-MAX
      *
           .
       3110-HKNFTNKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    設定と異なる記載設定コンボ編集処理
      *****************************************************************
       3110-RECEKSIKBNLST-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-RECEKSIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-RECEKSIKBNL    (1)
           MOVE    "設定なし" 
                                   TO  SPA-GMN-RECEKSIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-RECEKSIKBNL    (2)
           MOVE    "負担に関係なく記載なし"
                                   TO  SPA-GMN-RECEKSIKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-RECEKSIKBNL    (3)
           MOVE    "負担上限未満記載あり"
                                   TO  SPA-GMN-RECEKSIKBNMEIL (3)
      *
           MOVE    "3"             TO  SPA-GMN-RECEKSIKBNL    (4)
           STRING    "設定額以上記載なし（設定額未満・以上で異なる"
                                       DELIMITED BY SIZE
                     "負担金計算）"
                                       DELIMITED BY SIZE
           INTO                        SPA-GMN-RECEKSIKBNMEIL (4)
           END-STRING
      *
           MOVE    4               TO  SPA-GMN-RECEKSIKBN-MAX
      *
           .
       3110-RECEKSIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象上限額設定（外来）コンボ編集処理
      *****************************************************************
       3110-CHKJGNKBNGLST-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-CHKJGNKBNGLST-G
      *
           MOVE    "0"             TO  SPA-GMN-CHKJGNKBNGL    (1)
           MOVE    "月−上限額（上限額が０円の場合はチェックしない）"
                                   TO  SPA-GMN-CHKJGNKBNGMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-CHKJGNKBNGL    (2)
           MOVE "日−上限額（上限額が０円の場合は月−上限額でチェック）"
                                   TO  SPA-GMN-CHKJGNKBNGMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-CHKJGNKBNGL    (3)
           MOVE "回−上限額（上限額が０円の場合は月−上限額でチェック）"
                                   TO  SPA-GMN-CHKJGNKBNGMEIL (3)
      *
           MOVE    3               TO  SPA-GMN-CHKJGNKBNG-MAX
      *
           .
       3110-CHKJGNKBNGLST-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象上限額設定（入院）コンボ編集処理
      *****************************************************************
       3110-CHKJGNKBNNLST-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-CHKJGNKBNNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-CHKJGNKBNNL    (1)
           MOVE    "月−上限額（上限額が０円の場合はチェックしない）"
                                   TO  SPA-GMN-CHKJGNKBNNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-CHKJGNKBNNL    (2)
           MOVE "日−上限額（上限額が０円の場合は月−上限額でチェック）"
                                   TO  SPA-GMN-CHKJGNKBNNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-CHKJGNKBNN-MAX
      *
           .
       3110-CHKJGNKBNNLST-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象上限額設定対象区分コンボ編集処理
      *****************************************************************
       3110-CHKJGNTAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-CHKJGNTAIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-CHKJGNTAIKBNL    (1)
           MOVE    "負担上限未満記載なし設定公費"
                                   TO  SPA-GMN-CHKJGNTAIKBNMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-CHKJGNTAIKBN-MAX
      *
           .
       3110-CHKJGNTAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    公費請求テーブル作成コンボ編集処理
      *****************************************************************
       3110-KOHSKYKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KOHSKYKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-KOHSKYKBNL    (1)
           MOVE    "作成しない"
                                   TO  SPA-GMN-KOHSKYKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-KOHSKYKBNL    (2)
           MOVE    "作成する"
                                   TO  SPA-GMN-KOHSKYKBNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-KOHSKYKBN-MAX
      *
           .
       3110-KOHSKYKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲記載コンボ編集処理
      *****************************************************************
       3110-KAKKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KAKKBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-KAKKBNL    (1)
           MOVE  "保険欄−一部負担金と同金額の場合、記載（記録）しない"
                                   TO  SPA-GMN-KAKKBNMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-KAKKBNL    (2)
           MOVE  "保険欄−一部負担金と同金額の場合、記載（記録）する"
                                   TO  SPA-GMN-KAKKBNMEIL (2)
      *
      *
           MOVE    2               TO  SPA-GMN-KAKKBN-MAX
      *
           .
       3110-KAKKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）記載コンボ編集処理
      *****************************************************************
       3110-KAK20KBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KAK20KBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-KAK20KBNL    (1)
           MOVE  "記載（記録）しない"
                                   TO  SPA-GMN-KAK20KBNMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-KAK20KBNL    (2)
           MOVE  "記載（記録）する"
                                   TO  SPA-GMN-KAK20KBNMEIL (2)
      *
      *
           MOVE    2               TO  SPA-GMN-KAK20KBN-MAX
      *
           .
       3110-KAK20KBNLST-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額計算コンボ編集処理（主＋全＋地）
      *****************************************************************
       3110-KAK31KBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KAK31KBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-KAK31KBNL    (1)
           MOVE  "（主＋地の主保険負担相当額）と（月上限額）を比較"
                                   TO  SPA-GMN-KAK31KBNMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-KAK31KBNL    (2)
           STRING    "（主＋地の主保険負担相当額）と"
                                       DELIMITED BY SIZE
                     "（月上限額−公１負担金）を比較"
                                       DELIMITED BY SIZE
           INTO                        SPA-GMN-KAK31KBNMEIL (2)
           END-STRING
      *
      *
           MOVE    2               TO  SPA-GMN-KAK31KBN-MAX
      *
           .
       3110-KAK31KBNLST-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額計算コンボ編集処理（主＋全＋地＋長）
      *****************************************************************
       3110-KAK32KBNKLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KAK32KBNKLST-G
      *
           MOVE  "0"               TO  SPA-GMN-KAK32KBNKL    (1)
           STRING    "（主＋地＋長の主保険負担相当額）と"
                                       DELIMITED BY SIZE
                     "（月上限額−括弧再掲（上））を比較"
                                       DELIMITED BY SIZE
           INTO                        SPA-GMN-KAK32KBNKMEIL (1)
           END-STRING
      *
           MOVE  "1"               TO  SPA-GMN-KAK32KBNKL    (2)
           STRING    "（主＋地＋長の主保険負担相当額）と"
                                       DELIMITED BY SIZE
                     "（月上限額−公１負担金）を比較"
                                       DELIMITED BY SIZE
           INTO                        SPA-GMN-KAK32KBNKMEIL (2)
           END-STRING
      *
      *
           MOVE  "2"               TO  SPA-GMN-KAK32KBNKL    (3)
           STRING    "（主＋地＋長の主保険負担相当額）と"
                                       DELIMITED BY SIZE
                     "（月上限額）を比較"
                                       DELIMITED BY SIZE
           INTO                        SPA-GMN-KAK32KBNKMEIL (3)
           END-STRING
      *
           MOVE  "3"               TO  SPA-GMN-KAK32KBNKL    (4)
           STRING    "１と同様（外来＋高齢＋低所の場合、"
                                       DELIMITED BY SIZE
                     "低所の月上限額で計算を行う）"
                                       DELIMITED BY SIZE
           INTO                        SPA-GMN-KAK32KBNKMEIL (4)
           END-STRING
      *
           MOVE    4               TO  SPA-GMN-KAK32KBNK-MAX
      *
           .
       3110-KAK32KBNKLST-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額計算コンボ編集処理（主＋全＋地＋長）
      *****************************************************************
       3110-KAK32KBNYLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KAK32KBNYLST-G
      *
           MOVE  "0"               TO  SPA-GMN-KAK32KBNYL    (1)
           STRING    "（主＋地＋長の主保険負担相当額）と"
                                       DELIMITED BY SIZE
                     "（月上限額−括弧再掲（上））を比較"
                                       DELIMITED BY SIZE
           INTO                        SPA-GMN-KAK32KBNYMEIL (1)
           END-STRING
      *
           MOVE  "1"               TO  SPA-GMN-KAK32KBNYL    (2)
           STRING    "（主＋地＋長の主保険負担相当額）と"
                                       DELIMITED BY SIZE
                     "（月上限額−公１負担金）を比較"
                                       DELIMITED BY SIZE
           INTO                        SPA-GMN-KAK32KBNYMEIL (2)
           END-STRING
      *
      *
           MOVE  "2"               TO  SPA-GMN-KAK32KBNYL    (3)
           STRING    "（主＋地＋長の主保険負担相当額）と"
                                       DELIMITED BY SIZE
                     "（月上限額）を比較"
                                       DELIMITED BY SIZE
           INTO                        SPA-GMN-KAK32KBNYMEIL (3)
           END-STRING
      *
           MOVE    3               TO  SPA-GMN-KAK32KBNY-MAX
      *
           .
       3110-KAK32KBNYLST-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額加算コンボ編集処理
      *****************************************************************
       3110-KAK40KBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KAK40KBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-KAK40KBNL    (1)
           MOVE  "設定なし"
                                   TO  SPA-GMN-KAK40KBNMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-KAK40KBNL    (2)
           MOVE "公１負担金を加算（保険欄−一部負担金には反映させない）"
                                   TO  SPA-GMN-KAK40KBNMEIL (2)
      *
      *
           MOVE    2               TO  SPA-GMN-KAK40KBN-MAX
      *
           .
       3110-KAK40KBNLST-EXT.
           EXIT.
      *****************************************************************
      *    保険欄−一部負担金無記載コンボ編集処理
      *****************************************************************
       3110-KSINGKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KSINGKBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-KSINGKBNL    (1)
           MOVE  "設定なし"
                                   TO  SPA-GMN-KSINGKBNMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-KSINGKBNL    (2)
           MOVE  "無記載（記録）とする"
                                   TO  SPA-GMN-KSINGKBNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-KSINGKBN-MAX
      *
           .
       3110-KSINGKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金１円→１０円単位記載コンボ編集処理
      *****************************************************************
       3110-ENCHGKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-ENCHGKBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-ENCHGKBNL    (1)
           MOVE  "設定なし"
                                   TO  SPA-GMN-ENCHGKBNMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-ENCHGKBNL    (2)
           MOVE  "１０円単位で記載（記録）する"
                                   TO  SPA-GMN-ENCHGKBNMEIL (2)
      *
           MOVE  "2"               TO  SPA-GMN-ENCHGKBNL    (3)
           MOVE  "保険欄−一部負担金記載がある場合、１０円単位"
                                   TO  SPA-GMN-ENCHGKBNMEIL (3)
      *
           MOVE  "3"               TO  SPA-GMN-ENCHGKBNL    (4)
           STRING  "保険欄−一部負担金記載がある場合、"
                                       DELIMITED BY SIZE
                   "１０円単位（１％計算時を除く）"
                                       DELIMITED BY SIZE
           INTO                        SPA-GMN-ENCHGKBNMEIL (4)
           END-STRING
      *
      *
           MOVE    4               TO  SPA-GMN-ENCHGKBN-MAX
      *
           .
       3110-ENCHGKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    入院時食事療養記載（公費２欄置き換え）コンボ編集処理
      *****************************************************************
       3110-KOH2SKJKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KOH2SKJKBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-KOH2SKJKBNL    (1)
           MOVE  "設定なし"
                                   TO  SPA-GMN-KOH2SKJKBNMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-KOH2SKJKBNL    (2)
           MOVE  "（保険欄−公費１欄）の値を記載（記録）する"
                                   TO  SPA-GMN-KOH2SKJKBNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-KOH2SKJKBN-MAX
      *
           .
       3110-KOH2SKJKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金記載１円未満端数処理（外来）コンボ編集処理
      *****************************************************************
       3110-FTNHASUKBNGLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-FTNHASUKBNGLST-G
      *
           MOVE  "0"               TO  SPA-GMN-FTNHASUKBNGL    (1)
           MOVE  "１円未満を切り捨てた金額記載"
                                   TO  SPA-GMN-FTNHASUKBNGMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-FTNHASUKBNGL    (2)
           MOVE  "１円未満を切り上げた金額記載"
                                   TO  SPA-GMN-FTNHASUKBNGMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-FTNHASUKBNG-MAX
      *
           .
       3110-FTNHASUKBNGLST-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金記載１円未満端数処理（外来）対象区分コンボ編集処理
      *****************************************************************
       3110-FTNHASUTAIKBNGLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-FTNHASUTAIKBNGLST-G
      *
           MOVE  "0"               TO  SPA-GMN-FTNHASUTAIKBNGL    (1)
           MOVE  "レセプト請求分全て"
                                   TO  SPA-GMN-FTNHASUTAIKBNGMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-FTNHASUTAIKBNG-MAX
      *
           .
       3110-FTNHASUTAIKBNGLST-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金記載１円未満端数処理（入院）コンボ編集処理
      *****************************************************************
       3110-FTNHASUKBNNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-FTNHASUKBNNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-FTNHASUKBNNL    (1)
           MOVE  "１円未満を切り捨てた金額記載"
                                   TO  SPA-GMN-FTNHASUKBNNMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-FTNHASUKBNNL    (2)
           MOVE  "１円未満を切り上げた金額記載"
                                   TO  SPA-GMN-FTNHASUKBNNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-FTNHASUKBNN-MAX
      *
           .
       3110-FTNHASUKBNNLST-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金記載１円未満端数処理（入院）対象区分コンボ編集処理
      *****************************************************************
       3110-FTNHASUTAIKBNNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-FTNHASUTAIKBNNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-FTNHASUTAIKBNNL    (1)
           MOVE  "レセプト請求分全て"
                                   TO  SPA-GMN-FTNHASUTAIKBNNMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-FTNHASUTAIKBNN-MAX
      *
           .
       3110-FTNHASUTAIKBNNLST-EXT.
           EXIT.
      *****************************************************************
      *    負担金未発生分の回数カウント（外来）コンボ編集処理
      *****************************************************************
       3110-KAICNTKBNGLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KAICNTKBNGLST-G
      *
           MOVE  "0"               TO  SPA-GMN-KAICNTKBNGL    (1)
           MOVE  "回数にカウントしない"
                                   TO  SPA-GMN-KAICNTKBNGMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-KAICNTKBNGL    (2)
           MOVE  "回数にカウントする"
                                   TO  SPA-GMN-KAICNTKBNGMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-KAICNTKBNG-MAX
      *
           .
       3110-KAICNTKBNGLST-EXT.
           EXIT.
      *****************************************************************
      *    負担金未発生分の回数カウント（外来）対象区分コンボ編集処理
      *****************************************************************
       3110-KAICNTTAIKBNGLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KAICNTTAIKBNGLST-G
      *
           MOVE  "0"               TO  SPA-GMN-KAICNTTAIKBNGL    (1)
           MOVE  "上限回数設定のある公費"
                                   TO  SPA-GMN-KAICNTTAIKBNGMEIL (1)
      *
      *
           MOVE    1               TO  SPA-GMN-KAICNTTAIKBNG-MAX
      *
           .
       3110-KAICNTTAIKBNGLST-EXT.
           EXIT.
      *****************************************************************
      *    長期併用時の特別計算（外来）コンボ編集処理
      *****************************************************************
       3110-CYOKEIKBNGLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-CYOKEIKBNGLST-G
      *
           MOVE  "0"               TO  SPA-GMN-CYOKEIKBNGL    (1)
           MOVE  "設定なし"
                                   TO  SPA-GMN-CYOKEIKBNGMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-CYOKEIKBNGL    (2)
           STRING
                 "（長期の月上限額−診療済み長期負担相当額）と"
                                       DELIMITED BY SIZE
                 "（回・日上限額）を比較"
                                       DELIMITED BY SIZE
           INTO                        SPA-GMN-CYOKEIKBNGMEIL (2)
           END-STRING
      *
           MOVE  "2"               TO  SPA-GMN-CYOKEIKBNGL    (3)
           STRING
                 "１と同様（高齢現物＋低所の場合、低所の月上限額で"
                                       DELIMITED BY SIZE
                 "計算を行う）"
                                       DELIMITED BY SIZE
           INTO                        SPA-GMN-CYOKEIKBNGMEIL (3)
           END-STRING
      *
           MOVE  "3"               TO  SPA-GMN-CYOKEIKBNGL    (4)
           MOVE  "１と同様（更生・育成併用時を除く）"
                                   TO  SPA-GMN-CYOKEIKBNGMEIL (4)
      *
           MOVE  "4"               TO  SPA-GMN-CYOKEIKBNGL    (5)
           MOVE  "２と同様（更生・育成併用時を除く）"
                                   TO  SPA-GMN-CYOKEIKBNGMEIL (5)
      *
           MOVE    5               TO  SPA-GMN-CYOKEIKBNG-MAX
      *
           .
       3110-CYOKEIKBNGLST-EXT.
           EXIT.
      *****************************************************************
      *    長期併用時の特別計算（外来）対象区分コンボ編集処理
      *****************************************************************
       3110-CYOKEITAIKBNGLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-CYOKEITAIKBNGLST-G
      *
           MOVE  "0"               TO  SPA-GMN-CYOKEITAIKBNGL    (1)
           MOVE  "回・日上限額設定のある公費"
                                   TO  SPA-GMN-CYOKEITAIKBNGMEIL (1)
      *
      *
           MOVE    1               TO  SPA-GMN-CYOKEITAIKBNG-MAX
      *
           .
       3110-CYOKEITAIKBNGLST-EXT.
           EXIT.
      *****************************************************************
      *    現物給付対象者の特別計算（高齢者・外）コンボ編集処理
      *****************************************************************
       3110-GNBKEIKBNKGLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-GNBKEIKBNKGLST-G
      *
           MOVE  "0"               TO  SPA-GMN-GNBKEIKBNKGL    (1)
           MOVE  "設定なし"
                                   TO  SPA-GMN-GNBKEIKBNKGMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-GNBKEIKBNKGL    (2)
           STRING    "（月上限額−診療済み主保険負担相当額）と"
                                       DELIMITED BY SIZE
                     "（回・日上限額）を比較"
                                       DELIMITED BY SIZE
           INTO                        SPA-GMN-GNBKEIKBNKGMEIL (2)
           END-STRING
      *
           MOVE  "2"               TO  SPA-GMN-GNBKEIKBNKGL    (3)
           STRING    " １と同様（診療済み主保険負担相当額に"
                                       DELIMITED BY SIZE
                     "主保険単独分も含める）"
                                       DELIMITED BY SIZE
           INTO                        SPA-GMN-GNBKEIKBNKGMEIL (3)
           END-STRING
      *
      *
           MOVE    3               TO  SPA-GMN-GNBKEIKBNKG-MAX
      *
           .
       3110-GNBKEIKBNKGLST-EXT.
           EXIT.
      *****************************************************************
      *    現物給付対象者の特別計算（高齢者・外）対象区分コンボ編集処理
      *****************************************************************
       3110-GNBKEITAIKBNKGLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-GNBKEITAIKBNKGLST-G
      *
           MOVE  "0"               TO  SPA-GMN-GNBKEITAIKBNKGL    (1)
           MOVE  "回・日上限額設定のある公費"
                                   TO  SPA-GMN-GNBKEITAIKBNKGMEIL (1)
      *
      *
           MOVE    1               TO  SPA-GMN-GNBKEITAIKBNKG-MAX
      *
           .
       3110-GNBKEITAIKBNKGLST-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の特別計算（外来）コンボ編集処理
      *****************************************************************
       3110-ZENKEIKBNGLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-ZENKEIKBNGLST-G
      *
           MOVE  "0"               TO  SPA-GMN-ZENKEIKBNGL    (1)
           MOVE  "設定なし"
                                   TO  SPA-GMN-ZENKEIKBNGMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-ZENKEIKBNGL    (2)
           MOVE
              "全国公費が月上限額に達した日以降も一部負担金を発生させる"
                                   TO  SPA-GMN-ZENKEIKBNGMEIL (2)
      *
      *
           MOVE    2               TO  SPA-GMN-ZENKEIKBNG-MAX
      *
           .
       3110-ZENKEIKBNGLST-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の特別計算（外来）対象区分コンボ編集処理
      *****************************************************************
       3110-ZENKEITAIKBNGLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-ZENKEITAIKBNGLST-G
      *
           MOVE  "0"               TO  SPA-GMN-ZENKEITAIKBNGL    (1)
           MOVE  "患者負担のある公費"
                                   TO  SPA-GMN-ZENKEITAIKBNGMEIL (1)
      *
      *
           MOVE    1               TO  SPA-GMN-ZENKEITAIKBNG-MAX
      *
           .
       3110-ZENKEITAIKBNGLST-EXT.
           EXIT.
      *****************************************************************
      *    診察料がない場合の負担金計算（外来）コンボ編集処理
      *****************************************************************
       3110-SINKEIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-SINKEIKBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-SINKEIKBNL    (1)
           MOVE  "通常計算"
                                   TO  SPA-GMN-SINKEIKBNMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-SINKEIKBNL    (2)
           MOVE  "一部負担金を発生させない"
                                   TO  SPA-GMN-SINKEIKBNMEIL (2)
      *
      *
           MOVE    2               TO  SPA-GMN-SINKEIKBN-MAX
      *
           .
       3110-SINKEIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    診察料がない場合の負担金計算（外来）対象区分コンボ編集処理
      *****************************************************************
       3110-SINKEITAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-SINKEITAIKBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-SINKEITAIKBNL    (1)
           MOVE  "患者負担のある公費"
                                   TO  SPA-GMN-SINKEITAIKBNMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-SINKEITAIKBN-MAX
      *
           .
       3110-SINKEITAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    初診時以外の負担金計算コンボ編集処理
      *****************************************************************
       3110-SYOKEIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-SYOKEIKBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-SYOKEIKBNL    (1)
           MOVE  "通常計算"
                                   TO  SPA-GMN-SYOKEIKBNMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-SYOKEIKBNL    (2)
           MOVE  "一部負担金を発生させない"
                                   TO  SPA-GMN-SYOKEIKBNMEIL (2)
      *
      *
           MOVE    2               TO  SPA-GMN-SYOKEIKBN-MAX
      *
           .
       3110-SYOKEIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    初診時以外の負担金計算対象区分コンボ編集処理
      *****************************************************************
       3110-SYOKEITAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-SYOKEITAIKBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-SYOKEITAIKBNL    (1)
           MOVE  "患者負担のある公費"
                                   TO  SPA-GMN-SYOKEITAIKBNMEIL (1)
      *
      *
           MOVE    1               TO  SPA-GMN-SYOKEITAIKBN-MAX
      *
           .
       3110-SYOKEITAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    １入院＠日限度とする特別計算（入院）コンボ編集処理
      *****************************************************************
       3110-NYUKEIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-NYUKEIKBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-NYUKEIKBNL    (1)
           MOVE  "設定なし"
                                   TO  SPA-GMN-NYUKEIKBNMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-NYUKEIKBNL    (2)
           STRING      "初回入院日を基準日とし、"
                                       DELIMITED BY SIZE
                       "１入院＠日限度とする負担金計算を行う"
                                       DELIMITED BY SIZE
           INTO                        SPA-GMN-NYUKEIKBNMEIL (2)
           END-STRING
      *
      *
           MOVE    2               TO  SPA-GMN-NYUKEIKBN-MAX
      *
           .
       3110-NYUKEIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    １入院＠日限度とする特別計算（入院）対象区分コンボ編集処理
      *****************************************************************
       3110-NYUKEITAIKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-NYUKEITAIKBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-NYUKEITAIKBNL    (1)
           MOVE  "月上限回数設定のある公費"
                                   TO  SPA-GMN-NYUKEITAIKBNMEIL (1)
      *
      *
           MOVE    1               TO  SPA-GMN-NYUKEITAIKBN-MAX
      *
           .
       3110-NYUKEITAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    日上限額を固定額とする特別計算（入院）コンボ編集処理
      *****************************************************************
       3110-DAYKEIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-DAYKEIKBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-DAYKEIKBNL    (1)
           MOVE  "設定なし"
                                   TO  SPA-GMN-DAYKEIKBNMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-DAYKEIKBNL    (2)
           STRING      "日上限額未満の場合も日上限額を固定額とする"
                                       DELIMITED BY SIZE
                       "負担金計算を行う"
                                       DELIMITED BY SIZE
           INTO                        SPA-GMN-DAYKEIKBNMEIL (2)
           END-STRING
      *
      *
           MOVE    2               TO  SPA-GMN-DAYKEIKBN-MAX
      *
           .
       3110-DAYKEIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    日上限額を固定額とする特別計算（入院）対象区分コンボ編集処理
      *****************************************************************
       3110-DAYKEITAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-DAYKEITAIKBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-DAYKEITAIKBNL    (1)
           MOVE  "日上限額設定のある公費"
                                   TO  SPA-GMN-DAYKEITAIKBNMEIL (1)
      *
      *
           MOVE    1               TO  SPA-GMN-DAYKEITAIKBN-MAX
      *
           .
       3110-DAYKEITAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    限度額認定証提示が無い場合の特別計算コンボ編集処理
      *****************************************************************
       3110-NINNASIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-NINNASIKBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-NINNASIKBNL    (1)
           MOVE  "設定なし"
                                   TO  SPA-GMN-NINNASIKBNMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-NINNASIKBNL    (2)
           MOVE  "一般区分の月上限額超の額を患者負担に加算"
                                   TO  SPA-GMN-NINNASIKBNMEIL (2)
      *
           MOVE  "2"               TO  SPA-GMN-NINNASIKBNL    (3)
           STRING      "一般区分の月上限額超の額を患者負担に加算"
                                       DELIMITED BY SIZE
                       "（高額４回目適用）"
                                       DELIMITED BY SIZE
           INTO                        SPA-GMN-NINNASIKBNMEIL (3)
           END-STRING
      *
           MOVE  "3"               TO  SPA-GMN-NINNASIKBNL    (4)
           MOVE  "一般・低所区分の月上限額超の額を患者負担に加算"
                                   TO  SPA-GMN-NINNASIKBNMEIL (4)
      *
           MOVE  "4"               TO  SPA-GMN-NINNASIKBNL    (5)
           STRING      "一般・低所区分の月上限額超の額を患者負担に加算"
                                       DELIMITED BY SIZE
                       "（高額４回目適用）"
                                       DELIMITED BY SIZE
           INTO                        SPA-GMN-NINNASIKBNMEIL (5)
           END-STRING
      *
           MOVE  "5"               TO  SPA-GMN-NINNASIKBNL    (6)
           STRING      "区分（エ）の月上限額超の額を患者負担に加算"
                                       DELIMITED BY SIZE
           INTO                        SPA-GMN-NINNASIKBNMEIL (6)
           END-STRING
      *
           MOVE  "6"               TO  SPA-GMN-NINNASIKBNL    (7)
           STRING      "区分（エ）の月上限額超の額を患者負担に加算"
                                       DELIMITED BY SIZE
                       "（高額４回目適用）"
                                       DELIMITED BY SIZE
           INTO                        SPA-GMN-NINNASIKBNMEIL (7)
           END-STRING
      *
           MOVE  "7"               TO  SPA-GMN-NINNASIKBNL    (8)
           STRING      "区分（エ・オ）の月上限額超の額を患者負担に加算"
                                       DELIMITED BY SIZE
           INTO                        SPA-GMN-NINNASIKBNMEIL (8)
           END-STRING
      *
           MOVE  "8"               TO  SPA-GMN-NINNASIKBNL    (9)
           STRING      "区分（エ・オ）の月上限額超の額を患者負担に加算"
                                       DELIMITED BY SIZE
                       "（高額４回目適用）"
                                       DELIMITED BY SIZE
           INTO                        SPA-GMN-NINNASIKBNMEIL (9)
           END-STRING
      *
      *
           MOVE    9               TO  SPA-GMN-NINNASIKBN-MAX
      *
           .
       3110-NINNASIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    限度額認定証提示が無い場合の特別計算対象区分コンボ編集処理
      *****************************************************************
       3110-NINNASITAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-NINNASITAIKBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-NINNASITAIKBNL    (1)
           MOVE  "該当公費対象者全員"
                                   TO  SPA-GMN-NINNASITAIKBNMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-NINNASITAIKBNL    (2)
           MOVE  "社保・国保組合"
                                   TO  SPA-GMN-NINNASITAIKBNMEIL (2)
      *
           MOVE  "2"               TO  SPA-GMN-NINNASITAIKBNL    (3)
           MOVE  "社保"
                                   TO  SPA-GMN-NINNASITAIKBNMEIL (3)
      *
           MOVE  "3"               TO  SPA-GMN-NINNASITAIKBNL    (4)
           MOVE  "国保組合"
                                   TO  SPA-GMN-NINNASITAIKBNMEIL (4)
      *
      *
           MOVE    4               TO  SPA-GMN-NINNASITAIKBN-MAX
      *
           .
       3110-NINNASITAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    限度額認定証（上位）の場合の特別計算コンボ編集処理
      *****************************************************************
       3110-NINJYOKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-NINJYOKBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-NINJYOKBNL    (1)
           MOVE  "設定なし"
                                   TO  SPA-GMN-NINJYOKBNMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-NINJYOKBNL    (2)
           MOVE  "一般区分の月上限額超の額を患者負担に加算"
                                   TO  SPA-GMN-NINJYOKBNMEIL (2)
      *
           MOVE  "2"               TO  SPA-GMN-NINJYOKBNL    (3)
           MOVE
           "一般区分の月上限額超の額を患者負担に加算（高額４回目適用）"
                                   TO  SPA-GMN-NINJYOKBNMEIL (3)
      *
      *
           MOVE    3               TO  SPA-GMN-NINJYOKBN-MAX
      *
           .
       3110-NINJYOKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    限度額認定証（上位）の場合の特別計算対象区分コンボ編集処理
      *****************************************************************
       3110-NINJYOTAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-NINJYOTAIKBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-NINJYOTAIKBNL    (1)
           MOVE  "該当公費対象者全員"
                                   TO  SPA-GMN-NINJYOTAIKBNMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-NINJYOTAIKBNL    (2)
           MOVE  "社保・国保組合"
                                   TO  SPA-GMN-NINJYOTAIKBNMEIL (2)
      *
           MOVE  "2"               TO  SPA-GMN-NINJYOTAIKBNL    (3)
           MOVE  "社保"
                                   TO  SPA-GMN-NINJYOTAIKBNMEIL (3)
      *
           MOVE  "3"               TO  SPA-GMN-NINJYOTAIKBNL    (4)
           MOVE  "国保組合"
                                   TO  SPA-GMN-NINJYOTAIKBNMEIL (4)
      *
      *
           MOVE    4               TO  SPA-GMN-NINJYOTAIKBN-MAX
      *
           .
       3110-NINJYOTAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    ７０歳未満現物給付高額療養費合算計算コンボ編集処理
      *****************************************************************
       3110-KOGAKKEIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KOGAKKEIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-KOGAKKEIKBNL    (1)
           STRING    "主単独と（主＋地）がある場合、"
                                       DELIMITED BY SIZE
                     "両方合算対象となる時のみ合算計算"
                                       DELIMITED BY SIZE
           INTO                        SPA-GMN-KOGAKKEIKBNMEIL (1)
           END-STRING
      *
           MOVE    "1"               TO  SPA-GMN-KOGAKKEIKBNL  (2)
           MOVE    "主単独と（主＋地）がある場合、無条件に合算計算"
                                   TO  SPA-GMN-KOGAKKEIKBNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-KOGAKKEIKBN-MAX
      *
           .
       3110-KOGAKKEIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    ７０歳未満現物給付高額療養費合算計算対象区分コンボ編集処理
      *****************************************************************
       3110-KOGAKKEITAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KOGAKKEITAIKBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-KOGAKKEITAIKBNL    (1)
           MOVE  "該当公費対象者全員"
                                   TO  SPA-GMN-KOGAKKEITAIKBNMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-KOGAKKEITAIKBNL    (2)
           MOVE  "国保・広域"
                                   TO  SPA-GMN-KOGAKKEITAIKBNMEIL (2)
      *
           MOVE  "2"               TO  SPA-GMN-KOGAKKEITAIKBNL    (3)
           MOVE  "社保"
                                   TO  SPA-GMN-KOGAKKEITAIKBNMEIL (3)
      *
           MOVE    3               TO  SPA-GMN-KOGAKKEITAIKBN-MAX
      *
           .
       3110-KOGAKKEITAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    短期滞在手術等基本料２、３の特別計算コンボ編集処理
      *****************************************************************
       3110-TANKIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-TANKIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-TANKIKBNL    (1)
           MOVE    "設定なし"      TO  SPA-GMN-TANKIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-TANKIKBNL    (2)
           STRING  "入院料のカレンダーに「１」が立っていない日は"
                                       DELIMITED BY SIZE
                   "日上限額を固定額とする"
                                       DELIMITED BY SIZE
           INTO    SPA-GMN-TANKIKBNMEIL (2)
           END-STRING
      *
           MOVE    2               TO  SPA-GMN-TANKIKBN-MAX
      *
           .
       3110-TANKIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    短期滞在手術等基本料２、３の特別計算対象区分コンボ編集処理
      *****************************************************************
       3110-TANKITAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-TANKITAIKBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-TANKITAIKBNL    (1)
           MOVE  "日上限額設定のある公費"
                                   TO  SPA-GMN-TANKITAIKBNMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-TANKITAIKBN-MAX
      *
           .
       3110-TANKITAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    負担金未発生分の回数カウント（入院）区分コンボ編集処理
      *****************************************************************
       3110-KAICNTKBNNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KAICNTKBNNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-KAICNTKBNNL    (1)
           MOVE    "回数にカウントしない"
                                   TO  SPA-GMN-KAICNTKBNNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-KAICNTKBNNL    (2)
           MOVE    "回数にカウントする"
                                   TO  SPA-GMN-KAICNTKBNNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-KAICNTKBNN-MAX
      *
           .
       3110-KAICNTKBNNLST-EXT.
           EXIT.
      *****************************************************************
      *    負担金未発生分の回数カウント（入院）対象区分コンボ編集処理
      *****************************************************************
       3110-KAICNTTAIKBNNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KAICNTTAIKBNNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-KAICNTTAIKBNNL    (1)
           MOVE    "月上限回数設定のある公費"
                                   TO  SPA-GMN-KAICNTTAIKBNNMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-KAICNTTAIKBNN-MAX
      *
           .
       3110-KAICNTTAIKBNNLST-EXT.
           EXIT.
      *****************************************************************
      *    長期併用時の特別計算（入院）区分コンボ編集処理
      *****************************************************************
       3110-CYOKEIKBNNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-CYOKEIKBNNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-CYOKEIKBNNL    (1)
           MOVE    "設定なし"
                                   TO  SPA-GMN-CYOKEIKBNNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-CYOKEIKBNNL    (2)
           STRING  "（長期の月上限額−前日までの長期負担相当額）"
                   "と（日上限額）を比較"
           INTO    SPA-GMN-CYOKEIKBNNMEIL    (2)
           END-STRING
      *
           MOVE    2               TO  SPA-GMN-CYOKEIKBNN-MAX
      *
           .
       3110-CYOKEIKBNNLST-EXT.
           EXIT.
      *****************************************************************
      *    長期併用時の特別計算（入院）対象区分コンボ編集処理
      *****************************************************************
       3110-CYOKEITAIKBNNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-CYOKEITAIKBNNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-CYOKEITAIKBNNL    (1)
           MOVE    "日上限額設定のある公費"
                                   TO  SPA-GMN-CYOKEITAIKBNNMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-CYOKEITAIKBNN-MAX
      *
           .
       3110-CYOKEITAIKBNNLST-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の日の負担金計算（入院）区分コンボ編集処理
      *****************************************************************
       3110-ZENDAYKBNNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-ZENDAYKBNNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-ZENDAYKBNNL    (1)
           STRING  "主保険の負担割合と比較し、"
                   "小さい負担割合で日の負担金計算を行う"
           INTO    SPA-GMN-ZENDAYKBNNMEIL    (1)
           END-STRING
      *
           MOVE    "1"             TO  SPA-GMN-ZENDAYKBNNL    (2)
           STRING  "全国公費の負担割合と比較し、"
                   "小さい負担割合で日の負担金計算を行う"
           INTO    SPA-GMN-ZENDAYKBNNMEIL    (2)
           END-STRING
      *
           MOVE    2               TO  SPA-GMN-ZENDAYKBNN-MAX
      *
           .
       3110-ZENDAYKBNNLST-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の日の負担金計算（入院）対象区分コンボ編集処理
      *****************************************************************
       3110-ZENDAYTAIKBNNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-ZENDAYTAIKBNNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-ZENDAYTAIKBNNL    (1)
           MOVE    "日上限額設定のある公費"
                                   TO  SPA-GMN-ZENDAYTAIKBNNMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-ZENDAYTAIKBNN-MAX
      *
           .
       3110-ZENDAYTAIKBNNLST-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の日の負担金計算２（入院）区分コンボ編集処理
      *****************************************************************
       3110-ZENDAY2KBNNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-ZENDAY2KBNNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-ZENDAY2KBNNL    (1)
           MOVE    "全国公費の日の一部負担金と比較しない"
                                   TO  SPA-GMN-ZENDAY2KBNNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-ZENDAY2KBNNL    (2)
           MOVE    "全国公費の日の一部負担金と比較する"
                                   TO  SPA-GMN-ZENDAY2KBNNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-ZENDAY2KBNN-MAX
      *
           .
       3110-ZENDAY2KBNNLST-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の日の負担金計算２（入院）対象区分コンボ編集処理
      *****************************************************************
       3110-ZENDAY2TAIKBNNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-ZENDAY2TAIKBNNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-ZENDAY2TAIKBNNL    (1)
           MOVE    "日上限額設定のある公費"
                                   TO  SPA-GMN-ZENDAY2TAIKBNNMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-ZENDAY2TAIKBNN-MAX
      *
           .
       3110-ZENDAY2TAIKBNNLST-EXT.
           EXIT.
      *****************************************************************
      *    日・月上限負担金計算コンボ編集処理
      *****************************************************************
       3110-JGNFTNKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-JGNFTNKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-JGNFTNKBNL    (1)
           MOVE    "入外それぞれで上限額までの計算をする"
                                   TO  SPA-GMN-JGNFTNKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-JGNFTNKBNL    (2)
           MOVE    "入外あわせて上限額までの計算をする"
                                   TO  SPA-GMN-JGNFTNKBNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-JGNFTNKBN-MAX
      *
           .
       3110-JGNFTNKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    月途中受給者証変更時負担計算コンボ編集処理
      *****************************************************************
       3110-JYUHENKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-JYUHENKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-JYUHENKBNL    (1)
           MOVE    "変更前の負担を考慮しない"
                                   TO  SPA-GMN-JYUHENKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-JYUHENKBNL    (2)
           MOVE    "変更前の負担を考慮する"
                                   TO  SPA-GMN-JYUHENKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-JYUHENKBNL    (3)
           MOVE    "変更前の負担を考慮する（同一市町村のみ）"
                                   TO  SPA-GMN-JYUHENKBNMEIL (3)
      *
           MOVE    "3"             TO  SPA-GMN-JYUHENKBNL    (4)
           MOVE    "変更前の負担を無条件に考慮しない"
                                   TO  SPA-GMN-JYUHENKBNMEIL (4)
      *
           MOVE    "4"             TO  SPA-GMN-JYUHENKBNL    (5)
           MOVE    "変更前の負担を考慮する（保険者変更時を除く）"
                                   TO  SPA-GMN-JYUHENKBNMEIL (5)
      *
           MOVE    "5"             TO  SPA-GMN-JYUHENKBNL    (6)
           STRING  "変更前の負担を考慮する（同一市町村のみ）"
                                       DELIMITED BY SIZE
                   "（保険者変更時を除く）"
                                       DELIMITED BY SIZE
           INTO    SPA-GMN-JYUHENKBNMEIL (6)
           END-STRING
      *
           MOVE    "6"             TO  SPA-GMN-JYUHENKBNL    (7)
           MOVE
             "変更前の負担を考慮する（入院は１と同様。外来は４と同様）"
                                   TO  SPA-GMN-JYUHENKBNMEIL (7)
      *
           MOVE    "7"             TO  SPA-GMN-JYUHENKBNL    (8)
           STRING  "変更前の負担を考慮しない（保険者変更＋"
                                       DELIMITED BY SIZE
                   "受給者証未変更時を含む）"
                                       DELIMITED BY SIZE
           INTO    SPA-GMN-JYUHENKBNMEIL (8)
           END-STRING
      *
           MOVE    8               TO  SPA-GMN-JYUHENKBN-MAX
      *
           .
       3110-JYUHENKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    患者登録−所得者情報−月上限額入力コンボ編集処理
      *****************************************************************
       3110-PTJGNKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-PTJGNKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-PTJGNKBNL    (1)
           MOVE    "設定なし"
                                   TO  SPA-GMN-PTJGNKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-PTJGNKBNL    (2)
           MOVE    "外来上限額・入院上限額を入力する"
                                   TO  SPA-GMN-PTJGNKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-PTJGNKBNL    (3)
           MOVE    "入外上限額・他一部負担累計を入力する"
                                   TO  SPA-GMN-PTJGNKBNMEIL (3)
      *
           MOVE    "3"             TO  SPA-GMN-PTJGNKBNL    (4)
           STRING  "入外上限額・他一部負担累計を入力する"
                   "（難病・小児慢性適用区分対応）"
           INTO    SPA-GMN-PTJGNKBNMEIL (4)
           END-STRING
      *
           MOVE    4               TO  SPA-GMN-PTJGNKBN-MAX
      *
           .
       3110-PTJGNKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    患者登録−所得者情報−月上限額入力対象区分コンボ編集処理
      *****************************************************************
       3110-PTJGNTAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-PTJGNTAIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-PTJGNTAIKBNL    (1)
           MOVE    "月上限額のある公費（患者単位）"
                                   TO  SPA-GMN-PTJGNTAIKBNMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-PTJGNTAIKBN-MAX
      *
           .
       3110-PTJGNTAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    医療費負担金計算コンボ編集処理
      *****************************************************************
       3110-IRYOKEIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-IRYOKEIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-IRYOKEIKBNL    (1)
           MOVE    "設定なし"
                                   TO  SPA-GMN-IRYOKEIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-IRYOKEIKBNL    (2)
           MOVE    "＜未満＞患者負担なし＜以上＞患者負担あり（全額）"
                                   TO  SPA-GMN-IRYOKEIKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-IRYOKEIKBNL    (3)
           STRING  "＜未満＞患者負担なし＜以上＞患者負担あり（全額）"
                                       DELIMITED BY SIZE
                   "【点数対応】"
                                       DELIMITED BY SIZE
           INTO    SPA-GMN-IRYOKEIKBNMEIL (3)
           END-STRING
      *
           MOVE    "3"             TO  SPA-GMN-IRYOKEIKBNL    (4)
           STRING  "＜未満＞通常計算＜以上＞患者負担あり（全額）"
                                       DELIMITED BY SIZE
                   "【設定額 21,000円】"
                                       DELIMITED BY SIZE
           INTO    SPA-GMN-IRYOKEIKBNMEIL (4)
           END-STRING
      *
           MOVE    "4"             TO  SPA-GMN-IRYOKEIKBNL    (5)
           STRING  "＜未満＞通常計算＜以上＞患者負担あり（全額）"
                                       DELIMITED BY SIZE
                   "【設定額 20,000円】"
                                       DELIMITED BY SIZE
           INTO    SPA-GMN-IRYOKEIKBNMEIL (5)
           END-STRING
      *
           MOVE    5               TO  SPA-GMN-IRYOKEIKBN-MAX
      *
           .
       3110-IRYOKEIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    医療費負担金計算対象区分コンボ編集処理
      *****************************************************************
       3110-IRYOKEITAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-IRYOKEITAIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-IRYOKEITAIKBNL    (1)
           MOVE    "該当公費対象者全員"
                                   TO  SPA-GMN-IRYOKEITAIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-IRYOKEITAIKBNL    (2)
           MOVE    "低所得・低年金の方のみ"
                                   TO  SPA-GMN-IRYOKEITAIKBNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-IRYOKEITAIKBN-MAX
      *
           .
       3110-IRYOKEITAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    食事費負担金計算コンボ編集処理
      *****************************************************************
       3110-SKJKEIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-SKJKEIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-SKJKEIKBNL    (1)
           MOVE    "設定なし"
                                   TO  SPA-GMN-SKJKEIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-SKJKEIKBNL    (2)
           MOVE    "＜未満＞患者負担なし＜以上＞患者負担あり（全額）"
                                   TO  SPA-GMN-SKJKEIKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-SKJKEIKBNL    (3)
           MOVE
           "＜未満＞患者負担あり（半額）＜以上＞患者負担あり（全額）"
                                   TO  SPA-GMN-SKJKEIKBNMEIL (3)
      *
           MOVE    3               TO  SPA-GMN-SKJKEIKBN-MAX
      *
           .
       3110-SKJKEIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    食事費負担金計算対象区分コンボ編集処理
      *****************************************************************
       3110-SKJKEITAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-SKJKEITAIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-SKJKEITAIKBNL    (1)
           MOVE    "該当公費対象者全員"
                                   TO  SPA-GMN-SKJKEITAIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-SKJKEITAIKBNL    (2)
           MOVE    "低所得・低年金の方のみ"
                                   TO  SPA-GMN-SKJKEITAIKBNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-SKJKEITAIKBN-MAX
      *
           .
       3110-SKJKEITAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    設定額を助成する負担金計算コンボ編集処理
      *****************************************************************
       3110-KINJYOKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KINJYOKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-KINJYOKBNL    (1)
           MOVE    "設定なし"
                                   TO  SPA-GMN-KINJYOKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-KINJYOKBNL    (2)
           MOVE    "設定額を助成する（設定額超の金額を患者負担とする）"
                                   TO  SPA-GMN-KINJYOKBNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-KINJYOKBN-MAX
      *
           .
       3110-KINJYOKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    設定額を助成する負担金計算対象区分コンボ編集処理
      *****************************************************************
       3110-KINJYOTAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KINJYOTAIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-KINJYOTAIKBNL    (1)
           MOVE    "該当公費対象者全員"
                                   TO  SPA-GMN-KINJYOTAIKBNMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-KINJYOTAIKBN-MAX
      *
           .
       3110-KINJYOTAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    「月−院内−上限額」参照条件（外来）コンボ編集処理
      *****************************************************************
       3110-INNAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-INNAIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-INNAIKBNL    (1)
           MOVE    "「院内」分のみの場合"
                                   TO  SPA-GMN-INNAIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-INNAIKBNL    (2)
           MOVE    "「院内」分のみで、該当月に処方がある場合"
                                   TO  SPA-GMN-INNAIKBNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-INNAIKBN-MAX
      *
           .
       3110-INNAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    「月−院内−上限額」参照条件（外来）対象区分コンボ編集処理
      *****************************************************************
       3110-INNAITAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-INNAITAIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-INNAITAIKBNL    (1)
           MOVE    "該当公費対象者全員"
                                   TO  SPA-GMN-INNAITAIKBNMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-INNAITAIKBN-MAX
      *
           .
       3110-INNAITAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    自立支援医療の特別計算コンボ編集処理
      *****************************************************************
       3110-JIRITUKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-JIRITUKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-JIRITUKBNL    (1)
           MOVE    "設定なし"
                                   TO  SPA-GMN-JIRITUKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-JIRITUKBNL    (2)
           STRING  "自立支援の一部負担金は"
                                       DELIMITED BY SIZE
                   "自立支援医療費助成公費の上限額で計算する"
                                       DELIMITED BY SIZE
           INTO    SPA-GMN-JIRITUKBNMEIL (2)
           END-STRING
      *
           MOVE    2               TO  SPA-GMN-JIRITUKBN-MAX
      *
           .
       3110-JIRITUKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    自立支援医療の特別計算対象区分コンボ編集処理
      *****************************************************************
       3110-JIRITUTAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-JIRITUTAIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-JIRITUTAIKBNL    (1)
           MOVE    "該当公費対象者全員"
                                   TO  SPA-GMN-JIRITUTAIKBNMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-JIRITUTAIKBN-MAX
      *
           .
       3110-JIRITUTAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    食事療養付加設定コンボ編集処理
      *****************************************************************
       3110-SKJFUKAKBNLST-SEC      SECTION.
      *
           INITIALIZE              SPA-GMN-SKJFUKAKBNLST-G
      *
           MOVE    "0"         TO  SPA-GMN-SKJFUKAKBNL    (1)
           MOVE    "設定なし"
                               TO  SPA-GMN-SKJFUKAKBNMEIL (1)
      *
           MOVE    "1"         TO  SPA-GMN-SKJFUKAKBNL    (2)
           MOVE    "患者負担なし"
                               TO  SPA-GMN-SKJFUKAKBNMEIL (2)
      *
           MOVE    "2"         TO  SPA-GMN-SKJFUKAKBNL    (3)
           MOVE    "患者負担あり（半額）"
                               TO  SPA-GMN-SKJFUKAKBNMEIL (3)
      *
           MOVE    "3"         TO  SPA-GMN-SKJFUKAKBNL    (4)
           MOVE    "患者負担あり（全額）"
                               TO  SPA-GMN-SKJFUKAKBNMEIL (4)
      *
           MOVE    "4"         TO  SPA-GMN-SKJFUKAKBNL    (5)
           MOVE    "患者負担あり（他公費助成後の半額）"
                               TO  SPA-GMN-SKJFUKAKBNMEIL (5)
      *
           MOVE    5           TO  SPA-GMN-SKJFUKAKBN-MAX
      *
           .
       3110-SKJFUKAKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    食事療養付加設定対象区分本人コンボ編集処理
      *****************************************************************
       3110-SKJFUKATAIKBNHONLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-SKJFUKATAIKBNHONLST-G
      *
           MOVE    "0"             TO  SPA-GMN-SKJFUKATAIKBNHONL    (1)
           MOVE    "本人タブ設定"
                                   TO  SPA-GMN-SKJFUKATAIKBNHONMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-SKJFUKATAIKBNHON-MAX
      *
           .
       3110-SKJFUKATAIKBNHONLST-EXT.
           EXIT.
      *****************************************************************
      *    食事療養付加設定対象区分低所得コンボ編集処理
      *****************************************************************
       3110-SKJFUKATAIKBNTSYLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-SKJFUKATAIKBNTSYLST-G
      *
           MOVE    "0"             TO  SPA-GMN-SKJFUKATAIKBNTSYL    (1)
           MOVE    "低所得タブ設定"
                                   TO  SPA-GMN-SKJFUKATAIKBNTSYMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-SKJFUKATAIKBNTSY-MAX
      *
           .
       3110-SKJFUKATAIKBNTSYLST-EXT.
           EXIT.
      *****************************************************************
      *    食事療養付加設定対象区分低年金コンボ編集処理
      *****************************************************************
       3110-SKJFUKATAIKBNTNKLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-SKJFUKATAIKBNTNKLST-G
      *
           MOVE    "0"             TO  SPA-GMN-SKJFUKATAIKBNTNKL    (1)
           MOVE    "低年金タブ設定"
                                   TO  SPA-GMN-SKJFUKATAIKBNTNKMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-SKJFUKATAIKBNTNK-MAX
      *
           .
       3110-SKJFUKATAIKBNTNKLST-EXT.
           EXIT.
      *****************************************************************
      *    共通対象区分コンボ編集処理
      *****************************************************************
       3110-COMMONTAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-COMMONTAIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-COMMONTAIKBNL    (1)
           MOVE    "レセプト請求分全て" 
                                   TO  SPA-GMN-COMMONTAIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-COMMONTAIKBNL    (2)
           MOVE    "国保・広域（請求分内）"
                                   TO  SPA-GMN-COMMONTAIKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-COMMONTAIKBNL    (3)
           MOVE    "社保（請求分内）"
                                   TO  SPA-GMN-COMMONTAIKBNMEIL (3)
      *
           MOVE    "3"             TO  SPA-GMN-COMMONTAIKBNL    (4)
           MOVE    "国保（請求分内）"
                                   TO  SPA-GMN-COMMONTAIKBNMEIL (4)
      *
           MOVE    "4"             TO  SPA-GMN-COMMONTAIKBNL    (5)
           MOVE    "広域（請求分内）"
                                   TO  SPA-GMN-COMMONTAIKBNMEIL (5)
      *
           MOVE    5               TO  SPA-GMN-COMMONTAIKBN-MAX
      *
           .
       3110-COMMONTAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    請求書公費再掲欄記載対象区分コンボ編集処理
      *****************************************************************
       3110-SAIKEITAIKBNLST-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-SAIKEITAIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-SAIKEITAIKBNL    (1)
           MOVE    "国保・広域（請求分内）"
                                   TO  SPA-GMN-SAIKEITAIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-SAIKEITAIKBNL    (2)
           MOVE    "国保（請求分内）"
                                   TO  SPA-GMN-SAIKEITAIKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-SAIKEITAIKBNL    (3)
           MOVE    "広域（請求分内）"
                                   TO  SPA-GMN-SAIKEITAIKBNMEIL (3)
      *
           MOVE    3               TO  SPA-GMN-SAIKEITAIKBN-MAX
      *
           .
       3110-SAIKEITAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    月途中受給者証変更時負担計算対象区分コンボ編集処理
      *****************************************************************
       3110-JYUHENTAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-JYUHENTAIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-JYUHENTAIKBNL    (1)
           MOVE    "同一保険番号"  TO  SPA-GMN-JYUHENTAIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-JYUHENTAIKBNL    (2)
           MOVE    "同一法別番号"  TO SPA-GMN-JYUHENTAIKBNMEIL  (2)
      *
           MOVE    "A"             TO  SPA-GMN-JYUHENTAIKBNL    (3)
           MOVE    "グループ（Ａ）設定"
                                   TO SPA-GMN-JYUHENTAIKBNMEIL  (3)
      *
           MOVE    "B"             TO  SPA-GMN-JYUHENTAIKBNL    (4)
           MOVE    "グループ（Ｂ）設定"
                                   TO SPA-GMN-JYUHENTAIKBNMEIL  (4)
      *
           MOVE    "C"             TO  SPA-GMN-JYUHENTAIKBNL    (5)
           MOVE    "グループ（Ｃ）設定"
                                   TO SPA-GMN-JYUHENTAIKBNMEIL  (5)
      *
           MOVE    "D"             TO  SPA-GMN-JYUHENTAIKBNL    (6)
           MOVE    "グループ（Ｄ）設定"
                                   TO SPA-GMN-JYUHENTAIKBNMEIL  (6)
      *
           MOVE    "E"             TO  SPA-GMN-JYUHENTAIKBNL    (7)
           MOVE    "グループ（Ｅ）設定"
                                   TO SPA-GMN-JYUHENTAIKBNMEIL  (7)
      *
           MOVE    "F"             TO  SPA-GMN-JYUHENTAIKBNL    (8)
           MOVE    "グループ（Ｆ）設定"
                                   TO SPA-GMN-JYUHENTAIKBNMEIL  (8)
      *
           MOVE    "G"             TO  SPA-GMN-JYUHENTAIKBNL    (9)
           MOVE    "グループ（Ｇ）設定"
                                   TO SPA-GMN-JYUHENTAIKBNMEIL  (9)
      *
           MOVE    "H"             TO  SPA-GMN-JYUHENTAIKBNL    (10)
           MOVE    "グループ（Ｈ）設定"
                                   TO SPA-GMN-JYUHENTAIKBNMEIL  (10)
      *
           MOVE    "I"             TO  SPA-GMN-JYUHENTAIKBNL    (11)
           MOVE    "グループ（Ｉ）設定"
                                   TO SPA-GMN-JYUHENTAIKBNMEIL  (11)
      *
           MOVE    "J"             TO  SPA-GMN-JYUHENTAIKBNL    (12)
           MOVE    "グループ（Ｊ）設定"
                                   TO SPA-GMN-JYUHENTAIKBNMEIL  (12)
      *
           MOVE    12              TO  SPA-GMN-JYUHENTAIKBN-MAX
      *
           .
       3110-JYUHENTAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の特別計算２（外来）コンボ編集処理
      *****************************************************************
       3110-ZENKEI2KBNGLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-ZENKEI2KBNGLST-G
      *
           MOVE  "0"               TO  SPA-GMN-ZENKEI2KBNGL    (1)
           MOVE  "設定なし"
                                   TO  SPA-GMN-ZENKEI2KBNGMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-ZENKEI2KBNGL    (2)
           STRING  "全国公費が月上限額に達した日は"
                                       DELIMITED BY SIZE
                   "レセプト一部負担金の特殊計算を行う"
                                       DELIMITED BY SIZE
           INTO    SPA-GMN-ZENKEI2KBNGMEIL (2)
           END-STRING
      *
      *
           MOVE    2               TO  SPA-GMN-ZENKEI2KBNG-MAX
      *
           .
       3110-ZENKEI2KBNGLST-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の特別計算２（外来）対象区分コンボ編集処理
      *****************************************************************
       3110-ZENKEI2TAIKBNGLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-ZENKEI2TAIKBNGLST-G
      *
           MOVE  "0"               TO  SPA-GMN-ZENKEI2TAIKBNGL    (1)
           MOVE  "該当公費対象者全員"
                                   TO  SPA-GMN-ZENKEI2TAIKBNGMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-ZENKEI2TAIKBNGL    (2)
           MOVE  "国保・広域"
                                   TO  SPA-GMN-ZENKEI2TAIKBNGMEIL (2)
      *
           MOVE  "2"               TO  SPA-GMN-ZENKEI2TAIKBNGL    (3)
           MOVE  "社保"
                                   TO  SPA-GMN-ZENKEI2TAIKBNGMEIL (3)
      *
      *
           MOVE    3               TO  SPA-GMN-ZENKEI2TAIKBNG-MAX
      *
           .
       3110-ZENKEI2TAIKBNGLST-EXT.
           EXIT.
      *****************************************************************
      *    初診時の負担金計算（外来）コンボ編集処理
      *****************************************************************
       3110-SYOKEI2KBNGLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-SYOKEI2KBNGLST-G
      *
           MOVE  "0"               TO  SPA-GMN-SYOKEI2KBNGL    (1)
           MOVE  "通常計算"
                                   TO  SPA-GMN-SYOKEI2KBNGMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-SYOKEI2KBNGL    (2)
           STRING  "初診時一部負担金（５８０円）を"
                                       DELIMITED BY SIZE
                   "差し引いた金額を一部負担金とする"
                                       DELIMITED BY SIZE
           INTO    SPA-GMN-SYOKEI2KBNGMEIL (2)
           END-STRING
      *
      *
           MOVE    2               TO  SPA-GMN-SYOKEI2KBNG-MAX
      *
           .
       3110-SYOKEI2KBNGLST-EXT.
           EXIT.
      *****************************************************************
      *    初診時の負担金計算（外来）対象区分コンボ編集処理
      *****************************************************************
       3110-SYOKEI2TAIKBNGLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-SYOKEI2TAIKBNGLST-G
      *
           MOVE  "0"               TO  SPA-GMN-SYOKEI2TAIKBNGL    (1)
           MOVE  "患者負担のある公費"
                                   TO  SPA-GMN-SYOKEI2TAIKBNGMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-SYOKEI2TAIKBNG-MAX
      *
           .
       3110-SYOKEI2TAIKBNGLST-EXT.
           EXIT.
      *****************************************************************
      *    初診時の負担金計算（入院）コンボ編集処理
      *****************************************************************
       3110-SYOKEI2KBNNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-SYOKEI2KBNNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-SYOKEI2KBNNL    (1)
           MOVE  "通常計算"
                                   TO  SPA-GMN-SYOKEI2KBNNMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-SYOKEI2KBNNL    (2)
           STRING  "初診時一部負担金（５８０円）を"
                                       DELIMITED BY SIZE
                   "差し引いた金額を一部負担金とする"
                                       DELIMITED BY SIZE
           INTO    SPA-GMN-SYOKEI2KBNNMEIL (2)
           END-STRING
      *
      *
           MOVE    2               TO  SPA-GMN-SYOKEI2KBNN-MAX
      *
           .
       3110-SYOKEI2KBNNLST-EXT.
           EXIT.
      *****************************************************************
      *    初診時の負担金計算（入院）対象区分コンボ編集処理
      *****************************************************************
       3110-SYOKEI2TAIKBNNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-SYOKEI2TAIKBNNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-SYOKEI2TAIKBNNL    (1)
           MOVE  "患者負担のある公費"
                                   TO  SPA-GMN-SYOKEI2TAIKBNNMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-SYOKEI2TAIKBNN-MAX
      *
           .
       3110-SYOKEI2TAIKBNNLST-EXT.
           EXIT.
      *****************************************************************
      *    限度額認定証提示が無い場合の特別計算（高齢者）コンボ編集処理
      *****************************************************************
       3110-NINNASIKBNKLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-NINNASIKBNKLST-G
      *
           MOVE  "0"               TO  SPA-GMN-NINNASIKBNKL    (1)
           MOVE  "設定なし"
                                   TO  SPA-GMN-NINNASIKBNKMEIL (1)
      *
           MOVE  "2"               TO  SPA-GMN-NINNASIKBNKL    (2)
           STRING  "現役並み�気侶郛絽続枋兇粒曚魎擬塢蘆瓦�"
                                       DELIMITED BY SIZE
                   "加算（高額４回目適用）"
                                       DELIMITED BY SIZE
           INTO    SPA-GMN-NINNASIKBNKMEIL (2)
           END-STRING
      *
      *
           MOVE    2               TO  SPA-GMN-NINNASIKBNK-MAX
      *
           .
       3110-NINNASIKBNKLST-EXT.
           EXIT.
      *****************************************************************
      *    限度額認定証提示が無い場合の特別計算（高齢者）対象区分コンボ編集処理
      *****************************************************************
       3110-NINNASITAIKBNKLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-NINNASITAIKBNKLST-G
      *
           MOVE  "0"               TO  SPA-GMN-NINNASITAIKBNKL    (1)
           MOVE  "該当公費対象者全員"
                                   TO  SPA-GMN-NINNASITAIKBNKMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-NINNASITAIKBNKL    (2)
           MOVE  "社保・国保組合"
                                   TO  SPA-GMN-NINNASITAIKBNKMEIL (2)
      *
           MOVE  "2"               TO  SPA-GMN-NINNASITAIKBNKL    (3)
           MOVE  "社保"
                                   TO  SPA-GMN-NINNASITAIKBNKMEIL (3)
      *
           MOVE  "3"               TO  SPA-GMN-NINNASITAIKBNKL    (4)
           MOVE  "国保組合"
                                   TO  SPA-GMN-NINNASITAIKBNKMEIL (4)
      *
           MOVE    4               TO  SPA-GMN-NINNASITAIKBNK-MAX
      *
           .
       3110-NINNASITAIKBNKLST-EXT.
           EXIT.
      *****************************************************************
      *    マル長負担金計算処理コンボ編集処理
      *****************************************************************
       3110-CYOKIFTNKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-CYOKIFTNKBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-CYOKIFTNKBNL    (1)
           MOVE  "長期の月上限額まで患者徴収する計算"
                                   TO  SPA-GMN-CYOKIFTNKBNMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-CYOKIFTNKBNL    (2)
           STRING  "（長期の月上限額）−"
                                       DELIMITED BY SIZE
                   "（主＋公＋長の主保険負担相当額）で計算"
                                       DELIMITED BY SIZE
           INTO    SPA-GMN-CYOKIFTNKBNMEIL (2)
           END-STRING
      *
      *
           MOVE    2               TO  SPA-GMN-CYOKIFTNKBN-MAX
      *
           .
       3110-CYOKIFTNKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    マル長負担金計算処理対象区分コンボ編集処理
      *****************************************************************
       3110-CYOKIFTNTAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-CYOKIFTNTAIKBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-CYOKIFTNTAIKBNL    (1)
           MOVE  "長期対象者全員"
                                   TO  SPA-GMN-CYOKIFTNTAIKBNMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-CYOKIFTNTAIKBN-MAX
      *
           .
       3110-CYOKIFTNTAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    マル長レセプト記載処理区分コンボ編集処理
      *****************************************************************
       3110-CYOKIKSIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-CYOKIKSIKBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-CYOKIKSIKBNL    (1)
           MOVE  "長期の月上限額を上限とする記載"
                                   TO  SPA-GMN-CYOKIKSIKBNMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-CYOKIKSIKBNL    (2)
           STRING  "患者徴収額と主＋公＋長"
                                       DELIMITED BY SIZE
                   "の公費負担額の合算額を記載"
                                       DELIMITED BY SIZE
           INTO    SPA-GMN-CYOKIKSIKBNMEIL (2)
           END-STRING
      *
      *
           MOVE    2               TO  SPA-GMN-CYOKIKSIKBN-MAX
      *
           .
       3110-CYOKIKSIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    マル長レセプト記載処理対象区分コンボ編集処理
      *****************************************************************
       3110-CYOKIKSITAIKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-CYOKIKSITAIKBNLST-G
      *
           MOVE  "0"               TO  SPA-GMN-CYOKIKSITAIKBNL    (1)
           MOVE  "長期レセプト全て"
                                   TO  SPA-GMN-CYOKIKSITAIKBNMEIL (1)
      *
           MOVE  "1"               TO  SPA-GMN-CYOKIKSITAIKBNL    (2)
           MOVE  "国保・広域"
                                   TO  SPA-GMN-CYOKIKSITAIKBNMEIL (2)
      *
           MOVE  "2"               TO  SPA-GMN-CYOKIKSITAIKBNL    (3)
           MOVE  "社保"
                                   TO  SPA-GMN-CYOKIKSITAIKBNMEIL (3)
      *
           MOVE  "3"               TO  SPA-GMN-CYOKIKSITAIKBNL    (4)
           MOVE  "国保"
                                   TO  SPA-GMN-CYOKIKSITAIKBNMEIL (4)
      *
           MOVE  "4"               TO  SPA-GMN-CYOKIKSITAIKBNL    (5)
           MOVE  "広域"
                                   TO  SPA-GMN-CYOKIKSITAIKBNMEIL (5)
      *
           MOVE    5               TO  SPA-GMN-CYOKIKSITAIKBN-MAX
      *
           .
       3110-CYOKIKSITAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    公費の種類コンボ編集処理
      *****************************************************************
       3110-KOHNUMLST-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-KOHNUMLST-G
           MOVE    0               TO  IDX0
           MOVE    LOW-VALUE       TO  KEY-HKNNUM
      *
           MOVE    SPA-NAI-YUKOSTYMD   TO  SEL-STYMD
           MOVE    SPA-NAI-YUKOEDYMD   TO  SEL-EDYMD
           PERFORM 900-HKNNUM-KEY9-SEL-SEC
      *
           PERFORM UNTIL ( FLG-HKNNUM  NOT =   ZERO )
                    OR   ( IDX0            >   200 )
      *
               EVALUATE    HKN-HKNNUM
               WHEN  "100"    THRU        "899"
               WHEN  "972"
      *
                   IF    ( HKN-HKNNUM  NOT =   KEY-HKNNUM )
      *
                       COMPUTE IDX0 =  IDX0 +  1
      *
                       MOVE    HKN-HKNNUM  TO  SPA-GMN-KOHNUML (IDX0)
                       MOVE    HKN-SEIDONAME
                                           TO  SPA-GMN-KOHNUMMEIL (IDX0)
      *
                       MOVE    HKN-HKNNUM  TO  KEY-HKNNUM
                   END-IF
      *
               END-EVALUATE
      *
               PERFORM 900-HKNNUM-KEY9-FET-SEC
      *
           END-PERFORM
      *
           MOVE    IDX0            TO  SPA-GMN-KOHNUM-MAX
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3110-KOHNUMLST-EXT.
           EXIT.
      *****************************************************************
      *    公費付加情報リスト編集処理
      *****************************************************************
       3110-KOHLST-EDIT-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KOHLST-AREA
                                       SPA-NAI-KOHLST-AREA
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "2010"          TO  SYS-KANRICD
           MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-2010-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( SPA-GMN-KOHLST-MAX  >   CONST-KOHLST-MAX )
      *
               COMPUTE SPA-GMN-KOHLST-MAX
                   =   SPA-GMN-KOHLST-MAX  +   1
      *
               MOVE    SPA-GMN-KOHLST-MAX
                                           TO  IDX-MAX
               MOVE    SYS-2010-KBNCD      TO  SPA-GMN-TKKOHNUM(IDX-MAX)
               MOVE    SYS-2010-KBNCD      TO  SEL-HKNNUM
               MOVE    SYS-2010-STYUKYMD   TO  SEL-STYMD
               MOVE    SYS-2010-EDYUKYMD   TO  SEL-EDYMD
               PERFORM 900-HKNNUM-KEY8-SEL-SEC
               MOVE    HKN-SEIDONAME
                                       TO  SPA-GMN-TKKOHNAME(IDX-MAX)
               MOVE    HKN-HON-NYUSKJRYYKBN
                                       TO  SPA-NAI-TKSKJKBNHON(IDX-MAX)
               MOVE    HKN-TSY-NYUSKJRYYKBN
                                       TO  SPA-NAI-TKSKJKBNTSY(IDX-MAX)
               MOVE    HKN-TNK-NYUSKJRYYKBN
                                       TO  SPA-NAI-TKSKJKBNTNK(IDX-MAX)
               MOVE    SYS-2010-STYUKYMD
                                       TO  SPA-NAI-TKYUKOSTYMD(IDX-MAX)
               MOVE    SYS-2010-STYUKYMD
                                       TO  WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TKYUKOSTYMD(IDX-MAX)
      *
               MOVE    SYS-2010-EDYUKYMD
                                       TO  SPA-NAI-TKYUKOEDYMD(IDX-MAX)
               MOVE    SYS-2010-EDYUKYMD
                                       TO  WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TKYUKOEDYMD(IDX-MAX)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-2010-REC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3110-KOHLST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       312-SPASET-SEC              SECTION.
      *
           MOVE    ZERO            TO  IDX-LST
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-KOHLST-MAX )
                    OR   ( IDX-LST >   ZERO )
      *
               IF    ( SPA-GMN-TKKOHNUM (IDX0)
                                   =   SPA-W01-KBNCD )
                   MOVE    IDX0    TO  IDX-LST
               END-IF
      *
           END-PERFORM
      *
           IF    ( IDX-LST >   ZERO )
               MOVE    IDX-LST     TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9    TO  W65-SELNUM1
               PERFORM 4100-SELNUM1-CHK-SEC
               MOVE    101     TO  SPA-GMN-CUR
           ELSE
               MOVE    SPA-W01-KBNCD
                                   TO  WRK-CMB-CD
               PERFORM 4200-KOHNUMLST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    SPA-W01-KBNCD
                                   TO  SPA-GMN-KOHNUM-G
                   PERFORM 4102-KIHON-CHK-SEC
                   MOVE    101     TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           MOVE    ZERO            TO  SPA-GMN-PAGENO
           MOVE    SPACE           TO  SPA-ERRCD
      *
           .
       312-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF    ( SPA-WID1-FLG    =   "OK" )
               EVALUATE    SPA-WIDCD
               WHEN "2001"
                       PERFORM 421-SAKUJO-SEC
               WHEN "3001"
                       PERFORM 42021-HENKOU-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM  210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 420-CLEAR-SEC
      *    追加
           WHEN    "CLICKED"       ALSO    "B06"
               PERFORM 420-TUIKA-SEC
      *    変更
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 420-HENKOU-SEC
      *    削除
           WHEN    "CLICKED"       ALSO    "B08"
               PERFORM 420-SAKUJO-SEC
      *    基準日
           WHEN    "CLICKED"       ALSO    "B04"
               PERFORM 4101-GMN-SPA-SET-SEC
               MOVE    6           TO      SPA-GMN-CUR
      *
      *
      *    タブ切替
      *    WHEN    "CLICKED"       ALSO    "NPAGE00"
      *       MOVE    0            TO      WRK-PAGENO
      *       PERFORM 420-SWITCH-PAGE-SEC
      *    WHEN    "CLICKED"       ALSO    "NPAGE01"
      *       MOVE    1            TO      WRK-PAGENO
      *       PERFORM 420-SWITCH-PAGE-SEC
      *    WHEN    "CLICKED"       ALSO    "NPAGE02"
      *       MOVE    2            TO      WRK-PAGENO
      *       PERFORM 420-SWITCH-PAGE-SEC
      *    WHEN    "CLICKED"       ALSO    "NPAGE03"
      *       MOVE    3            TO      WRK-PAGENO
      *       PERFORM 420-SWITCH-PAGE-SEC
           WHEN    "CLICKED"       ALSO    "B09"
              COMPUTE WRK-PAGENO
                  =   SPA-GMN-PAGENO   +   1
              PERFORM 420-SWITCH-PAGE-SEC
      *
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    基準日
           WHEN    ANY             ALSO    "KJNYMD"
               PERFORM 4100-KJNYMD-CHK-SEC
      *    選択番号１
           WHEN    "ACTIVATE"      ALSO    "SELNUM1"
               PERFORM 4100-SELNUM1-CHK-SEC
      *    選択番号
           WHEN    "ACTIVATE"      ALSO    "SELNUM2"
               PERFORM 4100-SELNUM2-CHK-SEC
      *    公費リスト
           WHEN    ANY             ALSO    "KOHLST"
               PERFORM 4100-KOHLST-CHK-SEC
      *    履歴リスト
           WHEN    ANY             ALSO    "RIREKILST"
               PERFORM 4100-RIREKILST-CHK-SEC
      *    タブ切替
           WHEN    "SWITCH"        ALSO    "nb1"
               MOVE    W65-NB1-PAGENO  TO  WRK-PAGENO
               PERFORM 420-SWITCH-PAGE-SEC
           WHEN    OTHER
      *
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           MOVE    ZERO        TO      SPA-GMN-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       411-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4103-KIHON-CHK-SEC
      *
           .
      *
       411-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    W65-KOHNUM          TO  SPA-GMN-KOHNUM-G
           MOVE    W65-JNISSUKBN       TO  SPA-GMN-JNISSUKBN-G
           MOVE    W65-JNISSUTAIKBN    TO  SPA-GMN-JNISSUTAIKBN-G
           MOVE    W65-TOTALTENKBN     TO  SPA-GMN-TOTALTENKBN-G
           MOVE    W65-TOTALTENTAIKBN  TO  SPA-GMN-TOTALTENTAIKBN-G
           MOVE    W65-SYUKEIKBN       TO  SPA-GMN-SYUKEIKBN-G
           MOVE    W65-SYUKEITAIKBN    TO  SPA-GMN-SYUKEITAIKBN-G
           MOVE    W65-FTNKEIKBNG      TO  SPA-GMN-FTNKEIKBNG-G
           MOVE    W65-FTNKEITAIKBNG   TO  SPA-GMN-FTNKEITAIKBNG-G
           MOVE    W65-FTNKEIKBNN      TO  SPA-GMN-FTNKEIKBNN-G
           MOVE    W65-FTNKEITAIKBNN   TO  SPA-GMN-FTNKEITAIKBNN-G
           MOVE    W65-FTNMONEYKBNG    TO  SPA-GMN-FTNMONEYKBNG-G
           MOVE    W65-FTNMONEYTAIKBNG TO  SPA-GMN-FTNMONEYTAIKBNG-G
           MOVE    W65-FTNMONEYKBNN    TO  SPA-GMN-FTNMONEYKBNN-G
           MOVE    W65-FTNMONEYTAIKBNN TO  SPA-GMN-FTNMONEYTAIKBNN-G
           MOVE    W65-FTNZEROKBNG     TO  SPA-GMN-FTNZEROKBNG-G
           MOVE    W65-FTNZEROTAIKBNG  TO  SPA-GMN-FTNZEROTAIKBNG-G
           MOVE    W65-FTNZEROKBNN     TO  SPA-GMN-FTNZEROKBNN-G
           MOVE    W65-FTNZEROTAIKBNN  TO  SPA-GMN-FTNZEROTAIKBNN-G
           MOVE    W65-SKJKBN          TO  SPA-GMN-SKJKBN-G
           MOVE    W65-SKJTAIKBN       TO  SPA-GMN-SKJTAIKBN-G
           MOVE    W65-OCRKBN          TO  SPA-GMN-OCRKBN-G
           MOVE    W65-OCRTAIKBN       TO  SPA-GMN-OCRTAIKBN-G
           MOVE    W65-SAIKEIKBN       TO  SPA-GMN-SAIKEIKBN-G
           MOVE    W65-SAIKEITAIKBN    TO  SPA-GMN-SAIKEITAIKBN-G
           MOVE    W65-HKNFTNKBNK      TO  SPA-GMN-HKNFTNKBNK-G
           MOVE    W65-HKNFTNTAIKBNK   TO  SPA-GMN-HKNFTNTAIKBNK-G
           MOVE    W65-HKNFTNKBNY      TO  SPA-GMN-HKNFTNKBNY-G
           MOVE    W65-HKNFTNTAIKBNY   TO  SPA-GMN-HKNFTNTAIKBNY-G
           MOVE    W65-RECEKSIKBNG     TO  SPA-GMN-RECEKSIKBNG-G
           MOVE    W65-RECEKSITAIKBNG  TO  SPA-GMN-RECEKSITAIKBNG-G
           MOVE    W65-RECEKSIKBNN     TO  SPA-GMN-RECEKSIKBNN-G
           MOVE    W65-RECEKSITAIKBNN  TO  SPA-GMN-RECEKSITAIKBNN-G
           MOVE    W65-CHKJGNKBNG      TO  SPA-GMN-CHKJGNKBNG-G
           MOVE    W65-CHKJGNTAIKBNG   TO  SPA-GMN-CHKJGNTAIKBNG-G
           MOVE    W65-CHKJGNKBNN      TO  SPA-GMN-CHKJGNKBNN-G
           MOVE    W65-CHKJGNTAIKBNN   TO  SPA-GMN-CHKJGNTAIKBNN-G
           MOVE    W65-KOHSKYKBNG      TO  SPA-GMN-KOHSKYKBNG-G
           MOVE    W65-KOHSKYTAIKBNG   TO  SPA-GMN-KOHSKYTAIKBNG-G
           MOVE    W65-KOHSKYKBNN      TO  SPA-GMN-KOHSKYKBNN-G
           MOVE    W65-KOHSKYTAIKBNN   TO  SPA-GMN-KOHSKYTAIKBNN-G
      *
           MOVE    W65-JYUHENKBN       TO  SPA-GMN-JYUHENKBN-G
           MOVE    W65-JYUHENTAIKBN    TO  SPA-GMN-JYUHENTAIKBN-G
           MOVE    W65-JGNFTNKBN       TO  SPA-GMN-JGNFTNKBN-G
           MOVE    W65-PTJGNKBN        TO  SPA-GMN-PTJGNKBN-G
           MOVE    W65-PTJGNTAIKBN     TO  SPA-GMN-PTJGNTAIKBN-G
           MOVE    W65-IRYOKEIKBNG     TO  SPA-GMN-IRYOKEIKBNG-G
           MOVE    W65-IRYOKEITAIKBNG  TO  SPA-GMN-IRYOKEITAIKBNG-G
           MOVE    W65-IRYOKEIKBNN     TO  SPA-GMN-IRYOKEIKBNN-G
           MOVE    W65-IRYOKEITAIKBNN  TO  SPA-GMN-IRYOKEITAIKBNN-G
           MOVE    W65-SKJKEIKBN       TO  SPA-GMN-SKJKEIKBN-G
           MOVE    W65-SKJKEITAIKBN    TO  SPA-GMN-SKJKEITAIKBN-G
           MOVE    W65-KINJYOKBNG       TO  SPA-GMN-KINJYOKBNG-G
           MOVE    W65-KINJYOTAIKBNG    TO  SPA-GMN-KINJYOTAIKBNG-G
           MOVE    W65-KINJYOKBNN       TO  SPA-GMN-KINJYOKBNN-G
           MOVE    W65-KINJYOTAIKBNN    TO  SPA-GMN-KINJYOTAIKBNN-G
           MOVE    W65-INNAIKBN        TO  SPA-GMN-INNAIKBN-G
           MOVE    W65-INNAITAIKBN     TO  SPA-GMN-INNAITAIKBN-G
           MOVE    W65-JIRITUKBN        TO  SPA-GMN-JIRITUKBN-G
           MOVE    W65-JIRITUTAIKBN     TO  SPA-GMN-JIRITUTAIKBN-G
           MOVE    W65-SKJRYOKBNHON    TO  SPA-GMN-SKJRYOKBNHON-G
           MOVE    W65-SKJRYOTAIKBNHON TO  SPA-GMN-SKJRYOTAIKBNHON-G
           MOVE    W65-SEIRYOKBNHON    TO  SPA-GMN-SEIRYOKBNHON-G
           MOVE    W65-SEIRYOTAIKBNHON TO  SPA-GMN-SEIRYOTAIKBNHON-G
           MOVE    W65-SEIKANKBNHON    TO  SPA-GMN-SEIKANKBNHON-G
           MOVE    W65-SEIKANTAIKBNHON TO  SPA-GMN-SEIKANTAIKBNHON-G
           MOVE    W65-SKJRYOKBNTSY    TO  SPA-GMN-SKJRYOKBNTSY-G
           MOVE    W65-SKJRYOTAIKBNTSY TO  SPA-GMN-SKJRYOTAIKBNTSY-G
           MOVE    W65-SEIRYOKBNTSY    TO  SPA-GMN-SEIRYOKBNTSY-G
           MOVE    W65-SEIRYOTAIKBNTSY TO  SPA-GMN-SEIRYOTAIKBNTSY-G
           MOVE    W65-SEIKANKBNTSY    TO  SPA-GMN-SEIKANKBNTSY-G
           MOVE    W65-SEIKANTAIKBNTSY TO  SPA-GMN-SEIKANTAIKBNTSY-G
           MOVE    W65-SKJRYOKBNTNK    TO  SPA-GMN-SKJRYOKBNTNK-G
           MOVE    W65-SKJRYOTAIKBNTNK TO  SPA-GMN-SKJRYOTAIKBNTNK-G
           MOVE    W65-SEIRYOKBNTNK    TO  SPA-GMN-SEIRYOKBNTNK-G
           MOVE    W65-SEIRYOTAIKBNTNK TO  SPA-GMN-SEIRYOTAIKBNTNK-G
           MOVE    W65-SEIKANKBNTNK    TO  SPA-GMN-SEIKANKBNTNK-G
           MOVE    W65-SEIKANTAIKBNTNK TO  SPA-GMN-SEIKANTAIKBNTNK-G
      *
           MOVE    W65-KAKKBNK         TO  SPA-GMN-KAKKBNK-G
           MOVE    W65-KAKTAIKBNK      TO  SPA-GMN-KAKTAIKBNK-G
           MOVE    W65-KAK21KBNK       TO  SPA-GMN-KAK21KBNK-G
           MOVE    W65-KAK21TAIKBNK    TO  SPA-GMN-KAK21TAIKBNK-G
           MOVE    W65-KAK22KBNK       TO  SPA-GMN-KAK22KBNK-G
           MOVE    W65-KAK22TAIKBNK    TO  SPA-GMN-KAK22TAIKBNK-G
           MOVE    W65-KAK23KBNK       TO  SPA-GMN-KAK23KBNK-G
           MOVE    W65-KAK23TAIKBNK    TO  SPA-GMN-KAK23TAIKBNK-G
           MOVE    W65-KAK31KBNK       TO  SPA-GMN-KAK31KBNK-G
           MOVE    W65-KAK31TAIKBNK    TO  SPA-GMN-KAK31TAIKBNK-G
           MOVE    W65-KAK32KBNK       TO  SPA-GMN-KAK32KBNK-G
           MOVE    W65-KAK32TAIKBNK    TO  SPA-GMN-KAK32TAIKBNK-G
           MOVE    W65-KAK41KBNK       TO  SPA-GMN-KAK41KBNK-G
           MOVE    W65-KAK41TAIKBNK    TO  SPA-GMN-KAK41TAIKBNK-G
           MOVE    W65-KAK42KBNK       TO  SPA-GMN-KAK42KBNK-G
           MOVE    W65-KAK42TAIKBNK    TO  SPA-GMN-KAK42TAIKBNK-G
           MOVE    W65-KAK43KBNK       TO  SPA-GMN-KAK43KBNK-G
           MOVE    W65-KAK43TAIKBNK    TO  SPA-GMN-KAK43TAIKBNK-G
           MOVE    W65-KAKKBNY         TO  SPA-GMN-KAKKBNY-G
           MOVE    W65-KAKTAIKBNY      TO  SPA-GMN-KAKTAIKBNY-G
           MOVE    W65-KAK21KBNY       TO  SPA-GMN-KAK21KBNY-G
           MOVE    W65-KAK21TAIKBNY    TO  SPA-GMN-KAK21TAIKBNY-G
           MOVE    W65-KAK22KBNY       TO  SPA-GMN-KAK22KBNY-G
           MOVE    W65-KAK22TAIKBNY    TO  SPA-GMN-KAK22TAIKBNY-G
           MOVE    W65-KAK23KBNY       TO  SPA-GMN-KAK23KBNY-G
           MOVE    W65-KAK23TAIKBNY    TO  SPA-GMN-KAK23TAIKBNY-G
           MOVE    W65-KAK31KBNY       TO  SPA-GMN-KAK31KBNY-G
           MOVE    W65-KAK31TAIKBNY    TO  SPA-GMN-KAK31TAIKBNY-G
           MOVE    W65-KAK32KBNY       TO  SPA-GMN-KAK32KBNY-G
           MOVE    W65-KAK32TAIKBNY    TO  SPA-GMN-KAK32TAIKBNY-G
           MOVE    W65-KAK41KBNY       TO  SPA-GMN-KAK41KBNY-G
           MOVE    W65-KAK41TAIKBNY    TO  SPA-GMN-KAK41TAIKBNY-G
           MOVE    W65-KAK42KBNY       TO  SPA-GMN-KAK42KBNY-G
           MOVE    W65-KAK42TAIKBNY    TO  SPA-GMN-KAK42TAIKBNY-G
           MOVE    W65-KAK43KBNY       TO  SPA-GMN-KAK43KBNY-G
           MOVE    W65-KAK43TAIKBNY    TO  SPA-GMN-KAK43TAIKBNY-G
           MOVE    W65-KSINGKBNKG      TO  SPA-GMN-KSINGKBNKG-G
           MOVE    W65-KSINGTAIKBNKG   TO  SPA-GMN-KSINGTAIKBNKG-G
           MOVE    W65-KSINGKBNKN      TO  SPA-GMN-KSINGKBNKN-G
           MOVE    W65-KSINGTAIKBNKN   TO  SPA-GMN-KSINGTAIKBNKN-G
           MOVE    W65-KSINGKBNYG      TO  SPA-GMN-KSINGKBNYG-G
           MOVE    W65-KSINGTAIKBNYG   TO  SPA-GMN-KSINGTAIKBNYG-G
           MOVE    W65-KSINGKBNYN      TO  SPA-GMN-KSINGKBNYN-G
           MOVE    W65-KSINGTAIKBNYN   TO  SPA-GMN-KSINGTAIKBNYN-G
           MOVE    W65-ENCHGKBNKG      TO  SPA-GMN-ENCHGKBNKG-G
           MOVE    W65-ENCHGTAIKBNKG   TO  SPA-GMN-ENCHGTAIKBNKG-G
           MOVE    W65-ENCHGKBNKN      TO  SPA-GMN-ENCHGKBNKN-G
           MOVE    W65-ENCHGTAIKBNKN   TO  SPA-GMN-ENCHGTAIKBNKN-G
           MOVE    W65-ENCHGKBNYG      TO  SPA-GMN-ENCHGKBNYG-G
           MOVE    W65-ENCHGTAIKBNYG   TO  SPA-GMN-ENCHGTAIKBNYG-G
           MOVE    W65-ENCHGKBNYN      TO  SPA-GMN-ENCHGKBNYN-G
           MOVE    W65-ENCHGTAIKBNYN   TO  SPA-GMN-ENCHGTAIKBNYN-G
           MOVE    W65-KOH2SKJKBN      TO  SPA-GMN-KOH2SKJKBN-G
           MOVE    W65-KOH2SKJTAIKBN   TO  SPA-GMN-KOH2SKJTAIKBN-G
           MOVE    W65-FTNHASUKBNG     TO  SPA-GMN-FTNHASUKBNG-G
           MOVE    W65-FTNHASUTAIKBNG  TO  SPA-GMN-FTNHASUTAIKBNG-G
           MOVE    W65-FTNHASUKBNN     TO  SPA-GMN-FTNHASUKBNN-G
           MOVE    W65-FTNHASUTAIKBNN  TO  SPA-GMN-FTNHASUTAIKBNN-G
           MOVE    W65-KAICNTKBNG      TO  SPA-GMN-KAICNTKBNG-G
           MOVE    W65-KAICNTTAIKBNG   TO  SPA-GMN-KAICNTTAIKBNG-G
           MOVE    W65-CYOKEIKBNG      TO  SPA-GMN-CYOKEIKBNG-G
           MOVE    W65-CYOKEITAIKBNG   TO  SPA-GMN-CYOKEITAIKBNG-G
           MOVE    W65-GNBKEIKBNKG     TO  SPA-GMN-GNBKEIKBNKG-G
           MOVE    W65-GNBKEITAIKBNKG  TO  SPA-GMN-GNBKEITAIKBNKG-G
           MOVE    W65-ZENKEIKBNG      TO  SPA-GMN-ZENKEIKBNG-G
           MOVE    W65-ZENKEITAIKBNG   TO  SPA-GMN-ZENKEITAIKBNG-G
           MOVE    W65-SINKEIKBNG      TO  SPA-GMN-SINKEIKBNG-G
           MOVE    W65-SINKEITAIKBNG   TO  SPA-GMN-SINKEITAIKBNG-G
           MOVE    W65-SYOKEIKBNG      TO  SPA-GMN-SYOKEIKBNG-G
           MOVE    W65-SYOKEITAIKBNG   TO  SPA-GMN-SYOKEITAIKBNG-G
           MOVE    W65-SYOKEIKBNN      TO  SPA-GMN-SYOKEIKBNN-G
           MOVE    W65-SYOKEITAIKBNN   TO  SPA-GMN-SYOKEITAIKBNN-G
           MOVE    W65-NYUKEIKBNN      TO  SPA-GMN-NYUKEIKBNN-G
           MOVE    W65-NYUKEITAIKBNN   TO  SPA-GMN-NYUKEITAIKBNN-G
           MOVE    W65-DAYKEIKBNN      TO  SPA-GMN-DAYKEIKBNN-G
           MOVE    W65-DAYKEITAIKBNN   TO  SPA-GMN-DAYKEITAIKBNN-G
           MOVE    W65-NINNASIKBN      TO  SPA-GMN-NINNASIKBN-G
           MOVE    W65-NINNASITAIKBN   TO  SPA-GMN-NINNASITAIKBN-G
           MOVE    W65-NINJYOKBN       TO  SPA-GMN-NINJYOKBN-G
           MOVE    W65-NINJYOTAIKBN    TO  SPA-GMN-NINJYOTAIKBN-G
           MOVE    W65-KOGAKKEIKBN     TO  SPA-GMN-KOGAKKEIKBN-G
           MOVE    W65-KOGAKKEITAIKBN  TO  SPA-GMN-KOGAKKEITAIKBN-G
           MOVE    W65-TANKIKBN        TO  SPA-GMN-TANKIKBN-G
           MOVE    W65-TANKITAIKBN     TO  SPA-GMN-TANKITAIKBN-G
           MOVE    W65-KAICNTKBNN        TO  SPA-GMN-KAICNTKBNN-G
           MOVE    W65-KAICNTTAIKBNN        TO  SPA-GMN-KAICNTTAIKBNN-G
           MOVE    W65-CYOKEIKBNN        TO  SPA-GMN-CYOKEIKBNN-G
           MOVE    W65-CYOKEITAIKBNN        TO  SPA-GMN-CYOKEITAIKBNN-G
           MOVE    W65-ZENDAYKBNN        TO  SPA-GMN-ZENDAYKBNN-G
           MOVE    W65-ZENDAYTAIKBNN        TO  SPA-GMN-ZENDAYTAIKBNN-G
           MOVE    W65-ZENDAY2KBNN      TO  SPA-GMN-ZENDAY2KBNN-G
           MOVE    W65-ZENDAY2TAIKBNN   TO  SPA-GMN-ZENDAY2TAIKBNN-G
           MOVE    W65-ZENKEI2KBNG      TO  SPA-GMN-ZENKEI2KBNG-G
           MOVE    W65-ZENKEI2TAIKBNG   TO  SPA-GMN-ZENKEI2TAIKBNG-G
           MOVE    W65-SYOKEI2KBNG      TO  SPA-GMN-SYOKEI2KBNG-G
           MOVE    W65-SYOKEI2TAIKBNG   TO  SPA-GMN-SYOKEI2TAIKBNG-G
           MOVE    W65-SYOKEI2KBNN      TO  SPA-GMN-SYOKEI2KBNN-G
           MOVE    W65-SYOKEI2TAIKBNN   TO  SPA-GMN-SYOKEI2TAIKBNN-G
           MOVE    W65-NINNASIKBNK      TO  SPA-GMN-NINNASIKBNK-G
           MOVE    W65-NINNASITAIKBNK   TO  SPA-GMN-NINNASITAIKBNK-G
           MOVE    W65-CYOKIFTNKBN      TO  SPA-GMN-CYOKIFTNKBN-G
           MOVE    W65-CYOKIFTNTAIKBN   TO  SPA-GMN-CYOKIFTNTAIKBN-G
           MOVE    W65-CYOKIKSIKBN      TO  SPA-GMN-CYOKIKSIKBN-G
           MOVE    W65-CYOKIKSITAIKBN   TO  SPA-GMN-CYOKIKSITAIKBN-G
      *
           MOVE    W65-YUKOSTYMD       TO  SPA-GMN-YUKOSTYMD
           MOVE    W65-YUKOEDYMD       TO  SPA-GMN-YUKOEDYMD
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
           PERFORM 41021-KIHON-CHK-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE00-KIHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE01-KIHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE02-KIHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE03-KIHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE04-KIHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE05-KIHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE06-KIHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE07-KIHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE08-KIHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE09-KIHON-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       41021-KIHON-CHK-SEC             SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SELNUM1-CHK-SEC
           END-IF
      *
           MOVE    SPA-NAI-YUKOSTYMD   TO  WRK-NAI-YUKOSTYMD
           MOVE    SPA-NAI-YUKOEDYMD   TO  WRK-NAI-YUKOEDYMD
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-YUKOSTYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-YUKOEDYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-NAI-YUKOSTYMD   NOT =   WRK-NAI-YUKOSTYMD )
            OR   ( SPA-NAI-YUKOEDYMD   NOT =   WRK-NAI-YUKOEDYMD )
               PERFORM 3110-KOHNUMLST-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KOHNUM-CHK-SEC
           END-IF
      *
           .
       41021-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4103-KIHON-CHK-SEC              SECTION.
      *
           PERFORM 41021-KIHON-CHK-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE00-KIHON-CHK-SEC
               PERFORM 4102-PAGE00-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE01-KIHON-CHK-SEC
               PERFORM 4102-PAGE01-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE02-KIHON-CHK-SEC
               PERFORM 4102-PAGE02-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE03-KIHON-CHK-SEC
               PERFORM 4102-PAGE03-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE04-KIHON-CHK-SEC
               PERFORM 4102-PAGE04-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE05-KIHON-CHK-SEC
               PERFORM 4102-PAGE05-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE06-KIHON-CHK-SEC
               PERFORM 4102-PAGE06-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE07-KIHON-CHK-SEC
               PERFORM 4102-PAGE07-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE08-KIHON-CHK-SEC
               PERFORM 4102-PAGE08-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PAGE09-KIHON-CHK-SEC
               PERFORM 4102-PAGE09-KANREN-CHK-SEC
           END-IF
      *
           .
      *
       4103-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理（０頁）
      *****************************************************************
       4102-PAGE00-KIHON-CHK-SEC       SECTION.
      *
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-JNISSUKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-JNISSUTAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-TOTALTENKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-TOTALTENTAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SYUKEIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SYUKEITAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNKEIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNKEITAIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNKEIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNKEITAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNMONEYKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNMONEYTAIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNMONEYKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNMONEYTAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNZEROKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNZEROTAIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNZEROKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-FTNZEROTAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKJKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKJTAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-OCRKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-OCRTAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SAIKEIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SAIKEITAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    0           TO  SPA-GMN-PAGENO
           END-IF
      *
           .
      *
       4102-PAGE00-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理（１頁）
      *****************************************************************
       4102-PAGE01-KIHON-CHK-SEC       SECTION.
      *
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-HKNFTNKBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-HKNFTNTAIKBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-HKNFTNKBNY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-HKNFTNTAIKBNY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-RECEKSIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-RECEKSITAIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-RECEKSIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-RECEKSITAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-CHKJGNKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-CHKJGNTAIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-CHKJGNKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-CHKJGNTAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KOHSKYKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KOHSKYTAIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KOHSKYKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KOHSKYTAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    1           TO  SPA-GMN-PAGENO
           END-IF
      *
           .
      *
       4102-PAGE01-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理（２頁）
      *****************************************************************
       4102-PAGE02-KIHON-CHK-SEC       SECTION.
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAKKBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAKTAIKBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK31KBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK31TAIKBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK32KBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK32TAIKBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK41KBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK41TAIKBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK42KBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK42TAIKBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK43KBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK43TAIKBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK21KBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK21TAIKBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK22KBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK22TAIKBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK23KBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK23TAIKBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    2           TO  SPA-GMN-PAGENO
           END-IF
      *
           .
      *
       4102-PAGE02-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理（３頁）
      *****************************************************************
       4102-PAGE03-KIHON-CHK-SEC       SECTION.
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAKKBNY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAKTAIKBNY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK31KBNY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK31TAIKBNY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK32KBNY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK32TAIKBNY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK41KBNY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK41TAIKBNY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK42KBNY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK42TAIKBNY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK43KBNY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK43TAIKBNY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK21KBNY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK21TAIKBNY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK22KBNY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK22TAIKBNY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK23KBNY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAK23TAIKBNY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    3           TO  SPA-GMN-PAGENO
           END-IF
      *
           .
      *
       4102-PAGE03-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理（４頁）
      *****************************************************************
       4102-PAGE04-KIHON-CHK-SEC       SECTION.
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KSINGKBNKG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KSINGTAIKBNKG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KSINGKBNKN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KSINGTAIKBNKN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KSINGKBNYG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KSINGTAIKBNYG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KSINGKBNYN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KSINGTAIKBNYN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-ENCHGKBNKG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-ENCHGTAIKBNKG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-ENCHGKBNKN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-ENCHGTAIKBNKN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-ENCHGKBNYG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-ENCHGTAIKBNYG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-ENCHGKBNYN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-ENCHGTAIKBNYN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KOH2SKJKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KOH2SKJTAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-FTNHASUKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-FTNHASUTAIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-FTNHASUKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-FTNHASUTAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    4           TO  SPA-GMN-PAGENO
           END-IF
      *
           .
      *
       4102-PAGE04-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理（５頁）
      *****************************************************************
       4102-PAGE05-KIHON-CHK-SEC       SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-JYUHENKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-JYUHENTAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-JGNFTNKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-PTJGNKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-PTJGNTAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-IRYOKEIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-IRYOKEITAIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-IRYOKEIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-IRYOKEITAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKJKEIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKJKEITAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KINJYOKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KINJYOTAIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KINJYOKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KINJYOTAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-INNAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-INNAITAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-JIRITUKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-JIRITUTAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    5           TO  SPA-GMN-PAGENO
           END-IF
      *
           .
      *
       4102-PAGE05-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理（６頁）
      *****************************************************************
       4102-PAGE06-KIHON-CHK-SEC       SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKJRYOKBNHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKJRYOTAIKBNHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIRYOKBNHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIRYOTAIKBNHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIKANKBNHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIKANTAIKBNHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKJRYOKBNTSY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKJRYOTAIKBNTSY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIRYOKBNTSY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIRYOTAIKBNTSY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIKANKBNTSY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIKANTAIKBNTSY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKJRYOKBNTNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKJRYOTAIKBNTNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIRYOKBNTNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIRYOTAIKBNTNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIKANKBNTNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SEIKANTAIKBNTNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    6           TO  SPA-GMN-PAGENO
           END-IF
      *
           .
      *
       4102-PAGE06-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理（７頁）
      *****************************************************************
       4102-PAGE07-KIHON-CHK-SEC       SECTION.
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAICNTKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAICNTTAIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-CYOKEIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-CYOKEITAIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-GNBKEIKBNKG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-GNBKEITAIKBNKG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-ZENKEIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-ZENKEITAIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SINKEIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SINKEITAIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SYOKEIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SYOKEITAIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SYOKEIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SYOKEITAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NYUKEIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NYUKEITAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-DAYKEIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-DAYKEITAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NINNASIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NINNASITAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NINJYOKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NINJYOTAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KOGAKKEIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KOGAKKEITAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-TANKIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-TANKITAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    7           TO  SPA-GMN-PAGENO
           END-IF
      *
           .
      *
       4102-PAGE07-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理（８頁）
      *****************************************************************
       4102-PAGE08-KIHON-CHK-SEC       SECTION.
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAICNTKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KAICNTTAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-CYOKEIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-CYOKEITAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-ZENDAYKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-ZENDAYTAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-ZENKEI2KBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-ZENKEI2TAIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-ZENDAY2KBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-ZENDAY2TAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SYOKEI2KBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SYOKEI2TAIKBNG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SYOKEI2KBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-SYOKEI2TAIKBNN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NINNASIKBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NINNASITAIKBNK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    8           TO  SPA-GMN-PAGENO
           END-IF
      *
           .
      *
       4102-PAGE08-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理（９頁）
      *****************************************************************
       4102-PAGE09-KIHON-CHK-SEC       SECTION.
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-CYOKIKSIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-CYOKIKSITAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    9           TO  SPA-GMN-PAGENO
           END-IF
      *
           .
      *
       4102-PAGE09-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理（０頁）
      *****************************************************************
       4102-PAGE00-KANREN-CHK-SEC       SECTION.
      *
      *
           .
       4102-PAGE00-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理（１頁）
      *****************************************************************
       4102-PAGE01-KANREN-CHK-SEC       SECTION.
      *
      *
           .
       4102-PAGE01-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理（２頁）
      *****************************************************************
       4102-PAGE02-KANREN-CHK-SEC       SECTION.
      *
           IF    ( SPA-GMN-KAK41KBNK       =   "0" )
               IF    ( SPA-GMN-KAK42KBNK   =   "0" )
                AND  ( SPA-GMN-KAK43KBNK   =   "0" )
                   CONTINUE
               ELSE
                   MOVE    "0018"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-KAK21KBNK       =   "0" )
               IF    ( SPA-GMN-KAK22KBNK   =   "0" )
                AND  ( SPA-GMN-KAK23KBNK   =   "0" )
                   CONTINUE
               ELSE
                   MOVE    "0019"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4102-PAGE02-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理（３頁）
      *****************************************************************
       4102-PAGE03-KANREN-CHK-SEC       SECTION.
      *
           IF    ( SPA-GMN-KAK41KBNY       =   "0" )
               IF    ( SPA-GMN-KAK42KBNY   =   "0" )
                AND  ( SPA-GMN-KAK43KBNY   =   "0" )
                   CONTINUE
               ELSE
                   MOVE    "0018"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-KAK21KBNY       =   "0" )
               IF    ( SPA-GMN-KAK22KBNY   =   "0" )
                AND  ( SPA-GMN-KAK23KBNY   =   "0" )
                   CONTINUE
               ELSE
                   MOVE    "0019"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4102-PAGE03-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理（４頁）
      *****************************************************************
       4102-PAGE04-KANREN-CHK-SEC       SECTION.
      *
      *
           .
       4102-PAGE04-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理（５頁）
      *****************************************************************
       4102-PAGE05-KANREN-CHK-SEC       SECTION.
      *
           IF    ( SPA-GMN-IRYOKEIKBNN     =   "0" )
               IF    ( SPA-GMN-SKJKEIKBN   =   "0" )
                   CONTINUE
               ELSE
                       MOVE    "0007"      TO  SPA-ERRCD
               END-IF
           ELSE
               IF    ( SPA-GMN-SKJKEIKBN   NOT =   "0" )
                   IF    ( SPA-GMN-IRYOKEITAIKBNN
                                   =   SPA-GMN-SKJKEITAIKBN )
                       CONTINUE
                   ELSE
                       MOVE    "0008"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       4102-PAGE05-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理（６頁）
      *****************************************************************
       4102-PAGE06-KANREN-CHK-SEC       SECTION.
      *
           IF    ( SPA-NAI-INPUT-MODE   =   2 )
               PERFORM VARYING IDX-LST FROM    1   BY  1
                       UNTIL ( IDX-LST >   SPA-GMN-KOHLST-MAX )
                        OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                   IF    ( SPA-NAI-SELNUM1-FLG (IDX-LST)   =   1 )
                       MOVE    SPA-NAI-TKSKJKBNHON (IDX-LST)
                                               TO  WRK-SKJKBN(1)
                       MOVE    SPA-NAI-TKSKJKBNTSY (IDX-LST)
                                               TO  WRK-SKJKBN(2)
                       MOVE    SPA-NAI-TKSKJKBNTNK (IDX-LST)
                                               TO  WRK-SKJKBN(3)
                       PERFORM 41021-PAGE06-KANREN-CHK-SEC
                   END-IF
      *
               END-PERFORM
           ELSE
               MOVE    SPA-NAI-SKJKBNHON       TO  WRK-SKJKBN(1)
               MOVE    SPA-NAI-SKJKBNTSY       TO  WRK-SKJKBN(2)
               MOVE    SPA-NAI-SKJKBNTNK       TO  WRK-SKJKBN(3)
                PERFORM 41021-PAGE06-KANREN-CHK-SEC
           END-IF
      *
           .
       4102-PAGE06-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理（６頁）
      *****************************************************************
       41021-PAGE06-KANREN-CHK-SEC       SECTION.
      *
           MOVE    ZERO            TO      FLG-ERR
           MOVE    SPACE           TO      WRK-TABNAME
      *
           PERFORM VARYING IDXE    FROM    1   BY  1
                   UNTIL ( IDXE    >       3 )
                    OR   ( FLG-ERR NOT =   ZERO )
      *
               EVALUATE    WRK-SKJKBN (IDXE)
               WHEN    "1"
                   EVALUATE    SPA-GMN-SKJRYOKBN (IDXE)
                               ALSO    SPA-GMN-SEIRYOKBN (IDXE)
                                   ALSO    SPA-GMN-SEIKANKBN (IDXE)
                   WHEN    "2" ALSO  "2" ALSO  "2"
                   WHEN    "2" ALSO  "2" ALSO  "0"
                   WHEN    "2" ALSO  "0" ALSO  "0"
                   WHEN    "2" ALSO  "2" ALSO  "3"
                   WHEN    "2" ALSO  "3" ALSO  "3"
                   WHEN    "0" ALSO  "0" ALSO  "0"
                   WHEN    "1" ALSO  "1" ALSO  "3"
                   WHEN    "1" ALSO  "3" ALSO  "3"
                   WHEN    "4" ALSO  "4" ALSO  "4"
      *
                   WHEN    "0" ALSO  "0" ALSO  "3"
                   WHEN    "0" ALSO  "3" ALSO  "0"
                   WHEN    "3" ALSO  "0" ALSO  "0"
                   WHEN    "0" ALSO  "3" ALSO  "3"
                   WHEN    "3" ALSO  "0" ALSO  "3"
                   WHEN    "3" ALSO  "3" ALSO  "0"
                   WHEN    "3" ALSO  "3" ALSO  "3"
                       CONTINUE
                   WHEN    OTHER
                       MOVE    IDXE    TO  FLG-ERR
                   END-EVALUATE
      *
               WHEN    "3"
      *
                   EVALUATE    SPA-GMN-SKJRYOKBN (IDXE)
                               ALSO    SPA-GMN-SEIRYOKBN (IDXE)
                                   ALSO    SPA-GMN-SEIKANKBN (IDXE)
                   WHEN    "0" ALSO  "0" ALSO  "3"
                   WHEN    "0" ALSO  "3" ALSO  "3"
                   WHEN    "1" ALSO  "1" ALSO  "3"
                   WHEN    "1" ALSO  "3" ALSO  "3"
                   WHEN    "0" ALSO  "0" ALSO  "0"
      *
                   WHEN    "0" ALSO  "0" ALSO  "1"
                   WHEN    "0" ALSO  "1" ALSO  "0"
                   WHEN    "1" ALSO  "0" ALSO  "0"
                   WHEN    "0" ALSO  "1" ALSO  "1"
                   WHEN    "1" ALSO  "0" ALSO  "1"
                   WHEN    "1" ALSO  "1" ALSO  "0"
                   WHEN    "1" ALSO  "1" ALSO  "1"
                       CONTINUE
                   WHEN    OTHER
                       MOVE    IDXE    TO  FLG-ERR
                   END-EVALUATE
      *
               WHEN    "0"
               WHEN    "2"
               WHEN    "4"
               WHEN    "5"
      *
                   EVALUATE    SPA-GMN-SKJRYOKBN (IDXE)
                               ALSO    SPA-GMN-SEIRYOKBN (IDXE)
                                   ALSO    SPA-GMN-SEIKANKBN (IDXE)
                   WHEN    "0" ALSO  "0" ALSO  "0"
                       CONTINUE
                   WHEN    OTHER
                       MOVE    IDXE    TO  FLG-ERR
                   END-EVALUATE
      *
               END-EVALUATE
           END-PERFORM
      *
           IF    ( FLG-ERR   NOT   =   ZERO )
      *
               MOVE    "0009"          TO  SPA-ERRCD
      *
               EVALUATE    FLG-ERR
               WHEN    1
                   MOVE    "本人タブ"      TO  WRK-TABNAME
                   MOVE    701             TO  SPA-GMN-CUR
               WHEN    2
                   MOVE    "低所得タブ"    TO  WRK-TABNAME
                   MOVE    707             TO  SPA-GMN-CUR
               WHEN    3
                   MOVE    "低年金タブ"    TO  WRK-TABNAME
                   MOVE    713             TO  SPA-GMN-CUR
               END-EVALUATE
      *
           END-IF
      *
           .
       41021-PAGE06-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理（７頁）
      *****************************************************************
       4102-PAGE07-KANREN-CHK-SEC       SECTION.
      *
      *
           .
       4102-PAGE07-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理（８頁）
      *****************************************************************
       4102-PAGE08-KANREN-CHK-SEC       SECTION.
      *
      *
           .
       4102-PAGE08-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理（９頁）
      *****************************************************************
       4102-PAGE09-KANREN-CHK-SEC       SECTION.
      *
      *
           .
       4102-PAGE09-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W65"               TO  SPA-MOTOPG
      *
      ***  MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    条件クリア処理
      *****************************************************************
       420-CLEAR-SEC               SECTION.
      *
           PERFORM 311-SPASET-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    追加処理
      *****************************************************************
       420-TUIKA-SEC               SECTION.
      *
           PERFORM 411-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4900-KANRENCHK-SEC
           END-IF
      *
      *    既存の履歴データをチェックし、期間の重複したデータが
      *    存在しないこと
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    1               TO  FLG-UPD-KBN
               PERFORM 4100-RIREKI-CHK-SEC
               IF   ( FLG-KIKAN-DABURI NOT =   ZERO )
                   MOVE    "1104"          TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4201-SYS2010-INSERT-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 311-SPASET-SEC
               IF    ( SPA-GMN-KOHLST-MAX  >=  CONST-KOHLST-MAX )
                   MOVE    "1001"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *
           .
       420-TUIKA-EXT.
           EXIT.
      *****************************************************************
      *    変更処理
      *****************************************************************
       420-HENKOU-SEC                  SECTION.
      *
           PERFORM 411-INPUT-CHK-SEC
      *
      *    選択番号が入力されていること
           IF    ( SPA-GMN-SELNUM1 =   SPACE )
               MOVE    "1105"      TO  SPA-ERRCD
               MOVE    1           TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-NAI-INPUT-MODE  =   2 )
                   PERFORM 4202-HENKOU-SEC
               ELSE
                   PERFORM 4201-HENKOU-SEC
      *
                   IF    ( SPA-ERRCD       =   SPACE )
                        PERFORM 311-SPASET-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       420-HENKOU-EXT.
           EXIT.
      *****************************************************************
      *    変更処理(通常の変更処理)
      *****************************************************************
       4201-HENKOU-SEC                 SECTION.
      *
      *    公費番号が変更されていないこと
           IF    ( SPA-GMN-KOHNUM  =   SPA-NAI-KOHNUM-ORG )
               CONTINUE
           ELSE
               MOVE    "1106"      TO  SPA-ERRCD
               MOVE    2           TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4900-KANRENCHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        有効期間の開始日と終了日が同時に変更されていないこと
               IF    ( SPA-NAI-YUKOSTYMD   NOT = SPA-NAI-YUKOSTYMD-ORG )
                AND  ( SPA-NAI-YUKOEDYMD   NOT = SPA-NAI-YUKOEDYMD-ORG )
                   MOVE    "1107"      TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        既存の履歴データをチェックし、期間の重複したデータが
      *        存在しないこと
               MOVE    2                   TO  FLG-UPD-KBN
               PERFORM 4100-RIREKI-CHK-SEC
               IF   ( FLG-KIKAN-DABURI NOT =   ZERO )
                   MOVE    "1104"          TO  SPA-ERRCD
                   MOVE    4               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4201-SYS2010-UPDATE-SEC
           END-IF
      *
           .
       4201-HENKOU-EXT.
           EXIT.
      *****************************************************************
      *    変更処理(一括変更)
      *****************************************************************
       4202-HENKOU-SEC                 SECTION.
      *
           MOVE    "3001"          TO  SPA-WIDCD
      *
           .
       4202-HENKOU-EXT.
           EXIT.
      *****************************************************************
      *    変更処理(一括変更)
      *****************************************************************
       42021-HENKOU-SEC                SECTION.
      *
           PERFORM VARYING IDX-LST     FROM    1   BY  1
                   UNTIL ( IDX-LST     >   SPA-GMN-KOHLST-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               IF  ( SPA-NAI-SELNUM1-FLG (IDX-LST) =   1 )
      *
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    "2010"          TO  SYS-KANRICD
                   MOVE    SPA-GMN-TKKOHNUM (IDX-LST)
                                           TO  SYS-KBNCD
                   MOVE    SPA-NAI-TKYUKOSTYMD (IDX-LST)
                                           TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   IF    ( FLG-SYSKANRI    =   ZERO )
                       MOVE    SYSKANRI-REC    TO  SYS-2010-REC
      *
      *                公費記載情報編集処理
                       PERFORM 4201-KOHINF-EDIT-SEC
      *
                       MOVE    SYS-2010-REC    TO  SYSKANRI-REC
                       PERFORM 800-MCNDATE-SEC
                       MOVE    SMCNDATE-YMD    TO  SYS-UPYMD
                       MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
                       MOVE    SYSKANRI-REC    TO  MCPDATA-REC
                       MOVE    "tbl_syskanri"  TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 910-DBUPDATE-SEC
                       IF   (  MCP-RC          NOT =   ZERO )
                           MOVE    "4005"      TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 311-SPASET-SEC
           END-IF
      *
      *
           .
       42021-HENKOU-EXT.
           EXIT.
      *****************************************************************
      *    公費基本情報編集処理
      *****************************************************************
       4201-SYS2010-EDIT-SEC            SECTION.
      *
           MOVE    SPACE                  TO  SYS-2010-REC
           INITIALIZE                         SYS-2010-REC
           MOVE    SPA-HOSPNUM            TO  SYS-2010-HOSPNUM
           MOVE    "2010"                 TO  SYS-2010-KANRICD
           MOVE    SPA-GMN-KOHNUM         TO  SYS-2010-KBNCD
           MOVE    SPA-NAI-YUKOSTYMD      TO  SYS-2010-STYUKYMD
           MOVE    SPA-NAI-YUKOEDYMD      TO  SYS-2010-EDYUKYMD
      *
           PERFORM 4201-KOHINF-EDIT-SEC
      *
           .
       4201-SYS2010-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    公費記載情報編集処理
      *****************************************************************
       4201-KOHINF-EDIT-SEC            SECTION.
      *
           MOVE    SPA-GMN-JNISSUKBN       TO SYS-2010-JNISSUKBN
           MOVE    SPA-GMN-JNISSUTAIKBN    TO SYS-2010-JNISSU-TAIKBN
           MOVE    SPA-GMN-TOTALTENKBN     TO SYS-2010-TOTALTENKBN
           MOVE    SPA-GMN-TOTALTENTAIKBN  TO SYS-2010-TOTALTEN-TAIKBN
           MOVE    SPA-GMN-SYUKEIKBN       TO SYS-2010-SYUKEIKBN
           MOVE    SPA-GMN-SYUKEITAIKBN    TO SYS-2010-SYUKEI-TAIKBN
           MOVE    SPA-GMN-FTNKEIKBNG      TO SYS-2010-FTNKEIKBN-G
           MOVE    SPA-GMN-FTNKEITAIKBNG   TO SYS-2010-FTNKEI-TAIKBN-G
           MOVE    SPA-GMN-FTNKEIKBNN      TO SYS-2010-FTNKEIKBN-N
           MOVE    SPA-GMN-FTNKEITAIKBNN   TO SYS-2010-FTNKEI-TAIKBN-N
           MOVE    SPA-GMN-FTNMONEYKBNG    TO SYS-2010-FTNMONEYKBN-G
           MOVE    SPA-GMN-FTNMONEYTAIKBNG TO SYS-2010-FTNMONEY-TAIKBN-G
           MOVE    SPA-GMN-FTNMONEYKBNN    TO SYS-2010-FTNMONEYKBN-N
           MOVE    SPA-GMN-FTNMONEYTAIKBNN TO SYS-2010-FTNMONEY-TAIKBN-N
           MOVE    SPA-GMN-FTNZEROKBNG     TO SYS-2010-FTNZEROKBN-G
           MOVE    SPA-GMN-FTNZEROTAIKBNG  TO SYS-2010-FTNZERO-TAIKBN-G
           MOVE    SPA-GMN-FTNZEROKBNN     TO SYS-2010-FTNZEROKBN-N
           MOVE    SPA-GMN-FTNZEROTAIKBNN  TO SYS-2010-FTNZERO-TAIKBN-N
           MOVE    SPA-GMN-SKJKBN          TO SYS-2010-SKJKBN
           MOVE    SPA-GMN-SKJTAIKBN       TO SYS-2010-SKJ-TAIKBN
           MOVE    SPA-GMN-OCRKBN          TO SYS-2010-OCRKBN
           MOVE    SPA-GMN-OCRTAIKBN       TO SYS-2010-OCR-TAIKBN
           MOVE    SPA-GMN-JYUHENKBN       TO SYS-2010-JYUHENKBN
           MOVE    SPA-GMN-JYUHENTAIKBN    TO SYS-2010-JYUHEN-TAIKBN
           MOVE    SPA-GMN-JGNFTNKBN       TO SYS-2010-JGNFTNKBN
           MOVE    "0"                     TO SYS-2010-JGNFTN-TAIKBN
           MOVE    SPA-GMN-SAIKEIKBN       TO SYS-2010-SAIKEIKBN
           MOVE    SPA-GMN-SAIKEITAIKBN    TO SYS-2010-SAIKEI-TAIKBN
           MOVE    SPA-GMN-HKNFTNKBNK      TO SYS-2010-HKNFTNKBN-K
           MOVE    SPA-GMN-HKNFTNTAIKBNK   TO SYS-2010-HKNFTN-TAIKBN-K
           MOVE    SPA-GMN-HKNFTNKBNY      TO SYS-2010-HKNFTNKBN-Y
           MOVE    SPA-GMN-HKNFTNTAIKBNY   TO SYS-2010-HKNFTN-TAIKBN-Y
           MOVE    SPA-GMN-RECEKSIKBNG     TO SYS-2010-RECEKSIKBN-G
           MOVE    SPA-GMN-RECEKSITAIKBNG  TO SYS-2010-RECEKSI-TAIKBN-G
           MOVE    SPA-GMN-RECEKSIKBNN     TO SYS-2010-RECEKSIKBN-N
           MOVE    SPA-GMN-RECEKSITAIKBNN  TO SYS-2010-RECEKSI-TAIKBN-N
           MOVE    SPA-GMN-CHKJGNKBNG      TO SYS-2010-CHKJGNKBN-G
           MOVE    SPA-GMN-CHKJGNTAIKBNG   TO SYS-2010-CHKJGN-TAIKBN-G
           MOVE    SPA-GMN-CHKJGNKBNN      TO SYS-2010-CHKJGNKBN-N
           MOVE    SPA-GMN-CHKJGNTAIKBNN   TO SYS-2010-CHKJGN-TAIKBN-N
           MOVE    SPA-GMN-KOHSKYKBNG      TO SYS-2010-KOHSKYKBN-G
           MOVE    SPA-GMN-KOHSKYTAIKBNG   TO SYS-2010-KOHSKY-TAIKBN-G
           MOVE    SPA-GMN-KOHSKYKBNN      TO SYS-2010-KOHSKYKBN-N
           MOVE    SPA-GMN-KOHSKYTAIKBNN   TO SYS-2010-KOHSKY-TAIKBN-N
           MOVE    SPA-GMN-PTJGNKBN        TO SYS-2010-PTJGNKBN
           MOVE    SPA-GMN-PTJGNTAIKBN     TO SYS-2010-PTJGN-TAIKBN
           MOVE    SPA-GMN-IRYOKEIKBNG     TO SYS-2010-IRYOKEIKBN-G
           MOVE    SPA-GMN-IRYOKEITAIKBNG  TO SYS-2010-IRYOKEI-TAIKBN-G
           MOVE    SPA-GMN-IRYOKEIKBNN     TO SYS-2010-IRYOKEIKBN-N
           MOVE    SPA-GMN-IRYOKEITAIKBNN  TO SYS-2010-IRYOKEI-TAIKBN-N
           MOVE    SPA-GMN-SKJKEIKBN       TO SYS-2010-SKJKEIKBN
           MOVE    SPA-GMN-SKJKEITAIKBN    TO SYS-2010-SKJKEI-TAIKBN
           MOVE    SPA-GMN-KINJYOKBNG       TO SYS-2010-KINJYOKBN-G
           MOVE    SPA-GMN-KINJYOTAIKBNG    TO SYS-2010-KINJYO-TAIKBN-G
           MOVE    SPA-GMN-KINJYOKBNN       TO SYS-2010-KINJYOKBN-N
           MOVE    SPA-GMN-KINJYOTAIKBNN    TO SYS-2010-KINJYO-TAIKBN-N
           MOVE    SPA-GMN-INNAIKBN        TO SYS-2010-INNAIKBN
           MOVE    SPA-GMN-INNAITAIKBN     TO SYS-2010-INNAI-TAIKBN
           MOVE    SPA-GMN-JIRITUKBN        TO SYS-2010-JIRITUKBN
           MOVE    SPA-GMN-JIRITUTAIKBN     TO SYS-2010-JIRITU-TAIKBN
           MOVE    SPA-GMN-SKJRYOKBNHON    TO SYS-2010-SKJRYOKBN-HON
           MOVE    SPA-GMN-SKJRYOTAIKBNHON TO SYS-2010-SKJRYO-TAIKBN-HON
           MOVE    SPA-GMN-SEIRYOKBNHON    TO SYS-2010-SEIRYOKBN-HON
           MOVE    SPA-GMN-SEIRYOTAIKBNHON TO SYS-2010-SEIRYO-TAIKBN-HON
           MOVE    SPA-GMN-SEIKANKBNHON    TO SYS-2010-SEIKANKBN-HON
           MOVE    SPA-GMN-SEIKANTAIKBNHON TO SYS-2010-SEIKAN-TAIKBN-HON
           MOVE    SPA-GMN-SKJRYOKBNTSY    TO SYS-2010-SKJRYOKBN-TSY
           MOVE    SPA-GMN-SKJRYOTAIKBNTSY TO SYS-2010-SKJRYO-TAIKBN-TSY
           MOVE    SPA-GMN-SEIRYOKBNTSY    TO SYS-2010-SEIRYOKBN-TSY
           MOVE    SPA-GMN-SEIRYOTAIKBNTSY TO SYS-2010-SEIRYO-TAIKBN-TSY
           MOVE    SPA-GMN-SEIKANKBNTSY    TO SYS-2010-SEIKANKBN-TSY
           MOVE    SPA-GMN-SEIKANTAIKBNTSY TO SYS-2010-SEIKAN-TAIKBN-TSY
           MOVE    SPA-GMN-SKJRYOKBNTNK    TO SYS-2010-SKJRYOKBN-TNK
           MOVE    SPA-GMN-SKJRYOTAIKBNTNK TO SYS-2010-SKJRYO-TAIKBN-TNK
           MOVE    SPA-GMN-SEIRYOKBNTNK    TO SYS-2010-SEIRYOKBN-TNK
           MOVE    SPA-GMN-SEIRYOTAIKBNTNK TO SYS-2010-SEIRYO-TAIKBN-TNK
           MOVE    SPA-GMN-SEIKANKBNTNK    TO SYS-2010-SEIKANKBN-TNK
           MOVE    SPA-GMN-SEIKANTAIKBNTNK TO SYS-2010-SEIKAN-TAIKBN-TNK
      ***
           MOVE    SPA-GMN-KAKKBNK         TO SYS-2010-KAKKBN-K
           MOVE    SPA-GMN-KAKTAIKBNK      TO SYS-2010-KAK-TAIKBN-K
           MOVE    SPA-GMN-KAK21KBNK       TO SYS-2010-KAK21KBN-K
           MOVE    SPA-GMN-KAK21TAIKBNK    TO SYS-2010-KAK21-TAIKBN-K
           MOVE    SPA-GMN-KAK22KBNK       TO SYS-2010-KAK22KBN-K
           MOVE    SPA-GMN-KAK22TAIKBNK    TO SYS-2010-KAK22-TAIKBN-K
           MOVE    SPA-GMN-KAK23KBNK       TO SYS-2010-KAK23KBN-K
           MOVE    SPA-GMN-KAK23TAIKBNK    TO SYS-2010-KAK23-TAIKBN-K
           MOVE    SPA-GMN-KAK31KBNK       TO SYS-2010-KAK31KBN-K
           MOVE    SPA-GMN-KAK31TAIKBNK    TO SYS-2010-KAK31-TAIKBN-K
           MOVE    SPA-GMN-KAK32KBNK       TO SYS-2010-KAK32KBN-K
           MOVE    SPA-GMN-KAK32TAIKBNK    TO SYS-2010-KAK32-TAIKBN-K
           MOVE    SPA-GMN-KAK41KBNK       TO SYS-2010-KAK41KBN-K
           MOVE    SPA-GMN-KAK41TAIKBNK    TO SYS-2010-KAK41-TAIKBN-K
           MOVE    SPA-GMN-KAK42KBNK       TO SYS-2010-KAK42KBN-K
           MOVE    SPA-GMN-KAK42TAIKBNK    TO SYS-2010-KAK42-TAIKBN-K
           MOVE    SPA-GMN-KAK43KBNK       TO SYS-2010-KAK43KBN-K
           MOVE    SPA-GMN-KAK43TAIKBNK    TO SYS-2010-KAK43-TAIKBN-K
           MOVE    SPA-GMN-KAKKBNY         TO SYS-2010-KAKKBN-Y
           MOVE    SPA-GMN-KAKTAIKBNY      TO SYS-2010-KAK-TAIKBN-Y
           MOVE    SPA-GMN-KAK21KBNY       TO SYS-2010-KAK21KBN-Y
           MOVE    SPA-GMN-KAK21TAIKBNY    TO SYS-2010-KAK21-TAIKBN-Y
           MOVE    SPA-GMN-KAK22KBNY       TO SYS-2010-KAK22KBN-Y
           MOVE    SPA-GMN-KAK22TAIKBNY    TO SYS-2010-KAK22-TAIKBN-Y
           MOVE    SPA-GMN-KAK23KBNY       TO SYS-2010-KAK23KBN-Y
           MOVE    SPA-GMN-KAK23TAIKBNY    TO SYS-2010-KAK23-TAIKBN-Y
           MOVE    SPA-GMN-KAK31KBNY       TO SYS-2010-KAK31KBN-Y
           MOVE    SPA-GMN-KAK31TAIKBNY    TO SYS-2010-KAK31-TAIKBN-Y
           MOVE    SPA-GMN-KAK32KBNY       TO SYS-2010-KAK32KBN-Y
           MOVE    SPA-GMN-KAK32TAIKBNY    TO SYS-2010-KAK32-TAIKBN-Y
           MOVE    SPA-GMN-KAK41KBNY       TO SYS-2010-KAK41KBN-Y
           MOVE    SPA-GMN-KAK41TAIKBNY    TO SYS-2010-KAK41-TAIKBN-Y
           MOVE    SPA-GMN-KAK42KBNY       TO SYS-2010-KAK42KBN-Y
           MOVE    SPA-GMN-KAK42TAIKBNY    TO SYS-2010-KAK42-TAIKBN-Y
           MOVE    SPA-GMN-KAK43KBNY       TO SYS-2010-KAK43KBN-Y
           MOVE    SPA-GMN-KAK43TAIKBNY    TO SYS-2010-KAK43-TAIKBN-Y
           MOVE    SPA-GMN-KSINGKBNKG      TO SYS-2010-KSINGKBN-K-G
           MOVE    SPA-GMN-KSINGTAIKBNKG   TO SYS-2010-KSING-TAIKBN-K-G
           MOVE    SPA-GMN-KSINGKBNKN      TO SYS-2010-KSINGKBN-K-N
           MOVE    SPA-GMN-KSINGTAIKBNKN   TO SYS-2010-KSING-TAIKBN-K-N
           MOVE    SPA-GMN-KSINGKBNYG      TO SYS-2010-KSINGKBN-Y-G
           MOVE    SPA-GMN-KSINGTAIKBNYG   TO SYS-2010-KSING-TAIKBN-Y-G
           MOVE    SPA-GMN-KSINGKBNYN      TO SYS-2010-KSINGKBN-Y-N
           MOVE    SPA-GMN-KSINGTAIKBNYN   TO SYS-2010-KSING-TAIKBN-Y-N
           MOVE    SPA-GMN-ENCHGKBNKG      TO SYS-2010-ENCHGKBN-K-G
           MOVE    SPA-GMN-ENCHGTAIKBNKG   TO SYS-2010-ENCHG-TAIKBN-K-G
           MOVE    SPA-GMN-ENCHGKBNKN      TO SYS-2010-ENCHGKBN-K-N
           MOVE    SPA-GMN-ENCHGTAIKBNKN   TO SYS-2010-ENCHG-TAIKBN-K-N
           MOVE    SPA-GMN-ENCHGKBNYG      TO SYS-2010-ENCHGKBN-Y-G
           MOVE    SPA-GMN-ENCHGTAIKBNYG   TO SYS-2010-ENCHG-TAIKBN-Y-G
           MOVE    SPA-GMN-ENCHGKBNYN      TO SYS-2010-ENCHGKBN-Y-N
           MOVE    SPA-GMN-ENCHGTAIKBNYN   TO SYS-2010-ENCHG-TAIKBN-Y-N
           MOVE    SPA-GMN-KOH2SKJKBN      TO SYS-2010-KOH2SKJKBN
           MOVE    SPA-GMN-KOH2SKJTAIKBN   TO SYS-2010-KOH2SKJ-TAIKBN
           MOVE    SPA-GMN-FTNHASUKBNG     TO SYS-2010-FTNHASUKBN-G
           MOVE    SPA-GMN-FTNHASUTAIKBNG  TO SYS-2010-FTNHASU-TAIKBN-G
           MOVE    SPA-GMN-FTNHASUKBNN     TO SYS-2010-FTNHASUKBN-N
           MOVE    SPA-GMN-FTNHASUTAIKBNN  TO SYS-2010-FTNHASU-TAIKBN-N
           MOVE    SPA-GMN-KAICNTKBNG      TO SYS-2010-KAICNTKBN-G
           MOVE    SPA-GMN-KAICNTTAIKBNG   TO SYS-2010-KAICNT-TAIKBN-G
           MOVE    SPA-GMN-CYOKEIKBNG      TO SYS-2010-CYOKEIKBN-G
           MOVE    SPA-GMN-CYOKEITAIKBNG   TO SYS-2010-CYOKEI-TAIKBN-G
           MOVE    SPA-GMN-GNBKEIKBNKG     TO SYS-2010-GNBKEIKBN-K-G
           MOVE    SPA-GMN-GNBKEITAIKBNKG  TO SYS-2010-GNBKEI-TAIKBN-K-G
           MOVE    SPA-GMN-ZENKEIKBNG      TO SYS-2010-ZENKEIKBN-G
           MOVE    SPA-GMN-ZENKEITAIKBNG   TO SYS-2010-ZENKEI-TAIKBN-G
           MOVE    SPA-GMN-SINKEIKBNG      TO SYS-2010-SINKEIKBN-G
           MOVE    SPA-GMN-SINKEITAIKBNG   TO SYS-2010-SINKEI-TAIKBN-G
           MOVE    SPA-GMN-SYOKEIKBNG      TO SYS-2010-SYOKEIKBN-G
           MOVE    SPA-GMN-SYOKEITAIKBNG   TO SYS-2010-SYOKEI-TAIKBN-G
           MOVE    SPA-GMN-SYOKEIKBNN      TO SYS-2010-SYOKEIKBN-N
           MOVE    SPA-GMN-SYOKEITAIKBNN   TO SYS-2010-SYOKEI-TAIKBN-N
           MOVE    SPA-GMN-NYUKEIKBNN      TO SYS-2010-NYUKEIKBN-N
           MOVE    SPA-GMN-NYUKEITAIKBNN   TO SYS-2010-NYUKEI-TAIKBN-N
           MOVE    SPA-GMN-DAYKEIKBNN      TO SYS-2010-DAYKEIKBN-N
           MOVE    SPA-GMN-DAYKEITAIKBNN   TO SYS-2010-DAYKEI-TAIKBN-N
           MOVE    SPA-GMN-NINNASIKBN      TO SYS-2010-NINNASIKBN
           MOVE    SPA-GMN-NINNASITAIKBN   TO SYS-2010-NINNASI-TAIKBN
           MOVE    SPA-GMN-NINJYOKBN       TO SYS-2010-NINJYOKBN
           MOVE    SPA-GMN-NINJYOTAIKBN    TO SYS-2010-NINJYO-TAIKBN
           MOVE    SPA-GMN-KOGAKKEIKBN     TO SYS-2010-KOGAKKEIKBN
           MOVE    SPA-GMN-KOGAKKEITAIKBN  TO SYS-2010-KOGAKKEI-TAIKBN
           MOVE    SPA-GMN-TANKIKBN        TO SYS-2010-TANKIKBN
           MOVE    SPA-GMN-TANKITAIKBN     TO SYS-2010-TANKI-TAIKBN
           MOVE    SPA-GMN-KAICNTKBNN      TO SYS-2010-KAICNTKBN-N
           MOVE    SPA-GMN-KAICNTTAIKBNN   TO SYS-2010-KAICNT-TAIKBN-N
           MOVE    SPA-GMN-CYOKEIKBNN      TO SYS-2010-CYOKEIKBN-N
           MOVE    SPA-GMN-CYOKEITAIKBNN   TO SYS-2010-CYOKEI-TAIKBN-N
           MOVE    SPA-GMN-ZENKEI2KBNG      TO SYS-2010-ZENKEI2KBN-G
           MOVE    SPA-GMN-ZENKEI2TAIKBNG   TO SYS-2010-ZENKEI2-TAIKBN-G
           MOVE    SPA-GMN-ZENDAYKBNN      TO SYS-2010-ZENDAYKBN-N
           MOVE    SPA-GMN-ZENDAYTAIKBNN   TO SYS-2010-ZENDAY-TAIKBN-N
           MOVE    SPA-GMN-ZENDAY2KBNN      TO SYS-2010-ZENDAY2KBN-N
           MOVE    SPA-GMN-ZENDAY2TAIKBNN   TO SYS-2010-ZENDAY2-TAIKBN-N
           MOVE    SPA-GMN-SYOKEI2KBNG      TO SYS-2010-SYOKEI2KBN-G
           MOVE    SPA-GMN-SYOKEI2TAIKBNG   TO SYS-2010-SYOKEI2-TAIKBN-G
           MOVE    SPA-GMN-SYOKEI2KBNN      TO SYS-2010-SYOKEI2KBN-N
           MOVE    SPA-GMN-SYOKEI2TAIKBNN   TO SYS-2010-SYOKEI2-TAIKBN-N
           MOVE    SPA-GMN-NINNASIKBNK      TO SYS-2010-NINNASIKBN-K
           MOVE    SPA-GMN-NINNASITAIKBNK   TO SYS-2010-NINNASI-TAIKBN-K
      *
           MOVE    SPA-GMN-CYOKIFTNKBN
                                       TO  SYS-2010-CYOKI-FTNKBN
           MOVE    SPA-GMN-CYOKIFTNTAIKBN
                                       TO  SYS-2010-CYOKI-FTNKBN-TAIKBN
           MOVE    SPA-GMN-CYOKIKSIKBN
                                       TO  SYS-2010-CYOKI-KSIKBN
           MOVE    SPA-GMN-CYOKIKSITAIKBN
                                       TO  SYS-2010-CYOKI-KSIKBN-TAIKBN
      *
           .
       4201-KOHINF-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       420-SAKUJO-SEC                  SECTION.
      *
           PERFORM 4101-SELNUM1-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
               CONTINUE
           ELSE
               MOVE    1           TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-SELNUM1 =   SPACE )
                   MOVE    "1105"  TO  SPA-ERRCD
                   MOVE    1       TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    "2001"      TO  SPA-WIDCD
           END-IF
      *
           .
       420-SAKUJO-EXT.
           EXIT.
      *****************************************************************
      *    タブ切替処理
      *****************************************************************
       420-SWITCH-PAGE-SEC             SECTION.
      *
      *    入力個所のセット
           PERFORM 400-GMN-INPUT-SEC
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               EVALUATE    SPA-GMN-PAGENO
               WHEN    0
                   PERFORM 4102-PAGE00-KANREN-CHK-SEC
               WHEN    1
                   PERFORM 4102-PAGE01-KANREN-CHK-SEC
               WHEN    2
                   PERFORM 4102-PAGE02-KANREN-CHK-SEC
               WHEN    3
                   PERFORM 4102-PAGE03-KANREN-CHK-SEC
               WHEN    4
                   PERFORM 4102-PAGE04-KANREN-CHK-SEC
               WHEN    5
                   PERFORM 4102-PAGE05-KANREN-CHK-SEC
               WHEN    6
                   PERFORM 4102-PAGE06-KANREN-CHK-SEC
               WHEN    7
                   PERFORM 4102-PAGE07-KANREN-CHK-SEC
               WHEN    8
                   PERFORM 4102-PAGE08-KANREN-CHK-SEC
               WHEN    9
                   PERFORM 4102-PAGE09-KANREN-CHK-SEC
               END-EVALUATE
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    WRK-PAGENO      TO  SPA-GMN-PAGENO
               IF    ( SPA-GMN-PAGENO  >   9 )
                   MOVE    0           TO  SPA-GMN-PAGENO
               END-IF
      *
               PERFORM 4300-SET-CURSOR-SEC
      *
           END-IF
      *
           .
       420-SWITCH-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       421-SAKUJO-SEC                  SECTION.
      *
           PERFORM  4201-SYS2010-DELETE-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 311-SPASET-SEC
           END-IF
      *
           .
       421-SAKUJO-EXT.
           EXIT.
      *****************************************************************
      *    履歴チェック処理
      *****************************************************************
       4100-RIREKI-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-KIKAN-DABURI
           MOVE    SPACE           TO  WRK-ERRSTYMD
                                       WRK-ERREDYMD
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "2010"          TO  SYS-KANRICD
           MOVE    SPA-GMN-KOHNUM  TO  SYS-KBNCD
           PERFORM 900-SYSKANRI-KEY8-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-2010-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( FLG-KIKAN-DABURI    =   1 )
      *
      *        有効期間が重複するデータがないかチェック
      *        （変更処理の場合、元データは対象外とする）
               IF    ( FLG-UPD-KBN           =  2 )
                AND  ( SPA-NAI-KOHNUM-ORG    =   SYS-2010-KBNCD  )
                AND  ( SPA-NAI-YUKOSTYMD-ORG =   SYS-2010-STYUKYMD )
                AND  ( SPA-NAI-YUKOEDYMD-ORG =   SYS-2010-EDYUKYMD )
                   CONTINUE
               ELSE
                   IF    ( SPA-NAI-YUKOEDYMD <   SYS-2010-STYUKYMD )
                     OR  ( SPA-NAI-YUKOSTYMD >   SYS-2010-EDYUKYMD )
                       CONTINUE
                   ELSE
                       MOVE    SYS-2010-STYUKYMD TO    WRK-YMD
                       PERFORM 800-HIZUKE-CHK-SEC
                       MOVE    WRK-HENYMDG       TO    WRK-ERRSTYMD
                       MOVE    SYS-2010-EDYUKYMD TO    WRK-YMD
                       PERFORM 800-HIZUKE-CHK-SEC
                       MOVE    WRK-HENYMDG       TO    WRK-ERREDYMD
                       MOVE    1             TO  FLG-KIKAN-DABURI
                   END-IF
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY8-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-2010-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
      *
       4100-RIREKI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費付加情報更新処理
      *****************************************************************
       4201-SYS2010-UPDATE-SEC         SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "2010"          TO  SYS-KANRICD
           MOVE    SPA-NAI-KOHNUM-ORG
                                   TO  SYS-KBNCD
           MOVE    SPA-NAI-YUKOSTYMD-ORG
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF   ( FLG-SYSKANRI     =   ZERO )
      *
               PERFORM 4201-SYS2010-EDIT-SEC
      *
               MOVE    SYS-2010-REC        TO  SYSKANRI-REC
               MOVE    SYS-2010-HOSPNUM    TO  SYSUP-HOSPNUM
               MOVE    SYS-2010-KANRICD    TO  SYSUP-KANRICD
               MOVE    SPA-NAI-KOHNUM-ORG  TO  SYSUP-KBNCD
               MOVE    SPA-NAI-YUKOSTYMD-ORG
                                           TO  SYSUP-STYUKYMD
               MOVE    SPA-NAI-YUKOEDYMD-ORG
                                           TO  SYSUP-EDYUKYMD
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
      *
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF   (  MCP-RC          NOT =   ZERO )
                   MOVE    "4001"      TO  SPA-ERRCD
               END-IF
      *
           ELSE
                   MOVE    "4002"      TO  SPA-ERRCD
           END-IF
      *
           .
      *
       4201-SYS2010-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    公費付加情報追加処理
      *****************************************************************
       4201-SYS2010-INSERT-SEC         SECTION.
      *
      *    公費付加情報編集処理
           PERFORM 4201-SYS2010-EDIT-SEC
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD    TO  SYS-2010-CREYMD
                                       SYS-2010-UPYMD
           MOVE    SMCNDATE-HMS    TO  SYS-2010-UPHMS
      *
           MOVE    SYS-2010-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF   (  MCP-RC          NOT =   ZERO )
               MOVE    "4003"      TO  SPA-ERRCD
           END-IF
      *
           .
      *
       4201-SYS2010-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    公費付加情報削除処理
      *****************************************************************
       4201-SYS2010-DELETE-SEC         SECTION.
      *
           INITIALIZE                      SYS-2010-REC
           MOVE    SPA-HOSPNUM         TO  SYS-2010-HOSPNUM
           MOVE    "2010"              TO  SYS-2010-KANRICD
           MOVE    SPA-NAI-KOHNUM-ORG  TO  SYS-2010-KBNCD
           MOVE    SPA-NAI-YUKOSTYMD-ORG
                                       TO  SYS-2010-STYUKYMD
           MOVE    SPA-NAI-YUKOEDYMD-ORG
                                       TO  SYS-2010-EDYUKYMD
           MOVE    SYS-2010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF   (  MCP-RC          NOT =   ZERO )
               MOVE    "4004"      TO  SPA-ERRCD
           END-IF
      *
           .
       4201-SYS2010-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    基準日
      *****************************************************************
       4100-KJNYMD-CHK-SEC             SECTION.
      *
           MOVE    ZERO        TO  SPA-GMN-CUR
      *
           MOVE    W65-KJNYMD  TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           IF  ( FLG-HIZUKECHK     =   ZERO )
      *
               INITIALIZE              SPA-W65
      *
               MOVE    WRK-HENYMDG TO  SPA-GMN-KJNYMD
               MOVE    WRK-SYMD    TO  SPA-NAI-KJNYMD
      *
               PERFORM 311-SPASET-SEC
      *
           ELSE
               MOVE    "0014"      TO  SPA-ERRCD
               MOVE    6           TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       4100-KJNYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号１
      *****************************************************************
       4100-SELNUM1-CHK-SEC            SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           INITIALIZE                  WRK-SELNUM-G
           MOVE    ZERO            TO  SPA-NAI-INPUT-MODE
      *
           MOVE    W65-SELNUM1     TO  SPA-GMN-SELNUM1
      *
           IF    ( SPA-GMN-SELNUM1 =  SPACE )
               PERFORM 4201-INPUT-AREA-CLEAR-SEC
               PERFORM 4201-RIREKI-AREA-CLEAR-SEC
               MOVE    ZERO            TO  SPA-GMN-CUR
           ELSE
               MOVE    SPA-GMN-SELNUM1     TO  WRK-KANACHK-INPUT
               PERFORM 800-ENRTRY-CHK-SEC
               MOVE    WRK-KANACHK-INPUT   TO  SPA-GMN-SELNUM1
      *
               PERFORM 4101-SELNUM1-CHK-SEC
               IF    ( SPA-ERRCD   =   SPACE )
                   PERFORM 41001-SELNUM1-CHK-SEC
               ELSE
                   PERFORM 4201-INPUT-AREA-CLEAR-SEC
                   PERFORM 4201-RIREKI-AREA-CLEAR-SEC
                   MOVE    W65-SELNUM1     TO  SPA-GMN-SELNUM1
               END-IF
           END-IF
      *
           .
      *
       4100-SELNUM1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号１
      *****************************************************************
       41001-SELNUM1-CHK-SEC           SECTION.
      *
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
           PERFORM 4201-RIREKI-AREA-CLEAR-SEC
      *
           IF    ( WRK-INPUT-MODE      =   2 )
               MOVE    W65-SELNUM1     TO  SPA-GMN-SELNUM1
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SPA-GMN-KOHLST-MAX )
                   MOVE    WRK-SELNUM-FLG (IDX0)
                                       TO  SPA-NAI-SELNUM1-FLG (IDX0)
               END-PERFORM
           END-IF
      *
           MOVE    WRK-BASESELNUM      TO  SPA-GMN-KOHLST-SEL
      *
           MOVE    WRK-INPUT-MODE      TO  SPA-NAI-INPUT-MODE
           MOVE    SPA-GMN-TKKOHNUM (WRK-BASESELNUM)
                                       TO  SEL-HKNNUM
           MOVE    SPA-NAI-KJNYMD      TO  SEL-KJNYMD
           PERFORM 4100-INPUT-AREA-EDIT-SEC
      *
           IF    ( SPA-NAI-INPUT-MODE  =   1 )
               PERFORM 4100-RIREKI-AREA-EDIT-SEC
           END-IF
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           .
       41001-SELNUM1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号１チェック処理
      *****************************************************************
       4101-SELNUM1-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-SELNUM1     =  SPACE )
               CONTINUE
           ELSE
               IF    ( SPA-GMN-SELNUM1 (1:1)    =   "." )
                   MOVE    SPA-GMN-SELNUM1 (2:)
                                           TO  WRK-SELNUM-MUL
               ELSE
                   MOVE    SPA-GMN-SELNUM1 TO  WRK-SELNUM-MUL
               END-IF
      *
               PERFORM 41011-MULTI-SEL-CHK-SEC
           END-IF
      *
           .
      *
       4101-SELNUM1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    複数行選択チェック処理
      *****************************************************************
       41011-MULTI-SEL-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-MULTI-AREA
      *
           MOVE    ZERO            TO  WRK-INPUT-MODE
                                       WRK-BASESELNUM
      *
           UNSTRING    WRK-SELNUM-MUL  DELIMITED   BY  ","
                                                   OR  "."
           INTO        WRK-MUL-NUM (1)
                       WRK-MUL-NUM (2)
                       WRK-MUL-NUM (3)
                       WRK-MUL-NUM (4)
                       WRK-MUL-NUM (5)
                       WRK-MUL-NUM (6)
                       WRK-MUL-NUM (7)
                       WRK-MUL-NUM (8)
                       WRK-MUL-NUM (9)
                       WRK-MUL-NUM (10)
                       WRK-MUL-NUM (11)
                       WRK-MUL-NUM (12)
                       WRK-MUL-NUM (13)
                       WRK-MUL-NUM (14)
                       WRK-MUL-NUM (15)
                       WRK-MUL-NUM (16)
                       WRK-MUL-NUM (17)
                       WRK-MUL-NUM (18)
                       WRK-MUL-NUM (19)
                       WRK-MUL-NUM (20)
           ON  OVERFLOW
               MOVE    1           TO  SPA-GMN-CUR
               MOVE    "0015"      TO  SPA-ERRCD  
               GO  TO  41011-MULTI-SEL-CHK-EXT
           END-UNSTRING
      *
           MOVE    ZERO            TO  CNT-SEL
                                       WRK-SELNUM
           PERFORM VARYING IDX-MUL    FROM    1   BY  1
                   UNTIL ( IDX-MUL    >       20 )
      *
               IF    ( WRK-MUL-NUM (IDX-MUL)  =   SPACE )
                   CONTINUE
               ELSE
      *
                   INITIALIZE  WRK-MUL-STNUM-X
                               WRK-MUL-EDNUM-X
                               WRK-MUL-STNUM
                               WRK-MUL-EDNUM
      *
                   UNSTRING    WRK-MUL-NUM (IDX-MUL) DELIMITED   BY  "-"
                   INTO        WRK-MUL-STNUM-X
                               WRK-MUL-EDNUM-X
                   END-UNSTRING
      *
                   IF    ( WRK-MUL-STNUM-X     =   SPACE )
                    AND  ( WRK-MUL-EDNUM-X     =   SPACE )
                       MOVE    1           TO  SPA-GMN-CUR
                       MOVE    "0001"      TO  SPA-ERRCD
                       GO  TO  41011-MULTI-SEL-CHK-EXT
                   ELSE
                       EVALUATE    TRUE
                       WHEN  ( WRK-MUL-EDNUM-X     =   SPACE )
                           MOVE    WRK-MUL-STNUM-X
                                           TO  WRK-MUL-EDNUM-X
                       WHEN  ( WRK-MUL-STNUM-X     =   SPACE )
                           MOVE    WRK-MUL-EDNUM-X
                                           TO  WRK-MUL-STNUM-X
                       END-EVALUATE
                   END-IF
      *
                   MOVE    WRK-MUL-STNUM-X     TO  WRK-SELNUM-X
                   PERFORM 4101-SELNUM-LIMIT-CHK-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                       GO  TO  41011-MULTI-SEL-CHK-EXT
                   END-IF
                   MOVE    WRK-SELNUM  TO  WRK-MUL-STNUM
      *
                   MOVE    WRK-MUL-EDNUM-X TO  WRK-SELNUM-X
                   PERFORM 4101-SELNUM-LIMIT-CHK-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                       GO  TO  41011-MULTI-SEL-CHK-EXT
                   END-IF
                   MOVE    WRK-SELNUM  TO  WRK-MUL-EDNUM
      *
      *
                   PERFORM VARYING IDX1    FROM    WRK-MUL-STNUM   BY  1
                           UNTIL ( IDX1    >       WRK-MUL-EDNUM )
                       IF    ( IDX1        >=  1 )
                        AND  ( IDX1        <=  SPA-GMN-KOHLST-MAX )
                           COMPUTE CNT-SEL  =   CNT-SEL  +   1
                           IF    ( CNT-SEL  =   1 )
                               MOVE    IDX1    TO  WRK-BASESELNUM
                           END-IF
                           MOVE    1       TO  WRK-SELNUM-FLG (IDX1)
                       END-IF
                   END-PERFORM
      *
               END-IF
      *
           END-PERFORM
      *
           MOVE    ZERO            TO  CNT-SEL
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       SPA-GMN-KOHLST-MAX )
      *
               IF    ( WRK-SELNUM-FLG (IDX1)   =   1 )
                   COMPUTE CNT-SEL =   CNT-SEL +   1
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( CNT-SEL         >   1 )
                   MOVE    2           TO  WRK-INPUT-MODE
               ELSE
                   MOVE    1           TO  WRK-INPUT-MODE
               END-IF
           END-IF
      *
           .
       41011-MULTI-SEL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号範囲チェック処理
      *****************************************************************
       4101-SELNUM-LIMIT-CHK-SEC       SECTION.
      *
           INITIALIZE                  ORCSNUMAREA
           MOVE    WRK-SELNUM-X    TO  SNUM-INX
           CALL    "ORCSNUM"       USING   ORCSNUMAREA
           IF    ( SNUM-RC         =   ZERO )
            AND  ( SNUM-MINKBN     =   SPACE )
            AND  ( SNUM-SYOKBN     =   SPACE ) 
               MOVE    SNUM-OUTNUM TO  WRK-SELNUM
               IF    ( WRK-SELNUM  >=  1 )
                AND  ( WRK-SELNUM  <=  SPA-GMN-KOHLST-MAX )
                   CONTINUE
               ELSE
                   MOVE    "0001"  TO  SPA-ERRCD
                   MOVE    1       TO  SPA-GMN-CUR
               END-IF
      *
           ELSE
                   MOVE    "0001"  TO  SPA-ERRCD
                   MOVE    1       TO  SPA-GMN-CUR
           END-IF
      *
           .
       4101-SELNUM-LIMIT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力項目領域編集処理
      *****************************************************************
       4100-INPUT-AREA-EDIT-SEC        SECTION.
      *
      *    選択番号１
           IF    ( SPA-NAI-INPUT-MODE      =   1 )
               MOVE    SPA-GMN-KOHLST-SEL  TO  WRK-ZZZ
               IF    ( WRK-ZZZ-G (1:1)     =   SPACE )
                   UNSTRING    WRK-ZZZ-G   DELIMITED   BY  ALL SPACE
                   INTO        WRK-STR-TMP
                               SPA-GMN-SELNUM1
                   END-UNSTRING
               ELSE
                   MOVE    WRK-ZZZ-G   TO  SPA-GMN-SELNUM1
               END-IF
           END-IF
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "2010"          TO  SYS-KANRICD
           MOVE    SEL-HKNNUM      TO  SYS-KBNCD
           MOVE    SEL-KJNYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-2010-REC
      *
           MOVE    SYS-2010-STYUKYMD
                                   TO  SPA-NAI-YUKOSTYMD
                                       SPA-NAI-YUKOSTYMD-ORG
                                       WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-YUKOSTYMD
      *
           MOVE    SYS-2010-EDYUKYMD
                                   TO  SPA-NAI-YUKOEDYMD
                                       SPA-NAI-YUKOEDYMD-ORG
                                       WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-YUKOEDYMD
      *
      *    公費の種類コンボ編集処理
           PERFORM 3110-KOHNUMLST-SEC
      *
           IF    ( SPA-NAI-INPUT-MODE  =   1 )
               MOVE    SYS-2010-KBNCD  TO  SPA-GMN-KOHNUM
                                           SPA-NAI-KOHNUM
                                           SPA-NAI-KOHNUM-ORG
           ELSE
               MOVE    SPACE           TO  SPA-GMN-KOHNUM
                                           SPA-NAI-KOHNUM
                                           SPA-NAI-KOHNUM-ORG
           END-IF
      *
           MOVE    SPA-GMN-KOHNUM  TO  WRK-CMB-CD
           PERFORM 4200-KOHNUMLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    WRK-CMB-CD  TO  SPA-GMN-KOHNUM-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-KOHNUM-G
           END-IF
      *
           MOVE    SYS-2010-JNISSUKBN  TO  WRK-CMB-CD
           PERFORM 4200-JNISSUKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-JNISSUKBN-G
      *
           MOVE    SYS-2010-JNISSU-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-JNISSUTAIKBN-G
      *
           MOVE    SYS-2010-TOTALTENKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-TOTALTENKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-TOTALTENKBN-G
      *
           MOVE    SYS-2010-TOTALTEN-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-TOTALTENTAIKBN-G
      *
           MOVE    SYS-2010-SYUKEIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYUKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYUKEIKBN-G
           IF    ( WRK-CMB-ITM2       =   SPACE )
               CONTINUE
           ELSE
               MOVE    WRK-CMB-ITM2   TO  SYS-2010-SYUKEI-TAIKBN
           END-IF
      *
           MOVE    SYS-2010-SYUKEI-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYUKEITAIKBN-G
      *
           MOVE    SYS-2010-FTNKEIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-FTNKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNKEIKBNG-G
      *
           MOVE    SYS-2010-FTNKEI-TAIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNKEITAIKBNG-G
      *
           MOVE    SYS-2010-FTNKEIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-FTNKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNKEIKBNN-G
      *
           MOVE    SYS-2010-FTNKEI-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNKEITAIKBNN-G
      
      *
           MOVE    SYS-2010-FTNMONEYKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-FTNMONEYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNMONEYKBNG-G
      *
           MOVE    SYS-2010-FTNMONEY-TAIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNMONEYTAIKBNG-G
      *
           MOVE    SYS-2010-FTNMONEYKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-FTNMONEYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNMONEYKBNN-G
      *
           MOVE    SYS-2010-FTNMONEY-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNMONEYTAIKBNN-G
      *
           MOVE    SYS-2010-FTNZEROKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-FTNZEROKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNZEROKBNG-G
      *
           MOVE    SYS-2010-FTNZERO-TAIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNZEROTAIKBNG-G
      *
           MOVE    SYS-2010-FTNZEROKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-FTNZEROKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNZEROKBNN-G
      *
           MOVE    SYS-2010-FTNZERO-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNZEROTAIKBNN-G
      *
           MOVE    SYS-2010-SKJKBN     TO  WRK-CMB-CD
           PERFORM 4200-SKJKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJKBN-G
      *
           MOVE    SYS-2010-SKJ-TAIKBN TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJTAIKBN-G
      *
           MOVE    SYS-2010-OCRKBN     TO  WRK-CMB-CD
           PERFORM 4200-OCRKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-OCRKBN-G
      *
           MOVE    SYS-2010-OCR-TAIKBN TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-OCRTAIKBN-G
      *
           MOVE    SYS-2010-SAIKEIKBN TO  WRK-CMB-CD
           PERFORM 4200-SAIKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SAIKEIKBN-G
      *
           MOVE    SYS-2010-SAIKEI-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SAIKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SAIKEITAIKBN-G
      *
           MOVE    SYS-2010-HKNFTNKBN-K
                                       TO  WRK-CMB-CD
           PERFORM 4200-HKNFTNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HKNFTNKBNK-G
      *
           MOVE    SYS-2010-HKNFTN-TAIKBN-K
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HKNFTNTAIKBNK-G
      *
           MOVE    SYS-2010-HKNFTNKBN-Y
                                       TO  WRK-CMB-CD
           PERFORM 4200-HKNFTNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HKNFTNKBNY-G
      *
           MOVE    SYS-2010-HKNFTN-TAIKBN-Y
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HKNFTNTAIKBNY-G
      *
           MOVE    SYS-2010-RECEKSIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-RECEKSIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-RECEKSIKBNG-G
      *
           MOVE    SYS-2010-RECEKSI-TAIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-RECEKSITAIKBNG-G
      *
           MOVE    SYS-2010-RECEKSIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-RECEKSIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-RECEKSIKBNN-G
      *
           MOVE    SYS-2010-RECEKSI-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-RECEKSITAIKBNN-G
      *
           MOVE    SYS-2010-CHKJGNKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-CHKJGNKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CHKJGNKBNG-G
      *
           MOVE    SYS-2010-CHKJGN-TAIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-CHKJGNTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CHKJGNTAIKBNG-G
      *
           MOVE    SYS-2010-CHKJGNKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-CHKJGNKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CHKJGNKBNN-G
      *
           MOVE    SYS-2010-CHKJGN-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-CHKJGNTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CHKJGNTAIKBNN-G
      *
           MOVE    SYS-2010-KOHSKYKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-KOHSKYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOHSKYKBNG-G
      *
           MOVE    SYS-2010-KOHSKY-TAIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOHSKYTAIKBNG-G
      *
           MOVE    SYS-2010-KOHSKYKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-KOHSKYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOHSKYKBNN-G
      *
           MOVE    SYS-2010-KOHSKY-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOHSKYTAIKBNN-G
      *
           MOVE    SYS-2010-JYUHENKBN  TO  WRK-CMB-CD
           PERFORM 4200-JYUHENKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-JYUHENKBN-G
      *
           MOVE    SYS-2010-JYUHEN-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-JYUHENTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-JYUHENTAIKBN-G
      *
           MOVE    SYS-2010-JGNFTNKBN  TO  WRK-CMB-CD
           PERFORM 4200-JGNFTNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-JGNFTNKBN-G
      *
           MOVE    SYS-2010-PTJGNKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-PTJGNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-PTJGNKBN-G
      *
           MOVE    SYS-2010-PTJGN-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-PTJGNTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-PTJGNTAIKBN-G
      *
           MOVE    SYS-2010-IRYOKEIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-IRYOKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-IRYOKEIKBNG-G
      *
           MOVE    SYS-2010-IRYOKEI-TAIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-IRYOKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-IRYOKEITAIKBNG-G
      *
           MOVE    SYS-2010-IRYOKEIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-IRYOKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-IRYOKEIKBNN-G
      *
           MOVE    SYS-2010-IRYOKEI-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-IRYOKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-IRYOKEITAIKBNN-G
      *
           MOVE    SYS-2010-SKJKEIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJKEIKBN-G
      *
           MOVE    SYS-2010-SKJKEI-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJKEITAIKBN-G
      *
           MOVE    SYS-2010-KINJYOKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-KINJYOKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KINJYOKBNG-G
      *
           MOVE    SYS-2010-KINJYO-TAIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-KINJYOTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KINJYOTAIKBNG-G
      *
           MOVE    SYS-2010-KINJYOKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-KINJYOKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KINJYOKBNN-G
      *
           MOVE    SYS-2010-KINJYO-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-KINJYOTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KINJYOTAIKBNN-G
      *
           MOVE    SYS-2010-INNAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-INNAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-INNAIKBN-G
      *
           MOVE    SYS-2010-INNAI-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-INNAITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-INNAITAIKBN-G
      *
           MOVE    SYS-2010-JIRITUKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-JIRITUKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-JIRITUKBN-G
      *
           MOVE    SYS-2010-JIRITU-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-JIRITUTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-JIRITUTAIKBN-G
      *
           MOVE    SYS-2010-SKJRYOKBN-HON
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOKBNHON-G
      *
           MOVE    SYS-2010-SKJRYO-TAIKBN-HON
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNHONLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOTAIKBNHON-G
      *
           MOVE    SYS-2010-SEIRYOKBN-HON
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOKBNHON-G
      *
           MOVE    SYS-2010-SEIRYO-TAIKBN-HON
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNHONLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOTAIKBNHON-G
      *
           MOVE    SYS-2010-SEIKANKBN-HON
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANKBNHON-G
      *
           MOVE    SYS-2010-SEIKAN-TAIKBN-HON
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNHONLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANTAIKBNHON-G
      *
           MOVE    SYS-2010-SKJRYOKBN-TSY
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOKBNTSY-G
      *
           MOVE    SYS-2010-SKJRYO-TAIKBN-TSY
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTSYLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOTAIKBNTSY-G
      *
           MOVE    SYS-2010-SEIRYOKBN-TSY
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOKBNTSY-G
      *
           MOVE    SYS-2010-SEIRYO-TAIKBN-TSY
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTSYLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOTAIKBNTSY-G
      *
           MOVE    SYS-2010-SEIKANKBN-TSY
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANKBNTSY-G
      *
           MOVE    SYS-2010-SEIKAN-TAIKBN-TSY
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTSYLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANTAIKBNTSY-G
      *
           MOVE    SYS-2010-SKJRYOKBN-TNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOKBNTNK-G
      *
           MOVE    SYS-2010-SKJRYO-TAIKBN-TNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTNKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOTAIKBNTNK-G
      *
           MOVE    SYS-2010-SEIRYOKBN-TNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOKBNTNK-G
      *
           MOVE    SYS-2010-SEIRYO-TAIKBN-TNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTNKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOTAIKBNTNK-G
      *
           MOVE    SYS-2010-SEIKANKBN-TNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANKBNTNK-G
      *
           MOVE    SYS-2010-SEIKAN-TAIKBN-TNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTNKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANTAIKBNTNK-G
      *
      **
           MOVE    SYS-2010-KAKKBN-K   TO  WRK-CMB-CD
           PERFORM 4200-KAKKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAKKBNK-G
      *
           MOVE    SYS-2010-KAK-TAIKBN-K
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAKTAIKBNK-G
      *
           MOVE    SYS-2010-KAK21KBN-K TO  WRK-CMB-CD
           PERFORM 4200-KAK20KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK21KBNK-G
      *
           MOVE    SYS-2010-KAK21-TAIKBN-K
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK21TAIKBNK-G
      *
           MOVE    SYS-2010-KAK22KBN-K TO  WRK-CMB-CD
           PERFORM 4200-KAK20KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK22KBNK-G
      *
           MOVE    SYS-2010-KAK22-TAIKBN-K
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK22TAIKBNK-G
      *
           MOVE    SYS-2010-KAK23KBN-K TO  WRK-CMB-CD
           PERFORM 4200-KAK20KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK23KBNK-G
      *
           MOVE    SYS-2010-KAK23-TAIKBN-K
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK23TAIKBNK-G
      *
           MOVE    SYS-2010-KAK31KBN-K TO  WRK-CMB-CD
           PERFORM 4200-KAK31KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK31KBNK-G
      *
           MOVE    SYS-2010-KAK31-TAIKBN-K
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK31TAIKBNK-G
      *
           MOVE    SYS-2010-KAK32KBN-K TO  WRK-CMB-CD
           PERFORM 4200-KAK32KBNKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK32KBNK-G
      *
           MOVE    SYS-2010-KAK32-TAIKBN-K
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK32TAIKBNK-G
      *
           MOVE    SYS-2010-KAK41KBN-K TO  WRK-CMB-CD
           PERFORM 4200-KAK40KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK41KBNK-G
      *
           MOVE    SYS-2010-KAK41-TAIKBN-K
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK41TAIKBNK-G
      *
           MOVE    SYS-2010-KAK42KBN-K TO  WRK-CMB-CD
           PERFORM 4200-KAK40KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK42KBNK-G
      *
           MOVE    SYS-2010-KAK42-TAIKBN-K
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK42TAIKBNK-G
      *
           MOVE    SYS-2010-KAK43KBN-K TO  WRK-CMB-CD
           PERFORM 4200-KAK40KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK43KBNK-G
      *
           MOVE    SYS-2010-KAK43-TAIKBN-K
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK43TAIKBNK-G
      *
           MOVE    SYS-2010-KAKKBN-Y   TO  WRK-CMB-CD
           PERFORM 4200-KAKKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAKKBNY-G
      *
           MOVE    SYS-2010-KAK-TAIKBN-Y
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAKTAIKBNY-G
      *
           MOVE    SYS-2010-KAK21KBN-Y TO  WRK-CMB-CD
           PERFORM 4200-KAK20KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK21KBNY-G
      *
           MOVE    SYS-2010-KAK21-TAIKBN-Y
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK21TAIKBNY-G
      *
           MOVE    SYS-2010-KAK22KBN-Y TO  WRK-CMB-CD
           PERFORM 4200-KAK20KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK22KBNY-G
      *
           MOVE    SYS-2010-KAK22-TAIKBN-Y
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK22TAIKBNY-G
      *
           MOVE    SYS-2010-KAK23KBN-Y TO  WRK-CMB-CD
           PERFORM 4200-KAK20KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK23KBNY-G
      *
           MOVE    SYS-2010-KAK23-TAIKBN-Y
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK23TAIKBNY-G
      *
           MOVE    SYS-2010-KAK31KBN-Y TO  WRK-CMB-CD
           PERFORM 4200-KAK31KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK31KBNY-G
      *
           MOVE    SYS-2010-KAK31-TAIKBN-Y
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK31TAIKBNY-G
      *
           MOVE    SYS-2010-KAK32KBN-Y TO  WRK-CMB-CD
           PERFORM 4200-KAK32KBNYLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK32KBNY-G
      *
           MOVE    SYS-2010-KAK32-TAIKBN-Y
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK32TAIKBNY-G
      *
           MOVE    SYS-2010-KAK41KBN-Y TO  WRK-CMB-CD
           PERFORM 4200-KAK40KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK41KBNY-G
      *
           MOVE    SYS-2010-KAK41-TAIKBN-Y
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK41TAIKBNY-G
      *
           MOVE    SYS-2010-KAK42KBN-Y TO  WRK-CMB-CD
           PERFORM 4200-KAK40KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK42KBNY-G
      *
           MOVE    SYS-2010-KAK42-TAIKBN-Y
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK42TAIKBNY-G
      *
           MOVE    SYS-2010-KAK43KBN-Y TO  WRK-CMB-CD
           PERFORM 4200-KAK40KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK43KBNY-G
      *
           MOVE    SYS-2010-KAK43-TAIKBN-Y
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK43TAIKBNY-G
      *
           MOVE    SYS-2010-KSINGKBN-K-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-KSINGKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KSINGKBNKG-G
      *
           MOVE    SYS-2010-KSING-TAIKBN-K-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KSINGTAIKBNKG-G
      *
           MOVE    SYS-2010-KSINGKBN-K-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-KSINGKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KSINGKBNKN-G
      *
           MOVE    SYS-2010-KSING-TAIKBN-K-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KSINGTAIKBNKN-G
      *
           MOVE    SYS-2010-KSINGKBN-Y-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-KSINGKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KSINGKBNYG-G
      *
           MOVE    SYS-2010-KSING-TAIKBN-Y-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KSINGTAIKBNYG-G
      *
           MOVE    SYS-2010-KSINGKBN-Y-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-KSINGKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KSINGKBNYN-G
      *
           MOVE    SYS-2010-KSING-TAIKBN-Y-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KSINGTAIKBNYN-G
      *
           MOVE    SYS-2010-ENCHGKBN-K-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-ENCHGKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ENCHGKBNKG-G
      *
           MOVE    SYS-2010-ENCHG-TAIKBN-K-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ENCHGTAIKBNKG-G
      *
           MOVE    SYS-2010-ENCHGKBN-K-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-ENCHGKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ENCHGKBNKN-G
      *
           MOVE    SYS-2010-ENCHG-TAIKBN-K-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ENCHGTAIKBNKN-G
      *
           MOVE    SYS-2010-ENCHGKBN-Y-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-ENCHGKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ENCHGKBNYG-G
      *
           MOVE    SYS-2010-ENCHG-TAIKBN-Y-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ENCHGTAIKBNYG-G
      *
           MOVE    SYS-2010-ENCHGKBN-Y-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-ENCHGKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ENCHGKBNYN-G
      *
           MOVE    SYS-2010-ENCHG-TAIKBN-Y-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ENCHGTAIKBNYN-G
      *
           MOVE    SYS-2010-KOH2SKJKBN TO  WRK-CMB-CD
           PERFORM 4200-KOH2SKJKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOH2SKJKBN-G
      *
           MOVE    SYS-2010-KOH2SKJ-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOH2SKJTAIKBN-G
      *
           MOVE    SYS-2010-FTNHASUKBN-G TO  WRK-CMB-CD
           PERFORM 4200-FTNHASUKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNHASUKBNG-G
      *
           MOVE    SYS-2010-FTNHASU-TAIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-FTNHASUTAIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNHASUTAIKBNG-G
      *
           MOVE    SYS-2010-FTNHASUKBN-N TO  WRK-CMB-CD
           PERFORM 4200-FTNHASUKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNHASUKBNN-G
      *
           MOVE    SYS-2010-FTNHASU-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-FTNHASUTAIKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNHASUTAIKBNN-G
      *
           MOVE    SYS-2010-KAICNTKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-KAICNTKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAICNTKBNG-G
      *
           MOVE    SYS-2010-KAICNT-TAIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-KAICNTTAIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAICNTTAIKBNG-G
      *
           MOVE    SYS-2010-CYOKEIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-CYOKEIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKEIKBNG-G
      *
           MOVE    SYS-2010-CYOKEI-TAIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-CYOKEITAIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKEITAIKBNG-G
      *
           MOVE    SYS-2010-GNBKEIKBN-K-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-GNBKEIKBNKGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-GNBKEIKBNKG-G
      *
           MOVE    SYS-2010-GNBKEI-TAIKBN-K-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-GNBKEITAIKBNKGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-GNBKEITAIKBNKG-G
      *
           MOVE    SYS-2010-ZENKEIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-ZENKEIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENKEIKBNG-G
      *
           MOVE    SYS-2010-ZENKEI-TAIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-ZENKEITAIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENKEITAIKBNG-G
      *
           MOVE    SYS-2010-SINKEIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-SINKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SINKEIKBNG-G
      *
           MOVE    SYS-2010-SINKEI-TAIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-SINKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SINKEITAIKBNG-G
      *
           MOVE    SYS-2010-SYOKEIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYOKEIKBNG-G
      *
           MOVE    SYS-2010-SYOKEI-TAIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYOKEITAIKBNG-G
      *
           MOVE    SYS-2010-SYOKEIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYOKEIKBNN-G
      *
           MOVE    SYS-2010-SYOKEI-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYOKEITAIKBNN-G
      *
           MOVE    SYS-2010-NYUKEIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-NYUKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NYUKEIKBNN-G
      *
           MOVE    SYS-2010-NYUKEI-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-NYUKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NYUKEITAIKBNN-G
      *
           MOVE    SYS-2010-DAYKEIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-DAYKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-DAYKEIKBNN-G
      *
           MOVE    SYS-2010-DAYKEI-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-DAYKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-DAYKEITAIKBNN-G
      *
           MOVE    SYS-2010-NINNASIKBN TO  WRK-CMB-CD
           PERFORM 4200-NINNASIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NINNASIKBN-G
      *
           MOVE    SYS-2010-NINNASI-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-NINNASITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NINNASITAIKBN-G
      *
           MOVE    SYS-2010-NINJYOKBN  TO  WRK-CMB-CD
           PERFORM 4200-NINJYOKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NINJYOKBN-G
      *
           MOVE    SYS-2010-NINJYO-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-NINJYOTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NINJYOTAIKBN-G
      *
           MOVE    SYS-2010-KOGAKKEIKBN  TO  WRK-CMB-CD
           PERFORM 4200-KOGAKKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOGAKKEIKBN-G
      *
           MOVE    SYS-2010-KOGAKKEI-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-KOGAKKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOGAKKEITAIKBN-G
      *
           MOVE    SYS-2010-TANKIKBN  TO  WRK-CMB-CD
           PERFORM 4200-TANKIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-TANKIKBN-G
      *
           MOVE    SYS-2010-TANKI-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-TANKITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-TANKITAIKBN-G
      *
           MOVE    SYS-2010-KAICNTKBN-N  TO  WRK-CMB-CD
           PERFORM 4200-KAICNTKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAICNTKBNN-G
      *
           MOVE    SYS-2010-KAICNT-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-KAICNTTAIKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAICNTTAIKBNN-G
      *
           MOVE    SYS-2010-CYOKEIKBN-N  TO  WRK-CMB-CD
           PERFORM 4200-CYOKEIKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKEIKBNN-G
      *
           MOVE    SYS-2010-CYOKEI-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-CYOKEITAIKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKEITAIKBNN-G
      *
           MOVE    SYS-2010-ZENDAYKBN-N  TO  WRK-CMB-CD
           PERFORM 4200-ZENDAYKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENDAYKBNN-G
      *
           MOVE    SYS-2010-ZENDAY-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-ZENDAYTAIKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENDAYTAIKBNN-G
      *
           MOVE    SYS-2010-ZENKEI2KBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-ZENKEI2KBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENKEI2KBNG-G
      *
           MOVE    SYS-2010-ZENKEI2-TAIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-ZENKEI2TAIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENKEI2TAIKBNG-G
      *
           MOVE    SYS-2010-ZENDAY2KBN-N  TO  WRK-CMB-CD
           PERFORM 4200-ZENDAY2KBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENDAY2KBNN-G
      *
           MOVE    SYS-2010-ZENDAY2-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-ZENDAY2TAIKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENDAY2TAIKBNN-G
      *
           MOVE    SYS-2010-SYOKEI2KBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKEI2KBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYOKEI2KBNG-G
      *
           MOVE    SYS-2010-SYOKEI2-TAIKBN-G
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKEI2TAIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYOKEI2TAIKBNG-G
      *
           MOVE    SYS-2010-SYOKEI2KBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKEI2KBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYOKEI2KBNN-G
      *
           MOVE    SYS-2010-SYOKEI2-TAIKBN-N
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKEI2TAIKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYOKEI2TAIKBNN-G
      *
           MOVE    SYS-2010-NINNASIKBN-K
                                       TO  WRK-CMB-CD
           PERFORM 4200-NINNASIKBNKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NINNASIKBNK-G
      *
           MOVE    SYS-2010-NINNASI-TAIKBN-K
                                       TO  WRK-CMB-CD
           PERFORM 4200-NINNASITAIKBNKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NINNASITAIKBNK-G
      *
           MOVE    SYS-2010-CYOKI-FTNKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-CYOKIFTNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKIFTNKBN-G
      *
           MOVE    SYS-2010-CYOKI-FTNKBN-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-CYOKIFTNTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKIFTNTAIKBN-G
      *
           MOVE    SYS-2010-CYOKI-KSIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-CYOKIKSIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKIKSIKBN-G
      *
           MOVE    SYS-2010-CYOKI-KSIKBN-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-CYOKIKSITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKIKSITAIKBN-G
      *
      **
      *
           MOVE    SYS-2010-STYUKYMD   TO  SEL-STYMD
           MOVE    SYS-2010-EDYUKYMD   TO  SEL-EDYMD
           MOVE    SYS-2010-KBNCD      TO  SEL-HKNNUM
           PERFORM 900-HKNNUM-KEY8-SEL-SEC
           MOVE    HKN-HON-NYUSKJRYYKBN
                                       TO  SPA-NAI-SKJKBNHON
           MOVE    HKN-TSY-NYUSKJRYYKBN
                                       TO  SPA-NAI-SKJKBNTSY
           MOVE    HKN-TNK-NYUSKJRYYKBN
                                       TO  SPA-NAI-SKJKBNTNK
      *
           IF    ( SYS-2010-FTNKEIKBN-G    =   SPACE )
               MOVE    HKNNUM-REC          TO  THKNNUM-REC
               PERFORM 42012-COMBO-SET-DEFAULT-SEC
           END-IF
      *
           .
       4100-INPUT-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    履歴領域編集処理
      *****************************************************************
       4100-RIREKI-AREA-EDIT-SEC       SECTION.
      *
           INITIALIZE      SPA-GMN-RIREKILST-AREA
                           SPA-NAI-RIREKILST-AREA
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "2010"              TO  SYS-KANRICD
           MOVE    SPA-GMN-KOHNUM      TO  SYS-KBNCD
           PERFORM 900-SYSKANRI-KEY8-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-2010-REC
      *
           PERFORM VARYING IDX-MAX FROM    1   BY  1
               UNTIL     ( FLG-SYSKANRI    NOT =   ZERO )
                 OR      ( IDX-MAX      >  CONST-RIREKILST-MAX )
      *
               MOVE    IDX-MAX         TO  SPA-GMN-RIREKILST-MAX
      *
      *        有効開始日
               MOVE    SYS-2010-STYUKYMD
                                       TO  SPA-NAI-TRYUKOSTYMD (IDX-MAX)
                                           WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TRYUKOSTYMD (IDX-MAX)
      *
      *        有効終了日
               MOVE    SYS-2010-EDYUKYMD
                                       TO  SPA-NAI-TRYUKOEDYMD (IDX-MAX)
                                           WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TRYUKOEDYMD (IDX-MAX)
      *
               IF    ( SPA-NAI-YUKOSTYMD-ORG
                                   =   SPA-NAI-TRYUKOSTYMD (IDX-MAX))
                AND  ( SPA-NAI-YUKOEDYMD-ORG
                                   =   SPA-NAI-TRYUKOEDYMD (IDX-MAX))
                   MOVE    IDX-MAX    TO  SPA-GMN-SELNUM2
                   MOVE    IDX-MAX    TO  SPA-GMN-RIREKILST-SEL
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY8-FET-SEC
               MOVE    SYSKANRI-REC   TO  SYS-2010-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
      *
      *
       4100-RIREKI-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    公費番号
      *****************************************************************
       4101-KOHNUM-CHK-SEC            SECTION.
      *
           INITIALIZE                      HKNNUM-REC
      *
           IF    ( SPA-GMN-KOHNUM =   SPACE   )
               MOVE    SPACE               TO  SPA-GMN-KOHNUM-G
           ELSE
               MOVE    SPA-GMN-KOHNUM      TO  WRK-CMB-CD
               PERFORM 4200-KOHNUMLST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   EVALUATE    TRUE
                   WHEN  ( SPA-GMN-KOHNUM      >=   "100" )
                    AND  ( SPA-GMN-KOHNUM      <=   "899" )
                   WHEN  ( SPA-GMN-KOHNUM       =   "972" )
                       MOVE    2           TO  SPA-GMN-CUR
                       MOVE    "0006"      TO  SPA-ERRCD
                   WHEN  OTHER
                       MOVE    2           TO  SPA-GMN-CUR
                       MOVE    "0005"      TO  SPA-ERRCD
                   END-EVALUATE
               ELSE
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-KOHNUM-G
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-KOHNUM      NOT =   SPA-NAI-KOHNUM )
      *
               IF    ( SPA-ERRCD           =   SPACE )
      *
                   PERFORM 4201-YUKOKIKAN-RESET-SEC
      *
                   INITIALIZE              SPA-GMN-NBOOK-AREA
                                           SPA-NAI-NBOOK-AREA
                   PERFORM 42011-COMBO-SET-DEFAULT-SEC
      *
                   IF    ( SPA-PREFNUM     =       CONST-NAGASAKI )
                    AND  ( SPA-GMN-KOHNUM  =   "180" )
                       MOVE    "1"         TO  SPA-GMN-JGNFTNKBN
                   END-IF
      *
                   MOVE    SPA-NAI-YUKOSTYMD   TO  SEL-STYMD
                   MOVE    SPA-NAI-YUKOEDYMD   TO  SEL-EDYMD
                   MOVE    SPA-GMN-KOHNUM      TO  SEL-HKNNUM
                   PERFORM 900-HKNNUM-KEY8-SEL-SEC
                   MOVE    HKN-HON-NYUSKJRYYKBN
                                       TO  SPA-NAI-SKJKBNHON
                   MOVE    HKN-TSY-NYUSKJRYYKBN
                                       TO  SPA-NAI-SKJKBNTSY
                   MOVE    HKN-TNK-NYUSKJRYYKBN
                                       TO  SPA-NAI-SKJKBNTNK
                   MOVE    HKNNUM-REC      TO  THKNNUM-REC
                   PERFORM 42012-COMBO-SET-DEFAULT-SEC
               END-IF
      *
               MOVE    SPA-GMN-KOHNUM  TO  SPA-NAI-KOHNUM
      *
           END-IF
      *
           .
      *
       4101-KOHNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    実日数省略記載チェック処理
      *****************************************************************
       4101-JNISSUKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-JNISSUKBN       TO  WRK-CMB-CD
           PERFORM 4200-JNISSUKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-JNISSUKBN-G
      *
           IF    ( SPA-GMN-JNISSUKBN       =   "0" )
               MOVE    "0"                 TO  SPA-GMN-JNISSUTAIKBN
           END-IF
      *
           .
      *
       4101-JNISSUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    実日数省略記載対象区分チェック処理
      *****************************************************************
       4101-JNISSUTAIKBN-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-JNISSUTAIKBN    TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-JNISSUTAIKBN-G
      *
           .
      *
       4101-JNISSUTAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求点省略記載チェック処理
      *****************************************************************
       4101-TOTALTENKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-TOTALTENKBN     TO  WRK-CMB-CD
           PERFORM 4200-TOTALTENKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-TOTALTENKBN-G
      *
           IF    ( SPA-GMN-TOTALTENKBN     =   "0" )
               MOVE    "0"                 TO  SPA-GMN-TOTALTENTAIKBN
           END-IF
      *
           .
      *
       4101-TOTALTENKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求点省略記載対象区分チェック処理
      *****************************************************************
       4101-TOTALTENTAIKBN-CHK-SEC     SECTION.
      *
           MOVE    SPA-GMN-TOTALTENTAIKBN  TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-TOTALTENTAIKBN-G
      *
           .
      *
       4101-TOTALTENTAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用分請求点集計チェック処理
      *****************************************************************
       4101-SYUKEIKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-SYUKEIKBN     TO  WRK-CMB-CD
           PERFORM 4200-SYUKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM        TO  SPA-GMN-SYUKEIKBN-G
           IF    ( WRK-CMB-ITM2       =   SPACE )
               CONTINUE
           ELSE
               MOVE    WRK-CMB-ITM2   TO  SPA-GMN-SYUKEITAIKBN
           END-IF
           IF    ( SPA-GMN-SYUKEIKBN  =   "0" )
               MOVE    "0"            TO  SPA-GMN-SYUKEITAIKBN
           END-IF
      *
           .
      *
       4101-SYUKEIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用分請求点集計対象区分チェック処理
      *****************************************************************
       4101-SYUKEITAIKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-SYUKEITAIKBN     TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-SYUKEITAIKBN-G
      *
           .
      *
       4101-SYUKEITAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金金額集計（外来）チェック処理
      *****************************************************************
       4101-FTNKEIKBNG-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-FTNKEIKBNG    TO  WRK-CMB-CD
           PERFORM 4200-FTNKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNKEIKBNG-G
      *
           IF    ( SPA-GMN-FTNKEIKBNG    =   "0" )
               MOVE    "0"                 TO  SPA-GMN-FTNKEITAIKBNG
           END-IF
      *
           .
      *
       4101-FTNKEIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金金額集計（外来）対象区分チェック処理
      *****************************************************************
       4101-FTNKEITAIKBNG-CHK-SEC    SECTION.
      *
           MOVE    SPA-GMN-FTNKEITAIKBNG TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNKEITAIKBNG-G
      *
           .
      *
       4101-FTNKEITAIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金金額集計（入院）チェック処理
      *****************************************************************
       4101-FTNKEIKBNN-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-FTNKEIKBNN    TO  WRK-CMB-CD
           PERFORM 4200-FTNKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNKEIKBNN-G
      *
           IF    ( SPA-GMN-FTNKEIKBNN    =   "0" )
               MOVE    "0"                 TO  SPA-GMN-FTNKEITAIKBNN
           END-IF
      *
           .
      *
       4101-FTNKEIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金金額集計（入院）対象区分チェック処理
      *****************************************************************
       4101-FTNKEITAIKBNN-CHK-SEC    SECTION.
      *
           MOVE    SPA-GMN-FTNKEITAIKBNN TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNKEITAIKBNN-G
      *
           .
      *
       4101-FTNKEITAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金省略記載（外来）チェック処理
      *****************************************************************
       4101-FTNMONEYKBNG-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-FTNMONEYKBNG    TO  WRK-CMB-CD
           PERFORM 4200-FTNMONEYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNMONEYKBNG-G
      *
           IF    ( SPA-GMN-FTNMONEYKBNG    =   "0" )
               MOVE    "0"                 TO  SPA-GMN-FTNMONEYTAIKBNG
           END-IF
      *
           .
      *
       4101-FTNMONEYKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金省略記載（外来）対象区分チェック処理
      *****************************************************************
       4101-FTNMONEYTAIKBNG-CHK-SEC    SECTION.
      *
           MOVE    SPA-GMN-FTNMONEYTAIKBNG TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNMONEYTAIKBNG-G
      *
           .
      *
       4101-FTNMONEYTAIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金省略記載（入院）チェック処理
      *****************************************************************
       4101-FTNMONEYKBNN-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-FTNMONEYKBNN    TO  WRK-CMB-CD
           PERFORM 4200-FTNMONEYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNMONEYKBNN-G
      *
           IF    ( SPA-GMN-FTNMONEYKBNN    =   "0" )
               MOVE    "0"                 TO  SPA-GMN-FTNMONEYTAIKBNN
           END-IF
      *
           .
      *
       4101-FTNMONEYKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金省略記載（入院）対象区分チェック処理
      *****************************************************************
       4101-FTNMONEYTAIKBNN-CHK-SEC    SECTION.
      *
           MOVE    SPA-GMN-FTNMONEYTAIKBNN TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNMONEYTAIKBNN-G
      *
           .
      *
       4101-FTNMONEYTAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金０円記載（記録）（外来）チェック処理
      *****************************************************************
       4101-FTNZEROKBNG-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-FTNZEROKBNG     TO  WRK-CMB-CD
           PERFORM 4200-FTNZEROKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNZEROKBNG-G
      *
           IF    ( SPA-GMN-FTNZEROKBNG     =   "0" )
               MOVE    "0"                 TO  SPA-GMN-FTNZEROTAIKBNG
           END-IF
      *
           .
      *
       4101-FTNZEROKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金０円記載（記録）（外来）対象区分チェック処理
      *****************************************************************
       4101-FTNZEROTAIKBNG-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-FTNZEROTAIKBNG  TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNZEROTAIKBNG-G
      *
           .
      *
       4101-FTNZEROTAIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金０円記載（記録）（入院）チェック処理
      *****************************************************************
       4101-FTNZEROKBNN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-FTNZEROKBNN     TO  WRK-CMB-CD
           PERFORM 4200-FTNZEROKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNZEROKBNN-G
      *
           IF    ( SPA-GMN-FTNZEROKBNN     =   "0" )
               MOVE    "0"                 TO  SPA-GMN-FTNZEROTAIKBNN
           END-IF
      *
           .
      *
       4101-FTNZEROKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金０円記載（記録）（入院）対象区分チェック処理
      *****************************************************************
       4101-FTNZEROTAIKBNN-CHK-SEC     SECTION.
      *
           MOVE    SPA-GMN-FTNZEROTAIKBNN  TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-FTNZEROTAIKBNN-G
      *
           .
      *
       4101-FTNZEROTAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院食事療養省略記載チェック処理
      *****************************************************************
       4101-SKJKBN-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-SKJKBN          TO  WRK-CMB-CD
           PERFORM 4200-SKJKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-SKJKBN-G
      *
           IF    ( SPA-GMN-SKJKBN          =   "0" )
               MOVE    "0"                 TO  SPA-GMN-SKJTAIKBN
           END-IF
      *
           .
      *
       4101-SKJKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院食事療養省略記載対象区分チェック処理
      *****************************************************************
       4101-SKJTAIKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-SKJTAIKBN       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-SKJTAIKBN-G
      *
           .
      *
       4101-SKJTAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ＯＣＲ実日数請求点省略記載チェック処理
      *****************************************************************
       4101-OCRKBN-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-OCRKBN          TO  WRK-CMB-CD
           PERFORM 4200-OCRKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-OCRKBN-G
      *
           IF    ( SPA-GMN-OCRKBN          =   "0" )
               MOVE    "0"                 TO  SPA-GMN-OCRTAIKBN
           END-IF
      *
           .
      *
       4101-OCRKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ＯＣＲ実日数請求点省略記載対象区分チェック処理
      *****************************************************************
       4101-OCRTAIKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-OCRTAIKBN       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-OCRTAIKBN-G
      *
           .
      *
       4101-OCRTAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書公費再掲欄記載チェック処理
      *****************************************************************
       4101-SAIKEIKBN-CHK-SEC         SECTION.
      *
           MOVE    SPA-GMN-SAIKEIKBN      TO  WRK-CMB-CD
           PERFORM 4200-SAIKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-SAIKEIKBN-G
      *
           IF    ( SPA-GMN-SAIKEIKBN       =   "0" )
               MOVE    "0"                 TO  SPA-GMN-SAIKEITAIKBN
           END-IF
      *
           .
      *
       4101-SAIKEIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書公費再掲欄記載対象区分チェック処理
      *****************************************************************
       4101-SAIKEITAIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SAIKEITAIKBN   TO  WRK-CMB-CD
           PERFORM 4200-SAIKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-SAIKEITAIKBN-G
      *
           .
      *
       4101-SAIKEITAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄−一部負担金記載（高齢者）チェック処理
      *****************************************************************
       4101-HKNFTNKBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-HKNFTNKBNK  TO  WRK-CMB-CD
           PERFORM 4200-HKNFTNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HKNFTNKBNK-G
      *
           IF    ( SPA-GMN-HKNFTNKBNK  =   "0" )
               MOVE    "0"             TO  SPA-GMN-HKNFTNTAIKBNK
           END-IF
      *
           .
      *
       4101-HKNFTNKBNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄−一部負担金記載（高齢者）対象区分チェック処理
      *****************************************************************
       4101-HKNFTNTAIKBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-HKNFTNTAIKBNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HKNFTNTAIKBNK-G
      *
           .
      *
       4101-HKNFTNTAIKBNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄−一部負担金記載（70歳未満）チェック処理
      *****************************************************************
       4101-HKNFTNKBNY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-HKNFTNKBNY  TO  WRK-CMB-CD
           PERFORM 4200-HKNFTNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HKNFTNKBNY-G
      *
           IF    ( SPA-GMN-HKNFTNKBNY  =   "0" )
               MOVE    "0"             TO  SPA-GMN-HKNFTNTAIKBNY
           END-IF
      *
           .
      *
       4101-HKNFTNKBNY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄−一部負担金記載（70歳未満）対象区分チェック処理
      *****************************************************************
       4101-HKNFTNTAIKBNY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-HKNFTNTAIKBNY
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HKNFTNTAIKBNY-G
      *
           .
      *
       4101-HKNFTNTAIKBNY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    設定と異なる記載設定（外来）チェック処理
      *****************************************************************
       4101-RECEKSIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-RECEKSIKBNG  TO  WRK-CMB-CD
           PERFORM 4200-RECEKSIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-RECEKSIKBNG-G
      *
           IF    ( SPA-GMN-RECEKSIKBNG  =   "0" )
               MOVE    "0"             TO  SPA-GMN-RECEKSITAIKBNG
           END-IF
      *
           .
      *
       4101-RECEKSIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    設定と異なる記載設定（外来）対象区分チェック処理
      *****************************************************************
       4101-RECEKSITAIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-RECEKSITAIKBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-RECEKSITAIKBNG-G
      *
           .
      *
       4101-RECEKSITAIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    設定と異なる記載設定（入院）チェック処理
      *****************************************************************
       4101-RECEKSIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-RECEKSIKBNN  TO  WRK-CMB-CD
           PERFORM 4200-RECEKSIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-RECEKSIKBNN-G
      *
           IF    ( SPA-GMN-RECEKSIKBNN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-RECEKSITAIKBNN
           END-IF
      *
           .
      *
       4101-RECEKSIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    設定と異なる記載設定（入院）対象区分チェック処理
      *****************************************************************
       4101-RECEKSITAIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-RECEKSITAIKBNN
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-RECEKSITAIKBNN-G
      *
           .
      *
       4101-RECEKSITAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象上限額設定（外来）チェック処理
      *****************************************************************
       4101-CHKJGNKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-CHKJGNKBNG  TO  WRK-CMB-CD
           PERFORM 4200-CHKJGNKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CHKJGNKBNG-G
      *
           IF    ( SPA-GMN-CHKJGNKBNG  =   "0" )
               MOVE    "0"             TO  SPA-GMN-CHKJGNTAIKBNG
           END-IF
      *
           .
      *
       4101-CHKJGNKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象上限額設定（外来）対象区分チェック処理
      *****************************************************************
       4101-CHKJGNTAIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-CHKJGNTAIKBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-CHKJGNTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CHKJGNTAIKBNG-G
      *
           .
      *
       4101-CHKJGNTAIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象上限額設定（入院）チェック処理
      *****************************************************************
       4101-CHKJGNKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-CHKJGNKBNN  TO  WRK-CMB-CD
           PERFORM 4200-CHKJGNKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CHKJGNKBNN-G
      *
           IF    ( SPA-GMN-CHKJGNKBNN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-CHKJGNTAIKBNN
           END-IF
      *
           .
      *
       4101-CHKJGNKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象上限額設定（入院）対象区分チェック処理
      *****************************************************************
       4101-CHKJGNTAIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-CHKJGNTAIKBNN
                                       TO  WRK-CMB-CD
           PERFORM 4200-CHKJGNTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CHKJGNTAIKBNN-G
      *
           .
      *
       4101-CHKJGNTAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費請求テーブル作成（外来）チェック処理
      *****************************************************************
       4101-KOHSKYKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KOHSKYKBNG  TO  WRK-CMB-CD
           PERFORM 4200-KOHSKYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOHSKYKBNG-G
      *
           IF    ( SPA-GMN-KOHSKYKBNG  =   "0" )
               MOVE    "0"             TO  SPA-GMN-KOHSKYTAIKBNG
           END-IF
      *
           .
      *
       4101-KOHSKYKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費請求テーブル作成（外来）対象区分チェック処理
      *****************************************************************
       4101-KOHSKYTAIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KOHSKYTAIKBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOHSKYTAIKBNG-G
      *
           .
      *
       4101-KOHSKYTAIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費請求テーブル作成（入院）チェック処理
      *****************************************************************
       4101-KOHSKYKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KOHSKYKBNN  TO  WRK-CMB-CD
           PERFORM 4200-KOHSKYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOHSKYKBNN-G
      *
           IF    ( SPA-GMN-KOHSKYKBNN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-KOHSKYTAIKBNN
           END-IF
      *
           .
      *
       4101-KOHSKYKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費請求テーブル作成（入院）対象区分チェック処理
      *****************************************************************
       4101-KOHSKYTAIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KOHSKYTAIKBNN
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOHSKYTAIKBNN-G
      *
           .
      *
       4101-KOHSKYTAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日・月上限額負担計算チェック処理
      *****************************************************************
       4101-JGNFTNKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-JGNFTNKBN       TO  WRK-CMB-CD
           PERFORM 4200-JGNFTNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-JGNFTNKBN-G
      *
           .
      *
       4101-JGNFTNKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    月途中受給者証変更時負担計算チェック処理
      *****************************************************************
       4101-JYUHENKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-JYUHENKBN       TO  WRK-CMB-CD
           PERFORM 4200-JYUHENKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-JYUHENKBN-G
      *
           IF    ( SPA-GMN-JYUHENKBN          =   "0" )
               MOVE    "0"                 TO  SPA-GMN-JYUHENTAIKBN
           END-IF
      *
           .
      *
       4101-JYUHENKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    月途中受給者証変更時負担計算対象区分チェック処理
      *****************************************************************
       4101-JYUHENTAIKBN-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-JYUHENTAIKBN    TO  WRK-CMB-CD
           PERFORM 4200-JYUHENTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-JYUHENTAIKBN-G
      *
           .
      *
       4101-JYUHENTAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者登録−所得者情報−月上限額入力チェック処理
      *****************************************************************
       4101-PTJGNKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-PTJGNKBN  TO  WRK-CMB-CD
           PERFORM 4200-PTJGNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-PTJGNKBN-G
      *
           IF    ( SPA-GMN-PTJGNKBN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-PTJGNTAIKBN
           END-IF
      *
           .
      *
       4101-PTJGNKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者登録−所得者情報−月上限額入力対象区分チェック処理
      *****************************************************************
       4101-PTJGNTAIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-PTJGNTAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-PTJGNTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-PTJGNTAIKBN-G
      *
           .
      *
       4101-PTJGNTAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医療費負担金計算（外来）チェック処理
      *****************************************************************
       4101-IRYOKEIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-IRYOKEIKBNG  TO  WRK-CMB-CD
           PERFORM 4200-IRYOKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-IRYOKEIKBNG-G
      *
           IF    ( SPA-GMN-IRYOKEIKBNG  =   "0" )
               MOVE    "0"             TO  SPA-GMN-IRYOKEITAIKBNG
           END-IF
      *
           .
      *
       4101-IRYOKEIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医療費負担金計算（外来）対象区分チェック処理
      *****************************************************************
       4101-IRYOKEITAIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-IRYOKEITAIKBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-IRYOKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-IRYOKEITAIKBNG-G
      *
           .
      *
       4101-IRYOKEITAIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医療費負担金計算（入院）チェック処理
      *****************************************************************
       4101-IRYOKEIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-IRYOKEIKBNN  TO  WRK-CMB-CD
           PERFORM 4200-IRYOKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-IRYOKEIKBNN-G
      *
           IF    ( SPA-GMN-IRYOKEIKBNN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-IRYOKEITAIKBNN
           END-IF
      *
           .
      *
       4101-IRYOKEIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医療費負担金計算（入院）対象区分チェック処理
      *****************************************************************
       4101-IRYOKEITAIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-IRYOKEITAIKBNN
                                       TO  WRK-CMB-CD
           PERFORM 4200-IRYOKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-IRYOKEITAIKBNN-G
      *
           .
      *
       4101-IRYOKEITAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事負担金計算チェック処理
      *****************************************************************
       4101-SKJKEIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SKJKEIKBN  TO  WRK-CMB-CD
           PERFORM 4200-SKJKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJKEIKBN-G
      *
           IF    ( SPA-GMN-SKJKEIKBN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SKJKEITAIKBN
           END-IF
      *
           .
      *
       4101-SKJKEIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事負担金計算対象区分チェック処理
      *****************************************************************
       4101-SKJKEITAIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SKJKEITAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJKEITAIKBN-G
      *
           .
      *
       4101-SKJKEITAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    設定額を助成する負担金計算（外来）チェック処理
      *****************************************************************
       4101-KINJYOKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KINJYOKBNG  TO  WRK-CMB-CD
           PERFORM 4200-KINJYOKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KINJYOKBNG-G
      *
           IF    ( SPA-GMN-KINJYOKBNG  =   "0" )
               MOVE    "0"             TO  SPA-GMN-KINJYOTAIKBNG
           END-IF
      *
           .
      *
       4101-KINJYOKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    設定額を助成する負担金計算（外来）対象区分チェック処理
      *****************************************************************
       4101-KINJYOTAIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KINJYOTAIKBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-KINJYOTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KINJYOTAIKBNG-G
      *
           .
      *
       4101-KINJYOTAIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    設定額を助成する負担金計算（入院）チェック処理
      *****************************************************************
       4101-KINJYOKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KINJYOKBNN  TO  WRK-CMB-CD
           PERFORM 4200-KINJYOKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KINJYOKBNN-G
      *
           IF    ( SPA-GMN-KINJYOKBNN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-KINJYOTAIKBNN
           END-IF
      *
           .
      *
       4101-KINJYOKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    設定額を助成する負担金計算（入院）対象区分チェック処理
      *****************************************************************
       4101-KINJYOTAIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KINJYOTAIKBNN
                                       TO  WRK-CMB-CD
           PERFORM 4200-KINJYOTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KINJYOTAIKBNN-G
      *
           .
      *
       4101-KINJYOTAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    「月−院内−上限額」参照条件（外来）チェック処理
      *****************************************************************
       4101-INNAIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-INNAIKBN  TO  WRK-CMB-CD
           PERFORM 4200-INNAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-INNAIKBN-G
      *
           IF    ( SPA-GMN-INNAIKBN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-INNAITAIKBN
           END-IF
      *
           .
      *
       4101-INNAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    「月−院内−上限額」参照条件（外来）対象区分チェック処理
      *****************************************************************
       4101-INNAITAIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-INNAITAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-INNAITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-INNAITAIKBN-G
      *
           .
      *
       4101-INNAITAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    自立支援医療の特別計算チェック処理
      *****************************************************************
       4101-JIRITUKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-JIRITUKBN  TO  WRK-CMB-CD
           PERFORM 4200-JIRITUKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-JIRITUKBN-G
      *
           IF    ( SPA-GMN-JIRITUKBN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-JIRITUTAIKBN
           END-IF
      *
           .
      *
       4101-JIRITUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    自立支援医療の特別計算対象区分チェック処理
      *****************************************************************
       4101-JIRITUTAIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-JIRITUTAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-JIRITUTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-JIRITUTAIKBN-G
      *
           .
      *
       4101-JIRITUTAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事療養（本人）チェック処理
      *****************************************************************
       4101-SKJRYOKBNHON-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SKJRYOKBNHON  TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOKBNHON-G
      *
           IF    ( SPA-GMN-SKJRYOKBNHON  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SKJRYOTAIKBNHON
           END-IF
      *
           .
      *
       4101-SKJRYOKBNHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事療養（本人）対象区分チェック処理
      *****************************************************************
       4101-SKJRYOTAIKBNHON-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SKJRYOTAIKBNHON
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNHONLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOTAIKBNHON-G
      *
           .
      *
       4101-SKJRYOTAIKBNHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    生活療養（本人）チェック処理
      *****************************************************************
       4101-SEIRYOKBNHON-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIRYOKBNHON  TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOKBNHON-G
      *
           IF    ( SPA-GMN-SEIRYOKBNHON  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SEIRYOTAIKBNHON
           END-IF
      *
           .
      *
       4101-SEIRYOKBNHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    生活療養（本人）対象区分チェック処理
      *****************************************************************
       4101-SEIRYOTAIKBNHON-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIRYOTAIKBNHON
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNHONLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOTAIKBNHON-G
      *
           .
      *
       4101-SEIRYOTAIKBNHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    環境療養（本人）チェック処理
      *****************************************************************
       4101-SEIKANKBNHON-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIKANKBNHON  TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANKBNHON-G
      *
           IF    ( SPA-GMN-SEIKANKBNHON  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SEIKANKBNHON
           END-IF
      *
           .
      *
       4101-SEIKANKBNHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    環境療養（本人）対象区分チェック処理
      *****************************************************************
       4101-SEIKANTAIKBNHON-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIKANTAIKBNHON
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNHONLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANTAIKBNHON-G
      *
           .
      *
       4101-SEIKANTAIKBNHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事療養（低所得）チェック処理
      *****************************************************************
       4101-SKJRYOKBNTSY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SKJRYOKBNTSY  TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOKBNTSY-G
      *
           IF    ( SPA-GMN-SKJRYOKBNTSY  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SKJRYOTAIKBNTSY
           END-IF
      *
           .
      *
       4101-SKJRYOKBNTSY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事療養（低所得）対象区分チェック処理
      *****************************************************************
       4101-SKJRYOTAIKBNTSY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SKJRYOTAIKBNTSY
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTSYLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOTAIKBNTSY-G
      *
           .
      *
       4101-SKJRYOTAIKBNTSY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    生活療養（低所得）チェック処理
      *****************************************************************
       4101-SEIRYOKBNTSY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIRYOKBNTSY  TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOKBNTSY-G
      *
           IF    ( SPA-GMN-SEIRYOKBNTSY  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SEIRYOTAIKBNTSY
           END-IF
      *
           .
      *
       4101-SEIRYOKBNTSY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    生活療養（低所得）対象区分チェック処理
      *****************************************************************
       4101-SEIRYOTAIKBNTSY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIRYOTAIKBNTSY
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTSYLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOTAIKBNTSY-G
      *
           .
      *
       4101-SEIRYOTAIKBNTSY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    環境療養（低所得）チェック処理
      *****************************************************************
       4101-SEIKANKBNTSY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIKANKBNTSY  TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANKBNTSY-G
      *
           IF    ( SPA-GMN-SEIKANKBNTSY  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SEIKANTAIKBNTSY
           END-IF
      *
           .
      *
       4101-SEIKANKBNTSY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    環境療養（低所得）対象区分チェック処理
      *****************************************************************
       4101-SEIKANTAIKBNTSY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIKANTAIKBNTSY
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTSYLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANTAIKBNTSY-G
      *
           .
      *
       4101-SEIKANTAIKBNTSY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事療養（本人）チェック処理
      *****************************************************************
       4101-SKJRYOKBNTNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SKJRYOKBNTNK  TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOKBNTNK-G
      *
           IF    ( SPA-GMN-SKJRYOKBNTNK  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SKJRYOTAIKBNTNK
           END-IF
      *
           .
      *
       4101-SKJRYOKBNTNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事療養（低年金）対象区分チェック処理
      *****************************************************************
       4101-SKJRYOTAIKBNTNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SKJRYOTAIKBNTNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTNKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKJRYOTAIKBNTNK-G
      *
           .
      *
       4101-SKJRYOTAIKBNTNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    生活療養（低年金）チェック処理
      *****************************************************************
       4101-SEIRYOKBNTNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIRYOKBNTNK  TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOKBNTNK-G
      *
           IF    ( SPA-GMN-SEIRYOKBNTNK  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SEIRYOTAIKBNTNK
           END-IF
      *
           .
      *
       4101-SEIRYOKBNTNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    生活療養（低年金）対象区分チェック処理
      *****************************************************************
       4101-SEIRYOTAIKBNTNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIRYOTAIKBNTNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTNKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIRYOTAIKBNTNK-G
      *
           .
      *
       4101-SEIRYOTAIKBNTNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    環境療養（低年金）チェック処理
      *****************************************************************
       4101-SEIKANKBNTNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIKANKBNTNK  TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANKBNTNK-G
      *
           IF    ( SPA-GMN-SEIKANKBNTNK  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SEIKANTAIKBNTNK
           END-IF
      *
           .
      *
       4101-SEIKANKBNTNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    環境療養（低年金）対象区分チェック処理
      *****************************************************************
       4101-SEIKANTAIKBNTNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SEIKANTAIKBNTNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTNKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SEIKANTAIKBNTNK-G
      *
           .
      *
       4101-SEIKANTAIKBNTNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲記載（高齢者）チェック処理
      *****************************************************************
       4101-KAKKBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAKKBNK     TO  WRK-CMB-CD
           PERFORM 4200-KAKKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAKKBNK-G
      *
           IF    ( SPA-GMN-KAKKBNK     =   "0" )
               MOVE    "0"             TO  SPA-GMN-KAKTAIKBNK
           END-IF
      *
           .
       4101-KAKKBNK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険欄の括弧再掲記載（高齢者）対象区分チェック処理
      *****************************************************************
       4101-KAKTAIKBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAKTAIKBNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAKTAIKBNK-G
      *
           .
       4101-KAKTAIKBNK-CHK-EXT.
           EXIT.
       
      *****************************************************************
      *    保険欄の括弧再掲（下）記載（高齢者）チェック処理
      *    　主＋全＋地（＋長）（分点ありの場合）
      *****************************************************************
       4101-KAK21KBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK21KBNK TO  WRK-CMB-CD
           PERFORM 4200-KAK20KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK21KBNK-G
      *
           IF    ( SPA-GMN-KAK21KBNK   =   "0" )
               MOVE    "0"             TO  SPA-GMN-KAK21TAIKBNK
           END-IF
      *
           .
       4101-KAK21KBNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）記載（高齢者）対象区分チェック処理
      *    　主＋全＋地（＋長）（分点ありの場合）
      *****************************************************************
       4101-KAK21TAIKBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK21TAIKBNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK21TAIKBNK-G
      *
      *
           .
       4101-KAK21TAIKBNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）記載（高齢者）チェック処理
      *    　主＋全＋地　　　　（分点なしの場合）
      *****************************************************************
       4101-KAK22KBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK22KBNK TO  WRK-CMB-CD
           PERFORM 4200-KAK20KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK22KBNK-G
      *
           IF    ( SPA-GMN-KAK22KBNK   =   "0" )
               MOVE    "0"             TO  SPA-GMN-KAK22TAIKBNK
           END-IF
      *
           .
       4101-KAK22KBNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）記載（高齢者）対象区分チェック処理
      *    　主＋全＋地　　　　（分点なしの場合）
      *****************************************************************
       4101-KAK22TAIKBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK22TAIKBNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK22TAIKBNK-G
      *
      *
           .
       4101-KAK22TAIKBNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）記載（高齢者）チェック処理
      *    　主＋全＋地＋長　　（分点なしの場合）
      *****************************************************************
       4101-KAK23KBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK23KBNK TO  WRK-CMB-CD
           PERFORM 4200-KAK20KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK23KBNK-G
      *
           IF    ( SPA-GMN-KAK23KBNK   =   "0" )
               MOVE    "0"             TO  SPA-GMN-KAK23TAIKBNK
           END-IF
      *
           .
       4101-KAK23KBNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）記載（高齢者）対象区分チェック処理
      *    　主＋全＋地＋長　　（分点なしの場合）
      *****************************************************************
       4101-KAK23TAIKBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK23TAIKBNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK23TAIKBNK-G
      *
      *
           .
       4101-KAK23TAIKBNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額計算（高齢者）チェック処理
      *    　主＋全＋地　　　　（分点ありの場合）
      *****************************************************************
       4101-KAK31KBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK31KBNK TO  WRK-CMB-CD
           PERFORM 4200-KAK31KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK31KBNK-G
      *
           IF    ( SPA-GMN-KAK31KBNK   =   "0" )
               MOVE    "0"             TO  SPA-GMN-KAK31TAIKBNK
           END-IF
      *
           .
       4101-KAK31KBNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額計算（高齢者）対象区分チェック処理
      *    　主＋全＋地　　　　（分点ありの場合）
      *****************************************************************
       4101-KAK31TAIKBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK31TAIKBNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK31TAIKBNK-G
      *
      *
           .
       4101-KAK31TAIKBNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額計算（高齢者）チェック処理
      *    　主＋全＋地＋長　　（分点ありの場合）
      *****************************************************************
       4101-KAK32KBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK32KBNK TO  WRK-CMB-CD
           PERFORM 4200-KAK32KBNKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK32KBNK-G
      *
           IF    ( SPA-GMN-KAK32KBNK   =   "0" )
               MOVE    "0"             TO  SPA-GMN-KAK32TAIKBNK
           END-IF
      *
           .
       4101-KAK32KBNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額計算（高齢者）対象区分チェック処理
      *    　主＋全＋地＋長　　（分点ありの場合）
      *****************************************************************
       4101-KAK32TAIKBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK32TAIKBNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK32TAIKBNK-G
      *
      *
           .
       4101-KAK32TAIKBNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額加算（高齢者）チェック処理
      *    　主＋全＋地（＋長）（分点ありの場合）
      *****************************************************************
       4101-KAK41KBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK41KBNK TO  WRK-CMB-CD
           PERFORM 4200-KAK40KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK41KBNK-G
      *
           IF    ( SPA-GMN-KAK41KBNK =   "0" )
               MOVE    "0"             TO  SPA-GMN-KAK41TAIKBNK
           END-IF
      *
           .
       4101-KAK41KBNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額加算（高齢者）対象区分チェック処理
      *    　主＋全＋地（＋長）（分点ありの場合）
      *****************************************************************
       4101-KAK41TAIKBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK41TAIKBNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK41TAIKBNK-G
      *
      *
           .
       4101-KAK41TAIKBNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額加算（高齢者）チェック処理
      *    　主＋全＋地　　　　（分点なしの場合）
      *****************************************************************
       4101-KAK42KBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK42KBNK TO  WRK-CMB-CD
           PERFORM 4200-KAK40KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK42KBNK-G
      *
           IF    ( SPA-GMN-KAK42KBNK   =   "0" )
               MOVE    "0"             TO  SPA-GMN-KAK42TAIKBNK
           END-IF
      *
           .
       4101-KAK42KBNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額加算（高齢者）対象区分チェック処理
      *    　主＋全＋地　　　　（分点なしの場合）
      *****************************************************************
       4101-KAK42TAIKBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK42TAIKBNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK42TAIKBNK-G
      *
      *
           .
       4101-KAK42TAIKBNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額加算（高齢者）チェック処理
      *    　主＋全＋地＋長　　（分点なしの場合）
      *****************************************************************
       4101-KAK43KBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK43KBNK TO  WRK-CMB-CD
           PERFORM 4200-KAK40KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK43KBNK-G
      *
           IF    ( SPA-GMN-KAK43KBNK   =   "0" )
               MOVE    "0"             TO  SPA-GMN-KAK43TAIKBNK
           END-IF
      *
           .
       4101-KAK43KBNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額加算（高齢者）対象区分チェック処理
      *    　主＋全＋地＋長　　（分点なしの場合）
      *****************************************************************
       4101-KAK43TAIKBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK43TAIKBNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK43TAIKBNK-G
      *
      *
           .
       4101-KAK43TAIKBNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲記載（７０歳未満）チェック処理
      *****************************************************************
       4101-KAKKBNY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAKKBNY   TO  WRK-CMB-CD
           PERFORM 4200-KAKKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAKKBNY-G
      *
           IF    ( SPA-GMN-KAKKBNY   =   "0" )
               MOVE    "0"             TO  SPA-GMN-KAKTAIKBNY
           END-IF
      *
           .
       4101-KAKKBNY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲記載（７０歳未満）対象区分チェック処理
      *****************************************************************
       4101-KAKTAIKBNY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAKTAIKBNY
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAKTAIKBNY-G
      *
      *
           .
       4101-KAKTAIKBNY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）記載（７０歳未満）チェック処理
      *    　主＋全＋地（＋長）（分点ありの場合）
      *****************************************************************
       4101-KAK21KBNY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK21KBNY TO  WRK-CMB-CD
           PERFORM 4200-KAK20KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK21KBNY-G
      *
           IF    ( SPA-GMN-KAK21KBNY   =   "0" )
               MOVE    "0"             TO  SPA-GMN-KAK21TAIKBNY
           END-IF
      *
           .
       4101-KAK21KBNY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）記載（７０歳未満）対象区分チェック処理
      *    　主＋全＋地（＋長）（分点ありの場合）
      *****************************************************************
       4101-KAK21TAIKBNY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK21TAIKBNY
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK21TAIKBNY-G
      *
      *
           .
       4101-KAK21TAIKBNY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）記載（７０歳未満）チェック処理
      *    　主＋全＋地　　　　（分点なしの場合）
      *****************************************************************
       4101-KAK22KBNY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK22KBNY TO  WRK-CMB-CD
           PERFORM 4200-KAK20KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK22KBNY-G
      *
           IF    ( SPA-GMN-KAK22KBNY   =   "0" )
               MOVE    "0"             TO  SPA-GMN-KAK22TAIKBNY
           END-IF
      *
           .
       4101-KAK22KBNY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）記載（７０歳未満）対象区分チェック処理
      *    　主＋全＋地　　　　（分点なしの場合）
      *****************************************************************
       4101-KAK22TAIKBNY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK22TAIKBNY
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK22TAIKBNY-G
      *
      *
           .
       4101-KAK22TAIKBNY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）記載（７０歳未満）チェック処理
      *    　主＋全＋地＋長　　（分点なしの場合）
      *****************************************************************
       4101-KAK23KBNY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK23KBNY TO  WRK-CMB-CD
           PERFORM 4200-KAK20KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK23KBNY-G
      *
           IF    ( SPA-GMN-KAK23KBNY   =   "0" )
               MOVE    "0"             TO  SPA-GMN-KAK23TAIKBNY
           END-IF
      *
           .
       4101-KAK23KBNY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）記載（７０歳未満）対象区分チェック処理
      *    　主＋全＋地＋長　　（分点なしの場合）
      *****************************************************************
       4101-KAK23TAIKBNY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK23TAIKBNY
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK23TAIKBNY-G
      *
      *
           .
       4101-KAK23TAIKBNY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額計算（７０歳未満）チェック処理
      *    　主＋全＋地　　　　（分点ありの場合）
      *****************************************************************
       4101-KAK31KBNY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK31KBNY TO  WRK-CMB-CD
           PERFORM 4200-KAK31KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK31KBNY-G
      *
           IF    ( SPA-GMN-KAK31KBNY   =   "0" )
               MOVE    "0"             TO  SPA-GMN-KAK31TAIKBNY
           END-IF
      *
           .
       4101-KAK31KBNY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額計算（７０歳未満）対象区分チェック処理
      *    　主＋全＋地　　　　（分点ありの場合）
      *****************************************************************
       4101-KAK31TAIKBNY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK31TAIKBNY
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK31TAIKBNY-G
      *
           .
       4101-KAK31TAIKBNY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額計算（７０歳未満）チェック処理
      *    　主＋全＋地＋長　　（分点ありの場合）
      *****************************************************************
       4101-KAK32KBNY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK32KBNY TO  WRK-CMB-CD
           PERFORM 4200-KAK32KBNYLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK32KBNY-G
      *
           IF    ( SPA-GMN-KAK32KBNY   =   "0" )
               MOVE    "0"             TO  SPA-GMN-KAK32TAIKBNY
           END-IF
      *
           .
       4101-KAK32KBNY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額計算（７０歳未満）対象区分チェック処理
      *    　主＋全＋地＋長　　（分点ありの場合）
      *****************************************************************
       4101-KAK32TAIKBNY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK32TAIKBNY
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK32TAIKBNY-G
      *
           .
       4101-KAK32TAIKBNY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額加算（７０歳未満）チェック処理
      *    　主＋全＋地（＋長）（分点ありの場合）
      *****************************************************************
       4101-KAK41KBNY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK41KBNY TO  WRK-CMB-CD
           PERFORM 4200-KAK40KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK41KBNY-G
      *
           IF    ( SPA-GMN-KAK41KBNY   =   "0" )
               MOVE    "0"             TO  SPA-GMN-KAK41TAIKBNY
           END-IF
      *
           .
       4101-KAK41KBNY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額加算（７０歳未満）対象区分チェック処理
      *    　主＋全＋地（＋長）（分点ありの場合）
      *****************************************************************
       4101-KAK41TAIKBNY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK41TAIKBNY
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK41TAIKBNY-G
      *
      *
           .
       4101-KAK41TAIKBNY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額加算（７０歳未満）チェック処理
      *    　主＋全＋地　　　　（分点なしの場合）
      *****************************************************************
       4101-KAK42KBNY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK42KBNY TO  WRK-CMB-CD
           PERFORM 4200-KAK40KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK42KBNY-G
      *
           IF    ( SPA-GMN-KAK42KBNY   =   "0" )
               MOVE    "0"             TO  SPA-GMN-KAK42TAIKBNY
           END-IF
      *
           .
       4101-KAK42KBNY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額加算（７０歳未満）対象区分チェック処理
      *    　主＋全＋地　　　　（分点なしの場合）
      *****************************************************************
       4101-KAK42TAIKBNY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK42TAIKBNY
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK42TAIKBNY-G
      *
      *
           .
       4101-KAK42TAIKBNY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額加算（７０歳未満）チェック処理
      *    　主＋全＋地＋長　　（分点なしの場合）
      *****************************************************************
       4101-KAK43KBNY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK43KBNY TO  WRK-CMB-CD
           PERFORM 4200-KAK40KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK43KBNY-G
      *
           IF    ( SPA-GMN-KAK43KBNY   =   "0" )
               MOVE    "0"             TO  SPA-GMN-KAK43TAIKBNY
           END-IF
      *
           .
       4101-KAK43KBNY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額加算（７０歳未満）対象区分チェック処理
      *    　主＋全＋地＋長　　（分点なしの場合）
      *****************************************************************
       4101-KAK43TAIKBNY-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAK43TAIKBNY
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK43TAIKBNY-G
      *
           .
       4101-KAK43TAIKBNY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄−一部負担金無記載チェック処理
      *    　（高齢者・外）
      *****************************************************************
       4101-KSINGKBNKG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KSINGKBNKG
                                       TO  WRK-CMB-CD
           PERFORM 4200-KSINGKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KSINGKBNKG-G
      *
           IF    ( SPA-GMN-KSINGKBNKG  =   "0" )
               MOVE    "0"             TO  SPA-GMN-KSINGTAIKBNKG
           END-IF
      *
           .
       4101-KSINGKBNKG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄−一部負担金無記載対象区分チェック処理
      *    　（高齢者・外）
      *****************************************************************
       4101-KSINGTAIKBNKG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KSINGTAIKBNKG
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KSINGTAIKBNKG-G
      *
           .
       4101-KSINGTAIKBNKG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄−一部負担金無記載チェック処理
      *    　（高齢者・入）
      *****************************************************************
       4101-KSINGKBNKN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KSINGKBNKN
                                       TO  WRK-CMB-CD
           PERFORM 4200-KSINGKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KSINGKBNKN-G
      *
           IF    ( SPA-GMN-KSINGKBNKN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-KSINGTAIKBNKN
           END-IF
           .
       4101-KSINGKBNKN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄−一部負担金無記載対象区分チェック処理
      *    　（高齢者・入）
      *****************************************************************
       4101-KSINGTAIKBNKN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KSINGTAIKBNKN
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KSINGTAIKBNKN-G
      *
           .
       4101-KSINGTAIKBNKN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄−一部負担金無記載チェック処理
      *    　（７０未・外）
      *****************************************************************
       4101-KSINGKBNYG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KSINGKBNYG
                                       TO  WRK-CMB-CD
           PERFORM 4200-KSINGKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KSINGKBNYG-G
      *
           IF    ( SPA-GMN-KSINGKBNYG  =   "0" )
               MOVE    "0"             TO  SPA-GMN-KSINGTAIKBNYG
           END-IF
      *
           .
       4101-KSINGKBNYG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄−一部負担金無記載対象区分チェック処理
      *    　（７０未・外）
      *****************************************************************
       4101-KSINGTAIKBNYG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KSINGTAIKBNYG
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KSINGTAIKBNYG-G
      *
           .
       4101-KSINGTAIKBNYG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄−一部負担金無記載チェック処理
      *    　（７０未・入）
      *****************************************************************
       4101-KSINGKBNYN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KSINGKBNYN
                                       TO  WRK-CMB-CD
           PERFORM 4200-KSINGKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KSINGKBNYN-G
      *
           IF    ( SPA-GMN-KSINGKBNYN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-KSINGTAIKBNYN
           END-IF
      *
           .
       4101-KSINGKBNYN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄−一部負担金無記載対象区分チェック処理
      *    　（７０未・入）
      *****************************************************************
       4101-KSINGTAIKBNYN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KSINGTAIKBNYN
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KSINGTAIKBNYN-G
      *
           .
       4101-KSINGTAIKBNYN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金１円→１０円単位記載チェック処理
      *    　（高齢者・外）
      *****************************************************************
       4101-ENCHGKBNKG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-ENCHGKBNKG
                                       TO  WRK-CMB-CD
           PERFORM 4200-ENCHGKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ENCHGKBNKG-G
      *
           IF    ( SPA-GMN-ENCHGKBNKG  =   "0" )
               MOVE    "0"             TO  SPA-GMN-ENCHGTAIKBNKG
           END-IF
      *
           .
       4101-ENCHGKBNKG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金１円→１０円単位記載対象区分チェック処理
      *    　（高齢者・外）
      *****************************************************************
       4101-ENCHGTAIKBNKG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-ENCHGTAIKBNKG
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ENCHGTAIKBNKG-G
      *
           .
       4101-ENCHGTAIKBNKG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金１円→１０円単位記載チェック処理
      *    　（高齢者・入）
      *****************************************************************
       4101-ENCHGKBNKN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-ENCHGKBNKN
                                       TO  WRK-CMB-CD
           PERFORM 4200-ENCHGKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ENCHGKBNKN-G
      *
           IF    ( SPA-GMN-ENCHGKBNKN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-ENCHGTAIKBNKN
           END-IF
      *
           .
       4101-ENCHGKBNKN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金１円→１０円単位記載対象区分チェック処理
      *    　（高齢者・入）
      *****************************************************************
       4101-ENCHGTAIKBNKN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-ENCHGTAIKBNKN
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ENCHGTAIKBNKN-G
      *
           .
       4101-ENCHGTAIKBNKN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金１円→１０円単位記載チェック処理
      *    　（７０未・外）
      *****************************************************************
       4101-ENCHGKBNYG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-ENCHGKBNYG
                                       TO  WRK-CMB-CD
           PERFORM 4200-ENCHGKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ENCHGKBNYG-G
      *
           IF    ( SPA-GMN-ENCHGKBNYG  =   "0" )
               MOVE    "0"             TO  SPA-GMN-ENCHGTAIKBNYG
           END-IF
      *
           .
       4101-ENCHGKBNYG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金１円→１０円単位記載対象区分チェック処理
      *    　（７０未・外）
      *****************************************************************
       4101-ENCHGTAIKBNYG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-ENCHGTAIKBNYG
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ENCHGTAIKBNYG-G
      *
           .
       4101-ENCHGTAIKBNYG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金１円→１０円単位記載チェック処理
      *    　（７０未・入）
      *****************************************************************
       4101-ENCHGKBNYN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-ENCHGKBNYN
                                       TO  WRK-CMB-CD
           PERFORM 4200-ENCHGKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ENCHGKBNYN-G
      *
           IF    ( SPA-GMN-ENCHGKBNYN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-ENCHGTAIKBNYN
           END-IF
      *
           .
       4101-ENCHGKBNYN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金１円→１０円単位記載対象区分チェック処理
      *    　（７０未・入）
      *****************************************************************
       4101-ENCHGTAIKBNYN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-ENCHGTAIKBNYN
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ENCHGTAIKBNYN-G
      *
           .
       4101-ENCHGTAIKBNYN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院時食事療養記載（公費２欄置き換え）チェック処理
      *****************************************************************
       4101-KOH2SKJKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KOH2SKJKBN TO  WRK-CMB-CD
           PERFORM 4200-KOH2SKJKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOH2SKJKBN-G
      *
           IF    ( SPA-GMN-KOH2SKJKBN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-KOH2SKJTAIKBN
           END-IF
      *
           .
       4101-KOH2SKJKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院時食事療養記載（公費２欄置き換え）対象区分チェック処理
      *****************************************************************
       4101-KOH2SKJTAIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KOH2SKJTAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOH2SKJTAIKBN-G
      *
           .
       4101-KOH2SKJTAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金記載１円未満端数処理（外来）チェック処理
      *****************************************************************
       4101-FTNHASUKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-FTNHASUKBNG TO  WRK-CMB-CD
           PERFORM 4200-FTNHASUKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNHASUKBNG-G
      *
           IF    ( SPA-GMN-FTNHASUKBNG  =   "0" )
               MOVE    "0"             TO  SPA-GMN-FTNHASUTAIKBNG
           END-IF
      *
           .
       4101-FTNHASUKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金記載１円未満端数処理（外来）対象区分チェック処理
      *****************************************************************
       4101-FTNHASUTAIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-FTNHASUTAIKBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-FTNHASUTAIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNHASUTAIKBNG-G
      *
           .
       4101-FTNHASUTAIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金記載１円未満端数処理（入院）チェック処理
      *****************************************************************
       4101-FTNHASUKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-FTNHASUKBNN TO  WRK-CMB-CD
           PERFORM 4200-FTNHASUKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNHASUKBNN-G
      *
           IF    ( SPA-GMN-FTNHASUKBNN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-FTNHASUTAIKBNN
           END-IF
      *
           .
       4101-FTNHASUKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金記載１円未満端数処理（入院）対象区分チェック処理
      *****************************************************************
       4101-FTNHASUTAIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-FTNHASUTAIKBNN
                                       TO  WRK-CMB-CD
           PERFORM 4200-FTNHASUTAIKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNHASUTAIKBNN-G
      *
           .
       4101-FTNHASUTAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    負担金未発生分の回数カウント（外来）チェック処理
      *****************************************************************
       4101-KAICNTKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAICNTKBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-KAICNTKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAICNTKBNG-G
      *
           IF    ( SPA-GMN-KAICNTKBNG  =   "0" )
               MOVE    "0"             TO  SPA-GMN-KAICNTTAIKBNG
           END-IF
      *
           .
       4101-KAICNTKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    負担金未発生分の回数カウント（外来）対象区分チェック処理
      *****************************************************************
       4101-KAICNTTAIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAICNTTAIKBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-KAICNTTAIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAICNTTAIKBNG-G
      *
           .
       4101-KAICNTTAIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    長期併用時の特別計算（外来）チェック処理
      *****************************************************************
       4101-CYOKEIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-CYOKEIKBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-CYOKEIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKEIKBNG-G
      *
           IF    ( SPA-GMN-CYOKEIKBNG  =   "0" )
               MOVE    "0"             TO  SPA-GMN-CYOKEITAIKBNG
           END-IF
      *
           .
       4101-CYOKEIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    長期併用時の特別計算（外来）対象区分チェック処理
      *****************************************************************
       4101-CYOKEITAIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-CYOKEITAIKBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-CYOKEITAIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKEITAIKBNG-G
      *
           .
       4101-CYOKEITAIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    現物給付対象者の特別計算（高齢者・外）チェック処理
      *****************************************************************
       4101-GNBKEIKBNKG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-GNBKEIKBNKG
                                       TO  WRK-CMB-CD
           PERFORM 4200-GNBKEIKBNKGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-GNBKEIKBNKG-G
      *
           IF    ( SPA-GMN-GNBKEIKBNKG =   "0" )
               MOVE    "0"             TO  SPA-GMN-GNBKEITAIKBNKG
           END-IF
      *
           .
       4101-GNBKEIKBNKG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    現物給付対象者の特別計算（高齢者・外）対象区分チェック処理
      *****************************************************************
       4101-GNBKEITAIKBNKG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-GNBKEITAIKBNKG
                                       TO  WRK-CMB-CD
           PERFORM 4200-GNBKEITAIKBNKGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-GNBKEITAIKBNKG-G
      *
           .
       4101-GNBKEITAIKBNKG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の特別計算（外来）チェック処理
      *****************************************************************
       4101-ZENKEIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-ZENKEIKBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-ZENKEIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENKEIKBNG-G
      *
           IF    ( SPA-GMN-ZENKEIKBNG  =   "0" )
               MOVE    "0"             TO  SPA-GMN-ZENKEITAIKBNG
           END-IF
      *
           .
       4101-ZENKEIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の特別計算（外来）対象区分チェック処理
      *****************************************************************
       4101-ZENKEITAIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-ZENKEITAIKBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-ZENKEITAIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENKEITAIKBNG-G
      *
           .
       4101-ZENKEITAIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診察料がない場合の負担金計算（外来）チェック処理
      *****************************************************************
       4101-SINKEIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SINKEIKBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-SINKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SINKEIKBNG-G
      *
           IF    ( SPA-GMN-SINKEIKBNG  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SINKEITAIKBNG
           END-IF
      *
           .
       4101-SINKEIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診察料がない場合の負担金計算（外来）対象区分チェック処理
      *****************************************************************
       4101-SINKEITAIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SINKEITAIKBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-SINKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SINKEITAIKBNG-G
      *
           .
       4101-SINKEITAIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初診時以外の負担金計算（外来）チェック処理
      *****************************************************************
       4101-SYOKEIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SYOKEIKBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYOKEIKBNG-G
      *
           IF    ( SPA-GMN-SYOKEIKBNG  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SYOKEITAIKBNG
           END-IF
      *
           .
       4101-SYOKEIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初診時以外の負担金計算（外来）対象区分チェック処理
      *****************************************************************
       4101-SYOKEITAIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SYOKEITAIKBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYOKEITAIKBNG-G
      *
           .
       4101-SYOKEITAIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初診時以外の負担金計算（入院）チェック処理
      *****************************************************************
       4101-SYOKEIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SYOKEIKBNN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYOKEIKBNN-G
      *
           IF    ( SPA-GMN-SYOKEIKBNN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SYOKEITAIKBNN
           END-IF
      *
           .
       4101-SYOKEIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初診時以外の負担金計算（入院）対象区分チェック処理
      *****************************************************************
       4101-SYOKEITAIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SYOKEITAIKBNN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYOKEITAIKBNN-G
      *
           .
       4101-SYOKEITAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    １入院＠日限度とする特別計算（入院）チェック処理
      *****************************************************************
       4101-NYUKEIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-NYUKEIKBNN
                                       TO  WRK-CMB-CD
           PERFORM 4200-NYUKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NYUKEIKBNN-G
      *
           IF    ( SPA-GMN-NYUKEIKBNN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-NYUKEITAIKBNN
           END-IF
      *
           .
       4101-NYUKEIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    １入院＠日限度とする特別計算（入院）対象区分チェック処理
      *****************************************************************
       4101-NYUKEITAIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-NYUKEITAIKBNN
                                       TO  WRK-CMB-CD
           PERFORM 4200-NYUKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NYUKEITAIKBNN-G
      *
           .
       4101-NYUKEITAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日上限額を固定額とする特別計算（入院）チェック処理
      *****************************************************************
       4101-DAYKEIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-DAYKEIKBNN
                                       TO  WRK-CMB-CD
           PERFORM 4200-DAYKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-DAYKEIKBNN-G
      *
           IF    ( SPA-GMN-DAYKEIKBNN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-DAYKEITAIKBNN
           END-IF
      *
           .
       4101-DAYKEIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日上限額を固定額とする特別計算（入院）対象区分チェック処理
      *****************************************************************
       4101-DAYKEITAIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-DAYKEITAIKBNN
                                       TO  WRK-CMB-CD
           PERFORM 4200-DAYKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-DAYKEITAIKBNN-G
      *
           .
       4101-DAYKEITAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    限度額認定証提示が無い場合の特別計算チェック処理
      *****************************************************************
       4101-NINNASIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-NINNASIKBN  TO  WRK-CMB-CD
           PERFORM 4200-NINNASIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NINNASIKBN-G
      *
           IF    ( SPA-GMN-NINNASIKBN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-NINNASITAIKBN
           END-IF
      *
           .
       4101-NINNASIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    限度額認定証提示が無い場合の特別計算対象区分チェック処理
      *****************************************************************
       4101-NINNASITAIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-NINNASITAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-NINNASITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NINNASITAIKBN-G
      *
           .
       4101-NINNASITAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    限度額認定証（上位）の場合の特別計算チェック処理
      *****************************************************************
       4101-NINJYOKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-NINJYOKBN  TO  WRK-CMB-CD
           PERFORM 4200-NINJYOKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NINJYOKBN-G
      *
           IF    ( SPA-GMN-NINJYOKBN   =   "0" )
               MOVE    "0"             TO  SPA-GMN-NINJYOTAIKBN
           END-IF
      *
           .
       4101-NINJYOKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    限度額認定証（上位）の場合の特別計算対象区分チェック処理
      *****************************************************************
       4101-NINJYOTAIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-NINJYOTAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-NINJYOTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NINJYOTAIKBN-G
      *
           .
       4101-NINJYOTAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ７０歳未満現物給付高額療養費合算計算チェック処理
      *****************************************************************
       4101-KOGAKKEIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KOGAKKEIKBN  TO  WRK-CMB-CD
           PERFORM 4200-KOGAKKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOGAKKEIKBN-G
      *
           IF    ( SPA-GMN-KOGAKKEIKBN   =   "0" )
               MOVE    "0"             TO  SPA-GMN-KOGAKKEITAIKBN
           END-IF
      *
           .
       4101-KOGAKKEIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ７０歳未満現物給付高額療養費合算計算対象区分チェック処理
      *****************************************************************
       4101-KOGAKKEITAIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KOGAKKEITAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-KOGAKKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOGAKKEITAIKBN-G
      *
           .
       4101-KOGAKKEITAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    短期滞在手術等基本料２、３の特別計算チェック処理
      *****************************************************************
       4101-TANKIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-TANKIKBN  TO  WRK-CMB-CD
           PERFORM 4200-TANKIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-TANKIKBN-G
      *
           IF    ( SPA-GMN-TANKIKBN   =   "0" )
               MOVE    "0"             TO  SPA-GMN-TANKITAIKBN
           END-IF
      *
           .
       4101-TANKIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    短期滞在手術等基本料２、３の特別計算対象区分チェック処理
      *****************************************************************
       4101-TANKITAIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-TANKITAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-TANKITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-TANKITAIKBN-G
      *
           .
       4101-TANKITAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    負担金未発生分の回数カウント（入院）区分チェック処理
      *****************************************************************
       4101-KAICNTKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAICNTKBNN  TO  WRK-CMB-CD
           PERFORM 4200-KAICNTKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAICNTKBNN-G
      *
           IF    ( SPA-GMN-KAICNTKBNN   =   "0" )
               MOVE    "0"             TO  SPA-GMN-KAICNTTAIKBNN
           END-IF
      *
           .
       4101-KAICNTKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    負担金未発生分の回数カウント（入院）対象区分チェック処理
      *****************************************************************
       4101-KAICNTTAIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-KAICNTTAIKBNN
                                       TO  WRK-CMB-CD
           PERFORM 4200-KAICNTTAIKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAICNTTAIKBNN-G
      *
           .
       4101-KAICNTTAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    長期併用時の特別計算（入院）区分チェック処理
      *****************************************************************
       4101-CYOKEIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-CYOKEIKBNN  TO  WRK-CMB-CD
           PERFORM 4200-CYOKEIKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKEIKBNN-G
      *
           IF    ( SPA-GMN-CYOKEIKBNN   =   "0" )
               MOVE    "0"             TO  SPA-GMN-CYOKEITAIKBNN
           END-IF
      *
           .
       4101-CYOKEIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    長期併用時の特別計算（入院）対象区分チェック処理
      *****************************************************************
       4101-CYOKEITAIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-CYOKEITAIKBNN
                                       TO  WRK-CMB-CD
           PERFORM 4200-CYOKEITAIKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKEITAIKBNN-G
      *
           .
       4101-CYOKEITAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の日の負担金計算（入院）区分チェック処理
      *****************************************************************
       4101-ZENDAYKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-ZENDAYKBNN  TO  WRK-CMB-CD
           PERFORM 4200-ZENDAYKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENDAYKBNN-G
      *
           IF    ( SPA-GMN-ZENDAYKBNN   =   "0" )
               MOVE    "0"             TO  SPA-GMN-ZENDAYTAIKBNN
           END-IF
      *
           .
       4101-ZENDAYKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の日の負担金計算（入院）対象区分チェック処理
      *****************************************************************
       4101-ZENDAYTAIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-ZENDAYTAIKBNN
                                       TO  WRK-CMB-CD
           PERFORM 4200-ZENDAYTAIKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENDAYTAIKBNN-G
      *
           .
       4101-ZENDAYTAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の日の負担金計算２（入院）区分チェック処理
      *****************************************************************
       4101-ZENDAY2KBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-ZENDAY2KBNN  TO  WRK-CMB-CD
           PERFORM 4200-ZENDAY2KBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENDAY2KBNN-G
      *
           IF    ( SPA-GMN-ZENDAY2KBNN   =   "0" )
               MOVE    "0"             TO  SPA-GMN-ZENDAY2TAIKBNN
           END-IF
      *
           .
       4101-ZENDAY2KBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の日の負担金計算２（入院）対象区分チェック処理
      *****************************************************************
       4101-ZENDAY2TAIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-ZENDAY2TAIKBNN
                                       TO  WRK-CMB-CD
           PERFORM 4200-ZENDAY2TAIKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENDAY2TAIKBNN-G
      *
           .
       4101-ZENDAY2TAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の特別計算２（外来）チェック処理
      *****************************************************************
       4101-ZENKEI2KBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-ZENKEI2KBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-ZENKEI2KBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENKEI2KBNG-G
      *
           IF    ( SPA-GMN-ZENKEI2KBNG  =   "0" )
               MOVE    "0"             TO  SPA-GMN-ZENKEI2TAIKBNG
           END-IF
      *
           .
       4101-ZENKEI2KBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の特別計算２（外来）対象区分チェック処理
      *****************************************************************
       4101-ZENKEI2TAIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-ZENKEI2TAIKBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-ZENKEI2TAIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENKEI2TAIKBNG-G
      *
           .
       4101-ZENKEI2TAIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初診時の負担金計算（外来）チェック処理
      *****************************************************************
       4101-SYOKEI2KBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SYOKEI2KBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKEI2KBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYOKEI2KBNG-G
      *
           IF    ( SPA-GMN-SYOKEI2KBNG  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SYOKEI2TAIKBNG
           END-IF
      *
           .
       4101-SYOKEI2KBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初診時の負担金計算（外来）対象区分チェック処理
      *****************************************************************
       4101-SYOKEI2TAIKBNG-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SYOKEI2TAIKBNG
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKEI2TAIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYOKEI2TAIKBNG-G
      *
           .
       4101-SYOKEI2TAIKBNG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初診時の負担金計算（入院）チェック処理
      *****************************************************************
       4101-SYOKEI2KBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SYOKEI2KBNN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKEI2KBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYOKEI2KBNN-G
      *
           IF    ( SPA-GMN-SYOKEI2KBNN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-SYOKEI2TAIKBNN
           END-IF
      *
           .
       4101-SYOKEI2KBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初診時の負担金計算（入院）対象区分チェック処理
      *****************************************************************
       4101-SYOKEI2TAIKBNN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-SYOKEI2TAIKBNN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKEI2TAIKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYOKEI2TAIKBNN-G
      *
           .
       4101-SYOKEI2TAIKBNN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    限度額認定証提示が無い場合の特別計算（高齢者）チェック処理
      *****************************************************************
       4101-NINNASIKBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-NINNASIKBNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-NINNASIKBNKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NINNASIKBNK-G
      *
           IF    ( SPA-GMN-NINNASIKBNK  =   "0" )
               MOVE    "0"             TO  SPA-GMN-NINNASITAIKBNK
           END-IF
      *
           .
       4101-NINNASIKBNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    限度額認定証提示が無い場合の特別計算（高齢者）対象区分チェック処理
      *****************************************************************
       4101-NINNASITAIKBNK-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-NINNASITAIKBNK
                                       TO  WRK-CMB-CD
           PERFORM 4200-NINNASITAIKBNKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NINNASITAIKBNK-G
      *
           .
       4101-NINNASITAIKBNK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    マル長負担金計算処理区分チェック処理
      *****************************************************************
       4101-CYOKIFTNKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-CYOKIFTNKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-CYOKIFTNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKIFTNKBN-G
      *
           IF    ( SPA-GMN-CYOKIFTNKBN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-CYOKIFTNTAIKBN
           END-IF
      *
           .
       4101-CYOKIFTNKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    マル長負担金計算処理対象区分チェック処理
      *****************************************************************
       4101-CYOKIFTNTAIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-CYOKIFTNTAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-CYOKIFTNTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKIFTNTAIKBN-G
      *
           .
       4101-CYOKIFTNTAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    マル長レセプト記載処理区分チェック処理
      *****************************************************************
       4101-CYOKIKSIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-CYOKIKSIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-CYOKIKSIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKIKSIKBN-G
      *
           IF    ( SPA-GMN-CYOKIKSIKBN  =   "0" )
               MOVE    "0"             TO  SPA-GMN-CYOKIKSITAIKBN
           END-IF
      *
           .
       4101-CYOKIKSIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    マル長レセプト記載処理対象区分チェック処理
      *****************************************************************
       4101-CYOKIKSITAIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-CYOKIKSITAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-CYOKIKSITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKIKSITAIKBN-G
      *
           .
       4101-CYOKIKSITAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効期間開始日
      *****************************************************************
       4101-YUKOSTYMD-CHK-SEC          SECTION.
      *
           MOVE    SPA-NAI-YUKOSTYMD   TO  WRK-YUKOSTYMD
           MOVE    SPACE               TO  SPA-NAI-YUKOSTYMD
      *
      *    日付妥当性チェック＆編集処理
           IF    ( SPA-GMN-YUKOSTYMD   =   SPACE OR "00000000" )
               MOVE    "00000000"  TO  SPA-GMN-YUKOSTYMD
                                       SPA-NAI-YUKOSTYMD
           ELSE
               MOVE    SPA-GMN-YUKOSTYMD
                                       TO  WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               IF  ( FLG-HIZUKECHK     =   ZERO )
                   MOVE    WRK-HENYMDG
                                       TO  SPA-GMN-YUKOSTYMD
                   MOVE    WRK-SYMD    TO  SPA-NAI-YUKOSTYMD
               ELSE
                   MOVE    WRK-YUKOSTYMD
                                       TO  WRK-YMD
                   PERFORM 800-HIZUKE-CHK-SEC
                   MOVE    WRK-HENYMDG
                                       TO  SPA-GMN-YUKOSTYMD
                   MOVE    WRK-SYMD    TO  SPA-NAI-YUKOSTYMD
                   MOVE    "0003"      TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-CUR
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   IF    ( SPA-NAI-YUKOSTYMD (7:2) =   "01" )
                       CONTINUE
                   ELSE
                       MOVE    "0016"  TO  SPA-ERRCD
                       MOVE    4       TO  SPA-GMN-CUR
                   END-IF
               END-IF
      *
           END-IF
      *
           .
      *
       4101-YUKOSTYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効期間終了日
      *****************************************************************
       4101-YUKOEDYMD-CHK-SEC           SECTION.
      *
           MOVE    SPA-NAI-YUKOEDYMD   TO  WRK-YUKOEDYMD
           MOVE    SPACE               TO  SPA-NAI-YUKOEDYMD
      *
      *    日付妥当性チェック＆編集処理
           IF    ( SPA-GMN-YUKOEDYMD   =   SPACE OR "99999999" )
               MOVE    "99999999"      TO  SPA-GMN-YUKOEDYMD
                                           SPA-NAI-YUKOEDYMD
           ELSE
               MOVE    SPA-GMN-YUKOEDYMD
                                       TO  WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               IF  ( FLG-HIZUKECHK     =   ZERO )
                   MOVE    WRK-HENYMDG TO  SPA-GMN-YUKOEDYMD
                   MOVE    WRK-SYMD    TO  SPA-NAI-YUKOEDYMD
               ELSE
                   MOVE    WRK-YUKOEDYMD
                                       TO  WRK-YMD
                   PERFORM 800-HIZUKE-CHK-SEC
                   MOVE    WRK-HENYMDG
                                       TO  SPA-GMN-YUKOEDYMD
                   MOVE    WRK-SYMD    TO  SPA-NAI-YUKOEDYMD
                   MOVE    "0004"      TO  SPA-ERRCD
                   MOVE    5           TO  SPA-GMN-CUR
               END-IF
               IF    ( SPA-ERRCD       =   SPACE )
                   INITIALIZE              STS-AREA-DAY
                   INITIALIZE              LNK-DAY7-AREA
                   MOVE    "71"        TO  LNK-DAY7-IRAI
                   MOVE    SPA-NAI-YUKOEDYMD (1:6)
                                       TO  LNK-DAY7-YM
                   CALL    "ORCSDAY"       USING
                                           STS-AREA-DAY
                                           LNK-DAY7-AREA
                   IF    ( LNK-DAY7-KOYOMI =   SPA-NAI-YUKOEDYMD )
                       CONTINUE
                   ELSE
                       MOVE    "0017"  TO  SPA-ERRCD
                       MOVE    5       TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
      *
       4101-YUKOEDYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号２
      *****************************************************************
       4100-SELNUM2-CHK-SEC            SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           MOVE    W65-SELNUM2     TO  SPA-GMN-SELNUM2
      *
           IF  (   SPA-GMN-SELNUM2 =   ZERO )
               MOVE    2           TO  SPA-GMN-CUR
           ELSE
               IF    ( SPA-GMN-RIREKILST-MAX
                                      >=   SPA-GMN-SELNUM2 )
                   PERFORM 41001-SELNUM2-CHK-SEC
               ELSE
                   MOVE    3       TO  SPA-GMN-CUR
                   MOVE    "1008"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
      *
       4100-SELNUM2-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号２
      *****************************************************************
       41001-SELNUM2-CHK-SEC           SECTION.
      *
           MOVE    SPA-GMN-SELNUM2 TO  SPA-GMN-RIREKILST-SEL
      *
           MOVE    SPA-NAI-KOHNUM-ORG
                                   TO  SEL-HKNNUM
           MOVE    SPA-NAI-TRYUKOSTYMD (SPA-GMN-SELNUM2)
                                   TO  SEL-KJNYMD
      *
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           MOVE    SPA-GMN-RIREKILST-SEL
                                   TO  SPA-GMN-SELNUM2
      *
           MOVE    1               TO  SPA-NAI-INPUT-MODE
           PERFORM 4100-INPUT-AREA-EDIT-SEC
      *
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           .
      *
       41001-SELNUM2-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費リスト
      *****************************************************************
       4100-KOHLST-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-KOHLST-SEL
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST >   SPA-GMN-KOHLST-MAX )
                    OR   ( SPA-GMN-KOHLST-SEL  NOT =   ZERO )
      *
               IF    ( W65-KOHLST-SELECT (IDX-LST) =   "T" )
                   MOVE    IDX-LST     TO  SPA-GMN-KOHLST-SEL
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-KOHLST-SEL  TO  WRK-ZZZ
           MOVE    WRK-ZZZ-G           TO  W65-SELNUM1
           PERFORM 4100-SELNUM1-CHK-SEC
      *
           MOVE    2               TO  SPA-GMN-CUR
      *
           .
       4100-KOHLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    履歴リスト
      *****************************************************************
       4100-RIREKILST-CHK-SEC      SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-RIREKILST-SEL
      *
           PERFORM VARYING IDX-LST     FROM    1   BY  1
                   UNTIL ( IDX-LST    >   20 )
                    OR   ( SPA-GMN-RIREKILST-SEL   NOT =   ZERO )
      *
               IF    ( W65-RIREKILST-SELECT (IDX-LST)  =  "T" )
                   MOVE    IDX-LST TO  SPA-GMN-RIREKILST-SEL
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-RIREKILST-SEL
                                       TO  W65-SELNUM2
           PERFORM 4100-SELNUM2-CHK-SEC
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           .
       4100-RIREKILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力情報領域初期化処理
      *****************************************************************
       4201-INPUT-AREA-CLEAR-SEC       SECTION.
      *
           INITIALIZE  SPA-GMN-INPUT-AREA
                       SPA-NAI-INPUT-AREA
      *
           PERFORM 4201-YUKOKIKAN-RESET-SEC
      *
           PERFORM 42011-COMBO-SET-DEFAULT-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
      *
       4201-INPUT-AREA-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    有効期間初期化処理
      *****************************************************************
       4201-YUKOKIKAN-RESET-SEC        SECTION.
      *
      *    有効期間の開始日と終了日を設定
           MOVE    "00000000"      TO  SPA-GMN-YUKOSTYMD
                                       SPA-NAI-YUKOSTYMD
      *
           MOVE    "99999999"      TO  SPA-GMN-YUKOEDYMD
                                       SPA-NAI-YUKOEDYMD
      *
      *    公費の種類コンボ編集処理
           PERFORM 3110-KOHNUMLST-SEC
      *
           .
      *
       4201-YUKOKIKAN-RESET-EXT.
           EXIT.
      *****************************************************************
      *    コンボボックス初期値設定処理
      *****************************************************************
       42011-COMBO-SET-DEFAULT-SEC     SECTION.
      *
      *    実日数省略記載
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-JNISSUKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-JNISSUKBN-G
      *
      *    請求点省略記載
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-TOTALTENKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-TOTALTENKBN-G
      *
      *    全国公費併用分請求点集計
           MOVE    SPACE           TO  WRK-CMB-CD
           PERFORM 4200-SYUKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SYUKEIKBN-G
           MOVE    WRK-CMB-ITM2    TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SYUKEITAIKBN-G
      *
      *    一部負担金金額集計
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-FTNKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-FTNKEIKBNG-G
                                       SPA-GMN-FTNKEIKBNN-G
      *
      *    一部負担金省略記載
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-FTNMONEYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-FTNMONEYKBNG-G
                                       SPA-GMN-FTNMONEYKBNN-G
      *
      *    一部負担金０円記載（記録）
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-FTNZEROKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-FTNZEROKBNG-G
                                       SPA-GMN-FTNZEROKBNN-G
      *
      *    入院食事療養省略記載
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SKJKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SKJKBN-G
      *
      *    ＯＣＲ実日数請求点省略記載
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-OCRKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-OCRKBN-G
      *
      *    月途中受給者証変更時負担計算
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-JYUHENKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-JYUHENKBN-G
      *
      *    日・月上限額負担計算
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-JGNFTNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-JGNFTNKBN-G
      *
      *    請求書公費再掲欄記載
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SAIKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SAIKEIKBN-G
      *
      *    保険欄−一部負担金記載
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-HKNFTNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-HKNFTNKBNK-G
                                       SPA-GMN-HKNFTNKBNY-G
      *
      *    設定と異なる記載設定
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-RECEKSIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-RECEKSIKBNG-G
                                       SPA-GMN-RECEKSIKBNN-G
      *
      *    設定と異なる記載設定対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-RECEKSITAIKBNG-G
                                       SPA-GMN-RECEKSITAIKBNN-G
      *
      *    チェック対象上限額設定
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-CHKJGNKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-CHKJGNKBNG-G
      *
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-CHKJGNKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-CHKJGNKBNN-G
      *
      *    チェック対象上限額設定対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-CHKJGNTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-CHKJGNTAIKBNG-G
                                       SPA-GMN-CHKJGNTAIKBNN-G
      *
      *    公費請求テーブル作成
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-KOHSKYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-KOHSKYKBNG-G
                                       SPA-GMN-KOHSKYKBNN-G
      *
      *    患者登録−所得者情報−月上限額入力
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-PTJGNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-PTJGNKBN-G
      *
      *    患者登録−所得者情報−月上限額入力対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-PTJGNTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-PTJGNTAIKBN-G
      *
      *    医療費負担金計算
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-IRYOKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-IRYOKEIKBNG-G
                                       SPA-GMN-IRYOKEIKBNN-G
      *
      *    医療費負担金計算対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-IRYOKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-IRYOKEITAIKBNG-G
                                       SPA-GMN-IRYOKEITAIKBNN-G
      *
      *    食事負担金計算
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SKJKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SKJKEIKBN-G
      *
      *    食事負担金計算対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SKJKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SKJKEITAIKBN-G
      *
      *    設定額を助成する負担金計算（外来）
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-KINJYOKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-KINJYOKBNG-G
      *
      *    設定額を助成する負担金計算（外来）対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-KINJYOTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-KINJYOTAIKBNG-G
      *
      *    設定額を助成する負担金計算（入院）
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-KINJYOKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-KINJYOKBNN-G
      *
      *    設定額を助成する負担金計算（入院）対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-KINJYOTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-KINJYOTAIKBNN-G
      *
      *    「月−院内−上限額」参照条件（外来）
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-INNAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-INNAIKBN-G
      *
      *    「月−院内−上限額」参照条件（外来）対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-INNAITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-INNAITAIKBN-G
      *
      *    自立支援医療の特別計算
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-JIRITUKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-JIRITUKBN-G
      *
      *    自立支援医療の特別計算対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-JIRITUTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-JIRITUTAIKBN-G
      *
      *    食事療養費付加設定
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SKJRYOKBNHON-G
                                       SPA-GMN-SEIRYOKBNHON-G
                                       SPA-GMN-SEIKANKBNHON-G
                                       SPA-GMN-SKJRYOKBNTSY-G
                                       SPA-GMN-SEIRYOKBNTSY-G
                                       SPA-GMN-SEIKANKBNTSY-G
                                       SPA-GMN-SKJRYOKBNTNK-G
                                       SPA-GMN-SEIRYOKBNTNK-G
                                       SPA-GMN-SEIKANKBNTNK-G
      *
      *    食事療養費付加設定対象区分本人
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNHONLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SKJRYOTAIKBNHON-G
                                       SPA-GMN-SEIRYOTAIKBNHON-G
                                       SPA-GMN-SEIKANTAIKBNHON-G
      *
      *    食事療養費付加設定対象区分低所得
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTSYLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SKJRYOTAIKBNTSY-G
                                       SPA-GMN-SEIRYOTAIKBNTSY-G
                                       SPA-GMN-SEIKANTAIKBNTSY-G
      *
      *    食事療養費付加設定対象区分低年金
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SKJFUKATAIKBNTNKLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SKJRYOTAIKBNTNK-G
                                       SPA-GMN-SEIRYOTAIKBNTNK-G
                                       SPA-GMN-SEIKANTAIKBNTNK-G
      *
      *    共通対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-COMMONTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-JNISSUTAIKBN-G
                                       SPA-GMN-TOTALTENTAIKBN-G
      *                                SPA-GMN-SYUKEITAIKBN-G
                                       SPA-GMN-FTNKEITAIKBNG-G
                                       SPA-GMN-FTNKEITAIKBNN-G
                                       SPA-GMN-FTNMONEYTAIKBNG-G
                                       SPA-GMN-FTNMONEYTAIKBNN-G
                                       SPA-GMN-FTNZEROTAIKBNG-G
                                       SPA-GMN-FTNZEROTAIKBNN-G
                                       SPA-GMN-SKJTAIKBN-G
                                       SPA-GMN-OCRTAIKBN-G
                                       SPA-GMN-HKNFTNTAIKBNK-G
                                       SPA-GMN-HKNFTNTAIKBNY-G
                                       SPA-GMN-RECEKSITAIKBNG-G
                                       SPA-GMN-RECEKSITAIKBNN-G
                                       SPA-GMN-KOHSKYTAIKBNG-G
                                       SPA-GMN-KOHSKYTAIKBNN-G
      *
                                       SPA-GMN-KAKTAIKBNK-G
                                       SPA-GMN-KAKTAIKBNY-G
                                       SPA-GMN-KAK21TAIKBNK-G
                                       SPA-GMN-KAK21TAIKBNY-G
                                       SPA-GMN-KAK22TAIKBNK-G
                                       SPA-GMN-KAK22TAIKBNY-G
                                       SPA-GMN-KAK23TAIKBNK-G
                                       SPA-GMN-KAK23TAIKBNY-G
                                       SPA-GMN-KAK31TAIKBNK-G
                                       SPA-GMN-KAK31TAIKBNY-G
                                       SPA-GMN-KAK32TAIKBNK-G
                                       SPA-GMN-KAK32TAIKBNY-G
                                       SPA-GMN-KAK41TAIKBNK-G
                                       SPA-GMN-KAK41TAIKBNY-G
                                       SPA-GMN-KAK42TAIKBNK-G
                                       SPA-GMN-KAK42TAIKBNY-G
                                       SPA-GMN-KAK43TAIKBNK-G
                                       SPA-GMN-KAK43TAIKBNY-G
                                       SPA-GMN-KSINGTAIKBNKG-G
                                       SPA-GMN-KSINGTAIKBNKN-G
                                       SPA-GMN-KSINGTAIKBNYG-G
                                       SPA-GMN-KSINGTAIKBNYN-G
                                       SPA-GMN-ENCHGTAIKBNKG-G
                                       SPA-GMN-ENCHGTAIKBNKN-G
                                       SPA-GMN-ENCHGTAIKBNYG-G
                                       SPA-GMN-ENCHGTAIKBNYN-G
                                       SPA-GMN-KOH2SKJTAIKBN-G
      *
      *    請求書公費再掲欄記載対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SAIKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SAIKEITAIKBN-G
      *
      *    月途中受給者証変更時負担計算対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-JYUHENTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-JYUHENTAIKBN-G
      *
      *    保険欄の括弧再掲記載
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-KAKKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAKKBNK-G
                                           SPA-GMN-KAKKBNY-G
      *
      *    保険欄の括弧再掲（下）記載
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-KAK20KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK21KBNK-G
                                           SPA-GMN-KAK21KBNY-G
                                           SPA-GMN-KAK22KBNK-G
                                           SPA-GMN-KAK22KBNY-G
                                           SPA-GMN-KAK23KBNK-G
                                           SPA-GMN-KAK23KBNY-G
      *
      *    保険欄の括弧再掲（下）金額計算
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-KAK31KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK31KBNK-G
                                           SPA-GMN-KAK31KBNY-G
      *
      *    保険欄の括弧再掲（下）金額計算
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-KAK32KBNKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK32KBNK-G
      *
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-KAK32KBNYLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK32KBNY-G
      *
      *    保険欄の括弧再掲（下）金額加算
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-KAK40KBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAK41KBNK-G
                                           SPA-GMN-KAK41KBNY-G
                                           SPA-GMN-KAK42KBNK-G
                                           SPA-GMN-KAK42KBNY-G
                                           SPA-GMN-KAK43KBNK-G
                                           SPA-GMN-KAK43KBNY-G
      *
      *    保険欄−一部負担金無記載
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-KSINGKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KSINGKBNKG-G
                                           SPA-GMN-KSINGKBNKN-G
                                           SPA-GMN-KSINGKBNYG-G
                                           SPA-GMN-KSINGKBNYN-G
      *
      *    一部負担金１円→１０円単位記載
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-ENCHGKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ENCHGKBNKG-G
                                           SPA-GMN-ENCHGKBNKN-G
                                           SPA-GMN-ENCHGKBNYG-G
                                           SPA-GMN-ENCHGKBNYN-G
      *
      *    入院時食事療養記載（公費２欄置き換え）
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-KOH2SKJKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOH2SKJKBN-G
      *
      *    一部負担金記載１円未満端数（外来）
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-FTNHASUKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNHASUKBNG-G
      *
      *    一部負担金記載１円未満端数対象区分（外来）
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-FTNHASUTAIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNHASUTAIKBNG-G
      *
      *    一部負担金記載１円未満端数（入院）
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-FTNHASUKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNHASUKBNN-G
      *
      *    一部負担金記載１円未満端数対象区分（入院）
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-FTNHASUTAIKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-FTNHASUTAIKBNN-G
      *
      *    負担金未発生分の回数カウント（外来）
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-KAICNTKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAICNTKBNG-G
      *
      *    負担金未発生分の回数カウント（外来）対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-KAICNTTAIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAICNTTAIKBNG-G
      *
      *    長期併用時の特別計算（外来）
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-CYOKEIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKEIKBNG-G
      *
      *    長期併用時の特別計算（外来）対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-CYOKEITAIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKEITAIKBNG-G
      *
      *    現物給付対象者の特別計算（高齢者・外）
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-GNBKEIKBNKGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-GNBKEIKBNKG-G
      *
      *    現物給付対象者の特別計算（高齢者・外）対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-GNBKEITAIKBNKGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-GNBKEITAIKBNKG-G
      *
      *    全国公費併用時の特別計算（外来）
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-ZENKEIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENKEIKBNG-G
      *
      *    全国公費併用時の特別計算（外来）対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-ZENKEITAIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENKEITAIKBNG-G
      *
      *    診察料がない場合の負担金計算（外来）
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SINKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SINKEIKBNG-G
      *
      *    診察料がない場合の負担金計算（外来）対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SINKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SINKEITAIKBNG-G
      *
      *    初診時以外の負担金計算
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SYOKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYOKEIKBNG-G
                                           SPA-GMN-SYOKEIKBNN-G
      *    初診時以外の負担金計算対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SYOKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYOKEITAIKBNG-G
                                           SPA-GMN-SYOKEITAIKBNN-G
      *
      *    １入院＠日限度とする特別計算（入院）
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-NYUKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NYUKEIKBNN-G
      *
      *    １入院＠日限度とする特別計算（入院）対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-NYUKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NYUKEITAIKBNN-G
      *
      *    日上限額を固定額とする特別計算（入院）
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-DAYKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-DAYKEIKBNN-G
      *
      *    日上限額を固定額とする特別計算（入院）対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-DAYKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-DAYKEITAIKBNN-G
      *
      *    限度額認定証提示が無い場合の特別計算
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-NINNASIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NINNASIKBN-G
      *
      *    限度額認定証提示が無い場合の特別計算対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-NINNASITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NINNASITAIKBN-G
      *
      *    限度額認定証（上位）の場合の特別計算
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-NINJYOKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NINJYOKBN-G
      *
      *    限度額認定証（上位）の場合の特別計算対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-NINJYOTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NINJYOTAIKBN-G
      *
      *    ７０歳未満現物給付高額療養費合算計算
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-KOGAKKEIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOGAKKEIKBN-G
      *
      *    ７０歳未満現物給付高額療養費合算計算対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-KOGAKKEITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KOGAKKEITAIKBN-G
      *
      *    短期滞在手術等基本料２、３の特別計算
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-TANKIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-TANKIKBN-G
      *
      *    短期滞在手術等基本料２、３の特別計算対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-TANKITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-TANKITAIKBN-G
      *
      *    負担金未発生分の回数カウント（入院）区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-KAICNTKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAICNTKBNN-G
      *
      *    負担金未発生分の回数カウント（入院）対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-KAICNTTAIKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KAICNTTAIKBNN-G
      *
      *    長期併用時の特別計算（入院）区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-CYOKEIKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKEIKBNN-G
      *
      *    長期併用時の特別計算（入院）対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-CYOKEITAIKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKEITAIKBNN-G
      *
      *    全国公費併用時の日の負担金計算（入院）区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-ZENDAYKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENDAYKBNN-G
      *
      *    全国公費併用時の日の負担金計算（入院）対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-ZENDAYTAIKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENDAYTAIKBNN-G
      *
      *    全国公費併用時の日の負担金計算２（入院）区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-ZENDAY2KBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENDAY2KBNN-G
      *
      *    全国公費併用時の日の負担金計算２（入院）対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-ZENDAY2TAIKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENDAY2TAIKBNN-G
      *
      *    全国公費併用時の特別計算２（外来）
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-ZENKEI2KBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENKEI2KBNG-G
      *
      *    全国公費併用時の特別計算２（外来）対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-ZENKEI2TAIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-ZENKEI2TAIKBNG-G
      *
      *    初診時の負担金計算（外来）
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SYOKEI2KBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYOKEI2KBNG-G
      *
      *    初診時の負担金計算（外来）対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SYOKEI2TAIKBNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYOKEI2TAIKBNG-G
      *
      *    初診時の負担金計算（入院）
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SYOKEI2KBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYOKEI2KBNN-G
      *
      *    初診時の負担金計算（入院）対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-SYOKEI2TAIKBNNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYOKEI2TAIKBNN-G
      *
      *    限度額認定証提示が無い場合の特別計算（高齢者）
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-NINNASIKBNKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NINNASIKBNK-G
      *
      *    限度額認定証提示が無い場合の特別計算（高齢者）対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-NINNASITAIKBNKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NINNASITAIKBNK-G
      *
      *    マル長負担金計算処理
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-CYOKIFTNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKIFTNKBN-G
      *
      *    マル長負担金計算処理対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-CYOKIFTNTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKIFTNTAIKBN-G
      *
      *    マル長負担金計算処理区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-CYOKIKSIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKIKSIKBN-G
      *
      *    マル長負担金計算処理対象区分
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-CYOKIKSITAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CYOKIKSITAIKBN-G
      *
           .
       42011-COMBO-SET-DEFAULT-EXT.
           EXIT.
      *****************************************************************
      *    コンボボックス初期値設定処理（Ver4.7.0追加分）
      *****************************************************************
       42012-COMBO-SET-DEFAULT-SEC     SECTION.
      *
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-NARA )
                 AND ( THKN-HBTNUM      =   "71" OR "81" OR "91" OR
                                            "80"      )
      *        WHEN  ( SPA-PREFNUM     =   CONST-SHIMANE )
      *          AND ( SPA-GMN-KOHNUM  =   "790"         )
               WHEN  ( SPA-PREFNUM     =   CONST-YAMAGUCHI )
                 AND ( SPA-GMN-KOHNUM  =   "781" )
      *        WHEN  ( SPA-PREFNUM     =   CONST-TOKUSHIMA )
      *          AND ( SPA-GMN-KOHNUM  =   "245" )
               WHEN  ( SPA-PREFNUM     =   CONST-KOCHI )
                 AND ( THKN-HBTNUM      =   "72" )
                   MOVE    "1"         TO  SPA-GMN-FTNKEIKBNG
                   MOVE    "1"         TO  SPA-GMN-FTNKEITAIKBNG
               WHEN  ( SPA-PREFNUM     =   CONST-MIYAZAKI )
                 AND ( SPA-GMN-KOHNUM  =   "190" )
                   MOVE    "2"         TO  SPA-GMN-FTNKEIKBNG
                   MOVE    "1"         TO  SPA-GMN-FTNKEITAIKBNG
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-NARA )
                 AND ( THKN-HBTNUM      =   "71" OR "81" OR "91" OR
                                            "80"      )
      *        WHEN  ( SPA-PREFNUM     =   CONST-SHIMANE )
      *          AND ( SPA-GMN-KOHNUM  =   "790"         )
               WHEN  ( SPA-PREFNUM     =   CONST-YAMAGUCHI )
                 AND ( SPA-GMN-KOHNUM  =   "781" )
      *        WHEN  ( SPA-PREFNUM     =   CONST-TOKUSHIMA )
      *          AND ( SPA-GMN-KOHNUM  =   "245" )
               WHEN  ( SPA-PREFNUM     =   CONST-KOCHI )
                 AND ( THKN-HBTNUM      =   "72" )
                   MOVE    "1"         TO  SPA-GMN-FTNKEIKBNN
                   MOVE    "1"         TO  SPA-GMN-FTNKEITAIKBNN
               WHEN  ( SPA-PREFNUM     =   CONST-MIYAZAKI )
                 AND ( SPA-GMN-KOHNUM  =   "190" )
                   MOVE    "2"         TO  SPA-GMN-FTNKEIKBNN
                   MOVE    "1"         TO  SPA-GMN-FTNKEITAIKBNN
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-NAGASAKI )
                AND  ( SPA-GMN-KOHNUM  =   "186" )
                   MOVE    "1"         TO  SPA-GMN-HKNFTNKBNK
                   MOVE    "0"         TO  SPA-GMN-HKNFTNTAIKBNK
               WHEN    OTHER
                   MOVE    "1"         TO  SPA-GMN-HKNFTNKBNK
                   MOVE    "2"         TO  SPA-GMN-HKNFTNTAIKBNK
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-NAGASAKI )
                AND  ( SPA-GMN-KOHNUM  =   "186" )
                   MOVE    "1"         TO  SPA-GMN-HKNFTNKBNY
                   MOVE    "0"         TO  SPA-GMN-HKNFTNTAIKBNY
               WHEN    OTHER
                   MOVE    "1"         TO  SPA-GMN-HKNFTNKBNY
                   MOVE    "2"         TO  SPA-GMN-HKNFTNTAIKBNY
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-NAGASAKI )
                AND  ( SPA-GMN-KOHNUM  =   "287" OR "380" 
                                        OR "281" OR "282"
                                        OR "188"          )
                   MOVE    "2"         TO  SPA-GMN-RECEKSIKBNG
                   MOVE    "2"         TO  SPA-GMN-RECEKSITAIKBNG
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "481" OR "482"
                                        OR "483" OR "383" )
                   MOVE    "1"         TO  SPA-GMN-RECEKSIKBNN
                   MOVE    "0"         TO  SPA-GMN-RECEKSITAIKBNN
               WHEN  ( SPA-PREFNUM     =   CONST-NAGASAKI )
                AND  ( SPA-GMN-KOHNUM  =   "287" OR "380" 
                                        OR "281" OR "282" )
                   MOVE    "2"         TO  SPA-GMN-RECEKSIKBNN
                   MOVE    "2"         TO  SPA-GMN-RECEKSITAIKBNN
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-TOKYO )
                AND  ( SPA-GMN-KOHNUM  =   "588" )
                   MOVE    "1"         TO  SPA-GMN-CHKJGNKBNG
                   MOVE    "0"         TO  SPA-GMN-CHKJGNTAIKBNG
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "683" OR "783" )
               WHEN  ( SPA-PREFNUM     =   CONST-SHIGA )
               WHEN  ( SPA-PREFNUM     =   CONST-FUKUOKA )
               WHEN  ( SPA-PREFNUM     =   CONST-NAGASAKI )
                AND  ( SPA-GMN-KOHNUM  =   "182" OR "282" )
                   MOVE    "1"         TO  SPA-GMN-CHKJGNKBNN
                   MOVE    "0"         TO  SPA-GMN-CHKJGNTAIKBNN
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-NIIGATA )
                AND  ( THKN-HBTNUM      =   "80" OR "82" OR "84"
                                        OR "86" OR "00" )
               WHEN  ( SPA-PREFNUM     =   CONST-NARA )
                AND  ( THKN-HBTNUM      =   "71" OR "81" OR "91" OR
                                            "80"      )
                   MOVE    "1"         TO  SPA-GMN-KOHSKYKBNG
                   MOVE    "1"         TO  SPA-GMN-KOHSKYTAIKBNG
               WHEN  ( SPA-PREFNUM     =   CONST-FUKUOKA )
                AND  ( THKN-HBTNUM NOT =   "41" )
                   MOVE    "1"         TO  SPA-GMN-KOHSKYKBNG
                   MOVE    "3"         TO  SPA-GMN-KOHSKYTAIKBNG
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-NARA )
                AND  ( THKN-HBTNUM      =   "71" OR "81" OR "91" OR
                                            "80"      )
                   MOVE    "1"         TO  SPA-GMN-KOHSKYKBNN
                   MOVE    "1"         TO  SPA-GMN-KOHSKYTAIKBNN
               WHEN  ( SPA-PREFNUM     =   CONST-FUKUOKA )
                AND  ( THKN-HBTNUM NOT =   "41" )
                   MOVE    "1"         TO  SPA-GMN-KOHSKYKBNN
                   MOVE    "3"         TO  SPA-GMN-KOHSKYTAIKBNN
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-IBARAKI )
                AND  ( SPA-GMN-KOHNUM  =   "152" )
               WHEN  ( SPA-PREFNUM     =   CONST-HYOGO )
                AND  ( SPA-GMN-KOHNUM  =   "186" )
                   MOVE    "1"         TO  SPA-GMN-PTJGNKBN
                   MOVE    "0"         TO  SPA-GMN-PTJGNTAIKBN
               WHEN  ( SPA-PREFNUM     =   CONST-OSAKA )
                AND  ( SPA-GMN-KOHNUM  =   "192" )
               WHEN  ( SPA-PREFNUM     =   CONST-FUKUSHIMA )
                AND  ( SPA-GMN-KOHNUM  =   "281" )
                   MOVE    "2"         TO  SPA-GMN-PTJGNKBN
                   MOVE    "0"         TO  SPA-GMN-PTJGNTAIKBN
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-FUKUSHIMA )
                AND  ( SPA-GMN-KOHNUM  =   "245" OR "246" OR "445"
                                        OR "545" OR "645" OR "480" )
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "343" OR "342" OR "181"
                                        OR "182" OR "183" OR "481"
                                        OR "482" OR "383" OR "543"
                                        OR "582" OR "743" OR "442"
                                        OR "781" OR "542" OR "681"
                                        OR "682" OR "381" OR "372"
                                        OR "373" OR "462"
                                        OR "562" OR "662" OR "762"
                                        OR "672" )
               WHEN  ( SPA-PREFNUM     =   CONST-KUMAMOTO )
                AND  ( SPA-GMN-KOHNUM  =   "341" OR "442" OR "480" )
                   MOVE    "1"         TO  SPA-GMN-IRYOKEIKBNG
                   MOVE    "0"         TO  SPA-GMN-IRYOKEITAIKBNG
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "344" OR "483" OR "783"
                                        OR "683" )
                   MOVE    "1"         TO  SPA-GMN-IRYOKEIKBNG
                   MOVE    "1"         TO  SPA-GMN-IRYOKEITAIKBNG
               END-EVALUATE
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-FUKUSHIMA )
                AND  ( SPA-GMN-KOHNUM  =   "245" OR "246" OR "445"
                                        OR "545" OR "645" OR "480" )
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "181" OR "182" OR "183"
                                        OR "543" OR "582" OR "743"
                                        OR "442" OR "781" OR "542"
                                        OR "681" OR "682" OR "381"
                                        OR "372" OR "373"
                                        OR "462" OR "762" OR "672" )
               WHEN  ( SPA-PREFNUM     =   CONST-KUMAMOTO )
                AND  ( SPA-GMN-KOHNUM  =   "341" OR "442" )
                   MOVE    "1"         TO  SPA-GMN-IRYOKEIKBNN
                   MOVE    "0"         TO  SPA-GMN-IRYOKEITAIKBNN
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "783" OR "683" )
                   MOVE    "1"         TO  SPA-GMN-IRYOKEIKBNN
                   MOVE    "1"         TO  SPA-GMN-IRYOKEITAIKBNN
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-FUKUSHIMA )
                AND  ( SPA-GMN-KOHNUM  =   "445" OR "545" OR "645"
                                        OR "480" )
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =            "582" OR "743"
                                        OR "442" OR "781" OR "542"
                                        OR "381" OR "372" OR "373"
                                        OR "672" )
                   MOVE    "1"         TO  SPA-GMN-SKJKEIKBN
                   MOVE    "0"         TO  SPA-GMN-SKJKEITAIKBN
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "682" )
                   MOVE    "2"         TO  SPA-GMN-SKJKEIKBN
                   MOVE    "0"         TO  SPA-GMN-SKJKEITAIKBN
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "683" )
                   MOVE    "2"         TO  SPA-GMN-SKJKEIKBN
                   MOVE    "1"         TO  SPA-GMN-SKJKEITAIKBN
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-TOKYO )
                AND  ( SPA-GMN-KOHNUM  =   "182" OR "382" )
               WHEN  ( SPA-PREFNUM     =   CONST-KANAGAWA )
                AND  ( SPA-GMN-KOHNUM  =   "182" )
                   MOVE    "1"         TO  SPA-GMN-KINJYOKBNG
                   MOVE    "0"         TO  SPA-GMN-KINJYOTAIKBNG
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-TOKYO )
                AND  ( SPA-GMN-KOHNUM  =   "182" OR "382" )
               WHEN  ( SPA-PREFNUM     =   CONST-KANAGAWA )
                AND  ( SPA-GMN-KOHNUM  =   "182" )
                   MOVE    "1"         TO  SPA-GMN-KINJYOKBNN
                   MOVE    "0"         TO  SPA-GMN-KINJYOTAIKBNN
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "672" )
                   MOVE    "1"         TO  SPA-GMN-SKJRYOKBNHON
                   MOVE    "0"         TO  SPA-GMN-SKJRYOTAIKBNHON
               WHEN  ( SPA-PREFNUM     =   CONST-MIYAGI )
                AND  ( SPA-GMN-KOHNUM  =   "183" OR "483" OR "583"
                                        OR "783" OR "263" OR "363"
                                        OR "463" OR "563")
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "683" )
                   MOVE    "2"         TO  SPA-GMN-SKJRYOKBNHON
                   MOVE    "0"         TO  SPA-GMN-SKJRYOTAIKBNHON
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "683" )
                   MOVE    "2"         TO  SPA-GMN-SEIRYOKBNHON
                   MOVE    "0"         TO  SPA-GMN-SEIRYOTAIKBNHON
               WHEN  ( SPA-PREFNUM     =   CONST-GUNMA )
                AND  ( SPA-GMN-KOHNUM  =   "143" OR "243" )
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "672" )
               WHEN  ( SPA-PREFNUM     =   CONST-NIIGATA )
                AND  ( SPA-GMN-KOHNUM  =   "261" OR "361" OR "561"
                                        OR "264" OR "364" OR "564"
                                        OR "298" )
                   MOVE    "3"         TO  SPA-GMN-SEIRYOKBNHON
                   MOVE    "0"         TO  SPA-GMN-SEIRYOTAIKBNHON
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "683" )
                   MOVE    "2"         TO  SPA-GMN-SEIKANKBNHON
                   MOVE    "0"         TO  SPA-GMN-SEIKANTAIKBNHON
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "672" )
               WHEN  ( SPA-PREFNUM     =   CONST-GUNMA )
                AND  ( SPA-GMN-KOHNUM  =   "143" OR "243" )
               WHEN  ( SPA-PREFNUM     =   CONST-NIIGATA )
                AND  ( SPA-GMN-KOHNUM  =   "261" OR "361" OR "561"
                                        OR "264" OR "364" OR "564"
                                        OR "298" )
                   MOVE    "3"         TO  SPA-GMN-SEIKANKBNHON
                   MOVE    "0"         TO  SPA-GMN-SEIKANTAIKBNHON
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-NIIGATA )
                AND  ( SPA-GMN-KOHNUM  =   "198" )
                   MOVE    "3"         TO  SPA-GMN-SEIRYOKBNTSY
                   MOVE    "0"         TO  SPA-GMN-SEIRYOTAIKBNTSY
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-NIIGATA )
                AND  ( SPA-GMN-KOHNUM  =   "198" )
                   MOVE    "3"         TO  SPA-GMN-SEIKANKBNTSY
                   MOVE    "0"         TO  SPA-GMN-SEIKANTAIKBNTSY
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-NIIGATA )
                AND  ( SPA-GMN-KOHNUM  =   "198" )
                   MOVE    "3"         TO  SPA-GMN-SEIRYOKBNTNK
                   MOVE    "0"         TO  SPA-GMN-SEIRYOTAIKBNTNK
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-NIIGATA )
                AND  ( SPA-GMN-KOHNUM  =   "198" )
                   MOVE    "3"         TO  SPA-GMN-SEIKANKBNTNK
                   MOVE    "0"         TO  SPA-GMN-SEIKANTAIKBNTNK
               END-EVALUATE
      ***
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-HOKKAIDO )
                AND  ( SPA-GMN-KOHNUM  =   "183" )
                   MOVE    "1"         TO  SPA-GMN-KAKKBNK
                   MOVE    "0"         TO  SPA-GMN-KAKTAIKBNK
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-OSAKA )
                   MOVE    "1"         TO  SPA-GMN-KAK21KBNK
                   MOVE    "0"         TO  SPA-GMN-KAK21TAIKBNK
               WHEN  ( SPA-PREFNUM     =   CONST-EHIME )
                   MOVE    "1"         TO  SPA-GMN-KAK21KBNK
                   MOVE    "1"         TO  SPA-GMN-KAK21TAIKBNK
               WHEN  ( SPA-PREFNUM     =   CONST-AICHI )
               WHEN  ( SPA-PREFNUM     =   CONST-HYOGO )
                   MOVE    "1"         TO  SPA-GMN-KAK21KBNK
                   MOVE    "4"         TO  SPA-GMN-KAK21TAIKBNK
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-OSAKA )
                   MOVE    "1"         TO  SPA-GMN-KAK22KBNK
                   MOVE    "1"         TO  SPA-GMN-KAK22TAIKBNK
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-OSAKA )
               WHEN  ( SPA-PREFNUM     =   CONST-EHIME )
                   MOVE    "1"         TO  SPA-GMN-KAK23KBNK
                   MOVE    "1"         TO  SPA-GMN-KAK23TAIKBNK
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-EHIME )
                   MOVE    "1"         TO  SPA-GMN-KAK31KBNK
                   MOVE    "1"         TO  SPA-GMN-KAK31TAIKBNK
               WHEN  ( SPA-PREFNUM     =   CONST-GUNMA )
               WHEN  ( SPA-PREFNUM     =   CONST-AICHI )
               WHEN  ( SPA-PREFNUM     =   CONST-OSAKA )
               WHEN  ( SPA-PREFNUM     =   CONST-HYOGO )
               WHEN  ( SPA-PREFNUM     =   CONST-HIROSHIMA )
               WHEN  ( SPA-PREFNUM     =   CONST-MIYAZAKI )
                   MOVE    "1"         TO  SPA-GMN-KAK31KBNK
                   MOVE    "4"         TO  SPA-GMN-KAK31TAIKBNK
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-HIROSHIMA )
                   MOVE    "3"         TO  SPA-GMN-KAK32KBNK
                   MOVE    "1"         TO  SPA-GMN-KAK32TAIKBNK
               WHEN  ( SPA-PREFNUM     =   CONST-EHIME )
                   MOVE    "2"         TO  SPA-GMN-KAK32KBNK
                   MOVE    "1"         TO  SPA-GMN-KAK32TAIKBNK
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-OSAKA )
                   MOVE    "1"         TO  SPA-GMN-KAK41KBNK
                   MOVE    "1"         TO  SPA-GMN-KAK41TAIKBNK
               WHEN  ( SPA-PREFNUM     =   CONST-AICHI )
                   MOVE    "1"         TO  SPA-GMN-KAK41KBNK
                   MOVE    "4"         TO  SPA-GMN-KAK41TAIKBNK
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-OSAKA )
                   MOVE    "1"         TO  SPA-GMN-KAK42KBNK
                   MOVE    "1"         TO  SPA-GMN-KAK42TAIKBNK
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-OSAKA )
                   MOVE    "1"         TO  SPA-GMN-KAK43KBNK
                   MOVE    "1"         TO  SPA-GMN-KAK43TAIKBNK
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-HOKKAIDO )
                AND  ( SPA-GMN-KOHNUM  =   "183" )
                   MOVE    "1"         TO  SPA-GMN-KAKKBNY
                   MOVE    "0"         TO  SPA-GMN-KAKTAIKBNY
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-OSAKA )
                   MOVE    "1"         TO  SPA-GMN-KAK21KBNY
                   MOVE    "0"         TO  SPA-GMN-KAK21TAIKBNY
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-HIROSHIMA )
                   MOVE    "1"         TO  SPA-GMN-KAK32KBNY
                   MOVE    "3"         TO  SPA-GMN-KAK32TAIKBNY
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-HOKKAIDO )
                AND  ( SPA-GMN-KOHNUM  =   "183" )
               WHEN  ( SPA-PREFNUM     =   CONST-OSAKA )
               WHEN  ( SPA-PREFNUM     =   CONST-OKAYAMA )
                   MOVE    "2"         TO  SPA-GMN-ENCHGKBNKG
                   MOVE    "0"         TO  SPA-GMN-ENCHGTAIKBNKG
               WHEN  ( SPA-PREFNUM     =   CONST-TOKYO )
                AND  ( SPA-GMN-KOHNUM  =   "180" OR "380"
                                        OR "181" OR "381" )
                   MOVE    "3"         TO  SPA-GMN-ENCHGKBNKG
                   MOVE    "0"         TO  SPA-GMN-ENCHGTAIKBNKG
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-TOKUSHIMA )
                AND  ( SPA-GMN-KOHNUM  =   "245" )
                   MOVE    "1"         TO  SPA-GMN-ENCHGKBNKN
                   MOVE    "1"         TO  SPA-GMN-ENCHGTAIKBNKN
               WHEN  ( SPA-PREFNUM     =   CONST-HOKKAIDO )
                AND  ( SPA-GMN-KOHNUM  =   "183" )
               WHEN  ( SPA-PREFNUM     =   CONST-OSAKA )
               WHEN  ( SPA-PREFNUM     =   CONST-OKAYAMA )
                   MOVE    "2"         TO  SPA-GMN-ENCHGKBNKN
                   MOVE    "0"         TO  SPA-GMN-ENCHGTAIKBNKN
               WHEN  ( SPA-PREFNUM     =   CONST-TOKYO )
                AND  ( SPA-GMN-KOHNUM  =   "180" OR "380"
                                        OR "181" OR "381" )
                   MOVE    "3"         TO  SPA-GMN-ENCHGKBNKN
                   MOVE    "0"         TO  SPA-GMN-ENCHGTAIKBNKN
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-HOKKAIDO )
                AND  ( SPA-GMN-KOHNUM  =   "183" )
               WHEN  ( SPA-PREFNUM     =   CONST-OSAKA )
                   MOVE    "2"         TO  SPA-GMN-ENCHGKBNYG
                   MOVE    "0"         TO  SPA-GMN-ENCHGTAIKBNYG
               WHEN  ( SPA-PREFNUM     =   CONST-TOKYO )
                AND  ( SPA-GMN-KOHNUM  =   "180" OR "380"
                                        OR "181" OR "381" )
                   MOVE    "3"         TO  SPA-GMN-ENCHGKBNYG
                   MOVE    "0"         TO  SPA-GMN-ENCHGTAIKBNYG
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-TOKUSHIMA )
                AND  ( SPA-GMN-KOHNUM  =   "245" )
                   MOVE    "1"         TO  SPA-GMN-ENCHGKBNYN
                   MOVE    "3"         TO  SPA-GMN-ENCHGTAIKBNYN
               WHEN  ( SPA-PREFNUM     =   CONST-HOKKAIDO )
                AND  ( SPA-GMN-KOHNUM  =   "183" )
               WHEN  ( SPA-PREFNUM     =   CONST-OSAKA )
                   MOVE    "2"         TO  SPA-GMN-ENCHGKBNYN
                   MOVE    "0"         TO  SPA-GMN-ENCHGTAIKBNYN
               WHEN  ( SPA-PREFNUM     =   CONST-TOKYO )
                AND  ( SPA-GMN-KOHNUM  =   "180" OR "380"
                                        OR "181" OR "381" )
                   MOVE    "3"         TO  SPA-GMN-ENCHGKBNYN
                   MOVE    "0"         TO  SPA-GMN-ENCHGTAIKBNYN
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-NIIGATA )
                   MOVE    "1"         TO  SPA-GMN-KOH2SKJKBN
                   MOVE    "1"         TO  SPA-GMN-KOH2SKJTAIKBN
               WHEN  ( SPA-PREFNUM     =   CONST-GUNMA )
                   MOVE    "1"         TO  SPA-GMN-KOH2SKJKBN
                   MOVE    "4"         TO  SPA-GMN-KOH2SKJTAIKBN
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-OSAKA )
                AND  ( SPA-GMN-KOHNUM  NOT =   "100" AND "200" )
                   MOVE    "1"         TO  SPA-GMN-KAICNTKBNG
                   MOVE    "0"         TO  SPA-GMN-KAICNTTAIKBNG
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-OSAKA )
                AND  ( SPA-GMN-KOHNUM  NOT =   "100" AND "200" )
                   MOVE    "1"         TO  SPA-GMN-CYOKEIKBNG
                   MOVE    "0"         TO  SPA-GMN-CYOKEITAIKBNG
               WHEN  ( SPA-PREFNUM     =   CONST-HIROSHIMA )
                AND  ( SPA-GMN-KOHNUM  =   "191" OR "291"
                                        OR "491" )
                   MOVE    "4"         TO  SPA-GMN-CYOKEIKBNG
                   MOVE    "0"         TO  SPA-GMN-CYOKEITAIKBNG
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-HIROSHIMA )
                AND  ( SPA-GMN-KOHNUM  =   "191" OR "291"
                                        OR "491" )
                   MOVE    "1"         TO  SPA-GMN-GNBKEIKBNKG
                   MOVE    "0"         TO  SPA-GMN-GNBKEITAIKBNKG
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-KYOTO )
               WHEN  ( SPA-PREFNUM     =   CONST-OKAYAMA )
               WHEN  ( SPA-PREFNUM     =   CONST-KUMAMOTO )
                   MOVE    "1"         TO  SPA-GMN-ZENKEIKBNG
                   MOVE    "0"         TO  SPA-GMN-ZENKEITAIKBNG
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-IBARAKI )
               WHEN  ( SPA-PREFNUM     =   CONST-TOKYO )
                AND  ( SPA-GMN-KOHNUM  =   "588" )
                   MOVE    "1"         TO  SPA-GMN-SINKEIKBNG
                   MOVE    "0"         TO  SPA-GMN-SINKEITAIKBNG
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-HOKKAIDO )
                AND  ( SPA-GMN-KOHNUM  =   "142" OR "143"
                                        OR "144" OR "190"
                                        OR "291" OR "292"
                                        OR "364" )
               WHEN  ( SPA-PREFNUM     =   CONST-MIYAGI )
                AND  ( SPA-GMN-KOHNUM  =   "263" OR "363"
                                        OR "463" OR "563" )
               WHEN  ( SPA-PREFNUM     =   CONST-HIROSHIMA )
                AND  ( SPA-GMN-KOHNUM  =   "390" )
                   MOVE    "1"         TO  SPA-GMN-SYOKEIKBNG
                   MOVE    "0"         TO  SPA-GMN-SYOKEITAIKBNG
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-HOKKAIDO )
                AND  ( SPA-GMN-KOHNUM  =   "142" OR "143"
                                        OR "144" OR "190"
                                        OR "291" OR "292"
                                        OR "744" OR "443"
                                        OR "444" OR "842"
                                        OR "364" )
                   MOVE    "1"         TO  SPA-GMN-SYOKEIKBNN
                   MOVE    "0"         TO  SPA-GMN-SYOKEITAIKBNN
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-MIYAGI )
                AND  ( SPA-GMN-KOHNUM  =   "463" OR "563" )
                   MOVE    "1"         TO  SPA-GMN-NYUKEIKBNN
                   MOVE    "0"         TO  SPA-GMN-NYUKEITAIKBNN
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-NIIGATA )
               WHEN  ( SPA-PREFNUM     =   CONST-FUKUOKA )
                   MOVE   "1"          TO  SPA-GMN-DAYKEIKBNN
                   MOVE   "0"          TO  SPA-GMN-DAYKEITAIKBNN
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-MIYAGI )
                AND  ( SPA-GMN-KOHNUM  =   "583" OR "683"
                                        OR "783" OR "363"
                                        OR "563" OR "763" )
               WHEN  ( SPA-PREFNUM     =   CONST-CHIBA )
                AND  ( SPA-GMN-KOHNUM  =   "263" OR "363"
                                        OR "463" )
               WHEN  ( SPA-PREFNUM     =   CONST-SAITAMA )
                AND  ( SPA-GMN-KOHNUM  =   "772" )
               WHEN  ( SPA-PREFNUM     =   CONST-FUKUOKA )
                AND  ( SPA-GMN-KOHNUM  =   "383" )
               WHEN  ( SPA-PREFNUM     =   CONST-OITA )
                AND  ( SPA-GMN-KOHNUM  =   "183" OR "283" )
                   MOVE    "1"         TO  SPA-GMN-NINNASIKBN
                   MOVE    "0"         TO  SPA-GMN-NINNASITAIKBN
               WHEN  ( SPA-PREFNUM     =   CONST-MIYAGI )
                AND  ( SPA-GMN-KOHNUM  =   "263" OR "463"
                                                 OR "663" )
                   MOVE    "1"         TO  SPA-GMN-NINNASIKBN
                   MOVE    "2"         TO  SPA-GMN-NINNASITAIKBN
               WHEN  ( SPA-PREFNUM     =   CONST-GUNMA )
                AND  ( SPA-GMN-KOHNUM  =   "145" OR "245"
                                        OR "146" OR "246"
                                        OR "143" OR "243" )
                   MOVE    "8"         TO  SPA-GMN-NINNASIKBN
                   MOVE    "1"         TO  SPA-GMN-NINNASITAIKBN
               END-EVALUATE
      *
      *        EVALUATE    TRUE
      *        WHEN  ( SPA-PREFNUM     =   CONST-GUNMA )
      *         AND  ( SPA-GMN-KOHNUM  =   "145" OR "245"
      *                                 OR "146" OR "246"
      *                                 OR "143" OR "243" )
      *            MOVE    "2"         TO  SPA-GMN-NINJYOKBN
      *            MOVE    "1"         TO  SPA-GMN-NINJYOTAIKBN
      *        END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-CHIBA )
                AND  ( SPA-GMN-KOHNUM  =   "100" OR "200" )
               WHEN  ( SPA-PREFNUM     =   CONST-KANAGAWA )
                AND  ( SPA-GMN-KOHNUM  =   "100" OR "200" )
               WHEN  ( SPA-PREFNUM     =   CONST-KYOTO )
                AND  ( SPA-GMN-KOHNUM  =   "100" OR "200" )
               WHEN  ( SPA-PREFNUM     =   CONST-OSAKA )
                AND  ( SPA-GMN-KOHNUM  =   "100" OR "200" )
               WHEN  ( SPA-PREFNUM     =   CONST-HYOGO )
                AND  ( SPA-GMN-KOHNUM  =   "100" OR "200" )
               WHEN  ( SPA-PREFNUM     =   CONST-KOCHI )
                AND  ( SPA-GMN-KOHNUM  =   "100" OR "200" )
                   MOVE    "1"         TO  SPA-GMN-JIRITUKBN
                   MOVE    "0"         TO  SPA-GMN-JIRITUTAIKBN
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-PREFNUM     =   CONST-HOKKAIDO )
                AND  ( SPA-GMN-KOHNUM  =   "642" OR "643" 
                                        OR "644" OR "743" )
                   MOVE    "1"         TO  SPA-GMN-SYOKEI2KBNG
                   MOVE    "0"         TO  SPA-GMN-SYOKEI2TAIKBNG
                   MOVE    "1"         TO  SPA-GMN-SYOKEI2KBNN
                   MOVE    "0"         TO  SPA-GMN-SYOKEI2TAIKBNN
               END-EVALUATE
      *
           .
       42012-COMBO-SET-DEFAULT-ETXT.
           EXIT.
      *****************************************************************
      *   履歴情報領域初期化処理
      *****************************************************************
       4201-RIREKI-AREA-CLEAR-SEC      SECTION.
      *
           INITIALIZE  SPA-GMN-RIREKILST-AREA
                       SPA-NAI-RIREKILST-AREA
      *
           MOVE    ZERO            TO  SPA-GMN-KOHLST-SEL
      *
           .
      *
       4201-RIREKI-AREA-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    共通関連チェック
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
           IF    ( SPA-GMN-KOHNUM          =   SPACE )
               MOVE    "1101"          TO  SPA-ERRCD
               MOVE   2                TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-NAI-YUKOSTYMD   =   SPACE )
                   MOVE    "1102"      TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-NAI-YUKOEDYMD   =   SPACE )
                   MOVE    "1103"      TO  SPA-ERRCD
                   MOVE    5           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-NAI-YUKOEDYMD   <   SPA-NAI-YUKOSTYMD )
                   MOVE    "1004"      TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-CUR
              END-IF
          END-IF
      *
           .
      *
       4900-KANRENCHK-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-WIDCD       NOT =   SPACE )
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W65    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE           TO  W65
           INITIALIZE                  W65
      *
           MOVE    SPA-GMN-PAGENO      TO  W65-NB1-PAGENO
      *
      *    基準日
           MOVE    SPA-GMN-KJNYMD      TO  W65-KJNYMD
      *
      *    選択番号１
           MOVE    SPA-GMN-SELNUM1     TO  W65-SELNUM1
      *
           IF    ( SPA-NAI-INPUT-MODE  =   2 )
              CONTINUE
           ELSE
              PERFORM 5001-GMNHEN-SEC
           END-IF
      *
           MOVE    SPA-GMN-JNISSUKBN-G         TO  W65-JNISSUKBN
           MOVE    SPA-GMN-JNISSUTAIKBN-G      TO  W65-JNISSUTAIKBN
           MOVE    SPA-GMN-TOTALTENKBN-G       TO  W65-TOTALTENKBN
           MOVE    SPA-GMN-TOTALTENTAIKBN-G    TO  W65-TOTALTENTAIKBN
           MOVE    SPA-GMN-SYUKEIKBN-G         TO  W65-SYUKEIKBN
           MOVE    SPA-GMN-SYUKEITAIKBN-G      TO  W65-SYUKEITAIKBN
           MOVE    SPA-GMN-FTNKEIKBNG-G        TO  W65-FTNKEIKBNG
           MOVE    SPA-GMN-FTNKEITAIKBNG-G     TO  W65-FTNKEITAIKBNG
           MOVE    SPA-GMN-FTNKEIKBNN-G        TO  W65-FTNKEIKBNN
           MOVE    SPA-GMN-FTNKEITAIKBNN-G     TO  W65-FTNKEITAIKBNN
           MOVE    SPA-GMN-FTNMONEYKBNG-G      TO  W65-FTNMONEYKBNG
           MOVE    SPA-GMN-FTNMONEYTAIKBNG-G   TO  W65-FTNMONEYTAIKBNG
           MOVE    SPA-GMN-FTNMONEYKBNN-G      TO  W65-FTNMONEYKBNN
           MOVE    SPA-GMN-FTNMONEYTAIKBNN-G   TO  W65-FTNMONEYTAIKBNN
           MOVE    SPA-GMN-FTNZEROKBNG-G       TO  W65-FTNZEROKBNG
           MOVE    SPA-GMN-FTNZEROTAIKBNG-G    TO  W65-FTNZEROTAIKBNG
           MOVE    SPA-GMN-FTNZEROKBNN-G       TO  W65-FTNZEROKBNN
           MOVE    SPA-GMN-FTNZEROTAIKBNN-G    TO  W65-FTNZEROTAIKBNN
           MOVE    SPA-GMN-SKJKBN-G            TO  W65-SKJKBN
           MOVE    SPA-GMN-SKJTAIKBN-G         TO  W65-SKJTAIKBN
           MOVE    SPA-GMN-OCRKBN-G            TO  W65-OCRKBN
           MOVE    SPA-GMN-OCRTAIKBN-G         TO  W65-OCRTAIKBN
           MOVE    SPA-GMN-JYUHENKBN-G         TO  W65-JYUHENKBN
           MOVE    SPA-GMN-JYUHENTAIKBN-G      TO  W65-JYUHENTAIKBN
           MOVE    SPA-GMN-JGNFTNKBN-G         TO  W65-JGNFTNKBN
           MOVE    "0 日・月上限額設定のある公費"
                                              TO   W65-JGNFTNTAIKBN
           MOVE    SPA-GMN-SAIKEIKBN-G        TO  W65-SAIKEIKBN
           MOVE    SPA-GMN-SAIKEITAIKBN-G     TO  W65-SAIKEITAIKBN
           MOVE    SPA-GMN-HKNFTNKBNK-G       TO  W65-HKNFTNKBNK
           MOVE    SPA-GMN-HKNFTNTAIKBNK-G    TO  W65-HKNFTNTAIKBNK
           MOVE    SPA-GMN-HKNFTNKBNY-G       TO  W65-HKNFTNKBNY
           MOVE    SPA-GMN-HKNFTNTAIKBNY-G    TO  W65-HKNFTNTAIKBNY
           MOVE    SPA-GMN-RECEKSIKBNG-G      TO  W65-RECEKSIKBNG
           MOVE    SPA-GMN-RECEKSITAIKBNG-G   TO  W65-RECEKSITAIKBNG
           MOVE    SPA-GMN-RECEKSIKBNN-G      TO  W65-RECEKSIKBNN
           MOVE    SPA-GMN-RECEKSITAIKBNN-G   TO  W65-RECEKSITAIKBNN
           MOVE    SPA-GMN-CHKJGNKBNG-G       TO  W65-CHKJGNKBNG
           MOVE    SPA-GMN-CHKJGNTAIKBNG-G    TO  W65-CHKJGNTAIKBNG
           MOVE    SPA-GMN-CHKJGNKBNN-G       TO  W65-CHKJGNKBNN
           MOVE    SPA-GMN-CHKJGNTAIKBNN-G    TO  W65-CHKJGNTAIKBNN
           MOVE    SPA-GMN-KOHSKYKBNG-G       TO  W65-KOHSKYKBNG
           MOVE    SPA-GMN-KOHSKYTAIKBNG-G    TO  W65-KOHSKYTAIKBNG
           MOVE    SPA-GMN-KOHSKYKBNN-G       TO  W65-KOHSKYKBNN
           MOVE    SPA-GMN-KOHSKYTAIKBNN-G    TO  W65-KOHSKYTAIKBNN
           MOVE    SPA-GMN-PTJGNKBN-G         TO  W65-PTJGNKBN
           MOVE    SPA-GMN-PTJGNTAIKBN-G      TO  W65-PTJGNTAIKBN
           MOVE    SPA-GMN-IRYOKEIKBNG-G      TO  W65-IRYOKEIKBNG
           MOVE    SPA-GMN-IRYOKEITAIKBNG-G   TO  W65-IRYOKEITAIKBNG
           MOVE    SPA-GMN-IRYOKEIKBNN-G      TO  W65-IRYOKEIKBNN
           MOVE    SPA-GMN-IRYOKEITAIKBNN-G   TO  W65-IRYOKEITAIKBNN
           MOVE    SPA-GMN-SKJKEIKBN-G        TO  W65-SKJKEIKBN
           MOVE    SPA-GMN-SKJKEITAIKBN-G     TO  W65-SKJKEITAIKBN
           MOVE    SPA-GMN-KINJYOKBNG-G       TO  W65-KINJYOKBNG
           MOVE    SPA-GMN-KINJYOTAIKBNG-G    TO  W65-KINJYOTAIKBNG
           MOVE    SPA-GMN-KINJYOKBNN-G       TO  W65-KINJYOKBNN
           MOVE    SPA-GMN-KINJYOTAIKBNN-G    TO  W65-KINJYOTAIKBNN
           MOVE    SPA-GMN-INNAIKBN-G         TO  W65-INNAIKBN
           MOVE    SPA-GMN-INNAITAIKBN-G      TO  W65-INNAITAIKBN
           MOVE    SPA-GMN-JIRITUKBN-G         TO  W65-JIRITUKBN
           MOVE    SPA-GMN-JIRITUTAIKBN-G      TO  W65-JIRITUTAIKBN
           MOVE    SPA-GMN-SKJRYOKBNHON-G     TO  W65-SKJRYOKBNHON
           MOVE    SPA-GMN-SKJRYOTAIKBNHON-G  TO  W65-SKJRYOTAIKBNHON
           MOVE    SPA-GMN-SEIRYOKBNHON-G     TO  W65-SEIRYOKBNHON
           MOVE    SPA-GMN-SEIRYOTAIKBNHON-G  TO  W65-SEIRYOTAIKBNHON
           MOVE    SPA-GMN-SEIKANKBNHON-G     TO  W65-SEIKANKBNHON
           MOVE    SPA-GMN-SEIKANTAIKBNHON-G  TO  W65-SEIKANTAIKBNHON
           MOVE    SPA-GMN-SKJRYOKBNTSY-G     TO  W65-SKJRYOKBNTSY
           MOVE    SPA-GMN-SKJRYOTAIKBNTSY-G  TO  W65-SKJRYOTAIKBNTSY
           MOVE    SPA-GMN-SEIRYOKBNTSY-G     TO  W65-SEIRYOKBNTSY
           MOVE    SPA-GMN-SEIRYOTAIKBNTSY-G  TO  W65-SEIRYOTAIKBNTSY
           MOVE    SPA-GMN-SEIKANKBNTSY-G     TO  W65-SEIKANKBNTSY
           MOVE    SPA-GMN-SEIKANTAIKBNTSY-G  TO  W65-SEIKANTAIKBNTSY
           MOVE    SPA-GMN-SKJRYOKBNTNK-G     TO  W65-SKJRYOKBNTNK
           MOVE    SPA-GMN-SKJRYOTAIKBNTNK-G  TO  W65-SKJRYOTAIKBNTNK
           MOVE    SPA-GMN-SEIRYOKBNTNK-G     TO  W65-SEIRYOKBNTNK
           MOVE    SPA-GMN-SEIRYOTAIKBNTNK-G  TO  W65-SEIRYOTAIKBNTNK
           MOVE    SPA-GMN-SEIKANKBNTNK-G     TO  W65-SEIKANKBNTNK
           MOVE    SPA-GMN-SEIKANTAIKBNTNK-G  TO  W65-SEIKANTAIKBNTNK
      *
           MOVE    SPA-GMN-KAKKBNK-G           TO  W65-KAKKBNK
           MOVE    SPA-GMN-KAKTAIKBNK-G        TO  W65-KAKTAIKBNK
           MOVE    SPA-GMN-KAK21KBNK-G         TO  W65-KAK21KBNK
           MOVE    SPA-GMN-KAK21TAIKBNK-G      TO  W65-KAK21TAIKBNK
           MOVE    SPA-GMN-KAK22KBNK-G         TO  W65-KAK22KBNK
           MOVE    SPA-GMN-KAK22TAIKBNK-G      TO  W65-KAK22TAIKBNK
           MOVE    SPA-GMN-KAK23KBNK-G         TO  W65-KAK23KBNK
           MOVE    SPA-GMN-KAK23TAIKBNK-G      TO  W65-KAK23TAIKBNK
           MOVE    SPA-GMN-KAK31KBNK-G         TO  W65-KAK31KBNK
           MOVE    SPA-GMN-KAK31TAIKBNK-G      TO  W65-KAK31TAIKBNK
           MOVE    SPA-GMN-KAK32KBNK-G         TO  W65-KAK32KBNK
           MOVE    SPA-GMN-KAK32TAIKBNK-G      TO  W65-KAK32TAIKBNK
           MOVE    SPA-GMN-KAK41KBNK-G         TO  W65-KAK41KBNK
           MOVE    SPA-GMN-KAK41TAIKBNK-G      TO  W65-KAK41TAIKBNK
           MOVE    SPA-GMN-KAK42KBNK-G         TO  W65-KAK42KBNK
           MOVE    SPA-GMN-KAK42TAIKBNK-G      TO  W65-KAK42TAIKBNK
           MOVE    SPA-GMN-KAK43KBNK-G         TO  W65-KAK43KBNK
           MOVE    SPA-GMN-KAK43TAIKBNK-G      TO  W65-KAK43TAIKBNK
           MOVE    SPA-GMN-KAKKBNY-G           TO  W65-KAKKBNY
           MOVE    SPA-GMN-KAKTAIKBNY-G        TO  W65-KAKTAIKBNY
           MOVE    SPA-GMN-KAK21KBNY-G         TO  W65-KAK21KBNY
           MOVE    SPA-GMN-KAK21TAIKBNY-G      TO  W65-KAK21TAIKBNY
           MOVE    SPA-GMN-KAK22KBNY-G         TO  W65-KAK22KBNY
           MOVE    SPA-GMN-KAK22TAIKBNY-G      TO  W65-KAK22TAIKBNY
           MOVE    SPA-GMN-KAK23KBNY-G         TO  W65-KAK23KBNY
           MOVE    SPA-GMN-KAK23TAIKBNY-G      TO  W65-KAK23TAIKBNY
           MOVE    SPA-GMN-KAK31KBNY-G         TO  W65-KAK31KBNY
           MOVE    SPA-GMN-KAK31TAIKBNY-G      TO  W65-KAK31TAIKBNY
           MOVE    SPA-GMN-KAK32KBNY-G         TO  W65-KAK32KBNY
           MOVE    SPA-GMN-KAK32TAIKBNY-G      TO  W65-KAK32TAIKBNY
           MOVE    SPA-GMN-KAK41KBNY-G         TO  W65-KAK41KBNY
           MOVE    SPA-GMN-KAK41TAIKBNY-G      TO  W65-KAK41TAIKBNY
           MOVE    SPA-GMN-KAK42KBNY-G         TO  W65-KAK42KBNY
           MOVE    SPA-GMN-KAK42TAIKBNY-G      TO  W65-KAK42TAIKBNY
           MOVE    SPA-GMN-KAK43KBNY-G         TO  W65-KAK43KBNY
           MOVE    SPA-GMN-KAK43TAIKBNY-G      TO  W65-KAK43TAIKBNY
           MOVE    SPA-GMN-KSINGKBNKG-G        TO  W65-KSINGKBNKG
           MOVE    SPA-GMN-KSINGTAIKBNKG-G     TO  W65-KSINGTAIKBNKG
           MOVE    SPA-GMN-KSINGKBNKN-G        TO  W65-KSINGKBNKN
           MOVE    SPA-GMN-KSINGTAIKBNKN-G     TO  W65-KSINGTAIKBNKN
           MOVE    SPA-GMN-KSINGKBNYG-G        TO  W65-KSINGKBNYG
           MOVE    SPA-GMN-KSINGTAIKBNYG-G     TO  W65-KSINGTAIKBNYG
           MOVE    SPA-GMN-KSINGKBNYN-G        TO  W65-KSINGKBNYN
           MOVE    SPA-GMN-KSINGTAIKBNYN-G     TO  W65-KSINGTAIKBNYN
           MOVE    SPA-GMN-ENCHGKBNKG-G        TO  W65-ENCHGKBNKG
           MOVE    SPA-GMN-ENCHGTAIKBNKG-G     TO  W65-ENCHGTAIKBNKG
           MOVE    SPA-GMN-ENCHGKBNKN-G        TO  W65-ENCHGKBNKN
           MOVE    SPA-GMN-ENCHGTAIKBNKN-G     TO  W65-ENCHGTAIKBNKN
           MOVE    SPA-GMN-ENCHGKBNYG-G        TO  W65-ENCHGKBNYG
           MOVE    SPA-GMN-ENCHGTAIKBNYG-G     TO  W65-ENCHGTAIKBNYG
           MOVE    SPA-GMN-ENCHGKBNYN-G        TO  W65-ENCHGKBNYN
           MOVE    SPA-GMN-ENCHGTAIKBNYN-G     TO  W65-ENCHGTAIKBNYN
           MOVE    SPA-GMN-KOH2SKJKBN-G        TO  W65-KOH2SKJKBN
           MOVE    SPA-GMN-KOH2SKJTAIKBN-G     TO  W65-KOH2SKJTAIKBN
           MOVE    SPA-GMN-FTNHASUKBNG-G       TO  W65-FTNHASUKBNG
           MOVE    SPA-GMN-FTNHASUTAIKBNG-G    TO  W65-FTNHASUTAIKBNG
           MOVE    SPA-GMN-FTNHASUKBNN-G       TO  W65-FTNHASUKBNN
           MOVE    SPA-GMN-FTNHASUTAIKBNN-G    TO  W65-FTNHASUTAIKBNN
           MOVE    SPA-GMN-KAICNTKBNG-G        TO  W65-KAICNTKBNG
           MOVE    SPA-GMN-KAICNTTAIKBNG-G     TO  W65-KAICNTTAIKBNG
           MOVE    SPA-GMN-CYOKEIKBNG-G        TO  W65-CYOKEIKBNG
           MOVE    SPA-GMN-CYOKEITAIKBNG-G     TO  W65-CYOKEITAIKBNG
           MOVE    SPA-GMN-GNBKEIKBNKG-G       TO  W65-GNBKEIKBNKG
           MOVE    SPA-GMN-GNBKEITAIKBNKG-G    TO  W65-GNBKEITAIKBNKG
           MOVE    SPA-GMN-ZENKEIKBNG-G        TO  W65-ZENKEIKBNG
           MOVE    SPA-GMN-ZENKEITAIKBNG-G     TO  W65-ZENKEITAIKBNG
           MOVE    SPA-GMN-SINKEIKBNG-G        TO  W65-SINKEIKBNG
           MOVE    SPA-GMN-SINKEITAIKBNG-G     TO  W65-SINKEITAIKBNG
           MOVE    SPA-GMN-SYOKEIKBNG-G        TO  W65-SYOKEIKBNG
           MOVE    SPA-GMN-SYOKEITAIKBNG-G     TO  W65-SYOKEITAIKBNG
           MOVE    SPA-GMN-SYOKEIKBNN-G        TO  W65-SYOKEIKBNN
           MOVE    SPA-GMN-SYOKEITAIKBNN-G     TO  W65-SYOKEITAIKBNN
           MOVE    SPA-GMN-NYUKEIKBNN-G        TO  W65-NYUKEIKBNN
           MOVE    SPA-GMN-NYUKEITAIKBNN-G     TO  W65-NYUKEITAIKBNN
           MOVE    SPA-GMN-DAYKEIKBNN-G        TO  W65-DAYKEIKBNN
           MOVE    SPA-GMN-DAYKEITAIKBNN-G     TO  W65-DAYKEITAIKBNN
           MOVE    SPA-GMN-NINNASIKBN-G        TO  W65-NINNASIKBN
           MOVE    SPA-GMN-NINNASITAIKBN-G     TO  W65-NINNASITAIKBN
           MOVE    SPA-GMN-NINJYOKBN-G         TO  W65-NINJYOKBN
           MOVE    SPA-GMN-NINJYOTAIKBN-G      TO  W65-NINJYOTAIKBN
           MOVE    SPA-GMN-KOGAKKEIKBN-G       TO  W65-KOGAKKEIKBN
           MOVE    SPA-GMN-KOGAKKEITAIKBN-G    TO  W65-KOGAKKEITAIKBN
           MOVE    SPA-GMN-TANKIKBN-G          TO  W65-TANKIKBN
           MOVE    SPA-GMN-TANKITAIKBN-G       TO  W65-TANKITAIKBN
           MOVE    SPA-GMN-KAICNTKBNN-G        TO  W65-KAICNTKBNN
           MOVE    SPA-GMN-KAICNTTAIKBNN-G     TO  W65-KAICNTTAIKBNN
           MOVE    SPA-GMN-CYOKEIKBNN-G        TO  W65-CYOKEIKBNN
           MOVE    SPA-GMN-CYOKEITAIKBNN-G     TO  W65-CYOKEITAIKBNN
           MOVE    SPA-GMN-ZENDAYKBNN-G        TO  W65-ZENDAYKBNN
           MOVE    SPA-GMN-ZENDAYTAIKBNN-G     TO  W65-ZENDAYTAIKBNN
           MOVE    SPA-GMN-ZENDAY2KBNN-G       TO  W65-ZENDAY2KBNN
           MOVE    SPA-GMN-ZENDAY2TAIKBNN-G    TO  W65-ZENDAY2TAIKBNN
           MOVE    SPA-GMN-ZENKEI2KBNG-G       TO  W65-ZENKEI2KBNG
           MOVE    SPA-GMN-ZENKEI2TAIKBNG-G    TO  W65-ZENKEI2TAIKBNG
           MOVE    SPA-GMN-SYOKEI2KBNG-G       TO  W65-SYOKEI2KBNG
           MOVE    SPA-GMN-SYOKEI2TAIKBNG-G    TO  W65-SYOKEI2TAIKBNG
           MOVE    SPA-GMN-SYOKEI2KBNN-G       TO  W65-SYOKEI2KBNN
           MOVE    SPA-GMN-SYOKEI2TAIKBNN-G    TO  W65-SYOKEI2TAIKBNN
           MOVE    SPA-GMN-NINNASIKBNK-G       TO  W65-NINNASIKBNK
           MOVE    SPA-GMN-NINNASITAIKBNK-G    TO  W65-NINNASITAIKBNK
           MOVE    SPA-GMN-CYOKIFTNKBN-G       TO  W65-CYOKIFTNKBN
           MOVE    SPA-GMN-CYOKIFTNTAIKBN-G    TO  W65-CYOKIFTNTAIKBN
           MOVE    SPA-GMN-CYOKIKSIKBN-G       TO  W65-CYOKIKSIKBN
           MOVE    SPA-GMN-CYOKIKSITAIKBN-G    TO  W65-CYOKIKSITAIKBN
      *
           MOVE    SPA-GMN-JNISSUKBN-MAX       TO  W65-JNISSUKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-JNISSUKBN-MAX )
                   MOVE    SPA-GMN-JNISSUKBNLST (IDX-CMB)
                                   TO  W65-JNISSUKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-TOTALTENKBN-MAX     TO  W65-TOTALTENKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-TOTALTENKBN-MAX )
                   MOVE    SPA-GMN-TOTALTENKBNLST (IDX-CMB)
                                   TO  W65-TOTALTENKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-SYUKEIKBN-MAX     TO  W65-SYUKEIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SYUKEIKBN-MAX )
                   MOVE    SPA-GMN-SYUKEIKBNLST (IDX-CMB)
                                   TO  W65-SYUKEIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-FTNKEIKBN-MAX TO    W65-FTNKEIKBNG-COUNT
                                               W65-FTNKEIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-FTNKEIKBN-MAX )
                   MOVE    SPA-GMN-FTNKEIKBNLST (IDX-CMB)
                                   TO  W65-FTNKEIKBNG-ITEM (IDX-CMB)
                                       W65-FTNKEIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-FTNMONEYKBN-MAX TO  W65-FTNMONEYKBNG-COUNT
                                               W65-FTNMONEYKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-FTNMONEYKBN-MAX )
                   MOVE    SPA-GMN-FTNMONEYKBNLST (IDX-CMB)
                                   TO  W65-FTNMONEYKBNG-ITEM (IDX-CMB)
                                       W65-FTNMONEYKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-FTNZEROKBN-MAX  TO  W65-FTNZEROKBNG-COUNT
                                               W65-FTNZEROKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-FTNZEROKBN-MAX )
                   MOVE    SPA-GMN-FTNZEROKBNLST (IDX-CMB)
                                   TO  W65-FTNZEROKBNG-ITEM (IDX-CMB)
                                       W65-FTNZEROKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-SKJKBN-MAX          TO  W65-SKJKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJKBN-MAX )
                   MOVE    SPA-GMN-SKJKBNLST (IDX-CMB)
                                   TO  W65-SKJKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-OCRKBN-MAX          TO  W65-OCRKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-OCRKBN-MAX )
                   MOVE    SPA-GMN-OCRKBNLST (IDX-CMB)
                                   TO  W65-OCRKBN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-JYUHENKBN-MAX       TO  W65-JYUHENKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-JYUHENKBN-MAX )
                   MOVE    SPA-GMN-JYUHENKBNLST (IDX-CMB)
                                   TO  W65-JYUHENKBN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-JGNFTNKBN-MAX       TO  W65-JGNFTNKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-JGNFTNKBN-MAX )
                   MOVE    SPA-GMN-JGNFTNKBNLST (IDX-CMB)
                                   TO  W65-JGNFTNKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    1                       TO  W65-JGNFTNTAIKBN-COUNT
           MOVE    "0 日・月上限額設定のある公費"
                                           TO  W65-JGNFTNTAIKBN-ITEM(1)
      *
           MOVE    SPA-GMN-SAIKEIKBN-MAX      TO  W65-SAIKEIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SAIKEIKBN-MAX )
                   MOVE    SPA-GMN-SAIKEIKBNLST (IDX-CMB)
                                   TO  W65-SAIKEIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-HKNFTNKBN-MAX   TO  W65-HKNFTNKBNK-COUNT
                                               W65-HKNFTNKBNY-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-HKNFTNKBN-MAX )
                   MOVE    SPA-GMN-HKNFTNKBNLST (IDX-CMB)
                                   TO  W65-HKNFTNKBNK-ITEM (IDX-CMB)
                                       W65-HKNFTNKBNY-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-RECEKSIKBN-MAX  TO  W65-RECEKSIKBNG-COUNT
                                               W65-RECEKSIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-RECEKSIKBN-MAX )
                   MOVE    SPA-GMN-RECEKSIKBNLST (IDX-CMB)
                                   TO  W65-RECEKSIKBNG-ITEM (IDX-CMB)
                                       W65-RECEKSIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-CHKJGNKBNG-MAX       TO  W65-CHKJGNKBNG-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CHKJGNKBNG-MAX )
                   MOVE    SPA-GMN-CHKJGNKBNGLST (IDX-CMB)
                                   TO  W65-CHKJGNKBNG-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-CHKJGNKBNN-MAX       TO  W65-CHKJGNKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CHKJGNKBNN-MAX )
                   MOVE    SPA-GMN-CHKJGNKBNNLST (IDX-CMB)
                                   TO  W65-CHKJGNKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-CHKJGNTAIKBN-MAX
                                    TO  W65-CHKJGNTAIKBNG-COUNT
                                        W65-CHKJGNTAIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CHKJGNTAIKBN-MAX )
                   MOVE    SPA-GMN-CHKJGNTAIKBNLST (IDX-CMB)
                                   TO  W65-CHKJGNTAIKBNG-ITEM (IDX-CMB)
                                       W65-CHKJGNTAIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-KOHSKYKBN-MAX
                                    TO  W65-KOHSKYKBNG-COUNT
                                        W65-KOHSKYKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KOHSKYKBN-MAX )
                   MOVE    SPA-GMN-KOHSKYKBNLST (IDX-CMB)
                                   TO  W65-KOHSKYKBNG-ITEM (IDX-CMB)
                                       W65-KOHSKYKBNN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-PTJGNKBN-MAX    TO  W65-PTJGNKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-PTJGNKBN-MAX )
                   MOVE    SPA-GMN-PTJGNKBNLST (IDX-CMB)
                                   TO  W65-PTJGNKBN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-PTJGNTAIKBN-MAX    TO  W65-PTJGNTAIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-PTJGNTAIKBN-MAX )
                   MOVE    SPA-GMN-PTJGNTAIKBNLST (IDX-CMB)
                                   TO  W65-PTJGNTAIKBN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-IRYOKEIKBN-MAX    TO  W65-IRYOKEIKBNG-COUNT
                                                 W65-IRYOKEIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-IRYOKEIKBN-MAX )
                   MOVE    SPA-GMN-IRYOKEIKBNLST (IDX-CMB)
                                   TO  W65-IRYOKEIKBNG-ITEM (IDX-CMB)
                                       W65-IRYOKEIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-IRYOKEITAIKBN-MAX
                                           TO  W65-IRYOKEITAIKBNG-COUNT
                                               W65-IRYOKEITAIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-IRYOKEITAIKBN-MAX )
                   MOVE    SPA-GMN-IRYOKEITAIKBNLST (IDX-CMB)
                                   TO  W65-IRYOKEITAIKBNG-ITEM (IDX-CMB)
                                       W65-IRYOKEITAIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-SKJKEIKBN-MAX   TO  W65-SKJKEIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJKEIKBN-MAX )
                   MOVE    SPA-GMN-SKJKEIKBNLST (IDX-CMB)
                                   TO  W65-SKJKEIKBN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-SKJKEITAIKBN-MAX
                                           TO  W65-SKJKEITAIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJKEITAIKBN-MAX )
                   MOVE    SPA-GMN-SKJKEITAIKBNLST (IDX-CMB)
                                   TO  W65-SKJKEITAIKBN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-KINJYOKBN-MAX   TO  W65-KINJYOKBNG-COUNT
                                              W65-KINJYOKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KINJYOKBN-MAX )
                   MOVE    SPA-GMN-KINJYOKBNLST (IDX-CMB)
                                   TO  W65-KINJYOKBNG-ITEM (IDX-CMB)
                                       W65-KINJYOKBNN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-KINJYOTAIKBN-MAX
                                           TO  W65-KINJYOTAIKBNG-COUNT
                                               W65-KINJYOTAIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KINJYOTAIKBN-MAX )
                   MOVE    SPA-GMN-KINJYOTAIKBNLST (IDX-CMB)
                                   TO  W65-KINJYOTAIKBNG-ITEM (IDX-CMB)
                                       W65-KINJYOTAIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-INNAIKBN-MAX   TO  W65-INNAIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-INNAIKBN-MAX )
                   MOVE    SPA-GMN-INNAIKBNLST (IDX-CMB)
                                   TO  W65-INNAIKBN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-INNAITAIKBN-MAX
                                           TO  W65-INNAITAIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-INNAITAIKBN-MAX )
                   MOVE    SPA-GMN-INNAITAIKBNLST (IDX-CMB)
                                   TO  W65-INNAITAIKBN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-JIRITUKBN-MAX   TO  W65-JIRITUKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-JIRITUKBN-MAX )
                   MOVE    SPA-GMN-JIRITUKBNLST (IDX-CMB)
                                   TO  W65-JIRITUKBN-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-JIRITUTAIKBN-MAX
                                           TO  W65-JIRITUTAIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-JIRITUTAIKBN-MAX )
                   MOVE    SPA-GMN-JIRITUTAIKBNLST (IDX-CMB)
                                   TO  W65-JIRITUTAIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           IF    ( SPA-NAI-INPUT-MODE  =   2 )
               CONTINUE
           ELSE
               MOVE    SPA-NAI-SKJKBNHON       TO  WRK-CMB-CD
               PERFORM 5002-LBLSKJ-EDIT-SEC
               MOVE    WRK-CMB-ITM             TO  W65-LBLSKJHON
      *
               MOVE    SPA-NAI-SKJKBNTSY       TO  WRK-CMB-CD
               PERFORM 5002-LBLSKJ-EDIT-SEC
               MOVE    WRK-CMB-ITM             TO  W65-LBLSKJTSY
      *
               MOVE    SPA-NAI-SKJKBNTNK       TO  WRK-CMB-CD
               PERFORM 5002-LBLSKJ-EDIT-SEC
               MOVE    WRK-CMB-ITM             TO  W65-LBLSKJTNK
           END-IF
      *
           MOVE    SPA-GMN-SKJFUKAKBN-MAX
                                   TO  W65-SKJRYOKBNHON-COUNT
                                       W65-SEIRYOKBNHON-COUNT
                                       W65-SEIKANKBNHON-COUNT
                                       W65-SKJRYOKBNTSY-COUNT
                                       W65-SEIRYOKBNTSY-COUNT
                                       W65-SEIKANKBNTSY-COUNT
                                       W65-SKJRYOKBNTNK-COUNT
                                       W65-SEIRYOKBNTNK-COUNT
                                       W65-SEIKANKBNTNK-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJFUKAKBN-MAX )
                   MOVE    SPA-GMN-SKJFUKAKBNLST (IDX-CMB)
                                   TO  W65-SKJRYOKBNHON-ITEM (IDX-CMB)
                                       W65-SEIRYOKBNHON-ITEM (IDX-CMB)
                                       W65-SEIKANKBNHON-ITEM (IDX-CMB)
                                       W65-SKJRYOKBNTSY-ITEM (IDX-CMB)
                                       W65-SEIRYOKBNTSY-ITEM (IDX-CMB)
                                       W65-SEIKANKBNTSY-ITEM (IDX-CMB)
                                       W65-SKJRYOKBNTNK-ITEM (IDX-CMB)
                                       W65-SEIRYOKBNTNK-ITEM (IDX-CMB)
                                       W65-SEIKANKBNTNK-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-SKJFUKATAIKBNHON-MAX
                               TO  W65-SKJRYOTAIKBNHON-COUNT
                                   W65-SEIRYOTAIKBNHON-COUNT
                                   W65-SEIKANTAIKBNHON-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJFUKATAIKBNHON-MAX )
                   MOVE    SPA-GMN-SKJFUKATAIKBNHONLST (IDX-CMB)
                               TO  W65-SKJRYOTAIKBNHON-ITEM (IDX-CMB)
                                   W65-SEIRYOTAIKBNHON-ITEM (IDX-CMB)
                                   W65-SEIKANTAIKBNHON-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-SKJFUKATAIKBNTSY-MAX
                               TO  W65-SKJRYOTAIKBNTSY-COUNT
                                   W65-SEIRYOTAIKBNTSY-COUNT
                                   W65-SEIKANTAIKBNTSY-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJFUKATAIKBNTSY-MAX )
                   MOVE    SPA-GMN-SKJFUKATAIKBNTSYLST (IDX-CMB)
                               TO  W65-SKJRYOTAIKBNTSY-ITEM (IDX-CMB)
                                   W65-SEIRYOTAIKBNTSY-ITEM (IDX-CMB)
                                   W65-SEIKANTAIKBNTSY-ITEM (IDX-CMB)
           END-PERFORM
      * 
           MOVE    SPA-GMN-SKJFUKATAIKBNTNK-MAX
                               TO  W65-SKJRYOTAIKBNTNK-COUNT
                                   W65-SEIRYOTAIKBNTNK-COUNT
                                   W65-SEIKANTAIKBNTNK-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJFUKATAIKBNTNK-MAX )
                   MOVE    SPA-GMN-SKJFUKATAIKBNTNKLST (IDX-CMB)
                               TO  W65-SKJRYOTAIKBNTNK-ITEM (IDX-CMB)
                                   W65-SEIRYOTAIKBNTNK-ITEM (IDX-CMB)
                                   W65-SEIKANTAIKBNTNK-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-COMMONTAIKBN-MAX
                                   TO  W65-JNISSUTAIKBN-COUNT
                                       W65-TOTALTENTAIKBN-COUNT
                                       W65-SYUKEITAIKBN-COUNT
                                       W65-FTNKEITAIKBNG-COUNT
                                       W65-FTNKEITAIKBNN-COUNT
                                       W65-FTNMONEYTAIKBNG-COUNT
                                       W65-FTNMONEYTAIKBNN-COUNT
                                       W65-FTNZEROTAIKBNG-COUNT
                                       W65-FTNZEROTAIKBNN-COUNT
                                       W65-SKJTAIKBN-COUNT
                                       W65-OCRTAIKBN-COUNT
                                       W65-HKNFTNTAIKBNK-COUNT
                                       W65-HKNFTNTAIKBNY-COUNT
                                       W65-RECEKSITAIKBNG-COUNT
                                       W65-RECEKSITAIKBNN-COUNT
                                       W65-KOHSKYTAIKBNG-COUNT
                                       W65-KOHSKYTAIKBNN-COUNT
      *
                                       W65-KAKTAIKBNK-COUNT
                                       W65-KAKTAIKBNY-COUNT
                                       W65-KAK21TAIKBNK-COUNT
                                       W65-KAK21TAIKBNY-COUNT
                                       W65-KAK22TAIKBNK-COUNT
                                       W65-KAK22TAIKBNY-COUNT
                                       W65-KAK23TAIKBNK-COUNT
                                       W65-KAK23TAIKBNY-COUNT
                                       W65-KAK31TAIKBNK-COUNT
                                       W65-KAK31TAIKBNY-COUNT
                                       W65-KAK32TAIKBNK-COUNT
                                       W65-KAK32TAIKBNY-COUNT
                                       W65-KAK41TAIKBNK-COUNT
                                       W65-KAK41TAIKBNY-COUNT
                                       W65-KAK42TAIKBNK-COUNT
                                       W65-KAK42TAIKBNY-COUNT
                                       W65-KAK43TAIKBNK-COUNT
                                       W65-KAK43TAIKBNY-COUNT
                                       W65-KSINGTAIKBNKG-COUNT
                                       W65-KSINGTAIKBNKN-COUNT
                                       W65-KSINGTAIKBNYG-COUNT
                                       W65-KSINGTAIKBNYN-COUNT
                                       W65-ENCHGTAIKBNKG-COUNT
                                       W65-ENCHGTAIKBNKN-COUNT
                                       W65-ENCHGTAIKBNYG-COUNT
                                       W65-ENCHGTAIKBNYN-COUNT
                                       W65-KOH2SKJTAIKBN-COUNT
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-COMMONTAIKBN-MAX )
                   MOVE    SPA-GMN-COMMONTAIKBNLST (IDX-CMB)
                                   TO  W65-JNISSUTAIKBN-ITEM (IDX-CMB)
                                       W65-TOTALTENTAIKBN-ITEM(IDX-CMB)
                                       W65-SYUKEITAIKBN-ITEM(IDX-CMB)
                                       W65-FTNKEITAIKBNG-ITEM(IDX-CMB)
                                       W65-FTNKEITAIKBNN-ITEM(IDX-CMB)
                                       W65-FTNMONEYTAIKBNG-ITEM(IDX-CMB)
                                       W65-FTNMONEYTAIKBNN-ITEM(IDX-CMB)
                                       W65-FTNZEROTAIKBNG-ITEM(IDX-CMB)
                                       W65-FTNZEROTAIKBNN-ITEM(IDX-CMB)
                                       W65-SKJTAIKBN-ITEM (IDX-CMB)
                                       W65-OCRTAIKBN-ITEM (IDX-CMB)
                                       W65-HKNFTNTAIKBNK-ITEM(IDX-CMB)
                                       W65-HKNFTNTAIKBNY-ITEM(IDX-CMB)
                                       W65-RECEKSITAIKBNG-ITEM (IDX-CMB)
                                       W65-RECEKSITAIKBNN-ITEM (IDX-CMB)
                                       W65-KOHSKYTAIKBNG-ITEM(IDX-CMB)
                                       W65-KOHSKYTAIKBNN-ITEM(IDX-CMB)
                                       W65-KAKTAIKBNK-ITEM(IDX-CMB)
                                       W65-KAKTAIKBNY-ITEM(IDX-CMB)
                                       W65-KAK21TAIKBNK-ITEM(IDX-CMB)
                                       W65-KAK21TAIKBNY-ITEM(IDX-CMB)
                                       W65-KAK22TAIKBNK-ITEM(IDX-CMB)
                                       W65-KAK22TAIKBNY-ITEM(IDX-CMB)
                                       W65-KAK23TAIKBNK-ITEM(IDX-CMB)
                                       W65-KAK23TAIKBNY-ITEM(IDX-CMB)
                                       W65-KAK31TAIKBNK-ITEM(IDX-CMB)
                                       W65-KAK31TAIKBNY-ITEM(IDX-CMB)
                                       W65-KAK32TAIKBNK-ITEM(IDX-CMB)
                                       W65-KAK32TAIKBNY-ITEM(IDX-CMB)
                                       W65-KAK41TAIKBNK-ITEM(IDX-CMB)
                                       W65-KAK41TAIKBNY-ITEM(IDX-CMB)
                                       W65-KAK42TAIKBNK-ITEM(IDX-CMB)
                                       W65-KAK42TAIKBNY-ITEM(IDX-CMB)
                                       W65-KAK43TAIKBNK-ITEM(IDX-CMB)
                                       W65-KAK43TAIKBNY-ITEM(IDX-CMB)
                                       W65-KSINGTAIKBNKG-ITEM(IDX-CMB)
                                       W65-KSINGTAIKBNKN-ITEM(IDX-CMB)
                                       W65-KSINGTAIKBNYG-ITEM(IDX-CMB)
                                       W65-KSINGTAIKBNYN-ITEM(IDX-CMB)
                                       W65-ENCHGTAIKBNKG-ITEM(IDX-CMB)
                                       W65-ENCHGTAIKBNKN-ITEM(IDX-CMB)
                                       W65-ENCHGTAIKBNYG-ITEM(IDX-CMB)
                                       W65-ENCHGTAIKBNYN-ITEM(IDX-CMB)
                                       W65-KOH2SKJTAIKBN-ITEM(IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-SAIKEITAIKBN-MAX
                                       TO  W65-SAIKEITAIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SAIKEITAIKBN-MAX )
                   MOVE    SPA-GMN-SAIKEITAIKBNLST (IDX-CMB)
                                   TO  W65-SAIKEITAIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-JYUHENTAIKBN-MAX
                                       TO  W65-JYUHENTAIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-JYUHENTAIKBN-MAX )
                   MOVE    SPA-GMN-JYUHENTAIKBNLST (IDX-CMB)
                                   TO  W65-JYUHENTAIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *
      *    保険欄の括弧再掲記載
           MOVE    SPA-GMN-KAKKBN-MAX  TO  W65-KAKKBNK-COUNT
                                           W65-KAKKBNY-COUNT
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KAKKBN-MAX )
                   MOVE    SPA-GMN-KAKKBNLST (IDX-CMB)
                                   TO  W65-KAKKBNK-ITEM (IDX-CMB)
                                       W65-KAKKBNY-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    保険欄の括弧再掲（下）記載
           MOVE    SPA-GMN-KAK20KBN-MAX
                                       TO  W65-KAK21KBNK-COUNT
                                           W65-KAK21KBNY-COUNT
                                           W65-KAK22KBNK-COUNT
                                           W65-KAK22KBNY-COUNT
                                           W65-KAK23KBNK-COUNT
                                           W65-KAK23KBNY-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KAK20KBN-MAX )
                   MOVE    SPA-GMN-KAK20KBNLST (IDX-CMB)
                                       TO  W65-KAK21KBNK-ITEM (IDX-CMB)
                                           W65-KAK21KBNY-ITEM (IDX-CMB)
                                           W65-KAK22KBNK-ITEM (IDX-CMB)
                                           W65-KAK22KBNY-ITEM (IDX-CMB)
                                           W65-KAK23KBNK-ITEM (IDX-CMB)
                                           W65-KAK23KBNY-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    保険欄の括弧再掲（下）金額計算
           MOVE    SPA-GMN-KAK31KBN-MAX
                                       TO  W65-KAK31KBNK-COUNT
                                           W65-KAK31KBNY-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KAK31KBN-MAX )
               MOVE    SPA-GMN-KAK31KBNLST (IDX-CMB)
                                       TO  W65-KAK31KBNK-ITEM (IDX-CMB)
                                           W65-KAK31KBNY-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    保険欄の括弧再掲（下）金額計算
           MOVE    SPA-GMN-KAK32KBNK-MAX
                                       TO  W65-KAK32KBNK-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KAK32KBNK-MAX )
               MOVE    SPA-GMN-KAK32KBNKLST (IDX-CMB)
                                       TO  W65-KAK32KBNK-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-KAK32KBNY-MAX
                                       TO  W65-KAK32KBNY-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KAK32KBNY-MAX )
               MOVE    SPA-GMN-KAK32KBNYLST (IDX-CMB)
                                       TO  W65-KAK32KBNY-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    保険欄の括弧再掲（下）金額加算
           MOVE    SPA-GMN-KAK40KBN-MAX
                                       TO  W65-KAK41KBNK-COUNT
                                           W65-KAK41KBNY-COUNT
                                           W65-KAK42KBNK-COUNT
                                           W65-KAK42KBNY-COUNT
                                           W65-KAK43KBNK-COUNT
                                           W65-KAK43KBNY-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KAK40KBN-MAX )
               MOVE    SPA-GMN-KAK40KBNLST (IDX-CMB)
                                       TO  W65-KAK41KBNK-ITEM (IDX-CMB)
                                           W65-KAK41KBNY-ITEM (IDX-CMB)
                                           W65-KAK42KBNK-ITEM (IDX-CMB)
                                           W65-KAK42KBNY-ITEM (IDX-CMB)
                                           W65-KAK43KBNK-ITEM (IDX-CMB)
                                           W65-KAK43KBNY-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    保険欄−一部負担金無記載
           MOVE    SPA-GMN-KSINGKBN-MAX
                                       TO  W65-KSINGKBNKG-COUNT
                                           W65-KSINGKBNKN-COUNT
                                           W65-KSINGKBNYG-COUNT
                                           W65-KSINGKBNYN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KSINGKBN-MAX )
               MOVE    SPA-GMN-KSINGKBNLST (IDX-CMB)
                                       TO  W65-KSINGKBNKG-ITEM (IDX-CMB)
                                           W65-KSINGKBNKN-ITEM (IDX-CMB)
                                           W65-KSINGKBNYG-ITEM (IDX-CMB)
                                           W65-KSINGKBNYN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    一部負担金１円→１０円単位記載
           MOVE    SPA-GMN-ENCHGKBN-MAX
                                       TO  W65-ENCHGKBNKG-COUNT
                                           W65-ENCHGKBNKN-COUNT
                                           W65-ENCHGKBNYG-COUNT
                                           W65-ENCHGKBNYN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-ENCHGKBN-MAX )
               MOVE    SPA-GMN-ENCHGKBNLST  (IDX-CMB)
                                       TO  W65-ENCHGKBNKG-ITEM (IDX-CMB)
                                           W65-ENCHGKBNKN-ITEM (IDX-CMB)
                                           W65-ENCHGKBNYG-ITEM (IDX-CMB)
                                           W65-ENCHGKBNYN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    入院時食事療養記載（公費２欄置き換え）
           MOVE    SPA-GMN-KOH2SKJKBN-MAX
                                       TO  W65-KOH2SKJKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KOH2SKJKBN-MAX )
               MOVE    SPA-GMN-KOH2SKJKBNLST(IDX-CMB)
                                       TO  W65-KOH2SKJKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    一部負担金記載１円未満端数処理（外来）
           MOVE    SPA-GMN-FTNHASUKBNG-MAX
                                   TO  W65-FTNHASUKBNG-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-FTNHASUKBNG-MAX )
               MOVE    SPA-GMN-FTNHASUKBNGLST(IDX-CMB)
                                   TO  W65-FTNHASUKBNG-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    一部負担金記載１円未満端数処理（外来）対象区分
           MOVE    SPA-GMN-FTNHASUTAIKBNG-MAX
                                   TO  W65-FTNHASUTAIKBNG-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-FTNHASUTAIKBNG-MAX )
               MOVE    SPA-GMN-FTNHASUTAIKBNGLST(IDX-CMB)
                                   TO  W65-FTNHASUTAIKBNG-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    一部負担金記載１円未満端数処理（入院）
           MOVE    SPA-GMN-FTNHASUKBNN-MAX
                                   TO  W65-FTNHASUKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-FTNHASUKBNN-MAX )
               MOVE    SPA-GMN-FTNHASUKBNNLST(IDX-CMB)
                                   TO  W65-FTNHASUKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    一部負担金記載１円未満端数処理（入院）対象区分
           MOVE    SPA-GMN-FTNHASUTAIKBNN-MAX
                                   TO  W65-FTNHASUTAIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-FTNHASUTAIKBNN-MAX )
               MOVE    SPA-GMN-FTNHASUTAIKBNNLST(IDX-CMB)
                                   TO  W65-FTNHASUTAIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    負担金未発生分の回数カウント（外来）
           MOVE    SPA-GMN-KAICNTKBNG-MAX
                                       TO  W65-KAICNTKBNG-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KAICNTKBNG-MAX )
               MOVE    SPA-GMN-KAICNTKBNGLST (IDX-CMB)
                                       TO  W65-KAICNTKBNG-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    負担金未発生分の回数カウント（外来）対象区分
           MOVE    SPA-GMN-KAICNTTAIKBNG-MAX
                                       TO  W65-KAICNTTAIKBNG-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KAICNTTAIKBNG-MAX )
               MOVE    SPA-GMN-KAICNTTAIKBNGLST (IDX-CMB)
                                   TO  W65-KAICNTTAIKBNG-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    長期併用時の特別計算（外来）
           MOVE    SPA-GMN-CYOKEIKBNG-MAX
                                       TO  W65-CYOKEIKBNG-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CYOKEIKBNG-MAX )
               MOVE    SPA-GMN-CYOKEIKBNGLST (IDX-CMB)
                                       TO  W65-CYOKEIKBNG-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    長期併用時の特別計算（外来）対象区分
           MOVE    SPA-GMN-CYOKEITAIKBNG-MAX
                                       TO  W65-CYOKEITAIKBNG-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CYOKEITAIKBNG-MAX )
               MOVE    SPA-GMN-CYOKEITAIKBNGLST (IDX-CMB)
                                   TO  W65-CYOKEITAIKBNG-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    現物給付対象者の特別計算（高齢者・外）
           MOVE    SPA-GMN-GNBKEIKBNKG-MAX
                                   TO  W65-GNBKEIKBNKG-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-GNBKEIKBNKG-MAX )
               MOVE    SPA-GMN-GNBKEIKBNKGLST (IDX-CMB)
                                   TO  W65-GNBKEIKBNKG-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    現物給付対象者の特別計算（高齢者・外）対象区分
           MOVE    SPA-GMN-GNBKEITAIKBNKG-MAX
                                   TO  W65-GNBKEITAIKBNKG-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-GNBKEITAIKBNKG-MAX )
               MOVE    SPA-GMN-GNBKEITAIKBNKGLST (IDX-CMB)
                                   TO  W65-GNBKEITAIKBNKG-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    全国公費併用時の特別計算（外来）
           MOVE    SPA-GMN-ZENKEIKBNG-MAX
                                   TO  W65-ZENKEIKBNG-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-ZENKEIKBNG-MAX )
               MOVE    SPA-GMN-ZENKEIKBNGLST (IDX-CMB)
                                   TO  W65-ZENKEIKBNG-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    全国公費併用時の特別計算（外来）対象区分
           MOVE    SPA-GMN-ZENKEITAIKBNG-MAX
                                   TO  W65-ZENKEITAIKBNG-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-ZENKEITAIKBNG-MAX )
               MOVE    SPA-GMN-ZENKEITAIKBNGLST (IDX-CMB)
                                   TO  W65-ZENKEITAIKBNG-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    診察料がない場合の負担金計算（外来）
           MOVE    SPA-GMN-SINKEIKBN-MAX
                                   TO  W65-SINKEIKBNG-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SINKEIKBN-MAX )
               MOVE    SPA-GMN-SINKEIKBNLST (IDX-CMB)
                                   TO  W65-SINKEIKBNG-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    診察料がない場合の負担金計算（外来）対象区分
           MOVE    SPA-GMN-SINKEITAIKBN-MAX
                                   TO  W65-SINKEITAIKBNG-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SINKEITAIKBN-MAX )
               MOVE    SPA-GMN-SINKEITAIKBNLST (IDX-CMB)
                                   TO  W65-SINKEITAIKBNG-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    初診時以外の負担金計算
           MOVE    SPA-GMN-SYOKEIKBN-MAX
                                   TO  W65-SYOKEIKBNG-COUNT
                                       W65-SYOKEIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SYOKEIKBN-MAX )
               MOVE    SPA-GMN-SYOKEIKBNLST (IDX-CMB)
                                   TO  W65-SYOKEIKBNG-ITEM (IDX-CMB)
                                       W65-SYOKEIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    初診時以外の負担金計算対象区分
           MOVE    SPA-GMN-SYOKEITAIKBN-MAX
                                   TO  W65-SYOKEITAIKBNG-COUNT
                                       W65-SYOKEITAIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SYOKEITAIKBN-MAX )
               MOVE    SPA-GMN-SYOKEITAIKBNLST (IDX-CMB)
                                   TO  W65-SYOKEITAIKBNG-ITEM (IDX-CMB)
                                       W65-SYOKEITAIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    １入院＠日限度とする特別計算（入院）
           MOVE    SPA-GMN-NYUKEIKBN-MAX
                                   TO  W65-NYUKEIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NYUKEIKBN-MAX )
               MOVE    SPA-GMN-NYUKEIKBNLST (IDX-CMB)
                                   TO  W65-NYUKEIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    １入院＠日限度とする特別計算（入院）対象区分
           MOVE    SPA-GMN-NYUKEITAIKBN-MAX
                                   TO  W65-NYUKEITAIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NYUKEITAIKBN-MAX )
               MOVE    SPA-GMN-NYUKEITAIKBNLST (IDX-CMB)
                                   TO  W65-NYUKEITAIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    日上限額を固定額とする特別計算（入院）
           MOVE    SPA-GMN-DAYKEIKBN-MAX
                                   TO  W65-DAYKEIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-DAYKEIKBN-MAX )
               MOVE    SPA-GMN-DAYKEIKBNLST (IDX-CMB)
                                   TO  W65-DAYKEIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    日上限額を固定額とする特別計算（入院）対象区分
           MOVE    SPA-GMN-DAYKEITAIKBN-MAX
                                   TO  W65-DAYKEITAIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-DAYKEITAIKBN-MAX )
               MOVE    SPA-GMN-DAYKEITAIKBNLST (IDX-CMB)
                                   TO  W65-DAYKEITAIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    限度額認定証提示が無い場合の特別計算
           MOVE    SPA-GMN-NINNASIKBN-MAX
                                   TO  W65-NINNASIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NINNASIKBN-MAX )
               MOVE    SPA-GMN-NINNASIKBNLST (IDX-CMB)
                                   TO  W65-NINNASIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    限度額認定証提示が無い場合の特別計算対象区分
           MOVE    SPA-GMN-NINNASITAIKBN-MAX
                                   TO  W65-NINNASITAIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NINNASITAIKBN-MAX )
               MOVE    SPA-GMN-NINNASITAIKBNLST (IDX-CMB)
                                   TO  W65-NINNASITAIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    限度額認定証（上位）の場合の特別計算
           MOVE    SPA-GMN-NINJYOKBN-MAX
                                   TO  W65-NINJYOKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NINJYOKBN-MAX )
               MOVE    SPA-GMN-NINJYOKBNLST (IDX-CMB)
                                   TO  W65-NINJYOKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    限度額認定証（上位）の場合の特別計算対象区分
           MOVE    SPA-GMN-NINJYOTAIKBN-MAX
                                   TO  W65-NINJYOTAIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NINJYOTAIKBN-MAX )
               MOVE    SPA-GMN-NINJYOTAIKBNLST (IDX-CMB)
                                   TO  W65-NINJYOTAIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    ７０歳未満現物給付高額療養費合算計算
           MOVE    SPA-GMN-KOGAKKEIKBN-MAX
                                   TO  W65-KOGAKKEIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KOGAKKEIKBN-MAX )
               MOVE    SPA-GMN-KOGAKKEIKBNLST (IDX-CMB)
                                   TO  W65-KOGAKKEIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    ７０歳未満現物給付高額療養費合算計算対象区分
           MOVE    SPA-GMN-KOGAKKEITAIKBN-MAX
                                   TO  W65-KOGAKKEITAIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KOGAKKEITAIKBN-MAX )
               MOVE    SPA-GMN-KOGAKKEITAIKBNLST (IDX-CMB)
                                   TO  W65-KOGAKKEITAIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    短期滞在手術等基本料２、３の特別計算
           MOVE    SPA-GMN-TANKIKBN-MAX
                                   TO  W65-TANKIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-TANKIKBN-MAX )
               MOVE    SPA-GMN-TANKIKBNLST (IDX-CMB)
                                   TO  W65-TANKIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    短期滞在手術等基本料２、３の特別計算対象区分
           MOVE    SPA-GMN-TANKITAIKBN-MAX
                                   TO  W65-TANKITAIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-TANKITAIKBN-MAX )
               MOVE    SPA-GMN-TANKITAIKBNLST (IDX-CMB)
                                   TO  W65-TANKITAIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    負担金未発生分の回数カウント（入院）区分
           MOVE    SPA-GMN-KAICNTKBNN-MAX
                                   TO  W65-KAICNTKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KAICNTKBNN-MAX )
               MOVE    SPA-GMN-KAICNTKBNNLST (IDX-CMB)
                                   TO  W65-KAICNTKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    負担金未発生分の回数カウント（入院）対象区分
           MOVE    SPA-GMN-KAICNTTAIKBNN-MAX
                                   TO  W65-KAICNTTAIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KAICNTTAIKBNN-MAX )
               MOVE    SPA-GMN-KAICNTTAIKBNNLST (IDX-CMB)
                                   TO  W65-KAICNTTAIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    長期併用時の特別計算（入院）区分
           MOVE    SPA-GMN-CYOKEIKBNN-MAX
                                   TO  W65-CYOKEIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CYOKEIKBNN-MAX )
               MOVE    SPA-GMN-CYOKEIKBNNLST (IDX-CMB)
                                   TO  W65-CYOKEIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    長期併用時の特別計算（入院）対象区分
           MOVE    SPA-GMN-CYOKEITAIKBNN-MAX
                                   TO  W65-CYOKEITAIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CYOKEITAIKBNN-MAX )
               MOVE    SPA-GMN-CYOKEITAIKBNNLST (IDX-CMB)
                                   TO  W65-CYOKEITAIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    全国公費併用時の日の負担金計算（入院）区分
           MOVE    SPA-GMN-ZENDAYKBNN-MAX
                                   TO  W65-ZENDAYKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-ZENDAYKBNN-MAX )
               MOVE    SPA-GMN-ZENDAYKBNNLST (IDX-CMB)
                                   TO  W65-ZENDAYKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    全国公費併用時の日の負担金計算（入院）対象区分
           MOVE    SPA-GMN-ZENDAYTAIKBNN-MAX
                                   TO  W65-ZENDAYTAIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-ZENDAYTAIKBNN-MAX )
               MOVE    SPA-GMN-ZENDAYTAIKBNNLST (IDX-CMB)
                                   TO  W65-ZENDAYTAIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    全国公費併用時の日の負担金計算２（入院）区分
           MOVE    SPA-GMN-ZENDAY2KBNN-MAX
                                   TO  W65-ZENDAY2KBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-ZENDAY2KBNN-MAX )
               MOVE    SPA-GMN-ZENDAY2KBNNLST (IDX-CMB)
                                   TO  W65-ZENDAY2KBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    全国公費併用時の日の負担金計算２（入院）対象区分
           MOVE    SPA-GMN-ZENDAY2TAIKBNN-MAX
                                   TO  W65-ZENDAY2TAIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-ZENDAY2TAIKBNN-MAX )
               MOVE    SPA-GMN-ZENDAY2TAIKBNNLST (IDX-CMB)
                                   TO  W65-ZENDAY2TAIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    全国公費併用時の特別計算２（外来）
           MOVE    SPA-GMN-ZENKEI2KBNG-MAX
                                   TO  W65-ZENKEI2KBNG-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-ZENKEI2KBNG-MAX )
               MOVE    SPA-GMN-ZENKEI2KBNGLST (IDX-CMB)
                                   TO  W65-ZENKEI2KBNG-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    全国公費併用時の特別計算２（外来）対象区分
           MOVE    SPA-GMN-ZENKEI2TAIKBNG-MAX
                                   TO  W65-ZENKEI2TAIKBNG-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-ZENKEI2TAIKBNG-MAX )
               MOVE    SPA-GMN-ZENKEI2TAIKBNGLST (IDX-CMB)
                                   TO  W65-ZENKEI2TAIKBNG-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    初診時の負担金計算（外来）
           MOVE    SPA-GMN-SYOKEI2KBNG-MAX
                                   TO  W65-SYOKEI2KBNG-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SYOKEI2KBNG-MAX )
               MOVE    SPA-GMN-SYOKEI2KBNGLST (IDX-CMB)
                                   TO  W65-SYOKEI2KBNG-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    初診時の負担金計算（外来）対象区分
           MOVE    SPA-GMN-SYOKEI2TAIKBNG-MAX
                                   TO  W65-SYOKEI2TAIKBNG-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SYOKEI2TAIKBNG-MAX )
               MOVE    SPA-GMN-SYOKEI2TAIKBNGLST (IDX-CMB)
                                   TO  W65-SYOKEI2TAIKBNG-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    初診時の負担金計算（入院）
           MOVE    SPA-GMN-SYOKEI2KBNN-MAX
                                   TO  W65-SYOKEI2KBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SYOKEI2KBNN-MAX )
               MOVE    SPA-GMN-SYOKEI2KBNNLST (IDX-CMB)
                                   TO  W65-SYOKEI2KBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    初診時の負担金計算（入院）対象区分
           MOVE    SPA-GMN-SYOKEI2TAIKBNN-MAX
                                   TO  W65-SYOKEI2TAIKBNN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SYOKEI2TAIKBNN-MAX )
               MOVE    SPA-GMN-SYOKEI2TAIKBNNLST (IDX-CMB)
                                   TO  W65-SYOKEI2TAIKBNN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    限度額認定証提示が無い場合の特別計算（高齢者）
           MOVE    SPA-GMN-NINNASIKBNK-MAX
                                   TO  W65-NINNASIKBNK-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NINNASIKBNK-MAX )
               MOVE    SPA-GMN-NINNASIKBNKLST (IDX-CMB)
                                   TO  W65-NINNASIKBNK-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    限度額認定証提示が無い場合の特別計算（高齢者）対象区分
           MOVE    SPA-GMN-NINNASITAIKBNK-MAX
                                   TO  W65-NINNASITAIKBNK-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NINNASITAIKBNK-MAX )
               MOVE    SPA-GMN-NINNASITAIKBNKLST (IDX-CMB)
                                   TO  W65-NINNASITAIKBNK-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    マル長負担金計算処理
           MOVE    SPA-GMN-CYOKIFTNKBN-MAX
                                   TO  W65-CYOKIFTNKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CYOKIFTNKBN-MAX )
               MOVE    SPA-GMN-CYOKIFTNKBNLST (IDX-CMB)
                                   TO  W65-CYOKIFTNKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    マル長負担金計算処理対象区分
           MOVE    SPA-GMN-CYOKIFTNTAIKBN-MAX
                                   TO  W65-CYOKIFTNTAIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CYOKIFTNTAIKBN-MAX )
               MOVE    SPA-GMN-CYOKIFTNTAIKBNLST (IDX-CMB)
                                   TO  W65-CYOKIFTNTAIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    マル長レセプト記載処理区分
           MOVE    SPA-GMN-CYOKIKSIKBN-MAX
                                   TO  W65-CYOKIKSIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CYOKIKSIKBN-MAX )
               MOVE    SPA-GMN-CYOKIKSIKBNLST (IDX-CMB)
                                   TO  W65-CYOKIKSIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    マル長レセプト記載処理対象区分
           MOVE    SPA-GMN-CYOKIKSITAIKBN-MAX
                                   TO  W65-CYOKIKSITAIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CYOKIKSITAIKBN-MAX )
               MOVE    SPA-GMN-CYOKIKSITAIKBNLST (IDX-CMB)
                                   TO  W65-CYOKIKSITAIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    公費リスト
           MOVE    SPA-GMN-KOHLST-MAX  TO  W65-KOHLST-COUNT
           PERFORM VARYING IDX-LST    FROM    1   BY  1
               UNTIL     ( IDX-LST    >   SPA-GMN-KOHLST-MAX )
      *
               MOVE    IDX-LST         TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9        TO  W65-TKNO   (IDX-LST)
               IF    ( SPA-NAI-INPUT-MODE  =   2 )
                   IF    ( SPA-NAI-SELNUM1-FLG (IDX-LST)
                                       =   1 )
                       MOVE    "*"     TO  W65-TKNO   (IDX-LST)(1:1)
                   END-IF
               END-IF
      *
               MOVE    SPA-GMN-TKKOHNUM  (IDX-LST)
                                       TO  W65-TKKOHNUM (IDX-LST)
               MOVE    SPA-GMN-TKKOHNAME (IDX-LST)
                                       TO  W65-TKKOHNAME(IDX-LST)
               MOVE    SPA-GMN-TKYUKOSTYMD(IDX-LST)
                                       TO  W65-TKYUKSTYMD(IDX-LST)
               MOVE    SPA-GMN-TKYUKOEDYMD(IDX-LST)
                                       TO  W65-TKYUKEDYMD(IDX-LST)
      *
               IF    ( SPA-NAI-INPUT-MODE  =   2 )
                   CONTINUE
               ELSE
                   IF    ( SPA-GMN-KOHLST-SEL  =   IDX-LST )
                       MOVE    "T"     TO  W65-KOHLST-SELECT (IDX-LST)
                   ELSE
                       MOVE    "F"     TO  W65-KOHLST-SELECT (IDX-LST)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
      *    履歴リスト
           MOVE    SPA-GMN-RIREKILST-MAX
                                       TO  W65-RIREKILST-COUNT
      *
           PERFORM VARYING IDX-LST    FROM    1   BY  1
               UNTIL     ( IDX-LST    >   SPA-GMN-RIREKILST-MAX )
      *
               MOVE    IDX-LST         TO  WRK-ZZ9
               MOVE    WRK-ZZ9         TO  W65-TRNO (IDX-LST)
      *
               STRING  SPA-GMN-TRYUKOSTYMD (IDX-LST)
                       " 〜 "
                       SPA-GMN-TRYUKOEDYMD (IDX-LST)
                       DELIMITED   BY  SIZE
               INTO    W65-TRKIKAN (IDX-LST)
               END-STRING
      *
               IF    ( IDX-LST    =    SPA-GMN-RIREKILST-SEL )
                   MOVE    "T"         TO  W65-RIREKILST-SELECT(IDX-LST)
               ELSE
                   MOVE    "F"         TO  W65-RIREKILST-SELECT(IDX-LST)
               END-IF
      *
           END-PERFORM
      *
           MOVE    "black"                 TO  W65-LBLMODE-STYLE
           IF    ( SPA-NAI-INPUT-MODE  =   2 )
               MOVE    "blue"              TO  W65-LBLMODE-STYLE
               MOVE    "<一括変更＞"       TO  W65-LBLMODE
           END-IF
      *
      *    項目状態編集
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       5001-GMNHEN-SEC                 SECTION.
      *
           MOVE    SPA-GMN-KOHNUM-G    TO  W65-KOHNUM
           MOVE    SPA-GMN-KOHNUM-MAX  TO  W65-KOHNUM-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KOHNUM-MAX )
                   MOVE    SPA-GMN-KOHNUMLST (IDX-CMB)
                                   TO  W65-KOHNUM-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-YUKOSTYMD   TO  W65-YUKOSTYMD
           MOVE    SPA-GMN-YUKOEDYMD   TO  W65-YUKOEDYMD
           MOVE    SPA-GMN-SELNUM2     TO  W65-SELNUM2
      *
           .
       5001-GMNHEN-EXT.
           EXIT.
      *****************************************************************
      *    食事療養費設定ラベル編集処理
      *****************************************************************
       5002-LBLSKJ-EDIT-SEC           SECTION.
      *
            INITIALIZE                 WRK-CMB-ITM-G
      *
            EVALUATE    WRK-CMB-CD
            WHEN    "0"
                MOVE    "【使用しない】"
                                   TO  WRK-CMB-ITM
            WHEN    "1"
                MOVE    "【患者負担あり】"
                                   TO  WRK-CMB-ITM
            WHEN    "2"
                MOVE    "【患者負担あり（上限あり）】"
                                   TO  WRK-CMB-ITM
            WHEN    "3"
                MOVE    "【患者負担なし】"
                                   TO  WRK-CMB-ITM
            WHEN    "4"
                MOVE    "【患者負担あり（消費税あり）】"
                                   TO  WRK-CMB-ITM
            WHEN    "5"
                MOVE    "【患者負担あり（消費税なし）】"
                                   TO  WRK-CMB-ITM
            END-EVALUATE
      *
           .
       5002-LBLSKJ-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
           MOVE    WIDGET-NORMAL   TO
                                       W65-B06-STATE
                                       W65-B07-STATE
                                       W65-B08-STATE
                                       W65-KOHNUM-STATE
                                       W65-KOHNUM-ITEM-STATE
                                       W65-JNISSUTAIKBN-STATE
                                       W65-JNISSUTAIKBN-ITEM-STATE
                                       W65-TOTALTENTAIKBN-STATE
                                       W65-TOTALTENTAIKBN-ITEM-STATE
                                       W65-SYUKEITAIKBN-STATE
                                       W65-SYUKEITAIKBN-ITEM-STATE
                                       W65-FTNKEITAIKBNG-STATE
                                       W65-FTNKEITAIKBNG-ITEM-STATE
                                       W65-FTNKEITAIKBNN-STATE
                                       W65-FTNKEITAIKBNN-ITEM-STATE
                                       W65-FTNMONEYTAIKBNG-STATE
                                       W65-FTNMONEYTAIKBNG-ITEM-STATE
                                       W65-FTNMONEYTAIKBNN-STATE
                                       W65-FTNMONEYTAIKBNN-ITEM-STATE
                                       W65-FTNZEROTAIKBNG-STATE
                                       W65-FTNZEROTAIKBNG-ITEM-STATE
                                       W65-FTNZEROTAIKBNN-STATE
                                       W65-FTNZEROTAIKBNN-ITEM-STATE
                                       W65-SKJTAIKBN-STATE
                                       W65-SKJTAIKBN-ITEM-STATE
                                       W65-OCRTAIKBN-STATE
                                       W65-OCRTAIKBN-ITEM-STATE
                                       W65-SAIKEITAIKBN-STATE
                                       W65-SAIKEITAIKBN-ITEM-STATE
                                       W65-HKNFTNTAIKBNK-STATE
                                       W65-HKNFTNTAIKBNK-ITEM-STATE
                                       W65-HKNFTNTAIKBNY-STATE
                                       W65-HKNFTNTAIKBNY-ITEM-STATE
                                       W65-RECEKSITAIKBNG-STATE
                                       W65-RECEKSITAIKBNG-ITEM-STATE
                                       W65-RECEKSITAIKBNN-STATE
                                       W65-RECEKSITAIKBNN-ITEM-STATE
                                       W65-CHKJGNTAIKBNG-STATE
                                       W65-CHKJGNTAIKBNG-ITEM-STATE
                                       W65-CHKJGNTAIKBNN-STATE
                                       W65-CHKJGNTAIKBNN-ITEM-STATE
                                       W65-KOHSKYTAIKBNG-STATE
                                       W65-KOHSKYTAIKBNG-ITEM-STATE
                                       W65-KOHSKYTAIKBNN-STATE
                                       W65-KOHSKYTAIKBNN-ITEM-STATE
                                       W65-KAKTAIKBNK-STATE
                                       W65-KAKTAIKBNK-ITEM-STATE
                                       W65-KAK21TAIKBNK-STATE
                                       W65-KAK21TAIKBNK-ITEM-STATE
                                       W65-KAK22TAIKBNK-STATE
                                       W65-KAK22TAIKBNK-ITEM-STATE
                                       W65-KAK23TAIKBNK-STATE
                                       W65-KAK23TAIKBNK-ITEM-STATE
                                       W65-KAK31TAIKBNK-STATE
                                       W65-KAK31TAIKBNK-ITEM-STATE
                                       W65-KAK32TAIKBNK-STATE
                                       W65-KAK32TAIKBNK-ITEM-STATE
                                       W65-KAK41TAIKBNK-STATE
                                       W65-KAK41TAIKBNK-ITEM-STATE
                                       W65-KAK42TAIKBNK-STATE
                                       W65-KAK42TAIKBNK-ITEM-STATE
                                       W65-KAK43TAIKBNK-STATE
                                       W65-KAK43TAIKBNK-ITEM-STATE
                                       W65-KAKTAIKBNY-STATE
                                       W65-KAKTAIKBNY-ITEM-STATE
                                       W65-KAK21TAIKBNY-STATE
                                       W65-KAK21TAIKBNY-ITEM-STATE
                                       W65-KAK22TAIKBNY-STATE
                                       W65-KAK22TAIKBNY-ITEM-STATE
                                       W65-KAK23TAIKBNY-STATE
                                       W65-KAK23TAIKBNY-ITEM-STATE
                                       W65-KAK31TAIKBNY-STATE
                                       W65-KAK31TAIKBNY-ITEM-STATE
                                       W65-KAK32TAIKBNY-STATE
                                       W65-KAK32TAIKBNY-ITEM-STATE
                                       W65-KAK41TAIKBNY-STATE
                                       W65-KAK41TAIKBNY-ITEM-STATE
                                       W65-KAK42TAIKBNY-STATE
                                       W65-KAK42TAIKBNY-ITEM-STATE
                                       W65-KAK43TAIKBNY-STATE
                                       W65-KAK43TAIKBNY-ITEM-STATE
                                       W65-KSINGTAIKBNKG-STATE
                                       W65-KSINGTAIKBNKG-ITEM-STATE
                                       W65-KSINGTAIKBNKN-STATE
                                       W65-KSINGTAIKBNKN-ITEM-STATE
                                       W65-KSINGTAIKBNYG-STATE
                                       W65-KSINGTAIKBNYG-ITEM-STATE
                                       W65-KSINGTAIKBNYN-STATE
                                       W65-KSINGTAIKBNYN-ITEM-STATE
                                       W65-ENCHGTAIKBNKG-STATE
                                       W65-ENCHGTAIKBNKG-ITEM-STATE
                                       W65-ENCHGTAIKBNKN-STATE
                                       W65-ENCHGTAIKBNKN-ITEM-STATE
                                       W65-ENCHGTAIKBNYG-STATE
                                       W65-ENCHGTAIKBNYG-ITEM-STATE
                                       W65-ENCHGTAIKBNYN-STATE
                                       W65-ENCHGTAIKBNYN-ITEM-STATE
                                       W65-KOH2SKJTAIKBN-STATE
                                       W65-KOH2SKJTAIKBN-ITEM-STATE
                                       W65-FTNHASUTAIKBNG-STATE
                                       W65-FTNHASUTAIKBNG-ITEM-STATE
                                       W65-FTNHASUTAIKBNN-STATE
                                       W65-FTNHASUTAIKBNN-ITEM-STATE
                                       W65-JYUHENTAIKBN-STATE
                                       W65-JYUHENTAIKBN-ITEM-STATE
                                       W65-JGNFTNTAIKBN-STATE
                                       W65-JGNFTNTAIKBN-ITEM-STATE
                                       W65-PTJGNTAIKBN-STATE
                                       W65-PTJGNTAIKBN-ITEM-STATE
                                       W65-IRYOKEITAIKBNG-STATE
                                       W65-IRYOKEITAIKBNG-ITEM-STATE
                                       W65-IRYOKEITAIKBNN-STATE
                                       W65-IRYOKEITAIKBNN-ITEM-STATE
                                       W65-SKJKEITAIKBN-STATE
                                       W65-SKJKEITAIKBN-ITEM-STATE
                                       W65-KINJYOTAIKBNG-STATE
                                       W65-KINJYOTAIKBNG-ITEM-STATE
                                       W65-KINJYOTAIKBNN-STATE
                                       W65-KINJYOTAIKBNN-ITEM-STATE
                                       W65-INNAITAIKBN-STATE
                                       W65-INNAITAIKBN-ITEM-STATE
                                       W65-JIRITUTAIKBN-STATE
                                       W65-JIRITUTAIKBN-ITEM-STATE
                                       W65-SKJRYOTAIKBNHON-STATE
                                       W65-SKJRYOTAIKBNHON-ITEM-STATE
                                       W65-SEIRYOTAIKBNHON-STATE
                                       W65-SEIRYOTAIKBNHON-ITEM-STATE
                                       W65-SEIKANTAIKBNHON-STATE
                                       W65-SEIKANTAIKBNHON-ITEM-STATE
                                       W65-SKJRYOTAIKBNTSY-STATE
                                       W65-SKJRYOTAIKBNTSY-ITEM-STATE
                                       W65-SEIRYOTAIKBNTSY-STATE
                                       W65-SEIRYOTAIKBNTSY-ITEM-STATE
                                       W65-SEIKANTAIKBNTSY-STATE
                                       W65-SEIKANTAIKBNTSY-ITEM-STATE
                                       W65-SKJRYOTAIKBNTNK-STATE
                                       W65-SKJRYOTAIKBNTNK-ITEM-STATE
                                       W65-SEIRYOTAIKBNTNK-STATE
                                       W65-SEIRYOTAIKBNTNK-ITEM-STATE
                                       W65-SEIKANTAIKBNTNK-STATE
                                       W65-SEIKANTAIKBNTNK-ITEM-STATE
                                       W65-KAICNTTAIKBNG-STATE
                                       W65-KAICNTTAIKBNG-ITEM-STATE
                                       W65-CYOKEITAIKBNG-STATE
                                       W65-CYOKEITAIKBNG-ITEM-STATE
                                       W65-GNBKEITAIKBNKG-STATE
                                       W65-GNBKEITAIKBNKG-ITEM-STATE
                                       W65-ZENKEITAIKBNG-STATE
                                       W65-ZENKEITAIKBNG-ITEM-STATE
                                       W65-SINKEITAIKBNG-STATE
                                       W65-SINKEITAIKBNG-ITEM-STATE
                                       W65-SYOKEITAIKBNG-STATE
                                       W65-SYOKEITAIKBNG-ITEM-STATE
                                       W65-SYOKEITAIKBNN-STATE
                                       W65-SYOKEITAIKBNN-ITEM-STATE
                                       W65-NYUKEITAIKBNN-STATE
                                       W65-NYUKEITAIKBNN-ITEM-STATE
                                       W65-DAYKEITAIKBNN-STATE
                                       W65-DAYKEITAIKBNN-ITEM-STATE
                                       W65-NINNASITAIKBN-STATE
                                       W65-NINNASITAIKBN-ITEM-STATE
                                       W65-NINJYOTAIKBN-STATE
                                       W65-NINJYOTAIKBN-ITEM-STATE
                                       W65-KOGAKKEITAIKBN-STATE
                                       W65-KOGAKKEITAIKBN-ITEM-STATE
                                       W65-TANKITAIKBN-STATE
                                       W65-TANKITAIKBN-ITEM-STATE
                                       W65-ZENKEI2TAIKBNG-STATE
                                       W65-ZENKEI2TAIKBNG-ITEM-STATE
                                       W65-SYOKEI2TAIKBNG-STATE
                                       W65-SYOKEI2TAIKBNG-ITEM-STATE
                                       W65-SYOKEI2TAIKBNN-STATE
                                       W65-SYOKEI2TAIKBNN-ITEM-STATE
                                       W65-NINNASITAIKBNK-STATE
                                       W65-NINNASITAIKBNK-ITEM-STATE
                                       W65-CYOKIFTNTAIKBN-STATE
                                       W65-CYOKIFTNTAIKBN-ITEM-STATE
                                       W65-CYOKIKSITAIKBN-STATE
                                       W65-CYOKIKSITAIKBN-ITEM-STATE
                                       W65-YUKOSTYMD-STATE
                                       W65-YUKOEDYMD-STATE
                                       W65-SELNUM2-STATE
                                       W65-N01-STATE
                                       W65-N02-STATE
                                       W65-N03-STATE
                                       W65-N04-STATE
                                       W65-N05-STATE
                                       W65-N06-STATE
                                       W65-N07-STATE
                                       W65-N08-STATE
                                       W65-N09-STATE
                                       W65-N10-STATE
      *
      *
           IF    ( SPA-GMN-JGNFTNKBN    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-JGNFTNTAIKBN-STATE
                                       W65-JGNFTNTAIKBN-ITEM-STATE
           END-IF
      *
           IF    ( SPA-GMN-JNISSUKBN    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-JNISSUTAIKBN-STATE
                                       W65-JNISSUTAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-TOTALTENKBN  =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-TOTALTENTAIKBN-STATE
                                       W65-TOTALTENTAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SYUKEIKBN  =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SYUKEITAIKBN-STATE
                                       W65-SYUKEITAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-FTNKEIKBNG    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-FTNKEITAIKBNG-STATE
                                       W65-FTNKEITAIKBNG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-FTNKEIKBNN    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-FTNKEITAIKBNN-STATE
                                       W65-FTNKEITAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-FTNMONEYKBNG    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-FTNMONEYTAIKBNG-STATE
                                       W65-FTNMONEYTAIKBNG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-FTNMONEYKBNN    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-FTNMONEYTAIKBNN-STATE
                                       W65-FTNMONEYTAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-FTNZEROKBNG    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-FTNZEROTAIKBNG-STATE
                                       W65-FTNZEROTAIKBNG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-FTNZEROKBNN    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-FTNZEROTAIKBNN-STATE
                                       W65-FTNZEROTAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SKJKBN    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SKJTAIKBN-STATE
                                       W65-SKJTAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-OCRKBN    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-OCRTAIKBN-STATE
                                       W65-OCRTAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SAIKEIKBN    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                     TO  W65-SAIKEITAIKBN-STATE
                                         W65-SAIKEITAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-JYUHENKBN    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-JYUHENTAIKBN-STATE
                                       W65-JYUHENTAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-HKNFTNKBNK = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-HKNFTNTAIKBNK-STATE
                                       W65-HKNFTNTAIKBNK-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-HKNFTNKBNY = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-HKNFTNTAIKBNY-STATE
                                       W65-HKNFTNTAIKBNY-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-RECEKSIKBNG = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-RECEKSITAIKBNG-STATE
                                       W65-RECEKSITAIKBNG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-RECEKSIKBNN = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-RECEKSITAIKBNN-STATE
                                       W65-RECEKSITAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-CHKJGNKBNG = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-CHKJGNTAIKBNG-STATE
                                       W65-CHKJGNTAIKBNG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-CHKJGNKBNN = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-CHKJGNTAIKBNN-STATE
                                       W65-CHKJGNTAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KOHSKYKBNG = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KOHSKYTAIKBNG-STATE
                                       W65-KOHSKYTAIKBNG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KOHSKYKBNN = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KOHSKYTAIKBNN-STATE
                                       W65-KOHSKYTAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-PTJGNKBN = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-PTJGNTAIKBN-STATE
                                       W65-PTJGNTAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-IRYOKEIKBNG = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-IRYOKEITAIKBNG-STATE
                                       W65-IRYOKEITAIKBNG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-IRYOKEIKBNN = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-IRYOKEITAIKBNN-STATE
                                       W65-IRYOKEITAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SKJKEIKBN = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SKJKEITAIKBN-STATE
                                       W65-SKJKEITAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KINJYOKBNG = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KINJYOTAIKBNG-STATE
                                       W65-KINJYOTAIKBNG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KINJYOKBNN = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KINJYOTAIKBNN-STATE
                                       W65-KINJYOTAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-INNAIKBN = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-INNAITAIKBN-STATE
                                       W65-INNAITAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-JIRITUKBN = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-JIRITUTAIKBN-STATE
                                       W65-JIRITUTAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SKJRYOKBNHON = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SKJRYOTAIKBNHON-STATE
                                       W65-SKJRYOTAIKBNHON-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SEIRYOKBNHON = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SEIRYOTAIKBNHON-STATE
                                       W65-SEIRYOTAIKBNHON-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SEIKANKBNHON = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SEIKANTAIKBNHON-STATE
                                       W65-SEIKANTAIKBNHON-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SKJRYOKBNTSY = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SKJRYOTAIKBNTSY-STATE
                                       W65-SKJRYOTAIKBNTSY-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SEIRYOKBNTSY = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SEIRYOTAIKBNTSY-STATE
                                       W65-SEIRYOTAIKBNTSY-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SEIKANKBNTSY = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SEIKANTAIKBNTSY-STATE
                                       W65-SEIKANTAIKBNTSY-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SKJRYOKBNTNK = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SKJRYOTAIKBNTNK-STATE
                                       W65-SKJRYOTAIKBNTNK-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SEIRYOKBNTNK = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SEIRYOTAIKBNTNK-STATE
                                       W65-SEIRYOTAIKBNTNK-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SEIKANKBNTNK = "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SEIKANTAIKBNTNK-STATE
                                       W65-SEIKANTAIKBNTNK-ITEM-STATE
           END-IF
      *
      *
           IF    ( SPA-GMN-KAKKBNK     =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KAKTAIKBNK-STATE
                                       W65-KAKTAIKBNK-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KAK21KBNK   =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KAK21TAIKBNK-STATE
                                       W65-KAK21TAIKBNK-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KAK22KBNK   =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KAK22TAIKBNK-STATE
                                       W65-KAK22TAIKBNK-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KAK23KBNK   =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KAK23TAIKBNK-STATE
                                       W65-KAK23TAIKBNK-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KAK31KBNK   =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KAK31TAIKBNK-STATE
                                       W65-KAK31TAIKBNK-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KAK32KBNK   =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KAK32TAIKBNK-STATE
                                       W65-KAK32TAIKBNK-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KAK41KBNK   =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KAK41TAIKBNK-STATE
                                       W65-KAK41TAIKBNK-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KAK42KBNK   =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KAK42TAIKBNK-STATE
                                       W65-KAK42TAIKBNK-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KAK43KBNK   =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KAK43TAIKBNK-STATE
                                       W65-KAK43TAIKBNK-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KAKKBNY     =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KAKTAIKBNY-STATE
                                       W65-KAKTAIKBNY-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KAK21KBNY   =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KAK21TAIKBNY-STATE
                                       W65-KAK21TAIKBNY-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KAK22KBNY   =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KAK22TAIKBNY-STATE
                                       W65-KAK22TAIKBNY-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KAK23KBNY   =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KAK23TAIKBNY-STATE
                                       W65-KAK23TAIKBNY-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KAK31KBNY   =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KAK31TAIKBNY-STATE
                                       W65-KAK31TAIKBNY-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KAK32KBNY   =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KAK32TAIKBNY-STATE
                                       W65-KAK32TAIKBNY-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KAK41KBNY   =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KAK41TAIKBNY-STATE
                                       W65-KAK41TAIKBNY-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KAK42KBNY   =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KAK42TAIKBNY-STATE
                                       W65-KAK42TAIKBNY-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KAK43KBNY   =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KAK43TAIKBNY-STATE
                                       W65-KAK43TAIKBNY-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KSINGKBNKG  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KSINGTAIKBNKG-STATE
                                       W65-KSINGTAIKBNKG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KSINGKBNKN  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KSINGTAIKBNKN-STATE
                                       W65-KSINGTAIKBNKN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KSINGKBNYG  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KSINGTAIKBNYG-STATE
                                       W65-KSINGTAIKBNYG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KSINGKBNYN  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KSINGTAIKBNYN-STATE
                                       W65-KSINGTAIKBNYN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-ENCHGKBNKG  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-ENCHGTAIKBNKG-STATE
                                       W65-ENCHGTAIKBNKG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-ENCHGKBNKN  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-ENCHGTAIKBNKN-STATE
                                       W65-ENCHGTAIKBNKN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-ENCHGKBNYG  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-ENCHGTAIKBNYG-STATE
                                       W65-ENCHGTAIKBNYG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-ENCHGKBNYN  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-ENCHGTAIKBNYN-STATE
                                       W65-ENCHGTAIKBNYN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KOH2SKJKBN  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KOH2SKJTAIKBN-STATE
                                       W65-KOH2SKJTAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-FTNHASUKBNG  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-FTNHASUTAIKBNG-STATE
                                       W65-FTNHASUTAIKBNG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-FTNHASUKBNN  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-FTNHASUTAIKBNN-STATE
                                       W65-FTNHASUTAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KAICNTKBNG  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KAICNTTAIKBNG-STATE
                                       W65-KAICNTTAIKBNG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-CYOKEIKBNG  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-CYOKEITAIKBNG-STATE
                                       W65-CYOKEITAIKBNG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-GNBKEIKBNKG =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-GNBKEITAIKBNKG-STATE
                                       W65-GNBKEITAIKBNKG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-ZENKEIKBNG  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-ZENKEITAIKBNG-STATE
                                       W65-ZENKEITAIKBNG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SINKEIKBNG  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SINKEITAIKBNG-STATE
                                       W65-SINKEITAIKBNG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SYOKEIKBNG  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SYOKEITAIKBNG-STATE
                                       W65-SYOKEITAIKBNG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SYOKEIKBNN  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SYOKEITAIKBNN-STATE
                                       W65-SYOKEITAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-NYUKEIKBNN  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-NYUKEITAIKBNN-STATE
                                       W65-NYUKEITAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-DAYKEIKBNN  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-DAYKEITAIKBNN-STATE
                                       W65-DAYKEITAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-NINNASIKBN  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-NINNASITAIKBN-STATE
                                       W65-NINNASITAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-NINJYOKBN   =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-NINJYOTAIKBN-STATE
                                       W65-NINJYOTAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KOGAKKEIKBN   =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KOGAKKEITAIKBN-STATE
                                       W65-KOGAKKEITAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-TANKIKBN   =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-TANKITAIKBN-STATE
                                       W65-TANKITAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-KAICNTKBNN   =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-KAICNTTAIKBNN-STATE
                                       W65-KAICNTTAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-CYOKEIKBNN   =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-CYOKEITAIKBNN-STATE
                                       W65-CYOKEITAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-ZENDAYKBNN   =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-ZENDAYTAIKBNN-STATE
                                       W65-ZENDAYTAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-ZENDAY2KBNN   =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-ZENDAY2TAIKBNN-STATE
                                       W65-ZENDAY2TAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-ZENKEI2KBNG  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-ZENKEI2TAIKBNG-STATE
                                       W65-ZENKEI2TAIKBNG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SYOKEI2KBNG  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SYOKEI2TAIKBNG-STATE
                                       W65-SYOKEI2TAIKBNG-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-SYOKEI2KBNN  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-SYOKEI2TAIKBNN-STATE
                                       W65-SYOKEI2TAIKBNN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-NINNASIKBNK  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-NINNASITAIKBNK-STATE
                                       W65-NINNASITAIKBNK-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-CYOKIFTNKBN  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-CYOKIFTNTAIKBN-STATE
                                       W65-CYOKIFTNTAIKBN-ITEM-STATE
           END-IF
           IF    ( SPA-GMN-CYOKIKSIKBN  =  "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W65-CYOKIKSITAIKBN-STATE
                                       W65-CYOKIKSITAIKBN-ITEM-STATE
           END-IF
      *
           IF    ( SPA-GMN-KOHLST-MAX  >=  CONST-KOHLST-MAX )
               MOVE    WIDGET-INSENSITIVE  TO  W65-B06-STATE
           END-IF
      *
           IF    ( SPA-NAI-INPUT-MODE   =  2 )
               MOVE    WIDGET-INSENSITIVE
                                        TO W65-B06-STATE
                                           W65-B08-STATE
                                           W65-KOHNUM-STATE
                                           W65-KOHNUM-ITEM-STATE
                                           W65-YUKOSTYMD-STATE
                                           W65-YUKOEDYMD-STATE
                                           W65-SELNUM2-STATE
           END-IF
      *
           IF    ( SPA-GMN-KOHLST-SEL   >  0 )
            AND  ( SPA-GMN-KOHLST-SEL   <= SPA-GMN-KOHLST-MAX )
               MOVE    WIDGET-INSENSITIVE
                                        TO W65-KOHNUM-STATE
                                           W65-KOHNUM-ITEM-STATE
           END-IF
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-CUR         =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-SKIP-CUR-SEC
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    001
               MOVE    "SELNUM1"           TO  MCP-WIDGET
           WHEN    002
               MOVE    "KOHNUM"            TO  MCP-WIDGET
           WHEN    101
               MOVE    "JNISSUKBN"         TO  MCP-WIDGET
           WHEN    102
               MOVE    "JNISSUTAIKBN"      TO  MCP-WIDGET
           WHEN    103
               MOVE    "TOTALTENKBN"       TO  MCP-WIDGET
           WHEN    104
               MOVE    "TOTALTENTAIKBN"    TO  MCP-WIDGET
           WHEN    105
               MOVE    "SYUKEIKBN"         TO  MCP-WIDGET
           WHEN    106
               MOVE    "SYUKEITAIKBN"      TO  MCP-WIDGET
           WHEN    107
               MOVE    "FTNKEIKBNG"        TO  MCP-WIDGET
           WHEN    108
               MOVE    "FTNKEITAIKBNG"     TO  MCP-WIDGET
           WHEN    109
               MOVE    "FTNKEIKBNN"        TO  MCP-WIDGET
           WHEN    110
               MOVE    "FTNKEITAIKBNN"     TO  MCP-WIDGET
           WHEN    111
               MOVE    "FTNMONEYKBNG"      TO  MCP-WIDGET
           WHEN    112
               MOVE    "FTNMONEYTAIKBNG"   TO  MCP-WIDGET
           WHEN    113
               MOVE    "FTNMONEYKBNN"      TO  MCP-WIDGET
           WHEN    114
               MOVE    "FTNMONEYTAIKBNN"   TO  MCP-WIDGET
           WHEN    115
               MOVE    "FTNZEROKBNG"       TO  MCP-WIDGET
           WHEN    116
               MOVE    "FTNZEROTAIKBNG"    TO  MCP-WIDGET
           WHEN    117
               MOVE    "FTNZEROKBNN"       TO  MCP-WIDGET
           WHEN    118
               MOVE    "FTNZEROTAIKBNN"    TO  MCP-WIDGET
           WHEN    119
               MOVE    "SKJKBN"            TO  MCP-WIDGET
           WHEN    120
               MOVE    "SKJTAIKBN"         TO  MCP-WIDGET
           WHEN    121
               MOVE    "OCRKBN"            TO  MCP-WIDGET
           WHEN    122
               MOVE    "OCRTAIKBN"         TO  MCP-WIDGET
           WHEN    123
               MOVE    "SAIKEIKBN"         TO  MCP-WIDGET
           WHEN    124
               MOVE    "SAIKEITAIKBN"      TO  MCP-WIDGET
           WHEN    201
               MOVE    "HKNFTNKBNK"        TO  MCP-WIDGET
           WHEN    202
               MOVE    "HKNFTNTAIKBNK"     TO  MCP-WIDGET
           WHEN    203
               MOVE    "HKNFTNKBNY"        TO  MCP-WIDGET
           WHEN    204
               MOVE    "HKNFTNTAIKBNY"     TO  MCP-WIDGET
           WHEN    205
               MOVE    "RECEKSIKBNG"       TO  MCP-WIDGET
           WHEN    206
               MOVE    "RECEKSITAIKBNG"    TO  MCP-WIDGET
           WHEN    207
               MOVE    "RECEKSIKBNN"       TO  MCP-WIDGET
           WHEN    208
               MOVE    "RECEKSITAIKBNN"    TO  MCP-WIDGET
           WHEN    209
               MOVE    "CHKJGNKBNG"        TO  MCP-WIDGET
           WHEN    210
               MOVE    "CHKJGNTAIKBNG"     TO  MCP-WIDGET
           WHEN    211
               MOVE    "CHKJGNKBNN"        TO  MCP-WIDGET
           WHEN    212
               MOVE    "CHKJGNTAIKBNN"     TO  MCP-WIDGET
           WHEN    213
               MOVE    "KOHSKYKBNG"        TO  MCP-WIDGET
           WHEN    214
               MOVE    "KOHSKYTAIKBNG"     TO  MCP-WIDGET
           WHEN    215
               MOVE    "KOHSKYKBNN"        TO  MCP-WIDGET
           WHEN    216
               MOVE    "KOHSKYTAIKBNN"     TO  MCP-WIDGET
      *
           WHEN    301
               MOVE    "KAKKBNK"           TO  MCP-WIDGET
           WHEN    302
               MOVE    "KAKTAIKBNK"        TO  MCP-WIDGET
           WHEN    303
               MOVE    "KAK21KBNK"         TO  MCP-WIDGET
           WHEN    304
               MOVE    "KAK21TAIKBNK"      TO  MCP-WIDGET
           WHEN    305
               MOVE    "KAK22KBNK"         TO  MCP-WIDGET
           WHEN    306
               MOVE    "KAK22TAIKBNK"      TO  MCP-WIDGET
           WHEN    307
               MOVE    "KAK23KBNK"         TO  MCP-WIDGET
           WHEN    308
               MOVE    "KAK23TAIKBNK"      TO  MCP-WIDGET
           WHEN    309
               MOVE    "KAK31KBNK"         TO  MCP-WIDGET
           WHEN    310
               MOVE    "KAK31TAIKBNK"      TO  MCP-WIDGET
           WHEN    311
               MOVE    "KAK32KBNK"         TO  MCP-WIDGET
           WHEN    312
               MOVE    "KAK32TAIKBNK"      TO  MCP-WIDGET
           WHEN    313
               MOVE    "KAK41KBNK"         TO  MCP-WIDGET
           WHEN    314
               MOVE    "KAK41TAIKBNK"      TO  MCP-WIDGET
           WHEN    315
               MOVE    "KAK42KBNK"         TO  MCP-WIDGET
           WHEN    316
               MOVE    "KAK42TAIKBNK"      TO  MCP-WIDGET
           WHEN    317
               MOVE    "KAK43KBNK"         TO  MCP-WIDGET
           WHEN    318
               MOVE    "KAK43TAIKBNK"      TO  MCP-WIDGET
           WHEN    401
               MOVE    "KAKKBNY"           TO  MCP-WIDGET
           WHEN    402
               MOVE    "KAKTAIKBNY"        TO  MCP-WIDGET
           WHEN    403
               MOVE    "KAK21KBNY"         TO  MCP-WIDGET
           WHEN    404
               MOVE    "KAK21TAIKBNY"      TO  MCP-WIDGET
           WHEN    405
               MOVE    "KAK22KBNY"         TO  MCP-WIDGET
           WHEN    406
               MOVE    "KAK22TAIKBNY"      TO  MCP-WIDGET
           WHEN    407
               MOVE    "KAK23KBNY"         TO  MCP-WIDGET
           WHEN    408
               MOVE    "KAK23TAIKBNY"      TO  MCP-WIDGET
           WHEN    409
               MOVE    "KAK31KBNY"         TO  MCP-WIDGET
           WHEN    410
               MOVE    "KAK31TAIKBNY"      TO  MCP-WIDGET
           WHEN    411
               MOVE    "KAK32KBNY"         TO  MCP-WIDGET
           WHEN    412
               MOVE    "KAK32TAIKBNY"      TO  MCP-WIDGET
           WHEN    413
               MOVE    "KAK41KBNY"         TO  MCP-WIDGET
           WHEN    414
               MOVE    "KAK41TAIKBNY"      TO  MCP-WIDGET
           WHEN    415
               MOVE    "KAK42KBNY"         TO  MCP-WIDGET
           WHEN    416
               MOVE    "KAK42TAIKBNY"      TO  MCP-WIDGET
           WHEN    417
               MOVE    "KAK43KBNY"         TO  MCP-WIDGET
           WHEN    418
               MOVE    "KAK43TAIKBNY"      TO  MCP-WIDGET
           WHEN    501
               MOVE    "KSINGKBNKG"        TO  MCP-WIDGET
           WHEN    502
               MOVE    "KSINGTAIKBNKG"     TO  MCP-WIDGET
           WHEN    503
               MOVE    "KSINGKBNKN"        TO  MCP-WIDGET
           WHEN    504
               MOVE    "KSINGTAIKBNKN"     TO  MCP-WIDGET
           WHEN    505
               MOVE    "KSINGKBNYG"        TO  MCP-WIDGET
           WHEN    506
               MOVE    "KSINGTAIKBNYG"     TO  MCP-WIDGET
           WHEN    507
               MOVE    "KSINGKBNYN"        TO  MCP-WIDGET
           WHEN    508
               MOVE    "KSINGTAIKBNYN"     TO  MCP-WIDGET
           WHEN    509
               MOVE    "ENCHGKBNKG"        TO  MCP-WIDGET
           WHEN    510
               MOVE    "ENCHGTAIKBNKG"     TO  MCP-WIDGET
           WHEN    511
               MOVE    "ENCHGKBNKN"        TO  MCP-WIDGET
           WHEN    512
               MOVE    "ENCHGTAIKBNKN"     TO  MCP-WIDGET
           WHEN    513
               MOVE    "ENCHGKBNYG"        TO  MCP-WIDGET
           WHEN    514
               MOVE    "ENCHGTAIKBNYG"     TO  MCP-WIDGET
           WHEN    515
               MOVE    "ENCHGKBNYN"        TO  MCP-WIDGET
           WHEN    516
               MOVE    "ENCHGTAIKBNYN"     TO  MCP-WIDGET
           WHEN    517
               MOVE    "KOH2SKJKBN"        TO  MCP-WIDGET
           WHEN    518
               MOVE    "KOH2SKJTAIKBN"     TO  MCP-WIDGET
           WHEN    519
               MOVE    "FTNHASUKBNG"       TO  MCP-WIDGET
           WHEN    520
               MOVE    "FTNHASUTAIKBNG"    TO  MCP-WIDGET
           WHEN    521
               MOVE    "FTNHASUKBNN"       TO  MCP-WIDGET
           WHEN    522
               MOVE    "FTNHASUTAIKBNN"    TO  MCP-WIDGET
      *
           WHEN    601
               MOVE    "JYUHENKBN"         TO  MCP-WIDGET
           WHEN    602
               MOVE    "JYUHENTAIKBN"      TO  MCP-WIDGET
           WHEN    603
               MOVE    "JGNFTNKBN"         TO  MCP-WIDGET
           WHEN    604
               MOVE    "JGNFTNTAIKBN"      TO  MCP-WIDGET
           WHEN    605
               MOVE    "PTJGNKBN"          TO  MCP-WIDGET
           WHEN    606
               MOVE    "PTJGNTAIKBN"       TO  MCP-WIDGET
           WHEN    607
               MOVE    "IRYOKEIKBNG"       TO  MCP-WIDGET
           WHEN    608
               MOVE    "IRYOKEITAIKBNG"    TO  MCP-WIDGET
           WHEN    609
               MOVE    "IRYOKEIKBNN"       TO  MCP-WIDGET
           WHEN    610
               MOVE    "IRYOKEITAIKBNN"    TO  MCP-WIDGET
           WHEN    611
               MOVE    "SKJKEIKBN"         TO  MCP-WIDGET
           WHEN    612
               MOVE    "SKJKEITAIKBN"      TO  MCP-WIDGET
           WHEN    613
               MOVE    "KINJYOKBNG"         TO  MCP-WIDGET
           WHEN    614
               MOVE    "KINJYOTAIKBNG"      TO  MCP-WIDGET
           WHEN    615
               MOVE    "KINJYOKBNN"         TO  MCP-WIDGET
           WHEN    616
               MOVE    "KINJYOTAIKBNN"      TO  MCP-WIDGET
           WHEN    617
               MOVE    "INNAIKBN"          TO  MCP-WIDGET
           WHEN    618
               MOVE    "INNAITAIKBN"       TO  MCP-WIDGET
           WHEN    619
               MOVE    "JIRITUKBN"          TO  MCP-WIDGET
           WHEN    620
               MOVE    "JIRITUTAIKBN"       TO  MCP-WIDGET
           WHEN    701
               MOVE    "SKJRYOKBNHON"      TO  MCP-WIDGET
           WHEN    702
               MOVE    "SKJRYOTAIKBNHON"   TO  MCP-WIDGET
           WHEN    703
               MOVE    "SEIRYOKBNHON"      TO  MCP-WIDGET
           WHEN    704
               MOVE    "SEIRYOTAIKBNHON"   TO  MCP-WIDGET
           WHEN    705
               MOVE    "SEIKANKBNHON"      TO  MCP-WIDGET
           WHEN    706
               MOVE    "SEIKANTAIKBNHON"   TO  MCP-WIDGET
           WHEN    707
               MOVE    "SKJRYOKBNTSY"      TO  MCP-WIDGET
           WHEN    708
               MOVE    "SKJRYOTAIKBNTSY"   TO  MCP-WIDGET
           WHEN    709
               MOVE    "SEIRYOKBNTSY"      TO  MCP-WIDGET
           WHEN    710
               MOVE    "SEIRYOTAIKBNTSY"   TO  MCP-WIDGET
           WHEN    711
               MOVE    "SEIKANKBNTSY"      TO  MCP-WIDGET
           WHEN    712
               MOVE    "SEIKANTAIKBNTSY"   TO  MCP-WIDGET
           WHEN    713
               MOVE    "SKJRYOKBNTNK"      TO  MCP-WIDGET
           WHEN    714
               MOVE    "SKJRYOTAIKBNTNK"   TO  MCP-WIDGET
           WHEN    715
               MOVE    "SEIRYOKBNTNK"      TO  MCP-WIDGET
           WHEN    716
               MOVE    "SEIRYOTAIKBNTNK"   TO  MCP-WIDGET
           WHEN    717
               MOVE    "SEIKANKBNTNK"      TO  MCP-WIDGET
           WHEN    718
               MOVE    "SEIKANTAIKBNTNK"   TO  MCP-WIDGET
           WHEN    801
              MOVE    "KAICNTKBNG"         TO  MCP-WIDGET
           WHEN    802
              MOVE    "KAICNTTAIKBNG"      TO  MCP-WIDGET
           WHEN    803
              MOVE    "GNBKEIKBNKG"        TO  MCP-WIDGET
           WHEN    804
              MOVE    "GNBKEITAIKBNKG"     TO  MCP-WIDGET
           WHEN    805
              MOVE    "ZENKEIKBNG"         TO  MCP-WIDGET
           WHEN    806
              MOVE    "ZENKEITAIKBNG"      TO  MCP-WIDGET
           WHEN    807
              MOVE    "SINKEIKBNG"         TO  MCP-WIDGET
           WHEN    808
              MOVE    "SINKEITAIKBNG"      TO  MCP-WIDGET
           WHEN    809
              MOVE    "SYOKEIKBNG"         TO  MCP-WIDGET
           WHEN    810
              MOVE    "SYOKEITAIKBNG"      TO  MCP-WIDGET
           WHEN    811
              MOVE    "SYOKEIKBNN"         TO  MCP-WIDGET
           WHEN    812
              MOVE    "SYOKEITAIKBNN"      TO  MCP-WIDGET
           WHEN    813
              MOVE    "NYUKEIKBNN"         TO  MCP-WIDGET
           WHEN    814
              MOVE    "NYUKEITAIKBNN"      TO  MCP-WIDGET
           WHEN    815
              MOVE    "DAYKEIKBNN"         TO  MCP-WIDGET
           WHEN    816
              MOVE    "DAYKEITAIKBNN"      TO  MCP-WIDGET
           WHEN    817
              MOVE    "NINNASIKBN"         TO  MCP-WIDGET
           WHEN    818
              MOVE    "NINNASITAIKBN"      TO  MCP-WIDGET
           WHEN    819
              MOVE    "NINJYOKBN"          TO  MCP-WIDGET
           WHEN    820
              MOVE    "NINJYOTAIKBN"       TO  MCP-WIDGET
           WHEN    821
              MOVE    "CYOKEIKBNG"         TO  MCP-WIDGET
           WHEN    822
              MOVE    "CYOKEITAIKBNG"      TO  MCP-WIDGET
           WHEN    823
              MOVE    "KOGAKKEIKBN"          TO  MCP-WIDGET
           WHEN    824
              MOVE    "KOGAKKEITAIKBN"       TO  MCP-WIDGET
           WHEN    825
              MOVE    "TANKIKBN"           TO  MCP-WIDGET
           WHEN    826
              MOVE    "TANKITAIKBN"        TO  MCP-WIDGET
           WHEN    901
              MOVE    "KAICNTKBNN"           TO  MCP-WIDGET
           WHEN    902
              MOVE    "KAICNTTAIKBNN"           TO  MCP-WIDGET
           WHEN    903
              MOVE    "CYOKEIKBNN"           TO  MCP-WIDGET
           WHEN    904
              MOVE    "CYOKEITAIKBNN"           TO  MCP-WIDGET
           WHEN    905
              MOVE    "ZENDAYKBNN"           TO  MCP-WIDGET
           WHEN    906
              MOVE    "ZENDAYTAIKBNN"           TO  MCP-WIDGET
           WHEN    907
              MOVE    "ZENKEI2KBNG"         TO  MCP-WIDGET
           WHEN    908
              MOVE    "ZENKEI2TAIKBNG"      TO  MCP-WIDGET
           WHEN    909
              MOVE    "ZENDAY2KBNN"         TO  MCP-WIDGET
           WHEN    910
              MOVE    "ZENDAY2TAIKBNN"      TO  MCP-WIDGET
           WHEN    911
              MOVE    "SYOKEI2KBNG"         TO  MCP-WIDGET
           WHEN    912
              MOVE    "SYOKEI2TAIKBNG"      TO  MCP-WIDGET
           WHEN    913
              MOVE    "SYOKEI2KBNN"         TO  MCP-WIDGET
           WHEN    914
              MOVE    "SYOKEI2TAIKBNN"      TO  MCP-WIDGET
           WHEN    915
              MOVE    "NINNASIKBNK"        TO  MCP-WIDGET
           WHEN    916
              MOVE    "NINNASITAIKBNK"     TO  MCP-WIDGET
           WHEN    1001
              MOVE    "CYOKIFTNKBN"        TO  MCP-WIDGET
           WHEN    1002
              MOVE    "CYOKIFTNTAIKBN"     TO  MCP-WIDGET
           WHEN    1003
              MOVE    "CYOKIKSIKBN"        TO  MCP-WIDGET
           WHEN    1004
              MOVE    "CYOKIKSITAIKBN"     TO  MCP-WIDGET
           WHEN    003
               MOVE    "SELNUM2"           TO  MCP-WIDGET
           WHEN    004
               MOVE    "YUKOSTYMD"         TO  MCP-WIDGET
           WHEN    005
               MOVE    "YUKOEDYMD"         TO  MCP-WIDGET
           WHEN    006
               MOVE    "KJNYMD"            TO  MCP-WIDGET
           WHEN    007
               MOVE    "B06"               TO  MCP-WIDGET
           WHEN    008
               MOVE    "B07"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SELNUM1"
               MOVE    002                 TO  SPA-GMN-CUR
           WHEN    "KOHNUM"
               PERFORM 4300-SET-CURSOR-SEC
           WHEN    "JNISSUKBN"
               MOVE    102                 TO  SPA-GMN-CUR
           WHEN    "JNISSUTAIKBN"
               MOVE    103                 TO  SPA-GMN-CUR
           WHEN    "TOTALTENKBN"
               MOVE    104                 TO  SPA-GMN-CUR
           WHEN    "TOTALTENTAIKBN"
               MOVE    105                 TO  SPA-GMN-CUR
           WHEN    "SYUKEIKBN"
               MOVE    106                 TO  SPA-GMN-CUR
           WHEN    "SYUKEITAIKBN"
               MOVE    107                 TO  SPA-GMN-CUR
           WHEN    "FTNKEIKBNG"
               MOVE    108                 TO  SPA-GMN-CUR
           WHEN    "FTNKEITAIKBNG"
               MOVE    109                 TO  SPA-GMN-CUR
           WHEN    "FTNKEIKBNN"
               MOVE    110                 TO  SPA-GMN-CUR
           WHEN    "FTNKEITAIKBNN"
               MOVE    111                 TO  SPA-GMN-CUR
           WHEN    "FTNMONEYKBNG"
               MOVE    112                 TO  SPA-GMN-CUR
           WHEN    "FTNMONEYTAIKBNG"
               MOVE    113                 TO  SPA-GMN-CUR
           WHEN    "FTNMONEYKBNN"
               MOVE    114                 TO  SPA-GMN-CUR
           WHEN    "FTNMONEYTAIKBNN"
               MOVE    115                 TO  SPA-GMN-CUR
           WHEN    "FTNZEROKBNG"
               MOVE    116                 TO  SPA-GMN-CUR
           WHEN    "FTNZEROTAIKBNG"
               MOVE    117                 TO  SPA-GMN-CUR
           WHEN    "FTNZEROKBNN"
               MOVE    118                 TO  SPA-GMN-CUR
           WHEN    "FTNZEROTAIKBNN"
               MOVE    119                 TO  SPA-GMN-CUR
           WHEN    "SKJKBN"
               MOVE    120                 TO  SPA-GMN-CUR
           WHEN    "SKJTAIKBN"
               MOVE    121                 TO  SPA-GMN-CUR
           WHEN    "OCRKBN"
               MOVE    122                 TO  SPA-GMN-CUR
           WHEN    "OCRTAIKBN"
               MOVE    123                 TO  SPA-GMN-CUR
           WHEN    "SAIKEIKBN"
               MOVE    124                 TO  SPA-GMN-CUR
           WHEN    "SAIKEITAIKBN"
               MOVE    004                 TO  SPA-GMN-CUR
           WHEN    "HKNFTNKBNK"
               MOVE    202                 TO  SPA-GMN-CUR
           WHEN    "HKNFTNTAIKBNK"
               MOVE    203                 TO  SPA-GMN-CUR
           WHEN    "HKNFTNKBNY"
               MOVE    204                 TO  SPA-GMN-CUR
           WHEN    "HKNFTNTAIKBNY"
               MOVE    205                 TO  SPA-GMN-CUR
           WHEN    "RECEKSIKBNG"
               MOVE    206                 TO  SPA-GMN-CUR
           WHEN    "RECEKSITAIKBNG"
               MOVE    207                 TO  SPA-GMN-CUR
           WHEN    "RECEKSIKBNN"
               MOVE    208                 TO  SPA-GMN-CUR
           WHEN    "RECEKSITAIKBNN"
               MOVE    209                 TO  SPA-GMN-CUR
           WHEN    "CHKJGNKBNG"
               MOVE    210                 TO  SPA-GMN-CUR
           WHEN    "CHKJGNTAIKBNG"
               MOVE    211                 TO  SPA-GMN-CUR
           WHEN    "CHKJGNKBNN"
               MOVE    212                 TO  SPA-GMN-CUR
           WHEN    "CHKJGNTAIKBNN"
               MOVE    213                 TO  SPA-GMN-CUR
           WHEN    "KOHSKYKBNG"
               MOVE    214                 TO  SPA-GMN-CUR
           WHEN    "KOHSKYTAIKBNG"
               MOVE    215                 TO  SPA-GMN-CUR
           WHEN    "KOHSKYKBNN"
               MOVE    216                 TO  SPA-GMN-CUR
           WHEN    "KOHSKYTAIKBNN"
               MOVE    004                 TO  SPA-GMN-CUR
      *
           WHEN    "KAKKBNK"
               MOVE    302                 TO  SPA-GMN-CUR
           WHEN    "KAKTAIKBNK"
               MOVE    309                 TO  SPA-GMN-CUR
           WHEN    "KAK31KBNK"
               MOVE    310                 TO  SPA-GMN-CUR
           WHEN    "KAK31TAIKBNK"
               MOVE    311                 TO  SPA-GMN-CUR
           WHEN    "KAK32KBNK"
               MOVE    312                 TO  SPA-GMN-CUR
           WHEN    "KAK32TAIKBNK"
               MOVE    313                 TO  SPA-GMN-CUR
           WHEN    "KAK41KBNK"
               MOVE    314                 TO  SPA-GMN-CUR
           WHEN    "KAK41TAIKBNK"
               MOVE    315                 TO  SPA-GMN-CUR
           WHEN    "KAK42KBNK"
               MOVE    316                 TO  SPA-GMN-CUR
           WHEN    "KAK42TAIKBNK"
               MOVE    317                 TO  SPA-GMN-CUR
           WHEN    "KAK43KBNK"
               MOVE    318                 TO  SPA-GMN-CUR
           WHEN    "KAK43TAIKBNK"
               MOVE    303                 TO  SPA-GMN-CUR
           WHEN    "KAK21KBNK"
               MOVE    304                 TO  SPA-GMN-CUR
           WHEN    "KAK21TAIKBNK"
               MOVE    305                 TO  SPA-GMN-CUR
           WHEN    "KAK22KBNK"
               MOVE    306                 TO  SPA-GMN-CUR
           WHEN    "KAK22TAIKBNK"
               MOVE    307                 TO  SPA-GMN-CUR
           WHEN    "KAK23KBNK"
               MOVE    308                 TO  SPA-GMN-CUR
           WHEN    "KAK23TAIKBNK"
               MOVE    004                 TO  SPA-GMN-CUR
           WHEN    "KAKKBNY"
               MOVE    402                 TO  SPA-GMN-CUR
           WHEN    "KAKTAIKBNY"
               MOVE    409                 TO  SPA-GMN-CUR
           WHEN    "KAK31KBNY"
               MOVE    410                 TO  SPA-GMN-CUR
           WHEN    "KAK31TAIKBNY"
               MOVE    411                 TO  SPA-GMN-CUR
           WHEN    "KAK32KBNY"
               MOVE    412                 TO  SPA-GMN-CUR
           WHEN    "KAK32TAIKBNY"
               MOVE    413                 TO  SPA-GMN-CUR
           WHEN    "KAK41KBNY"
               MOVE    414                 TO  SPA-GMN-CUR
           WHEN    "KAK41TAIKBNY"
               MOVE    415                 TO  SPA-GMN-CUR
           WHEN    "KAK42KBNY"
               MOVE    416                 TO  SPA-GMN-CUR
           WHEN    "KAK42TAIKBNY"
               MOVE    417                 TO  SPA-GMN-CUR
           WHEN    "KAK43KBNY"
               MOVE    418                 TO  SPA-GMN-CUR
           WHEN    "KAK43TAIKBNY"
               MOVE    403                 TO  SPA-GMN-CUR
           WHEN    "KAK21KBNY"
               MOVE    404                 TO  SPA-GMN-CUR
           WHEN    "KAK21TAIKBNY"
               MOVE    405                 TO  SPA-GMN-CUR
           WHEN    "KAK22KBNY"
               MOVE    406                 TO  SPA-GMN-CUR
           WHEN    "KAK22TAIKBNY"
               MOVE    407                 TO  SPA-GMN-CUR
           WHEN    "KAK23KBNY"
               MOVE    408                 TO  SPA-GMN-CUR
           WHEN    "KAK23TAIKBNY"
               MOVE    004                 TO  SPA-GMN-CUR
           WHEN    "KSINGKBNKG"
               MOVE    502                 TO  SPA-GMN-CUR
           WHEN    "KSINGTAIKBNKG"
               MOVE    503                 TO  SPA-GMN-CUR
           WHEN    "KSINGKBNKN"
               MOVE    504                 TO  SPA-GMN-CUR
           WHEN    "KSINGTAIKBNKN"
               MOVE    505                 TO  SPA-GMN-CUR
           WHEN    "KSINGKBNYG"
               MOVE    506                 TO  SPA-GMN-CUR
           WHEN    "KSINGTAIKBNYG"
               MOVE    507                 TO  SPA-GMN-CUR
           WHEN    "KSINGKBNYN"
               MOVE    508                 TO  SPA-GMN-CUR
           WHEN    "KSINGTAIKBNYN"
               MOVE    509                 TO  SPA-GMN-CUR
           WHEN    "ENCHGKBNKG"
               MOVE    510                 TO  SPA-GMN-CUR
           WHEN    "ENCHGTAIKBNKG"
               MOVE    511                 TO  SPA-GMN-CUR
           WHEN    "ENCHGKBNKN"
               MOVE    512                 TO  SPA-GMN-CUR
           WHEN    "ENCHGTAIKBNKN"
               MOVE    513                 TO  SPA-GMN-CUR
           WHEN    "ENCHGKBNYG"
               MOVE    514                 TO  SPA-GMN-CUR
           WHEN    "ENCHGTAIKBNYG"
               MOVE    515                 TO  SPA-GMN-CUR
           WHEN    "ENCHGKBNYN"
               MOVE    516                 TO  SPA-GMN-CUR
           WHEN    "ENCHGTAIKBNYN"
               MOVE    517                 TO  SPA-GMN-CUR
           WHEN    "KOH2SKJKBN"
               MOVE    518                 TO  SPA-GMN-CUR
           WHEN    "KOH2SKJTAIKBN"
               MOVE    519                 TO  SPA-GMN-CUR
           WHEN    "FTNHASUKBNG"
               MOVE    520                 TO  SPA-GMN-CUR
           WHEN    "FTNHASUTAIKBNG"
               MOVE    521                 TO  SPA-GMN-CUR
           WHEN    "FTNHASUKBNN"
               MOVE    522                 TO  SPA-GMN-CUR
           WHEN    "FTNHASUTAIKBNN"
               MOVE    004                 TO  SPA-GMN-CUR
           WHEN    "JYUHENKBN"
               MOVE    602                 TO  SPA-GMN-CUR
           WHEN    "JYUHENTAIKBN"
               MOVE    603                 TO  SPA-GMN-CUR
           WHEN    "JGNFTNKBN"
               MOVE    604                 TO  SPA-GMN-CUR
           WHEN    "JGNFTNTAIKBN"
               MOVE    605                 TO  SPA-GMN-CUR
           WHEN    "PTJGNKBN"
               MOVE    606                 TO  SPA-GMN-CUR
           WHEN    "PTJGNTAIKBN"
               MOVE    607                 TO  SPA-GMN-CUR
           WHEN    "IRYOKEIKBNG"
               MOVE    608                 TO  SPA-GMN-CUR
           WHEN    "IRYOKEITAIKBNG"
               MOVE    609                 TO  SPA-GMN-CUR
           WHEN    "IRYOKEIKBNN"
               MOVE    610                 TO  SPA-GMN-CUR
           WHEN    "IRYOKEITAIKBNN"
               MOVE    611                 TO  SPA-GMN-CUR
           WHEN    "SKJKEIKBN"
               MOVE    612                 TO  SPA-GMN-CUR
           WHEN    "SKJKEITAIKBN"
               MOVE    613                 TO  SPA-GMN-CUR
           WHEN    "KINJYOKBNG"
               MOVE    614                 TO  SPA-GMN-CUR
           WHEN    "KINJYOTAIKBNG"
               MOVE    615                 TO  SPA-GMN-CUR
           WHEN    "KINJYOKBNN"
               MOVE    616                 TO  SPA-GMN-CUR
           WHEN    "KINJYOTAIKBNN"
               MOVE    617                 TO  SPA-GMN-CUR
           WHEN    "INNAIKBN"
               MOVE    618                 TO  SPA-GMN-CUR
           WHEN    "INNAITAIKBN"
               MOVE    619                 TO  SPA-GMN-CUR
           WHEN    "JIRITUKBN"
               MOVE    620                 TO  SPA-GMN-CUR
           WHEN    "JIRITUTAIKBN"
               MOVE    004                 TO  SPA-GMN-CUR
           WHEN    "SKJRYOKBNHON"
               MOVE    702                 TO  SPA-GMN-CUR
           WHEN    "SKJRYOTAIKBNHON"
               MOVE    703                 TO  SPA-GMN-CUR
           WHEN    "SEIRYOKBNHON"
               MOVE    704                 TO  SPA-GMN-CUR
           WHEN    "SEIRYOTAIKBNHON"
               MOVE    705                 TO  SPA-GMN-CUR
           WHEN    "SEIKANKBNHON"
               MOVE    706                 TO  SPA-GMN-CUR
           WHEN    "SEIKANTAIKBNHON"
               MOVE    707                 TO  SPA-GMN-CUR
           WHEN    "SKJRYOKBNTSY"
               MOVE    708                 TO  SPA-GMN-CUR
           WHEN    "SKJRYOTAIKBNTSY"
               MOVE    709                 TO  SPA-GMN-CUR
           WHEN    "SEIRYOKBNTSY"
               MOVE    710                 TO  SPA-GMN-CUR
           WHEN    "SEIRYOTAIKBNTSY"
               MOVE    711                 TO  SPA-GMN-CUR
           WHEN    "SEIKANKBNTSY"
               MOVE    712                 TO  SPA-GMN-CUR
           WHEN    "SEIKANTAIKBNTSY"
               MOVE    713                 TO  SPA-GMN-CUR
           WHEN    "SKJRYOKBNTNK"
               MOVE    714                 TO  SPA-GMN-CUR
           WHEN    "SKJRYOTAIKBNTNK"
               MOVE    715                 TO  SPA-GMN-CUR
           WHEN    "SEIRYOKBNTNK"
               MOVE    716                 TO  SPA-GMN-CUR
           WHEN    "SEIRYOTAIKBNTNK"
               MOVE    717                 TO  SPA-GMN-CUR
           WHEN    "SEIKANKBNTNK"
               MOVE    718                 TO  SPA-GMN-CUR
           WHEN    "SEIKANTAIKBNTNK"
               MOVE    004                 TO  SPA-GMN-CUR
           WHEN    "KAICNTKBNG"
               MOVE    802                 TO  SPA-GMN-CUR
           WHEN    "KAICNTTAIKBNG"
               MOVE    821                 TO  SPA-GMN-CUR
           WHEN    "CYOKEIKBNG"
               MOVE    822                 TO  SPA-GMN-CUR
           WHEN    "CYOKEITAIKBNG"
               MOVE    803                 TO  SPA-GMN-CUR
           WHEN    "GNBKEIKBNKG"
               MOVE    804                 TO  SPA-GMN-CUR
           WHEN    "GNBKEITAIKBNKG"
               MOVE    805                 TO  SPA-GMN-CUR
           WHEN    "ZENKEIKBNG"
               MOVE    806                 TO  SPA-GMN-CUR
           WHEN    "ZENKEITAIKBNG"
               MOVE    807                 TO  SPA-GMN-CUR
           WHEN    "SINKEIKBNG"
               MOVE    808                 TO  SPA-GMN-CUR
           WHEN    "SINKEITAIKBNG"
               MOVE    809                 TO  SPA-GMN-CUR
           WHEN    "SYOKEIKBNG"
               MOVE    810                 TO  SPA-GMN-CUR
           WHEN    "SYOKEITAIKBNG"
               MOVE    811                 TO  SPA-GMN-CUR
           WHEN    "SYOKEIKBNN"
               MOVE    812                 TO  SPA-GMN-CUR
           WHEN    "SYOKEITAIKBNN"
               MOVE    813                 TO  SPA-GMN-CUR
           WHEN    "NYUKEIKBNN"
               MOVE    814                 TO  SPA-GMN-CUR
           WHEN    "NYUKEITAIKBNN"
               MOVE    815                 TO  SPA-GMN-CUR
           WHEN    "DAYKEIKBNN"
               MOVE    816                 TO  SPA-GMN-CUR
           WHEN    "DAYKEITAIKBNN"
               MOVE    817                 TO  SPA-GMN-CUR
           WHEN    "NINNASIKBN"
               MOVE    818                 TO  SPA-GMN-CUR
           WHEN    "NINNASITAIKBN"
               MOVE    819                 TO  SPA-GMN-CUR
           WHEN    "NINJYOKBN"
               MOVE    820                 TO  SPA-GMN-CUR
           WHEN    "NINJYOTAIKBN"
               MOVE    823                 TO  SPA-GMN-CUR
           WHEN    "KOGAKKEIKBN"
               MOVE    824                 TO  SPA-GMN-CUR
           WHEN    "KOGAKKEITAIKBN"
               MOVE    825                 TO  SPA-GMN-CUR
           WHEN    "TANKIKBN"
               MOVE    826                 TO  SPA-GMN-CUR
           WHEN    "TANKITAIKBN"
               MOVE    004                 TO  SPA-GMN-CUR
           WHEN    "KAICNTKBNN"
               MOVE    902                 TO  SPA-GMN-CUR
           WHEN    "KAICNTTAIKBNN"
               MOVE    903                 TO  SPA-GMN-CUR
           WHEN    "CYOKEIKBNN"
               MOVE    904                 TO  SPA-GMN-CUR
           WHEN    "CYOKEITAIKBNN"
               MOVE    905                 TO  SPA-GMN-CUR
           WHEN    "ZENDAYKBNN"
               MOVE    906                 TO  SPA-GMN-CUR
           WHEN    "ZENDAYTAIKBNN"
               MOVE    907                 TO  SPA-GMN-CUR
           WHEN    "ZENKEI2KBNG"
               MOVE    908                 TO  SPA-GMN-CUR
           WHEN    "ZENKEI2TAIKBNG"
               MOVE    909                 TO  SPA-GMN-CUR
           WHEN    "ZENDAY2KBNN"
               MOVE    910                 TO  SPA-GMN-CUR
           WHEN    "ZENDAY2TAIKBNN"
               MOVE    911                 TO  SPA-GMN-CUR
           WHEN    "SYOKEI2KBNG"
               MOVE    912                 TO  SPA-GMN-CUR
           WHEN    "SYOKEI2TAIKBNG"
               MOVE    913                 TO  SPA-GMN-CUR
           WHEN    "SYOKEI2KBNN"
               MOVE    914                 TO  SPA-GMN-CUR
           WHEN    "SYOKEI2TAIKBNN"
               MOVE    915                 TO  SPA-GMN-CUR
           WHEN    "NINNASIKBNK"
               MOVE    916                 TO  SPA-GMN-CUR
           WHEN    "NINNASITAIKBNK"
               MOVE    004                 TO  SPA-GMN-CUR
      *
           WHEN    "CYOKIFTNKBN"
               MOVE    1002                TO  SPA-GMN-CUR
           WHEN    "CYOKIFTNTAIKBN"
               MOVE    1003                TO  SPA-GMN-CUR
           WHEN    "CYOKIKSIKBN"
               MOVE    1004                TO  SPA-GMN-CUR
           WHEN    "CYOKIKSITAIKBN"
               MOVE    004                 TO  SPA-GMN-CUR
      *
           WHEN    "SELNUM2"
               MOVE    002                 TO  SPA-GMN-CUR
           WHEN    "YUKOSTYMD"
               MOVE    005                 TO  SPA-GMN-CUR
           WHEN    "YUKOEDYMD"
               MOVE    007                 TO  SPA-GMN-CUR
           WHEN    "KJNYMD"
               MOVE    001                 TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルスキップ処理
      *****************************************************************
       5012-SKIP-CUR-SEC           SECTION.
      *
           IF    ( SPA-GMN-CUR         =   7 )
               IF    ( W65-B06-STATE   =   WIDGET-INSENSITIVE )
                   MOVE    8           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   1 )
               IF    ( SPA-GMN-KOHLST-MAX  =   ZERO )
                   MOVE    2           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   2)
               IF    ( W65-KOHNUM-STATE    =   WIDGET-INSENSITIVE )
                   PERFORM 4300-SET-CURSOR-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   102 )
               IF    ( W65-JNISSUTAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    103                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   104 )
               IF    ( W65-TOTALTENTAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    105                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   106 )
               IF    ( W65-SYUKEITAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    107                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   108 )
               IF    ( W65-FTNKEITAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    109                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   110 )
               IF    ( W65-FTNKEITAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    111                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   112 )
               IF    ( W65-FTNMONEYTAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    113                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   114 )
               IF    ( W65-FTNMONEYTAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    115                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   116 )
               IF    ( W65-FTNZEROTAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    117                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   118 )
               IF    ( W65-FTNZEROTAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    119                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   120 )
               IF    ( W65-SKJTAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    121                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   122 )
               IF    ( W65-OCRTAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    123                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   124 )
               IF    ( W65-SAIKEITAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    004                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   202 )
               IF    ( W65-HKNFTNTAIKBNK-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    203                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   204 )
               IF    ( W65-HKNFTNTAIKBNY-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    205                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   206 )
               IF    ( W65-RECEKSITAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    207                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   208 )
               IF    ( W65-RECEKSITAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    209                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   210 )
               IF    ( W65-CHKJGNTAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    211                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   212 )
               IF    ( W65-CHKJGNTAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    213                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   214 )
               IF    ( W65-KOHSKYTAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    215                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   216 )
               IF    ( W65-KOHSKYTAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    004                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   302 )
               IF    ( W65-KAKTAIKBNK-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    309                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   310 )
               IF    ( W65-KAK31TAIKBNK-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    311                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   312 )
               IF    ( W65-KAK32TAIKBNK-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    313                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   314 )
               IF    ( W65-KAK41TAIKBNK-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    315                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   316 )
               IF    ( W65-KAK42TAIKBNK-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    317                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   318 )
               IF    ( W65-KAK43TAIKBNK-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    303                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   304 )
               IF    ( W65-KAK21TAIKBNK-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    305                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   306 )
               IF    ( W65-KAK22TAIKBNK-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    307                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   308 )
               IF    ( W65-KAK23TAIKBNK-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    004                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   402 )
               IF    ( W65-KAKTAIKBNY-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    409                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   410 )
               IF    ( W65-KAK31TAIKBNY-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    411                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   412 )
               IF    ( W65-KAK32TAIKBNY-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    413                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   414 )
               IF    ( W65-KAK41TAIKBNY-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    415                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   416 )
               IF    ( W65-KAK42TAIKBNY-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    417                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   418 )
               IF    ( W65-KAK43TAIKBNY-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    403                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   404 )
               IF    ( W65-KAK21TAIKBNY-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    405                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   406 )
               IF    ( W65-KAK22TAIKBNY-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    407                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   408 )
               IF    ( W65-KAK23TAIKBNY-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    004                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   502 )
               IF    ( W65-KSINGTAIKBNKG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    503                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   504 )
               IF    ( W65-KSINGTAIKBNKN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    505                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   506 )
               IF    ( W65-KSINGTAIKBNYG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    507                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   508 )
               IF    ( W65-KSINGTAIKBNYN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    509                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   510 )
               IF    ( W65-ENCHGTAIKBNKG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    511                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   512 )
               IF    ( W65-ENCHGTAIKBNKN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    513                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   514 )
               IF    ( W65-ENCHGTAIKBNYG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    515                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   516 )
               IF    ( W65-ENCHGTAIKBNYN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    517                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   518 )
               IF    ( W65-KOH2SKJTAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    519                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   520 )
               IF    ( W65-FTNHASUTAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    521                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   522 )
               IF    ( W65-FTNHASUTAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    004                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   602 )
               IF    ( W65-JYUHENTAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    603                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   604 )
               IF    ( W65-JGNFTNTAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    605                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   606 )
               IF    ( W65-PTJGNTAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    607                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   608 )
               IF    ( W65-IRYOKEITAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    609                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   610 )
               IF    ( W65-IRYOKEITAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    611                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   612 )
               IF    ( W65-SKJKEITAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    613                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   614 )
               IF    ( W65-KINJYOTAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    615                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   616 )
               IF    ( W65-KINJYOTAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    617                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   618 )
               IF    ( W65-INNAITAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    619                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   620 )
               IF    ( W65-JIRITUTAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    004                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   702 )
               IF    ( W65-SKJRYOTAIKBNHON-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    703                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   704 )
               IF    ( W65-SEIRYOTAIKBNHON-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    705                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   706 )
               IF    ( W65-SEIKANTAIKBNHON-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    707                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   708 )
               IF    ( W65-SKJRYOTAIKBNTSY-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    709                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   710 )
               IF    ( W65-SEIRYOTAIKBNTSY-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    711                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   712 )
               IF    ( W65-SEIKANTAIKBNTSY-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    713                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   714 )
               IF    ( W65-SKJRYOTAIKBNTNK-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    715                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   716 )
               IF    ( W65-SEIRYOTAIKBNTNK-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    717                 TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   718 )
               IF    ( W65-SEIKANTAIKBNTNK-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    004                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   802 )
               IF    ( W65-KAICNTTAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    821                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   822 )
               IF    ( W65-CYOKEITAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    803                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   804 )
               IF    ( W65-GNBKEITAIKBNKG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    805                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   806 )
               IF    ( W65-ZENKEITAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    807                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   808 )
               IF    ( W65-SINKEITAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    809                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   810 )
               IF    ( W65-SYOKEITAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    811                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   812 )
               IF    ( W65-SYOKEITAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    813                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   814 )
               IF    ( W65-NYUKEITAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    815                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   816 )
               IF    ( W65-DAYKEITAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    817                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   818 )
               IF    ( W65-NINNASITAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    819                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   820 )
               IF    ( W65-NINJYOTAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    823                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   824 )
               IF    ( W65-KOGAKKEITAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    825                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   826 )
               IF    ( W65-TANKITAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    004                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   902 )
               IF    ( W65-KAICNTTAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    903         TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   904 )
               IF    ( W65-CYOKEITAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    905         TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   906 )
               IF    ( W65-ZENDAYTAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    907         TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   908 )
               IF    ( W65-ZENKEI2TAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    909         TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   910 )
               IF    ( W65-ZENDAY2TAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    911         TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   912 )
               IF    ( W65-SYOKEI2TAIKBNG-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    913         TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   914 )
               IF    ( W65-SYOKEI2TAIKBNN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    915         TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   916 )
               IF    ( W65-NINNASITAIKBNK-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    004         TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   1002 )
               IF    ( W65-CYOKIFTNTAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    1003        TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   1004 )
               IF    ( W65-CYOKIKSITAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    004         TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-NAI-INPUT-MODE  =   2 )
               EVALUATE    SPA-GMN-CUR
               WHEN    2
                   PERFORM 4300-SET-CURSOR-SEC
               WHEN    4
               WHEN    5
                   COMPUTE SPA-GMN-CUR =   8
               END-EVALUATE
           END-IF
      *
           .
       5012-SKIP-CUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "選択番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "公費番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "有効期間開始日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "有効期間終了日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "地方公費の番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "有効期間に該当公費番号のマスタがありません"
                                       TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "医療費負担金計算（入院）が０の時、"
                                       DELIMITED   BY  SIZE
                       "食事負担金計算（入院）は０を設定してください"
                                       DELIMITED   BY  SIZE
               INTO                        SPA-ERRMSG
               END-STRING
           WHEN    "0008"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "医療費負担金計算（入院）と"
                                       DELIMITED   BY  SIZE
                       "食事負担金計算（入院）の対象区分の設定が"
                                       DELIMITED   BY  SIZE
                       "異なります"
                                       DELIMITED   BY  SIZE
               INTO                    SPA-ERRMSG
               END-STRING
           WHEN    "0009"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "「食事療養費」付加設定に"
                                       DELIMITED   BY  SIZE
                       "登録不可な組み合わせが設定されています【"
                                       DELIMITED   BY  SIZE
                       WRK-TABNAME     DELIMITED   BY  SPACE
                                       "】"
                                       DELIMITED   BY  SIZE
               INTO                        SPA-ERRMSG
               END-STRING
           WHEN    "0014"
               MOVE    "基準日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0015"
               MOVE    "選択された番号が多すぎます"
                                       TO  SPA-ERRMSG
           WHEN    "0016"
               MOVE    "有効期間開始日は１日で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0017"
               MOVE    "有効期間終了日は月末日で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0018"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "金額加算"
                                       DELIMITED   BY  SIZE
                       "（分点ありの場合）が０の時、"
                                       DELIMITED   BY  SIZE
                       "（分点なしの場合）は０を設定してください"
                                       DELIMITED   BY  SIZE
               INTO                        SPA-ERRMSG
               END-STRING
           WHEN    "0019"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "記載"
                                       DELIMITED   BY  SIZE
                       "（分点ありの場合）が０の時、"
                                       DELIMITED   BY  SIZE
                       "（分点なしの場合）は０を設定してください"
                                       DELIMITED   BY  SIZE
               INTO                        SPA-ERRMSG
               END-STRING
           WHEN    "1008"
               MOVE    "範囲内の選択番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE    "有効期間開始日＞有効期間終了日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1101"
               MOVE     "公費番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1102"
               MOVE     "有効期間開始日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1103"
               MOVE     "有効期間終了日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1104"
               STRING  "有効期間の重複するデータが存在します（"
                                           DELIMITED   BY  SIZE
                       WRK-ERRSTYMD        DELIMITED   BY  SIZE
                       " 〜 "              DELIMITED   BY  SIZE
                       WRK-ERREDYMD        DELIMITED   BY  SIZE
                       "）"                DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1001"
               MOVE    "追加可能最大件数に達しました"
                                       TO  SPA-ERRMSG
           WHEN    "1105"
               MOVE    "公費を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "1106"
               MOVE    "公費番号の変更はできません"
                                       TO  SPA-ERRMSG
           WHEN    "1107"
               MOVE    "有効期間の開始日と終了日は同時に変更できません"
                                       TO  SPA-ERRMSG
           WHEN    "4000"  THRU    "4999"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W65"               TO  SPA-MOTOPG
           MOVE    "WERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-WIDCD
           WHEN    "2001"
               MOVE   "選択中の公費付加情報を削除します。よろしいですか"
                                        TO  WRK-WIDMSG
           WHEN    "3001"
               MOVE   "選択中の公費付加情報を変更します"
                                        TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W65"                TO  SPA-MOTOPG
           MOVE    "WID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    実日数省略記載コンボボックスチェック処理
      *****************************************************************
       4200-JNISSUKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-JNISSUKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-JNISSUKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-JNISSUKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-JNISSUKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-JNISSUKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求点省略記載コンボボックスチェック処理
      *****************************************************************
       4200-TOTALTENKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-TOTALTENKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-TOTALTENKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-TOTALTENKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-TOTALTENKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-TOTALTENKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用分請求点集計コンボボックスチェック処理
      *****************************************************************
       4200-SYUKEIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SYUKEIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SYUKEIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SYUKEIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               EVALUATE    SPA-PREFNUM
               WHEN    CONST-TOKYO
      *        WHEN    CONST-KYOTO
      *        WHEN    CONST-IBARAKI
               WHEN    CONST-SHIMANE
      *            MOVE    SPA-GMN-SYUKEIKBNLST (3)
                   MOVE    SPA-GMN-SYUKEIKBNLST (5)
                                   TO  WRK-CMB-ITM
                   MOVE    "1"     TO  WRK-CMB-ITM2
               WHEN    CONST-GUNMA
      *            MOVE    SPA-GMN-SYUKEIKBNLST (3)
                   MOVE    SPA-GMN-SYUKEIKBNLST (5)
                                   TO  WRK-CMB-ITM
                   MOVE    "4"     TO  WRK-CMB-ITM2
               WHEN    CONST-TOKUSHIMA
      *            MOVE    SPA-GMN-SYUKEIKBNLST (3)
      *            MOVE    SPA-GMN-SYUKEIKBNLST (5)
                   MOVE    SPA-GMN-SYUKEIKBNLST (4)
                                   TO  WRK-CMB-ITM
                   MOVE    "0"     TO  WRK-CMB-ITM2
               WHEN    OTHER
                   MOVE    SPA-GMN-SYUKEIKBNLST (1)
                                   TO  WRK-CMB-ITM
                   MOVE    "0"     TO  WRK-CMB-ITM2
               END-EVALUATE
           END-IF
      *
           .
       4200-SYUKEIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金金額集計コンボボックスチェック処理
      *****************************************************************
       4200-FTNKEIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-FTNKEIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-FTNKEIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-FTNKEIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-FTNKEIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-FTNKEIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金省略記載コンボボックスチェック処理
      *****************************************************************
       4200-FTNMONEYKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-FTNMONEYKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-FTNMONEYKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-FTNMONEYKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-FTNMONEYKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-FTNMONEYKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金０円記載（記録）コンボボックスチェック処理
      *****************************************************************
       4200-FTNZEROKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-FTNZEROKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-FTNZEROKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-FTNZEROKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-FTNZEROKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-FTNZEROKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院食事療養省略記載コンボボックスチェック処理
      *****************************************************************
       4200-SKJKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SKJKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SKJKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SKJKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SKJKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ＯＣＲ実日数請求点省略記載コンボボックスチェック処理
      *****************************************************************
       4200-OCRKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-OCRKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-OCRKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-OCRKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-OCRKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-OCRKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書公費再掲欄記載コンボボックスチェック処理
      *****************************************************************
       4200-SAIKEIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SAIKEIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SAIKEIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SAIKEIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SAIKEIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SAIKEIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄−一部負担金記載コンボボックスチェック処理
      *****************************************************************
       4200-HKNFTNKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-HKNFTNKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-HKNFTNKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-HKNFTNKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-HKNFTNKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-HKNFTNKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    設定と異なる記載設定コンボボックスチェック処理
      *****************************************************************
       4200-RECEKSIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-RECEKSIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-RECEKSIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-RECEKSIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-RECEKSIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-RECEKSIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象上限額設定（外来）コンボボックスチェック処理
      *****************************************************************
       4200-CHKJGNKBNGLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CHKJGNKBNG-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-CHKJGNKBNGL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-CHKJGNKBNGLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-CHKJGNKBNGLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-CHKJGNKBNGLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象上限額設定（入院）コンボボックスチェック処理
      *****************************************************************
       4200-CHKJGNKBNNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CHKJGNKBNN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-CHKJGNKBNNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-CHKJGNKBNNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-CHKJGNKBNNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-CHKJGNKBNNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象上限額設定対象区分コンボボックスチェック処理
      *****************************************************************
       4200-CHKJGNTAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CHKJGNTAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-CHKJGNTAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-CHKJGNTAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-CHKJGNTAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-CHKJGNTAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費請求テーブル作成コンボボックスチェック処理
      *****************************************************************
       4200-KOHSKYKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KOHSKYKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KOHSKYKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KOHSKYKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-KOHSKYKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KOHSKYKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    月途中受給者証変更時負担計算コンボボックスチェック処理
      *****************************************************************
       4200-JYUHENKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-JYUHENKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-JYUHENKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-JYUHENKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-JYUHENKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-JYUHENKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日・月上限負担計算コンボボックスチェック処理
      *****************************************************************
       4200-JGNFTNKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-JGNFTNKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-JGNFTNKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-JGNFTNKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-JGNFTNKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-JGNFTNKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者登録−所得者情報−月上限額入力コンボボックスチェック処理
      *****************************************************************
       4200-PTJGNKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-PTJGNKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-PTJGNKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-PTJGNKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-PTJGNKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-PTJGNKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者登録−所得者情報−月上限額入力対象区分コンボボックスチェック処理
      *****************************************************************
       4200-PTJGNTAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-PTJGNTAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-PTJGNTAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-PTJGNTAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-PTJGNTAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-PTJGNTAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医療費負担金計算コンボボックスチェック処理
      *****************************************************************
       4200-IRYOKEIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-IRYOKEIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-IRYOKEIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-IRYOKEIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-IRYOKEIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-IRYOKEIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医療費負担金計算対象区分コンボボックスチェック処理
      *****************************************************************
       4200-IRYOKEITAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-IRYOKEITAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-IRYOKEITAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-IRYOKEITAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-IRYOKEITAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-IRYOKEITAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事負担金計算コンボボックスチェック処理
      *****************************************************************
       4200-SKJKEIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJKEIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SKJKEIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SKJKEIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SKJKEIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SKJKEIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事負担金計算対象区分コンボボックスチェック処理
      *****************************************************************
       4200-SKJKEITAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJKEITAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SKJKEITAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SKJKEITAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SKJKEITAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SKJKEITAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    設定額を助成する負担金計算コンボボックスチェック処理
      *****************************************************************
       4200-KINJYOKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KINJYOKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KINJYOKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KINJYOKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-KINJYOKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KINJYOKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    設定額を助成する負担金計算対象区分コンボボックスチェック処理
      *****************************************************************
       4200-KINJYOTAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KINJYOTAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KINJYOTAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KINJYOTAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-KINJYOTAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KINJYOTAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    「月−院内−上限額」参照条件（外来）コンボボックスチェック処理
      *****************************************************************
       4200-INNAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-INNAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-INNAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-INNAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-INNAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-INNAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    「月−院内−上限額」参照条件（外来）対象区分コンボボックスチェック処理
      *****************************************************************
       4200-INNAITAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-INNAITAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-INNAITAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-INNAITAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-INNAITAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-INNAITAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    自立支援医療の特別計算コンボボックスチェック処理
      *****************************************************************
       4200-JIRITUKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-JIRITUKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-JIRITUKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-JIRITUKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-JIRITUKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-JIRITUKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    自立支援医療の特別計算対象区分コンボボックスチェック処理
      *****************************************************************
       4200-JIRITUTAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-JIRITUTAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-JIRITUTAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-JIRITUTAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-JIRITUTAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-JIRITUTAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事療養費付加設定コンボボックスチェック処理
      *****************************************************************
       4200-SKJFUKAKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJFUKAKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SKJFUKAKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SKJFUKAKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SKJFUKAKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SKJFUKAKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事療養費付加設定対象区分本人コンボボックスチェック処理
      *****************************************************************
       4200-SKJFUKATAIKBNHONLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJFUKATAIKBNHON-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SKJFUKATAIKBNHONL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SKJFUKATAIKBNHONLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SKJFUKATAIKBNHONLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SKJFUKATAIKBNHONLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事療養費付加設定対象区分低所得コンボボックスチェック処理
      *****************************************************************
       4200-SKJFUKATAIKBNTSYLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJFUKATAIKBNTSY-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SKJFUKATAIKBNTSYL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SKJFUKATAIKBNTSYLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SKJFUKATAIKBNTSYLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SKJFUKATAIKBNTSYLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事療養費付加設定対象区分低年金コンボボックスチェック処理
      *****************************************************************
       4200-SKJFUKATAIKBNTNKLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKJFUKATAIKBNTNK-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SKJFUKATAIKBNTNKL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SKJFUKATAIKBNTNKLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SKJFUKATAIKBNTNKLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SKJFUKATAIKBNTNKLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    共通対象区分コンボボックスチェック処理
      *****************************************************************
       4200-COMMONTAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-COMMONTAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-COMMONTAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-COMMONTAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-COMMONTAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-COMMONTAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書公費再掲欄記載対象区分コンボボックスチェック処理
      *****************************************************************
       4200-SAIKEITAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SAIKEITAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SAIKEITAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SAIKEITAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SAIKEITAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SAIKEITAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    月途中受給者証変更時負担計算対象区分コンボボックスチェック処理
      *****************************************************************
       4200-JYUHENTAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-JYUHENTAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-JYUHENTAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-JYUHENTAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-JYUHENTAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-JYUHENTAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲記載コンボチェック処理
      *****************************************************************
       4200-KAKKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KAKKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KAKKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KAKKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-KAKKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KAKKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）記載コンボチェック処理
      *****************************************************************
       4200-KAK20KBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KAK20KBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KAK20KBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KAK20KBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-KAK20KBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KAK20KBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額計算コンボチェック処理（主＋全＋地）
      *****************************************************************
       4200-KAK31KBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KAK31KBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KAK31KBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KAK31KBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-KAK31KBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KAK31KBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額計算コンボチェック処理（主＋全＋地＋長）
      *****************************************************************
       4200-KAK32KBNKLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KAK32KBNK-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KAK32KBNKL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KAK32KBNKLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-KAK32KBNKLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KAK32KBNKLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額計算コンボチェック処理（主＋全＋地＋長）
      *****************************************************************
       4200-KAK32KBNYLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KAK32KBNY-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KAK32KBNYL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KAK32KBNYLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-KAK32KBNYLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KAK32KBNYLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄の括弧再掲（下）金額加算コンボチェック処理
      *****************************************************************
       4200-KAK40KBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KAK40KBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KAK40KBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KAK40KBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-KAK40KBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KAK40KBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険欄−一部負担金無記載コンボチェック処理
      *****************************************************************
       4200-KSINGKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KSINGKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KSINGKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KSINGKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-KSINGKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KSINGKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金１円→１０円単位記載コンボチェック処理
      *****************************************************************
       4200-ENCHGKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-ENCHGKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-ENCHGKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-ENCHGKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-ENCHGKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-ENCHGKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院時食事療養記載（公費２欄置き換え）コンボチェック処理
      *****************************************************************
       4200-KOH2SKJKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KOH2SKJKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KOH2SKJKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KOH2SKJKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-KOH2SKJKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KOH2SKJKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金記載１円未満端数（外来）コンボチェック処理
      *****************************************************************
       4200-FTNHASUKBNGLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-FTNHASUKBNG-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-FTNHASUKBNGL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-FTNHASUKBNGLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-FTNHASUKBNGLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-FTNHASUKBNGLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金記載１円未満端数（外来）コンボチェック処理
      *****************************************************************
       4200-FTNHASUTAIKBNGLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-FTNHASUTAIKBNG-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-FTNHASUTAIKBNGL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-FTNHASUTAIKBNGLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-FTNHASUTAIKBNGLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-FTNHASUTAIKBNGLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金記載１円未満端数（入院）コンボチェック処理
      *****************************************************************
       4200-FTNHASUKBNNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-FTNHASUKBNN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-FTNHASUKBNNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-FTNHASUKBNNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-FTNHASUKBNNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-FTNHASUKBNNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金記載１円未満端数（入院）コンボチェック処理
      *****************************************************************
       4200-FTNHASUTAIKBNNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-FTNHASUTAIKBNN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-FTNHASUTAIKBNNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-FTNHASUTAIKBNNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-FTNHASUTAIKBNNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-FTNHASUTAIKBNNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    負担金未発生分の回数カウント（外来）コンボチェック処理
      *****************************************************************
       4200-KAICNTKBNGLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KAICNTKBNG-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KAICNTKBNGL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KAICNTKBNGLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-KAICNTKBNGLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KAICNTKBNGLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    負担金未発生分の回数カウント（外来）対象区分コンボチェック処理
      *****************************************************************
       4200-KAICNTTAIKBNGLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KAICNTTAIKBNG-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KAICNTTAIKBNGL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KAICNTTAIKBNGLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-KAICNTTAIKBNGLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KAICNTTAIKBNGLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    長期併用時の特別計算（外来）コンボチェック処理
      *****************************************************************
       4200-CYOKEIKBNGLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CYOKEIKBNG-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-CYOKEIKBNGL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-CYOKEIKBNGLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-CYOKEIKBNGLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-CYOKEIKBNGLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    長期併用時の特別計算（外来）対象区分コンボチェック処理
      *****************************************************************
       4200-CYOKEITAIKBNGLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CYOKEITAIKBNG-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-CYOKEITAIKBNGL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-CYOKEITAIKBNGLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-CYOKEITAIKBNGLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-CYOKEITAIKBNGLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    現物給付対象者の特別計算（高齢者・外）コンボチェック処理
      *****************************************************************
       4200-GNBKEIKBNKGLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-GNBKEIKBNKG-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-GNBKEIKBNKGL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-GNBKEIKBNKGLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-GNBKEIKBNKGLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-GNBKEIKBNKGLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    現物給付対象者の特別計算（高齢者・外）対象区分コンボチェック処理
      *****************************************************************
       4200-GNBKEITAIKBNKGLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-GNBKEITAIKBNKG-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-GNBKEITAIKBNKGL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-GNBKEITAIKBNKGLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-GNBKEITAIKBNKGLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-GNBKEITAIKBNKGLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の特別計算（外来）コンボチェック処理
      *****************************************************************
       4200-ZENKEIKBNGLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-ZENKEIKBNG-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-ZENKEIKBNGL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-ZENKEIKBNGLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-ZENKEIKBNGLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-ZENKEIKBNGLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の特別計算（外来）対象区分コンボチェック処理
      *****************************************************************
       4200-ZENKEITAIKBNGLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-ZENKEITAIKBNG-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-ZENKEITAIKBNGL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-ZENKEITAIKBNGLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-ZENKEITAIKBNGLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-ZENKEITAIKBNGLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診察料がない場合の負担金計算（外来）コンボチェック処理
      *****************************************************************
       4200-SINKEIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SINKEIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SINKEIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SINKEIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SINKEIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SINKEIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診察料がない場合の負担金計算（外来）対象区分コンボチェック処理
      *****************************************************************
       4200-SINKEITAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SINKEITAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SINKEITAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SINKEITAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SINKEITAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SINKEITAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初診時以外の負担金計算コンボチェック処理
      *****************************************************************
       4200-SYOKEIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SYOKEIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SYOKEIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SYOKEIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SYOKEIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SYOKEIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初診時以外の負担金計算対象区分コンボチェック処理
      *****************************************************************
       4200-SYOKEITAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SYOKEITAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SYOKEITAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SYOKEITAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SYOKEITAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SYOKEITAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    １入院＠日限度とする特別計算（入院）コンボチェック処理
      *****************************************************************
       4200-NYUKEIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NYUKEIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NYUKEIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-NYUKEIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-NYUKEIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-NYUKEIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    １入院＠日限度とする特別計算（入院）対象区分コンボチェック処理
      *****************************************************************
       4200-NYUKEITAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NYUKEITAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NYUKEITAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-NYUKEITAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-NYUKEITAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-NYUKEITAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日上限額を固定額とする特別計算（入院）コンボチェック処理
      *****************************************************************
       4200-DAYKEIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-DAYKEIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-DAYKEIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-DAYKEIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-DAYKEIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-DAYKEIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日上限額を固定額とする特別計算（入院）対象区分コンボチェック処理
      *****************************************************************
       4200-DAYKEITAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-DAYKEITAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-DAYKEITAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-DAYKEITAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-DAYKEITAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-DAYKEITAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    限度額認定証提示が無い場合の特別計算コンボチェック処理
      *****************************************************************
       4200-NINNASIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NINNASIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NINNASIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-NINNASIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-NINNASIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-NINNASIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    限度額認定証提示が無い場合の特別計算対象区分コンボチェック処理
      *****************************************************************
       4200-NINNASITAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NINNASITAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NINNASITAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-NINNASITAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-NINNASITAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-NINNASITAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    限度額認定証（上位）の場合の特別計算コンボチェック処理
      *****************************************************************
       4200-NINJYOKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NINJYOKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NINJYOKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-NINJYOKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-NINJYOKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-NINJYOKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    限度額認定証（上位）の場合の特別計算対象区分コンボチェック処理
      *****************************************************************
       4200-NINJYOTAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NINJYOTAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NINJYOTAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-NINJYOTAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-NINJYOTAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-NINJYOTAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ７０歳未満現物給付高額療養費合算計算コンボチェック処理
      *****************************************************************
       4200-KOGAKKEIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KOGAKKEIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KOGAKKEIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KOGAKKEIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-KOGAKKEIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KOGAKKEIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ７０歳未満現物給付高額療養費合算計算対象区分コンボチェック処理
      *****************************************************************
       4200-KOGAKKEITAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KOGAKKEITAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KOGAKKEITAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KOGAKKEITAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-KOGAKKEITAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KOGAKKEITAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    短期滞在手術等基本料２、３の特別計算コンボチェック処理
      *****************************************************************
       4200-TANKIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-TANKIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-TANKIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-TANKIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-TANKIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-TANKIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    短期滞在手術等基本料２、３の特別計算対象区分コンボチェック処理
      *****************************************************************
       4200-TANKITAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-TANKITAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-TANKITAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-TANKITAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-TANKITAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-TANKITAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    負担金未発生分の回数カウント（入院）区分コンボチェック処理
      *****************************************************************
       4200-KAICNTKBNNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KAICNTKBNN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KAICNTKBNNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KAICNTKBNNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-KAICNTKBNNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KAICNTKBNNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    負担金未発生分の回数カウント（入院）対象区分コンボチェック処理
      *****************************************************************
       4200-KAICNTTAIKBNNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KAICNTTAIKBNN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KAICNTTAIKBNNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KAICNTTAIKBNNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-KAICNTTAIKBNNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KAICNTTAIKBNNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    長期併用時の特別計算（入院）区分コンボチェック処理
      *****************************************************************
       4200-CYOKEIKBNNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CYOKEIKBNN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-CYOKEIKBNNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-CYOKEIKBNNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-CYOKEIKBNNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-CYOKEIKBNNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    長期併用時の特別計算（入院）対象区分コンボチェック処理
      *****************************************************************
       4200-CYOKEITAIKBNNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CYOKEITAIKBNN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-CYOKEITAIKBNNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-CYOKEITAIKBNNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-CYOKEITAIKBNNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-CYOKEITAIKBNNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の日の負担金計算（入院）区分コンボチェック処理
      *****************************************************************
       4200-ZENDAYKBNNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-ZENDAYKBNN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-ZENDAYKBNNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-ZENDAYKBNNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-ZENDAYKBNNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-ZENDAYKBNNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の日の負担金計算（入院）対象区分コンボチェック処理
      *****************************************************************
       4200-ZENDAYTAIKBNNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-ZENDAYTAIKBNN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-ZENDAYTAIKBNNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-ZENDAYTAIKBNNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-ZENDAYTAIKBNNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-ZENDAYTAIKBNNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の日の負担金計算２（入院）区分コンボチェック処理
      *****************************************************************
       4200-ZENDAY2KBNNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-ZENDAY2KBNN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-ZENDAY2KBNNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-ZENDAY2KBNNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-ZENDAY2KBNNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-ZENDAY2KBNNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の日の負担金計算２（入院）対象区分コンボチェック処理
      *****************************************************************
       4200-ZENDAY2TAIKBNNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-ZENDAY2TAIKBNN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-ZENDAY2TAIKBNNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-ZENDAY2TAIKBNNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-ZENDAY2TAIKBNNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-ZENDAY2TAIKBNNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の特別計算２（外来）コンボチェック処理
      *****************************************************************
       4200-ZENKEI2KBNGLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-ZENKEI2KBNG-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-ZENKEI2KBNGL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-ZENKEI2KBNGLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-ZENKEI2KBNGLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-ZENKEI2KBNGLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全国公費併用時の特別計算２（外来）対象区分コンボチェック処理
      *****************************************************************
       4200-ZENKEI2TAIKBNGLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-ZENKEI2TAIKBNG-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-ZENKEI2TAIKBNGL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-ZENKEI2TAIKBNGLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-ZENKEI2TAIKBNGLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-ZENKEI2TAIKBNGLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初診時の負担金計算（外来）コンボチェック処理
      *****************************************************************
       4200-SYOKEI2KBNGLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SYOKEI2KBNG-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SYOKEI2KBNGL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SYOKEI2KBNGLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SYOKEI2KBNGLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SYOKEI2KBNGLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初診時の負担金計算（外来）対象区分コンボチェック処理
      *****************************************************************
       4200-SYOKEI2TAIKBNGLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SYOKEI2TAIKBNG-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SYOKEI2TAIKBNGL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SYOKEI2TAIKBNGLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SYOKEI2TAIKBNGLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SYOKEI2TAIKBNGLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初診時の負担金計算（外来）コンボチェック処理
      *****************************************************************
       4200-SYOKEI2KBNNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SYOKEI2KBNN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SYOKEI2KBNNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SYOKEI2KBNNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SYOKEI2KBNNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SYOKEI2KBNNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初診時の負担金計算（外来）対象区分コンボチェック処理
      *****************************************************************
       4200-SYOKEI2TAIKBNNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SYOKEI2TAIKBNN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SYOKEI2TAIKBNNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SYOKEI2TAIKBNNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SYOKEI2TAIKBNNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SYOKEI2TAIKBNNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    限度額認定証提示が無い場合の特別計算（高齢者）コンボチェック処理
      *****************************************************************
       4200-NINNASIKBNKLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NINNASIKBNK-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NINNASIKBNKL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-NINNASIKBNKLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-NINNASIKBNKLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-NINNASIKBNKLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    限度額認定証提示が無い場合の特別計算（高齢者）対象区分コンボチェック処理
      *****************************************************************
       4200-NINNASITAIKBNKLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NINNASITAIKBNK-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NINNASITAIKBNKL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-NINNASITAIKBNKLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-NINNASITAIKBNKLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-NINNASITAIKBNKLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    マル長負担金計算処理コンボチェック処理
      *****************************************************************
       4200-CYOKIFTNKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CYOKIFTNKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-CYOKIFTNKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-CYOKIFTNKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-CYOKIFTNKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-CYOKIFTNKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    マル長負担金計算処理対象区分コンボチェック処理
      *****************************************************************
       4200-CYOKIFTNTAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CYOKIFTNTAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-CYOKIFTNTAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-CYOKIFTNTAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-CYOKIFTNTAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-CYOKIFTNTAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    マル長レセプト記載処理区分コンボチェック処理
      *****************************************************************
       4200-CYOKIKSIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CYOKIKSIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-CYOKIKSIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-CYOKIKSIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-CYOKIKSIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-CYOKIKSIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    マル長レセプト記載処理対象区分コンボチェック処理
      *****************************************************************
       4200-CYOKIKSITAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-CYOKIKSITAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-CYOKIKSITAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-CYOKIKSITAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-CYOKIKSITAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-CYOKIKSITAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費の種類コンボボックスチェック処理
      *****************************************************************
       4200-KOHNUMLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KOHNUM-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KOHNUML (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KOHNUMLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-KOHNUMLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    カーソル位置設定処理
      *****************************************************************
       4300-SET-CURSOR-SEC             SECTION.
      *
           COMPUTE SPA-GMN-CUR =   SPA-GMN-PAGENO  *   100 +   101
      *
           .
       4300-SET-CURSOR-EXT.
           EXIT.
      *****************************************************************
      *    全角半角混在チェック
      *****************************************************************
       800-ENRTRY-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-INPUT
                                   =   SPACE )
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-INPUT
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-INPUT
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    9           TO  FLG-ENTRYCHK
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    KANACHK-SYUBETU TO  FLG-ENTRYCHK
      *
           .
       800-ENRTRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       800-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-HIZUKECHK
      *
           IF    ( WRK-YMD         =   "00000000" 
                                   OR  "99999999" )
               MOVE    WRK-YMD     TO  WRK-HENYMDG
                                       WRK-SYMD
           ELSE
               INITIALIZE                  ORCSGDAYAREA
               MOVE    WRK-YMD         TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
               IF  ( SGDAY-RC          =   ZERO )
                   MOVE    SGDAY-OTDAY TO  WRK-HENYMDG
                   MOVE    SGDAY-SDAY  TO  WRK-SYMD
               ELSE
                   MOVE    1           TO  FLG-HIZUKECHK
               END-IF
           END-IF
      *
           .
       800-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY8)
      *****************************************************************
       900-SYSKANRI-KEY8-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY8-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY8)
      *****************************************************************
       900-SYSKANRI-KEY8-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY8-FET-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスタ検索処理
      *****************************************************************
       900-HKNNUM-KEY8-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNNUM
      *
      *    保険番号検索
           MOVE    ZERO                TO  FLG-HKNNUM
           INITIALIZE                      HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    SEL-HKNNUM          TO  HKN-HKNNUM
           MOVE    ZERO                TO  HKN-PAYKBN
           IF    ( SEL-EDYMD           >   SEL-STYMD )
               MOVE    SEL-EDYMD       TO  HKN-TEKSTYMD
               MOVE    SEL-STYMD       TO  HKN-TEKEDYMD
           ELSE
               MOVE    SEL-STYMD       TO  HKN-TEKSTYMD
               MOVE    SEL-EDYMD       TO  HKN-TEKEDYMD
           END-IF
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             NOT =   ZERO )
               MOVE    1               TO  FLG-HKNNUM
           ELSE
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
      *
               IF    ( HKN-HON-NYUSKJRYYKBN    =   SPACE )
                   MOVE    "0"         TO  HKN-HON-NYUSKJRYYKBN
               END-IF
               IF    ( HKN-TSY-NYUSKJRYYKBN    =   SPACE )
                   MOVE    "0"         TO  HKN-TSY-NYUSKJRYYKBN
               END-IF
               IF    ( HKN-TNK-NYUSKJRYYKBN    =   SPACE )
                   MOVE    "0"         TO  HKN-TNK-NYUSKJRYYKBN
               END-IF
      *
           END-IF
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNNUM-KEY8-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスタ検索処理
      *****************************************************************
       900-HKNNUM-KEY9-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNNUM
      *
      *    保険番号検索
           MOVE    ZERO                TO  FLG-HKNNUM
           INITIALIZE                      HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           IF    ( SEL-EDYMD           >   SEL-STYMD )
               MOVE    SEL-EDYMD       TO  HKN-TEKSTYMD
               MOVE    SEL-STYMD       TO  HKN-TEKEDYMD
           ELSE
               MOVE    SEL-STYMD       TO  HKN-TEKSTYMD
               MOVE    SEL-EDYMD       TO  HKN-TEKEDYMD
           END-IF
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             NOT =   ZERO )
               MOVE    1               TO  FLG-HKNNUM
           ELSE
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
           END-IF
      *
           .
       900-HKNNUM-KEY9-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスタFETCH処理
      *****************************************************************
       900-HKNNUM-KEY9-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNNUM
      *
      *    保険番号検索
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF ( MCP-RC             NOT =   ZERO )
               MOVE    1               TO  FLG-HKNNUM
           ELSE
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
           END-IF
      *
           .
       900-HKNNUM-KEY9-FET-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC          SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING       MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF        ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       910-DBFETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING       MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING       MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING       MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                  SECTION.
      *
           MOVE   LOW-VALUE         TO  MCP-TABLE
                                        MCP-PATHNAME
           MOVE   "PUTWINDOW"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
