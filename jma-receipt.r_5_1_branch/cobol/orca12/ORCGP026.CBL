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
       PROGRAM-ID.             ORCGP026.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 特記事項情報（Ｐ０２６）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/07/22    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.04.00    NACL-多々納  09/04/09  Ｈ２１．５対応
      *  04.05.00    NACL-多々納  09/07/28  Ｈ２１年１０月対応
      *  04.07.00    NACL-多々納  12/12/25  レセプト分割対応
      *  04.07.00    NACL-多々納  14/12/XX  Ｈ２７年０１月対応
      *  04.08.00    NACL-多々納  15/11/XX  レセプト分割５０件他対応
      *  04.08.00    NACL-多々納  18/04/XX  Ｈ３０年０４月対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *    患者画面スパ領域
           COPY    "P02SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PARA            PIC 9(01).
           03  FLG-END             PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-HKNOK           PIC 9(01).
           03  FLG-HKNOK-ALL       PIC 9(01).
           03  FLG-HYOJI           PIC 9(01).
           03  FLG-HKNCOMBI-ERR    PIC 9(01).
           03  FLG-HKNOK-ERR       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-D               PIC 9(04).
           03  IDX-N               PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-H               PIC 9(04).
           03  IDX-H1              PIC 9(04).
           03  IDX-H2              PIC 9(04).
           03  IDX-H3              PIC 9(04).
           03  IDX-L               PIC 9(04).
           03  IDX-L1              PIC 9(04).
           03  IDX-L2              PIC 9(04).
           03  IDR                 PIC 9(04).
           03  IDX-C               PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDX-HW              PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYM.
                   07  WRK-SYY         PIC 9(04).
                   07  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *
           03  WRK-NYUGAI-G.
               05  WRK-NYUGAIKBN       PIC X(01).
               05  WRK-NYUGAIF         PIC X(01).
               05  WRK-NYUGAIMEI       PIC X(18).
      *
           03  WRK-RENNUMZ         PIC ZZZZ.
           03  WRK-SELNUM          PIC 9(04).
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-PARA-EDANUM         PIC 9(03).
      *
           03  WRK-HDI-MAX         PIC 9(04).
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC X(06).
               05  WRK-SRYDD           PIC X(02).
           03  WRK-SRYYMD-1.
               05  WRK-SRYYM-1         PIC X(06).
               05  WRK-SRYDD-1         PIC X(02).
      *    保険期間チェック
           03  WRK-HKNINF-CHK.
               05  WRK-SRT-HKNNUM          PIC X(03).
               05  WRK-END-HKNNUM          PIC X(03).
               05  WRK-CHKSRT-HKNNUM       PIC X(03).
               05  WRK-CHKEND-HKNNUM       PIC X(03).
      *
               05  WRK-SRT-HKNJANUM        PIC X(08).
               05  WRK-END-HKNJANUM        PIC X(08).
               05  WRK-CHKSRT-HKNJANUM     PIC X(08).
               05  WRK-CHKEND-HKNJANUM     PIC X(08).
      *
               05  WRK-SRT-HKNID           PIC 9(10).
               05  WRK-END-HKNID           PIC 9(10).
      *
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-HKNCOMBI-MEI        PIC X(100).
           03  WRK-HKNCOMBI-MAX        PIC 9(04).
      *
           03  WRK-CHK-HKNCOMBI        PIC X(04).
           03  WRK-HKNDELFLG           PIC X(01).
      *
           03  WRK-TUKICNT             PIC 9(03).
      *
      *    対象保険組合せチェック
           03  WRK-HDHKNLST-TBL.
               05  WRK-HDHKNLST-OCC         OCCURS  2.
                   07  WRK-HDHKNLST-OCC2       OCCURS  20.
                       11  WRK-THDHKNCOMBI     PIC X(04).
                       11  WRK-THDHKNDELFLG    PIC X(01).
                       11  WRK-THDHKNNAME      PIC X(60).
      *
      *
      *    レセプト分割対象保険組合せ
       01  TBL-HDHKNLST-AREA.
           03  TBL-HDHKNLST-OCC        OCCURS  2.
               05  TBL-HDHKNLST-OCC2       OCCURS   50.
                   07  TBL-HD-HKNCOMBI     PIC X(04).
                   07  TBL-HD-HKNCOMBI-MEI PIC X(60).
                   07  TBL-HD-HYOJIKBN     PIC X(01).
      *
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *    特記事項一覧
       01  TBL-TOKKI-AREA.
           03  FILLER              PIC X(15)   VALUE   "01 ０１公".
           03  FILLER              PIC X(15)   VALUE   "02 ０２長".
           03  FILLER              PIC X(15)   VALUE   "03 ０３長処".
           03  FILLER              PIC X(15)   VALUE   "04 ０４後保".
           03  FILLER              PIC X(15)   VALUE   "07 ０７老併".
           03  FILLER              PIC X(15)   VALUE   "08 ０８老健".
           03  FILLER              PIC X(15)   VALUE   "09 ０９施".
           03  FILLER              PIC X(15)   VALUE   "10 １０第三".
           03  FILLER              PIC X(15)   VALUE   "11 １１薬治".
           03  FILLER              PIC X(15)   VALUE   "12 １２器治".
           03  FILLER              PIC X(15)   VALUE   "13 １３先進".
           03  FILLER              PIC X(15)   VALUE   "14 １４制超".
           03  FILLER              PIC X(15)   VALUE   "15 １５経過".
           03  FILLER              PIC X(15)   VALUE   "16 １６長２".
           03  FILLER              PIC X(15)   VALUE   "17 １７上位".
           03  FILLER              PIC X(15)   VALUE   "18 １８一般".
           03  FILLER              PIC X(15)   VALUE   "19 １９低所".
           03  FILLER              PIC X(15)   VALUE   "20 ２０二割".
           03  FILLER              PIC X(15)   VALUE   "21 ２１高半".
           03  FILLER              PIC X(15)   VALUE   "22 ２２多上".
           03  FILLER              PIC X(15)   VALUE   "23 ２３多一".
           03  FILLER              PIC X(15)   VALUE   "24 ２４多低".
           03  FILLER              PIC X(15)   VALUE   "25 ２５出産".
      *H27.1 から
           03  FILLER              PIC X(15)   VALUE   "26 ２６区ア".
           03  FILLER              PIC X(15)   VALUE   "27 ２７区イ".
           03  FILLER              PIC X(15)   VALUE   "28 ２８区ウ".
           03  FILLER              PIC X(15)   VALUE   "29 ２９区エ".
           03  FILLER              PIC X(15)   VALUE   "30 ３０区オ".
           03  FILLER              PIC X(15)   VALUE   "31 ３１多ア".
           03  FILLER              PIC X(15)   VALUE   "32 ３２多イ".
           03  FILLER              PIC X(15)   VALUE   "33 ３３多ウ".
           03  FILLER              PIC X(15)   VALUE   "34 ３４多エ".
           03  FILLER              PIC X(15)   VALUE   "35 ３５多オ".
           03  FILLER              PIC X(15)   VALUE   "36 ３６加治".
           03  FILLER              PIC X(15)   VALUE   "37 ３７申出".
      *H30.4 から
           03  FILLER              PIC X(15)   VALUE   "38 ３８医併".
           03  FILLER              PIC X(15)   VALUE   "39 ３９医療".
       01  TBL-TOKKI-AREA-R        REDEFINES   TBL-TOKKI-AREA.
      **** 03  TBL-TOKKI-OCC       OCCURS   23.
           03  TBL-TOKKI-OCC       OCCURS   37.
               05  TBL-TOKK-CD     PIC X(02).
               05  TBL-TOKK-K      PIC X(01).
               05  TBL-TOKK-NAIYO  PIC X(12).
       01  TBL-TOKK-MAX            PIC 9(02)   VALUE   37.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
       01  HZN-PARA-REC.
           COPY    "CPPARA.INC"
                              REPLACING  //PARA-// BY //HZN-PARA-//.
      *
      *   患者特記事項
       01  LNK-PTTOKKI-AREA.
           02  LNK-PTTOKKI-REC           OCCURS  50.
           COPY    "CPPTTOKKI.INC"
                            REPLACING  //PTTOKKI-// BY //LNK-PTTOKKI-//.
      *
      *    患者レセプト分割テーブル
       01  PT-HDIST-DAY-REC.
           COPY    "CPPT-HDIST-DAY.INC".
      *
      *    患者レセプト分割テーブル
       01  LNK-HDIST-DAY-AREA.
           02  LNK-HDIST-DAY-REC           OCCURS  50.
           COPY    "CPPT-HDIST-DAY.INC"
                            REPLACING  //PT-// BY //LNK-//.
      *
      *    患者保険情報  *
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    保険組合せ
       01  HKNCOMB-REC.
           COPY  "CPHKNCOMBI.INC".
      *    保険組合せテーブル
       01  LNK-HKNCOMB-AREA.
           02  LNK-HKNCOMB-REC         OCCURS   200.
           COPY  "CPHKNCOMBI.INC"
                         REPLACING  //COMB-// BY //LNK-COMB-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
      *
       01  SCRAREA.
           COPY    "ORCA12SCRAREA.INC".
      *    COPY    "P02.INC".
      *    COPY    "P02A.INC".
      *    COPY    "P02D.INC".
      *    COPY    "P02E.INC".
      *    COPY    "P02F.INC".
      *    COPY    "P014.INC".
      *    COPY    "P015.INC".
      *    COPY    "P98.INC".
      *    COPY    "P981.INC".
      *    COPY    "P0201.INC".
      *    COPY    "P982.INC".
      *    COPY    "P100.INC".
      *    COPY    "P02X.INC".
      *    COPY    "P02S.INC".
      *    COPY    "P02Q.INC".
      *    COPY    "P02Q1.INC".
      *    COPY    "P02Y.INC".
      *    COPY    "P02Y1.INC".
      *    COPY    "P02G.INC".
      *    COPY    "P02G2.INC".
      *    COPY    "P02K.INC".
      *    COPY    "P02H.INC".
      *    COPY    "P02T.INC".
      *    COPY    "PERR.INC".
      *    COPY    "PID1.INC".
      *    COPY    "PDMY01.INC".
      *    COPY    "CPAPIMOD.INC".
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
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  SPA-FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"         ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
           IF      SPA-FLG-END         =   ZERO
               PERFORM 5001-MAPCUR-SEC
      *********PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA  TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
      *    初期画面編集
           PERFORM 300-SCREEN-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    パラメタファイル検索処理
      **** PERFORM 320-PARAFILE-KENSAKU-SEC
      *
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
      *    初期画面編集
           PERFORM 310-SPASET-SEC
      *
           INITIALIZE                  SPA-GMN26-INAREA
           MOVE    1                   TO  SPA-GMN26-CUR
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
      *    初期編集内容
           INITIALIZE                  SPA-GMN26-INIT
           MOVE    "0 入院・入院外"    TO  SPA-GMN26-NYUGAI-LST (1)
           MOVE    "1 入院"            TO  SPA-GMN26-NYUGAI-LST (2)
           MOVE    "2 入院外"          TO  SPA-GMN26-NYUGAI-LST (3)
           MOVE    3                   TO  SPA-GMN26-NYUMAX
      *    特記事項
           MOVE    ZERO                TO  SPA-GMN26-TOKMAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   TBL-TOKK-MAX
               MOVE    TBL-TOKKI-OCC (IDX) TO  SPA-GMN26-TOKCD-LST (IDX)
               MOVE    IDX                 TO  SPA-GMN26-TOKMAX
           END-PERFORM
      *
      *    一覧表変換編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN26-TMAX
               MOVE    SPA-NAI26-TNYUGIKBN (IDX)   TO  WRK-NYUGAIKBN
               PERFORM 3101-NYUGAIKBN-HEN-SEC
               MOVE    WRK-NYUGAIMEI           TO  SPA-GMN26-TNYUGI(IDX)
      *
               MOVE    SPA-NAI26-TSTYM (IDX)   TO  WRK-SYM
               PERFORM 800-SE-WAFORMAT-YMDEDIT-SEC
               MOVE    WRK-HENYMD(1:7)         TO  SPA-GMN26-TSTYM (IDX)
      *
               MOVE    SPA-NAI26-TEDYM (IDX)   TO  WRK-SYM
               PERFORM 800-SE-WAFORMAT-YMDEDIT-SEC
               MOVE    WRK-HENYMD(1:7)         TO  SPA-GMN26-TEDYM (IDX)
           END-PERFORM
      *
      *    前回の選択番号は反映させない
           INITIALIZE                  SPA-GMN26-INAREA
      *
      *    レセプト分割
           PERFORM 3101-PTHDISTDAY-SET-SEC
      *    前回の選択番号は反映させない
           INITIALIZE                  SPA-GMN26-HDIST-INAREA
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト分割設定処理
      *****************************************************************
       3101-PTHDISTDAY-SET-SEC              SECTION.
      *
      *    パラメタファイル検索処理
           PERFORM 320-PARAFILE-KENSAKU-SEC
      *
      *    初期編集内容
           INITIALIZE                  SPA-GMN26-HDIST-INIT
           MOVE    "0 入院・入院外"    TO  SPA-GMN26-HDNYUGAI-LST (1)
           MOVE    1                   TO  SPA-GMN26-HDNYUMAX
      *
           MOVE    "1 社保"            TO  SPA-GMN26-HDRESEKBN-LST (1)
           MOVE    "2 国保"            TO  SPA-GMN26-HDRESEKBN-LST (2)
           MOVE    "3 後期"            TO  SPA-GMN26-HDRESEKBN-LST (3)
           MOVE    3                   TO  SPA-GMN26-HDRESEMAX
      *
           INITIALIZE                  SPA-GMN26-HDLIST-TBL
           INITIALIZE                  SPA-GMN26-HDHKNLST-TBL
      *
           MOVE    1                   TO  SPA-NAI26-INIT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   50 )
                   OR        ( IDX     >   WRK-HDI-MAX )
               MOVE    LNK-HDIST-DAY-REC (IDX) TO  PT-HDIST-DAY-REC
      *        ＳＰＡテーブル編集
               MOVE    IDX                 TO  SPA-GMN26-THDNUM
                                                                 (IDX)
               MOVE    PT-HDIST-DAY-SRYYM  TO  SPA-NAI26-THDSRYYM
                                                                 (IDX)
               MOVE    PT-HDIST-DAY-SRYYM  TO  WRK-SYM
               PERFORM 800-SE-WAFORMAT-YMDEDIT-SEC
               MOVE    WRK-HENYMD(1:7)     TO  SPA-GMN26-THDSRYYM (IDX)
      *
               MOVE    PT-HDIST-DAY-DAY    TO  SPA-GMN26-THDDAY
                                                                 (IDX)
      *        入外区分
               MOVE    PT-HDIST-DAY-NYUGAIKBN
                                           TO  SPA-NAI26-THDNYUGAI
                                                                 (IDX)
               PERFORM VARYING     IDR     FROM    1   BY  1
                       UNTIL       IDR     >   SPA-GMN26-HDNYUMAX
                   IF      SPA-NAI26-THDNYUGAI(IDX)
                                        =   SPA-GMN26-HDNYUGAI-LST(IDR)
                                                              (1:1)
                       MOVE    SPA-GMN26-HDNYUGAI-LST(IDR)(3:)
                                        TO  SPA-GMN26-THDNYUGAI(IDX)
                   END-IF
               END-PERFORM
      *        レセ区分
               MOVE    PT-HDIST-DAY-RECEIPT-KBN
                                           TO  SPA-NAI26-THDRESEKBN
                                                                 (IDX)
               PERFORM VARYING     IDR     FROM    1   BY  1
                       UNTIL       IDR     >   SPA-GMN26-HDRESEMAX
                   IF      SPA-NAI26-THDRESEKBN(IDX)
                                        =   SPA-GMN26-HDRESEKBN-LST(IDR)
                                                              (1:1)
                       MOVE    SPA-GMN26-HDRESEKBN-LST(IDR)(3:)
                                        TO  SPA-GMN26-THDRESEKBN(IDX)
                   END-IF
               END-PERFORM
      *
               PERFORM VARYING    IDX-H1   FROM    1   BY  1
                       UNTIL     (IDX-H1   >   2  )
                   PERFORM VARYING    IDX-H2   FROM    1   BY  1
                           UNTIL     (IDX-H2   >   20 )
                       IF      PT-HDIST-DAY-HKNCOMBI (IDX-H1 IDX-H2)
                                               NOT =   ZERO
                           MOVE    PT-HDIST-DAY-HKNCOMBI (IDX-H1 IDX-H2)
                                           TO  SPA-NAI26-THKNCOMBI
                                                   (IDX IDX-H1 IDX-H2)
                           MOVE    PT-HDIST-DAY-DLTFLG (IDX-H1 IDX-H2)
                                           TO  SPA-NAI26-THKNDLTFLG
                                                   (IDX IDX-H1 IDX-H2)
                       END-IF
                   END-PERFORM
               END-PERFORM
               MOVE    IDX                 TO  SPA-GMN26-HDLSTMAX
           END-PERFORM
      *
      *    振分日見出し初期表示
           PERFORM 31011-HDMIDASI-INI-SEC
      *
           .
       3101-PTHDISTDAY-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    振分日見出し初期表示
      *****************************************************************
       31011-HDMIDASI-INI-SEC             SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN26-HDMIDASI-G
           .
       31011-HDMIDASI-INI-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタファイル検索処理（患者レセプト分割）
      *****************************************************************
       320-PARAFILE-KENSAKU-SEC             SECTION.
      *
           INITIALIZE                  LNK-HDIST-DAY-AREA
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTHDISTDAY"        TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           MOVE    ZERO                TO  WRK-HDI-MAX
           PERFORM
               UNTIL   FLG-PARA            =   1
               ADD     1                   TO  WRK-HDI-MAX
               IF      WRK-HDI-MAX         <=  50
                   MOVE    PARA-DATA-REC   TO  LNK-HDIST-DAY-REC
                                                         (WRK-HDI-MAX)
               END-IF
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       320-PARAFILE-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分設定処理
      *****************************************************************
       3101-NYUGAIKBN-HEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-NYUGAIMEI
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX-N   FROM    1   BY  1
                   UNTIL      (IDX-N   >   SPA-GMN26-NYUMAX )
                          OR  (FLG-OK  =   1                )
               IF      WRK-NYUGAIKBN   =   SPA-GMN26-NYUGAI-LST (IDX-N)
                                                              (1:1)
                   MOVE    SPA-GMN26-NYUGAI-LST (IDX-N)
                                               TO  WRK-NYUGAI-G
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           .
       3101-NYUGAIKBN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *    カーソル指定のない時、入力した項目の次ぎへ
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN26-CUR       =   ZERO )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN26-CUR
               WHEN    01
                   MOVE    "P026_SELNUM"        TO  MCP-WIDGET
               WHEN    02
                   MOVE    "P026_NYUGAI"        TO  MCP-WIDGET
               WHEN    03
                   MOVE    "P026_CD"            TO  MCP-WIDGET
               WHEN    04
                   MOVE    "P026_NAIYO"         TO  MCP-WIDGET
               WHEN    05
                   MOVE    "P026_STYM"          TO  MCP-WIDGET
               WHEN    06
                   MOVE    "P026_EDYM"          TO  MCP-WIDGET
               WHEN    07
                   MOVE    "P026_TOKCD"         TO  MCP-WIDGET
               WHEN    10
                   MOVE    "BTO2"               TO  MCP-WIDGET
      *
               WHEN    20
                   MOVE    "P026_HDSELNUM"      TO  MCP-WIDGET
               WHEN    21
                   MOVE    "P026_HDSRYYM"       TO  MCP-WIDGET
               WHEN    22
                   MOVE    "P026_HDDAY"         TO  MCP-WIDGET
               WHEN    23
                   MOVE    "P026_HD_NYUGAI"     TO  MCP-WIDGET
               WHEN    24
                   MOVE    "P026_HD_RESEKBN"    TO  MCP-WIDGET
               WHEN    25
                   MOVE    "BHD02"              TO  MCP-WIDGET
               WHEN    26
                   MOVE    "BHD03"              TO  MCP-WIDGET
               WHEN    27
                   MOVE    "BHD01"              TO  MCP-WIDGET
               WHEN    28
                   MOVE    "P026_DELSEL1"       TO  MCP-WIDGET
               WHEN    29
                   MOVE    "P026_DELSEL2"       TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC          SECTION.
      *
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "P026_SELNUM"
                   MOVE    2           TO  SPA-GMN26-CUR
               WHEN    "P026_NYUGAI"
                   MOVE    3           TO  SPA-GMN26-CUR
               WHEN    "P026_CD"
                   MOVE    4           TO  SPA-GMN26-CUR
               WHEN    "P026_NAIYO"
                   MOVE    5           TO  SPA-GMN26-CUR
               WHEN    "P026_STYM"
                   MOVE    6           TO  SPA-GMN26-CUR
               WHEN    "P026_EDYM"
                   MOVE    10          TO  SPA-GMN26-CUR
               WHEN    "P026_TOKCD"
                   MOVE    5           TO  SPA-GMN26-CUR
      *
               WHEN    "P026_HDSELNUM"
                   MOVE    21          TO  SPA-GMN26-CUR
               WHEN    "P026_HDSRYYM"
                   MOVE    22          TO  SPA-GMN26-CUR
               WHEN    "P026_HDDAY"   
                   MOVE    23          TO  SPA-GMN26-CUR
               WHEN    "P026_HD_NYUGAI"
                   MOVE    24          TO  SPA-GMN26-CUR
               WHEN    "P026_HD_RESEKBN"
                   MOVE    26          TO  SPA-GMN26-CUR
      *
           END-EVALUATE
      *
           .
      *
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    登録処理
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    更新
               WHEN    "CLICKED"       ALSO    "BTO2"
                   PERFORM 411-KOUSIN-SEC
      *    削除
               WHEN    "CLICKED"       ALSO    "BTO1"
                   PERFORM 412-DELETE-SEC
      *  レセ分割
      *    削除
               WHEN    "CLICKED"       ALSO    "BHD01"
                   PERFORM 430-HD-DELETE-SEC
      *
      *    確認
               WHEN    "CLICKED"       ALSO    "BHD02"
                   PERFORM 431-HD-KAKUNIN-SEC
      *    更新
               WHEN    "CLICKED"       ALSO    "BHD03"
                   PERFORM 432-HD-KOUSIN-SEC
      *
      *     以外はエラー
               WHEN    "CLICKED"       ALSO     ANY
                   MOVE    "0110"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-GMN26-CUR
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       411-KOUSIN-SEC           SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =    SPACE
               IF      SPA-GMN26-SELNUM    =   ZERO
                   IF      SPA-GMN26-TMAX      >=  50
                       MOVE    "2620"          TO  SPA-ERRCD
                   ELSE
                       ADD     1                   TO  SPA-GMN26-TMAX
                       MOVE    SPA-GMN26-TMAX      TO  IDX
                   END-IF
               ELSE
                   MOVE    SPA-GMN26-SELNUM    TO  IDX
               END-IF
           END-IF
           IF      SPA-ERRCD           =    SPACE
               MOVE    SPA-GMN26-NYUGAI    TO  SPA-NAI26-TNYUGIKBN(IDX)
               MOVE    SPA-GMN26-NYUGAI-MEI
                                           TO  SPA-GMN26-TNYUGI (IDX)
               MOVE    SPA-GMN26-CD        TO  SPA-GMN26-TCD  (IDX)
               MOVE    SPA-GMN26-NAIYO     TO  SPA-GMN26-TNAIYO (IDX)
               MOVE    SPA-GMN26-STYM      TO  SPA-GMN26-TSTYM (IDX)
               MOVE    SPA-GMN26-EDYM      TO  SPA-GMN26-TEDYM (IDX)
               MOVE    SPA-NAI26-STYM      TO  SPA-NAI26-TSTYM (IDX)
               MOVE    SPA-NAI26-EDYM      TO  SPA-NAI26-TEDYM (IDX)
      *
               INITIALIZE                  SPA-GMN26-INAREA
               MOVE    1                   TO  SPA-GMN26-CUR
           END-IF
      *
           .
       411-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       412-DELETE-SEC          SECTION.
      *
           IF       SPA-GMN26-SELNUM   NOT =   ZERO
               MOVE    SPA-GMN26-SELNUM    TO  IDX
               INITIALIZE                  SPA-GMN26-TOKK-OCC (IDX)
      *        空白行削除
               PERFORM 4121-LIST-HEN-SEC
               INITIALIZE                  SPA-GMN26-INAREA
               MOVE    1                   TO  SPA-GMN26-CUR
           END-IF
      *
           .
       412-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    空白行削除処理
      *****************************************************************
       4121-LIST-HEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO   SPA-GMN26-TMAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   50
              IF     (SPA-GMN26-TNYUGI (IDX)   =   SPACE )
                 AND (IDX              <   50   )
                   MOVE    SPA-GMN26-TOKK-OCC (IDX + 1)
                                       TO  SPA-GMN26-TOKK-OCC (IDX)
                   INITIALIZE              SPA-GMN26-TOKK-OCC (IDX + 1)
              END-IF
           END-PERFORM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   50
              IF     (SPA-GMN26-TNYUGI (IDX)   NOT =   SPACE )
                   ADD     1               TO  SPA-GMN26-TMAX
                   MOVE    SPA-GMN26-TMAX  TO  SPA-GMN26-TNUM (IDX)
              END-IF
           END-PERFORM
           .
       4121-LIST-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       490-TOUROKU-SEC             SECTION.
      *
      *    レセプト分割の登録チェック
           IF      SPA-TAGU-FLG        =   ZERO
              IF      SPA-P026-KOHKEIFLG  =   ZERO
                   PERFORM 49009-PTHDISTDAY-CHK-SEC
               END-IF
           END-IF
      *
      *    レセプト分割パラメタ追加
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4901-PTHDISTDAY-PARA-INS-SEC
           END-IF
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト分割の登録チェック処理
      *****************************************************************
       49009-PTHDISTDAY-CHK-SEC          SECTION.
      *
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL  (IDX         >   SPA-GMN26-HDLSTMAX )  OR
                          (SPA-ERRCD   NOT =   SPACE )
               PERFORM 410131-HDLIST-HEN-SEC
               IF      SPA-ERRCD           =   "2631"
                   MOVE    "2639"          TO  SPA-ERRCD
                   MOVE    25              TO  SPA-GMN26-CUR
               END-IF
               IF      SPA-ERRCD           =   "2641"
                   MOVE    27                  TO  SPA-GMN26-CUR
               END-IF
      *
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    IDX             TO  SPA-GMN26-HDSELNUM
               END-IF
           END-PERFORM
      *
           IF     (SPA-ERRCD           =   SPACE )
               INITIALIZE                  SPA-GMN26-HDIST-INAREA
               INITIALIZE                  SPA-GMN26-HDHKNLST-TBL
      *        振分日見出し初期表示
               PERFORM 31011-HDMIDASI-INI-SEC
           END-IF
           .
       49009-PTHDISTDAY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト分割パラメタ追加処理
      *****************************************************************
       4901-PTHDISTDAY-PARA-INS-SEC          SECTION.
      *
           INITIALIZE                  LNK-HDIST-DAY-AREA
      *    テーブル編集
           MOVE    ZERO                TO  WRK-HDI-MAX
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL   IDX         >   SPA-GMN26-HDLSTMAX
               ADD     1                   TO  WRK-HDI-MAX
               INITIALIZE                  PT-HDIST-DAY-REC
               MOVE    SPA-NAI26-THDSRYYM (IDX)
                                           TO  PT-HDIST-DAY-SRYYM
               MOVE    SPA-GMN26-THDDAY   (IDX)
                                           TO  PT-HDIST-DAY-DAY
               MOVE    SPA-NAI26-THDNYUGAI  (IDX)
                                           TO  PT-HDIST-DAY-NYUGAIKBN
               MOVE    SPA-NAI26-THDRESEKBN  (IDX)
                                           TO  PT-HDIST-DAY-RECEIPT-KBN
               PERFORM VARYING    IDX-H1   FROM    1   BY  1
                       UNTIL     (IDX-H1   >   2  )
                   PERFORM VARYING    IDX-H2   FROM    1   BY  1
                           UNTIL     (IDX-H2   >   20 )
                       IF      SPA-NAI26-THKNCOMBI (IDX IDX-H1 IDX-H2)
                                               NOT =   SPACE
                           MOVE    SPA-NAI26-THKNCOMBI
                                                   (IDX IDX-H1 IDX-H2)
                                           TO  PT-HDIST-DAY-HKNCOMBI
                                                        (IDX-H1 IDX-H2)
                           MOVE    SPA-NAI26-THKNDLTFLG
                                                   (IDX IDX-H1 IDX-H2)
                                           TO  PT-HDIST-DAY-DLTFLG
                                                        (IDX-H1 IDX-H2)
                       END-IF
                   END-PERFORM
               END-PERFORM
      *
               MOVE    PT-HDIST-DAY-REC    TO  LNK-HDIST-DAY-REC
                                                     (WRK-HDI-MAX)
           END-PERFORM
      *
      *    削除する
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTHDISTDAY"        TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    ZERO                TO  WRK-PARA-EDANUM
      *
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL   IDX         >   SPA-GMN26-HDLSTMAX
      *
               ADD     1                   TO  WRK-PARA-EDANUM
               MOVE    SPACE               TO  PARA-REC
               INITIALIZE                      PARA-REC
               MOVE    "P02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "PTHDISTDAY"        TO  PARA-FILEMEI
               MOVE    WRK-PARA-EDANUM     TO  PARA-EDANUM
      *
               MOVE    LNK-HDIST-DAY-REC  (IDX)
                                           TO  PARA-DATA-REC
               PERFORM 400-PARA-INSERT-SEC
           END-PERFORM
           .
       4901-PTHDISTDAY-PARA-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       430-HD-DELETE-SEC          SECTION.
      *
           IF       SPA-GMN26-HDSELNUM   NOT =   ZERO
               MOVE    SPA-GMN26-HDSELNUM  TO  IDX
               INITIALIZE                  SPA-GMN26-HDLIST-OCC (IDX)
      *        空白行削除
               PERFORM 4121-LIST02-HEN-SEC
      *
           END-IF
      *
           INITIALIZE                  SPA-GMN26-HDHKNLST-TBL
           INITIALIZE                  SPA-GMN26-HDIST-INAREA
      *    振分日見出し初期表示
           PERFORM 31011-HDMIDASI-INI-SEC
           MOVE    20                  TO  SPA-GMN26-CUR
      *
           .
       430-HD-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    空白行削除処理
      *****************************************************************
       4121-LIST02-HEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  SPA-GMN26-HDLSTMAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >=  50
              IF     (SPA-GMN26-THDSRYYM (IDX)  =   SPACE )
                 AND (IDX              <   50   )
                   MOVE    SPA-GMN26-HDLIST-OCC (IDX + 1)
                                       TO  SPA-GMN26-HDLIST-OCC(IDX)
                   INITIALIZE              SPA-GMN26-HDLIST-OCC(IDX + 1)
              END-IF
           END-PERFORM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   50
              IF     (SPA-GMN26-THDSRYYM (IDX)   NOT =   SPACE )
                   ADD     1               TO  SPA-GMN26-HDLSTMAX
                   MOVE    SPA-GMN26-HDLSTMAX
                                           TO  SPA-GMN26-THDNUM (IDX)
              END-IF
           END-PERFORM
           .
       4121-LIST02-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ分割確認　処理
      *****************************************************************
       431-HD-KAKUNIN-SEC              SECTION.
      *
           IF      (SPA-GMN26-HDSRYYM      NOT =   SPACE )
              AND  (SPA-GMN26-HDDAY        NOT =   SPACE )
              AND  (SPA-GMN26-HDNYUGAI     NOT =   SPACE )
              AND  (SPA-GMN26-HDRESEKBN    NOT =   SPACE )
      *        対象保険組合せチェック・編集
               PERFORM 410131-HDHKNCOMBI-CHK-SEC
               MOVE    26                  TO  SPA-GMN26-CUR
           END-IF
           .
       431-HD-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ分割更新　処理
      *****************************************************************
       432-HD-KOUSIN-SEC              SECTION.
      *
           MOVE    2                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN26-HDSELNUM  =   ZERO
                   IF      SPA-GMN26-HDLSTMAX  >=  50
                       MOVE    "2643"          TO  SPA-ERRCD
                   ELSE
                       ADD     1               TO  SPA-GMN26-HDLSTMAX
                       MOVE    SPA-GMN26-HDLSTMAX  TO  IDX
                   END-IF
               ELSE
                   MOVE    SPA-GMN26-HDSELNUM      TO  IDX
               END-IF
           END-IF
           IF      SPA-ERRCD           =    SPACE
      *        画面テーブル編集
               MOVE    IDX                 TO  SPA-GMN26-THDNUM  (IDX) 
               MOVE    SPA-GMN26-HDSRYYM   TO  SPA-GMN26-THDSRYYM(IDX) 
               MOVE    SPA-NAI26-HDSRYYM   TO  SPA-NAI26-THDSRYYM(IDX) 
               MOVE    SPA-GMN26-HDDAY     TO  SPA-GMN26-THDDAY  (IDX) 
               MOVE    SPA-GMN26-HDNYUGAI  TO  SPA-NAI26-THDNYUGAI
                                                                 (IDX)
               MOVE    SPA-GMN26-HDNYUGAI-MEI
                                           TO  SPA-GMN26-THDNYUGAI
                                                                (IDX)
               MOVE    SPA-GMN26-HDRESEKBN TO  SPA-NAI26-THDRESEKBN
                                                                 (IDX)
               MOVE    SPA-GMN26-HDRESEKBN-MEI
                                           TO  SPA-GMN26-THDRESEKBN
                                                                (IDX)
               PERFORM VARYING    IDX-H    FROM    1   BY  1
                       UNTIL     (IDX-H    >   2  )
                   PERFORM VARYING    IDX-H2   FROM    1   BY  1
                           UNTIL     (IDX-H2   >   20 )
                       MOVE    SPA-GMN26-THDHKNCOMBI (IDX-H  IDX-H2)
                               TO  SPA-NAI26-THKNCOMBI
                                               (IDX IDX-H  IDX-H2)
                       MOVE    SPA-GMN26-THDHKNDELFLG (IDX-H  IDX-H2)
                               TO  SPA-NAI26-THKNDLTFLG
                                               (IDX IDX-H  IDX-H2)
                   END-PERFORM
               END-PERFORM
      *
               INITIALIZE                  SPA-GMN26-HDIST-INAREA
               INITIALIZE                  SPA-GMN26-HDHKNLST-TBL
      *        振分日見出し初期表示
               PERFORM 31011-HDMIDASI-INI-SEC
               MOVE    20                  TO  SPA-GMN26-CUR
           END-IF
      *
           .
       432-HD-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力個所のセット
           PERFORM 400-GMN-INPUT-SEC
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    リスト選択
               WHEN    ANY             ALSO    "P026_TOKLST"
                   PERFORM 41012-TOKLST-SYORI-SEC
      *    番号選択
               WHEN    "ACTIVATE"      ALSO    "P026_SELNUM"
                   PERFORM 41013-SELNUM-SEC
      *    リスト選択
               WHEN    ANY             ALSO    "P026_HDLST"
                   PERFORM 41014-HDLST-SYORI-SEC
      *    番号選択
               WHEN    "ACTIVATE"      ALSO    "P026_HDSELNUM"
                   PERFORM 41015-HDSELNUM-SEC
      *    振日前リスト選択
               WHEN    ANY             ALSO    "P026_HDHKNLST1"
                   PERFORM 41016-HDHKNLST1-SYORI-SEC
      *        番号選択
               WHEN    "ACTIVATE"      ALSO    "P026_DELSEL1"
                   PERFORM 41017-DELSEL1-SEC
      *    振日後リスト選択
               WHEN    ANY             ALSO    "P026_HDHKNLST2"
                   PERFORM 41016-HDHKNLST2-SYORI-SEC
      *        番号選択
               WHEN    "ACTIVATE"      ALSO    "P026_DELSEL2"
                   PERFORM 41017-DELSEL2-SEC
               WHEN    OTHER
                   MOVE    ZERO                TO  FLG-TOUROKU
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN26-CUR
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    特記事項一覧選択
      *****************************************************************
       41012-TOKLST-SYORI-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN26-TMAX
               IF      P02-P026-TOK-SELECT (IDX)  =   "T"
                   IF      IDX             =   SPA-GMN26-SELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX         TO  WRK-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-GMN26-SELNUM
           END-IF
      *
           IF      SPA-GMN26-SELNUM    >   ZERO
               MOVE    SPA-GMN26-SELNUM    TO   P02-P026-SELNUM
               PERFORM 41013-SELNUM-SEC
           END-IF
      *
           .
       41012-TOKLST-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       41013-SELNUM-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN26-INAREA
      *
           MOVE    P02-P026-SELNUM     TO  SPA-GMN26-SELNUM
      *
           IF      SPA-GMN26-SELNUM    >   SPA-GMN26-TMAX
               MOVE    "2601"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN26-CUR
           ELSE
               IF      SPA-GMN26-SELNUM        =   ZERO
                   MOVE    SPA-GMN26-NYUGAI-LST (1)
                                               TO  SPA-GMN26-NYUGAI-G
               ELSE
                   PERFORM 410131-TOKKI-HEN-SEC
               END-IF
           END-IF
      *
      *
           .
       41013-SELNUM-EXT.
           EXIT.
      *****************************************************************
      *    特記内容編集処理
      *****************************************************************
       410131-TOKKI-HEN-SEC           SECTION.
      *
           MOVE    SPA-GMN26-SELNUM    TO  IDX
      *
           MOVE    SPA-NAI26-TNYUGIKBN (IDX)    TO  WRK-NYUGAIKBN
           PERFORM 3101-NYUGAIKBN-HEN-SEC
           MOVE    WRK-NYUGAI-G            TO  SPA-GMN26-NYUGAI-G
      *
           MOVE    SPA-GMN26-TCD   (IDX)   TO  SPA-GMN26-CD
           MOVE    SPA-GMN26-TNAIYO(IDX)   TO  SPA-GMN26-NAIYO
           MOVE    SPA-GMN26-TSTYM (IDX)   TO  SPA-GMN26-STYM
           MOVE    SPA-GMN26-TEDYM (IDX)   TO  SPA-GMN26-EDYM
      *
           MOVE    SPA-NAI26-TSTYM (IDX)   TO  SPA-NAI26-STYM
           MOVE    SPA-NAI26-TEDYM (IDX)   TO  SPA-NAI26-EDYM
           MOVE    SPA-GMN26-TCD   (IDX)   TO  SPA-MAE26-CD
           .
       410131-TOKKI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト分割一覧選択
      *****************************************************************
       41014-HDLST-SYORI-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN26-HDLSTMAX
               IF      P02-P026-HDLST-SELECT (IDX)  =   "T"
                   IF      IDX             =   SPA-GMN26-HDSELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX         TO  WRK-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-GMN26-HDSELNUM
           END-IF
      *
           IF      SPA-GMN26-HDSELNUM  >   ZERO
               MOVE    SPA-GMN26-HDSELNUM  TO   P02-P026-HDSELNUM
               PERFORM 41015-HDSELNUM-SEC
           END-IF
      *
           .
       41014-HDLST-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       41015-HDSELNUM-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN26-HDIST-INAREA
           INITIALIZE                  SPA-GMN26-HDHKNLST-TBL
      *
           MOVE    P02-P026-HDSELNUM   TO  SPA-GMN26-HDSELNUM
      *
           IF      SPA-GMN26-HDSELNUM  >   SPA-GMN26-HDLSTMAX
               MOVE    "2630"          TO  SPA-ERRCD
               MOVE    20              TO  SPA-GMN26-CUR
           ELSE
               IF      SPA-GMN26-HDSELNUM  NOT =   ZERO
                   MOVE    SPA-GMN26-HDSELNUM  TO  IDX
                   PERFORM 410131-HDLIST-HEN-SEC
               END-IF
               MOVE    21              TO  SPA-GMN26-CUR
           END-IF
      *
           .
       41015-HDSELNUM-EXT.
           EXIT.
      *****************************************************************
      *    レセプト分割テーブル編集処理
      *****************************************************************
       410131-HDLIST-HEN-SEC           SECTION.
      *
           MOVE    SPA-GMN26-THDSRYYM(IDX) TO  SPA-GMN26-HDSRYYM
           MOVE    SPA-NAI26-THDSRYYM(IDX) TO  SPA-NAI26-HDSRYYM
           MOVE    SPA-GMN26-THDDAY  (IDX) TO  SPA-GMN26-HDDAY
      *
           MOVE    SPA-NAI26-THDNYUGAI (IDX)
                                           TO  SPA-GMN26-HDNYUGAI
           MOVE    SPA-GMN26-THDNYUGAI (IDX)
                                           TO  SPA-GMN26-HDNYUGAI-MEI
      *
           MOVE    SPA-NAI26-THDRESEKBN (IDX)
                                           TO  SPA-GMN26-HDRESEKBN
           MOVE    SPA-GMN26-THDRESEKBN (IDX)
                                           TO  SPA-GMN26-HDRESEKBN-MEI
      *    パラメタファイルより編集
           PERFORM 41013-LNKHKNCOMBI-HEN-SEC
      *
           MOVE    SPA-NAI26-HDSRYYM   TO  WRK-SRYYM
           MOVE    SPA-GMN26-HDDAY     TO  WRK-SRYDD
           PERFORM VARYING    IDX-H    FROM    1   BY  1
                   UNTIL     (IDX-H    >   2  )
               PERFORM VARYING    IDX-H2   FROM    1   BY  1
                       UNTIL     (IDX-H2   >   20 )
                             OR  (SPA-NAI26-THKNCOMBI(IDX IDX-H  IDX-H2)
                                           =   SPACE
                                           OR  ZERO  )
                   MOVE    SPA-NAI26-THKNCOMBI (IDX IDX-H  IDX-H2)
                                           TO  WRK-HKNCOMBI
                   MOVE    SPA-NAI26-THKNDLTFLG (IDX IDX-H  IDX-H2)
                                           TO  WRK-HKNDELFLG
                   PERFORM 410131-HDLIST-HKNCOMBI-SEC
               END-PERFORM
           END-PERFORM
      *    保険期間チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 410131-PTHKNINF-CHK-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *        対象保険組合せチェック
               PERFORM 41013-HD-HKNCOMBI-CHK-SEC
      *        一致すること
               PERFORM VARYING    IDX-H    FROM    1   BY  1
                       UNTIL     (IDX-H    >   2   )
                              OR (SPA-ERRCD    NOT =   SPACE )
                   MOVE    ZERO                TO  IDX-L
                   PERFORM VARYING    IDX-H2   FROM    1   BY  1
                           UNTIL     (IDX-H2   >   20   )
                                  OR (SPA-ERRCD    NOT =   SPACE )
                       IF      SPA-NAI26-THKNCOMBI (IDX IDX-H IDX-H2)
                                                   =   SPACE
                           MOVE    20              TO  IDX-H2
                       ELSE
                           ADD     1               TO  IDX-L
                           MOVE    SPA-NAI26-THKNCOMBI(IDX IDX-H IDX-H2)
                                                   TO  WRK-CHK-HKNCOMBI
                           PERFORM 410131-HKNCOMBI-CHK-SEC
                       END-IF
                   END-PERFORM
      *            保険組合せ数一致判定
                   IF      IDX-H               =   1
                       IF      IDX-L1          NOT =   IDX-L
                           MOVE    "2631"              TO  SPA-ERRCD
                           MOVE    25                  TO  SPA-GMN26-CUR
                       END-IF
                   ELSE
                       IF      IDX-L2          NOT =   IDX-L
                           MOVE    "2631"              TO  SPA-ERRCD
                           MOVE    25                  TO  SPA-GMN26-CUR
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .
       410131-HDLIST-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ一致チェック処理
      *****************************************************************
       410131-HKNCOMBI-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING    IDX-H3   FROM    1   BY  1
                   UNTIL     (IDX-H3   >   50   )
                          OR (FLG-OK   NOT =   ZERO )
               IF      TBL-HD-HKNCOMBI (IDX-H IDX-H3)
                                               =   WRK-CHK-HKNCOMBI
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "2631"              TO  SPA-ERRCD
               MOVE    25                  TO  SPA-GMN26-CUR
           END-IF
           .
       410131-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410131-HDLIST-HKNCOMBI-SEC          SECTION.
      *
           MOVE    SPA-NAI26-HDSRYYM   TO  WRK-SRYYM
           MOVE    SPA-GMN26-HDDAY     TO  WRK-SRYDD
      *
           PERFORM VARYING    IDX-C    FROM    1   BY  1
                   UNTIL      IDX-C    >   IDY
               MOVE    LNK-HKNCOMB-REC(IDX-C)  TO  HKNCOMB-REC
               IF      WRK-HKNCOMBI        =   COMB-HKNCOMBINUM
      *            保険組合せ名称編集
                   PERFORM 41013-HD-HKNCOMMEI-SEC
                   MOVE    TBL-HD-HKNCOMBI (IDX-H IDX-H2)
                                           TO  SPA-GMN26-THDHKNCOMBI
                                                    (IDX-H IDX-H2)
                   MOVE    TBL-HD-HKNCOMBI-MEI (IDX-H IDX-H2)
                                           TO  SPA-GMN26-THDHKNNAME
                                                    (IDX-H IDX-H2)
      *            削除区分
                   MOVE    WRK-HKNDELFLG   TO  SPA-GMN26-THDHKNDELFLG
                                                    (IDX-H IDX-H2)
      *
      *            対象チェック
                   IF     (COMB-DELKBN     NOT =   SPACE )
      *                削除
                       MOVE    SPA-GMN26-THDHKNNAME  (IDX-H IDX-H2)
                                               TO  WRK-HKNCOMBI-MEI
                       MOVE    SPACE           TO  SPA-GMN26-THDHKNNAME
                                                     (IDX-H IDX-H2)
                       STRING  "（削）"        DELIMITED  BY  SIZE
                           WRK-HKNCOMBI-MEI    DELIMITED  BY  SIZE
                                       INTO    SPA-GMN26-THDHKNNAME
                                                     (IDX-H IDX-H2)
                       END-STRING
                       MOVE    "2631"              TO  SPA-ERRCD
                       MOVE    25                  TO  SPA-GMN26-CUR
                   END-IF
               END-IF
           END-PERFORM
           .
       410131-HDLIST-HKNCOMBI-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト分割保険組合せ一覧覧選択
      *****************************************************************
       41016-HDHKNLST1-SYORI-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   20   )  OR
                              (SPA-GMN26-THDHKNCOMBI (1 IDX) =  SPACE)
               IF      P02-P026-HDHKNLST1-SELECT (IDX)  =   "T"
                   MOVE    SPA-GMN26-THDHKNCOMBI (1 IDX)
                                               TO  WRK-SELNUM
                   MOVE    20                  TO  IDX
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-GMN26-DELSEL1
           END-IF
      *
           IF      SPA-GMN26-DELSEL1   >   ZERO
               MOVE    SPA-GMN26-DELSEL1   TO   P02-P026-DELSEL1
               PERFORM 41017-DELSEL1-SEC
           END-IF
      *
           .
       41016-HDHKNLST1-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       41017-DELSEL1-SEC           SECTION.
      *
           MOVE    P02-P026-DELSEL1    TO  SPA-GMN26-DELSEL1
      *
           IF      SPA-GMN26-DELSEL1   =   ZERO
               MOVE    29              TO  SPA-GMN26-CUR
               GO      TO      41017-DELSEL1-EXT
           END-IF
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   20  )
                          OR  (FLG-OK      =   1   )
                          OR  (SPA-GMN26-THDHKNCOMBI (1 IDX) =  SPACE)
               IF      SPA-GMN26-THDHKNCOMBI (1 IDX)
                                               =   SPA-GMN26-DELSEL1
                   IF      SPA-GMN26-THDHKNDELFLG (1 IDX)
                                               =   SPACE
                       MOVE    "1"         TO  SPA-GMN26-THDHKNDELFLG
                                                              (1 IDX)
                   ELSE
                       MOVE    SPACE       TO  SPA-GMN26-THDHKNDELFLG
                                                              (1 IDX)
                   END-IF
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "2642"          TO  SPA-ERRCD
               MOVE    28              TO  SPA-GMN26-CUR
           ELSE
               MOVE    29              TO  SPA-GMN26-CUR
               MOVE    ZERO            TO  SPA-GMN26-DELSEL1
           END-IF
      *
           .
       41017-DELSEL1-EXT.
           EXIT.
      *****************************************************************
      *    レセプト分割保険組合せ一覧覧選択
      *****************************************************************
       41016-HDHKNLST2-SYORI-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   20   )  OR
                              (SPA-GMN26-THDHKNCOMBI (2 IDX) =  SPACE)
               IF      P02-P026-HDHKNLST2-SELECT (IDX)  =   "T"
                   MOVE    SPA-GMN26-THDHKNCOMBI (2 IDX)
                                               TO  WRK-SELNUM
                   MOVE    20                  TO  IDX
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-GMN26-DELSEL2
           END-IF
      *
           IF      SPA-GMN26-DELSEL2   >   ZERO
               MOVE    SPA-GMN26-DELSEL2   TO   P02-P026-DELSEL2
               PERFORM 41017-DELSEL2-SEC
           END-IF
      *
           .
       41016-HDHKNLST2-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       41017-DELSEL2-SEC           SECTION.
      *
           MOVE    P02-P026-DELSEL2    TO  SPA-GMN26-DELSEL2
      *
           IF      SPA-GMN26-DELSEL2   =   ZERO
               MOVE    29              TO  SPA-GMN26-CUR
               GO      TO      41017-DELSEL2-EXT
           END-IF
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   20  )
                          OR  (FLG-OK      =   1   )
                          OR  (SPA-GMN26-THDHKNCOMBI (2 IDX) =  SPACE)
               IF      SPA-GMN26-THDHKNCOMBI (2 IDX)
                                               =   SPA-GMN26-DELSEL2
                   IF      SPA-GMN26-THDHKNDELFLG (2 IDX)
                                               =   SPACE
                       MOVE    "1"         TO  SPA-GMN26-THDHKNDELFLG
                                                              (2 IDX)
                   ELSE
                       MOVE    SPACE       TO  SPA-GMN26-THDHKNDELFLG
                                                              (2 IDX)
                   END-IF
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "2642"          TO  SPA-ERRCD
               MOVE    29              TO  SPA-GMN26-CUR
           ELSE
               MOVE    26              TO  SPA-GMN26-CUR
               MOVE    ZERO            TO  SPA-GMN26-DELSEL2
           END-IF
      *
           .
       41017-DELSEL2-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
      *
      *    特記事項登録
           IF      FLG-TOUROKU         =   1
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 4109-TOUROKU-CHK-SEC
               END-IF
           END-IF
      *
      *    レセ分割登録
           IF      FLG-TOUROKU         =   2
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 4110-TOUROKU-CHK02-SEC
               END-IF
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    P02-P026-NYUGAI     TO  SPA-GMN26-NYUGAI-G
           MOVE    P02-P026-CD         TO  SPA-GMN26-CD
           MOVE    P02-P026-NAIYO      TO  SPA-GMN26-NAIYO
           MOVE    P02-P026-STYM       TO  SPA-GMN26-STYM
           MOVE    P02-P026-EDYM       TO  SPA-GMN26-EDYM
      *
           MOVE    P02-P026-TOKCD      TO  SPA-GMN26-TOKCD-G
      *    レセ分割
           MOVE    P02-P026-HDSRYYM    TO  SPA-GMN26-HDSRYYM
           MOVE    P02-P026-HDDAY      TO  SPA-GMN26-HDDAY
           MOVE    P02-P026-HD-NYUGAI  TO  SPA-GMN26-HDNYUGAI-G
           MOVE    P02-P026-HD-RESEKBN TO  SPA-GMN26-HDRESEKBN-G
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェックと別画面処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    入外区分
           PERFORM 41012-NYUGAI-CHK-SEC
      *    レセ電
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-CD-CHK-SEC
           END-IF
      *    紙レセプト
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-NAIYO-CHK-SEC
           END-IF
      *    特記リスト
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN26-TOKCD-G   NOT =   SPACE
                   PERFORM 41012-TOKCD-CHK-SEC
               END-IF
           END-IF
      *    開始年月
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-STYM-CHK-SEC
           END-IF
      *    開始年月
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-EDYM-CHK-SEC
           END-IF
      *
      *レセ分割
      *    対象年月
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41013-HDSRYYM-CHK-SEC
           END-IF
      *    対象日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41013-HDDAY-CHK-SEC
           END-IF
      *    入外区分
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41013-HDNYUGAI-CHK-SEC
           END-IF
      *    レセ区分
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41013-HDRESEKBN-CHK-SEC
           END-IF
      *    対象保険組合せ設定
           IF      SPA-ERRCD           =   SPACE
               IF      (SPA-GMN26-HDSRYYM  NOT =   SPACE )
                  AND  (SPA-GMN26-HDDAY    NOT =   SPACE )
                  AND  (SPA-GMN26-HDNYUGAI NOT =   SPACE )
                  AND  (SPA-GMN26-HDRESEKBN    NOT =   SPACE )
      *            対象保険組合せチェック・編集
                   PERFORM 410131-HDHKNCOMBI-CHK-SEC
               END-IF
           END-IF
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入外区分チェック処理
      *****************************************************************
       41012-NYUGAI-CHK-SEC           SECTION.
      *
           IF      SPA-GMN26-NYUGAI    =   SPACE
               CONTINUE
           ELSE
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX-N   FROM    1   BY  1
                   UNTIL      (IDX-N   >   SPA-GMN26-NYUMAX )
                          OR  (FLG-OK  =   1                )
               IF      SPA-GMN26-NYUGAI    =   SPA-GMN26-NYUGAI-LST
                                                      (IDX-N) (1:1)
                   MOVE    SPA-GMN26-NYUGAI-LST (IDX-N)
                                               TO  SPA-GMN26-NYUGAI-G
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "2602"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN26-CUR
           END-IF
      *
           END-IF
           .
       41012-NYUGAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    レセ電区分チェック処理
      *****************************************************************
       41012-CD-CHK-SEC           SECTION.
      *
           IF      SPA-GMN26-CD        =   SPACE
               CONTINUE
           ELSE
      *        数値チェック
               INITIALIZE                       ORCSNUMAREA
               MOVE    SPA-GMN26-CD         TO  SNUM-INX
               CALL    "ORCSNUM"                USING
                                                ORCSNUMAREA
               IF     (SNUM-RC          NOT =   ZERO  )   OR
                      (SNUM-MINKBN      NOT =   SPACE )   OR
                      (SNUM-SYOKBN      NOT =   SPACE )
                   MOVE    "2603"           TO  SPA-ERRCD
                   MOVE    3                TO  SPA-GMN26-CUR
               ELSE
                   MOVE    SNUM-OUTEDT(14:2)TO  SPA-GMN26-CD
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-GMN26-CD    NOT =   SPA-MAE26-CD )
                  AND (SPA-GMN26-CD    NOT =   SPACE     )
                   PERFORM 410121-TOKCD-HEN-SEC
               END-IF
               MOVE    SPA-GMN26-CD        TO  SPA-MAE26-CD
           END-IF
      *!!!
      *H21.5 特記追加分の入外区分設定
           IF      SPA-GMN26-CD        =   "22"
                                       OR  "23"
                                       OR  "24"
      *        出産は外来可
      ***                              OR  "25"
      *H26.12   多アから多オ
                                       OR  "31"
                                       OR  "32"
                                       OR  "33"
                                       OR  "34"
                                       OR  "35"
      *        入院
               MOVE    SPA-GMN26-NYUGAI-LST (2)
                                           TO  SPA-GMN26-NYUGAI-G
           END-IF
      *!!!
           .
       41012-CD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特記内容編集
      *****************************************************************
       410121-TOKCD-HEN-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL      (IDX-T       >   SPA-GMN26-TOKMAX )
                          OR  (FLG-OK      =   1                )
               IF      SPA-GMN26-CD        =   SPA-GMN26-TOKCDL (IDX-T)
                   MOVE    SPA-GMN26-NAIYOL (IDX-T)
                                               TO  SPA-GMN26-NAIYO
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           .
       410121-TOKCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    紙レセプト内容チェック処理
      *****************************************************************
       41012-NAIYO-CHK-SEC           SECTION.
      *
           IF      SPA-GMN26-NAIYO     =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN26-NAIYO     TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN26-NAIYO
               IF     (KANACHK-RC      NOT =   ZERO  ) 
                   MOVE    "2604"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-GMN26-CUR
               ELSE
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN26-NAIYO
               END-IF
           END-IF
           .
       41012-NAIYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    特記内容編集処理
      *****************************************************************
       41012-TOKCD-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL      (IDX-T       >   SPA-GMN26-TOKMAX )
                          OR  (FLG-OK      =   1                )
               IF      SPA-GMN26-TOKCD     =   SPA-GMN26-TOKCDL (IDX-T)
                   MOVE    SPA-GMN26-TOKCDL (IDX-T)
                                               TO  SPA-GMN26-CD
                   MOVE    SPA-GMN26-NAIYOL (IDX-T)
                                               TO  SPA-GMN26-NAIYO
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "2607"              TO  SPA-ERRCD
               MOVE    7                   TO  SPA-GMN26-CUR
           ELSE
               MOVE    SPACE               TO  SPA-GMN26-TOKCD-G
           END-IF
           .
       41012-TOKCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    開始年日チェック処理
      *****************************************************************
       41012-STYM-CHK-SEC           SECTION.
      *
           IF      SPA-GMN26-STYM      =   SPACE
               MOVE    ZERO                TO  SPA-NAI26-STYM
           ELSE
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN26-STYM      TO  SGDAY-INDAY
               MOVE    "1"                 TO  SGDAY-INTYPE
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN26-STYM
                   MOVE    SGDAY-SDAY (1:6)    TO  SPA-NAI26-STYM
               ELSE
                   MOVE    "2605"              TO  SPA-ERRCD
                   MOVE    05                  TO  SPA-GMN26-CUR
               END-IF
           END-IF
      *!!!
      *H21.5 特記追加分
           IF     (SPA-GMN26-CD        =   "22"
                                       OR  "23"
                                       OR  "24"  )
           AND    (SPA-ERRCD           =   SPACE )
           AND    (SPA-NAI26-STYM  NOT =   ZERO  )
               IF      SPA-NAI26-STYM      <   "200905"
                   MOVE    "2612"              TO  SPA-ERRCD
                   MOVE    05                  TO  SPA-GMN26-CUR
               END-IF
           END-IF
      *!!!
      *!!!
      *H21.10 特記追加分
           IF     (SPA-GMN26-CD        =   "25"  )
           AND    (SPA-ERRCD           =   SPACE )
           AND    (SPA-NAI26-STYM  NOT =   ZERO  )
               IF      SPA-NAI26-STYM      <   "200910"
                   MOVE    "2613"              TO  SPA-ERRCD
                   MOVE    05                  TO  SPA-GMN26-CUR
               END-IF
           END-IF
      *!!!
      *
      *H26.12
      *    区アから多オ　平成２７年１月から
           IF     (SPA-GMN26-CD        >=  "26"  AND
                                       <=  "35"  )
           AND    (SPA-ERRCD           =   SPACE )
           AND    (SPA-NAI26-STYM  NOT =   ZERO  )
               IF      SPA-NAI26-STYM      <   "201501"
                   MOVE    "2614"              TO  SPA-ERRCD
                   MOVE    05                  TO  SPA-GMN26-CUR
               END-IF
           END-IF
      *    加治　平成２６年１１月から
           IF     (SPA-GMN26-CD        =   "36"  )
           AND    (SPA-ERRCD           =   SPACE )
           AND    (SPA-NAI26-STYM  NOT =   ZERO  )
               IF      SPA-NAI26-STYM      <   "201411"
                   MOVE    "2615"              TO  SPA-ERRCD
                   MOVE    05                  TO  SPA-GMN26-CUR
               END-IF
           END-IF
      *
      *    申出　平成２８年４月から
           IF     (SPA-GMN26-CD        =   "37"  )
           AND    (SPA-ERRCD           =   SPACE )
           AND    (SPA-NAI26-STYM  NOT =   ZERO  )
               IF      SPA-NAI26-STYM      <   "201604"
                   MOVE    "2616"              TO  SPA-ERRCD
                   MOVE    05                  TO  SPA-GMN26-CUR
               END-IF
           END-IF
      *
      *H30.4
      *    医併,医療　平成３０年４月から
           IF     (SPA-GMN26-CD        =   "38"
                                       OR  "39"  )
           AND    (SPA-ERRCD           =   SPACE )
           AND    (SPA-NAI26-STYM  NOT =   ZERO  )
               IF      SPA-NAI26-STYM      <   "201804"
                   MOVE    "2617"              TO  SPA-ERRCD
                   MOVE    05                  TO  SPA-GMN26-CUR
               END-IF
           END-IF
      *
           .
       41012-STYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了年日チェック処理
      *****************************************************************
       41012-EDYM-CHK-SEC           SECTION.
      *
           IF      SPA-GMN26-EDYM      =   SPACE
               MOVE    ZERO                TO  SPA-NAI26-EDYM
               IF      SPA-GMN26-STYM      NOT =   SPACE
                   MOVE    "999999"            TO  SPA-GMN26-EDYM
               END-IF
           END-IF
           IF      SPA-GMN26-EDYM      =   "999999"
               MOVE    ALL "9"             TO  SPA-NAI26-EDYM
           END-IF
           IF      SPA-GMN26-EDYM      =   SPACE
                                       OR  "999999"
               CONTINUE
           ELSE
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN26-EDYM      TO  SGDAY-INDAY
               MOVE    "1"                 TO  SGDAY-INTYPE
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN26-EDYM
                   MOVE    SGDAY-SDAY (1:6)    TO  SPA-NAI26-EDYM
               ELSE
                   MOVE    "2606"              TO  SPA-ERRCD
                   MOVE    06                  TO  SPA-GMN26-CUR
               END-IF
           END-IF
      *
           .
       41012-EDYM-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      **    登録前関連チェック処理
      ****************************************************************
       4109-TOUROKU-CHK-SEC             SECTION.
      *
      *    必須チェック
           IF      SPA-GMN26-NAIYO     =   SPACE
               MOVE    "2608"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN26-CUR
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-GMN26-STYM      =   SPACE )
                   OR (SPA-GMN26-EDYM      =   SPACE )
                   MOVE    "2609"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN26-CUR
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-NAI26-STYM      >   SPA-NAI26-EDYM)
                   MOVE    "2610"              TO  SPA-ERRCD
                   MOVE    6                   TO  SPA-GMN26-CUR
               END-IF
           END-IF
      *H26.12
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-GMN26-CD    NOT =   SPACE )
                  AND (SPA-GMN26-NYUGAI    =   SPACE )
      *            入外区分未設定
                   MOVE    SPA-GMN26-NYUGAI-LST (1)
                                           TO  SPA-GMN26-NYUGAI-G
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN26-CD    NOT =   SPACE
      *            レセ電コードの重複チェック
                   PERFORM 41091-JYUFUKU-CHK-SEC
               END-IF
           END-IF
      *
           .
       4109-TOUROKU-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセ電コードの重複チェック処理
      ****************************************************************
       41091-JYUFUKU-CHK-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN26-TMAX )
                           OR (SPA-ERRCD   NOT =   SPACE  )
               IF      SPA-GMN26-CD    =   SPA-GMN26-TCD (IDX)
                   IF     (SPA-GMN26-SELNUM    NOT =   ZERO )
                     AND  (SPA-GMN26-SELNUM        =   IDX  )
                       CONTINUE
                   ELSE
                       IF     (SPA-NAI26-EDYM  <   SPA-NAI26-TSTYM(IDX))
                         OR   (SPA-NAI26-STYM  >   SPA-NAI26-TEDYM(IDX))
                           CONTINUE
                       ELSE
                           MOVE    "2611"          TO  SPA-ERRCD
                           MOVE    6               TO  SPA-GMN26-CUR
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
       41091-SYOTOKU-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      **    レセ分割　登録前関連チェック処理
      ****************************************************************
       4110-TOUROKU-CHK02-SEC             SECTION.
      *
      *    必須チェック
           IF      SPA-GMN26-HDSRYYM   =   SPACE
               MOVE    "2632"              TO  SPA-ERRCD
               MOVE    21                  TO  SPA-GMN26-CUR
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN26-HDDAY     =   SPACE
                   MOVE    "2633"              TO  SPA-ERRCD
                   MOVE    22                  TO  SPA-GMN26-CUR
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-GMN26-HDNYUGAI  =   SPACE )
                   MOVE    "2634"              TO  SPA-ERRCD
                   MOVE    23                  TO  SPA-GMN26-CUR
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-GMN26-HDRESEKBN =   SPACE )
                   MOVE    "2635"              TO  SPA-ERRCD
                   MOVE    24                  TO  SPA-GMN26-CUR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        重複チェック
               PERFORM 411001-JYUFUKU-CHK02-SEC
           END-IF
      *H27.11
           IF      SPA-ERRCD           =   SPACE
      *        月１０件チェック
               PERFORM 411002-TUKICNT-CHK-SEC
           END-IF
           .
       4110-TOUROKU-CHK02-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセ分割テーブルの重複チェック処理
      ****************************************************************
       411001-JYUFUKU-CHK02-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN26-HDLSTMAX )
                           OR (SPA-ERRCD   NOT =   SPACE  )
               IF     (SPA-GMN26-HDSELNUM  NOT =   ZERO )
                 AND  (SPA-GMN26-HDSELNUM      =   IDX  )
                   CONTINUE
               ELSE
                   IF     (SPA-NAI26-HDSRYYM   =   SPA-NAI26-THDSRYYM
                                                               (IDX))
                     AND  (SPA-GMN26-HDDAY     =   SPA-GMN26-THDDAY
                                                               (IDX))
                     AND  (SPA-GMN26-HDNYUGAI  =   SPA-NAI26-THDNYUGAI
                                                               (IDX))
                     AND  (SPA-GMN26-HDRESEKBN =   SPA-NAI26-THDRESEKBN
                                                               (IDX))
                       MOVE    "2636"          TO  SPA-ERRCD
                       MOVE    21              TO  SPA-GMN26-CUR
                   END-IF
               END-IF
           END-PERFORM
           .
       411001-JYUFUKU-CHK02-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセ分割テーブルの重複チェック処理
      ****************************************************************
       411002-TUKICNT-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-TUKICNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN26-HDLSTMAX )
               IF     (SPA-NAI26-HDSRYYM   =   SPA-NAI26-THDSRYYM
                                                             (IDX))
                   IF     (SPA-GMN26-HDSELNUM  NOT =   ZERO )
                     AND  (SPA-GMN26-HDSELNUM      =   IDX  )
                       CONTINUE
                   ELSE
                       ADD     1               TO  WRK-TUKICNT
                   END-IF
               END-IF
           END-PERFORM
           IF      WRK-TUKICNT         >=  10
               MOVE    "2644"          TO  SPA-ERRCD
               MOVE    21              TO  SPA-GMN26-CUR
            END-IF
           .
       411002-TUKICNT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ分割対象年月チェック処理
      *****************************************************************
       41013-HDSRYYM-CHK-SEC           SECTION.
      *
           IF      SPA-GMN26-HDSRYYM   =   SPACE
               MOVE    ZERO                TO  SPA-NAI26-HDSRYYM
           ELSE
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN26-HDSRYYM   TO  SGDAY-INDAY
               MOVE    "1"                 TO  SGDAY-INTYPE
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN26-HDSRYYM
                   MOVE    SGDAY-SDAY (1:6)    TO  SPA-NAI26-HDSRYYM
               ELSE
                   MOVE    "2632"              TO  SPA-ERRCD
                   MOVE    21                  TO  SPA-GMN26-CUR
               END-IF
      *        入外区分の初期設定
               IF     (SPA-ERRCD           =   SPACE )
                 AND  (SPA-GMN26-HDNYUGAI  =   SPACE )
                   MOVE    SPA-GMN26-HDNYUGAI-LST(1)
                                           TO  SPA-GMN26-HDNYUGAI-G
               END-IF
           END-IF
      *
           .
       41013-HDSRYYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    レセ分割対象日チェック処理
      *****************************************************************
       41013-HDDAY-CHK-SEC           SECTION.
      *
           IF      SPA-GMN26-HDDAY     =   SPACE
               CONTINUE
           ELSE
      *        数値チェック
               INITIALIZE                       ORCSNUMAREA
               MOVE    SPA-GMN26-HDDAY     TO  SNUM-INX
               CALL    "ORCSNUM"               USING
                                               ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "2633"              TO  SPA-ERRCD
                   MOVE    22                  TO  SPA-GMN26-CUR
               ELSE
                   MOVE    SNUM-OUTEDT(14:2)   TO  SPA-GMN26-HDDAY
               END-IF
               IF     (SPA-ERRCD           =   SPACE  )
                  AND (SPA-GMN26-HDSRYYM   =   SPACE  )
                   MOVE    "2632"              TO  SPA-ERRCD
                   MOVE    21                  TO  SPA-GMN26-CUR
               END-IF
               IF      SPA-ERRCD       NOT =   SPACE
                   GO     TO     41013-HDDAY-CHK-EXT
               END-IF
      *
               MOVE    SPACE               TO  STS-AREA-DAY
               INITIALIZE                      STS-AREA-DAY
               MOVE    SPACE               TO  LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    SPA-NAI26-HDSRYYM   TO  WRK-SYM
               MOVE    SPA-GMN26-HDDAY     TO  WRK-SDD
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY2-AREA
               IF      STS-DAY-RC1     NOT =   ZERO
                   MOVE    "2633"              TO  SPA-ERRCD
                   MOVE    22                  TO  SPA-GMN26-CUR
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD               =   SPACE  )
             AND  (SPA-NAI26-HDSRYYM   NOT =   SPACE  )
             AND  (SPA-GMN26-HDDAY     NOT =   SPACE  )
             AND  (SPA-GMN26-HDRESEKBN     =   SPACE  )
               MOVE    SPA-NAI26-HDSRYYM   TO  WRK-SRYYM
               MOVE    SPA-GMN26-HDDAY     TO  WRK-SRYDD
      *        保険期間チェック
               PERFORM 410131-PTHKNINF-CHK-SEC
               IF     (SPA-ERRCD      NOT  =   SPACE  )
                AND   (FLG-HKNOK-ERR       =   ZERO   )
                   MOVE    SPACE               TO  SPA-ERRCD
                   MOVE    ZERO                TO  SPA-GMN26-CUR
               END-IF
               IF     (WRK-CHKSRT-HKNNUM   =   WRK-CHKEND-HKNNUM)
      *********  AND  (WRK-CHKSRT-HKNNUM   NOT =   SPACE        )
                 AND  (SPA-ERRCD               =   SPACE        )
                 AND  (SPA-GMN26-HDRESEKBN     =   SPACE        )
                   EVALUATE    WRK-CHKSRT-HKNNUM
                       WHEN    "060"
                       WHEN    "063"
                       WHEN    "067"
                           MOVE    "2"         TO  SPA-GMN26-HDRESEKBN
                       WHEN    "039"
                       WHEN    "040"
                           MOVE    "3"         TO  SPA-GMN26-HDRESEKBN
                       WHEN    OTHER
                           MOVE    "1"         TO  SPA-GMN26-HDRESEKBN
                   END-EVALUATE
               END-IF
           END-IF
           .
       41013-HDDAY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ分割入外区分チェック処理
      *****************************************************************
       41013-HDNYUGAI-CHK-SEC             SECTION.
      *
           IF      SPA-GMN26-HDNYUGAI  =   SPACE
               CONTINUE
           ELSE
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDR     FROM    1   BY  1
                       UNTIL      (IDR     >   SPA-GMN26-HDNYUMAX )
                               OR (FLG-OK  =   1    )
                   IF      SPA-GMN26-HDNYUGAI
                                       =   SPA-GMN26-HDNYUGAI-LST(IDR)
                                                              (1:1)
                       MOVE    SPA-GMN26-HDNYUGAI-LST(IDR)(3:)
                                           TO  SPA-GMN26-HDNYUGAI-MEI
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
                   MOVE    "2634"              TO  SPA-ERRCD
                   MOVE    23                  TO  SPA-GMN26-CUR
               END-IF
           END-IF
          .
       41013-HDNYUGAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ分割レセ区分チェック処理
      *****************************************************************
       41013-HDRESEKBN-CHK-SEC             SECTION.
      *
           IF      SPA-GMN26-HDRESEKBN =   SPACE
               CONTINUE
           ELSE
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDR     FROM    1   BY  1
                       UNTIL      (IDR     >   SPA-GMN26-HDRESEMAX )
                               OR (FLG-OK  =   1    )
                   IF      SPA-GMN26-HDRESEKBN
                                       =   SPA-GMN26-HDRESEKBN-LST(IDR)
                                                              (1:1)
                       MOVE    SPA-GMN26-HDRESEKBN-LST(IDR)(3:)
                                           TO  SPA-GMN26-HDRESEKBN-MEI
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
                   MOVE    "2635"              TO  SPA-ERRCD
                   MOVE    24                  TO  SPA-GMN26-CUR
               END-IF
           END-IF
      *
           .
       41013-HDRESEKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ分割対象保険組合せチェック処理
      *****************************************************************
       410131-HDHKNCOMBI-CHK-SEC           SECTION.
      *
           MOVE    SPA-NAI26-HDSRYYM   TO  WRK-SRYYM
           MOVE    SPA-GMN26-HDDAY     TO  WRK-SRYDD
      *    保険期間チェック
           PERFORM 410131-PTHKNINF-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
      *        対象保険組合せチェック
               PERFORM 41013-HD-HKNCOMBI-CHK-SEC
      *
               MOVE    SPA-GMN26-HDHKNLST-TBL  TO  WRK-HDHKNLST-TBL
      *
               INITIALIZE                  SPA-GMN26-HDHKNLST-TBL
               PERFORM VARYING    IDX-H    FROM    1   BY  1
                       UNTIL      IDX-H    >   2
                   MOVE    ZERO                TO  FLG-HYOJI
      *            対象が２０件以上の時、非表示は削除
                   IF      IDX-H           =   1
                       IF      IDX-L1          >   20
                           MOVE    1               TO  FLG-HYOJI
                       END-IF
                   ELSE
                       IF      IDX-L2          >   20
                           MOVE    1               TO  FLG-HYOJI
                       END-IF
                   END-IF
                   MOVE    ZERO                TO  IDX-H3
                   PERFORM VARYING    IDX-H2   FROM    1   BY  1
                           UNTIL     (IDX-H2   >   50  )
                                  OR (TBL-HD-HKNCOMBI (IDX-H IDX-H2)
                                                   =   SPACE )
                                  OR (IDX-H3       >=  20    )
                       IF     (FLG-HYOJI           =   1  )  AND
                              (TBL-HD-HYOJIKBN (IDX-H IDX-H2)
                                                   =   "1" )
                           CONTINUE
                       ELSE
                           ADD     1               TO  IDX-H3
                           MOVE    TBL-HD-HKNCOMBI (IDX-H IDX-H2)
                                           TO  SPA-GMN26-THDHKNCOMBI
                                                   (IDX-H IDX-H3)
                           MOVE    TBL-HD-HKNCOMBI-MEI (IDX-H IDX-H2)
                                           TO  SPA-GMN26-THDHKNNAME
                                                   (IDX-H IDX-H3)
      *                    削除分反映
                           PERFORM VARYING    IDX-HW   FROM    1   BY  1
                                   UNTIL     (IDX-HW   >   20  )
                                      OR (WRK-THDHKNCOMBI (IDX-H IDX-HW)
                                                   =   SPACE )
                           IF      SPA-GMN26-THDHKNCOMBI(IDX-H IDX-H3)
                                                   =   WRK-THDHKNCOMBI
                                                       (IDX-H IDX-HW)
                               MOVE    WRK-THDHKNDELFLG (IDX-H IDX-HW)
                                           TO  SPA-GMN26-THDHKNDELFLG
                                                       (IDX-H IDX-H3)
                                   MOVE    20      TO  IDX-HW
                               END-IF
                           END-PERFORM
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
      *
           .
       410131-HDHKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ分割対象保険組合せチェック処理
      *****************************************************************
       410131-PTHKNINF-CHK-SEC             SECTION.
      *
           INITIALIZE                      WRK-HKNINF-CHK
           MOVE    SPACE               TO  WRK-SRT-HKNNUM
           MOVE    SPACE               TO  WRK-END-HKNNUM
           MOVE    SPACE               TO  WRK-CHKSRT-HKNNUM
           MOVE    SPACE               TO  WRK-CHKEND-HKNNUM
      *    対象年月日の前日
           MOVE    SPACE            TO  STS-AREA-DAY
           INITIALIZE                   STS-AREA-DAY
           INITIALIZE                   LNK-DAY6-AREA
           MOVE    "61"             TO  LNK-DAY6-IRAI
           MOVE    WRK-SRYYMD       TO  LNK-DAY6-KIJUN
           MOVE    "1"              TO  LNK-DAY6-ZOGENPTN
           MOVE    -1               TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"    USING   STS-AREA-DAY
                                        LNK-DAY6-AREA
           IF      STS-DAY-RC1          =   ZERO
               MOVE    LNK-DAY6-KEISAN  TO  WRK-SRYYMD-1
           END-IF
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTHKNINF"          TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           MOVE    ZERO                TO  FLG-HKNOK-ALL
           MOVE    ZERO                TO  FLG-HKNOK-ERR
           PERFORM
               UNTIL   (FLG-PARA           =   1  )
      *
               MOVE    ZERO                TO  FLG-HKNOK
               MOVE    PARA-DATA-REC       TO  PTHKNINF-REC
               IF     (PTHKN-SAKJOKBN      =   SPACE   )
      *            対象年月に有効
                   EVALUATE    PTHKN-HKNNUM
      *                国保
                       WHEN    "060"
                       WHEN    "063"
                       WHEN    "067"
                           IF      SPA-GMN26-HDRESEKBN     =   "2"
                               MOVE    1           TO  FLG-HKNOK
                           END-IF
      *                高齢者
                       WHEN    "039"
                       WHEN    "040"
                           IF      SPA-GMN26-HDRESEKBN     =   "3"
                               MOVE    1           TO  FLG-HKNOK
                           END-IF
      *                社保
                       WHEN    OTHER
                           IF     (SPA-GMN26-HDRESEKBN     =   "1" )
                           AND    (PTHKN-HKNNUM(1:1)   NOT =   "9" )
                               MOVE    1               TO  FLG-HKNOK
                           END-IF
                   END-EVALUATE
      *
                   IF      FLG-HKNOK           =   1
                       IF      PTHKN-TEKSTYMD      =   WRK-SRYYMD
                           MOVE    PTHKN-HKNNUM    TO  WRK-SRT-HKNNUM
                           MOVE    PTHKN-HKNJANUM  TO  WRK-SRT-HKNJANUM
                           MOVE    PTHKN-HKNID     TO  WRK-SRT-HKNID
                       END-IF
                       IF      PTHKN-TEKEDYMD      =   WRK-SRYYMD-1
                           MOVE    PTHKN-HKNNUM    TO  WRK-END-HKNNUM
                           MOVE    PTHKN-HKNJANUM  TO  WRK-END-HKNJANUM
                           MOVE    PTHKN-HKNID     TO  WRK-END-HKNID
                       END-IF
                   END-IF
      *
                   IF     (SPA-GMN26-HDRESEKBN =   SPACE    )
                    AND   (PTHKN-HKNNUM(1:1)   NOT =   "9"  )
                       IF      PTHKN-TEKSTYMD      =   WRK-SRYYMD
                           IF     (WRK-CHKSRT-HKNNUM   =   SPACE)
                           OR     (WRK-CHKEND-HKNNUM   =   PTHKN-HKNNUM)
                               MOVE    PTHKN-HKNNUM    TO
                                                   WRK-CHKSRT-HKNNUM
                               MOVE    PTHKN-HKNJANUM  TO
                                                   WRK-CHKSRT-HKNJANUM
                           END-IF
                       END-IF
                       IF      PTHKN-TEKEDYMD      =   WRK-SRYYMD-1
                           IF     (WRK-CHKEND-HKNNUM   =   SPACE)
                           OR     (WRK-CHKSRT-HKNNUM   =   PTHKN-HKNNUM)
                               MOVE    PTHKN-HKNNUM    TO
                                                   WRK-CHKEND-HKNNUM
                               MOVE    PTHKN-HKNJANUM  TO
                                                   WRK-CHKEND-HKNJANUM
                           END-IF
                       END-IF
                   END-IF
      *            対象年月の保険判定
                   IF    (PTHKN-TEKSTYMD(1:6)  <=  WRK-SRYYM )
                   AND   (PTHKN-TEKEDYMD(1:6)  >=  WRK-SRYYM )
                   AND   (PTHKN-HKNNUM(1:1)    NOT =   "9" )
                       MOVE    1               TO  FLG-HKNOK-ALL
                   END-IF
               END-IF
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               IF     (FLG-HKNOK-ALL       =   1     )  AND
                      (WRK-SRT-HKNNUM      =   SPACE )  AND
                      (WRK-CHKSRT-HKNNUM   =   SPACE )  AND
                      (SPA-GMN26-HDRESEKBN =   SPACE )
                   MOVE    "2637"              TO  SPA-ERRCD
                   MOVE    22                  TO  SPA-GMN26-CUR
                   MOVE    1                   TO  FLG-HKNOK-ERR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF    ((WRK-SRT-HKNNUM      =   SPACE     )  AND
                      (FLG-HKNOK-ALL       =   1         ))
               OR    ( WRK-SRT-HKNNUM      NOT =   WRK-END-HKNNUM)
                   MOVE    "2637"              TO  SPA-ERRCD
                   MOVE    24                  TO  SPA-GMN26-CUR
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF     (WRK-SRT-HKNNUM      =   WRK-END-HKNNUM)  AND
                      (WRK-SRT-HKNJANUM    NOT =   WRK-END-HKNJANUM)
                   MOVE    "2640"              TO  SPA-ERRCD
                   MOVE    24                  TO  SPA-GMN26-CUR
               END-IF
           END-IF
      *
      *    保険期間見出し
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  SPA-GMN26-HDMIDASI-G
               STRING  "（〜"
                       WRK-SRYDD-1
                       "日）"
                                           DELIMITED  BY  SIZE
                                           INTO  SPA-GMN26-HDMIDASI (1)
               END-STRING
               STRING  "（"
                       WRK-SRYDD
                       "日〜）"
                                           DELIMITED  BY  SIZE
                                           INTO  SPA-GMN26-HDMIDASI (2)
               END-STRING
      *
               IF      WRK-SRT-HKNNUM  NOT =   SPACE
                   MOVE    WRK-SRT-HKNID       TO  SPA-NAI26-STHKNID
                   MOVE    WRK-END-HKNID       TO  SPA-NAI26-EDHKNID
                   MOVE    WRK-SRT-HKNNUM      TO  SPA-NAI26-STHKNNUM
                   MOVE    WRK-END-HKNNUM      TO  SPA-NAI26-EDHKNNUM
               END-IF
           END-IF
           .
       410131-PTHKNINF-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ分割対象保険組合せテーブル編集処理
      *****************************************************************
       41013-LNKHKNCOMBI-HEN-SEC             SECTION.
      *
      *    パラメタファイルより編集
           INITIALIZE                  LNK-HKNCOMB-AREA
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  WRK-HKNCOMBI-MAX
           PERFORM
               UNTIL   FLG-PARA            =   1
               ADD     1                   TO  IDY
               IF      IDY                 <   200
                   MOVE    PARA-DATA-REC       TO  LNK-HKNCOMB-REC(IDY)
                   ADD     1                   TO  WRK-HKNCOMBI-MAX
               END-IF
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       41013-LNKHKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ分割対象保険組合せチェック処理
      *****************************************************************
       41013-HD-HKNCOMBI-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX-H
           MOVE    ZERO                TO  IDX-L1
           MOVE    ZERO                TO  IDX-L2
           MOVE    ZERO                TO  FLG-HKNCOMBI-ERR
      *
           INITIALIZE                  TBL-HDHKNLST-AREA
      *
      *    パラメタファイルより編集
           PERFORM 41013-LNKHKNCOMBI-HEN-SEC
      *
           PERFORM VARYING    IDX-C    FROM    1   BY  1
                   UNTIL      IDX-C    >   WRK-HKNCOMBI-MAX
               MOVE    LNK-HKNCOMB-REC(IDX-C)  TO  HKNCOMB-REC
      *
               MOVE    ZERO                TO  FLG-HKNOK
               IF     (COMB-TEKSTYMD(1:6)  <=  WRK-SRYYM  )
                 AND  (COMB-TEKEDYMD(1:6)  >=  WRK-SRYYM  )
                 AND  (COMB-DELKBN         =   SPACE )
      *            対象年月に有効
                   EVALUATE    COMB-HKNNUM
      *                国保
                       WHEN    "060"
                       WHEN    "063"
                       WHEN    "067"
                           IF      SPA-GMN26-HDRESEKBN     =   "2"
                               MOVE    1           TO  FLG-HKNOK
                           END-IF
      *                高齢者
                       WHEN    "039"
                       WHEN    "040"
                           IF      SPA-GMN26-HDRESEKBN     =   "3"
                               MOVE    1           TO  FLG-HKNOK
                           END-IF
      *                社保
                       WHEN    OTHER
                           IF      SPA-GMN26-HDRESEKBN     =   "1"
                               IF     (WRK-SRT-HKNNUM  NOT =   SPACE  )
                               AND    (WRK-SRT-HKNNUM  =   COMB-HKNNUM)
                                   MOVE    1               TO  FLG-HKNOK
                               END-IF
                               IF     (COMB-HKNNUM         =   SPACE)
      *?????                   AND    (WRK-SRT-HKNNUM      =   SPACE)
      *                            公費単独チェック
                                   PERFORM 410131-KOHONLY-CHK-SEC
                               END-IF
                           END-IF
                   END-EVALUATE
      *            保険ＩＤ判定
                   IF      FLG-HKNOK       =   1
      *                振替日前で保険ＩＤ判定
                       IF     (SPA-NAI26-EDHKNNUM  NOT =   SPACE)
                         AND  (SPA-NAI26-EDHKNNUM  NOT =   COMB-HKNNUM)
                         AND  (COMB-TEKEDYMD       <   WRK-SRYYMD)
                           MOVE    ZERO            TO  FLG-HKNOK
                       END-IF
      *                振替日後で保険ＩＤ判定
                       IF     (SPA-NAI26-STHKNNUM   NOT =   SPACE)
                         AND  (SPA-NAI26-STHKNNUM   NOT =   COMB-HKNNUM)
                         AND  (COMB-TEKSTYMD       >=  WRK-SRYYMD  )
                           MOVE    ZERO            TO  FLG-HKNOK
                       END-IF
                   END-IF
      *
                   IF     (FLG-HKNOK       =   1   )
                      OR  (FLG-HKNOK       =   2   )
      *                振替日以前
                       IF     (COMB-TEKEDYMD   <   WRK-SRYYMD  )
                           MOVE    1               TO  IDX-H
                           ADD     1               TO  IDX-L1
                           IF      IDX-L1          <=  50
                              MOVE    IDX-L1           TO  IDX-H2
                              PERFORM 41013-HD-HKNCOMMEI-SEC
                           END-IF
                       END-IF
      *                振替日以降
                       IF     (COMB-TEKSTYMD   >=  WRK-SRYYMD  )
                           MOVE    2               TO  IDX-H
                           ADD     1               TO  IDX-L2
                           IF      IDX-L2          <=  50
                              MOVE    IDX-L2           TO  IDX-H2
                              PERFORM 41013-HD-HKNCOMMEI-SEC
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           IF     (IDX-L1              =   ZERO )  OR
                  (IDX-L2              =   ZERO )
      *        対象なし
               MOVE    "2638"              TO  SPA-ERRCD
               MOVE    24                  TO  SPA-GMN26-CUR
               IF      SPA-NAI26-STHKNID   =   ZERO
                   MOVE    22                  TO  SPA-GMN26-CUR
               END-IF
           END-IF
      *    期間外の公費単独あり
           IF     (FLG-HKNCOMBI-ERR    =   1  )
             AND  (SPA-ERRCD           =   SPACE )
               MOVE    "2641"              TO  SPA-ERRCD
               MOVE    22                  TO  SPA-GMN26-CUR
           END-IF
           .
       41013-HD-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ公費単独チェック処理
      *****************************************************************
       410131-KOHONLY-CHK-SEC             SECTION.
      *
      *    心身喪失・精神入院・児童保護単独は対象外
           IF     (COMB-KOH1HKNNUM    =   "030"
                                      OR  "020"
                                      OR  "053" )
              AND (COMB-KOH2HKNNUM     =   SPACE )
              AND (COMB-KOH3HKNNUM     =   SPACE )
              AND (COMB-KOH4HKNNUM     =   SPACE )
               MOVE    ZERO                TO  FLG-HKNOK
               GO      TO     410131-KOHONLY-CHK-EXT
           END-IF
      *    振日での期間開始・終了
           IF     (COMB-TEKSTYMD       =   WRK-SRYYMD )
               MOVE    1               TO  FLG-HKNOK
           END-IF
           IF     (COMB-TEKEDYMD       =   WRK-SRYYMD-1 )
               MOVE    1               TO  FLG-HKNOK
           END-IF
      *
      *    保険ありの時は、公費単独はすべて対象とする
           IF     (FLG-HKNOK           =   ZERO )
             AND  (WRK-SRT-HKNNUM  NOT =   SPACE)
               MOVE    2               TO  FLG-HKNOK
           END-IF
      *H27.11
      *    公費単独もすべて対象
      *    IF      FLG-HKNOK           =   ZERO
      *        MOVE    1               TO  FLG-HKNCOMBI-ERR
      *    END-IF
      *
      *
           .
       410131-KOHONLY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       41013-HD-HKNCOMMEI-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBI-MEI
           IF      COMB-SYU-TANSEIDONAME   =   SPACE
               STRING  COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                                       INTO      WRK-HKNCOMBI-MEI
               END-STRING
           ELSE
               STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                                       INTO      WRK-HKNCOMBI-MEI
               END-STRING
           END-IF
      *
           MOVE    COMB-HKNCOMBINUM    TO  TBL-HD-HKNCOMBI
                                                  (IDX-H IDX-H2)
           MOVE    WRK-HKNCOMBI-MEI    TO  TBL-HD-HKNCOMBI-MEI
                                                  (IDX-H IDX-H2)
           MOVE    COMB-HYOJIKBN       TO  TBL-HD-HYOJIKBN
                                                  (IDX-H IDX-H2)
      *    非表示
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX5    FROM    1   BY  1
                   UNTIL      (IDX5        >   SPA-HYO-MAX )
                           OR (FLG-OK      =   1    )
               IF     (COMB-HKNCOMBINUM
                                       =   SPA-HYO-HKNCOMBINUM(IDX5))
               AND    (COMB-HKNNUM
                                       =   SPA-HYO-HKNNUM  (IDX5))
               AND    (COMB-KOH1HKNNUM
                                       =   SPA-HYO-KOH1HKNNUM (IDX5))
               AND    (COMB-KOH2HKNNUM
                                       =   SPA-HYO-KOH2HKNNUM (IDX5))
               AND    (COMB-KOH3HKNNUM
                                       =   SPA-HYO-KOH3HKNNUM (IDX5))
               AND    (COMB-KOH4HKNNUM
                                       =   SPA-HYO-KOH4HKNNUM (IDX5))
                   MOVE    SPA-HYO-HYOJIKBN (IDX5)
                                           TO  TBL-HD-HYOJIKBN
                                                  (IDX-H IDX-H2)
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           EVALUATE    TBL-HD-HYOJIKBN (IDX-H IDX-H2)
           WHEN    "1"
               MOVE    SPACE               TO  TBL-HD-HKNCOMBI-MEI
                                                  (IDX-H IDX-H2)
               STRING  "●非 "                 DELIMITED  BY  SIZE
                       WRK-HKNCOMBI-MEI        DELIMITED  BY  "  "
                                       INTO    TBL-HD-HKNCOMBI-MEI
                                                  (IDX-H IDX-H2)
               END-STRING
           WHEN    "2"
               MOVE    SPACE               TO  TBL-HD-HKNCOMBI-MEI
                                                  (IDX-H IDX-H2)
               STRING  "●入 "                 DELIMITED  BY  SIZE
                       WRK-HKNCOMBI-MEI        DELIMITED  BY  "  "
                                       INTO    TBL-HD-HKNCOMBI-MEI
                                                  (IDX-H IDX-H2)
               END-STRING
           WHEN    "3"
               MOVE    SPACE               TO  TBL-HD-HKNCOMBI-MEI
                                                  (IDX-H IDX-H2)
               STRING  "●外 "                 DELIMITED  BY  SIZE
                       WRK-HKNCOMBI-MEI        DELIMITED  BY  "  "
                                       INTO    TBL-HD-HKNCOMBI-MEI
                                                  (IDX-H IDX-H2)
               END-STRING
           END-EVALUATE
           .
       41013-HD-HKNCOMMEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦→和暦FORMAT日付編集
      *****************************************************************
       800-SE-WAFORMAT-YMDEDIT-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMD
           MOVE    ZERO                TO  FLG-ERR
           EVALUATE    WRK-SYM
               WHEN    ZERO
               WHEN    SPACE
                   MOVE    ALL "0"         TO  WRK-HENYMD
               WHEN    "999999"
                   MOVE    "999999 "       TO  WRK-HENYMD(1:7)
               WHEN    OTHER
                   MOVE    SPACE               TO  STS-AREA-DAY
                   INITIALIZE                      STS-AREA-DAY
                   MOVE    SPACE               TO  LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-SYM             TO  LNK-DAY2-YMD(1:6)
                   MOVE    "01"                TO  LNK-DAY2-YMD(7:2)
                   CALL    "ORCSDAY"           USING
                                                   STS-AREA-DAY
                                                   LNK-DAY2-AREA
                   IF    STS-DAY-RC1           =   ZERO
                      MOVE    LNK-DAY2-EDTYMD1     TO  WRK-HENYMD
                   ELSE
                      MOVE    SPACE                TO  WRK-HENYMD
                      MOVE    1                    TO  FLG-ERR
                   END-IF
           END-EVALUATE
           .
       800-SE-WAFORMAT-YMDEDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタファイル作成処理
      *****************************************************************
       400-PARA-INSERT-SEC           SECTION.
      *
      *    パラメタ登録
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  HZN-PARA-REC
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PARA
           END-IF
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    HZN-PARA-REC        TO  PARA-REC
           IF      FLG-PARA            =   1
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02 PARA INSERT ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY
                           ",EDANUM:" PARA-EDANUM
               END-IF
           ELSE
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02 PARA UPDATE ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY
                           ",EDANUM:" PARA-EDANUM
               END-IF
           END-IF
      *
           .
       400-PARA-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           MOVE    "tbl_para"         TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
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
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
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
