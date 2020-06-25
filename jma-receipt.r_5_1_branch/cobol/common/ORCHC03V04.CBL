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
       PROGRAM-ID.             ORCHC03V04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 外来請求書兼明細書　印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  10/05/17    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.00    NACL-多々納  14/03/XX  平成２６年４月改正対応
      *  04.07.00    NACL-多々納  14/10/14  コメント・自費ゼロ対象対応
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
       01  PRT-REC                 PIC X(20000).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "HCM03V04.INC".
      *
      *     印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
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
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
      *
           03  FLG-SP              PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-HIZUKECHK       PIC 9(01).
      *
           03  FLG-SYORI           PIC 9(01).
           03  FLG-DOUJI           PIC 9(01).
           03  FLG-HENPARA         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDXA                PIC 9(04).
           03  IDX-ACT             PIC 9(02).
           03  IDX-REN             PIC 9(04).
           03  IDX-JYU             PIC 9(04).
           03  IDX-JIHI            PIC 9(04).
      *
           03  IDX-SYU             PIC 9(04).
           03  IDX-SYU2            PIC 9(04).
           03  IDH3                PIC 9(04).
      *
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(04).
           03  IDXP                PIC 9(02).
           03  IDXM                PIC 9(02).
           03  IDK                 PIC 9(04).
           03  IDX2-1              PIC 9(01).
           03  IDX-MEMO            PIC 9(04).
      *
           03  IDX-H               PIC 9(04).
           03  IDX-TBL             PIC 9(04).
      *H26.10
           03  IDXC                PIC 9(04).
           03  IDX-CM              PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-YMD             PIC X(10).
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
           03  WRK-HKNCOMBIMEI     PIC X(40).
           03  WRK-SRYKA-MEI       PIC X(40).
      *
           03  WRK-SKYMONEY        PIC S9(11).
           03  WRK-MISYUMONEY      PIC S9(11).
           03  WRK-MISYU           PIC S9(11).
           03  WRK-GOKTEN          PIC S9(11).
           03  WRK-SKYMONEY-TAX-SAI        PIC S9(11).
      *
           03  WRK-TOTAL-HKNTEN            PIC S9(11).
           03  WRK-HKNTEKMONEY             PIC S9(11).
           03  WRK-KOH-FTN                 PIC S9(11).
           03  WRK-GOK-JIHI                PIC S9(11).
           03  WRK-CHOSEI                  PIC S9(11).
      *
           03  WRK-HEAD-DENPNUM        PIC 9(07).
           03  WRK-HEAD-HKNCOMBIMEI    PIC X(40).
           03  WRK-HEAD-SRYKAMEI       PIC X(40).
           03  WRK-FUTAN               PIC X(04).
           03  WRK-FUTAN-MEI           PIC X(02).
      *
           03  WRK-DENPNUM             PIC 9(07).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-HKNCOMBI            PIC X(04).
      *
           03  WRK-WARI                PIC 9(02)V9.
           03  WRK-Z6                  PIC ZZZ.
           03  WRK-Z62                 PIC Z9.9.
      *
           03  WRK-ZZZ                 PIC ZZZ.
           03  WRK-NO                  PIC Z(7).
           03  WRK-Z7-G.
               05  WRK-FIL             PIC X(09).
               05  WRK-Z7              PIC Z,ZZZ,ZZZ.
           03  WRK-Z72-G.
               05  WRK-FIL2            PIC X(18).
           03  WRK-Z72                 PIC S9(07).
           03  WRK-Z72-H               PIC Z,ZZZ,ZZZ.
           03  WRK-Z72-HS              REDEFINES   WRK-Z72-H
                                       PIC -,---,---.
           03  WRK-Z72-H9              PIC Z,ZZZ,ZZ9.
      *    全角変換用
           03  WRK-HENKAN-G            PIC X(10).
      *    自費集計
           03  WRK-JIHI-TOTAL          PIC S9(08).
      *
      *    複数科まとめ集計
           03  WRK-HKNTEN-KIN              PIC 9(07).
           03  WRK-HKNTEN-KEI              PIC 9(07)V99.
           03  WRK-NYUKIN                  PIC S9(07).
      *
           03  WRK-NYUKIN-TOTAL            PIC S9(07).
           03  WRK-ZENMISYU                PIC S9(07).
      *
           03  WRK-MEMOID-G.
               05  WRK-MEMOID              PIC X(04).
               05  WRK-MEMOIDNO            PIC 9(02).
      *
           03  WRK-LBLSRYYMD           PIC X(10).
           03  WRK-SRYYMDG             PIC X(50).
           03  WRK-STSRYYMDG           PIC X(22).
           03  WRK-EDSRYYMDG           PIC X(22).
           03  WRK-HAKYMD              PIC X(22).
           03  WRK-JNISSU              PIC ZZ9.
      *
           03  WRK-HOSPNAME            PIC X(100).
           03  WRK-HOSPNAME-R          REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1       PIC X(50).
               05  WRK-HOSPNAME2       PIC X(50).
           03  WRK-HOSPADRS            PIC X(100).
           03  WRK-HOSPADRS-R          REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1       PIC X(50).
               05  WRK-HOSPADRS2       PIC X(50).
           03  WRK-HOSPTEL             PIC X(15).
      *
           03  WRK-PAGECNT             PIC 9(03).
      *    伝票毎総ページ
           03  WRK-PAGEALL             PIC 9(03).
      *    総ページ
           03  WRK-PAGE-CNT            PIC 9(03).
      *
           03  WRK-PAGE                PIC 9(03).
           03  WRK-PAGE-MAX            PIC 9(03).
      *
           03  WRK-KEY-AREA.
               05  WRK-KEY-SRYKA           PIC X(02).
               05  WRK-KEY-HKNCOMBINUM     PIC X(04).
               05  WRK-KEY-DENPNUM         PIC 9(07).
               05  WRK-KEY-PCNT            PIC 9(07).
      *    診療費明細書
      *    名称＋数量編集
           03  WRK-NAME                 PIC X(148).
           03  WRK-NAME-R               REDEFINES   WRK-NAME.
               05  WRK-NAME-1           PIC X(74).
               05  WRK-NAME-2           PIC X(74).
           03  WRK-NAME-R2              REDEFINES   WRK-NAME.
               05  WRK-NAME-21.
                   07  WRK-NAME-211         PIC X(66).
                   07  WRK-NAME-212         PIC X(08).
               05  WRK-NAME-22          PIC X(74).
      *    診療日編集
           03  WRK-SRYMEI-H            PIC X(80).
           03  WRK-DAY-HEN.
               05  WRK-DAY-H1          PIC X(02).
               05  WRK-DAY-H2          PIC X(06).
               05  WRK-DAY-H3          PIC X(02).
           03  WRK-FILEMEI.
               05  WRK-FILEMEI-YMD     PIC X(08).
               05  WRK-FILEMEI-HMS     PIC X(06).
               05  WRK-FILEMEI-CD      PIC X(06).
      *H26.10
      *    コメントコード範囲
           03  WRK-COMMNT.
               05  WRK-COM-STR         PIC X(09).
               05  WRK-COM-FIL         PIC X(01).
               05  WRK-COM-END         PIC X(09).
      *
      *H26.10
       01  TBL-COM-HYOJI-AREA.
           03  TBL-COM-HYOJI-OCC       OCCURS   5.
               05  TBL-COM-STR             PIC X(09).
               05  TBL-COM-END             PIC X(09).
      *
      *    帳票テーブル
       01  TBL-MEISAI-AREA.
           03  TBL-MEISA-PAGE          OCCURS   50.
               05  TBL-MEISA-REC           OCCURS   36.
                   07  TBL-H-SRYKBN          PIC X(10).
                   07  TBL-H-SRYMEI.
                       09  TBL-H-ZKBN            PIC X(02).
                       09  TBL-H-NAME            PIC X(74).
                   07  TBL-H-TANINAME        PIC X(10).
                   07  TBL-H-SURYO           PIC X(11).
                   07  TBL-H-KAISU           PIC X(03).
                   07  TBL-H-TENSU.
                       09  TBL-H-TENSU1          PIC X(02).
                       09  TBL-H-TENSU2          PIC X(07).
                   07  TBL-H-KINGAKU.
                       09  TBL-H-KINGAKU1        PIC X(02).
                       09  TBL-H-KINGAKU2        PIC X(09).
                   07  TBL-H-EN              PIC X(01).
      *帳票明細数
       01  LINE-MAX                    PIC 9(03)   VALUE   0.
       01  LINE-MAX1                   PIC 9(03)   VALUE   24.
       01  LINE-MAX2                   PIC 9(03)   VALUE   36.
       01  PAGE-MAX                    PIC 9(03)   VALUE   50.
      *
      *点数記載用
       01  CONS-LABEL1.
           03  FILLER          PIC X(48)   VALUE
           "※金額欄については１０割分の表示をしております。".
           03  FILLER          PIC X(52)   VALUE
           "患者一部負担金は保険公費による負担割合分となります。".
      *金額記載用
       01  CONS-LABEL2.
           03  FILLER          PIC X(36)   VALUE
           "※合計欄は点数での表示をしています。".
           03  FILLER          PIC X(52)   VALUE
           "患者一部負担金はこの点数より計算（１点＝１０円）し、".
           03  FILLER          PIC X(38)   VALUE
           "保険公費による負担割合分となります。".
      *
      *    プログラムオプションパラメタ
       01  PARA-AREA.
           03  PARA-HAKKOUBIKBN        PIC X(01).
      *
           03  PARA-GOKEITANI          PIC X(01).
           03  PARA-SHADE              PIC X(01).
           03  PARA-HKNMEI             PIC X(01).
           03  PARA-FTNKBN             PIC X(01).
           03  PARA-SRYKA              PIC X(01).
           03  PARA-TEISEIKBN          PIC X(01).
           03  PARA-KANANAME           PIC X(01).
           03  PARA-LINE               PIC X(01).
           03  PARA-SYOKEI             PIC X(01).
           03  PARA-MISYUKBN1          PIC X(01).
           03  PARA-MISYUKBN2          PIC X(01).
      *H26.4
      *    注意書き
           03  PARA-CHUI-YUKOSTYMD     PIC X(08).
           03  PARA-CHUI-YUKOEDYMD     PIC X(08).
           03  PARA-RCHUI              PIC X(100).
           03  PARA-MCHUI              PIC X(100).
      *
      *H26.10
      *    対象コメントコード
           03  PARA-COM-HYOJI-G.
               05  PARA-COM-HYOJI01        PIC X(20).
               05  PARA-COM-HYOJI02        PIC X(20).
               05  PARA-COM-HYOJI03        PIC X(20).
               05  PARA-COM-HYOJI04        PIC X(20).
               05  PARA-COM-HYOJI05        PIC X(20).
           03  PARA-COM-HYOJI-G2       REDEFINES   PARA-COM-HYOJI-G.
               05  PARA-COM-HYOJI          PIC X(20)  OCCURS  5.
      *    自費対象
           03  PARA-JIHIKBN            PIC X(01).
      *
           03  WRK-PARA-NAME           PIC X(25).
           03  WRK-PARA                PIC X(100).
           03  WRK-PARA-N              PIC X(100).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    医療機関情報−基本情報 
           COPY    "CPSK1001.INC".
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1010.INC".
      *
           COPY    "CPSK1013.INC".
      *
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    保存用収納ワーク
       01  WRK-SYUNOU-REC.
           COPY    "CPSYUNOU.INC"  REPLACING
                                     //SYU-// BY //WRK-SYU-//.
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
           COPY    "CPSYUPARA01.INC".
           COPY    "CPSYUPARA03.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSGDAY.INC".
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *    請求書収納パラメタ
           COPY    "CPORCHC03SUB.INC".
      *
      *    請求書編集サブパラメタ
           COPY    "CPORCHC04SUB.INC".
      *
      *    保険組合せ名称編集サブ
           COPY    "CPORCSHKNMEI.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    請求書パラメタ
           COPY    "CPORCHC03.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCHC03AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           MOVE    ZERO                TO  WRK-PAGE-CNT
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    各ＤＢ読込
           PERFORM 100-SYORI-SEC
      *
           IF    ( ORCHC03-PARAFLG     =   1 )
               PERFORM 203-PARA-SYUNOU-SEC
           ELSE
               IF      ORCHC03-HAKHOUFLG   =   ZERO
      *            対象の伝票番号分のみ
                   PERFORM 201-ONLY-SYUNOU-SEC
               ELSE
      *            発行区分による
                   PERFORM 202-KAKHOU-SYUNOU-SEC
               END-IF
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    帳票印刷開始処理
      *****************************************************************
       400-SYOKI-PRINT-SEC             SECTION.
      *
      *    ファイルＯＰＥＮ
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    "HC03"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
      *
           ADD     1                   TO  WRK-PAGE-CNT
           MOVE    WRK-PAGE-CNT        TO  HC01PARA-CNT
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"           USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
      *
           OPEN    OUTPUT              PRT-FILE
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
           MOVE    HCM03V04            TO  PRT-REC
      *
      *    印刷用データ出力
           WRITE   PRT-REC
      *
           CLOSE                       PRT-FILE
      *
           INITIALIZE                  ORCSMKPRTAREA
           MOVE   "HC01.sh"            TO  MKPRT-ID
           MOVE   "HCM03V04.red"       TO  MKPRT-DIA
           MOVE   SPACE                TO  MKPRT-DEF
           MOVE   ASGNPARA             TO  MKPRT-DAT
           MOVE   SYS-1031-PRTNM(3)    TO  MKPRT-PRTNM
           IF    ( ORCHC03-PARAFLG     =   1 )
             AND ( ORCHC03-PARA-GYOUMUID   =   "SU1" )
               MOVE   "領収書兼明細書（修正）" 
                                       TO  MKPRT-INFO
           ELSE
               MOVE   "領収書兼明細書" TO  MKPRT-INFO
           END-IF
           CALL   "ORCSMKPRT1"         USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       400-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理 (印刷ＤＢに出力）
      *****************************************************************
       401-PRINT-SEC                SECTION.
      *
           MOVE    ORCHC03-PRTAREA     TO  ORCSPRTAREA
      *
           MOVE    "HCM03V04.red"      TO  SPRT-PRTID
           MOVE    HCM03V04            TO  SPRT-PRTDATA
      *-----------------------------------------------------------------
           MOVE    "1"                 TO  SPRT-SITEKBN
      *    カスタマイズＰＧの場合"2"をセットすること。
      *    MOVE    "2"                 TO  SPRT-SITEKBN
      *-----------------------------------------------------------------
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF    ( SPRT-RETURN         =   ZERO )
               CONTINUE
           ELSE
               DISPLAY "ORCSPRT ERR: PTID = " SPRT-PTID
           END-IF
      *
           .
       401-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       100-SYORI-SEC                SECTION.
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
      *    システム管理検索
           MOVE    SPA-SRYYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1001-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1001-REC
           ELSE
               INITIALIZE                  SYS-1001-REC
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1002-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1002-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1002-REC
           ELSE
               INITIALIZE                  SYS-1002-REC
           END-IF
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
      *
      *    プログラムオプション
           PERFORM 100-OPTION-SELECT-SEC
      *
      *    マシン日付取得サブ
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
      *
           .
       100-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           INITIALIZE                  PARA-AREA
           MOVE    "0"                 TO  PARA-HAKKOUBIKBN
      *
      *    伝票発行日区分
           MOVE    "HAKKOUBIKBN"       TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-HAKKOUBIKBN
      *    合計単位
           MOVE    "GOKEITANI"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-GOKEITANI
      *    網掛け
           MOVE    "SHADE"             TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-SHADE
      *
      *    保険組合せ表示
           MOVE    "HKNMEI"            TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-HKNMEI
           IF      PARA-HKNMEI         =   SPACE
               MOVE    "1"                 TO  PARA-HKNMEI
           END-IF
      *    負担割合表示
           MOVE    "FTNKBN"           TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-FTNKBN
           IF      PARA-FTNKBN         =   SPACE
               MOVE    "2"                 TO  PARA-FTNKBN
           END-IF
      *    診療科表示
           MOVE    "SRYKA"             TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-SRYKA
           IF      PARA-SRYKA          =   SPACE
               MOVE    "1"                 TO  PARA-SRYKA
           END-IF
      *    （訂正）表示
           MOVE    "TEISEIKBN"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-TEISEIKBN
           IF      PARA-TEISEIKBN      =   SPACE
               MOVE    "1"             TO  PARA-TEISEIKBN
           END-IF
      *    カナ氏名表示
           MOVE    "KANANAME"          TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-KANANAME
           IF      PARA-KANANAME       =   SPACE
               MOVE    "0"             TO  PARA-KANANAME
           END-IF
      *    行間
           MOVE    "LINE"              TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-LINE
           IF      PARA-LINE           =   SPACE
               MOVE    "1"                 TO  PARA-LINE
           END-IF
      *    小計
           MOVE    "SYOKEI"            TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-SYOKEI
           IF      PARA-SYOKEI         =   SPACE
               MOVE    "1"                 TO  PARA-SYOKEI
           END-IF
      *    前回請求額表示（診療行為業務）
           MOVE    "MISYUKBN1"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-MISYUKBN1
           IF      PARA-MISYUKBN1      =   "0" OR "1"
               CONTINUE
           ELSE
               MOVE    "1"                 TO  PARA-MISYUKBN1
           END-IF
      *    前回請求額表示（収納業務）
           MOVE    "MISYUKBN2"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-MISYUKBN2
           IF      PARA-MISYUKBN2      =   "0" OR "1"
               CONTINUE
           ELSE
               MOVE    "1"                 TO  PARA-MISYUKBN2
           END-IF
      *
      *    注意書き有効開始日
           MOVE    "CHUI_YUKOSTYMD"    TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA            TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           IF      FLG-HIZUKECHK       =   ZERO
               MOVE    WRK-SYMD            TO  PARA-CHUI-YUKOSTYMD
           ELSE
               MOVE    "00000000"          TO  PARA-CHUI-YUKOSTYMD
           END-IF
      *
      *    注意書き有効終了日
           MOVE    "CHUI_YUKOEDYMD"    TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA            TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           IF      FLG-HIZUKECHK       =   ZERO
               MOVE    WRK-SYMD            TO  PARA-CHUI-YUKOEDYMD
           ELSE
               MOVE    "99999999"          TO  PARA-CHUI-YUKOEDYMD
           END-IF
      *
      *    注意書き
           MOVE    "RCHUI"             TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA            TO  PARA-RCHUI
      *    注意書き
           MOVE    "MCHUI"             TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA            TO  PARA-MCHUI
      *
      *H26.10
      *    対象コメントコード
           MOVE    "COM_HYOUJI_1"      TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA            TO  PARA-COM-HYOJI01
           MOVE    "COM_HYOUJI_2"      TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA            TO  PARA-COM-HYOJI02
           MOVE    "COM_HYOUJI_3"      TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA            TO  PARA-COM-HYOJI03
           MOVE    "COM_HYOUJI_4"      TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA            TO  PARA-COM-HYOJI04
           MOVE    "COM_HYOUJI_5"      TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA            TO  PARA-COM-HYOJI05
           MOVE    "JIHIKBN"           TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA            TO  PARA-JIHIKBN
      *    コメント範囲編集
           PERFORM 1001-COMHYOJI-HEN-SEC
      *
           .
       100-OPTION-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプションパラメタ取得処理
      *****************************************************************
       1001-OPTION-CALL-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-PARA
           MOVE    SPACE               TO  WRK-PARA-N
      *
           INITIALIZE                  ORCSPRGOPTIONAREA
           MOVE    "ORCHC03V04"        TO  ORCSPRGOPTION-PRGID
           MOVE    WRK-PARA-NAME       TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
      *
           EVALUATE    WRK-PARA-NAME
           WHEN    "RCHUI" 
           WHEN    "MCHUI" 
      *        注意
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    ORCSPRGOPTION-OT-PARA(1:100)
                                           TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  WRK-PARA
           WHEN    OTHER
               IF      ORCSPRGOPTION-OT-TYPE   =   1
      *            半角
                   MOVE    ORCSPRGOPTION-OT-PARA   TO  WRK-PARA
               ELSE
                   MOVE    ORCSPRGOPTION-OT-PARA   TO  WRK-PARA-N
               END-IF
           END-EVALUATE
      *
           .
       1001-OPTION-CALL-EXT.
           EXIT.
      *
      *H26.10
      *****************************************************************
      *    対象コメントコード編集処理
      *****************************************************************
       1001-COMHYOJI-HEN-SEC                SECTION.
      *
           MOVE    SPACE               TO  TBL-COM-HYOJI-AREA
           MOVE    ZERO                TO  IDX
           PERFORM VARYING    IDXC     FROM    1   BY  1
                   UNTIL      IDXC     >   5
               MOVE    PARA-COM-HYOJI (IDXC)   TO  WRK-COMMNT
               PERFORM 10011-COMHYOJI-HENSYU-SEC
           END-PERFORM
           .
       1001-COMHYOJI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    対象コメントコード編集処理
      *****************************************************************
       10011-COMHYOJI-HENSYU-SEC                SECTION.
      *
           IF     (WRK-COM-STR(1:1)    =   "8"  )
               OR (WRK-COM-STR(1:3)    =   "008")
               ADD     1                   TO  IDX
               MOVE    WRK-COM-STR         TO  TBL-COM-STR (IDX)
               IF     (WRK-COM-END(1:1)    =   "8"  )
                   OR (WRK-COM-END(1:3)    =   "008")
                   MOVE    WRK-COM-END         TO  TBL-COM-END (IDX)
               ELSE
                   MOVE    WRK-COM-STR         TO  TBL-COM-END (IDX)
               END-IF
               IF      TBL-COM-STR (IDX)   >   TBL-COM-END (IDX)
                   MOVE    WRK-COM-STR         TO  TBL-COM-END (IDX)
               END-IF
           END-IF
           .
       10011-COMHYOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       1001-HKNCOMBI-HEN-SEC             SECTION.
      *
           INITIALIZE                  ORCSHKNMEIAREA
           MOVE    WRK-HKNCOMBI        TO  ORCSHKNMEI-HKNCOMBINUM
           MOVE    SYU-SRYYMD          TO  ORCSHKNMEI-SRYYMD
           MOVE    SPA-PTID            TO  ORCSHKNMEI-PTID
           CALL    "ORCSHKNMEI"        USING
                                       SPA-AREA
                                       ORCSHKNMEIAREA
           MOVE    ORCSHKNMEI-HKNCOMBIMEI  TO  WRK-HKNCOMBIMEI
      *
           .
       1001-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科名編集処理
      *****************************************************************
       1000-SRYKAMEI-HEN-SEC                SECTION.
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
               IF      WRK-SRYKA-MEI       =   SPACE
                   MOVE    SYS-1005-SRYKANAME2
                                               TO  WRK-SRYKA-MEI
               ELSE
                   IF      WRK-SRYKA-MEI(39:2) NOT =   SPACE
                       CONTINUE
                   ELSE
                       STRING  WRK-SRYKA-MEI   DELIMITED  BY  SPACE
                               "、"            DELIMITED  BY  SIZE
                               SYS-1005-SRYKANAME2
                                               DELIMITED  BY  SPACE
                               INTO                WRK-SRYKA-MEI
                       END-STRING
                   END-IF
               END-IF
           ELSE
               INITIALIZE                  SYS-1005-REC
           END-IF
      *
           .
       1000-SRYKAMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       300-HC03-HEN-SEC                SECTION.
      *
           INITIALIZE                  HCM03V04
      *
      *    頁
           MOVE    WRK-PAGEALL         TO  WRK-ZZZ
           MOVE    WRK-ZZZ             TO  HCM03V04-PAGE
      *
      *    伝票番号
           MOVE    WRK-HEAD-DENPNUM    TO  WRK-NO  
           MOVE    WRK-NO              TO  HCM03V04-NO
      *    診療科
           MOVE    WRK-HEAD-SRYKAMEI   TO  HCM03V04-SRYKANAME
      *    患者番号
           MOVE    SPA-PTNUM           TO  HCM03V04-PTNUM
      *    カナ名称
           IF    ( PARA-KANANAME   =   "1" )
               MOVE    SPA-KANANAME    TO  HCM03V04-KANANAME
           END-IF
      *    患者名称
           IF    ( SPA-NAME (31:)  NOT =   SPACE )
               MOVE    SPA-NAME        TO  HCM03V04-NAME2
           ELSE
               MOVE    SPA-NAME        TO  HCM03V04-NAME
           END-IF
      *    保険種別
           MOVE    WRK-HEAD-HKNCOMBIMEI     TO  HCM03V04-HOKEN
      *    負担割合
           MOVE    WRK-FUTAN           TO  HCM03V04-FUTAN
           MOVE    WRK-FUTAN-MEI       TO  HCM03V04-FUTANLABEL
      *    診療日
           MOVE    WRK-LBLSRYYMD       TO  HCM03V04-LBLSRYYMD
           MOVE    WRK-SRYYMDG         TO  HCM03V04-SRYYMD
      *    発行日
           MOVE    WRK-HAKYMD          TO  HCM03V04-HAKYMD
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS        TO  HCM03V04-IRYOADRS
           INSPECT HCM03V04-IRYOADRS   REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  HCM03V04-IRYOMEI
           INSPECT HCM03V04-IRYOMEI    REPLACING ALL "  " BY "　"
      *    今回入金額
           MOVE    WRK-NYUKIN          TO  WRK-Z72
           MOVE    1                   TO  FLG-HENPARA
           PERFORM 810-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HCM03V04-KONNYUKIN
      *    医療機関ＴＥＬ
           MOVE    WRK-HOSPTEL         TO  HCM03V04-IRYOTEL
      *    合計点数
           MOVE    WRK-TOTAL-HKNTEN    TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HCM03V04-GOKTEN
      *    保険適用分
           MOVE    WRK-HKNTEKMONEY     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HCM03V04-FTNKIN
      *    自費合計
           MOVE    WRK-GOK-JIHI        TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HCM03V04-JIHI
      *    公費一部負担金
           MOVE    WRK-KOH-FTN         TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HCM03V04-KOUHI
      *    調整金
           MOVE    WRK-CHOSEI          TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HCM03V04-CHOSEI
      *    前回未収額計算
           MOVE    WRK-MISYUMONEY      TO  WRK-Z72
           PERFORM 810-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HCM03V04-ZENMISYU
      *    今回請求額
           MOVE    WRK-SKYMONEY        TO  WRK-Z72
           PERFORM 810-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HCM03V04-KONSEIKYU
      *    合計請求額
           COMPUTE WRK-GOKTEN          =   WRK-SKYMONEY
                                       +   WRK-MISYUMONEY
           MOVE    WRK-GOKTEN          TO  WRK-Z72
           PERFORM 810-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HCM03V04-GOUKEIGAK
      *
      *    発行区分
           EVALUATE    ORCHC03-HAKKOFLG
               WHEN    2
                   IF    ( PARA-TEISEIKBN  =   "1" )
                       MOVE    "（訂正）"  TO  HCM03V04-MIDASHI
                   END-IF
           END-EVALUATE
      *
      *    列名（点数記載）
           IF      PARA-GOKEITANI      =   "1"
               MOVE    "　点　数　"        TO  HCM03V04-LABEL12
               MOVE    "　点　数　"        TO  HCM03V04-LABEL22
           ELSE
               MOVE    "　単　価　"        TO  HCM03V04-LABEL12
               MOVE    "　単　価　"        TO  HCM03V04-LABEL22
           END-IF
           MOVE    "　　合　　計　　"      TO  HCM03V04-LABEL1
           MOVE    "　　合　　計　　"      TO  HCM03V04-LABEL2
      *
      *    明細内容
      *    １枚目
           IF      WRK-PAGEALL         =   1
      *
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2    >   LINE-MAX1
                   MOVE    TBL-H-SRYKBN (IDX IDX2)
                                          TO  HCM03V04-SRYKBN1 (IDX2)
                   MOVE    TBL-H-SRYMEI (IDX IDX2)
                                          TO  HCM03V04-SRYMEI1 (IDX2)
                   IF      TBL-H-KAISU  (IDX IDX2)    =   "---"
                       MOVE    ALL "―"       TO  HCM03V04-KAIUSU12
                                                                (IDX2)
                   ELSE
                       MOVE    TBL-H-KAISU  (IDX IDX2)
                                              TO  HCM03V04-KAISU1 (IDX2)
                       MOVE    TBL-H-KINGAKU(IDX IDX2)
                                              TO  HCM03V04-KINGAKU1
                                                                 (IDX2)
                       MOVE    TBL-H-TENSU  (IDX IDX2)
                                              TO  HCM03V04-TENSU1
                                                                 (IDX2)
                   END-IF
      *            点数記載で円のもの
                   IF      PARA-GOKEITANI      =   "1"
                       IF      TBL-H-EN (IDX IDX2)     =   "1"
                           MOVE    SPACE           TO  HCM03V04-TENSU1
                                                                (IDX2)
                           STRING  TBL-H-TENSU2  (IDX IDX2)
                                   "　"
                                              DELIMITED  BY  SIZE
                                       INTO   HCM03V04-TENSU1   (IDX2)
                           END-STRING
                           MOVE    "円"            TO  HCM03V04-TENSU12
                                                                (IDX2)
                           MOVE    SPACE           TO  HCM03V04-KINGAKU1
                                                                (IDX2)
                           MOVE    SPACE         TO  HCM03V04-KINGAKU12
                                                                (IDX2)
                           STRING  TBL-H-KINGAKU2(IDX IDX2)
                                   "　"
                                              DELIMITED  BY  SIZE
                                       INTO   HCM03V04-KINGAKU1 (IDX2)
                           END-STRING
                           MOVE    "円"           TO  HCM03V04-KINGAKU12
                                                                (IDX2)
                       END-IF
                   END-IF
               END-PERFORM
           ELSE
      *        ２枚目
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2    >   LINE-MAX2
                   MOVE    TBL-H-SRYKBN (IDX IDX2)
                                          TO  HCM03V04-SRYKBN2 (IDX2)
                   MOVE    TBL-H-SRYMEI (IDX IDX2)
                                          TO  HCM03V04-SRYMEI2 (IDX2)
                   IF      TBL-H-KAISU  (IDX IDX2)    =   "---"
                       MOVE    ALL "―"       TO  HCM03V04-KAISU22(IDX2)
                   ELSE
                       MOVE    TBL-H-KAISU  (IDX IDX2)
                                              TO  HCM03V04-KAISU2 (IDX2)
                       MOVE    TBL-H-KINGAKU(IDX IDX2)
                                              TO  HCM03V04-KINGAKU2
                                                                 (IDX2)
                       MOVE    TBL-H-TENSU  (IDX IDX2)
                                              TO  HCM03V04-TENSU2
                                                                 (IDX2)
                   END-IF
      *            点数記載で円のもの
                   IF      PARA-GOKEITANI      =   "1"
                       IF      TBL-H-EN (IDX IDX2)     =   "1"
                           MOVE    SPACE           TO  HCM03V04-TENSU2
                                                                (IDX2)
                           STRING  TBL-H-TENSU2  (IDX IDX2)
                                   "　"
                                              DELIMITED  BY  SIZE
                                       INTO   HCM03V04-TENSU2   (IDX2)
                           END-STRING
                           MOVE    "円"           TO  HCM03V04-TENSU22
                                                                (IDX2)
                           MOVE    SPACE           TO  HCM03V04-KINGAKU2
                                                                (IDX2)
                           MOVE    SPACE         TO  HCM03V04-KINGAKU22
                                                                (IDX2)
                           STRING  TBL-H-KINGAKU2(IDX IDX2)
                                   "　"
                                              DELIMITED  BY  SIZE
                                       INTO   HCM03V04-KINGAKU2 (IDX2)
                           END-STRING
                           MOVE    "円"           TO  HCM03V04-KINGAKU22
                                                                (IDX2)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      PARA-GOKEITANI      =   "1"
               MOVE    CONS-LABEL2         TO  HCM03V04-LABEL3
           ELSE
               MOVE    CONS-LABEL1         TO  HCM03V04-LABEL3
           END-IF
      *H26.4
      *    注意
           IF      (SMCNDATE-YMD       >=  PARA-CHUI-YUKOSTYMD )
              AND  (SMCNDATE-YMD       <=  PARA-CHUI-YUKOEDYMD )
               MOVE    PARA-RCHUI          TO  HCM03V04-RCHUI
               MOVE    PARA-MCHUI          TO  HCM03V04-MCHUI
           END-IF
      *
           EVALUATE   PARA-SHADE
           WHEN    "0"
      *        網掛けなし
               IF      WRK-PAGEALL         =   1
      *            １枚目
                   STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                           " -L PAGE2"
                           " -L L1 -L L2 -L L3 "
                           " -L L21 -L L22 -L L23 "
                                               DELIMITED  BY  SIZE
                           "MonpeLayerOut"     DELIMITED  BY  SIZE
                                               INTO    HCM03V04-LOPTION
                   END-STRING
               ELSE
      *            ２枚目
                   STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                           " -L PAGE"
                           " -L L1 -L L2 -L L3 "
                           " -L L21 -L L22 -L L23 "
                                               DELIMITED  BY  SIZE
                           "MonpeLayerOut"     DELIMITED  BY  SIZE
                                               INTO    HCM03V04-LOPTION
                   END-STRING
               END-IF
      *
           WHEN    "1"
      *        網掛の濃さ１
               IF      WRK-PAGEALL         =   1
      *            １枚目
                   STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                           " -L PAGE2 -L L2 -L L3"
                           " -L L21 -L L22 -L L23 "
                                               DELIMITED  BY  SIZE
                           "MonpeLayerOut"     DELIMITED  BY  SIZE
                                               INTO    HCM03V04-LOPTION
                   END-STRING
               ELSE
      *            ２枚目
                   STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                           " -L PAGE -L L22 -L L23"
                           " -L L1 -L L2 -L L3 "
                                               DELIMITED  BY  SIZE
                           "MonpeLayerOut"     DELIMITED  BY  SIZE
                                               INTO    HCM03V04-LOPTION
                   END-STRING
               END-IF
           WHEN    "2"
      *        網掛の濃さ２
               IF      WRK-PAGEALL         =   1
      *            １枚目
                   STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                           " -L PAGE2 -L L1 -L L3"
                           " -L L21 -L L22 -L L23 "
                                               DELIMITED  BY  SIZE
                           "MonpeLayerOut"     DELIMITED  BY  SIZE
                                               INTO    HCM03V04-LOPTION
                   END-STRING
               ELSE
      *            ２枚目
                   STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                           " -L PAGE -L L21 -L L23"
                           " -L L1 -L L2 -L L3 "
                                               DELIMITED  BY  SIZE
                           "MonpeLayerOut"     DELIMITED  BY  SIZE
                                               INTO    HCM03V04-LOPTION
                   END-STRING
               END-IF
           WHEN    OTHER
      *            網掛濃さ３（標準）
               IF      WRK-PAGEALL         =   1
      *            １枚目
                   STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                           " -L PAGE2 -L L1 -L L2"
                           " -L L21 -L L22 -L L23"
                                               DELIMITED  BY  SIZE
                           "MonpeLayerOut"     DELIMITED  BY  SIZE
                                               INTO    HCM03V04-LOPTION
                   END-STRING
               ELSE
      *            ２枚目
                   STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                           " -L PAGE -L L21 -L L22"
                           " -L L1 -L L2 -L L3 "
                                               DELIMITED  BY  SIZE
                           "MonpeLayerOut"     DELIMITED  BY  SIZE
                                               INTO    HCM03V04-LOPTION
                   END-STRING
               END-IF
           END-EVALUATE
           .
       300-HC03-HEN-EXT.
           EXIT.
      *****************************************************************
      *   対象の伝票番号分のみ処理
      *****************************************************************
       201-ONLY-SYUNOU-SEC                SECTION.
      *
      *    収納マスタ検索
           MOVE    ORCHC03-DENPNUM     TO  WRK-DENPNUM
           PERFORM 2011-SYUNOU-KEN-SEC
      *
      *!!!
      *    マル長の公費負担を保険適用金額へ
           IF      SYU-HKNTEKKBN   =   "1"
               MOVE    SYU-KOH-FTN         TO  SYU-HKNTEKMONEY
               MOVE    ZERO                TO  SYU-KOH-FTN
           END-IF
      *!!!
      *    見出し合計編集処理
           PERFORM 21001-HAED-KEISAN-SEC
      *    見出し編集処理
           PERFORM 2100-HAED-HENSYU-SEC
      *
           MOVE    ORCHC03-DENPNUM     TO  WRK-HEAD-DENPNUM
           MOVE    ORCHC03-SEIKYU      TO  WRK-SKYMONEY
           MOVE    ORCHC03-NYUKIN      TO  WRK-NYUKIN
           MOVE    ORCHC03-SEIKYU-TAX-SAI  TO  WRK-SKYMONEY-TAX-SAI
           MOVE    ORCHC03-ZENMISYU-2  TO  WRK-MISYUMONEY
      *    前回請求額
           IF     (ORCHC03-ZEN-HENKIN  >   ZERO ) OR
                  (ORCHC03-KON-HENKIN (1)  >   ZERO )
      *        今回返金額
               COMPUTE WRK-MISYUMONEY      =   WRK-MISYUMONEY
                                           -   ORCHC03-ZEN-HENKIN
                                           -   ORCHC03-KON-HENKIN
                                                             (1)
           ELSE
      *        前回までの超過額
               COMPUTE WRK-MISYUMONEY      =   WRK-MISYUMONEY
                                           -   ORCHC03-KANYUKIN
               IF      WRK-MISYUMONEY      <   ZERO
                   MOVE    ZERO            TO  WRK-MISYUMONEY
               END-IF
           END-IF
      *
      *    プログラムオプションによる前回未収額印字判定
           PERFORM 2011-MISYU-OPTION-CHK-SEC
      *
      *    返金対応
           IF      (ORCHC03-HAKKOFLG   =   ZERO )  AND
                   (ORCHC03-NYUKIN     =   ZERO )  AND
                   (ORCHC03-ZEN-HENKIN >   ZERO )  AND
                   (WRK-MISYUMONEY     <   ZERO )  AND
                   ((WRK-MISYUMONEY * -1 )
                                       >   WRK-SKYMONEY)
      *        返金額が今回請求額より多い時、マイナス入金とする
               COMPUTE WRK-NYUKIN          =   WRK-SKYMONEY
                                           +   WRK-MISYUMONEY
           END-IF
      *    訂正時に合計金額表示
           IF     (ORCHC03-TEIKIN-KBN  =   1   )  AND
                  (ORCHC03-HAKKOFLG    =   2   )
               MOVE    SYU-SKYMONEY        TO  WRK-SKYMONEY
               MOVE    SYU-NYUKIN-TOTAL    TO  WRK-NYUKIN
               MOVE    SYU-SKYMONEY-TAX-SAI
                                           TO  WRK-SKYMONEY-TAX-SAI
           END-IF
      *
           MOVE    1                   TO  FLG-SYORI
           MOVE    ZERO                TO  FLG-DOUJI
      *
           INITIALIZE                  TBL-MEISAI-AREA
           MOVE    1                   TO  WRK-PAGE
           MOVE    1                   TO  WRK-PAGECNT
           MOVE    ZERO                TO  WRK-PAGEALL
      *   １枚目の最大
           MOVE    LINE-MAX1           TO  LINE-MAX
           MOVE    ZERO                TO  IDX-H
           MOVE    ZERO                TO  WRK-KEY-PCNT
           MOVE    ORCHC03-DENPNUM     TO  WRK-DENPNUM
      *    伝票番号毎の印刷
           PERFORM 2100-DENPNUM-SYORI-SEC
      *??  IF      IDX-H               >   ZERO
      *       印刷処理
              PERFORM 400-PRINT-SYORI-SEC
      *??  END-IF
           .
       201-ONLY-SYUNOU-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプションによる前回未収額印字判定
      *****************************************************************
       2011-MISYU-OPTION-CHK-SEC       SECTION.
      *
           IF    ( ORCHC03-PARAFLG     =   ZERO )
               IF    ( ORCHC03-MOTOID      =   ZERO )
                  AND( PARA-MISYUKBN1      =   ZERO )
                   IF    ( ORCHC03-MATOME-NYUHENKIN-KBN
                                       =   SPACE )
                       MOVE    ZERO    TO  WRK-MISYUMONEY
                   END-IF
               END-IF
      *
               IF    ( ORCHC03-MOTOID      =   3 )
                  AND( PARA-MISYUKBN2      =   ZERO )
                   IF    ( ORCHC03-MATOME-NYUHENKIN-KBN
                                       =   SPACE )
                       MOVE    ZERO    TO  WRK-MISYUMONEY
                   END-IF
               END-IF
           END-IF
      *
           .
       2011-MISYU-OPTION-CHK-EXT.
           EXIT.
      *****************************************************************
      *   収納検索処理
      *****************************************************************
       2011-SYUNOU-KEN-SEC                SECTION.
      *
      *    収納マスタ検索
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    WRK-DENPNUM         TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2011-SYUNOU-KEN-EXT.
           EXIT.
      *****************************************************************
      *    見出し編集処理
      *****************************************************************
       2100-HAED-HENSYU-SEC                SECTION.
      *
      *    診療日
           PERFORM 3101-SRYYMD-HEN-SEC
      *    発行日
           IF    ( ORCHC03-MOTOID      =    1 OR 3 OR 4 )
            AND  ( PARA-HAKKOUBIKBN    =   "1" )
               MOVE    SPA-SYSYMD          TO  WRK-SYMD
           ELSE
               MOVE    SYU-DENPPRTYMD      TO  WRK-SYMD
           END-IF
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-HAKYMD
      *
      *    診療科名編集
           IF      PARA-SRYKA          =   "0"
               MOVE    SPACE               TO  WRK-SRYKA-MEI
           ELSE
               MOVE    SPACE               TO  WRK-SRYKA-MEI
               MOVE    SYU-SRYKA           TO  WRK-SRYKA
               PERFORM 1000-SRYKAMEI-HEN-SEC
           END-IF
           MOVE    WRK-SRYKA-MEI       TO  WRK-HEAD-SRYKAMEI
      *    保険組合せ
           IF     (PARA-HKNMEI         =   "0" )
               MOVE    SPACE               TO  WRK-HKNCOMBIMEI
           ELSE
               MOVE    SYU-HKNCOMBINUM     TO  WRK-HKNCOMBI
               PERFORM 1001-HKNCOMBI-HEN-SEC
           END-IF
           MOVE    WRK-HKNCOMBIMEI     TO  WRK-HEAD-HKNCOMBIMEI
      *    負担割合
           MOVE    SPACE               TO  WRK-FUTAN
           MOVE    SPACE               TO  WRK-FUTAN-MEI
           EVALUATE    PARA-FTNKBN
               WHEN    "0"
                   MOVE    SPACE               TO  WRK-FUTAN
               WHEN    "1"
      *            %　表示
                   MOVE    SYU-PTFTNRATE       TO  WRK-Z6
                   MOVE    WRK-Z6              TO  WRK-FUTAN
                   MOVE    "％"                TO  WRK-FUTAN-MEI
               WHEN    "2"
      *            割　表示
                   IF      SYU-PTFTNRATE       >   ZERO
                       COMPUTE WRK-WARI        =   SYU-PTFTNRATE
                                               /   10
                       IF      SYU-PTFTNRATE(3:1)      =   ZERO
                           MOVE    WRK-WARI            TO  WRK-Z6
                           MOVE    WRK-Z6              TO  WRK-FUTAN
                       ELSE
                           MOVE    WRK-WARI            TO  WRK-Z62
                           MOVE    WRK-Z62             TO  WRK-FUTAN
                       END-IF
                   ELSE
                       MOVE    SYU-PTFTNRATE       TO  WRK-Z6
                       MOVE    WRK-Z6              TO  WRK-FUTAN
                   END-IF
                   MOVE    "割"                TO  WRK-FUTAN-MEI
           END-EVALUATE
           .
       2100-HAED-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    見出し合計編集処理
      *****************************************************************
       21001-HAED-KEISAN-SEC                SECTION.
      *
      *    調整金
           MOVE    SYU-CHOSEI          TO  WRK-CHOSEI
      *    合計点数
           MOVE    SYU-TOTAL-HKNTEN    TO  WRK-TOTAL-HKNTEN
      *    保険適用金額
           MOVE    SYU-HKNTEKMONEY     TO  WRK-HKNTEKMONEY
      *    公費一部負担金
           MOVE    SYU-KOH-FTN         TO  WRK-KOH-FTN
      *    自費合計
           COMPUTE WRK-JIHI-TOTAL      =   SYU-JIHI-TOTAL
                                       +   SYU-JIHI-TOTAL-TAX
                                       +   SYU-TOTAL-TGMONEY
                                       +   SYU-TOTAL-TGMONEY-TAX
           IF      SYU-SRYYMD          <=  "20070331"
               COMPUTE WRK-GOKTEN          =   WRK-JIHI-TOTAL
                                           +   SYU-JIHI-TAX
           ELSE
               MOVE    WRK-JIHI-TOTAL      TO  WRK-GOKTEN
           END-IF
           MOVE    WRK-GOKTEN          TO  WRK-GOK-JIHI
           .
       21001-HAED-KEISAN-EXT.
           EXIT.
      *****************************************************************
      *   複数科・保険入力の印刷処理
      *****************************************************************
       202-KAKHOU-SYUNOU-SEC                SECTION.
      *
           INITIALIZE                  ORCHC03SUBAREA
      *    収納マスタまとめ計算
           CALL    "ORCHC03SUB"        USING
                                       SPA-AREA
                                       ORCHC03AREA
                                       ORCHC03SUBAREA
      *    入金合計
           MOVE    ZERO                TO  WRK-NYUKIN-TOTAL
           PERFORM VARYING IDX-SYU     FROM    1   BY  1
                   UNTIL   IDX-SYU     >   ORCHC03SUB-MAX
               ADD     ORCHC03SUB-NYUKIN-TOTAL(IDX-SYU)
                                           TO  WRK-NYUKIN-TOTAL
           END-PERFORM
      *    入金合計から前回未収額の入金額を求める
           IF      ORCHC03-NYUKIN      >   WRK-NYUKIN-TOTAL
               COMPUTE WRK-ZENMISYU    =   ORCHC03-NYUKIN
                                       -   WRK-NYUKIN-TOTAL
           ELSE
               MOVE    ZERO            TO  WRK-ZENMISYU
           END-IF
      *
           MOVE    1                   TO  FLG-SYORI
      *
           PERFORM VARYING IDX-SYU     FROM    1   BY  1
                   UNTIL   IDX-SYU     >   ORCHC03SUB-MAX
               MOVE    ORCHC03SUB-SYUNOU-REC(IDX-SYU)
                                           TO  SYUNOU-REC
      *
               MOVE    SYU-DENPNUM         TO  WRK-HEAD-DENPNUM
               MOVE    ORCHC03SUB-SKYMONEY (IDX-SYU)
                                           TO  WRK-SKYMONEY
               MOVE    ORCHC03SUB-NYUKIN-TOTAL (IDX-SYU)
                                           TO  WRK-NYUKIN
               MOVE    ORCHC03SUB-SKYMONEY-TAX-SAI (IDX-SYU)
                                           TO  WRK-SKYMONEY-TAX-SAI
      *
      *        １枚目に前回未収額を表示する
               IF      IDX-SYU             =   1
                  AND  (ORCHC03-HAKKOFLG   =   ZERO )
                   MOVE    ORCHC03-ZENMISYU-2  TO  WRK-MISYUMONEY
                   COMPUTE WRK-NYUKIN          =   WRK-NYUKIN
                                               +   WRK-ZENMISYU
               ELSE
                   MOVE    ZERO                TO  WRK-MISYUMONEY
               END-IF
               IF      IDX-SYU             =   1
                   IF     (ORCHC03-ZEN-HENKIN  >   ZERO )  OR
                          (ORCHC03SUB-HENKIN (IDX-SYU) >  ZERO)
                       COMPUTE WRK-MISYUMONEY  =   WRK-MISYUMONEY
                                               -   ORCHC03-ZEN-HENKIN
                                               -   ORCHC03SUB-HENKIN
                                                         (IDX-SYU)
                   ELSE
                       COMPUTE WRK-MISYUMONEY  =   WRK-MISYUMONEY
                                               -   ORCHC03-KANYUKIN
                       IF      WRK-MISYUMONEY      <   ZERO
                           MOVE    ZERO            TO  WRK-MISYUMONEY
                       END-IF
                   END-IF
      *
      *            プログラムオプションによる前回未収額印字判定
                   PERFORM 2011-MISYU-OPTION-CHK-SEC
      *
      *            返金対応
                   IF      (ORCHC03-HAKKOFLG   =   ZERO )  AND
                           (WRK-NYUKIN         =   ZERO )  AND
                           (ORCHC03-ZEN-HENKIN >   ZERO )  AND
                           (WRK-MISYUMONEY     <   ZERO )  AND
                           ((WRK-MISYUMONEY * -1)
                                                >   WRK-SKYMONEY)
      *            返金額が今回請求額より多い時、マイナス入金とする
                       COMPUTE WRK-NYUKIN      =   WRK-SKYMONEY
                                               +   WRK-MISYUMONEY
                   END-IF
               ELSE
                   COMPUTE WRK-MISYUMONEY  =   WRK-MISYUMONEY
                                           -   ORCHC03SUB-HENKIN
                                                         (IDX-SYU)
               END-IF
      *        見出し合計編集処理
               PERFORM 21001-HAED-KEISAN-SEC
      *        見出し編集処理
               PERFORM 2100-HAED-HENSYU-SEC
      *        複数診療科名編集
               IF     (ORCHC03SUB-DOUJI-SRYKA-G (IDX-SYU)
                                           NOT =   SPACE )
                AND   (PARA-SRYKA          NOT =   "0"   )
                   MOVE    SPACE               TO  WRK-SRYKA-MEI
                   PERFORM 2021-GRP-SRYKAMEI-HEN-SEC
                   MOVE    WRK-SRYKA-MEI       TO  WRK-HEAD-SRYKAMEI
               END-IF
      *        保険組合せ
               IF     (SYU-HKNCOMBINUM     =   ZERO  OR  SPACE )
                AND   (PARA-HKNMEI     NOT =   "0"   )
                   MOVE    "複数保険使用"      TO  WRK-HEAD-HKNCOMBIMEI
                   MOVE    SPACE               TO  WRK-FUTAN
                   MOVE    SPACE               TO  WRK-FUTAN-MEI
               END-IF
      *
               MOVE    SYU-DENPNUM         TO  WRK-DENPNUM
      *        収納マスタ検索
               PERFORM 2011-SYUNOU-KEN-SEC
      *        複数伝票
               IF      ORCHC03SUB-DOUJI-DENPNUM (IDX-SYU 1)
                                           =   ZERO
                   MOVE    ZERO                TO  FLG-DOUJI
               ELSE
                   MOVE    1                   TO  FLG-DOUJI
               END-IF
               INITIALIZE                  TBL-MEISAI-AREA
               MOVE    1                   TO  WRK-PAGE
               MOVE    1                   TO  WRK-PAGECNT
               MOVE    ZERO                TO  WRK-PAGEALL
      *        １枚目
               MOVE    LINE-MAX1           TO  LINE-MAX
               MOVE    ZERO                TO  WRK-KEY-PCNT
               MOVE    ZERO                TO  IDX-H
      *        伝票番号毎の印刷
               PERFORM 2100-DENPNUM-SYORI-SEC
      *        複数伝票番号
               PERFORM VARYING IDX-SYU2    FROM    1   BY  1
                       UNTIL  (IDX-SYU2    >   15   )   OR
                              (ORCHC03SUB-DOUJI-DENPNUM
                                              (IDX-SYU IDX-SYU2)
                                               =   ZERO )
                   MOVE    ORCHC03SUB-DOUJI-DENPNUM (IDX-SYU IDX-SYU2)
                                               TO  WRK-DENPNUM
      *            収納マスタ検索
                   PERFORM 2011-SYUNOU-KEN-SEC
      *            伝票番号毎の印刷
                   PERFORM 2100-DENPNUM-SYORI-SEC
               END-PERFORM
      *
      *??      IF      IDX-H               >   ZERO
      *            印刷処理
                   PERFORM 400-PRINT-SYORI-SEC
      *??      END-IF
           END-PERFORM
      *
           .
       202-KAKHOU-SYUNOU-EXT.
           EXIT.
      *
      *****************************************************************
      *   伝票番号毎の印刷処理
      *****************************************************************
       2100-DENPNUM-SYORI-SEC                 SECTION.
      *
           EVALUATE    ORCHC03-HAKHOUFLG
      *        科・保険別
               WHEN    1
                   MOVE    SYU-SRYKA       TO  WRK-KEY-SRYKA
                   MOVE    SYU-HKNCOMBINUM TO  WRK-KEY-HKNCOMBINUM
      *        保険別
               WHEN    2
                   MOVE    SPACE           TO  WRK-KEY-SRYKA
                   MOVE    SYU-HKNCOMBINUM TO  WRK-KEY-HKNCOMBINUM
      *        科別
               WHEN    3
                   MOVE    SYU-SRYKA       TO  WRK-KEY-SRYKA
                   MOVE    SPACE           TO  WRK-KEY-HKNCOMBINUM
      *        合計
               WHEN    4
                   MOVE    SPACE           TO  WRK-KEY-SRYKA
                   MOVE    SPACE           TO  WRK-KEY-HKNCOMBINUM
           END-EVALUATE
      *
      *    伝票番号毎明細編集処理
           INITIALIZE                  ORCHC04SUBAREA
           MOVE    WRK-DENPNUM         TO  ORCHC04SUB-DENPNUM
           MOVE    PARA-GOKEITANI      TO  ORCHC04SUB-GOKEITANI
           MOVE    PARA-SYOKEI         TO  ORCHC04SUB-PARAKBN
      *H26.10
      *    コメント対象区分
           IF      TBL-COM-STR (1) NOT =   SPACE
               MOVE    "1"             TO  ORCHC04SUB-PARACOMKBN
           END-IF
      *    自費対象区分
           IF      PARA-JIHIKBN        =   "1"
               MOVE    "1"             TO  ORCHC04SUB-PARAJIHIKBN
           END-IF
      *
           CALL    "ORCHC04SUB"        USING
                                       SPA-AREA
                                       ORCHC04SUBAREA
           MOVE    ORCHC04SUB-TBL-MAX  TO  IDX-TBL
      *
           IF      IDX-TBL             >   ZERO
      *        診療科・保険組合せ
               IF    ((WRK-KEY-PCNT        =   ZERO )
               AND    (ORCHC03-HAKHOUFLG   =   1    )
               AND    (FLG-SYORI           =   1    ))
      *        伝票が１枚の時、切り替えなし
               OR     (FLG-DOUJI           =   ZERO )
                   MOVE    SYU-SRYKA       TO  WRK-SYU-SRYKA
                   MOVE    SYU-HKNCOMBINUM TO  WRK-SYU-HKNCOMBINUM
               ELSE
      *            伝票毎の切り替え
                   PERFORM 3006-KEY-HENSYU-SEC
               END-IF
               PERFORM 3005-PRTTBL-HENSYU-SEC
               ADD     1                   TO  WRK-KEY-PCNT
           END-IF
      *
           .
       2100-DENPNUM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    伝票毎の切り替え処理
      *****************************************************************
       3006-KEY-HENSYU-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYKA-MEI
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
      *    診療科名編集
           IF      WRK-KEY-SRYKA       =   SPACE
               IF     (PARA-SRYKA          =   "0" )
                   MOVE    SPACE               TO  WRK-SRYKA-MEI
               ELSE
                   MOVE    SYU-SRYKA           TO  WRK-SRYKA
                   PERFORM 1000-SRYKAMEI-HEN-SEC
               END-IF
           END-IF
      *    保険組合せ
           IF      WRK-KEY-HKNCOMBINUM =   SPACE
               IF     (PARA-HKNMEI         =   "0" )
                   MOVE    SPACE               TO  WRK-HKNCOMBIMEI
               ELSE
                   MOVE    SYU-HKNCOMBINUM     TO  WRK-HKNCOMBI
                   PERFORM 1001-HKNCOMBI-HEN-SEC
               END-IF
           END-IF
      *
      *    剤毎に１行開ける
           IF     (PARA-LINE           =   "1"   )
           AND    (IDX-H               >   ZERO  )
               ADD     2                   TO  IDX-H
           ELSE
               ADD     1                   TO  IDX-H
           END-IF
           IF      IDX-H               >=  LINE-MAX
               PERFORM 200520-PAGE-CNT-SEC
           END-IF
           MOVE    ALL "―"            TO  TBL-H-SRYKBN (WRK-PAGE IDX-H)
           MOVE    ALL "-"             TO  TBL-H-KAISU  (WRK-PAGE IDX-H)
           MOVE    ALL "-"             TO  TBL-H-TENSU  (WRK-PAGE IDX-H)
           MOVE    ALL "-"             TO  TBL-H-KINGAKU(WRK-PAGE IDX-H)
           EVALUATE    TRUE
               WHEN   (WRK-SRYKA-MEI   NOT =   SPACE )  AND
                      (WRK-HKNCOMBIMEI NOT =   SPACE )
                   STRING    "≪"
                             WRK-SRYKA-MEI
                             "≫　" 
                             "【"
                             WRK-HKNCOMBIMEI
                             "】"              DELIMITED  BY  SPACE
                                   INTO    TBL-H-SRYMEI(WRK-PAGE IDX-H)
                   END-STRING
               WHEN   (WRK-SRYKA-MEI   NOT =   SPACE )  AND
                      (WRK-HKNCOMBIMEI     =   SPACE )
                   STRING    "≪"
                             WRK-SRYKA-MEI
                             "≫"              DELIMITED  BY  SPACE
                                   INTO    TBL-H-SRYMEI(WRK-PAGE IDX-H)
                   END-STRING
               WHEN   (WRK-SRYKA-MEI       =   SPACE )  AND
                      (WRK-HKNCOMBIMEI NOT =   SPACE )
                   STRING    "【"
                             WRK-HKNCOMBIMEI
                             "】"              DELIMITED  BY  SPACE
                                   INTO    TBL-H-SRYMEI(WRK-PAGE IDX-H)
                   END-STRING
           END-EVALUATE
      *
           IF      FLG-SYORI           =   2
               MOVE    TBL-H-SRYMEI(WRK-PAGE IDX-H)  TO  WRK-SRYMEI-H
               MOVE    SPACE           TO  TBL-H-SRYMEI(WRK-PAGE IDX-H)
               STRING  WRK-DAY-HEN
                       WRK-SRYMEI-H         DELIMITED  BY  SIZE
                                   INTO    TBL-H-SRYMEI(WRK-PAGE IDX-H)
               END-STRING
           END-IF
      *
           INSPECT TBL-H-SRYMEI(WRK-PAGE IDX-H)
                           REPLACING    ALL    "  "   BY  "―"
      *
           IF      PARA-LINE           =   "1"
               CONTINUE
           ELSE
               ADD     1                   TO  IDX-H
           END-IF
           .
        3006-KEY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷内容明細編集処理
      *****************************************************************
       3005-PRTTBL-HENSYU-SEC          SECTION.
      *
      *H26.10
      *    コメント対象区分
           IF      TBL-COM-STR (1) NOT =   SPACE
               PERFORM 30050-COMMNT-CHK-SEC
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   ORCHC04SUB-TBL-MAX
      *H26.10
             IF      ORCHC04SUB-SRYCD (IDX)    NOT =   "999999999"
      *
               ADD     1                   TO  IDX-H
               IF      IDX-H               >=  LINE-MAX
                   PERFORM 200520-PAGE-CNT-SEC
               END-IF
      *        名称＋数量編集
               IF     (ORCHC04SUB-SURYO  (IDX) =   ZERO )
                 OR   (ORCHC04SUB-SRYKBN (IDX) =   "95"
                                               OR  "96"     )
      *            自費の数量記載なし
                   MOVE    ORCHC04SUB-NAME (IDX)   TO  WRK-NAME
               ELSE
                   MOVE    SPACE                   TO  WRK-NAME
                   STRING  ORCHC04SUB-NAME (IDX)
                           "　　"
                           ORCHC04SUB-H-ZEN-SURYO (IDX)
                           DELIMITED  BY  SPACE
                                           INTO   WRK-NAME
                   END-STRING
               END-IF
      *        小計
               IF      ORCHC04SUB-SRYKBN (IDX)     =   "XX"
                   MOVE    ALL "　"                TO  WRK-NAME-211
                   MOVE    "（小計）"              TO  WRK-NAME-212
               END-IF
      *
      *        名称と単位名称チェック
               IF     (WRK-NAME-2        NOT =   SPACE)
      *            最終行の時、改ページ
                   IF      IDX-H               >=  (LINE-MAX  - 1 )
                       PERFORM 200520-PAGE-CNT-SEC
                   END-IF
               END-IF
               MOVE    ORCHC04SUB-H-SRYKBN(IDX)    TO  TBL-H-SRYKBN
                                                    (WRK-PAGE IDX-H)
      *        ＊
               MOVE    ORCHC04SUB-H-ZAIHEN (IDX)
                                           TO  TBL-H-ZKBN
                                                     (WRK-PAGE IDX-H)
               MOVE    WRK-NAME-1          TO  TBL-H-NAME
                                                     (WRK-PAGE IDX-H)
      *        単位名称
               IF     (WRK-NAME-2      NOT =   SPACE)
                   ADD     1                   TO  IDX-H
                   MOVE    WRK-NAME-2          TO  TBL-H-NAME
                                                     (WRK-PAGE IDX-H)
                   MOVE    "　"                TO  TBL-H-ZKBN
                                                     (WRK-PAGE IDX-H)
               END-IF
      *        点数（単価）
               MOVE    ORCHC04SUB-H-TENSU   (IDX)  TO  TBL-H-TENSU
                                                   (WRK-PAGE IDX-H)
      *        合計
               MOVE    ORCHC04SUB-H-KINGAKU (IDX)  TO  TBL-H-KINGAKU
                                                   (WRK-PAGE IDX-H)
      *        円
               MOVE    ORCHC04SUB-H-ENKBN (IDX)    TO  TBL-H-EN
                                                   (WRK-PAGE IDX-H)
      *        回数
               MOVE    ORCHC04SUB-H-KAISU (IDX)    TO  TBL-H-KAISU
                                                   (WRK-PAGE IDX-H)
      *        剤毎に１行開ける
               IF      PARA-LINE           =   "1"
                   CONTINUE
               ELSE
                   IF      ORCHC04SUB-ZAIEND (IDX) =   "1"
                       ADD     1                   TO  IDX-H
                   END-IF
               END-IF
      *H26.10
             END-IF
      *
           END-PERFORM
           MOVE    WRK-PAGE            TO  WRK-PAGE-MAX
      *
           .
       3005-PRTTBL-HENSYU-EXT.
           EXIT.
      *
      *H26.10
      *****************************************************************
      *    コメント判定追加処理
      *****************************************************************
       30050-COMMNT-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-CM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   ORCHC04SUB-TBL-MAX
               MOVE    1                   TO  FLG-OK
      *
               IF     (ORCHC04SUB-SRYCD (IDX)(1:1) =   "8" )
                  OR  (ORCHC04SUB-SRYCD (IDX)(1:3) =   "008")
                   MOVE    ZERO                TO  FLG-OK
                   PERFORM VARYING     IDXC    FROM    1   BY  1
                           UNTIL      (IDXC    >   5 )
                                OR    (TBL-COM-STR (IDXC)  =   SPACE)
                                OR    (FLG-OK      =   1   )
                       IF    (ORCHC04SUB-SRYCD (IDX)
                                           >=  TBL-COM-STR (IDXC))
                        AND  (ORCHC04SUB-SRYCD (IDX)
                                           <=  TBL-COM-END (IDXC))
                           MOVE    1           TO  FLG-OK
                       END-IF
                   END-PERFORM
                   IF      FLG-OK              =   ZERO
      *                対象外
                       IF     (ORCHC04SUB-H-ZAIHEN(IDX)
                                                   NOT =   "＊")
                         AND  (ORCHC04SUB-H-TENSU(IDX-CM)
                                                       =   SPACE )
                         AND  (ORCHC04SUB-H-KINGAKU(IDX-CM)
                                                       =   SPACE )
      *                    剤の最後
                           MOVE    ORCHC04SUB-H-KAISU (IDX)
                                               TO  ORCHC04SUB-H-KAISU
                                                             (IDX-CM)
                           MOVE    ORCHC04SUB-H-TENSU (IDX)
                                               TO  ORCHC04SUB-H-TENSU
                                                             (IDX-CM)
                           MOVE    ORCHC04SUB-H-KINGAKU (IDX)
                                               TO  ORCHC04SUB-H-KINGAKU
                                                             (IDX-CM)
                           MOVE    ORCHC04SUB-H-ENKBN (IDX)
                                               TO  ORCHC04SUB-H-ENKBN
                                                             (IDX-CM)
                           MOVE    ORCHC04SUB-ZAIEND (IDX)
                                               TO  ORCHC04SUB-ZAIEND
                                                             (IDX-CM)
                       END-IF
                       IF     (ORCHC04SUB-H-ZAIHEN(IDX)
                                                       =   "＊")
                       AND    (ORCHC04SUB-ZAIEND(IDX)  =   SPACE)
      *                    剤の先頭
                           MOVE    ORCHC04SUB-H-SRYKBN (IDX)
                                               TO  ORCHC04SUB-H-SRYKBN
                                                             (IDX + 1)
                           MOVE    ORCHC04SUB-H-ZAIHEN (IDX)
                                               TO  ORCHC04SUB-H-ZAIHEN
                                                             (IDX + 1)
                       END-IF
                       MOVE    "999999999"     TO ORCHC04SUB-SRYCD (IDX)
                   END-IF
               END-IF
      *
               IF      FLG-OK              =   1
                   MOVE    IDX             TO  IDX-CM
               END-IF
           END-PERFORM
           .
       30050-COMMNT-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ページカウント処理
      *****************************************************************
       200520-PAGE-CNT-SEC                  SECTION.
      *
      *    最終明細の時、改ページなし
           MOVE    LINE-MAX            TO  IDX-H
           MOVE    "次ページへ続く"    TO  TBL-H-SRYMEI
                                               (WRK-PAGE IDX-H)(30:)
           ADD     1                   TO  WRK-PAGE
      *    テーブル最大
           IF      WRK-PAGE            >   PAGE-MAX
      *        帳票印刷
               PERFORM 400-PRINT-SYORI-SEC
           END-IF
      *
           MOVE    1                   TO  IDX-H
      *    ２頁以降の最大
           MOVE    LINE-MAX2           TO  LINE-MAX
           ADD     1                   TO  WRK-PAGECNT
      *
           .
       200520-PAGE-CNT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       400-PRINT-SYORI-SEC          SECTION.
      *
           IF      WRK-PAGE            =   ZERO
               MOVE    1               TO  WRK-PAGE
           END-IF
           IF      WRK-PAGE-MAX        =   ZERO
               MOVE    1               TO  WRK-PAGE-MAX
           END-IF
      *
      *    最終行
           IF      TBL-H-SRYMEI(WRK-PAGE LINE-MAX)
                                       =   SPACE
               IF      PARA-LINE           =   "1"
                   ADD     2               TO  IDX-H
               ELSE
                   ADD     1                   TO  IDX-H
               END-IF
               IF      IDX-H               >   LINE-MAX
                   MOVE    LINE-MAX            TO  IDX-H
               END-IF
               MOVE    "以下余白"          TO  TBL-H-SRYMEI
                                               (WRK-PAGE IDX-H)(30:)
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-PAGE-MAX
      *
      *        総ページ数
               ADD     1                   TO  WRK-PAGEALL
      *
               IF    ( ORCHC03-PRTKBN      =   "1" )
      *
      *            帳票編集処理
                   PERFORM 300-HC03-HEN-SEC
      *
      *            帳票印刷処理
                   PERFORM 401-PRINT-SEC
               ELSE
      *            帳票印刷開始処理
                   PERFORM 400-SYOKI-PRINT-SEC
      *
      *            帳票編集処理
                   PERFORM 300-HC03-HEN-SEC
      *
      *            帳票印刷処理
                   PERFORM 400-PRINT-SEC
               END-IF
           END-PERFORM
      *
           INITIALIZE                  TBL-MEISAI-AREA
           MOVE    1                   TO  WRK-PAGE
           MOVE    ZERO                TO  IDX-H
      *
           .
       400-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   パラメタテーブルからの印刷処理
      *****************************************************************
       203-PARA-SYUNOU-SEC                 SECTION.
      *
           PERFORM 900-PARA-KEY4-SEL-SEC
           MOVE    PARA-DATA-REC       TO  SYUPARA01-AREA
      *
           PERFORM 900-PARA-KEY5-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PARA    NOT =   ZERO )
      *
               MOVE    PARA-DATA-REC       TO  SYUPARA03-AREA
               MOVE    SYUPARA03-SYU-DATA  TO  SYUNOU-REC
      *
               MOVE    SYUPARA03-DENPNUM   TO  WRK-DENPNUM
               MOVE    SYUPARA03-DENPNUM   TO  WRK-HEAD-DENPNUM
               MOVE    SYUPARA03-SEIKYU    TO  WRK-SKYMONEY
               MOVE    SYUPARA03-SEIKYU-TAX-SAI
                                           TO  WRK-SKYMONEY-TAX-SAI
               MOVE    SYUPARA03-NYUKIN    TO  WRK-NYUKIN
               MOVE    SYUPARA03-MISYU     TO  WRK-MISYUMONEY
               IF    ( ORCHC03-MOTOID      =   1 OR 2 OR 3 OR 4 )
                  AND( SYUPARA01-UPDFLG    =   ZERO )
                  AND( PARA-MISYUKBN2      =   ZERO )
                   IF    ( ORCHC03-MATOME-NYUHENKIN-KBN
                                           =   SPACE )
                       MOVE    ZERO        TO  WRK-MISYUMONEY
                   END-IF
               END-IF
      *!!
      *        見出し合計編集処理
               PERFORM 21001-HAED-KEISAN-SEC
      *        見出し編集処理
               PERFORM 2100-HAED-HENSYU-SEC
      *        複数診療科名編集
               IF    ( PARA-SRYKA          =   "0" )
                   MOVE    SPACE           TO  WRK-HEAD-SRYKAMEI
               ELSE
                   MOVE    SYUPARA03-SRYKAMEI
                                           TO  WRK-HEAD-SRYKAMEI
               END-IF
      *        保険組合せ
               IF    ( SYUPARA03-HKNCOMBI-MAX  >   1 )
               AND   ( PARA-HKNMEI     NOT =   "0" )
                   MOVE    "複数保険使用"      TO  WRK-HEAD-HKNCOMBIMEI
                   MOVE    SPACE               TO  WRK-FUTAN
                   MOVE    SPACE               TO  WRK-FUTAN-MEI
               END-IF
      *        複数伝票
               IF     (SYUPARA03-GRP-DENPNUM-MAX   >   1 )
               OR     (FLG-SYORI               =   2     )
                   MOVE    1                   TO  FLG-DOUJI
               ELSE
                   MOVE    ZERO                TO  FLG-DOUJI
               END-IF
      *
               INITIALIZE                  TBL-MEISAI-AREA
               MOVE    1                   TO  WRK-PAGE
               MOVE    1                   TO  WRK-PAGECNT
               MOVE    ZERO                TO  WRK-PAGEALL
      *        １枚目
               MOVE    LINE-MAX1           TO  LINE-MAX
               MOVE    ZERO                TO  WRK-KEY-PCNT
               MOVE    ZERO                TO  IDX-H
      *
               PERFORM VARYING IDX-SYU2    FROM    1   BY  1
                       UNTIL  (IDX-SYU2    >
                                           SYUPARA03-GRP-DENPNUM-MAX)
                   IF     (SYUPARA03-GRP-DENPNUM (IDX-SYU2)
                                           NOT =   ZERO )
                       MOVE    SYUPARA03-GRP-DENPNUM (IDX-SYU2)
                                               TO  WRK-DENPNUM
      *                収納マスタ検索
                       PERFORM 2011-SYUNOU-KEN-SEC
                       PERFORM 2031-SRYYMD-HEN-SEC
      *                伝票番号毎の印刷
                       PERFORM 2100-DENPNUM-SYORI-SEC
                   END-IF
               END-PERFORM
      *
      *??      IF      IDX-H               >   ZERO
      *            印刷処理
                   PERFORM 400-PRINT-SYORI-SEC
      *??      END-IF
      *
               PERFORM 900-PARA-KEY5-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       203-PARA-SYUNOU-EXT.
           EXIT.
      *****************************************************************
      *    診療日編集処理
      *****************************************************************
       2031-SRYYMD-HEN-SEC             SECTION.
      *
           MOVE    SYU-SRYYMD          TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(17:6)   TO  WRK-DAY-H2
           MOVE    "〔"                TO  WRK-DAY-H1
           MOVE    "〕"                TO  WRK-DAY-H3
      *
           .
       2031-SRYYMD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    複数診療科編集処理
      *****************************************************************
       2021-GRP-SRYKAMEI-HEN-SEC             SECTION.
      *
           PERFORM VARYING     IDH3    FROM    1   BY  1
                   UNTIL       IDH3    >   10
               IF      ORCHC03SUB-DOUJI-SRYKA (IDX-SYU IDH3)
                                       NOT =   SPACE
                   MOVE    ORCHC03SUB-DOUJI-SRYKA(IDX-SYU IDH3)
                                           TO  WRK-SRYKA
                   PERFORM 1000-SRYKAMEI-HEN-SEC
               END-IF
           END-PERFORM
           .
       2021-GRP-SRYKAMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角全角変換処理
      *****************************************************************
       800-HENKAN-SEC             SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
      *****MOVE    WRK-Z72             TO  KANACHK-MAE-INPUT
           IF      WRK-Z72             <   ZERO
               MOVE    WRK-Z72             TO  WRK-Z72-HS
               MOVE    WRK-Z72-HS          TO  KANACHK-MAE-INPUT
           ELSE
               MOVE    WRK-Z72             TO  WRK-Z72-H
               MOVE    WRK-Z72-H           TO  KANACHK-MAE-INPUT
           END-IF
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT(1:18)    TO  WRK-Z72-G
           END-IF
           .
       800-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角全角変換処理（ゼロ表示用）
      *****************************************************************
       810-HENKAN-SEC             SECTION.
     *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           IF      WRK-Z72             <   ZERO
               MOVE    WRK-Z72             TO  WRK-Z72-HS
               MOVE    WRK-Z72-HS          TO  KANACHK-MAE-INPUT
           ELSE
               MOVE    WRK-Z72             TO  WRK-Z72-H9
               MOVE    WRK-Z72-H9          TO  KANACHK-MAE-INPUT
           END-IF
      *    左ずめ
           IF      FLG-HENPARA         =   1
               MOVE    KANACHK-MAE-INPUT   TO  WRK-HENKAN-G
               MOVE    SPACE               TO  KANACHK-MAE-INPUT
               PERFORM VARYING         IDX3    FROM    1   BY  1
                       UNTIL           IDX3    >   10
                   IF      WRK-HENKAN-G (IDX3:1)   NOT =   SPACE
                       MOVE    WRK-HENKAN-G (IDX3:)
                                               TO  KANACHK-MAE-INPUT
                       MOVE    10              TO  IDX3
                   END-IF
               END-PERFORM
           END-IF
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT(1:18)    TO  WRK-Z72-G
           END-IF
      *
           MOVE    ZERO                TO  FLG-HENPARA
           .
       810-HENKAN-EXT.
           EXIT.
      *****************************************************************
      *    診療日編集処理
      *****************************************************************
       3101-SRYYMD-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYYMDG
                                           WRK-LBLSRYYMD
           MOVE    1                   TO  FLG-SYORI
      *
           IF    ( ORCHC03-HAKKOFLG    =   4 )
               IF    ( SYUPARA03-KIKANKBN  =   "1" )
                   MOVE    "診療日"        TO  WRK-LBLSRYYMD
                   PERFORM 31012-SRYYMD-HEN-SEC
               ELSE
                   MOVE    "診療年月"      TO  WRK-LBLSRYYMD
                   MOVE    SYU-SRYYMD      TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG  (1:16)
                                           TO  WRK-SRYYMDG
      *
                   MOVE    2                   TO  FLG-SYORI
               END-IF
           ELSE
               IF    ( ORCHC03-PARAFLG         =   1 )
                AND  ( SYUPARA03-STSRYYMD  NOT =   SYUPARA03-EDSRYYMD )
                   MOVE    "診療日"        TO  WRK-LBLSRYYMD
                   PERFORM 31012-SRYYMD-HEN-SEC
               ELSE
                   MOVE    "診療日"        TO  WRK-LBLSRYYMD
                   MOVE    SYU-SRYYMD      TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG     TO  WRK-SRYYMDG
               END-IF
           END-IF
      *
           .
       3101-SRYYMD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療日編集処理
      *****************************************************************
       31012-SRYYMD-HEN-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYYMDG
      *
           MOVE    SYUPARA03-STSRYYMD  TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-STSRYYMDG
      *
           MOVE    SYUPARA03-EDSRYYMD  TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-EDSRYYMDG
      *
           STRING  WRK-STSRYYMDG       DELIMITED   BY  SIZE
                   "〜"                DELIMITED   BY  SIZE
                   WRK-EDSRYYMDG       DELIMITED   BY  SIZE
           INTO    WRK-SRYYMDG
           END-STRING
      *
           MOVE    2                   TO  FLG-SYORI
           .
       31012-SRYYMD-HEN-EXT.
           EXIT.
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
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
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
           .
       800-HIZUKE-CHK-EXT.
           EXIT.
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
           MOVE    SPA-SRYYMD      TO  SYS-1900-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1900-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(10) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
               END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
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
           MOVE    SYS-1900-PRTKBN(10)
                                   TO  SYS-1901-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1901-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-HOSPNAME(1:50)
               MOVE    SYS-1901-HOSPNAME2  TO  WRK-HOSPNAME(51:50)
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
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
           MOVE    SYS-1900-PRTKBN(10)
                                   TO  SYS-1902-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1902-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1  TO  WRK-HOSPADRS(1:50)
               MOVE    SYS-1902-ADRS2  TO  WRK-HOSPADRS(51:50)
               MOVE    SYS-1902-TEL    TO  WRK-HOSPTEL
           ELSE
               INITIALIZE                  SYS-1902-REC
           END-IF
           .
       900-1902-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ主キー読み込み処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEC         SECTION.
      *
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-KEY10-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-KANRIMST-READ-EXT.
           EXIT.
      *
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
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTINF-REC
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    パラメタテーブル検索
      *****************************************************************
       900-PARA-KEY5-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           INITIALIZE                      PARA-REC
           MOVE    ORCHC03-PARA-GYOUMUID
                                       TO  PARA-GYOUMUID
           MOVE    ORCHC03-PARA-TERMID
                                       TO  PARA-TERMID
           MOVE    ORCHC03-PARA-FILEMEI
                                       TO  PARA-FILEMEI
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-PARA-KEY5-FET-SEC
           ELSE
               MOVE    1               TO  FLG-PARA
               INITIALIZE                  PARA-REC
           END-IF
      *
           .
       900-PARA-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    パラメタテーブルFETCH
      *****************************************************************
       900-PARA-KEY5-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PARA-REC
           ELSE
               MOVE    1               TO  FLG-PARA
               INITIALIZE                  PARA-REC
           END-IF
      *
           .
       900-PARA-KEY5-FET-EXT.
           EXIT.
      *****************************************************************
      *    パラメタテーブル検索処理
      *****************************************************************
       900-PARA-KEY4-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    ORCHC03-PARA-GYOUMUID
                                       TO  PARA-GYOUMUID
           MOVE    ORCHC03-PARA-TERMID TO  PARA-TERMID
           MOVE    ORCHC03-PARA-FILEMEI TO WRK-FILEMEI
           MOVE    "00101"             TO  WRK-FILEMEI-CD
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
           MOVE    1                   TO  PARA-EDANUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_para"      TO  MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  PARA-REC
               ELSE
                   MOVE    1           TO  FLG-PARA
                   INITIALIZE              PARA-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PARA
               INITIALIZE                  PARA-REC
           END-IF
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
      *
       900-PARA-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       980-SYUNOU-READ-EXT.
           EXIT.
      *
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
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *
