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
       PROGRAM-ID.             A00000H03A5.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 外来請求書兼領収書　印刷（ＨＣＭ０３Ｖ０２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/02/23    @@@@−半田   新規作成(jma-receipt-hosp 2.1.0 カスタマイズ)
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者       日付      内容
      *  01.00.01    @@@@-門間    11/01/07  コメント履歴削除
      *                                     open-cobol1.0対応
      *  01.00.02    @@@@-門間    11/05/26  主保険＋マル長の請求対応
      *  01.00.03    @@@@-門間    11/09/12  複数科保険の本家区分の印字修正
      *  01.00.04    @@@@-門間    14/01/17  第三者行為対応
      *  01.00.05    @@@@-門間    14/02/07  プログラムオプション対応（訂正印字）
      *  01.00.06    @@@@-門間    14/03/24  消費税対応
      *  01.00.07    @@@@-門間    14/08/13  主保険＋マル長の請求対応の不具合対応
      *  01.00.08    @@@@-門間    14/10/07  右下枠内の消費税(再掲)を修正
      *  02.00.01    @@@@-門間    14/10/31  ver4.8.0対応（一時ディレクトリ変更）
      *  02.00.02    @@@@-門間    17/03/15  負担割合４桁へ修正
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
           COPY    "A00000H03A5.INC"  REPLACING   //HCM03V02//
                                      BY          //HC03//.
      *
       WORKING-STORAGE             SECTION.
      *
      *     印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
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
           03  IDH3                PIC 9(04).
      *
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
           03  IDXP                PIC 9(02).
           03  IDXM                PIC 9(02).
           03  IDK                 PIC 9(04).
           03  IDX2-1              PIC 9(01).
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
      *
           03  WRK-SKYMONEY        PIC S9(11).
           03  WRK-MISYUMONEY      PIC S9(11).
           03  WRK-MISYU           PIC S9(11).
           03  WRK-GOKTEN          PIC S9(11).
           03  WRK-SKYMONEY-TAX-SAI    PIC S9(11).
      *
           03  WRK-DENPNUM         PIC 9(07).
           03  WRK-SRYKA           PIC X(02).
           03  WRK-HKNCOMBI        PIC X(04).
      *
           03  WRK-WARI            PIC 9(02)V9.
           03  WRK-Z6              PIC ZZZZ.
           03  WRK-Z62             PIC Z9.9.
      *
           03  WRK-ZZZ             PIC ZZZ.
           03  WRK-NO              PIC Z(7).
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
      *    自費集計
           03  WRK-JIHI-REC.
               05  WRK-JIHI            PIC S9(07)   OCCURS  10.
           03  WRK-JIHI-TOTAL          PIC S9(08).
           03  WRK-JIHIMEI-REC.
               05  WRK-JIHIMEI         PIC X(20)   OCCURS  10.
      *
      *    複数科まとめ集計
           03  WRK-HKNTEN-KIN              PIC 9(07).
           03  WRK-HKNTEN-KEI              PIC 9(07)V99.
           03  WRK-NYUKIN                  PIC S9(07).
           03  WRK-NYUKIN-TOTAL            PIC S9(07).
           03  WRK-ZENMISYU                PIC S9(07).
      *
           03  WRK-SRYKA-MEI               PIC X(40).
           03  WRK-PAGE                    PIC 9(03).
      *
      *    文字変換
           03  WRK-ZENKAKU-G.
               05  WRK-ZENKAKU         PIC X(02)   OCCURS   10.
           03  WRK-MOJI                PIC X(10).
           03  WRK-MOJISU              PIC 9(02).
           03  WRK-LBLSRYYMD           PIC X(10).
           03  WRK-SRYYMDG             PIC X(50).
           03  WRK-STSRYYMDG           PIC X(22).
           03  WRK-EDSRYYMDG           PIC X(22).
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
           03  WRK-MSG                 PIC X(1000).
           03  WRK-FORMID              PIC X(30).
           03  WRK-HONKZKKBN           PIC X(04).
           03  WRK-Z73-G.
               05  WRK-FIL3            PIC X(14).
           03  WRK-Z72-H2              PIC ZZZ,ZZZ.
           03  WRK-Z72-HS2             REDEFINES  WRK-Z72-H2
                                       PIC ---,---.
      *
       01  CONST-AREA.
           03  CONST-H200401           PIC X(08)   VALUE "20080401".
      *
      *    日本語数字
       01  TBL-SUJI-AREA.
           03  FILLER          PIC X(20)   VALUE
              "１２３４５６７８９０".
       01  TBL-SUJI-R          REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI        PIC X(02)   OCCURS   10.
      *
      *    プログラムオプションパラメタ
       01  TPARA-AREA.
           03  TPARA-TEISEIKBN          PIC X(01).
           03  TPARA-CHUI-YUKOSTYMD     PIC X(08).
           03  TPARA-CHUI-YUKOEDYMD     PIC X(08).
           03  TPARA-CHUI               PIC X(100).
      *
           03  WRK-PARA-NAME            PIC X(25).
           03  WRK-PARA                 PIC X(100).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
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
      *    点数マスタ
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    保存用収納ワーク
       01  WRK-SYUNOU-REC.
           COPY    "CPSYUNOU.INC"  REPLACING
                                     //SYU-// BY //WRK-SYU-//.
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
           COPY    "CPSYUPARA03.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
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
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
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
                                       HKNCOMBI-REC
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    各ＤＢ読込
           PERFORM 100-SYORI-SEC
      *
           IF  (ORCHC03-PARAFLG  =  1)
               PERFORM  203-PARA-SYUNOU-SEC
           ELSE
      *        発行区分による
             IF  ORCHC03-HAKHOUFLG  =  ZERO
      *        対象の伝票番号分のみ
               PERFORM 201-ONLY-SYUNOU-SEC
             ELSE
      *        発行区分による
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
           ADD     1                   TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC01PARA-CNT
      *
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
              WRITE  HC03
      *
            CLOSE                       PRT-FILE
      *
            INITIALIZE                 ORCSMKPRTAREA
            MOVE   "HC01.sh"           TO  MKPRT-ID
            MOVE   WRK-FORMID          TO  MKPRT-DIA
            MOVE   SPACE               TO  MKPRT-DEF
            MOVE   ASGNPARA            TO  MKPRT-DAT
            MOVE   SYS-1031-PRTNM(3)   TO  MKPRT-PRTNM
            MOVE  "請求書兼領収書"     TO MKPRT-INFO
            CALL   "ORCSMKPRTSITE1"     USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       400-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理（印刷ＤＢに出力）
      *****************************************************************
       401-PRINT-SEC                SECTION.
      *
           MOVE    ORCHC03-PRTAREA     TO  ORCSPRTAREA
           MOVE    WRK-FORMID          TO  SPRT-PRTID
           MOVE    HC03                TO  SPRT-PRTDATA
           MOVE    "2"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF  (SPRT-RETURN  =  ZERO)
               CONTINUE
           ELSE
               DISPLAY  "ORCSPRT ERR: PTID =  " SPRT-PTID
           END-IF
      *
           .
       401-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    対象の伝票番号分のみ処理
      *****************************************************************
       201-ONLY-SYUNOU-SEC                SECTION.
      *
      *    収納マスタ検索
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    ORCHC03-DENPNUM     TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
      *    診療科名編集
           MOVE    SPACE                TO  WRK-SRYKA-MEI
           MOVE    SYU-SRYKA            TO  WRK-SRYKA
           PERFORM 1000-SRYKAMEI-HEN-SEC
      *    保険組合せ
           PERFORM 1001-HKNCOMBI-HEN-SEC
      *
           MOVE    ORCHC03-DENPNUM     TO  WRK-DENPNUM
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
      *
           IF     (ORCHC03-TEIKIN-KBN  =   1   )  AND
                  (ORCHC03-HAKKOFLG    =   2   )
               MOVE    SYU-SKYMONEY        TO  WRK-SKYMONEY
               MOVE    SYU-NYUKIN-TOTAL    TO  WRK-NYUKIN
               MOVE    SYU-SKYMONEY-TAX-SAI
                                           TO  WRK-SKYMONEY-TAX-SAI
           END-IF
      *    帳票印刷開始処理
           PERFORM 400-SYOKI-PRINT-SEC
      *
      *    帳票編集処理
           PERFORM 300-HC03-HEN-SEC
      *
      *    帳票印刷処理
           PERFORM 400-PRINT-SEC
      *
           .
       201-ONLY-SYUNOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       100-SYORI-SEC                SECTION.
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SPA-SYSYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1001-EDYUKYMD
      *
      *    システム管理検索
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1001-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
            MOVE    "tbl_syskanri"       TO  MCP-TABLE
            MOVE    "key10"                TO  MCP-PATHNAME
            CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                                   SPA-AREA
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1002-REC
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
           MOVE    SPA-SYSYMD          TO  SYS-1002-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1002-EDYUKYMD
      *
      *    システム管理検索
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
            MOVE    "tbl_syskanri"     TO  MCP-TABLE
            MOVE    "key10"              TO  MCP-PATHNAME
            CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
            MOVE    "tbl_syskanri"         TO  MCP-TABLE
            MOVE    "key10"                TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1002-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1002-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
            MOVE    "tbl_syskanri"         TO  MCP-TABLE
            MOVE    "key10"                TO  MCP-PATHNAME
            CALL    "ORCDBMAIN"           USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
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
      *    労災医療機関情報編集
           MOVE    SPACE               TO  SYS-4001-REC
           INITIALIZE                  SYS-4001-REC
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-4001-HOSPNUM
           MOVE    SPA-SYSYMD          TO  SYS-4001-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-4001-EDYUKYMD
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"       TO  MCP-TABLE
               MOVE    "key10"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-4001-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-4001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
      *    自費名称編集
           PERFORM 1002-JIHIMSG-HEN-SEC
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
           PERFORM 100-OPTION-SELECT-SEC
      *
           PERFORM 800-MCNDATE-SEC
      *
           .
       100-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費名称編集処理
      *****************************************************************
       1002-JIHIMSG-HEN-SEC             SECTION.
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1013-REC
           MOVE     SPA-HOSPNUM        TO SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-1013-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1013-EDYUKYMD
      *
      *    システム管理検索
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
            MOVE    "tbl_syskanri"      TO  MCP-TABLE
            MOVE    "key2"              TO  MCP-PATHNAME
            CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
            MOVE    "tbl_syskanri"      TO  MCP-TABLE
            MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               INITIALIZE                  SYS-1013-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           INITIALIZE                  WRK-JIHIMEI-REC
           PERFORM
               UNTIL       FLG-SYSKANRI    =   1
               MOVE    MCPDATA-REC         TO  SYS-1013-REC
               IF      SPA-NYUGAIKBN       =   SYS-1013-KBNCD(1:1)
                   EVALUATE    SYS-1013-KBNCD(2:1)
                       WHEN    "1"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (1)
                       WHEN    "2"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (2)
                       WHEN    "3"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (3)
                       WHEN    "4"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (4)
                       WHEN    "5"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (5)
                       WHEN    "6"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (6)
                       WHEN    "7"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (7)
                       WHEN    "8"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (8)
                       WHEN    "9"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (9)
                       WHEN    "0"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                   (10)
                   END-EVALUATE
               END-IF
      *
            MOVE    "tbl_syskanri"      TO  MCP-TABLE
            MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
            MOVE    "tbl_syskanri"      TO  MCP-TABLE
            MOVE    "key2"              TO  MCP-PATHNAME
            CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       1002-JIHIMSG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           INITIALIZE                  TPARA-AREA
           MOVE    "1"             TO  TPARA-TEISEIKBN
           MOVE    "00000000"      TO  TPARA-CHUI-YUKOSTYMD
           MOVE    "99999999"      TO  TPARA-CHUI-YUKOEDYMD
      *
      *    （訂正）表示
           MOVE    "TEISEIKBN"     TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA        =   "0" OR "1" )
               MOVE    WRK-PARA    TO  TPARA-TEISEIKBN
           END-IF
      *
      *    注意書き有効開始日
           MOVE    "CHUI_YUKOSTYMD" TO WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA        TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           IF    ( FLG-HIZUKECHK   =   ZERO )
               MOVE    WRK-SYMD    TO  TPARA-CHUI-YUKOSTYMD
           END-IF
      *
      *    注意書き有効終了日
           MOVE    "CHUI_YUKOEDYMD" TO WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA        TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           IF    ( FLG-HIZUKECHK   =   ZERO )
               MOVE    WRK-SYMD    TO  TPARA-CHUI-YUKOEDYMD
           END-IF
      *
      *    注意書き
           MOVE    "CHUI"          TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA        TO  TPARA-CHUI
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
      *
           INITIALIZE                      ORCSPRGOPTIONAREA
           MOVE    "A00000H03"         TO  ORCSPRGOPTION-PRGID
           MOVE    WRK-PARA-NAME       TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
      *
           EVALUATE    WRK-PARA-NAME
           WHEN    "CHUI"
               MOVE    ORCSPRGOPTION-OT-PARA (1:100)
                                       TO  KANACHK-MAE-INPUT
           WHEN    "CHUI_YUKOSTYMD"
           WHEN    "CHUI_YUKOEDYMD"
               MOVE    ORCSPRGOPTION-OT-PARA(1:10)
                                       TO  KANACHK-MAE-INPUT
           WHEN    OTHER
               MOVE    ORCSPRGOPTION-OT-PARA
                                       TO  KANACHK-MAE-INPUT
           END-EVALUATE
      *
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-PARA
      *
           .
       1001-OPTION-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       1001-HKNCOMBI-HEN-SEC             SECTION.
      *
      *    保険組合せマスター検索
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM          TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
           PERFORM 910-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   1
                DISPLAY "*** HKNCOMBI  ERR:"   COMB-KEY "."
           END-IF
           PERFORM 910-PTHKNINF-READ-SEC
      *
           IF    ( COMB-HKNNUM         =   "971" OR "973" )
      *
      *        患者労災保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPNUM          TO  PTRSI-HOSPNUM
               MOVE    SPA-PTID            TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               PERFORM 910-PTRSIINF-READ-SEC
           ELSE
               INITIALIZE                      PTRSIINF-REC
           END-IF
           PERFORM 10011-HKNCOMBIMEI-HEN-SEC
      *
           .
       1001-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       10011-HKNCOMBIMEI-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
      *
           IF  ( COMB-HKNNUM   =  "971" )
           AND ( PTRSI-HKNKBN  =  "5"   )
               MOVE    PTHKN-KIGO     TO   WRK-HKNCOMBIMEI
           ELSE
               IF    ( SYU-SRYYMD      >=     "20081001" )
                 AND  ( COMB-HKNNUM      =      "001" )
                    INSPECT COMB-SYU-TANSEIDONAME
                           REPLACING    ALL    "政管"   BY  "協会"
               END-IF
               STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                       "　"                    DELIMITED  BY  SIZE
                       COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       "　"                    DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       "　"                    DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       "　"                    DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                             INTO              WRK-HKNCOMBIMEI
               END-STRING
           END-IF
      *
           .
       10011-HKNCOMBIMEI-HEN-EXT.
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
           MOVE     SPA-HOSPNUM        TO  SYS-1005-HOSPNUM
           MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
      *
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"       TO  MCP-TABLE
               MOVE    "key10"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
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
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
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
           INITIALIZE                  HC03
      *
      *    伝票番号
           MOVE    WRK-DENPNUM         TO  WRK-NO  
           MOVE    WRK-NO              TO  HC03-NO
      *    診療科
           MOVE    WRK-SRYKA-MEI(1:40) TO  HC03-MIDASI
      *    患者番号
           MOVE    SPA-PTNUM           TO  HC03-KNJNO
      *    患者カナ名
           INITIALIZE   PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF   FLG-PTINF   =  ZERO
             MOVE    PTINF-KANANAME      TO  HC03-KNJKANANAME
           END-IF
      *    患者名称
           MOVE    SPA-NAME            TO  HC03-KNJNAMW
      *    保険種別
           MOVE    WRK-HKNCOMBIMEI     TO  HC03-HOKEN
      *    負担割合
           IF      SYU-PTFTNRATE       >   ZERO
               COMPUTE WRK-WARI        =   SYU-PTFTNRATE   /   10
               IF      SYU-PTFTNRATE(3:1)  =   ZERO
                   MOVE    WRK-WARI            TO  WRK-Z6
                   MOVE    WRK-Z6              TO  HC03-FUTAN2
               ELSE
                   MOVE    WRK-WARI            TO  WRK-Z62
                   MOVE    WRK-Z62             TO  HC03-FUTAN2
               END-IF
           END-IF
      *    生年月日
           MOVE    PTINF-BIRTHDAY         TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:22)      TO  HC03-TANJYOYMD(1:22)
           MOVE   "生"   TO     HC03-TANJYOYMD(23:2)
      *    性別
           EVALUATE    PTINF-SEX
             WHEN    "1"
               MOVE    "男"           TO  HC03-SEX
             WHEN    "2"
               MOVE    "女"           TO  HC03-SEX
           END-EVALUATE
      *    診療日
           PERFORM 3101-SRYYMD-HEN-SEC
           MOVE    WRK-LBLSRYYMD       TO  HC03-LBLSRYYMD
           MOVE    WRK-SRYYMDG         TO  HC03-SRYYMD
      *    発行日
           MOVE    SYU-DENPPRTYMD      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HC03-HAKYMD
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS        TO  HC03-IRYOADRS
           INSPECT HC03-IRYOADRS       REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  HC03-IRYOMEI
           INSPECT HC03-IRYOMEI        REPLACING ALL "  " BY "　"
      *    医療機関ＴＥＬ
           MOVE    WRK-HOSPTEL         TO  HC03-IRYOTEL
      *
      *    自費名称編集
           PERFORM 3102-JIHIMSG-HEN-SEC
      *
           MOVE    SPACE               TO  WRK-Z7-G
           MOVE    SPACE               TO  WRK-Z72-G
      *    診療科
           MOVE    SYU-HKNTEN (01)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO1
      *    指導科
           MOVE    SYU-HKNTEN (02)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO2
      *    在宅科
           MOVE    SYU-HKNTEN (03)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO3
      *    投薬科
           MOVE    SYU-HKNTEN (04)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO4
      *    注射科
           MOVE    SYU-HKNTEN (05)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO5
      *    処置科
           MOVE    SYU-HKNTEN (06)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO6
      *    手術科
           MOVE    SYU-HKNTEN (07)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO7
      *    麻酔
           MOVE    SYU-HKNTEN (08)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO8
      *    検査科
           MOVE    SYU-HKNTEN (09)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G          TO  HC03-HKNRYO9
      *    Ｘ線科
           MOVE    SYU-HKNTEN (10)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO10
      *    リハビリ
           MOVE    SYU-HKNTEN (11)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO11
      *    精神科専門
           MOVE    SYU-HKNTEN (12)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO12
      *    放射線治療
           MOVE    SYU-HKNTEN (13)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO13
      *    病理診断
           MOVE    SYU-HKNTEN (14)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO14
      *    入院料
           MOVE    SYU-HKNTEN (15)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO15
      *    療養担当手当て
           MOVE    SYU-HKNTEN (16)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO16
      *    見出し
           PERFORM  3001-MEISAI-HEN-SEC
      *    合計点数
           MOVE    SYU-TOTAL-HKNTEN    TO  WRK-GOKTEN
           MOVE    WRK-GOKTEN          TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-GOKTEN
      *
           IF    ( COMB-HKNNUM             =    "971"
           AND     PTRSI-HKNKBN        NOT =    "5"
           AND     SYS-4001-RSIIRYOKBN     =    "1" )
           OR    ( COMB-HKNNUM             =    "971"
           AND     PTRSI-HKNKBN            =    "5"
           AND     SYU-PTFTNRATE           =    ZERO )
           OR    ( COMB-HKNNUM             =    "973"
           AND     SYU-PTFTNRATE           =    ZERO )
               MOVE    SPACE               TO  HC03-HKNRYO1
               MOVE    SPACE               TO  HC03-HKNRYO2
               MOVE    SPACE               TO  HC03-HKNRYO3
               MOVE    SPACE               TO  HC03-HKNRYO4
               MOVE    SPACE               TO  HC03-HKNRYO5
               MOVE    SPACE               TO  HC03-HKNRYO6
               MOVE    SPACE               TO  HC03-HKNRYO7
               MOVE    SPACE               TO  HC03-HKNRYO8
               MOVE    SPACE               TO  HC03-HKNRYO9
               MOVE    SPACE               TO  HC03-HKNRYO10
               MOVE    SPACE               TO  HC03-HKNRYO11
               MOVE    SPACE               TO  HC03-HKNRYO12
               MOVE    SPACE               TO  HC03-HKNRYO13
               MOVE    SPACE               TO  HC03-HKNRYO14
               MOVE    SPACE               TO  HC03-HKNRYO15
               MOVE    SPACE               TO  HC03-HKNRYO16
               MOVE    ALL "*"             TO  HC03-GOKTEN
           END-IF
      *
      *    本人家族区分
           EVALUATE  COMB-HONKZKKBN
             WHEN  1
               MOVE    "本人"   TO    HC03-HONKZKKBN
             WHEN  2
               MOVE    "家族"   TO    HC03-HONKZKKBN
             WHEN  OTHER
               MOVE    SPACE    TO    HC03-HONKZKKBN
           END-EVALUATE 
      *    労災・自賠の診察料等の記載
           IF   (SYU-RSI-TOTAL     NOT =   ZERO) AND
                (SYS-4001-RSIPRTKBN    =   "2")
             EVALUATE    COMB-HKNNUM
               WHEN    "971"
                 MOVE  "内労災診察等"     TO      HC03-RSIMIDASI
                 MOVE  "（"               TO      HC03-RSIMIDASI2
                 MOVE  "）"               TO      HC03-RSIMIDASI3
               WHEN    "973"
                 MOVE  "内自賠診察等"     TO      HC03-RSIMIDASI
                 MOVE  "（"               TO      HC03-RSIMIDASI2
                 MOVE  "）"               TO      HC03-RSIMIDASI3
               WHEN    OTHER
                 IF    ( COMB-KOHHKNNUM (1)   =   "970" )
                   MOVE  "内自賠文書等"     TO      HC03-RSIMIDASI
                   MOVE  "（"               TO      HC03-RSIMIDASI2
                   MOVE  "）"               TO      HC03-RSIMIDASI3
                 END-IF
             END-EVALUATE
             MOVE    SYU-RSI-TOTAL        TO      WRK-Z72
             PERFORM 820-HENKAN-SEC
             MOVE    WRK-Z72-G            TO      HC03-RSIBUN
             MOVE    "円"                 TO      HC03-RSIEN
           ELSE
             MOVE    SPACE              TO      HC03-RSIMIDASI
             MOVE    SPACE              TO      HC03-RSIMIDASI2
             MOVE    SPACE              TO      HC03-RSIMIDASI3
             MOVE    SPACE              TO      HC03-RSIBUN
             MOVE    SPACE              TO      HC03-RSIEN
           END-IF
      *
      *    保険適用分
           MOVE    SYU-HKNTEKMONEY     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HOKENBUN
      *
      *    保険適用外編集
           PERFORM     VARYING   IDX-JIHI FROM  1  BY  1
                       UNTIL     IDX-JIHI  >  16
               MOVE   ZERO      TO    WRK-Z72
               COMPUTE   WRK-Z72   =   SYU-TGMONEY(IDX-JIHI)  +
                                       SYU-TGMONEY-TAX(IDX-JIHI)
               PERFORM   800-HENKAN-SEC
               MOVE    WRK-Z72-G           TO  HC03-JIHIBUN(IDX-JIHI)
           END-PERFORM
      *
      *    保険適用外消費税（再掲）
           MOVE    SYU-TGMONEY-TAX-SAI     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G               TO  HC03-HOKENZEI
      *
           IF    ( COMB-HKNNUM             =    "971"
                                           OR   "973" )
             OR  ( COMB-KOHHKNNUM (1)      =    "970"
              AND  SYU-PTFTNRATE       NOT =     ZERO )
               IF    ( COMB-HKNNUM             =    "971"
               AND     PTRSI-HKNKBN        NOT =    "5"
               AND     SYS-4001-RSIIRYOKBN     =    "1" )
               OR    ( COMB-HKNNUM             =    "971"
               AND     PTRSI-HKNKBN            =    "5"
               AND     SYU-PTFTNRATE           =    ZERO )
               OR    ( COMB-HKNNUM             =    "973"
               AND     SYU-PTFTNRATE           =    ZERO )
                   MOVE    SPACE           TO  HC03-HOKENBUN
           ELSE
      *
                   COMPUTE WRK-GOKTEN      =    SYU-HKNTEKMONEY
                                           +    SYU-RSISHOSHIN-MONEY
                                           +    SYU-RSISAISHIN-MONEY
                                           +    SYU-RSISDO-MONEY
                                           +    SYU-RSIETC-MONEY
                                           +    SYU-KOH-FTN
                   MOVE    WRK-GOKTEN      TO  WRK-Z72
                   PERFORM 800-HENKAN-SEC
                   MOVE    WRK-Z72-G           TO  HC03-HOKENBUN
               END-IF
           END-IF
      *
      *    保険適用外分
           MOVE    ZERO                TO  WRK-GOKTEN
           COMPUTE WRK-GOKTEN          =   SYU-TOTAL-TGMONEY
                                       +   SYU-TOTAL-TGMONEY-TAX
           MOVE    WRK-GOKTEN          TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HOKENGAI
      *    自費１から１０
           MOVE    ZERO                TO  WRK-JIHI-REC
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >  10 
               COMPUTE WRK-JIHI (IDX)      =   SYU-JIHI-TAXNASI (IDX)
                                           +   SYU-JIHI-TAXARI  (IDX)
               MOVE    WRK-JIHI (IDX)  TO  WRK-Z72
               PERFORM 800-HENKAN-SEC
               MOVE    WRK-Z72-G       TO  HC03-JIHI(IDX)
           END-PERFORM
      *
      *    自費計
           COMPUTE WRK-JIHI-TOTAL      =   SYU-JIHI-TOTAL
                                       +   SYU-JIHI-TOTAL-TAX
      *    自費消費税
           MOVE    SYU-JIHI-TAX        TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-JIHI(12)
      *    自費合計
           IF      SYU-SRYYMD          <=  "20070331"
             COMPUTE WRK-GOKTEN        =   WRK-JIHI-TOTAL
                                       +   SYU-JIHI-TAX
           ELSE
             MOVE    WRK-JIHI-TOTAL    TO  WRK-GOKTEN
           END-IF
           MOVE    WRK-GOKTEN          TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-JIHI(11)
      *    薬剤一部負担
           MOVE    SYU-YKZ-FTN         TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-YAKUZAI
      *    老人一部負担金
           MOVE    SYU-RJN-FTN         TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-ROUJIN
      *    公費一部負担金
           MOVE    SYU-KOH-FTN         TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-KOUHI
      *    調整金
           MOVE    SYU-CHOSEI          TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-CHOSEI
      *    今回請求額
           MOVE    WRK-SKYMONEY        TO  WRK-Z72
           PERFORM 810-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-KONSEIKYU
      *    今回請求額消費税（再掲）
           MOVE    WRK-SKYMONEY-TAX-SAI    TO  WRK-Z72
           PERFORM 810-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-SEIKYUZEI
      *
      *    今回入金額
           MOVE    WRK-NYUKIN          TO  WRK-Z72
           PERFORM 810-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-KONNYUKIN
      *    前回未収額計算
           MOVE    WRK-MISYUMONEY      TO  WRK-Z72
           PERFORM 810-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-ZENMISYU
      *    合計請求額
           COMPUTE WRK-GOKTEN          =   WRK-SKYMONEY
                                       +   WRK-MISYUMONEY
           MOVE    WRK-GOKTEN          TO  WRK-Z72
           PERFORM 810-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-GOUKEIGAK
      *
      *    発行区分
           EVALUATE    ORCHC03-HAKKOFLG
               WHEN    2
                   IF    ( TPARA-TEISEIKBN =   "1" )
                       MOVE    "（訂正）"  TO  HC03-MIDASHI
                   END-IF
           END-EVALUATE
      *
      *    注意書き
           IF    ( SMCNDATE-YMD        >=  TPARA-CHUI-YUKOSTYMD )
            AND  ( SMCNDATE-YMD        <=  TPARA-CHUI-YUKOEDYMD )
               MOVE    TPARA-CHUI      TO  HC03-CHUI
           END-IF
      *
           IF    ( SYU-SRYYMD              >=  CONST-H200401 )
               MOVE    "A00000H03A5N.red"       TO  WRK-FORMID
           ELSE
               MOVE    "A00000H03A5O.red"       TO  WRK-FORMID
           END-IF
      *
           .
       300-HC03-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   明細見出編集処理
      *****************************************************************
       3001-MEISAI-HEN-SEC                SECTION.
      *
           MOVE  "初・再診料"      TO  HC03-MEISAI (1)
           MOVE  "医学管理等"      TO  HC03-MEISAI (2)
           MOVE  "在 宅 医 療"      TO  HC03-MEISAI (3)
           MOVE  "投　 　　薬"      TO  HC03-MEISAI (4)
           MOVE  "注　 　　射"      TO  HC03-MEISAI (5)
           MOVE  "処 　　　置"      TO  HC03-MEISAI (6)
           MOVE  "手 　　　術"      TO  HC03-MEISAI (7)
           MOVE  "麻 　　　酔"      TO  HC03-MEISAI (8)
           MOVE  "検 　　　査"      TO  HC03-MEISAI (9)
           MOVE  "画 像 診 断"      TO  HC03-MEISAI (10)
           MOVE  "リハビリテーション"  TO  HC03-MEISAI (11)
           MOVE  "精神科専門療法"      TO  HC03-MEISAI (12)
           MOVE  "放射線治療"      TO  HC03-MEISAI (13)
           MOVE  "病 理 診 断"      TO  HC03-MEISAI (14)
           MOVE  "入 院 料 等"      TO  HC03-MEISAI (15)
           MOVE  "療養担当手当"        TO  HC03-MEISAI (16)
           IF    (SYU-HKNTEN (16)      =  ZERO)  AND
                 (SYU-TGMONEY (16)     =  ZERO)  AND
                 (SYU-TGMONEY-TAX(16)  =  ZERO)
             MOVE  SPACE                 TO  HC03-MEISAI (16)
           END-IF
      *
           .
       3001-MEISAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数科・保険入力の印刷処理
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
           PERFORM VARYING IDX-SYU     FROM    1   BY  1
                   UNTIL   IDX-SYU     >   ORCHC03SUB-MAX
               MOVE    ORCHC03SUB-SYUNOU-REC(IDX-SYU)
                                           TO  SYUNOU-REC
      *        診療科名編集
               MOVE    SPACE               TO  WRK-SRYKA-MEI
               IF      ORCHC03SUB-DOUJI-SRYKA-G (IDX-SYU)
                                           =   SPACE
                   MOVE    SYU-SRYKA           TO  WRK-SRYKA
                   PERFORM 1000-SRYKAMEI-HEN-SEC
               ELSE
                   PERFORM 2021-GRP-SRYKAMEI-HEN-SEC
               END-IF
      *        保険組合せ
               IF      SYU-HKNCOMBINUM     =   ZERO  OR  SPACE
                   MOVE    "複数保険使用"      TO  WRK-HKNCOMBIMEI
                   MOVE    ZERO                TO  SYU-PTFTNRATE
               ELSE
                   PERFORM 1001-HKNCOMBI-HEN-SEC
               END-IF
      *
               MOVE    SYU-DENPNUM         TO  WRK-DENPNUM
               MOVE    ORCHC03SUB-SKYMONEY (IDX-SYU)
                                           TO  WRK-SKYMONEY
               MOVE    ORCHC03SUB-NYUKIN-TOTAL (IDX-SYU)
                                           TO  WRK-NYUKIN
               MOVE    ORCHC03SUB-SKYMONEY-TAX-SAI (IDX-SYU)
                                           TO  WRK-SKYMONEY-TAX-SAI
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
      *
      *        帳票印刷開始処理
               PERFORM 400-SYOKI-PRINT-SEC
      *
      *        帳票編集処理
               PERFORM 300-HC03-HEN-SEC
      *
      *        帳票印刷処理
               PERFORM 400-PRINT-SEC
           END-PERFORM
      *
           .
       202-KAKHOU-SYUNOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタテーブルからの印刷処理
      *****************************************************************
       203-PARA-SYUNOU-SEC                   SECTION.
      *
           PERFORM  900-PARA-KEY5-SEL-SEC
      *
           PERFORM  UNTIL  ( FLG-PARA  NOT =  ZERO )
      *
               MOVE    PARA-DATA-REC         TO  SYUPARA03-AREA
               MOVE    SYUPARA03-SYU-DATA    TO  SYUNOU-REC
      *
               MOVE    SYUPARA03-DENPNUM     TO  WRK-DENPNUM
               MOVE    SYUPARA03-SEIKYU      TO  WRK-SKYMONEY
               MOVE    SYUPARA03-SEIKYU-TAX-SAI
                                             TO  WRK-SKYMONEY-TAX-SAI
               MOVE    SYUPARA03-NYUKIN      TO  WRK-NYUKIN
               MOVE    SYUPARA03-MISYU       TO  WRK-MISYUMONEY
               MOVE    SYUPARA03-SRYKAMEI    TO  WRK-SRYKA-MEI
      *
               INITIALIZE                        HKNCOMBI-REC
                                                 PTRSIINF-REC
               IF  ( SYUPARA03-HKNCOMBI-MAX  =  1 )
                   PERFORM  1001-HKNCOMBI-HEN-SEC
               END-IF
      *
               MOVE    SYUPARA03-HKNCOMBIMEI TO  WRK-HKNCOMBIMEI
      *
               IF  ( ORCHC03-PRTKBN  =  "1" )
      *
      *            帳票編集処理
                   PERFORM  300-HC03-HEN-SEC
      *
      *            帳票印刷処理
                   PERFORM  401-PRINT-SEC
               ELSE
      *
      *            帳票印刷開始処理
                   PERFORM  400-SYOKI-PRINT-SEC
      *
      *            帳票編集処理
                   PERFORM  300-HC03-HEN-SEC
      *
      *            帳票印刷処理
                   PERFORM  400-PRINT-SEC
               END-IF
      *
               PERFORM  900-PARA-KEY5-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_para"                TO  MCP-TABLE
           MOVE    "key5"                    TO  MCP-PATHNAME
           PERFORM  990-DBCLOSE-SEC
      *
           .
       202-KAKHOU-SYUNOU-EXT.
           EXIT.
      *
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
               MOVE    WRK-Z72-H9           TO  KANACHK-MAE-INPUT
           END-IF
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT(1:18)    TO  WRK-Z72-G
           END-IF
           .
       810-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角全角変換処理
      *****************************************************************
       820-HENKAN-SEC             SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           IF      WRK-Z72             <   ZERO
             MOVE    WRK-Z72             TO  WRK-Z72-HS2
             MOVE    WRK-Z72-HS2          TO  KANACHK-MAE-INPUT
           ELSE
             MOVE    WRK-Z72             TO  WRK-Z72-H2
             MOVE    WRK-Z72-H2           TO  KANACHK-MAE-INPUT
           END-IF
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
             MOVE    KANACHK-OUT-INPUT(1:14)    TO  WRK-Z72-G
           END-IF
           .
       820-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費名称編集処理
      *****************************************************************
       3102-JIHIMSG-HEN-SEC             SECTION.
      *
           MOVE    WRK-JIHIMEI(1)      TO  HC03-JIHIMSG1
           MOVE    WRK-JIHIMEI(2)      TO  HC03-JIHIMSG2
           MOVE    WRK-JIHIMEI(3)      TO  HC03-JIHIMSG3
           MOVE    WRK-JIHIMEI(4)      TO  HC03-JIHIMSG4
           MOVE    WRK-JIHIMEI(5)      TO  HC03-JIHIMSG5
           MOVE    WRK-JIHIMEI(6)      TO  HC03-JIHIMSG6
           MOVE    WRK-JIHIMEI(7)      TO  HC03-JIHIMSG7
           MOVE    WRK-JIHIMEI(8)      TO  HC03-JIHIMSG8
           MOVE    WRK-JIHIMEI(9)      TO  HC03-JIHIMSG9
           MOVE    WRK-JIHIMEI(10)     TO  HC03-JIHIMSG10
      *
           .
       3102-JIHIMSG-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療日編集処理
      *****************************************************************
       3101-SRYYMD-HEN-SEC        SECTION.
      *
           MOVE    SPACE                   TO  WRK-SRYYMDG
                                               WRK-LBLSRYYMD
      *
           IF  ( ORCHC03-HAKKOFLG  =  4 )
               MOVE    "診療年月"          TO  WRK-LBLSRYYMD
               MOVE    SYU-SRYYMD          TO  WRK-SYMD
               PERFORM  31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG (1:16)  TO  WRK-SRYYMDG
           ELSE
               IF  ( ORCHC03-PARAFLG  =  1 )
               AND ( SYUPARA03-STSRYYMD  NOT =  SYUPARA03-EDSRYYMD )
                   MOVE    "診療日"        TO  WRK-LBLSRYYMD
                   PERFORM  31012-SRYYMD-HEN-SEC
               ELSE
                   MOVE    "診療日"        TO  WRK-LBLSRYYMD
                   MOVE    SYU-SRYYMD      TO  WRK-SYMD
                   PERFORM  31012-SEIWA-HEN-SEC
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
           MOVE    SPACE                   TO  WRK-SRYYMDG
      *
           MOVE    SYUPARA03-STSRYYMD      TO  WRK-SYMD
           PERFORM  31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG             TO  WRK-STSRYYMDG
      *
           MOVE    SYUPARA03-EDSRYYMD      TO  WRK-SYMD
           PERFORM  31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG             TO  WRK-EDSRYYMDG
      *
           STRING  WRK-STSRYYMDG           DELIMITED  BY  SIZE
                   " 〜 "                  DELIMITED  BY  SIZE
                   WRK-EDSRYYMDG           DELIMITED  BY  SIZE
                   INTO                    WRK-SRYYMDG
           END-STRING
      *
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
      *
           .
       800-HIZUKE-CHK-EXT.
           EXIT.
      *
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
      *
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
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
           MOVE    SPA-SRYYMD      TO  SYS-1900-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1900-EDYUKYMD
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1900-REC
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
           MOVE    SPACE           TO  SYS-1901-REC
           INITIALIZE                  SYS-1901-REC
           MOVE    "1901"          TO  SYS-1901-KANRICD
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    SYS-1900-PRTKBN(10)
                                   TO  SYS-1901-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1901-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1901-EDYUKYMD
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1901-REC
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
           MOVE    SPACE           TO  SYS-1902-REC
           INITIALIZE                  SYS-1902-REC
           MOVE    "1902"          TO  SYS-1902-KANRICD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1900-PRTKBN(10)
                                   TO  SYS-1902-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1902-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1902-EDYUKYMD
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
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
      *
      *****************************************************************
      *    システム管理マスタ主キー読み込み処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
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
      *    全角変換処理
      *****************************************************************
       2009-HENKAN-SEC                 SECTION.
      *
           MOVE    ALL "　"            TO  WRK-ZENKAKU-G
      *
           PERFORM VARYING     IDK     FROM    1   BY  1
                   UNTIL       IDK     >   WRK-MOJISU
               IF      WRK-MOJI (IDK:1)    NUMERIC
                   MOVE    WRK-MOJI(IDK:1)     TO  IDX2-1
                   IF      IDX2-1              =   ZERO
                       MOVE    10                  TO  IDX2
                   ELSE
                       MOVE    IDX2-1              TO  IDX2
                   END-IF
                   MOVE    TBL-SUJI (IDX2)     TO  WRK-ZENKAKU(IDK)
               ELSE
                   IF      WRK-MOJI (IDK:1)    =   "."
                       MOVE    "．"            TO  WRK-ZENKAKU(IDK)
                   END-IF
               END-IF
           END-PERFORM
           .
       2009-HENKAN-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
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
               MOVE    "tbl_ptinf"        TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *
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
           MOVE    "tbl_ptinf"        TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                            SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    パラメタテーブル検索
      *****************************************************************
       900-PARA-KEY5-SEL-SEC         SECTION.
      *
           MOVE    ZERO                   TO  FLG-PARA
      *
           INITIALIZE                         PARA-REC
           MOVE    SPA-HOSPNUM            TO  PARA-HOSPNUM
           MOVE    ORCHC03-PARA-GYOUMUID  TO  PARA-GYOUMUID
           MOVE    ORCHC03-PARA-TERMID    TO  PARA-TERMID
           MOVE    ORCHC03-PARA-FILEMEI   TO  PARA-FILEMEI
           MOVE    PARA-REC               TO  MCPDATA-REC
           MOVE    "tbl_para"             TO  MCP-TABLE
           MOVE    "key5"                 TO  MCP-PATHNAME
           PERFORM  910-DBSELECT-SEC
           IF  ( MCP-RC  =  ZERO )
               PERFORM  900-PARA-KEY5-FET-SEC
           ELSE
               MOVE  1                    TO  FLG-PARA
               INITIALIZE                     PARA-REC
           END-IF
      *
           .
       900-PARA-KEY5-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタテーブルFETCH
      *****************************************************************
       900-PARA-KEY5-FET-SEC         SECTION.
      *
           MOVE    ZERO                   TO  FLG-PARA
      *
           MOVE    "tbl_para"             TO  MCP-TABLE
           MOVE    "key5"                 TO  MCP-PATHNAME
           PERFORM  920-DBFETCH-SEC
           IF  ( MCP-RC  =  ZERO )
               MOVE  MCPDATA-REC          TO  PARA-REC
           ELSE
               MOVE  1                    TO  FLG-PARA
               INITIALIZE                     PARA-REC
           END-IF
      *
           .
       900-PARA-KEY5-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
      *    マル長の公費負担を保険適用金額へ
           IF    ( SYU-HKNTEKKBN       =   "1" )
               MOVE    SYU-KOH-FTN     TO  SYU-HKNTEKMONEY
               MOVE    ZERO            TO  SYU-KOH-FTN
           END-IF
      *
           .
       980-SYUNOU-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報読み込み
      *****************************************************************
       910-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    SPA-PTID            TO  PTHKN-PTID
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC    TO    PTHKNINF-REC
                   MOVE    ZERO           TO    FLG-PTHKNINF
               ELSE
                   MOVE    1              TO    FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1              TO    FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_hkncombi"        TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_hkncombi"        TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                            SPA-AREA
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       910-PTRSIINF-READ-SEC         SECTION.
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                            SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptrsiinf"        TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTRSIINF-REC
                   MOVE    ZERO            TO  FLG-PTRSIINF
               ELSE
                   MOVE    1               TO  FLG-PTRSIINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"        TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                            SPA-AREA
      *
           .
       910-PTRSIINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"          USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"          USING
                                       MCPAREA
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
