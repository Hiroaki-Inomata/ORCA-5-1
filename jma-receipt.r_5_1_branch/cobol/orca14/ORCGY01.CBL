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
       PROGRAM-ID.             ORCGY01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 予約登録
      *  コンポーネント名  : 予約状況入力（Ｙ０１）
      *  管理者            : 
      *  00/12/01    MCC−   　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/05/24  患者番号ゼロ詰め処理
      *  01.00.02    NACL-多々納  02/08/02  画面のＳＰＡを保存領域へ編集
      *  01.00.03    NACL-多々納  02/11/20  GO TO 先修正他
      *  01.00.04    NACL-多々納  03/03/05  排他制御処理追加
      *  01.00.05    NACL-多々納  03/12/01  一覧表を２００件へ
      *  01.00.06    NACL-多々納  05/09/21  患者予約日検索追加他
      *  01.00.07    NACL-多々納  06/05/12  MONFUNC 対応
      *  03.03.00    NACL-多々納  06/10/10  名前検索の追加
      *  03.05.00    NACL-多々納  07/05/07  グループ診療対応
      *  04.01.00    NACL-多々納  08/01/30  予約内容テーブル対応
      *  04.03.00    NACL-多々納  08/06/18  初期表示設定対応
      *  04.04.00    NACL-多々納  09/01/16  予約メモ・予約票対応
      *  04.05.00    NACL-多々納  09/10/27  拡張漢字対応
      *  04.08.00    NACL-森脇    13/10/10  クライアント印刷対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    予約用ワークスパ領域
           COPY    "Y01COMMON-SPA".
      *
      *    予約パ領域
           COPY    "Y01SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-YYK             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-YYKCOM          PIC 9(01).
           03  FLG-YYKEXAMPLE      PIC 9(01).
      *
           03  FLG-HENFLG          PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-HENKOU          PIC 9(01).
           03  FLG-CALENDAR        PIC 9(01).
      *----(04.08.00)--ADD-START---
      *    クライアント印刷有
           03  FLG-CLIENT-OK       PIC 9(01).
      *----(04.08.00)--ADD-END-----
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX-Y               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-T1              PIC 9(04).
           03  IDX-T2              PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX-INIT            PIC 9(04).
      *
           03  IDX-YY              PIC 9(04).
           03  IDX-C               PIC 9(04).
           03  IDD2                PIC 9(02).
      *
           03  IDX-S1              PIC 9(04).
           03  IDX-S2              PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
      *
           03  WRK-WSYSYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-GMN-YYKTIME     PIC X(05).
      *
           03  WRK-HI              PIC 9(02).
           03  WRK-KEI             PIC 9(03).
           03  WRK-SEL-SYUU        PIC 9(02).
           03  WRK-KEY-YYKTIME     PIC 9(04).
      *
           03  WRK-SPA-Y01-PTNUM   PIC X(20).
           03  WRK-SPA-Y01-PTID    PIC X(10).
           03  WRK-SPA-Y01-NAME    PIC X(50).
           03  WRK-GMN-GOKSEL      PIC 9(01).
           03  WRK-GMN-KENSEL      PIC 9(02).
           03  WRK-MOJISU          PIC 9(04).
     *
           03  WRK-TIME-X.
               05  WRK-THHX        PIC X(02).
               05  WRK-TSSX        PIC X(02).
           03  WRK-TIME.
               05  WRK-THH         PIC 9(02).
               05  WRK-TSS         PIC 9(02).
           03  FLG-TIME            PIC 9(01).
      *
           03  WRK-DRCD           PIC X(04).
      *
           03  WRK-HKENSUU.
      ****     05  FILLER          PIC X(01).
               05  WRK-KENSUU      PIC ZZZ.
               05  FILLER          PIC X(02).
      *
           03  WRK-KNJ.
               05  WRK-PTNUM       PIC X(20).
               05  WRK-PTID        PIC X(10).
               05  WRK-NAME        PIC X(50).
               05  WRK-KANANAME    PIC X(50).
               05  WRK-SEX         PIC X(02).
               05  WRK-BIRTHDAYH   PIC X(09).
      *
           03  WRK-NAME-KEY        PIC X(50).
           03  WRK-NAME1           PIC X(50).
           03  WRK-NAME2           PIC X(50).
      *
           03  WRK-YYKID           PIC 9(02).
           03  WRK-YYKCNT          PIC 9(02).
      *
           03  WRK-ERRMSG          PIC X(40).
           03  WRK-PATH            PIC X(64).
           03  WRK-MCP-WIDGET      PIC X(64).
      *
           03  WRK-SAKIPG          PIC X(08).
           03  WRK-MOTOPG          PIC X(08).
      *
           03  WRK-NAI-DR-SEMON.
               05  WRK-DR-SEMONKACD   PIC X(02)
                                              OCCURS  5.
      *
      *    コラムリスト位置
           03  WRK-Y01-ROW             PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
      *
           03  WRK-GMN-CUR         PIC 9(02).
      *
           03  WRK-HYOJI               PIC 9(04).
      *
           03  WRK-YYKHYO-PG           PIC X(64).
      *
           03  WRK-YID1MSG             PIC X(80).
      *
           03  WRK-E-NAME              PIC X(50).
           03  WRK-H-NAME              PIC X(50).
           03  WRK-H-NAME1             PIC X(02).
      *
      *  ＳＯＲＴ用ワーク
      *    受付患者一覧
       01  WRK-SORT-AREA.
           03  WRK-YYKTIME             PIC X(04).
           03  WRK-IDX                 PIC 9(04).
           03  WRK-GMN-TBL.
               05  WRK-GMN-TBLO        OCCURS   200.
                   07  WRK-GMN-TBANGO      PIC 9(03).
                   07  WRK-GMN-TYYKTIME    PIC X(04).
                   07  WRK-GMN-TNAME       PIC X(20).
                   07  WRK-GMN-TKAMEI      PIC X(04).
                   07  WRK-GMN-TRAIINZUMI  PIC X(04).
           03  WRK-NAI-TBL.
               05  WRK-NAI-TBLO        OCCURS   200.
                   07  WRK-NAI-TPTID       PIC X(10).
                   07  WRK-NAI-TPTNUM      PIC X(20).
                   07  WRK-NAI-TYYKYMD     PIC X(08).
                   07  WRK-NAI-TKYYKTIME   PIC 9(04).
                   07  WRK-NAI-TYYKID      PIC 9(02).
                   07  WRK-NAI-TSRYKA      PIC X(02).
                   07  WRK-NAI-TYYKKBN     PIC X(02).
                   07  WRK-NAI-TKAKREN     PIC X(02).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
       01  WRK-GMN-KENSUU.
           03  WRK-GMN-KENSUU-OCC  OCCURS 98.
               05  WRK-TBL-KENSUU-STYLE    PIC X(20).
               05  WRK-TBL-KENSUU          PIC X(05).
      *
       01  WRK-CAL-YMD.
           03  WRK-CAL-YEAR        PIC 9(04).
           03  WRK-CAL-MONTH       PIC 9(02).
           03  WRK-CAL-DAY         PIC 9(02).
       01  WRK-GMN-TAIYYMM         PIC X(12).
      *
      *    スパ領域
           COPY    "COMMON-SPA"    REPLACING
                                   //SPA-// BY //HZN-SPA-//.
      *
      *    予約マスターワーク
       01  HZN-YYK-REC.
           COPY    "CPYYK.INC"      REPLACING
                                   //YYK-// BY //HZN-YYK-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    診療内容情報
           COPY  "CPSK1012.INC".
      *    診療科
           COPY  "CPSK1005.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *    予約確認連絡コード情報
           COPY  "CPSK0051.INC".
      *    予約内容
           COPY  "CPSK1028.INC".
      *    予約内容
           COPY  "CPSK1045.INC".
      *    帳票ＰＧ
           COPY  "CPSK1032.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    予約マスター
       01  YYK-REC.
           COPY    "CPYYK.INC".
      *
      *    予約メモマスター
       01  YYKCOM-REC.
           COPY    "CPYYKCOM.INC".
      *
      *    予約雛形マスター
       01  YYKEXAMPLE-REC.
           COPY    "CPYYKEXAMPLE.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
      *
           COPY  "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *
           COPY  "CPORCSGDAY.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    ドクター編集変換サブ
           COPY    "CPORCSDRSET.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK2.INC".
      *    拡張文字対応
           COPY    "CPORCSSTRING.INC".
      *
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    予約票パラメタ
           COPY    "CPORCHC67.INC".
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *    オンライン帳票共通パラメタ
           COPY    "CPONPRTDATA.INC".
      *----(04.08.00)--ADD-START---
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *----(04.08.00)--ADD-END-----
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
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "Y01.INC".
           COPY    "Y011.INC".
           COPY    "Y012.INC".
           COPY    "Y03.INC".
           COPY    "Y04.INC".
           COPY    "Y05.INC".
           COPY    "YERR.INC".
           COPY    "YID1.INC".
           COPY    "CPAPIMODV2.INC".
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
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-Y01
           MOVE    SPA-WORK        TO  SPA-Y01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO MCP-EVENT ALSO MCP-WIDGET(1:1)
               WHEN    "LINK"      ALSO ANY       ALSO ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"      ALSO "CLICKED" ALSO "B"
                   PERFORM 210-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-Y01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-Y01         TO  SPA-FREE
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
           IF      SPA-MOTOPG          =   "YERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
      *****MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    SPACE                TO  MCP-PUTTYPE
           MOVE    "Y01"                TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                    TO  FLG-END
          .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "Y011"
      *            メモ登録画面
                   MOVE    10                  TO  SPA-GMN-CUR
               WHEN    "Y012"
      *            メモ・文例登録画面
                   MOVE    3                   TO  SPA-GMN-CUR
               WHEN    "YID1"
      *            確認画面
                   PERFORM 30010-YID1-SET-SEC
               WHEN    OTHER
                   PERFORM 300-INIT-SYORI-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       300-INIT-SYORI-SEC              SECTION.
      *
           INITIALIZE              SPA-Y01-AREA
      *
           IF      LNK-KYOUTU      NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           IF      (SPA-MOTOPG(1:1)    =   "Y"  )  OR
                   (SPA-MOTOPG         =   "U01" OR  "P97")
               MOVE    SPA-WORK            TO  SPA-Y01KYOUTU
           ELSE
               INITIALIZE                  SPA-Y01KYOUTU
           END-IF
      *
           PERFORM 310-SPASET-SEC
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "Y03"
               WHEN    "Y04"
                   MOVE    SPACE               TO  SPA-GMN-PTNUM
                   MOVE    ZERO                TO  SPA-GMN-PTID
                   MOVE    SPACE               TO  SPA-GMN-NAME
                   MOVE    SPACE               TO  SPA-GMN-SEX
                   MOVE    SPACE               TO  SPA-GMN-BIRTHDAYH
                   MOVE    SPACE               TO  SPA-GMN-YYKNAME
               WHEN    "Y05"
      *            患者検索の時、患者番号表示
                   MOVE    SPA-Y01-PTNUM       TO  SPA-GMN-PTNUM
                   MOVE    SPA-Y01-PTID        TO  SPA-GMN-PTID
                   MOVE    SPA-Y01-YYKNAME     TO  SPA-GMN-NAME
                   MOVE    SPA-Y01-SEX         TO  SPA-GMN-SEX
                   MOVE    SPA-Y01-BIRTHDAYH   TO  SPA-GMN-BIRTHDAYH
                   MOVE    SPA-Y01-YYKNAME     TO  SPA-GMN-YYKNAME
               WHEN    "U01"
      *            受付より選択後
                   IF     (SPA-PTID       NOT =   ZERO   )
                       MOVE    SPA-PTID           TO  SPA-GMN-PTID
                       MOVE    SPA-PTNUM          TO  SPA-GMN-PTNUM
                       MOVE    SPA-NAME           TO  SPA-GMN-NAME
                       MOVE    SPA-SEX            TO  SPA-GMN-SEX
                       MOVE    SPA-BIRTHDAYW      TO  SPA-GMN-BIRTHDAYH
                       MOVE    SPA-NAME           TO  SPA-GMN-YYKNAME
                   END-IF
               WHEN    "P97"
      *            氏名検索より
                   IF     (SPA-PTID           =   ZERO   )
                       MOVE    SPA-Y01-YYKNAME    TO  SPA-GMN-YYKNAME
                       MOVE    SPACE              TO  SPA-Y01-YYKNAME
                   ELSE
                       MOVE    SPA-PTID           TO  SPA-GMN-PTID
                       MOVE    SPA-PTNUM          TO  SPA-GMN-PTNUM
                       MOVE    SPA-NAME           TO  SPA-GMN-NAME
                       MOVE    SPA-SEX            TO  SPA-GMN-SEX
                       MOVE    SPA-BIRTHDAYW      TO  SPA-GMN-BIRTHDAYH
                       MOVE    SPA-NAME           TO  SPA-GMN-YYKNAME
                   END-IF
      *TEST
               WHEN    "K02"
               WHEN    "U02"
      *            受付・診療行為より選択後
                   IF     (SPA-PTID        NOT =   ZERO   )
                   AND    (SYS-1045-YYKPTNUM-KBN   =   "1"  )
      *                 患者番号連動編集処理
                       PERFORM 3101-PTNUM-AUTO-SEC
                   END-IF
                   INITIALIZE                  SPA-KYOTUKEY
      *
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-GMN-SELBAN
           MOVE    ZERO                TO  SPA-NAI-SELBAN
      *
      *    初期表示があるので、患者入力より
           IF     (SPA-GMN-PTNUM       =   SPACE  )  AND
                  (SPA-GMN-YYKNAME     =   SPACE  )
               MOVE    3                   TO  SPA-GMN-CUR
           ELSE
               MOVE    5                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       300-INIT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号連動編集処理
      *****************************************************************
       3101-PTNUM-AUTO-SEC              SECTION.
      *
           MOVE    SPA-PTID            TO  SPA-GMN-PTID
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
           MOVE    SPA-NAME            TO  SPA-GMN-NAME
           MOVE    SPA-SEX             TO  SPA-GMN-SEX
           MOVE    SPA-BIRTHDAYW       TO  SPA-GMN-BIRTHDAYH
           MOVE    SPA-NAME            TO  SPA-GMN-YYKNAME
      *    診療科
           IF     (SPA-SRYKA           =   SPACE
                                       OR  ZERO      )
             OR   (SPA-SRYKA           =   SPA-GMN-SRYKA )
               CONTINUE
           ELSE
               PERFORM VARYING IDY     FROM   1   BY  1
                       UNTIL  (IDY     >   SPA-SRYKA-MAX  )
                   IF      SPA-SRYKA           =   SPA-GMN-SRYKACDL(IDY)
                       MOVE    SPA-GMN-SRYKALST (IDY)
                                               TO  SPA-GMN-SRYKA-G
                       MOVE    SPA-SRYKA-MAX   TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *    ドクター
           IF     (SPA-DRCD(2:4)       =   SPACE
                                       OR  ZERO   )
             OR   (SPA-DRCD(2:4)       =   SPA-GMN-DRCD)
               CONTINUE
           ELSE
               MOVE    ZERO               TO  FLG-HENFLG
               PERFORM VARYING IDY     FROM   1   BY  1
                       UNTIL  (IDY     >      SPA-DRCD-MAX  )  OR
                              (FLG-HENFLG     =   1  )
                   IF      SPA-DRCD(2:4)   =   SPA-GMN-DRCDLST(IDY)(1:4)
                       MOVE    1               TO  FLG-HENFLG
                   END-IF
               END-PERFORM
               IF      FLG-HENFLG          =   1
                   MOVE    SPA-DRCD(2:4)       TO  Y01-DRNAME
                   PERFORM 41012-DRNAME-SEC
               END-IF
           END-IF
      *
           .
       3101-PTNUM-AUTO-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-Y01-AREA
      *
      *    初期表示情報
           MOVE    SPACE               TO  SYS-1045-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1045-HOSPNUM
           MOVE    "1045"              TO  SYS-1045-KANRICD
           MOVE    "*"                 TO  SYS-1045-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1045-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1045-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1045-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRIT-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1045-REC
               ELSE
                   INITIALIZE                  SYS-1045-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1045-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    初期表示ドクター
           MOVE    SYS-1045-INIT-DRCD  TO  SPA-Y01-INIT-DRCD
           MOVE    SYS-1045-YYKINIT-FLG
                                       TO  SPA-Y01-YYKINIT-FLG
           MOVE    SYS-1045-YYKHYO-KBN
                                       TO  SPA-Y01-YYKHYO-KBN
      *    予約票区分
           MOVE    "0 発行しない"      TO  SPA-GMN-YYKHYO-LIST (1)
           MOVE    "1 発行する"        TO  SPA-GMN-YYKHYO-LIST (2)
           MOVE    2                   TO  SPA-YYKHYO-MAX
           EVALUATE    SPA-Y01-YYKHYO-KBN
               WHEN    "1"
                   MOVE    SPA-GMN-YYKHYO-LIST (2) TO  SPA-GMN-YYKHYO-G
               WHEN    OTHER
                   MOVE    SPA-GMN-YYKHYO-LIST (1) TO  SPA-GMN-YYKHYO-G
                   MOVE    "0"                 TO  SPA-Y01-YYKHYO-KBN
           END-EVALUATE
      *
      *    予約枠最大件数
           IF     (FLG-SYSKANRI        =   ZERO )
              AND (SYS-1045-YYKMAXCNT  >   ZERO )
               MOVE    SYS-1045-YYKMAXCNT  TO  SPA-Y01-YYKMAXCNT
           ELSE
               MOVE    12                  TO  SPA-Y01-YYKMAXCNT
           END-IF
      *
      *    診療内容をＳＰＡにセット
           MOVE    SPACE               TO  SYS-1012-REC
           MOVE    "1012"              TO  SYS-1012-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-1012-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1012-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1012-HOSPNUM
           MOVE    SYS-1012-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRIT-READ-SEC
           ELSE
               INITIALIZE                  SYS-1012-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  SPA-SRYNAIYO-HYOJI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX            >   99   )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC         TO  SYS-1012-REC
               MOVE    SYS-1012-KBNCD      TO  SPA-GMN-SRYNAIYOCD
               MOVE    SPACE               TO  SPA-GMN-SK1
               MOVE    SYS-1012-SRYNAIYODSP
                                           TO  SPA-GMN-SRYNAIYOMEI
      *
               MOVE    SPA-GMN-SRYNAIYO    TO  SPA-GMN-SRYNAIYOLST(IDX)
               MOVE    IDX                 TO  SPA-SRYNAIYO-MAX
               IF     (SYS-1012-SRYNAIYOHYOJI  =   "1" )
                  AND (SPA-SRYNAIYO-HYOJI      =   ZERO)
                   MOVE    IDX             TO  SPA-SRYNAIYO-HYOJI
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRIT-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SPA-Y01-SRYNAIYOG   =   SPACE
               IF      SPA-SRYNAIYO-HYOJI  =   ZERO
                   MOVE    1               TO  SPA-SRYNAIYO-HYOJI
               END-IF
               MOVE    SPA-GMN-SRYNAIYOLST (SPA-SRYNAIYO-HYOJI)
                                       TO  SPA-GMN-SRYNAIYO
           ELSE
               MOVE    SPA-Y01-SRYNAIYOG
                                       TO  SPA-GMN-SRYNAIYO
           END-IF
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1005-KEY
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRIT-READ-SEC
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX            >   99   )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC             TO  SYS-1005-REC
      *
               MOVE    SYS-1005-KBNCD (1:2)    TO  SPA-GMN-SRYKA
               MOVE    SPACE                   TO  SPA-GMN-SRYKAH1
               MOVE    SYS-1005-SRYKANAME1     TO  SPA-GMN-SRYKAMEI
               MOVE    SPA-GMN-SRYKA-G         TO  SPA-GMN-SRYKALST
                                                              (IDX)
      *        短縮名３保存
               MOVE    SYS-1005-SRYKANAME3     TO  SPA-NAI-SRYKAMEI3
                                                              (IDX)
               MOVE    IDX                     TO  SPA-SRYKA-MAX
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRIT-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    初期設定
           MOVE    SPA-GMN-SRYKALST(1) TO  SPA-GMN-SRYKA-G
      *
      *    職員情報をＳＰＡにセット
           MOVE    SPACE               TO  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRIT-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX-INIT
           PERFORM
                   UNTIL       (IDX            >=  99   )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC             TO  SYS-1010-REC
               IF     (SYS-1010-KBNCD(1:1) =   "1"  )
      *H27.12  非表示
                AND   (SYS-1010-HYOJIFLG   NOT =   "1" )
                   ADD     1                   TO  IDX
                   MOVE    SYS-1010-KBNCD(2:4)
                                               TO  SPA-GMN-DRCD
                   MOVE    SPACE               TO  SPA-GMN-D1
                   MOVE    SYS-1010-NAME       TO  SPA-GMN-DRNAMEMEI
                   MOVE    SPA-GMN-DRNAME      TO  SPA-GMN-DRCDLST (IDX)
      *            専門家
                   MOVE    SYS-1010-SEMONKACD (1)
                                               TO  SPA-NAI-DR-SEMONKACD
                                                                (IDX 1)
                   MOVE    SYS-1010-SEMONKACD (2)
                                               TO  SPA-NAI-DR-SEMONKACD
                                                                (IDX 2)
                   MOVE    SYS-1010-SEMONKACD (3)
                                               TO  SPA-NAI-DR-SEMONKACD
                                                                (IDX 3)
                   MOVE    SYS-1010-SEMONKACD (4)
                                               TO  SPA-NAI-DR-SEMONKACD
                                                                (IDX 4)
                   MOVE    SYS-1010-SEMONKACD (5)
                                               TO  SPA-NAI-DR-SEMONKACD
                                                                (IDX 5)
      *            初期表示ドクター
                   IF     (SPA-Y01-INIT-DRCD   NOT =   SPACE  )
                      AND (SPA-Y01-INIT-DRCD   =   SYS-1010-KBNCD(1:5))
                       MOVE    IDX             TO  IDX-INIT
                   END-IF
      *
                   MOVE    IDX                 TO  SPA-DRCD-MAX
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRIT-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SPA-Y01-DRNAME      =   SPACE
               IF      IDX-INIT            =   ZERO
                   MOVE    1                   TO  IDX-INIT
               END-IF
               MOVE    SPA-GMN-DRCDLST (IDX-INIT)
                                           TO  SPA-GMN-DRNAME
           ELSE
               MOVE    SPA-Y01-DRNAME      TO  SPA-GMN-DRNAME
           END-IF
      *    ドクターコードからの編集
           PERFORM 410121-DRCD-HENSYU-SEC
      *
      *    予約確認連絡コード
           MOVE    ZERO                TO  FLG-SYSKANRI
           MOVE    SPACE               TO  SYS-0051-REC
           INITIALIZE                  SYS-0051-REC
           MOVE    "0051"              TO  SYS-0051-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-0051-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-0051-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-0051-HOSPNUM
           MOVE    SYS-0051-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRIT-READ-SEC
           ELSE
               INITIALIZE                  SYS-0051-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX            >   99   )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC             TO  SYS-0051-REC
      *
               MOVE    SYS-0051-KBNCD (1:2)    TO  SPA-GMN-KAKREN
               MOVE    SPACE                   TO  SPA-GMN-KAKRENH1
               MOVE    SYS-0051-YYKKAKNAME     TO  SPA-GMN-KAKRENMEI
               MOVE    SPA-GMN-KAKREN-G        TO  SPA-GMN-KAKRENLST
                                                              (IDX)
               MOVE    IDX                     TO  SPA-KAKREN-MAX
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRIT-READ-SEC
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           MOVE    SPA-GMN-KAKRENLST (1)
                                       TO  SPA-GMN-KAKREN-G
      *
      **** MOVE    "00 　　　　"       TO  SPA-GMN-YYKKBNLST (01)
      *    MOVE    "01 患者による予約" TO  SPA-GMN-YYKKBNLST (02)
      *    MOVE    "02 医師による予約" TO  SPA-GMN-YYKKBNLST (03)
      *    MOVE    SPA-GMN-YYKKBNLST (2)
      *                                TO  SPA-GMN-YYKKBN-G
      ***  MOVE    3                   TO  SPA-YYKKBN-MAX
      *    予約内容
           MOVE    ZERO                TO  FLG-SYSKANRI
           MOVE    SPACE               TO  SYS-1028-REC
           INITIALIZE                  SYS-1028-REC
           MOVE    "1028"              TO  SYS-1028-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-1028-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1028-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1028-HOSPNUM
           MOVE    SYS-1028-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRIT-READ-SEC
           ELSE
               INITIALIZE                  SYS-1028-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  SPA-YYKKBN-HYOJI
           MOVE    1                   TO  IDX
           MOVE    "00"                TO  SPA-GMN-YYKKBNLST(IDX)
           MOVE    IDX                 TO  SPA-YYKKBN-MAX
           PERFORM
                   UNTIL       (IDX            >=  100  )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC             TO  SYS-1028-REC
      *
               IF      SYS-1028-KBNCD (1:2)    =   "00"
                   MOVE    1                       TO  IDX
                   MOVE    SYS-1028-KBNCD (1:2)    TO  SPA-GMN-YYKKBNLST
                                                           (IDX)(1:2)
                   MOVE    SYS-1028-YYKKBNNAME     TO  SPA-GMN-YYKKBNLST
                                                           (IDX)(4:)
               ELSE
                   ADD     1                       TO  IDX
                   MOVE    SYS-1028-KBNCD (1:2)    TO  SPA-GMN-YYKKBNLST
                                                           (IDX)(1:2)
                   MOVE    SYS-1028-YYKKBNNAME     TO  SPA-GMN-YYKKBNLST
                                                           (IDX)(4:)
                   MOVE    IDX                     TO  SPA-YYKKBN-MAX
               END-IF
               IF     (SYS-1028-HYOJIKBN   =   "1" )
                 AND  (SPA-YYKKBN-HYOJI    =   ZERO )
                   MOVE    IDX             TO  SPA-YYKKBN-HYOJI
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRIT-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      SPA-YYKKBN-HYOJI    =   ZERO
               MOVE    SPA-GMN-YYKKBNLST (2)
                                           TO  SPA-GMN-YYKKBN-G
           ELSE
               MOVE    SPA-GMN-YYKKBNLST (SPA-YYKKBN-HYOJI)
                                           TO  SPA-GMN-YYKKBN-G
           END-IF
      *
           IF    (SPA-Y01-YYKYMDS  NUMERIC  )  AND
                 (SPA-Y01-YYKYMDS  NOT =   SPACE )  AND
                 (SPA-Y01-YYKYMDS  NOT =   ZERO  )
               MOVE    SPA-Y01-YYKYMDS     TO  WRK-SYSYMD
           ELSE
               MOVE    SPA-SYSYMD          TO  WRK-SYSYMD
           END-IF
      *    予約日編集
           PERFORM 3101-YYKYMD-HENSYU-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約日からの編集処理
      *****************************************************************
       3101-YYKYMD-HENSYU-SEC         SECTION.
      *
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYSYMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"      USING    STS-AREA-DAY
                                           LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
              MOVE LNK-DAY2-EDTYMD3 (1:4)
                                       TO  SPA-GMN-TAIYYMM
              MOVE LNK-DAY2-EDTYMD1 (2:2)
                                       TO  SPA-GMN-TAIYYMM (5:2)
              MOVE "年"                TO  SPA-GMN-TAIYYMM (7:2)
              MOVE LNK-DAY2-EDTYMD1 (5:2)
                                       TO  SPA-GMN-TAIYYMM (9:2)
              MOVE "月"                TO  SPA-GMN-TAIYYMM (11:2)
           ELSE
              MOVE SPACE               TO  SPA-GMN-TAIYYMM
           END-IF
      *
      * １日の曜日算出
           MOVE    "31"                TO  LNK-DAY3-IRAI
           MOVE    WRK-SYSYMD          TO  LNK-DAY3-YMD
           MOVE    "01"                TO  LNK-DAY3-YMD(7:2)
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY3-AREA
           IF      STS-DAY-RC1      NOT =   ZERO
              MOVE   0                   TO  LNK-DAY3-YOBI
           END-IF
      *
           IF      LNK-DAY3-YOBI   NOT =   ZERO
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX         >   LNK-DAY3-YOBI )
                   MOVE  ZERO          TO  SPA-GMN-HYODD (IDX)
                   MOVE  ZERO          TO  SPA-NAI-YMD (IDX)
               END-PERFORM
           END-IF
      *
      * 月末日の算出
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-SYSYMD (1:6)    TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING    STS-AREA-DAY
                                            LNK-DAY7-AREA
           IF      STS-DAY-RC1      NOT =   ZERO
              MOVE WRK-SYSYMD          TO  LNK-DAY7-KOYOMI
              MOVE "31"                TO  LNK-DAY7-KOYOMI (7:2)
           END-IF
      *
           MOVE    IDX                 TO  WRK-IDX
           IF      WRK-IDX             =   ZERO
               MOVE    1               TO  WRK-IDX
           END-IF
      *
           MOVE    ZERO                TO  WRK-HI
           PERFORM VARYING     IDX     FROM    WRK-IDX   BY  1
                   UNTIL      (IDX         >   42  )   OR
                              (WRK-HI      =   LNK-DAY7-KOYOMI (7:2) )
               ADD     1               TO  WRK-HI
               MOVE    WRK-HI          TO  SPA-GMN-HYODD (IDX)
               MOVE    WRK-SYSYMD (1:6)
                                       TO  SPA-NAI-YMD (IDX)(1:6)
               MOVE    WRK-HI          TO  SPA-NAI-YMD (IDX)(7:2)
           END-PERFORM
      *
      *    初期画面では、システム日付を対象とし、表示する
           MOVE    WRK-SYSDD           TO  SPA-NAI-HYOJHI
           MOVE    ZERO                TO  WRK-HI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   42
               IF      SPA-NAI-HYOJHI      =   SPA-GMN-HYODD (IDX)
                   MOVE    IDX             TO  WRK-HI
                   MOVE    98              TO  IDX
               END-IF
           END-PERFORM
           IF      WRK-HI          >   ZERO
               COMPUTE WRK-SEL-SYUU    =  (WRK-HI    -  1  )
                                       /   7    +    1
           END-IF
      *
           MOVE    WRK-SEL-SYUU        TO  SPA-SEL-SYUU
      *    予約テーブル編集処理
           PERFORM 510-YYKTBL-SET-SEC
      *
      *    予約患者一覧再編集
           COMPUTE SPA-SEL-YOUBI    =   WRK-HI
                                    -  ((SPA-SEL-SYUU  -  1 )  *  7 )
           MOVE    ZERO             TO  SPA-SEL-TIME
           PERFORM 4001-KNJICHI-SEC
      *
           .
       3101-YYKYMD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       3101-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       3101-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約テーブル編集処理
      *****************************************************************
       510-YYKTBL-SET-SEC           SECTION.
      *
           MOVE    SPA-SEL-SYUU        TO  WRK-SEL-SYUU
           INITIALIZE                  SPA-GMN-GOKKEN-G
           INITIALIZE                  SPA-GMN-KENSUU-G
           INITIALIZE                  SPA-NAI-YOYAKU-G
           INITIALIZE                  SPA-GMN-TBL
           INITIALIZE                  SPA-GMN-SELBAN
           INITIALIZE                  SPA-NAI-SELBAN
           INITIALIZE                  SPA-KAKSEL-AREA
           INITIALIZE                  SPA-NAI-TBL
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    WRK-SEL-SYUU        TO  SPA-SEL-SYUU
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   7
               MOVE    SPA-NAI-YMDR (SPA-SEL-SYUU IDX)
                                       TO  SPA-NAI-YYYKYMD  (01 IDX)
               MOVE    0800            TO  SPA-NAI-YYYKTIME (01 IDX)
               MOVE    0900            TO  SPA-NAI-YYYKTIME (02 IDX)
               MOVE    1000            TO  SPA-NAI-YYYKTIME (03 IDX)
               MOVE    1100            TO  SPA-NAI-YYYKTIME (04 IDX)
               MOVE    1200            TO  SPA-NAI-YYYKTIME (05 IDX)
               MOVE    1300            TO  SPA-NAI-YYYKTIME (06 IDX)
               MOVE    1400            TO  SPA-NAI-YYYKTIME (07 IDX)
               MOVE    1500            TO  SPA-NAI-YYYKTIME (08 IDX)
               MOVE    1600            TO  SPA-NAI-YYYKTIME (09 IDX)
               MOVE    1700            TO  SPA-NAI-YYYKTIME (10 IDX)
               MOVE    1800            TO  SPA-NAI-YYYKTIME (11 IDX)
               MOVE    1900            TO  SPA-NAI-YYYKTIME (12 IDX)
               MOVE    2000            TO  SPA-NAI-YYYKTIME (13 IDX)
               MOVE    2100            TO  SPA-NAI-YYYKTIME (14 IDX)
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   7
      *            UNTIL       IDX     >   1
               IF      SPA-NAI-YMDR (SPA-SEL-SYUU IDX)
                                       NOT =   SPACE  AND  ZERO
      *            対象見出日をカレンダーよりセット
                   MOVE    SPA-NAI-YMDR (SPA-SEL-SYUU IDX)(7:2)
                                           TO  SPA-GMN-GOKKEN (IDX)
                                                          (2:2)
      **                                                  (3:2)
      *            時間帯毎の予約を保存する
                   INITIALIZE                  YYK-REC
                   MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
                   MOVE    SPA-GMN-SRYNAIYOCD  TO  YYK-SRYNAIYO
                   MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
                   MOVE    SPA-NAI-YMDR (SPA-SEL-SYUU IDX)
                                               TO  YYK-YYKYMD
      *
                   MOVE    YYK-REC             TO  MCPDATA-REC
                   MOVE    "tbl_yyk"           TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_yyk"           TO  MCP-TABLE
                       MOVE    "key3"              TO  MCP-PATHNAME
                       PERFORM 900-YOYAKU-READ-SEC
                   ELSE
                       MOVE    1               TO  FLG-YYK
                   END-IF
      * 
                   PERFORM UNTIL       FLG-YYK     =   1
                       PERFORM 5201-YYKHEN-SEC
      *
                       MOVE    "tbl_yyk"           TO  MCP-TABLE
                       MOVE    "key3"              TO  MCP-PATHNAME
                       PERFORM 900-YOYAKU-READ-SEC
                   END-PERFORM
      *    カーソルクロース
                   MOVE    "tbl_yyk"           TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
               END-IF
           END-PERFORM
      *
           .
       510-YYKTBL-SET-EXT.
           EXIT.
      *       
      *****************************************************************
      *    時間帯毎の予約編集処理
      *****************************************************************
       5201-YYKHEN-SEC           SECTION.
      *
           EVALUATE    YYK-KEYYYKTIME
               WHEN    0800
               WHEN    0859
                   MOVE    1           TO  IDX-Y
               WHEN    0900
                   MOVE    2           TO  IDX-Y
               WHEN    1000
                   MOVE    3           TO  IDX-Y
               WHEN    1100
                   MOVE    4           TO  IDX-Y
               WHEN    1200
                   MOVE    5           TO  IDX-Y
               WHEN    1300
                   MOVE    6           TO  IDX-Y
               WHEN    1400
                   MOVE    7           TO  IDX-Y
               WHEN    1500
                   MOVE    8           TO  IDX-Y
               WHEN    1600
                   MOVE    9           TO  IDX-Y
               WHEN    1700
                   MOVE   10           TO  IDX-Y
               WHEN    1800
                   MOVE   11           TO  IDX-Y
               WHEN    1900
                   MOVE   12           TO  IDX-Y
               WHEN    2000
                   MOVE   13           TO  IDX-Y
               WHEN    2100
                   MOVE   14           TO  IDX-Y
               WHEN    OTHER
                   MOVE   ZERO         TO  IDX-Y
           END-EVALUATE
      *
           IF      YYK-YYKID           =   ZERO   AND
                   IDX-Y               >   ZERO
               MOVE    YYK-YYKMAXCNT   TO  SPA-NAI-YYYKMAXCNT
                                                         (IDX-Y IDX)
               MOVE    YYK-YYKCNT      TO  SPA-GMN-YYKCNT  (IDX-Y IDX)
           END-IF
      *
           .
        5201-YYKHEN-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＹＩＤ１）ＯＫ処理
      *****************************************************************
       30010-YID1-SET-SEC          SECTION.
      *
           EVALUATE    SPA-YID1CD
      *        予約氏名確定
               WHEN    "0101"
                   MOVE    SPACE               TO  SPA-YID1CD
                   IF      SPA-YID1-FLG         =   "OK"
                       MOVE    SPA-Y01-PTNUM       TO  SPA-GMN-PTNUM
                       MOVE    SPA-Y01-PTID        TO  SPA-GMN-PTID
                       MOVE    SPA-Y01-YYKNAME     TO  SPA-GMN-NAME
                       MOVE    SPA-Y01-SEX         TO  SPA-GMN-SEX
                       MOVE    SPA-Y01-BIRTHDAYH   TO  SPA-GMN-BIRTHDAYH
                       MOVE    SPA-Y01-YYKNAME     TO  SPA-GMN-YYKNAME
                   ELSE
                       MOVE    SPACE               TO  SPA-GMN-PTNUM
                       MOVE    ZERO                TO  SPA-GMN-PTID
                       MOVE    SPACE               TO  SPA-GMN-SEX
                       MOVE    SPACE               TO  SPA-GMN-BIRTHDAYH
                   END-IF
                   MOVE    5               TO  SPA-GMN-CUR
      *
           END-EVALUATE
           MOVE    SPACE               TO  SPA-YID1CD
           MOVE    SPACE               TO  SPA-YID1-FLG
           .
        30010-YID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       210-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
      *            MOVE ZERO             TO   FLG-END
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 220-CLEAR-SEC
      *    予約取消
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 420-YYKDEL-SEC
      *    予約
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 480-YOYAKU-SEC
      *    予約一覧へ
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 440-YYKICHI-SEC
      *    未院一覧
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-MINICHI-SEC
      *    週間一覧
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 470-KANJCHI-SEC
      *    予約日検索
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 470-PTNUM-ICHI-SEC
      *    氏名検索
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 470-SIMEIKEN-SEC
      *    受付一覧へ
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-UKEICHI-SEC
      *    予約登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOROKU-SEC
      *    予約メモ登録
               WHEN    "CLICKED"       ALSO    "B10"
               WHEN    "CLICKED"       ALSO    "B10C"
                   PERFORM 4100-Y011-SYORI-SEC
           END-EVALUATE
      *
           .
       210-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
          EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "SRYNAIYO"
      *        診療内容入力
                   PERFORM 41011-SRYNAIYO-SEC
      *       ドクター入力
               WHEN    "ACTIVATE"      ALSO    "DRNAME"
                   PERFORM 41012-DRNAME-SEC
      *        患者番号入力
               WHEN    "ACTIVATE"      ALSO    "PTNUM"
                   PERFORM 41013-PTNUM-SEC
      *        新患予約者氏名
               WHEN    "ACTIVATE"      ALSO    "YYKNAME"
                   PERFORM 41014-YYKNAME-SEC
      *        選択番号入力
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 41018-SELBAN-SEC
      *        リスト選択
               WHEN    "SELECT"        ALSO    "PTLIST"
                   PERFORM 41019-CLIST-SEC
      *        カレンダー選択
               WHEN    "CLICKED"       ALSO    "C01"
               WHEN    "CLICKED"       ALSO    "CALENDAR1"
                   PERFORM 41020-CALENDAR-SEC
      *        その他選択
               WHEN    "CLICKED"       ALSO     ANY
                   PERFORM 4101A-SELFURIWAKE-SEC
      *        一括入力分
               WHEN    OTHER
      *           入力チェック処理
                  MOVE    1                   TO  FLG-TOUROKU
                  PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    予約患者振分処理
      *****************************************************************
       4101A-SELFURIWAKE-SEC         SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET(1:6)
               WHEN    "CLICKED"       ALSO    "CH_DAY"
      *        日選択入力
                   MOVE    MCP-WIDGET(7:1)
                                       TO  WRK-GMN-GOKSEL
                   PERFORM 41016-GOKSEL-SEC
                   GO                  TO  4101A-SELFURIWAKE-EXT
           END-EVALUATE
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET(1:9)
               WHEN    "CLICKED"       ALSO    "CH_KENSUU"
      *        時間選択入力
                   MOVE    MCP-WIDGET(10:2)
                                       TO  WRK-GMN-KENSEL
                   PERFORM 41017-KENSEL-SEC
           END-EVALUATE
           .
       4101A-SELFURIWAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    Y01-YYKYMD          TO  SPA-GMN-YYKYMD
           MOVE    Y01-YYKTIME         TO  SPA-GMN-YYKTIME
           MOVE    Y01-SRYKA           TO  SPA-GMN-SRYKA-G
           MOVE    Y01-KAKREN          TO  SPA-GMN-KAKREN-G
           MOVE    Y01-YYKKBN          TO  SPA-GMN-YYKKBN-G
           MOVE    Y01-KAKREN          TO  SPA-GMN-KAKREN-G
      *
           MOVE    Y01-YYKHYO          TO  SPA-GMN-YYKHYO-G
      *
           MOVE    Y01-COMMENT1        TO  SPA-NAI-COMMENT (1)
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
      *    予約日入力
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41014-YYKYMD-SEC
           END-IF
      *    予約時間
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41015-YYKTIME-SEC
           END-IF
      *    診療科
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41016-SRYKA-SEC
           END-IF
      *    予約内容
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41017-YYKKBN-SEC
           END-IF
      *    確認連絡
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41018-KAKREN-SEC
           END-IF
      *    メモ
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41018-COMMENT-SEC
           END-IF
      *    予約票発行
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41018-YYKHYO-SEC
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療内容入力
      *****************************************************************
       41011-SRYNAIYO-SEC            SECTION.
      *
           MOVE    01                  TO  SPA-GMN-CUR
      *
           MOVE    Y01-SRYNAIYO        TO  SPA-GMN-SRYNAIYO
      *
           MOVE    ZERO               TO  FLG-HENFLG
           PERFORM VARYING IDY     FROM   1   BY  1
                   UNTIL  (IDY     >      SPA-SRYNAIYO-MAX  )  OR
                          (FLG-HENFLG     =   1  )
               IF      SPA-GMN-SRYNAIYOCD =   SPA-GMN-SRYNAIYOLST(IDY)
                                                                  (1:2)
                   MOVE    SPA-GMN-SRYNAIYOLST (IDY)
                                           TO  SPA-GMN-SRYNAIYO
                   MOVE    1               TO  FLG-HENFLG
               END-IF
           END-PERFORM
           IF      FLG-HENFLG          =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    01                  TO  SPA-GMN-CUR
               GO      TO          41011-SRYNAIYO-EXT
           END-IF
      *
           IF      SPA-GMN-SRYNAIYOCD  NOT =   SPA-MAE-SRYNAIYOCD
      *        予約表設定処理
               PERFORM 410151-HYOJI-HEN-SEC
           END-IF
           MOVE    SPA-GMN-SRYNAIYOCD  TO  SPA-MAE-SRYNAIYOCD
      *
           MOVE    2                   TO  SPA-GMN-CUR
           .
       41011-SRYNAIYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター入力
      *****************************************************************
       41012-DRNAME-SEC            SECTION.
      *
           MOVE    02                  TO  SPA-GMN-CUR
      *
           MOVE    SPA-GMN-DRCD        TO  WRK-DRCD
      *
           MOVE    Y01-DRNAME          TO  SPA-GMN-DRNAME
      *
           IF      SPA-GMN-DRCD        =   SPACE
               MOVE    "1"                 TO  SPA-NAI-DRCD1
               MOVE    ZERO                TO  SPA-NAI-DRCD2
               GO      TO      41012-DRNAME-EXT
           END-IF
      *    コード変換
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-DRCD        TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   4     )   OR
                  (SNUM-SYOKBN         =   "1"   )   OR
                  (SNUM-MINKBN         =   "1"   )
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    02                  TO  SPA-GMN-CUR
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  SPA-GMN-DRCD
           END-IF
      *
           IF      SPA-ERRCD          =   SPACE
      *        ドクターコードからの編集
               PERFORM 410121-DRCD-HENSYU-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-DRCD    NOT =   WRK-DRCD
      *            予約表設定処理
                    PERFORM 410151-HYOJI-HEN-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF
           .
       41012-DRNAME-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター入力チェック・編集処理
      *****************************************************************
       410121-DRCD-HENSYU-SEC            SECTION.
      *
           MOVE    ZERO               TO  FLG-HENFLG
           PERFORM VARYING IDY     FROM   1   BY  1
                   UNTIL  (IDY     >      SPA-DRCD-MAX  )  OR
                          (FLG-HENFLG     =   1  )
               IF      SPA-GMN-DRCD       =   SPA-GMN-DRCDLST(IDY)(1:4)
                   MOVE    SPA-GMN-DRCDLST (IDY)
                                           TO  SPA-GMN-DRNAME
                   MOVE    1               TO  FLG-HENFLG
                   MOVE    SPA-NAI-DR-SEMON (IDY)
                                           TO  WRK-NAI-DR-SEMON
               END-IF
           END-PERFORM
           IF      FLG-HENFLG          =   ZERO
      *        ドクターがない時、システム管理を検索する
               PERFORM 410121-DRCD-CHK-SEC
               IF      FLG-HENFLG          =   ZERO
                   MOVE    "0002"              TO  SPA-ERRCD
                   MOVE    02                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "1"                 TO  SPA-NAI-DRCD1
               MOVE    SPA-GMN-DRCD        TO  SPA-NAI-DRCD2
      *
      *        専門科を初期表示する（予約氏名がない時）
               IF      SPA-GMN-NAME        =   SPACE
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL       IDX     >   5
                       IF      WRK-DR-SEMONKACD (IDX)  NOT =   SPACE
                       PERFORM VARYING     IDY     FROM    1   BY  1
                               UNTIL       IDY     >   SPA-SRYKA-MAX
                           IF      WRK-DR-SEMONKACD (IDX)
                                           =   SPA-GMN-SRYKACDL(IDY)
                               MOVE    SPA-GMN-SRYKALST(IDY)
                                                   TO  SPA-GMN-SRYKA-G
                               MOVE    SPA-SRYKA-MAX   TO  IDY
                               MOVE    5               TO  IDX
                           END-IF
                       END-PERFORM
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
           .
       410121-DRCD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター入力チェック処理
      *****************************************************************
       410121-DRCD-CHK-SEC            SECTION.
      *
      *    職員情報（ドクター）
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    "1"                 TO  SYS-1010-KBNCD(1:1)
           MOVE    SPA-GMN-DRCD        TO  SYS-1010-KBNCD(2:4)
           MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD
      *
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRIT-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               MOVE    SPACE               TO  SPA-GMN-D1
               MOVE    SYS-1010-NAME       TO  SPA-GMN-DRNAMEMEI
               MOVE    SYS-1010-SEMONKACD(1)
                                           TO  WRK-DR-SEMONKACD(1)
               MOVE    SYS-1010-SEMONKACD(2)
                                           TO  WRK-DR-SEMONKACD(2)
               MOVE    SYS-1010-SEMONKACD(3)
                                           TO  WRK-DR-SEMONKACD(3)
               MOVE    SYS-1010-SEMONKACD(4)
                                           TO  WRK-DR-SEMONKACD(4)
               MOVE    SYS-1010-SEMONKACD(5)
                                           TO  WRK-DR-SEMONKACD(5)
      *        最後の行へ編集する
               IF      SPA-DRCD-MAX        =   99
                   MOVE    99              TO  SPA-DRCD-MAX
               ELSE
                   ADD     1               TO  SPA-DRCD-MAX
               END-IF
               MOVE    SPA-GMN-DRNAME      TO  SPA-GMN-DRCDLST
                                                      (SPA-DRCD-MAX)
               MOVE    WRK-NAI-DR-SEMON    TO  SPA-NAI-DR-SEMON
                                                      (SPA-DRCD-MAX)
               MOVE    1                   TO  FLG-HENFLG
           END-IF
      *
           .
       410121-DRCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号入力
      *****************************************************************
       41013-PTNUM-SEC            SECTION.
      *
           MOVE    03                  TO  SPA-GMN-CUR
           MOVE    Y01-PTNUM           TO  SPA-GMN-PTNUM
      *
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    ZERO                TO  SPA-GMN-PTID
               MOVE    SPACE               TO  SPA-GMN-NAME
               MOVE    SPACE               TO  SPA-GMN-SEX
               MOVE    SPACE               TO  SPA-GMN-BIRTHDAYH
      *        氏名入力へ
               MOVE    4               TO  SPA-GMN-CUR
               GO      TO      41013-PTNUM-EXT
           END-IF
      *
           INITIALIZE                      ORCSPTNUMAREA
           MOVE    SPA-GMN-PTNUM       TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           IF      SPTNUM-RC       NOT =   ZERO
               MOVE    "0003"           TO  SPA-ERRCD
               MOVE    ZERO             TO  SPA-GMN-PTID
               MOVE    SPACE            TO  SPA-GMN-NAME
               MOVE    SPACE            TO  SPA-GMN-SEX
               MOVE    SPACE            TO  SPA-GMN-BIRTHDAYH
               MOVE    03                  TO  SPA-GMN-CUR
               GO  TO                  41013-PTNUM-EXT
           END-IF
      *
           MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
           MOVE    SPTNUM-PTID         TO  SPA-GMN-PTID
      *
      *    患者マスター存在チェック
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTINF-PTID
           PERFORM 920-PTINF-READ-SEC
           IF      FLG-PTINF      NOT =   ZERO
               MOVE    SPACE               TO  SPA-GMN-NAME
               MOVE    ZERO                TO  SPA-GMN-PTID
               MOVE    SPACE               TO  SPA-GMN-NAME
               MOVE    SPACE               TO  SPA-GMN-SEX
               MOVE    SPACE               TO  SPA-GMN-BIRTHDAYH
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    03                  TO  SPA-GMN-CUR
               GO      TO      41013-PTNUM-EXT
           END-IF
      *
           MOVE    PTINF-NAME         TO  SPA-GMN-NAME
      *
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"                TO  SPA-GMN-SEX
               WHEN    "2"
                   MOVE    "女"                TO  SPA-GMN-SEX
           END-EVALUATE
      *
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 3101-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-BIRTHDAYH
      *
      *    排他制御処理
           PERFORM 990-LOCK-IN-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41013-PTNUM-EXT
           END-IF
      *
           IF      SPA-GMN-YYKNAME NOT =   SPA-GMN-NAME
               MOVE    SPA-GMN-NAME    TO  SPA-GMN-YYKNAME
           END-IF
           MOVE    1                   TO   SPA-YYK-SYORI
      *
           INITIALIZE                  SPA-Y01-YYK-AREA
      *
           MOVE    5                   TO  SPA-GMN-CUR
           .
       41013-PTNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    新患予約者氏名
      *****************************************************************
       41014-YYKNAME-SEC            SECTION.
      *
           MOVE    04                  TO  SPA-GMN-CUR
      *
           MOVE    Y01-YYKNAME         TO  SPA-GMN-YYKNAME
      *
      *    INITIALIZE                  SPA-Y01-YYK-AREA
      *
           IF      SPA-GMN-YYKNAME     =   SPACE
               GO      TO      41014-YYKNAME-EXT
           END-IF
      *
           IF      SPA-GMN-PTNUM   NOT =   SPACE
               IF      SPA-GMN-YYKNAME     =   SPA-GMN-NAME
                   GO      TO      41014-YYKNAME-EXT
               END-IF
           END-IF
      *
      *    予約修正
           IF      SPA-YYK-SYORI       =   2
               IF      SPA-GMN-YYKNAME     =   SPA-GMN-NAME
                   MOVE    5               TO  SPA-GMN-CUR
                   GO      TO      41014-YYKNAME-EXT
               END-IF
           END-IF
      *
      *    全角半角チェック
      *    全角チェック
           IF      SPA-ENCODING        =   2
      *        拡張漢字対応
               INITIALIZE                  ORCSKANACHK2AREA
               MOVE    "1"                 TO  KANACHK2-SYORI
               MOVE    SPA-GMN-YYKNAME     TO  KANACHK2-MAE-INPUT
               CALL    "ORCSKANACHK2"      USING
                                           ORCSKANACHK2AREA
               IF     (KANACHK2-RC     NOT =   ZERO  )  OR
                      (KANACHK2-SYUBETU    =   1     )
                   MOVE    KANACHK2-MAE-INPUT  TO  SPA-GMN-YYKNAME
                   MOVE    "0017"              TO  SPA-ERRCD
               ELSE
                   MOVE    KANACHK2-OUT-INPUT  TO  SPA-GMN-YYKNAME
               END-IF
           ELSE
      *        拡張漢字不可
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-YYKNAME     TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-RC      NOT =   ZERO  )  OR
                      (KANACHK-SYUBETU     =   1     )
                   MOVE    KANACHK2-MAE-INPUT  TO  SPA-GMN-YYKNAME
                   MOVE    "0017"              TO  SPA-ERRCD
               ELSE
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-YYKNAME
               END-IF
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41014-YYKNAME-EXT
           END-IF
      *
           MOVE    SPACE           TO  WRK-KNJ
           PERFORM 410141-KNJKEN-HEN-SEC
      *
      *
      *    予約修正中は新規予約氏名のみ変更
           IF      SPA-YYK-SYORI       =   2
               MOVE    SPA-GMN-YYKNAME     TO  SPA-GMN-NAME
               MOVE    5                   TO  SPA-GMN-CUR
               IF     (IDZ                 =   1     ) AND
                      (WRK-PTID        NOT =   SPACE ) AND
                     ((WRK-NAME            =   SPA-GMN-YYKNAME) OR
                      (WRK-KANANAME        =   SPA-GMN-YYKNAME))
                   MOVE    "0020"          TO  SPA-ERRCD
                   MOVE    4               TO  SPA-GMN-CUR
               END-IF
               GO      TO      41014-YYKNAME-EXT
           END-IF
      *
           INITIALIZE                  SPA-Y01-YYK-AREA
      *
      *    IF      IDZ                  =   ZERO
      *        MOVE    "0004"          TO  SPA-ERRCD
      *        GO      TO      41014-YYKNAME-EXT
      *    END-IF
      *
           IF      IDZ                  >   1
      *        氏名検索へ
               MOVE    SPA-GMN-YYKNAME     TO  SPA-Y01-YYKNAME
               PERFORM 470-SIMEIKEN-SEC
           ELSE
               IF      WRK-PTID           =   SPACE
                   MOVE    ZERO                TO  SPA-GMN-PTID
                   MOVE    SPACE               TO  SPA-GMN-PTNUM
                   MOVE    SPACE               TO  SPA-GMN-NAME
                   MOVE    SPACE               TO  SPA-GMN-SEX
                   MOVE    SPACE               TO  SPA-GMN-BIRTHDAYH
               ELSE
      *        患者マスタに予約者あり
                   IF     (WRK-NAME            =   SPA-GMN-YYKNAME) OR
                          (WRK-KANANAME        =   SPA-GMN-YYKNAME)
                       CONTINUE
                   ELSE
                       MOVE    WRK-PTID        TO  SPA-Y01-PTID
                       MOVE    WRK-PTNUM       TO  SPA-Y01-PTNUM
                       MOVE    WRK-SEX         TO  SPA-Y01-SEX
                       MOVE    WRK-BIRTHDAYH   TO  SPA-Y01-BIRTHDAYH
                       MOVE    WRK-NAME        TO  SPA-Y01-YYKNAME
      *
                       MOVE    "0101"          TO  SPA-YID1CD
                       GO      TO      41014-YYKNAME-EXT
                   END-IF
      *
                   MOVE    WRK-PTID        TO  SPA-GMN-PTID
                   MOVE    WRK-PTNUM       TO  SPA-GMN-PTNUM
                   MOVE    WRK-NAME        TO  SPA-GMN-NAME
                   MOVE    WRK-SEX         TO  SPA-GMN-SEX
                   MOVE    WRK-BIRTHDAYH   TO  SPA-GMN-BIRTHDAYH
                   MOVE    WRK-NAME        TO  SPA-GMN-YYKNAME
      *
      *            排他制御処理
                   PERFORM 990-LOCK-IN-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                        GO      TO      41014-YYKNAME-EXT
                   END-IF
               END-IF
               MOVE    5               TO  SPA-GMN-CUR
           END-IF
           MOVE    1                   TO   SPA-YYK-SYORI
           .
       41014-YYKNAME-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者検索処理
      *****************************************************************
       410141-KNJKEN-HEN-SEC        SECTION.
      *
           MOVE    ZERO                TO  IDZ
      *
           MOVE    SPACE               TO  WRK-NAME-KEY
      *    漢字検索
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
      ***  STRING  SPA-GMN-YYKNAME     DELIMITED  BY  SPACE
      *            "%"                 DELIMITED  BY  SIZE
      **************************       INTO    PTINF-NAME
      *                                INTO    WRK-NAME-KEY
      ***  END-STRING
      *    部分検索
           PERFORM 410141-NAME-HEN-SEC
           MOVE    WRK-NAME-KEY        TO  PTINF-NAME
           MOVE    WRK-NAME-KEY        TO  PTINF-KANANAME
      *
      *    患者マスターを漢字氏名で検索
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-PTINF-READ-SEC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1                   TO  FLG-PTINF
           END-IF
      *
           PERFORM UNTIL      (IDZ             >   1         ) OR
                              (FLG-PTINF       =   1         )
               PERFORM 410141-PTINF-HENSYU-SEC
      *
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-PTINF-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       410141-KNJKEN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名編集処理
      *****************************************************************
       410141-NAME-HEN-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-NAME1
           MOVE    SPACE               TO  WRK-NAME2
           MOVE    SPA-GMN-YYKNAME     TO  WRK-NAME-KEY
      *    ＊の次の　削除
           INITIALIZE                  ORCSSTRINGAREA
           MOVE    "sub"               TO  ORCSSTR-COMMAND
           MOVE    WRK-NAME-KEY        TO  ORCSSTR-STRING1
           MOVE    "＊　"              TO  ORCSSTR-STRING2
           MOVE    "＊"                TO  ORCSSTR-STRING3
           CALL    "orcsstring"        USING
                                       ORCSSTRINGAREA
           MOVE    ORCSSTR-STRING1     TO  WRK-NAME-KEY
      *    ＊は名前とする
           INITIALIZE                  ORCSSTRINGAREA
           MOVE    "search"            TO  ORCSSTR-COMMAND
           MOVE    WRK-NAME-KEY        TO  ORCSSTR-STRING1
           MOVE    "＊"                TO  ORCSSTR-STRING2
           CALL    "orcsstring"        USING
                                       ORCSSTRINGAREA
           IF      ORCSSTR-NUM1        =   ZERO
               MOVE    WRK-NAME-KEY        TO  WRK-NAME1
           ELSE
               MOVE    ORCSSTR-NUM1        TO  IDX-S1
               MOVE    ORCSSTR-NUM2        TO  IDX-S2
               IF      ORCSSTR-NUM1        =   1
                   MOVE    SPACE               TO  WRK-NAME1
               ELSE
                   INITIALIZE                  ORCSSTRINGAREA
                   MOVE    "substr"            TO  ORCSSTR-COMMAND
                   MOVE    WRK-NAME-KEY        TO  ORCSSTR-STRING1
                   MOVE    1                   TO  ORCSSTR-NUM1
                   COMPUTE ORCSSTR-NUM2        =   IDX-S1
                                               -   1
                   CALL    "orcsstring"        USING
                                               ORCSSTRINGAREA
                   MOVE    ORCSSTR-STRING2     TO  WRK-NAME1
               END-IF
      *
               INITIALIZE                  ORCSSTRINGAREA
               MOVE    "substr"            TO  ORCSSTR-COMMAND
               MOVE    WRK-NAME-KEY        TO  ORCSSTR-STRING1
               COMPUTE ORCSSTR-NUM1        =   IDX-S1
                                           +   1
               MOVE    0                   TO  ORCSSTR-NUM2
               CALL    "orcsstring"        USING
                                           ORCSSTRINGAREA
               MOVE    ORCSSTR-STRING2     TO  WRK-NAME2
           END-IF
      *
           IF     (WRK-NAME1       NOT =   SPACE ) AND
                  (WRK-NAME2           =   SPACE )
               MOVE    SPACE               TO  WRK-NAME-KEY
               STRING  WRK-NAME1       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
                                       INTO    WRK-NAME-KEY
               END-STRING
           END-IF
           IF     (WRK-NAME1       NOT =   SPACE ) AND
                  (WRK-NAME2       NOT =   SPACE )
               MOVE    SPACE               TO  WRK-NAME-KEY
               STRING  WRK-NAME1       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
                       "　"            DELIMITED  BY  SIZE
                       WRK-NAME2       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
                                       INTO    WRK-NAME-KEY
               END-STRING
           END-IF
           IF     (WRK-NAME1           =   SPACE ) AND
                  (WRK-NAME2       NOT =   SPACE )
               MOVE    SPACE               TO  WRK-NAME-KEY
               STRING  "%"             DELIMITED  BY  SIZE
                       "　"            DELIMITED  BY  SIZE
                       WRK-NAME2       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
                                       INTO    WRK-NAME-KEY
               END-STRING
           END-IF
           .
       410141-NAME-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者マスタ編集処理
      *****************************************************************
       410141-PTINF-HENSYU-SEC          SECTION.
      *
      *    患者番号変換より、患者NOより患者IDを取得する
      *
           ADD     1                   TO  IDZ
           MOVE    PTINF-PTID          TO  WRK-PTID
      *
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    PTINF-PTID          TO  SPTID-PTID
           CALL    "ORCSPTID"          USING  ORCSPTIDAREA
                                              SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               MOVE    SPACE           TO  WRK-PTNUM
           ELSE
               MOVE    SPTID-PTNUM     TO  WRK-PTNUM
           END-IF
      *
           MOVE    PTINF-NAME         TO  WRK-NAME
           MOVE    PTINF-KANANAME     TO  WRK-KANANAME
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"                TO  WRK-SEX
               WHEN    "2"
                   MOVE    "女"                TO  WRK-SEX
           END-EVALUATE
      *
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 3101-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-BIRTHDAYH
           .
       410141-PTINF-HENSYU-EXT.
           EXIT.
      *       
      *****************************************************************
      *    予約日入力
      *****************************************************************
       41014-YYKYMD-SEC             SECTION.
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN-YYKYMD      TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN-YYKYMD
               MOVE    SGDAY-SDAY          TO  SPA-NAI-YYKYMD
           ELSE
               MOVE    "0007"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-CUR
               GO      TO      41014-YYKYMD-EXT
           END-IF
      *
      *    一覧表より選択時、予約時間へ
           IF      SPA-YYK-SYORI       =   2
      *********MOVE    6                   TO  SPA-GMN-CUR
               CONTINUE
           ELSE
               IF     (SPA-NAI-YYKYMD  NUMERIC   )  AND
                      (SPA-GMN-YYKYMD  NOT =   SPA-GMN-TAIYMD )
                   MOVE    SPA-GMN-YYKTIME     TO  WRK-GMN-YYKTIME
                   MOVE    SPA-NAI-YYKYY       TO  Y01-YEAR
                   MOVE    SPA-NAI-YYKMM       TO  Y01-MONTH
                   MOVE    SPA-NAI-YYKDD       TO  Y01-DAY
                   PERFORM 41020-CALENDAR-SEC
                   IF      WRK-GMN-YYKTIME     NOT =   SPACE
                       MOVE    WRK-GMN-YYKTIME     TO  SPA-GMN-YYKTIME
                   END-IF
               END-IF
           END-IF
      *
           .
       41014-YYKYMD-EXT.
           EXIT.
      *       
      *****************************************************************
      *    予約時間入力
      *****************************************************************
       41015-YYKTIME-SEC             SECTION.
      *
      **   MOVE    6                   TO  SPA-GMN-CUR
      *
      **   MOVE    Y01-YYKTIME         TO  SPA-GMN-YYKTIME
           IF     (SPA-GMN-YYKTIME     =   SPACE )  AND
                  (FLG-TOUROKU         =   1     )
               GO      TO      41015-YYKTIME-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-TIME-X
           MOVE    ZERO                TO  WRK-TIME
           MOVE    ZERO                TO  IDX-T1
           MOVE    ZERO                TO  IDX-T2
           MOVE    ZERO                TO  FLG-TIME
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   5   )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-YYKTIME(IDX:1)  NOT =   SPACE
                   IF      SPA-GMN-YYKTIME(IDX:1)  =   ":"
                                                   OR  ","
                                                   OR  "."
                       MOVE    1               TO  FLG-TIME
                   ELSE
                     IF      FLG-TIME            =   ZERO
      *                時
                       ADD     1               TO  IDX-T1
                       IF      IDX-T1          >   2
                           MOVE    1               TO  FLG-TIME
                       ELSE
                           MOVE    SPA-GMN-YYKTIME(IDX:1)
                                               TO  WRK-THHX(IDX-T1:1)
                       END-IF
                     END-IF
                     IF      FLG-TIME          =   1
      *                分
                       ADD     1               TO  IDX-T2
                       IF      IDX-T2          >   2
                           MOVE    "0008"          TO  SPA-ERRCD
                       ELSE
                           MOVE    SPA-GMN-YYKTIME(IDX:1)
                                               TO  WRK-TSSX(IDX-T2:1)
                       END-IF
                     END-IF
                   END-IF
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    6                   TO  SPA-GMN-CUR
               GO    TO      41015-YYKTIME-EXT
           END-IF
      *
      *    予約時
           INITIALIZE                      ORCSNUMAREA
           MOVE    2                   TO  SNUM-KETSUU
           MOVE    WRK-THHX            TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-GMN-CUR
               GO      TO      41015-YYKTIME-EXT
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-THH
           END-IF
      *    予約分
           INITIALIZE                      ORCSNUMAREA
           MOVE    2                   TO  SNUM-KETSUU
           MOVE    WRK-TSSX            TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-GMN-CUR
               GO      TO      41015-YYKTIME-EXT
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-TSS
           END-IF
      *
           MOVE    WRK-THH             TO  SPA-GMN-YYKTIME(1:2)
           MOVE    ":"                 TO  SPA-GMN-YYKTIME(3:1)
           MOVE    WRK-TSS             TO  SPA-GMN-YYKTIME(4:2)
      *
           IF      WRK-TSS             >=  60
               MOVE    "0011"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-GMN-CUR
               GO      TO      41015-YYKTIME-EXT
           END-IF
           IF      WRK-THH             >   24
               MOVE    "0012"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-GMN-CUR
               GO      TO      41015-YYKTIME-EXT
           END-IF
      *    予約時間
           MOVE    WRK-TIME            TO  SPA-NAI-YYKTIME
           MOVE    WRK-TIME            TO  SPA-NAI-KYYKTIME
           IF      SPA-NAI-YYKTIME(1:2)    <=  "08"
               MOVE    "08"                TO  SPA-NAI-KYYKTIME(1:2)
           END-IF
           IF      SPA-NAI-YYKTIME(1:2)    >=  "21"
               MOVE    "21"                TO  SPA-NAI-KYYKTIME(1:2)
           END-IF
      *    キー予約時間
      *****MOVE    SPA-NAI-YYKTIME         TO  SPA-NAI-KYYKTIME(1:2)
           MOVE    ZERO                    TO  SPA-NAI-KYYKTIME(3:2)
      *
           IF      (SPA-NAI-YYKYMD     =   SPA-Y01-YYKYMDS )  AND
                   (SPA-NAI-KYYKTIME   =   SPA-Y01-YYKTIME )
               CONTINUE
           ELSE
               IF      (SPA-NAI-YYKYMD      =   SPA-CHK-YYKYMD )  AND
                       (SPA-NAI-KYYKTIME    =   SPA-CHK-KYYKTIME)
                   CONTINUE
               ELSE
      *            予約件数チェック
                   PERFORM 410161-YYKKEN-CHK-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    6                   TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
      *****MOVE    7                   TO  SPA-GMN-CUR
      *
           .
       41015-YYKTIME-EXT.
           EXIT.
      * 
      *****************************************************************
      *    予約件数チェック
      *****************************************************************
       410161-YYKKEN-CHK-SEC        SECTION.
      *
           MOVE    SPACE               TO  SPA-CHK-YYKYMD
           MOVE    ZERO                TO  SPA-CHK-KYYKTIME
      *
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    SPA-GMN-SRYNAIYO    TO  YYK-SRYNAIYO
           MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
           MOVE    SPA-NAI-YYKYMD      TO  YYK-YYKYMD
           MOVE    SPA-NAI-KYYKTIME    TO  YYK-KEYYYKTIME
           MOVE    ZERO                TO  YYK-YYKID
           PERFORM 910-YOYAKU-INV-SEC
           IF      FLG-YYK             =   ZERO
      *        最大件数修正（システム日付より後の日）
               IF     (YYK-YYKYMD          >=  SPA-SYSYMD )
                 AND  (YYK-YYKMAXCNT   NOT =   SPA-Y01-YYKMAXCNT)
                   MOVE    SPA-Y01-YYKMAXCNT   TO  YYK-YYKMAXCNT
               END-IF
               IF      YYK-YYKMAXCNT       <=  YYK-YYKCNT
                   MOVE    SPA-NAI-YYKYMD      TO  SPA-CHK-YYKYMD
                   MOVE    SPA-NAI-KYYKTIME    TO  SPA-CHK-KYYKTIME
                   MOVE    "K100"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       410161-YYKKEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療科入力
      *****************************************************************
       41016-SRYKA-SEC            SECTION.
      *
      **** MOVE    7                   TO  SPA-GMN-CUR
      *
      **** MOVE    Y01-SRYKA           TO  SPA-GMN-SRYKA-G
      *
           MOVE    ZERO               TO  FLG-HENFLG
           PERFORM VARYING IDY     FROM   1   BY  1
                   UNTIL  (IDY     >      SPA-SRYKA-MAX  )  OR
                          (FLG-HENFLG     =   1  )
               IF      SPA-GMN-SRYKA      =   SPA-GMN-SRYKACDL  (IDY)
                   MOVE    SPA-GMN-SRYKALST (IDY)
                                           TO  SPA-GMN-SRYKA-G
                   MOVE    1               TO  FLG-HENFLG
               END-IF
           END-PERFORM
      *    選択なしはエラー
           IF      FLG-HENFLG          =   ZERO
               MOVE    "0009"              TO  SPA-ERRCD
               MOVE    7                   TO  SPA-GMN-CUR
               GO      TO          41016-SRYKA-EXT
           END-IF
      *
      *****MOVE    8                   TO  SPA-GMN-CUR
           .
       41016-SRYKA-EXT.
           EXIT.
      *****************************************************************
      *    予約内容入力
      *****************************************************************
       41017-YYKKBN-SEC            SECTION.
      *
      *****MOVE    8                   TO  SPA-GMN-CUR
      *
      *****MOVE    Y01-YYKKBN        TO  SPA-GMN-YYKKBN-G
           MOVE    ZERO               TO  FLG-HENFLG
           PERFORM VARYING IDY     FROM   1   BY  1
                   UNTIL  (IDY     >      SPA-YYKKBN-MAX  )  OR
                          (FLG-HENFLG     =   1  )
               IF      SPA-GMN-YYKKBN     =   SPA-GMN-YYKKBNLST (IDY)
                                                                  (1:2)
                   MOVE    SPA-GMN-YYKKBNLST (IDY)
                                           TO  SPA-GMN-YYKKBN-G
                   MOVE    1               TO  FLG-HENFLG
               END-IF
           END-PERFORM
      *    選択なしはエラー
           IF      FLG-HENFLG          =   ZERO
               MOVE    "0010"              TO  SPA-ERRCD
               MOVE    8                   TO  SPA-GMN-CUR
               GO      TO          41017-YYKKBN-EXT
           END-IF
      *
           .
       41017-YYKKBN-EXT.
           EXIT.
      *****************************************************************
      *    確認連絡入力
      *****************************************************************
       41018-KAKREN-SEC            SECTION.
      *
           MOVE    ZERO               TO  FLG-HENFLG
           PERFORM VARYING IDY     FROM   1   BY  1
                   UNTIL  (IDY     >      SPA-KAKREN-MAX  )  OR
                          (FLG-HENFLG     =   1  )
               IF      SPA-GMN-KAKREN     =   SPA-GMN-KAKRENLST (IDY)
                                                                  (1:2)
                   MOVE    SPA-GMN-KAKRENLST (IDY)
                                           TO  SPA-GMN-KAKREN-G
                   MOVE    1               TO  FLG-HENFLG
               END-IF
           END-PERFORM
      *
           .
       41018-KAKREN-EXT.
           EXIT.
      *****************************************************************
      *    メモ入力
      *****************************************************************
       41018-COMMENT-SEC            SECTION.
      *
           IF      SPA-NAI-COMMENT(1)(1:1) =   "/"
               MOVE    SPACE               TO  SPA-NAI-COMMENT(1)
               PERFORM 41002-Y012-SET-SEC
           ELSE
               IF      SPA-NAI-COMMENT(1)      =   SPACE
                   IF      SPA-NAI-COMMENT(2)  NOT =   SPACE
                       PERFORM 410180-COMMENT-DEL-SEC
                   END-IF
               ELSE
      *            全角チェック
                   PERFORM 410181-COMMENT-CHEK-SEC
               END-IF
           END-IF
           IF     (SPA-ERRCD           =   SPACE )
             AND  (FLG-END             =   ZERO  )
             AND  (WRK-MCP-WIDGET      =   "COMMENT1" )
               IF     (SPA-NAI-COMMENT(1)  =   SPACE )
                  AND (SPA-NAI-YYKID       =   ZERO  )
                  AND (SPA-NAI-YYKCOPY     =   ZERO  )
      *            雛形複写
                   PERFORM 410189-YYKEXAMPLE-HEN-SEC
                   MOVE    1               TO  SPA-NAI-YYKCOPY
               END-IF
           END-IF
           .
       41018-COMMENT-EXT.
           EXIT.
      *****************************************************************
      *    メモ空白削除処理
      *****************************************************************
       410180-COMMENT-DEL-SEC                SECTION.
      *
           PERFORM VARYING     IDX-C   FROM    1   BY  1
                   UNTIL       IDX-C   >=  6
               IF      SPA-NAI-COMMENT(IDX-C)  =   SPACE
                   MOVE    SPA-NAI-COMMENT (IDX-C + 1 )
                                        TO  SPA-NAI-COMMENT(IDX-C)
                   MOVE    SPACE        TO  SPA-NAI-COMMENT (IDX-C + 1 )
               END-IF
           END-PERFORM
           .
       410180-COMMENT-DEL-EXT.
           EXIT.
      *****************************************************************
      *    メモチェック処理
      *****************************************************************
       410181-COMMENT-CHEK-SEC                SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-NAI-COMMENT(1)  TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )
               MOVE    KANACHK-MAE-INPUT   TO  SPA-NAI-COMMENT(1)
               MOVE    "0019"              TO  SPA-ERRCD
               MOVE    13                  TO  SPA-GMN-CUR
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  SPA-NAI-COMMENT(1)
           END-IF
           .
       410181-COMMENT-CHEK-EXT.
           EXIT.
      *****************************************************************
      *    予約文例複写処理
      *****************************************************************
       410189-YYKEXAMPLE-HEN-SEC                SECTION.
      *
           INITIALIZE                  YYKEXAMPLE-REC
           MOVE    SPA-HOSPNUM         TO  YYKEXAMPLE-HOSPNUM
      *
           MOVE    YYKEXAMPLE-REC      TO  MCPDATA-REC
           MOVE    "tbl_yykexample"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_yykexample"    TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-YYKEXAMPLE-READ-SEC
           ELSE
               INITIALIZE                  YYKEXAMPLE-REC
               MOVE    1               TO  FLG-YYKEXAMPLE
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM
                   UNTIL      (IDX     >=  6           )  OR
                              (FLG-YYKEXAMPLE  =   1   )
               IF      YYKEXAMPLE-HYOKBN   NOT =   ZERO
                   ADD     1                  TO  IDX
                   MOVE    YYKEXAMPLE-COMMENT TO  SPA-NAI-COMMENT
                                                                (IDX)
               END-IF
               MOVE    "tbl_yykexample"    TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-YYKEXAMPLE-READ-SEC
           END-PERFORM
           MOVE    "tbl_yykexample"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       410189-YYKEXAMPLE-HEN-EXT.
           EXIT.
      *****************************************************************
      *    予約票区分入力
      *****************************************************************
       41018-YYKHYO-SEC                SECTION.
      *
           MOVE    ZERO               TO  FLG-HENFLG
           PERFORM VARYING IDY     FROM   1   BY  1
                   UNTIL  (IDY     >      SPA-YYKHYO-MAX  )  OR
                          (FLG-HENFLG     =   1  )
               IF      SPA-GMN-YYKHYO     =   SPA-GMN-YYKHYO-LIST(IDY)
                                                                 (1:1)
                   MOVE    SPA-GMN-YYKHYO-LIST(IDY)
                                           TO  SPA-GMN-YYKHYO-G
                   MOVE    1               TO  FLG-HENFLG
               END-IF
           END-PERFORM
      *
      *    選択エラーは初期表示へ
           IF      FLG-HENFLG          =   ZERO
               MOVE    SPA-Y01-YYKHYO-KBN  TO  SPA-GMN-YYKHYO
               PERFORM VARYING IDY     FROM   1   BY  1
                       UNTIL  (IDY     >      SPA-YYKHYO-MAX  )
                   IF      SPA-GMN-YYKHYO     =   SPA-GMN-YYKHYO-LIST
                                                             (IDY) (1:1)
                       MOVE    SPA-GMN-YYKHYO-LIST(IDY)
                                               TO  SPA-GMN-YYKHYO-G
                       MOVE    SPA-YYKHYO-MAX  TO  IDY
                   END-IF
               END-PERFORM
           END-IF
           .
       41018-YYKHYO-EXT.
           EXIT.
      *****************************************************************
      *    日選択入力
      *****************************************************************
       410151-HYOJI-HEN-SEC        SECTION.
      *
           MOVE    ZERO                TO  WRK-HI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   42
               IF      SPA-NAI-HYOJHI      =   SPA-GMN-HYODD (IDX)
                   MOVE    IDX             TO  WRK-HI
                   MOVE    98              TO  IDX
               END-IF
           END-PERFORM
           IF      WRK-HI              >   ZERO
               COMPUTE WRK-SEL-SYUU    =  (WRK-HI    -  1  )
                                       /   7    +    1
           ELSE
               MOVE    "0018"          TO  SPA-ERRCD
               GO      TO      410151-HYOJI-HEN-EXT
           END-IF
      *    予約テーブル編集
           INITIALIZE                  SPA-SEL-AREA
           MOVE    WRK-SEL-SYUU        TO  SPA-SEL-SYUU
           PERFORM 510-YYKTBL-SET-SEC
      *
      *    予約患者一覧再編集
           COMPUTE SPA-SEL-YOUBI    =   WRK-HI
                                    -  ((SPA-SEL-SYUU  -  1 )  *  7 )
           MOVE    ZERO             TO  SPA-SEL-TIME
           PERFORM 4001-KNJICHI-SEC
      *
           .
       410151-HYOJI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    日選択入力
      *****************************************************************
       41016-GOKSEL-SEC             SECTION.
      *
           MOVE    WRK-GMN-GOKSEL   TO  SPA-SEL-YOUBI
      *****COMPUTE SPA-SEL-YOUBI = WRK-GMN-GOKSEL * 1
           MOVE    ZERO             TO  SPA-SEL-TIME
      *    予約患者一覧再編集
           PERFORM 4001-KNJICHI-SEC
      *
      *    予約処理中でなく、選択数がある時、選択番号へ
           IF      SPA-YYK-SYORI       =   ZERO
               IF      SPA-GMN-MAX         >   ZERO
                   MOVE    12              TO  SPA-GMN-CUR
               ELSE
                   MOVE    3               TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    5               TO  SPA-GMN-CUR
           END-IF
           .
       41016-GOKSEL-EXT.
           EXIT.
      *       
      *****************************************************************
      *    予約患者一覧再編集
      *****************************************************************
       4001-KNJICHI-SEC            SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
           INITIALIZE                  SPA-GMN-TBL
           INITIALIZE                  SPA-NAI-TBL
           INITIALIZE                  SPA-GMN-SELBAN
           INITIALIZE                  SPA-NAI-SELBAN
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
      *
           MOVE    SPA-SEL-YOUBI       TO  IDX-Y
           IF      SPA-SEL-TIME        =   ZERO
               MOVE    1               TO  IDX-T
           ELSE
               MOVE    SPA-SEL-TIME    TO  IDX-T
           END-IF
      *
      *    対象日を予約日へ
           MOVE    SPA-NAI-YYYKYMD (01 IDX-Y)
                                       TO  SPA-NAI-YYKYMD
      *
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    SPA-GMN-SRYNAIYOCD  TO  YYK-SRYNAIYO
           MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
           MOVE    SPA-NAI-YYYKYMD (01 IDX-Y)
                                       TO  YYK-YYKYMD
           IF      SPA-SEL-TIME        =   ZERO
               MOVE    ZERO            TO  WRK-KEY-YYKTIME
           ELSE
               MOVE    SPA-NAI-YYYKTIME (IDX-T IDX-Y)
                                       TO  WRK-KEY-YYKTIME
           END-IF
           MOVE    WRK-KEY-YYKTIME     TO  YYK-KEYYYKTIME
           MOVE    ZERO                TO  YYK-YYKID
      *    時間帯毎の予約を保存する
           INITIALIZE                  YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    SPA-GMN-SRYNAIYOCD  TO  YYK-SRYNAIYO
           MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
           MOVE    SPA-NAI-YYYKYMD (01 IDX-Y)
                                       TO  YYK-YYKYMD
           IF      SPA-SEL-TIME        =   ZERO
      *        MOVE    ZERO            TO  YYK-KEYYYKTIME
               MOVE    "key3"          TO  WRK-PATH
           ELSE
               MOVE    SPA-NAI-YYYKTIME (IDX-T IDX-Y)
                                       TO  YYK-KEYYYKTIME
               MOVE    "key2"          TO  WRK-PATH
           END-IF
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 900-YOYAKU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-YYK
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM 
               UNTIL     (FLG-YYK          =   1       )
                     OR  (IDX              >=  200     )
      *
               IF      YYK-YYKID       =   ZERO
                   CONTINUE
               ELSE
                   ADD     1               TO  IDX
                   MOVE    IDX             TO  SPA-GMN-TBANGO   (IDX)
                   MOVE    YYK-YYKTIME     TO  SPA-GMN-TYYKTIME (IDX)
                   MOVE    YYK-NAME        TO  SPA-GMN-TNAME    (IDX)
                   PERFORM 41002-SRYKAMEI-SET-SEC
                   IF      YYK-RAIINZUMI   =   "1"
                       MOVE    "来院"      TO  SPA-GMN-TRAIINZUMI(IDX)
                   END-IF
                   MOVE    YYK-PTID        TO  SPA-NAI-TPTID   (IDX)
                   IF      YYK-PTID    NOT =   ZERO
                       INITIALIZE                      ORCSPTIDAREA
                       MOVE    YYK-HOSPNUM         TO  SPTID-HOSPNUM
                       MOVE    YYK-PTID            TO  SPTID-PTID
                       CALL    "ORCSPTID"      USING   ORCSPTIDAREA
      *----(01.01.01) LINE ADD START ----------------------------------
                                                       SPA-AREA
      *----(01.01.01) LINE ADD END   ----------------------------------
                       IF      SPTID-RC        =   ZERO
                            MOVE    SPTID-PTNUM    TO  SPA-NAI-TPTNUM
                                                                 (IDX)
                       END-IF
                   END-IF
                   MOVE    YYK-KEYYYKTIME  TO  SPA-NAI-TKYYKTIME (IDX)
                   MOVE    YYK-YYKYMD      TO  SPA-NAI-TYYKYMD  (IDX)
                   MOVE    YYK-YYKID       TO  SPA-NAI-TYYKID   (IDX)
                   MOVE    YYK-SRYKA       TO  SPA-NAI-TSRYKA   (IDX)
                   MOVE    YYK-SRYKA       TO  SPA-NAI-TSRYKA   (IDX)
                   MOVE    YYK-YYKKBN      TO  SPA-NAI-TYYKKBN  (IDX)
                   MOVE    YYK-KAKREN      TO  SPA-NAI-TKAKREN  (IDX)
      *
                   MOVE    IDX             TO  SPA-YYK-MAX
               END-IF
      *
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 900-YOYAKU-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    IDX             TO  SPA-GMN-MAX
      *
           PERFORM 40011-TBLSORT-SEC 
      *
      *     対象時間表示
           IF      WRK-KEY-YYKTIME     =   ZERO
               MOVE    SPACE                   TO  SPA-GMN-TAITIME
               MOVE    ZERO                    TO  SPA-NAI-KYYKTIME
               MOVE    SPACE                   TO  SPA-GMN-YYKTIME
               MOVE    ZERO                    TO  SPA-NAI-YYKTIME
           ELSE
               MOVE    WRK-KEY-YYKTIME(1:2)    TO  SPA-GMN-TAITIME(1:2)
               MOVE    ":"                     TO  SPA-GMN-TAITIME(3:1)
               MOVE    WRK-KEY-YYKTIME(3:2)    TO  SPA-GMN-TAITIME(4:2)
      *        予約時間の表示
               MOVE    WRK-KEY-YYKTIME         TO  SPA-NAI-KYYKTIME
               MOVE    SPA-GMN-TAITIME         TO  SPA-GMN-YYKTIME
               MOVE    WRK-KEY-YYKTIME         TO  SPA-NAI-YYKTIME
           END-IF
      *
      *     対象日表示
           MOVE    SPA-NAI-YYYKYMD (01 IDX-Y)
                                       TO  WRK-SYMD
           PERFORM 3101-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-TAIYMD
      *
           MOVE    SPA-GMN-TAIYMD      TO  SPA-GMN-YYKYMD
      *
           .
       4001-KNJICHI-EXT.
           EXIT.
      *       
      *****************************************************************
      *    科名設定処理
      *****************************************************************
       41002-SRYKAMEI-SET-SEC             SECTION.
      *
           MOVE    ZERO               TO  FLG-HENFLG
           PERFORM VARYING IDY     FROM   1   BY  1
                   UNTIL  (IDY     >      SPA-SRYKA-MAX  )  OR
                          (FLG-HENFLG     =   1  )
               IF      YYK-SRYKA          =   SPA-GMN-SRYKACDL  (IDY)
                   MOVE    SPA-NAI-SRYKAMEI3  (IDY)
                                           TO  SPA-GMN-TKAMEI  (IDX)
                   MOVE    1               TO  FLG-HENFLG
               END-IF
           END-PERFORM
      *
           IF      FLG-HENFLG          =   ZERO
      *        テーブルにないとき、システム管理より
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    YYK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    YYK-YYKYMD          TO  SYS-1005-STYUKYMD
           MOVE    YYK-YYKYMD          TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRIT-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME3 TO  SPA-GMN-TKAMEI  (IDX)
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           END-IF
           .
       41002-SRYKAMEI-SET-EXT.
           EXIT.
      *       
      *****************************************************************
      *    患者一覧ＳＯＲＴ処理
      *****************************************************************
       40011-TBLSORT-SEC             SECTION.
      *
           INITIALIZE                  WRK-SORT-AREA
           MOVE    SPA-GMN-TBL         TO  WRK-GMN-TBL
           MOVE    SPA-NAI-TBL         TO  WRK-NAI-TBL
           INITIALIZE                  SPA-GMN-TBL
           INITIALIZE                  SPA-NAI-TBL
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
               MOVE    HIGH-VALUE          TO  WRK-YYKTIME
               MOVE    ZERO                TO  WRK-IDX
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   SPA-GMN-MAX
                   IF      WRK-GMN-TBANGO (IDY)  =   ZERO
                       CONTINUE
                   ELSE
                       IF      WRK-GMN-TYYKTIME (IDY)  <  WRK-YYKTIME
                           MOVE    WRK-GMN-TYYKTIME (IDY)
                                               TO  WRK-YYKTIME
                           MOVE    IDY         TO  WRK-IDX
                       END-IF
                   END-IF
               END-PERFORM
               IF      WRK-IDX         >   ZERO
                   MOVE    WRK-GMN-TBLO (WRK-IDX)
                                           TO  SPA-GMN-TBLO(IDX)
                   MOVE    WRK-NAI-TBLO (WRK-IDX)
                                           TO  SPA-NAI-TBLO(IDX)
                   MOVE    IDX             TO  SPA-GMN-TBANGO (IDX)
      *
      *            INITIALIZE              WRK-GMN-TBLO (WRK-IDX)
      *            INITIALIZE              WRK-NAI-TBLO (WRK-IDX)
                   MOVE    SPACE           TO  WRK-GMN-TBLO (WRK-IDX)
                   MOVE    ZERO            TO  WRK-GMN-TBANGO (WRK-IDX)
                   MOVE    SPACE           TO  WRK-NAI-TBLO   (WRK-IDX)
                   MOVE    ZERO            TO  WRK-NAI-TKYYKTIME
                                                              (WRK-IDX)
                   MOVE    ZERO            TO  WRK-NAI-TYYKID
                                                              (WRK-IDX)
               END-IF
           END-PERFORM
           .
       40011-TBLSORT-EXT.
           EXIT.
      *       
      *****************************************************************
      *    時間選択入力
      *****************************************************************
       41017-KENSEL-SEC             SECTION.
      *
           COMPUTE SPA-SEL-TIME     =  (WRK-GMN-KENSEL -  1 )
                                    /   7    +  1
           COMPUTE WRK-KEI          =   SPA-SEL-TIME   *  7
           COMPUTE SPA-SEL-YOUBI    =   7
                                    -  (WRK-KEI    -   WRK-GMN-KENSEL)
      *
      *    予約患者一覧再編集
           PERFORM 4001-KNJICHI-SEC
      *
      *    予約処理中でなく、選択数がある時、選択番号へ
           IF      SPA-YYK-SYORI       =   ZERO
               IF      SPA-GMN-MAX      >   ZERO
                   MOVE    12               TO  SPA-GMN-CUR
               ELSE
                   MOVE    3            TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    6               TO  SPA-GMN-CUR
           END-IF
      *
           .
       41017-KENSEL-EXT.
           EXIT.
      *       
      *****************************************************************
      *    選択番号入力
      *****************************************************************
       41018-SELBAN-SEC             SECTION.
      *
           MOVE    12                  TO  SPA-GMN-CUR
           MOVE    Y01-SELNUM          TO  SPA-GMN-SELBAN
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    Y01-SELNUM          TO  SNUM-INX
           MOVE    3                   TO  SNUM-KETSUU
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0005"              TO  SPA-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-NAI-SELBAN
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-SELBAN      <   1   OR  >  SPA-GMN-MAX
                   MOVE    "0005"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 410181-YYKSET-SEC
               MOVE    5                   TO  SPA-GMN-CUR
           END-IF
           .
       41018-SELBAN-EXT.
           EXIT.
      *****************************************************************
      *    予約リスト選択時
      *****************************************************************
       410181-YYKSET-SEC            SECTION.
      *
      *    予約マスター変更へ
           INITIALIZE                  SPA-GMN-YYKAREA
      *
           MOVE    SPA-NAI-TYYKYMD (SPA-NAI-SELBAN)
                                       TO  SPA-NAI-YYKYMD
           MOVE    SPA-NAI-YYKYMD      TO  WRK-SYMD
           PERFORM 3101-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG        TO  SPA-GMN-YYKYMD
      *
           MOVE    SPA-GMN-TYYKTIME(SPA-NAI-SELBAN)
                                       TO  SPA-NAI-YYKTIME
           MOVE    SPA-NAI-YYKTIME(1:2)    TO  SPA-GMN-YYKTIME(1:2)
           MOVE    ":"                     TO  SPA-GMN-YYKTIME(3:1)
           MOVE    SPA-NAI-YYKTIME(3:2)    TO  SPA-GMN-YYKTIME(4:2)
           MOVE    SPA-NAI-TKYYKTIME(SPA-NAI-SELBAN)
                                       TO  SPA-NAI-KYYKTIME
           MOVE    SPA-NAI-TYYKID  (SPA-NAI-SELBAN)
                                       TO  SPA-NAI-YYKID
           MOVE    SPA-NAI-TPTNUM  (SPA-NAI-SELBAN)
                                       TO  SPA-GMN-PTNUM
           MOVE    SPA-NAI-TPTID  (SPA-NAI-SELBAN)
                                       TO  SPA-GMN-PTID
           MOVE    SPA-GMN-TNAME  (SPA-NAI-SELBAN)
                                       TO  SPA-GMN-NAME
           MOVE    SPA-NAI-TSRYKA (SPA-NAI-SELBAN)
                                       TO  SPA-GMN-SRYKA
           MOVE    SPA-NAI-TYYKKBN (SPA-NAI-SELBAN)
                                       TO  SPA-GMN-YYKKBN
           MOVE    SPA-NAI-TKAKREN(SPA-NAI-SELBAN)
                                       TO  SPA-GMN-KAKREN
      *
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    SPA-GMN-NAME        TO  SPA-GMN-YYKNAME
               MOVE    SPACE               TO  SPA-GMN-NAME
           ELSE
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  PTINF-PTID
               PERFORM 920-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
                   MOVE    ZERO                TO  IDZ
                   PERFORM 410141-PTINF-HENSYU-SEC
                   MOVE    PTINF-NAME          TO  SPA-GMN-NAME
                   MOVE    WRK-SEX             TO  SPA-GMN-SEX
                   MOVE    WRK-BIRTHDAYH       TO  SPA-GMN-BIRTHDAYH
                   MOVE    PTINF-NAME          TO  SPA-GMN-YYKNAME
      *
      *            排他制御処理
                   PERFORM 990-LOCK-IN-SEC
               END-IF
           END-IF
      *
      *    診療科
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               IF      SPA-GMN-SRYKACDL (IDX)      =   SPA-GMN-SRYKA
                   MOVE    SPA-GMN-SRYKALST(IDX)   TO  SPA-GMN-SRYKA-G
                   MOVE    SPA-SRYKA-MAX           TO  IDX
               END-IF
           END-PERFORM
      *    予約内容
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-YYKKBN-MAX
               IF      SPA-GMN-YYKKBNLST(IDX)(1:2)
                                            =   SPA-GMN-YYKKBN
                   MOVE    SPA-GMN-YYKKBNLST(IDX)
                                           TO  SPA-GMN-YYKKBN-G
                   MOVE    SPA-YYKKBN-MAX  TO  IDX
               END-IF
           END-PERFORM
      *    連絡
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KAKREN-MAX
               IF      SPA-GMN-KAKRENLST(IDX)(1:2)
                                            =   SPA-GMN-KAKREN
                   MOVE    SPA-GMN-KAKRENLST(IDX)
                                           TO  SPA-GMN-KAKREN-G
                   MOVE    SPA-KAKREN-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    2                   TO  SPA-YYK-SYORI
      *
           INITIALIZE                  SPA-Y01-YYK-AREA
           MOVE    SPA-NAI-DRCD        TO  SPA-Y01-DRCD
           MOVE    SPA-GMN-SRYNAIYOCD  TO  SPA-Y01-SRYNAIYO
           MOVE    SPA-GMN-YYKYMD      TO  SPA-Y01-YYKYMD
           MOVE    SPA-NAI-YYKYMD      TO  SPA-Y01-YYKYMDS
           MOVE    SPA-NAI-KYYKTIME    TO  SPA-Y01-YYKTIME
           MOVE    SPA-NAI-YYKID       TO  SPA-Y01-YYKID
           MOVE    SPA-NAI-YYKYMD      TO  SPA-OLD-YYKYMD
      *
           MOVE    SPA-NAI-YYKYMD(7:2) TO  SPA-NAI-HYOJHI
      *!
      *    予約メモ
           MOVE    ZERO                TO  SPA-NAI-YYKCOPY
           INITIALIZE                  SPA-NAI-COMMENT-G
           INITIALIZE                  YYKCOM-REC
           MOVE    SPA-HOSPNUM         TO  YYKCOM-HOSPNUM
           MOVE    SPA-Y01-SRYNAIYO    TO  YYKCOM-SRYNAIYO
           MOVE    SPA-Y01-DRCD        TO  YYKCOM-DRCD
           MOVE    SPA-NAI-YYKYMD      TO  YYKCOM-YYKYMD
      *    キー予約時間
           MOVE    SPA-Y01-YYKTIME     TO  YYKCOM-KEYYYKTIME
      *    予約ＩＤ
           MOVE    SPA-NAI-YYKID       TO  YYKCOM-YYKID
      *
           MOVE    YYKCOM-REC          TO  MCPDATA-REC
           MOVE    "tbl_yykcom"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_yykcom"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-YYKCOM-READ-SEC
           ELSE
               MOVE    1                  TO  FLG-YYKCOM
           END-IF
           MOVE    ZERO                TO  IDX-YY
           PERFORM UNTIL   (FLG-YYKCOM     =   1    )
                       OR  (IDX-YY         >=  6    )
               ADD     1                   TO  IDX-YY
               MOVE    YYKCOM-COMMENT      TO  SPA-NAI-COMMENT (IDX-YY)
      *
               MOVE    "tbl_yykcom"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-YYKCOM-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_yykcom"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       410181-YYKSET-EXT.
           EXIT.
      *****************************************************************
      *    予約リスト選択時
      *****************************************************************
       41019-CLIST-SEC            SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-GMN-MAX     )
               IF      Y01-PTLIST-SELECT (IDX) =   "T"
                   MOVE  SPA-GMN-TBANGO(IDX)
                                               TO  Y01-SELNUM
                   MOVE    SPA-GMN-MAX         TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM 41018-SELBAN-SEC
           .
       41019-CLIST-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー選択時
      *****************************************************************
       41020-CALENDAR-SEC         SECTION.
      *
      *    画面入力の年月＝ＳＰＡの年月のチェック
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    Y01-YEAR            TO  WRK-CAL-YEAR
           MOVE    Y01-MONTH           TO  WRK-CAL-MONTH
           MOVE    Y01-DAY             TO  WRK-CAL-DAY
                                           SPA-NAI-HYOJHI
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-CAL-YMD         TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"      USING    STS-AREA-DAY
                                           LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
                   MOVE LNK-DAY2-EDTYMD3 (1:4)
                                       TO  SPA-GMN-TAIYYMM
                   MOVE LNK-DAY2-EDTYMD1 (2:2)
                                       TO  SPA-GMN-TAIYYMM (5:2)
                   MOVE "年"                TO  SPA-GMN-TAIYYMM (7:2)
                   MOVE LNK-DAY2-EDTYMD1 (5:2)
                                       TO  SPA-GMN-TAIYYMM (9:2)
                   MOVE "月"           TO  SPA-GMN-TAIYYMM (11:2)
                   MOVE  SPA-GMN-TAIYYMM
                                       TO  WRK-GMN-TAIYYMM
           ELSE
      *            MOVE SPACE          TO  SPA-GMN-TAIYYMM
                   MOVE    "0015"      TO  SPA-ERRCD
                   GO                  TO  41020-CALENDAR-EXT
           END-IF
           MOVE    WRK-CAL-YMD         TO  WRK-SYMD
           PERFORM 3101-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-TAIYMD
      *
           IF      (WRK-CAL-YEAR       =   SPA-GMN-YEAR )  AND
                   (WRK-CAL-MONTH      =   SPA-GMN-MONTH )
               MOVE    WRK-CAL-YMD         TO  SPA-GMN-CALENDR
               PERFORM 41015-HYOJHI-SEC
           ELSE
               MOVE    WRK-CAL-YMD         TO  SPA-GMN-CALENDR
               PERFORM 410202-CALSET-SEC
           END-IF
      *
      *    予約処理中でなく、選択数がある時、選択番号へ
           IF      SPA-YYK-SYORI       =   ZERO
               IF      SPA-GMN-MAX      >   ZERO
                   MOVE    12               TO  SPA-GMN-CUR
               ELSE
                   IF      SPA-GMN-CUR      =   12  OR  ZERO
                       MOVE    3            TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
       41020-CALENDAR-EXT.
           EXIT.
      *       
      *****************************************************************
      *    対象日入力
      *****************************************************************
       41015-HYOJHI-SEC             SECTION.
      *
      *
           IF      SPA-GMN-DAY        >=  01  AND  <=  31
      *        予約表設定処理
               PERFORM 410151-HYOJI-HEN-SEC
           ELSE
               MOVE    "0015"          TO  SPA-ERRCD
           END-IF
           .
       41015-HYOJHI-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー選択時の患者一覧等の設定（異月）
      *****************************************************************
       410202-CALSET-SEC               SECTION.
      *
      *
           MOVE    SPACE               TO  SPA-GMN-GOKKEN-G
                                           SPA-NAI-HYOTBL-G
           MOVE    ZERO                TO  SPA-GMN-HYOTBL-G
           MOVE    Y01-YEAR            TO  WRK-SYSYY
           MOVE    Y01-MONTH           TO  WRK-SYSMM
           MOVE    Y01-DAY             TO  WRK-SYSDD
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYSYMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"      USING    STS-AREA-DAY
                                           LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
              MOVE LNK-DAY2-EDTYMD3 (1:4)
                                       TO  SPA-GMN-TAIYYMM
              MOVE LNK-DAY2-EDTYMD1 (2:2)
                                       TO  SPA-GMN-TAIYYMM (5:2)
              MOVE "年"                TO  SPA-GMN-TAIYYMM (7:2)
              MOVE LNK-DAY2-EDTYMD1 (5:2)
                                       TO  SPA-GMN-TAIYYMM (9:2)
              MOVE "月"                TO  SPA-GMN-TAIYYMM (11:2)
           ELSE
              MOVE SPACE               TO  SPA-GMN-TAIYYMM
           END-IF
      *
      * １日の曜日算出
           MOVE    "31"                TO  LNK-DAY3-IRAI
           MOVE    WRK-SYSYMD          TO  LNK-DAY3-YMD
           MOVE    "01"                TO  LNK-DAY3-YMD(7:2)
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY3-AREA
           IF      STS-DAY-RC1      NOT =   ZERO
              MOVE   0                   TO  LNK-DAY3-YOBI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           IF      LNK-DAY3-YOBI   NOT =   0
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX         >   LNK-DAY3-YOBI )
                   MOVE  ZERO          TO  SPA-GMN-HYODD (IDX)
                   MOVE  ZERO          TO  SPA-NAI-YMD (IDX)
               END-PERFORM
           END-IF
      *
      * 月末日の算出
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-SYSYMD (1:6)    TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING    STS-AREA-DAY
                                            LNK-DAY7-AREA
           IF      STS-DAY-RC1      NOT =   ZERO
              MOVE WRK-SYSYMD          TO  LNK-DAY7-KOYOMI
              MOVE "31"                TO  LNK-DAY7-KOYOMI (7:2)
           END-IF
      *
           MOVE    ZERO                TO  WRK-HI
           IF      IDX                 =   ZERO
               MOVE    1               TO  IDX-S
           ELSE
               MOVE    IDX             TO  IDX-S
           END-IF
           PERFORM VARYING     IDX     FROM    IDX-S   BY  1
                   UNTIL      (IDX         >   42  )   OR
                              (WRK-HI      =   LNK-DAY7-KOYOMI (7:2) )
               ADD     1               TO  WRK-HI
               MOVE    WRK-HI          TO  SPA-GMN-HYODD (IDX)
               MOVE    WRK-SYSYMD (1:6)
                                       TO  SPA-NAI-YMD (IDX)(1:6)
               MOVE    WRK-HI          TO  SPA-NAI-YMD (IDX)(7:2)
           END-PERFORM
      *
           MOVE    WRK-CAL-DAY         TO  SPA-NAI-HYOJHI
           MOVE    ZERO                TO  WRK-HI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   42
               IF      SPA-NAI-HYOJHI      =   SPA-GMN-HYODD (IDX)
                   MOVE    IDX             TO  WRK-HI
                   MOVE    98              TO  IDX
               END-IF
           END-PERFORM
           IF      WRK-HI          >   ZERO
               COMPUTE WRK-SEL-SYUU    =  (WRK-HI    -  1  )
                                       /   7    +    1
           END-IF
      *
           MOVE    WRK-SEL-SYUU        TO  SPA-SEL-SYUU
      *    予約テーブル編集処理
           PERFORM 510-YYKTBL-SET-SEC
      *
      *    予約患者一覧再編集
           COMPUTE SPA-SEL-YOUBI    =   WRK-HI
                                    -  ((SPA-SEL-SYUU  -  1 )  *  7 )
           MOVE    ZERO             TO  SPA-SEL-TIME
      *
           PERFORM 4001-KNJICHI-SEC
      *
           .
       410202-CALSET-EXT.
           EXIT.
      *****************************************************************
      *    予約取消し
      *****************************************************************
       420-YYKDEL-SEC             SECTION.
      *
           MOVE    Y01-SELNUM          TO  SNUM-INX
           MOVE    3                   TO  SNUM-KETSUU
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0005"              TO  SPA-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-NAI-SELBAN
               IF      SPA-NAI-SELBAN      <   01   OR  >  SPA-GMN-MAX
                   MOVE    "0005"              TO  SPA-ERRCD
               END-IF
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    12                  TO  SPA-GMN-CUR
               GO      TO      420-YYKDEL-EXT
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    予約削除処理
           PERFORM 4903-YYKDEL-SEC
      ***  INITIALIZE                  YYK-REC
      *    MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
      *    MOVE    SPA-GMN-SRYNAIYOCD  TO  YYK-SRYNAIYO
      *    MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
      *    MOVE    SPA-NAI-TYYKYMD (SPA-NAI-SELBAN)
      *                                TO  YYK-YYKYMD
      *    MOVE    SPA-NAI-TKYYKTIME(SPA-NAI-SELBAN)
      *                                TO  YYK-KEYYYKTIME
      *    MOVE    SPA-NAI-TYYKID  (SPA-NAI-SELBAN)
      *                                TO  YYK-YYKID
      *
      *    MOVE    YYK-REC             TO  MCPDATA-REC
      *    MOVE    "DBDELETE"          TO  MCP-FUNC
      *    MOVE    "tbl_yyk"           TO  MCP-TABLE
      *    MOVE    "key"               TO  MCP-PATHNAME
      *    CALL    "ORCDBMAIN"         USING
      *                                    MCPAREA
      *                                    MCPDATA-REC
      *                                    SPA-AREA
      *
      *    予約人数変更
      *    MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
      *    MOVE    SPA-GMN-SRYNAIYOCD  TO  YYK-SRYNAIYO
      *    MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
      *    MOVE    SPA-NAI-TYYKYMD (SPA-NAI-SELBAN)
      *                                TO  YYK-YYKYMD
      *    MOVE    SPA-NAI-TKYYKTIME(SPA-NAI-SELBAN)
      *                                TO  YYK-KEYYYKTIME
      *    MOVE    ZERO                TO  YYK-YYKID
      *    PERFORM 910-YOYAKU-INV-SEC
      *
      *    IF      FLG-YYK             =   ZERO
      *        COMPUTE YYK-YYKCNT      =   YYK-YYKCNT   -  1
      *
      *        MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
      *        MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
      *
      *        更新
      *        MOVE    YYK-REC             TO  MCPDATA-REC
      *        MOVE    "DBUPDATE"          TO  MCP-FUNC
      *        MOVE    "tbl_yyk"           TO  MCP-TABLE
      *        MOVE    "key"               TO  MCP-PATHNAME
      *        CALL    "ORCDBMAIN"         USING
      *                                        MCPAREA
      *                                        MCPDATA-REC
      *                                        SPA-AREA
      *******  DISPLAY "Y01 YYK DEL :" MCP-RC "."
      **** END-IF
      *
           MOVE    ZERO                TO  WRK-HI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   42
               IF      SPA-NAI-HYOJHI      =   SPA-GMN-HYODD (IDX)
                   MOVE    IDX             TO  WRK-HI
                   MOVE    98              TO  IDX
               END-IF
           END-PERFORM
           IF      WRK-HI          >   ZERO
               COMPUTE WRK-SEL-SYUU    =  (WRK-HI    -  1  )
                                       /   7    +    1
           ELSE
               MOVE    "0015"          TO  SPA-ERRCD
               GO      TO      420-YYKDEL-EXT
           END-IF
      *    予約テーブル編集
           INITIALIZE                  SPA-SEL-AREA
           MOVE    WRK-SEL-SYUU        TO  SPA-SEL-SYUU
           PERFORM 510-YYKTBL-SET-SEC
      *
      *    予約患者一覧再編集
           COMPUTE SPA-SEL-YOUBI    =   WRK-HI
                                    -  ((SPA-SEL-SYUU  -  1 )  *  7 )
           MOVE    ZERO             TO  SPA-SEL-TIME
           PERFORM 4001-KNJICHI-SEC
      *
      *    予約クリア
           INITIALIZE                  SPA-GMN-YYKAREA
           MOVE    SPA-SYSYMD          TO  SPA-NAI-YYKYMD
           MOVE    SPA-SYSYMDWH        TO  SPA-GMN-YYKYMD
           MOVE    0                   TO  FLG-CALENDAR
           PERFORM 2201-YYK-CLEAR-SEC
           MOVE    3                   TO  SPA-GMN-CUR
      *
           .
       420-YYKDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理へ
      *****************************************************************
       220-CLEAR-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-HENKOU
           IF     (SPA-GMN-PTNUM       =   SPACE )  AND
                  (SPA-GMN-PTID        =   ZERO  )  AND
                  (SPA-GMN-YYKNAME     =   SPACE )
               INITIALIZE                      SPA-Y01-YYK-AREA
      *        初期表示へ
               PERFORM 310-SPASET-SEC
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               MOVE    SPA-GMN-SRYNAIYO    TO  SPA-Y01-SRYNAIYOG
               MOVE    SPA-GMN-DRNAME      TO  SPA-Y01-DRNAME
               MOVE    SPA-NAI-DRCD        TO  SPA-Y01-DRCD
      *        排他制御解除処理
               PERFORM 990-LOCK-OUT-SEC
      *        予約内容初期
               MOVE    SPA-SYSYMD          TO  SPA-NAI-YYKYMD
               MOVE    SPA-SYSYMDWH        TO  SPA-GMN-YYKYMD
               MOVE    1                   TO  FLG-CALENDAR
               PERFORM 2201-YYK-CLEAR-SEC
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF
           .
       220-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約へカーソル移動
      *****************************************************************
       480-YOYAKU-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-YYK-SYORI
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *
          .
       480-YOYAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約登録　処理
      *****************************************************************
       490-TOROKU-SEC             SECTION.
      *
      *    登録チェック処理
           PERFORM 4901-TOROKU-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      490-TOROKU-EXT
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF      SPA-Y01-YYKID       =   ZERO
      *        予約追加処理
               PERFORM 4901-YYKADD-SEC
           ELSE
               IF      (SPA-GMN-SRYNAIYOCD =   SPA-Y01-SRYNAIYO) AND
                       (SPA-NAI-DRCD(2:4)  =   SPA-Y01-DRCD(2:4)) AND
                       (SPA-NAI-YYKYMD     =   SPA-OLD-YYKYMD )   AND
                       (SPA-NAI-KYYKTIME   =   SPA-Y01-YYKTIME)
      *            予約修正処理
                   PERFORM 4902-YYKUPD-SEC
               ELSE
      *            予約削除処理
                   PERFORM 4903-YYKDEL-SEC
      *            予約追加処理
                   PERFORM 4901-YYKADD-SEC
               END-IF
           END-IF
      *
      *    予約票印刷
           IF      SPA-ERRCD           =   SPACE
      *        新規患者は印刷しない
               IF     (SPA-GMN-YYKHYO      =   "0" )
               OR     (SPA-GMN-PTID        =   ZERO )
                   CONTINUE
               ELSE
                   PERFORM 4908-YYKHYO-PRINT-SEC
               END-IF
           END-IF
      *
      *    予約クリア
           MOVE    1                   TO  FLG-CALENDAR
           PERFORM 2201-YYK-CLEAR-SEC
           MOVE    3                   TO  SPA-GMN-CUR
      *
           .
       490-TOROKU-EXT.
           EXIT.
      *****************************************************************
      *    予約クリア処理
      *****************************************************************
       2201-YYK-CLEAR-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-YYK-SYORI
           MOVE    ZERO                TO  SPA-Y01-KEIKOKU
      *
      *    更新後カレンダー内容表示
           IF      FLG-CALENDAR        =   1
               MOVE    SPA-NAI-YYKYY       TO  Y01-YEAR
               MOVE    SPA-NAI-YYKMM       TO  Y01-MONTH
               MOVE    SPA-NAI-YYKDD       TO  Y01-DAY
               INITIALIZE                  SPA-GMN-YYKAREA
               PERFORM 41020-CALENDAR-SEC
           END-IF
      *
           MOVE    SPACE               TO  SPA-CHK-YYKYMD
           MOVE    ZERO                TO  SPA-CHK-KYYKTIME
      *
      *    診療科（ドクターコードから）
           PERFORM 410121-DRCD-HENSYU-SEC
           IF      SPA-GMN-SRYKA       =   SPACE
               MOVE    SPA-GMN-SRYKALST (1)    TO  SPA-GMN-SRYKA-G
           END-IF
      *
      *    予約内容
           IF      SPA-YYKKBN-HYOJI    =   ZERO
               MOVE    SPA-GMN-YYKKBNLST (2)
                                           TO  SPA-GMN-YYKKBN-G
           ELSE
               MOVE    SPA-GMN-YYKKBNLST (SPA-YYKKBN-HYOJI)
                                           TO  SPA-GMN-YYKKBN-G
           END-IF
      *    確認連絡
           MOVE    SPA-GMN-KAKRENLST (1)
                                       TO  SPA-GMN-KAKREN-G
           MOVE    SPACE               TO  SPA-GMN-SELBAN
           MOVE    ZERO                TO  SPA-NAI-SELBAN
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           .
       2201-YYK-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    予約追加処理
      *****************************************************************
       4901-TOROKU-CHK-SEC             SECTION.
      *
      *    診療内容チェック
           MOVE    ZERO               TO  FLG-HENFLG
           PERFORM VARYING IDY     FROM   1   BY  1
                   UNTIL  (IDY     >      SPA-SRYNAIYO-MAX  )  OR
                          (FLG-HENFLG     =   1  )
               IF      SPA-GMN-SRYNAIYOCD =   SPA-GMN-SRYNAIYOLST(IDY)
                                                                  (1:2)
                   MOVE    1               TO  FLG-HENFLG
               END-IF
           END-PERFORM
           IF      FLG-HENFLG          =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    01                  TO  SPA-GMN-CUR
           END-IF
      *   ドクター入力
           MOVE    ZERO               TO  FLG-HENFLG
           PERFORM VARYING IDY     FROM   1   BY  1
                   UNTIL  (IDY     >      SPA-DRCD-MAX  )  OR
                          (SPA-ERRCD  NOT =   SPACE     )  OR
                          (FLG-HENFLG     =   1         )
               IF      SPA-GMN-DRCD       =   SPA-GMN-DRCDLST(IDY)(1:4)
                   MOVE    1               TO  FLG-HENFLG
               END-IF
           END-PERFORM
           IF     (FLG-HENFLG          =   ZERO ) AND
                  (SPA-ERRCD           =   SPACE)
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    02                  TO  SPA-GMN-CUR
           END-IF
      *    入力チェック処理
           IF      SPA-ERRCD           =   SPACE
               MOVE    2                   TO  FLG-TOUROKU
               PERFORM 410-INPUT-CHK-SEC
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4901-TOROKU-CHK-EXT
           END-IF
      *
           IF     (SPA-GMN-PTNUM       =   SPACE  )  AND
                  (SPA-GMN-YYKNAME     =   SPACE  )
               MOVE    "0013"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
               GO      TO      4901-TOROKU-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-YYKTIME     =   SPACE  OR  ZERO
               MOVE    6                   TO  SPA-GMN-CUR
               MOVE    "0014"              TO  SPA-ERRCD
               GO      TO      4901-TOROKU-CHK-EXT
           END-IF
      *    予約日か過去の時警告
           IF     (SPA-Y01-KEIKOKU     =   ZERO )  AND
                  (SPA-NAI-YYKYMD      <   SPA-SYSYMD)
               MOVE    "K001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-Y01-KEIKOKU
           END-IF
          .
       4901-TOROKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    予約追加処理
      *****************************************************************
       4901-YYKADD-SEC             SECTION.
      *
      *    明細レコード最大件数
           MOVE    SPACE               TO  YYK-REC
           INITIALIZE                      YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    SPA-GMN-SRYNAIYOCD  TO  YYK-SRYNAIYO
           MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
           MOVE    SPA-NAI-YYKYMD      TO  YYK-YYKYMD
           MOVE    SPA-NAI-KYYKTIME    TO  YYK-KEYYYKTIME
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-YOYAKU-READ-SEC
           ELSE
                MOVE    1              TO  FLG-YYK
           END-IF
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  WRK-YYKID
           IF      FLG-YYK             =   ZERO
               MOVE    YYK-YYKID           TO  WRK-YYKID
               IF      WRK-YYKID           =   99
                   MOVE    "1013"          TO  SPA-ERRCD
                   GO      TO      4901-YYKADD-EXT
               END-IF
               ADD     1                   TO  WRK-YYKID
           END-IF
      *
      *    予約ＩＤ＝００検索
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    SPA-GMN-SRYNAIYOCD  TO  YYK-SRYNAIYO
           MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
           MOVE    SPA-NAI-YYKYMD      TO  YYK-YYKYMD
           MOVE    SPA-NAI-KYYKTIME    TO  YYK-KEYYYKTIME
           MOVE    ZERO                TO  YYK-YYKID
           MOVE    ZERO                TO  YYK-PTID
           PERFORM 910-YOYAKU-INV-SEC
           IF      FLG-YYK             =   ZERO
               ADD     1                   TO  YYK-YYKCNT
               MOVE    YYK-YYKCNT          TO  WRK-YYKCNT
      *        最大件数修正（システム日付より後の日）
               IF     (YYK-YYKYMD          >=  SPA-SYSYMD )
                 AND  (YYK-YYKMAXCNT   NOT =   SPA-Y01-YYKMAXCNT)
                   MOVE    SPA-Y01-YYKMAXCNT   TO  YYK-YYKMAXCNT
               END-IF
      *
               MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
               MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
               MOVE    SPA-OPID            TO  YYK-OPID
      *
               MOVE    YYK-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC          NOT  =  ZERO
                   MOVE    "1010"      TO  SPA-ERRCD
                   DISPLAY "Y01 YYK-KEY UPD ERR:" MCP-RC "."
                           ",KEY:" YYK-KEY
                   GO                  TO  4901-YYKADD-EXT
               END-IF
           ELSE
               MOVE    SPACE               TO  YYK-REC
               INITIALIZE                      YYK-REC
               MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
               MOVE    SPA-GMN-SRYNAIYOCD  TO  YYK-SRYNAIYO
               MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
               MOVE    SPA-NAI-YYKYMD      TO  YYK-YYKYMD
               MOVE    SPA-NAI-KYYKTIME    TO  YYK-KEYYYKTIME
               MOVE    ZERO                TO  YYK-YYKID
               MOVE    SPA-Y01-YYKMAXCNT   TO  YYK-YYKMAXCNT
               MOVE    1                   TO  YYK-YYKCNT
               MOVE    1                   TO  WRK-YYKCNT
               MOVE    ZERO                TO  YYK-PTID
      *
               MOVE    SMCNDATE-YMD        TO  YYK-CREYMD
               MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
               MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
               MOVE    SPA-OPID            TO  YYK-OPID
      *
               MOVE    YYK-REC             TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC          NOT  =  ZERO
                   MOVE    "1010"  TO  SPA-ERRCD
                   DISPLAY "Y01 YYK-KEY INS ERR:" MCP-RC "."
                           ",KEY:" YYK-KEY
                   GO              TO  4901-YYKADD-EXT
               END-IF
           END-IF
      *
      *    明細レコード追加
      *    MOVE    SPACE               TO  YYK-REC
      *    INITIALIZE                      YYK-REC
      *    MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
      *    MOVE    SPA-GMN-SRYNAIYOCD  TO  YYK-SRYNAIYO
      *    MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
      *    MOVE    SPA-NAI-YYKYMD      TO  YYK-YYKYMD
      *    MOVE    SPA-NAI-KYYKTIME    TO  YYK-KEYYYKTIME
      *
      *    MOVE    YYK-REC             TO  MCPDATA-REC
      *    MOVE    "tbl_yyk"           TO  MCP-TABLE
      *    MOVE    "key7"              TO  MCP-PATHNAME
      *    PERFORM 910-DBSELECT-SEC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "tbl_yyk"           TO  MCP-TABLE
      *        MOVE    "key7"              TO  MCP-PATHNAME
      *        PERFORM 900-YOYAKU-READ-SEC
      *    ELSE
      *         MOVE    1              TO  FLG-YYK
      *    END-IF
      *    MOVE    "tbl_yyk"           TO  MCP-TABLE
      *    MOVE    "key7"              TO  MCP-PATHNAME
      *    PERFORM 990-DBCLOSE-SEC
      *
      *    IF      FLG-YYK             =   ZERO
      *        MOVE    YYK-YYKID           TO  WRK-YYKID
      *        IF      WRK-YYKID           =   99
      *            MOVE    "1013"          TO  SPA-ERRCD
      *            GO      TO      4901-YYKADD-EXT
      *        END-IF
      *        ADD     1                   TO  WRK-YYKID
      **** END-IF
      *
           IF      WRK-YYKID           =   ZERO
               MOVE    WRK-YYKCNT          TO  WRK-YYKID
           END-IF
      *    明細レコード追加
           MOVE    SPACE               TO  YYK-REC
           INITIALIZE                      YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    SPA-GMN-SRYNAIYOCD  TO  YYK-SRYNAIYO
           MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
           MOVE    SPA-NAI-YYKYMD      TO  YYK-YYKYMD
           MOVE    SPA-NAI-KYYKTIME    TO  YYK-KEYYYKTIME
           MOVE    WRK-YYKID           TO  YYK-YYKID
      *
           MOVE    SPA-NAI-YYKTIME     TO  YYK-YYKTIME
           MOVE    SPA-GMN-PTID        TO  YYK-PTID
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    SPA-GMN-YYKNAME     TO  YYK-NAME
           ELSE
               MOVE    SPA-GMN-NAME        TO  YYK-NAME
           END-IF
           MOVE    SPA-GMN-SRYKA       TO  YYK-SRYKA
           MOVE    SPA-GMN-YYKKBN      TO  YYK-YYKKBN
           MOVE    SPA-GMN-KAKREN      TO  YYK-KAKREN
      *
           MOVE    SMCNDATE-YMD        TO  YYK-CREYMD
           MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
           MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
      *
           MOVE    SPA-OPID            TO  YYK-OPID
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          NOT  =  ZERO
               MOVE    "1011"           TO  SPA-ERRCD
               DISPLAY "Y01 YYK MEI INS ERR:" MCP-RC "."
           END-IF
      *
      *    予約メモ登録
           IF      SPA-ERRCD           =   SPACE
               PERFORM 49014-YYKCOM-ADD-SEC
           END-IF
           .
       4901-YYKADD-EXT.
           EXIT.
      *****************************************************************
      *    予約メモ登録処理
      *****************************************************************
       49014-YYKCOM-ADD-SEC            SECTION.
      *
           PERFORM VARYING     IDX-YY  FROM    1   BY  1
                   UNTIL      (IDX-YY      >   6   )
                           OR (SPA-NAI-COMMENT (IDX-YY)  =   SPACE)
      *        レコード追加
               MOVE    SPACE               TO  YYKCOM-REC
               INITIALIZE                      YYKCOM-REC
               MOVE    SPA-HOSPNUM         TO  YYKCOM-HOSPNUM
               MOVE    SPA-GMN-SRYNAIYOCD  TO  YYKCOM-SRYNAIYO
               MOVE    SPA-NAI-DRCD        TO  YYKCOM-DRCD
               MOVE    SPA-NAI-YYKYMD      TO  YYKCOM-YYKYMD
               MOVE    SPA-NAI-KYYKTIME    TO  YYKCOM-KEYYYKTIME
               MOVE    WRK-YYKID           TO  YYKCOM-YYKID
      *
               MOVE    IDX-YY              TO  YYKCOM-RENNUM
               MOVE    SPA-NAI-COMMENT(IDX-YY)
                                           TO  YYKCOM-COMMENT
      *
               MOVE    SMCNDATE-YMD        TO  YYKCOM-CREYMD
               MOVE    SMCNDATE-YMD        TO  YYKCOM-UPYMD
               MOVE    SMCNDATE-HMS        TO  YYKCOM-UPHMS
               MOVE    SPA-OPID            TO  YYKCOM-OPID
      *
               MOVE    YYKCOM-REC          TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_yykcom"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC          NOT  =  ZERO
                   MOVE    "1011"           TO  SPA-ERRCD
                   DISPLAY "Y01 YYKCOM INS ERR :"  YYKCOM-KEY
                                  ",MCP-RC:" MCP-RC "@"
               END-IF
           END-PERFORM
           .
       49014-YYKCOM-ADD-EXT.
           EXIT.
      *****************************************************************
      *    予約修正処理
      *****************************************************************
       4902-YYKUPD-SEC            SECTION.
      *
      *    予約メモ削除
           PERFORM 49031-YYKCOM-DEL-SEC
      *
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    SPA-GMN-SRYNAIYOCD  TO  YYK-SRYNAIYO
           MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
           MOVE    SPA-NAI-YYKYMD      TO  YYK-YYKYMD
           MOVE    SPA-NAI-KYYKTIME    TO  YYK-KEYYYKTIME
           MOVE    SPA-NAI-YYKID       TO  YYK-YYKID
           PERFORM 910-YOYAKU-INV-SEC
           IF      FLG-YYK          =   ZERO
               MOVE    SPA-NAI-YYKTIME     TO  YYK-YYKTIME
               MOVE    SPA-GMN-SRYKA       TO  YYK-SRYKA
               MOVE    SPA-GMN-YYKKBN      TO  YYK-YYKKBN
               MOVE    SPA-GMN-KAKREN      TO  YYK-KAKREN
      *        予約氏名
               IF      YYK-PTID            =   ZERO
                   MOVE    SPA-GMN-YYKNAME     TO  YYK-NAME
               END-IF
      *        予約データ更新
               PERFORM 49030-YYK-UPDATE-SEC
               IF      SPA-ERRCD           =   SPACE
                   MOVE    SPA-NAI-YYKID       TO  WRK-YYKID
      *            予約メモ登録
                   PERFORM 49014-YYKCOM-ADD-SEC
               END-IF
           ELSE
      *        予約追加処理
               PERFORM 4901-YYKADD-SEC
           END-IF
      *
           .
       4902-YYKUPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約削除処理
      *****************************************************************
       4903-YYKDEL-SEC            SECTION.
      *
      *    前予約ＩＤ≠００削除
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    SPA-Y01-SRYNAIYO    TO  YYK-SRYNAIYO
           MOVE    SPA-Y01-DRCD        TO  YYK-DRCD
           MOVE    SPA-OLD-YYKYMD      TO  YYK-YYKYMD
           MOVE    SPA-Y01-YYKTIME     TO  YYK-KEYYYKTIME
           MOVE    SPA-Y01-YYKID       TO  YYK-YYKID
           PERFORM 910-YOYAKU-INV-SEC
           IF      FLG-YYK             =   ZERO
               PERFORM 49030-YYK-DELETE-SEC
           ELSE
               DISPLAY "Y01 YYK MEI DEL ??:" MCP-RC "."
                       ",KEY:" YYK-KEY
           END-IF
      *    予約メモ削除
           PERFORM 49031-YYKCOM-DEL-SEC
      *
      *    明細レコード最大件数
           MOVE    SPACE               TO  YYK-REC
           INITIALIZE                      YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    SPA-Y01-SRYNAIYO    TO  YYK-SRYNAIYO
           MOVE    SPA-Y01-DRCD        TO  YYK-DRCD
           MOVE    SPA-OLD-YYKYMD      TO  YYK-YYKYMD
           MOVE    SPA-Y01-YYKTIME     TO  YYK-KEYYYKTIME
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-YOYAKU-READ-SEC
           ELSE
                MOVE    1              TO  FLG-YYK
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL    FLG-YYK    =   1
               IF      YYK-YYKID       NOT =   ZERO
                   ADD     1               TO  IDX
               END-IF
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-YOYAKU-READ-SEC
           END-PERFORM
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    前予約ＩＤ＝００修正
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    SPA-Y01-SRYNAIYO    TO  YYK-SRYNAIYO
           MOVE    SPA-Y01-DRCD        TO  YYK-DRCD
           MOVE    SPA-OLD-YYKYMD      TO  YYK-YYKYMD
           MOVE    SPA-Y01-YYKTIME     TO  YYK-KEYYYKTIME
           MOVE    ZERO                TO  YYK-YYKID
           PERFORM 910-YOYAKU-INV-SEC
           IF      FLG-YYK             =   ZERO
               MOVE    IDX                 TO  YYK-YYKCNT
      ***      COMPUTE YYK-YYKCNT          =   YYK-YYKCNT  -   1
               PERFORM 49030-YYK-UPDATE-SEC
           END-IF
           .
       4903-YYKDEL-EXT.
           EXIT.
      *****************************************************************
      *    予約削除処理
      *****************************************************************
       49030-YYK-DELETE-SEC             SECTION.
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          NOT  =  ZERO
               MOVE    "1012"      TO  SPA-ERRCD
               DISPLAY "Y01 YYK MEI DEL ERR:" MCP-RC "."
                       ",KEY:" YYK-KEY
           END-IF
           .
       49030-YYK-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    予約更新処理
      *****************************************************************
       49030-YYK-UPDATE-SEC             SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
           MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
           MOVE    SPA-OPID            TO  YYK-OPID
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          NOT  =  ZERO
               MOVE    "1010"           TO  SPA-ERRCD
               DISPLAY "Y01 YYK UPD ERR:" MCP-RC 
                       ",KEY:" YYK-KEY
           END-IF
           .
       49030-YYK-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    予約メモ削除処理
      *****************************************************************
       49031-YYKCOM-DEL-SEC             SECTION.
      *
           MOVE    SPACE               TO  YYKCOM-REC
           INITIALIZE                      YYKCOM-REC
           MOVE    SPA-HOSPNUM         TO  YYKCOM-HOSPNUM
           MOVE    SPA-Y01-SRYNAIYO    TO  YYKCOM-SRYNAIYO
           MOVE    SPA-Y01-DRCD        TO  YYKCOM-DRCD
           MOVE    SPA-OLD-YYKYMD      TO  YYKCOM-YYKYMD
           MOVE    SPA-Y01-YYKTIME     TO  YYKCOM-KEYYYKTIME
           MOVE    SPA-Y01-YYKID       TO  YYKCOM-YYKID
      *
           MOVE    YYKCOM-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_yykcom"        TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       49031-YYKCOM-DEL-EXT.
           EXIT.
      *****************************************************************
      *    予約票印刷処理
      *****************************************************************
       4908-YYKHYO-PRINT-SEC             SECTION.
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"      USING   SPA-AREA
           MOVE    2                   TO  SPA-CLIENT-TEMP-FLG
      *
           IF     (SPA-CLIENT-PRT-FLG  =   2 )
             OR   (SPA-PRTCONF         =   "1" )
      *        クライアント印刷制御情報取得サブ
               PERFORM 4901-CLIENT-PRT-SEC
           END-IF
      *
      *    予約票出力
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000010"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1032-REC
           IF      ORCSPRTNM-RC        =   ZERO
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-YYKHYO-PG
           END-IF
      *
           INITIALIZE                  ORCHC67AREA
           MOVE    "1"                 TO  ORCHC67-KBN
      *    予約日
           MOVE    SPA-NAI-YYKYMD      TO  ORCHC67-SRYYMD
      *
           MOVE    SPA-AREA            TO  HZN-SPA-AREA
           IF      WRK-YYKHYO-PG   NOT =   SPACE
               MOVE    SPA-GMN-PTID        TO  SPA-PTID
               MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
               MOVE    SPA-GMN-NAME        TO  SPA-NAME
               MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
      *
               MOVE    "01"                TO  SPA-PRT-FLG
      *
               MOVE    ORCHC67AREA         TO  ONPRTDATA-DATA
               CALL    WRK-YYKHYO-PG       USING
                                           SPA-AREA
                                           ONPRTDATA
           END-IF
      *
           MOVE    HZN-SPA-AREA        TO  SPA-AREA
      *
           IF      (SPA-CLIENT-PRT-FLG =   1  )
      *        印刷ダミー処理
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
           MOVE    ZERO                TO  SPA-CLIENT-TEMP-FLG
      *
          .
       4908-YYKHYO-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    メモ登録画面へ
      *****************************************************************
       4100-Y011-SYORI-SEC            SECTION.
      *
      *    患者の指定がない時、文例登録へ
           IF     (SPA-GMN-PTNUM       =   SPACE )  AND
                  (SPA-GMN-PTID        =   ZERO  )  AND
                  (SPA-GMN-YYKNAME     =   SPACE )
               PERFORM 41002-Y012-SET-SEC
           ELSE
               PERFORM 41001-Y011-SET-SEC
           END-IF
           .
       4100-Y011-SYORI-SEC-EXT.
           EXIT.
      *****************************************************************
      *    メモ登録画面
      *****************************************************************
       41001-Y011-SET-SEC       SECTION.
      *
           MOVE    SPACE               TO  SPA-MOTOPG
      *
           MOVE    SPA-Y01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-Y01             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGY011"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-Y01KYOUTU
           MOVE    SPA-FREE            TO  SPA-Y01
      *
      *    カーソル移動
           MOVE    SPACE               TO  MCP-WIDGET
           PERFORM VARYING     IDD2    FROM    1   BY  1
                   UNTIL      (IDD2    >       6  )
               IF      SPA-Y011-COMMENT (IDD2) =   SPACE
                   STRING  "COMMENT"
                           IDD2                DELIMITED  BY  SIZE
                                               INTO    MCP-WIDGET
                   END-STRING
                   MOVE    6               TO  IDD2
               END-IF
           END-PERFORM
           IF      MCP-WIDGET          =   SPACE
               MOVE    "COMMENT01"         TO  MCP-WIDGET
           END-IF
      *
           MOVE    "Y01"               TO  SPA-MOTOPG
           MOVE    "Y011"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "Y011"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       41001-Y011-SET-EXT.
           EXIT.
      *****************************************************************
      *    文例選択画面
      *****************************************************************
       41002-Y012-SET-SEC       SECTION.
      *
           MOVE    "Y01"               TO  SPA-MOTOPG
           MOVE    "Y012"              TO  SPA-SAKIPG
      *
           MOVE    SPA-Y01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-Y01             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGY012"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-Y01KYOUTU
           MOVE    SPA-FREE            TO  SPA-Y01
      *
      *    カーソル移動
           MOVE    "BUNNUM"            TO  MCP-WIDGET
      *
           MOVE    "Y01"               TO  SPA-MOTOPG
           MOVE    "Y012"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "Y012"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       41002-Y012-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    来院済み　処理
      *****************************************************************
       450-RAIZUMI-SEC             SECTION.
      *
           IF      SPA-NAI-SELBAN      <   01   OR  >  SPA-GMN-MAX
               MOVE    "0016"          TO  SPA-ERRCD
               GO      TO      450-RAIZUMI-EXT
           END-IF
      *
      *    予約マスター読込処理
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    SPA-GMN-SRYNAIYOCD  TO  YYK-SRYNAIYO
           MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
           MOVE    SPA-NAI-TYYKYMD (SPA-NAI-SELBAN)
                                       TO  YYK-YYKYMD
           MOVE    SPA-NAI-TKYYKTIME(SPA-NAI-SELBAN)
                                       TO  YYK-KEYYYKTIME
           MOVE    SPA-NAI-TYYKID  (SPA-NAI-SELBAN)
                                       TO  YYK-YYKID
           PERFORM 910-YOYAKU-INV-SEC
           IF      FLG-YYK             =   ZERO
               IF      YYK-RAIINZUMI       =   "1"
                   MOVE    SPACE           TO  YYK-RAIINZUMI
               ELSE
                   MOVE    "1"             TO  YYK-RAIINZUMI
               END-IF
      *
               MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
               MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
               MOVE    SPA-OPID            TO  YYK-OPID
      *
      *        更新
               MOVE    YYK-REC             TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           END-IF
      *
      *    予約患者一覧再編集
           PERFORM 4001-KNJICHI-SEC
      *
           MOVE    ZERO                TO  SPA-NAI-SELBAN
           MOVE    SPACE               TO  SPA-GMN-SELBAN
      *
           MOVE    ZERO                TO  SPA-YYK-SYORI
           INITIALIZE                  SPA-GMN-YYKAREA
      *
           .
       450-RAIZUMI-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約一覧処理へ
      *****************************************************************
       440-YYKICHI-SEC            SECTION.
      *
           INITIALIZE                      SPA-Y01-YYK-AREA
      *
           MOVE    SPA-GMN-SRYNAIYO    TO  SPA-Y01-SRYNAIYOG
           MOVE    SPA-GMN-DRNAME      TO  SPA-Y01-DRNAME
           MOVE    SPA-NAI-DRCD        TO  SPA-Y01-DRCD
      *    予約日が選択時、予約時、以外の時、システム日付とする
           IF      SPA-SEL-YOUBI       =   ZERO
               MOVE    WRK-SYSYMD          TO  SPA-Y01-YYKYMDS
           ELSE
                MOVE    SPA-NAI-YYYKYMD (01  SPA-SEL-YOUBI)
                                           TO  SPA-Y01-YYKYMDS
           END-IF
      *    予約一覧の時ゼロを編集
           MOVE    ZERO                TO  SPA-Y01-YYKID
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    SPA-Y01KYOUTU       TO  SPA-WORK
      *
           MOVE    "Y01"               TO  SPA-MOTOPG
           MOVE    "Y03"               TO  SPA-SAKIPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "Y03"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
          .
       440-YYKICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    未院一覧処理へ
      *****************************************************************
       460-MINICHI-SEC            SECTION.
      *
           INITIALIZE                      SPA-Y01-YYK-AREA
      *
           MOVE    SPA-GMN-SRYNAIYO    TO  SPA-Y01-SRYNAIYOG
           MOVE    SPA-GMN-DRNAME      TO  SPA-Y01-DRNAME
           MOVE    SPA-NAI-DRCD        TO  SPA-Y01-DRCD
      *    予約日が選択時、予約時、以外の時、システム日付とする
           IF      SPA-SEL-YOUBI       =   ZERO
               MOVE    WRK-SYSYMD          TO  SPA-Y01-YYKYMDS
           ELSE
                MOVE    SPA-NAI-YYYKYMD (01  SPA-SEL-YOUBI)
                                           TO  SPA-Y01-YYKYMDS
           END-IF
      *    未院一覧の時01を編集
           MOVE    01                  TO  SPA-Y01-YYKID
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    SPA-Y01KYOUTU       TO  SPA-WORK
      *D   MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
      *    IF      SPA-MOTOPG2         =   SPACE
      *        MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
      *    END-IF
      *
           MOVE    "Y01"               TO  SPA-MOTOPG
           MOVE    "Y03"               TO  SPA-SAKIPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "Y03"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
          .
       460-MINICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者一覧処理へ
      *****************************************************************
       470-KANJCHI-SEC            SECTION.
      *
           INITIALIZE                      SPA-Y01-YYK-AREA
      *
           MOVE    SPA-GMN-SRYNAIYO    TO  SPA-Y01-SRYNAIYOG
           MOVE    SPA-GMN-DRNAME      TO  SPA-Y01-DRNAME
           MOVE    SPA-NAI-DRCD        TO  SPA-Y01-DRCD
      *    予約日が選択時、予約時、以外の時、システム日付とする
           IF      SPA-SEL-YOUBI       =   ZERO
               MOVE    WRK-SYSYMD          TO  SPA-Y01-YYKYMDS
           ELSE
                MOVE    SPA-NAI-YYYKYMD (01  SPA-SEL-YOUBI)
                                           TO  SPA-Y01-YYKYMDS
           END-IF
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    "Y01"               TO  SPA-MOTOPG
           MOVE    "Y04"               TO  SPA-SAKIPG
      *
           MOVE    SPA-Y01KYOUTU       TO  SPA-WORK
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "Y04"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *
          .
       470-KANJCHI-EXT.
           EXIT.
      *
      *****************************************************************
      *   予約日検索処理へ
      *****************************************************************
       470-PTNUM-ICHI-SEC            SECTION.
      *
           INITIALIZE                      SPA-Y01-YYK-AREA
           MOVE    SPA-GMN-SRYNAIYO    TO  SPA-Y01-SRYNAIYOG
           MOVE    SPA-GMN-DRNAME      TO  SPA-Y01-DRNAME
           MOVE    SPA-NAI-DRCD        TO  SPA-Y01-DRCD
      *
      *    予約日検索へ
           IF     (SPA-GMN-PTID        =   ZERO ) OR
                  (SPA-GMN-NAME        =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-PTID        TO  SPA-Y01-PTID
               MOVE    SPA-GMN-PTNUM       TO  SPA-Y01-PTNUM
               MOVE    SPA-GMN-NAME        TO  SPA-Y01-YYKNAME
               MOVE    SPA-GMN-SEX         TO  SPA-Y01-SEX
               MOVE    SPA-GMN-BIRTHDAYH   TO  SPA-Y01-BIRTHDAYH
           END-IF
      *
           MOVE    "Y01"               TO  SPA-MOTOPG
           MOVE    "Y05"               TO  SPA-SAKIPG
      *
           MOVE    SPA-Y01KYOUTU       TO  SPA-WORK
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "Y05"                TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
      *
          .
       470-PTNUM-ICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       470-SIMEIKEN-SEC             SECTION.
      *
      *    画面移行用のスパを編集する
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-GMN-SRYNAIYO    TO  SPA-Y01-SRYNAIYOG
           MOVE    SPA-GMN-DRNAME      TO  SPA-Y01-DRNAME
           MOVE    SPA-NAI-DRCD        TO  SPA-Y01-DRCD
      *
           MOVE    SPA-Y01KYOUTU       TO  SPA-WORK
      *
           MOVE    "Y01"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
           MOVE    SPA-GMN-YYKNAME     TO  SPA-NAME
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "P97    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       470-SIMEIKEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付一覧へ
      *****************************************************************
       490-UKEICHI-SEC             SECTION.
      *
      *    画面移行用のスパを編集する
           INITIALIZE                  SPA-KYOTUKEY
           INITIALIZE                  SPA-Y01-YYK-AREA
           MOVE    SPA-GMN-SRYNAIYO    TO  SPA-Y01-SRYNAIYOG
           MOVE    SPA-GMN-DRNAME      TO  SPA-Y01-DRNAME
           MOVE    SPA-NAI-DRCD        TO  SPA-Y01-DRCD
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    SPA-Y01KYOUTU       TO  SPA-WORK
      *
           MOVE    "Y01"               TO  SPA-MOTOPG
           MOVE    "U01"               TO  SPA-SAKIPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "U01"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-UKEICHI-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPA-MOTOPG3         TO  SPA-SAKIPG
           IF      SPA-MOTOPG3         =   SPACE
      *                                OR  "U01"
      *         業務選択へ
                MOVE   "M01"            TO  SPA-SAKIPG
           END-IF
           IF      SPA-MOTOPG3         =   "U01"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  SPA-MOTOPG3
           END-IF
           MOVE    "Y01"               TO  SPA-MOTOPG
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クライアント印刷前　処理
      *****************************************************************
       4901-CLIENT-PRT-SEC          SECTION.
      *
      *    クライアント印刷制御情報取得サブ
           INITIALIZE                  ORCSPRTCTRLAREA
           CALL    "ORCSPRTCTRL"       USING   SPA-AREA
                                               ORCSPRTCTRLAREA
                                               MCPAREA
      *
           .
       4901-CLIENT-PRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-YID1CD       NOT =   SPACE
               PERFORM 510-YID1CDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
      *    MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Y01    "           TO  MCP-WINDOW.
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
           IF      FLG-GMN-INIT        =   1
               MOVE    ZERO                TO  WRK-Y01-ROW
           ELSE
               MOVE    Y01-PTLIST-ROW      TO  WRK-Y01-ROW
           END-IF
      *
           MOVE    SPACE               TO  Y01
           INITIALIZE                      Y01
      *
           MOVE    ZERO                TO  Y01-PTLIST-ROW
           MOVE    ZERO                TO  Y01-PTLIST-ROWATTR
           IF      WRK-Y01-ROW     NOT =   ZERO
               MOVE    WRK-Y01-ROW         TO  Y01-PTLIST-ROW
           END-IF
      *
      *    カレンダー
      *    予約日が選択時、予約時、以外の時、システム日付とする
      *    IF      SPA-SEL-YOUBI       =   ZERO
      *********MOVE    WRK-SYSYMD          TO  SPA-Y01-YYKYMDS
      *        MOVE    SPA-GMN-YEAR        TO  WRK-CAL-YEAR
      *        MOVE    SPA-GMN-MONTH       TO  WRK-CAL-MONTH
      *        MOVE    SPA-GMN-DAY         TO  WRK-CAL-DAY
      *    ELSE
      *        MOVE    SPA-NAI-YYYKYMD (01  SPA-SEL-YOUBI)
      *                                    TO  SPA-Y01-YYKYMDS
      *        MOVE    SPA-Y01-YYKYMDS     TO  WRK-CAL-YMD
      *    END-IF
      ****  MOVE    SPA-Y01-YYKYMDS     TO  WRK-CAL-YMD
           IF      SPA-GMN-YEAR        =   ZERO
               MOVE    SPA-NAI-YYYKYMD (01  SPA-SEL-YOUBI)
                                           TO  SPA-Y01-YYKYMDS
               MOVE    SPA-Y01-YYKYMDS     TO  WRK-CAL-YMD
               MOVE    WRK-CAL-YMD         TO  SPA-GMN-CALENDR
           ELSE
               MOVE    SPA-GMN-CALENDR     TO  SPA-Y01-YYKYMDS
           END-IF
      *
           MOVE    SPA-GMN-YEAR        TO  Y01-YEAR
           MOVE    SPA-GMN-MONTH       TO  Y01-MONTH
           MOVE    SPA-GMN-DAY         TO  Y01-DAY
      *
      *    診療内容
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-SRYNAIYO-MAX
               MOVE    SPA-GMN-SRYNAIYOLST (IDX)
                                           TO  Y01-SRYNAIYO-LST (IDX)
           END-PERFORM
           MOVE    SPA-SRYNAIYO-MAX    TO  Y01-SRYNAIYO-COUNT
           MOVE    SPA-GMN-SRYNAIYO    TO  Y01-SRYNAIYO
      *    ドクター
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-DRCD-MAX
               MOVE    SPA-GMN-DRCDLST (IDX)
                                           TO  Y01-DRNAME-LST (IDX)
           END-PERFORM
           MOVE    SPA-DRCD-MAX        TO  Y01-DRNAME-COUNT
           MOVE    SPA-GMN-DRNAME      TO  Y01-DRNAME
      *    診療科
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               MOVE    SPA-GMN-SRYKALST (IDX)
                                           TO  Y01-SRYKA-LST (IDX)
           END-PERFORM
           MOVE    SPA-SRYKA-MAX       TO  Y01-SRYKA-COUNT
           MOVE    SPA-GMN-SRYKA-G     TO  Y01-SRYKA
      *    予約内容
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-YYKKBN-MAX
               MOVE    SPA-GMN-YYKKBNLST (IDX)
                                           TO  Y01-YYKKBN-LST (IDX)
           END-PERFORM
           MOVE    SPA-YYKKBN-MAX       TO  Y01-YYKKBN-COUNT
           MOVE    SPA-GMN-YYKKBN-G     TO  Y01-YYKKBN
      *    連絡確認
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-KAKREN-MAX
               MOVE    SPA-GMN-KAKRENLST (IDX)
                                           TO  Y01-KAKREN-LST (IDX)
           END-PERFORM
           MOVE    SPA-KAKREN-MAX      TO  Y01-KAKREN-COUNT
           MOVE    SPA-GMN-KAKREN-G    TO  Y01-KAKREN
      *
           MOVE    SPA-GMN-PTNUM       TO  Y01-PTNUM
      *****MOVE    SPA-GMN-NAME        TO  Y01-NAME
           MOVE    SPA-GMN-SEX         TO  Y01-SEX
           MOVE    SPA-GMN-BIRTHDAYH   TO  Y01-BARTHDAY
           MOVE    SPA-GMN-YYKNAME     TO  Y01-YYKNAME
           MOVE    SPA-GMN-YYKYMD      TO  Y01-YYKYMD
           MOVE    SPA-GMN-YYKTIME     TO  Y01-YYKTIME
      *    メモ
           MOVE    SPA-NAI-COMMENT(1)  TO  Y01-COMMENT1
      *
           MOVE    SPA-GMN-TAIYYMM     TO  Y01-TAIYYMM
           MOVE    SPA-GMN-TAIYMD      TO  Y01-TAIYMD
           MOVE    SPA-GMN-TAITIME     TO  Y01-TAIKENSUU
      *    MOVE    SPA-GMN-HYOJHI      TO  Y01-HYOJHI
      *    予約表示
           MOVE    "whitebutton"       TO  Y01-CH-DAY1-STYLE
                                           Y01-CH-DAY2-STYLE
                                           Y01-CH-DAY3-STYLE
                                           Y01-CH-DAY4-STYLE
                                           Y01-CH-DAY5-STYLE
                                           Y01-CH-DAY6-STYLE
                                           Y01-CH-DAY7-STYLE
           MOVE    SPA-GMN-GOKKEN (01) TO  Y01-DAY1
           MOVE    SPA-GMN-GOKKEN (02) TO  Y01-DAY2
           MOVE    SPA-GMN-GOKKEN (03) TO  Y01-DAY3
           MOVE    SPA-GMN-GOKKEN (04) TO  Y01-DAY4
           MOVE    SPA-GMN-GOKKEN (05) TO  Y01-DAY5
           MOVE    SPA-GMN-GOKKEN (06) TO  Y01-DAY6
           MOVE    SPA-GMN-GOKKEN (07) TO  Y01-DAY7
      *
           INITIALIZE   WRK-GMN-KENSUU
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   98
               MOVE    "whitebutton"       TO  WRK-TBL-KENSUU-STYLE(IDX)
               MOVE    SPACE               TO  WRK-HKENSUU
               MOVE    SPA-GMN-KENSUU (IDX)
                                           TO  WRK-KENSUU
      **       MOVE    WRK-HKENSUU(2:4)    TO  WRK-TBL-KENSUU (IDX)
               MOVE    WRK-HKENSUU         TO  WRK-TBL-KENSUU (IDX)
           END-PERFORM
           MOVE     WRK-GMN-KENSUU         TO  Y01-FIXED2
      *
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
      *******IF  SPA-GMN-TBANGO (IDX)  NOT  =  ZERO
               MOVE    SPA-GMN-TBANGO (IDX)
                                           TO  Y01-TBANGO (IDX)
               IF      SPA-GMN-TYYKTIME(IDX)   NOT =   SPACE
                   MOVE    SPA-GMN-TYYKTIME (IDX)(1:2)
                                           TO  Y01-TYYKTIME (IDX)(1:2)
                   MOVE    ":"             TO  Y01-TYYKTIME (IDX)(3:1)
                   MOVE    SPA-GMN-TYYKTIME (IDX)(3:2)
                                           TO  Y01-TYYKTIME (IDX)(4:2)
               END-IF
               MOVE    SPA-GMN-TNAME (IDX)
                                           TO  Y01-TNAME (IDX)
               MOVE    SPA-GMN-TKAMEI (IDX)
                                           TO  Y01-TKAMEI (IDX)
               MOVE    SPA-GMN-TRAIINZUMI(IDX)
                                           TO  Y01-TRAIINZUMI (IDX)
               MOVE    IDX
                                           TO  Y01-PTLIST-COUNT
      *        選択行反転
               IF      SPA-GMN-TBANGO (IDX)    =   SPA-NAI-SELBAN
                   MOVE    "T"             TO  Y01-PTLIST-SELECT  (IDX)
               ELSE
                   MOVE    "F"             TO  Y01-PTLIST-SELECT  (IDX)
               END-IF
      ****** END-IF
           END-PERFORM
           MOVE    SPA-GMN-MAX         TO  Y01-PTLIST-COUNT
      *
      *    予約票
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-YYKHYO-MAX
               MOVE    SPA-GMN-YYKHYO-LIST (IDX)
                                           TO  Y01-YYKHYO-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-YYKHYO-MAX      TO  Y01-YYKHYO-COUNT
           MOVE    SPA-GMN-YYKHYO-G    TO  Y01-YYKHYO
      *
           IF      SPA-YYK-MAX         =   ZERO
               MOVE    1               TO  SPA-GMN-PAGE
           ELSE
               COMPUTE SPA-GMN-PAGE    =   (SPA-YYK-MAX   -  1  )
                                       /   12   +   1
           END-IF
      *
           MOVE    SPA-GMN-SELBAN      TO  Y01-SELNUM
      *
           PERFORM 5001-MAPCUR-SEC
      *
      *    非活性処理
           PERFORM 510-GSRAUTH-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           MOVE    WIDGET-NORMAL       TO  Y01-PTNUM-STATE
           MOVE    WIDGET-NORMAL       TO  Y01-YYKNAME-STATE
      *
      *    選択番号が入力時は、患者番号入力不可
           IF      SPA-YYK-SYORI         =   2
               MOVE    WIDGET-INSENSITIVE      TO  Y01-PTNUM-STATE
               IF      SPA-GMN-PTNUM   NOT =   SPACE
                   MOVE    WIDGET-INSENSITIVE  TO  Y01-YYKNAME-STATE
               END-IF
           END-IF
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
          IF      SPA-GMN-CUR          =   ZERO
               PERFORM 50011-GMN-CURSET-SEC
          END-IF
      *
          EVALUATE    SPA-GMN-CUR
                WHEN    01
                    MOVE  "SRYNAIYO"    TO   MCP-WIDGET
                WHEN    02
                    MOVE  "DRNAME"      TO   MCP-WIDGET
                WHEN    03
                    MOVE  "PTNUM"       TO   MCP-WIDGET
                WHEN    04
                    MOVE  "YYKNAME"     TO   MCP-WIDGET
                WHEN    05
                    MOVE  "YYKYMD"      TO   MCP-WIDGET
                WHEN    06
                    MOVE  "YYKTIME"     TO   MCP-WIDGET
                WHEN    07
                    MOVE  "SRYKA"       TO   MCP-WIDGET
                WHEN    08
                    MOVE  "YYKKBN"      TO   MCP-WIDGET
                WHEN    09
                    MOVE  "KAKREN"      TO   MCP-WIDGET
                WHEN    10
                    MOVE  "B12"         TO   MCP-WIDGET
                WHEN    11
                    MOVE  "DAY1"        TO   MCP-WIDGET
                WHEN    12
                    MOVE  "SELNUM"      TO   MCP-WIDGET
                WHEN    13
                    MOVE  "COMMENT1"    TO   MCP-WIDGET
                WHEN    14
                    MOVE  "YYKHYO"      TO   MCP-WIDGET
                WHEN    OTHER
                    MOVE  "DAY1"        TO   MCP-WIDGET
            END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括入力後カーソルセット処理
      *****************************************************************
       50011-GMN-CURSET-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "YYKYMD"
                   MOVE    6                   TO  SPA-GMN-CUR
               WHEN    "YYKTIME"
                   MOVE    7                   TO  SPA-GMN-CUR
               WHEN    "SRYKA"
                   MOVE    8                   TO  SPA-GMN-CUR
               WHEN    "YYKKBN"
                   MOVE    9                   TO  SPA-GMN-CUR
               WHEN    "KAKREN"
      *********    MOVE    10                  TO  SPA-GMN-CUR
                   MOVE    13                  TO  SPA-GMN-CUR
               WHEN    "COMMENT1"
                   MOVE    10                  TO  SPA-GMN-CUR
               WHEN    "YYKHYO"
                   MOVE    10                  TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-GMN-CURSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "診療内容コードの入力に誤りがあります"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "ドクターコードの入力に誤りがあります"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "入力された患者番号は登録されていません"
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "該当の患者が存在しません"
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "選択番号に誤りがあります"
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "患者番号と予約氏名が共に入力されています"
                                               TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "予約日の暦日エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "予約時間はHH:SSで入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "診療科選択エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "予約内容選択エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "予約時間の入力に誤りがあります"
                                               TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "予約時間は２４時間で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "予約患者を指定して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "予約時間を指定して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "カレンダーの選択エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "選択番号選択エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "予約氏名は全角で入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "予約一覧が表示できません。"
                                               TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "メモ１は全角で入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0020"
                   STRING  "同姓同名の患者が存在します。"
                           "患者を変更するなら再度予約して下さい"
                                               DELIMITED   BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K100"
                   MOVE    "警告！予約件数がオーバーしてます"
                                               TO  SPA-ERRMSG
               WHEN    "1010"
                   MOVE    "予約キーデータ更新エラー"
                                               TO  SPA-ERRMSG
               WHEN    "1011"
                   MOVE    "予約明細データ更新エラー"
                                               TO  SPA-ERRMSG
               WHEN    "1012"
                   MOVE    "予約明細データ削除エラー"
                                               TO  SPA-ERRMSG
               WHEN    "1013"
                   STRING  "予約時間帯の予約登録回数が９９以上に"
                           "なります。"
                           "これ以上登録できません。"
      ******************   "予約時間の予約すべて削除して下さい。"
                                       DELIMITED   BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K001"
                   MOVE    "注意！予約日が過去です。"
                                               TO  SPA-ERRMSG
      *
               WHEN    "9999"
                   MOVE    "他端末で使用中です。"
                                               TO  SPA-ERRMSG
                   MOVE    SLOCK-MSG           TO  SPA-ERRMSG(21:)
      *
           END-EVALUATE
      *
           MOVE    SPACE                TO  YERR
           MOVE    SPA-ERRCD            TO  YERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  YERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "Y01"                TO  SPA-MOTOPG
           MOVE    "YERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "YERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
       510-ERRMSG-EXT.
           EXIT.
      *****************************************************************
      *    確認メッセージ表示処理
      *****************************************************************
       510-YID1CDSET-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-YID1MSG
      *
           MOVE    SPACE               TO  WRK-YID1MSG
           MOVE    SPACE               TO  SPA-YID1-FLG
      *
           EVALUATE    SPA-YID1CD
               WHEN    "0101"
                   STRING  "予約氏名で該当の患者が１件存在します。"
                           "患者番号を編集しますか？"
                                           DELIMITED  BY  SIZE
                                           INTO    WRK-YID1MSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-YID1CD           TO  WRK-YID1MSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  YID1
           INITIALIZE                       YID1
           MOVE    SPA-YID1CD           TO  YID1-ID1CODE
           MOVE    WRK-YID1MSG          TO  YID1-ID1MSG
           MOVE    "ＮＯ"               TO  YID1-B01
      *
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "Y01"                TO  SPA-MOTOPG
           MOVE    "YID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "YID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-YID1CDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスター読込処理
      *****************************************************************
       900-YOYAKU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-YYK
               MOVE    MCPDATA-REC         TO  YYK-REC
           ELSE
               MOVE    1                   TO  FLG-YYK
           END-IF
      *
           .
       900-YOYAKU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスター読込処理（主キー検索、読み込み）
      *****************************************************************
       910-YOYAKU-INV-SEC         SECTION.
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-YOYAKU-READ-SEC
           ELSE
                MOVE    1              TO  FLG-YYK
           END-IF
      *    カーソルクロース
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-YOYAKU-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       910-PTINF-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTINF
               MOVE    MCPDATA-REC         TO  PTINF-REC
           ELSE
               MOVE    1                   TO  FLG-PTINF
           END-IF
      *
           .
       910-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       920-PTINF-READ-SEC         SECTION.
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
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       920-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRIT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRIT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約メモ　ＲＥＡＤ
      *****************************************************************
       900-YYKCOM-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  YYKCOM-REC
               MOVE    ZERO            TO  FLG-YYKCOM
           ELSE
               INITIALIZE                  YYKCOM-REC
               MOVE    1               TO  FLG-YYKCOM
           END-IF
      *
           .
       900-YYKCOM-READ-EXT.
           EXIT.
      *****************************************************************
      *    予約文例　ＲＥＡＤ
      *****************************************************************
       900-YYKEXAMPLE-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  YYKEXAMPLE-REC
               MOVE    ZERO            TO  FLG-YYKEXAMPLE
           ELSE
               INITIALIZE                  YYKEXAMPLE-REC
               MOVE    1               TO  FLG-YYKEXAMPLE
           END-IF
      *
           .
       900-YYKEXAMPLE-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING
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
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-GMN-PTID        TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    排他制御解除処理
           MOVE    2                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL   "ORCDBMAIN"       USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
