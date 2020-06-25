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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORAPI021SUB3V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 診療内容編集サブ（一体化）
      *                      （J02対象をSPAとAPI返却編集）
      *  管理者            :
      *  作成日付    作業者        記述
      *  18/12/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PARA                PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-SRYACCTPLUS         PIC 9(01).
           03  FLG-TENSUPLUS           PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTCOM               PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
      *
           03  FLG-HKN-CHK             PIC 9(01).
           03  FLG-HENCHK              PIC 9(01).
           03  FLG-SP                  PIC 9(01).
           03  FLG-CHOZAIAUTO          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDW                 PIC 9(04).
      *
           03  IDX-A               PIC 9(04).
      *
           03  IDXM                PIC 9(04).
           03  IDXR                PIC 9(04).
           03  IDX-D               PIC 9(02).
      *
           03  IDX-C               PIC 9(04).
           03  IDX-H               PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDH                 PIC 9(04).
           03  IDX-IDH             PIC 9(04).
      *
      *    カウント領域
      *01  CNT-AREA.
      *    03  CNT-SRYCNT              PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *
      *    数値変換用
           03  WRK-SURYO               PIC S9(08)V9(05).
           03  WRK-SURYO-S             PIC 9(08)V9(05).
           03  WRK-SURYO-R             REDEFINES   WRK-SURYO-S.
               05  WRK-SURYO-S1        PIC 9(08).
               05  WRK-SURYO-S2        PIC 9(05).
           03  WRK-SURYO2-S1           PIC S9(08).
      *
           03  WRK-SURYO-XX            PIC X(15).
           03  WRK-SURYO-X             REDEFINES    WRK-SURYO-XX.
               05  WRK-SURYO-Z1        PIC -(08)9.
               05  WRK-SURYO-Z2        PIC X(01).
               05  WRK-SURYO-Z3        PIC ZZZZZ.
           03  WRK-SURYO-HX            PIC X(15).
           03  WRK-S-MAX               PIC 9(02).
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC X(06).
               05  WRK-SRYDD           PIC 9(02).
           03  WRK-ACTSRYYMD.
               05  WRK-ACTSRYYM        PIC X(06).
               05  WRK-ACTSRYDD        PIC 9(02).
      *
           03  WRK-SRYSYUKBN           PIC X(03).
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-SRYSYUKBNMEI        PIC X(40).
           03  WRK-SRYCD               PIC X(09).
      *    包括検査数
           03  WRK-HOUKNS-CNT          PIC 9(02).
      *
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-NAME          PIC X(20).
      *
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(100).
           03  WRK-ATO-INPUT           PIC X(100).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *    保険組み合わせテーブル
       01  TBL-HKNCOMIB-AREA.
         02  TBL-HKNCOMBI-REC              OCCURS   100.
           COPY    "CPHKNCOMBI.INC"    REPLACING
                                     //COMB-// BY //TBL-COMB-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
      *    COPY    "CPORCSMCNDATE.INC".
      *
      *    COPY    "CPORCXKANACONV.INC".
      *
      *    COPY    "CPORCSKANACHK.INC".
      *    日付変換サブ
      *    COPY    "CPORCSDAY.INC".
      *    COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    診療行為入力項目変換パラメタ（入院の算定日変換）
      *    COPY    "CPORCSC97.INC".
      *
      *    一括日変換パラメタ
      *    COPY    "CPORCSNDAYCHG.INC".
      *
      *    ＳＰＡパラメタ（ORCSC97.CBL 用のDAMMY)
      *01  SPA-SCR-REC.
      *    COPY    "CPK02SPASCR.INC".
      *
      *H30.XX
      *    保険組合せセット領域
      *    COPY    "CPORCSAPIHKN.INC".
      *!!!!!
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
      *    診療科情報
           COPY    "CPSK1005.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療会計附加情報マスタ−
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    一般名テーブル
      *01  GENERICNAME-REC.
      *    COPY    "CPGENERICNAME.INC".
      *
      *     パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *
           COPY    "MCPDATA.INC".
      *
      *    COPY    "ORCA-BLOB".
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "MCPAREA".
      *    スパ領域
           COPY    "COMMON-SPA".
      *    剤対象データ編集領域
           COPY    "CPORAPI021SUB3V3.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPA-AREA
           ORAPI021SUB3V3AREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  ORAPISUB3-ERRCD
           INITIALIZE                  ORAPISUB3-ERRMSG
           INITIALIZE                  ORAPISUB3-HENSYU-AREA
           INITIALIZE                  TBL-HKNCOMIB-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           PERFORM 200-MAIN-SEC
      *
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    パラメタより診療会計マスターを編集
           INITIALIZE                      PARA-REC
           MOVE    "J02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SRYACCT"           TO  PARA-FILEMEI
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
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           PERFORM
               UNTIL  (FLG-PARA            =   1 )
                   OR (FLG-HENCHK          =   1 )
               MOVE    PARA-DATA-REC       TO  SRYACCT-REC
               PERFORM 20021-SRYACCT-SPA-SEC
      *
               IF      IDX                 >=  400
                   MOVE    1               TO  FLG-HENCHK
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
           IF     (FLG-HENCHK          =   1     )
              AND (FLG-PARA            =   ZERO  )
      *        剤数オーバー
               MOVE    "AP03"              TO  ORAPISUB3-ERRCD
               MOVE    "剤数が最大件数以上となります"
                                           TO  ORAPISUB3-ERRMSG
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *   診療会計マスターより画面内容編集処理
      *****************************************************************
       20021-SRYACCT-SPA-SEC             SECTION.
      *
           ADD     1                   TO  IDX
           ADD     1                   TO  ORAPISUB3-ZAI-MAX
      *
           MOVE    ACCT-SRYKA          TO  ORAPISUB3-SRYKA (IDX)
           MOVE    ACCT-HKNCOMBI       TO  ORAPISUB3-HKNCOMBI(IDX)
           MOVE    ACCT-ZAINUM         TO  ORAPISUB3-ZAINUM(IDX)
           MOVE    ACCT-SRYKBN         TO  ORAPISUB3-SRYKBN(IDX)
           MOVE    ACCT-JIHOKBN        TO  ORAPISUB3-JIHOKBN (IDX)
           MOVE    ACCT-ZAITEN         TO  ORAPISUB3-ZAITEN (IDX)
           MOVE    ACCT-ZAIKBN         TO  ORAPISUB3-ZAIKBN (IDX)
      *    剤回数
           MOVE    ACCT-DAY-AREA       TO  ORAPISUB3-NAI-DAY-AREA(IDX)
      *    点数検索用日付編集
           MOVE    ACCT-SRYYM          TO  WRK-SRYYMD(1:6)
           MOVE    01                  TO  WRK-SRYDD
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  NOT =   ZERO
                   MOVE    IDX-D           TO  WRK-SRYDD
                   MOVE    31              TO  IDX-D
               END-IF
           END-PERFORM
      *
           INITIALIZE                  SRYACCTPLUS-REC
           IF      ACCT-ZAIREQFLG      =   "1"
      *        附加情報あり
               PERFORM 940-SRYACCTPLUS-READ-SEC
      *        減点
               IF     (ACCTP-PLUSKBN       =   1    )
                 AND  (ACCT-ZAITEN         >   ZERO )
                   MOVE    1               TO  ORAPISUB3-PLUSKBN(IDX)
               END-IF
           END-IF
      *
      *    診療科名称編集
           PERFORM 20022-SRYKA-HEN-SEC
      *
      *    保険組合せ名称編集
           PERFORM 20023-HKNCOMBI-HEN-SEC
      *
      *    診療行為内容編集
           PERFORM 20021-SRYACT-HEN-SEC
      *
           .
       20021-SRYACCT-SPA-EXT.
           EXIT.
      *****************************************************************
      *   診療科名称編集処理
      *****************************************************************
       20022-SRYKA-HEN-SEC            SECTION.
      *
      *    診療科名称
           MOVE    SPACE               TO  WRK-SRYKA-NAME
           PERFORM VARYING     IDX-C   FROM    1   BY  1
                   UNTIL      (IDX-C       >=  IDX  )
                          OR  (ORAPISUB3-SRYKA (IDX-C)
                                                   =   SPACE )
                          OR  (WRK-SRYKA-NAME  NOT =   SPACE )
               IF      ACCT-SRYKA          =   ORAPISUB3-SRYKA
                                                       (IDX-C)
                   MOVE    ORAPISUB3-SRYKA-NAME (IDX-C)
                                       TO  WRK-SRYKA-NAME
               END-IF
           END-PERFORM
           IF      WRK-SRYKA-NAME      =   SPACE
      *        診療科の存在チェック
               MOVE    SPACE               TO  SYS-1005-REC
               INITIALIZE                  SYS-1005-REC
               MOVE    "1005"              TO  SYS-1005-KANRICD
               MOVE    ACCT-SRYKA          TO  SYS-1005-KBNCD
               MOVE    WRK-SRYYMD          TO  SYS-1005-STYUKYMD
               MOVE    WRK-SRYYMD          TO  SYS-1005-EDYUKYMD
      *        システム管理検索
               MOVE    SYS-1005-REC        TO  SYSKANRI-REC
               PERFORM 900-SYSKANRI-SEL-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    SYSKANRI-REC        TO  SYS-1005-REC
                   MOVE    SYS-1005-SRYKANAME1 TO  WRK-SRYKA-NAME
               END-IF
           END-IF
           MOVE    WRK-SRYKA-NAME      TO  ORAPISUB3-SRYKA-NAME
                                                             (IDX)
           .
       20022-SRYKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *   保険組合せ名称編集処理
      *****************************************************************
       20023-HKNCOMBI-HEN-SEC            SECTION.
      *
      *    保険組合せ名称
           MOVE    ACCT-HKNCOMBI       TO  WRK-HKNCOMBI
      *
           MOVE    ZERO                TO  IDX-IDH
           PERFORM VARYING     IDX-C   FROM    1   BY  1
                   UNTIL      (IDX-C       >=  IDH  )
                          OR  (IDX-IDH     NOT =   ZERO  )
               IF      TBL-COMB-HKNCOMBINUM (IDX-C)
                                               =   ACCT-HKNCOMBI
                   MOVE    IDX-C           TO  IDX-IDH
               END-IF
           END-PERFORM
           IF      IDX-IDH             =   ZERO
               IF      ACCT-HKNCOMBI       =   "9999"
                   MOVE    "包括分入力"    TO  ORAPISUB3-MAIN-INS-NAME
                                                           (IDX)
               ELSE
                   PERFORM 2002121-HKNCOMBI-HENSYU-SEC
               END-IF
           END-IF
      *    保険組合せ番号なし
           IF      IDX-IDH             =   ZERO
               MOVE    "1"             TO  ORAPISUB3-COMBINATION-MODE
                                                               (IDX)
               GO      TO    20023-HKNCOMBI-HEN-EXT
           END-IF
      *
      *    保険名称
      *    主保険−短縮制度名
           MOVE    TBL-COMB-SYU-TANSEIDONAME(IDX-IDH)
                                   TO  ORAPISUB3-MAIN-INS-NAME (IDX)
      *    公費情報
           PERFORM VARYING     IDK     FROM    1   BY  1
                   UNTIL      (IDK     >   4   )
                           OR (TBL-COMB-KOHHKNNUM (IDX-IDH IDK)
                                       =   SPACE)
      *        公費の種類
               MOVE    TBL-COMB-KOH-TANSEIDONAME(IDX-IDH IDK)
                                   TO  ORAPISUB3-INSURANCE-NAME
                                                      (IDX IDK)
           END-PERFORM
      *
           IF      TBL-COMB-DELKBN(IDX-IDH)    NOT  =   SPACE
      *         削除分
                MOVE    "1"            TO  ORAPISUB3-COMBINATION-MODE
                                                               (IDX)
           END-IF
      *
      *    有効期間判定
           MOVE    ZERO                TO  FLG-HKN-CHK
           MOVE    ACCT-SRYYM          TO  WRK-ACTSRYYM
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  NOT =   ZERO
                   MOVE    IDX-D               TO  WRK-ACTSRYDD
      *            保険組合せ期間外
                   IF     (WRK-ACTSRYYMD       <   TBL-COMB-TEKSTYMD
                                                            (IDX-IDH))
                     OR   (WRK-ACTSRYYMD       >   TBL-COMB-TEKEDYMD
                                                            (IDX-IDH))
                       MOVE    1               TO  FLG-HKN-CHK
                       MOVE    31              TO  IDX-D
                   END-IF
               END-IF
           END-PERFORM
      *    期間外あり
           IF      FLG-HKN-CHK     =   1
                MOVE    "2"        TO  ORAPISUB3-COMBINATION-MODE
                                                              (IDX)
           END-IF
           .
       20023-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *   保険組合せ内容編集処理
      *****************************************************************
       2002121-HKNCOMBI-HENSYU-SEC             SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    ACCT-HOSPNUM        TO  COMB-HOSPNUM
           MOVE    ACCT-PTID           TO  COMB-PTID
           MOVE    ACCT-HKNCOMBI       TO  COMB-HKNCOMBINUM
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   MOVE    ZERO                TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                  HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *
           ADD     1                   TO  IDH
           IF      IDH                 <=  100
               MOVE    HKNCOMBI-REC        TO  TBL-HKNCOMBI-REC (IDH)
               MOVE    IDH                 TO  IDX-IDH
           END-IF
      *
      *
           .
       2002121-HKNCOMBI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *   診療会計マスターより画面内容編集処理
      *****************************************************************
       20021-SRYACT-HEN-SEC            SECTION.
      *
           MOVE    ZERO                TO  IDY
      *    包括検査数
           MOVE    ZERO                TO  WRK-HOUKNS-CNT
           MOVE    ZERO                TO  FLG-CHOZAIAUTO
      *
      *    診療行為マスター読み込み
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-RAED-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           IF      FLG-SRYACT          =   ZERO
      *        診療種別区分
               MOVE    SRY-SRYSYUKBN       TO  ORAPISUB3-SRYSYUKBN
                                                              (IDX)
      *        自費金額
               MOVE    SRY-JIHIMONEYTOTAL  TO
                                           ORAPISUB3-JIHIMONEYTOTAL
                                                              (IDX)
           END-IF
      *
           PERFORM UNTIL       FLG-SRYACT      =   1
      *        剤明細編集
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ         >   5   )  OR
                                  (IDY         >=  50  )  OR
                                  (SRY-SRYCD (IDZ) =   SPACE )
               IF      SRY-SRYCD (IDZ)(1:1)   =   "S"
                   CONTINUE
               ELSE
      *
                   ADD     1                   TO  IDY
                   MOVE    SRY-SRYCD (IDZ)     TO  ORAPISUB3-SRYCD
                                                               (IDX IDY)
                   MOVE    SRY-SRYSURYO (IDZ)  TO  ORAPISUB3-SRYSURYO
                                                               (IDX IDY)
                   MOVE    SRY-SRYKAISU (IDZ)  TO  ORAPISUB3-SRYKAISU
                                                               (IDX IDY)
                   MOVE    SRY-MEISKYFLG (IDZ) TO  ORAPISUB3-MEISKYFLG
                                                               (IDX IDY)
                   MOVE    SRY-AUTOKBN (IDZ)   TO  ORAPISUB3-AUTOKBN
                                                               (IDX IDY)
                   MOVE    SRY-INPUTNUM (IDZ)  TO  ORAPISUB3-INPUTNUM
                                                               (IDX IDY)
      *            換算数量入力
                   MOVE    SRY-KANSURYO (IDZ)  TO  ORAPISUB3-KANSURYO
                                                               (IDX IDY)
      *            API用編集
                   PERFORM 200211-APMEI-HEN-SEC
               END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-RAED-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    API用編集
           PERFORM 200212-APHEAD-HEN-SEC
      *
           .
       20021-SRYACT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     API剤編集処理
      *****************************************************************
       200212-APHEAD-HEN-SEC          SECTION.
      *
      *    剤点数
           IF      ORAPISUB3-ZAITEN (IDX)  NOT =   ZERO
      *        減点
               IF     (ORAPISUB3-PLUSKBN(IDX)  =   1    )
                   COMPUTE WRK-SURYO       =   ORAPISUB3-ZAITEN (IDX)
                                           *   -1
               ELSE
                   MOVE    ORAPISUB3-ZAITEN (IDX)  TO  WRK-SURYO
               END-IF
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  ORAPISUB3-MDC-POINT(IDX)
           END-IF
      *    剤金額
           IF      ORAPISUB3-JIHIMONEYTOTAL(IDX)   >   ZERO
               MOVE    ORAPISUB3-JIHIMONEYTOTAL(IDX)
                                           TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  ORAPISUB3-MDC-MONEY(IDX)
           END-IF
      *    剤区分（包括分）
           IF      ORAPISUB3-JIHOKBN (IDX) =   "1"
                MOVE    "1"            TO  ORAPISUB3-MDC-CODE (IDX)
           END-IF
      *    診療区分
           MOVE    ORAPISUB3-SRYKBN(IDX)   TO  WRK-SRYKBN
           MOVE    ORAPISUB3-SRYSYUKBN(IDX)    TO  WRK-SRYSYUKBN
           IF      WRK-SRYSYUKBN       =   SPACE
               MOVE    WRK-SRYKBN          TO  WRK-SRYSYUKBN(1:2)
               MOVE    "0"                 TO  WRK-SRYSYUKBN(3:1)
           END-IF
           PERFORM 510-SRYKBN-MEI-SEC
           MOVE    WRK-SRYSYUKBNMEI    TO  ORAPISUB3-MDC-CLASS-NAME
                                                                (IDX)
      *
      *    包括検査の検査数
           IF     (WRK-HOUKNS-CNT      >   ZERO )
                 MOVE    WRK-HOUKNS-CNT    TO  ORAPISUB3-HKTKNSA-CNT
                                                                 (IDX)
           END-IF
      *    入院調剤料自動算定剤
           IF      FLG-CHOZAIAUTO      =   1
                MOVE    "1"            TO  ORAPISUB3-CHOUZAIAUTO (IDX)
           END-IF
           .
       200212-APHEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤明細編集処理
      *****************************************************************
       200211-APMEI-HEN-SEC          SECTION.
      *
      *    コード
           MOVE    SRY-SRYCD  (IDZ)    TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           MOVE    TNS-NAME            TO  ORAPISUB3-PRSCRPT-NAME 
                                                             (IDX IDY)
      *    コメント名称
           IF    ((SRY-SRYCD  (IDZ)(1:1)   =   "8"  )  OR
                  (SRY-SRYCD  (IDZ)(1:3)   =   "008"
                                           OR  "001")  OR
                  (SRY-SRYCD  (IDZ)(1:7)   =   "0992081"))
             AND  (SRY-INPUTNUM(IDZ)   NOT =   ZERO  )
               PERFORM 200211-PTCOM-SET-SEC
           END-IF
      *
      *    数量
           MOVE    SRY-SRYSURYO(IDZ)   TO  WRK-SURYO
           IF      WRK-SURYO           =   ZERO
      *        ZERO は１とする（CLAIMでゼロあり）
               MOVE    1                   TO  WRK-SURYO
           END-IF
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  ORAPISUB3-PRSCRPT-NUMBER
                                                             (IDX IDY)
      *
      *    単位コード
           IF     (SRY-SRYCD (IDZ)(1:1)    =   "6"
                                           OR  "7" )
      *        薬剤・器材
               MOVE    TNS-TANICD      TO  ORAPISUB3-PRSCRPT-TANICD
                                                             (IDX IDY)
               MOVE    TNS-TANINAME    TO  ORAPISUB3-PRSCRPT-TANICD-NAME
                                                             (IDX IDY)
           END-IF
           IF    ((SRY-SRYCD (IDZ)(1:1)    =   "1" )  OR
                  (SRY-SRYCD (IDZ)(1:3)    =   "058"
                                           OR  "059"
                                           OR  "095"
                                           OR  "096"))
               IF      TNS-TANICD          NOT =   ZERO
                   MOVE    TNS-TANICD      TO  ORAPISUB3-PRSCRPT-TANICD
                                                             (IDX IDY)
                   MOVE    TNS-TANINAME    TO
                                           ORAPISUB3-PRSCRPT-TANICD-NAME
                                                             (IDX IDY)
               END-IF
           END-IF
      *
      *    分画数を編集
           IF     (SRY-SRYKBN              =   "70" )
              AND (SRY-SRYCD(IDZ)(1:1)     =   "7"  )
              AND (SRY-SRYKAISU(IDZ)   NOT =   ZERO )
               MOVE    SRY-SRYKAISU(IDZ)   TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  ORAPISUB3-PRSCRPT-FILMNUM
                                                             (IDX IDY)
           END-IF
      *
      *    自費金額・自賠責金額
           IF     (SRY-SRYKBN          =   "96"
                                       OR  "95"  )
           OR     (SRY-SRYSYUKBN       =   "809" )
               IF      SRY-JIHIMONEY(IDZ)  >   ZERO
                   MOVE    SRY-JIHIMONEY(IDZ)  TO  WRK-SURYO
                   PERFORM 810-NUMHENKAN-SEC
                   MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  ORAPISUB3-PRSCRPT-MONEY
                                                             (IDX IDY)
               END-IF
           END-IF
      *    器材の商品名入力がある時
           IF     (SRY-SRYCD(IDZ)(1:1) =   "7" )
             AND  (SRY-MEISKYFLG(IDZ)  =   "1" )
      *        器材商品名対応
               MOVE    "2"             TO  ORAPISUB3-PRSCRPT-CONTKBN
                                                             (IDX IDY)
           END-IF
      *    継続コメント
           IF    ((SRY-SRYCD(IDZ)(1:1) =   "8"  )
             OR   (SRY-SRYCD(IDZ)(1:3) =   "008"))
             AND  (SRY-MEISKYFLG(IDZ)  =   "2"   )
               MOVE    "1"            TO  ORAPISUB3-PRSCRPT-CONTKBN
                                                             (IDX IDY)
           END-IF
      *
      *    自動算定区分
           IF      SRY-AUTOKBN (IDZ)   =   "2"
                                       OR  "3"
                                       OR  "4"
                                       OR  "5"
                                       OR  "6"
                                       OR  "7"
               MOVE    SRY-AUTOKBN (IDZ)   TO  ORAPISUB3-PRSCRPT-AUTOKBN
                                                             (IDX IDY)
           END-IF
      *
      *    包括検査の判定
           IF     (SRY-SRYCD(IDZ)(1:1) =   "1" )  AND
                  (SRY-SRYKBN          =   "60" ) AND
                  (TNS-DATAKBN         =   1    ) AND
                  (TNS-HOUKNSKBN   NOT =   ZERO )
               ADD     1                   TO  WRK-HOUKNS-CNT
           END-IF
      *
      *    入院調剤料自動算定区分
           IF     (ACCT-NYUGAIKBN      =   "1" )
              AND (SRY-SRYKBN          =   "24"
                                       OR  "26" )
              AND (SRY-SRYCD(IDZ)(1:1) =   "1" )
              AND (SRY-AUTOKBN (IDZ)   =   "3" )
               MOVE    1                   TO   FLG-CHOZAIAUTO
           END-IF
           .
       200211-APMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者コメントから編集する
      *****************************************************************
       200211-PTCOM-SET-SEC              SECTION.
      *
           MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
           MOVE    SRY-PTID            TO  PTCOM-PTID
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    SRY-SRYCD (IDZ)     TO  PTCOM-SRYCD
           MOVE    SRY-INPUTNUM(IDZ)   TO  PTCOM-RENNUM
      *主キーで検索
           PERFORM 950-PTCOM-READ-SEC
           IF      FLG-PTCOM           =   ZERO
      *        名称
               MOVE    PTCOM-INPUTCOMENT   TO  ORAPISUB3-PRSCRPT-NAME 
                                                           (IDX IDY)
      *
               MOVE    PTCOM-INPUTCHI (1)  TO  ORAPISUB3-PRSCRPT-ATAI
                                                           (IDX IDY 1)
               MOVE    PTCOM-INPUTCHI (2)  TO  ORAPISUB3-PRSCRPT-ATAI
                                                           (IDX IDY 2)
               MOVE    PTCOM-INPUTCHI (3)  TO  ORAPISUB3-PRSCRPT-ATAI
                                                           (IDX IDY 3)
               MOVE    PTCOM-INPUTCHI (4)  TO  ORAPISUB3-PRSCRPT-ATAI
                                                           (IDX IDY 4)
               MOVE    PTCOM-INPUTCHI (5)  TO  ORAPISUB3-PRSCRPT-ATAI
                                                           (IDX IDY 5)
           END-IF
           .
       200211-PTCOM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療種別区分名称、診療行為区分セット
      *****************************************************************
       510-SRYKBN-MEI-SEC            SECTION.
      *
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
               AT   END
                   MOVE    SPACE           TO  WRK-SRYKBN
                   MOVE    SPACE           TO  WRK-SRYSYUKBNMEI
               WHEN    WRK-SRYSYUKBN       =   MSYU-SRYSYUKBN 
                                                           (MSYU-IDX)
                   MOVE    MSYU-SRYKBN  (MSYU-IDX)
                                                TO  WRK-SRYKBN
                   MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-SRYSYUKBNMEI
           END-SEARCH 
      *
           .
       510-SRYKBN-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    数値変換編集処理
      *****************************************************************
       810-NUMHENKAN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           IF      WRK-SURYO           <   ZERO
               COMPUTE WRK-SURYO-S         =   WRK-SURYO      *  -1
               COMPUTE WRK-SURYO2-S1       =   WRK-SURYO-S1   *  -1
               MOVE    WRK-SURYO2-S1       TO  WRK-SURYO-Z1
           ELSE
               MOVE    WRK-SURYO           TO  WRK-SURYO-S
               MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
           END-IF
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDXM    FROM    5  BY  -1
                   UNTIL       IDXM    <   1
               IF      WRK-SURYO-S2(IDXM:1)    NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDXM:1)    TO  WRK-SURYO-Z3
                                                             (IDXM:1)
               END-IF
           END-PERFORM
           MOVE    SPACE               TO  WRK-SURYO-Z2
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
           END-IF
      *    左詰
           MOVE    SPACE              TO  WRK-SURYO-HX
           MOVE    1                  TO  IDXR
           MOVE    ZERO               TO  WRK-S-MAX
           PERFORM VARYING     IDXM    FROM    1    BY  1
                   UNTIL       IDXM    >   15
               IF      WRK-SURYO-X (IDXM:1)    NOT =   SPACE
                   MOVE    WRK-SURYO-X (IDXM:1)    TO  WRK-SURYO-HX
                                                       (IDXR:1)
                   COMPUTE IDXR            =   IDXR    +   1
      *            桁数
                   ADD     1               TO  WRK-S-MAX
               END-IF
           END-PERFORM
           .
       810-NUMHENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
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
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *R02.2
      *    ユーザー単位反映
           IF     (TNS-SRYCD(1:1)      =   "6"
                                       OR  "7"  )
             AND  (FLG-TENSU           =   ZERO )
               PERFORM 9101-TENSUPLUS-READ-SEC
               IF      (FLG-TENSUPLUS          =   ZERO )
                 AND   (TNSP-USERTANICD    NOT =   SPACE)
                    MOVE    TNSP-USERTANICD    TO  TNS-TANICD
                    MOVE    TNSP-USERTANINAME  TO  TNS-TANINAME
               END-IF
           END-IF
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
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
           .
       9101-TENSUPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       930-SRYACT-RAED-SEC         SECTION.
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
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
       930-SRYACT-RAED-EXT.
           EXIT.
      *****************************************************************
      *    診療会計附加情報検索処理
      *****************************************************************
       940-SRYACCTPLUS-READ-SEC       SECTION.
      *
           MOVE    SPACE               TO  SRYACCTPLUS-REC
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM        TO  ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID           TO  ACCTP-PTID
           MOVE    ACCT-SRYKA          TO  ACCTP-SRYKA
           MOVE    ACCT-SRYYM          TO  ACCTP-SRYYM
           MOVE    ACCT-ZAINUM         TO  ACCTP-ZAINUM
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SRYACCTPLUS-REC
                   MOVE    ZERO                TO  FLG-SRYACCTPLUS
               ELSE
                   INITIALIZE                      SRYACCTPLUS-REC
                   MOVE    1                   TO  FLG-SRYACCTPLUS
               END-IF
           ELSE
               INITIALIZE                  SRYACCTPLUS-REC
               MOVE    1               TO  FLG-SRYACCTPLUS
           END-IF
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       940-SRYACCTPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       950-PTCOM-READ-SEC           SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   MOVE    1                   TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-SEL-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-SYSKANRI-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
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
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
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
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *
